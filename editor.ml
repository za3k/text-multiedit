(* Minimal text editor. Designed to support multiple users on one machine.
    
    (?) Standard 80 col max width, and use for justify?

    User Color Guide (shown at bottom)
    Shortcuts (shown at bottom)

    Limitations:
        Undo/redo not supported in v1 (may require semantic actions)
        Explicit save required
        ASCII only, no Unicode
*)

(* CLIENT LOGIC *)
(* Setup:
    Parse command line. Set username, filename.
    Set up initial data structures.
    Connect to the server via a unix socket, /tmp/ocaml-text
    Send "Open <document> as <name> [, view-only please]" to the server.
        The server sends back the current text and cursor positions of other
        connected users, creating the document if needed.
*)

open Types

let max_rows = 80

let color_code : background_color -> string = function
    | Default -> "0m"
    | Black -> "40m" | Red -> "41m" | Green -> "42m" | Yellow -> "43m" | Blue -> "44m" | Magenta -> "45m" | Cyan -> "46m" | White -> "47m"
    | BrightBlack -> "100m" | BrightRed -> "101m" | BrightGreen -> "102m" | BrightYellow -> "103m" | BrightBlue -> "104m" | BrightMagenta -> "105m" | BrightCyan -> "106m" | BrightWhite -> "107m"

let init_state = {
    text="Hello, world.\nThis is the second line.\nThis is the third line.\nThis is the fourth line\nThis is the fifth line\n"; 
    document_name="test_file.txt";
    per_user = [
        { user="zachary"; cursor=2; color=Red;  };
        { user="editor2"; cursor=11; color=BrightMagenta; };
    ]
}

let init_local_state = {
    uid = Some 0;
    view = 7;
    move_since_cut = true;
    terminal_size = { rows=25; cols=75 };
    clipboard = "";
    locked = false;
    error = None;
}

let remove1 (i: int) : 'a list -> 'a list = List.filteri (fun j x -> j <> i)
let compose f g x = f (g x) (* Support OCaml 4.14.1 *)
let copies num x = List.init num (fun _ -> x)
let sum = List.fold_left (+) 0
let rec any : 'a option list -> 'a option = function
    | [] -> None
    | Some x :: _ -> Some x
    | None :: l -> any l
let option_or a b = any [a; b]

let get_cursor_unsafe (state: state) (local_state: local_state) : int = 
    (List.nth state.per_user (Option.get local_state.uid)).cursor (* Option.get should never throw, because local_state.uid should always be set when this is called *)

(* Step 1: Read a keystroke *)
let get_keystroke char_reader =
    let c1 = char_reader () in
    if c1 != '\027' then Printf.sprintf "%c" c1
    else let c2 = char_reader() in
    if c2 != '[' then Printf.sprintf "%c%c" c1 c2
    else let rec loop s =
        let c = char_reader() in
        let s = Printf.sprintf "%s%c" s c in
        if String.contains "0123456789;" c then loop s
        else s
    in loop "\027["
    
let get_keystroke () = 
    let termio = Unix.tcgetattr Unix.stdin in
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { termio with Unix.c_icanon = false; Unix.c_echo = false; Unix.c_icrnl = false; Unix.c_ixon = false } in
    let res = get_keystroke (function () -> input_char stdin) in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

(* Step 2: Compute the [non-parameterized] action *)
let printable_ascii = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ " (* \n and \t are missing on purpose *)
let is_printable_ascii keystroke = String.length keystroke = 1 && String.get keystroke 0 |> String.contains printable_ascii
let shortcuts : (string list * (string*string) option * button) list = [
    (* Shortcuts are here listed in priority order of how to show them in the help *)
    (* input-bytes list / Help Display / enum-name *)
    (["\024"; "\022"], Some ("C-x", "Exit"), Exit); (* Ctrl-X or Ctrl-C. Note that Ctrl-C just exits with SIGINT currently. *)
    (["\019"], Some ("C-s", "Save"), Save); (* Requires ~IXON terminal setting *)
    (["\021"], Some ("C-u", "Paste"), Paste);
    (["\011"], Some ("C-k", "Cut"), Cut);
    (["\027[1;3B"], Some ("A-↓", "Scroll Down"), ScrollDown); (* Alt-Down *)
    (["\027[1;3A"], Some ("A-↑", "Scroll Up"), ScrollUp); (* Alt-Up *)
    (["\007"], Some ("C-g", "Help"), Help); (* Ctrl-G because Ctrl-H is backspace *)
    (["\n"], Some ("C-j", "Justify"), Justify); (* Requires ~ICRNL terminal setting to tell apart from enter *)

    (["\027[6~"], None, PageDown);
    (["\027[5~"], None, PageUp);
    (["\027[1~"; "\027[H"], None, Home);
    (["\027[4~"; "\027[F"], None, End);
    (["\027[A"], None, Up);
    (["\027[B"], None, Down);
    (["\027[C"], None, Right);
    (["\027[D"], None, Left);
    (["\r"], None, Key '\n'); (* Enter *)
    (["\t"], None, Tab);
    (["\b"; "\127"], None, Backspace);
    (["\027[3~"], None, Del); 
]

let rec lookup_shortcut keystroke = function
    | (ks, _, action) :: _ when List.mem keystroke ks -> Some action
    | _ :: l -> lookup_shortcut keystroke l
    | [] -> None

let get_button keystroke =
    if is_printable_ascii keystroke then Key (String.get keystroke 0)
    else match lookup_shortcut keystroke shortcuts with
        | Some action -> action
        | None -> Unknown keystroke
    

(* Step 3: Compute the parameterized action (ex, move cursor from where to where) *)
let split_send (actions: send_action list) : (send_local_action list * send_remote_action list) =
    let either : (send_action -> (send_local_action, send_remote_action) Either.t) = function
    | Local a -> Left a
    | Remote a -> Right a in
    List.partition_map either actions
let has_remote actions =
    split_send actions |> fst |> List.is_empty |> not
    
let insert offset str = [Remote(ReplaceText (offset, 0, str))]
let delete offset len = [Remote(ReplaceText (offset, len, ""))]
let move_cursor offset = if offset = 0 then [] else [Remote(ReplaceText (offset, 0, ""))]
let move_view offset = if offset = 0 then [] else [Local(ShiftView offset)]
let cut append_move clipboard text pos line_start line_end = 
    if line_start = line_end && (Text.document_end text) = line_end then
        [ Local( DisplayError( Some "Nothing was cut" )) ] else 
    let new_text = String.sub text line_start (line_end-line_start+1) in
    let clipboard = if append_move then clipboard ^ new_text else new_text in
    [
        Local(CopyText clipboard);
        Remote(ReplaceText (line_start-pos, line_end-line_start+1, ""));
        Local(CutFlag true)
    ]
let exit_action = Local Exit
let lock = Local Lock
let save = Remote Save
let error x = Local (DisplayError (Some x))
let no_error = Local (DisplayError None)
let cut_flag x = Local (CutFlag x)

(* There is a constraint that the cursor should always be inside the viewport.
This is logic to deal with it. *)

let avail_height (terminal: terminal_size) : int = terminal.rows - 4 (* 2 rows for help, 1 for status bar, 1 for error line *)
let status_width (terminal: terminal_size) : int = (min max_rows terminal.cols)
let avail_cols   (terminal: terminal_size) : int = (status_width terminal) - 5 (* room for line number display *)

let viewport state local_state =
    let width = (avail_cols local_state.terminal_size)
    and height = (avail_height local_state.terminal_size)
    and text = state.text in
    let (view_start, _) = Text.sline_for width text local_state.view in
    let view_end = Text.sline_add_whole width text view_start height in
    (* view_end-1 works because sometimes view_end is 1 aft:r the end of the document *)
    (view_start, view_end-1) 
let in_viewport vp pos = pos >= (fst vp) && pos <= (snd vp)

let cursor_in_viewport (text: string) (terminal: terminal_size) (view) (cursor: int) : cursor_bound =
    match Text.sline_difference (avail_cols terminal) text view cursor with
    | (n, _) when n < 0 -> OffTop n
    | (n, _) when n >= (avail_height terminal) -> OffBottom (n + 1 - (avail_height terminal))
    | _ -> OnScreen

let adjust_view_to_include_cursor (text: string) (terminal: terminal_size) (view: int) (cursor: int) : int =
    (* When the cursor moves, we need to scroll the view to include it. Return
    the new view. *)
    match cursor_in_viewport text terminal view cursor with
    | OnScreen -> view
    | OffTop n | OffBottom n -> Text.sline_add (avail_cols terminal) text view (n, 0)

let adjust_cursor_to_be_visible (text: string) (terminal: terminal_size) (view: int) (cursor: int) : send_action list =
    (* When the view moves, we need to move the cursor to stay inside it.
    Return cursor movements as actions. *)
    let new_cursor = match cursor_in_viewport text terminal view cursor with
    | OnScreen -> cursor
    | OffTop n | OffBottom n -> Text.sline_add (avail_cols terminal) text cursor (-n, 0)
    in move_cursor (new_cursor - cursor)

(* Functions such as PageUp/PageDown shift both the cursor and the view, and
even ScrollUp/ScrollDown can sometimes move the cursor *)

let shift_sline_action (state: state) (local_state: local_state) (slines: int) (pos: int) : int * int * int =
    if slines = 0 then (0,pos,0) else
    let width = avail_cols local_state.terminal_size in
    let new_pos = Text.sline_add width state.text pos (slines, 0) in
    let (shifted_slines, _) = Text.sline_difference width state.text pos new_pos in
    (shifted_slines, new_pos, new_pos-pos)

let shift_only_cursor (state: state) (local_state: local_state) (slines: int) : int * int * send_action list =
    let (shifted_slines, new_cursor, cursor_delta) = shift_sline_action state local_state slines (get_cursor_unsafe state local_state) in
    (shifted_slines, new_cursor, move_cursor cursor_delta)

let shift_only_view (state: state) (local_state: local_state) (slines: int) : int * int * send_action list =
    let (shifted_slines, new_view, view_delta) = shift_sline_action state local_state slines local_state.view in
    (shifted_slines, new_view, move_view view_delta)

let shift_view (state: state) (local_state: local_state) (slines: int) : send_action list = (* Used in: ScrollUp/ScrollDown *)
    let (shifted_slines, new_view, view_actions) = shift_only_view state local_state slines in
    let cursor = get_cursor_unsafe state local_state in
    let cursor_actions = adjust_cursor_to_be_visible state.text local_state.terminal_size new_view cursor in
    view_actions @ cursor_actions

let shift_cursor_and_view (state: state) (local_state: local_state) (slines: int) : send_action list = (* Used in: PageUp/PageDown *)
    let (shifted_slines, new_cursor, cursor_actions) = shift_only_cursor state local_state slines in
    let (_, _, view_actions) = shift_only_view state local_state shifted_slines in
    view_actions @ cursor_actions

let compute_actions (state: state) (local_state: local_state) button = 
    let page_lines = avail_height local_state.terminal_size in
    let text = state.text in
    let pos = get_cursor_unsafe state local_state in
    let (physical_line, col) = Text.line_of text pos in
    let clipboard = local_state.clipboard in
    let move_since_cut = local_state.move_since_cut in
    let (first, last) = Text.line_for text pos in 
    let actions = match button with
    | Backspace -> if pos > 0 then delete (-1) 1 else []
    | Del -> delete 0 1
    | Cut -> cut (not move_since_cut) clipboard text pos first last
    | Down -> move_cursor ((Text.pos_of text (physical_line+1) col)-pos)
    | Up ->   move_cursor ((Text.pos_of text (physical_line-1) col)-pos)
    | End ->  move_cursor (last-pos)
    | Home -> move_cursor (first-pos)
    | PageDown -> shift_cursor_and_view state local_state page_lines
    | PageUp ->   shift_cursor_and_view state local_state (-page_lines)
    | Exit -> [exit_action]
    | Help -> [] (* TODO *)
    | Justify -> [] (* TODO *)
    | Left ->  move_cursor (-1)
    | Right -> move_cursor 1
    | Paste -> insert 0 clipboard
    | Save -> [save]
    | ScrollDown -> shift_view state local_state 1
    | ScrollUp ->   shift_view state local_state (-1)
    | Tab -> insert 0 "    "
    | Key c -> insert 0 (String.make 1 c)
    | Unknown s -> [error (Printf.sprintf "Unknown key pressed: %s" s)]
    in let actions = actions @ match button with
    | Cut -> []
    | _ -> [cut_flag false]
    in let actions = actions @ match button with
    | Unknown _ -> []
    | _ -> [no_error] 
    in let actions = (if has_remote actions then [lock] else []) @ actions
    in actions

(* Step 4: Apply local state changes *)

let apply_local_action (state: state) (local_state: local_state): send_local_action -> local_state = function
    (*print_endline (Printf.sprintf "ApplyingLocal: %s" (Debug.string_of_send_action
    action));*)
    | CopyText s -> { local_state with clipboard = s }
    | CutFlag b -> { local_state with move_since_cut = not b }
    | DisplayError s -> { local_state with error = s }
    | Exit -> exit 0
    | Lock -> { local_state with locked = true }
    | ShiftView d -> { local_state with view = local_state.view + d }

(* Step 5: Send the action to the server *)
(* Step 6: Receive an action from the server *)

let server_stub1 (uid : int option): (send_remote_action -> receive_action list) = function
    | ReplaceText (a,b,c) -> (match uid with
        | Some uid -> [ReplaceText (uid,a,b,c)]
        | None -> [] )
    | OpenDocument (document, username) -> [UserJoins { user=username; cursor=0; color=Red }; SetUser 0] (* TODO *)
    | Save -> [] (* Doesn't happen in stub *)
let server_stub (uid: int option) (actions: send_remote_action list) : receive_action list = 
    List.concat_map (server_stub1 uid) actions @ if actions = [] then [] else [Unlock]

(* Step 7: Transform the state based on the action
    - Text
    - Cursor position (per-user)
    - User list
    VIEW is not included in the state, but consists of a start position only,
    which is always at the beginning of a line.
*)

let apply_remote_action (combined_state: state * local_state) : receive_action -> state * local_state = 
    (*print_endline (Printf.sprintf "ApplyingRemote: %s" (Debug.string_of_receive_action action));*)
    let (state, local_state) = combined_state in
    function
    | ReplaceText (ed_uid, start, length, replacement) ->
        (* 
            1. Change the text
            2. Move the editor's cursor
            3. Shift view
            4. Update view if the user's cursor is off screen
        *)
        let text_length = String.length state.text in
        let user = List.nth state.per_user ed_uid in
        let pos = user.cursor + start in (* Clip removed part to bounds of the document *)
        let pos = min (max 0 pos) (text_length - 1) in
        let length = min length (text_length - 1 - pos) in
        let net_change = (String.length replacement) - length in
        let new_text = (String.sub state.text 0 pos) ^ replacement ^ (String.sub state.text (pos + length) (text_length - pos - length)) in
        (*             state.text[0:pos]             + replacement + state.text[pos+length:] *)
        let shift p = if p <= pos then p else p + net_change in
        let new_users = List.mapi (fun (uid: int) (user: user_state) ->
            if uid = ed_uid then { user with cursor = pos + (String.length replacement) }
            else { user with cursor = shift user.cursor }
        ) state.per_user in
        let state = { state with per_user = new_users; text = new_text } in
        let cursor = get_cursor_unsafe state local_state in
        let view = adjust_view_to_include_cursor new_text
                   local_state.terminal_size (shift local_state.view) cursor in
        let local_state = { local_state with view = view } in
        (state, local_state)
    | UserJoins user_state -> ({state with per_user = state.per_user @ [user_state]}, local_state)
    | UserLeaves uid -> ({state with per_user = remove1 uid state.per_user}, local_state)

    | SetUser uid -> (state, {local_state with uid=local_state.uid})
    | Unlock -> (state, { local_state with locked = false })

(* Step 8: Update the UI
    8a: Calculate the view, based on the last view, the current terminal size,
        and the cursor position.
    8b: Display the text (or the help)
    8c: Display the active users
    8d: Display the shortcuts
*)

let rec lookup_cursor_color (users: user_state list) (pos: int) : background_color option = 
    (* { user="zachary"; cursor=2; color=Red;  }; *)
    List.map (function
        | { cursor; color } when cursor = pos -> Some color
        | _ -> None
    ) users |> any

let colorize (color: background_color) (s: string) : string =
    "\027[" ^ (color_code color) ^ s ^ "\027[0m"
        
let rec except_last = function
    | [] -> []
    | a :: [] -> []
    | h :: l -> h :: except_last l

let display_viewport (width: int) (text: string) (color_of: int -> background_color option) 
                     (visible_range: int * int) (debug: bool): string list =
    let colorize_char (index: int) (c: char) =
        if (not debug) &&
            (* Restrict to the viewport -- this is a bad way to do this *)
           (index < (fst visible_range) || index > (snd visible_range))
        then "" else
        let s = match c with
            | '\n' -> " "
            | _ -> String.make 1 c
        in match color_of index with
            | Some color -> colorize color s
            | None -> s in

    let open Seq in
    let lines = String.split_on_char '\n' text |> except_last |> List.to_seq
                |> map (fun x -> x ^ "\n") in
    let start_indices = lines |> map String.length |> scan (+) 0 in
    zip start_indices lines |> zip (ints 0) |> map (function
        | (line_no, (line_start, line)) ->
            let slines = line |> unfold (function
                | "" -> None
                | s when String.length s <= width -> Some (s, "")
                | s -> let first = String.sub s 0 width
                       and rest = String.sub s width ((String.length s)-width) in
                       Some (first, rest)) in
            let start_indices = map String.length slines |> scan (+) line_start in
            zip start_indices slines |> zip (ints 0) |> zip (repeat line_no))
    |> concat |> filter_map (function
        | (line_no, (sline_no, (sline_start, sline))) ->
            let colorize = fun i c -> colorize_char (i+sline_start) c in
            let line_number = if sline_no = 0 then Printf.sprintf "%4d " (line_no + 1 - Bool.to_int debug) else "     " and
                sline_num = if debug then Printf.sprintf "%2d " sline_no else "" and
                colorized_sline = (sline |> String.to_seq |> mapi colorize |> List.of_seq |> String.concat "") and
                (* We have to pad horizontally here because of ANSI escape codes mucking with string lengths *)
                padding = String.make (max 0 (width - (String.length sline))) ' ' in
            if colorized_sline = "" then None else
            line_number ^ sline_num ^ colorized_sline ^ padding |> Option.some)
    |> List.of_seq

let display_document width text color_of debug : string list =
    display_viewport width text color_of (0, Text.document_end text) debug

let spacer_lines width num =
    copies num (String.make width ' ')

let display_cursors state : string =
    state.per_user |> List.map (function
        | {user; cursor; color} ->
            Printf.sprintf " %s: %d " user cursor |> colorize color)
    |> String.concat "  "

let display_view state local_state : string =
    let (vs, ve) = viewport state local_state in
    Printf.sprintf " Viewport %d-%d [%d] " vs ve local_state.view |> colorize Blue

let display_help width : string list =
    let second = function (a, b, c) -> b in
    let rec consecutive_pairs default = function
        | [] -> []
        | [a] -> [(a, default)]
        | a :: b :: l -> (a, b) :: consecutive_pairs default l in
    let rec prefixes = function
        | [] -> []
        | _ :: l as all -> all :: (prefixes l) in
    let pad_shorter s1 s2 = 
        match (String.length s1, String.length s2) with
        | (a, b) when a = b -> (s1, s2)
        | (a, b) when a < b -> (s1 ^ (String.make (b-a) ' '), s2)
        | (a, b)            -> (s1, s2 ^ (String.make (a-b) ' ')) in

    let equalize = function ((s1, n1), (s2, n2)) ->
        let (s1, s2) = pad_shorter s1 s2 and
            (n1, n2) = pad_shorter n1 n2 in
        ((s1, n1), (s2, n2)) in
    let bit_for = function
        | (shortcut, name) -> 
            (((String.length shortcut) + (String.length name) + 1), 
             (colorize Blue shortcut) ^ " " ^ name) in

    let shortcuts = List.filter_map second shortcuts in
    let sublists = prefixes @@ List.map equalize @@ consecutive_pairs ("","") shortcuts in
    let display_all cuts : string list option =
        (* Assume we can display every shortcut, then try to do it. *)
        let min_padding = 2 and
            count = List.length cuts and
            line1 = List.map fst cuts |> List.map bit_for and
            line2 = List.map snd cuts |> List.map bit_for in
        let min_size = List.map (fun x -> (fst x)+min_padding) line1 |> sum and
            line1 = List.map snd line1 and
            line2 = List.map snd line2 in
        (* Check the line lengths -- are they too long? If so, return None *)
        if min_size > width then None else
        let padding = String.make ((width - min_size) / count) ' ' in
        List.map (String.concat padding) [line1; line2]
        |> Option.some
    in
    List.find_map display_all sublists |> Option.value ~default:["";""]

let status_line width debug state local_state =
    [display_cursors state ^ display_view state local_state]

let display_debug (state: state) (local_state: local_state) : unit =
    let visible = in_viewport (viewport state local_state) and
        text_width = (avail_cols local_state.terminal_size) in
    let color p = any [
        lookup_cursor_color state.per_user p;
        if visible p then Some Blue else None
    ] in

    let lines = [""] @ display_document text_width state.text color true
    @ status_line text_width true state local_state
    @ display_help text_width in

    print_string (String.concat "\n" lines); flush stdout

let print_lines lines = (* Print 25x 80-column lines *)
    let print_line i line =
        print_string (Printf.sprintf "\027[%d;0H%s" (i+1) line) in
    List.iteri print_line lines; flush stdout

let display (state: state) (local_state: local_state) : unit =
    let viewport = viewport state local_state and
        color = lookup_cursor_color state.per_user and
        text_width = avail_cols local_state.terminal_size and
        status_width = status_width local_state.terminal_size and
        height = avail_height local_state.terminal_size in
    let display_lines = display_viewport text_width state.text color viewport false in
    let lines = display_lines
    @ spacer_lines status_width (max (height - (List.length display_lines)) 0)
    @ status_line status_width false state local_state
    @ display_help status_width in

    print_lines lines

(* [Go to step 1] *)
(* Teardown:
    Send "Disconnect" to the server.
*)

type client_args = {
    debug: bool;
    files: string list;
    user: string;
}
let client_main (client_args: client_args) : unit =
    (* Terminal stuff for start/exit *)
    (* Restore the terminal, show the cursor *)
    let reset () : unit = print_string "\027[?1049l\027[?25h" in at_exit reset;
    (* Save terminal, home the cursor, hide the cursor *)
    print_string "\027[?1049h\027[H\027[?25l"; 
    Sys.set_signal Sys.sigint (Signal_handle (function | _ -> exit 0));

    (* Setup of the client *)
    let local_state = ref init_local_state in
    if client_args.debug then
        local_state := { !local_state with terminal_size = { rows=7; cols=11 } }
    else ();
    let state = ref init_state in

    (* Main loop of the client *)
    (* In a loop, listen for any of:
        - Messages from the server
        - Terminal resize events (SIGWINCH)
        - User keyboard input
    *)
    let ps = if client_args.debug then print_string else (fun _ -> ()) in
    while true do
        (if client_args.debug then display_debug else display)
            !state !local_state;
        let keystroke = get_keystroke() in
            ps (Printf.sprintf "\"%s\" || " (String.escaped keystroke));
        let button = get_button keystroke in
            ps @@ Debug.string_of_button button ^ " || ";
        let actions = compute_actions !state !local_state button in
            ps @@ String.concat "" (List.map Debug.string_of_send_action actions); ps "\n";
        let (local, remote) = split_send actions in
        local_state := List.fold_left (apply_local_action !state) !local_state local;
        let msgs = server_stub !local_state.uid remote in
        let (s, ls) = List.fold_left apply_remote_action (!state, !local_state) msgs in
        state := s;
        local_state := ls
    done

(* SERVER LOGIC *)
(* Setup:
    Listen to unix socket /tmp/ocaml-text
    On connect, listen for first command. Should always be "Open <document> as
    <name>". Tag the connection with document and name. Put into the list of
    connections for that document, setting up the document if neded.
*)

type server_args = {
    dir: string;
}

let server_main (server_args: server_args) : unit =
    ()

(* Document setup
    Open file for reading
    Populate state:
        text (contents, or empty if file doesn't exist)
        empty list of connects
    Add first connection
*)
(* Per Connection Setup
    Tag the connection with the username.
    Locate the document in the list of documents. If it doesn't exist, set up
    the document.
    Send+apply: "<user> connected and their cursor is at position 0" action
*)
(* Connection Action received:
    Connect/Disconnect: See above/below
    Save: Save the document
    Else: Send+apply. Send the message to everyone connected, and apply it to
          the server's state model.
*)
(* Per Connection Disconnect 
    Send+apply: "<user> disconnected" action
    If there are zero users connected, save the document and remove it from the
    documents list.
*)

type run_mode = Client | Server | StandAlone
type cli_args = {
    mode: run_mode;
    client_args: client_args;
    server_args: server_args;
}
    
let parse_args () : cli_args =
    let usage_msg = "text [--debug] [--dir DIR] [--stand-alone|--server|--client]" in
    let debug = ref false 
    and mode = ref Client
    and dir = ref None
    and files = ref [] in
    let anon_fun filename = files := filename :: !files
    and set_mode m = (Arg.Unit (fun () -> mode := m))
    and set_option_string r = (Arg.String (fun s -> r := Some s)) in
    let speclist = [
        ("--debug", Arg.Set debug, "Make the screen small (6x4) and turn off screen refresh");
        ("--stand-alone", set_mode StandAlone, "Run a stand-alone server to edit files with just one user");
        ("--client", set_mode Client, "Connect to an existing server (the default)");
        ("--server", set_mode Server, "Run a server to edit files. Files will be owned by the server uid.");
        ("--dir", set_option_string dir, "Set the server working directory, where text files will be located.");
        ("--", Arg.Rest anon_fun, "Stop parsing arguments");
    ] in
    Arg.parse speclist anon_fun usage_msg;
    let get_dir mode dir = 
        let default = match mode with
        | Client -> None
        | Server -> Some "/var/text"
        | StandAlone -> Some (Sys.getcwd ())
        in option_or dir default in
    let user = (Unix.getuid () |> Unix.getpwuid).pw_name in
    let dir = get_dir !mode !dir in
    { 
        mode = !mode; 
        client_args = { files = !files; debug = !debug; user };
        server_args = { dir = Option.value dir ~default: "" }
    }

let main () : unit =
    let args = parse_args () in
    match args.mode with
    | Client -> client_main args.client_args
    | Server -> server_main args.server_args
    | StandAlone -> ()
        (*
        TODO: Does not work in OCaml 4.14.1, find an older solution?
        let server = Domain.spawn (fun () -> server_main args.server_args) in (* TODO: Make sure the server is ready *)
        client_main args.client_args;
        Domain.join server
        *)
