(*  textmu, a multi-user text editor

    Designed to support multiple users ssh-ed into one machine, editing a
    set of documents at the same time.
    
    Limitations:
        ASCII only, no Unicode
        Explicit save required
        Undo/redo not supported
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

let version = "0.0.1"
let max_rows = 80

let editor_colors = [Red;Green;Yellow;Blue;Magenta;Cyan;White;BrightBlack;BrightRed;BrightGreen;BrightYellow;BrightBlue;BrightMagenta;BrightCyan;BrightWhite]

let empty_document document_name = {
    text="\n"; 
    document_name;
    per_user = []
}

let test_doc_name = "TESTING.txt"
let testing_document = {
    text="  RAINBOWrainbowRAINBOW\nThis is the second line.\nThis is the third line.\nThis is the fourth line\nThis is the fifth line\n"; 
    document_name=test_doc_name;
    per_user = 
        []
    (*
        { user="zachary"; cursor=0; color=Red;  }
        ::(List.mapi (fun i c -> { 
            user=Printf.sprintf "editor%d" (i+1);
            cursor=2+i;
            color=c
        }) (List.tl editor_colors));
        *)
}

let init_local_state = {
    uid = None;
    view = 0;
    move_since_cut = true;
    terminal_size = { rows=25; cols=80 };
    clipboard = "";
    locked = true;
    error = None;
}

let remove_index (i: int) : 'a list -> 'a list = List.filteri (fun j x -> j <> i)
let remove (x: 'a) (l: 'a list) = List.filter ((!=) x) l
let compose f g x = f (g x) (* Support OCaml 4.14.1 *)
let copies num x = List.init num (fun _ -> x)
let sum = List.fold_left (+) 0
let rec any : 'a option list -> 'a option = function
    | [] -> None
    | Some x :: _ -> Some x
    | None :: l -> any l
let option_or a b = any [a; b]
let rec except_last : 'a list -> 'a list = function
    | [] -> []
    | a :: [] -> []
    | h :: l -> h :: except_last l
let rec suffixes = function (* In order from biggest to smallest *)
    | [] -> []
    | _ :: l as all -> all :: (suffixes l)
let prefixes l = (* In order from biggest to smallest *)
    l |> List.rev |> suffixes |> List.map List.rev
let string_of_list = Debug.string_of_list
let truncate len s =
    if String.length s <= len then s
    else String.sub s 0 (len-1)
let has_trailing_newline : string -> bool = String.ends_with ~suffix:"\n"
let remove_trailing_newline s = 
    String.sub s 0 ((String.length s)-1)

let get_cursor (state: state) (local_state: local_state) : int option =
    let user = match local_state.uid with
        | None -> None
        | Some uid -> List.nth_opt state.per_user uid in
    Option.map (fun user -> user.cursor) user
let get_cursor_unsafe state local_state : int = 
    get_cursor state local_state |> Option.get

let sigwinch = 28 (* Source: https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/signal.h *)

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
    
let get_keystroke ic = 
    get_keystroke (function () -> input_char ic)

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
    (*
    (["\007"], Some ("C-g", "Help"), Help); (* Ctrl-G because Ctrl-H is backspace *)
    (["\n"], Some ("C-j", "Justify"), Justify); (* Requires ~ICRNL terminal setting to tell apart from enter *)
    *)

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
    split_send actions |> snd |> (<>) []
let has_error actions = 
    let is_error = function
    | Local (DisplayError (Some _)) -> true
    | _ -> false in
    List.exists is_error actions
    
let exit_action = [Local Exit]
let lock = [Local Lock]
let save = [Remote Save]
let error x = [Local (DisplayError (Some x))]
let no_error = [Local (DisplayError None)]
let cut_flag x = [Local (CutFlag x)]
let insert offset str = [Remote(ReplaceText (offset, 0, str))]
let delete offset len = [Remote(ReplaceText (offset, len, ""))]
let move_cursor offset = if offset = 0 then [] else [Remote(ReplaceText (offset, 0, ""))]
let move_view offset = if offset = 0 then [] else [Local(ShiftView offset)]
let cut append_move clipboard text pos line_start line_end = 
    if line_start = line_end && (Text.document_end text) = line_end then
        error "Nothing was cut" else 
    let new_text = String.sub text line_start (line_end-line_start+1) in
    let clipboard = if append_move then clipboard ^ new_text else new_text in
    [
        Local(CopyText clipboard);
        Remote(ReplaceText (line_start-pos, line_end-line_start+1, ""));
    ] @ cut_flag true

(* There is a constraint that the cursor should always be inside the viewport.
This is logic to deal with it. *)

let viewport_height (terminal: terminal_size) : int = terminal.rows - 5 (* 1 row for title, 2 rows for help, 1 for status bar, 1 for error line *)
let status_width (terminal: terminal_size) : int = terminal.cols
let avail_cols   (terminal: terminal_size) : int = (min max_rows terminal.cols) - 5 (* room for line number display *)

let viewport state local_state =
    let width = (avail_cols local_state.terminal_size)
    and height = (viewport_height local_state.terminal_size)
    and text = state.text in
    if text = "\n" then (0,0) else
    let (view_start, _) = Text.sline_for width text local_state.view in
    let view_end = Text.sline_add_whole width text view_start height in
    (* view_end-1 works because sometimes view_end is 1 aft:r the end of the document *)
    (view_start, view_end-1) 
let in_viewport vp pos = pos >= (fst vp) && pos <= (snd vp)

let cursor_in_viewport (text: string) (terminal: terminal_size) (view) (cursor: int) : cursor_bound =
    match Text.sline_difference (avail_cols terminal) text view cursor with
    | (n, _) when n < 0 -> OffTop n
    | (n, _) when n >= (viewport_height terminal) -> OffBottom (n + 1 - (viewport_height terminal))
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

let shift_sline_action (state: state) (local_state: local_state) 
                       (slines: int) (pos: int) (f: int -> 'a) 
                       : int * int * 'a =
    if slines = 0 then (0,pos,f 0) else
    let width = avail_cols local_state.terminal_size in
    let new_pos = Text.sline_add width state.text pos (slines, 0) in
    let (shifted_slines, _) = Text.sline_difference width state.text pos new_pos in
    (shifted_slines, new_pos, f (new_pos-pos))

let shift_only_view s ls slines : int * int * send_action list = (* Used in: terminal resize events *)
    shift_sline_action s ls slines ls.view move_view

let shift_only_cursor s ls slines : int * int * send_action list =
    shift_sline_action s ls slines (get_cursor_unsafe s ls) move_cursor

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
    let page_lines = viewport_height local_state.terminal_size in
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
    | Exit -> exit_action
    | Help -> [] (* TODO *)
    | Justify -> [] (* TODO *)
    | Left ->  move_cursor (-1)
    | Right -> move_cursor 1
    | Paste -> insert 0 clipboard
    | Save -> save
    | ScrollDown -> shift_view state local_state 1
    | ScrollUp ->   shift_view state local_state (-1)
    | Tab -> insert 0 "    "
    | Key c -> insert 0 (String.make 1 c)
    | Unknown s -> error (Printf.sprintf "Unknown key pressed: '%s'" (String.escaped s))
    in let actions = actions @ match button with
    | Cut -> []
    | _ -> cut_flag false
    in let actions = actions @ (if has_error actions then [] else no_error)
    in let actions = (if has_remote actions then lock else []) @ actions
    in actions

(* Step 4: Apply local state changes *)

let apply_local_action (state: state) (local_state: local_state): send_local_action -> local_state = function
    | CopyText s -> { local_state with clipboard = s }
    | CutFlag b -> { local_state with move_since_cut = not b }
    | DisplayError s -> { local_state with error = s }
    | Exit -> exit 0
    | Lock -> { local_state with locked = true }
    | ShiftView d -> { local_state with view = local_state.view + d }

(* Step 5: Send the action to the server *)
(* Step 6: Receive an action from the server *)

(* Step 7: Transform the state based on the action
    - Text
    - Cursor position (per-user)
    - User list
    VIEW is not included in the state, but consists of a start position only,
    which is always at the beginning of a line.
*)

let apply_remote_action (state: state) : receive_action -> (state * (local_state -> local_state)) = 
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
        let pos = Text.document_clamp state.text pos in
        let length = min length (text_length - 1 - pos) in
        let net_change = (String.length replacement) - length in
        let new_text = (String.sub state.text 0 pos) ^ replacement ^ (String.sub state.text (pos + length) (text_length - pos - length)) in
        (*             state.text[0:pos]             + replacement + state.text[pos+length:] *)
        let shift p = 
            (* If cursor is in affected region, move to the start of the replacement/deletion *)
            if p >= pos && p < pos + length then pos
            (* Otherwise shift cursor if needed *)
            else if p < pos then p else p + net_change in 
        let new_users = List.mapi (fun (uid: int) (user: user_state) ->
            if uid = ed_uid then { user with cursor = pos + (String.length replacement) }
            else { user with cursor = shift user.cursor }
        ) state.per_user in
        let state = { state with per_user = new_users; text = new_text } in

        let rest local_state =
            match get_cursor state local_state with
            | None -> local_state
            | Some cursor ->
                let view = adjust_view_to_include_cursor new_text
                        local_state.terminal_size (shift local_state.view) cursor in
                { local_state with view = view } in
        (state, rest)
    | UserJoins user_state -> 
        ({state with per_user = state.per_user @ [user_state]}, Fun.id)
    | UserLeaves uid -> 
        let state = {state with per_user = remove_index uid state.per_user} in
        let adjust = function
            | None -> None
            | Some u when u < uid -> Some u
            | Some u when u = uid -> failwith "apply_remote_action: The current user left."
            | Some u -> Some (u - 1) in
        let rest (local_state: local_state) =
            { local_state with uid=adjust local_state.uid} in
        (state, rest)
    | SetUser uid -> 
        let rest local_state : local_state =
            {local_state with uid=Some uid} in
        (state, rest)
    | Unlock -> 
        let rest local_state =
            { local_state with locked = false } in
        (state, rest)

let tee s = failwith s
let get_terminal_size () : terminal_size =
    let command x =
        let p = Unix.open_process_in x in
        let r = In_channel.input_all p in
        In_channel.close p; r in
    match command "stty size" |> remove_trailing_newline |> String.split_on_char ' ' |> List.map int_of_string with
        | [rows; cols] -> {rows;cols}
        | _ -> failwith "Invalid terminal size"

let resize_terminal (state: state) (local_state: local_state) : local_state = 
    let old_size = local_state.terminal_size and
        new_size = get_terminal_size () in
    let local_state = { local_state with terminal_size = get_terminal_size () } in

    (* Clear the screen so there's no garbage left after 80 cols on the edges. Note, this could be removed if we re-wrote display logic to write the full line. *)
    (* TODO: Don't do this in debug mode -- by just printing the full width always*)
    print_string "\027[2J"; 

    (* Set the new size *)
    let local_state = apply_local_action state local_state (DisplayError (Some "Terminal size changed")) in
    
    let local_actions = (match get_cursor state local_state with
        | None -> []
        | Some cursor ->
            (match cursor_in_viewport state.text new_size local_state.view cursor with
                | OnScreen -> []
                | OffTop _ -> failwith "The cursor somehow went off the top of the screen during terminal resize. This should never happen."
                | OffBottom amt -> 
                    let only_local actions = fst @@ split_send actions in
                    let (_, _, actions) = shift_only_view state local_state amt in
                    only_local actions
            )
    ) in

    List.fold_left (apply_local_action state) local_state local_actions

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

let color_code : background_color -> string = function
    | Black   -> "40m"    | BrightBlack   -> "100;30m" 
    | Red     -> "41;30m" | BrightRed     -> "101;30m" 
    | Green   -> "42;30m" | BrightGreen   -> "102;30m" 
    | Yellow  -> "43;30m" | BrightYellow  -> "103;30m" 
    | Blue    -> "44;97m" | BrightBlue    -> "104;97m" 
    | Magenta -> "45;30m" | BrightMagenta -> "105;30m" 
    | Cyan    -> "46;30m" | BrightCyan    -> "106;30m" 
    | White   -> "47;30m" | BrightWhite   -> "107;30m"
let colorize (color: background_color) : string -> string =
    Printf.sprintf "\027[%s%s\027[0m" (color_code color)
        
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
            if colorized_sline = "" then None else (* This is needed because of the visible_range colorizining hack, and the illegal document ''. *)
            line_number ^ sline_num ^ colorized_sline ^ padding |> Option.some)
    |> List.of_seq

let display_document width text color_of debug : string list =
    display_viewport width text color_of (0, Text.document_end text) debug

let spacer_lines width num =
    copies num (String.make width ' ')

let debug_cursors state : string =
    state.per_user |> List.map (function
        | {user; cursor; color} ->
            Printf.sprintf " %s: %d " user cursor |> colorize color)
    |> String.concat "  "

let debug_view state local_state : string =
    let (vs, ve) = viewport state local_state in
    Printf.sprintf " Viewport %d-%d [%d] " vs ve local_state.view |> colorize Blue

let display_help width : string list =
    let second = function (a, b, c) -> b in
    let rec consecutive_pairs default = function
        | [] -> []
        | [a] -> [(a, default)]
        | a :: b :: l -> (a, b) :: consecutive_pairs default l in
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

let title_line width doc =
    let open String in
    let program = Printf.sprintf " textmu %s " version and
        docname = Printf.sprintf "[%s]" doc in
    let slack = (width - (length program) - (length docname)) in
    let text = program ^ (make slack ' ') ^ docname in
    [colorize Blue text]

let error_line width = function
    | None -> [String.make width ' ']
    | Some error ->
        let error = Printf.sprintf "[ %s ]" error in
        let e = width - (String.length error) in
        let left = String.make (e/2) ' ' and
            right = String.make (e-e/2) ' ' in
        [left ^ error ^ right]

let status_line width state local_state =
    let max_user_len = 20 in
    let for_cursor = function
        | {user; cursor; color} -> (user, color) in
    let bits : (string * background_color) list = List.map for_cursor state.per_user in
    let resize target_size s =
        match String.length s with
        | l when l = target_size -> s
        | l when l < target_size -> s ^ (String.make (target_size - l) ' ')
        | l -> (String.sub s 0 (target_size-3)) ^ "..." in
    let display_all bits = 
        let bit_len = function (s, c) -> min max_user_len @@ String.length s in
        let lengths = List.map bit_len bits and
            num_bits = List.length bits in
        let max_len = List.fold_left max 0 lengths and
            bit_width = (width / (max 2 num_bits)) - 2 in
        if bit_width < max_len then None else

        let slack = width - (num_bits * bit_width) in
        let string_of_bit = function (s, c) -> colorize c @@ " " ^ (resize bit_width s) ^ " " in
        
        (String.concat "" @@ List.map string_of_bit bits) ^ (String.make slack ' ') 
        |> Option.some in

    List.find_map display_all (prefixes bits) |> Option.value ~default:""
    |> (fun x -> [x])

let display_debug (state: state) (local_state: local_state) : unit =
    let visible = in_viewport (viewport state local_state) and
        text_width = (avail_cols local_state.terminal_size) in
    let color p = any [
        lookup_cursor_color state.per_user p;
        if visible p then Some Blue else None
    ] in

    let lines = [""] @ display_document text_width state.text color true
    @ [debug_cursors state ^ debug_view state local_state]
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
        height = viewport_height local_state.terminal_size in
    let document_lines = display_viewport text_width state.text color viewport false 
        |> List.map (fun x -> x ^ String.make (status_width - text_width) ' ') in
    let lines = 
          title_line status_width state.document_name
        @ document_lines
        @ spacer_lines status_width (max (height - (List.length document_lines)) 0)
        @ error_line status_width local_state.error
        @ status_line status_width state local_state
        @ display_help status_width in

    print_lines lines
    (*
    ;Debug.string_of_list (Printf.sprintf "'%s'") lines |> print_endline
    ;Printf.printf "Viewport: (%d - %d)\n" (fst viewport) (snd viewport)
    *)

(* [Go to step 1] *)
(* Teardown:
    Send "Disconnect" to the server.
*)

type client_args = {
    debug: bool;
    file: string;
    user: string;
    socket: string;
}
let client_main (client_args: client_args) : unit =
    (* Terminal stuff for start/exit *)
    (* Restore the terminal, show the cursor *)
    let reset () : unit = print_string "\027[?1049l\027[?25h" in at_exit reset;
    (* Save terminal, home the cursor, hide the cursor *)
    print_string "\027[?1049h\027[H\027[?25l"; 
    Sys.set_signal Sys.sigint (Signal_handle (function | _ -> exit 0));

    (* Let us read one keystroke at a time from stdin *)
    let termio = Unix.tcgetattr Unix.stdin in
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { termio with 
            Unix.c_icanon = false; 
            Unix.c_echo = false; 
            Unix.c_icrnl = false; 
            Unix.c_ixon = false };
    let reset_stdin () : unit = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio in
        at_exit reset_stdin;

    (* Setup of the client state *)
    let local_state = ref {init_local_state with terminal_size = get_terminal_size ()} in
    let state = ref @@ empty_document client_args.file in

    (* Debug mode prints extra info and doesn't clear the screen *)
    let ps =      if client_args.debug then print_string  else ignore  and
        display = if client_args.debug then display_debug else display in
    if client_args.debug then
        local_state := { !local_state with terminal_size = { rows=7; cols=11 } };

    (* Listen for any of:
        - Messages from the server
        - Terminal resize events via SIGWINCH
        - User keyboard input (disabled if !local_state.locked
    *)
    let ((server_received: receive_action list Input.t), 
        (server_sent: send_remote_action list Output.t)) = 
            Input.of_socket client_args.socket and
        terminal_resizes : unit Input.t = Input.of_signal sigwinch and
        key_presses : string Input.t = Input.of_file ~reader:get_keystroke stdin in

    (* Initial message to server *)
    let () = Output.send server_sent [OpenDocument (client_args.file, client_args.user)] in

    (* Main loop. Listen to any of the three channels listed. *)
    while true do
        display !state !local_state;

        (Input.select ([
            Input.handle server_received;
            Input.handle terminal_resizes;
        ] @ 
            if (!local_state).locked then [] else [Input.handle key_presses]
        ));

        match Input.read server_received with
            | Some msgs -> 
                    ps @@ Printf.sprintf "Event: SERVER MSG %s\n" @@ Debug.string_of_list Debug.string_of_receive_action msgs;
                let apply1 msg =
                    let (s, rest) = apply_remote_action !state msg in
                    state := s;
                    local_state := rest !local_state in
                List.iter apply1 msgs
            | Closed -> exit 2
            | NotReady ->
        match Input.read terminal_resizes with
            | Some () ->
                    ps "Event: TERM RESIZE\n";
                local_state := resize_terminal !state !local_state
            | Closed -> failwith "terminal_resizes closed (this should never happen)"
            | NotReady ->
        match Input.read key_presses with
            | Some keystroke ->
                    ps (Printf.sprintf "Event: KEYPRESS \"%s\" || " (String.escaped keystroke));
                let button = get_button keystroke in
                    ps @@ Debug.string_of_button button ^ " || ";
                let actions = compute_actions !state !local_state button in
                    ps @@ String.concat "" (List.map Debug.string_of_send_action actions); ps "\n";
                let (local, remote) = split_send actions in
                local_state := List.fold_left (apply_local_action !state) !local_state local;
                if not @@ List.is_empty remote then 
                    Output.send server_sent remote
            | Closed -> exit 0 (* Standard input closed *)
            | NotReady -> ()
    done

(* SERVER LOGIC *)
(* Setup:
    Listen to unix socket /tmp/ocaml-text
    On connect, listen for first command. Should always be "Open <document> as
    <name>". Tag the connection with document and name. Put into the list of
    connections for that document, setting up the document if neded.
*)

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

let path (dir: string) (docname: string) : string =
    dir ^ "/" ^ docname

let load path : string option =
    (*Printf.printf "Loading file: %s\n" path;*)
    match In_channel.open_bin path with
        | exception Sys_error _ -> None
        | c -> 
            let t = In_channel.input_all c in
            In_channel.close c; 
            Option.some @@ if has_trailing_newline t then t else t ^ "\n"

let save path contents : unit =
    (*Printf.printf "Saving file: %s\n" path;*)
    match Out_channel.open_bin path with
        | exception Sys_error _ -> ()
        | c ->
            Out_channel.output_string c contents;
            Out_channel.close c

let free_color (state: state) : background_color =
    let used_colors = List.map (fun u -> u.color) state.per_user in
    let is_free_color c = not @@ List.mem c used_colors in
    let free_colors = List.filter is_free_color editor_colors in
    match free_colors with
    | [] -> White (* 17th editor and beyond all get the same color *)
    | c :: _ -> c

let process_actions dir debug (user: user) actions : unit =
    (* We got some actions from a user. Do the commands they send.
        This will involve:
        - Changing the document
        - Possibly changing the filesystem (save commmand)
        - Sending back responses to all users on that document right now
        TODO: Deal with user disconnections, too.
    *)
    let document = user.document and
        users = !(user.document.users) and
        list_of_queue q = List.of_seq @@ Queue.to_seq q and
        q_user = Queue.create () and
        q_everyone = Queue.create () in
    let send_one u (q: receive_action Queue.t) : unit = 
            if not @@ Queue.is_empty q then
            if debug then (
                print_endline @@ Printf.sprintf "%s (%d): %s"
                    (colorize Green u.name) u.uid
                    (Debug.string_of_list Debug.string_of_receive_action (list_of_queue q))
            );
            Output.send u.conn.out (list_of_queue q) and
        enqueue_user x = Queue.push x q_user and
        enqueue_all  x = Queue.push x q_user;
                         Queue.push x q_everyone in
    let flush_one (u: user) = 
            send_one u (if user == u then q_user else q_everyone) in
    let flush_all ()   = List.iter flush_one users in
    
    let process_action : send_remote_action -> unit = function
        | ReplaceText (a,b,c) ->
            enqueue_all @@ ReplaceText (user.uid,a,b,c)
        | Save -> 
            let state = !(document.state) in
            if state.document_name = test_doc_name then () else
            save (path dir state.document_name) state.text
        | OpenDocument (_, username) ->
            let state = !(document.state) in

            (* Load the document for the user that joined by sending fake events *)
            (* Have a fake user join, "create" the document, and leave *)
            enqueue_user @@ UserJoins { user="god"; cursor=0; color=Black };
            enqueue_user @@ ReplaceText (0,0,0,remove_trailing_newline state.text);
            enqueue_user @@ UserLeaves 0;

            (* Have all previous users "join".
                The new user is not yet in document.state.per_user *)
            let user_joins u =
                enqueue_user @@ UserJoins u in
            List.iter user_joins state.per_user;

            (* Now announce the new user that just joined *)
            let color = free_color state in
            enqueue_all @@ UserJoins { user=username; cursor=0; color=color };
            enqueue_user @@ SetUser user.uid (* Our logic is such that this is the same as the computed value *) 
        | Disconnect ->
            enqueue_all @@ UserLeaves user.uid
    in

    if debug then (
        print_newline ();
        print_string "user: ";
        print_endline @@ Debug.string_of_user user;
        print_string "actions: ";
        print_endline @@ string_of_list Debug.string_of_remote_action @@ actions
        );

    (* Process each action *)
    List.iter process_action actions;
    enqueue_user Unlock;

    if debug then (
        print_string "q_user: ";
        print_endline @@ string_of_list Debug.string_of_receive_action @@ list_of_queue q_user;
        print_string "q_everyone: ";
        print_endline @@ string_of_list Debug.string_of_receive_action @@ list_of_queue q_everyone
        );

    (* Update the server copy of the document *)
    let apply1 state action = fst @@ apply_remote_action state action in
    document.state := Queue.fold apply1 !(document.state) q_everyone;

    (* Send out messages to users *)
    flush_all ()

module StringMap = Map.Make(String)
type server_args = {
    dir: string;
    socket: string;
    debug: bool;
}

let server_main (server_args: server_args) (on_ready: unit->unit) : unit =
    (* If the client exits, we should not exit *)
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore; 

    (* Partially evaluate top-level stuff *)
    let process_actions = process_actions server_args.dir server_args.debug in

    (* Listen on a socket for client connections *)
    let new_connections : connection Input.t =
        Input.of_listening_socket server_args.socket |> Input.map (function
            | (inp, out) -> {inp; out}) in

    on_ready ();

    let all_users : user list ref = ref [] and
        unauthed_connections : connection list ref = ref [] and
        documents = ref StringMap.empty in

    let get_document docname : document =
        documents := StringMap.update docname (function
            | Some x -> Some x
            | None -> 
                if docname = test_doc_name then Some { state=ref testing_document; users=ref [] }
                else
                    let text = Option.value (load (path server_args.dir docname)) ~default:"\n" in
                    let state = { text; per_user=[]; document_name=docname } in
                    Some { state=ref state; users=ref [] }
        ) !documents;
        StringMap.find docname !documents in

    let auth_user (conn: connection) (actions: send_remote_action list) : user option =
        match actions with
        | OpenDocument (document_name, name) :: _ ->
            let document = get_document document_name in
            let uid = List.length !(document.state).per_user in
            let user = { conn; document; uid; name } in
            document.users := user :: !(document.users);
            Some user
        | _ -> None in

    let remove_conn conn = unauthed_connections := remove conn !unauthed_connections in (* TODO: Also close pipe? *)
    let add_user user = all_users := user :: !all_users in
    let remove_user user = 
        all_users := remove user !all_users;
        user.document.users := remove user !(user.document.users) in

    while true do
        Input.select (
            [ Input.handle new_connections; ] @
            (List.map (fun conn -> Input.handle conn.inp) !unauthed_connections) @
            (List.map (fun user -> Input.handle user.conn.inp) !all_users)
        );

        (match Input.read new_connections with
            | Some conn -> unauthed_connections := conn :: !unauthed_connections
            | NotReady -> ()
            | Closed -> exit 0
        );

        !unauthed_connections |> List.iter (function conn ->
            match Input.read conn.inp with
                | Some actions ->
                    remove_conn conn;
                    (match auth_user conn actions with
                        | None -> ()
                        | Some user ->
                            add_user user;
                            process_actions user actions)
                | NotReady -> ()
                | Closed -> remove_conn conn
        );

        !all_users |> List.iter (function user ->
            match Input.read user.conn.inp with
                | Some actions -> process_actions user actions
                | NotReady -> ()
                | Closed ->
                    process_actions user [Disconnect];
                    remove_user user
        )
    done

type run_mode = Client | Server | StandAlone
type cli_args = {
    mode: run_mode;
    client_args: client_args;
    server_args: server_args;
}
    
let parse_args () : cli_args =
    let usage_msg = "text [--debug] [--dir DIR] [--stand-alone|--server|--client] FILE" and
        debug = ref false and
        mode = ref StandAlone and
        dir = ref None and
        user = ref None and
        files = ref [] in
    let anon_fun filename = files := filename :: !files and
        set_mode m = (Arg.Unit (fun () -> mode := m)) and
        set_option_string r = (Arg.String (fun s -> r := Some s)) in
    let speclist = [
        ("--debug", Arg.Set debug, "Make the screen small (6x4) and turn off screen refresh");
        ("--stand-alone", set_mode StandAlone, "Run a stand-alone server to edit files with just one user (meant for testing only)");
        ("--client", set_mode Client, "Connect to an existing server (the default)");
        ("--server", set_mode Server, "Run a server to edit files. Files will be owned by the server uid.");
        ("--name", set_option_string user, "Your display. [default: username]");
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
    let user = Option.value (!user) ~default:(Unix.getuid () |> Unix.getpwuid).pw_name |> truncate 12 in 
    let socket = if !mode = StandAlone then "textmu.socket" else "/tmp/textmu.socket" in
    let dir = get_dir !mode !dir in
    let only_file = match (List.length !files) with
        | 0 -> test_doc_name
        | 1 -> List.hd !files
        | _ -> Arg.usage speclist usage_msg; exit 3 in
    { 
        mode = !mode; 
        client_args = { file = only_file; debug = !debug; user; socket };
        server_args = { dir = Option.value dir ~default: ""; socket; debug=(Server = !mode) }
    }

let cvar () : (unit -> unit) * (unit -> unit) =
    let flag = Atomic.make false and
        cv = Condition.create () and
        m = Mutex.create () in
    Mutex.lock m;

    let set_ready () =
            Atomic.set flag true;
            Condition.broadcast cv and
        wait_until_ready () =
            while not @@ Atomic.get flag do
                Condition.wait cv m
            done
    in (set_ready, wait_until_ready)
        
let crash = Atomic.make None
let enable_error_reporting () =
    at_exit (fun () -> 
        Out_channel.flush stdout;
        match Atomic.get crash with 
        | Some msg -> Out_channel.output_string stderr msg
        | _ -> () 
    )
let fail_catastrophically (f: 'a -> 'b) (x: 'a) : 'b =
    Printexc.record_backtrace true; (* Fun fact: this is a per-domain setting *)
    try f x with
        | e ->
            Atomic.set crash (Printf.sprintf "%s\n%s\n"
                (Printexc.to_string e)
                (Printexc.get_backtrace ()) |> Option.some);
            exit 2

let main () : unit =
    enable_error_reporting ();

    let args = parse_args () in
    match args.mode with
    | Client -> fail_catastrophically client_main args.client_args
    | Server -> fail_catastrophically (server_main args.server_args) ignore
    | StandAlone ->
        let (set_ready, wait_until_ready) = cvar () in
        let server = Domain.spawn
            (fun () -> fail_catastrophically (server_main args.server_args) set_ready)
        in
        wait_until_ready ();
        fail_catastrophically client_main args.client_args;
        Domain.join server
