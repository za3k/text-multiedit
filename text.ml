(* Minimal text editor. Designed to support multiple users on one machine.
    
    (?) Standard 80 col max width, and use for justify?

    User Color Guide (shown at bottom)
    Shortcuts (shown at bottom)

    Actions
        Insert text at position X
        Delete text at position X
        Move cursor to position X
        Cut at position X

    Limitations:
        Undo/redo not supported in v1 (may require semantic actions)
        Explicit save required
        ASCII only
*)

(* TODO LIST
    [ ] Make each shortcut work, one by one
    [ ] Improve the display to look like the real thing

    [ ] Deal with terminal resizing
    [ ] Make the server stub fully realistic
    [ ] Add the view
        [ ] Add word wrap
    [ ] Add keyboard locking/unlocking

    | Left -> "<Left>"
    | Right -> "<Right>"
    | Del -> "<Del>"

    | Backspace -> "<Backspace>"
    | Cut -> "<Cut>"
    | Down -> "<Down>"
    | End -> "<End>"
    | Exit -> "<Exit>"
    | Help -> "<Help>"
    | Home -> "<Home>"
    | Justify -> "<Justify>"
    | PageDown -> "<PageDown>"
    | PageUp -> "<PageUp>"
    | Paste -> "<Paste>"
    | Save -> "<Save>"
    | ScrollDown -> "<ScrollDown>"
    | ScrollUp -> "<ScrollUp>"
    | Tab -> "<Tab>"
    | Up -> "<Up>"
    | Key c -> Printf.sprintf "<Key '%s'>" (String.escaped (String.make 1 c))
*)

(* CLIENT LOGIC *)
(* Setup:
    Parse command line. Set username, filename, and view-only flag.
    Set up initial data structures.
    Connect to the server via a unix socket, /tmp/ocaml-text
    Send "Open <document> as <name> [, view-only please]" to the server.
        The server sends back the current text and cursor positions of other connected users, creating the document if needed.
*)

type background_color = 
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White 
    | BrightBlack | BrightRed | BrightGreen | BrightYellow | BrightBlue | BrightMagenta | BrightCyan | BrightWhite
    | Default
let color_code (color: background_color) : string = match color with
    | Default -> "0m"
    | Black -> "40m" | Red -> "41m" | Green -> "42m" | Yellow -> "43m" | Blue -> "44m" | Magenta -> "45m" | Cyan -> "46m" | White -> "47m"
    | BrightBlack -> "100m" | BrightRed -> "101m" | BrightGreen -> "102m" | BrightYellow -> "103m" | BrightBlue -> "104m" | BrightMagenta -> "105m" | BrightCyan -> "106m" | BrightWhite -> "107m"

type user_state = { user: string; cursor: int; color: background_color; }
type state = { text: string; document_name: string; per_user: user_state list }
let state = {
    text="Hello, world.\nThis is the second line.\n"; (* Invariant: every file has a newline at the end *)
    document_name="test_file.txt";
    per_user = [
        { user="zachary"; cursor=2; color=Red;  };
        { user="editor2"; cursor=13; color=Blue; };
    ]
}

type view = { start: int } (* Should be the start of a physical line *)
type terminal_size = { rows: int; cols: int }
type local_state = { view: view; move_since_cut: bool; clipboard: string; uid: int option; terminal_size: terminal_size }
let local_state = {
    uid = Some 0;
    view = { start=0 };
    move_since_cut = true;
    terminal_size = { rows=80; cols=25 };
    clipboard = "";
}

let join_with sep = function
    | [] -> ""
    | first :: l -> List.fold_left (fun s1 s2 -> (Printf.sprintf "%s%s%s" s1 sep s2)) first l
let join_with_spaces = join_with " "
let clamp (x1:int) (x2:int) (x:int) : int = min (max x x1) x2

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
type button = 
  | Backspace | Cut | Del | Down | End | Exit | Help | Home | Justify | Left | PageDown | PageUp | Paste | Right | Save | ScrollDown | ScrollUp | Tab | Up
  | Key of char
  | Unknown of string
let string_of_button = function
    | Backspace -> "<Backspace>"
    | Cut -> "<Cut>"
    | Del -> "<Del>"
    | Down -> "<Down>"
    | End -> "<End>"
    | Exit -> "<Exit>"
    | Help -> "<Help>"
    | Home -> "<Home>"
    | Justify -> "<Justify>"
    | Left -> "<Left>"
    | PageDown -> "<PageDown>"
    | PageUp -> "<PageUp>"
    | Paste -> "<Paste>"
    | Right -> "<Right>"
    | Save -> "<Save>"
    | ScrollDown -> "<ScrollDown>"
    | ScrollUp -> "<ScrollUp>"
    | Tab -> "<Tab>"
    | Up -> "<Up>"
    | Key c -> Printf.sprintf "<Key '%s'>" (String.escaped (String.make 1 c))
    | Unknown s -> Printf.sprintf "<Unknown \"%s\">" (String.escaped s)
let printable_ascii = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ " (* \n and \t are missing on purpose *)
let is_printable_ascii keystroke = String.length keystroke = 1 
    && String.get keystroke 0 |> String.contains printable_ascii
let shortcuts = [
    (["\b"], None, None, Backspace);
    (["\011"], Some "^K", Some "Cut", Cut);
    (["\027[3~"], None, None, Del); 
    (["\027[B"], None, None, Down);
    (["\027[4~"; "\027[F"], None, None, End);
    (["\r"], None, None, Key '\n'); (* Enter *)
    (["\024"; "\022"], Some "C-x", Some "Exit", Exit); (* Ctrl-X or Ctrl-C *)
    (["\007"], Some "C-g", Some "Help", Help); (* Ctrl-G because Ctrl-H is backspace *)
    (["\027[1~"; "\027[H"], None, None, Home);
    (["\n"], Some "C-j", Some "Justify", Justify); (* Requires ~ICRNL terminal setting to tell apart from enter *)
    (["\027[D"], None, None, Left);
    (["\027[6~"], None, None, PageDown);
    (["\027[5~"], None, None, PageUp);
    (["\021"], Some "^U", Some "Paste", Paste);
    (["\027[C"], None, None, Right);
    (["\019"], Some "C-s", Some "Save", Save); (* Requires ~IXON terminal setting *)
    (["\027[1;3B"], None, None, ScrollDown); (* Alt-Down *)
    (["\027[1;3A"], None, None, ScrollUp); (* Alt-Up *)
    (["\t"], None, None, Tab);
    (["\027[A"], None, None, Up);
]

let rec lookup_shortcut keystroke = function
    | (ks, _, _, action) :: _ when List.mem keystroke ks -> Some action
    | _ :: l -> lookup_shortcut keystroke l
    | [] -> None

let get_button keystroke =
    if is_printable_ascii keystroke then Key (String.get keystroke 0)
    else match lookup_shortcut keystroke shortcuts with
        | Some action -> action
        | None -> Unknown keystroke
    

(* Step 3: Compute the parameterized action (ex, move cursor from where to where) *)
type send_action =
    (* Remote only *)
    | Save 
    (* Remote, will be sent back as local *)
    | OpenDocument of string * string
    | ReplaceText of int * int * string
    (* Local only *)
    | CopyText of string
    | CutFlag of bool
    | DisplayError of string
    | Exit
let is_local = function
    | CopyText _ | CutFlag _ | DisplayError _ | Exit -> true
    | _ -> false
let local_only = List.filter is_local
let remote_only = List.filter (function x -> not (is_local x))
let string_of_send_action = function
    | Save -> "<Save>"
    | OpenDocument (u, d) -> Printf.sprintf "<OpenDocument \"%s\" \"%s\">" (String.escaped u) (String.escaped d)
    | ReplaceText (start, len, s) -> Printf.sprintf "<ReplaceText %d %d \"%s\">" start len (String.escaped s)
    | CopyText s -> Printf.sprintf "<CopyText \"%s\">" (String.escaped s)
    | CutFlag f -> Printf.sprintf "<CutFlag %s>" (Bool.to_string f)
    | DisplayError s -> Printf.sprintf "<DisplayError \"%s\">" (String.escaped s)
    | Exit -> "<Exit>"

let insert offset str = [ReplaceText (offset, 0, str)]
let delete offset len = [ReplaceText (offset, len, "")]
let move_cursor offset = [ReplaceText (offset, 0, "")]
let cut append_move clipboard text pos line_start line_end = 
    let new_text = String.sub text line_start (line_end-line_start) in
    let clipboard = if append_move then clipboard ^ new_text else new_text in
    [
        CopyText clipboard;
        ReplaceText (line_start-pos, line_end-line_start, "");
        CutFlag true
    ]


(*
pos = get_cursor state local_state.username
*)

(* type local_position = { pos: int; physical_line: int; col: int } (* 0-indexed *) *)

let string_count (s:string) (c: char) : int =
    String.fold_left (fun acc x -> if x = c then acc + 1 else acc) 0 s

let nth_char (c: char) (s: string) (n: int) : int = 
    (* [nth_char s c n] is the index of the n-th copy of [c] in [s].
    Raises [Not_found] if there are less than [n] copies of [c] in [s]. *)
    let rec helper (c: char) (s: string) (offset: int) : int->int = function
        | 0 -> offset
        | n -> helper c s (String.index_from s offset c) (n-1)
    in helper c s 0 n
let nth_line = nth_char '\n'

let line_of (text : string) (pos : int) : int * int =
    (* [line_of text pos] is the line and column number of the [pos]-th byte in [text].
    All indices are from 0.*)
    let rec helper (text: string) (pos: int) (lines_before_offset: int) (offset: int)=
        (* Invariant: offset is first character after a newline OR first char in file *)
        match String.index_from_opt text offset '\n' with
            | Some i when i < pos -> helper text pos (lines_before_offset+1) (i+1)
            | _ -> (lines_before_offset, pos-offset)
    in helper text (clamp 0 ((String.length text)-1) pos) 0 0

let line_for (text : string) (pos : int) : int * int =
    (* [line_for text pos] is the start and end of the line containing the [pos]-th byte in [text]. *)
    let (line_num, col) = line_of text pos in
    let start = if line_num = 0 then 0 else (nth_line text (line_num-1)) in
    let end_line = nth_line text (line_num+1) in
    (start, end_line)

let pos_of (text: string) (line: int) (col: int) : int =
    (* [pos_of text line col] is the byte index of the [col]-th byte in the [line]-th line of [text].
    All indices are from 0.*)
    let max_line_num = (string_count text '\n') in
    col + nth_line text (clamp 0 max_line_num line)

let page_lines = 10 (* TODO: Make depend on the terminal height *)
let compute_actions state button = 
    let text = state.text in
    let pos = (List.nth state.per_user (Option.get local_state.uid)).cursor in (* TODO: Can throw *)
    let (physical_line, col) = line_of text pos in
    let clipboard = local_state.clipboard in
    let move_since_cut = local_state.move_since_cut in
    let (first, last) = line_for text pos in 
    let actions = match button with
    | Backspace -> if pos > 0 then delete (-1) 1 else []
    | Del -> delete 0 1
    | Cut -> cut (not move_since_cut) clipboard text pos first last
    | Down -> move_cursor ((pos_of text (physical_line+1) col)-pos)
    | End -> move_cursor (last-pos)
    | Exit -> [Exit]
    | Help -> [] (* TODO *)
    | Home -> move_cursor (first-pos)
    | Justify -> [] (* TODO *)
    | Left -> move_cursor (-1)
    | PageDown -> move_cursor ((pos_of text (physical_line+page_lines) col)-pos)
    | PageUp -> move_cursor ((pos_of text (physical_line-page_lines) col)-pos)
    | Paste -> insert 0 clipboard
    | Right -> move_cursor 1
    | Save -> [Save]
    | ScrollDown -> [] (* TODO: Affect the view, not the cursor *)
    | ScrollUp -> []
    | Tab -> insert 0 "    "
    | Up -> move_cursor ((pos_of text (physical_line-1) col)-pos)
    | Key c -> insert 0 (String.make 1 c)
    | Unknown s -> [DisplayError (Printf.sprintf "Unknown key pressed: %s" s)]
    in let actions = actions @ match button with
    | Cut -> []
    | _ -> [CutFlag false] 
    in actions

(* Step 4: Apply local state changes *)

(* TODO: Implement *)
let apply_local_action (state: state) (local_state: local_state) (action: send_action): local_state =
    (*print_endline (Printf.sprintf "ApplyingLocal: %s" (string_of_send_action action));*)
    local_state

(* Step 5: Send the action to the server *)
(* Step 6: Receive an action from the server *)
type receive_action =  
    | ReplaceText of int * int * int * string
    | UserLeaves of int
    | UserJoins of user_state
    | SetUser of int
    (* TODO: Lock and unlock user input *)
let string_of_receive_action = function
    | ReplaceText (uid, a, b, s) -> Printf.sprintf "ReplaceText[u=%d,%d,%d,\"%s\"]" uid a b (String.escaped s)
    | UserLeaves uid -> Printf.sprintf "UserLeaves[u=%d]" uid
    | UserJoins { user } -> Printf.sprintf "UserJoins[un=%s,...]" user
    | SetUser uid -> Printf.sprintf "SetUser[u=%d]" uid

let server_stub (uid : int option): (send_action -> receive_action list) = function
    | ReplaceText (a,b,c) -> (match uid with
        | Some uid -> [ReplaceText (uid,a,b,c)]
        | None -> [] )
    | OpenDocument (document, username) -> [UserJoins { user=username; cursor=0; color=Red }; SetUser 0] (* TODO *)
    | Save -> [] (* Doesn't happen in stub *)
    | _ -> [] (* Local events are never sent *)

(* Step 7: Transform the state based on the action
    - Text
    - Cursor position (per-user)
    - User list
    VIEW is not included in the state, but consists of a start position only, which is always at the beginning of a line.
*)

(* TODO: Implement *)
let apply_remote_action (combined_state: state * local_state) (action: receive_action) : state * local_state = 
    (*print_endline (Printf.sprintf "ApplyingRemote: %s" (string_of_receive_action action));*)
    let (state, local_state) = combined_state in
    match action with
    | ReplaceText (ed_uid, start, length, replacement) ->
        let text_length = String.length state.text in
        let user = List.nth state.per_user ed_uid in
        let pos = user.cursor + start in (* Clip removed part to bounds of the document *)
        let pos = min (max 0 pos) (text_length - 1) in
        let length = min length (text_length - 1 - pos) in
        let net_change = (String.length replacement) - length in
        let new_text = (String.sub state.text 0 pos) ^ replacement ^ (String.sub state.text (pos + length) (text_length - pos - length)) in
        (*             state.text[0:pos]             + replacement + state.text[pos+length:] *)
        let new_users = List.mapi (fun (uid: int) (user: user_state) ->
            if uid = ed_uid then { user with cursor = pos }
            else if user.cursor <= pos then user
            else { user with cursor = user.cursor + net_change }
        ) state.per_user in
        let state = { state with per_user = new_users; text = new_text } in
        (* TODO: Update view *)
        (state, local_state)
        (* 
            1. Change the text
            2. Move the editor's cursor
            3. Shift user cursors and view
            4. Update view if the user's cursor is off screen
               TODO: The view should be adjusted if the cursor is off screen, either here or elsewhere
        *)
    | UserLeaves uid -> (state, local_state)
    | UserJoins user_state -> (state, local_state)
    | SetUser uid -> (state, local_state)

(* Step 8: Update the UI
    8a: Calculate the view, based on the last view, the current terminal size, and the cursor position.
    8b: Display the text (or the help)
    8c: Display the active users
    8d: Display the shortcuts
*)

(* TODO: Implement actual version *)

let rec any (l : 'a option list) : 'a option = match l with
    | [] -> None
    | Some x :: _ -> Some x
    | None :: l -> any l

let rec lookup_cursor_color (users: user_state list) (pos: int) : background_color option = 
    (* { user="zachary"; cursor=2; color=Red;  }; *)
    List.map (function x -> match x with
        | { cursor; color } when cursor = pos -> Some color
        | _ -> None
    ) users |> any

let colorize (s: string) (color: background_color) : string =
    "\027[" ^ (color_code color) ^ s ^ "\027[0m"
        
let display_document (text: string) (color_of: int -> background_color option) : string =
    List.mapi (fun (index: int) (c: char) -> let s = String.make 1 c in
        let color = color_of index in
        match color with
            | Some color -> if c ='\n' then colorize " " color ^ "\n" else colorize s color
            | None -> s
    ) (text |> String.to_seq |> List.of_seq) |> String.concat ""

let display (state: state) (local_state: local_state) : unit =
    let color_cursor = lookup_cursor_color state.per_user in
    print_endline (display_document state.text color_cursor)

(* [Go to step 1] *)
(* Teardown:
    Send "Disconnect" to the server.
*)

(* (Unix.getuid () |> Unix.getpwuid).pw_name; *)

(* In a loop, listen for any of:
    - Messages from the server
    - Terminal resize events (SIGWINCH)
    - User keyboard input
*)
let local_state = ref local_state
let state = ref state
let () = while true do
    let keystroke = get_keystroke() in
        print_string (Printf.sprintf "\"%s\" || " (String.escaped keystroke));
    let button = get_button keystroke in
        string_of_button button |> print_string; print_string " || ";
    let actions = compute_actions !state button in
        print_endline (join_with "" (List.map string_of_send_action actions));
        (*print_string "Local: "; print_endline (join_with "" (List.map string_of_send_action (local_only actions)));*)
    local_state := List.fold_left (apply_local_action !state) !local_state (local_only actions);
        (*print_string "Sent to server: "; print_endline (join_with "" (List.map string_of_send_action (remote_only actions)));*)
    let msgs = List.concat (List.map (server_stub (Some 0)) (remote_only actions)) in
        (*print_string "Received from server: "; print_endline (join_with "" (List.map string_of_receive_action msgs));*)
    let (s, ls) = List.fold_left apply_remote_action (!state, !local_state) msgs in
    state := s;
    local_state := ls;
    display !state !local_state
done

(* SERVER LOGIC *)
(* Setup:
    Listen to unix socket /tmp/ocaml-text
    On connect, listen for first command. Should always be "Open <docuemnt> as <name>". Tag the connection with document and name. Put into the list of connections for that document, setting up the document if neded.
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
    Locate the document in the list of documents. If it doesn't exist, set up the document.
    Send+apply: "<user> connected and their cursor is at position 0" action
*)
(* Connection Action received:
    Connect/Disconnect: See above/below
    Save: Save the document
    Else: Send+apply. Send the message to everyone connected, and apply it to the server's state model.
*)
(* Per Connection Disconnect 
    Send+apply: "<user> disconnected" action
    If there are zero users connected, save the document and remove it from the documents list.
*)


