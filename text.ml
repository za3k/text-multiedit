(* Minimal text editor. Designed to support multiple users on one machine.
    
    (?) Standard 80 col max width, and use for justify?

    User Color Guide (shown at bottom)
    Shortcuts (shown at bottom)

    Limitations:
        Undo/redo not supported in v1 (may require semantic actions)
        Explicit save required
        ASCII only, no Unicode
*)

(* TODO LIST
    [x] Make each shortcut work, one by one
        [x] Make sure stuff works on an empty document
        [x] Make sure stuff works with empty lines
        [x] Make sure stuff works on "\n\n" document
        [ ] Up/down should remember the "imaginary" column off the right end
            the cursor is on until the user types or presses left/right
    [ ] Parse command-line arguments
        [ ] --debug (old-style display printing, with 6x4 size screen and hardcoded initial state)
        [ ] -1, --stand-alone (run both client and server together)
        [ ] --server (run in server mode)
        [ ] --client (run in client mode, the default)
        [ ] --dir (directory to look files, default to /var/text in server mode, or current dir in single-user mode)
        [ ] filename
    [ ] Improve the display to look like the real thing
        [ ] Add a help line
        [ ] Add the line of users
        [ ] Add an error message display
        [ ] Clean the screen on each update
        [ ] Reset the screen on_exit
    [ ] Make the server stub fully realistic
        [ ] Add document setup
        [ ] Add file load+save
        [ ] Add message queues for each user
    [x] Add the view
        [x] Make the view move with the user cursor
        [x] Make the view shift on inserts/deletes
        [x] Add line wrap: min(80, term-width)
        [x] Add ScrollDown/ScrollUp
    [ ] Add networking (real client/server)
        [ ] Listen to first of (network, keyboard, SIGWINCH)
        [ ] Add keyboard locking/unlocking, Don't listen to keyboard while
            locked

    FUTURE FEATURES
    [ ] Read-only mode
    [ ] Line numbers, go to line
    [ ] Built-in help
    [ ] Justify
    [ ] Auto-save
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

let max_rows = 80

type background_color = 
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White 
    | BrightBlack | BrightRed | BrightGreen | BrightYellow | BrightBlue | BrightMagenta | BrightCyan | BrightWhite
    | Default
let color_code : background_color -> string = function
    | Default -> "0m"
    | Black -> "40m" | Red -> "41m" | Green -> "42m" | Yellow -> "43m" | Blue -> "44m" | Magenta -> "45m" | Cyan -> "46m" | White -> "47m"
    | BrightBlack -> "100m" | BrightRed -> "101m" | BrightGreen -> "102m" | BrightYellow -> "103m" | BrightBlue -> "104m" | BrightMagenta -> "105m" | BrightCyan -> "106m" | BrightWhite -> "107m"

type user_state = { user: string; cursor: int; color: background_color; }
type state = { text: string; document_name: string; per_user: user_state list }
let init_state = {
    text="Hello, world.\nThis is the second line.\nThis is the third line.\nThis is the fourth line\nThis is the fifth line\n"; (* Invariant: every file has a newline at the end *)
    document_name="test_file.txt";
    per_user = [
        { user="zachary"; cursor=2; color=Red;  };
        { user="editor2"; cursor=11; color=BrightMagenta; };
    ]
}

type view = int (* Should be the start of a s-line after any view adjustment *)
type terminal_size = { rows: int; cols: int }
type local_state = { view: view; move_since_cut: bool; clipboard: string; uid: int option; terminal_size: terminal_size; error: string option; locked: bool }
let init_local_state = {
    uid = Some 0;
    view = 7;
    move_since_cut = true;
    (*terminal_size = { rows=80; cols=25 };*)
    terminal_size = { rows=7; cols=6 };
    clipboard = "";
    locked = false;
    error = None;
}

let clamp (x1:int) (x2:int) (x:int) : int = min (max x x1) x2
let remove1 (i: int) : 'a list -> 'a list = List.filteri (fun j x -> j <> i)
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
    (["\024"; "\022"], Some "C-x", Some "Exit", Exit); (* Ctrl-X or Ctrl-C. Note that Ctrl-C just exits with SIGINT currently. *)
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
    | DisplayError of string option
    | Exit
    | Lock
    | ShiftView of int
let is_local = function
    | CopyText _ | CutFlag _ | DisplayError _  | Exit | Lock | ShiftView _ -> true
    | _ -> false
let is_remote = Fun.compose not is_local
let local_only = List.filter is_local
let remote_only = List.filter is_remote
let string_of_send_action = function
    | Save -> "<Save>"
    | OpenDocument (u, d) -> Printf.sprintf "<OpenDocument \"%s\" \"%s\">" (String.escaped u) (String.escaped d)
    | ReplaceText (start, len, s) -> Printf.sprintf "<ReplaceText %d %d \"%s\">" start len (String.escaped s)
    | CopyText s -> Printf.sprintf "<CopyText \"%s\">" (String.escaped s)
    | CutFlag f -> Printf.sprintf "<CutFlag %s>" (Bool.to_string f)
    | DisplayError None -> "<DisplayError OK>"
    | DisplayError Some s -> Printf.sprintf "<DisplayError \"%s\">" (String.escaped s)
    | ShiftView n -> Printf.sprintf "<ShiftView %d>" n
    | Lock -> "<Lock>"
    | Exit -> "<Exit>"

let insert offset str = [ReplaceText (offset, 0, str)]
let delete offset len = [ReplaceText (offset, len, "")]
let move_cursor offset = if offset = 0 then [] else [ReplaceText (offset, 0, "")]
let move_view offset = if offset = 0 then [] else [ShiftView offset]
let cut append_move clipboard text pos line_start line_end = 
    let new_text = String.sub text line_start (line_end-line_start+1) in
    let clipboard = if append_move then clipboard ^ new_text else new_text in
    [
        CopyText clipboard;
        ReplaceText (line_start-pos, line_end-line_start+1, "");
        CutFlag true
    ]


(* Three Views of a Document
*  =========================
*
* A document is an immutable string. All documents end in a newline (and 
* therefore are at least 1 character long).
* 
* Cursors always point at a character, never between characters. Cursors can
* validly point at any character in the document, including newlines. They
* cannot go past the beginning/end character in a document.
* 
* Example (newlines written as 'N')
* 
*     ==POS view==
*     0 0123456789
*     1           012345
*      "Line 0NLine 1NNN"
*         ^          ^^^
* 
*     ==LINE/COL view==
*     Line 0N
*       ^     This is line 0, col 2 == pos 2
*     Line 1N
*           ^ This is line 1, col 6 == pos 13
*     N
*     ^       This is line 2, col 0 == pos 14
*     N
*     ^       This is line 3, col 0 == pos 15
*
*     ==S-LINE/S-COL view (width 3)==
*     Lin
*       ^       s-line 0, col 2
*     e 0       
*
*     N
*
*     Lin
*
*     e 1
*
*     N         s-line 5, col 0
*     ^
*     N         s-line 6, col 0
*     ^
*     N         s-line 7, col 0
*     ^
* 
* The three views are:
* 
*  - the POS view           [POS]2
*     - POS counts characters from start of document.
*     - POS 0 is the first character in the document.
*  - the LINE/COL view:     L0C2    
*     - LINE/COL view is based on "physical" lines within the document based
*       only on newline characters. It doesn't depend on word wrap, terminal
*       size, or viewport position.
*     - LINE 0 is the first line in a document, and LINE 0, COL 0 is the first
*       character in a document.
*     - The newline is considered the last character in each line. Remember
*       that the last line in a document also ends in a newline.
*     - LINE N, COL M is the same as POS(Nth newline position + M+1 more chars)
*  - the SLINE/SCOL view:   0      + SL 0 + SC 2
*                           L0[C0] + SL 0 + SC 2
*     - the SLINE/SCOL view is based on "screen" lines, taking into account
*       word wrap but not the viewport. 
*       In other words, how would the document look if it was wrapped to 80
*       columns, but infinitely tall?
*     - While you could count S-LINE from the top of the document, this means
*       that a pointer to (ex.) SLINE 50 would not the be same across users,
*       would become invalid if the terminal width changed, and generally is
*       not very stable in the face of editing.
*       Therefore, we store as deltas from something more stable:
*         - Given a POS, we can treat it as an s-line containing that POS value
*           [ignore or zero the s-col value]. 
*           This is how we store where the top of the screen should be ('view')
*         - Given two POS values, what is the delta of S-LINES and S-COLS
*           between them?         
*           0      + SL 0 + SC 2
*         - Given a LINE and a POS, what is the delta of S-LINES and S-COLS
*           between them?       
*           L0[C0] + SL 0 + SC 2
*       Additionally, we avoid storing even these deltas anywhere permanent.
* 
* POS should always be between 0 and LEN-1 (inclusive) -- 0 to 15 in the example
* POS functions accept values outside this range but clip them. Returned values
* will be in this range.
*     Exception: For convenience, [nth_line_start NEWLINES] returns [LEN],
*     which is not a valid input LINE or a valid output POS
* 
* LINE should be between 0 and NEWLINES-1 (inclusive) -- 0 to 3 in the example
* COL should be between 0 and the length of the line (excluding any newline
* before the line, including the one at the end of the line). In example line
* 1, COL could range from 0 to 6.
* LINE/COL functions accept values outside these ranges and clip them. Returned
* values will be in these ranges.
*)

let document_end (s: string) : int = (String.length s)-1
let document_clamp (s: string) : int->int = clamp 0 (document_end s)

let string_count (s:string) (c: char) : int =
    String.fold_left (fun acc x -> if x = c then acc + 1 else acc) 0 s

let nth_char (c: char) (s: string) (n: int) : int = 
    (* [nth_char s c n] is the index of the n-th copy of [c] in [s].
    Raises [Not_found] if there are less than [n] copies of [c] in [s]. *)
    if n <= 0 then 0 else
    let rec helper (offset: int) : int->int = function
        | 0 -> offset
        | n -> helper (String.index_from s (offset+1) c) (n-1)
    in helper ~-1 n
(* [nth_line_start] happily accepts one line past the end of the document, and
returns an index one past the end of the document. *)
let nth_line_start s n = 
    if n = 0 || (n = 1 && s = "\n") then 0
    else 1 + nth_char '\n' s n 

let line_of (text : string) (pos : int) : int * int =
    (* [line_of text pos] is the line and column number of the [pos]-th byte in
    [text].
    All indices are from 0.*)
    let rec helper (pos: int) (lines_before_offset: int) (offset: int) =
        (* Invariant: offset is first character after a newline OR first char
         * in file *)
        match String.index_from_opt text offset '\n' with
            | Some i when i < pos -> helper pos (lines_before_offset+1) (i+1)
            | _ -> (lines_before_offset, pos-offset)
    in helper (document_clamp text pos) 0 0

let line_for (text : string) (pos : int) : int * int =
    (* [line_for text pos] is the start and end of the line containing the
       [pos]-th byte in [text]. *)
    (* TODO: Optimize *)
    let (line_num, col) = line_of text pos in
    let start = nth_line_start text line_num in
    let end_line = ~-1 + nth_line_start text (line_num+1) in
    (start, end_line)

let pos_of (text: string) (line: int) (col: int) : int =
    (* [pos_of text line col] is the byte index of the [col]-th byte in the
    [line]-th line of [text].
    All indices are from 0.*)
    let max_line_num = (string_count text '\n')-1 in 
    match line with
        | n when n<0 -> 0
        | n when n>max_line_num -> document_end text
        | _ ->
            let line_start = nth_line_start text line in
            let next_line_start = nth_line_start text (line+1) in
            let line_length = next_line_start-line_start in (* includes trailing newline *)
            line_start + clamp 0 (line_length-1) col

(* S-line/s-col functions *)
type sline_delta = int * int

let wholeline_sline_index (width: int) (line_start: int) (pos: int) : int * int =
    (* [pos] is located within the s-line with index [wholeline_sline_index width
    line_start pos]. [line_start] is the index of the first character in the
    physical line containing [pos].
       The first s-line is sline 0. *)
    ((pos - line_start) / width, (pos - line_start) mod width)
let sline_index (width: int) (text: string) (pos: int) : int * int =
    let (line_start, _) = line_for text pos in
    wholeline_sline_index width line_start pos

let num_slines_in_line (width: int) (first: int) (last: int) : int = 
    (last - first) / width + 1
let num_slines_in_lines (width: int) (text: string) (first: int) (last: int) : int =
    (* Count the number of s-lines between [first] (at the beginning of a
    physical line)
       and [last]  (at the end of a physical line--some \n) *)
    let rec helper (first: int) (counted: int) : int =
        if first > last then counted else
        let (_, endl) = line_for text first in
        helper (endl+1) (counted + num_slines_in_line width first endl)
    in helper first 0

let sline_for (width: int) (text: string) (pos: int) : int * int =
    let (_, col) = sline_index width text pos
    and (_, line_end) = line_for text pos in

    let start = pos - col in
    let end_ = min (start + width - 1) line_end in
    (start, end_)
let sline_length (width: int) (text: string) (pos: int) : int =
    let (s,e) = sline_for width text pos in
    e-s+1

let rec sline_difference (width: int) (text: string) (pos1: int) (pos2: int) : sline_delta =
    (* To go from [pos1] to [pos2], how many rows and columns must you add? *)
    if (pos1 > pos2) then let (a, b) = 
        (sline_difference width text pos2 pos1) in (-a, -b) else
    let (start1, end1) = line_for text pos1
    and (start2, end2) = line_for text pos2 in
    let total_slines = num_slines_in_lines width text start1 end2
    and line2slines  = num_slines_in_lines width text start2 end2 in
    let (pos1sline, pos1col) = wholeline_sline_index width start1 pos1
    and (pos2sline, pos2col) = wholeline_sline_index width start2 pos2 in
    (*
    --o++                               -- = A = pos1sline[2]
    +++++
    +++++
    +o--                                -- = B = line2slines[4] - pos2sline[1] - 1

                -- o ++..++ o -- = total_slines[5+5+5+4]
    (subtract)  -- o        o -- = A + 1 + B
    (equals)         ++..++ o    = difference 
                                 = total_slines - A - B - 1
                                 = total_slines - pos1sline - line2slines + pos2sline

    --o+++o--- (one-line case)   = pos2sline - pos1sline
                                   (total_slines = line2slines)
                                 = total_slines - pos1sline - line2slines + pos2sline

    Note: if both ends are in the same sline, the result should be 0, which is not really
    covered by the above logic (but 
    *)
    (total_slines - pos1sline - (line2slines - pos2sline), pos2col - pos1col)

let sline_add_whole (width: int) (text: string) (sline_start: int) (delta_slines1: int) : int =
    (* Given [sline_start] which is the start of some s-line, move an integer
    number of s-lines in either direction. Clip to the document boundries.
    If the result is off the top of the document, [0] is returned.
    If the result is off the bottom of the document, [String.length text] is returned (one past the final valid index).
    *)
    let (start1, end1) = line_for text sline_start in
    let (sline_num, col) = sline_index width text sline_start in
    if col <> 0 then -999 else
    let rec helper pos delta_slines = (* POS is the start of a physical line *)
        if delta_slines < 0 then
            (* Move left 1 physical line from pos (or less) *)
            if pos = 0 then 0 else (* Don't go off the start of the document *)
            let (start1, end1) = line_for text (pos-1) in
            helper start1 (delta_slines + num_slines_in_line width start1 end1)
        else 
            (* Move right 1 physical line from pos (or less) *)
            let (start1, end1) = line_for text pos in
            let num_slines = num_slines_in_line width start1 end1 in
            if delta_slines < num_slines then pos + delta_slines * width
            else if end1 >= (document_end text) then (String.length text) (* Only go 1 off the end of the document *)
            else helper (end1+1) (delta_slines - num_slines)
    in helper start1 (delta_slines1 + sline_num)

let sline_add (width: int) (text: string) (pos: int) (delta: sline_delta) : int =
    (* Given the index [pos] into [text], move [delta] columns and rows from that index. The row is adjusted first, then the column.
    If the result is off the top of the document, [0] is returned.
    If the result is off the bottom of the document, [String.length text] is returned (one past the final valid index).
    If the result is off the left or right side of an s-line, it is silently clipped (not wrapped around).
    *)
    let (delta_slines, delta_scols) = delta
    and (start1, end1) = line_for text pos in
    let (_, scol1) = wholeline_sline_index width start1 pos
    and (sline_start, _) = sline_for width text pos in
    let final_sline_start = sline_add_whole width text sline_start delta_slines in
    if final_sline_start >= (String.length text) then final_sline_start else
    final_sline_start + clamp 0 ((sline_length width text final_sline_start) - 1) (delta_scols + scol1)

(* There is a constraint that the cursor should always be inside the viewport.
This is logic to deal with it. *)

let avail_height (terminal: terminal_size) : int = terminal.rows - 3
let avail_cols   (terminal: terminal_size) : int = min max_rows terminal.cols
type viewport = int * int

type cursor_bound = | OnScreen | OffTop of int | OffBottom of int
let string_of_cursor_bound = function 
    | OnScreen -> "OnScreen"
    | OffTop n -> Printf.sprintf "OffTop(%d)" n
    | OffBottom n -> Printf.sprintf "OffBottom(%d)" n

let viewport state local_state =
    let width = (avail_cols local_state.terminal_size)
    and height = (avail_height local_state.terminal_size)
    and text = state.text in
    let (view_start, _) = sline_for width text local_state.view in
    let view_end = sline_add_whole width text view_start height in
    (* view_end-1 works because sometimes view_end is 1 aft:r the end of the document *)
    (view_start, view_end-1) 
let in_viewport vp pos = pos >= (fst vp) && pos <= (snd vp)

let cursor_in_viewport (text: string) (terminal: terminal_size) (view) (cursor: int) : cursor_bound =
    match sline_difference (avail_cols terminal) text view cursor with
    | (n, _) when n < 0 -> OffTop n
    | (n, _) when n >= (avail_height terminal) -> OffBottom (n + 1 - (avail_height terminal))
    | _ -> OnScreen

let adjust_view_to_include_cursor (text: string) (terminal: terminal_size) (view: int) (cursor: int) : int =
    (* When the cursor moves, we need to scroll the view to include it. Return
    the new view. *)
    match cursor_in_viewport text terminal view cursor with
    | OnScreen -> view
    | OffTop n | OffBottom n -> sline_add (avail_cols terminal) text view (n, 0)

let adjust_cursor_to_be_visible (text: string) (terminal: terminal_size) (view: int) (cursor: int) : send_action list =
    (* When the view moves, we need to move the cursor to stay inside it.
    Return cursor movements as actions. *)
    let new_cursor = match cursor_in_viewport text terminal view cursor with
    | OnScreen -> cursor
    | OffTop n | OffBottom n -> sline_add (avail_cols terminal) text cursor (-n, 0)
    in move_cursor (new_cursor - cursor)

(* Functions such as PageUp/PageDown shift both the cursor and the view, and
even ScrollUp/ScrollDown can sometimes move the cursor *)

let shift_sline_action (state: state) (local_state: local_state) (slines: int) (pos: int) : int * int * int =
    if slines = 0 then (0,pos,0) else
    let width = avail_cols local_state.terminal_size in
    let new_pos = sline_add width state.text pos (slines, 0) in
    let (shifted_slines, _) = sline_difference width state.text pos new_pos in
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
    let (physical_line, col) = line_of text pos in
    let clipboard = local_state.clipboard in
    let move_since_cut = local_state.move_since_cut in
    let (first, last) = line_for text pos in 
    let actions = match button with
    | Backspace -> if pos > 0 then delete (-1) 1 else []
    | Del -> delete 0 1
    | Cut -> cut (not move_since_cut) clipboard text pos first last
    | Down -> move_cursor ((pos_of text (physical_line+1) col)-pos)
    | Up ->   move_cursor ((pos_of text (physical_line-1) col)-pos)
    | End ->  move_cursor (last-pos)
    | Home -> move_cursor (first-pos)
    | PageDown -> shift_cursor_and_view state local_state page_lines
    | PageUp ->   shift_cursor_and_view state local_state (-page_lines)
    | Exit -> [Exit]
    | Help -> [] (* TODO *)
    | Justify -> [] (* TODO *)
    | Left ->  move_cursor (-1)
    | Right -> move_cursor 1
    | Paste -> insert 0 clipboard
    | Save -> [Save]
    | ScrollDown -> shift_view state local_state 1
    | ScrollUp ->   shift_view state local_state (-1)
    | Tab -> insert 0 "    "
    | Key c -> insert 0 (String.make 1 c)
    | Unknown s -> [DisplayError (Some (Printf.sprintf "Unknown key pressed: %s" s))]
    in let actions = actions @ match button with
    | Cut -> []
    | _ -> [CutFlag false] 
    in let actions = actions @ match button with
    | Unknown _ -> []
    | _ -> [DisplayError None] 
    in let actions = (if List.exists is_remote actions then [Lock] else []) @ actions
    in actions

(* Step 4: Apply local state changes *)

let apply_local_action (state: state) (local_state: local_state): send_action -> local_state = function
    (*print_endline (Printf.sprintf "ApplyingLocal: %s" (string_of_send_action
    action));*)
    | CopyText s -> { local_state with clipboard = s }
    | CutFlag b -> { local_state with move_since_cut = not b }
    | DisplayError s -> { local_state with error = s }
    | Exit -> exit 0
    | Lock -> { local_state with locked = true }
    | ShiftView d -> { local_state with view = local_state.view + d }
    | _ -> local_state

(* Step 5: Send the action to the server *)
(* Step 6: Receive an action from the server *)
type receive_action =  
    | ReplaceText of int * int * int * string
    | UserLeaves of int
    | UserJoins of user_state
    | SetUser of int
    | Unlock
let string_of_receive_action = function
    | ReplaceText (uid, a, b, s) -> Printf.sprintf "ReplaceText[u=%d,%d,%d,\"%s\"]" uid a b (String.escaped s)
    | UserLeaves uid -> Printf.sprintf "UserLeaves[u=%d]" uid
    | UserJoins { user } -> Printf.sprintf "UserJoins[un=%s,...]" user
    | SetUser uid -> Printf.sprintf "SetUser[u=%d]" uid
    | Unlock -> "Unlock[]"

let server_stub1 (uid : int option): (send_action -> receive_action list) = function
    | ReplaceText (a,b,c) -> (match uid with
        | Some uid -> [ReplaceText (uid,a,b,c)]
        | None -> [] )
    | OpenDocument (document, username) -> [UserJoins { user=username; cursor=0; color=Red }; SetUser 0] (* TODO *)
    | Save -> [] (* Doesn't happen in stub *)
    | _ -> [] (* Local events are never sent *)
let server_stub (uid: int option) (actions: send_action list) : receive_action list = 
    List.concat_map (server_stub1 uid) actions @ if actions = [] then [] else [Unlock]

(* Step 7: Transform the state based on the action
    - Text
    - Cursor position (per-user)
    - User list
    VIEW is not included in the state, but consists of a start position only,
    which is always at the beginning of a line.
*)

let apply_remote_action (combined_state: state * local_state) : receive_action -> state * local_state = 
    (*print_endline (Printf.sprintf "ApplyingRemote: %s" (string_of_receive_action action));*)
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

let rec any : 'a option list -> 'a option = function
    | [] -> None
    | Some x :: _ -> Some x
    | None :: l -> any l
let option_or a b = any [a; b]

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

let display_document (width: int) (text: string) (color_of: int -> background_color option) : string =
    let colorize_char (index: int) (c: char) =
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
    |> concat |> map (function
        | (line_no, (sline_no, (sline_start, sline))) ->
            let colorize = fun i c -> colorize_char (i+sline_start) c in
            (if sline_no = 0 then Printf.sprintf "%2d " line_no else "   ") ^
            Printf.sprintf "%2d " sline_no ^
            (sline |> String.to_seq |> mapi colorize |> List.of_seq |> String.concat "") ^
            "\n")
    |> List.of_seq |> String.concat ""

let display_cursors state =
    state.per_user |> List.map (function
        | {user; cursor; color} ->
            Printf.sprintf " %s: %d " user cursor |> colorize color)
    |> String.concat "  "

let display_view state local_state =
    let (vs, ve) = viewport state local_state in
    Printf.sprintf " Viewport %d-%d [%d] " vs ve local_state.view |> colorize Blue

let display_help width =
    ""

let display (state: state) (local_state: local_state) : unit =
    let color_cursor = lookup_cursor_color state.per_user in
    let visible = in_viewport (viewport state local_state) in
    let color_viewport p = match visible p with
        | true -> Some Blue
        | false -> None in
    let color p = any [color_cursor p; color_viewport p] in
    let width = (avail_cols local_state.terminal_size) in
    print_endline ""; display_document width state.text color
    ^ display_cursors state ^ "  " ^ display_view state local_state ^ "\n"
    ^ display_help width
    |> print_endline

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
    (*
    let reset () : unit = print_string "\027[?1049l" in 
    (* Save terminal, home the cursor *)
    at_exit reset;
    print_string "\027[?1049h\027[H"; (* Restore the terminal *)
    Sys.set_signal Sys.sigint (Signal_handle (function | _ -> exit 0));
    *)

    (* Setup of the client *)
    let local_state = ref init_local_state in
    let state = ref init_state in

    (* Main loop of the client *)
    (* In a loop, listen for any of:
        - Messages from the server
        - Terminal resize events (SIGWINCH)
        - User keyboard input
    *)
    while true do
        let keystroke = get_keystroke() in
            print_string (Printf.sprintf "\"%s\" || " (String.escaped keystroke));
        let button = get_button keystroke in
            string_of_button button |> print_string; print_string " || ";
        let actions = compute_actions !state !local_state button in
            print_endline (String.concat "" (List.map string_of_send_action actions));
            (*print_string "Local: "; print_endline (String.concat "" (List.map string_of_send_action (local_only actions)));*)
        local_state := List.fold_left (apply_local_action !state) !local_state (local_only actions);
            (*print_string "Sent to server: "; print_endline (String.concat "" (List.map string_of_send_action (remote_only actions)));*)
        let msgs = server_stub !local_state.uid (remote_only actions) in
            (*print_string "Received from server: "; print_endline (String.concat "" (List.map string_of_receive_action msgs));*)
        let (s, ls) = List.fold_left apply_remote_action (!state, !local_state) msgs in
        state := s;
        local_state := ls;
        display !state !local_state
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
    | StandAlone ->
        let server = Domain.spawn (fun () -> server_main args.server_args) in (* TODO: Make sure the server is ready *)
        client_main args.client_args;
        Domain.join server
        
