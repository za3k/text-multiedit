(* CLIENT LOGIC *)
(* Setup:
    Parse command line. Set username, filename.
    Set up initial data structures.
    Connect to the server via a unix socket, /tmp/ocaml-text
    Send "Open <document> as <name> [, view-only please]" to the server.
        The server sends back the current text and cursor positions of other
        connected users, creating the document if needed.
*)

open Common
open Constants
open Types
open Utils
open Display

let init_local_state = {
    uid = None;
    view = 0;
    move_since_cut = true;
    terminal_size = { rows=25; cols=80 };
    clipboard = "";
    locked = true;
    error = None;
}

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

let rec lookup_shortcut keystroke = function
    | (ks, _, action) :: _ when List.mem keystroke ks -> Some action
    | _ :: l -> lookup_shortcut keystroke l
    | [] -> None

let get_button keystroke =
    if is_printable_ascii keystroke then Key (String.get keystroke 0)
    else match lookup_shortcut keystroke shortcuts with
        | Some action -> action
        | None -> Unknown keystroke


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


let get_terminal_size () : terminal_size =
    let command x =
        let p = Unix.open_process_in x in
        let r = In_channel.input_all p in
        In_channel.close p; r in
    match command "stty size" |> remove_trailing_newline |> String.split_on_char ' ' |> List.map int_of_string with
        | [rows; cols] -> {rows;cols}
        | _ -> failwith "Invalid terminal size"

let resize_terminal (state: state) (local_state: local_state) : local_state = 
    let _ (*old_size*) = local_state.terminal_size and
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

(* [Go to step 1] *)
(* Teardown:
    Send "Disconnect" to the server.
*)

let client_main (client_args: Args.client_args) : unit =
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
