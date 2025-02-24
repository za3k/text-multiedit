(* Stuff common between the client and server *)

open Types
open Constants
open Utils

let empty_document document_name = {
    text="\n"; 
    document_name;
    per_user = []
}

let get_cursor (state: state) (local_state: local_state) : int option =
    let user = match local_state.uid with
        | None -> None
        | Some uid -> List.nth_opt state.per_user uid in
    Option.map (fun user -> user.cursor) user

let get_cursor_unsafe state local_state : int = 
    get_cursor state local_state |> Option.get

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
