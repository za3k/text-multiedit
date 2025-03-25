type background_color = Color.t

(* Invariant: text has a newline at the end *)
type user_state = { user: string; cursor: int; color: background_color; }
type state = { text: string; document_name: string; per_user: user_state list }
type view = int (* Indicates somewhere in the s-line which is the top of what you can see *)
type terminal_size = { rows: int; cols: int }
type local_state = { view: view; move_since_cut: bool; clipboard: string; uid: int option; terminal_size: terminal_size; error: string option; locked: bool }

type button = 
  | Backspace | Cut | Del | Down | End | Exit | Help | Home | Justify | Left | PageDown | PageUp | Paste | Right | Save | ScrollDown | ScrollUp | Tab | Up
  | Key of char
  | Unknown of string

type send_local_action = 
    | CopyText of string
    | CutFlag of bool
    | DisplayError of string option
    | Exit
    | Lock
    | ShiftView of int
type send_remote_action =
    (* Remote only *)
    | Save 
    (* Remote, will be sent back by the server as receive_action *)
    | OpenDocument of string * string (* document name, user display name *)
    | ReplaceText of int * int * string (* start index, length of replaced text, replacement text *)
    | Disconnect
type send_action = Local of send_local_action | Remote of send_remote_action

type viewport = int * int (* start, end POS values (inclusive) *)
type cursor_bound = | OnScreen | OffTop of int | OffBottom of int (* by how many lines *)

type receive_action =  
    | ReplaceText of int * int * int * string (* uid, start index, length of replaced text, replacement text *)
    | UserLeaves of int (* uid *)
    | UserJoins of user_state (* uid is implicit *)
    | SetUser of int (* uid *)
    | DisplayMessage of string
    | Unlock

(* These don't really need to be in types.ml except we want debug printing for 'user' *)
type connection = {
    inp: send_remote_action list Input.t;
    out: receive_action list Output.t;
}
and user = {
    conn: connection;
    document: document;
    name: string;
    uid: int ref;
}
and document = {
    state: state ref;
    users: user list ref;
}
