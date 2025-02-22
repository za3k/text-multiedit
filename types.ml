type background_color = 
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White 
    | BrightBlack | BrightRed | BrightGreen | BrightYellow | BrightBlue | BrightMagenta | BrightCyan | BrightWhite

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
    | OpenDocument of string * string
    | ReplaceText of int * int * string
type send_action = Local of send_local_action | Remote of send_remote_action

type viewport = int * int
type cursor_bound = | OnScreen | OffTop of int | OffBottom of int

type receive_action =  
    | ReplaceText of int * int * int * string
    | UserLeaves of int
    | UserJoins of user_state
    | SetUser of int
    | Unlock

type connection = {
    inp: send_remote_action list Input.t;
    out: receive_action list Output.t;
}
and user = {
    conn: connection;
    document: document;
    uid: int;
}
and document = {
    state: state ref;
    users: user list ref;
}
