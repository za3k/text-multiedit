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

(* CLIENT LOGIC *)
(* Setup:
    Parse command line. Set username, filename, and view-only flag.
    Set up initial data structures.
    Connect to the server via a unix socket, /tmp/ocaml-text
    Send "Open <document> as <name> [, view-only please]" to the server.
        The server sends back the current text and cursor positions of other connected users, creating the document if needed.
*)

(* Step 1: Read a keystroke *)
(* Step 2: Compute the [non-parameterized] action *)
type button = 
  | Backspace | Cut | Del | Down | End | Exit | Help | Home | Justify | Left | PageDown | PageUp | Paste | Right | Save | ScrollDown | ScrollUp | Tab | Up
  | Key of char
  | Unknown of string
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
(* Step 4: Apply local state changes *)
(* Step 5: Send the action to the server *)
(* Step 6: Receive an action from the server *)
type receive_action =  
    | ReplaceText of int * int * int * string
    | UserLeaves of int
    | UserJoins of user_state
    | SetUser of int
(* Step 7: Transform the state based on the action
    - Text
    - Cursor position (per-user)
    VIEW is not included in the state, but consists of a start position only, which is always at the beginning of a line.
*)
(* Step 8: Update the UI
    8a: Calculate the view, based on the last view, the current terminal size, and the cursor position.
    8b: Display the text (or the help)
    8c: Display the active users
    8d: Display the shortcuts
*)
(* [Go to step 1] *)
(* Teardown:
    Send "Disconnect" to the server.
*)

(* In a loop, listen for any of:
    - Messages from the server
    - Terminal resize events (SIGWINCH)
    - User keyboard input
*)

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


