open Types

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

let string_of_send_action = function
    | Remote Save -> "<Save>"
    | Remote OpenDocument (u, d) -> Printf.sprintf "<OpenDocument \"%s\" \"%s\">" (String.escaped u) (String.escaped d)
    | Remote ReplaceText (start, len, s) -> Printf.sprintf "<ReplaceText %d %d \"%s\">" start len (String.escaped s)
    | Local CopyText s -> Printf.sprintf "<CopyText \"%s\">" (String.escaped s)
    | Local CutFlag f -> Printf.sprintf "<CutFlag %s>" (Bool.to_string f)
    | Local DisplayError None -> "<DisplayError OK>"
    | Local DisplayError Some s -> Printf.sprintf "<DisplayError \"%s\">" (String.escaped s)
    | Local ShiftView n -> Printf.sprintf "<ShiftView %d>" n
    | Local Lock -> "<Lock>"
    | Local Exit -> "<Exit>"

let string_of_cursor_bound = function 
    | OnScreen -> "OnScreen"
    | OffTop n -> Printf.sprintf "OffTop(%d)" n
    | OffBottom n -> Printf.sprintf "OffBottom(%d)" n

let string_of_receive_action = function
    | ReplaceText (uid, a, b, s) -> Printf.sprintf "ReplaceText[u=%d,%d,%d,\"%s\"]" uid a b (String.escaped s)
    | UserLeaves uid -> Printf.sprintf "UserLeaves[u=%d]" uid
    | UserJoins { user } -> Printf.sprintf "UserJoins[un=%s,...]" user
    | SetUser uid -> Printf.sprintf "SetUser[u=%d]" uid
    | Unlock -> "Unlock[]"
