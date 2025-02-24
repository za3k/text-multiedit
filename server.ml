(* SERVER LOGIC *)

open Common
open Constants
open Types
open Utils

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
                    (Color.colorize Green u.name) !(u.uid)
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
            enqueue_all @@ ReplaceText (!(user.uid),a,b,c)
        | Save -> 
            let state = !(document.state) in
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
            enqueue_user @@ SetUser !(user.uid) (* Our logic is such that this is the same as the computed value *) 
        | Disconnect ->
            enqueue_all @@ UserLeaves !(user.uid)
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

let server_main (server_args: Args.server_args) (on_ready: unit->unit) : unit =
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
                let text = Option.value (load (path server_args.dir docname)) ~default:"\n" in
                let state = { text; per_user=[]; document_name=docname } in
                Some { state=ref state; users=ref [] }
        ) !documents;
        StringMap.find docname !documents in

    let auth_user (conn: connection) (actions: send_remote_action list) : user option =
        match actions with
        | OpenDocument (document_name, name) :: _ ->
            let document = get_document document_name in
            let uid = ref @@ List.length !(document.state).per_user in
            let user = { conn; document; uid; name } in
            document.users := user :: !(document.users);
            Some user
        | _ -> None in

    let remove_conn conn = unauthed_connections := remove conn !unauthed_connections in (* TODO: Also close pipe? *)
    let add_user user = all_users := user :: !all_users in
    let remove_user user = 
        all_users := remove user !all_users;
        user.document.users := remove user !(user.document.users);
        let adjust_uid u = (* Shift server uids *)
            if !(u.uid) > !(user.uid) then
                u.uid := !(u.uid) - 1 in
        List.iter adjust_uid !(user.document.users) in

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
