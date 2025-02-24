open Utils

type run_mode = Client | Server | StandAlone

type server_args = {
    dir: string;
    socket: string;
    debug: bool;
}

type client_args = {
    debug: bool;
    file: string;
    user: string;
    socket: string;
}

type cli_args = {
    mode: run_mode;
    client_args: client_args;
    server_args: server_args;
}

let parse_args () : cli_args =
    let usage_msg = "text [--debug] [--dir DIR] [--stand-alone|--server|--client] FILE" and
        debug = ref false and
        mode = ref Client and
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
        | 1 -> List.hd !files
        | _ -> Arg.usage speclist usage_msg; exit 3 in
    { 
        mode = !mode; 
        client_args = { file = only_file; debug = !debug; user; socket };
        server_args = { dir = Option.value dir ~default: ""; socket; debug=(Server = !mode) }
    }
