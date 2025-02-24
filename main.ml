(*  textmu, a multi-user text editor

    Designed to support multiple users ssh-ed into one machine, editing a
    set of documents at the same time.
    
    Limitations:
        ASCII only, no Unicode
        Explicit save required
        Undo/redo not supported
*)

let cvar () : (unit -> unit) * (unit -> unit) =
    let flag = Atomic.make false and
        cv = Condition.create () and
        m = Mutex.create () in
    Mutex.lock m;

    let set_ready () =
            Atomic.set flag true;
            Condition.broadcast cv and
        wait_until_ready () =
            while not @@ Atomic.get flag do
                Condition.wait cv m
            done
    in (set_ready, wait_until_ready)
        
let crash = Atomic.make None
let enable_error_reporting () =
    at_exit (fun () -> 
        Out_channel.flush stdout;
        match Atomic.get crash with 
        | Some msg -> Out_channel.output_string stderr msg
        | _ -> () 
    )
let fail_catastrophically (f: 'a -> 'b) (x: 'a) : 'b =
    Printexc.record_backtrace true; (* Fun fact: this is a per-domain setting *)
    try f x with
        | e ->
            Atomic.set crash (Printf.sprintf "%s\n%s\n"
                (Printexc.to_string e)
                (Printexc.get_backtrace ()) |> Option.some);
            exit 2

let main () : unit =
    enable_error_reporting ();

    let args = Args.parse_args () in
    match args.mode with
    | Client -> fail_catastrophically Client.client_main args.client_args
    | Server -> fail_catastrophically (Server.server_main args.server_args) ignore
    | StandAlone ->
        let (set_ready, wait_until_ready) = cvar () in
        let server = Domain.spawn
            (fun () -> fail_catastrophically (Server.server_main args.server_args) set_ready)
        in
        wait_until_ready ();
        fail_catastrophically Client.client_main args.client_args;
        Domain.join server

let () = main ()
