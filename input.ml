type 'a t = {
    file: In_channel.t;
    reader: In_channel.t -> 'a;
    ready: bool ref;
    closed: bool ref;
}
type 'a read_value = NotReady | Closed | Some of 'a

type handle = { file_descr: Unix.file_descr; mark_ready: unit -> unit }

let default_reader = Marshal.from_channel
let of_file ?(reader: (In_channel.t -> 'a) = default_reader) (file: In_channel.t) : 'a t =
    { file; reader; ready=ref false; closed=ref false }

(* let for_listening_socket file_descr -> 'a t t = fail() *)

let is_ready (channel: 'a t) : bool = !(channel.ready)

let is_closed (channel: 'a t) : bool = !(channel.closed)

let read (channel: 'a t) : 'a read_value =
    (* Reads a value ONLY if ready and selected by a call to 'select' *)
    if not @@ is_ready channel then NotReady
    else if is_closed channel then Closed
    else (
        channel.ready := false;
        try Some (channel.reader channel.file) with
            | End_of_file ->
                channel.closed := true;
                Closed
    )

let handle (channel: 'a t) : handle = 
    let mark_ready = function () -> channel.ready := true and
        file_descr = Unix.descr_of_in_channel channel.file in
    { file_descr; mark_ready }

let rec select (handles: handle list) : unit =
    (* TODO: If one of the handles is already waiting, return that set first *)
    let files = List.map (function {file_descr} -> file_descr) handles in

    match Unix.select files [] [] (-4.0) with
        | exception Unix.Unix_error (EINTR, _, _) -> select handles
        | (ready_files, _, _) ->

    handles 
    |> List.filter (function h -> List.mem h.file_descr ready_files)
    |> List.iter (function h -> h.mark_ready ())

let map (f: 'a -> 'b) (i: 'a t) : 'b t =
    let reader n = f @@ i.reader n in
    of_file ~reader i.file

(* TODO: Add a non-blocking select (so we can see if there are additional new connections *)

let channel_pair () : In_channel.t * Out_channel.t =
    let (i, o) = Unix.pipe () in
    (Unix.in_channel_of_descr i, Unix.out_channel_of_descr o)

let fake () : 'a t =
    let fake_inchannel = fst @@ channel_pair () and
        fail _ = failwith "Not implemented" in
    of_file ~reader:fail fake_inchannel

let of_signal (signal: int) : unit t =
    let (i, o) = channel_pair () in
    Out_channel.set_buffered o false;
    Unix.set_nonblock (Unix.descr_of_in_channel i);

    (* TODO: Flush needed in non-buffered mode? *)
    let producer o _ : unit = output_byte o 0; Out_channel.flush o in 
    let rec consumer i : unit =
        match Unix.read (Unix.descr_of_in_channel i) (Bytes.create 100) 0 100 with
            | 100 -> consumer i
            (*
            | exception Unix.Unix_error (EAGAIN, _, _)
            | exception Unix.Unix_error (EWOULDBLOCK, _, _) -> None
            *)
            | _ -> () in

    (* let _ = Unix.sigprocmask Unix.SIG_BLOCK [signal]; *)
    Sys.set_signal signal (Sys.Signal_handle (producer o));

    of_file ~reader:consumer i

let of_socket ?reader ?writer (path: string) : ('a t) * ('b Output.t) =
    let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let addr = Unix.ADDR_UNIX path in
    Unix.connect fd addr;
    (of_file ?reader (Unix.in_channel_of_descr fd), 
    Output.of_file ?writer (Unix.out_channel_of_descr fd))

let of_listening_socket ?reader ?writer (path: string) : (('a t) * ('b Output.t)) t =
    let open_unix_socket path =
        let clean () = if Sys.file_exists path then Sys.remove path in
        clean (); at_exit clean;

        let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        let addr = Unix.ADDR_UNIX path in
        Unix.bind fd addr;
        Unix.listen fd 10;
        Unix.in_channel_of_descr fd in

    let process_connection (ic: In_channel.t) =
        let (client_fd, client_addr) = Unix.accept (Unix.descr_of_in_channel ic) in
        (of_file (Unix.in_channel_of_descr client_fd),
        Output.of_file (Unix.out_channel_of_descr client_fd)) in

    of_file ~reader:process_connection (open_unix_socket path)
