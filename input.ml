type 'a t = {
    file: In_channel.t;
    reader: In_channel.t -> ('a option);
    ready: bool ref;
}

type handle = { file_descr: Unix.file_descr; mark_ready: unit -> unit }

let default_reader i = Option.some @@ Marshal.from_channel i
let of_file ?(reader: (In_channel.t -> ('a option)) = default_reader) 
            (file: In_channel.t) : 'a t =
    { file; reader; ready=ref false }

(* let for_listening_socket file_descr -> 'a t t = fail() *)

let is_ready (channel: 'a t) : bool = !(channel.ready)

let read (channel: 'a t) : 'a option =
    if not @@ is_ready channel then None
    else 
        (channel.ready := false;
        channel.reader channel.file)
let read_exn (channel: 'a t) : 'a = read channel |> Option.get

let handle (channel: 'a t) : handle = 
    let mark_ready = function () -> channel.ready := true and
        file_descr = Unix.descr_of_in_channel channel.file in
    { file_descr; mark_ready }

let rec select (handles: handle list) : unit =
    let files = List.map (function {file_descr} -> file_descr) handles in

    match Unix.select files [] [] (-4.0) with
        | exception Unix.Unix_error (EINTR, _, _) -> select handles
        | (ready_files, _, _) ->

    handles 
    |> List.filter (function h -> List.mem h.file_descr ready_files)
    |> List.iter (function h -> h.mark_ready ())

let channel_pair () : In_channel.t * Out_channel.t =
    let (i, o) = Unix.pipe () in
    (Unix.in_channel_of_descr i, Unix.out_channel_of_descr o)

let fake_inchannel = fst @@ channel_pair ()
let fail _ = failwith "Not implemented"

(*let for_signal int : unit t = fail()*)

let fake () : 'a t =
    of_file ~reader:fail fake_inchannel

let of_signal (signal: int) : unit t =
    let (i, o) = channel_pair () in
    Out_channel.set_buffered o false;
    Unix.set_nonblock (Unix.descr_of_in_channel i);

    (* TODO: Flush needed in non-buffered mode? *)
    let producer o _ : unit = output_byte o 0; Out_channel.flush o in 
    let rec consumer i : unit option =
        match Unix.read (Unix.descr_of_in_channel i) (Bytes.create 100) 0 100 with
            | 100 -> consumer i
            (*
            | exception Unix.Unix_error (EAGAIN, _, _)
            | exception Unix.Unix_error (EWOULDBLOCK, _, _) -> None
            *)
            | _ -> Some () in

    (* let _ = Unix.sigprocmask Unix.SIG_BLOCK [signal]; *)
    Sys.set_signal signal (Sys.Signal_handle (producer o));

    of_file ~reader:consumer i
