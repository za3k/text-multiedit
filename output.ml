type 'a t = {
    file: Out_channel.t;
    writer: Out_channel.t -> 'a -> unit;
}

let default_writer o v = Marshal.to_channel o v []; Out_channel.flush o
let of_file ?(writer : Out_channel.t -> 'a -> unit = default_writer) (file: Out_channel.t) : 'a t =
    { file; writer }

let send (channel: 'a t) (value: 'a) : unit = channel.writer channel.file value

let fake () : 'a t =
    of_file ~writer:(fun _ _ -> ()) stdout

