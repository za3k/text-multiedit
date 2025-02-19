type 'a t = {
    file: Out_channel.t;
    writer: Out_channel.t -> 'a -> unit;
}

(* TODO: Automatic writer should use marshalling *)

let of_file (file: Out_channel.t) (writer: Out_channel.t -> 'a -> unit) : 'a t =
    { file; writer }
let send (channel: 'a t) (value: 'a) : unit = channel.writer channel.file value
