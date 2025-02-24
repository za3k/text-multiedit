let remove_index (i: int) : 'a list -> 'a list = List.filteri (fun j x -> j <> i)
let remove (x: 'a) (l: 'a list) = List.filter ((!=) x) l
let compose f g x = f (g x) (* Support OCaml 4.14.1 *)
let copies num x = List.init num (fun _ -> x)
let sum = List.fold_left (+) 0
let rec any : 'a option list -> 'a option = function
    | [] -> None
    | Some x :: _ -> Some x
    | None :: l -> any l
let option_or a b = any [a; b]
let rec except_last : 'a list -> 'a list = function
    | [] -> []
    | a :: [] -> []
    | h :: l -> h :: except_last l
let rec suffixes = function (* In order from biggest to smallest *)
    | [] -> []
    | _ :: l as all -> all :: (suffixes l)
let prefixes l = (* In order from biggest to smallest *)
    l |> List.rev |> suffixes |> List.map List.rev
let truncate len s =
    if String.length s <= len then s
    else String.sub s 0 (len-1)
let has_trailing_newline : string -> bool = String.ends_with ~suffix:"\n"
let remove_trailing_newline s = 
    String.sub s 0 ((String.length s)-1)

let sigwinch = 28 (* Source: https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/signal.h *)

let string_of_list f l =
    String.concat " " (List.map f l) |> Printf.sprintf "[%s]"
