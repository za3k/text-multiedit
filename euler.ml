let rec range min max =
    if min > max then []
    else min :: range (min + 1) max
let iota = range 1
let rec filter p = function
    | [] -> []
    | h :: ls -> if p h then h :: filter p ls else filter p ls
let rec map f = function
    | [] -> []
    | h :: l -> f h :: map f l
let rec foldl f a = function
    | [] -> a
    | h :: l -> foldl f (f a h) l
let fold = foldl

let rec print_inner = function
    | [] -> ()
    | h :: l -> print_int h; print_string " "; print_inner l
let print_ints l =
    print_string "[ ";
    print_inner l;
    print_endline "]\n"

(* Problem 23: Find the sum of all positive integers which cannot be expressed as the sum of two abundant numbers.
   An abundant number is one whose proper divisors add to more than the number.
   All integers greater than 28123 can be written as the sum of two abundant numbers
*)

let rec sum = function
    | [] -> 0
    | h :: ls -> h + sum ls
let divides x y = (0 = x mod y)
let proper_divisors n = iota (n - 1) |> filter (divides n)
let sum_of_factors n = proper_divisors n |> sum
let abundant n = sum_of_factors n > n

(* We assume a set of integers is a list of integers in increasing order *)
let rec set_diff l1 l2 = match l1, l2 with
    | [], _ -> []
    | _, [] -> l1
    | h1 :: r1, h2 :: r2 -> 
        if h1 < h2 then h1 :: set_diff r1 l2
        else if h1 = h2 then set_diff r1 r2
        else set_diff l1 r2
let rec set_union l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | h1 :: r1, h2 :: r2 ->
        if h1 = h2 then h1 :: set_union r1 r2
        else if h1 < h2 then h1 :: set_union r1 l2
        else h2 :: set_union l1 r2
let set_flatten = fold set_union []
let cartesian_sum xs ys = (* Given strict evaluation, this implementation is slow because of memory usage 2025-01-28 13:24*)
    map (function x -> map (function y -> x+y) ys) xs |> set_flatten

let () = print_ints (proper_divisors 24980)
let () = print_endline (if (abundant 24980) then "ab" else "notab")

let limit = 28123
let abundant_numbers = iota limit |> filter abundant
let () = print_ints abundant_numbers

let missing = set_diff (iota limit) (cartesian_sum abundant_numbers abundant_numbers)
let () = print_string "missing "; print_int (sum missing); print_endline ""
