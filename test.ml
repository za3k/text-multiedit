open Editor
open Text
open OUnit2

let all_tests = ref []
let add_test (test: test) : unit = all_tests := test :: !all_tests
let run_tests () = run_test_tt_main ("text editor" >::: (List.rev !all_tests))

let make_test ?printer : ('a * 'a) -> test = function
    | (a, b) -> "?" >:: (fun _ -> assert_equal ?printer b a)

let add_eq_tests ?printer (label: string) (tests: ('a * 'a) list) : unit =
    label >::: List.map (make_test ?printer) (List.rev tests) |> add_test

(* Printers *)
let string_of_iit = function
    | (a, b) -> Printf.sprintf "(%d, %d)" a b

let () = add_eq_tests "nth_char" [
   ((nth_char 'a' "baaaa" 1), 1);
   ((nth_char 'a' "baaaa" 2), 2);
   ((nth_char 'a' "babba" 2), 4);
   ((nth_char 'a' "aaaa" 1), 0);
   ((nth_char 'a' "aaaa" 2), 1);
] 

(*let text = "14\nThis is the first line\nThis is the second line\nThis is the third line\n"*)
let () =
let text1 = "01\n34\n67\n"
and text2 = "01\n\n\n"
and text3 = "\n12\n45\n" in
add_eq_tests "nth_line_start" [
   ((nth_line_start text1 0), 0);
   ((nth_line_start text1 1), 3);
   ((nth_line_start text1 2), 6);
   ((nth_line_start text2 0), 0);
   ((nth_line_start text2 1), 3);
   ((nth_line_start text2 2), 4);
   ((nth_line_start text3 0), 0);
   ((nth_line_start text3 1), 1);
   ((nth_line_start text3 2), 4);
   ((nth_line_start "\n" 0), 0);

   (* +1 out of bounds edge case *)
   ((nth_line_start text1 3), 9);
   ((nth_line_start text2 3), 5);
   ((nth_line_start text3 3), 7);
   ((nth_line_start "\n" 1), 0);
]

let () =
let text1 = "01\n34\n67\n"
and text2 = "01\n\n\n" in
add_eq_tests "line_of" [
   ((line_of text1 0), (0, 0));
   ((line_of text1 1), (0, 1));
   ((line_of text1 2), (0, 2));
   ((line_of text1 3), (1, 0));
   ((line_of text1 4), (1, 1));
   ((line_of text1 5), (1, 2));
   ((line_of text1 6), (2, 0));
   ((line_of text1 7), (2, 1));
   ((line_of text1 8), (2, 2));
   ((line_of text2 3), (1, 0));
   ((line_of text2 4), (2, 0));

   ((line_of text1 ~-1), (0, 0));
   ((line_of text1 9), (2, 2));
]

let () =
let text1 = "01\n34\n67\n"
and text2 = "01\n\n\n" in
add_eq_tests "pos_of" [
   ((pos_of text1 0 0), 0);
   ((pos_of text1 0 1), 1);
   ((pos_of text1 0 2), 2);
   ((pos_of text1 1 0), 3);
   ((pos_of text1 1 1), 4);
   ((pos_of text1 1 2), 5);
   ((pos_of text1 2 0), 6);
   ((pos_of text1 2 1), 7);
   ((pos_of text1 2 2), 8);
   ((pos_of text2 1 0), 3);

   (* off right/left *)
   ((pos_of text2 1 5), 3);
   ((pos_of text2 1 ~-5), 3);

   (* off top/left/right/bottom *)
   ((pos_of text1 ~-3 2), 0);
   ((pos_of text1 0 ~-5), 0);
   ((pos_of text1 2 7), 8);
   ((pos_of text1 3 6), 8);
]

let () =
let text1 = "iiiiiiiiixiii\niiiiiiiiixiiiiiiiiixiiiiiii\n"
and viewport ?(terminal_size={rows=7;cols=6}) text view = 
    let fake_state text = { text; document_name=""; per_user=[] }
    and fake_local_state view = { view; move_since_cut=false; clipboard=""; uid=None; terminal_size; error=None; locked=false } in
    viewport (fake_state text) (fake_local_state view) in

add_eq_tests ~printer:string_of_iit "viewport" [
    ((viewport text1 0), (0, 19));
    ((viewport text1 3), (0, 19));
    ((viewport text1 7), (6, 25));
    ((viewport text1 24), (20, 41));
]

let () =
let text1 = "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii\n" in
add_eq_tests ~printer:string_of_iit "sline_num" [
    ((sline_index 6 text1 27), (4, 3));
    ((sline_index 6 text1 0), (0, 0));
    ((sline_index 6 text1 5), (0, 5));
    ((sline_index 6 text1 6), (1, 0));
    ((sline_index 6 text1 7), (1, 1));
    ((sline_index 6 text1 24), (4, 0));
]

let () =
let text1 = "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii\n" in
add_eq_tests ~printer:string_of_iit "sline_for" [
    ((sline_for 6 text1 27), (24, 29));
    ((sline_for 6 text1 6), (6, 11));
    ((sline_for 6 text1 7), (6, 11));
]

let () = add_eq_tests ~printer:string_of_int "num_slines_in_line" [
    ((num_slines_in_line 6 0 13), 3);
    ((num_slines_in_line 6 14 41), 5);
]

let () =
let text1 = "iiiiiiiiixiii\niiiiiiiiixiiiiiiiiixiiiiiii\n"
and text2="Hello, world.\nThis is the second line.\nThis is the third line.\nThis is the fourth line\nThis is the fifth line\n" in
add_eq_tests ~printer:string_of_int "num_slines_in_lines" [
    ((num_slines_in_lines 6 text1 0 13), 3);
    ((num_slines_in_lines 6 text1 14 41), 5);
    ((num_slines_in_lines 6 text2 0 38), 8);
]

let () =
let text1 = "iiiiiiiiixiii\niiiiiiiiixiiiiiiiiixiiiiiii\n" in
add_eq_tests ~printer:string_of_int "sline_add_whole" [
    ((sline_add_whole 6 text1 0 0), 0);
    ((sline_add_whole 6 text1 0 1), 6);
    ((sline_add_whole 6 text1 0 2), 12);
    ((sline_add_whole 6 text1 0 3), 14);
    ((sline_add_whole 6 text1 0 4), 20);
    ((sline_add_whole 6 text1 6 1), 12);
    ((sline_add_whole 6 text1 6 2), 14);
    ((sline_add_whole 6 text1 14 ~-1), 12);
    ((sline_add_whole 6 text1 14 ~-2), 6);
    ((sline_add_whole 6 text1 14 ~-3), 0);
    ((sline_add_whole 6 text1 14 ~-100), 0);
    ((sline_add_whole 6 text1 12 ~-1), 6);
    ((sline_add_whole 6 text1 12 ~-2), 0);
    ((sline_add_whole 6 text1 12 ~-3), 0);
    ((sline_add_whole 6 text1 12 ~-100), 0);
    ((sline_add_whole 6 text1 12 1), 14);
    ((sline_add_whole 6 text1 20 1), 26);
    ((sline_add_whole 6 text1 20 4), 42);
    ((sline_add_whole 6 text1 20 100), 42);
]

let () =
let text1 = "iiiiiiiiixiii\niiiiiiiiixiiiiiiiiixiiiiiii\n" in
add_eq_tests ~printer:string_of_int "sline_add" [
    ((sline_add 6 text1 0 (0,0)), 0);
    ((sline_add 6 text1 0 (1,0)), 6);
    ((sline_add 6 text1 0 (2,0)), 12);
    ((sline_add 6 text1 1 (1,0)), 7);
    ((sline_add 6 text1 24 (4,0)), 42);
]

let () =
let text1 = "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii\n"
and text2="Hello, world.\nThis is the second line.\nThis is the third line.\nThis is the fourth line\nThis is the fifth line\n" in
add_eq_tests ~printer:string_of_iit "sline_difference" [
    ((sline_difference 6 text1 0 6), (1, 0));
    ((sline_difference 6 text1 0 8), (1, 2));
    ((sline_difference 6 text1 2 6), (1, ~-2));
    ((sline_difference 6 text1 8 0), (~-1, ~-2));
    ((sline_difference 6 text2 0 39), (8, 0));
]

let () =
let text2="Hello, world.\nThis is the second line.\nThis is the third line.\nThis is the fourth line\nThis is the fifth line\n"
and term = {rows=7; cols=6} in
add_eq_tests ~printer:string_of_cursor_bound "cursor_in_viewport" [
    ((cursor_in_viewport text2 term 0 0), OnScreen);
    ((cursor_in_viewport text2 term 0 19), OnScreen);
    ((cursor_in_viewport text2 term 0 20), OffBottom 1);
    ((cursor_in_viewport text2 term 0 22), OffBottom 1);
    ((cursor_in_viewport text2 term 0 39), OffBottom 5);
]

(*let rec sline_difference (width: int) (text: string) (pos1: int) (pos2: int) : sline_delta =*)

let () = run_tests ()


