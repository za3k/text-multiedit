open Text
open OUnit2

let all_tests = ref []
let add_test (test: test) : unit = all_tests := test :: !all_tests
let run_tests () = run_test_tt_main ("text editor" >::: !all_tests)

let make_test ?printer : ('a * 'a) -> test = function
    | (x, b) -> "?" >:: (fun _ -> assert_equal ?printer x b)

let add_eq_tests ?printer (label: string) (tests: ('a * 'a) list) : unit =
    label >::: List.map (make_test ?printer) tests |> add_test

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
add_eq_tests "test suite for line_of" [
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
add_eq_tests "test suite for pos_of" [
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

let () = run_tests ()


