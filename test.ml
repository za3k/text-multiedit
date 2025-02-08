open Text
open OUnit2

(*let text = "14\nThis is the first line\nThis is the second line\nThis is the third line\n"*)
let text = "01\n34\n67\n"
let text2 = "01\n\n\n"
let text3 = "\n12\n45\n"
let empty_text = "\n"

let _ = "test suite for nth_char" >::: [
   "t1" >:: (fun _ -> assert_equal (nth_char 'a' "baaaa" 1) 1);
   "t2" >:: (fun _ -> assert_equal (nth_char 'a' "baaaa" 2) 2);
   "t3" >:: (fun _ -> assert_equal (nth_char 'a' "babba" 2) 4);
   "t4" >:: (fun _ -> assert_equal (nth_char 'a' "aaaa" 1) 0);
   "t5" >:: (fun _ -> assert_equal (nth_char 'a' "aaaa" 2) 1);
] |> run_test_tt_main

let _ = "test suite for nth_line_start" >::: [
   "line 1.0" >:: (fun _ -> assert_equal (nth_line_start text 0) 0);
   "line 1.1" >:: (fun _ -> assert_equal (nth_line_start text 1) 3);
   "line 1.2" >:: (fun _ -> assert_equal (nth_line_start text 2) 6);
   "line 2.0" >:: (fun _ -> assert_equal (nth_line_start text2 0) 0);
   "line 2.1" >:: (fun _ -> assert_equal (nth_line_start text2 1) 3);
   "line 2.2" >:: (fun _ -> assert_equal (nth_line_start text2 2) 4);
   "empty" >:: (fun _ -> assert_equal (nth_line_start empty_text 0) 0);
   "line 3.0" >:: (fun _ -> assert_equal (nth_line_start text3 0) 0);
   "line 3.1" >:: (fun _ -> assert_equal (nth_line_start text3 1) 1);
   "line 3.2" >:: (fun _ -> assert_equal (nth_line_start text3 2) 4);

   (* +1 out of bounds edge case *)
   "line 1.end" >:: (fun _ -> assert_equal (nth_line_start text 3) 9);
   "line 2.end" >:: (fun _ -> assert_equal (nth_line_start text2 3) 5);
   "line 3.end" >:: (fun _ -> assert_equal (nth_line_start text3 3) 7);
   "empty OOB" >:: (fun _ -> assert_equal (nth_line_start empty_text 1) 0);
] |> run_test_tt_main

let _ = "test suite for line_of" >::: [
   "t0" >:: (fun _ -> assert_equal (line_of text 0) (0, 0));
   "t1" >:: (fun _ -> assert_equal (line_of text 1) (0, 1));
   "t2" >:: (fun _ -> assert_equal (line_of text 2) (0, 2));
   "t3" >:: (fun _ -> assert_equal (line_of text 3) (1, 0));
   "t4" >:: (fun _ -> assert_equal (line_of text 4) (1, 1));
   "t5" >:: (fun _ -> assert_equal (line_of text 5) (1, 2));
   "t6" >:: (fun _ -> assert_equal (line_of text 6) (2, 0));
   "t7" >:: (fun _ -> assert_equal (line_of text 7) (2, 1));
   "t8" >:: (fun _ -> assert_equal (line_of text 8) (2, 2));
   "text 2" >:: (fun _ -> assert_equal (line_of text2 3) (1, 0));
   "text 2" >:: (fun _ -> assert_equal (line_of text2 4) (2, 0));

   "off left"  >:: (fun _ -> assert_equal (line_of text ~-1) (0, 0));
   "off right" >:: (fun _ -> assert_equal (line_of text 9) (2, 2));
] |> run_test_tt_main

let _ = "test suite for pos_of" >::: [
   "t0" >:: (fun _ -> assert_equal (pos_of text 0 0) 0);
   "t1" >:: (fun _ -> assert_equal (pos_of text 0 1) 1);
   "t2" >:: (fun _ -> assert_equal (pos_of text 0 2) 2);
   "t3" >:: (fun _ -> assert_equal (pos_of text 1 0) 3);
   "t4" >:: (fun _ -> assert_equal (pos_of text 1 1) 4);
   "t5" >:: (fun _ -> assert_equal (pos_of text 1 2) 5);
   "t6" >:: (fun _ -> assert_equal (pos_of text 2 0) 6);
   "t7" >:: (fun _ -> assert_equal (pos_of text 2 1) 7);
   "t8" >:: (fun _ -> assert_equal (pos_of text 2 2) 8);
   "text2"           >:: (fun _ -> assert_equal (pos_of text2 1 0) 3);
   "off right empty" >:: (fun _ -> assert_equal (pos_of text2 1 5) 3);
   "off left empty"  >:: (fun _ -> assert_equal (pos_of text2 1 ~-5) 3);

   "off top"    >:: (fun _ -> assert_equal (pos_of text ~-3 2) 0);
   "off left"   >:: (fun _ -> assert_equal (pos_of text 0 ~-5) 0);
   "off right"  >:: (fun _ -> assert_equal (pos_of text 2 7) 8);
   "off bottom" >:: (fun _ -> assert_equal (pos_of text 3 6) 8);
] |> run_test_tt_main



