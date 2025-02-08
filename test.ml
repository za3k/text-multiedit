open Text
open OUnit2

(*
let line_of (text : string) (pos : int) : int * int =
let pos_of (text: string) (line: int) (col: int) : int =
*)

let text = "14\nThis is the first line\nThis is the second line\nThis is the third line\n"
let text = "01\n34\n67\n"
let tests1 = "test suite for line_of" >::: [
   "t0" >:: (fun _ -> assert_equal (line_of text 0) (0, 0));
   "t1" >:: (fun _ -> assert_equal (line_of text 1) (0, 1));
   "t2" >:: (fun _ -> assert_equal (line_of text 2) (0, 2));
   "t3" >:: (fun _ -> assert_equal (line_of text 3) (1, 0));
   "t4" >:: (fun _ -> assert_equal (line_of text 4) (1, 1));
   "t5" >:: (fun _ -> assert_equal (line_of text 5) (1, 2));
   "t6" >:: (fun _ -> assert_equal (line_of text 6) (2, 0));
   "t7" >:: (fun _ -> assert_equal (line_of text 7) (2, 1));
   "t8" >:: (fun _ -> assert_equal (line_of text 8) (2, 2));

   "off left" >:: (fun _ -> assert_equal (line_of text ~-1) (0, 0));
   "off right" >:: (fun _ -> assert_equal (line_of text 9) (2, 2));
]


let tests2 = "test suite for pos_of" >::: [
   "t0" >:: (fun _ -> assert_equal (pos_of text 0 0) 0);
   "t1" >:: (fun _ -> assert_equal (pos_of text 0 1) 1);
   "t2" >:: (fun _ -> assert_equal (pos_of text 0 2) 2);
   "t3" >:: (fun _ -> assert_equal (pos_of text 1 0) 3);
   "t4" >:: (fun _ -> assert_equal (pos_of text 1 1) 4);
   "t5" >:: (fun _ -> assert_equal (pos_of text 1 2) 5);
   "t6" >:: (fun _ -> assert_equal (pos_of text 2 0) 6);
   "t7" >:: (fun _ -> assert_equal (pos_of text 2 1) 7);
   "t8" >:: (fun _ -> assert_equal (pos_of text 2 2) 8);

   "off top" >:: (fun _ -> assert_equal (pos_of text ~-3 2) 0);
   "off left" >:: (fun _ -> assert_equal (pos_of text 0 ~-5) 0);
   "off right" >:: (fun _ -> assert_equal (pos_of text 2 7) 8);
   "off bottom" >:: (fun _ -> assert_equal (pos_of text 3 6) 8);
]

let _ = run_test_tt_main tests1
let _ = run_test_tt_main tests2


