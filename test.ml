open OUnit2

let to_tex = [
  "test" >:: (fun _ -> assert_equal true true);
]

let suite =
  "OCamTex test suite"
  >::: List.flatten [
    to_tex
  ]

let _ = run_test_tt_main suite
