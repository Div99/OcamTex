open OUnit2
open To_tex
open Ast
open Str

let strip = global_replace (regexp "\n") ""

let equal s1 s2 = assert_equal (strip s1) (strip s2)

let to_tex = [
  "string" >:: (fun _ -> equal "string" (expr_to_tex (String "string")));
  "text" >:: (fun _ -> equal "text" (expr_to_tex (Text [String "text"])));
  "math" >:: (fun _ -> equal "$math$" (expr_to_tex (Math (MathStr "math"))));
  "comment" >:: (fun _ -> equal "\\begin{comment}comment\\end{comment}"
                    (expr_to_tex (Comment "comment")));
  "title" >:: (fun _ -> equal "\\title{title}" (head_to_tex (Title "title")));
  "author" >:: (fun _ -> equal "\\author{author}" (head_to_tex (Author "author")));
  "font" >:: (fun _ -> equal ("\\usepackage[T1]{fontenc}\\usepackage{font}") (head_to_tex (Font "font")));
  "hcomment" >:: (fun _ -> equal "\\begin{comment}comment\\end{comment}" (head_to_tex (HComment "comment")));
  "hstring" >:: (fun _ -> equal "string" (head_to_tex (HString "string")));
]

let suite =
  "OCamTex test suite"
  >::: List.flatten [
    to_tex
  ]

let _ = run_test_tt_main suite
