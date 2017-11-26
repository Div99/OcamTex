open OUnit2
open To_tex
open Ast

let to_tex = [
  "text" >:: (fun _ -> assert_equal "text" (expr_to_tex (Text "text")));
  "comment" >:: (fun _ -> assert_equal "\\begin{comment}\ncomment\n\\end{comment}"
                    (expr_to_tex (Comment "comment")));
  "math" >:: (fun _ -> assert_equal "$math$" (expr_to_tex (Math "math")));
  "list" >:: (fun _ -> assert_equal "\\begin{itemize}
\\item Text item 1
\\item $Math item 2$
\\begin{itemize}
\\item Nested text item 1
\\item $Nested math item 2$
\\end{itemize}
\\end{itemize}" (expr_to_tex (Cmd (List ([Text "Text item 1";
                                          Math "Math item 2";
                                          Cmd (List ([Text "Nested text item 1";
                                                      Math "Nested math item 2"], None))], None)))))
]

let suite =
  "OCamTex test suite"
  >::: List.flatten [
    to_tex
  ]

let _ = run_test_tt_main suite
