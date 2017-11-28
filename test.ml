open OUnit2
open To_tex
open Ast
open Str

let strip = global_replace (regexp "\n") ""

let equal s1 s2 = assert_equal (strip s1) (strip s2)

let to_tex = [
  "text" >:: (fun _ -> equal "text" (expr_to_tex (Text [String "text"])));
  "comment" >:: (fun _ -> equal "\\begin{comment}comment\\end{comment}"
                    (expr_to_tex (Comment "comment")));
  "math" >:: (fun _ -> equal "$math$" (expr_to_tex (Math [String "math"])));
  "unordered" >:: (fun _ -> equal "\\begin{itemize}
\\item Text item 1
\\item $Math item 2$
\\begin{itemize}
\\item Nested text item 1
\\item $Nested math item 2$
\\end{itemize}
\\end{itemize}" (expr_to_tex (Cmd ("list",
                                   None, [
                                     Text [String "Text item 1"];
                                     Math [String "Math item 2"];
                                     Cmd ("list",
                                          None, [
                                            Text [String "Nested text item 1"];
                                            Math [String "Nested math item 2"]
                                          ])
                                   ]))));
  "ordered" >:: (fun _ -> equal "\\begin{enumerate}[label=(\\alph*)]
\\item Text item 1
\\item $Math item 2$
\\begin{enumerate}[label=(\\Alph*)]
\\item Nested text item 1
\\item $Nested math item 2$
\\end{enumerate}
\\end{enumerate}" (expr_to_tex (Cmd ("list",
                                     Some "(\\alph*)", [
                                       Text [String "Text item 1"];
                                       Math [String "Math item 2"];
                                       Cmd ("list",
                                            Some "(\\Alph*)", [
                                              Text [String "Nested text item 1"];
                                              Math [String "Nested math item 2"]
                                            ])
                                     ]))))
]

let suite =
  "OCamTex test suite"
  >::: List.flatten [
    to_tex
  ]

let _ = run_test_tt_main suite
