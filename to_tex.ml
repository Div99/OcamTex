open Ast

let cmd_to_tex cmd = match cmd with
  | List (style, items) -> failwith "Unimplemented"
  | _ -> failwith "Unimplemented"

let expr_to_tex expr = match expr with
  | Text s -> s
  | Comment s -> "\\begin{comment}" ^ s ^ "\\end{comment}"
  | Math s -> "$" ^ s ^ "$"
  | Var s -> failwith "?"
  | Cmd cmd -> cmd_to_tex cmd

let head_to_tex head_list = failwith "Unimplemented"

let body_to_tex expr_list =
  List.fold_left
    (fun acc exp -> acc ^ (expr_to_tex exp) ^ "\n")
    "" expr_list

let write_string_to_file filename str =
  let chan = open_out filename in
  output_string chan str; close_out chan

let write_to_tex doc = let output_str = match doc with
    | (_, body) ->
      "\\documentclass{article}\n" ^
      "\\usepackage{verbatim}\n" ^
      "\\begin{document}\n"
      ^ body_to_tex body ^
      "\\end{document}" in
  write_string_to_file "output.tex" output_str;
  "output.tex"
