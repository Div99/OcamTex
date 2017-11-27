open Ast

let is_cmd = function Cmd _ -> true | _ -> false

let rec expr_to_tex expr = match expr with
  | Text s -> s
  | Comment s -> "\\begin{comment}\n" ^ s ^ "\n\\end{comment}"
  | Math s -> "$" ^ s ^ "$"
  | Var s -> failwith "?"
  | Cmd cmd -> cmd_to_tex cmd

and cmd_to_tex cmd = match cmd with
  | List (items, style) -> "\\begin{itemize}\n" ^
                           fold_exprs ~prefix:"\\item " items ^
                           "\\end{itemize}"
  | _ -> failwith "Unimplemented"

and head_to_tex head_list = failwith "Unimplemented"

and fold_exprs ?prefix:(pre = "") expr_list =
  List.fold_left
    (fun acc expr -> let pre = if is_cmd expr then "" else pre in
      acc ^ pre ^ expr_to_tex expr ^ "\n")
    "" expr_list

let write_string_to_file filename str =
  let chan = open_out filename in
  output_string chan str; close_out chan

let write_to_tex doc = let output_str = match doc with
    | (_, body) ->
      "\\documentclass{article}\n" ^
      "\\usepackage{verbatim}\n\n" ^
      "\\begin{document}\n"
      ^ fold_exprs body ^
      "\\end{document}" in
  write_string_to_file "output.tex" output_str;
  "output.tex"
