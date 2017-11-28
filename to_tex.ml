open Ast

let is_cmd = function Cmd _ -> true | _ -> false

let rec expr_to_tex expr = match expr with
  | String s -> s
  | Text exprs -> fold_exprs exprs
  | Math exprs -> "$" ^ fold_exprs exprs ^ "$"
  | Comment s -> "\\begin{comment}\n" ^ s ^ "\n\\end{comment}"
  | Var s -> failwith "Unimplemented"
  | Cmd (cmd, style, exprs) -> cmd_to_tex cmd style exprs

and cmd_to_tex cmd style exprs = match cmd with
  | "list" ->
    let order = if style = None then "itemize" else "enumerate" in
    let sty = match style with
      | None -> ""
      | Some s -> "[label=" ^ s ^ "]" in
    "\\begin{" ^ order ^ "}" ^ sty ^ "\n" ^
    fold_exprs ~prefix:"\\item " exprs ^
    "\\end{" ^ order ^ "}"
  | _ -> "\\" ^ cmd

and head_to_tex head_list = failwith "Unimplemented"

and fold_exprs ?prefix:(pre = "") expr_list =
  List.fold_left
    (fun acc expr -> let pre = if is_cmd expr then "" else pre in
     acc ^ pre ^ expr_to_tex expr)  "" expr_list

body_to_tex =
  Str.split (regexp "[^a-zA-Z0-9]*[\t\r ]")
let write_string_to_file filename str =
  let chan = open_out filename in
  output_string chan str; close_out chan

let write_to_tex doc = let output_str = match doc with
    | (_,body) ->
      "\\documentclass{article}\n" ^
      "\\usepackage{verbatim}\n\n" ^
      "\\begin{document}\n"
      ^ fold_exprs body ^
      "\n\\end{document}" in
  write_string_to_file "output.tex" output_str;
  "output.tex"
