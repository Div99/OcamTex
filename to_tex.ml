open Ast

let rec expr_to_tex = function
  | String s -> s
  | Text exprs -> fold_body exprs
  | Math exprs -> "$" ^ fold_body exprs ^ "$"
  | Comment s -> "\\begin{comment}\n" ^ s ^ "\n\\end{comment}"
  | Var s -> failwith "Unimplemented"
  | Cmd (cmd, style, exprs) -> cmd_to_tex cmd style exprs

and cmd_to_tex cmd style exprs = match cmd with
  | "list" ->
    let order = if style = None then "itemize" else "enumerate" in
    let sty = match style with
      | None -> ""
      | Some s -> "[label=" ^ s ^ "]" in
    "\\begin{" ^ order ^ "}" ^ sty ^
    fold_body exprs ^
    "\n\\end{" ^ order ^ "}"
  | _ -> "\\" ^ cmd ^ " " ^ fold_body exprs

and head_to_tex = function
  | Title s -> "\\title{" ^ s ^ "}"
  | Author s -> "\\author{" ^ s ^ "}"
  | Margins f -> failwith "Unimplemented"
  | Linespace sp -> failwith "Unimplemented"
  | Indent f -> failwith "Unimplemented"
  | Font (s, i) -> failwith "Unimplemented"
  | HString s -> s
  | HComment s -> "\\begin{comment}\n" ^ s ^ "\n\\end{comment}"

and fold_body exprs = List.fold_left (fun acc expr -> acc ^ expr_to_tex expr) "" exprs

and fold_head exprs = List.fold_left (fun acc expr -> acc ^ head_to_tex expr) "" exprs

and with_newline s =
  let f x = x ^ "   \\\\" in
  Str.(substitute_first (regexp "\n[^\n\r]+.*$") f s)

let write_string_to_file filename str =
  let chan = open_out filename in
  output_string chan str; close_out chan

let write_to_tex doc = let output_str = match doc with
    | (head,body) ->
      "\\documentclass{article}\n" ^
      "\\usepackage{verbatim}\n\n" ^
      fold_head head ^
      "\\date{\\today}" ^
      "\\begin{document}\n\n" ^
      "\\maketitle\n\n" ^
      fold_body body ^
      "\n\\end{document}" in
  write_string_to_file "output.tex" output_str;
  "output.tex"
