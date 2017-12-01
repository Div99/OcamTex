open Ast

let rec expr_to_tex = function
  | String s -> s
  | Text exprs -> fold_body exprs
  | Math exprs -> "$" ^ fold_body exprs ^ "$"
  | Comment s -> "\\begin{comment}\n" ^ s ^ "\n\\end{comment}"
  | Var s -> failwith "Unimplemented"
  | Cmd ((cmd, style), exprs) -> cmd_to_tex cmd style exprs

and cmd_to_tex cmd style exprs = match cmd with
  | "list" -> list_to_tex style exprs
  | "image" -> image_to_tex style
  | "section" -> "\\section{" ^ fold_body exprs ^ "}"
  | "subsection" -> "\\subsection{" ^ fold_body exprs ^ "}"
  | "subsubsection" -> "\\subsubsection{" ^ fold_body exprs ^ "}"
  | _ -> "\\" ^ cmd ^ " " ^ fold_body exprs

and list_to_tex style exprs =
  let order = if style = None then "itemize" else "enumerate" in
  let sty = match style with
    | None -> ""
    | Some s -> "[label=(\\" ^ s ^ "*)]" in
  "\\begin{" ^ order ^ "}" ^ sty ^
  fold_body exprs ^
  "\n\\end{" ^ order ^ "}"

and image_to_tex style = match style with
  | Some s -> let s = Str.split (Str.regexp ", +") s in (match s with
      | [img;width] -> "\\includegraphics[width=" ^ width ^ "\\textwidth]{"^ img ^"}"
      | [img] -> "\\includegraphics[width=.5\\textwidth]{"^ img ^"}"
      | _ -> "[Bad image]")
  | None -> "[Bad image]"

and table_gen_col = function
  | 0 -> ""
  | 1 -> "|c|"
  | n -> "|c" ^ (table_gen_col (n-1))

and table_to_tex style exprs =
  let columns = (match int_of_string style with
    | n -> n
    | _ -> 1) in
  "\\begin{tabular}" ^ (table_gen_col columns) ^
  fold_body exprs ^
  "\n\\end{tabular}"

and fold_body exprs =
  List.fold_left (fun acc expr -> acc ^ expr_to_tex expr) "" exprs

and fold_head exprs =
  List.fold_left (fun acc expr -> acc ^ head_to_tex expr) "" exprs

and head_to_tex = function
  | Title s -> "\\title{" ^ s ^ "}\n"
  | Author s -> "\\author{" ^ s ^ "}\n"
  | Font s -> "\\usepackage[T1]{fontenc}\n" ^
                   "\\usepackage{" ^ s ^ "}\n" (* tgtermes, mathptmx, txfonts *)
  | HString s -> s
  | HComment s -> "\\begin{comment}\n" ^ s ^ "\n\\end{comment}\n"
  | _ -> ""

and make_head exprs = let assocs = List.fold_left (fun acc -> function
    | Margins f -> ("margins", string_of_float f)::acc
    | Linespace sp -> failwith "Unimplemented"
    | Indent f -> ("indent", string_of_float f)::acc
    | Fontsize i -> ("font_size", string_of_int i)::acc
(* 8pt, 9pt, 10pt, 11pt, 12pt, 14pt, 17pt, 20pt *)
    | _ -> acc) [] exprs in
  let font_size = match List.assoc_opt "font_size" assocs with
    | Some s -> "[" ^ s ^ "pt]"
    | None -> "" in
  "\\documentclass" ^ font_size ^ "{article}\n" ^
  "\\usepackage{graphicx}\n" ^
  "\\usepackage{enumitem}\n" ^
  "\\graphicspath{ {images/} }\n" ^
  "\\usepackage{verbatim}\n\n" ^
  fold_head exprs ^
  "\\date{\\today}\n"

and with_newline s =
  let f x = x ^ "   \\\\" in
  Str.(substitute_first (regexp "\n[^\n\r]+.*$") f s)

let write_string_to_file filename str =
  let chan = open_out filename in
  output_string chan str; close_out chan

let get_filename str =
  let n = String.length str in
  String.sub str 0 (n-4)

let write_to_tex doc str = let output_str = match doc with
    | (head,body) ->
      make_head head ^
      "\\begin{document}\n\n" ^
      "\\maketitle\n\n" ^
      fold_body body ^
      "\n\\end{document}" in
  let file = ((get_filename str) ^ "tex") in
  write_string_to_file file output_str; file
