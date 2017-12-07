open Ast

let rec expr_to_tex = function
  | String s ->  s
  | Text exprs -> fold_body exprs
  | Math s -> "$" ^ (math_to_tex s) ^ "$"
  | Comment s -> "\\begin{comment}\n" ^ s ^ "\n\\end{comment}"
  | Var s -> "\\" ^ var_to_tex s
  | Cmd ((cmd, style), exprs) -> cmd_to_tex cmd style exprs

and var_to_tex v = match v with
  | "inf" -> "infty"
  | "arrow" -> "rightarrow"
  | "larrow" -> "leftarrow"
  | "lrarrow" -> "leftrightarrow"
  | "Arrow" -> "Rightarrow"
  | "Larrow" -> "Leftarrow"
  | "Lrarrow" -> "Leftrightarrow"
  | "or" -> "lor"
  | "and" -> "land"
  | "union" -> "cup"
  | "intersect" -> "cap"
  | "nats" -> "\\mathbb{N}"
  | "reals" -> "\\mathbb{R}"
  | "ints" -> "\\mathbb{Z}"
  | "rats" -> "\\mathbb{Q}"
  | "del" | "grad" -> "\\nabla"
  | _ -> v

and cmd_to_tex cmd style exprs = match cmd with
  | "list" -> list_to_tex style exprs
  | "image" -> image_to_tex style
  | "section" -> "\\section{" ^ fold_body exprs ^ "}\n"
  | "subsection" -> "\\subsection{" ^ fold_body exprs ^ "}\n"
  | "subsubsection" -> "\\subsubsection{" ^ fold_body exprs ^ "}\n"
  | "equation" | "eqn" -> "$$" ^ fold_body exprs ^ "$$\n"
  | "matrix" -> matrix_to_tex style exprs
  | "table" -> table_to_tex style exprs
  | _ -> "\\" ^ cmd ^ " " ^ fold_body exprs

and math_to_tex = function
  | Math_op s -> "\\" ^ s
  | MathStr s -> s

and math_op_to_tex = function
  | Leaf_op s -> s
  | Unary_op (s, e1) -> s ^ "{" ^ math_to_tex e1 ^ "}"
  | Binary_op (s, e1, e2) -> s ^ "{" ^ math_to_tex e1 ^ "{" ^ math_to_tex e2  ^ "}"

and list_to_tex style exprs =
  let order = if style = None then "itemize" else "enumerate" in
  (* alph, Alph, arabic, roman, Roman *)
  let sty = match style with
    | None -> "\n"
    | Some s -> "[" ^ s ^ "]\n" in
  "\\begin{" ^ order ^ "}" ^ sty ^
  fold_body exprs ^
  "\n\\end{" ^ order ^ "}\n"

and matrix_to_tex style exprs =
  let sty = match style with
    | None -> "matrix"
    | Some "()" -> "pmatrix"
    | Some "[]" -> "bmatrix"
    | Some "{}" -> "Bmatrix"
    | Some "||" -> "vmatrix"
    | Some "||||" -> "Vmatrix"
    | Some _ -> "matrix" in
  "$\\begin{" ^ sty ^ "}" ^ nlt_to_slash (tabs_to_and (fold_body exprs)) ^
  "\\end{" ^ sty ^ "}$"

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

and tabs_to_and = Str.global_replace (Str.regexp "\t") " & "

and nlt_to_slash s = Str.replace_first (Str.regexp "\\\\\\\\\n")
"\n"
(Str.global_replace (Str.regexp "\n") "\\\\\\\n\n" s)

and nl_to_dash s = Str.replace_first (Str.regexp "\\\\\\\\\n")
"\n"
(Str.global_replace (Str.regexp "\n") "\\\\\\\n\\hline\n" s)

and table_to_tex style exprs =
  let columns = (match style with
    | Some style -> (match int_of_string style with
      | n -> n
      | exception e -> 1)
    | None -> 1) in
  "\\begin{tabular}" ^ "{" ^ (table_gen_col columns) ^ "}" ^
  (nl_to_dash (tabs_to_and (fold_body exprs))) ^
  "\\\\\n\\hline\n\\end{tabular}"

and fold_body exprs =
  List.fold_left (fun acc expr -> acc ^ expr_to_tex expr) "" exprs

and fold_head exprs =
  List.fold_left (fun acc expr -> acc ^ head_to_tex expr) "" exprs

and head_to_tex = function
  | Title s -> "\\title{" ^ s ^ "}\n"
  | Author s -> "\\author{" ^ s ^ "}\n"
  | Font s -> "\\usepackage[T1]{fontenc}\n" ^
              "\\usepackage{" ^ s ^ "}\n" (* tgtermes, mathptmx, txfonts *)
  | HComment s -> "\\begin{comment}\n" ^ s ^ "\n\\end{comment}\n"
  | HString s -> s
  | _ -> ""

and make_head exprs = let assocs = List.fold_left (fun acc -> function
    | Margin f -> ("margin", string_of_float f)::acc
    | Fontsize i -> ("font_size", string_of_int i)::acc
(* 8pt, 9pt, 10pt, 11pt, 12pt, 14pt, 17pt, 20pt *)
    | Date s -> ("date", s)::acc
    | Landscape -> ("landscape,", "")::acc
    | _ -> acc) [] exprs in
  let assoc s = List.assoc_opt s assocs in
  let font_size = match assoc "font_size" with
    | Some s -> "[" ^ s ^ "pt]"
    | None -> "" in
  let author = match assoc "date" with
    | Some s -> s
    | None -> "\\today" in
  let landscape = match assoc "landscape," with
    | Some _ -> "landscape, "
    | None -> "" in
  let margin = match assoc "margin" with
    | Some s -> "margin=" ^ s ^ "in"
    | None -> "" in
  "\\documentclass" ^ font_size ^ "{extarticle}\n" ^
  "\\usepackage{graphicx}\n" ^
  "\\usepackage{enumerate}\n" ^
  "\\usepackage{amsmath}\n"^
  "\\usepackage{amsfonts}\n"^
  "\\graphicspath{ {images/} }\n" ^
  "\\usepackage{verbatim}\n" ^
  "\\usepackage{geometry}\n" ^
  "\\geometry{legalpaper, " ^ landscape ^ margin ^ "}\n" ^
  "\\setlength\\parindent{0pt}\n" ^
  fold_head exprs ^
  "\\date{" ^ author ^ "}\n"

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
