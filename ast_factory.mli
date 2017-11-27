open Ast

(* Header and formatting - functions that make head expressions *)
(* [make_title str] represents the title [str] *)
val make_title : string -> head_expr

(* [make_author str] represents the author [str] *)
val make_author : string -> head_expr

(* [make_margins flt] represents margin of [flt]
 * requires: [flt >= 0] *)
val make_margins : float -> head_expr

(* [make_linespace str] represents the line space option of [str]
 * requires: [str] is one of "single", "double", "singlehalf" *)
val make_linespace : string -> head_expr

(* [make_indent flt] represents the indentation width of [flt]
 * requires: [flt] >= 0 *)
val make_indent : float -> head_expr

(* [make_author str] represents the author [str] *)
val make_font : string * int -> head_expr

(* Expressions *)

(* [make_var str] represents the variable [str] *)
val make_var : string -> expr

(* [make_string str] represents the text [str] in math mode *)
val make_string : string -> expr

(* [make_math expr_list] represents the string made up of [expr_list] in math mode *)
val make_math : expr list -> expr

(* [make_text expr_list] represents the plain text made of [expr_list] *)
val make_text: expr list -> expr

(* [make_comment str] represents the commend [str] *)
val make_comment : string -> expr

(* Commands *)
(* [make_command cmd style [x1; ...; xn]] is the command expression of [cmd style exprlst] *)
val make_cmd : string -> string option -> expr list -> expr

(* [make_list [x1; ...; xn]] represents the list [x1; ...; xn] *)
val make_list : expr list -> cmd

(* [make_table [x1; ...; xn]] represents the table [x1; ...; xn] *)
val make_table : expr list list -> cmd

val make_eq : expr -> cmd

(* [make_inline_eq exp] represents the inline equation [exp] *)
val make_inline_eq : expr -> cmd

(* [make_list_item exp] represents the list item [exp] *)
val make_list_item : expr -> cmd

(* [make_matrix [x1; ...; xn]] represents the matrix [x1; ...; xn] *)
val make_matrix : expr list list -> cmd

(* [make_image str] represents the image [str] *)
val make_image : string -> cmd

(* [make_ection str] represents the section [str] *)
val make_section : string -> cmd

(* [make_subsection str] represents the subsection [str] *)
val make_subsection : string -> cmd

(* [make_subsubsection str] represents the subsubsection [str] *)
val make_subsubsection : string -> cmd

(* [make_newpage] represents the newpage command *)
val make_newpage : unit -> cmd
