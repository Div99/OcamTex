(**)

(* The type of the entire document *)
type doc = head_expr list * expr list

(* The type of the document header *)
and head_expr =
  | Title of string option
  | Author of string option
  | Margins of float
  | Linespace of space
  | Indent of float
  | Font of string * int

(* The type for determining spacing between lines*)
and space = Single | SingleHalf | Double

(* Expressions making up the document *)
and expr =
  | Cmd of cmd * style option * expr list
  | Var of string
  | Math of string
  | Text of string
  | Comment of string

(* Commands that the user can use *)
and cmd =
  | List of expr list
  | Table of expr list list
  | Equation of expr
  | InlineEq of expr
  | ListItem of expr
  | Matrix of expr list list
  | Image of string
  | Section of string
  | Subsection of string
  | Subsubsection of string
  | Newpage

(* The style of a command *)
and style = string list