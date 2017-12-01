(**)

(* The type of the entire document *)
type doc = head_expr list * expr list

(* The type of the document header *)
and head_expr =
  | Title of string
  | Author of string
  | Margins of float
  | Linespace of space
  | Indent of float
  | Font of string
  | Fontsize of int
  | HString of string
  | HComment of string

(* The type for determining spacing between lines*)
and space = Single | SingleHalf | Double

(* Expressions making up the document *)
and expr =
  | Cmd of (string * style) * expr list
  | Var of string
  | String of string
  | Math of expr list
  | Text of expr list
  | Comment of string

(* Commands that the user can use
and cmd =
  | List of expr list * style option
  | Table of expr list list
  | Equation of expr
  | InlineEq of expr
  | ListItem of expr
  | Matrix of expr list list
  | Image of string
  | Section of string
  | Subsection of string
  | Subsubsection of string
  | Newpage *)

(* The style of a command *)
and style = string option
