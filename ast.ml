(**)

(* The type of the entire document *)
type doc = head_expr list * expr list

(* The type of the document header *)
and head_expr =
  | Title of string
  | Author of string
  | Date of string
  | Margin of float
  | Landscape
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
  | Math of math list
  | Text of expr list
  | Comment of string

and math =
  | Math_op of string
  | MathStr of string
  | Expr of expr


(* The style of a command *)
and style = string option
