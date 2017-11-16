(* The type of the abstract syntax tree (AST). *)
type doc = (Head of head_expr * Body of expr list * Foot)
           | Config

and head_expr =
  | Title of string option
  | Author of string option
  | MetaData

and expr =
  | Cmd of cmd * format * expr list
  | Var of string
  | Math of expr list
  | Text of string
  | Comment of string

and cmd =
  | List
  | Table
  | Equation
  | InlineEq
  | ListItem
  | Matrix
  | Image

and format =
  | Style of string
