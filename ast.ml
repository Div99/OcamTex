(* The type of the abstract syntax tree (AST). *)
type section =
  head_expr * expr list (* Foot | Config*)
  (* need to define foot and config types *)

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
