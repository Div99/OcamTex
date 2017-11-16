(* The type of the abstract syntax tree (AST). *)
type doc = head_expr option * format * expr list

and head_expr = title option * author option * metadata option

and title = string

and author = string

and metadata = string

and format = margins * spacing

and margins = int

and spacing = int

and expr =
  | Cmd of cmd * style option * expr list
  | Var of string
  | Math of string
  | Text of string
  | Comment of string

and cmd =
  | List of expr list
  | Table of expr list list
  | Equation of expr
  | InlineEq of expr
  | ListItem of expr
  | Matrix of expr list list
  | Image of expr

and style = string
