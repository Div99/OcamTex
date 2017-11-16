open Ast

val make_head_expr : string -> string -> string -> head_expr

val make_format : int -> int -> format

val make_var : string -> expr

val make_math : string -> expr

val make_text: string -> expr

val make_comment : string -> expr

val make_list : expr list -> expr

val make_table : expr list list -> expr

val make_eq : expr -> expr

val make_inline_eq : expr -> expr

val make_list_item : expr -> expr

val make_matrix : expr list list -> expr

val make_image : expr -> expr
