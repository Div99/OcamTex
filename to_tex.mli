(* write_to_tex [doc] takes an OCamTex document and converts it to a LaTeX
 * document. The name of the file is returned. *)
val write_to_tex : Ast.doc -> string -> string

(* expr_to_tex [expr] takes an expr (the body) and converts it into Latex. The
*  body of the Latex file as a string is returned. *)
val expr_to_tex : Ast.expr -> string

(* head_to_tex [head_expr] takes an head_expr and converts it to a Latex head
*  description. This includes packages used and layout specifics. The string of
* the Latex snippet is returned. *)
val head_to_tex : Ast.head_expr -> string
