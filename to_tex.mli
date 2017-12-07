(* write_to_tex [doc] takes an OCamTex document and converts it to a LaTeX
 * document. The name of the file is returned. *)
val write_to_tex : Ast.doc -> string -> string

val expr_to_tex : Ast.expr -> string

val head_to_tex : Ast.head_expr -> string
