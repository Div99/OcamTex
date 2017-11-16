(* write_to_file [doc] takes an OCamTex document and converts it to a LaTeX
 * document. The name of the file is returned. *)
val write_to_file : Ast.doc -> string
