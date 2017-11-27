exception SyntaxError of string

(* parse_file [file] takes an OCamTex file and parses it into a document.
 * throws: SyntaxError if [file] cannot be parsed. *)
val parse_file : string -> Ast.doc

val parse_expr : string -> Ast.expr

val parse_head_expr : string -> Ast.head_expr
