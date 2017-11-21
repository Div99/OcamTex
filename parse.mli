exception SyntaxError of string

(* parse_file [file] takes an OCamTex file and parses it into a document.
 * throws: SyntaxError if [file] cannot be parsed. *)
val parse_file : string -> Ast.doc

val string_of_file : string -> string