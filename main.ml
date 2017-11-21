open Ast
open Parse
open To_tex

let convert_file str =
  let ast_doc = parse_file str in
  write_to_file ast_doc