open Ast
open Parse
open To_tex
open Arg

let convert_file str =
  let ast_doc = parse_file str in
  write_to_file ast_doc

let output_file = convert_file "test.otex"

let _ = print_string ("Converted to " ^ output_file ^ "\n")