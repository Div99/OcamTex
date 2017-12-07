open Parse
open To_tex
open Arg
open Unix

let convert_file str =
  let ast_doc = parse_file str in
  write_to_tex ast_doc str

let filename = try Sys.argv.(1) with _ -> "document.otex"

let basename = let lst = Str.split (Str.regexp ".otex") filename in
  if List.length lst > 1 then failwith "Bad .otex target"
  else (List.hd lst)

let _ = convert_file filename

let _ = print_string ("Converted to " ^ basename ^ ".tex\n")

let _ = system ("pdflatex -jobname=" ^ basename ^ " --interaction=batchmode " ^ basename ^ ".tex")
