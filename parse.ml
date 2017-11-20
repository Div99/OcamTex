(* parse_file [file] takes an OCamTex file and parses it into a document.
 * throws: SyntaxError if [file] cannot be parsed. *)
open ExtLib

exception SyntaxError of string
 
let parse_file _ = failwith "Unimplemented"

let string_of_file filename = 
 let chan = open_in filename in
 let input = IO.input_channel chan in
 IO.read_all input