(* parse_file [file] takes an OCamTex file and parses it into a document.
 * throws: SyntaxError if [file] cannot be parsed. *)
open ExtLib
open Lexing
open Ast

exception SyntaxError of string

let location_message lexbuf =
  let start = lexeme_start_p lexbuf in
  let finish = lexeme_end_p lexbuf in
  Printf.sprintf "line %d, characters %d-%d"
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    (finish.pos_cnum - finish.pos_bol)

let syntax_error_message lexbuf  =
  Printf.sprintf
    "Syntax error, %s: %s"
    (location_message lexbuf)
    (lexeme lexbuf)

let parse_error lexbuf =
  raise (SyntaxError (syntax_error_message lexbuf))

let unexpected_error msg lexbuf =
  failwith ("Unexpected parsing exception: " ^ msg
            ^ "\noccurred at " ^ (location_message lexbuf))

let string_of_file filename =
 let chan = open_in filename in
 let input = IO.input_channel chan in
 IO.read_all input

let parse_file filename =
  let inx = open_in filename in
  let lexbuf = from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    Parser.parse_expression Lexer.token lexbuf
 (*  ([], [Text "This is text.";
        Comment "This is a comment.";
        Math "This is math.";
        Cmd (List ([Text "Text item 1";
                    Math "Math item 2";
                    Cmd (List ([Text "Nested text item 1";
                                Math "Nested math item 2"], None))], None))]) *)
  with
      | Parser.Error | Lexer.Lexical_error _ -> parse_error lexbuf
      | Failure s -> unexpected_error s lexbuf
