(******************************************************************)
(* Lexer header *)
(******************************************************************)

{
  open Parser
  open Lexing

  type mode =
    | M
    | T
    | CMD of string * (string option)

  type loc = Lexing.position * Lexing.position

  exception Lexical_error of
    loc (* error segment - start and end pos *)
    * (mode * loc) list (* stack of open modes *)
    * string (* explanation *)

  let loc lexbuf = (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

  let lex_error lexbuf stack s =
    Printf.ksprintf (fun s -> raise (Lexical_error(loc lexbuf, stack, s))) s

(******************************************************************)
(* Helper functions for lexing strings *)
(******************************************************************)

  let st = ref []

  let get_stack () = !st

  let get_mode () =
      match !st with
        | (m,_)::_ -> m
        | [] -> T

  let cmd_begin = ref None

  let set_cmd_begin cmd = (cmd_begin := Some cmd)

  let add_cmd () =
    match !cmd_begin with
    | Some cmd -> cmd_begin := None; CMD_BEGIN cmd
    | None -> STRING ""

  let begin_mode m lexbuf =
      st := (m , loc lexbuf) :: !st;
      match m with
        | M -> MATH_BEGIN
        | T -> TEXT_BEGIN
        | CMD (cmd, style) -> set_cmd_begin (cmd, style); STRING "\n"

  let end_mode lexbuf =
      match !st with
        | (CMD _, _)::_ -> lex_error lexbuf !st "Not valid end of cmd"
        | (m,_)::rem ->
            st := rem;
            ( match m with
              | M -> MATH_END
              | T -> TEXT_END
              | CMD _ -> CMD_END )
        | [] -> lex_error lexbuf !st "Not valid end of mode"

  let end_cmd lexbuf =
    match !st with
     | (CMD _, _)::rem ->
            st := rem; CMD_END
     | _ -> lex_error lexbuf !st "no cmd to end"

  let end_cmd_newline () =
    match !st with
     | (CMD _, _)::rem ->
            st := rem; CMD_END
     | _ -> STRING "\n"

  let get_mode2 () =
     match !st with
       | (m,_)::(m2,_)::_-> m2
       | [] -> T

  let check_mode letter def lexbuf=
     match letter, get_mode2 () with
     | "t", CMD("matrix",_) ->  "\\&"
     | "nt", CMD("matrix",_) ->  "\\\\\t"
     | "n" , CMD("matrix",_) ->(end_mode lexbuf; end_cmd lexbuf; def)
     | _, _ -> def

  let reset_st () =
      st := []

  let top_level () =
      !st = []

  let lex_error lexbuf s = lex_error lexbuf (get_stack ()) s

  let string_buffer = Buffer.create 256

  (*let token_item lexbuf =*)

  let reset_string_buffer () = Buffer.reset string_buffer

  let get_stored_string () = Buffer.contents string_buffer

  let indent_st = ref []

  let curr_level s =
   let l = ref 0 in
	 String.iter (fun c -> if c='\t' then l := !l+1) s; !l

  let levels = ref 0

  let incr_levels () = levels := !levels + 1

  let decr_levels () = levels := !levels - 1

	let change_indent curr_level is_cmd lexbuf =
    new_line lexbuf;
    let f n acc = if curr_level <= n then (incr_levels (); acc)
                  else n::acc in
		if is_cmd then
		 (if !indent_st = [] || curr_level > List.hd !indent_st then
        indent_st := curr_level::(!indent_st)
      else (indent_st := List.fold_right f !indent_st [];
         indent_st := curr_level::(!indent_st)))
		else  indent_st := List.fold_right f !indent_st []

  let comment_buf = Buffer.create 128

  let comment_nests = ref 0

  let start_comment () =
    incr comment_nests

  (* Close the current comment. If we are still in a comment, raise Exit.
     Else, return a COMMENT token containing the whole comment. *)
  let end_comment () =
    decr comment_nests;
    if !comment_nests >= 1 then raise Exit;
    let s = Buffer.contents comment_buf in
    Buffer.reset comment_buf;
    COMMENT s

  let latex_buf = Buffer.create 128

  let end_latex () =
    let s = Buffer.contents latex_buf in
    Buffer.reset latex_buf;
    LATEX s

  let token_return lexbuf =
    new_line lexbuf;
    STRING "\n" (* to keep the line count correct *)

  let head = ref true

  let is_head () = !head

  let end_head () = head := false

  let reset_head () = head := true

  let math_cmd = ref false

  let open_math lexbuf =
  match get_mode () with
  | CMD (cmd, _) ->
        (match cmd with
          | "matrix" | "eqn" | "equation" -> math_cmd := false; begin_mode M lexbuf
          | _ -> lex_error lexbuf "Not a valid math mode command")
  | _ -> lex_error lexbuf "Not a valid math mode command"


}

(******************************************************************)
(* Lexer body *)
(******************************************************************)


let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = ('_' | letter) ('_' | letter | digit)*

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z']

rule head = parse
  | "|HEAD" { HEAD }
  | "|BODY" { end_head (); BODY }
  | "|title" white ([^ '\n' '/']+ as c)   {TITLE c}
  | "|author" white ([^ '\n' '/']+ as c)   {AUTHOR c}
  | "|font" white ([^ '\n']+ as c) {FONT c}
  | "|fontsize" white (digit+ as c) {FONTSIZE (int_of_string c)}
  | "```" { latex lexbuf }
  | '\n' { token_return lexbuf }
  | ':' (id as v)  { VAR v }
  | '#' { STRING "\\#" }
  | '_' { STRING "\\_" }
  | '%' { STRING "\\%" }

  | "\\\\" { STRING "\\\\" }
  | "\\{" { STRING "\\{" }
  | "\\}" { STRING "\\}" }
  | "\\$" { STRING "\\$" }
  | "\\\"" { STRING "\"" }
  | "\\&" { STRING "\\&" }
  | "\\ " { STRING "\\ " }
  | "\\'" { STRING "\\'" }
  | "\\`" { STRING "\\`" }

  | '\\' [^ '\\' '{' '}' '$' '"' '&' ' ']
      { lex_error lexbuf "invalid escaping in head" }

  | "/*" { start_comment (); comment lexbuf }
  | "//" ([^'\n' '\r']* as c)
      { start_comment (); Buffer.add_string comment_buf c;
        end_comment () }
  | '(' { STRING "(" }

  | [^ '"' '$' '{' '<' '\n' '\\' '#' '_' '^' '}' '%' '(' '/' '|']+
      { STRING(lexeme lexbuf) }
  | '|' ([^ '\n' ' ']+ as s)  { lex_error lexbuf "Unknown tag in head '%s'" s}
  | (_ as c) { lex_error lexbuf "Unexpected char in head mode '%c'" c}
  | '\n' (' ')+ {lex_error lexbuf "non-tab indent"}
  | eof { lex_error lexbuf "no body given" }

and text = parse
  | "|m [" { begin_mode M lexbuf }
  | '\n' ('\t')* "|m [" { new_line lexbuf; begin_mode M lexbuf }
  | '\n' { new_line lexbuf; STRING "        \\\\\n"}
  | "```" { latex lexbuf }
  | ('\n' ('\t')* as c) '|' (id as apply) ' '* "->" ' '* ([^'\n']+ as style)
      { change_indent (curr_level c) true lexbuf; begin_mode (CMD (apply, Some style)) lexbuf }
  | ('\n' ('\t')* as c) '|' (id as apply)
      { change_indent (curr_level c) true lexbuf; begin_mode (CMD (apply, None)) lexbuf}
  | "/*" { start_comment (); comment lexbuf }
  | "//" ([^'\n' '\r']* as c)
      { start_comment (); Buffer.add_string comment_buf c;
        end_comment () }
  | ':' (id as v) { VAR v }
  | '#' { STRING "\\#" }
  | '_' { STRING "\\_" }
  | '%' { STRING "\\%" }
  | "\\\\" { STRING "\\\\" }
  | "\\{" { STRING "\\{" }
  | "\\}" { STRING "\\}" }
  | "\\$" { STRING "\\$" }
  | "\\\"" { STRING "\"" }
  | '&' { STRING "\\$" }
  | "\\ " { STRING "\\ " }
  | "\\'" { STRING "\\'" }
  | "\\`" { STRING "\\`" }
  | '\\' [^ '\\' '{' '}' '$' '"' '&' ' ']
      { lex_error lexbuf "invalid escaping in text mode" }
  | '(' { STRING "(" }
  | [^ '"' '$' '{' '<' '\n' '\\' '#' '_' '^' '}' '%' '(' '/' '|' '[' ']']+
      { STRING(lexeme lexbuf) }
  | (_ as c) { lex_error lexbuf "Unexpected char in text mode '%c'" c}
  | '\n' (' ')+ {lex_error lexbuf "non-tab indent"}
  | eof { if top_level () then EOF else
          lex_error lexbuf "unexpected end of file in text mode" }

and comment = parse
  | "*/" { try end_comment () with Exit -> comment lexbuf }
  | "/*" { start_comment (); comment lexbuf }
  | '\n' { new_line lexbuf; Buffer.add_char comment_buf '\n'; comment lexbuf }
  | "\\\"" { Buffer.add_char comment_buf '"'; comment lexbuf }
  | (_ as c) { Buffer.add_char comment_buf c; comment lexbuf }
  | eof { lex_error lexbuf "unexpected end of file in comment" }

and latex = parse
  | "```" { try end_latex () with Exit -> latex lexbuf }
  | '\n' { new_line lexbuf; Buffer.add_char latex_buf '\n'; latex lexbuf }
  | "\\\"" { Buffer.add_char latex_buf '"'; latex lexbuf }
  | (_ as c) { Buffer.add_char latex_buf c; latex lexbuf }
  | eof { lex_error lexbuf "unexpected end of file in latex" }

and math = parse
  | "|t [" { begin_mode T lexbuf }
  | ']' {end_mode lexbuf}
  | '\t' { STRING "\\&"}
  | "\n\t" {STRING (check_mode "nt" "\n\\t" lexbuf)}
  | '\n' {new_line lexbuf; STRING (check_mode "n" "\n" lexbuf)}
  | ':' (id as v) { VAR v }
  | '`' (id as op) { MATH_OP op }
  | "<=" { MATH_OP "leq" }
  | ">=" { MATH_OP "geq" }
  | "!=" { MATH_OP "ne" }
  | '%' { STRING "\\%" }
  | "\\\\" { STRING "\\\\" }
  | "\\{" { STRING "\\{}" }
  | "\\}" { STRING "\\}" }
  | "\\$" { STRING "\\$" }
  | "\\\"" { STRING "\"" }
  | "\\ " { STRING "\\ " }
  | "\\_" { STRING "\\_" }
  | ' ' { STRING " " }
  | ':' (['a'-'z' 'A'-'Z']+ as c) {STRING ("\\" ^ c)}
  | '\\' [^ '\\' '{' '}' '$' '"' '&' ' ' '_']
      { lex_error lexbuf "invalid escaping in math mode" }

  | "/*" { start_comment (); comment lexbuf }
  | "//" ([^'\n' '\r']* as c)
      { start_comment (); Buffer.add_string comment_buf c;
        end_comment () }
  | "```" { latex lexbuf }
  | '(' { STRING "(" }

  | [^ '!' ' ' '"' '$' '{' '\n' '\\' '}' '%' '(' '|' '[' ']' ':' '`' ]+ { STRING(lexeme lexbuf) }
  | (_ as c) { lex_error lexbuf "Unexpected char in math mode '%c'" c}
  | '\n' (' ')+ {lex_error lexbuf "non-tab indent"}
  | eof { lex_error lexbuf "unexpected end of file in math mode" }

and command = parse
  | "|m [" { begin_mode M lexbuf }
  | "```" { latex lexbuf }
  | '\n' ('\t')* "```" { new_line lexbuf; latex lexbuf }
  | ('\n' ('\t')* as c) "|m" { change_indent (curr_level c) false lexbuf;
                               begin_mode M lexbuf }
  | "|t [" { begin_mode T lexbuf }
  | ('\n' ('\t')* as c) "|t" { change_indent (curr_level c) false lexbuf;
                               begin_mode T lexbuf }
  | ']' {end_mode lexbuf}
  | ('\n' ('\t')* as c) '-'
      { change_indent (curr_level c) true lexbuf; begin_mode (CMD ("item", None)) lexbuf}
  | '\n' ('\t')*  "|end" { new_line lexbuf; end_cmd lexbuf }
  | "|END" { end_cmd lexbuf }
  | ('\n' ('\t')* as c) '|' (id as apply) ' '* "->" ' '* ([^'\n']+ as style)
      { change_indent (curr_level c) true lexbuf; begin_mode (CMD (apply, Some style)) lexbuf }
  | ('\n' ('\t')* as c) '|' (id as apply)
      { change_indent (curr_level c) true lexbuf; begin_mode (CMD (apply, None)) lexbuf}
  | ('\n' ('\t')* as c)  { change_indent (curr_level c) false lexbuf;
                           STRING "\n" }
  | ':' (id as v) { VAR v }
  | "/*" { start_comment (); comment lexbuf }
  | "//" ([^'\n' '\r']* as c)
      { start_comment (); Buffer.add_string comment_buf c;
        end_comment () }
  | '\n' ('\t')* "/*" { new_line lexbuf; start_comment (); comment lexbuf }
  | '\n' ('\t')* "//" ([^'\n' '\r']* as c)
      { new_line lexbuf; start_comment (); Buffer.add_string comment_buf c;
        end_comment () }
  | '#' { STRING "\\#" }
  | '_' { STRING "\\_" }
  | '%' { STRING "\\%" }
  | "\\\\" { STRING "\\\\" }
  | "\\{" { STRING "\\{" }
  | "\\}" { STRING "\\}" }
  | "\\$" { STRING "\\$" }
  | "\\\"" { STRING "\"" }
  | '&' { STRING "\\&" }
  | "\\ " { STRING "\\ " }
  | "\\'" { STRING "\\'" }
  | "\\`" { STRING "\\`" }
  | '\\' [^ '\\' '{' '}' '$' '"' '&' ' ']
      { lex_error lexbuf "invalid escaping in command mode" }
  | '(' { STRING "(" }
  | [^ '"' '$' '{' '<' '\n' '\\' '#' '_' '^' '}' '%' '(' '/' '|' '[' ']' '-']+
      { STRING(lexeme lexbuf) }
  | (_ as c) { lex_error lexbuf "Unexpected char in cmd mode '%c'" c}
  | '\n' (' ')+ {lex_error lexbuf "non-tab indent"}
  | eof { lex_error lexbuf "unexpected end of file in command mode" }

{
  let token lexbuf =
    if is_head () then head lexbuf
    else if !levels > 0 then (decr_levels (); end_cmd_newline ())
    else if !cmd_begin <> None then add_cmd ()
    else if !math_cmd then open_math lexbuf
    else match get_mode () with
      | M -> math lexbuf
      | T -> text lexbuf
      | CMD _ -> command lexbuf
}
