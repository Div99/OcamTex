(******************************************************************)
(* Lexer header *)
(******************************************************************)

{
  open Parser
  open Lexing

  type mode =
    | A
    | M
    | T
    | CMD of string * (string option)

  type loc = Lexing.position * Lexing.position

  exception Lexical_error of
    loc (* error segment - start and end pos *)
    * (mode * loc) list (* stack of open modes *)
    * string (* explanation *)

  (* loc [lexbuf] returns tuple of start and end of lexbuf *)
  let loc lexbuf = (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

  (* lex_error [lexbuf stack s] prints out a lexer error *)
  let lex_error lexbuf stack s =
    Printf.ksprintf (fun s -> raise (Lexical_error(loc lexbuf, stack, s))) s

(******************************************************************)
(* Helper functions for lexing strings *)
(******************************************************************)

  let st = ref []

  let get_stack () = !st

  let levels = ref []

  (* add_level [m] appends [m] to the end of levels *)
  let add_level m = levels := !levels @ [m]

  (* add_new_line [())] adds a new level of new line *)
  let add_new_line () = add_level (STRING "\n")

  (* decr_levels [lexbuf] pops off the top level to decrease*)
  let decr_levels lexbuf =
    match !levels with
    | [] -> lex_error lexbuf !st "can't decrease levels"
    | h::t -> levels := t; h

  (* get_mode () returns the current mode based on the stack [st]. If the stack
   * is empty, returns T for text mode. *)
  let get_mode () =
      match !st with
        | (m,_)::_ -> m
        | [] -> A

  (* begin_mode [m lexbuf] appends the mode to the stack [st] and matches [m]
   * to the proper parser token. *)
  let begin_mode m lexbuf =
      st := (m , loc lexbuf) :: !st;
      match m with
        | A -> lex_error lexbuf !st "not valid start of auto mode"
        | M -> MATH_BEGIN
        | T -> TEXT_BEGIN
        | CMD (cmd, style) ->
            add_new_line (); add_level (CMD_BEGIN (cmd, style)); STRING ""


  (* end_mode [lexbuf] matches the top of the stack [st] with the proper mode
   * and returns the correct ending token for the mode. Does not end commands.*)
  let end_mode lexbuf =
      match !st with
        | (CMD _, _)::_ -> lex_error lexbuf !st "Not valid end of cmd"
        | (m,_)::rem ->
            st := rem;
            ( match m with
              | A -> lex_error lexbuf !st "not valid end of auto mode"
              | M -> MATH_END
              | T -> TEXT_END
              | CMD _ -> CMD_END )
        | [] -> lex_error lexbuf !st "Not valid end of mode"

  (* end_cmd [lexbuf] matches the command on the top of the stack [st] with the
   * proper command and returns the correct ending token for the command. *)
  let end_cmd lexbuf =
    match !st with
     | (CMD _, _)::rem ->
            st := rem; CMD_END
     | _ -> lex_error lexbuf !st "no cmd to end"

  (* end_cmd_newline () matches the command on the top of the stack [st]
   * with the proper command and returns the correct ending token for the mode  or
   * returns a new line *)
  let end_cmd_newline () =
    match !st with
     | (CMD _, _)::rem ->
            st := rem; CMD_END
     | _ -> STRING "\n"

  (* reset_st [()] resets state [st]*)
  let reset_st () =  st := []

  (* top_level [()] returns true if [st] is empty, false otherwise *)
  let top_level () = !st = []

  (* lex_error [lexbuf s] calls lex_error to print out an error *)
  let lex_error lexbuf s = lex_error lexbuf (get_stack ()) s

  let string_buffer = Buffer.create 256

  (* reset_string_buffer [()] resets the string_buffer *)
  let reset_string_buffer () = Buffer.reset string_buffer

  (* get_stored_string [()] gets the contents of the string buffer *)
  let get_stored_string () = Buffer.contents string_buffer

  let indent_st = ref []

  (* curr_level [s] gets the current level based on the number of tabs in [s] *)
  let curr_level s =
   let l = ref 0 in
	 String.iter (fun c -> if c='\t' then l := !l+1) s; !l

  (* end_cmd_level [()] gets the current nested level of commands based on the
   * number of tabs in [s] *)
  let end_cmd_level () =
    match !st with
     | (CMD _, _)::rem ->  st := rem; add_level CMD_END
     | _ -> ()

  (* change_indent [curr_level is_cmd lexbuf] will update the stack controlling
   * the indentation level of each nested command*)
  let change_indent curr_level is_cmd lexbuf =
    new_line lexbuf;
    let f n acc = if curr_level <= n then (end_cmd_level (); acc) else n::acc in
    if is_cmd then
        (if !indent_st = [] || curr_level > List.hd !indent_st then
        indent_st := curr_level::(!indent_st)
    else (indent_st := List.fold_right f !indent_st [];
        indent_st := curr_level::(!indent_st)))
        else indent_st := List.fold_right f !indent_st []

  let comment_buf = Buffer.create 128

  let comment_nests = ref 0

  (* start_comment [()] increases the comment_nests var whenever a comment is
   * started *)
  let start_comment () =
    incr comment_nests

  (* end_comment [()] closes the current comment. If still in a comment, raise
   * Exit, else returns a COMMENT token with the whole comment. *)
  let end_comment () =
    decr comment_nests;
    if !comment_nests >= 1 then raise Exit;
    let s = Buffer.contents comment_buf in
    Buffer.reset comment_buf;
    COMMENT s

  let latex_buf = Buffer.create 128

  (* end_latex [()] resets the latex_buffer. *)
  let end_latex () =
    let s = Buffer.contents latex_buf in
    Buffer.reset latex_buf;
    LATEX s

  (* token_return [lexbuf] adds a new line to the lexbuf *)
  let token_return lexbuf =
    new_line lexbuf;
    STRING "\\\\\n"

  let head = ref true

  (* is_head [()] returns the value within the head reference *)
  let is_head () = !head

  (* end_head [()] sets head reference to false *)
  let end_head () = head := false

  (* reset_head [()] sets head refereence to true*)
  let reset_head () = head := true

}

(******************************************************************)
(* Lexer body *)
(******************************************************************)


let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ '.' digit*
let letter = ['a'-'z' 'A'-'Z']
let id = ('_' | letter) ('_' | letter | digit)*
let op = ('_' | letter)+
let lowercase = ['a'-'z']

rule head = parse
  | "|HEAD" { HEAD }
  | "|BODY" { end_head (); BODY }
  | "|title" white ([^ '\n' '/']+ as c) {TITLE c}
  | "|author" white ([^ '\n' '/']+ as c) {AUTHOR c}
  | "|date" white ([^ '\n']+ as c) {DATE c}
  | "|margins" white ([^ '\n' '/']+ as c) {MARGIN (float_of_string c)}
  | "|landscape" {LANDSCAPE}
  | "|font" white ([^ '\n']+ as c) {FONT c}
  | "|fontsize" white (digit+ as c) {FONTSIZE (int_of_string c)}
  | "```" { latex lexbuf }
  | '\n' { new_line lexbuf; STRING "\n" }
  | ':' (id as v)  { VAR v }
  | '#' { STRING "\\#" }
  | '_' { STRING "\\_" }
  | '%' { STRING "\\%" }
  | '/' { STRING "/" }
  | '\\' [^ '\\' '{' '}' '$' '"' '&' ' ']
      { lex_error lexbuf "invalid escaping in head" }

  | "/*" { start_comment (); comment lexbuf }
  | "//" ([^'\n' '\r']* as c)
      { start_comment (); Buffer.add_string comment_buf c;
        end_comment () }
  | '(' { STRING "(" }
  | '\t' {STRING ""}
  | [^ '"' '$' '{' '<' '\n' '\\' '#' '_' '^' '}' '%' '(' '/' '|' '\t']+
      { STRING(lexeme lexbuf) }
  | '|' ([^ '\n' ' ']+ as s)  { lex_error lexbuf "Unknown tag in head '%s'" s}
  | (_ as c) { lex_error lexbuf "Unexpected char in head mode '%c'" c}
  | '\n' (' ')+ {lex_error lexbuf "non-tab indent"}
  | eof { lex_error lexbuf "no body given" }

and auto = parse
  | "|m [" { begin_mode M lexbuf }
  | '\n' ('\t')* "|m [" { add_level (begin_mode M lexbuf); token_return lexbuf }
  | "|t [" { begin_mode M lexbuf }
  | '\n' ('\t')* "|t [" { add_level (begin_mode T lexbuf); token_return lexbuf }
  | '\n' { new_line lexbuf; STRING "\\\\\n"}
  | "```" { latex lexbuf }
  | ('\n' ('\t')* as c) '|' (id as apply) ' '* "->" ' '* ([^'\n' '/']+ as style)
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
  | '\\' [^ '\\' '{' '}' '$' '"' '&' ' ']
      { lex_error lexbuf "invalid escaping in text mode" }
  | '(' { STRING "(" }
  | ')' { STRING ")" }
  | '.' { STRING "." }
  | ' ' { STRING " " }
  | 'I' {STRING "I" }
  | 'A' {STRING "A" }
  | ':' (id as v) { VAR v }
  | '`' (op as n) { MATH_OP n }
  | "<=" { MATH_OP "leq" }
  | ">=" { MATH_OP "geq" }
  | "!=" { MATH_OP "ne" }
  | "lim" { MATH_OP "limit" }
  | ',' { COMMA }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '\t' {STRING "\t"}
  | '/' { STRING "/" }
  | letter letter+ {STRING (lexeme lexbuf) }
  | float { MATH (lexeme lexbuf) }
  | [^ '"' '$' '{' '\n' '\\' '#' '}' '%' '\t' '/' ',' '|' '[' ']' '.' ' ']+
      { MATH (lexeme lexbuf) }
  | (_ as c) { lex_error lexbuf "Unexpected char in text mode '%c'" c}
  | '\n' (' ')+ {lex_error lexbuf "non-tab indent"}
  | eof { if top_level () then EOF else
          lex_error lexbuf "unexpected end of file in text mode" }

 and text = parse
   | "|m [" { begin_mode M lexbuf }
   | '\n' ('\t')* "|m [" { add_level (begin_mode M lexbuf); token_return lexbuf }
   | '\n' { new_line lexbuf; STRING "\\\\\n"}
   | ('\n' ('\t')* as c) '|' (id as apply) ' '* "->" ' '* ([^'\n' '/']+ as style)
       { change_indent (curr_level c) true lexbuf; begin_mode (CMD (apply, Some style)) lexbuf }
   | ('\n' ('\t')* as c) '|' (id as apply)
       { change_indent (curr_level c) true lexbuf; begin_mode (CMD (apply, None)) lexbuf}
   | "/*" { start_comment (); comment lexbuf }
   | "//" ([^'\n' '\r']* as c)
       { start_comment (); Buffer.add_string comment_buf c;
         end_comment () }
   | "```" { latex lexbuf }
   | '#' { STRING "\\#" }
   | '_' { STRING "\\_" }
   | '\\' [^ '\\' '{' '}' '$' '"' '&' ' ']
       { lex_error lexbuf "invalid escaping in text mode" }
   | '(' { STRING "(" }
   | '/' { STRING "/" }
   | ']' {end_mode lexbuf}
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
  | '\n' ('\t')* "|t [" { add_level (begin_mode T lexbuf); token_return lexbuf }
  | ('\n' ('\t')* as c) '|' (id as apply) ' '* "->" ' '* ([^'\n' '/']+ as style)
       { change_indent (curr_level c) true lexbuf; begin_mode (CMD (apply, Some style)) lexbuf }
  | ('\n' ('\t')* as c) '|' (id as apply)
       { change_indent (curr_level c) true lexbuf; begin_mode (CMD (apply, None)) lexbuf}
  | ']' {end_mode lexbuf}
  | '\n' {new_line lexbuf; STRING "\n"}
  | ':' (id as v) { VAR v }
  | '`' (id as op) { MATH_OP op }
  | "<=" { MATH_OP "leq" }
  | ">=" { MATH_OP "geq" }
  | "!=" { MATH_OP "ne" }
  | '%' { STRING "\\%" }
  | ' ' { STRING " " }
  | '`' (op as n) { MATH_OP n }
  | _  { STRING (lexeme lexbuf) }
  | "```" { latex lexbuf }
  | '\n' (' ')+ {lex_error lexbuf "non-tab indent"}
  | eof { lex_error lexbuf "unexpected end of file in math mode" }

and command = parse
  | "|m [" { begin_mode M lexbuf }
  | "```" { latex lexbuf }
  | '\n' ('\t')* "```" { new_line lexbuf; add_new_line (); latex lexbuf }
  | ('\n' ('\t')* as c) "|m" { change_indent (curr_level c) false lexbuf;
                               add_level (begin_mode M lexbuf); token_return lexbuf }
  | "|t [" { begin_mode T lexbuf }
  | ('\n' ('\t')* as c) "|t" { change_indent (curr_level c) false lexbuf;
                               add_level (begin_mode M lexbuf); token_return lexbuf }
  | ']' {end_mode lexbuf}
  | ('\n' ('\t')* as c) '-'
      { change_indent (curr_level c) true lexbuf; begin_mode (CMD ("item", None)) lexbuf}
  | '\n' ('\t')*  "|end" { new_line lexbuf; add_new_line (); end_cmd lexbuf }
  | "|END" { end_cmd lexbuf }
  | ('\n' ('\t')* as c) '|' (id as apply) ' '* "->" ' '* ([^'\n' '/']+ as style)
      { change_indent (curr_level c) true lexbuf; begin_mode (CMD (apply, Some style)) lexbuf }
  | ('\n' ('\t')* as c) '|' (id as apply)
      { change_indent (curr_level c) true lexbuf; begin_mode (CMD (apply, None)) lexbuf}
  | ('\n' ('\t')* as c)  { change_indent (curr_level c) false lexbuf;
                           add_new_line (); (STRING "") }
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
    else if List.length !levels > 0 then decr_levels lexbuf
    else match get_mode () with
      | A -> auto lexbuf
      | M -> math lexbuf
      | T -> text lexbuf
      | CMD _ -> command lexbuf
}
