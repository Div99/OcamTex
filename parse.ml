(* parse_file [file] takes an OCamTex file and parses it into a document.
 * throws: SyntaxError if [file] cannot be parsed. *)
open ExtLib
open Lexing
open Ast
open Format
open Lexer

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

let dir = ref ""

  (* Let's call "commands" expressions of type
      [Format.formatter -> (Format.formatter -> 'k) ->'k],
      with the intention that a command prints something
      into the formatter and then gives it to its continuation.
      Commands can be given a monoid structure which we
      write [(<:>)] , [unit] (the definitions are much in the spirit
      of the continuation monad in Haskell). With [a<:>b]
      executing [a] first, then [b], then its continuation and
      [unit] doing nothing and executes its continuation.
  *)
  let (<:>) a b k fmt = a (fun fmt' -> b k fmt') fmt
  let unit k fmt = k fmt
  (* [flush fmt] executes [cmd] on the formatter [fmt] then flushes
      [fmt]. *)
  let flush fmt cmd = cmd (fun fmt' -> fprintf fmt' "@.") fmt
  (* We use continuation passing style to change the order in which
      arguments are needed by fprintf.
      The cost is that we have to indirect through a string which means
      we lose some of the abilities of format strings. *)
  let print_gen k m =
    let buf = Buffer.create 42 in
    kfprintf (fun buffmt ->
    fprintf buffmt "@?";
    k (Buffer.contents buf))
      (formatter_of_buffer buf) m

  let print m =
    print_gen (fun s k fmt -> kfprintf k fmt "%s" s) m

  let print_loc (b,e) =
    print "File \"%s%s\", line %d, characters %d-%d:"
      !dir b.pos_fname
      b.pos_lnum (b.pos_cnum - b.pos_bol)
      (e.pos_cnum - b.pos_bol)

  let print_msg loc m =
    print_loc loc <:> print "@\n" <:> print "%s" m <:> print "@\n"

  let string_mode = function
    | M -> "Math mode"
    | T -> "Text mode"
    | CMD name -> sprintf "Command mode (%s)" name

  let print_mode_line (m,lc) =
    print_loc lc <:> print "@,"<:>print "%s opened and pending@," (string_mode m)

  let print_mode_stack ms =
    List.fold_left (<:>) unit (List.map print_mode_line ms)

  let print_err_msg_and_exit lc stk x =
    flush std_formatter (print_msg lc x <:>
                    print "@[<v>" <:>
                print_mode_stack stk <:>
                    print "@]");
    exit 1

let error lc stk m =
    print_gen (print_err_msg_and_exit lc stk) m

let parse_file filename =
  let inx = open_in filename in
  let lexbuf = from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
     let s = Parser.doc Lexer.token lexbuf in
     flush std_formatter (print_mode_stack (Lexer.get_stack ()));
     reset_head (); s
 (*  ([], [Text "This is text.";
        Comment "This is a comment.";
        Math "This is math.";
        Cmd (List ([Text "Text item 1";
                    Math "Math item 2";
                    Cmd (List ([Text "Nested text item 1";
                                Math "Nested math item 2"], None))], None))]) *)
  with
      | Lexer.Lexical_error(loc, stk, s) -> error loc stk "parse error: %s" s
      | Parser.Error -> parse_error lexbuf
      | Failure s -> unexpected_error s lexbuf
