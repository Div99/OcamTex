%{
open Ast
open Ast_factory

let has_dups lst =
  let open List in
  length lst <> length (sort_uniq Pervasives.compare lst)
%}

%token <string> STRING
%token <string> COMMENT
%token EOF
%token TEXT_BEGIN TEXT_END
%token MATH_BEGIN MATH_END
%token <string> CMD_BEGIN
%token <string option> STYLE
%token CMD_END
%token <string> TITLE, AUTHOR
%token <float> MARGIN, IDENT

%start <Ast.head_expr> parse_head_expression
%start <Ast.expr> parse_expression

%%

parse_head_expression:
  | head_expr EOF
        { $1 }

parse_expression:
  | expr EOF
    { $1 }
;


head_expr:
| TITLE
    { make_title $1 }
| AUTHOR
    { make_author $1 }
| MARGIN
    { make_margins $1 }
| IDENT
    { make_indent $1 }

;

expr:
| TEXT_BEGIN text* TEXT_END
    { make_text $2 }
| MATH_BEGIN math* MATH_END
    { make_math $2 }
| CMD_BEGIN STYLE cmd* CMD_END
    { make_cmd $1 $2 $3 }
| CMD_BEGIN cmd* CMD_END
    { make_cmd $1 None $2 }
| STRING
    { make_string $1 }
| COMMENT
    { make_comment $1 }
;

text:
| MATH_BEGIN math* MATH_END
    { make_math $2 }
| CMD_BEGIN STYLE cmd* CMD_END
    { make_cmd $1 $2 $3 }
| CMD_BEGIN cmd* CMD_END
    { make_cmd $1 None $2 }
| STRING
    { make_string $1 }
| COMMENT
    { make_comment $1 }
;

math:
| TEXT_BEGIN text* TEXT_END
    { make_text $2 }
| CMD_BEGIN STYLE cmd* CMD_END
    { make_cmd $1 $2 $3 }
| CMD_BEGIN cmd* CMD_END
    { make_cmd $1 None $2 }
| STRING
    { make_string $1 }
| COMMENT
    { make_comment $1 }
;

cmd:
| TEXT_BEGIN text* TEXT_END
    { make_text $2 }
| MATH_BEGIN math* MATH_END
    { make_math $2 }
| CMD_BEGIN STYLE cmd* CMD_END
    { make_cmd $1 $2 $3 }
| CMD_BEGIN cmd* CMD_END
    { make_cmd $1 None $2 }
| STRING
    { make_string $1 }
| COMMENT
    { make_comment $1 }
;

