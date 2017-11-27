%{
open Ast
%}

%token <string> STRING
%token <string> COMMENT
%token <int> PAR
%token EOF
%token TEXT_BEGIN TEXT_END
%token MATH_BEGIN MATH_END
%token <string> CMD_BEGIN
%token <string> STYLE
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
    { Title (Some $1) }
| AUTHOR
    { Author (Some $1) }
| MARGIN
    { Margins $1 }
| IDENT
    { Indent $1 }

;

expr:
| TEXT_BEGIN text* TEXT_END
    { Text $2 }
| MATH_BEGIN math* MATH_END
    { Math $2 }
| CMD_BEGIN STYLE cmd* CMD_END
    { Cmd ($1, Some $2, $3) }
| CMD_BEGIN cmd* CMD_END
    { Cmd ($1, None, $2) }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
;

text:
| MATH_BEGIN math* MATH_END
    { Math $2 }
| CMD_BEGIN STYLE cmd* CMD_END
    { Cmd ($1, Some $2, $3) }
| CMD_BEGIN cmd* CMD_END
    { Cmd ($1, None, $2) }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
;

math:
| TEXT_BEGIN text* TEXT_END
    { Text $2 }
| CMD_BEGIN STYLE cmd* CMD_END
    { Cmd ($1, Some $2, $3) }
| CMD_BEGIN cmd* CMD_END
    { Cmd ($1, None, $2) }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
;

cmd:
| TEXT_BEGIN text* TEXT_END
    { Text $2 }
| MATH_BEGIN math* MATH_END
    { Math $2 }
| CMD_BEGIN STYLE cmd* CMD_END
    { Cmd ($1, Some $2, $3) }
| CMD_BEGIN cmd* CMD_END
    { Cmd ($1, None, $2) }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
;
