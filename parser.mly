%{
open Ast
%}

%token <string> STRING
%token HEAD, BODY
%token EOF
%token TEXT_BEGIN TEXT_END
%token MATH_BEGIN MATH_END
%token <string * Ast.style> CMD_BEGIN
%token CMD_END
%token <string> TITLE
%token <string> AUTHOR
%token <string> DATE
%token <float> MARGIN
%token LANDSCAPE
%token <string> COMMENT
%token <string> LATEX
%token <string> VAR
%token <string> FONT
%token <int> FONTSIZE
%token <string> LEAF_OP, UNARY_OP, BINARY_OP
%token OPENPARA, CLOSEPARA
%left COMMENT
%left LATEX

%start <Ast.doc> doc

%%

doc:
  | head body EOF
        { ($1, $2) }

head:
  | HEAD head_expr*
        { $2 }
  | head_expr*
        { $1 }
body:
  | BODY expr*
        { $2 }
;


head_expr:
| TITLE
    { Title $1 }
| AUTHOR
    { Author $1 }
| DATE
    { Date $1 }
| MARGIN
    { Margin $1 }
| LANDSCAPE
    { Landscape }
| STRING
    { HString $1 }
| COMMENT
    { HComment $1 }
| LATEX
    { HString $1 }
| FONT
    { Font $1 }
| FONTSIZE
    { Fontsize $1 }
;


expr:
| TEXT_BEGIN text* TEXT_END
    { Text $2 }
| MATH_BEGIN math* MATH_END
    { Math $2 }
| CMD_BEGIN cmd* CMD_END
    { Cmd ($1, $2) }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
| LATEX
    { String $1 }
| VAR
    { Var $1 }
;

text:
| MATH_BEGIN math* MATH_END
    { Math $2 }
| CMD_BEGIN cmd* CMD_END
    { Cmd ($1, $2) }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
| LATEX
    { String $1 }
| VAR
    { Var $1 }
;

math:
| math_expr
    { Expr $1 }
| math_op
    { Math_op $1 }

math_expr:
| TEXT_BEGIN text* TEXT_END
    { Text $2 }
| CMD_BEGIN cmd* CMD_END
    { Cmd ($1, $2) }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
| LATEX
    { String $1 }
| VAR
    { Var $1 }
;

math_op:
| LEAF_OP
    { Leaf_op $1 }
| UNARY_OP OPENPARA math CLOSEPARA
    { Unary_op ($1,$3)}
| BINARY_OP OPENPARA math OPENPARA math CLOSEPARA
    { Binary_op ($1,$3, $5)}
cmd:
| TEXT_BEGIN text* TEXT_END
    { Text $2 }
| MATH_BEGIN math* MATH_END
    { Math $2 }
| CMD_BEGIN cmd* CMD_END
    { Cmd ($1, $2) }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
| LATEX
    { String $1 }
| VAR
    { Var $1 }
;
