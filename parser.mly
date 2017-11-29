%{
open Ast
%}

%token <string> STRING
%token HEAD, BODY
%token EOF
%token TEXT_BEGIN TEXT_END
%token MATH_BEGIN MATH_END
%token <string*string option> CMD_BEGIN
%token CMD_END
%token <string> TITLE
%token <string> AUTHOR
%token <float> MARGIN, IDENT
%token <string> COMMENT
%token <string> VAR

%left COMMENT

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
| MARGIN
    { Margins $1 }
| IDENT
    { Indent $1 }
| STRING
    { HString $1 }
| COMMENT
    { HComment $1 }
;


expr:
| TEXT_BEGIN text* TEXT_END
    { Text $2 }
| MATH_BEGIN math* MATH_END
    { Math $2 }
| CMD_BEGIN cmd* CMD_END
    { Cmd $1 $2 }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
| VAR
    { Var $1 }
;

text:
| MATH_BEGIN math* MATH_END
    { Math $2 }
| CMD_BEGIN cmd* CMD_END
    { Cmd $1 $2 }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
;

math:
| TEXT_BEGIN text* TEXT_END
    { Text $2 }
| CMD_BEGIN cmd* CMD_END
    { Cmd $1 $2 }
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
| CMD_BEGIN cmd* CMD_END
    { Cmd $1 $2 }
| STRING
    { String $1 }
| COMMENT
    { Comment $1 }
;
