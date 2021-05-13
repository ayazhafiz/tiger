%{
  open Language
  open Symbol
%}

%token <int>    INT
%token <string> STRING
%token <string> IDENT

%token ARRAY
%token BREAK
%token DO
%token ELSE
%token END
%token FOR
%token FUNCTION
%token IF
%token IN
%token LET
%token NIL
%token OF
%token THEN
%token TO
%token TYPE
%token VAR
%token WHILE
%token ASSIGN
%token NEQ
%token LEQ
%token GEQ
%token COMMA
%token COLON
%token SEMI
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token DOT
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token EQ
%token LT
%token GT
%token AND
%token OR
%token EOF

%left OR
%left AND
%nonassoc ASSIGN
%nonassoc EQ NEQ GT LT GEQ LEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start toplevel
%type <expr> toplevel
%%

toplevel:
  | expr EOF { $1 }

expr:
  | STRING      { StringExpr $1 }
  | INT         { IntExpr $1 }
  | NIL         { NilExpr }
  | lvalue      { VarExpr $1 }
  | MINUS expr  { OpExpr { left=IntExpr 0; oper=MinusOp; right=$2 } }
  | binary_expr { $1 }
  | symbol LPAREN expr_list RPAREN
      { CallExpr { func=$1; args=$3 } }
  | LPAREN expr_seq RPAREN
      { SeqExpr $2 }
  | symbol LBRACE field_list RBRACE
      { RecordExpr { typ=$1; fields=$3 } }
  | symbol LBRACKET expr RBRACKET OF expr
      { ArrayExpr { typ=$1; size=$3; init=$6 } }
  | IF expr THEN expr
      { IfExpr { test=$2; then'=$4; else'=Option.none } }
  | IF expr THEN expr ELSE expr
      { IfExpr { test=$2; then'=$4; else'=Option.some $6 } }
  | WHILE expr DO expr
      { WhileExpr { test=$2; body=$4 } }
  | FOR symbol ASSIGN expr TO expr DO expr
      { ForExpr { var=$2; escape=ref true; lo=$4; hi=$6; body=$8 } }
  | BREAK
      { BreakExpr }
  | LET decl_list IN expr_seq END
      { LetExpr { decls=$2; body=SeqExpr $4 } }

expr_seq:
  | { [] }
  | expr { [$1] }
  | expr SEMI expr_seq { $1::$3 }

expr_list:
  | { [] }
  | expr { [$1] }
  | expr COMMA expr_list { $1::$3 }

field_list:
  | { [] }
  | symbol EQ expr { [($1, $3)] }
  | symbol EQ expr COMMA field_list { ($1, $3)::$5 }
 
symbol:
  | IDENT { symbol $1 }

lvalue:
  | symbol { SimpleVar $1 } 
  | symbol DOT symbol { FieldVar (SimpleVar $1, $3) }
  | lvalue DOT symbol { FieldVar ($1, $3) }
  | symbol LBRACKET expr RBRACKET { SubscriptVar (SimpleVar $1, $3) }
  | lvalue LBRACKET expr RBRACKET { SubscriptVar ($1, $3) }

decl_list:
  | decl { [$1] }
  | decl decl_list { $1::$2 }

decl:
  | type_decl { TypeDecl $1 }
  | var_decl  { $1 }
  | fun_decl  { FunctionDecl $1 }

type_decl:
  | type_decl_atom { [$1] }
  | type_decl_atom type_decl { $1::$2 }

type_decl_atom:
  | TYPE symbol EQ ty { { name=$2; ty=$4 } }

ty:
  | symbol { NameTy $1 }
  | LBRACE type_fields RBRACE { RecordTy $2 }
  | ARRAY OF symbol { ArrayTy $3 }
 
type_fields:
  | { [] }
  | type_field { [$1] }
  | type_field COMMA type_fields { $1::$3 }

type_field:
  | symbol COLON symbol { { fld_name=$1; escape=ref true; typ=$3 } }

var_decl:
  | VAR symbol ASSIGN expr
      { VarDecl { name=$2; escape=ref true; typ=Option.none; init=$4 } }
  | VAR symbol COLON symbol ASSIGN expr
      { VarDecl { name=$2; escape=ref true; typ=Option.some $4; init=$6 } }

fun_decl:
  | fun_decl_atom { [$1] }
  | fun_decl_atom fun_decl { $1::$2 }

fun_decl_atom:
  | FUNCTION symbol LPAREN type_fields RPAREN EQ expr
      { { fn_name=$2; params=$4; result=Option.none; body=$7 } }
  | FUNCTION symbol LPAREN type_fields RPAREN COLON symbol EQ expr
      { { fn_name=$2; params=$4; result=Option.some $7; body=$9 } }

binary_expr:
  | lvalue ASSIGN expr
      { AssignExpr { var=$1; expr=$3 } }
  | expr TIMES expr
      { OpExpr { left=$1; oper=TimesOp; right=$3 } }
  | expr DIVIDE expr
      { OpExpr { left=$1; oper=DivideOp; right=$3 } }
  | expr PLUS expr
      { OpExpr { left=$1; oper=PlusOp; right=$3 } }
  | expr MINUS expr
      { OpExpr { left=$1; oper=MinusOp; right=$3 } }
  | expr EQ expr
      { OpExpr { left=$1; oper=EqOp; right=$3 } }
  | expr NEQ expr
      { OpExpr { left=$1; oper=NeqOp; right=$3 } }
  | expr LEQ expr
      { OpExpr { left=$1; oper=LeOp; right=$3 } }
  | expr GEQ expr
      { OpExpr { left=$1; oper=GeOp; right=$3 } }
  | expr LT expr
      { OpExpr { left=$1; oper=LtOp; right=$3 } }
  | expr GT expr
      { OpExpr { left=$1; oper=GtOp; right=$3 } }
  | expr AND expr
      { IfExpr { test=$1; then'=$3; else'=Option.some (IntExpr 0) } }
  | expr OR expr
      { IfExpr { test=$1; then'=IntExpr 1; else'=Option.some $3 } }
