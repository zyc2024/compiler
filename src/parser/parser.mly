

%token <int64> INT_LIT
%token <int> CHAR_LIT
%token <string> STR_LIT
%token <bool> BOOL_LIT

%token BOOL 
%token INT
%token CHAR
%token TYPE
%token VOID
%token NULL
%token CONST
%token IF
%token ELSE
%token FOR
%token WHILE
%token RETURN CONTINUE BREAK 

%token IMPORT

%token <string> ID
%token USCORE
%token CINT
%token CCHAR

%token LSBRAC
%token RSBRAC
%token LPAREN
%token RPAREN
%token LCBRAC
%token RCBRAC
%token SCOLON
%token PERIOD 
%token COMMA

%token DEQ
%token NEQ
%token LTE
%token GTE
%token LT
%token GT
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token EQ
%token LNOT
%token LAND
%token LOR

%token BNOT
%token BAND
%token BOR

%token EOF

%token MOD_START INF_START

%start <unit> main
%%
(** ======== TOP LEVEL ITEMS ======== **)

main:
    | MOD_START modFile EOF {()}
    | INF_START infFile EOF {()}

modFile:
    | modFile funcDef {()}
    | modFile varDef SCOLON{()}
    | modFile varDecl SCOLON{()}
    | modFile typeDef {()}
    | {()}

infFile:
    | infFile funcDecl {()}
    | infFile typeDef {()}
    | {()}

varDecl:
    | CONST Type ID {()}
    | Type ID {()}

varDeclList:
    | varDeclList COMMA varDecl {()}
    | varDecl    {()}

varDef:
    | varDecl EQ INT_LIT {(*THIS IS A PLACEHOLDER*)()}

funcDecl:
    | Type ID LPAREN varDeclList RPAREN {()}
    | ID LPAREN varDeclList RPAREN {()}

funcDef:
    | funcDecl body {()}


typeDef:
    | TYPE ID LCBRAC varDeclList RCBRAC {()}


(** ======== BODY LEVEL ITEMS ======== **)

body:
    | LCBRAC stmtList RCBRAC {()}

(* this is a better substitute for ([])* *)
kleenelrsbrac:
    | kleenelrsbrac LSBRAC RSBRAC {()}
    | {()}

Type:
    | INT kleenelrsbrac {()}
    | CHAR kleenelrsbrac {()}
    | BOOL kleenelrsbrac {()}
    | ID kleenelrsbrac {()}


stmtList:
    | stmtList stmt {()}
    | {()}

stmt:
    | closedStmt {()}
    | openStmt {()}

closedStmt:
    | IF LPAREN expr RPAREN closedStmt ELSE closedStmt {()}
    | otherStmt {()}

openStmt:    
    | IF LPAREN expr RPAREN stmt {()}
    | IF LPAREN expr RPAREN closedStmt ELSE openStmt {()}

otherStmt:
    | lhs EQ rhs {()}


(** ======== EXPRESSION LEVEL ITEMS ======== **)

primaryList:
    | primaryList COMMA primary {()}
    | {()}

arrayLiteral:
    | LCBRAC primaryList RCBRAC {()}

literal:
    | INT_LIT {()}
    | CHAR_LIT {()}
    | BOOL_LIT {()}
    | STR_LIT {()}
    | arrayLiteral {()}

(* all primary things are expression components and all expression components are primaries *)
primary:
    | lhs {()}
    | literal {()}


fieldAccess:
    | primary PERIOD ID  {()}


arrayAccess:
    | primary LSBRAC expr RSBRAC {()}

(* this is a subset of the primary *)
lhs:
    | ID {()}
    | arrayAccess {()}
    | fieldAccess {()}

constant:
    | INT_LIT {()}
    | BOOL_LIT {()}
    | CHAR_LIT {()}

rhs:
    | constant {()}

expr:   
    | constant {()}
