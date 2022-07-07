

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


%right EQ
%left LAND LOR 
%left BAND BOR
%left DEQ NEQ
%left GT LT LTE GTE
%left ADD SUB
%left MUL DIV MOD 
%left LNOT BNOT 
%left CCHAR CINT


%start <unit> main
%%
(** ======== TOP LEVEL ITEMS ======== **)

main:
    | MOD_START importList modFile EOF {()}
    | INF_START importList infFile EOF {()}

importList:
    | importList IMPORT ID {()}
    | {()}

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
    | {()}

varDef:
    | varDecl EQ INT_LIT {(*THIS IS A PLACEHOLDER*)()}

funcDecl:
    | Type ID LPAREN varDeclList RPAREN {()}
    | ID LPAREN varDeclList RPAREN {()}
    | VOID ID LPAREN varDeclList RPAREN {()}

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
    | ID {()}
    | ID LSBRAC RSBRAC kleenelrsbrac {()}
    | IDPChain  {()}
    | IDPChain LSBRAC RSBRAC kleenelrsbrac {()}

    


stmtList:
    | stmtList stmt {()}
    | {()}

stmt:
    | closedStmt {()}
    | openStmt {()}

closedStmt:
    | IF LPAREN expr RPAREN closedStmt ELSE closedStmt {()}
    | FOR LPAREN optExpr SCOLON optExpr SCOLON optExpr RPAREN closedStmt {()}
    | WHILE LPAREN expr RPAREN closedStmt {()}
    | otherStmt {()}

openStmt:    
    | IF LPAREN expr RPAREN stmt {()}
    | IF LPAREN expr RPAREN closedStmt ELSE openStmt {()}
    | FOR LPAREN optExpr SCOLON optExpr SCOLON optExpr RPAREN openStmt {()}
    | WHILE LPAREN expr RPAREN openStmt {()}

otherStmt:
    | varDecl SCOLON
    | varDef SCOLON {()}
    | exprStmt SCOLON {()}
    | BREAK SCOLON {()}
    | CONTINUE SCOLON {()}
    | RETURN optExpr SCOLON {()}
    | body {()}




(** ======== Reduce reduce conflict resolver between arr[6] and int[] ======== **)



(* ======== EXPRESSION LEVEL ITEMS ======== *)


literal:
    | INT_LIT {()}
    | STR_LIT {()}
    | CHAR_LIT {()}
    | BOOL_LIT {()}
    | arrayLiteral {()}
    | NULL {()}

arrayLiteral:
    | LCBRAC exprList RCBRAC {()}

lhs:
    | monoExpr {print_endline "LHS IS monoexpr"}
    | USCORE {print_endline "LHS IS _"}
    | IDPChain {print_endline "LHS IS CHAIN"}




exprList:
    | nonEmptyExprList {()}
    | {()}

nonEmptyExprList:
    | nonEmptyExprList COMMA expr {()}
    | expr {()}

optExpr:
    | expr {()}
    | {()}

(* "ID PERIOD Chain" of at least length 2 *)
IDPChain:
    | IDPChain PERIOD ID {()}
    | ID PERIOD ID {()}


monoExpr:
    | fieldAccess {()}
    | arrayAccess {()}
    | funcCall {()}
    | LPAREN expr RPAREN {()}



fieldAccess:
    | monoExpr PERIOD ID {()}

arrayAccess:
    | monoExpr LSBRAC expr RSBRAC {()}
    | IDPChain LSBRAC expr RSBRAC {()}
    | ID LSBRAC expr RSBRAC {()}

funcCall:
    | IDPChain LPAREN exprList RPAREN {()}
    | ID LPAREN exprList RPAREN {()}



exprStmt:
    | lhs EQ expr {print_endline "??? assignment???"}
    | funcCall {()}
    | varDef SCOLON {()}
    | varDecl SCOLON {()}


expr:
    | ID {()}
    | monoExpr {()}
    | literal {()}
    | IDPChain {print_endline "IDP BECOMIND EXPR " }
    | lhs EQ expr {print_endline "??? assignment???"}
    | expr LAND expr {}
    | expr LOR expr {()}
    | expr BAND expr {()}
    | expr BOR expr {()}
    | expr DEQ expr {()}
    | expr NEQ expr {()}
    | expr GT expr {()}
    | expr LT expr {()}
    | expr GTE expr {()}
    | expr LTE expr {()}
    | expr ADD expr {()}
    | expr SUB expr {print_endline "BINARY MINUS" }
    | expr MUL expr {()}
    | expr MOD expr {()}
    | expr DIV expr {()}
    | LNOT expr {()}
    | BNOT expr {()}
    | SUB expr {print_endline "UNARY MINUS" }
    | CCHAR expr {()}
    | CINT expr {()}

(* sue me; this is for debugging *)
(* comment the one below and uncomment the one further down *) (*
expr:
    | primary {()}
    | LPAREN expr RPAREN {()}
    | lhs EQ expr {print_endline ""}
    | expr LAND expr {print_endline "LAND"}
    | expr LOR expr {print_endline "LOR"}
    | expr BAND expr {print_endline "BAND"}
    | expr BOR expr {print_endline "BOR"}
    | expr DEQ expr {print_endline "DEQ"}
    | expr NEQ expr {print_endline "NEQ"}
    | expr GT expr {print_endline "GT"}
    | expr LT expr {print_endline "LT"}
    | expr GTE expr {print_endline "GTE"}
    | expr LTE expr {print_endline "LTE"}
    | expr ADD expr {print_endline "BINARY ADD"}
    | expr SUB expr {print_endline "BINARY MINUS" }
    | expr MUL expr {print_endline "MUL"}
    | expr MOD expr {print_endline "MOD"}
    | expr DIV expr {print_endline "DIV"}
    | LNOT expr {print_endline "LNOT"}
    | BNOT expr {print_endline "BNOT"}
    | SUB expr {print_endline "UNARY MINUS" }
    | ADD expr {print_endline "UNARY ADD"}
    | CCHAR expr {print_endline "CCHAR"}
    | CINT expr {print_endline "CINT"} *)