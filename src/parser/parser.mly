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

%token <string> MODULE_ID
%token <string> ID
%token USCORE

%token LSBRAC
%token RSBRAC
%token LPAREN
%token RPAREN
%token LCBRAC
%token RCBRAC
%token SCOLON
%token PERIOD 
%token COMMA

%token COLON
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

%right EQ
%left LAND LOR 
%left BAND BOR
%left DEQ NEQ
%left GT LT LTE GTE
%left ADD SUB
%left MUL DIV MOD 
%left LNOT BNOT 
%left CAST
%left LSBRAC 
%left PERIOD

%token EOF
// %start <unit> parseModule
// %start <unit> parseInterface
%start <unit> main
%start <unit> parseModule
%start <unit> parseInterface
%%


main:
    | expr EOF {()}

importList:
    | importList IMPORT sourceList {()}
    | {()}

parseModule:
    | importList moduleFile EOF {()}

moduleFile:
    | moduleFile functionDef {()}
    | moduleFile varDecl SCOLON {()}
    | moduleFile varDecl EQ constArg SCOLON {()}
    | moduleFile typeDef {()}
    | {()}

parseInterface:
    | importList interfaceFile EOF {()}

interfaceFile:
    | interfaceFile varDecl SCOLON {()}
    | interfaceFile functionDecl SCOLON{()}
    | interfaceFile typeExpose {()}
    | interfaceFile typeDef {()}
    | {()}

(* General items *)
sourceList:
    | sourceList PERIOD MODULE_ID {()}
    | MODULE_ID {()}


(* Top level items *)

(* constant constructor calls *)
constConstructorCall:
    | sourceList PERIOD ID LPAREN constNamedArgsList RPAREN {()} (* this one and the ones below are constructor calls *)
    | ID LPAREN constNamedArgsList RPAREN {()}   

constNamedArgsList:
    | reqConstNamedArgsList {()}
    | {()}

reqConstNamedArgsList:
    | reqConstNamedArgsList COMMA ID EQ constArg {()}
    | ID EQ constArg {()}

(* valid arguments *)
constArg:
    | baseLiteral {()}
    | constArray {()}
    | constConstructorCall {()}

constArgList:
    | reqConstArgList {()}
    | {()}
    
reqConstArgList:
    | reqConstArgList COMMA constArg {()}
    | constArg {()}

constArray: (* arrayLiteral *)
    | LSBRAC constArgList RSBRAC {()}

(* end of constant constructor calls *)

kleenelrsbrac:
    | kleenelrsbrac LSBRAC RSBRAC {()}
    | {()}

dataType:
    | INT kleenelrsbrac {()}
    | CHAR kleenelrsbrac {()}
    | BOOL kleenelrsbrac {()}
    | ID {()}
    | ID LSBRAC RSBRAC kleenelrsbrac {()}
    | sourceList PERIOD ID {()}
    | sourceList PERIOD ID LSBRAC RSBRAC kleenelrsbrac {()}

dataTypeList:
    | dataTypeList COMMA dataType {()}
    | dataType {()}

varDecl:
    | CONST dataType ID {()}
    | dataType ID {()}

varDeclList:
    | reqVarDeclList {()}
    | {()}

reqVarDeclList:
    | reqVarDeclList COMMA varDecl {()}
    | varDecl {()}

functionDecl:
    | dataTypeList ID LPAREN varDeclList RPAREN {()}
    | ID LPAREN varDeclList RPAREN {()}
    | VOID ID LPAREN varDeclList RPAREN {()}

functionDef:
    | functionDecl body {()}

typeDef:
    | TYPE ID EQ LCBRAC varDeclList RCBRAC {()}

typeExpose:
    | TYPE ID COLON LCBRAC varDeclList RCBRAC {()}



(* Statement level items*)

body:
    | LCBRAC stmtList RCBRAC {()}

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
    | varDecl SCOLON {()}
    | assignment SCOLON {()}
    | BREAK SCOLON {()}
    | CONTINUE SCOLON {()}
    | RETURN optExpr SCOLON {()}
    | body {()}





(* Expression level items *)
baseLiteral:
    | INT_LIT {()}
    | CHAR_LIT {()}
    | BOOL_LIT {()}
    | STR_LIT {()}
    | NULL {()}

literal:
    | baseLiteral {()}
    | LSBRAC exprList RSBRAC {()} (*arrayLiteral*)


reqUnnamedArgsList:
    | reqUnnamedArgsList COMMA expr {()}
    | expr {()}

reqNamedArgsList:
    | reqNamedArgsList COMMA ID EQ expr {()}
    | ID EQ expr {()}


moduleAccess:
    | sourceList PERIOD ID {()}

functionCall:
    | sourceList PERIOD ID LPAREN reqUnnamedArgsList RPAREN {()}
    | ID LPAREN reqUnnamedArgsList RPAREN {()}
    | sourceList PERIOD ID LPAREN reqNamedArgsList RPAREN {()} (* this one and the ones below are constructor calls *)
    | ID LPAREN reqNamedArgsList RPAREN {()}    
    | sourceList PERIOD ID LPAREN  RPAREN {()} (* ambiguous cases *)
    | ID LPAREN RPAREN {()}




%inline fieldAccess:
    | primary PERIOD ID {()}
    | moduleAccess PERIOD ID {()}
    | ID PERIOD ID {()}

%inline arrayAccess:
    | primary LSBRAC expr RSBRAC {()}
    | ID LSBRAC expr RSBRAC {()}
    | sourceList PERIOD ID LSBRAC expr RSBRAC {()}

assignment:
    | lhs EQ expr {print_endline "??? assignment???"}
    | lhsList EQ functionCall {()}

lhsList:
    | lhsList COMMA lhs {()}
    | lhs COMMA lhs {()}

lhs:
    | ID {()}
    | varDecl {()}
    | fieldAccess {()}
    | arrayAccess {()}
    | moduleAccess {()}
    | USCORE {()}

cast:
    | LPAREN INT RPAREN {()}
    | LPAREN CHAR RPAREN {()}


(* note that if any production uses primary, it must also use ID separately*)
primary:
    | literal {()}
    | functionCall {()}
    | fieldAccess {()}
    | arrayAccess {()}
    | LPAREN expr RPAREN {()}

exprList:
    | reqExprList {()}
    | {()}

reqExprList:
    | reqExprList COMMA expr {()}
    | expr {()}

expr:
    | ID {()}
    | moduleAccess {()}

    | primary {()}
    | expr binOp expr {()}
    | unOp expr {()}
    | cast expr %prec CAST {()}


    // | lhs EQ expr {()} 
    // | expr LAND expr {()}
    // | expr LOR expr {()}
    // | expr BAND expr {()}
    // | expr BOR expr {()}
    // | expr DEQ expr {()}
    // | expr NEQ expr {()}
    // | expr GT expr {()}
    // | expr LT expr {()}
    // | expr GTE expr {()}
    // | expr LTE expr {()}
    // | expr ADD expr {()}
    // | expr SUB expr {print_endline "BINARY MINUS" }
    // | expr MUL expr {()}
    // | expr MOD expr {()}
    // | expr DIV expr {()}
    // | LNOT expr {()}
    // | BNOT expr {()}
    // | SUB expr {print_endline "UNARY MINUS" }
    // | cast expr %prec CAST{()}

optExpr:
    | expr {()}
    | {()}

%inline binOp:
    | LAND {()}
    | LOR {()}
    | BAND {()}
    | BOR {()}
    | DEQ {()}
    | NEQ {()}
    | GT {()}
    | LT {()}
    | GTE {()}
    | LTE {()}
    | ADD {()}
    | SUB {()}
    | MUL {()}
    | MOD {()}
    | DIV {()}

%inline unOp:
    | LNOT {()}
    | BNOT {()}
    | SUB {()}
    | ADD {()}
