

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
%token RETURN CONTINUE BREAK FINAL

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

%start <unit> main
%%

main:
    main_aux EOF {()}

main_aux:
    | main_aux moduleFile {()}
    | moduleFile {()}

moduleFile:
    | moduleFile modTLI {()} 
    | modTLI {()}

modTLI:
    | funcDef {()}

funcDecl:
    | Type ID LPAREN formalParamList RPAREN {()}
    | ID ID LPAREN formalParamList RPAREN {()}
    | ID LPAREN formalParamList RPAREN {()}

varDecl:
    | FINAL? Type ID {()}
    | FINAL? ID ID  {()}

funcDef:
    | funcDecl body     {Printf.printf "func def\n"}

formalParamList:
    | formalParamList COMMA varDecl {Printf.printf "form a formal param "}
    | varDecl        {Printf.printf "form a formal param "}

Type:
    | INT                   {Printf.printf "type int\n"}
    | CHAR                  {Printf.printf "type char\n"}
    | BOOL                  {Printf.printf "type bool\n"}
    | Type LSBRAC RSBRAC    {Printf.printf "+d"}



body:
    | LCBRAC RCBRAC     {print_endline "BODY\n"}

