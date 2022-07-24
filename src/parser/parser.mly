%{
    open Ast.AstNode

    (* given any ast node variant, [get_pos node] is the position of [node]. *)
    let get_pos : (Lexing.position * 'a -> Lexing.position) = 
        function pos, _ -> pos
%}

%token <int64> INT_LIT
%token <int> CHAR_LIT
%token <int Queue.t> STR_LIT
%token <bool> BOOL_LIT

%token BOOL INT CHAR
%token TYPE
%token VOID CONST
%token IF ELSE WHILE FOR RETURN CONTINUE BREAK
%token IMPORT
%token EOF
%token <string> MODULE_ID
%token <string> ID
%token USCORE NULL

// delimiters
%token LSBRAC RSBRAC LPAREN RPAREN LCBRAC RCBRAC SCOLON PERIOD COMMA COLON

// operators
%token DEQ NEQ LTE GTE LT GT ADD SUB MUL DIV MOD EQ LNOT LAND LOR BNOT BAND BOR

%nonassoc IF
%nonassoc ELSE
%right EQ
%left LAND LOR 
%left BAND BOR
%left DEQ NEQ
%left GT LT LTE GTE
%left ADD SUB
%left MUL DIV MOD 
%left LNOT BNOT 
%left CAST UMINUS
%left LSBRAC 
%left PERIOD

%start <Ast.AstNode.stmt_node> parse_stmt
%%

parse_stmt: 
    | s=stmt EOF {s}

(* note that this is stored in reverse manner, 
    so the first element is the type when used as type. *)
sourceList:
    | slst=sourceList PERIOD source_name=MODULE_ID {
        ($startpos(source_name), source_name ) :: slst
    }
    | source_name=MODULE_ID {[($startpos, source_name)]}                        


stmt:
    | b=body {b}
    // declarations
    | d=varDecl SCOLON {
        let pos, is_const, type_node, name = d in
            (pos, Declaration(is_const, type_node, name, None)) 
    }
    | d=varDecl EQ src=expr SCOLON {
        (* choice of location: char[] str = 3; report error on "="*)
        let _, is_const, type_node, name = d in
            ($startpos($2), Declaration(is_const, type_node, name, Some(src))) 
    }

    //MOVE THIS INTO ANOTHER PRODUCTION
    // assignments
    | dest=lhs EQ src=expr SCOLON {($startpos($2), Assign(dest, src))}
    // control-flow
    | BREAK SCOLON {($startpos, Break)}
    | CONTINUE SCOLON {($startpos, Continue)}
    | RETURN SCOLON {($startpos, Return([]))}
    | RETURN elst=exprList SCOLON {($startpos, Return(elst))}


    | IF LPAREN expr RPAREN stmt %prec IF {()}
    | IF LPAREN expr RPAREN stmt ELSE stmt {()}
    | FOR LPAREN opt {()}
    | WHILE LPAREN e=expr RPAREN s=stmt {($symbolstartpos, While(e,s))}
    // I FORGOT HTE NAME
    | expr SCOLON {(Lexing.dummy_pos, Break)}
    
// don't do this yet, perhaps  only a subset of stmts allowed so no need yet.
// we decide.
stmtExpr: //GET A BETTER NAME
    | 



// varDecl = 4-tuple (position * is_const? * data_type_node * name)
// do not store as a declaration because pattern matching to get 
// the needed parts results in an impossible branch for all other expr variants
varDecl:
    | CONST t=dataType name=ID {($startpos, true, t, name)}
    | t=dataType name=ID {(get_pos(t), false, t, name)}

exprList:
    | e=expr {[e]}
    | elst=exprList COMMA e=expr {e :: elst}

body:
    | LCBRAC slst=stmtList RCBRAC {($startpos, Block(slst))}

stmtList:
    | slst=stmtList s=stmt {s :: slst}
    | {[]}


kleenelrsbrac:
    | total=kleenelrsbrac LSBRAC RSBRAC {total + 1}
    | {0}

dataType:
    | INT dim=kleenelrsbrac {($startpos, Int(dim))}
    | CHAR dim=kleenelrsbrac {($startpos, Char(dim))}
    | BOOL dim=kleenelrsbrac {($startpos, Bool(dim))}
    (* IN CASE WE EVER CAN MAKE LOWERCASE TYPES dominant / coexists for uppercase
    // subject to removal.
    | name=ID {($startpos, NameType([], name, 0))}
    | name=ID LSBRAC RSBRAC dim=kleenelrsbrac {($startpos, NameType([],name,dim+1))} 
    *)
    | slst=sourceList {
        let pos, name = List.hd slst in
        (pos, NameType(List.tl slst, name, 0))
    } 
    | slst=sourceList LSBRAC RSBRAC dim=kleenelrsbrac {
        let pos, name = List.hd slst in
        (pos, NameType(List.tl slst, name, dim))
    } 
    

(* Expression level items *)

expr:
    | e = constant_expr {e}
    | LPAREN e=expr RPAREN {e}
    | dest = lhs {(get_pos(dest), LhsExpr(dest))}
    | e1=expr b=binOp e2=expr {($startpos(b), BinopExpr(b, e1, e2))}
    | LNOT e=expr {($startpos, UnaryExpr(Not, e))}
    | BNOT e=expr {($startpos, UnaryExpr(BinNot, e))}
    | SUB e=expr %prec UMINUS {($startpos, UnaryExpr(Neg, e))}
    | c=cast e=expr %prec CAST {($startpos(e), Cast(c,e))}  


(* these values can be assigned to global variables. string isn't exactly 
    constant but has guanranteed constant property. *)
constant_expr:
    | i = INT_LIT {($startpos, IntLiteral(i))}
    | c = CHAR_LIT {($startpos, CharLiteral(c))}
    | b = BOOL_LIT {($startpos, BoolLiteral(b))}
    | s = STR_LIT {($startpos, StrLiteral(s))}
    | NULL {($startpos, Null)}

lhs:
    | f=fieldAccess {f}
    | a=arrayAccess {a}
    | m=moduleAccess {m}
    | var=ID {($startpos, Var(var))}
    | USCORE {($startpos, Underscore)}

fieldAccess:
    | e=expr PERIOD field_name=ID {($startpos($2), FieldAccess(e, field_name))}

moduleAccess:
    | slst=sourceList PERIOD var_name=ID 
    {($startpos(var_name), ModuleAccess(slst, var_name))}
    
arrayAccess:
    | e1=expr LSBRAC e2=expr RSBRAC {($startpos($2), ArrayAccess(e1,e2))}

%inline binOp:
    | LAND {And}
    | LOR {Or}
    | BAND {BinAnd}
    | BOR {BinOr}
    | DEQ {Deq}
    | NEQ {Neq}
    | GT {Gt}
    | LT {Lt}
    | GTE {Gte}
    | LTE {Lte}
    | ADD {Add}
    | SUB {Sub}
    | MUL {Mul}
    | MOD {Mod}
    | DIV {Div}

cast:
    | LPAREN INT RPAREN {($startpos($2), Int(0))}
    | LPAREN CHAR RPAREN {($startpos($2), Char(0))}

////////////////////////////////////////////////// fix below



// functionCall:
//     | l=sourceList PERIOD n=ID LPAREN a=exprList RPAREN {
//         ($startpos(n), FunctionCall(l,n,a))
//     }
//     | l=sourceList PERIOD n=ID LPAREN RPAREN {
//         ($startpos(n), FunctionCall(l,n,[]))
//     }
//     | n=ID LPAREN a=exprList RPAREN {
//         ($startpos(n), FunctionCall([],n,a))
//     }
//     | n=ID LPAREN RPAREN {
//         ($startpos(n), FunctionCall([],n,[]))
//     }



// constructorCall:
//     | l=sourceList LPAREN a=namedArgsList RPAREN {
//         match l with
//         | (npos, n)::s -> (npos, ConstructorCall(List.rev s), n, a) }



// namedArgsList:
//     | k=reqNamedArgsList {List.rev k}
//     | {[]}


// reqNamedArgsList:
//     | l=reqNamedArgsList COMMA n=ID EQ e=expr {
//         ($startpos(n),n,e)::l
//     }
//     | n=ID EQ e=expr {[($symbolstartpos, n,e )]}



// lhsList:
//     | ls=lhsList COMMA l=lhs {l::ls}
//     | l1=lhs COMMA l2=lhs {[l2;l1]}





