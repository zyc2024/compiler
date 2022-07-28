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

// %right EQ
%nonassoc IF
%nonassoc ELSE
%left LAND LOR 
%left BAND BOR
%left DEQ NEQ
%left GT LT LTE GTE
%left ADD SUB
%left MUL DIV MOD 
%left LNOT BNOT 
%nonassoc CAST UMINUS
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
    | s=unfinish_sentence SCOLON {s}
    | WHILE LPAREN e=expr RPAREN s=stmt {($symbolstartpos, While(e,s))}
    | IF LPAREN e=expr RPAREN s=stmt %prec IF {($startpos, If(e, s, None))}
    | IF LPAREN e=expr RPAREN s1=stmt ELSE s2=stmt {($startpos, If(e,s1,Some(s2)))}
    | b=block {b}

block:
    | LCBRAC slst=stmtList RCBRAC {($startpos, Block(slst))}

stmtList:
    | slst=stmtList s=stmt {s :: slst}
    | {[]}

// name implies a stmt is a completed sentence with a semicolon as the period.
unfinish_sentence:
    // declarations
    | d=varDecl {
        let pos, is_const, type_node, name = d in
            (pos, Declaration(is_const, type_node, name, None)) 
    }
    | d=varDecl EQ src=expr {
        (* choice of location: char[] str = 3; report error on "="*)
        let _, is_const, type_node, name = d in
            ($startpos($2), Declaration(is_const, type_node, name, Some(src))) 
    }
    // assignments
    | dest=expr EQ src=expr {($startpos($2), Assign(Some(dest), src))}
    | USCORE EQ src=expr {($startpos($2), Assign(None, src))}
    | dest_list=destList EQ src=expr {
        ($startpos($2), MultiAssign(dest_list, src))
    }
    // control-flow
    | BREAK {($startpos, Break)}
    | CONTINUE {($startpos, Continue)}
    | RETURN {($startpos, Return([]))}
    | RETURN elst=exprList {($startpos, Return(elst))}
    // arbitrary
    | e=expr {(get_pos(e), ExprStmt(e))}

// varDecl = 4-tuple (position * is_const? * data_type_node * name)
// do not store as a declaration because pattern matching to get 
// the needed parts results in an impossible branch for all other expr variants
varDecl:
    | CONST t=dataType name=ID {($startpos, true, t, name)}
    | t=dataType name=ID {(get_pos(t), false, t, name)}

// destList is stored in reverse order
destList:
    | loc1=expr_or_uscore COMMA loc2=expr_or_uscore {[loc2; loc1]}
    | loc_list=destList COMMA loc=expr_or_uscore {loc :: loc_list}

expr_or_uscore:
    | e=expr {Some(e)}
    | USCORE {None}

exprList:
    | e=expr {[e]}
    | elst=exprList COMMA e=expr {e :: elst}


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
    | e1=expr b=binOp e2=expr {($startpos(b), BinopExpr(b, e1, e2))}
    | LNOT e=expr {($startpos, UnaryExpr(Not, e))}
    | BNOT e=expr {($startpos, UnaryExpr(BinNot, e))}
    | SUB e=expr %prec UMINUS {($startpos, UnaryExpr(Neg, e))}
    | c=cast e=expr %prec CAST {($startpos(e), Cast(c,e))}  
    | array=arrayLiteral {array}
    | call=functionCall {call}
    | field_access=fieldAccess {field_access}
    | array_access=arrayAccess {array_access}
    | module_access=moduleAccess {module_access}
    | var=ID {($startpos, Var(var))}

fieldAccess:
    | e=expr PERIOD field_name=ID {($startpos($2), FieldAccess(e, field_name))}

moduleAccess:
    | slst=sourceList PERIOD var_name=ID 
    {($startpos(var_name), ModuleAccess(slst, var_name))}
    
arrayAccess:
    | e1=expr LSBRAC e2=expr RSBRAC {($startpos($2), ArrayAccess(e1,e2))}

(* these values can be assigned to global variables *)
constant_expr:
    | i = INT_LIT {($startpos, IntLiteral(i))}
    | c = CHAR_LIT {($startpos, CharLiteral(c))}
    | b = BOOL_LIT {($startpos, BoolLiteral(b))}
    | s = STR_LIT {($startpos, StrLiteral(s))}  //maybe not actually CONSTANT.
    | NULL {($startpos, Null)}

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

arrayLiteral:
    | LSBRAC elst=exprList RSBRAC {($startpos, ArrayLiteral(elst))}
    | LSBRAC elst=exprList COMMA RSBRAC {($startpos, ArrayLiteral(elst))}
    | LSBRAC RSBRAC {($startpos, ArrayLiteral([]))}

functionCall:
    | l=sourceList PERIOD n=ID LPAREN a=exprList RPAREN {
        ($startpos(n), FunctionCall(l,n,a))
    }
    | l=sourceList PERIOD n=ID LPAREN RPAREN {
        ($startpos(n), FunctionCall(l,n,[]))
    }
    | n=ID LPAREN a=exprList RPAREN {($startpos(n), FunctionCall([],n,a))}
    | n=ID LPAREN RPAREN {($startpos(n), FunctionCall([],n,[]))}

///////////////////////////////////////////////////////////////////////////////

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





