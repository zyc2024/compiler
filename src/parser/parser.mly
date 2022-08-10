%{
    open Ast

    open SyntaxError

    (** given any ast node variant, [get_pos node] is the position of [node]. *)
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
%left LOR
%left LAND
%left BOR
%left BAND
%left DEQ NEQ
%left GT LT LTE GTE
%left ADD SUB
%left MUL DIV MOD 
%left LNOT BNOT 
%nonassoc CAST UMINUS
%left LSBRAC PERIOD

// %start <Ast.stmt_node> parse_stmt
%start <Ast.file> parse_module
%%

// parse_stmt: 
//     | s=stmt EOF {s}


parse_module:
    | imports=importList items=moduleFile EOF {
        Module(List.rev imports, List.rev items)
    }

(* Global level items*)

importList:
    | lst=importList IMPORT name=MODULE_ID {($startpos($2), name) :: lst}
    | {[]}

moduleFile:
    | lst=moduleFile def=typeDef {def :: lst}
    | lst=moduleFile def=functionDef {def :: lst}
    | lst=moduleFile def=globalDecl {def :: lst}
    | {[]}

typeDef:
    | TYPE module_name=MODULE_ID EQ LCBRAC field_list=varDeclStmtList RCBRAC {
        (TypeDef(($startpos(module_name), module_name), List.rev field_list))
    }

(* similar to varDeclList except the delimiter is the semicolon instead.*)
varDeclStmtList:
    | {[]}
    | vlst=varDeclStmtList v=varDecl SCOLON {v :: vlst}

functionDef:
    | fdecl=functionDecl LCBRAC slst=stmtList RCBRAC 
    { FunctionDef(fdecl, List.rev slst) }

(* type function_decl = (position * name, [var_decl*], [output_type*]))*)
functionDecl:
    | t=dataType name=ID LPAREN arg_list=varDeclList RPAREN 
    {(($startpos(name), name), List.rev arg_list, [t])}
    | out_list=reqDataTypeList name=ID LPAREN arg_list=varDeclList RPAREN 
    {(($startpos(name), name), List.rev arg_list, List.rev out_list)}
    | name=ID LPAREN arg_list=varDeclList RPAREN 
    {(($startpos(name), name), List.rev arg_list, [])}
    | VOID name=ID LPAREN arg_list=varDeclList RPAREN 
    {(($startpos(name), name), List.rev arg_list, [])}

varDeclList:
    | lst=reqVarDeclList {lst}
    | {[]}

reqVarDeclList:
    | vlst=reqVarDeclList COMMA v=varDecl {v :: vlst}
    | v=varDecl {[v]}

reqDataTypeList:
    | t1=dataType COMMA t2=dataType {[t2; t1]}
    | tlst=reqDataTypeList COMMA t=dataType {t :: tlst}

globalDecl:
    | v=varDecl SCOLON {
        let _, _, (pos, _) = v in (GlobalVarDecl(pos, v, None))
    }
    | v=varDecl EQ e=constant_expr SCOLON
    { GlobalVarDecl($startpos($2), v, Some(e))}

(* Statement level items *)

stmt:
    | s=completeStmt {s}
    | s=incompleteStmt SCOLON {s}
    
// these are statements that terminate without semicolons
completeStmt:
    | b=block {b}
    | WHILE LPAREN e=expr RPAREN s=stmt {($symbolstartpos, While(e,s))}
    | IF LPAREN e=expr RPAREN s=stmt %prec IF {($startpos, If(e, s, None))}
    | IF LPAREN e=expr RPAREN s1=stmt ELSE s2=stmt {($startpos, If(e,s1,Some(s2)))}
    | FOR LPAREN opt_s1=optForStmt SCOLON opt_e=optExpr SCOLON opt_s2=optForStmt 
        RPAREN s=stmt {($startpos, For(opt_s1, opt_e, opt_s2, s))}

block:
    | LCBRAC slst=stmtList RCBRAC {($startpos, Block(List.rev slst))}

stmtList:
    | slst=stmtList s=stmt {s :: slst}
    | {[]}

(* statements in for (...) do not have semicolons. That is why this rule is not
    named optStmt as the productions would then be epsilon | stmt *)
optForStmt:
    | {None}
    | s=completeStmt {Some(s)}
    | s=incompleteStmt {Some(s)}

optExpr:
    | {None}
    | e=expr {Some(e)}

(* these are statements that require a semicolon for completeness. *)
incompleteStmt:
    // declarations
    | d=varDecl {
        let _, _, (pos, _) = d in
        (pos, Declaration(d, None)) 
    }
    | d=varDecl EQ src=expr {
        (* choice of location: char[] str = 3; report error on "="*)
        ($startpos($2), Declaration(d, Some(src))) 
    }
    | md=multiDecl {md}
    // assignments
    | a=arrayInit {a}
    | dest=expr EQ src=expr {
        let (dest_pos, dest_expr) = dest in
        match dest_expr with
        | Var _ | FieldAccess _ | ArrayAccess _ 
        | ModuleAccess _ -> ($startpos($2), Assign(Some(dest), src))
        | _ -> raise (Not_a_location(dest_pos))
    }
    | USCORE EQ src=expr {($startpos($2), Assign(None, src))}
    | dest_list=destList EQ src=expr {
        List.iter (function 
            | None -> ()
            | Some (pos, e) -> 
                match e with
                | Var _ | FieldAccess _ | ArrayAccess _ | ModuleAccess _ -> ()
                | _ -> raise (Not_a_location(pos))
        ) dest_list;
        match src with
        |(_, FunctionCall _) -> ($startpos($2), MultiAssign(List.rev dest_list, src))
        |_ -> raise (Not_a_function_call (get_pos(src)))
    }
    // control-flow
    | BREAK {($startpos, Break)}
    | CONTINUE {($startpos, Continue)}
    | RETURN {($startpos, Return([]))}
    | RETURN elst=exprList {($startpos, Return(List.rev elst))}
    // arbitrary
    | e=expr {
        let (pos, expr) = e in
        match expr with
        (* move contents from function call into procedure call. *)
        | FunctionCall (source_list, name, arg_list) -> 
            (pos, ProcedureCall(source_list, name, arg_list))
        | _ -> raise (Not_a_statement(pos))
    }

// varDecl = 3-tuple (is_const? * data_type_node * (position * name))
varDecl:
    | CONST t=dataType name=ID {(true, t, ($startpos(name), name))}
    | t=dataType name=ID {(false, t, ($startpos(name), name))}

multiDecl:
    | CONST t=dataType names=nameList {
        (get_pos(t), MultiDeclaration(true, t, List.rev names))
    }
    | t=dataType names=nameList {
        (get_pos(t), MultiDeclaration(false, t, List.rev names))
    }

nameList:
    | id1=ID COMMA id2=ID   {[($startpos(id2), id2); ($startpos(id1), id1)]}
    | lst=nameList COMMA id=ID {($startpos(id), id) :: lst}

arrayInit:
    | data=completeDimensions
    {
        let pos, generate_type, dim_expr_list, name = data in
        let dims = List.length dim_expr_list in
        (pos, ArrayInit(generate_type dims, dim_expr_list, name))
    }
    | data=completeDimensions LSBRAC RSBRAC dim=kleenelrsbrac {
        let pos, generate_type, dim_expr_list, name = data in
        let dims = List.length dim_expr_list + dim + 1 in
        (pos, ArrayInit(generate_type dims, dim_expr_list, name))
    }

(* 4-tuple structured as given:
    (position, data_type_node generator function, [expr]+, var_name)*)
%inline completeDimensions:
    | name=ID COLON t=baseType dim_expr_list=kleenePlusDimension
    {
        let (type_pos, type_node) = t in
        let type_node_maker dims = (type_pos, match type_node with
        | Int _ -> Int(dims)
        | Char _ -> Char(dims)
        | Bool _ -> Bool(dims)
        | NameType (s, n, _) -> NameType(s, n, dims))
        in
        ($startpos(name), type_node_maker, List.rev dim_expr_list, name)
    }

baseType:
    | INT {($startpos, Int(0))}
    | CHAR {($startpos, Char(0))}
    | BOOL {($startpos, Bool(0))}
    | slst=sourceList {
        let pos, name = List.hd slst in
        (pos, NameType(List.rev (List.tl slst), name, 0))
    }

sourceList:
    | slst=sourceList PERIOD source_name=MODULE_ID {
        ($startpos(source_name), source_name ) :: slst
    }
    | source_name=MODULE_ID {[($startpos, source_name)]}    

kleenePlusDimension:
    | LSBRAC e=expr RSBRAC {[e]}
    | elst=kleenePlusDimension LSBRAC e=expr RSBRAC {e :: elst}

kleenelrsbrac:
    | total=kleenelrsbrac LSBRAC RSBRAC {total + 1}
    | {0}

destList:
    | loc1=expr_or_uscore COMMA loc2=expr_or_uscore {[loc2; loc1]}
    | loc_list=destList COMMA loc=expr_or_uscore {loc :: loc_list}

expr_or_uscore:
    | e=expr {Some(e)}
    | USCORE {None}

exprList:
    | e=expr {[e]}
    | elst=exprList COMMA e=expr {e :: elst}

dataType:
    | INT dim=kleenelrsbrac {($startpos, Int(dim))}
    | CHAR dim=kleenelrsbrac {($startpos, Char(dim))}
    | BOOL dim=kleenelrsbrac {($startpos, Bool(dim))}
    | slst=sourceList {
        let pos, name = List.hd slst in
        (pos, NameType(List.rev (List.tl slst), name, 0))
    } 
    | slst=sourceList LSBRAC RSBRAC dim=kleenelrsbrac {
        let pos, name = List.hd slst in
        (pos, NameType(List.rev (List.tl slst), name, dim+1))
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
    | c=constructorCall {c}

fieldAccess:
    | e=expr PERIOD field_name=ID {($startpos($2), FieldAccess(e, field_name))}

moduleAccess:
    | slst=sourceList PERIOD var_name=ID 
    {($startpos(var_name), ModuleAccess(List.rev slst, var_name))}
    
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
    | LSBRAC elst=exprList RSBRAC {($startpos, ArrayLiteral(List.rev elst))}
    | LSBRAC elst=exprList COMMA RSBRAC {($startpos, ArrayLiteral(List.rev elst))}
    | LSBRAC RSBRAC {($startpos, ArrayLiteral([]))}

functionCall:
    | l=sourceList PERIOD n=ID LPAREN a=exprList RPAREN {
        ($startpos(n), FunctionCall(List.rev l, n, List.rev a))
    }
    | l=sourceList PERIOD n=ID LPAREN RPAREN {
        ($startpos(n), FunctionCall(List.rev l, n, []))
    }
    | n=ID LPAREN a=exprList RPAREN {
        ($startpos(n), FunctionCall([], n , List.rev a))
    }
    | n=ID LPAREN RPAREN {($startpos(n), FunctionCall([], n, []))}


constructorCall:
    | l=sourceList LPAREN a=namedArgsList RPAREN {
        let npos, n = List.hd l in
        (npos, ConstructorCall(List.rev (List.tl l), n, a)) }

namedArgsList:
    | k=reqNamedArgsList {List.rev k}
    | {[]}

reqNamedArgsList:
    | l=reqNamedArgsList COMMA n=ID EQ e=expr {($startpos(n),n,e)::l}
    | n=ID EQ e=expr {[($symbolstartpos, n,e )]}
