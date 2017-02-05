module Parser

open Lexer
open Types

type ASTnode =
    |Number of int
    |Ident of string
    |Assignment of ASTnode*ASTnode  // ident * value
    |If of ASTnode*ASTnode*ASTnode  // condition * true_case * false_case
    |While of ASTnode*ASTnode       // condition * body
    |Skip                           
    |Stop
    |Print of ASTnode               // value
    |Sequence of (ASTnode list)     
    |Equal of ASTnode*ASTnode       // value == value
    |LessOrEq of ASTnode*ASTnode    // value <= value
    |Less of ASTnode*ASTnode        // value < value
    |GreaterOrEq of ASTnode*ASTnode // value >= value
    |Greater of ASTnode*ASTnode     // value > value
    |BinOp of ASTnode*ASTnode*int   // 0+ 1- 2* 3/
    
/// expr := expr0 "<" expr | expr0 "<=" expr  | expr0 "==" expr  
/// 	| expr0 ">" expr | expr0 ">=" expr | expr0 
let rec parseExpression lexems =
    let (left, othrs) = parseExpression0 lexems
    match othrs with
    | TokenTypeEnum.Ls::t ->
        let (right, othrs) = parseExpression t
        ASTnode.Less(left, right), othrs
    | TokenTypeEnum.Leq::t ->
        let(right, othrs) = parseExpression t
        ASTnode.LessOrEq(left, right), othrs
    | TokenTypeEnum.Eq::t ->
        let(right, othrs) = parseExpression t
        ASTnode.Equal(left, right), othrs
    | TokenTypeEnum.Gt::t ->
        let(right, othrs) = parseExpression t
        ASTnode.GreaterOrEq(left, right), othrs
    | TokenTypeEnum.Geq::t->
        let(right, othrs) = parseExpression t 
        ASTnode.Greater(left, right), othrs
    | _ -> left, othrs
    
/// expr0 = expr1 "+" expr0 | expr1 "-" expr0 | expr1
and parseExpression0 lexems = 
    let (left, othrs) = parseExpression1 lexems
    match othrs with
    | TokenTypeEnum.Plus::t ->
        let (right, othrs) = parseExpression0 t
        ASTnode.BinOp(left, right, 0), othrs
    | TokenTypeEnum.Minus::t ->
        let (right, othrs) = parseExpression0 t
        ASTnode.BinOp(left, right, 1), othrs
    | _ -> (left, othrs)

/// expr1 := atom "*" expr1 | atom "/" expr1 | atom
and parseExpression1 lexems = 
    let (left, othrs) = parseAtom lexems
    match othrs with
    | TokenTypeEnum.Mul::t ->
        let (right, othrs) = parseExpression1 t
        ASTnode.BinOp ( left, right, 2), othrs
    | TokenTypeEnum.Div::t ->
        let (right, othrs) = parseExpression1 t
        ASTnode.BinOp ( left, right, 3), othrs
    | _ -> left, othrs

/// atom := "(" expr ")" | NUMBER | IDENT
and parseAtom lexems =
    match lexems with
    |Types.Lpar::t -> 
        let (node, othrs) = parseExpression t
        (node, tail othrs)
    |Types.Number num::t->
        (ASTnode.Number num, t)
    |Types.Ident id::t->
        (ASTnode.Ident id, t) 

/// statement := "{" statements "}" | assignment | if | while | print | "skip" | "stop"
/// assignment := IDENT "=" expr
/// if := "if" "(" expr ")" statement "else" statement
/// while := "while" "(" expr ")" statement
/// print := "print" "(" expr ")"
let rec parseStatement lexems = 
        match lexems with
        |Types.Lbrace::t -> 
            let  (seq, othrs) = parseStatements t []
            (ASTnode.Sequence seq, othrs)
        |(TokenTypeEnum.Ident s)::t ->
            let ident = ASTnode.Ident s
            let (value, othrs) = parseExpression (tail t)
            ASTnode.Assignment  (ident, value),  othrs
        |Types.If::t ->
            let (condition, othrs) = parseAtom t
            let (t_stmnt, othrs) = parseStatement othrs
            let (f_stmnt, othrs) = parseStatement (tail othrs) 
            ASTnode.If(condition,  t_stmnt,  f_stmnt), tail othrs
        |Types.While::t ->
            let (condition, othrs) = parseAtom t
            let (body, othrs) = parseStatement othrs 
            ASTnode.While(condition,  body), othrs
        |Types.Print::t ->
            let (value, othrs) = parseAtom t
            ASTnode.Print(value), othrs
        |Types.Skip::t ->
            ASTnode.Skip, t
        |Types.Stop::t ->
            ASTnode.Stop, t

/// statements := statement | statement ";" statements
and parseStatements lexems stmts = 
    let (next, others) = parseStatement lexems 
    match others with
    |TokenTypeEnum.Semicolon::t -> (parseStatements t (next::stmts))
    |_ -> (next::stmts, others)


