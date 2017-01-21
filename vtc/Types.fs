module Types

type TokenTypeEnum = 
    | Semicolon
    | Plus
    | Minus
    | Mul
    | Div
    | Sub
    | If
    | Then
    | Else
    | Skip
    | While
    | Lpar
    | Rpar
    | Lbrace
    | Rbrace
    | Lt
    | Leq
    | Gt
    | Geq
    | Eq
    | Assign
    | Print
    | Ident of string
    | Number of int
    | EOF




