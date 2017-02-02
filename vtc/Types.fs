module Types

type TokenTypeEnum = 
    | Number of int
    | Semicolon
    | Plus 
    | Minus
    | Mul
    | Div
    | EOF
    | Lbrace
    | Rbrace
    | Lpar
    | Rpar
    | Leq
    | Ls
    | Geq
    | Gt
    | Eq
    | Assign
    | If
    | Else
    | Skip
    | While
    | Print
    | Ident of string
    



