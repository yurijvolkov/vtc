module Types

type TokenTypeEnum = 
    | Number of int
    | String of string
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
    | Prints
    | Stop
    | Ident of string
    



