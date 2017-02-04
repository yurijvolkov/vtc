module Lexer

open Types
open System

let head = function
    |h::t -> h

let tail = function
    |h::t -> t

let isIdentChar c = (Char.IsLetterOrDigit(c) || c='_')
let isDigit c = Char.IsDigit c
let isNumber s = (String.filter (fun c -> isDigit c <> true) s).Length = 0


let nextWord prog = 
    let space = List.findIndex (fun c -> isIdentChar c <> true) prog 
    String.Concat (fst(List.splitAt space prog))

let rec lexer prog tokens = 
    let h = head prog
    match h with
    |' ' | '\t' | '\n' | '\r' -> lexer (tail prog) tokens
    |';' ->  lexer (tail prog) (TokenTypeEnum.Semicolon::tokens)
    |'+' ->  lexer (tail prog) (TokenTypeEnum.Plus::tokens)
    |'-' ->  lexer (tail prog) (TokenTypeEnum.Minus::tokens)
    |'*' ->  lexer (tail prog) (TokenTypeEnum.Mul::tokens)
    |'/' ->  lexer (tail prog) (TokenTypeEnum.Div::tokens)
    |'$' ->  TokenTypeEnum.EOF::tokens
    |'(' ->  lexer (tail prog) (TokenTypeEnum.Lpar::tokens)
    |')' ->  lexer (tail prog) (TokenTypeEnum.Rpar::tokens)
    |'{' ->  lexer (tail prog) (TokenTypeEnum.Lbrace::tokens)
    |'}' ->  lexer (tail prog) (TokenTypeEnum.Rbrace::tokens)
    |'<' ->
        match prog.[1] with
            |'=' -> lexer (tail (tail prog)) (TokenTypeEnum.Leq::tokens)
            | _ -> lexer (tail prog) (TokenTypeEnum.Ls::tokens)
    |'>' ->
        match prog.[1] with
            |'=' -> lexer (tail(tail prog)) (TokenTypeEnum.Geq::tokens)
            | _ -> lexer (tail prog) (TokenTypeEnum.Gt::tokens)
    |'=' ->
        match prog.[1] with
            |'=' -> lexer (tail (tail prog)) (TokenTypeEnum.Eq::tokens)
            | _ -> lexer (tail prog) (TokenTypeEnum.Assign::tokens)
            
    | _  ->  
        match (nextWord prog) with
            | "if" -> lexer (snd(List.splitAt 2 prog)) (TokenTypeEnum.If::tokens)
            | "else" -> lexer (snd(List.splitAt 4 prog)) (TokenTypeEnum.Else::tokens)
            | "skip" -> lexer (snd(List.splitAt 4 prog)) (TokenTypeEnum.Skip::tokens)
            | "while" -> lexer (snd(List.splitAt 5 prog)) (TokenTypeEnum.While::tokens)
            | "print" -> lexer (snd(List.splitAt 5 prog)) (TokenTypeEnum.Print::tokens)
            | "stop" -> lexer (snd(List.splitAt 5 prog)) (TokenTypeEnum.Stop::tokens)
            | s when (isDigit s.[0] <> true) -> lexer (snd(List.splitAt s.Length prog)) ((TokenTypeEnum.Ident s)::tokens)
            | s when (isNumber s=true) -> lexer (snd(List.splitAt s.Length prog)) ((TokenTypeEnum.Number (int s))::tokens)

