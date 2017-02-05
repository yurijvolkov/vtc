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
    match prog with
    |(' ' | '\t' | '\n' | '\r')::t -> lexer t tokens
    |';'::t ->  lexer t (TokenTypeEnum.Semicolon::tokens)
    |'+'::t ->  lexer t (TokenTypeEnum.Plus::tokens)
    |'-'::t ->  lexer t (TokenTypeEnum.Minus::tokens)
    |'*'::t ->  lexer t (TokenTypeEnum.Mul::tokens)
    |'/'::t ->  lexer t (TokenTypeEnum.Div::tokens)
    |'$'::t ->  TokenTypeEnum.EOF::tokens
    |'('::t ->  lexer t (TokenTypeEnum.Lpar::tokens)
    |')'::t ->  lexer t (TokenTypeEnum.Rpar::tokens)
    |'{'::t ->  lexer t (TokenTypeEnum.Lbrace::tokens)
    |'}'::t ->  lexer t (TokenTypeEnum.Rbrace::tokens)
    |'<'::t ->
        match prog.[1] with
            |'=' -> lexer (tail t) (TokenTypeEnum.Leq::tokens)
            | _ -> lexer t (TokenTypeEnum.Ls::tokens)
    |'>'::t->
        match t with
            |'='::tt -> lexer tt (TokenTypeEnum.Geq::tokens)
            | _ -> lexer t (TokenTypeEnum.Gt::tokens)
    |'='::t ->
        match t with
            |'='::tt -> lexer tt (TokenTypeEnum.Eq::tokens)
            | _ -> lexer t (TokenTypeEnum.Assign::tokens)
    |'''::t ->
            let strLen = List.findIndex(fun c -> c=''') t
            let split = List.splitAt strLen t 
            let str = String.Concat(fst split)
            let othrs = tail (snd split)
            lexer othrs (TokenTypeEnum.String(str)::tokens)
    | _  -> 
        match (nextWord prog) with
            | "if" -> lexer (snd(List.splitAt 2 prog)) (TokenTypeEnum.If::tokens)
            | "else" -> lexer (snd(List.splitAt 4 prog)) (TokenTypeEnum.Else::tokens)
            | "skip" -> lexer (snd(List.splitAt 4 prog)) (TokenTypeEnum.Skip::tokens)
            | "while" -> lexer (snd(List.splitAt 5 prog)) (TokenTypeEnum.While::tokens)
            | "print" -> lexer (snd(List.splitAt 5 prog)) (TokenTypeEnum.Print::tokens)
            | "prints" -> lexer (snd(List.splitAt 6 prog)) (TokenTypeEnum.Prints::tokens)
            | "stop" -> lexer (snd(List.splitAt 4 prog)) (TokenTypeEnum.Stop::tokens)
            | s when (isDigit s.[0] <> true) -> lexer (snd(List.splitAt s.Length prog)) ((TokenTypeEnum.Ident s)::tokens)
            | s when (isNumber s=true) -> lexer (snd(List.splitAt s.Length prog)) ((TokenTypeEnum.Number (int s))::tokens)

