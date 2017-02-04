module Codegen

open System
open Parser
open Types
open Commands


let prog = new Collections.ArrayList()

let genCode v =
    prog.Add v |> ignore

let varDict = new Collections.Generic.Dictionary<string, int>()
let addId id = 
         match varDict.ContainsKey(id) with
        |true -> genCode (varDict.[id])
        |false -> 
            varDict.[id] <- varDict.Count
            genCode (varDict.[id])
    
let rec visit node  =
    match node with
    |ASTnode.Number num -> 
        genCode Commands.LOAD
        genCode num 
    |ASTnode.Ident id -> 
        genCode Commands.LOADVAR
        addId id
    |ASTnode.Assignment (ASTnode.Ident id, value) ->
        visit value 
        genCode Commands.STOREVAR
        addId id
    |ASTnode.Sequence (h::t) ->
        visit h 
        visit (ASTnode.Sequence t) 
    |ASTnode.Print (value) ->
        visit value
        genCode Commands.IPRINT
    |ASTnode.Skip ->
        0 |> ignore
    |ASTnode.If (cond, t_case, f_case) ->
        match cond with
        |ASTnode.LessOrEq (left, right) ->
            visit left
            visit right
            genCode Commands.JLEI
        |ASTnode.Less (left, right) ->
            visit left
            visit right
            genCode Commands.JLI
        |ASTnode.GreaterOrEq (left, right) ->
            visit left
            visit right
            genCode Commands.JMEI
        |ASTnode.Greater (left, right) ->
            visit left
            visit right
            genCode Commands.JMI
        genCode (int 0)
        let f_pos = prog.Count
        visit f_case
        genCode Commands.JA
        genCode 0
        let t_pos = prog.Count
        visit t_case
        prog.[f_pos-1] <- t_pos - f_pos
        prog.[t_pos-1] <- (prog.Count - t_pos)
    |ASTnode.While (cond, body) ->
        let start_pos = prog.Count
        match cond with
        |ASTnode.LessOrEq (left, right) ->
            visit left
            visit right
            genCode Commands.JMI
        |ASTnode.Less (left, right) ->
            visit left
            visit right
            genCode Commands.JMEI
        |ASTnode.GreaterOrEq (left, right) ->
            visit left
            visit right
            genCode Commands.JLI
        |ASTnode.Greater (left, right) ->
            visit left
            visit right
            genCode Commands.JLEI
        genCode (int 0)
        let body_pos = prog.Count
        visit body
        genCode Commands.JA
        genCode 0
        prog.[body_pos - 1] <- prog.Count - body_pos
        prog.[prog.Count - 1] <- start_pos - prog.Count
    |ASTnode.BinOp (left, right, op) ->
        visit left
        visit right
        match op with
        |0 -> genCode Commands.IADD
        |1 -> genCode Commands.ISUB
        |2 -> genCode Commands.IMUL
        |3 -> genCode Commands.IDIV
    |ASTnode.Sequence [] -> 
        prog |> ignore
    
