module Codegen

open System
open Parser
open Types
open Commands

type CodeElnmts = 
    |Str of string
    |Num of int
    |Com of Commands


let prog = new Collections.Generic.List<CodeElnmts>()

let genCodeS s =
     prog.Add (CodeElnmts.Str s)

let genCodeN num =
    prog.Add (CodeElnmts.Num num)

let genCodeC com =
    prog.Add (CodeElnmts.Com com)

let varDict = new Collections.Generic.Dictionary<string, int>()
let addId id = 
         match varDict.ContainsKey(id) with
        |true -> genCodeN varDict.[id]
        |false -> 
            varDict.[id] <- varDict.Count
            genCodeN varDict.[id]
    
let rec visit node  =
    match node with
    |ASTnode.Number num -> 
        genCodeC Commands.LOAD
        genCodeN  num 
    |ASTnode.Ident id -> 
        genCodeC Commands.LOADVAR
        addId id
    |ASTnode.Assignment (ASTnode.Ident id, value) ->
        visit value 
        genCodeC Commands.STOREVAR
        addId id
    |ASTnode.Sequence (h::t) ->
        visit h 
        visit (ASTnode.Sequence t) 
    |ASTnode.Print (value) ->
        visit value
        genCodeC Commands.IPRINT
    |ASTnode.Skip ->
        0 |> ignore
    |ASTnode.If (cond, t_case, f_case) ->
        match cond with
        |ASTnode.LessOrEq (left, right) ->
            visit left
            visit right
            genCodeC Commands.JLEI
        |ASTnode.Less (left, right) ->
            visit left
            visit right
            genCodeC Commands.JLI
        |ASTnode.GreaterOrEq (left, right) ->
            visit left
            visit right
            genCodeC Commands.JMEI
        |ASTnode.Greater (left, right) ->
            visit left
            visit right
            genCodeC Commands.JMI
        genCodeN 0
        let f_pos = prog.Count
        visit f_case
        genCodeC Commands.JA
        genCodeN 0
        let t_pos = prog.Count
        visit t_case
        prog.[f_pos-1] <- CodeElnmts.Num( t_pos - f_pos )
        prog.[t_pos-1] <- CodeElnmts.Num(prog.Count - t_pos)
    |ASTnode.While (cond, body) ->
        let start_pos = prog.Count
        match cond with
        |ASTnode.LessOrEq (left, right) ->
            visit left
            visit right
            genCodeC Commands.JMI
        |ASTnode.Less (left, right) ->
            visit left
            visit right
            genCodeC Commands.JMEI
        |ASTnode.GreaterOrEq (left, right) ->
            visit left
            visit right
            genCodeC Commands.JLI
        |ASTnode.Greater (left, right) ->
            visit left
            visit right
            genCodeC Commands.JLEI
        genCodeN 0
        let body_pos = prog.Count
        visit body
        genCodeC Commands.JA
        genCodeN 0
        prog.[body_pos - 1] <- CodeElnmts.Num(prog.Count - body_pos)
        prog.[prog.Count - 1] <- CodeElnmts.Num (start_pos - prog.Count)
    |ASTnode.BinOp (left, right, op) ->
        visit left
        visit right
        match op with
        |0 -> genCodeC Commands.IADD
        |1 -> genCodeC Commands.ISUB
        |2 -> genCodeC Commands.IMUL
        |3 -> genCodeC Commands.IDIV
    |ASTnode.Sequence [] -> 
        prog |> ignore
    
