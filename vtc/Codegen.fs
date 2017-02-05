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
let strDict = new Collections.Generic.Dictionary<string, int>()
let addStr str = 
        match strDict.ContainsKey(str) with
        |true -> genCodeN strDict.[str]
        |false ->
            strDict.[str] <- strDict.Count
            genCodeN strDict.[str]
    
let rec visit node  =
    match node with
    |ASTnode.Number num -> 
        genCodeC Commands.LOAD
        genCodeN  num 
    |ASTnode.Ident id -> 
        genCodeC Commands.LOADVAR
        addId id
    |ASTnode.String (str) ->
        genCodeC Commands.LOADS
        addStr str
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
    |ASTnode.PrintS (value) ->
        visit value
        genCodeC Commands.SPRINT
    |ASTnode.Skip ->
        0 |> ignore
    |ASTnode.Stop ->
        genCodeC Commands.STOP
    |ASTnode.If (cond, t_case, f_case) ->
        match cond with
        |ASTnode.LessOrEq (left, right) ->
            visit right
            visit left
            genCodeC Commands.JLEI
        |ASTnode.Less (left, right) ->
            visit right
            visit left
            genCodeC Commands.JLI
        |ASTnode.GreaterOrEq (left, right) ->
            visit right
            visit left
            genCodeC Commands.JMEI
        |ASTnode.Greater (left, right) ->
            visit right
            visit left
            genCodeC Commands.JMI
        |ASTnode.Equal (left, right) ->
            visit right
            visit left
            genCodeC Commands.ISUB
            genCodeC Commands.JZI
        genCodeN 0
        let f_pos = prog.Count
        visit f_case
        genCodeC Commands.JA
        genCodeN 0
        
        let t_pos = prog.Count
        visit t_case
        
        
        let l1 = (List.ofSeq (prog.GetRange(f_pos, t_pos - f_pos)))
        let l2 = (List.ofSeq (prog.GetRange(t_pos, prog.Count - t_pos)))
        let offs1 = List.fold( fun o i -> match i with
                        |CodeElnmts.Num _ -> o + 8
                        |CodeElnmts.Com _ -> o + 1) 0 l1
        let offs2 = List.fold( fun o i -> match i with
                        |CodeElnmts.Num _ -> o + 8
                        |CodeElnmts.Com _ -> o + 1) 0 l2
        
        
        prog.[f_pos-1] <- CodeElnmts.Num( offs1 )
        prog.[t_pos-1] <- CodeElnmts.Num( offs2 )
    |ASTnode.While (cond, body) ->
        let start_pos = prog.Count
        match cond with
        |ASTnode.LessOrEq (left, right) ->
            visit right
            visit left
            genCodeC Commands.JMI
        |ASTnode.Less (left, right) ->
            visit right
            visit left
            genCodeC Commands.JMEI
        |ASTnode.GreaterOrEq (left, right) ->
            visit right
            visit left
            genCodeC Commands.JLI
        |ASTnode.Greater (left, right) ->
            visit right
            visit left
            genCodeC Commands.JLEI
        |ASTnode.Equal (left, right) ->
            visit right
            visit left
            genCodeC Commands.ISUB
            genCodeC Commands.JNZI
        genCodeN 0
        let body_pos = prog.Count
        visit body
        genCodeC Commands.JA
        genCodeN 0
        let l1 = (List.ofSeq (prog.GetRange(body_pos, prog.Count - body_pos)))
        let l2 = (List.ofSeq (prog.GetRange(start_pos, prog.Count - start_pos)))
        let offs1 = List.fold( fun o i -> match i with
                        |CodeElnmts.Num _ -> o + 8
                        |CodeElnmts.Com _ -> o + 1) 0 l1
        let offs2 = List.fold( fun o i -> match i with
                        |CodeElnmts.Num _ -> o + 8
                        |CodeElnmts.Com _ -> o + 1) 0 l2
        prog.[body_pos - 1] <- CodeElnmts.Num(offs1)
        prog.[prog.Count - 1] <-  CodeElnmts.Num (offs2 * -1)
        printfn "Range(%d) : %A\n" (List.length l1) l1
        printfn "Range(%d) : %A\n" (List.length l2) l2
        
    |ASTnode.BinOp (left, right, op) ->
        visit right
        visit left
        match op with
        |0 -> genCodeC Commands.IADD
        |1 -> genCodeC Commands.ISUB
        |2 -> genCodeC Commands.IMUL
        |3 -> genCodeC Commands.IDIV
    |ASTnode.Sequence [] -> 
        prog |> ignore
    
