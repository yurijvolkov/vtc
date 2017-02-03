module Printer

open System
open Parser

let rec DFS ast (locals:Collections.Generic.Dictionary<string, int>) = 
    match ast with
    |ASTnode.Assignment( id, value) ->
        let name = DFS id locals
        let value = DFS value locals
        if( locals.ContainsKey(name) = false) then locals.[name] <- 2   
        "SADAS"     