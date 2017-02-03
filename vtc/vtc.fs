module vt

open System
open Lexer
open Types
open Parser

let program = @"
n=10;
x=y*(x+y*2);
x= 1;
while (n> 0 ) {
    y = y + x;
    if (x ==0) { y = 10 } else skip;
    print(x        );
    print(y);
    n =n-2;
}
"


[<EntryPoint>]
let main argv = 
    let dict = new Collections.Generic.Dictionary<string, int>()
    dict.["a"] <- 1
    dict.["b"] <- 2
    if (dict.ContainsKey("a")) then printfn "a" 
    printfn "%d %d" dict.Count dict.["a"]
    let prog =  (program+"$")
    printfn "%s" program
    let l = lexer (List.ofSeq prog) []
    printfn "%A" (List.rev l) 
    let ast = parseStatements (List.rev l) []
    printfn "%A" (List.rev (fst ast)) 
    
    0