open System
open Lexer
open Types
open Parser

let program = @"
n=10;
x=x+y;
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
    let prog =  (program+"$")
    printfn "%s" program
    let l = lexer (List.ofSeq prog) []
    printfn "%A" (List.rev l) 
    let ast = parseStatements (List.rev l) []
    printfn "%A" (List.rev (fst ast)) 
    
    0