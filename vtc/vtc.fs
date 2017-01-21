open System
open Lexer
open Types
let program = @"
n=10;
x= 1;
y = 1;
while (n> 0 ) {
    x=x+y;
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
    printf "%A" (List.rev l) 
    0