module vt

open System
open System.IO
open Lexer
open Types
open Parser
open Codegen
let program = @"
n=10;
print n;
while (x > 0) {x=x-1;}
"
//else {
//    if(z >= 2){
//     print 2;
//     print x;
//     }
//     else {print 3;}
//}


//x=x+2;
//x= 1;
//while (n> 0 ) {
//    y = y + x;
//    if (x ==0) { y = 10 } else skip;
//    print(x        );
//    print(y);
//    n =n-2;
//}

//let makeExec = 
//    use fs = new FileStream("try_exe.vtc",FileMode.OpenOrCreate)


[<EntryPoint>]
let main argv = 
    let prog =  (program+"$")
    printfn "%s" program
    let l = lexer (List.ofSeq prog) []
    printfn "%A" (List.rev l) 
    let ast = parseStatements (List.rev l) []
    printfn "%A" (List.rev (fst ast)) 
    printfn "%A" Codegen.prog
    visit (ASTnode.Sequence(List.rev (fst ast))) 
    let compiled = Codegen.prog
    printfn "%A" ""
    for i in compiled do
        match i with
        |string -> printf "String : "
        |Commands -> printf "Command : "
        printfn "%A" i
    0