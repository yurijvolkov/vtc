module vt

open System
open System.IO
open System.Text
open Lexer
open Types
open Parser
open Codegen
open Commands
let program = @"
    n = 10;
    print n;
    x = 5;
    while (x > 0) {
        print x;
        x=x-1
    };
    stop
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

let rec makeByteCode compiled code = 
    match  compiled with
    |(CodeElnmts.Com com)::t -> makeByteCode t ((LanguagePrimitives.EnumToValue com)::code)
    |(CodeElnmts.Num num)::t -> makeByteCode t (List.rev( Array.toList( BitConverter.GetBytes (int64 num)))@code)
    |[] -> code

let rec makeExe (code : byte list) = 
    use fs = new FileStream("test.vtc", FileMode.Create)
    use bw = new BinaryWriter(fs)
    bw.Write( [|0xbauy;0xbauy|])
    bw.Write( int64 1)
    let funOffset = fs.Position
    bw.Write( int64 0)
    bw.Write( int64 1)
    bw.Write( Encoding.ASCII.GetBytes("main") )
    bw.Write( 0uy )
    
    let curPos = fs.Position
    fs.Seek(funOffset, SeekOrigin.Begin)
    bw.Write (curPos)
    fs.Seek(curPos, SeekOrigin.Begin)

    bw.Write( int64 1 )
    bw.Write( int64 0 )
    bw.Write( int64 2 )
    bw.Write( int64 0 )
    bw.Write( int64 0 )
    bw.Write( int64 (List.length code) )
    bw.Write( List.toArray code )
    bw.Close()


[<EntryPoint>]
let main argv = 
    
    let prog =  (program+"$")
    printfn "%s" program
    let l = lexer (List.ofSeq prog) []
    printfn "%A\n" (List.rev l) 
    let ast = parseStatements (List.rev l) []
    printfn "%A\n" (List.rev (fst ast)) 
 //   printfn "%A" Codegen.prog
    visit (ASTnode.Sequence (fst ast)) 
    let compiled = Codegen.prog
    printfn "%A\n" (List.ofSeq compiled) 
    let byteCode = (List.rev (makeByteCode (List.ofSeq compiled) []))
    printfn "%A" byteCode
    makeExe byteCode
    0