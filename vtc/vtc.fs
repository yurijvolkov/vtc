module vt

open System
open System.IO
open System.Text
open Lexer
open Types
open Parser
open Codegen
open Commands
let program = "
    header = 'Numbers from [1;100] that divides by 13:\n';
    prints header;
    nl = '\n';
    x = 1;
    while(x<100) {
        if( x/13 - (x-1)/13 > 0) {print x; prints nl}
        else skip;
        x = x+1
    };
    stop
"

let rec makeByteCode compiled code = 
    match  compiled with
    |(CodeElnmts.Com com)::t -> makeByteCode t ((LanguagePrimitives.EnumToValue com)::code)
    |(CodeElnmts.Num num)::t -> makeByteCode t (List.rev( Array.toList( BitConverter.GetBytes (int64 num)))@code)
    |[] -> code



let rec makeExe (code : byte list) (strPool : Collections.Generic.Dictionary<string, int>) = 
    use fs = new FileStream(@"C:\Users\yura2\Source\Repos\sysProgrammingProject\vm_fsharp\bin\Debug\tests\test.vtc", FileMode.Create)
    use bw = new BinaryWriter(fs)
    bw.Write( [|0xbauy;0xbauy|])
    bw.Write( int64 1)
    let funOffset = fs.Position
    bw.Write( int64 0)
    bw.Write( int64 (strPool.Count))
    for i in strPool do
        bw.Write( Encoding.ASCII.GetBytes(i.Key) )
        bw.Write( 0uy )
    
    let curPos = fs.Position
    fs.Seek(funOffset, SeekOrigin.Begin)
    bw.Write (curPos)
    fs.Seek(curPos, SeekOrigin.Begin)

    bw.Write( int64 1 )
    bw.Write(int64 strPool.["main"] )
    bw.Write(int64 varDict.Count )
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
    printfn "%A\n"  (fst ast) 
 //   printfn "%A" Codegen.prog
    visit (ASTnode.Sequence (fst ast)) 
    let compiled = Codegen.prog
    printfn "%A\n" (List.ofSeq compiled) 
    let byteCode = (List.rev (makeByteCode (List.ofSeq compiled) []))
    printfn "%A" byteCode
    strDict.["main"] <- strDict.Count
    makeExe byteCode strDict
    0