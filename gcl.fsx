#r "FsLexYacc.Runtime.dll"

open Microsoft.FSharp.Text.Lexing
open System
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer

// Error message stuff is from
// https://stackoverflow.com/questions/7928438/meaningful-errors-during-parsing-with-fsyacc
let parse input =
    let lexbuf = LexBuffer<_>.FromString input
    //try
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    //let smth lexbuf = GCLLexer.tokenize lexbuf
    //let res = GCLParser.start smth lexbuf
    res
    (*with e ->
        let position = lexbuf.EndPos
        let line = position.Line
        let column = position.Column
        let message = e.Message
        let lastToken = new System.String(lexbuf.Lexeme)
        printf "Parse failed at line %d, column %d:\n" line column
        printf "Last token: %s" lastToken
        printf "\n"
        printfn "%A" lexbuf
        exit 1  *)

let constructLabel qStart act qEnd =
    //printfn "q%s -> q%s [label = \"%s\"];" qStart qEnd act
    sprintf "\nq%s -> q%s [label = \"%s\"];" qStart qEnd act

let rec arithToString expr =
    match expr with
    | Number i -> string i
    | UMinus a -> "-" + (arithToString a)
    | Name s -> s
    | Add (a1, a2) -> "(" + (arithToString a1) + "+" + (arithToString a2) + ")"
    | Subtr (a1, a2) -> "(" + (arithToString a1) + "-" + (arithToString a2) + ")"
    | Mult (a1, a2) -> "(" + (arithToString a1) + "*" + (arithToString a2) + ")"
    | Div (a1, a2) -> "(" + (arithToString a1) + "/" + (arithToString a2) + ")"
    | Power (a1, a2) -> (arithToString a1) + "^" + (arithToString a2)

let rec boolToString bool =
    match bool with
    | True -> "true"
    | False -> "false"
    | And (b1, b2) -> "(" + (boolToString b1) + "&" + (boolToString b2) + ")"
    | AndShort (b1, b2) -> "(" + (boolToString b1) + "&&" + (boolToString b2) + ")"
    | Or (b1, b2) -> "(" + (boolToString b1) + "|" + (boolToString b2) + ")"
    | OrShort (b1, b2) -> "(" + (boolToString b1) + "||" + (boolToString b2) + ")"
    | Not b -> "!" + (boolToString b)
    | Equality (a1, a2) -> (arithToString a1) + "=" + (arithToString a2)
    | Inequality (a1, a2) -> (arithToString a1) + "!=" + (arithToString a2)
    | GreaterThan (a1, a2) -> (arithToString a1) + ">" + (arithToString a2)
    | GreaterOrEqual (a1, a2) -> (arithToString a1) + ">=" + (arithToString a2)
    | LessThan (a1, a2) -> (arithToString a1) + "<" + (arithToString a2)
    | LessOrEqual (a1, a2) -> (arithToString a1) + "<=" + (arithToString a2)

let rec endDo guardedC =
    match guardedC with
    | Guarded (b, c) -> Not b
    | GCList (gc1, gc2) -> And (endDo gc1, endDo gc2)

let rec edges qStart qEnd command (result, stateNum) = 
    match command with 
    | Assign (name, a) -> 
        let label = constructLabel qStart ((arithToString name) + ":=" + (arithToString a)) qEnd
        (result + label, stateNum)
    | Skip -> 
        let label = constructLabel qStart "skip" qEnd
        (result + label, stateNum)
    | CList (c1, c2) -> 
        let qMid = stateNum + 1
        let (newResult, newStateNum) = edges qStart (string qMid) c1 (result, qMid)
        edges (string qMid) qEnd c2 (newResult, newStateNum)
    | If gC -> 
        gEdges qStart qEnd gC (result, stateNum)
    | Do gC ->
        let b = endDo gC
        let label = constructLabel qStart (boolToString b) qEnd
        gEdges qStart qStart gC (result + label, stateNum)
and gEdges qStart qEnd guardedC (result, stateNum) =
    match guardedC with
    | Guarded (b, c) ->
        let qMid = stateNum + 1
        let label = constructLabel qStart (boolToString b) (string qMid)
        edges (string qMid) qEnd c (result + label, qMid)
    | GCList (gc1, gc2) ->
        let qMid = stateNum + 1
        let (newResult, newStateNum) = gEdges qStart (string qMid) gc1 (result, qMid)
        gEdges (string qMid) qEnd gc2 (newResult, newStateNum)

let printHeader = 
    //printfn "digraph program_graph {rankdir=LR;"
    //printfn "node [shape = circle]; q▷;"
    //printfn "node [shape = doublecircle]; q◀; "
    //printfn "node [shape = circle]"
    //printfn "}"
    "
digraph program_graph {rankdir=LR;
node [shape = circle]; q▷;
node [shape = doublecircle]; q◀;
node [shape = circle]
}"

let startState = "▷"
let endState = "◀";;

let run =
    //let input = parse (Console.ReadLine())
    let input = Console.ReadLine()
    let ast = parse input
    //try
    let (output, _) = edges startState endState ast (printHeader, 0)
    //edges startState endState input 0
    //with err -> printfn "Error!"
    printfn "%s" output

run

//while true do
    //run
