#r "FsLexYacc.Runtime.dll"

open Microsoft.FSharp.Text.Lexing
open System
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer


// ********************************************************************
//
//                    This code is incomplete
//
// ********************************************************************



type Action =
    | Bool_step of B
    | Assign_step of (String * A)
    | Skip_step

type State = 
    | Terminal
    | Nonterminal of ExecutionStep list
and ExecutionStep = Action * State


type Sign =
    | Plus
    | Minus
    | Zero


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
    | Number i ->       string i
    | UMinus a ->       "-" + (arithToString a)
    | Name s ->         s
    | Add (a1, a2) ->   "(" + (arithToString a1) + "+" + (arithToString a2) + ")"
    | Subtr (a1, a2) -> "(" + (arithToString a1) + "-" + (arithToString a2) + ")"
    | Mult (a1, a2) ->  "(" + (arithToString a1) + "*" + (arithToString a2) + ")"
    | Div (a1, a2) ->   "(" + (arithToString a1) + "/" + (arithToString a2) + ")"
    | Power (a1, a2) -> (arithToString a1) + "^" + (arithToString a2)

let rec boolToString bool =
    match bool with
    | True ->                    "true"
    | False ->                   "false"
    | And (b1, b2) ->            "(" + (boolToString b1) + "&" + (boolToString b2) + ")"
    | AndShort (b1, b2) ->       "(" + (boolToString b1) + "&&" + (boolToString b2) + ")"
    | Or (b1, b2) ->             "(" + (boolToString b1) + "|" + (boolToString b2) + ")"
    | OrShort (b1, b2) ->        "(" + (boolToString b1) + "||" + (boolToString b2) + ")"
    | Not b ->                   "!" + (boolToString b)
    | Equality (a1, a2) ->       (arithToString a1) + "=" + (arithToString a2)
    | Inequality (a1, a2) ->     (arithToString a1) + "!=" + (arithToString a2)
    | GreaterThan (a1, a2) ->    (arithToString a1) + ">" + (arithToString a2)
    | GreaterOrEqual (a1, a2) -> (arithToString a1) + ">=" + (arithToString a2)
    | LessThan (a1, a2) ->       (arithToString a1) + "<" + (arithToString a2)
    | LessOrEqual (a1, a2) ->    (arithToString a1) + "<=" + (arithToString a2)

let rec endDo guardedC =
    match guardedC with
    | Guarded (b, c) ->    Not b
    | GCList (gc1, gc2) -> And (endDo gc1, endDo gc2)

let rec edges qStart qEnd command (result, stateNum) = 
    match command with 
    | Assign (name, a) -> let label = constructLabel qStart ((arithToString name) + ":=" + (arithToString a)) qEnd
                          (result + label, stateNum)
    | Skip ->             let label = constructLabel qStart "skip" qEnd
                          (result + label, stateNum)
    | CList (c1, c2) ->   let qMid = stateNum + 1
                          let (newResult, newStateNum) = edges qStart (string qMid) c1 (result, qMid)
                          edges (string qMid) qEnd c2 (newResult, newStateNum)
    | If gC ->            gEdges qStart qEnd gC (result, stateNum)
    | Do gC ->            let b = endDo gC
                          let label = constructLabel qStart (boolToString b) qEnd
                          gEdges qStart qStart gC (result + label, stateNum)
and gEdges qStart qEnd guardedC (result, stateNum) =
    match guardedC with
    | Guarded (b, c) ->    let qMid = stateNum + 1
                           let label = constructLabel qStart (boolToString b) (string qMid)
                           edges (string qMid) qEnd c (result + label, qMid)
    | GCList (gc1, gc2) -> let (newResult, newStateNum) = gEdges qStart qEnd gc1 (result, stateNum)
                           gEdges qStart qEnd gc2 (newResult, newStateNum)


let freshState = Nonterminal []

let rec pgEdges startState endState command = 
    match command, startState with 
    | Assign (Name name, a), Nonterminal s ->
        let step_list = (Assign_step (name, a), endState) :: s
        Nonterminal step_list
    | Skip, Nonterminal steps ->
        let step_list = (Skip_step, endState) :: steps
        Nonterminal step_list
    | CList (c1, c2), Nonterminal steps ->
        let midState = pgEdges freshState endState c2
        pgEdges startState midState c1
    | If gC, Nonterminal steps ->
        let newSteps = pgGuarded startState endState gC
        Nonterminal (steps @ newSteps)
    | Do gC, Nonterminal steps ->
        let loop_done = endDo gC
        let done_step = (Bool_step loop_done, endState)
        let loop = pgGuarded startState startState gC
        Nonterminal (done_step :: loop)
    | _, Terminal ->
        failwith "Outgoing edge from end state."
    | _, _ ->
        failwith "Bad syntax, and/or bad AST structure on our part."
and pgGuarded startState endState guardedC =
    match guardedC, startState with
    | Guarded (b, c), Nonterminal steps ->
        let midState = pgEdges freshState endState c
        (Bool_step b, midState) :: steps
    | GCList (gc1, gc2), Nonterminal steps ->
        let step2 = pgGuarded startState endState gc2
        let step1 = pgGuarded startState endState gc1
        step1 @ step2 @ steps
    | _, Terminal ->
        failwith "Outgoing edge from end state."


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

(*let startState = "start"
let endState = "end";;*)


let rec eval_arith expr memory =
    match expr with
    | Number n ->           n
    | Name name ->          let exists = Map.containsKey name memory
                            if exists 
                                then Map.find name memory
                                else failwith ("Variable name not in memory: " + name)
    | Add (a1, a2) ->       let n1 = eval_arith a1 memory
                            let n2 = eval_arith a2 memory
                            n1 + n2
    | Subtr (a1, a2) ->     let n1 = eval_arith a1 memory
                            let n2 = eval_arith a2 memory
                            n1 - n2
    | Mult (a1, a2) ->      let n1 = eval_arith a1 memory
                            let n2 = eval_arith a2 memory
                            n1 * n2
    | Div (a1, a2) ->       let n1 = eval_arith a1 memory
                            let n2 = eval_arith a2 memory
                            n1 / n2
    | Power (a1, a2) ->     let n1 = eval_arith a1 memory
                            let n2 = eval_arith a2 memory
                            pown n1 n2
    | UMinus a ->           -(eval_arith a memory)


// Converts a number to its sign
let sign n =
    match n with
    | n when n>0 -> Plus
    | n when n<0 -> Minus
    | 0 ->          Zero

// TODO
// Addition of signs
let sign_addition a1 a2 signs =

// TODO
// Analyse signs of arithmetic expressions
let rec analyze_arith expr signs =
    match expr with
    | Number n ->           sign n
    | Name name ->          if Map.exists name then sign (Map.find name) else failwith "Implementation error"
    | Add (a1, a2) ->       sign_addition (analyze_arith a1 signs) (analyze_arith a2 signs)
    | Subtr (a1, a2) ->     
    | Mult (a1, a2) ->      
    | Div (a1, a2) ->       
    | Power (a1, a2) ->     
    | UMinus a ->           


let rec eval_bool expr memory =
    match expr with
    | True ->                       true
    | False ->                      false
    | And (b1, b2) ->               (eval_bool b1 memory) && (eval_bool b2 memory)
    | Or (b1, b2) ->                (eval_bool b1 memory) || (eval_bool b2 memory)
    | AndShort (b1, b2) ->          (eval_bool b1 memory) && (eval_bool b2 memory)
    | OrShort (b1, b2) ->           (eval_bool b1 memory) || (eval_bool b2 memory)
    | Not b ->                      not (eval_bool b memory)
    | Equality (a1, a2) ->          (eval_arith a1 memory) = (eval_arith a2 memory)
    | Inequality (a1, a2) ->        (eval_arith a1 memory) <> (eval_arith a2 memory)
    | GreaterThan (a1, a2) ->       (eval_arith a1 memory) > (eval_arith a2 memory)
    | GreaterOrEqual (a1, a2) ->    (eval_arith a1 memory) >= (eval_arith a2 memory)
    | LessThan (a1, a2) ->          (eval_arith a1 memory) < (eval_arith a2 memory)
    | LessOrEqual (a1, a2) ->       (eval_arith a1 memory) <= (eval_arith a2 memory)

// TODO
// Analyze outcomes of boolean expressions
let rec analyze_bool expr signs =
    match expr with
    | True ->                       set [true]
    | False ->                      set [false]
    | And (b1, b2) ->               //suitable_function_call_here (analyze_bool b1) (analyze_bool b2)
    | Or (b1, b2) ->                
    | AndShort (b1, b2) ->          
    | OrShort (b1, b2) ->           
    | Not b ->                      
    | Equality (a1, a2) ->          
    | Inequality (a1, a2) ->        
    | GreaterThan (a1, a2) ->       
    | GreaterOrEqual (a1, a2) ->    
    | LessThan (a1, a2) ->          
    | LessOrEqual (a1, a2) ->       

let rec interpret graph memory = 
    match graph with
    | Terminal ->               ("terminated", memory)
    | Nonterminal stepList ->   execute stepList memory
and execute stepList memory =
    match stepList with
    | [] ->                     ("stuck", memory)
    | step :: other_steps -> 
        match step with
        | (Skip_step, nextState) ->                 interpret nextState memory
        | (Bool_step b, nextState) ->               if (eval_bool b memory) 
                                                        then interpret nextState memory
                                                        else execute other_steps memory
        | (Assign_step (name, value), nextState) -> let new_mem = memory.Add (name, eval_arith value memory)
                                                    interpret nextState new_mem

// Analyse program graph
let rec analyze graph signs = 
    match graph with
    | Terminal ->               signs
    | Nonterminal stepList ->   execute stepList signs
and analyzeSteps stepList signs =
    match stepList with
    | [] ->                     failwith "Stuck"
    | step :: other_steps -> 
        match step with
        | (Skip_step, nextState) ->                 analyze nextState signs
        | (Bool_step b, nextState) ->               if (analyze_bool b signs) 
                                                        then analyze nextState signs
                                                        else analyzeSteps other_steps signs
        | (Assign_step (name, expr), nextState) -> let new_signs = memory.Add (name, analyze_arith expr signs)
                                                    analyse nextState new_signs

//let buildPG ast = pgEdges freshState Terminal ast

(*let eval_command c memory =
    match c with
    | Assign (name, a) ->   let num = eval_arith a memory
                         Map.add name num memory
    | Skip ->            memory
    | CList (c1, c2) -> let new_mem = eval_command c1 memory
                         eval_command c2 new_mem
    | If gc ->           eval_guarded gc memory
    | Do gc ->           eval_guarded gc memory
and eval_guarded gc memory =
    match gc with
    | Guarded (b, c) ->   let success = eval_bool b memory
                           if b then eval_command c memory else memory
    | GCList (gc1, gc2) -> let new_mem = eval_guarded gc1 memory
                           eval_guarded gc2 new_mem*)


// TODO
// Switch to analysis
let run =
    
    let input = Console.ReadLine()
    let ast = parse input
    let graph = pgEdges freshState Terminal ast
    
    let (state, memory) = interpret graph (Map.ofList([]))
    
    let memList = Map.toList memory
    printf "%A" memList

run

//while true do
    //run
