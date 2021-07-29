module REPL

open System
open System.Text.RegularExpressions

let READ () : string = Console.ReadLine()

let EVAL (expression: string) = expression

let PRINT (expression) = printfn "%A" expression

let rec REPL () : unit =
    printf "USER> "

    READ()
    |> (fun x ->
        match x with
        | "(quit)" -> ()
        | _ ->
            x |> EVAL |> PRINT
            REPL())

let run =
    REPL()
    0
