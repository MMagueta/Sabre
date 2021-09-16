open System

open Lexer
open Parser
open ILGenerator

[<EntryPoint>]
let main _ =
    "(printfn \"Hello from Sabre :)\")"
    |> tokenize
    |> parse
    |> printfn "%A"

    """
    (let fib [num num2]
      (match num
       ((x when (< x 2)) -> (x))
        (x when (>= 2)) -> (+ (fib (- x 1)) (fib (- x 2)))))
    """
    |> tokenize
    |> parse
    |> printfn "%A"

    "(let x (printfn \"Hello from Sabre :)\"))"
    |> tokenize
    |> parse
    |> traverse
    |> format
    |> printfn "%A"

    0
