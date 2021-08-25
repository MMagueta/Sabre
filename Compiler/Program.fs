open System

open Lexer
open Parser

[<EntryPoint>]
let main argv =
    "(format \"Hello from Sabre :)\")"
    |> tokenize
    |> parse
    |> printfn "%A"

    0
