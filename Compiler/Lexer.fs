module Lexer

open System.Text.RegularExpressions

let tokenize (expression: string): string array =
    (Seq.filter
        (fun x -> x <> "")
        (Regex.Split(expression, "[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)"))
     |> Array.ofSeq)

