module Lexer

open System.Text.RegularExpressions

let tokenize (expression: string) : string list =
    (Seq.filter
        (fun x -> x <> "")
        (Regex.Split(expression, "[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)"))
     |> List.ofSeq)
