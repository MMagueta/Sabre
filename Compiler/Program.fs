open System

open Lexer
open Parser

[<EntryPoint>]
let main argv =
    "(format \"Hello from Sabre :)\")"
    |> tokenize
    |> parse
    |> printfn "%A"

    """
    (defun fib [num num2]
      (match num
       ((x when (< x 2)) -> (x))
        (x when (>= 2)) -> (+ (fib (- x 1)) (fib (- x 2)))))
    """
    |> tokenize
    |> parse
    |> printfn "%A"

    """
    (setv var1 'abc)
    """
    |> tokenize
    |> parse
    |> printfn "%A"


    0
