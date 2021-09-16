module ILGenerator

open Parser
open System.IO
open FParsec.CharParsers

let write (text) =
    File.WriteAllText("Output.fs", text)
    
let consume (exp: string) =
    (Any exp)

let isKeyword (word) =
    match word with
    | "let" -> true, 1, "="
    |  _ -> false, 0, ""

let rec format (exps: LispTypes list) =
    let insert' (n, xs, elem) =
        List.append (List.append (List.take n xs) [elem]) (List.skip n xs)

    let parseFsharp (exps: LispTypes list) =
        exps
        |> List.map(fun x ->
                   match x with
                   | List x -> List.fold (fun acc x -> match x with | Any x -> acc + x | _ -> "") "" x
                   | Any x -> x)
        |> List.fold (fun acc x -> acc + x + " ") ""
    let mutable clone = exps
    exps
    |> List.iteri(fun index x ->
                 match x with
                 | Any x ->
                       match (isKeyword x) with
                       | true, amount, elem ->
                             printfn "%A" ((insert' ((index+amount+1), clone, Any elem) |> parseFsharp).TrimEnd())
                             write ((insert' ((index+amount+1), clone, Any elem) |> parseFsharp).TrimEnd())
                       | false, _, _ -> ();
                 | List x -> format x
                 | _ -> ())
    
let rec traverse (exps: ParserResult<LispTypes list, unit>) =
    match exps with
    | Success (expressions, _, _) ->
        expressions
        |> List.map(
            fun x -> 
            match x with
            | List x ->
                  List (traverse (Success (x, (), (FParsec.Position("", 0|>int64,0|>int64,0|>int64)))))
            | Atom x -> LispTypes.Any x
            | String x -> LispTypes.Any (" \""+x+"\"")
            | _ -> failwith "[ERROR] Non expected type")
    | Failure (x, _, _) -> failwithf "%A" x
    
