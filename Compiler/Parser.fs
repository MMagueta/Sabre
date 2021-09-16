module Parser

open FParsec
open System
//#r "nuget: FParsec";; open FParsec;;

type Env = Map<string, LispTypes>

and LispTypes =
    | Atom of string
    | List of LispTypes list
    | Params of LispTypes list
    | Number of int64
    | String of string
    | Bool of bool
    | Port of System.IO.FileStream
    | Any of string

type LispParser<'T> = Parser<'T, unit>

let parserBind: LispParser<LispTypes> * LispParser<LispTypes> ref = createParserForwardedToRef ()

let parseExpr = fst parserBind
let parseExprRef = snd parserBind

let chr c = skipChar c
let endBy p sep = many (p .>> sep)
let symbol: LispParser<char> = anyOf "!$%&|*+-/:<=>?@^_~#"

//let parseList : LispParser<LispTypes> = sepBy parseExpr spaces |>> List

let parseQuoted: LispParser<LispTypes> =
    chr '\'' .>> spaces >>. parseExpr
    |>> fun expr -> List [ Atom "quote"; expr ]

let parseNumber: LispParser<LispTypes> = spaces >>. pint64 .>> spaces |>> Number

let parseString: LispParser<LispTypes> =
    parse {
        do! chr '"'
        let! xs = manyChars (noneOf "\"") .>> spaces
        do! chr '"'
        return String(xs)
    }
    .>> spaces

let parseList =
    between (pchar '(' .>> spaces) (pchar ')' .>> spaces) (many parseExpr)
    |>> List

let parseParams =
    between (pchar '[' .>> spaces) (pchar ']' .>> spaces) (many parseExpr)
    |>> Params

let parseAtom: LispParser<LispTypes> =
    parse {
        let! first = letter <|> symbol

        let! rest = manyChars (letter <|> digit <|> symbol) .>> spaces

        return
            match first.ToString() + rest with
            | "#t" -> Bool true
            | "#f" -> Bool false
            | atom -> Atom atom
    }

let runParserRef () =
    do
        (parseExprRef
         := choice [ parseAtom
                     parseNumber
                     parseString
                     parseQuoted
                     parseParams
                     parseList ])

let parseExpression (input: string) = run (spaces >>. many parseExpr) input

let unparse (result) =
        match result with
        | ParserResult.Success (a,_,_) -> a
        | ParserResult.Failure (msg, _, _) -> failwithf "%A" msg
    
let parse (input: string list) =
    runParserRef ()
    parseExpression (String.Join(" ", input))
    // |> unparse
