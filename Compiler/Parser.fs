module Parser

open FParsec
open System
//#r "nuget: FParsec";; open FParsec;;

type Env = (string * LispVal ref) list ref

and FunctionMetadata =
    { closure: Env
      parms: string list
      varargs: string option
      body: LispVal list }

and LispVal =
    | Atom of string
    | Func of FunctionMetadata
    | List of LispVal list
    | Params of LispVal list
    | Number of int64
    | String of string
    | Bool of bool
    | PrimitiveFunc of (LispVal list -> LispVal)
    | Port of System.IO.FileStream

type LispParser<'T> = Parser<'T, unit>

let parseExpr, parseExprRef: LispParser<LispVal> * LispParser<LispVal> ref = createParserForwardedToRef ()

let chr c = skipChar c
let endBy p sep = many (p .>> sep)
let symbol: LispParser<char> = anyOf "!$%&|*+-/:<=>?@^_~#"

//let parseList : LispParser<LispVal> = sepBy parseExpr spaces |>> List

let parseQuoted: LispParser<LispVal> =
    chr '\'' .>> spaces >>. parseExpr
    |>> fun expr -> List [ Atom "quote"; expr ]

let parseNumber: LispParser<LispVal> = spaces >>. pint64 .>> spaces |>> Number

let parseString: LispParser<LispVal> =
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

let parseAtom: LispParser<LispVal> =
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
    |> unparse
