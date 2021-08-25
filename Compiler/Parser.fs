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
    | Number of int64
    | String of string
    | Bool of bool
    | PrimitiveFunc of (LispVal list -> LispVal)
    | Port of System.IO.FileStream

type LispParser<'T> = Parser<'T, unit>

let parseExpr, parseExprRef : LispParser<LispVal> * LispParser<LispVal> ref = createParserForwardedToRef ()

let chr c = skipChar c
let endBy p sep = many (p .>> sep)
let symbol : LispParser<char> = anyOf "!$%&|*+-/:<=>?@^_~#"

//let parseList : LispParser<LispVal> = sepBy parseExpr spaces |>> List

let parseQuoted : LispParser<LispVal> =
    chr ("'" |> char) >>. parseExpr
    |>> fun expr -> List [ Atom "quote"; expr ]

let parseNumber : LispParser<LispVal> = pint64 .>> spaces |>> Number

let parseString : LispParser<LispVal> =
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

let parseAtom : LispParser<LispVal> =
    parse {
        let! first = (letter <|> symbol) .>> spaces

        let! rest = manyChars (letter <|> symbol) .>> spaces

        return
            match first.ToString() + rest with
            | "#t" -> Bool true
            | "#f" -> Bool false
            | atom -> Atom atom
    }

let runParserRef () =
    do
        (parseExprRef
         := choice [ parseNumber
                     parseAtom
                     parseString
                     parseQuoted
                     parseList ])

let parseExpression (input: string) = run (spaces >>. many parseExpr) input

let parse (input: string list) =
    runParserRef ()
    parseExpression (String.Join(" ", input))
