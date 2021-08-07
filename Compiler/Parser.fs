module Parser

open FParsec
open System

//For the REPL: #r "nuget: FParsec";; open FParsec;;

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
    | Number of int
    | String of string
    | Bool of bool
    | PrimitiveFunc of (LispVal list -> LispVal)
    | Port of System.IO.FileStream

type LispParser<'T> = Parser<'T, unit>

let parseExpr, parseExprRef : LispParser<LispVal> * LispParser<LispVal> ref = createParserForwardedToRef ()

let chr c = skipChar c
let endBy p sep = many (p .>> sep)
let symbol : LispParser<char> = anyOf "!$%&|*+-/:<=>?@^_~#"
let numberSign : LispParser<char> = anyOf "+-"

//let parseList : LispParser<LispVal> = sepBy parseExpr spaces |>> List

let parseQuoted : LispParser<LispVal> =
    chr ("'" |> char) >>. parseExpr
    |>> fun expr -> List [ Atom "quote"; expr ]

let parseNumber : LispParser<LispVal> =
    many1Chars (digit .>> spaces)
    |>> (System.Int32.Parse >> Number)

let parseString : LispParser<LispVal> =
    parse {
        do! chr '"'
        let! xs = manyChars (noneOf "\"") .>> spaces
        do! chr '"'
        return String(xs)
    }

let parseList =
    between (pchar '(' .>> spaces) (pchar ')' .>> spaces) (many parseExpr)
    |>> List

let parseAtom : LispParser<LispVal> =
    parse {
        let! first = (letter <|> symbol)
        let! rest = manyChars (letter <|> symbol <|> digit) .>> spaces

        return
            match first.ToString() + rest with
            | "#t" -> Bool true
            | "#f" -> Bool false
            | atom -> Atom atom
    }

let runParserRef =
    //If possible, find a way to change the ref, this destroys potential parallelism
    do
        (parseExprRef
         := choice [ parseAtom
                     parseString
                     parseNumber
                     parseQuoted
                     parseList ])

let parseExpression (input: string) = run (spaces >>. many parseExpr) input

let parse (input: string array) =
    runParserRef
    parseExpression (System.String.Join(" ", input))
