module ILGenerator

open Parser

let keywordsMatch (exp: LispVal) =
    [exp]

let rec traverse (ast: LispVal list) =
    match ast with
    | head::tail -> List.append (traverse [head]) (traverse tail);
    | [head] -> keywordsMatch head
        
