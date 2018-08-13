#load @"../../.paket/load/netstandard2.0/FParsec.fsx"
open System
open FParsec

let (<||>) f g = fun x -> f x || g x
let (<&&>) f g = fun x -> f x && g x

let inline isCtrlChar c = 
    c <= '\u001f' || c = '\u007f'

let inline isNonSpaceControlChar c =
    isCtrlChar c && c <> '\n' && c <> '\r' && c <> '\t'

let test parser str =
    match run parser str with
    | Success (s1, s2, s3) -> printfn "Ok: %A %A %A" s1 s2 s3
    | Failure (f1, f2, f3) -> printfn "Fail: %A %A %A" f1 f2 f3

let tomlSpaces  : Parser<string,unit> = manySatisfy (isAnyOf ['\t';' '])
let comment     : Parser<unit,unit>   = skipChar '#' >>. skipRestOfLine true
let bareKey     : Parser<string,unit> = many1Satisfy (isAsciiLetter <||> isDigit <||> isAnyOf ['_';'-'])
let equals      : Parser<char,unit>   = pchar '='

let basicStringContents   : Parser<string,unit> = 
    manySatisfy (isCtrlChar >> not <&&> isNoneOf ['\"'])
let basicString           : Parser<string,unit> = 
    between (pchar '\"') (pchar '\"') basicStringContents
let literalString         : Parser<string,unit> = 
    manySatisfy (isNoneOf ['\'']) |> between (pchar '\'') (pchar '\'')

let multiLineStringContents : Parser<char,unit> =
    satisfy (isNonSpaceControlChar >> not)
let multiLineString         : Parser<string,unit> =
    optional newline >>. manyCharsTill multiLineStringContents (lookAhead (pstring "\"\"\""))
    |> between (pstring "\"\"\"") (pstring "\"\"\"") 

let quotedKey   : Parser<string,unit> = basicString <|> literalString
let key         : Parser<string,unit> = bareKey <|> quotedKey
let stringValue = basicString <|> literalString


let keyValuePair     : Parser<string*string,unit> = 
    tomlSpaces >>. (key .>> tomlSpaces .>> equals .>> tomlSpaces .>>. stringValue) .>> tomlSpaces

