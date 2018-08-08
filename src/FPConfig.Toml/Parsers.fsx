#load @"../../.paket/load/netstandard2.0/FParsec.fsx"
open System
open FParsec

let (<||>) f g = fun x -> f x || g x
let (<&&>) f g = fun x -> f x && g x

let controlChars = 
    ['\u0000'; '\u0001'; '\u0002'; '\u0003'; '\u0004'; '\u0005'; '\u0006'; '\u0007';
     '\u0008'; '\u0009'; '\u000a'; '\u000b'; '\u000c'; '\u000d'; '\u000e'; '\u000f';
     '\u0010'; '\u0011'; '\u0012'; '\u0013'; '\u0014'; '\u0015'; '\u0016'; '\u0017';
     '\u0018'; '\u0019'; '\u001a'; '\u001b'; '\u001c'; '\u001d'; '\u001e'; '\u001f';
     '\u007f']

let nonSpaceCtrlChars =
    Set.difference (Set.ofList controlChars) (Set.ofList ['\n';'\r';'\t'])

let inline isCtrlChar c = 
    List.contains c controlChars

let test parser str =
    match run parser str with
    | Success (s1, s2, s3) -> printfn "Ok: %A %A %A" s1 s2 s3
    | Failure (f1, f2, f3) -> printfn "Fail: %A %A %A" f1 f2 f3

let tomlSpaces  : Parser<string,unit> = manySatisfy (isAnyOf ['\t';' '])
let comment     : Parser<unit,unit>   = skipChar '#' >>. skipRestOfLine true
let bareKey     : Parser<string,unit> = many1Satisfy (isAsciiLetter <||> isDigit <||> isAnyOf ['_';'-'])
let equals      : Parser<char,unit>   = pchar '='

let basicStringContents   : Parser<string,unit> = 
    manySatisfy (isNoneOf (controlChars @ ['\"';'\\']))
let basicString           : Parser<string,unit> = 
    between (pchar '\"') (pchar '\"') basicStringContents
let literalString         : Parser<string,unit> = 
    manySatisfy (isNoneOf ['\'']) |> between (pchar '\'') (pchar '\'')

let multiLineStringContents : Parser<string,unit> =
    manySatisfy (isNoneOf nonSpaceCtrlChars)
let multiLineString         : Parser<string,unit> =
    between (pstring "\"\"\"") (pstring "\"\"\"") (skipNewline >>. multiLineStringContents)

let quotedKey   : Parser<string,unit> = basicString <|> literalString
let key         : Parser<string,unit> = bareKey <|> quotedKey
let stringValue = basicString <|> literalString


let keyValuePair     : Parser<string*string,unit> = 
    tomlSpaces >>. (key .>> tomlSpaces .>> equals .>> tomlSpaces .>>. stringValue) .>> tomlSpaces

