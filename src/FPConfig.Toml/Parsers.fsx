#load @"../../.paket/load/netstandard2.0/FParsec.fsx"
open System
open FParsec

let (<||>) f g = fun x -> f x || g x
let (<&&>) f g = fun x -> f x && g x

let test parser str =
    match run parser str with
    | Success (s1, s2, s3) -> printfn "Ok: %A %A %A" s1 s2 s3
    | Failure (f1, f2, f3) -> printfn "Fail: %A %A %A" f1 f2 f3

(*
    Comment Parsers
*)
let pComment     : Parser<unit,unit>   = skipChar '#' >>. skipRestOfLine true

(*
    String parsers
*)
let inline isCtrlChar c = 
    c <= '\u001f' || c = '\u007f'

let inline isNonSpaceControlChar c =
    isCtrlChar c && c <> '\n' && c <> '\r' && c <> '\t'

let tomlSpaces  : Parser<string,unit> = manySatisfy (isAnyOf ['\t';' '])

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

(*
    Integer Parsers
*)
// Integer utility methods
let stripUnderscore = String.filter (function | '_' -> false | _ -> true)

let applySignToBigint (sign,n) = 
    match sign with
    | Some '-' -> bigint.MinusOne * n
    | _ -> n

let charDigit (c:char) = (int c) - (int '0')
let foldCharToBigint (radix:bigint) (bi:bigint) (c:char) = bi * radix + bigint (charDigit c)
let octalStringToBigint (raw:string) = seq raw |> Seq.fold (foldCharToBigint 8I) 0I
let binaryStringToBigint (raw:string) = seq raw |> Seq.fold (foldCharToBigint 2I) 0I

// Parsers
let pSign     : Parser<char,unit>   = satisfy (isAnyOf ['-';'+'])
let p_        : Parser<char,unit>   = pchar '_' .>> (notFollowedBy (pchar '_'))

let pDecimal  : Parser<bigint,unit> = 
    digit <|> p_ |> many1Chars |>> (stripUnderscore >> bigint.Parse)

let pHex      : Parser<bigint,unit> = 
    pstring "0x" >>. many1Chars hex
    |>> (fun n -> bigint.Parse("0" + n, Globalization.NumberStyles.HexNumber))

let pOctal    : Parser<bigint,unit> = 
    pstring "0o" >>. many1Chars octal |>> octalStringToBigint

let pBinary   : Parser<bigint,unit> = 
    pstring "0b" >>. many1Chars (anyOf ['0';'1']) |>> binaryStringToBigint

let pInteger  : Parser<bigint,unit> = 
    opt pSign .>>. (pHex <|> pOctal <|> pBinary <|> pDecimal)
    |>> applySignToBigint

(*
    Float Parsers
*)

(*
    Boolean Parsers
*)

(*
    Offset Date-Time Parsers
*)

(*
    Local Date-Time Parsers
*)

(*
    Local Time Parsers
*)

(*
    Array Parsers
*)

(*
    Table Parsers
*)

let quotedKey   : Parser<string,unit> = basicString <|> literalString
let key         : Parser<string,unit> = bareKey <|> quotedKey
let stringValue = basicString <|> literalString


let keyValuePair     : Parser<string*string,unit> = 
    tomlSpaces >>. (key .>> tomlSpaces .>> equals .>> tomlSpaces .>>. stringValue) .>> tomlSpaces

