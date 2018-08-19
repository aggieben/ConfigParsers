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
let hexStringToBigint (raw:string) = bigint.Parse("0" + raw, Globalization.NumberStyles.HexNumber)
let octalStringToBigint (raw:string) = seq raw |> Seq.fold (foldCharToBigint 8I) 0I
let binaryStringToBigint (raw:string) = seq raw |> Seq.fold (foldCharToBigint 2I) 0I

//let fp_ (pChar:Parser<char,unit>) : Parser<char,unit> = pChar >>? pchar '_' .>>? pChar
let p_ : Parser<char,unit> = pchar '_' .>> (notFollowedBy (pchar '_'))

// Parsers
let pSign     : Parser<char,unit>   = satisfy (isAnyOf ['-';'+'])
let pNoLeading0 : Parser<unit,unit> = notFollowedByL (pchar '0' .>> lookAhead anyChar) "leading zero not allowed"

let pDecimal  : Parser<bigint,unit> =
//    let p_ = fp_ digit
    pNoLeading0 >>. ((digit <|> p_) |> many1Chars)
    |>> (stripUnderscore >> bigint.Parse)

let pHex      : Parser<bigint,unit> = 
//    let p_ = fp_ hex
    pstring "0x" >>. (hex <|> p_ |> many1Chars)
    |>> (stripUnderscore >> hexStringToBigint)

let pOctal    : Parser<bigint,unit> = 
//    let p_ = fp_ octal
    pstring "0o" >>. (octal <|> p_) |> many1Chars
    |>> (stripUnderscore >> octalStringToBigint)

let pBinary   : Parser<bigint,unit> = 
    let binary = anyOf ['0';'1']
//    let p_ = fp_ binary
    pstring "0b" >>. (binary <|> p_ |> many1Chars)
    |>> (stripUnderscore >> binaryStringToBigint)

let pInteger  : Parser<bigint,unit> = 
    opt pSign .>>. (pHex <|> pOctal <|> pBinary <|> pDecimal)
    |>> applySignToBigint

(*
    Float Parsers
*)
// float utility functions
let parseTomlFloat = function
                     | "inf" -> infinity
                     | "nan" -> nan
                     | raw -> Convert.ToDouble raw

let applySignToFloat (sign,f) =
    match sign with
    | Some '-' -> -1.0 * f
    | _ -> f

// Parsers
let pSpecial        : Parser<string,unit> = pstring "inf" <|> pstring "nan"
let pFloat          : Parser<float,unit> =
    opt pSign .>>. (pSpecial <|> (many1Chars digit) |>> (stripUnderscore >> parseTomlFloat))
    |>> applySignToFloat

// NOTE: looks like we can use the build-in FParsec parser `pfloat`

(*
    Boolean Parsers
*)
let pBool : Parser<bool,unit> = pstring "true" <|> pstring "false" |>> Boolean.Parse

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

