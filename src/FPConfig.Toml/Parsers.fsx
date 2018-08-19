#load @"../../.paket/load/netstandard2.0/FParsec.fsx"
#load @"../../.paket/load/netstandard2.0/System.Time.fsx"

open System
open System.Text.RegularExpressions
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

//#region String parsers
let inline isCtrlChar c = 
    c <= '\u001f' || c = '\u007f'

let inline isNonSpaceControlChar c =
    isCtrlChar c && c <> '\n' && c <> '\r' && c <> '\t'

let pTomlSpaces             : Parser<string,unit> = manySatisfy (isAnyOf ['\t';' '])

let pBareKey                : Parser<string,unit> 
    = many1Satisfy (isAsciiLetter <||> isDigit <||> isAnyOf ['_';'-'])
let pEquals                 : Parser<char,unit>   = pchar '='

let pBasicStringContents     : Parser<string,unit> = 
    manySatisfy (isCtrlChar >> not <&&> isNoneOf ['\"'])
let pBasicString             : Parser<string,unit> = 
    between (pchar '\"') (pchar '\"') pBasicStringContents
let pLiteralString           : Parser<string,unit> = 
    manySatisfy (isNoneOf ['\'']) |> between (pchar '\'') (pchar '\'')
let pMultilineLiteralString  : Parser<string,unit> =
    optional newline >>. manyCharsTill anyChar (lookAhead (pstring "'''"))
    |> between (pstring "'''") (pstring "'''")

let pMultilineStringContents : Parser<char,unit> =
    satisfy (isNonSpaceControlChar >> not)
let pMultilineString         : Parser<string,unit> =
    optional newline >>. manyCharsTill pMultilineStringContents (lookAhead (pstring "\"\"\""))
    |> between (pstring "\"\"\"") (pstring "\"\"\"")
    |>> (fun s -> Regex.Replace(s, @"\\\s*", ""))
//#endregion

//#region Integer parsers
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
//#endregion

// NOTE: looks like we can use the build-in FParsec parser `pfloat`
//#region Float parser
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
//#endregion

(*
    Boolean Parsers
*)
let pBool : Parser<bool,unit> = pstring "true" <|> pstring "false" |>> Boolean.Parse

//#region Date parsers
let p4DigitInt : Parser<int,unit> = parray 4 digit |>> (String >> int)
let p2DigitInt : Parser<int,unit> = parray 2 digit |>> (String >> int)
let pSeconds   : Parser<(int*int),unit> = p2DigitInt .>>. opt (skipChar '.' >>. manyChars digit)
                                          |>> (fun (sec,frac) -> match frac with
                                                                 | Some str -> (sec, str.Substring(0, min str.Length 3) |> int)
                                                                 | _ -> (sec, 0))

let pSkipDash  : Parser<unit,unit> = skipChar '-'
let pDate      : Parser<Date,unit> = pipe5 p4DigitInt pSkipDash p2DigitInt pSkipDash p2DigitInt 
                                      (fun y _ m _ d -> Date(y,m,d))

let pSkipColon : Parser<unit,unit> = skipChar ':'
let pTime      : Parser<Time,unit> = pipe5 p2DigitInt pSkipColon p2DigitInt pSkipColon pSeconds
                                      (fun h _ m _ s -> Time(h,m,fst s, snd s))

let pOffset    : Parser<TimeSpan,unit> = 
    (pstring "Z" >>% TimeSpan(0,0,0)) <|> (pSign .>>. p2DigitInt .>> pSkipColon .>> pstring "00" 
                                           |>> (fun (sign,offset) -> match sign with
                                                                     | '-' -> -1 * offset |> (fun h -> TimeSpan(h,0,0))
                                                                     | _ -> TimeSpan(offset,0,0)))

let toDateTimeOffset (date:Date) _ (time:Time) offset = DateTimeOffset(date.Year, date.Month, date.Day, time.Hour, time.Minute, time.Second, time.Millisecond, offset)
let pOffsetDateTime : Parser<DateTimeOffset,unit> =
    pipe4 pDate (skipAnyOf [' ';'T']) pTime pOffset toDateTimeOffset

let toDateTime (date:Date) _ (time:Time) = DateTime(date.Year, date.Month, date.Day, time.Hour, time.Minute, time.Second, time.Millisecond)
let pLocalDateTime  : Parser<DateTime, unit> =
    pipe3 pDate (skipAnyOf [' ';'T']) pTime toDateTime
//#endregion

(*
    Array Parsers
*)
let pArrayOf<'a> (parser:Parser<'a,_>) : Parser<'a list, unit> =
    pchar '[' >>. (sepBy parser (pchar ',')) .>> pchar ']'
let pBasicStringArray = pArrayOf pBasicString
let pLiteralStringArray = pArrayOf pLiteralString
let pMultilineLiteralStringArray = pArrayOf pMultilineLiteralString
let pMultilineStringArray = pArrayOf pMultilineString

let pIntegerArray = pArrayOf pInteger
let pFloatArray = pArrayOf pFloat
let pBoolArray = pArrayOf pBool

(*
    Table Parsers
*)

