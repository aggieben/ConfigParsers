#load @"../../.paket/load/netstandard2.0/FParsec.fsx"
#load @"../../.paket/load/netstandard2.0/System.Time.fsx"
#load "DocumentModel.fs"

open System
open System.Text.RegularExpressions
open FParsec

module Helpers =
    let (<||>) f g = fun x -> f x || g x
    let (<&&>) f g = fun x -> f x && g x

    let test parser str =
        match run parser str with
        | Success (s1, s2, s3) -> printfn "Ok: %A %A %A" s1 s2 s3
        | Failure (f1, f2, f3) -> printfn "Fail: %A %A %A" f1 f2 f3

module InternalParsers =
    open Helpers
    //#region String parsers
    let inline isCtrlChar c = 
        c <= '\u001f' || c = '\u007f'

    let inline isNonSpaceControlChar c =
        isCtrlChar c && c <> '\n' && c <> '\r' && c <> '\t'

    let pEquals                 : Parser<char,unit>   = pchar '='

    let pBasicStringContents     : Parser<string,unit> = 
        manySatisfy (isCtrlChar >> not <&&> isNoneOf ['\"'])

    let stripUnderscore = String.filter (function | '_' -> false | _ -> true)

    let applySignToInt64 (sign,n) = 
        match sign with
        | Some '-' -> -1L * n
        | _ -> n

    let charDigit (c:char) = (int c) - (int '0')
    let foldCharToInteger (radix:int64) (bi:int64) (c:char) = bi * radix + int64 (charDigit c)
    let hexStringToInteger (raw:string) = Int64.Parse("0" + raw, Globalization.NumberStyles.HexNumber)
    let octalStringToInteger (raw:string) = seq raw |> Seq.fold (foldCharToInteger 8L) 0L
    let binaryStringToInteger (raw:string) = seq raw |> Seq.fold (foldCharToInteger 2L) 0L

    //let fp_ (pChar:Parser<char,unit>) : Parser<char,unit> = pChar >>? pchar '_' .>>? pChar
    let p_ : Parser<char,unit> = pchar '_' .>> (notFollowedBy (pchar '_'))

    // Parsers
    let pSign     : Parser<char,unit>   = satisfy (isAnyOf ['-';'+'])
    let pNoLeading0 : Parser<unit,unit> = notFollowedByL (pchar '0' .>> lookAhead anyChar) "leading zero not allowed"

    let pDecimal  : Parser<int64,unit> =
    //    let p_ = fp_ digit
        pNoLeading0 >>. ((digit <|> p_) |> many1Chars)
        |>> (stripUnderscore >> int64)

    let pHex      : Parser<int64,unit> = 
    //    let p_ = fp_ hex
        pstring "0x" >>. (hex <|> p_ |> many1Chars)
        |>> (stripUnderscore >> hexStringToInteger)

    let pOctal    : Parser<int64,unit> = 
    //    let p_ = fp_ octal
        pstring "0o" >>. (octal <|> p_) |> many1Chars
        |>> (stripUnderscore >> octalStringToInteger)

    let pBinary   : Parser<int64,unit> = 
        let binary = anyOf ['0';'1']
    //    let p_ = fp_ binary
        pstring "0b" >>. (binary <|> p_ |> many1Chars)
        |>> (stripUnderscore >> binaryStringToInteger)

    let p4DigitInt : Parser<int,unit> = parray 4 digit |>> (String >> int)
    let p2DigitInt : Parser<int,unit> = parray 2 digit |>> (String >> int)
    let pSeconds   : Parser<(int*int),unit> = p2DigitInt .>>. opt (skipChar '.' >>. manyChars digit)
                                              |>> (fun (sec,frac) -> match frac with
                                                                     | Some str -> (sec, str.Substring(0, min str.Length 3) |> int)
                                                                     | _ -> (sec, 0))

    let pSkipDash  : Parser<unit,unit> = skipChar '-'

    let pSkipColon : Parser<unit,unit> = skipChar ':'

    let pOffset    : Parser<TimeSpan,unit> = 
        (pstring "Z" >>% TimeSpan(0,0,0)) <|> (pSign .>>. p2DigitInt .>> pSkipColon .>> pstring "00" 
                                               |>> (fun (sign,offset) -> match sign with
                                                                         | '-' -> -1 * offset |> (fun h -> TimeSpan(h,0,0))
                                                                         | _ -> TimeSpan(offset,0,0)))

    let toDateTimeOffset (date:Date) _ (time:Time) offset = DateTimeOffset(date.Year, date.Month, date.Day, time.Hour, time.Minute, time.Second, time.Millisecond, offset)
    let toDateTime (date:Date) _ (time:Time) = DateTime(date.Year, date.Month, date.Day, time.Hour, time.Minute, time.Second, time.Millisecond)

    // // NOTE: looks like we can use the build-in FParsec parser `pfloat`
    // //#region Float parser
    // // float utility functions
    // let parseTomlFloat = function
    //                      | "inf" -> infinity
    //                      | "nan" -> nan
    //                      | raw -> Convert.ToDouble raw

    // let applySignToFloat (sign,f) =
    //     match sign with
    //     | Some '-' -> -1.0 * f
    //     | _ -> f

    // // Parsers
    // let pSpecial        : Parser<string,unit> = pstring "inf" <|> pstring "nan"
    // let pFloat          : Parser<float,unit> =
    //     opt pSign .>>. (pSpecial <|> (many1Chars digit) |>> (stripUnderscore >> parseTomlFloat))
    //     |>> applySignToFloat
    // //#endregion

open InternalParsers

let pComment                 : Parser<unit,unit>   = skipChar '#' >>. skipRestOfLine true
let pTomlSpaces              : Parser<string,unit> = manySatisfy (isAnyOf ['\t';' '])
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
let pInteger  : Parser<int64,unit> = 
    opt pSign .>>. (pHex <|> pOctal <|> pBinary <|> pDecimal)
    |>> applySignToInt64
let pBool : Parser<bool,unit> = pstring "true" <|> pstring "false" |>> Boolean.Parse

//#region Date parsers
let pLocalDate : Parser<Date,unit> = 
    pipe5 p4DigitInt pSkipDash p2DigitInt pSkipDash p2DigitInt (fun y _ m _ d -> Date(y,m,d))
let pLocalTime : Parser<Time,unit> = 
    pipe5 p2DigitInt pSkipColon p2DigitInt pSkipColon pSeconds (fun h _ m _ s -> Time(h,m,fst s, snd s))
let pOffsetDateTime : Parser<DateTimeOffset,unit> =
    pipe4 pLocalDate (skipAnyOf [' ';'T']) pLocalTime pOffset toDateTimeOffset
let pLocalDateTime  : Parser<DateTime, unit> =
    pipe3 pLocalDate (skipAnyOf [' ';'T']) pLocalTime toDateTime
//#endregion

(*
    Array Parsers
*)
let pArrayOf<'a> (parser:Parser<'a,_>) : Parser<'a list, unit> =
    pchar '[' >>. (sepBy parser (spaces >>. pchar ',' .>> spaces)) .>> pchar ']'
let pBasicStringArray = pArrayOf pBasicString
let pLiteralStringArray = pArrayOf pLiteralString
let pMultilineLiteralStringArray = pArrayOf pMultilineLiteralString
let pMultilineStringArray = pArrayOf pMultilineString
let pIntegerArray = pArrayOf pInteger
let pFloatArray = pArrayOf pfloat
let pBoolArray = pArrayOf pBool
let pOffsetDateTimeArray = pArrayOf pOffsetDateTime
let pLocalDateTimeArray = pArrayOf pLocalDateTime
let pLocalDateArray = pArrayOf pLocalDate
let pLocalTimeArray = pArrayOf pLocalTime

let pStringArray = (attempt pBasicStringArray) <|> (attempt pLiteralStringArray) <|> (attempt pMultilineLiteralStringArray) <|> (attempt pMultilineStringArray)

let sv = FPConfig.Toml.SimpleValue
let mapStringValue = FPConfig.Toml.String >> sv
let mapIntegerValue = FPConfig.Toml.Integer >> sv
let mapFloatValue = FPConfig.Toml.Float >> sv
let mapBoolValue = FPConfig.Toml.Boolean >> sv
let mapOffsetDateTimeValue = FPConfig.Toml.OffsetDateTime >> sv
let mapLocalDateTimeValue = FPConfig.Toml.LocalDateTime >> sv
let mapLocalDateValue = FPConfig.Toml.LocalDate >> sv
let mapLocalTimeValue = FPConfig.Toml.LocalTime >> sv
let mapArrayValue = FPConfig.Toml.Array

let pArray,pArrayRef = createParserForwardedToRef()
let pNestedArray = pArrayOf pArray
pArrayRef :=
    choice [
        attempt pStringArray |>> List.map mapStringValue
        attempt pIntegerArray |>> List.map mapIntegerValue
        attempt pFloatArray |>> List.map mapFloatValue
        attempt pBoolArray |>> List.map mapBoolValue
        attempt pOffsetDateTimeArray |>> List.map mapOffsetDateTimeValue
        attempt pLocalDateTimeArray |>> List.map mapLocalDateTimeValue
        attempt pLocalDateArray |>> List.map mapLocalDateValue
        attempt pLocalTimeArray |>> List.map mapLocalTimeValue
        attempt pNestedArray |>> List.map mapArrayValue
    ]

//#region Key & key-value parsers
let pBareKey      : Parser<string,unit> =
    many1Satisfy (isAsciiLetter <||> isDigit <||> isAnyOf ['_';'-'])

let pKeySegment = pBareKey <|> pBasicString <|> pLiteralString
let pKey = sepBy pKeySegment (pchar '.')

let pValue = choice [
    attempt pBasicString |>> mapStringValue
    attempt pMultilineString |>> mapStringValue
    attempt pLiteralString |>> mapStringValue
    attempt pMultilineLiteralString |>> mapStringValue
    attempt pInteger |>> mapIntegerValue
    attempt pfloat |>> mapFloatValue
    attempt pBool |>> mapBoolValue
    attempt pOffsetDateTime |>> mapOffsetDateTimeValue
    attempt pLocalDateTime |>> mapLocalDateTimeValue
    attempt pLocalDate |>> mapLocalDateValue
    attempt pLocalTime |>> mapLocalTimeValue
    attempt pArray |>> mapArrayValue
]

let pKeyValuePair = 
    pTomlSpaces >>. pKey .>> pTomlSpaces .>> skipChar '=' .>> pTomlSpaces .>>. pValue .>> pTomlSpaces
//#endregion

(*
    Table Parsers
*)
let pTableName = pchar '[' >>. pKey .>> pchar ']'
let pTableArrayName = pstring "[[" >>. pKey .>> pstring "]]"

let pTable = 
    pTableName .>> pTomlSpaces .>> skipNewline .>>.
        (manyTill pKeyValuePair (followedBy pTableName <|> eof))
