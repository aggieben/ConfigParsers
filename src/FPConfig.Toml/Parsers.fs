#nowarn "62"
namespace FPConfig.Toml

open System
open System.Text
open FParsec
open System.Text.RegularExpressions

module internal Helpers =
    /// Composes two predicate functions into a single function using the logical OR operator `||`
    let (<||>) f g = fun x -> f x || g x
    
    /// Composes two predicate functions into a single function using the logical AND operator `&&`
    let (<&&>) f g = fun x -> f x && g x
    
    /// Removes `'_'` from strings.
    let stripUnderscore = String.filter (function | '_' -> false | _ -> true)

    /// Apply a given `char option` as a sign to a given 64-bit integer.  The `'-'` character will result in 
    /// `-1L * n`; everything else will result in just `n`.
    let applySignToInt64 (sign, n) = match sign with | Some '-' -> -1L * n | _ -> n

    /// Gets the digit represented by a char; will work for any char, but really only makes sense for ['0'..'f'].
    let charDigit (c:char) = (int c) - (int '0')

    /// A "folder" function for use with `Seq.fold`.  For example:
    /// ```F#
    /// seq "0444" |> Seq.fold (foldCharToInteger 8L) 0L
    /// ```
    let foldCharToInteger (radix:int64) (state:int64) (c:char) = state * radix + int64 (charDigit c)

    /// Parses a hexidecimal number in a string string to an integer.
    let parseHex (raw:string) = Int64.Parse("0" + raw, Globalization.NumberStyles.HexNumber)

    /// Parses an octal number in a string to an integer.
    let parseOctal (raw:string) = seq raw |> Seq.fold (foldCharToInteger 8L) 0L

    /// Parses a binary number in a string to an integer.
    let parseBinary (raw:string) = seq raw |> Seq.fold (foldCharToInteger 2L) 0L

    let toDateTimeOffset (date:Date) (time:Time) offset = 
        DateTimeOffset(date.Year, date.Month, date.Day, time.Hour, time.Minute, time.Second, time.Millisecond, offset)
    
    let toDateTime (date:Date) (time:Time) = 
        DateTime(date.Year, date.Month, date.Day, time.Hour, time.Minute, time.Second, time.Millisecond)

    /// Determines if a character is a control character as defined in the [TOML spec](https://github.com/toml-lang/toml#user-content-string).
    /// Control characters are thereby defined as [U+0000..U+001F, U+007f].
    let inline isCtrlChar c = 
        c <= '\u001f' || c = '\u007f'

    /// Some of the control characters as defined in the [TOML spec](https://github.com/toml-lang/toml#user-content-string)
    /// are represented as whitespace; this function determines if a character is _not_ a whitespace character.
    let inline isNonSpaceControlChar c = 
        isCtrlChar c && c <> '\n' && c <> '\r' && c <> '\t'

module Parsers =
    open Helpers

// TODO: re-implement parsers from Parsers.fsx here, and use DocumentModel types
// ref: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-4/
    module Internal =

        /// Determines if a character is '='
        let pEquals : Parser<char,unit> = 
            pchar '='
        
        /// Determines the contents of a basic string.  Very similar to `pBasicString`, but leaves out the
        /// quotes.
        let pBasicStringContents : Parser<string,unit> = 
            manySatisfy (isCtrlChar >> not <&&> isNoneOf ['\"'])

        let pMultilineLiteralStringContents : Parser<char,unit> =
            satisfy (isNonSpaceControlChar >> not)

        /// Parses and ignores single underscore character when followed by anything else.
        let p_ : Parser<char,unit> = 
            pchar '_' .>> (notFollowedBy (pchar '_'))
        
        /// Parses sign chars; one of ['-', '+']
        let pSign : Parser<char,unit> = 
            satisfy (isAnyOf ['-';'+'])

        /// validates that a given string does not have a leading 0
        let pNoLeading0 : Parser<unit,unit> = 
            notFollowedByL (pchar '0' .>> lookAhead anyChar) "leading zero not allowed"

        /// Parses decimal numbers, allowing for _ separators as specified in the [TOML spec](https://github.com/toml-lang/toml#user-content-integer).
        let pDecimal : Parser<int64,unit> =
            pNoLeading0 >>. (digit <|> p_ |> many1Chars)
            |>> (stripUnderscore >> int64)

        /// Parses hexidecimal numbers, allowing for _ separators.
        let pHex : Parser<int64,unit> =
            pstring "0x" >>. (hex <|> p_ |> many1Chars)
            |>> (stripUnderscore >> parseHex)

        /// Parses octal numbers, allowing for _ separators.
        let pOctal : Parser<int64,unit> =
            pstring "0o" >>. (octal <|> p_ |> many1Chars)
            |>> (stripUnderscore >> parseOctal)
        
        /// Parses binary numbers, allowing for _ separators.
        let pBinary : Parser<int64,unit> =
            let binary = anyOf ['0';'1']
            pstring "0b" >>. (binary <|> p_ |> many1Chars)
            |>> (stripUnderscore >> parseBinary)
        
        /// Parses 4-digit int; intended for use in parsing TOML dates
        let p4DigitInt : Parser<int,unit> = 
            parray 4 digit |>> (String >> int)

        /// Parses 2-digit int; intended for use in parsing TOML dates
        let p2DigitInt : Parser<int,unit> =
            parray 2 digit |>> (String >> int)
        
        /// Parses seconds for TOML date
        let pSeconds : Parser<(int*int),unit> =
            p2DigitInt .>>. opt (skipChar '.' >>. manyChars digit)
            |>> (fun (sec,frac) -> match frac with
                                   | Some str -> (sec, str.Substring(0, min str.Length 3) |> int)
                                   | _ -> (sec,0))

        // Parses (and discards) a '-' character; used primarily for parsing dates.
        let pSkipDash : Parser<unit,unit> =
            skipChar '-'

        // Parses (and discards) a ':' character; used primarily for parsing dates.
        let pSkipColon : Parser<unit,unit> =
            skipChar ':'

        let pOffset : Parser<TimeSpan,unit> =
            (pstring "Z" >>% TimeSpan(0,0,0)) <|> (pSign .>>. p2DigitInt .>> pSkipColon .>> pstring "00"
                                                   |>> (fun (sign,offset) -> match sign with
                                                                             | '-' -> -1 * offset |> (fun h -> TimeSpan(h,0,0))
                                                                             | _ -> TimeSpan(offset,0,0)))

        let pArrayOf<'a> (parser:Parser<'a,_>) : Parser<'a list, unit> =
            pchar '[' >>. (sepBy parser (spaces >>. pchar ',' .>> spaces)) .>> pchar ']'

    let pComment : Parser<unit,unit> =
        skipChar '#' >>. skipRestOfLine true

    let pTomlSpaces : Parser<string,unit> = 
        manySatisfy (isAnyOf ['\t';' '])

    let pBasicString : Parser<string,unit> = 
        between (pchar '\"') (pchar '\"') Internal.pBasicStringContents
    
    let pLiteralString : Parser<string,unit> =
        manySatisfy (isNoneOf ['\'']) |> between (pchar '\'') (pchar '\'')

    let pMultilineString : Parser<string,unit> =
        optional newline >>. manyCharsTill Internal.pMultilineLiteralStringContents (lookAhead (pstring "\"\"\""))
        |> between (pstring "\"\"\"") (pstring "\"\"\"")
        |>> fun s -> Regex.Replace(s, @"\\\s*", "")

    let pMultilineLiteralString : Parser<string,unit> =
        optional newline >>. manyCharsTill anyChar (lookAhead (pstring "'''"))
        |> between (pstring "'''") (pstring "'''")

    let pInteger : Parser<int64,unit> =
        opt Internal.pSign .>>. (Internal.pHex <|> Internal.pOctal <|> Internal.pBinary <|> Internal.pDecimal)
        |>> applySignToInt64

    let pBool : Parser<bool,unit> =
        pstring "true" <|> pstring "false" |>> Boolean.Parse

    let pLocalDate : Parser<Date,unit> =
        pipe5 Internal.p4DigitInt Internal.pSkipDash Internal.p2DigitInt Internal.pSkipDash Internal.p2DigitInt (fun y _ m _ d -> Date(y,m,d))
    
    let pLocalTime : Parser<Time,unit> =
        pipe5 Internal.p2DigitInt Internal.pSkipColon Internal.p2DigitInt Internal.pSkipColon Internal.pSeconds (fun h _ m _ s -> Time(h,m,fst s, snd s))

    let pOffsetDateTime : Parser<DateTimeOffset,unit> =
        pipe4 pLocalDate (skipAnyOf [' ';'T']) pLocalTime Internal.pOffset (fun ld _ lt off -> toDateTimeOffset ld lt off)

    let pLocalDateTime : Parser<DateTime,unit> =
        pipe3 pLocalDate (skipAnyOf [' ';'T']) pLocalTime (fun ld _ lt -> toDateTime ld lt)

    let pBasicStringArray = Internal.pArrayOf pBasicString

    let pLiteralStringArray = Internal.pArrayOf pLiteralString

    let pMultilineLiteralStringArray = Internal.pArrayOf pMultilineLiteralString

    let pMultilineStringArray = Internal.pArrayOf pMultilineString

    let pIntegerArray = Internal.pArrayOf pInteger

    let pFloatArray = Internal.pArrayOf pfloat

    let pBoolArray = Internal.pArrayOf pBool

    let pOffsetDateTimeArray = Internal.pArrayOf pOffsetDateTime

    let pLocalDateTimeArray = Internal.pArrayOf pLocalDateTime

    let pLocalDateArray = Internal.pArrayOf pLocalDate

    let pLocalTimeArray = Internal.pArrayOf pLocalTime

    let pStringArray = 
        (attempt pBasicStringArray) 
        <|> (attempt pLiteralStringArray) 
        <|> (attempt pMultilineLiteralStringArray) 
        <|> (attempt pMultilineStringArray)

    let pArray,pArrayRef = createParserForwardedToRef()
    let pNestedArray = Internal.pArrayOf pArray
    pArrayRef := 
        choice [
            attempt pStringArray |>> List.map DocumentModel.mapStringValue
            attempt pIntegerArray |>> List.map DocumentModel.mapIntegerValue
            attempt pFloatArray |>> List.map DocumentModel.mapFloatValue
            attempt pBoolArray |>> List.map DocumentModel.mapBoolValue
            attempt pOffsetDateTimeArray |>> List.map DocumentModel.mapOffsetDateTimeValue
            attempt pLocalDateTimeArray |>> List.map DocumentModel.mapLocalDateTimeValue
            attempt pLocalDateArray |>> List.map DocumentModel.mapLocalDateValue
            attempt pLocalTimeArray |>> List.map DocumentModel.mapLocalTimeValue
            attempt pNestedArray |>> List.map DocumentModel.mapArrayValue
        ]

    let pBareKey : Parser<string,unit> =
        many1Satisfy (isAsciiLetter <||> isDigit <||> isAnyOf ['_';'-'])
    
    let pKeySegment : Parser<string,unit> =
        pBareKey <|> pBasicString <|> pLiteralString

    let pKey : Parser<string list,unit> =
        sepBy pKeySegment (pchar '.')

    let pValue = choice [
        attempt pBasicString |>> DocumentModel.mapStringValue
        attempt pMultilineString |>> DocumentModel.mapStringValue
        attempt pLiteralString |>> DocumentModel.mapStringValue
        attempt pMultilineLiteralString |>> DocumentModel.mapStringValue
        attempt pInteger |>> DocumentModel.mapIntegerValue
        attempt pfloat |>> DocumentModel.mapFloatValue
        attempt pBool |>> DocumentModel.mapBoolValue
        attempt pOffsetDateTime |>> DocumentModel.mapOffsetDateTimeValue
        attempt pLocalDateTime |>> DocumentModel.mapLocalDateTimeValue
        attempt pLocalDate |>> DocumentModel.mapLocalDateValue
        attempt pLocalTime |>> DocumentModel.mapLocalTimeValue
        attempt pArray |>> DocumentModel.mapArrayValue
    ]

    let pKeyValuePair : Parser<(string list * Value), unit> =
        pTomlSpaces >>. pKey .>> pTomlSpaces .>> skipChar '=' .>> pTomlSpaces .>>. pValue.>> pTomlSpaces
    
    let pTableName : Parser<string list,unit> =
        pchar '[' >>. pKey .>> pchar ']'

    let pTableArrayName : Parser<string list,unit> =
        pstring "[[" >>. pKey .>> pstring "]]"

    let pTable =
        pTomlSpaces >>. pTableName .>> pTomlSpaces .>> skipNewline .>>.
            (manyTill pKeyValuePair (followedBy pTableName <|> eof))