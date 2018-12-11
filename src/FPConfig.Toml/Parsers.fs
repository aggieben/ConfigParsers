#nowarn "62"
namespace FPConfig.Toml

open System
open System.Text
open FParsec
open Prelude

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

    let toDateTimeOfset (date:Date) _ (time:Time) offset = 
        DateTimeOffset(date.Year, date.Month, date.Day, time.Hour, time.Minute, time.Second, time.Millisecond, offset)
    
    let toDateTime (date:Date) _ (time:Time) = 
        DateTime(date.Year, date.Month, date.Day, time.Hour, time.Minute, time.Second, time.Millesecond)

module Parsers =
    open Helpers

// TODO: re-implement parsers from Parsers.fsx here, and use DocumentModel types
// ref: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-4/
    module Internal =
        /// Determines if a character is a control character as defined in the [TOML spec](https://github.com/toml-lang/toml#user-content-string).
        /// Control characters are thereby defined as [U+0000..U+001F, U+007f].
        let inline isCtrlChar c = 
            c <= '\u001f' || c = '\u007f'
        
        /// Some of the control characters as defined in the [TOML spec](https://github.com/toml-lang/toml#user-content-string)
        /// are represented as whitespace; this function determines if a character is _not_ a whitespace character.
        let inline isNonSpaceControlChar c = 
            isCtrlChar c && c <> '\n' && c <> '\r' && c <> '\t'
        
        /// Determines if a character is '='
        let pEquals : Parser<char,unit> = 
            pchar '='
        
        /// Determines the contents of a basic string.  Very similar to `pBasicString`, but leaves out the
        /// quotes.
        let pBasicStringContents : Parser<string,unit> = 
            manySatisfy (isCtrlChar >> not <&&> isNoneOf ['\"'])

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
                                                                             | _ -> TimeSpan(offst,0,0)))

    let pComment : Parser<unit,unit> =
        skipChar '#' >>. skipRestOfLine true

    let pTomlSpace : Parser<string,unit> = 
        manySatisfy (isAnyOf ['\t';' '])

    let pBasicString : Parser<string,unit> = 
        between (pchar '\"') (pchar '\"') Internal.pBasicStringContents