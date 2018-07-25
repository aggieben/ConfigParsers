namespace Toml.FSharp
module Toml =

    open System
    open FParsec
    open Toml.FSharp.Parsers

    /// Read TOML data out of a file at `path`
    let readTomlFile (path:string) =
        match runParserOnFile parse_toml_table () path (Text.Encoding.UTF8) with
        | Failure (errorMsg,_,_) -> failwith errorMsg
        | Success (result,_,_)   -> result

    /// Read TOML data out of a string
    let readTomlString (text:string) =
        match runParserOnString parse_toml_table () "toml" text with
        | Failure (errorMsg,_,_) -> failwith errorMsg
        | Success (result,_,_)   -> result