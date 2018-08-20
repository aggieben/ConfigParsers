#nowarn "62"
namespace FPConfig.Toml

open System
open System.Text
open FParsec
open Prelude

module internal Helpers =
    let (<|>) f g = fun x -> f x || g x
    let (<&>) f g = fun x -> f x && g x

module Parsers =
    open Helpers

// TODO: re-implement parsers from Parsers.fsx here, and use DocumentModel types
// follow this: https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-4/