module Toml.FSharp.Tests.Prelude

open FsCheck
open FParsec


let inline registerArb<'a>() = Arb.register<'a>() |> ignore

let parseString parser str = runParserOnString parser () "toml-fs string parser test" str