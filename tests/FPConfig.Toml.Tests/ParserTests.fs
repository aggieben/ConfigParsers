module FPConfig.Toml.Tests.ParserTests

open Xunit
open FsCheck
open FsCheck.Xunit
open FParsec 
open FPConfig.Toml.Parsers
open FPConfig.Toml.Tests.Generators
open FPConfig.Toml.Tests.Prelude

let inline throwConfig maxTest startSize endSize = 
    { Config.QuickThrowOnFailure with MaxTest = maxTest; StartSize = startSize; EndSize = endSize}

// string parser tests

let parserTest bound parser  =
    fun (str:string) -> 
        (str.Length > bound) ==>
            match parseString parser str with
            | ParserResult.Success(_,_,_) -> true
            | ParserResult.Failure(_,_,_) -> false 

let stringParser psr = parserTest 6 psr

let [<Property(MaxTest = 1000)>] ``parses all basic strings`` () =
    Prop.forAll basic_string_arb (stringParser basic_string)

let [<Property(MaxTest = 1000)>] ``parses all multi strings`` () =
    Prop.forAll multi_string_arb (stringParser multi_string)

let [<Property(MaxTest = 1000)>] ``parses all literal strings`` () =
    Prop.forAll literal_string_arb (stringParser literal_string)

let [<Property(MaxTest = 1000)>] ``parses all multi literal strings`` () =
    Prop.forAll multi_lit_string_arb (stringParser multi_literal_string)

let [<Property(MaxTest = 500)>] ``unified string parser reads all toml string types`` () =
    Prop.forAll toml_string_arb (stringParser toml_string)


// Simple Value Parser Tests

let valueParser psr = parserTest 3 psr

let [<Property>] ``parses all ints`` () =
    Prop.forAll toml_int_arb (valueParser toml_int)

let [<Property>] ``parses all floats`` () =
    Prop.forAll toml_float_arb (valueParser toml_float)

let [<Property>] ``parses bools`` () =
    Prop.forAll toml_bool_arb (valueParser toml_bool )

let [<Property>] ``parses all DateTimes`` () =
    Prop.forAll toml_datetime_arb (valueParser toml_datetime)

let [<Property>] ``parses all Arrays`` () =
    Prop.forAll toml_array_arb (valueParser toml_array)


// Table Parser Tests 

let [<Property>] ``parses keys for table elements `` () =
    Prop.forAll toml_key_arb (valueParser toml_key)

let [<Property>] ``parses bare table keys`` () =
    Prop.forAll toml_bareTableKey_arb (valueParser pBareKey)

let [<Property>] ``parses quote table keys`` () =
    Prop.forAll toml_quoteTableKey_arb (valueParser pQuoteKey)

let [<Property>] ``parses toml keys`` () =
    Prop.forAll toml_key_arb (valueParser toml_key)

let [<Property>] ``parses toml items (key value pairs)`` () =
    Prop.forAll toml_item_arb (valueParser toml_item)



