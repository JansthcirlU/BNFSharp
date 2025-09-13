module CharacterTests

open Xunit
open BNFSharp.Grammar

let ValidLetterData = CharacterTestData.ValidLetterData
[<Theory>]
[<MemberData(nameof(ValidLetterData))>]
let ``Letter.create should succeed for valid letter`` (c: char) =
    match Letter.create c with
    | Ok (Letter.Letter letter) -> Assert.Equal(c, letter)
    | wrong -> Assert.Fail $"Expected Letter but got %A{wrong} for letter: {c}"

let InvalidLetterData = CharacterTestData.InvalidLetterData
[<Theory>]
[<MemberData(nameof(InvalidLetterData))>]
let ``Letter.create should fail for invalid letter`` (c: char) =
    match Letter.create c with
    | Error (Letter.InvalidLetter invalid) -> Assert.Equal(c, invalid)
    | wrong -> Assert.Fail $"Expected InvalidLetter but got %A{wrong} for invalid letter: {c}"

let ValidDigitData = CharacterTestData.ValidDigitData
[<Theory>]
[<MemberData(nameof(ValidDigitData))>]
let ``Digit.create should succeed for valid digit`` (c: char) =
    match Digit.create c with
    | Ok (Digit.Digit digit) -> Assert.Equal(c, digit)
    | wrong -> Assert.Fail $"Expected Digit but got %A{wrong} for digit: {c}"

let InvalidDigitData = CharacterTestData.InvalidDigitData
[<Theory>]
[<MemberData(nameof(InvalidDigitData))>]
let ``Digit.create should fail for invalid digit`` (c: char) =
    match Digit.create c with
    | Error (Digit.InvalidDigit invalid) -> Assert.Equal(c, invalid)
    | wrong -> Assert.Fail $"Expected InvalidDigit but got %A{wrong} for invalid digit: {c}"

let ValidSymbolData = CharacterTestData.ValidSymbolData
[<Theory>]
[<MemberData(nameof(ValidSymbolData))>]
let ``Symbol.create should succeed for valid symbol`` (c: char) =
    match Symbol.create c with
    | Ok (Symbol.Symbol symbol) -> Assert.Equal(c, symbol)
    | wrong -> Assert.Fail $"Expected Symbol but got {wrong} for symbol: {c}"

let InvalidSymbolData = CharacterTestData.InvalidSymbolData
[<Theory>]
[<MemberData(nameof(InvalidSymbolData))>]
let ``Symbol.create should fail for invalid symbol`` (c: char) =
    match Symbol.create c with
    | Error (Symbol.InvalidSymbol invalid) -> Assert.Equal(c, invalid)
    | wrong -> Assert.Fail $"Expected InvalidSymbol but got {wrong} for invalid symbol: {c}"

[<Theory>]
[<MemberData(nameof(ValidLetterData))>]
let ``Character.create should succeed for valid letter`` (c: char) =
    match Character.create c with
    | Ok (Character.Letter (Letter.Letter letter)) -> Assert.Equal(c, letter)
    | wrong -> Assert.Fail $"Expected Letter but got %A{wrong} for letter: {c}"

[<Theory>]
[<MemberData(nameof(ValidDigitData))>]
let ``Character.create should succeed for valid digit`` (c: char) =
    match Character.create c with
    | Ok (Character.Digit (Digit.Digit digit)) -> Assert.Equal(c, digit)
    | wrong -> Assert.Fail $"Expected Digit but got %A{wrong} for digit: {c}"

[<Theory>]
[<MemberData(nameof(ValidSymbolData))>]
let ``Character.create should succeed for valid symbol`` (c: char) =
    match Character.create c with
    | Ok (Character.Symbol (Symbol.Symbol symbol)) -> Assert.Equal(c, symbol)
    | wrong -> Assert.Fail $"Expected Symbol but got %A{wrong} for symbol: {c}"

let ValidCharacterData = CharacterTestData.ValidCharacterData
[<Theory>]
[<MemberData(nameof(ValidCharacterData))>]
let ``Character.create should succeed for valid character`` (c: char) =
    match Character.create c with
    | Ok _ -> ()
    | wrong -> Assert.Fail $"Expected Ok but got {wrong} for character: {c}"