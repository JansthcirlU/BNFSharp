module CharacterTests

open Xunit
open BNFSharp.Grammar

let ValidLetterData = CharacterTestData.ValidLetterData
[<Theory>]
[<MemberData(nameof(ValidLetterData))>]
let ``Letter.create should succeed for valid letter`` (c: char) =
    match Character.Letter.create c with
    | Ok _ -> ()
    | Error _ -> Assert.Fail $"Expected Ok but got Error for character '{c}'"

let InvalidLetterData = CharacterTestData.InvalidLetterData
[<Theory>]
[<MemberData(nameof(InvalidLetterData))>]
let ``Letter.create should fail for invalid letter`` (c: char) =
    match Character.Letter.create c with
    | Ok _ -> Assert.Fail $"Expected Error but got Ok for invalid letter '{c}'"
    | Error _ -> ()

let ValidDigitData = CharacterTestData.ValidDigitData
[<Theory>]
[<MemberData(nameof(ValidDigitData))>]
let ``Digit.create should succeed for valid digit`` (c: char) =
    match Character.Digit.create c with
    | Ok _ -> ()
    | Error _ -> Assert.Fail $"Expected Ok but got Error for character '{c}'"

let InvalidDigitData = CharacterTestData.InvalidDigitData
[<Theory>]
[<MemberData(nameof(InvalidDigitData))>]
let ``Digit.create should fail for invalid digit`` (c: char) =
    match Character.Digit.create c with
    | Ok _ -> Assert.Fail $"Expected Error but got Ok for invalid digit '{c}'"
    | Error _ -> ()

let ValidSymbolData = CharacterTestData.ValidSymbolData
[<Theory>]
[<MemberData(nameof(ValidSymbolData))>]
let ``Symbol.create should succeed for valid symbol`` (c: char) =
    match Character.Symbol.create c with
    | Ok _ -> ()
    | Error _ -> Assert.Fail $"Expected Ok but got Error for character '{c}'"

let InvalidSymbolData = CharacterTestData.InvalidSymbolData
[<Theory>]
[<MemberData(nameof(InvalidSymbolData))>]
let ``Symbol.create should fail for invalid symbol`` (c: char) =
    match Character.Symbol.create c with
    | Ok _ -> Assert.Fail $"Expected Error but got Ok for invalid symbol '{c}'"
    | Error _ -> ()

let ValidCharacterData = CharacterTestData.ValidCharacterData
[<Theory>]
[<MemberData(nameof(ValidCharacterData))>]
let ``Character.create should succeed for valid character`` (c: char) =
    match Character.create c with
    | Ok _ -> ()
    | Error _ -> Assert.Fail $"Expected Ok but got Error for character '{c}'"