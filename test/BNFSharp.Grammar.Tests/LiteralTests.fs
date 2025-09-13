module LiteralTests

open Xunit
open BNFSharp.Grammar

let ValidCharacterData = CharacterTestData.ValidCharacterData
[<Theory>]
[<MemberData(nameof(ValidCharacterData))>]
let ``Literal.create should succeed for single valid character`` (c: char) =
    match
        c.ToString()
        |> Literal.create 
    with
    | Ok (Literal.SingleQuoteLiteral _) -> ()
    | wrong -> Assert.Fail $"Expected SingleQuoteLiteral but got %A{wrong} for character: {c}"

let ValidCharactersWrappedInDoubleQuotesData =
    CharacterTestData.ValidCharacters
    |> Seq.map (fun c -> $"\"{c}\"")
    |> CharacterTestData.toTheoryData
[<Theory>]
[<MemberData(nameof(ValidCharactersWrappedInDoubleQuotesData))>]
let ``Literal.create should succeed for valid character wrapped in double quotes`` (s: string) =
    match Literal.create s with
    | Ok (Literal.DoubleQuoteLiteral _) -> ()
    | wrong -> Assert.Fail $"Expected DoubleQuoteLiteral but got %A{wrong} for string: {s}"

let ValidCharactersWrappedInSingleQuotesData =
    CharacterTestData.ValidCharacters
    |> Seq.map (fun c -> $"\'{c}\'")
    |> CharacterTestData.toTheoryData
[<Theory>]
[<MemberData(nameof(ValidCharactersWrappedInSingleQuotesData))>]
let ``Literal.create should succeed for valid character wrapped in single quotes`` (s: string) =
    match Literal.create s with
    | Ok (Literal.SingleQuoteLiteral _) -> ()
    | wrong -> Assert.Fail $"Expected SingleQuoteLiteral but got %A{wrong} for string: {s}"

[<Theory>]
[<InlineData("\".'/xyzf_AB45")>]
let ``Literal.create should fail for a string containing both single and double quotes`` (s: string) =
    match Literal.create s with
    | Error Literal.MixedQuotesError -> ()
    | wrong -> Assert.Fail $"Expected MixedQuotesError but got %A{wrong} for invalid string: {s}"

let ValidLetterData = CharacterTestData.ValidLetterData
[<Theory>]
[<MemberData(nameof(ValidLetterData))>]
let ``Literal.create should succeed for a valid letter`` (c: char) =
    // Arrange
    let s = c.ToString()

    // Act
    let literal = Literal.create s

    // Assert
    match literal with
    | Ok (Literal.SingleQuoteLiteral (SingleQuoteText.SingleQuoteText [ CharacterOrSingleQuote.Character (Character.Letter (Letter.Letter letter)) ])) -> Assert.Equal(c, letter)
    | wrong -> Assert.Fail $"Expected Letter but got %A{wrong} for letter: {c}"

let ValidDigitData = CharacterTestData.ValidDigitData
[<Theory>]
[<MemberData(nameof(ValidDigitData))>]
let ``Literal.create should succeed for a valid digit`` (c: char) =
    // Arrange
    let s = c.ToString()

    // Act
    let literal = Literal.create s

    // Assert
    match literal with
    | Ok (Literal.SingleQuoteLiteral (SingleQuoteText.SingleQuoteText [ CharacterOrSingleQuote.Character (Character.Digit (Digit.Digit digit)) ])) -> Assert.Equal(c, digit)
    | wrong -> Assert.Fail $"Expected Digit but got %A{wrong} for digit: {c}"

let ValidSymbolData = CharacterTestData.ValidSymbolData
[<Theory>]
[<MemberData(nameof(ValidSymbolData))>]
let ``Literal.create should succeed for a valid symbol`` (c: char) =
    // Arrange
    let s = c.ToString()

    // Act
    let literal = Literal.create s

    // Assert
    match literal with
    | Ok (Literal.SingleQuoteLiteral (SingleQuoteText.SingleQuoteText [ CharacterOrSingleQuote.Character (Character.Symbol (Symbol.Symbol symbol)) ])) -> Assert.Equal(c, symbol)
    | wrong -> Assert.Fail $"Expected Symbol but got %A{wrong} for symbol: {c}"