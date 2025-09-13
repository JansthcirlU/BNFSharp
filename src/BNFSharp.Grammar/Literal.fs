namespace rec BNFSharp.Grammar

open Common

module Literal =
    type Literal =
        internal
        | SingleQuoteLiteral of SingleQuoteText.SingleQuoteText
        | DoubleQuoteLiteral of DoubleQuoteText.DoubleQuoteText
    type LiteralError =
        internal
        | SingleQuoteLiteralError of SingleQuoteText.SingleQuoteTextError
        | DoubleQuoteLiteralError of DoubleQuoteText.DoubleQuoteTextError
        | MixedQuotesError
        | NewLineError
    
    let create (s: string) =
        match s with
        | hasNewLines when hasNewLines.Contains '\n' || hasNewLines.Contains '\r' -> Error NewLineError
        | hasMixedQuotes when hasMixedQuotes.Contains '\'' && hasMixedQuotes.Contains '\"' -> Error MixedQuotesError
        | doubleQuoteString when doubleQuoteString.Contains '\"' -> DoubleQuoteText.create s |> StateResult.map DoubleQuoteLiteralError DoubleQuoteLiteral
        | singleQuoteString when singleQuoteString.Contains '\'' -> SingleQuoteText.create s |> StateResult.map SingleQuoteLiteralError SingleQuoteLiteral
        | other -> SingleQuoteText.create other |> StateResult.map SingleQuoteLiteralError SingleQuoteLiteral

module SingleQuoteText =
    type SingleQuoteText = internal SingleQuoteText of CharacterOrSingleQuote.CharacterOrSingleQuote list
    type SingleQuoteTextError =
        internal
        | CharacterOrSingleQuoteError of CharacterOrSingleQuote.CharacterOrSingleQuoteError
        | DoubleQuoteError

    let create (s: string) =
        match s with
        | containsDoubleQuote when containsDoubleQuote.Contains '\"' -> Error DoubleQuoteError
        | _ -> 
            s.ToCharArray()
            |> Array.toList
            |> StateResultList.foldOrError CharacterOrSingleQuote.create
            |> StateResult.map CharacterOrSingleQuoteError SingleQuoteText

module DoubleQuoteText =
    type DoubleQuoteText = internal DoubleQuoteText of CharacterOrDoubleQuote.CharacterOrDoubleQuote list
    type DoubleQuoteTextError =
        internal
        | CharacterOrDoubleQuoteError of CharacterOrDoubleQuote.CharacterOrDoubleQuoteError
        | SingleQuoteError

    let create (s: string) =
        match s with
        | containsSingleQuote when containsSingleQuote.Contains '\'' -> Error SingleQuoteError
        | _ ->
            s.ToCharArray()
            |> Array.toList
            |> StateResultList.foldOrError CharacterOrDoubleQuote.create
            |> StateResult.map CharacterOrDoubleQuoteError DoubleQuoteText

module CharacterOrSingleQuote =
    type CharacterOrSingleQuote =
        internal
        | Character of Character.Character
        | SingleQuote
    type CharacterOrSingleQuoteError =
        internal
        | CharacterError of Character.CharacterError
        | DoubleQuoteError
    
    let create (c: char) =
        match c with
        | '\"' -> Error DoubleQuoteError
        | '\'' -> Ok SingleQuote
        | character ->
            Character.create character
            |> StateResult.map CharacterError Character

module CharacterOrDoubleQuote =
    type CharacterOrDoubleQuote =
        internal
        | Character of Character.Character
        | DoubleQuote
    type CharacterOrDoubleQuoteError =
        internal
        | CharacterError of Character.CharacterError
        | SingleQuoteError
    
    let create (c: char) =
        match c with
        | '\'' -> Error SingleQuoteError
        | '\"' -> Ok DoubleQuote
        | character ->
            Character.create character
            |> StateResult.map CharacterError Character
