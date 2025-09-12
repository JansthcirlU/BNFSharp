namespace BNFSharp.Grammar

open Common

module rec Literal =
    type Literal =
        private
        | SingleQuoteLiteral of SingleQuoteText.SingleQuoteText
        | DoubleQuoteLiteral of DoubleQuoteText.DoubleQuoteText
    type LiteralError =
        private
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
        type SingleQuoteText = private SingleQuoteText of CharacterOrSingleQuote.CharacterOrSingleQuote list
        type SingleQuoteTextError =
            private
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
        type DoubleQuoteText = private DoubleQuoteText of CharacterOrDoubleQuote.CharacterOrDoubleQuote list
        type DoubleQuoteTextError =
            private
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
            private
            | Character of Character.Character
            | SingleQuote
        type CharacterOrSingleQuoteError =
            private
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
            private
            | Character of Character.Character
            | DoubleQuote
        type CharacterOrDoubleQuoteError =
            private
            | CharacterError of Character.CharacterError
            | SingleQuoteError
        
        let create (c: char) =
            match c with
            | '\'' -> Error SingleQuoteError
            | '\"' -> Ok DoubleQuote
            | character ->
                Character.create character
                |> StateResult.map CharacterError Character
