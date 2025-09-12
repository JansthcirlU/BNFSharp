namespace BNFSharp.Grammar

open Common

module rec Character =
    type Character =
        private
        | Letter of Letter.Letter
        | Digit of Digit.Digit
        | Symbol of Symbol.Symbol
    type CharacterError =
        private
        | LetterError of Letter.LetterError
        | DigitError of Digit.DigitError
        | SymbolError of Symbol.SymbolError
        | InvalidCharacter of char

    let create c =
        let possibleCreates = [
            Letter.create >> StateResult.map LetterError Letter
            Digit.create >> StateResult.map DigitError Digit
            Symbol.create >> StateResult.map SymbolError Symbol
        ]
        match
            possibleCreates
            |> List.map (fun create -> create c)
            |> List.filter Result.isOk
        with
            | [] -> Error (InvalidCharacter c)
            | [ x ] -> x
            | _ -> failwith $"More than one character type matched for '{c}' but character types must be mutually exclusive."

    module Letter =
        type Letter = private Letter of char
        type LetterError = private InvalidLetter of char

        let private ValidLetters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        
        let create c =
            if ValidLetters.Contains c
            then Ok (Letter c)
            else Error (InvalidLetter c)
    
    module Digit =
        type Digit = private Digit of char
        type DigitError = private InvalidDigit of char
        
        let private ValidDigits = "0123456789"
        
        let create c =
            if ValidDigits.Contains c
            then Ok (Digit c)
            else Error (InvalidDigit c)
    
    module Symbol =
        type Symbol = private Symbol of char
        type SymbolError = private InvalidSymbol of char

        let private ValidSymbols = "|!#$%&()*+,-./:;>=<?@[\\]^_{}~"
        
        let create c =
            if ValidSymbols.Contains c
            then Ok (Symbol c)
            else Error (InvalidSymbol c)
