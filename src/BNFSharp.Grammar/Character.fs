namespace rec BNFSharp.Grammar

open Common

module Character =
    type Character =
        internal
        | Letter of Letter.Letter
        | Digit of Digit.Digit
        | Symbol of Symbol.Symbol
    type CharacterError =
        internal
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
    type Letter = internal Letter of char
    type LetterError = internal InvalidLetter of char

    let private ValidLetters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    
    let create (c: char) =
        if ValidLetters.Contains c
        then Ok (Letter c)
        else Error (InvalidLetter c)

module Digit =
    type Digit = internal Digit of char
    type DigitError = internal InvalidDigit of char
    
    let private ValidDigits = "0123456789"
    
    let create (c: char) =
        if ValidDigits.Contains c
        then Ok (Digit c)
        else Error (InvalidDigit c)

module Symbol =
    type Symbol = internal Symbol of char
    type SymbolError = internal InvalidSymbol of char

    let private ValidSymbols = "|!#$%&()*+,-./:;>=<?@[\\]^_{}~"
    
    let create (c: char) =
        if ValidSymbols.Contains c
        then Ok (Symbol c)
        else Error (InvalidSymbol c)
