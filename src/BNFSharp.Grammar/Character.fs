namespace BNFSharp.Grammar

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
    
    module Letter =
        type Letter = private Letter of char
        type LetterError =
            private
            | InvalidLetter
    
    module Digit =
        type Digit = private Digit of char
        type DigitError =
            private
            | InvalidDigit
    
    module Symbol =
        type Symbol = private Symbol of char
        type SymbolError =
            private
            | InvalidSymbol