module CharacterTestData

let toTheoryData (s: 'T seq) =
    s
    |> Seq.map (fun c -> [| box c |])
    |> Seq.toList

// Valid per type
let ValidLetters =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let ValidLetterData =
    ValidLetters
    |> toTheoryData

let ValidDigits =
    "0123456789"
let ValidDigitData =
    ValidDigits
    |> toTheoryData

let ValidSymbols =
    "|!#$%&()*+,-./:;>=<?@[\\]^_{}~"
let ValidSymbolData =
    ValidSymbols
    |> toTheoryData

// Invalid per type (test mutual exclusivity)
let InvalidLetters =
    ValidDigits + ValidSymbols
let InvalidLetterData =
    InvalidLetters
    |> toTheoryData

let InvalidDigits =
    ValidLetters + ValidSymbols
let InvalidDigitData =
    InvalidDigits
    |> toTheoryData

let InvalidSymbols =
    ValidLetters + ValidDigits
let InvalidSymbolData =
    InvalidSymbols
    |> toTheoryData

// Valid characters
let ValidCharacters =
    ValidLetters + ValidDigits + ValidSymbols
let ValidCharacterData =
    ValidCharacters
    |> toTheoryData