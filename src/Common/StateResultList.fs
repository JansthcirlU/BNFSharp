namespace Common

module StateResultList =
    let inline private appendOkOrError f state input =
        match state with
        | Error e -> Error e
        | Ok items ->
            match f input with
            | Ok item -> Ok (item :: items)
            | Error e -> Error e

    let inline foldOrError f items =
        items
        |> List.fold (appendOkOrError f) (Ok [])
        |> Result.map List.rev