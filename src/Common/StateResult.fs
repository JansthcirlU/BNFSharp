namespace Common

module StateResult =
    /// <summary>Represents the result of a state transition with the state it yielded.</summary>
    type State<'TState, 'TError> = Result<'TState, 'TError>

    /// <summary>Represents the result of a state transition with the output of the operation as well as the state it yielded.</summary>
    type StateAndOutput<'TState, 'TOutput, 'TError> = Result<'TOutput * 'TState, 'TError>

    /// <summary>Transforms the given state result such that the new state result has a transformed state or a transformed error.</summary>
    let inline map
        (mapError: 'TErrorBefore -> 'TErrorAfter)
        (mapOk: 'TStateBefore -> 'TStateAfter)
        (result: State<'TStateBefore, 'TErrorBefore>)
        =
        match result with
        | Error e -> Error (mapError e)
        | Ok x -> Ok (mapOk x)
    
    /// <summary>Transforms the given state and output result such that the new state and output result has a transformed state and a transformed output or a transformed error.</summary>
    let inline mapStateAndOutput
        (mapError: 'TErrorBefore -> 'TErrorAfter)
        (mapOk: 'TStateBefore -> 'TStateAfter)
        (mapOutput: 'TOutputBefore -> 'TOutputAfter)
        (result: StateAndOutput<'TStateBefore, 'TOutputBefore, 'TErrorBefore>)
        =
        match result with
        | Error e -> Error (mapError e)
        | Ok (output, state) -> Ok (mapOutput output, mapOk state)

    /// <summary>Transforms the given state and output result such that the new state and output result has a transformed state or a transformed error, leaving the output intact.</summary>
    let inline mapState
        (mapError: 'TErrorBefore -> 'TErrorAfter)
        (mapOk: 'TStateBefore -> 'TStateAfter)
        (result: StateAndOutput<'TStateBefore, 'TOutput, 'TErrorBefore>)
        =
        mapStateAndOutput mapError mapOk id result