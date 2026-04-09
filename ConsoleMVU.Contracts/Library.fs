namespace ConsoleMVU.Contracts

type ScreenDef<'model, 'message, 'hotkey> = {
    View: 'model -> unit
    Update: 'model -> 'message -> 'model
    Hotkeys: 'hotkey list
}

[<RequireQualifiedAccess>]
type FeatureEffect<'message> =
    | Dispatch of 'message
    | DelayDispatchMs of milliseconds: int * message: 'message

type FeatureUpdateResult<'model, 'message> = {
    Model: 'model
    Effects: FeatureEffect<'message> list
}

module FeatureUpdateResult =
    let noEffects model =
        {
            Model = model
            Effects = []
        }

    let withEffect effect model =
        {
            Model = model
            Effects = [ effect ]
        }
