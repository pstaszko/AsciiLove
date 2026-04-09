namespace ConsoleMVU.Runtime

open System.Threading.Tasks
open ConsoleMVU.Contracts

module EffectRunner =
    let run (dispatch: 'message -> unit) (effects: FeatureEffect<'message> list) =
        effects
        |> List.iter (function
            | FeatureEffect.Dispatch message ->
                dispatch message
            | FeatureEffect.DelayDispatchMs (milliseconds, message) ->
                Task.Delay(milliseconds).ContinueWith(fun _ -> dispatch message)
                |> ignore)
