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

module FeatureAdapter =
    let update
        (toFeatureModel: 'rootModel -> 'featureModel)
        (fromFeatureModel: 'featureModel -> 'rootModel -> 'rootModel)
        (updateFeatureModel: 'featureModel -> 'featureMsg -> FeatureUpdateResult<'featureModel, 'dispatchMsg>)
        (dispatch: 'dispatchMsg -> unit)
        (model: 'rootModel)
        (msg: 'featureMsg) : 'rootModel =
        let featureModel = toFeatureModel model
        let result = updateFeatureModel featureModel msg

        result.Effects
        |> EffectRunner.run dispatch

        fromFeatureModel result.Model model
