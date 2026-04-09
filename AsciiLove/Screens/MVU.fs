namespace AsciiLove
module MVU =
    type ScreenDef<'message> = {
        View: Model -> unit
        Update: Model -> 'message -> Model
        Hotkeys: Hotkey list
    }