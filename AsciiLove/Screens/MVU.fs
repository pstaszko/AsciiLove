namespace AsciiLove
module MVU =
    type ScreenDef<'message> = ConsoleMVU.Contracts.ScreenDef<Model, 'message, Hotkey>