namespace AsciiLove
//Op: Auto
open Microsoft.Extensions.Configuration
//Op: End
module Config =
    type LoggingConfig =
        {
            LogFilePath: string
        }

    let getConfig () =
        let config =
            ConfigurationBuilder()
                .AddJsonFile("config.json")
                .Build()
        {|
            EditorPath = config.["EditorPath"]
            SelectedFileSymbol = config.["SelectedFileSymbol"]
            LoggingConfig = {LogFilePath = config.["LogFilePath"]}
        |}