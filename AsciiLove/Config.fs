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
            //config = config
            EditorPath = config.["EditorPath"]
            LoggingConfig = {LogFilePath = config.["LogFilePath"]}
        |}