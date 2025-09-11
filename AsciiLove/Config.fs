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
            config = config
            LoggingConfig = {LogFilePath = config.["LogFilePath"]}
        |}