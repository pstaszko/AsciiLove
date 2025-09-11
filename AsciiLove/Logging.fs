namespace AsciiLove
//Op: Auto
open Serilog
//Op: End

[<AutoOpen>]
module Logging =
    let mutable private logger: ILogger = null
    let SetLogger l = logger <- l

    let getLogger (lc: Config.LoggingConfig) =
        LoggerConfiguration()
            .MinimumLevel.Debug()
            .WriteTo.RollingFile(Serilog.Formatting.Display.MessageTemplateTextFormatter("{Timestamp:yy-MM-dd HH:mm:ss.fff} [{Level:u3}] {Message:lj}{NewLine}{Exception}", null), lc.LogFilePath)
            //.WriteTo.Console(Events.LogEventLevel.Information)
            .CreateLogger()

    let logInfo x = logger.Information x
    let logWarn x = logger.Warning x
    let logError (ex: exn) x = logger.Error(ex, x)
    let logDebug x = logger.Debug x
    let logVerbose x = logger.Verbose x