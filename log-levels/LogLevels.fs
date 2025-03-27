module LogLevels

let message (logLine: string): string = 
       logLine.Remove(0, logLine.IndexOf(":") + 1).Trim()

let logLevel(logLine: string): string = 
       logLine.Remove(0, 1).Remove(logLine.IndexOf("]") - 1).ToLower()

let reformat(logLine: string): string = 
       $"""{logLine |> message} ({logLine |> logLevel})"""
