open System
open System.IO
open MF.ConsoleApplication
open MF.AdventOfCode
open MF.AdventOfCode.Console

[<EntryPoint>]
let main argv =
    consoleApplication {
        title AssemblyVersionInformation.AssemblyProduct
        info ApplicationInfo.MainTitle
        version AssemblyVersionInformation.AssemblyVersion

        version AssemblyVersionInformation.AssemblyVersion
        description AssemblyVersionInformation.AssemblyDescription
        meta ("BuildAt", AssemblyVersionInformation.AssemblyMetadata_createdAt)

        gitBranch AssemblyVersionInformation.AssemblyMetadata_gitbranch
        gitCommit AssemblyVersionInformation.AssemblyMetadata_gitcommit

        command "advent:run" {
            Description = "Runs an advent-of-code application."
            Help = None
            Arguments = Command.AdventOfCode.args
            Options = [
                Option.noValue "second-puzzle" (Some "s") "Whether you are expecting a result of the second puzzle."
            ]
            Initialize = None
            Interact = None
            Execute = Command.AdventOfCode.execute
        }
    }
    |> run argv
