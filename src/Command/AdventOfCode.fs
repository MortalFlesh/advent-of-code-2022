namespace MF.AdventOfCode.Command

[<RequireQualifiedAccess>]
module AdventOfCode =
    open System.IO
    open MF.ConsoleApplication
    open MF.AdventOfCode.Console
    open MF.ErrorHandling
    open MF.ErrorHandling.Result.Operators
    open MF.Utils

    [<RequireQualifiedAccess>]
    module private DayExample =
        let task1 (input: string list) =
            input
            |> List.choose Int.tryParse
            |> Seq.sum

        let task2 (input: string list) =
            input
            |> List.choose Int.tryParse64
            |> List.filter (fun i -> i % int64 2 = 0)
            |> Seq.sum

    // todo - add more days here ...

    // --- end of days ---

    let args = [
        Argument.required "day" "A number of a day you are running"
        Argument.required "input" "Input data file path"
        Argument.optional "expectedResult" "Expected result" None
    ]

    let private err = CommandError.Message >> ConsoleApplicationError.CommandError

    let execute = ExecuteResult <| fun (input, output) -> result {
        output.Title "Advent of Code 2022"

        let expected =
            input
            |> Input.Argument.asString "expectedResult"
            |> Option.map (fun expected ->
                if expected |> File.Exists
                    then expected |> FileSystem.readContent |> String.trim ' '
                    else expected
            )

        let day = input |> Input.Argument.asInt "day" |> Option.defaultValue 1

        let! file =
            input
            |> Input.Argument.asString "input"
            |> Result.ofOption (err "Missing input file")

        let inputLines =
            file
            |> FileSystem.readLines

        let firstPuzzle =
            match input with
            | Input.Option.Has "second-puzzle" _ -> false
            | _ -> true

        let handleResult f dayResult = result {
            match expected with
            | Some expected ->
                do! dayResult |> Assert.eq (f expected) <@> err
                output.Success "Done"
                return ExitCode.Success
            | _ ->
                output.Warning("Result value is %A", dayResult)
                return ExitCode.Error
        }

        match day with
        | 0 ->
            let result =
                if firstPuzzle
                then inputLines |> DayExample.task1 |> int64
                else inputLines |> DayExample.task2

            return! handleResult int64 result

        | day ->
            return! sprintf "Day %A is not ready yet." day |> err |> Error
    }
