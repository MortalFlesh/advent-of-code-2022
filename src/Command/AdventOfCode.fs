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

    [<RequireQualifiedAccess>]
    module private Day1 =
        let task1 (input: string list) =
            input
            |> List.map Int.tryParse
            |> List.fold
                (fun (currentElf, elves) -> function
                    | Some calory -> calory :: currentElf, elves
                    | _ -> [], (currentElf |> List.sum) :: elves
                )
                ([], [])
            |> function
                | [], elves -> elves
                | currentElf, elves -> (currentElf |> List.sum) :: elves
            |> List.max

        let task2 (input: string list) =
            input
            |> List.map Int.tryParse
            |> List.fold
                (fun (currentElf, elves) -> function
                    | Some calory -> calory :: currentElf, elves
                    | _ -> [], (currentElf |> List.sum) :: elves
                )
                ([], [])
            |> function
                | [], elves -> elves
                | currentElf, elves -> (currentElf |> List.sum) :: elves
            |> List.sortDescending
            |> List.take 3
            |> List.sum

    [<RequireQualifiedAccess>]
    module private Day2 =
        type GameResult =
            | Win
            | Draw
            | Lose

        type Hand =
            | Rock
            | Paper
            | Scissors

        let private (|ParseHand|_|) = function
            | "A" | "X" -> Some Rock
            | "B" | "Y" -> Some Paper
            | "C" | "Z" -> Some Scissors
            | _ -> None

        let private points = function
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3

        let private gamePoints = function
            | Win -> 6
            | Draw -> 3
            | Lose -> 0

        let private play = function
            | Regex "([A-C]) ([X-Z])" [ ParseHand oponnent; ParseHand myHand ] ->
                let outcome =
                    match oponnent, myHand with
                    | Rock, Rock -> Draw
                    | Rock, Paper -> Win
                    | Rock, Scissors -> Lose
                    | Paper, Rock -> Lose
                    | Paper, Paper -> Draw
                    | Paper, Scissors -> Win
                    | Scissors, Rock -> Win
                    | Scissors, Paper -> Lose
                    | Scissors, Scissors -> Draw

                (outcome |> gamePoints) +
                (myHand |> points)
            | _ -> 0

        let task1 (input: string list) =
            printfn "It is not: 10052"
            input
            |> List.sumBy play

        let private (|ShouldPlay|_|) = function
            | "X" -> Some Lose
            | "Y" -> Some Draw
            | "Z" -> Some Win
            | _ -> None

        let private playRight = function
            | Regex "([A-C]) ([X-Z])" [ ParseHand oponnent; ShouldPlay outcome ] ->
                let myHand =
                    match oponnent, outcome with
                    | Rock, Win -> Paper
                    | Rock, Lose -> Scissors
                    | Paper, Win -> Scissors
                    | Paper, Lose -> Rock
                    | Scissors, Win -> Rock
                    | Scissors, Lose -> Paper
                    | _, Draw -> oponnent

                (outcome |> gamePoints) +
                (myHand |> points)

            | _ -> 0

        let task2 (input: string list) =
            input
            |> List.sumBy playRight

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
        | 1 ->
            let result =
                if firstPuzzle
                then inputLines |> Day1.task1
                else inputLines |> Day1.task2

            return! handleResult int result
        | 2 ->
            let result =
                if firstPuzzle
                then inputLines |> Day2.task1
                else inputLines |> Day2.task2

            return! handleResult int result

        | day ->
            return! sprintf "Day %A is not ready yet." day |> err |> Error
    }
