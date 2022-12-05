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

    [<RequireQualifiedAccess>]
    module private Day3 =
        let private findCharValue =
            let values =
                [
                    let mutable i = 1
                    for lower in 'a' .. 'z' do
                        yield lower, i
                        i <- i + 1

                    for upper in 'A' .. 'Z' do
                        yield upper, i
                        i <- i + 1
                ]
                |> Map.ofList

            fun c -> values[c]

        let task1 (input: string list) =
            input
            |> List.choose (fun input ->
                match input.ToCharArray() |> Array.toList |> List.splitInto 2 with
                | [] | [ _ ] -> None
                | [ first; second ] ->
                    let f = first |> Set.ofList
                    let s = second |> Set.ofList

                    f
                    |> Set.intersect s
                    |> Set.toList
                    |> List.sumBy findCharValue
                    |> Some
                | _ -> None
            )
            |> Seq.sum

        let task2 (input: string list) =
            input
            |> List.chunkBySize 3
            |> List.choose (fun group ->
                match group |> List.map (fun line -> line.ToCharArray() |> Array.toList |> Set.ofList) with
                | [] | [_] | [_;_] -> None
                | [ first; second; third ] as group ->
                    group
                    |> Set.intersectMany
                    |> Set.toList
                    |> List.sumBy findCharValue
                    |> Some
                | _ -> None

            )
            |> Seq.sum

    [<RequireQualifiedAccess>]
    module private Day4 =
        let task1 (input: string list) =
            input
            |> List.choose (function
                | Regex @"(\d+)\-(\d+),(\d+)\-(\d+)" [ a1; a2; b1; b2 ] ->
                    let a = [ int a1 .. int a2 ] |> Set.ofList
                    let b = [ int b1 .. int b2 ] |> Set.ofList

                    if (a |> Set.isSubset b) || (b |> Set.isSubset a)
                    then Some 1
                    else None
                | _ -> None
            )
            |> Seq.sum

        let task2 (input: string list) =
            input
            |> List.choose (function
                | Regex @"(\d+)\-(\d+),(\d+)\-(\d+)" [ a1; a2; b1; b2 ] ->
                    let a = [ int a1 .. int a2 ] |> Set.ofList
                    let b = [ int b1 .. int b2 ] |> Set.ofList

                    if (a |> Set.intersect b |> Set.isEmpty)
                    then None
                    else Some 1
                | _ -> None
            )
            |> Seq.sum

    [<RequireQualifiedAccess>]
    module private Day5 =
        open System.Collections.Generic

        let private printStack i (stack: Stack<_>) =
            printfn "___"
            stack
            |> Seq.iter (printfn "[%s]")
            printfn "---"
            printfn " %A " i

        let private parseSample config =
            printfn "Config: %A" config
            printfn "---"

            let stacks =
                config
                |> List.rev
                |> List.pick (function
                    | Regex @"(\d+)" _ as line -> line.Split " " |> Seq.choose Int.tryParse |> List.ofSeq |> Some
                    | _ -> None
                )
                |> List.map (fun id -> id, Stack<string>())
                |> Map.ofList

            printfn "Stack IDs: %A" stacks

            let crates =
                config
                |> List.filter (String.contains "[")
                |> List.map (
                    String.replace "    [" "- ["
                    >> String.replace "]    " "] -"
                    >> String.replace "   " "-"
                    >> String.replace "[" ""
                    >> String.replace "]" ""
                    >> String.split " "
                )
                |> List.map (List.map (function
                    | "-" -> None
                    | crate -> Some crate
                ))
                |> tee (List.iteri (printfn "[%A] %A"))
                |> List.rev
            printfn "======"

            for id in stacks.Keys do
                let stack = stacks[id]

                printfn "id: %A" id
                for i in 0 .. crates.Length - 1 do
                    printfn "[%A][%A] %A" i id (crates[i][id - 1])
                    match crates[i][id - 1] with
                    | Some crate -> stack.Push(crate)
                    | _ -> ()

                printfn ""

            stacks

        let private inputStack =
            (*                      [L]     [H] [W]
                                [J] [Z] [J] [Q] [Q]
                [S]             [M] [C] [T] [F] [B]
                [P]     [H]     [B] [D] [G] [B] [P]
                [W]     [L] [D] [D] [J] [W] [T] [C]
                [N] [T] [R] [T] [T] [T] [M] [M] [G]
                [J] [S] [Q] [S] [Z] [W] [P] [G] [D]
                [Z] [G] [V] [V] [Q] [M] [L] [N] [R]
                1   2   3   4   5   6   7   8   9  *)
            [
                1, Stack(["S";"P";"W";"N";"J";"Z"] |> List.rev)
                2, Stack(["T"; "S"; "G"] |> List.rev)
                3, Stack(["H"; "L";"R";"Q";"V"] |> List.rev)
                4, Stack([ "D"; "T"; "S"; "V" ] |> List.rev)
                5, Stack([ "J";"M";"B";"D";"T";"Z";"Q" ] |> List.rev)
                6, Stack([ "L";"Z";"C";"D";"J";"T";"W";"M" ] |> List.rev)
                7, Stack([ "J";"T";"G";"W";"M";"P";"L" ] |> List.rev)
                8, Stack([ "H";"Q";"F";"B";"T";"M";"G";"N" ] |> List.rev)
                9, Stack([ "W";"Q";"B";"P";"C";"G";"D";"R" ] |> List.rev)
            ]
            |> Map.ofList

        let private printStacks stacks =
            printfn "Stacks:"
            stacks
            |> Map.toList
            |> List.iter (fun (id, stack) -> printStack id stack)

        let task1 (input: string list) =
            let (actions, config) =
                input
                |> List.partition (String.startsWith "move")

            // let stacks = parseSample config
            let stacks = inputStack

            printStacks stacks

            //printfn "Actions: %A" actions
            actions
            |> List.iter (function
                | Regex @"move (\d+) from (\d+) to (\d+)" [ count; source; target ] ->
                    for _ in 1 .. int count do
                        stacks[int source].Pop()
                        |> stacks[int target].Push

                    //printStacks stacks
                | _ -> ()
            )

            stacks
            |> Map.toList
            |> List.map (fun (_, stack) -> stack.Peek())
            |> String.concat ""

        let task2 (input: string list) =
            let (actions, config) =
                input
                |> List.partition (String.startsWith "move")

            //let stacks = parseSample config
            let stacks = inputStack

            printStacks stacks

            //printfn "Actions: %A" actions
            actions
            |> List.iter (function
                | Regex @"move (\d+) from (\d+) to (\d+)" [ count; source; target ] ->
                    [
                        for _ in 1 .. int count do
                            yield stacks[int source].Pop()
                    ]
                    |> List.rev
                    |> List.iter (stacks[int target].Push)

                    printStacks stacks
                    ()
                | _ -> ()
            )

            stacks
            |> Map.toList
            |> List.map (fun (_, stack) -> stack.Peek())
            |> String.concat ""

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
        | 3 ->
            let result =
                if firstPuzzle
                then inputLines |> Day3.task1
                else inputLines |> Day3.task2

            return! handleResult int result
        | 4 ->
            let result =
                if firstPuzzle
                then inputLines |> Day4.task1
                else inputLines |> Day4.task2

            return! handleResult int result
        | 5 ->
            let result =
                if firstPuzzle
                then inputLines |> Day5.task1
                else inputLines |> Day5.task2

            return! handleResult string result

        | day ->
            return! sprintf "Day %A is not ready yet." day |> err |> Error
    }
