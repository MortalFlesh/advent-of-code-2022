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
    module private Day1 =
        let countDepthIncreses (measurements: string list) =
            seq {
                let measurements = measurements |> List.map int

                for current in 1 .. measurements.Length - 1 do
                    let previous = current - 1

                    if measurements.[current] > measurements.[previous] then
                        yield 1
            }
            |> Seq.sum

        let countDepthGroupsIncreases measurements =
            seq {
                let measurements = measurements |> List.map int

                for current in 3 .. measurements.Length - 1 do
                    let previous = List.sum [
                        measurements.[current - 1]
                        measurements.[current - 2]
                        measurements.[current - 3]
                    ]

                    let current = List.sum [
                        measurements.[current]
                        measurements.[current - 1]
                        measurements.[current - 2]
                    ]

                    if current > previous then
                        yield 1
            }
            |> Seq.sum

    [<RequireQualifiedAccess>]
    module private Day2 =
        type Position = {
            Horizontal: int
            Depth: int
        }

        let calculate (instructions: string list) =
            instructions
            |> List.fold (fun acc -> function
                | Regex @"forward (\d+)" [ value ] -> { acc with Horizontal = acc.Horizontal + (int value) }
                | Regex @"down (\d+)" [ value ] -> { acc with Depth = acc.Depth + (int value) }
                | Regex @"up (\d+)" [ value ] -> { acc with Depth = acc.Depth - (int value) }
                | _ -> acc
            ) { Horizontal = 0; Depth = 0 }
            |> fun { Horizontal = h; Depth = d } -> h * d

        type AimedPosition = {
            Horizontal: int
            Depth: int
            Aim: int
        }

        let calculateAimed (instructions: string list) =
            instructions
            |> List.fold (fun (acc: AimedPosition) -> function
                | Regex @"forward (\d+)" [ value ] -> {
                    acc with
                        Horizontal = acc.Horizontal + (int value)
                        Depth = acc.Depth + acc.Aim * (int value)
                    }
                | Regex @"down (\d+)" [ value ] -> { acc with Aim = acc.Aim + (int value) }
                | Regex @"up (\d+)" [ value ] -> { acc with Aim = acc.Aim - (int value) }
                | _ -> acc
            ) { Horizontal = 0; Depth = 0; Aim = 0 }
            |> fun { Horizontal = h; Depth = d } -> h * d

    [<RequireQualifiedAccess>]
    module private Day3 =
        let calculatePowerConsumption (values: string list) =
            let lineLength = values.[0].Length
            let commonValues = Array.init lineLength (fun _ -> 0)

            for line in 0 .. values.Length - 1 do
                for bit in 0 .. lineLength - 1 do
                    let value = values.[line].[bit] |> string |> int
                    commonValues.[bit] <- commonValues.[bit] + value

            printfn "common: %A" commonValues

            let gammaRate =
                [
                    let halfValue = values.Length / 2
                    printfn "Half: %A" halfValue

                    for bit in 0 .. commonValues.Length - 1 do
                        if commonValues.[bit] > halfValue then yield 1
                        else yield 0
                ]

            let epsilonRate = Binary.negate gammaRate
            printfn "gamma rate: %A | epsilon rate: %A" gammaRate epsilonRate

            (gammaRate |> Binary.binIntToDec) * (epsilonRate |> Binary.binIntToDec)

        let calculateLifeSupportRating (values: string list) =
            (* let commonValues (values: string list) =
                let lineLength = values.[0].Length
                let commonValues = Array.init lineLength (fun _ -> 0)

                for line in 0 .. values.Length - 1 do
                    for bit in 0 .. lineLength - 1 do
                        let value = values.[line].[bit] |> string |> int
                        commonValues.[bit] <- commonValues.[bit] + value

                [
                    let halfValue = values.Length / 2
                    printfn "Half: %A" halfValue

                    for bit in 0 .. commonValues.Length - 1 do
                        if commonValues.[bit] >= halfValue then yield 1
                        else yield 0
                ]

            let firstBitValue =
                values
                |> List.mapi (fun bit v -> bit, v.[0] |> string |> int)

            let matchCriteria f (commonValues: int list) (bit, value) = f commonValues.[bit] value

            let rec search mostCommon criteria = function
                | [] -> failwithf "last value should be returned, empty list is not supported"
                | [ (_, value) ] -> value
                | (bit, current) :: rest ->
                    let newValues =
                        if (bit, current) |> criteria
                            then rest @ [bit, current]
                            else rest

                    let mostCommon =
                        newValues
                        |> List.map (fun (_, value) -> )
                        |> commonValues

                    newValues
                    |> List.mapi (fun bit (_, value) -> bit, value)
                    |> search (mostCommon) criteria

            let oxygenGeneratorRating = firstBitValue |> search (matchCriteria (=))
            let co2ScrubberRating = firstBitValue |> search (matchCriteria (<>))

            (oxygenGeneratorRating |> Binary.binIntToDec) * (co2ScrubberRating |> Binary.binIntToDec) *)
            int64 0

    // --- end of days ---

    let args = [
        Argument.required "day" "A number of a day you are running"
        Argument.required "input" "Input data file path"
        Argument.optional "expectedResult" "Expected result" None
    ]

    let execute: ExecuteCommand = fun (input, output) -> ExitCode.ofResult output (result {
        output.Title "Advent of Code 2021"

        let expected =
            input
            |> Input.getArgumentValueAsString "expectedResult"
            |> Option.map (fun expected ->
                if expected |> File.Exists
                    then expected |> FileSystem.readContent |> String.trim ' '
                    else expected
            )

        let day = input |> Input.getArgumentValueAsInt "day" |> Option.defaultValue 1

        let! file =
            input
            |> Input.getArgumentValueAsString "input"
            |> Result.ofOption "Missing input file"

        let inputLines =
            file
            |> FileSystem.readLines

        let firstPuzzle =
            match input with
            | Input.HasOption "second-puzzle" _ -> false
            | _ -> true

        let handleResult f dayResult = result {
            match expected with
            | Some expected ->
                do! dayResult |> Assert.eq (f expected)
                return "Done"
            | _ ->
                return sprintf "Result value is %A" dayResult
        }

        match day with
        | 1 ->
            let result =
                if firstPuzzle
                then inputLines |> Day1.countDepthIncreses
                else inputLines |> Day1.countDepthGroupsIncreases

            return! handleResult int result

        | 2 ->
            let result =
                if firstPuzzle
                then inputLines |> Day2.calculate
                else inputLines |> Day2.calculateAimed

            return! handleResult int result

        | 3 ->
            let result =
                if firstPuzzle
                then inputLines |> Day3.calculatePowerConsumption
                else inputLines |> Day3.calculateLifeSupportRating

            return! handleResult int64 result

        | day ->
            return! Error <| sprintf "Day %A is not ready yet." day
    })
