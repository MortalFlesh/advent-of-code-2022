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
                    >> List.map (function
                        | "-" -> None
                        | crate -> Some crate
                    )
                )
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

    [<RequireQualifiedAccess>]
    module private Day6 =
        let task1 (input: string list) =
            let distinctNeeded = 4

            input
            |> List.choose (fun line ->
                seq {
                    for i in 0 .. line.Length - 1 do
                        if line.ToCharArray(i, distinctNeeded) |> Array.distinct |> Array.length = distinctNeeded
                        then yield Some (i + distinctNeeded)
                        else yield None
                }
                |> Seq.tryPick id
            )
            |> List.map string
            |> String.concat ", "

        let task2 (input: string list) =
            let distinctNeeded = 14

            input
            |> List.choose (fun line ->
                seq {
                    for i in 0 .. line.Length - 1 do
                        if line.ToCharArray(i, distinctNeeded) |> Array.distinct |> Array.length = distinctNeeded
                        then yield Some (i + distinctNeeded)
                        else yield None
                }
                |> Seq.tryPick id
            )
            |> List.map string
            |> String.concat ", "

    [<RequireQualifiedAccess>]
    module private Day7 =
        type Path = string list // in reverse order!
        let private formatPath path = path |> List.rev |> String.concat "/" |> String.replace "//" "/"
        let inline private (</>) path dir = dir :: path

        type Structure =
            | Directory of Path * string
            | File of string * int

        type Run = {
            CWD: string list
            Structure: Map<Path, Structure list>
        }

        let private addStructure structure run =
            let cwdStructure =
                match run.Structure |> Map.tryFind run.CWD with
                | Some current -> structure :: current
                | _ -> [ structure ]

            { run with Structure = run.Structure.Add(run.CWD, cwdStructure) }

        let private getStructure input =
            input
            |> List.fold
                (fun run line ->
                    match run, line with
                    // cd /
                    | _, "$ cd /" -> { run with CWD = [ "/" ] }

                    // cd ..
                    | { CWD = [] }, "$ cd .." -> { run with CWD = [ "/" ] }
                    | { CWD = [ "/" ] }, "$ cd .." -> run
                    | { CWD = _ :: dir }, "$ cd .." -> { run with CWD = dir }

                    // cd X
                    | _, Regex @"\$ cd (.+)" [ dir ] -> { run with CWD = run.CWD </> dir }

                    // ls
                    | _, "$ ls" -> run
                    | run, Regex @"dir (.+)" [ dir ] -> run |> addStructure (Directory (run.CWD, dir))
                    | run, Regex @"(\d+) (.+)" [ size; file ] -> run |> addStructure (File (file, int size))

                    | _ -> run
                )
                {
                    CWD = [ "/" ]
                    Structure =
                        [
                            [], [ Directory ([], "/") ]
                            ["/"], []
                        ]
                        |> Map.ofList
                }
        // |> tee (printfn "%A\n-------\n")

        let rec private calculateDirectorySize (path, directory) run =
            // printfn " - %A (at %A) " directory (path |> formatPath)
            match run.Structure |> Map.tryFind (path </> directory) with
            | Some structure -> structure |> List.sumBy (calucateStructureSize run) // |> tee (printfn "==> size: %A")
            | _ -> 0

        and private calucateStructureSize run = function
            | Directory (path, dir) ->
                // printfn " -> go deeper to: %A" (path </> dir |> formatPath)
                run |> calculateDirectorySize (path, dir)
            | File (_, size) -> size

        let task1 (input: string list) =
            let structure = input |> getStructure

            structure.Structure
            |> Map.toList
            |> List.collect snd
            |> List.choose (function
                | Directory (path, dir) ->
                    // printfn "\nDirectory: %A" dir
                    structure |> calculateDirectorySize (path, dir) |> Some
                | _ -> None
            )
            |> List.filter (fun size -> size <= 100000)
            |> List.sum

        let task2 (input: string list) =
            let structure = input |> getStructure
            let totalDiskSize = 70000000
            let updateNeeds = 30000000
            let totalUsedSpace = structure |> calculateDirectorySize ([], "/")
            let unusedSpace = totalDiskSize - totalUsedSpace
            let requiredSpace = updateNeeds - unusedSpace
            printfn "Required Space: %A" requiredSpace

            structure.Structure
            |> Map.toList
            |> List.collect snd
            |> List.choose (function
                | Directory (path, dir) ->
                    // printfn "\nDirectory: %A" dir
                    structure |> calculateDirectorySize (path, dir) |> Some
                | _ -> None
            )
            |> List.filter (fun size -> size >= requiredSpace)
            |> List.min

    [<RequireQualifiedAccess>]
    module private Day8 =
        type private Direction =
            | Top
            | Bottom
            | Left
            | Right

        let private isVisible (grid: int list list) (i, j) =
            let isVisibleInDirection direction range tree =
                let treesInLine =
                    seq {
                        for k in range do
                            yield
                                match direction with
                                | Top | Bottom -> grid[k][j]
                                | Left | Right -> grid[i][k]
                    }
                    |> Set.ofSeq
                    // |> tee (printf " - is %A visible (%A) in %A" tree direction)

                tree > (treesInLine |> Set.maxElement) // |> tee (printfn " -> %A")

            [
                Left, isVisibleInDirection Left [ 0 .. j - 1 ]
                Right, isVisibleInDirection Right [ j + 1 .. grid[i].Length - 1 ]
                Top, isVisibleInDirection Top [ 0 .. i - 1 ]
                Bottom, isVisibleInDirection Bottom [ i + 1 .. grid.Length - 1 ]
            ]
            |> Seq.tryPick (fun (direction, f) ->
                let isVisibleInDirection =
                    grid[i][j]
                    //|> tee (printfn "tree[%A][%A]: %A" i j)
                    |> f
                    //|> tee (printfn " - is visible to %A: %A" direction)

                if isVisibleInDirection
                then Some true
                else None
            )
            |> Option.defaultValue false
            //|> tee (printfn " --> is visible: %A\n")

        let task1 (input: string list) =
            let grid =
                input
                |> List.map (fun line -> line.ToCharArray() |> Seq.choose (string >> Int.tryParse) |> List.ofSeq)

            let visibleByDefault = 2 * grid.Length + (grid[0].Length - 2) * 2 |> tee (printfn "Default visible: %A")

            seq {
                for i in 1 .. grid.Length - 2 do
                    for j in 1 .. grid.Length - 2 do
                        if (i, j) |> isVisible grid then yield grid[i][j]
            }
            |> Seq.length
            |> tee (printfn "Visible in directions: %A")
            |> (+) visibleByDefault

        let private scenicScore (grid: int list list) (i, j) =
            let lowerTreesInLine direction range tree =
                seq {
                    let mutable run = true

                    for k in range do
                        let currentTree =
                            match direction with
                            | Top | Bottom -> grid[k][j]
                            | Left | Right -> grid[i][k]

                        //printf " ? %A <= %A" currentTree tree
                        if run then
                            //printfn " -> yield"
                            yield currentTree
                        //else printfn " -> skip"

                        if currentTree >= tree then
                            run <- false
                }
                //|> tee (Seq.map string >> String.concat "," >> printf " - [%s]")
                |> Seq.length
                // |> tee (printf " - is %A visible (%A) in %A" tree direction)

            //printfn "\nScenic for %A at [%A][%A]" (grid[i][j]) i j
            [
                Top, lowerTreesInLine Top ([ 0 .. i - 1 ] |> List.rev)
                Left, lowerTreesInLine Left ([ 0 .. j - 1 ] |> List.rev)
                Bottom, lowerTreesInLine Bottom [ i + 1 .. grid.Length - 1 ]
                Right, lowerTreesInLine Right [ j + 1 .. grid[i].Length - 1 ]
            ]
            |> Seq.map (fun (direction, f) -> grid[i][j] |> f (* |> tee (printfn " - in direction %A: %A" direction) *))
            |> Seq.reduce (*)

        let task2 (input: string list) =
            let grid =
                input
                |> List.map (fun line -> line.ToCharArray() |> Seq.choose (string >> Int.tryParse) |> List.ofSeq)

            seq {
                for i in 1 .. grid.Length - 2 do
                    for j in 1 .. grid.Length - 2 do
                        (i, j) |> scenicScore grid
            }
            |> Seq.max

    [<RequireQualifiedAccess>]
    module private Day11 =
        open System.Collections.Generic

        type MonkeyId = int
        type Item = bigint

        let private _inventory = Dictionary<MonkeyId, Item list>()
        let private inventory = Dictionary<MonkeyId, Stack<Item>>()
        let private activity = Dictionary<MonkeyId, bigint>()

        type Monkey =
            {
                Id: MonkeyId
                Operation: Item -> Item
                Test: Item -> MonkeyId
            }

            with
                member this.Inspect (item) =
                    let currentActivity =
                        match activity.TryGetValue(this.Id) with
                        | true, activity -> activity
                        | _ -> bigint 0
                    activity[this.Id] <- currentActivity + bigint 1

                    this.Operation item

                member this.Items
                    with get () =
                        match inventory.TryGetValue(this.Id) with
                        | true, items -> items
                        | _ -> Stack()
                    and set (items) =
                        inventory[this.Id] <- items

                member this.ThrowItem(item) =
                    inventory[this.Test item].Push(item)

        let private parseMonkey = List.filter ((<>) "") >> function
            |   [
                    Regex @"Monkey (\d+):" [ monkeyId ];
                    Regex @"\s+Starting items: ([\d, ]+)" [ items ];
                    Regex @"\s+Operation: new = old (.) (old|\d+)" [ operator; value ];
                    Regex @"\s+Test: divisible by (\d+)" [ testValue ];
                    Regex @"\s+If true: throw to monkey (\d+)" [ onTrue ];
                    Regex @"\s+If false: throw to monkey (\d+)" [ onFalse ];
                ] ->
                    let testValue: Item = testValue |> bigint.Parse
                    let onTrue: MonkeyId = int onTrue
                    let onFalse: MonkeyId = int onFalse

                    let operation =
                        match operator, value |> Int.tryParseBigInt with
                        | "+", Some value -> (+) value
                        | "*", Some value -> (*) value
                        | "+", _ -> fun old -> old + old
                        | "*", _ -> fun old -> old * old
                        | invalid -> failwithf "Invalid operator %A" invalid

                    let monkey = {
                        Id = int monkeyId
                        Operation = operation
                        Test = fun item -> if item % testValue = bigint 0 then onTrue else onFalse
                    }

                    monkey.Items <-
                        items.Split(", ")
                        |> Seq.choose Int.tryParseBigInt
                        |> Stack

                    Some monkey

            | skipped ->
                failwithf "Invalid monkey %A\n" skipped
                None

        let private lowerWorryLevel item =
            item / bigint 3

        let private round (monkeys: Monkey list) i =
            monkeys
            |> List.iter (fun monkey ->
                [
                    while monkey.Items.Count > 0 do
                        yield monkey.Items.Pop()
                ]
                |> List.iter (monkey.Inspect >> lowerWorryLevel >> monkey.ThrowItem)
            )

            (* printfn "\nAfter round %A" i
            monkeys
            |> List.iter (fun monkey -> monkey.Items |> Seq.map string |> String.concat ", " |> printfn "Monkey %A: %s" monkey.Id) *)

        let task1 (input: string list) =
            let monkeyLines = 7.

            let monkeys =
                input
                |> List.splitInto (float input.Length / monkeyLines |> int)
                |> tee (List.iter (printfn "Monkey input: %A"))
                |> List.choose parseMonkey
                |> tee (List.iter (fun m -> printfn "Monkey: %A %A" m m.Items))

            // printfn "Rounds ..."
            [ 1 .. 20 ]
            |> List.iter (round monkeys)

            // printfn "\nMonkeys activity"
            activity
            // |> tee (Seq.iter (fun kv -> printfn "Monkey %A inspected items %A times." kv.Key kv.Value))
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.sortDescending
            |> Seq.take 2
            |> Seq.reduce (*)

        let private roundWithoutLoweringWorry (monkeys: Monkey list) i =
            monkeys
            |> List.iter (fun monkey ->
                [
                    while monkey.Items.Count > 0 do
                        yield monkey.Items.Pop()
                ]
                |> List.iter (monkey.Inspect >> monkey.ThrowItem)
            )

            //if [ 1; 20; 1000; 2000; 5000; 10000 ] |> Set.ofList |> Set.contains i then
            if i % 250 = 0 then
                printfn "\nAfter round %A" i
                (* monkeys
                |> List.iter (fun monkey -> monkey.Items |> Seq.map string |> String.concat ", " |> printfn "Monkey %A: %s" monkey.Id) *)

                printfn "\nMonkeys activity"
                activity
                |> Seq.iter (fun kv -> printfn "Monkey %A inspected items %A times." kv.Key kv.Value)

        let task2 (input: string list) =
            let monkeyLines = 7.

            let monkeys =
                input
                //|> List.splitInto (float input.Length / monkeyLines |> int)
                |> List.splitInto 4
                //|> tee (List.iter (printfn "Monkey input: %A"))
                |> List.choose parseMonkey
                |> tee (List.iter (fun m -> printfn "Monkey: %A %A" m m.Items))

            // printfn "Rounds ..."
            [ 1 .. 10_000 ]
            |> List.iter (roundWithoutLoweringWorry monkeys)

            printfn "\nMonkeys activity"
            activity
            |> tee (Seq.iter (fun kv -> printfn "Monkey %A inspected items %A times." kv.Key kv.Value))
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.sortDescending
            |> Seq.take 2
            |> Seq.reduce (*)

    // todo - add more days here ...

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
        | 6 ->
            let result =
                if firstPuzzle
                then inputLines |> Day6.task1
                else inputLines |> Day6.task2

            return! handleResult string result
        | 7 ->
            let result =
                if firstPuzzle
                then inputLines |> Day7.task1
                else inputLines |> Day7.task2

            return! handleResult int result
        | 8 ->
            let result =
                if firstPuzzle
                then inputLines |> Day8.task1
                else inputLines |> Day8.task2

            return! handleResult int result
        | 11 ->
            let result =
                if firstPuzzle
                then inputLines |> Day11.task1
                else inputLines |> Day11.task2

            return! handleResult bigint.Parse result

        | day ->
            return! sprintf "Day %A is not ready yet." day |> err |> Error
    }
