Advent of Code 2022
===================

![Check](https://github.com/MortalFlesh/advent-of-code-2022/workflows/Check/badge.svg)

> A console app solving an [Advent of Code 2022](https://adventofcode.com/2022)

## Usage

       ___      __                   __              ___       _____          __
      / _ | ___/ / _  __ ___   ___  / /_      ___   / _/      / ___/ ___  ___/ / ___
     / __ |/ _  / | |/ // -_) / _ \/ __/     / _ \ / _/      / /__  / _ \/ _  / / -_)
    /_/ |_|\_,_/  |___/ \__/ /_//_/\__/      \___//_/        \___/  \___/\_,_/  \__/


    -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    Description:
        Runs an advent-of-code application.

    Usage:
        advent:run [options] [--] <day> <input> [<expectedResult>]

    Arguments:
        day             A number of a day you are running
        input           Input data file path
        expectedResult  Expected result

    Options:
        -s, --second-puzzle   Whether you are expecting a result of the second puzzle.
        -h, --help            Display this help message
        -q, --quiet           Do not output any message
        -V, --version         Display this application version
        -n, --no-interaction  Do not ask any interactive question
            --no-progress     Whether to disable all progress bars
            --no-ansi         Whether to disable all markup with ansi formatting
        -v|vv|vvv, --verbose  Increase the verbosity of messages

First task of the day
```shell
bin/console advent:run 0 data/day0/input.txt
```

Second task of the day
```shell
bin/console advent:run 0 data/day0/input.txt -s
```

NOTE: this will only output the result, if you want to validate directly pass a third argument.

---
### Development

First run:
```
paket install
./build.sh
```

or `./build.sh -t Watch`

List commands
```sh
bin/console list
```

Run tests locally
```sh
./build.sh -t Tests
```
