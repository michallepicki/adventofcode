app "3a.exe"
    packages { cli: "/var/home/michal/rocking/.local/bin/examples/cli/cli-platform/main.roc" }
    imports [
        cli.File,
        cli.Path,
        cli.Program.{ Program, ExitCode },
        cli.Stdout,
        cli.Task.{ Task },
    ]
    provides [main] to cli

main = Program.quick mainTask

mainTask =
    fileContents <- Task.await (File.readUtf8 (Path.fromStr "../3"))
    input =
        fileContents
        |> Str.trim
        |> Str.split "\n"
        |> List.map parse

    flipBit = \x -> if x == 1 then 0 else 1
    getOxygenDigit = \x, n -> if x >= Num.intCast n - x then 1 else 0
    getCo2Digit = \x, n -> flipBit (getOxygenDigit x n)
    oxygen = getRating input getOxygenDigit [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 0
    co2 = getRating input getCo2Digit [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 0

    Stdout.line (Num.toStr (ratingToDecimal oxygen * ratingToDecimal co2))

parse = \line ->
    line
    |> Str.toScalars
    |> List.map (\n -> n - 48)

ratingToDecimal = \digitsList ->
    List.walk digitsList 0 \acc, n ->
        acc * 2 + n

getRating = \input, ratingDigitFun, acc, i ->
    n = List.len input

    if n <= 1 then
        when List.first input is
            Err _ -> acc
            Ok lastRow ->
                result =
                    List.walk lastRow { acc2: acc, j: 0 } \{ acc2, j }, x ->
                        if j < i then
                            { acc2: acc2, j: j + 1 }
                        else
                            { acc2: List.set acc2 j x, j: j + 1 }

                result.acc2
    else
        onesOnPosition = List.walk input 0 \sum, row ->
            when List.get row i is
                Ok x -> sum + x
                Err _ -> sum
        digit = ratingDigitFun onesOnPosition n
        newInput = List.dropIf input \row ->
            when List.get row i is
                Ok x ->
                    if x == digit then
                        Bool.false
                    else
                        Bool.true

                _ -> Bool.true

        getRating newInput ratingDigitFun (List.set acc i digit) (i + 1)
