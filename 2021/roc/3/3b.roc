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
                copyListFragment acc lastRow i 12
    else
        onesOnPosition = List.walk input 0 \sum, row ->
            sum + listUnsafeGet row i
        digit = ratingDigitFun onesOnPosition n
        newInput = List.dropIf input \row ->
            x = listUnsafeGet row i

            x != digit

        getRating newInput ratingDigitFun (List.set acc i digit) (i + 1)

copyListFragment = \target, source, i, to ->
    if i >= to then
        target
    else
        List.set target i (listUnsafeGet source i)
        |> copyListFragment source (i + 1) to

listUnsafeGet = \list, i ->
    when List.get list i is
        Ok x -> x
        Err _ -> unreachable

unreachable : *
