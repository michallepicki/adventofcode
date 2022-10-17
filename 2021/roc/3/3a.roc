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
    n = List.len input
    onesPerPosition = List.walk input [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] \acc, row ->
        List.map2 acc row \a, b ->
            a + Num.intCast b
    gammaRating = List.map onesPerPosition \x ->
        if x > n - x then
            1
        else
            0

    flipBit = \x -> if x == 1 then 0 else 1
    epsilonRating = List.map gammaRating flipBit

    Stdout.line (Num.toStr (ratingToDecimal gammaRating * ratingToDecimal epsilonRating))

parse = \line ->
    line
    |> Str.toScalars
    |> List.map (\n -> n - 48)

ratingToDecimal = \digitsList ->
    List.walk digitsList 0 \acc, n ->
        acc * 2 + n
