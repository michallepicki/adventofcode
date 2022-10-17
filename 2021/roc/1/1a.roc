app "1a.exe"
    packages { cli: "../../../../roc/examples/interactive/cli-platform/main.roc" }
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
    fileContents <- Task.await (File.readUtf8 (Path.fromStr "../1"))
    measurements =
        fileContents
        |> Str.trim
        |> Str.split "\n"
        |> List.keepOks Str.toI16

    { count } =
        List.walk measurements { count: 0, previous: None } countMeasurementIncreases

    Stdout.line (Num.toStr count)

countMeasurementIncreases = \{ count, previous }, measurement ->
    newCount =
        when previous is
            None -> count
            Some previousMeasurement ->
                if measurement > previousMeasurement then
                    count + 1
                else
                    count

    { count: newCount, previous: Some measurement }
