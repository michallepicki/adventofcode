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
    lines = Str.split (Str.trim fileContents) "\n"
    result =
        List.walkTry lines { count: 0, previous: None } countMeasurementIncreases

    when result is
        Ok { count } -> Stdout.line (Num.toStr count)
        _ -> Stdout.line "Uh-oh!"

countMeasurementIncreases = \{ count, previous }, measurementStr ->
    measurement <- Result.map (Str.toI16 measurementStr)
    newCount =
        when previous is
            None -> count
            Some previousMeasurement ->
                if measurement > previousMeasurement then
                    count + 1
                else
                    count

    { count: newCount, previous: Some measurement }
