app "1b.exe"
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
        List.walkTry lines { count: 0, previous: { one: None, two: None, three: None } } countMeasurementIncreases

    when result is
        Ok { count } -> Stdout.line (Num.toStr count)
        _ -> Stdout.line "Whoopsie-doopsie!"

countMeasurementIncreases = \{ count, previous }, measurementStr ->
    { two, three } = previous

    measurement <- Result.map (Str.toI16 measurementStr)
    newCount =
        when previous is
            { one: Some previous1, two: Some previous2, three: Some previous3 } ->
                if previous2 + previous3 + measurement > previous1 + previous2 + previous3 then
                    count + 1
                else
                    count

            _ -> count

    { count: newCount, previous: { one: two, two: three, three: Some measurement } }
