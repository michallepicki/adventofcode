app "1b.exe"
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
    fileContents <- Task.await (File.readUtf8 (Path.fromStr "../1"))
    measurements =
        fileContents
        |> Str.trim
        |> Str.split "\n"
        |> List.keepOks Str.toI16

    { count } =
        List.walk measurements { count: 0, previous: { previous1: None, previous2: None, previous3: None } } countMeasurementIncreases

    Stdout.line (Num.toStr count)

countMeasurementIncreases = \{ count, previous }, measurement ->
    newCount =
        when previous is
            { previous1: Some previous1, previous2: Some previous2, previous3: Some previous3 } ->
                if previous2 + previous3 + measurement > previous1 + previous2 + previous3 then
                    count + 1
                else
                    count

            _ -> count

    { previous2: previous2Other, previous3: previous3Other } = previous

    { count: newCount, previous: { previous1: previous2Other, previous2: previous3Other, previous3: Some measurement } }
