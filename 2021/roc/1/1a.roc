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

main : Program
main = Program.noArgs
    (
        Task.attempt mainTask \result ->
            exitCode =
                when result is
                    Ok {} -> 0
                    Err _ -> 1

            Program.exit (Task.succeed {}) exitCode
    )

mainTask =
    fileContents <- Task.await (File.readUtf8 (Path.fromStr "../1"))
    lines = Str.split (Str.trim fileContents) "\n"
    firstResult = List.first lines |> Result.map Str.toI16

    result =
        when firstResult is
            Ok (Ok first) ->
                List.walkTry lines { count: 0, previous: first } countMeasurementIncreases

            _ -> Err FirstLineNotANum

    when result is
        Ok { count } -> Stdout.line (Num.toStr count)
        _ -> Stdout.line "Uh-oh!"

countMeasurementIncreases = \{ count, previous }, measurementStr ->
    measurement <- Result.map (Str.toI16 measurementStr)
    if measurement > previous then
        { count: count + 1, previous: measurement }
    else
        { count, previous: measurement }
