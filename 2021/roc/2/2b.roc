app "2b.exe"
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
    fileContents <- Task.await (File.readUtf8 (Path.fromStr "../2"))
    commands =
        fileContents
        |> Str.trim
        |> Str.split "\n"
        |> List.keepOks parseCommand

    { horizontal, depth } =
        List.walk commands { horizontal: 0, depth: 0, aim: 0 } navigate

    Stdout.line (Num.toStr (horizontal * depth))

parseCommand = \line ->
    { before: direction, after: amountStr } <- Result.try (Str.splitFirst line " ")
    amount <- Result.try (Str.toI32 amountStr)
    parseDirection direction amount

parseDirection = \direction, amount ->
    when direction is
        "forward" -> Ok (Forward amount)
        "down" -> Ok (Down amount)
        "up" -> Ok (Up amount)
        _ -> Err BadDirection

navigate = \{ horizontal, depth, aim }, command ->
    when command is
        Down x -> { horizontal: horizontal, depth: depth, aim: aim + x }
        Up x -> { horizontal: horizontal, depth: depth, aim: aim - x }
        Forward x -> { horizontal: horizontal + x, depth: depth + aim * x, aim: aim }
