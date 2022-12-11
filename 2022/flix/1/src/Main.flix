def main(): Unit \ Impure =
    discard calorieCounting()

def calorieCounting(): Result[Unit, String] \ Impure =
    use Result.flatMap;
    let* stringLines = File.readLines("../../1");
    let grouped = splitListBy(stringLines, "");
    let ints = grouped |> List.map(List.map(stringToInt));
    let result = ints |> List.map(List.foldLeft((a, b) -> a + b, 0)) |> List.maximum;
    debug(result);
    Ok(())

def stringToInt(s: String) : Int32 =
    match Int32.fromString(s) {
        case None => unreachable!()
        case Some(i) => i
    }

def splitListBy[a : Type](list: List[a], by: a): List[List[a]] with Eq[a] =
    let (grouped, lastGroup) = list |> List.foldLeft((acc, i) -> {
        let (grouped, currentGroup) = acc;
        if (i == by) (List.reverse(currentGroup) :: grouped, Nil) else (grouped, i :: currentGroup)
    }, (Nil, Nil));
    List.reverse(lastGroup :: grouped)
