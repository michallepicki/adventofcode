external type Any
external fn e_io_put_chars(a) -> Any = "io" "put_chars"

external type Fd
external fn e_file_open(path: String, modes: List(Any)) -> Result(Fd, Any) = "file" "open"
external fn e_file_read_line(file: Fd) -> Result(String, Any) = "file" "read_line"

external fn e_string_trim(string: String) -> String = "string" "trim"
enum Where {
  Leading
  Trailing
  All
}
external fn e_string_split(string: String, pattern: String, where: Where) -> List(String) = "string" "split"
external type Characters
external fn e_string_slice(string: String, start: Int) -> Characters = "string" "slice"
external fn e_string_slice_with_length(string: String, start: Int, length: Int) -> Characters = "string" "slice"

external fn e_unicode_characters_to_binary(characters: Characters) -> String = "unicode" "characters_to_binary"

external fn e_lists_map(function: fn(a) -> b, list: List(a)) -> List(b) = "lists" "map"

external fn e_binary_to_integer(string: String) -> Int = "erlang" "binary_to_integer"
external fn e_integer_to_binary(int: Int) -> String = "erlang" "integer_to_binary"

enum Direction {
  Right
  Down
  Left
  Up
}
fn parse_single_input(string: String) -> struct(Direction, Int) {
  let direction = case e_unicode_characters_to_binary(e_string_slice_with_length(string, 0, 1)) {
    "R" -> Right
    "D" -> Down
    "L" -> Left
    "U" -> Up
  }
  let distance = e_binary_to_integer(e_unicode_characters_to_binary(e_string_slice(string, 1)))
  struct(direction, distance)
}

fn solve_a(_, _) {
  1
}

pub fn main(_) {
  let Ok(file) = e_file_open("../3", [])
  let Ok(first_line) = e_file_read_line(file)
  let first_input =
    first_line
    |> e_string_trim
    |> e_string_split(_, ",", All)
    |> e_lists_map(parse_single_input(_), _)
  let Ok(second_line) = e_file_read_line(file)
  let second_input =
    second_line
    |> e_string_trim
    |> e_string_split(_, ",", All)
    |> e_lists_map(parse_single_input(_), _)
  let a = solve_a(first_input, second_input)
  e_io_put_chars(e_integer_to_binary(a))
  e_io_put_chars("\n")
  // let b = solve_b(input, input, -1, 0, 0)
  // e_io_put_chars(e_integer_to_binary(b))
  // e_io_put_chars("\n")
  0
}