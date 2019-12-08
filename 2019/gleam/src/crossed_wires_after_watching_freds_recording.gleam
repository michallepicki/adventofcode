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

external fn e_hd(List(a)) -> a = "erlang" "hd"
external fn e_tl(List(a)) -> List(a) = "erlang" "tl"
external fn e_binary_to_integer(string: String) -> Int = "erlang" "binary_to_integer"
external fn e_integer_to_binary(int: Int) -> String = "erlang" "integer_to_binary"
external fn e_abs(int: Int) -> Int = "erlang" "abs"

external fn e_lists_map(function: fn(a) -> b, list: List(a)) -> List(b) = "lists" "map"
external fn e_lists_foldl(function: fn(a, b) -> b, acc: b, list: List(a)) -> b = "lists" "foldl"
external fn e_lists_sort(list: List(a)) -> List(a) = "lists" "sort"
external fn e_lists_sort_by(function: fn(a, a) -> Bool, List(a)) -> List(a) = "lists" "sort"

external type Ordset(a)
external fn e_ordsets_from_list(list: List(a)) -> Ordset(a) = "ordsets" "from_list"
external fn e_ordsets_intersection(a: Ordset(a), b: Ordset(a)) -> Ordset(a) = "ordsets" "intersection"
external fn e_ordsets_to_list(ordset: Ordset(a)) -> List(a) = "ordsets" "to_list"

fn parse_single_input(string: String) -> struct(String, Int) {
  let direction = e_unicode_characters_to_binary(e_string_slice_with_length(string, 0, 1))
  let distance = e_binary_to_integer(e_unicode_characters_to_binary(e_string_slice(string, 1)))
  struct(direction, distance)
}

fn points(input, acc) {
  let struct(current_coords, wire) = acc
  let struct(x, y) = current_coords
  let struct(direction, steps) = input
  case steps {
    0 -> acc
    _ -> {
      let new_point =
        case direction {
          "R" -> struct(x + 1, y)
          "L" -> struct(x - 1, y)
          "U" -> struct(x, y + 1)
          "D" -> struct(x, y - 1)
        }
      points(struct(direction, steps - 1), struct(new_point, [new_point | wire]))
    }
  }
}

fn compare_points_a(a, b) {
  let struct(a_x, a_y) = a
  let struct(b_x, b_y) = b
  e_abs(a_x) + e_abs(a_y) <= e_abs(b_x) + e_abs(b_y)
}

fn solve_a(first_input, second_input) {
  let struct(_, first_wire) = e_lists_foldl(fn(a, b) { points(a, b) }, struct(struct(0, 0), []), first_input)
  let struct(_, second_wire) = e_lists_foldl(fn(a, b) { points(a, b) }, struct(struct(0, 0), []), second_input)
  let first_wire = e_ordsets_from_list(e_lists_sort(first_wire))
  let second_wire = e_ordsets_from_list(e_lists_sort(second_wire))
  let struct(x, y) = e_hd(e_lists_sort_by(fn(a, b) { compare_points_a(a, b) }, e_ordsets_to_list(e_ordsets_intersection(first_wire, second_wire))))
  e_abs(x) + e_abs(y)
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
  // let b = solve_b(first_input, second_input)
  // e_io_put_chars(e_integer_to_binary(b))
  // e_io_put_chars("\n")
  0
}