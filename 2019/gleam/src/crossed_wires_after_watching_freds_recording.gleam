external type Any
external fn e_io_put_chars(a) -> Any = "io" "put_chars"

external type Fd
external fn e_file_open(path: String, modes: List(Any)) -> Result(Fd, Any) = "file" "open"
external fn e_file_read_line(file: Fd) -> Result(String, Any) = "file" "read_line"

external fn e_string_trim(string: String) -> String = "string" "trim"
type Where {
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
// external fn e_tl(List(a)) -> List(a) = "erlang" "tl"
external fn e_binary_to_integer(string: String) -> Int = "erlang" "binary_to_integer"
external fn e_integer_to_binary(int: Int) -> String = "erlang" "integer_to_binary"
external fn e_abs(int: Int) -> Int = "erlang" "abs"

external fn e_lists_map(function: fn(a) -> b, list: List(a)) -> List(b) = "lists" "map"
external fn e_lists_foldl(function: fn(a, b) -> b, acc: b, list: List(a)) -> b = "lists" "foldl"
external fn e_lists_sort(list: List(a)) -> List(a) = "lists" "sort"
// external fn e_lists_sort_by(function: fn(a, a) -> Bool, List(a)) -> List(a) = "lists" "sort"
external fn e_lists_reverse(list: List(a)) -> List(a) = "lists" "reverse"

external type Ordset(a)
external fn e_ordsets_from_list(list: List(a)) -> Ordset(a) = "ordsets" "from_list"
external fn e_ordsets_intersection(a: Ordset(a), b: Ordset(a)) -> Ordset(a) = "ordsets" "intersection"
external fn e_ordsets_to_list(ordset: Ordset(a)) -> List(a) = "ordsets" "to_list"

external type Map(k, v)
external fn e_maps_new() -> Map(k, v) = "maps" "new"
external fn e_maps_put(key: k, value: v, map: Map(k, v)) -> Map(k, v) = "maps" "put"
external fn e_maps_get(key: k, map: Map(k, v)) -> v = "maps" "get"
// external fn e_maps_get_with_default(key: k, map: Map(k, v), default: v) -> v = "maps" "get"


fn parse_single_input(string: String) -> tuple(String, Int) {
  let direction = e_unicode_characters_to_binary(e_string_slice_with_length(string, 0, 1))
  let distance = e_binary_to_integer(e_unicode_characters_to_binary(e_string_slice(string, 1)))
  tuple(direction, distance)
}

fn points(single_input, acc) {
  let tuple(current_coords, wire) = acc
  let tuple(x, y) = current_coords
  let tuple(direction, steps) = single_input
  case steps {
    0 -> acc
    _ -> {
      let new_point =
        case direction {
          "R" -> tuple(x + 1, y)
          "L" -> tuple(x - 1, y)
          "U" -> tuple(x, y + 1)
          "D" -> tuple(x, y - 1)
        }
      points(tuple(direction, steps - 1), tuple(new_point, [new_point | wire]))
    }
  }
}

fn manhattan_distance(point) {
  let tuple(x, y) = point
  e_abs(x) + e_abs(y)
}

fn solve_a(first_input, second_input) {
  // get points for first wire
  let tuple(_, first_wire) =
    first_input
    |> e_lists_foldl(fn(input, acc) { points(input, acc) }, tuple(tuple(0, 0), []), _)
  let first_wire = e_ordsets_from_list(first_wire)
  // get points for second wire
  let tuple(_, second_wire) =
    second_input
    |> e_lists_foldl(fn(input, acc) { points(input, acc) }, tuple(tuple(0, 0), []), _)
  let second_wire = e_ordsets_from_list(second_wire)

  first_wire
  |> e_ordsets_intersection(_, second_wire)
  |> e_ordsets_to_list(_)
  |> e_lists_map(fn(point) { manhattan_distance(point) }, _)
  |> e_lists_sort(_)
  |> e_hd(_)
}

fn stash_index(point, acc) {
  let tuple(index, map) = acc
  tuple(index + 1, e_maps_put(point, index, map))
}

fn combined_steps(point, first_wire_indexes, second_wire_indexes) {
  e_maps_get(point, first_wire_indexes) + e_maps_get(point, second_wire_indexes)
}

fn solve_b(first_input, second_input) {
  // get points and indexes for first wire
  let tuple(_, first_wire) =
    first_input
    |> e_lists_foldl(fn(a, b) { points(a, b) }, tuple(tuple(0, 0), []), _)
  let first_wire_set = e_ordsets_from_list(first_wire)
  let tuple(_, first_wire_indexes) =
  first_wire
  |> e_lists_reverse(_)
  |> e_lists_foldl(fn(point, acc) { stash_index(point, acc) }, tuple(1, e_maps_new()), _)
  // get points and indexes for second wire
  let tuple(_, second_wire) =
    second_input
    |> e_lists_foldl(fn(a, b) { points(a, b) }, tuple(tuple(0, 0), []), _)
  let second_wire_set = e_ordsets_from_list(second_wire)
  let tuple(_, second_wire_indexes) =
    second_wire
    |> e_lists_reverse(_)
    |> e_lists_foldl(fn(point, acc) { stash_index(point, acc) }, tuple(1, e_maps_new()), _)

  first_wire_set
  |> e_ordsets_intersection(_, second_wire_set)
  |> e_ordsets_to_list(_)
  |> e_lists_map(fn(point) { combined_steps(point, first_wire_indexes, second_wire_indexes) }, _)
  |> e_lists_sort(_)
  |> e_hd(_)
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
  let b = solve_b(first_input, second_input)
  e_io_put_chars(e_integer_to_binary(b))
  e_io_put_chars("\n")
  0
}