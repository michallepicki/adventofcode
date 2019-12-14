external fn e_display(a) -> Bool = "erlang" "display"
external type Any

external type Fd
enum FileMode {
  Binary
}
external fn e_file_open(path: String, modes: List(FileMode)) -> Result(Fd, Any) = "file" "open"
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
external fn e_lists_foldl(function: fn(a, b) -> b, acc: b, list: List(a)) -> b = "lists" "foldl"
external fn e_lists_reverse(list: List(a)) -> List(a) = "lists" "reverse"
external fn e_lists_max(list: List(a)) -> a = "lists" "max"
external fn e_lists_min(list: List(a)) -> a = "lists" "min"
external fn e_lists_seq(from: Int, to: Int) -> List(Int) = "lists" "seq"
external fn e_lists_append(list_of_lists: List(List(a))) -> List(a) = "lists" "append"
external fn e_lists_split(n: Int, list: List(a)) -> struct(List(a), List(a)) = "lists" "split"
external fn e_lists_zipwith(combine: fn(a, b) -> c, list1: List(a), list2: List(b)) -> List(c) = "lists" "zipwith"
external type Array(a)
external fn e_array_from_list(list: List(a)) -> Array(a) = "array" "from_list"
external fn e_array_get(index: Int, array: Array(a)) -> a = "array" "get"
external fn e_array_set(index: Int, value: a, array: Array(a)) -> Array(a) = "array" "set"

external fn e_binary_to_integer(string: String) -> Int = "erlang" "binary_to_integer"
external fn e_hd(list: List(a)) -> a = "erlang" "hd"
external fn e_length(list: List(a)) -> Int = "erlang" "length"
external fn e_abs(Int) -> Int = "erlang" "abs"

external fn e_binary_to_list(string: String) -> List(Characters) = "erlang" "binary_to_list"
external fn e_string_to_integer(characters: List(Characters)) -> struct(Int, List(Characters)) = "string" "to_integer"

external type Set(a)
external fn e_sets_from_list(list: List(a)) -> Set(a) = "sets" "from_list"
external fn e_sets_to_list(set: Set(a)) -> List(a) = "sets" "to_list"

enum Point {
  Asteroid
  Nothing
}

fn parse_input(file, acc) {
  let struct(y, asteroids) = acc
  case e_file_read_line(file) {
    Ok(line) -> {
      let struct(_, new_asteroids) =
        line
        |> e_string_trim(_)
        |> e_binary_to_list(_)
        |> e_lists_foldl(
          fn(point_as_byte, acc) {
            let struct(x, asteroids_in_row) = acc
            case [point_as_byte] == e_binary_to_list("#") {
              True -> struct(x + 1, [struct(x, y) | asteroids_in_row])
              False -> struct(x + 1, asteroids_in_row)
            }
          },
          struct(0, []),
          _)
      parse_input(file, struct(y + 1, e_lists_append([new_asteroids, asteroids])))
    }
    _ -> {
      asteroids
    }
  }
}

fn remove_nth(n, list, acc) {
  let [nth | rest] = list
  case n {
    0 -> struct(nth, e_lists_append([e_lists_reverse(acc), rest]))
    _ -> remove_nth(n - 1, rest, [nth | acc])
  }
}

// enum Comparison {
//   Further
//   Same
//   Closer
// }

// fn compare(coord1, coord2) {
//   case coord1 - coord2 {
//     0 -> Same
//     difference -> {
//       case difference < 0 {
//         True -> Further
//         False -> Closer
//       }
//     }
//   }
// }

fn gcd(a, b) {
  case b == 0 {
    True -> a
    False -> gcd(b, a % b)
  }
}

fn simplify(vector) {
  let struct(numerator, denominator) = vector
  case numerator == 0 || denominator == 0 {
    True -> {
      case numerator == 0 {
        True -> struct(numerator, denominator / e_abs(denominator))
        False -> struct(numerator / e_abs(numerator), denominator)
      }
    }
    False -> {
      let gcd = gcd(e_abs(numerator), e_abs(denominator))
      struct(numerator / gcd, denominator / gcd)
    }
  }
}

fn compute_simplified_vector(station, asteroid) {
  let struct(x0, y0) = station
  let struct(x, y) = asteroid
  let vector = struct(y0 - y, x0 - x)
  simplify(vector)
}

fn count_visible(asteroids, station) {
  let struct(x0, y0) = station
  asteroids
  |> e_lists_map(
    fn(asteroid) {
      // let struct(x, y) = asteroid
      // let horizontally = compare(x0, x)
      // let vertically = compare(y0, y)
      // let line_struct =
      compute_simplified_vector(station, asteroid)
      // struct(line_struct)
    },
    _)
  |> e_sets_from_list(_)
  |> e_sets_to_list(_)
  |> e_length(_)
}

pub fn main(_) {
  let Ok(file) = e_file_open("../10", [Binary])
  let asteroids = parse_input(file, struct(0, []))
  let n = e_length(asteroids)
  
  e_lists_seq(0, n - 1)
  |> e_lists_map(
    fn(index){
      let struct(station, asteroids) = remove_nth(index, asteroids, [])
      count_visible(asteroids, station)
    },
    _)
  |> e_lists_max(_)
  |> e_display(_)
  0
}