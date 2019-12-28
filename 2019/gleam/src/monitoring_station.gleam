external fn e_display(a) -> Bool = "erlang" "display"
external type Any

external type Fd
type FileMode {
  Binary
}
external fn e_file_open(path: String, modes: List(FileMode)) -> Result(Fd, Any) = "file" "open"
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


external fn e_lists_map(function: fn(a) -> b, list: List(a)) -> List(b) = "lists" "map"
external fn e_lists_foldl(function: fn(a, b) -> b, acc: b, list: List(a)) -> b = "lists" "foldl"
external fn e_lists_reverse(list: List(a)) -> List(a) = "lists" "reverse"
external fn e_lists_max(list: List(a)) -> a = "lists" "max"
external fn e_lists_min(list: List(a)) -> a = "lists" "min"
external fn e_lists_seq(from: Int, to: Int) -> List(Int) = "lists" "seq"
external fn e_lists_append(list_of_lists: List(List(a))) -> List(a) = "lists" "append"
external fn e_lists_split(n: Int, list: List(a)) -> tuple(List(a), List(a)) = "lists" "split"
external fn e_lists_zipwith(combine: fn(a, b) -> c, list1: List(a), list2: List(b)) -> List(c) = "lists" "zipwith"
external fn e_lists_nth(n: Int, list: List(a)) -> a = "lists" "nth"
external fn e_lists_sort(fun: fn(a, a) -> Bool, List(a)) -> List(a) = "lists" "sort"
external type Array(a)
external fn e_array_from_list(list: List(a)) -> Array(a) = "array" "from_list"
external fn e_array_get(index: Int, array: Array(a)) -> a = "array" "get"
external fn e_array_set(index: Int, value: a, array: Array(a)) -> Array(a) = "array" "set"

external fn e_binary_to_integer(string: String) -> Int = "erlang" "binary_to_integer"
external fn e_hd(list: List(a)) -> a = "erlang" "hd"
external fn e_length(list: List(a)) -> Int = "erlang" "length"
external fn e_abs(Int) -> Int = "erlang" "abs"

external fn e_binary_to_list(string: String) -> List(Characters) = "erlang" "binary_to_list"
external fn e_string_to_integer(characters: List(Characters)) -> tuple(Int, List(Characters)) = "string" "to_integer"

external type Set(a)
external fn e_sets_from_list(list: List(a)) -> Set(a) = "sets" "from_list"
external fn e_sets_to_list(set: Set(a)) -> List(a) = "sets" "to_list"

type Point {
  Asteroid
  Nothing
}

fn parse_input(file, acc) {
  let tuple(y, asteroids) = acc
  case e_file_read_line(file) {
    Ok(line) -> {
      let tuple(_, new_asteroids) =
        line
        |> e_string_trim(_)
        |> e_binary_to_list(_)
        |> e_lists_foldl(
          fn(point_as_byte, acc) {
            let tuple(x, asteroids_in_row) = acc
            case [point_as_byte] == e_binary_to_list("#") {
              True -> tuple(x + 1, [tuple(x, y) | asteroids_in_row])
              False -> tuple(x + 1, asteroids_in_row)
            }
          },
          tuple(0, []),
          _)
      parse_input(file, tuple(y + 1, e_lists_append([new_asteroids, asteroids])))
    }
    _ -> {
      asteroids
    }
  }
}

fn remove_nth(n, list, acc) {
  let [nth | rest] = list
  case n {
    0 -> tuple(nth, e_lists_append([e_lists_reverse(acc), rest]))
    _ -> remove_nth(n - 1, rest, [nth | acc])
  }
}

fn gcd(a, b) {
  case b == 0 {
    True -> a
    False -> gcd(b, a % b)
  }
}

fn direction_vector(vector) {
  let tuple(x, y) = vector
  case x == 0 || y == 0 {
    True -> {
      case y == 0 {
        True -> tuple(x / e_abs(x), y)
        False -> tuple(x, y / e_abs(y))
      }
    }
    False -> {
      let gcd = gcd(e_abs(x), e_abs(y))
      tuple(x / gcd, y / gcd)
    }
  }
}

fn relative_coordinates(station, asteroid) {
  let tuple(x0, y0) = station
  let tuple(x, y) = asteroid
  tuple(x - x0, y - y0)
}

fn count_visible(asteroids, station) {
  let tuple(x0, y0) = station
  asteroids
  |> e_lists_map(
    fn(asteroid) {
      direction_vector(relative_coordinates(station, asteroid))
    },
    _)
  |> e_sets_from_list(_)
  |> e_sets_to_list(_)
  |> e_length(_)
}

fn distance_squared(vector) {
  let tuple(x, y) = vector
  x * x + y * y
}

fn deploy_and_observe(station, asteroids) {
  let tuple(x0, y0) = station
  asteroids
  |> e_lists_map(
    fn(asteroid) {
      let relative = relative_coordinates(station, asteroid)
      tuple(relative, direction_vector(relative), distance_squared(relative), asteroid)
    },
    _)
}

fn negation(a) { 0 - a }

fn compare_asteroids(a, b) {
  let tuple(a_relative, _, a_distance, _) = a
  let tuple(xa, ya) = a_relative
  let tuple(b_relative, _, b_distance, _) = b
  let tuple(xb, yb) = b_relative
  case xa >= 0 && xb < 0 {
    True -> True
    False -> {
      case xa < 0 && xb >= 0 {
        True -> False
        False -> {
          let det = xa * negation(yb) - xb * negation(ya)
          case det < 0 {
            True -> True
            False -> {
              case det > 0 {
                True -> False
                False -> {
                  a_distance <= b_distance
                }
              }
            }
          }
        }
      }
    }
  }
}

fn invalid_asteroid() { tuple(tuple(0, 0), tuple(0, 0), 0, tuple(-1,-1)) }

fn vaporize(n, asteroids, acc, last_vaporized) {
  case n {
    0 -> last_vaporized
    _ -> case asteroids {
      [] -> vaporize(n, e_lists_reverse(acc), [], invalid_asteroid())
      [asteroid | rest] -> {
        let tuple(_, last_direction, _, _) = last_vaporized
        let tuple(_, direction, _, _) = asteroid
        case direction == last_direction {
          True -> vaporize(n, rest, [asteroid | acc], last_vaporized)
          False -> vaporize(n - 1, rest, acc, asteroid)
        }
      }
    }
  }
}

pub fn main(_) {
  let Ok(file) = e_file_open("../10", [Binary])
  let asteroids = parse_input(file, tuple(0, []))
  let n = e_length(asteroids)
  
  let tuple(visible_count, station, visible_asteroids) =
    e_lists_seq(0, n - 1)
    |> e_lists_map(
      fn(index){
        let tuple(station, other_asteroids) = remove_nth(index, asteroids, [])
        tuple(count_visible(other_asteroids, station), station, other_asteroids)
      },
      _)
    |> e_lists_max(_)

  e_display(visible_count)

  let tuple(_, _, _, vaporized200) =
    visible_asteroids
    |> deploy_and_observe(station, _)
    |> e_lists_sort(fn(a, b) { compare_asteroids(a, b) }, _ )
    |> vaporize(200, _, [], invalid_asteroid())

  e_display(vaporized200)
  0
}