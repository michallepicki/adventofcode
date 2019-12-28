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

external fn e_lists_map(function: fn(a) -> b, list: List(a)) -> List(b) = "lists" "map"

external fn e_hd(List(a)) -> a = "erlang" "hd"
external fn e_tl(List(a)) -> List(a) = "erlang" "tl"
external fn e_binary_to_integer(string: String) -> Int = "erlang" "binary_to_integer"
external fn e_integer_to_binary(int: Int) -> String = "erlang" "integer_to_binary"

type Wire {
  FirstWire
  SecondWire
}
external type Map(k, v)
external fn e_maps_new() -> Map(k, v) = "maps" "new"
external fn e_maps_put(key: k, value: v, map: Map(k, v)) -> Map(k, v) = "maps" "put"
external fn e_maps_get(key: k, map: Map(k, v), default: v) -> v = "maps" "get"

fn min(a: Int, b: Int) -> Int {
  case a <= b {
    True -> a
    False -> b
  }
}

fn abs(a: Int) -> Int {
  case a >= 0 {
    True -> a
    False -> 0 - a
  }
}

type Direction {
  Right
  Down
  Left
  Up
}
fn parse_single_input(string: String) -> tuple(Direction, Int) {
  let direction = case e_unicode_characters_to_binary(e_string_slice_with_length(string, 0, 1)) {
    "R" -> Right
    "D" -> Down
    "L" -> Left
    "U" -> Up
  }
  let distance = e_binary_to_integer(e_unicode_characters_to_binary(e_string_slice(string, 1)))
  tuple(direction, distance)
}

fn walk_a(map, x, y, direction, distance, current_wire, best_crossing) {
  case distance {
    0 ->
      tuple(map, x, y, best_crossing)
    distance -> {
      let new_x = case direction {
        Right -> x + 1
        Left -> x - 1
        _ -> x
      }
      let new_y = case direction {
        Up -> y + 1
        Down -> y - 1
        _ -> y
      }
      let mark = e_maps_get(tuple(new_x, new_y), map, current_wire)
      let tuple(new_map, new_best_crossing) = case mark == current_wire {
        True -> tuple(e_maps_put(tuple(new_x, new_y), current_wire, map), best_crossing)
        False -> {
          let our_new_best_crossing = min(best_crossing, abs(new_x) + abs(new_y))
          tuple(map, our_new_best_crossing)
        }
      }
      walk_a(new_map, new_x, new_y, direction, distance - 1, current_wire, new_best_crossing)
    }
  }
}

fn solve_a(first_wire, second_wire, map, x, y, best_crossing) {
  case first_wire {
    [] -> {
      case second_wire {
        [] -> best_crossing
        _  -> {
          let tuple(direction, distance) = e_hd(second_wire)
          let tuple(new_map, new_x, new_y, new_best_crossing) = walk_a(map, x, y, direction, distance, SecondWire, best_crossing)
          solve_a([], e_tl(second_wire), new_map, new_x, new_y, new_best_crossing)
        }
      }
    }
    [_last_wire] -> {
      let tuple(direction, distance) = e_hd(first_wire)
      let tuple(new_map, _new_x, _new_y, new_best_crossing) = walk_a(map, x, y, direction, distance, FirstWire, best_crossing)
      solve_a(e_tl(first_wire), second_wire, new_map, 0, 0, new_best_crossing)
    }
    _ -> {
      let tuple(direction, distance) = e_hd(first_wire)
      let tuple(new_map, new_x, new_y, new_best_crossing) = walk_a(map, x, y, direction, distance, FirstWire, best_crossing)
      solve_a(e_tl(first_wire), second_wire, new_map, new_x, new_y, new_best_crossing)
    }
  }
}

fn walk_b(map, x, y, direction, distance, current_wire, steps, best_crossing) {
  case distance {
    0 ->
      tuple(map, x, y, steps, best_crossing)
    distance -> {
      let current_steps = steps + 1
      let new_x = case direction {
        Right -> x + 1
        Left -> x - 1
        _ -> x
      }
      let new_y = case direction {
        Up -> y + 1
        Down -> y - 1
        _ -> y
      }
      let tuple(mark, steps_mark) = e_maps_get(tuple(new_x, new_y), map, tuple(current_wire, current_steps))
      let tuple(new_map, new_best_crossing) = case mark == current_wire {
        True -> {
          case steps_mark == current_steps {
            True -> tuple(e_maps_put(tuple(new_x, new_y), tuple(current_wire, current_steps), map), best_crossing)
            False -> tuple(map, best_crossing)
          }
        }
        False -> {
          let our_new_best_crossing = min(best_crossing, current_steps + steps_mark)
          tuple(map, our_new_best_crossing)
        }
      }
      walk_b(new_map, new_x, new_y, direction, distance - 1, current_wire, current_steps, new_best_crossing)
    }
  }
}

fn solve_b(first_wire, second_wire, map, x, y, steps, best_crossing) {
  case first_wire {
    [] -> {
      case second_wire {
        [] -> best_crossing
        _  -> {
          let tuple(direction, distance) = e_hd(second_wire)
          let tuple(new_map, new_x, new_y, new_steps, new_best_crossing) = walk_b(map, x, y, direction, distance, SecondWire, steps, best_crossing)
          solve_b([], e_tl(second_wire), new_map, new_x, new_y, new_steps, new_best_crossing)
        }
      }
    }
    [_last_wire] -> {
      let tuple(direction, distance) = e_hd(first_wire)
      let tuple(new_map, _new_x, _new_y, _new_steps, new_best_crossing) = walk_b(map, x, y, direction, distance, FirstWire, steps, best_crossing)
      solve_b(e_tl(first_wire), second_wire, new_map, 0, 0, 0, new_best_crossing)
    }
    _ -> {
      let tuple(direction, distance) = e_hd(first_wire)
      let tuple(new_map, new_x, new_y, new_steps, new_best_crossing) = walk_b(map, x, y, direction, distance, FirstWire, steps, best_crossing)
      solve_b(e_tl(first_wire), second_wire, new_map, new_x, new_y, new_steps, new_best_crossing)
    }
  }
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
  let a = solve_a(first_input, second_input, e_maps_new(), 0, 0, 999999999999999999)
  e_io_put_chars(e_integer_to_binary(a))
  e_io_put_chars("\n")
  let b = solve_b(first_input, second_input, e_maps_new(), 0, 0, 0, 999999999999999999)
  e_io_put_chars(e_integer_to_binary(b))
  e_io_put_chars("\n")
  0
}