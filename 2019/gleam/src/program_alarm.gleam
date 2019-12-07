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
external fn e_string_to_integer(string: String) -> struct(Int, String) = "string" "to_integer"

external fn e_lists_map(function: fn(a) -> b, list: List(a)) -> List(b) = "lists" "map"

external type Array(a)
external fn e_array_from_list(list: List(a)) -> Array(a) = "array" "from_list"
external fn e_array_get(index: Int, array: Array(a)) -> a = "array" "get"
external fn e_array_set(index: Int, value: a, array: Array(a)) -> Array(a) = "array" "set"

external fn e_integer_to_binary(int: Int) -> String = "erlang" "integer_to_binary"

fn struct2_1(s: struct(a, b)) -> a {
  let struct(x, _) = s
  x
}

fn string_to_integer(string: String) -> Int {
  struct2_1(e_string_to_integer(string))
}

fn solve_a(array, index) {
  case index {
    -1 -> {
      // initialize
      array
      |> e_array_set(1, 12, _)
      |> e_array_set(2, 2, _)
      |> solve_a(_, 0)
    }
    index -> {
      case e_array_get(index, array) {
        99 -> {
          e_array_get(0, array)
        }
        code -> {
          let first_value = e_array_get(e_array_get(index + 1, array), array)
          let second_value = e_array_get(e_array_get(index + 2, array), array)
          let target_value = case code {
            1 -> first_value + second_value
            2 -> first_value * second_value
          }
          let target_index = e_array_get(index + 3, array)
          let array = e_array_set(target_index, target_value, array)
          solve_a(array, index + 4)
        }
      }
    }
  }
}

fn solve_b(initial_array, array, index, noun, verb) {
  case index {
    -1 -> {
      // initialize
      array
      |> e_array_set(1, noun, _)
      |> e_array_set(2, verb, _)
      |> solve_b(initial_array, _, 0, noun, verb)
    }
    index -> {
      case e_array_get(index, array) {
        99 -> {
          case e_array_get(0, array) {
            19690720 -> {
              100 * noun + verb
            }
            _ -> {
              let struct(noun, verb) = case struct(noun, verb) {
                struct(_, 99)    -> struct(noun + 1, 0)
                struct(_, _) -> struct(noun, verb + 1)
              }
              solve_b(initial_array, initial_array, -1, noun, verb)
            }
          }
        }
        code -> {
          let first_value = e_array_get(e_array_get(index + 1, array), array)
          let second_value = e_array_get(e_array_get(index + 2, array), array)
          let target_value = case code {
            1 -> first_value + second_value
            2 -> first_value * second_value
          }
          let target_index = e_array_get(index + 3, array)
          let array = e_array_set(target_index, target_value, array)
          solve_b(initial_array, array, index + 4, noun, verb)
        }
      }
    }
  }
}

pub fn main(_) {
  let Ok(file) = e_file_open("../2", [])
  let Ok(line) = e_file_read_line(file)
  let input =
    line
    |> e_string_trim
    |> e_string_split(_, ",", All)
    |> e_lists_map(string_to_integer(_), _)
    |> e_array_from_list
  let a = solve_a(input, -1)
  e_io_put_chars(e_integer_to_binary(a))
  e_io_put_chars("\n")
  let b = solve_b(input, input, -1, 0, 0)
  e_io_put_chars(e_integer_to_binary(b))
  e_io_put_chars("\n")
  0
}