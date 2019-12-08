external type Any
external fn e_io_put_chars(a) -> Any = "io" "put_chars"

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

external fn e_length(List(a)) -> Int = "erlang" "length"
external fn e_binary_to_integer(string: String) -> Int = "erlang" "binary_to_integer"
external fn e_integer_to_binary(int: Int) -> String = "erlang" "integer_to_binary"

fn parse_input(input) {
  let [range_start_string, range_end_string] = e_string_split(input, "-", All)
  struct(e_binary_to_integer(range_start_string), e_binary_to_integer(range_end_string))
}

fn is_a_good_password(digits) {
  let [a, b, c, d, e, f] = digits
  let double_exists = a == b || b == c || c == d || d == e || e == f
  double_exists && a <= b && b <= c && c <= d && d <= e && e <= f
}

fn solve_a(current, end, passwords) {
  case current > end {
    True -> e_length(passwords)
    False -> {
      let digits = [current / 100000, current / 10000 % 10, current / 1000 % 10, current / 100 % 10, current / 10 % 10, current % 10]
      case is_a_good_password(digits) {
        True -> solve_a(current + 1, end, [current | passwords])
        False -> solve_a(current + 1, end, passwords)
      }
    }
  }
}

fn the_double_is_lonely(digits, previous_digit, streak) {
  case digits {
    [] ->
      case streak {
        2 -> True
        _ -> False
      }
    [digit | digits] -> {
      case digit == previous_digit {
        True -> the_double_is_lonely(digits, digit, streak + 1)
        False ->
          case streak {
            2 -> True
            _ -> the_double_is_lonely(digits, digit, 1)
          }
      }
    }
  }
}

fn solve_b(current, end, passwords) {
  case current > end {
    True -> e_length(passwords)
    False -> {
      let digits = [current / 100000, current / 10000 % 10, current / 1000 % 10, current / 100 % 10, current / 10 % 10, current % 10]
      let passwords =
        case is_a_good_password(digits) {
          True ->
            case the_double_is_lonely(digits, -1, 0) {
              True -> [current | passwords]
              False -> passwords
            }
          False -> passwords
        }
      solve_b(current + 1, end, passwords)
    }
  }
}

pub fn main(_) {
  let Ok(file) = e_file_open("../4", [Binary])
  let Ok(input) = e_file_read_line(file)
  let struct(start, end) = parse_input(e_string_trim(input))
  let a = solve_a(start, end, [])
  e_io_put_chars(e_integer_to_binary(a))
  e_io_put_chars("\n")
  let b = solve_b(start, end, [])
  e_io_put_chars(e_integer_to_binary(b))
  e_io_put_chars("\n")
  0
}