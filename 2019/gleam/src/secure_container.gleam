// external fn e_display(a) -> Bool = "erlang" "display"
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
external type Characters
// external fn e_string_slice(string: String, start: Int) -> Characters = "string" "slice"
// external fn e_string_slice_with_length(string: String, start: Int, length: Int) -> Characters = "string" "slice"
// external fn e_string_to_graphemes(string: String) -> List(Characters) = "string" "to_graphemes"

// external fn e_unicode_characters_to_binary(characters: List(Characters)) -> String = "unicode" "characters_to_binary"

// external fn e_hd(List(a)) -> a = "erlang" "hd"
// external fn e_tl(List(a)) -> List(a) = "erlang" "tl"
external fn e_length(List(a)) -> Int = "erlang" "length"
external fn e_binary_to_integer(string: String) -> Int = "erlang" "binary_to_integer"
external fn e_integer_to_binary(int: Int) -> String = "erlang" "integer_to_binary"
// external fn e_abs(int: Int) -> Int = "erlang" "abs"

// external fn e_lists_map(function: fn(a) -> b, list: List(a)) -> List(b) = "lists" "map"
// external fn e_lists_foldl(function: fn(a, b) -> b, acc: b, list: List(a)) -> b = "lists" "foldl"
// external fn e_lists_sort(list: List(a)) -> List(a) = "lists" "sort"
// external fn e_lists_sort_by(function: fn(a, a) -> Bool, List(a)) -> List(a) = "lists" "sort"
// external fn e_lists_reverse(list: List(a)) -> List(a) = "lists" "reverse"
// external fn e_lists_seq(start: Int, end: Int) -> List(int) = "lists" "seq"
// external fn e_lists_filter(function: fn(a) -> Bool, List(a)) -> List(a) = "lists" "filter"

// external type Ordset(a)
// external fn e_ordsets_from_list(list: List(a)) -> Ordset(a) = "ordsets" "from_list"
// external fn e_ordsets_intersection(a: Ordset(a), b: Ordset(a)) -> Ordset(a) = "ordsets" "intersection"
// external fn e_ordsets_to_list(ordset: Ordset(a)) -> List(a) = "ordsets" "to_list"

// external type Map(k, v)
// external fn e_maps_new() -> Map(k, v) = "maps" "new"
// external fn e_maps_put(key: k, value: v, map: Map(k, v)) -> Map(k, v) = "maps" "put"
// external fn e_maps_get(key: k, map: Map(k, v)) -> v = "maps" "get"
// external fn e_maps_get_with_default(key: k, map: Map(k, v), default: v) -> v = "maps" "get"

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