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

external fn e_binary_to_list(string: String) -> List(Characters) = "erlang" "binary_to_list"
external fn e_string_to_integer(characters: List(Characters)) -> struct(Int, List(Characters)) = "string" "to_integer"

fn split_by(count, list, acc) {
  case list {
    [] -> e_lists_reverse(acc)
    _ -> {
      let struct(layer, rest) = e_lists_split(count, list)
      split_by(count, rest, [layer | acc])
    }
  }
}

pub fn main(_) {
  let Ok(file) = e_file_open("../8", [Binary])
  let Ok(line) = e_file_read_line(file)
  let layers =
    line
    |> e_string_trim(_)
    |> e_binary_to_list(_)
    |> e_lists_map(fn(digit_as_byte) {
        let struct(digit, _) = e_string_to_integer([digit_as_byte])
        digit
      }, _)
    |> split_by(25 * 6, _, [])

  let struct(_, ones, twos) =
    layers
    |> e_lists_map(fn(layer) {
        e_lists_foldl(fn(d, acc) {
          let struct(zeros, ones, twos) = acc
          case d {
            0 -> struct(zeros + 1, ones, twos)
            1 -> struct(zeros, ones + 1, twos)
            2 -> struct(zeros, ones, twos + 1)
            _ -> struct(zeros, ones, twos)
          }
        },
        struct(0, 0, 0),
        layer)
      }, _)
    |> e_lists_min(_)
  e_display(ones * twos)

  let [first_layer | remaining_layers] = layers
  remaining_layers
  |> e_lists_foldl(
    fn(layer, image) {
      e_lists_zipwith(
        fn(below, above) {
          case above {
            2 -> below
            _ -> above
          }
        },
        layer,
        image
      )
    },
    first_layer,
    _
  )
  |> split_by(25, _, [])
  |> e_lists_map(e_display(_), _)
  0
}