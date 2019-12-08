external fn e_display(a) -> Bool = "erlang" "display"
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

external fn e_lists_map(function: fn(a) -> b, list: List(a)) -> List(b) = "lists" "map"

external type Array(a)
external fn e_array_from_list(list: List(a)) -> Array(a) = "array" "from_list"
external fn e_array_get(index: Int, array: Array(a)) -> a = "array" "get"
external fn e_array_set(index: Int, value: a, array: Array(a)) -> Array(a) = "array" "set"

external fn e_binary_to_integer(string: String) -> Int = "erlang" "binary_to_integer"
external fn e_integer_to_binary(int: Int) -> String = "erlang" "integer_to_binary"

enum Opcode {
  Halt
  Add
  Multiply
  Input
  Output
  JumpIfTrue
  JumpIfFalse
  LessThan
  Equals
}
enum Mode {
  Position
  Immediate
}
fn parse_instruction(instruction) {
  let third_param_mode = case instruction / 10000 % 10 {
    0 -> Position
    1 -> Immediate
  }
  let second_param_mode = case instruction / 1000 % 10 {
    0 -> Position
    1 -> Immediate
  }
  let first_param_mode = case instruction / 100 % 10 {
    0 -> Position
    1 -> Immediate
  }
  let opcode = case instruction % 100 {
    99 -> Halt
    1 -> Add
    2 -> Multiply
    3 -> Input
    4 -> Output
    5 -> JumpIfTrue
    6 -> JumpIfFalse
    7 -> LessThan
    8 -> Equals
  }
  struct(third_param_mode, second_param_mode, first_param_mode, opcode)
}

fn read(array, mode, param_index) {
  case mode {
    Position -> e_array_get(e_array_get(param_index, array), array)
    Immediate -> e_array_get(param_index, array)
  }
}
fn write(value, array, param_index) {
  e_array_set(e_array_get(param_index, array), value, array)
}

fn solve(array, index, inputs, outputs) {
  let struct(_third_param_mode, second_param_mode, first_param_mode, opcode) = parse_instruction(e_array_get(index, array))
  case opcode {
    Halt -> {
      outputs
    }
    Add -> {
      let first_param = read(array, first_param_mode, index + 1)
      let second_param = read(array, second_param_mode, index + 2)
      let new_array = write(first_param + second_param, array, index + 3)
      solve(new_array, index + 4, inputs, outputs)
    }
    Multiply -> {
      let first_param = read(array, first_param_mode, index + 1)
      let second_param = read(array, second_param_mode, index + 2)
      let new_array = write(first_param * second_param, array, index + 3)
      solve(new_array, index + 4, inputs, outputs)
    }
    Input -> {
      let [input | new_inputs] = inputs
      let new_array = write(input, array, index + 1)
      solve(new_array, index + 2, new_inputs, outputs)
    }
    Output -> {
      let output = read(array, first_param_mode, index + 1)
      let new_outputs = [output | outputs]
      solve(array, index + 2, inputs, new_outputs)
    }
    JumpIfTrue -> {
      let first_param = read(array, first_param_mode, index + 1)
      let second_param = read(array, second_param_mode, index + 2)
      case first_param {
        0 -> solve(array, index + 3, inputs, outputs)
        _ -> solve(array, second_param, inputs, outputs)
      }
    }
    JumpIfFalse -> {
      let first_param = read(array, first_param_mode, index + 1)
      let second_param = read(array, second_param_mode, index + 2)
      case first_param {
        0 -> solve(array, second_param, inputs, outputs)
        _ -> solve(array, index + 3, inputs, outputs)
      }
    }
    LessThan -> {
      let first_param = read(array, first_param_mode, index + 1)
      let second_param = read(array, second_param_mode, index + 2)
      let value = case first_param < second_param {
        True -> 1
        False -> 0
      }
      let new_array = write(value, array, index + 3)
      solve(new_array, index + 4, inputs, outputs)
    }
    Equals -> {
      let first_param = read(array, first_param_mode, index + 1)
      let second_param = read(array, second_param_mode, index + 2)
      let value = case first_param == second_param {
        True -> 1
        False -> 0
      }
      let new_array = write(value, array, index + 3)
      solve(new_array, index + 4, inputs, outputs)
    }
  }
}

pub fn main(_) {
  let Ok(file) = e_file_open("../5", [Binary])
  let Ok(line) = e_file_read_line(file)
  let input =
    line
    |> e_string_trim
    |> e_string_split(_, ",", All)
    |> e_lists_map(e_binary_to_integer(_), _)
    |> e_array_from_list
  let outputs_a = solve(input, 0, [1], [])
  e_display(outputs_a)
  let outputs_b = solve(input, 0, [5], [])
  e_display(outputs_b)
  0
}