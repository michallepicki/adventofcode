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

external fn e_lists_map(function: fn(a) -> b, list: List(a)) -> List(b) = "lists" "map"
external fn e_lists_foldl(function: fn(a, b) -> b, acc: b, list: List(a)) -> b = "lists" "foldl"
external fn e_lists_reverse(list: List(a)) -> List(a) = "lists" "reverse"
external fn e_lists_max(list: List(a)) -> a = "lists" "max"
external fn e_lists_seq(from: Int, to: Int) -> List(Int) = "lists" "seq"
external fn e_lists_merge(list_of_lists: List(List(a))) -> List(a) = "lists" "merge"

external type Array(a)
external fn e_array_from_list(list: List(a)) -> Array(a) = "array" "from_list"
external fn e_array_get(index: Int, array: Array(a)) -> a = "array" "get"
external fn e_array_set(index: Int, value: a, array: Array(a)) -> Array(a) = "array" "set"

external fn e_binary_to_integer(string: String) -> Int = "erlang" "binary_to_integer"
external fn e_hd(list: List(a)) -> a = "erlang" "hd"
external fn e_length(list: List(a)) -> Int = "erlang" "length"

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

fn run_program(array, index, inputs, outputs) {
  let struct(_third_param_mode, second_param_mode, first_param_mode, opcode) = parse_instruction(e_array_get(index, array))
  case opcode {
    Halt -> {
      outputs
    }
    Add -> {
      let first_param = read(array, first_param_mode, index + 1)
      let second_param = read(array, second_param_mode, index + 2)
      let new_array = write(first_param + second_param, array, index + 3)
      run_program(new_array, index + 4, inputs, outputs)
    }
    Multiply -> {
      let first_param = read(array, first_param_mode, index + 1)
      let second_param = read(array, second_param_mode, index + 2)
      let new_array = write(first_param * second_param, array, index + 3)
      run_program(new_array, index + 4, inputs, outputs)
    }
    Input -> {
      let [input | new_inputs] = inputs
      let new_array = write(input, array, index + 1)
      run_program(new_array, index + 2, new_inputs, outputs)
    }
    Output -> {
      let output = read(array, first_param_mode, index + 1)
      let new_outputs = [output | outputs]
      run_program(array, index + 2, inputs, new_outputs)
    }
    JumpIfTrue -> {
      let first_param = read(array, first_param_mode, index + 1)
      let second_param = read(array, second_param_mode, index + 2)
      case first_param {
        0 -> run_program(array, index + 3, inputs, outputs)
        _ -> run_program(array, second_param, inputs, outputs)
      }
    }
    JumpIfFalse -> {
      let first_param = read(array, first_param_mode, index + 1)
      let second_param = read(array, second_param_mode, index + 2)
      case first_param {
        0 -> run_program(array, second_param, inputs, outputs)
        _ -> run_program(array, index + 3, inputs, outputs)
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
      run_program(new_array, index + 4, inputs, outputs)
    }
    Equals -> {
      let first_param = read(array, first_param_mode, index + 1)
      let second_param = read(array, second_param_mode, index + 2)
      let value = case first_param == second_param {
        True -> 1
        False -> 0
      }
      let new_array = write(value, array, index + 3)
      run_program(new_array, index + 4, inputs, outputs)
    }
  }
}

fn remove_nth(n, list, acc) {
  let [nth | rest] = list
  case n {
    1 -> struct(nth, e_lists_merge([e_lists_reverse(acc), rest]))
    _ -> remove_nth(n - 1, rest, [nth | acc])
  }
}

fn perms(list) {
  case e_length(list) {
    1 -> [list]
    n -> {
      e_lists_seq(1, n)
      |> e_lists_map(
        fn(index){
          let struct(nth, rest) = remove_nth(index, list, [])
          rest
          |> perms(_)
          |> e_lists_map(fn(perm){ [nth | perm] }, _)
        },
        _
      )
      |> e_lists_merge(_)
    }
  }
}

fn run_with_settings(program, settings) {
  e_lists_foldl(
    fn(x, previous_output) {
      run_program(program, 0, [x | e_lists_reverse(previous_output)], [])
    },
    [0],
    settings
  )
} 

pub fn main(_) {
  let Ok(file) = e_file_open("../7", [Binary])
  let Ok(line) = e_file_read_line(file)
  let program =
    line
    |> e_string_trim
    |> e_string_split(_, ",", All)
    |> e_lists_map(e_binary_to_integer(_), _)
    |> e_array_from_list
  let all_phase_settings = perms([0, 1, 2, 3, 4])
  all_phase_settings
  |> e_lists_map(fn(settings) { e_hd(run_with_settings(program, settings)) }, _)
  |> e_lists_max(_)
  |> e_display(_)
  0
}