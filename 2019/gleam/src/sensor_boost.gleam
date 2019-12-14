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
external fn e_lists_append(list_of_lists: List(List(a))) -> List(a) = "lists" "append"

external type Array(a)
external fn e_array_from_list(list: List(a), default: a) -> Array(a) = "array" "from_list"
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
  SetRelativeBase
  Unknown
}
enum ParamMode {
  Position
  Immediate
  Relative
}
fn parse_instruction(instruction) {
  let param3_mode = case instruction / 10000 % 10 {
    0 -> Position
    1 -> Immediate
    2 -> Relative
  }
  let param2_mode = case instruction / 1000 % 10 {
    0 -> Position
    1 -> Immediate
    2 -> Relative
  }
  let param1_mode = case instruction / 100 % 10 {
    0 -> Position
    1 -> Immediate
    2 -> Relative
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
    9 -> SetRelativeBase
    _ -> Unknown
  }
  struct(param3_mode, param2_mode, param1_mode, opcode)
}

fn read(array, relative_base, mode, param_index) {
  case mode {
    Position -> e_array_get(e_array_get(param_index, array), array)
    Immediate -> e_array_get(param_index, array)
    Relative -> e_array_get(e_array_get(param_index, array) + relative_base, array)
  }
}

fn write(value, relative_base, mode, array, param_index) {
  case mode {
    Position -> e_array_set(e_array_get(param_index, array), value, array)
    Relative -> e_array_set(e_array_get(param_index, array) + relative_base, value, array)
  }
}

fn run_program(program_state) {
  let struct(_last_opcode, memory, program_counter, relative_base, inputs, outputs) = program_state
  let struct(param3_mode, param2_mode, param1_mode, opcode) = parse_instruction(e_array_get(program_counter, memory))
  case opcode {
    Halt -> {
      struct(Halt, memory, program_counter, relative_base, inputs, outputs)
    }
    Add -> {
      let param1 = read(memory, relative_base, param1_mode, program_counter + 1)
      let param2 = read(memory, relative_base, param2_mode, program_counter + 2)
      let new_memory = write(param1 + param2, relative_base, param3_mode, memory, program_counter + 3)
      run_program(struct(Add, new_memory, program_counter + 4, relative_base, inputs, outputs))
    }
    Multiply -> {
      let param1 = read(memory, relative_base, param1_mode, program_counter + 1)
      let param2 = read(memory, relative_base, param2_mode, program_counter + 2)
      let new_memory = write(param1 * param2, relative_base, param3_mode, memory, program_counter + 3)
      run_program(struct(Multiply, new_memory, program_counter + 4, relative_base, inputs, outputs))
    }
    Input -> {
      case inputs {
        [] -> {
          struct(Input, memory, program_counter, relative_base, inputs, outputs)
        }
        [input | new_inputs] -> {
          let new_memory = write(input, relative_base, param1_mode, memory, program_counter + 1)
          run_program(struct(Input, new_memory, program_counter + 2, relative_base, new_inputs, outputs))
        }
      }
    }
    Output -> {
      let output = read(memory, relative_base, param1_mode, program_counter + 1)
      let new_outputs = [output | outputs]
      run_program(struct(Output, memory, program_counter + 2, relative_base, inputs, new_outputs))
    }
    JumpIfTrue -> {
      let param1 = read(memory, relative_base, param1_mode, program_counter + 1)
      let param2 = read(memory, relative_base, param2_mode, program_counter + 2)
      case param1 {
        0 -> run_program(struct(JumpIfTrue, memory, program_counter + 3, relative_base, inputs, outputs))
        _ -> run_program(struct(JumpIfTrue, memory, param2, relative_base, inputs, outputs))
      }
    }
    JumpIfFalse -> {
      let param1 = read(memory, relative_base, param1_mode, program_counter + 1)
      let param2 = read(memory, relative_base, param2_mode, program_counter + 2)
      case param1 {
        0 -> run_program(struct(JumpIfFalse, memory, param2, relative_base, inputs, outputs))
        _ -> run_program(struct(JumpIfFalse, memory, program_counter + 3, relative_base, inputs, outputs))
      }
    }
    LessThan -> {
      let param1 = read(memory, relative_base, param1_mode, program_counter + 1)
      let param2 = read(memory, relative_base, param2_mode, program_counter + 2)
      let value = case param1 < param2 {
        True -> 1
        False -> 0
      }
      let new_memory = write(value, relative_base, param3_mode, memory, program_counter + 3)
      run_program(struct(LessThan, new_memory, program_counter + 4, relative_base, inputs, outputs))
    }
    Equals -> {
      let param1 = read(memory, relative_base, param1_mode, program_counter + 1)
      let param2 = read(memory, relative_base, param2_mode, program_counter + 2)
      let value = case param1 == param2 {
        True -> 1
        False -> 0
      }
      let new_memory = write(value, relative_base, param3_mode, memory, program_counter + 3)
      run_program(struct(Equals, new_memory, program_counter + 4, relative_base, inputs, outputs))
    }
    SetRelativeBase -> {
      let param1 = read(memory, relative_base, param1_mode, program_counter + 1)
      let new_relative_base = relative_base + param1
      run_program(struct(SetRelativeBase, memory, program_counter + 2, new_relative_base, inputs, outputs))
    }
  }
}


pub fn main(_) {
  let Ok(file) = e_file_open("../9", [Binary])
  let Ok(line) = e_file_read_line(file)
  let software =
    line
    |> e_string_trim
    |> e_string_split(_, ",", All)
    |> e_lists_map(e_binary_to_integer(_), _)
    |> e_array_from_list(_, 0)

  let struct(_, _, _, _, _, outputs) =
    struct(Unknown, software, 0, 0, [1], [])
    |> run_program(_)

  e_display(e_lists_reverse(outputs))

  let struct(_, _, _, _, _, outputs) =
    struct(Unknown, software, 0, 0, [2], [])
    |> run_program(_)

  e_display(e_lists_reverse(outputs))
  0
}