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

external fn e_lists_map(function: fn(a) -> b, list: List(a)) -> List(b) = "lists" "map"
// external fn e_lists_foldl(function: fn(a, b) -> b, acc: b, list: List(a)) -> b = "lists" "foldl"
external fn e_lists_reverse(list: List(a)) -> List(a) = "lists" "reverse"
external fn e_lists_max(list: List(a)) -> a = "lists" "max"
external fn e_lists_seq(from: Int, to: Int) -> List(Int) = "lists" "seq"
external fn e_lists_append(list_of_lists: List(List(a))) -> List(a) = "lists" "append"

external type Array(a)
external fn e_array_from_list(list: List(a)) -> Array(a) = "array" "from_list"
external fn e_array_get(index: Int, array: Array(a)) -> a = "array" "get"
external fn e_array_set(index: Int, value: a, array: Array(a)) -> Array(a) = "array" "set"

external fn e_binary_to_integer(string: String) -> Int = "erlang" "binary_to_integer"
external fn e_hd(list: List(a)) -> a = "erlang" "hd"
external fn e_length(list: List(a)) -> Int = "erlang" "length"

type Opcode {
  Halt
  Add
  Multiply
  Input
  Output
  JumpIfTrue
  JumpIfFalse
  LessThan
  Equals
  Unknown
}
type ParamMode {
  Position
  Immediate
}
fn parse_instruction(instruction) {
  let param3_mode = case instruction / 10000 % 10 {
    0 -> Position
    1 -> Immediate
  }
  let param2_mode = case instruction / 1000 % 10 {
    0 -> Position
    1 -> Immediate
  }
  let param1_mode = case instruction / 100 % 10 {
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
    _ -> Unknown
  }
  tuple(param3_mode, param2_mode, param1_mode, opcode)
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

fn run_program(program_state) {
  let tuple(_last_opcode, memory, program_counter, inputs, outputs) = program_state
  let tuple(_, param2_mode, param1_mode, opcode) = parse_instruction(e_array_get(program_counter, memory))
  case opcode {
    Halt -> {
      tuple(Halt, memory, program_counter, inputs, outputs)
    }
    Add -> {
      let param1 = read(memory, param1_mode, program_counter + 1)
      let param2 = read(memory, param2_mode, program_counter + 2)
      let new_memory = write(param1 + param2, memory, program_counter + 3)
      run_program(tuple(Add, new_memory, program_counter + 4, inputs, outputs))
    }
    Multiply -> {
      let param1 = read(memory, param1_mode, program_counter + 1)
      let param2 = read(memory, param2_mode, program_counter + 2)
      let new_memory = write(param1 * param2, memory, program_counter + 3)
      run_program(tuple(Multiply, new_memory, program_counter + 4, inputs, outputs))
    }
    Input -> {
      case inputs {
        [] -> {
          tuple(Input, memory, program_counter, inputs, outputs)
        }
        [input | new_inputs] -> {
          let new_memory = write(input, memory, program_counter + 1)
          run_program(tuple(Input, new_memory, program_counter + 2, new_inputs, outputs))
        }
      }
    }
    Output -> {
      let output = read(memory, param1_mode, program_counter + 1)
      let new_outputs = [output | outputs]
      run_program(tuple(Output, memory, program_counter + 2, inputs, new_outputs))
    }
    JumpIfTrue -> {
      let param1 = read(memory, param1_mode, program_counter + 1)
      let param2 = read(memory, param2_mode, program_counter + 2)
      case param1 {
        0 -> run_program(tuple(JumpIfTrue, memory, program_counter + 3, inputs, outputs))
        _ -> run_program(tuple(JumpIfTrue, memory, param2, inputs, outputs))
      }
    }
    JumpIfFalse -> {
      let param1 = read(memory, param1_mode, program_counter + 1)
      let param2 = read(memory, param2_mode, program_counter + 2)
      case param1 {
        0 -> run_program(tuple(JumpIfFalse, memory, param2, inputs, outputs))
        _ -> run_program(tuple(JumpIfFalse, memory, program_counter + 3, inputs, outputs))
      }
    }
    LessThan -> {
      let param1 = read(memory, param1_mode, program_counter + 1)
      let param2 = read(memory, param2_mode, program_counter + 2)
      let value = case param1 < param2 {
        True -> 1
        False -> 0
      }
      let new_memory = write(value, memory, program_counter + 3)
      run_program(tuple(LessThan, new_memory, program_counter + 4, inputs, outputs))
    }
    Equals -> {
      let param1 = read(memory, param1_mode, program_counter + 1)
      let param2 = read(memory, param2_mode, program_counter + 2)
      let value = case param1 == param2 {
        True -> 1
        False -> 0
      }
      let new_memory = write(value, memory, program_counter + 3)
      run_program(tuple(Equals, new_memory, program_counter + 4, inputs, outputs))
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

fn permutations(list) {
  case e_length(list) {
    1 -> [list]
    n -> {
      e_lists_seq(0, n - 1)
      |> e_lists_map(
        fn(index){
          let tuple(nth, rest) = remove_nth(index, list, [])
          rest
          |> permutations(_)
          |> e_lists_map(fn(permutation){ [nth | permutation] }, _)
        },
        _
      )
      |> e_lists_append(_)
    }
  }
}

fn create_amplifier(memory, inputs) {
  tuple(Unknown, memory, 0, inputs, [])
}

fn amplify_signal(amplifiers, acc, previous_outputs) {
  case amplifiers {
    [] -> {
      let [last_amplifier | _] = acc
      let tuple(opcode, _, _, _, _) = last_amplifier
      case opcode {
        Halt -> e_hd(previous_outputs)
        _ -> amplify_signal(e_lists_reverse(acc), [], previous_outputs)
      }
    }
    [amplifier | rest]  -> {
      let tuple(opcode, memory, program_counter, inputs, _) = amplifier
      let inputs = e_lists_append([inputs, e_lists_reverse(previous_outputs)])
      let amplifier = run_program(tuple(opcode, memory, program_counter, inputs, []))
      let tuple(_, _, _, _, outputs) = amplifier
      amplify_signal(rest, [amplifier | acc], outputs)
    }
  }
}

fn run_circuit_with_phase_settings(software, phase_settings) {
  phase_settings
  |> e_lists_map(fn(phase_setting) { create_amplifier(software, [phase_setting]) }, _)
  |> amplify_signal(_, [], [0])
}

pub fn main(_) {
  let Ok(file) = e_file_open("../7", [Binary])
  let Ok(line) = e_file_read_line(file)
  let software =
    line
    |> e_string_trim
    |> e_string_split(_, ",", All)
    |> e_lists_map(e_binary_to_integer(_), _)
    |> e_array_from_list

  let phase_settings_permutations = permutations([0, 1, 2, 3, 4])
  phase_settings_permutations
  |> e_lists_map(fn(phase_settings) { run_circuit_with_phase_settings(software, phase_settings) }, _)
  |> e_lists_max(_)
  |> e_display(_)

  let phase_settings_permutations = permutations([5, 6, 7, 8, 9])
  phase_settings_permutations
  |> e_lists_map(fn(phase_settings) { run_circuit_with_phase_settings(software, phase_settings) }, _)
  |> e_lists_max(_)
  |> e_display(_)
  0
}