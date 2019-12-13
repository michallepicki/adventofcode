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
  Unknown
}
enum ParamMode {
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
    _ -> Unknown
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

fn run_program(program_state) {
  let struct(_last_instruction, memory_state, program_counter, inputs, outputs) = program_state
  let struct(_third_param_mode, second_param_mode, first_param_mode, instruction) = parse_instruction(e_array_get(program_counter, memory_state))
  case instruction {
    Halt -> {
      struct(Halt, memory_state, program_counter, inputs, outputs)
    }
    Add -> {
      let first_param = read(memory_state, first_param_mode, program_counter + 1)
      let second_param = read(memory_state, second_param_mode, program_counter + 2)
      let new_memory_state = write(first_param + second_param, memory_state, program_counter + 3)
      run_program(struct(Add, new_memory_state, program_counter + 4, inputs, outputs))
    }
    Multiply -> {
      let first_param = read(memory_state, first_param_mode, program_counter + 1)
      let second_param = read(memory_state, second_param_mode, program_counter + 2)
      let new_memory_state = write(first_param * second_param, memory_state, program_counter + 3)
      run_program(struct(Multiply, new_memory_state, program_counter + 4, inputs, outputs))
    }
    Input -> {
      case inputs {
        [] -> {
          struct(Input, memory_state, program_counter, inputs, outputs)
        }
        [input | new_inputs] -> {
          let new_memory_state = write(input, memory_state, program_counter + 1)
          run_program(struct(Input, new_memory_state, program_counter + 2, new_inputs, outputs))
        }
      }
    }
    Output -> {
      let output = read(memory_state, first_param_mode, program_counter + 1)
      let new_outputs = [output | outputs]
      run_program(struct(Output, memory_state, program_counter + 2, inputs, new_outputs))
    }
    JumpIfTrue -> {
      let first_param = read(memory_state, first_param_mode, program_counter + 1)
      let second_param = read(memory_state, second_param_mode, program_counter + 2)
      case first_param {
        0 -> run_program(struct(JumpIfTrue, memory_state, program_counter + 3, inputs, outputs))
        _ -> run_program(struct(JumpIfTrue, memory_state, second_param, inputs, outputs))
      }
    }
    JumpIfFalse -> {
      let first_param = read(memory_state, first_param_mode, program_counter + 1)
      let second_param = read(memory_state, second_param_mode, program_counter + 2)
      case first_param {
        0 -> run_program(struct(JumpIfFalse, memory_state, second_param, inputs, outputs))
        _ -> run_program(struct(JumpIfFalse, memory_state, program_counter + 3, inputs, outputs))
      }
    }
    LessThan -> {
      let first_param = read(memory_state, first_param_mode, program_counter + 1)
      let second_param = read(memory_state, second_param_mode, program_counter + 2)
      let value = case first_param < second_param {
        True -> 1
        False -> 0
      }
      let new_memory_state = write(value, memory_state, program_counter + 3)
      run_program(struct(LessThan, new_memory_state, program_counter + 4, inputs, outputs))
    }
    Equals -> {
      let first_param = read(memory_state, first_param_mode, program_counter + 1)
      let second_param = read(memory_state, second_param_mode, program_counter + 2)
      let value = case first_param == second_param {
        True -> 1
        False -> 0
      }
      let new_memory_state = write(value, memory_state, program_counter + 3)
      run_program(struct(Equals, new_memory_state, program_counter + 4, inputs, outputs))
    }
  }
}

fn remove_nth(n, list, acc) {
  let [nth | rest] = list
  case n {
    0 -> struct(nth, e_lists_append([e_lists_reverse(acc), rest]))
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
          let struct(nth, rest) = remove_nth(index, list, [])
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

fn part_a_run_with_settings(program, settings) {
  settings
  |> e_lists_foldl(
      fn(phase_setting, previous_outputs) {
        let inputs = [phase_setting | e_lists_reverse(previous_outputs)]
        let struct(Halt, _, _, _, outputs) = run_program(struct(Unknown, program, 0, inputs, []))
        outputs
      },
      [0],
      _
    )
  |> e_hd(_)
}

fn part_b_run(amplifiers) {
  let [a, b, c, d, e] = amplifiers
  let struct(_, _, _, _, e_o) = e

  let struct(a_inst, a_m, a_pc, a_i, _) = a
  let new_a = run_program(struct(a_inst, a_m, a_pc, e_lists_append([a_i, e_lists_reverse(e_o)]), []))
  let struct(a_new_inst, _, _, _, a_new_o) = new_a

  let struct(b_inst, b_m, b_pc, b_i, _) = b
  let new_b = run_program(struct(b_inst, b_m, b_pc, e_lists_append([b_i, e_lists_reverse(a_new_o)]), []))
  let struct(b_new_inst, _, _, _, b_new_o) = new_b

  let struct(c_inst, c_m, c_pc, c_i, _) = c
  let new_c = run_program(struct(c_inst, c_m, c_pc, e_lists_append([c_i, e_lists_reverse(b_new_o)]), []))
  let struct(c_new_inst, _, _, _, c_new_o) = new_c

  let struct(d_inst, d_m, d_pc, d_i, _) = d
  let new_d = run_program(struct(d_inst, d_m, d_pc, e_lists_append([d_i, e_lists_reverse(c_new_o)]), []))
  let struct(d_new_inst, _, _, _, d_new_o) = new_d

  let struct(e_inst, e_m, e_pc, e_i, _) = e
  let new_e = run_program(struct(e_inst, e_m, e_pc, e_lists_append([e_i, e_lists_reverse(d_new_o)]), []))
  let struct(e_new_inst, _, _, _, e_new_o) = new_e

  case a_new_inst == Halt && b_new_inst == Halt && c_new_inst == Halt && d_new_inst == Halt && e_new_inst == Halt {
    True -> e_hd(e_new_o)
    False -> {
      part_b_run([new_a, new_b, new_c, new_d, new_e])
    }
  }
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

  let phase_settings = permutations([0, 1, 2, 3, 4])
  phase_settings
  |> e_lists_map(fn(settings) { part_a_run_with_settings(program, settings) }, _)
  |> e_lists_max(_)
  |> e_display(_)

  let phase_settings = permutations([5, 6, 7, 8, 9])
  phase_settings
  |> e_lists_map(
      fn(settings) {
        let [a, b, c, d, e] = settings
        let result = part_b_run([struct(Unknown, program, 0, [a, 0], []), struct(Unknown, program, 0, [b], []), struct(Unknown, program, 0, [c], []), struct(Unknown, program, 0, [d], []), struct(Unknown, program, 0, [e], [])])
        result
      },
      _
    )
  |> e_lists_max(_)
  |> e_display(_)
  0
}