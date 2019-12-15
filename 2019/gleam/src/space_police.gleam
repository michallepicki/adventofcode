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
external fn e_lists_foldl(function: fn(a, b) -> b, acc: b, list: List(a)) -> b = "lists" "foldl"
external fn e_lists_sort(list: List(a)) -> List(a) = "lists" "sort"
external fn e_lists_reverse(list: List(a)) -> List(a) = "lists" "reverse"
external fn e_lists_max(list: List(a)) -> a = "lists" "max"
external fn e_lists_min(list: List(a)) -> a = "lists" "min"
external fn e_lists_seq(from: Int, to: Int) -> List(Int) = "lists" "seq"
external fn e_lists_append(list_of_lists: List(List(a))) -> List(a) = "lists" "append"

external type Array(a)
external fn e_array_from_list(list: List(a), default: a) -> Array(a) = "array" "from_list"
external fn e_array_get(index: Int, array: Array(a)) -> a = "array" "get"
external fn e_array_set(index: Int, value: a, array: Array(a)) -> Array(a) = "array" "set"

external type Map(k, v)
external fn e_maps_new() -> Map(k, v) = "maps" "new"
external fn e_maps_put(key: k, value: v, map: Map(k, v)) -> Map(k, v) = "maps" "put"
external fn e_maps_get(key: k, map: Map(k, v)) -> v = "maps" "get"
external fn e_maps_get_with_default(key: k, map: Map(k, v), default: v) -> v = "maps" "get"
external fn e_maps_to_list(map: Map(k, v)) -> List(struct(k, v)) = "maps" "to_list"

external type Set(a)
external fn e_sets_from_list(list: List(a)) -> Set(a) = "sets" "from_list"
external fn e_sets_to_list(set: Set(a)) -> List(a) = "sets" "to_list"

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

fn reset_outputs(program_state) {
  let struct(a, b, c, d, e, _) = program_state
  struct(a, b, c, d, e, [])
}

fn add_inputs(program_state, new_inputs) {
  let struct(a, b, c, d, e, f) = program_state
  struct(a, b, c, d, e_lists_append([e, new_inputs]), f)
}

enum Direction {
  Up
  Right
  Down
  Left
}

fn turn(current_direction, turn_direction) {
  case turn_direction {
    0 -> { //left
      case current_direction {
        Up -> Left
        Right -> Up
        Down -> Right
        Left -> Down
      }
    }
    1 -> { //right
      case current_direction {
        Up -> Right
        Right -> Down
        Down -> Left
        Left -> Up
      }
    }
  }
}

fn step(current_panel, new_direction) {
  let struct(x, y) = current_panel
  case new_direction {
    Up -> struct(x, y + 1)
    Right -> struct(x + 1, y)
    Down -> struct(x, y - 1)
    Left -> struct(x - 1, y)
  }
}

fn move(current_panel, current_direction, turn_direction) {
  let new_direction = turn(current_direction, turn_direction)
  let new_panel = step(current_panel, new_direction)
  struct(new_panel, new_direction)
}

fn paint(program_state, area, current_panel, current_direction) {
  let struct(_, _, _, _, _, outputs) = program_state
  case outputs {
    [] -> {
      area
    }
    [turn_direction, color_to_paint] -> {
      let struct(new_panel, new_direction) = move(current_panel, current_direction, turn_direction)
      let new_program = 
        program_state
        |> reset_outputs(_)
        |> add_inputs(_, [e_maps_get_with_default(new_panel, area, 0)])
        |> run_program(_)
      let new_area = e_maps_put(current_panel, color_to_paint, area)
      paint(new_program, new_area, new_panel, new_direction)
    }
  }
}


pub fn main(_) {
  let Ok(file) = e_file_open("../11", [Binary])
  let Ok(line) = e_file_read_line(file)
  let software =
    line
    |> e_string_trim
    |> e_string_split(_, ",", All)
    |> e_lists_map(e_binary_to_integer(_), _)
    |> e_array_from_list(_, 0)

  let painted_panels_count =
    paint(run_program(struct(Unknown, software, 0, 0, [0], [])), e_maps_new(), struct(0, 0), Up)
    |> e_maps_to_list(_)
    |> e_length(_)

  e_display(painted_panels_count)

  let area =
    paint(run_program(struct(Unknown, software, 0, 0, [1], [])), e_maps_new(), struct(0, 0), Up)

  let area_list =
    area
    |> e_maps_to_list(_)
    |> e_lists_sort(_)

  let struct(struct(left_edge, _), _) = e_lists_min(area_list)
  let struct(struct(right_edge, _), _) = e_lists_max(area_list)
  let ys =
    area_list
    |> e_lists_map(
      fn(point) {
        let struct(struct(_, y), _) = point
        y
      },
      _
    )
  let top_edge = e_lists_min(ys)
  let bottom_edge = e_lists_max(ys)

  e_lists_seq(top_edge, bottom_edge)
  |> e_lists_reverse(_)
  |> e_lists_map(
    fn(y) {
      e_lists_seq(left_edge, right_edge)
      |> e_lists_map(
        fn(x) {
          case e_maps_get_with_default(struct(x, y), area, 0) {
            0 -> e_io_put_chars("#")
            1 -> e_io_put_chars(" ")
          }
          case x == right_edge {
            True -> {
              e_io_put_chars("\n")
              0
            }
            False -> 0
          }
        },
        _
      )
    },
    _
  )

  0
}