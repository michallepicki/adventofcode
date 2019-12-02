external type Any
external fn e_io_put_chars(a) -> Any = "io" "put_chars"

external type Fd
external fn e_file_open(path: String, modes: List(Any)) -> Result(Fd, Any) = "file" "open"
external fn e_file_read_line(file: Fd) -> Result(String, Any) = "file" "read_line"

external type Tuple
external fn e_string_to_integer(string: String) -> Tuple = "string" "to_integer"

external fn e_lists_reverse(list: List(a)) -> List(a) = "lists" "reverse"

external fn e_integer_tuple_element(index: Int, tuple: Tuple) -> Int = "erlang" "element"
external fn e_integer_to_binary(int: Int) -> String = "erlang" "integer_to_binary"

fn read_puzzle_input(file, accumulator) {
  case e_file_read_line(file) {
    Ok(line) -> {
      let integer = e_integer_tuple_element(1, e_string_to_integer(line))
      read_puzzle_input(file, [integer | accumulator])
    }
    _ -> {
      e_lists_reverse(accumulator)
    }
  }
}

fn solve_a(puzzle_input, fuel) {
  case puzzle_input {
    [] -> {
      fuel
    }
    [module_mass | rest] -> {
      solve_a(rest, fuel + module_mass / 3 - 2)
    }
  }
}

fn fuel(mass, accumulator) {
  let extra_fuel = mass / 3 - 2
  case extra_fuel > 0 {
    True -> {
      fuel(extra_fuel, accumulator + extra_fuel)
    }
    False -> {
      accumulator
    }
  }
}

fn solve_b(puzzle_input, accumulator) {
  case puzzle_input {
    [] -> {
      accumulator
    }
    [module_mass | rest] -> {
      solve_b(rest, accumulator + fuel(module_mass, 0))
    }
  }
}

pub fn main(_) {
  let Ok(file) = e_file_open("../1", [])
  let puzzle_input = read_puzzle_input(file, [])
  let a = solve_a(puzzle_input, 0)
  e_io_put_chars(e_integer_to_binary(a))
  e_io_put_chars("\n")
  let b = solve_b(puzzle_input, 0)
  e_io_put_chars(e_integer_to_binary(b))
  e_io_put_chars("\n")
  0
}