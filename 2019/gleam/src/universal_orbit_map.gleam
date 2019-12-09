external type Any
external fn e_display(a) -> Bool = "erlang" "display"

external type Fd
enum FileMode {
  Binary
}
external fn e_file_open(path: String, modes: List(FileMode)) -> Result(Fd, Any) = "file" "open"
external fn e_file_read_line(file: Fd) -> Result(String, Any) = "file" "read_line"

enum Where {
  Leading
  Trailing
  All
}
external fn e_string_split(string: String, pattern: String, where: Where) -> List(String) = "string" "split"
external fn e_string_trim(string: String) -> String = "string" "trim"

external type Graph
external fn e_digraph_new() -> Graph = "digraph" "new"
external fn e_digraph_add_vertex(g: Graph, v: a) -> Any = "digraph" "add_vertex"
external fn e_digraph_add_edge(g: Graph, x: a, y: a) -> Any = "digraph" "add_edge"
external fn e_digraph_out_neighbours(g: Graph, v: a) -> List(a) = "digraph" "out_neighbours"
external fn e_digraph_in_neighbours(g: Graph, v: a) -> List(a) = "digraph" "in_neighbours"
external fn e_digraph_del_vertex(g: Graph, v: a) -> Any = "digraph" "del_vertex"

external fn e_lists_map(function: fn(a) -> b, list: List(a)) -> List(b) = "lists" "map"
external fn e_lists_foldl(function: fn(a, b) -> b, acc: b, list: List(a)) -> b = "lists" "foldl"
external fn e_lists_merge(list_of_lists: List(List(a))) -> List(a) = "lists" "merge"


fn populate_graph(file, graph) {
  case e_file_read_line(file) {
    Ok(line) -> {
      case e_string_split(e_string_trim(line), ")", All) {
        [object, satellite] -> {
          e_digraph_add_vertex(graph, object)
          e_digraph_add_vertex(graph, satellite)
          e_digraph_add_edge(graph, object, satellite)
          populate_graph(file, graph)
        }
        _ -> 0
      }
    }
    _ -> 0
  }
}

fn total_orbits(graph, vertex, acc) {
  vertex
  |> e_digraph_out_neighbours(graph, _)
  |> e_lists_map(fn(v){ total_orbits(graph, v, acc + 1) }, _)
  |> e_lists_foldl(fn(a, b){ a + b }, acc, _)
}

fn distance(graph, to, search) {
  let [struct(current_vertex, current_distance) | remaining_search] = search
  case current_vertex == to {
    True -> current_distance - 2
    False -> {
      let neighbours = e_lists_merge([e_digraph_out_neighbours(graph, current_vertex), e_digraph_in_neighbours(graph, current_vertex)])
      let neighbours_with_distance = e_lists_map(fn(neighbour) { struct(neighbour, current_distance + 1) }, neighbours)
      e_digraph_del_vertex(graph, current_vertex)
      distance(graph, to, e_lists_merge([remaining_search, neighbours_with_distance]))
    }
  }
}

pub fn main(_) {
  let Ok(file) = e_file_open("../6", [Binary])
  let graph = e_digraph_new()
  populate_graph(file, graph)
  e_display(total_orbits(graph, "COM", 0))
  e_display(distance(graph, "SAN", [struct("YOU", 0)]))
  0
}