#!/usr/bin/env elixir
defmodule TheSumOfItsParts do
  def solve_a(instructions) do
    instructions
    |> build_graph()
    |> sort()
  end

  def build_graph(instructions) do
    instructions
    |> Enum.reduce(
      Map.new(),
      fn {a, b}, graph ->
        graph = Map.put_new(graph, a, [])
        Map.update(graph, b, [a], fn requirements -> [a | requirements] end)
      end
    )
  end

  def sort(graph), do: sort(graph, [])

  def sort(graph, done) when map_size(graph) == 0, do: Enum.reverse(done)

  def sort(graph, done) do
    available = available_parts(graph) |> Enum.sort()
    doing = hd(available)
    graph = remove(graph, doing)
    sort(graph, [doing | done])
  end

  def remove(graph, removed_part) do
    graph
    |> Enum.map(fn {part, requirements} ->
      {part, Enum.reject(requirements, &(&1 == removed_part))}
    end)
    |> Enum.into(%{})
    |> Map.delete(removed_part)
  end

  def available_parts(graph) do
    graph
    |> Enum.filter(&(elem(&1, 1) == []))
    |> Enum.map(&elem(&1, 0))
  end

  def solve_b(instructions) do
    instructions
  end

  def read_file(filename) do
    filename
    |> File.stream!()
    |> Enum.map(fn line ->
      line
      |> String.trim()
      |> interpret_line()
    end)
  end

  def interpret_line(
        "Step " <> <<first>> <> " must be finished before step " <> <<second>> <> " can begin."
      ) do
    {first, second}
  end
end

case System.argv() do
  ["--test"] ->
    ExUnit.start()

    defmodule TheSumOfItsPartsTest do
      use ExUnit.Case

      test "part a" do
        assert 'CABDFEG' =
                 TheSumOfItsParts.solve_a([
                   {?C, ?A},
                   {?C, ?F},
                   {?A, ?B},
                   {?A, ?D},
                   {?B, ?E},
                   {?D, ?E},
                   {?F, ?E},
                   {?A, ?G}
                 ])
      end
    end

  _ ->
    "../7" |> TheSumOfItsParts.read_file() |> TheSumOfItsParts.solve_a() |> IO.inspect()
    #"../7" |> TheSumOfItsParts.read_file() |> TheSumOfItsParts.solve_b() |> IO.inspect()
end
