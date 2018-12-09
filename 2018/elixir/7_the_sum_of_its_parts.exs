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
    |> build_graph()
    |> get_time()
  end

  def get_time(graph), do: schedule_work(graph, [nil, nil, nil, nil, nil], 0)
  # def get_time(graph), do: schedule_work(graph, [nil, nil], 0) # swap for test

  def schedule_work(graph, [nil, nil, nil, nil, nil], time) when map_size(graph) == 0, do: time
  # def schedule_work(graph, [nil, nil], time) when map_size(graph) == 0, do: time # swap for test

  def schedule_work(graph, workers, time) do
    in_progress = workers |> Enum.filter(& &1) |> Enum.map(&elem(&1, 0))
    available = (available_parts(graph) -- in_progress) |> Enum.sort()

    if nil in workers and Enum.count(available) > 0 do
      doing = hd(available)
      workers = List.delete(workers, nil)
      workers = [{doing, doing - 5} | workers]
      schedule_work(graph, workers, time)
    else
      tick(graph, workers, time)
    end
  end

  def tick(graph, workers, time) do
    {graph, workers} =
      Enum.reduce(workers, {graph, workers}, fn
        {part, 0}, {graph, workers} ->
          graph = remove(graph, part)
          workers = List.delete(workers, {part, 0})
          {graph, [nil | workers]}

        {part, time}, {graph, workers} ->
          workers = List.delete(workers, {part, time})
          {graph, [{part, time - 1} | workers]}
        nil, {graph, workers} -> {graph, workers}
      end)

    schedule_work(graph, workers, time + 1)
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

      test "part b" do
        assert 15 =
                 TheSumOfItsParts.solve_b([
                   {?C - 60, ?A - 60},
                   {?C - 60, ?F - 60},
                   {?A - 60, ?B - 60},
                   {?A - 60, ?D - 60},
                   {?B - 60, ?E - 60},
                   {?D - 60, ?E - 60},
                   {?F - 60, ?E - 60}
                 ])
      end
    end

  _ ->
    "../7" |> TheSumOfItsParts.read_file() |> TheSumOfItsParts.solve_a() |> IO.inspect()
    "../7" |> TheSumOfItsParts.read_file() |> TheSumOfItsParts.solve_b() |> IO.inspect()
end
