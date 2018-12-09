#!/usr/bin/env elixir
defmodule MemoryManeuver do
  def solve_a(data) do
    data
    |> read_node()
    |> elem(0)
    |> sum_metadata()
  end

  def read_node([children_count, metadata_count | data]) do
    {children, leftover} = read_nodes(data, children_count)
    {metadata, leftover} = read_metadata(leftover, metadata_count)
    {{children_count, metadata_count, children, metadata}, leftover}
  end

  def read_nodes(data, 0), do: {[], data}

  def read_nodes(data, children_count) do
    {node, leftover} = read_node(data)
    {nodes, leftover} = read_nodes(leftover, children_count - 1)
    {[node | nodes], leftover}
  end

  def read_metadata(data, 0), do: {[], data}

  def read_metadata([metadata | leftover], metadata_count) do
    {metadatas, leftover} = read_metadata(leftover, metadata_count - 1)
    {[metadata | metadatas], leftover}
  end

  def sum_metadata({_, _, children, metadata}) do
    Enum.sum(metadata) + Enum.sum(Enum.map(children, &sum_metadata/1))
  end

  def solve_b(data) do
    data
  end

  def read_file(filename) do
    filename
    |> File.read!()
    |> String.trim()
    |> String.split(" ")
    |> Enum.map(&String.to_integer/1)
  end
end

case System.argv() do
  ["--test"] ->
    ExUnit.start()

    defmodule MemoryManeuverTest do
      use ExUnit.Case

      test "part a" do
        assert 138 = MemoryManeuver.solve_a([2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2])
      end

      # test "part b" do
      #   assert 138 = MemoryManeuver.solve_a([2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2])
      # end
    end

  _ ->
    "../8" |> MemoryManeuver.read_file() |> MemoryManeuver.solve_a() |> IO.inspect()
    # "../8" |> MemoryManeuver.read_file() |> MemoryManeuver.solve_b() |> IO.inspect()
end
