#!/usr/bin/env elixir
defmodule InventoryManagementSystem do
  def checksum(box_ids) do
    box_ids
    |> Enum.reduce({0, 0}, fn id, {twos, threes} ->
      letter_counts =
        id
        |> String.graphemes()
        |> Enum.sort()
        |> Enum.chunk_by(& &1)
        |> Enum.map(&length/1)

      {if(2 in letter_counts, do: twos + 1, else: twos),
       if(3 in letter_counts, do: threes + 1, else: threes)}
    end)
    |> (&(elem(&1, 0) * elem(&1, 1))).()
  end

  def similar_ids(box_ids) do
    box_ids
    |> Enum.reduce_while(
      MapSet.new(),
      fn id, patterns ->
        for i <- 0..(String.length(id) - 1) do
          {head, tail} = String.split_at(id, i)
          {_head, tail} = String.split_at(tail, 1)
          head <> "_" <> tail
        end
        |> Enum.reduce_while(
          patterns,
          fn pattern, patterns ->
            if pattern in patterns do
              {:halt, pattern}
            else
              {:cont, MapSet.put(patterns, pattern)}
            end
          end
        )
        |> case do
          pattern when is_binary(pattern) ->
            {:halt, pattern |> String.replace("_", "")}

          patterns ->
            {:cont, patterns}
        end
      end
    )
  end

  def read_file(filename) do
    filename
    |> File.stream!()
    |> Stream.map(&String.trim/1)
  end
end

"../2"
|> InventoryManagementSystem.read_file()
|> InventoryManagementSystem.checksum()
|> IO.puts()

"../2"
|> InventoryManagementSystem.read_file()
|> InventoryManagementSystem.similar_ids()
|> IO.puts()
