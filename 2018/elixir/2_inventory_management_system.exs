#! /usr/bin/env elixir
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
