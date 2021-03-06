#!/usr/bin/env elixir
defmodule ChronalCalibration do
  def solve_b(freq_changes) do
    freq_changes
    |> Stream.cycle()
    |> Enum.reduce_while(
      {0, MapSet.new([0])},
      fn freq_change, {prev_freq, reached_freqs} = _acc ->
        freq = prev_freq + freq_change

        if freq in reached_freqs do
          {:halt, freq}
        else
          reached_freqs = MapSet.put(reached_freqs, freq)
          {:cont, {freq, reached_freqs}}
        end
      end
    )
  end

  def read_file(filename) do
    filename
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Stream.map(&String.to_integer/1)
  end
end

"../1" |> ChronalCalibration.read_file() |> Enum.sum() |> IO.puts()
"../1" |> ChronalCalibration.read_file() |> ChronalCalibration.solve_b() |> IO.puts()
