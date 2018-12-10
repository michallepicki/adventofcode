#!/usr/bin/env elixir
defmodule TheStarsAlign do
  def solve_a(data) do
    # found the magic seconds number by trial and error, looking at stars converging into smallest area LOL
    # I guess it's not a real solution but whatever :)
    seconds = 10595

    points =
      Enum.map(data, fn [x, y, xv, yv] -> {{x + seconds * xv, y + seconds * yv}, {xv, yv}} end)
      |> Enum.into(%{})

    sky =
      for i <- 0..400, j <- 0..401, into: "" do
        case {j, i} do
          {401, _} ->
            "\n"

          pair ->
            if Map.get(points, pair) do
              "#"
            else
              "."
            end
        end
      end

    File.write("sky", sky)
  end

  def read_file(filename) do
    filename
    |> File.stream!()
    |> Enum.map(&(Regex.scan(~r/\-?\d+/, &1) |> Enum.map(fn [i] -> String.to_integer(i) end)))
  end
end

"../10" |> TheStarsAlign.read_file() |> TheStarsAlign.solve() |> IO.inspect()
