#!/usr/bin/env elixir
defmodule NoMatterHowYouSliceIt do
  def solve(data) do
    Enum.reduce(
      data,
      {0, %{}, MapSet.new()},
      fn [id, x, y, w, h], {overlap_count, fabric, candidates} ->
        candidates = MapSet.put(candidates, id)
        coords = for i <- x..(x + w - 1), j <- y..(y + h - 1), do: {i, j}

        Enum.reduce(
          coords,
          {overlap_count, fabric, candidates},
          fn coords, {overlap_count, fabric, candidates} ->
            case Map.get(fabric, coords, :none) do
              :none ->
                {overlap_count, Map.put(fabric, coords, {id, 1}), candidates}

              {previous_id, 1} ->
                candidates =
                  candidates
                  |> MapSet.delete(previous_id)
                  |> MapSet.delete(id)

                {overlap_count + 1, Map.put(fabric, coords, 2), candidates}

              count ->
                {overlap_count, Map.put(fabric, coords, count + 1), MapSet.delete(candidates, id)}
            end
          end
        )
      end
    )
  end

  def read_file(filename) do
    filename
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Stream.map(&String.split(&1, [" ", ",", "x", "#", "@", ":"]))
    |> Stream.map(&Enum.filter(&1, fn part -> part != "" end))
    |> Stream.map(&Enum.map(&1, fn string -> String.to_integer(string) end))
  end
end

"../3"
|> NoMatterHowYouSliceIt.read_file()
|> Enum.to_list()
|> NoMatterHowYouSliceIt.solve()
|> (fn {p1, _, p2} ->
      IO.inspect(p1)
      IO.inspect(p2)
    end).()
