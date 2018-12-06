#!/usr/bin/env elixir
defmodule ChronalCoordinates do
  def solve_a(locations) do
    {{min_x, min_y}, {max_x, max_y}} = bounding_box = bounding_box(locations)

    for x <- min_x..max_x, y <- min_y..max_y do
      {x, y}
    end
    |> Enum.map(&find_closest(locations, &1))
    |> Enum.into(%{})
    |> remove_infinite(locations, bounding_box)
    |> Enum.map(&elem(&1, 1))
    |> Enum.filter(& &1)
    |> Enum.group_by(& &1)
    |> Enum.max_by(fn {_point, list} -> length(list) end)
    |> elem(1)
    |> length()
  end

  def bounding_box(locations) do
    {{min_x, _}, {max_x, _}} = Enum.min_max_by(locations, &elem(&1, 0))
    {{_, min_y}, {_, max_y}} = Enum.min_max_by(locations, &elem(&1, 1))
    {{min_x, min_y}, {max_x, max_y}}
  end

  def find_closest(locations, position) do
    distances = Enum.map(locations, &{&1, manhattan_distance(&1, position)})
    {_, min_distance} = Enum.min_by(distances, &elem(&1, 1))

    case Enum.filter(distances, &(elem(&1, 1) == min_distance)) do
      [{nearest, _}] -> {position, nearest}
      _ -> {position, nil}
    end
  end

  def remove_infinite(points_with_nearest, locations, bounding_box) do
    infinite_solutions =
      Enum.reduce(locations, [], &check_if_infinite(points_with_nearest, bounding_box, &1, &2))

    Enum.reject(points_with_nearest, &(elem(&1, 1) in infinite_solutions))
  end

  def check_if_infinite(
        points_with_nearest,
        {{min_x, min_y}, {max_x, max_y}},
        {x, y} = location,
        acc
      ) do
    if Map.get(points_with_nearest, {min_x, y}) == location ||
         Map.get(points_with_nearest, {max_x, y}) == location ||
         Map.get(points_with_nearest, {x, min_y}) == location ||
         Map.get(points_with_nearest, {max_x, max_y}) == location do
      [location | acc]
    else
      acc
    end
  end

  def solve_b(locations) do
    {{min_x, min_y}, {max_x, max_y}} = bounding_box(locations)

    for x <- min_x..max_x, y <- min_y..max_y do
      {x, y}
    end
    |> Enum.map(fn position ->
      locations |> Enum.map(&manhattan_distance(&1, position)) |> Enum.sum()
    end)
    |> Enum.filter(&(&1 < 10000))
    |> Enum.count()
  end

  def manhattan_distance({x1, y1}, {x2, y2}) do
    abs(x1 - x2) + abs(y1 - y2)
  end

  def read_file(filename) do
    filename
    |> File.stream!()
    |> Enum.map(fn line ->
      line
      |> String.trim()
      |> String.split(", ")
      |> Enum.map(&String.to_integer/1)
      |> (fn [x, y] -> {x, y} end).()
    end)
  end
end

case System.argv() do
  ["--test"] ->
    ExUnit.start()

    defmodule ChronalCoordinatesTest do
      use ExUnit.Case

      test "example input" do
        assert 17 = ChronalCoordinates.solve_a([{1, 1}, {1, 6}, {8, 3}, {3, 4}, {5, 5}, {8, 9}])
      end
    end

  _ ->
    "../6" |> ChronalCoordinates.read_file() |> ChronalCoordinates.solve_a() |> IO.inspect()
    "../6" |> ChronalCoordinates.read_file() |> ChronalCoordinates.solve_b() |> IO.inspect()
end
