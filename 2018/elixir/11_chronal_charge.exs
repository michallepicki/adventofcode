#!/usr/bin/env elixir
defmodule ChronalCharge do
  def solve_a(serial_number) do
    power_grid(serial_number)
    |> Enum.filter(fn {_, {_, area_power}} -> area_power != nil end)
    |> Enum.max_by(fn {_, {_, area_power}} -> area_power end)
    |> elem(0)
  end

  def power_grid(serial_number) do
    for(
      x <- 300..1,
      y <- 300..1,
      do: {x, y}
    )
    |> Enum.reduce(%{}, &add_cell(&1, &2, serial_number))
  end

  def add_cell({x, y}, grid, serial_number) do
    cell_power = power_level({x, y}, serial_number)
    grid = Map.put(grid, {x, y}, {cell_power, nil})

    area_power =
      case {x, y} do
        {x, y} when x in [300, 299] or y in [300, 299] ->
          nil

        _ ->
          for(
            i <- x..(x + 2),
            j <- y..(y + 2),
            do: {i, j}
          )
          |> Enum.reduce(
            0,
            fn coords, sum ->
              {cell_power, _} = Map.get(grid, coords)
              sum + cell_power
            end
          )
      end

    Map.put(grid, {x, y}, {cell_power, area_power})
  end

  def power_level({x, y}, serial_number) do
    rack_id = x + 10
    (((rack_id * y + serial_number) * rack_id) |> rem(1000) |> div(100)) - 5
  end
end

case System.argv() do
  ["--test"] ->
    ExUnit.start()

    defmodule ChronalChargeTest do
      use ExUnit.Case

      test "power level" do
        assert 4 = ChronalCharge.power_level({3, 5}, 8)
        assert -5 = ChronalCharge.power_level({122, 79}, 57)
        assert 0 = ChronalCharge.power_level({217, 196}, 39)
        assert 4 = ChronalCharge.power_level({101, 153}, 71)
      end
    end

  _ ->
    8141 |> ChronalCharge.solve_a() |> IO.inspect()
    # 8141 |> ChronalCharge.solve_b() |> IO.inspect()
end
