#!/usr/bin/env elixir
defmodule ChronalCharge do
  @grid_size 300

  def solve(serial_number) do
    grid = power_grid(serial_number)
    IO.inspect(grid.max_3)
    IO.inspect(grid.max)
  end

  def power_grid(serial_number) do
    for(
      x <- @grid_size..1,
      y <- @grid_size..1,
      do: {x, y}
    )
    |> Enum.reduce(%{max_3: nil, max: nil}, &add_cell(&1, &2, serial_number))
  end

  def add_cell({x, y}, grid, serial_number) do
    cell_power = power_level({x, y}, serial_number)
    grid = Map.put(grid, {x, y}, {cell_power, nil})

    {_topleft_sum, area_power, maximum, maximum_3} =
      Enum.reduce(
        1..min(@grid_size - x + 1, @grid_size - y + 1),
        {0, %{}, grid.max, grid.max_3},
        &update_area_power(&1, &2, {x, y}, grid)
      )

    grid
    |> Map.put(:max, maximum)
    |> Map.put(:max_3, maximum_3)
    |> Map.put({x, y}, {cell_power, area_power})
  end

  def update_area_power(1, {0, %{}, maximum, maximum_3}, coords, grid) do
    {cell_power, nil} = Map.get(grid, coords)
    area_power = %{1 => cell_power}

    maximum =
      if maximum == nil or max(elem(maximum, 0), cell_power) == cell_power do
        {cell_power, coords, 1}
      else
        maximum
      end

    {cell_power, area_power, maximum, maximum_3}
  end

  def update_area_power(square_size, {topleft_sum, area_power, maximum, maximum_3}, {x, y}, grid) do
    new_topleft_sum =
      topleft_sum + read_cell_power(grid, {x, y + square_size - 1}) +
        read_cell_power(grid, {x + square_size - 1, y})

    new_sum = new_topleft_sum + read_area_power(grid, {x + 1, y + 1})[square_size - 1]

    maximum =
      if maximum == nil or max(elem(maximum, 0), new_sum) == new_sum do
        {new_sum, {x, y}, square_size}
      else
        maximum
      end

    maximum_3 =
      if square_size == 3 and (maximum_3 == nil or max(elem(maximum_3, 0), new_sum) == new_sum) do
        {new_sum, {x, y}, square_size}
      else
        maximum_3
      end

    {new_topleft_sum,
     Map.put(
       area_power,
       square_size,
       new_sum
     ), maximum, maximum_3}
  end

  def read_cell_power(grid, coords) do
    grid
    |> Map.get(coords)
    |> elem(0)
  end

  def read_area_power(grid, coords) do
    grid
    |> Map.get(coords)
    |> elem(1)
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
    8141 |> ChronalCharge.solve()
end
