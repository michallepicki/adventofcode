#!/usr/bin/env elixir
defmodule MarbleMania do
  def solve_a({players_count, latest_marble}) do
    marbles_circle = %{0 => {0, 0}}
    players = for i <- 1..players_count, into: %{}, do: {i, 0}
    curr_marble = 0
    curr_player = 1

    Enum.reduce(
      1..latest_marble,
      {marbles_circle, players, players_count, curr_marble, curr_player},
      &turn/2
    )
    |> elem(1)
    |> Enum.map(&elem(&1, 1))
    |> Enum.max()
  end

  def turn(new_marble, {marbles_circle, players, players_count, curr_marble, curr_player})
      when rem(new_marble, 23) == 0 do
    {one_counter_clockwise, _} = Map.get(marbles_circle, curr_marble)
    {two_counter_clockwise, _} = Map.get(marbles_circle, one_counter_clockwise)
    {three_counter_clockwise, _} = Map.get(marbles_circle, two_counter_clockwise)
    {four_counter_clockwise, _} = Map.get(marbles_circle, three_counter_clockwise)
    {five_counter_clockwise, _} = Map.get(marbles_circle, four_counter_clockwise)
    {six_counter_clockwise, _} = Map.get(marbles_circle, five_counter_clockwise)
    {seven_counter_clockwise, _} = Map.get(marbles_circle, six_counter_clockwise)
    {eight_counter_clockwise, _} = Map.get(marbles_circle, seven_counter_clockwise)

    updated_marbles_circle =
      marbles_circle
      |> Map.delete(seven_counter_clockwise)
      |> Map.update!(eight_counter_clockwise, fn {prev, _next} ->
        {prev, six_counter_clockwise}
      end)
      |> Map.update!(six_counter_clockwise, fn {_prev, next} ->
        {eight_counter_clockwise, next}
      end)

    new_curr_marble = six_counter_clockwise

    updated_players =
      Map.update!(players, curr_player, &(&1 + new_marble + seven_counter_clockwise))

    new_player =
      case curr_player + 1 do
        new_player when new_player > players_count -> 1
        new_player -> new_player
      end

    {updated_marbles_circle, updated_players, players_count, new_curr_marble, new_player}
  end

  def turn(new_marble, {marbles_circle, players, players_count, curr_marble, curr_player}) do
    {_, one_clockwise} = Map.get(marbles_circle, curr_marble)
    {_, two_clockwise} = Map.get(marbles_circle, one_clockwise)

    updated_marbles_circle =
      marbles_circle
      |> Map.put(new_marble, {one_clockwise, two_clockwise})
      |> Map.update!(one_clockwise, fn {prev, _next} -> {prev, new_marble} end)
      |> Map.update!(two_clockwise, fn {_prev, next} -> {new_marble, next} end)

    new_curr_marble = new_marble

    new_player =
      case curr_player + 1 do
        new_player when new_player > players_count -> 1
        new_player -> new_player
      end

    {updated_marbles_circle, players, players_count, new_curr_marble, new_player}
  end

  def solve_b({players_count, latest_marble}) do
    solve_a({players_count, latest_marble * 100})
  end

  def read_file(filename) do
    filename
    |> File.read!()
    |> String.trim()
    |> String.split(" ")
    |> (fn [players, _, _, _, _, _, latest_marble, _] ->
          {String.to_integer(players), String.to_integer(latest_marble)}
        end).()
  end
end

case System.argv() do
  ["--test"] ->
    ExUnit.start()

    defmodule MarbleManiaTest do
      use ExUnit.Case

      test "part a" do
        assert 8317 = MarbleMania.solve_a({10, 1618})
      end
    end

  _ ->
    "../9" |> MarbleMania.read_file() |> MarbleMania.solve_a() |> IO.inspect()
    "../9" |> MarbleMania.read_file() |> MarbleMania.solve_b() |> IO.inspect()
end
