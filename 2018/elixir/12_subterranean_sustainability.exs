#! /usr/bin/env elixir
defmodule SubterraneanSustainability do
  def solve_a({initial, patterns}) do
    n_generations(20, {initial, patterns})
    |> count_sum()
  end

  # for b we have to observe that the sum is an arithmetic progression so we can calculate it

  def solve_b(data) do
    sum_five_hundred = n_generations(500, data) |> count_sum()
    diff = (n_generations(501, data) |> count_sum()) - sum_five_hundred
    (50000000000 - 500) * diff + sum_five_hundred
  end

  def count_sum({base_index, pattern}) do
    Enum.reduce(pattern, {base_index, 0}, fn
      ?#, {base_index, sum} -> {base_index + 1, sum + base_index}
      ?., {base_index, sum} -> {base_index + 1, sum}
    end)
    |> elem(1)
  end

  def n_generations(n, {initial, patterns}) do
    Enum.reduce(1..n, {0, initial}, &next_generation(&1, &2, patterns))
  end

  def next_generation(_ignore, {base_index, state}, patterns) do
    {_, reversed} = state |> Enum.reverse() |> fix_prefix()
    {added, state} = reversed |> Enum.reverse() |> fix_prefix()
    base_index = base_index - added
    {starting_point, rest} = Enum.split(state, 4)

    {new_generation_reversed, _} =
      Enum.reduce(rest, {'..', starting_point}, fn next_plant, {result, previous} ->
        pattern = previous ++ [next_plant]
        {[Map.get(patterns, pattern, ?.) | result], tl(pattern)}
      end)

    new_generation = new_generation_reversed |> Enum.reverse()
    {base_index, new_generation}
  end

  def fix_prefix([?# | _rest] = state), do: {4, [?., ?., ?., ?. | state]}
  def fix_prefix([?., ?# | _rest] = state), do: {3, [?., ?., ?. | state]}
  def fix_prefix([?., ?., ?# | _rest] = state), do: {2, [?., ?. | state]}
  def fix_prefix([?., ?., ?., ?# | _rest] = state), do: {1, [?. | state]}
  def fix_prefix(state), do: {0, state}

  def read_file(filename) do
    filename
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Enum.reduce(nil, &interpret_line/2)
  end

  def interpret_line("initial state: " <> initial, nil), do: {String.to_charlist(initial), %{}}
  def interpret_line("", acc), do: acc

  def interpret_line(<<pattern::bytes-size(5)>> <> " => " <> <<val::utf8>>, {i, patterns}),
    do: {i, Map.put(patterns, String.to_charlist(pattern), val)}
end

case System.argv() do
  ["--test"] ->
    ExUnit.start()

    defmodule SubterraneanSustainabilityTest do
      use ExUnit.Case

      @test_patterns %{
        '...##' => ?#,
        '..#..' => ?#,
        '.#...' => ?#,
        '.#.#.' => ?#,
        '.#.##' => ?#,
        '.##..' => ?#,
        '.####' => ?#,
        '#.#.#' => ?#,
        '#.###' => ?#,
        '##.#.' => ?#,
        '##.##' => ?#,
        '###..' => ?#,
        '###.#' => ?#,
        '####.' => ?#
      }

      test "next_generation" do
        assert {-4, '....#...#....#.....#..#..#..#..'} =
                 SubterraneanSustainability.next_generation(
                   1,
                   {0, '#..#.#..##......###...###'},
                   @test_patterns
                 )
      end

      test "20 generations" do
        assert {-6, '....#....##....#####...#######....#.#..##.'} =
                 SubterraneanSustainability.n_generations(
                   20,
                   {'#..#.#..##......###...###', @test_patterns}
                 )
      end

      test "part a" do
        assert 325 =
                 SubterraneanSustainability.solve_a({'#..#.#..##......###...###', @test_patterns})
      end
    end

  _ ->
    "../12"
    |> SubterraneanSustainability.read_file()
    |> SubterraneanSustainability.solve_a()
    |> IO.inspect()

    "../12"
    |> SubterraneanSustainability.read_file()
    |> SubterraneanSustainability.solve_b()
    |> IO.inspect()
end
