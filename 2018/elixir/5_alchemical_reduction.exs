#!/usr/bin/env elixir
defmodule AlchemicalReduction do
  def solve(polymer_chain) do
    initially_reduced = polymer_reduction(polymer_chain)
    part_1 = length(initially_reduced)
    alphabet = for n <- ?A..?Z, do: <<n::utf8>>

    part_2 =
      Enum.reduce(
        alphabet,
        part_1,
        fn letter, min_length ->
          min(
            min_length,
            initially_reduced
            |> Enum.reject(&(String.upcase(&1) == letter))
            |> polymer_reduction()
            |> length()
          )
        end
      )

    {part_1, part_2}
  end

  def polymer_reduction(polymer_chain) do
    polymer_chain
    |> Enum.reduce(
      [],
      fn
        next, [] ->
          [next]

        next, [last | tail] = acc ->
          case next != last && String.upcase(next) == String.upcase(last) do
            true -> tail
            false -> [next | acc]
          end
      end
    )
  end
end

"../5"
|> File.read!()
|> String.trim()
|> String.graphemes()
|> AlchemicalReduction.solve()
|> IO.inspect()
