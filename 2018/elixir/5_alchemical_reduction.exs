#!/usr/bin/env elixir
defmodule AlchemicalReduction do
  def solve(polymer_chain) do
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
    |> length()
  end
end

"../5"
|> File.read!()
|> String.trim()
|> String.graphemes()
|> AlchemicalReduction.solve()
|> IO.inspect()
