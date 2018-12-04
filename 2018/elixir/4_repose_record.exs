#!/usr/bin/env elixir
defmodule ReposeRecords do
  def solve(records_strings) do
    # accumulator:
    # { guard_id,
    #   asleep_at,
    #   %{guard_id =>
    #      { minutes_asleep,
    #        %{minute => days_asleep},
    #        max_so_far,
    #        minute_candidate },
    #   max_so_far,
    #   guard_candidate }
    {_, _, records, _, candidate} =
      Enum.reduce(records_strings, {nil, nil, %{}, 0, nil}, &interpret_record_string/2)

    {_, _, _, minute} = Map.get(records, candidate)
    candidate * minute
  end

  def interpret_record_string(
        <<_prefix::bytes-size(17)>> <> "] Guard #" <> suffix,
        acc
      ) do
    {_, _, records, max_so_far, candidate} = acc
    {new_guard_id, _} = Integer.parse(suffix)
    {new_guard_id, nil, records, max_so_far, candidate}
  end

  def interpret_record_string(
        <<_prefix::bytes-size(15)>> <> <<minute::bytes-size(2)>> <> "] falls asleep",
        acc
      ) do
    {guard_id, _, records, max_so_far, candidate} = acc
    minute = String.to_integer(minute)
    {guard_id, minute, records, max_so_far, candidate}
  end

  def interpret_record_string(
        <<_prefix::bytes-size(15)>> <> <<minute::bytes-size(2)>> <> "] wakes up",
        acc
      ) do
    {guard_id, asleep_at, records, max_so_far, candidate} = acc
    minute = String.to_integer(minute)
    guards_records = Map.get(records, guard_id, {0, %{}, 0, nil})

    {count, _, _, _} =
      updated_guards_records =
      Enum.reduce(
        asleep_at..(minute - 1),
        guards_records,
        fn minute, {asleep_count, minute_day_counts, max_so_far, candidate} ->
          minute_day_counts = Map.update(minute_day_counts, minute, 1, &(&1 + 1))
          count = Map.get(minute_day_counts, minute)

          {new_max, new_candidate} =
            case max(count, max_so_far) do
              ^max_so_far -> {max_so_far, candidate}
              ^count -> {count, minute}
            end

          {asleep_count + 1, minute_day_counts, new_max, new_candidate}
        end
      )

    {new_max, new_candidate} =
      case max(count, max_so_far) do
        ^max_so_far -> {max_so_far, candidate}
        ^count -> {count, guard_id}
      end

    {guard_id, minute, Map.put(records, guard_id, updated_guards_records), new_max, new_candidate}
  end

  def read_file(filename) do
    filename
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Enum.into([])
    |> Enum.sort()
  end
end

"../4"
|> ReposeRecords.read_file()
|> Enum.to_list()
|> ReposeRecords.solve()
|> IO.puts()
