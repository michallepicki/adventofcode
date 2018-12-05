#!/usr/bin/env elixir
defmodule ReposeRecords do
  def solve(records_strings) do
    # accumulator:
    # { current_guard_id,
    #   asleep_at,
    #   %{guard_id =>
    #      { minutes_asleep,
    #        %{minute => days_asleep},
    #        { max_days_asleep, minute_candidate }
    #      },
    #   { max_minutes_asleep, guard_candidate },
    #   { max_days_asleep, guard_candidate }
    # }
    {_, _, records, {_, candidate}, {_, candidate2}} =
      Enum.reduce(
        records_strings,
        {nil, nil, %{}, {0, nil}, {0, nil}},
        &interpret_record_string/2
      )

    {_, _, {_, minute}} = Map.get(records, candidate)
    {_, _, {_, minute2}} = Map.get(records, candidate2)
    {candidate * minute, candidate2 * minute2}
  end

  def interpret_record_string(
        <<_prefix::bytes-size(17)>> <> "] Guard #" <> suffix,
        acc
      ) do
    {new_guard_id, _} = Integer.parse(suffix)
    put_elem(acc, 0, new_guard_id)
  end

  def interpret_record_string(
        <<_prefix::bytes-size(15)>> <> <<minute::bytes-size(2)>> <> "] falls asleep\n",
        acc
      ) do
    minute = String.to_integer(minute)
    put_elem(acc, 1, minute)
  end

  def interpret_record_string(
        <<_prefix::bytes-size(15)>> <> <<minute::bytes-size(2)>> <> "] wakes up\n",
        acc
      ) do
    {guard_id, asleep_at, records, {max_minutes_asleep, candidate}, {max_days_asleep, candidate2}} =
      acc

    minute = String.to_integer(minute)
    guard_record = Map.get(records, guard_id, {0, %{}, {0, nil}})

    updated_guard_record =
      {minutes_asleep, _, {days_asleep, _}} =
      Enum.reduce(
        asleep_at..(minute - 1),
        guard_record,
        fn minute, {minutes_asleep, minute_day_counts, {max_days_asleep, minute_candidate}} ->
          minute_day_counts = Map.update(minute_day_counts, minute, 1, &(&1 + 1))
          days = Map.get(minute_day_counts, minute)

          sleepiest_minute =
            case max(days, max_days_asleep) do
              ^max_days_asleep -> {max_days_asleep, minute_candidate}
              ^days -> {days, minute}
            end

          {minutes_asleep + 1, minute_day_counts, sleepiest_minute}
        end
      )

    strat1 =
      case max(minutes_asleep, max_minutes_asleep) do
        ^max_minutes_asleep -> {max_minutes_asleep, candidate}
        ^minutes_asleep -> {minutes_asleep, guard_id}
      end

    strat2 =
      case max(days_asleep, max_days_asleep) do
        ^max_days_asleep -> {max_days_asleep, candidate2}
        ^days_asleep -> {days_asleep, guard_id}
      end

    {guard_id, minute, Map.put(records, guard_id, updated_guard_record), strat1, strat2}
  end
end

"../4"
|> File.stream!()
|> Enum.sort()
|> ReposeRecords.solve()
|> IO.inspect()
