#!/usr/bin/env escript
-module(gear_ratios).
-mode(compile).
-export([main/1]).


-type state() :: new_number | {number, NumberAcc :: integer(), IsPartNumber :: boolean()}.


main(_) ->
  {ok, FileContents} = file:read_file("../03"),
  LineLength = find_line_length(FileContents, 1),
  erlang:display(part_one(new_number, 0, 0, LineLength, FileContents, [])).


-spec find_line_length(FileContents :: binary(), Acc :: integer()) -> integer().

find_line_length(<<$\n, _Rest/binary>>, Acc) -> Acc;
find_line_length(<<_, Rest/binary>>, Acc) -> find_line_length(Rest, Acc + 1).


-spec part_one(
  State :: state(),
  X :: integer(),
  Y :: integer(),
  LineLength :: integer(),
  FileContents :: binary(),
  Acc :: list(integer())
) -> Answer :: integer().

part_one(new_number, X, Y, LineLength, FileContents, Acc) ->
  case FileContents of
    <<_:((Y*LineLength+X)*8), Digit, _Rest/binary>> = FileContents when Digit >= $0 andalso Digit =< $9 ->
      IsPartNumber = check_if_part_number([{X - 1, Y - 1}, {X - 1, Y}, {X - 1, Y + 1}, {X, Y - 1}, {X, Y + 1}], false, FileContents, LineLength),
      part_one({number, Digit - $0, IsPartNumber}, X + 1, Y, LineLength, FileContents, Acc);
    <<_:((Y*LineLength+X)*8), $\n, _Rest/binary>> ->
      part_one(new_number, 0, Y + 1, LineLength, FileContents, Acc);
    <<_:((Y*LineLength+X)*8), _, _Rest/binary>> ->
      part_one(new_number, X + 1, Y, LineLength, FileContents, Acc);
    <<_:((Y*LineLength+X)*8)>> ->
      sum(Acc, 0)
  end;

part_one({number, NumberAcc, IsPartNumber}, X, Y, LineLength, FileContents, Acc) ->
  case FileContents of
    <<_:((Y*LineLength+X)*8), Digit, _Rest/binary>> = FileContents when Digit >= $0 andalso Digit =< $9 ->
      NewIsPartNumber = check_if_part_number([{X, Y - 1}, {X, Y + 1}], IsPartNumber, FileContents, LineLength),
      part_one({number, NumberAcc * 10 + Digit - $0, NewIsPartNumber}, X + 1, Y, LineLength, FileContents, Acc);
    <<_:((Y*LineLength+X)*8), $\n, _Rest/binary>> ->
      part_one(new_number, 0, Y + 1, LineLength, FileContents, prepend_if_part_number(IsPartNumber, NumberAcc, Acc));
    <<_:((Y*LineLength+X)*8), $., _Rest/binary>> ->
      NewIsPartNumber = check_if_part_number([{X, Y - 1}, {X, Y + 1}], IsPartNumber, FileContents, LineLength),
      part_one(new_number, X + 1, Y, LineLength, FileContents, prepend_if_part_number(NewIsPartNumber, NumberAcc, Acc));
    <<_:((Y*LineLength+X)*8), _, _Rest/binary>> ->
      part_one(new_number, X + 1, Y, LineLength, FileContents, prepend_if_part_number(true, NumberAcc, Acc))
  end.


check_if_part_number(_, true, _, _) ->
  true;

check_if_part_number([], false, _, _) ->
  false;

check_if_part_number([{X, Y} | Rest], false, FileContents, LineLength) ->
  case FileContents of
    <<_:((Y*LineLength+X)*8), Character, _Rest/binary>> when X >= 0 andalso Y >= 0 andalso Y =< LineLength andalso Character /= $. andalso Character /= $\n andalso not (Character >= $0 andalso Character =< $9) ->
      true;
    _ ->
      check_if_part_number(Rest, false, FileContents, LineLength)
  end.


prepend_if_part_number(true, Number, Acc) ->
  [Number | Acc];

prepend_if_part_number(_, _, Acc) ->
  Acc.

sum([N | Rest], Acc) ->
  sum(Rest, Acc + N);

sum([], Acc) ->
  Acc.
