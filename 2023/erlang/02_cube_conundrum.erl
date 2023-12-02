#!/usr/bin/env escript
-module(trebuchet).
-mode(compile).
-export([main/1]).

main(_) ->
  {ok, FileContents} = file:read_file("../02"),
  erlang:display(part_one(FileContents, new_line, 1, 0)).


-type state() :: new_line | {game, ColorAcc :: integer(), Possible :: boolean()}.


-spec part_one(
  FileContents :: binary(),
  State :: state(),
  LineNumber :: integer(),
  Acc :: integer()
) -> Answer :: integer().

part_one(<<": ", Rest/binary>>, new_line, LineNumber, Acc) ->
  part_one(Rest, {game, 0, true}, LineNumber, Acc);

part_one(<<_, Rest/binary>>, new_line = State, LineNumber, Acc) ->
  part_one(Rest, State, LineNumber, Acc);

part_one(<<Digit, Rest/binary>>, {game, ColorAcc, Possible}, LineNumber, Acc) when Digit >= $0 andalso Digit =< $9 ->
  part_one(Rest, {game, ColorAcc * 10 + Digit - $0, Possible}, LineNumber, Acc);

part_one(<<" red", Rest/binary>>, {game, ColorAcc, Possible}, LineNumber, Acc) ->
  NewPossible = Possible andalso ColorAcc =< 12,
  part_one(Rest, {game, 0, NewPossible}, LineNumber, Acc);

part_one(<<" green", Rest/binary>>, {game, ColorAcc, Possible}, LineNumber, Acc) ->
  NewPossible = Possible andalso ColorAcc =< 13,
  part_one(Rest, {game, 0, NewPossible}, LineNumber, Acc);

part_one(<<" blue", Rest/binary>>, {game, ColorAcc, Possible}, LineNumber, Acc) ->
  NewPossible = Possible andalso ColorAcc =< 14,
  part_one(Rest, {game, 0, NewPossible}, LineNumber, Acc);

part_one(<<$\n, Rest/binary>>, {game, _ColorAcc, Possible}, LineNumber, Acc) ->
  NewAcc = update_acc(Possible, LineNumber, Acc),
  part_one(Rest, new_line, LineNumber + 1, NewAcc);

part_one(<<_, Rest/binary>>, {game, _ColorAcc, Possible}, LineNumber, Acc) ->
  part_one(Rest, {game, 0, Possible}, LineNumber, Acc);

part_one(<<>>, _State, _LineNumber, Acc) ->
  Acc.


-spec update_acc(
  Possible :: boolean(),
  LineNumber :: integer(),
  CurrentAcc :: integer()
) -> NewAcc :: integer().

update_acc(true, LineNumber, Acc) ->
  Acc + LineNumber;

update_acc(false, _LineNumber, Acc) ->
  Acc.
