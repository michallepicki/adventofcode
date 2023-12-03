#!/usr/bin/env escript
-module(cube_conundrum).
-mode(compile).
-export([main/1]).

main(_) ->
  {ok, FileContents} = file:read_file("../02"),
  erlang:display(part_one(FileContents, new_line, 1, 0)),
  erlang:display(part_two(FileContents, new_line, 1, 0)).


-type state1() :: new_line | {game, ColorAcc :: integer(), Possible :: boolean()}.


-spec part_one(
  FileContents :: binary(),
  State :: state1(),
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
  NewAcc = update_acc1(Possible, LineNumber, Acc),
  part_one(Rest, new_line, LineNumber + 1, NewAcc);

part_one(<<_, Rest/binary>>, {game, _ColorAcc, Possible}, LineNumber, Acc) ->
  part_one(Rest, {game, 0, Possible}, LineNumber, Acc);

part_one(<<>>, _State, _LineNumber, Acc) ->
  Acc.


-spec update_acc1(
  Possible :: boolean(),
  LineNumber :: integer(),
  CurrentAcc :: integer()
) -> NewAcc :: integer().

update_acc1(true, LineNumber, Acc) ->
  Acc + LineNumber;

update_acc1(false, _LineNumber, Acc) ->
  Acc.



-type state2() :: new_line | {game, ColorAcc :: integer(), MaxValues :: {integer(), integer(), integer()}}.


-spec part_two(
  FileContents :: binary(),
  State :: state2(),
  LineNumber :: integer(),
  Acc :: integer()
) -> Answer :: integer().

part_two(<<": ", Rest/binary>>, new_line, LineNumber, Acc) ->
  part_two(Rest, {game, 0, {0, 0, 0}}, LineNumber, Acc);

part_two(<<_, Rest/binary>>, new_line = State, LineNumber, Acc) ->
  part_two(Rest, State, LineNumber, Acc);

part_two(<<Digit, Rest/binary>>, {game, ColorAcc, MaxValues}, LineNumber, Acc) when Digit >= $0 andalso Digit =< $9 ->
  part_two(Rest, {game, ColorAcc * 10 + Digit - $0, MaxValues}, LineNumber, Acc);

part_two(<<" red", Rest/binary>>, {game, ColorAcc, {R, G, B}}, LineNumber, Acc) ->
  MaxValues = {max(R, ColorAcc), G, B},
  part_two(Rest, {game, 0, MaxValues}, LineNumber, Acc);

part_two(<<" green", Rest/binary>>, {game, ColorAcc, {R, G, B}}, LineNumber, Acc) ->
  MaxValues = {R, max(G, ColorAcc), B},
  part_two(Rest, {game, 0, MaxValues}, LineNumber, Acc);

part_two(<<" blue", Rest/binary>>, {game, ColorAcc, {R, G, B}}, LineNumber, Acc) ->
  MaxValues = {R, G, max(B, ColorAcc)},
  part_two(Rest, {game, 0, MaxValues}, LineNumber, Acc);

part_two(<<$\n, Rest/binary>>, {game, _ColorAcc, {R, G, B}}, LineNumber, Acc) ->
  part_two(Rest, new_line, LineNumber + 1, Acc + R * G * B);

part_two(<<_, Rest/binary>>, {game, _ColorAcc, MaxValues}, LineNumber, Acc) ->
  part_two(Rest, {game, 0, MaxValues}, LineNumber, Acc);

part_two(<<>>, _State, _LineNumber, Acc) ->
  Acc.
