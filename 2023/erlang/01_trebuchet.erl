#!/usr/bin/env escript
-module(trebuchet).
-mode(compile).
-export([main/1]).

main(_) ->
  {ok, FileContents} = file:read_file("../1"),
  erlang:display(part_one(FileContents, new_line, 0)),
  erlang:display(part_two(FileContents, new_line, 0)).


-type state() :: new_line | complete_line_state().

-type complete_line_state() :: {FirstDigit :: integer(), LastDigit :: integer()}.


-spec part_one(
  FileContents :: binary(),
  State :: state(),
  Acc :: integer()
) -> Answer :: integer().

part_one(<<Digit, Rest/binary>>, State, Acc) when Digit >= $0 andalso Digit =< $9 ->
  part_one(Rest, update_state(State, Digit), Acc);

part_one(<<$\n, Rest/binary>>, State, Acc) ->
  part_one(Rest, new_line, update_acc(State, Acc));

part_one(<<_, Rest/binary>>, State, Acc) ->
  part_one(Rest, State, Acc);

part_one(<<>>, new_line, Acc) ->
  Acc.



-spec part_two(
  FileContents :: binary(),
  State :: state(),
  Acc :: integer()
) -> Answer :: integer().

part_two(<<Digit, Rest/binary>>, State, Acc) when Digit >= $0 andalso Digit =< $9 ->
  part_two(Rest, update_state(State, Digit), Acc);

part_two(<<"one", Rest/binary>>, State, Acc) ->
  part_two(<<"e", Rest/binary>>, update_state(State, $1), Acc);

part_two(<<"two", Rest/binary>>, State, Acc) ->
  part_two(<<"o", Rest/binary>>, update_state(State, $2), Acc);

part_two(<<"three", Rest/binary>>, State, Acc) ->
  part_two(<<"e", Rest/binary>>, update_state(State, $3), Acc);

part_two(<<"four", Rest/binary>>, State, Acc) ->
  part_two(Rest, update_state(State, $4), Acc);

part_two(<<"five", Rest/binary>>, State, Acc) ->
  part_two(<<"e", Rest/binary>>, update_state(State, $5), Acc);

part_two(<<"six", Rest/binary>>, State, Acc) ->
  part_two(Rest, update_state(State, $6), Acc);

part_two(<<"seven", Rest/binary>>, State, Acc) ->
  part_two(<<"n", Rest/binary>>, update_state(State, $7), Acc);

part_two(<<"eight", Rest/binary>>, State, Acc) ->
  part_two(<<"t", Rest/binary>>, update_state(State, $8), Acc);

part_two(<<"nine", Rest/binary>>, State, Acc) ->
  part_two(<<"e", Rest/binary>>, update_state(State, $9), Acc);

part_two(<<$\n, Rest/binary>>, State, Acc) ->
  part_two(Rest, new_line, update_acc(State, Acc));

part_two(<<_, Rest/binary>>, State, Acc) ->
  part_two(Rest, State, Acc);

part_two(<<>>, new_line, Acc) ->
  Acc.



-spec update_state(CurrentState :: state(), Digit :: integer()) -> NewState :: state().

update_state(new_line, Digit) ->
  {Digit, Digit};

update_state({FirstDigit, _LastDigit}, Digit) ->
  {FirstDigit, Digit}.



-spec update_acc(State :: complete_line_state(), CurrentAcc :: integer()) -> NewAcc :: integer().

update_acc({FirstDigit, LastDigit}, Acc) ->
  Acc + 10 * (FirstDigit - $0) + (LastDigit - $0).
