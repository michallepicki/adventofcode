#!/usr/bin/env escript
-module(gear_ratios).
-mode(compile).
-export([main/1]).

-type state1() ::
    new_number
    | {number, NumberAcc :: integer(), IsPartNumber :: boolean()}.

-type state2() ::
    new_number
    | {number, NumberAcc :: integer(), SurroundingStarCoordinates :: list({integer(), integer()})}.

main(_) ->
    {ok, FileContents} = file:read_file("../03"),
    LineLength = find_line_length(FileContents, 1),
    erlang:display(part_one(new_number, 0, 0, LineLength, FileContents, [])),
    erlang:display(part_two(new_number, 0, 0, LineLength, FileContents, #{})).

-spec find_line_length(FileContents :: binary(), Acc :: integer()) -> integer().

find_line_length(<<$\n, _Rest/binary>>, Acc) -> Acc;
find_line_length(<<_, Rest/binary>>, Acc) -> find_line_length(Rest, Acc + 1).

-spec part_one(
    State :: state1(),
    X :: integer(),
    Y :: integer(),
    LineLength :: integer(),
    FileContents :: binary(),
    Acc :: list(integer())
) -> Answer :: integer().

part_one(new_number, X, Y, LineLength, FileContents, Acc) ->
    Offset = ((Y * LineLength + X) * 8),
    case FileContents of
        <<_:Offset, Digit, _/binary>> = FileContents when Digit >= $0 andalso Digit =< $9 ->
            CoordinatesToCheck = [
                {X - 1, Y - 1}, {X - 1, Y}, {X - 1, Y + 1}, {X, Y - 1}, {X, Y + 1}
            ],
            IsPartNumber = check_if_part_number(
                CoordinatesToCheck, false, FileContents, LineLength
            ),
            part_one({number, Digit - $0, IsPartNumber}, X + 1, Y, LineLength, FileContents, Acc);
        <<_:Offset, $\n, _/binary>> ->
            part_one(new_number, 0, Y + 1, LineLength, FileContents, Acc);
        <<_:Offset, _, _/binary>> ->
            part_one(new_number, X + 1, Y, LineLength, FileContents, Acc);
        <<_:Offset>> ->
            sum(Acc, 0)
    end;
part_one({number, NumberAcc, IsPartNumber}, X, Y, LineLength, FileContents, Acc) ->
    Offset = ((Y * LineLength + X) * 8),
    case FileContents of
        <<_:Offset, Digit, _/binary>> = FileContents when Digit >= $0 andalso Digit =< $9 ->
            NewIsPartNumber = check_if_part_number(
                [{X, Y - 1}, {X, Y + 1}], IsPartNumber, FileContents, LineLength
            ),
            NewState = {number, NumberAcc * 10 + Digit - $0, NewIsPartNumber},
            part_one(NewState, X + 1, Y, LineLength, FileContents, Acc);
        <<_:Offset, $\n, _/binary>> ->
            NewAcc = prepend_if_part_number(IsPartNumber, NumberAcc, Acc),
            part_one(new_number, 0, Y + 1, LineLength, FileContents, NewAcc);
        <<_:Offset, _, _/binary>> ->
            NewIsPartNumber = check_if_part_number(
                [{X, Y - 1}, {X, Y}, {X, Y + 1}], IsPartNumber, FileContents, LineLength
            ),
            NewAcc = prepend_if_part_number(NewIsPartNumber, NumberAcc, Acc),
            part_one(new_number, X + 1, Y, LineLength, FileContents, NewAcc)
    end.

-spec check_if_part_number(
    CoordinatesToCheck :: list({integer(), integer()}),
    IsPartNumber :: boolean(),
    FileContents :: binary(),
    LineLength :: integer()
) -> boolean().

check_if_part_number(_, true, _, _) ->
    true;
check_if_part_number([], false, _, _) ->
    false;
check_if_part_number([{X, Y} | Rest], false, FileContents, LineLength) ->
    Offset = ((Y * LineLength + X) * 8),
    case FileContents of
        <<_:Offset, Character, _/binary>> when
            X >= 0 andalso Y >= 0 andalso Y =< LineLength andalso Character /= $. andalso
                Character /= $\n andalso not (Character >= $0 andalso Character =< $9)
        ->
            true;
        _ ->
            check_if_part_number(Rest, false, FileContents, LineLength)
    end.

-spec prepend_if_part_number(
    IsPartNumber :: boolean(),
    Number :: integer(),
    Acc :: list(integer())
) -> list(integer()).

prepend_if_part_number(true, Number, Acc) ->
    [Number | Acc];
prepend_if_part_number(_, _, Acc) ->
    Acc.

-spec sum(list(integer()), integer()) -> integer().

sum([N | Rest], Acc) ->
    sum(Rest, Acc + N);
sum([], Acc) ->
    Acc.

-spec part_two(
    State :: state2(),
    X :: integer(),
    Y :: integer(),
    LineLength :: integer(),
    FileContents :: binary(),
    Acc :: #{StarCoordinate :: {integer(), integer()} => SurroundingNumbers :: list(integer())}
) -> Answer :: integer().

part_two(new_number, X, Y, LineLength, FileContents, Acc) ->
    Offset = ((Y * LineLength + X) * 8),
    case FileContents of
        <<_:Offset, Digit, _/binary>> = FileContents when
            Digit >= $0 andalso Digit =< $9
        ->
            CoordinatesToCheck = [
                {X - 1, Y - 1}, {X - 1, Y}, {X - 1, Y + 1}, {X, Y - 1}, {X, Y + 1}
            ],
            StarCoordinates = check_surroundings_for_stars(
                CoordinatesToCheck, [], FileContents, LineLength
            ),
            NewState = {number, Digit - $0, StarCoordinates},
            part_two(NewState, X + 1, Y, LineLength, FileContents, Acc);
        <<_:Offset, $\n, _/binary>> ->
            part_two(new_number, 0, Y + 1, LineLength, FileContents, Acc);
        <<_:Offset, _, _/binary>> ->
            part_two(new_number, X + 1, Y, LineLength, FileContents, Acc);
        <<_:Offset>> ->
            Gears = maps:filtermap(fun check_if_gear/2, Acc),
            GearRatios = maps:values(Gears),
            sum(GearRatios, 0)
    end;
part_two({number, NumberAcc, StarCoordinates}, X, Y, LineLength, FileContents, Acc) ->
    Offset = ((Y * LineLength + X) * 8),
    case FileContents of
        <<_:Offset, Digit, _/binary>> = FileContents when Digit >= $0 andalso Digit =< $9 ->
            CoordinatesToCheck = [{X, Y - 1}, {X, Y + 1}],
            NewStarCoordinates = check_surroundings_for_stars(
                CoordinatesToCheck, StarCoordinates, FileContents, LineLength
            ),
            NewState = {number, NumberAcc * 10 + Digit - $0, NewStarCoordinates},
            part_two(NewState, X + 1, Y, LineLength, FileContents, Acc);
        <<_:Offset, $\n, _/binary>> ->
            NewAcc = add_number_to_stars(StarCoordinates, NumberAcc, Acc),
            part_two(new_number, 0, Y + 1, LineLength, FileContents, NewAcc);
        <<_:Offset, _, _/binary>> ->
            CoordinatesToCheck = [{X, Y - 1}, {X, Y}, {X, Y + 1}],
            NewStarCoordinates = check_surroundings_for_stars(
                CoordinatesToCheck, StarCoordinates, FileContents, LineLength
            ),
            NewAcc = add_number_to_stars(NewStarCoordinates, NumberAcc, Acc),
            part_two(new_number, X + 1, Y, LineLength, FileContents, NewAcc)
    end.

-spec check_surroundings_for_stars(
    CoordinatesToCheck :: list({integer(), integer()}),
    AlreadyKnownStarCoordinates :: list({integer(), integer()}),
    FileContents :: binary(),
    LineLength :: integer()
) -> StarCoordinates :: list({integer(), integer()}).

check_surroundings_for_stars([], StarCoordinates, _, _) ->
    StarCoordinates;
check_surroundings_for_stars([{X, Y} | Rest], StarCoordinates, FileContents, LineLength) ->
    Offset = ((Y * LineLength + X) * 8),
    case FileContents of
        <<_:Offset, $*, _/binary>> ->
            check_surroundings_for_stars(
                Rest, [{X, Y} | StarCoordinates], FileContents, LineLength
            );
        _ ->
            check_surroundings_for_stars(Rest, StarCoordinates, FileContents, LineLength)
    end.

-spec add_number_to_stars(
    StarCoordinates :: list({integer(), integer()}),
    Number :: integer(),
    Acc
) -> Acc when
    Acc :: #{StarCoordinate :: {integer(), integer()} => SurroundingNumbers :: list(integer())}.

add_number_to_stars([], _, Acc) ->
    Acc;
add_number_to_stars([{X, Y} | Rest], Number, Acc) ->
    NewAcc = maps:update_with({X, Y}, fun(Numbers) -> [Number | Numbers] end, [Number], Acc),
    add_number_to_stars(Rest, Number, NewAcc).

-spec check_if_gear(
    StarCoordinate :: {integer(), integer()},
    SurroundingNumbers :: list(integer)
) -> {true, integer()} | false.

check_if_gear(_, [N1, N2]) ->
    {true, N1 * N2};
check_if_gear(_, _) ->
    false.
