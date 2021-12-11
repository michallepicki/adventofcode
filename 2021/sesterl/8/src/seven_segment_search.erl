-module(seven_segment_search).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    % io:format("Args: ~p~n", [Args]),
    'SevenSegmentSearch.Main':main(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
