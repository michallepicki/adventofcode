-module(hydrothermal_venture).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    % io:format("Args: ~p~n", [Args]),
    'HydrothermalVenture.Main':main(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
