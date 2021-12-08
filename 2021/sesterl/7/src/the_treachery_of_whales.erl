-module(the_treachery_of_whales).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    % io:format("Args: ~p~n", [Args]),
    'TheTreacheryOfWhales.Main':main(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
