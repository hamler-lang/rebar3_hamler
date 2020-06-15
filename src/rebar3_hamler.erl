-module(rebar3_hamler).

-export([init/1, find_hamler_paths/0]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, rebar3_hamler_compile:init(
            rebar_state:add_resource(State, {{hamler,git}, rebar3_hamler_git_resource}))}.

find_hamler_paths() ->
    %% find all dirs that have an src sub-dir and contains .hm files in it
    lists:usort(
        [filename:dirname(filename:dirname(Dir))
            || Dir <- filelib:wildcard("./**/src/*.hm")]).