-module(rebar3_hamler).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_hamler_compile:init(State),
    {ok, State1}.
