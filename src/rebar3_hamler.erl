-module(rebar3_hamler).

-export([init/1, find_hamler_paths/1]).

-define(LOG(LEVEL, FORMAT, ARGS),
        rebar_api:LEVEL("[hamler] " ++ FORMAT, ARGS)).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, rebar3_hamler_compile:init(
            rebar_state:add_resource(State, {{hamler,git}, rebar3_hamler_git_resource}))}.

find_hamler_paths(State) ->
    %% find all dirs that have an src sub-dir and contains .hm files in it
    SearchPaths = [src_path(State),
                   deps_path(State),
                   apps_path(State),
                   lib_path(State)],
    ?LOG(debug, "find hamler path in ~p", [SearchPaths]),
    HmDirs = lists:append([filelib:wildcard(Dir) || Dir <- SearchPaths]),
    lists:usort([filename:dirname(filename:dirname(Dir))
                 || Dir <- HmDirs]).

src_path(State) ->
    filename:join([rebar_dir:root_dir(State), "src", "*.hm"]).

deps_path(State) ->
    filename:join([rebar_dir:deps_dir(State), "*", "src", "*.hm"]).

apps_path(State) ->
    filename:join([rebar_dir:root_dir(State), "apps", "*", "src", "*.hm"]).

lib_path(State) ->
    filename:join([rebar_dir:root_dir(State), "lib", "*", "src", "*.hm"]).

