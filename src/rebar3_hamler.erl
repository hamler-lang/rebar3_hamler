-module(rebar3_hamler).

-export([ init/1
        , find_hamler_paths/1
        , absolute_deps_path/1
        , find_hamler_bin/1
        , find_all_ebin_paths/1
        ]).

-include("include/rebar3_hamler.hrl").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Resource = {hamler_git, rebar3_hamler_git_resource},
    {ok, rebar3_hamler_repl:init(
            rebar3_hamler_compile:init(
                rebar_state:add_resource(State, Resource)))}.

find_hamler_paths(State) ->
    %% find all dirs that have an src sub-dir and contains .hm files in it
    SearchPaths = [src_path(State),
                   deps_path(State),
                   apps_path(State),
                   lib_path(State)],
    ?LOG(debug, "find hamler path in ~p", [SearchPaths]),
    HmDirs = lists:append([filelib:wildcard(Dir) || Dir <- SearchPaths]),
    usort_project_paths([extract_project_path(Dir) || Dir <- HmDirs],
        absolute_deps_path(State)).

absolute_deps_path(State) ->
    filename:absname(rebar_dir:deps_dir(State)).

-spec find_hamler_bin(list()) -> {ok, string()} | {error, term()}.
find_hamler_bin(Paths) ->
    try
        [case filelib:find_file("hamler", Path) of
            {ok, BinFile} -> throw({found, BinFile});
            _ -> not_found
         end || Path <- string:tokens(Paths, ": ")],
        {error, hamler_not_found_in_path}
    catch
        throw:{found, BinFile} -> {ok, BinFile}
    end.

find_all_ebin_paths(State) ->
    filelib:wildcard(filename:join([absolute_deps_path(State), "**", "ebin"])).

%% ================================================================
%% Interal functions
%% ================================================================

src_path(State) ->
    filename:join([rebar_dir:root_dir(State), "src", "**", "*.hm"]).

deps_path(State) ->
    filename:join([absolute_deps_path(State), "*", "src", "**", "*.hm"]).

apps_path(State) ->
    filename:join([rebar_dir:root_dir(State), "apps", "*", "src", "**", "*.hm"]).

lib_path(State) ->
    filename:join([rebar_dir:root_dir(State), "lib", "*", "src", "**", "*.hm"]).

%% the project dir is the parent dir of "src"
extract_project_path(Dir) ->
    Basename = filename:basename(Dir),
    extract_project_path(Dir, Basename).

extract_project_path(Dir, "src") ->
    filename:dirname(Dir);
extract_project_path(Dir, _Basename) ->
    extract_project_path(filename:dirname(Dir)).

usort_project_paths(Paths, DepsPath) ->
    [{project_path_type(P, DepsPath), P} || P <- lists:usort(Paths)].

project_path_type(Path, DepsPath) ->
    case string:prefix(Path, DepsPath) of
        nomatch -> lib_path;
        _ -> build_path
    end.
