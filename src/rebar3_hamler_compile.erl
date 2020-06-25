-module(rebar3_hamler_compile).

-export([init/1, do/1, format_error/1]).

-include("include/rebar3_hamler.hrl").

-define(PROVIDER, compile).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> rebar_state:t().
init(State) ->
    ?LOG(debug, "init ~p", [?MODULE]),
    Provider = providers:create([
            {namespace, hamler},
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 hamler compile"},   % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Find and compile hamler code"},
            {desc, "Find and compile [Hamler](https://github.com/hamler-lang/hamler) code."}
    ]),
    rebar_state:add_provider(State, Provider).

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?LOG(info, "validating hamler toolchains", []),
    case validate_hamler_tools() of
        ok ->
            Paths = rebar3_hamler:find_hamler_paths(State),
            ?LOG(debug, "got paths of hamler projects: ~0p", [Paths]),
            [case Type of
                build_path -> ok = compile(P);
                _ -> ok = preproc_project(P)
             end || {Type, P} <- Paths],
            true = fetch_hamler_lang(State),
            {ok, State};
        {error, Reason} ->
            ?LOG(error, "validate hamler tools failed: ~p", [Reason]),
            {error, Reason}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% validate the hamler and its tool-chain are ready on this system
validate_hamler_tools() ->
    case os:getenv("PATH") of
        false ->
            {error, {os_env_not_found, "$PATH"}};
        PathV ->
            case rebar3_hamler:find_hamler_bin(PathV) of
                {ok, _} ->
                    case find_hamler_ebins() of
                        ok -> ok;
                        Error -> Error
                    end;
                Error -> Error
            end
    end.

find_hamler_ebins() ->
    InstallPath = os:getenv("HAMLER_INSTALL_DIR", ?HAMLER_INSTALL_DIR),
    case length(find_beam_files(InstallPath)) > 0 of
        true -> ok;
        false -> {error, hamler_ebins_not_found}
    end.

fetch_hamler_lang(State) ->
    HamlerTarget = filename:join([rebar_dir:deps_dir(State), "hamler"]),
    InstallDir = os:getenv("HAMLER_INSTALL_DIR", ?HAMLER_INSTALL_DIR),
    ?LOG(debug, "fetching hamler beams from installing dir: ~s, to target dir: ~s", [InstallDir, HamlerTarget]),
    ok = filelib:ensure_dir(filename:join([HamlerTarget, "ebin", "a"])),
    ok = filelib:ensure_dir(filename:join([HamlerTarget, "src", "a"])),
    [{ok, _} = file:copy(File, filename:join([HamlerTarget, "ebin", filename:basename(File)]))
     || File <- find_beam_files(InstallDir)],
    Version = hamler_version(),
    ?LOG(info, "settle hamler-~s into ~s~n", [Version, HamlerTarget]),
    ok = rebar3_hamler_git_resource:create_app_src(HamlerTarget,
            #{name => "hamler", description => "Hamler Language",
              vsn => Version, applications => [kernel,stdlib,sasl]}),
    create_app(HamlerTarget),
    code:add_path(filename:join([HamlerTarget, "ebin"])).

preproc_project(Path) ->
    ?LOG(debug, "preprocessing project: ~s", [Path]),
    Appname = filename:basename(Path),
    AppSrcFile = filename:join([Path, "src", Appname++".app.src"]),
    case filelib:is_file(AppSrcFile) of
        true -> ok;
        false ->
            ?LOG(warn, "cannot found file: ~s, creating a default one", [AppSrcFile]),
            rebar3_hamler_git_resource:create_app_src(Path, #{name => Appname})
    end.

compile(Path) ->
    ?LOG(info, "compiling ~s", [Path]),
    {ok, _} = rebar_utils:sh("mkdir -p ebin", [{cd, Path}]), %% tmp fix ebin cannot found
    {ok, Result} = rebar_utils:sh("hamler build 2>&1", [{cd, Path}]),
    ?LOG(debug, "~p~n", [Result]), %% show result in single line
    ?LOG(debug, "~s built successfully", [Path]),
    create_app(Path).

hamler_version() ->
    {ok, Version} = rebar_utils:sh("hamler --version", []),
    string:trim(Version).

create_app(Path) ->
    ?LOG(debug, "making *.app for ~s", [Path]),
    Appname = filename:basename(Path),
    AppSrcFile = filename:join([Path, "src", Appname++".app.src"]),
    case filelib:is_file(AppSrcFile) of
        true ->
            do_create_app(Path, Appname, AppSrcFile);
        false ->
            %?LOG(warn, "cannot found file: ~s, creating a default one", [AppSrcFile]),
            ?LOG(error, "cannot found file: ~s", [AppSrcFile]),
            error({enoent, AppSrcFile})
    end.

do_create_app(Path, Appname, AppSrcFile) ->
    {ok, [{application,AName,AppParams}]} = file:consult(AppSrcFile),
    Mods = get_beams(find_beam_files(Path)),
    Apps = [hamler],
    AppContent = io_lib:format("~tp.~n", [
        {application, AName, patch_appsrc_params(AppParams, Mods, Apps)}]),
    file:write_file(filename:join([Path, "ebin", Appname++".app"]), AppContent).

find_beam_files(Path) ->
    filelib:wildcard(filename:join([Path, "ebin", "*.beam"])).

get_beams(BeamFiles) ->
    [list_to_atom(filename:rootname(filename:basename(File))) || File <- BeamFiles].

patch_appsrc_params(AppParams, Mods, Apps) ->
    lists:keyreplace(modules, 1, AppParams, {modules, Mods}),
    OldApps = proplists:get_value(applications, AppParams),
    lists:keyreplace(applications, 1, AppParams, {applications, lists:usort(OldApps ++ Apps)}).
