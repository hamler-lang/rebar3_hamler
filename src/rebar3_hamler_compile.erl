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
    ?LOG(info, "init hamler compiler", []),
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
    ?LOG(info, "validating hamler tools are installed", []),
    case validate_hamler_tools() of
        ok ->
            [ok = compile(P) || P <- rebar3_hamler:find_hamler_paths(State)],
            fetch_hamler_lang(State),
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
            case find_hamler_bin(PathV) of
                ok ->
                    case find_hamler_ebins() of
                        ok -> ok;
                        Error -> Error
                    end;
                Error -> Error
            end
    end.

find_hamler_bin(Paths) ->
    try
        [case filelib:find_file("hamler", Path) of
            {ok, _BinFile} -> throw(ok);
            _ -> not_found
        end || Path <- string:tokens(Paths, ": ")],
        {error, hamler_not_found_in_path}
    catch
        throw:ok -> ok
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
    ?LOG(info, "fetching hamler beams from installing dir: ~s, to target dir: ~s", [InstallDir, HamlerTarget]),
    ok = filelib:ensure_dir(filename:join([HamlerTarget, "ebin", "a"])),
    ok = filelib:ensure_dir(filename:join([HamlerTarget, "src", "a"])),
    [{ok, _} = file:copy(File, filename:join([HamlerTarget, "ebin", filename:basename(File)]))
     || File <- find_beam_files(InstallDir)],
    ok = rebar3_hamler_git_resource:create_app_src(HamlerTarget,
            #{name => "hamler", description => "Hamler Language",
              vsn => hamler_version(), applications => [kernel,stdlib,sasl]}),
    create_app(HamlerTarget).

compile(Path) ->
    ?LOG(info, "compiling ~s", [Path]),
    case exec_cmd(Path, "hamler build 2>&1") of
        {0, Result} ->
            ?LOG(debug, "~p~n", [Result]),
            ?LOG(debug, "~p built successfully", [Path]),
            create_app(Path);
        {Code, Result} ->
            ?LOG(error, "~p~n", [Result]),
            ?LOG(error, "~p build failed with exit code: ~p", [Path, Code])
    end.

hamler_version() ->
    case exec_cmd(".", "hamler --version") of
        {0, Version} ->
            ?LOG(info, "hamler version: ~p~n", [Version]),
            string:trim(Version);
        {Code, Result} ->
            ?LOG(error, "get hamler version failed: ~p~n", [{Code, Result}]),
            "unknown"
    end.

exec_cmd(Path, Command) ->
    Port = open_port({spawn, Command}, [{cd, Path}, stream, in, eof, hide, exit_status]),
    collect_cmd_result(Port, []).

collect_cmd_result(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            collect_cmd_result(Port, [Sofar|Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
              {Port, closed} -> ignore
            end,
            {collect_cmd_exit_code(Port), lists:flatten(Sofar)}
    end.

collect_cmd_exit_code(Port) ->
    receive
        {Port, {exit_status, Code}} -> Code
    end.

create_app(Path) ->
    ?LOG(info, "making *.app for ~p", [Path]),
    Appname = filename:basename(Path),
    AppSrcFile = filename:join([Path, "src", Appname++".app.src"]),
    case filelib:is_file(AppSrcFile) of
        true ->
            {ok, [{application,AName,AppParams}]} = file:consult(AppSrcFile),
            Mods = proplists:get_value(modules, AppParams) ++ get_beams(find_beam_files(Path)),
            AppContent = io_lib:format("~tp.~n", [
                {application, AName,
                lists:keyreplace(modules, 1, AppParams, {modules, Mods})}]),
            file:write_file(filename:join([Path, "ebin", Appname++".app"]), AppContent);
        false ->
            ?LOG(error, "cannot found file: ~p~n", [AppSrcFile]),
            erlang:error({enoent, AppSrcFile})
    end.

find_beam_files(Path) ->
    filelib:wildcard(filename:join([Path, "ebin", "*.beam"])).

get_beams(BeamFiles) ->
    [list_to_atom(filename:rootname(filename:basename(File))) || File <- BeamFiles].
