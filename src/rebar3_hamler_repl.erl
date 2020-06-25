-module(rebar3_hamler_repl).

-export([init/1, do/1, format_error/1]).
-export([repl_cmd_args/2]).

-include("include/rebar3_hamler.hrl").

-define(PROVIDER, repl).
-define(DEPS, [{default, deps}, {default, compile}]).

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
            {example, "rebar3 hamler repl"},   % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Run Hamler REPL"},
            {desc, "Run [Hamler REPL](https://github.com/hamler-lang/hamler)"}
    ]),
    rebar_state:add_provider(State, Provider).

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ProjectPaths = lists:filtermap(
        fun ({build_path, Path}) -> {true, Path};
            ({_, _Path}) -> false
        end, rebar3_hamler:find_hamler_paths(State)),
    SrcPaths = [filename:join([Path, "src"]) || Path <- ProjectPaths],
    EBinPaths = rebar3_hamler:find_all_ebin_paths(State),
    {ok, HamlerPath} = rebar3_hamler:find_hamler_bin(os:getenv("PATH")),
    Args = repl_cmd_args(SrcPaths, EBinPaths),
    ?LOG(info, "execv_run: ~p", [{HamlerPath, Args}]),
    execv:run(HamlerPath, Args),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

repl_cmd_args(SrcPaths, EBinPaths) ->
    ["repl" | repl_args(SrcPaths, EBinPaths)].

repl_args([], []) ->
    [""];
repl_args([], EBinPaths) ->
    ["-e", add_cmd_path_opts(EBinPaths)];
repl_args(SrcPaths, []) ->
    ["-s", add_cmd_path_opts(SrcPaths)];
repl_args(SrcPaths, EBinPaths) ->
    ["-s", add_cmd_path_opts(SrcPaths), "-e", add_cmd_path_opts(EBinPaths)].

add_cmd_path_opts([Path]) -> Path;
add_cmd_path_opts([Path | Paths]) ->
    Path ++ ":" ++ add_cmd_path_opts(Paths).
