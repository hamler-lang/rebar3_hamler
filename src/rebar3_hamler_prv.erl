-module(rebar3_hamler_compile).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, rebar3_hamler).
-define(DEPS, [app_discovery]).

-define(LOG(LEVEL, FORMAT, ARGS),
        rebar_api:LEVEL("[hamler] " ++ FORMAT, ARGS)).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 hamler compile"},   % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Find and compile hamler code"},
            {desc, "A rebar plugin that enables Erlang projects working along with "
                   "[Hamler](https://github.com/hamler-lang/hamler) code."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case validate_hamler_tools() of
        ok ->
            %% find all dirs that have an src sub-dir and contains .hm files in it
            Paths = lists:usort(
                [filename:dirname(filename:dirname(Dir))
                 || Dir <- filelib:wildcard("./**/src/*.hm")]),
            [compile(P) || P <- Paths],
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% validate the hamler and its tool-chain are ready on this system
validate_hamler_tools() ->
    case os:getenv("PATH") of
       false -> ?LOG(error, "Cannot get OS environment variable: $PATH", []);
       PathV ->
          try
              [case filelib:find_file("hamler", Path) of
                   {ok, _} -> throw(ok);
                   _ -> not_found
               end || Path <- string:tokens(PathV, ": ")],
               {error, hamler_not_found_in_path}
          catch
              throw:ok -> ok
          end
    end.

compile(Path) ->
   ?LOG(info, "compiling ~p", [Path]),
   case exec_cmd(Path, "hamler build 2>&1") of
     {0, Result} ->
        ?LOG(debug, "~p~n", [Result]),
        ?LOG(debug, "~p built successfully", [Path]);
     {Code, Result} ->
        ?LOG(error, "~p~n", [Result]),
        ?LOG(error, "~p build failed with exit code: ~p", [Path, Code])
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

