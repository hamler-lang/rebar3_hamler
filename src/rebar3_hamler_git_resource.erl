-module(rebar3_hamler_git_resource).

-export([init/2,
         lock/2,
         download/4, download/3,
         needs_update/2,
         make_vsn/2]).

-define(LOG(LEVEL, FORMAT, ARGS),
        rebar_api:LEVEL("[hamler] " ++ FORMAT, ARGS)).

init(Type, _RebarState) ->
   Resource = rebar_resource_v2:new(
       Type,    % type tag such as 'git' or 'hg'
       ?MODULE, % this callback module
       #{}      % anything you want to carry around for next calls
   ),
   {ok, Resource}.

lock(AppInfo, CustomState) ->
  rebar_git_resource:lock(strip_source_tag(AppInfo), CustomState).

download(TmpDir, AppInfo, CustomState, RebarState) ->
  Result = rebar_git_resource:download(TmpDir, strip_source_tag(AppInfo), CustomState, RebarState),
  create_app(TmpDir, rebar_app_info:name(AppInfo)),
  Result.

%% For backward compatibilty
download(Dir, AppInfo, State) ->
  Result = rebar_git_resource:download(Dir, strip_source_tag(AppInfo), State),
  create_app(Dir, rebar_app_info:name(AppInfo)),
  Result.

make_vsn(Dir, ResourceState) ->
  rebar_git_resource:make_vsn(Dir, ResourceState).

needs_update(AppInfo, ResourceState) ->
  rebar_git_resource:needs_update(strip_source_tag(AppInfo), ResourceState).

%%====================================================================
%% Internal functions
%%====================================================================
create_app(Path, Name) ->
    Appname = str(Name),
    AppSrcFile = filename:join([Path, "src", Appname++".app.src"]),
    case filelib:is_file(AppSrcFile) of
        true -> {ok, AppSrcFile};
        false ->
            ?LOG(info, "creating *.app.src for ~p", [Path]),
            file:write_file(AppSrcFile, dummy_app_src(Appname))
    end.

dummy_app_src(Appname) ->
    io_lib:format("{application, ~s,
    [{description, \"~s\"},
     {vsn, \"git\"},
     {registered, []},
     {applications,
     [kernel,
     stdlib,
     sasl
     ]},
     {env,[]},
     {modules, []},
     {licenses, [\"Apache 2.0\"]},
     {links, []}
    ]}.", [Appname, Appname]).

strip_source_tag(AppInfo) ->
    rebar_app_info:source(AppInfo, strip_hamler(rebar_app_info:source(AppInfo))).

strip_hamler({{hamler,git}, Url}) ->
    {git, Url};
strip_hamler({{hamler,git}, Url, Branch}) ->
    {git, Url, Branch}.

str(Str) when is_list(Str) -> Str;
str(Bin) when is_binary(Bin) -> binary_to_list(Bin);
str(Atom) when is_atom(Atom) -> atom_to_list(Atom).
