%%%-------------------------------------------------------------------
%% @doc particle supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(relocating_particle_sup).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(supervisor).

%% API
-export([
  start_child/3,
  start_children/2,
  start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_children(PidSup, Ctxs) ->
  Pids = lists:map(fun(Ctx) ->
      {ok, Pid} = start_child(PidSup, random_name(PidSup), Ctx),
      Pid
    end, Ctxs),
  {ok, Pids}.

start_child(PidSup, Name, Ctx) ->
  supervisor:start_child(
    PidSup, child(PidSup, Name, relocating_particle, Ctx)).

start_link() -> supervisor:start_link(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

%% Private functions

child(PidSup, Name, Module, Ctx) ->
  FullName = pid_to_list(PidSup) ++ "_" ++ Name,
  {FullName,
   {Module, start_link, [Ctx#{name => FullName}]},
   temporary, 1000, worker, [Module]
  }.

random_name(Namespace) ->
  uuid:to_string(uuid:uuid5(uuid:uuid4(), pid_to_list(Namespace))).
