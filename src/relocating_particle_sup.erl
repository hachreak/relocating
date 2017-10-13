%%%-------------------------------------------------------------------
%% @doc particle supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(relocating_particle_sup).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(supervisor).

%% API
-export([
  child/4,
  start_child/3,
  start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_child(PidSup, Name, Ctx) ->
  supervisor:start_child(
    PidSup, child(PidSup, Name, relocating_particle, Ctx)).

child(PidSup, Name, Module, Ctx) ->
  FullName = pid_to_list(PidSup) ++ "_" ++ Name,
  {FullName,
   {Module, start_link, [Ctx]},
   transient, 1000, worker, [Module]
  }.

start_link() ->
    supervisor:start_link(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.
