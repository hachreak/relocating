%%%-------------------------------------------------------------------
%% @doc environment supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(relocating_env_sup).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(supervisor).

%% API
-export([
  child/4,
  start_child/4,
  start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_child(PidSup, Name, Ctx, ParticleCtx) ->
  supervisor:start_child(
    PidSup, child(PidSup, Name, relocating_env,
                  relocating_env:setup(Ctx, ParticleCtx))).

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
