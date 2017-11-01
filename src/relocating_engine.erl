%%%-------------------------------------------------------------------
%% @doc Engine
%% @end
%%%-------------------------------------------------------------------

-module(relocating_engine).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  log_move/2,
  log_update/2,
  run/4
]).

%%====================================================================
%% API
%%====================================================================

run(Name, Beacons, Quantity, Rounds) ->
  {ok, _} = relocating_logger:start_link(#{filename => Name}),
  MoveFun = fun(A,B,C,D,E) -> relocating_pso:move(A,B,C,D,E) end,
  FitnessFun = fun(A, B) -> relocating_pso:fitness(A,B) end,
  % run environment
  {ok, PidEnvSup} = relocating_env_sup:start_link(),
  {ok, PidEnv} = relocating_env_sup:start_child(
      PidEnvSup, Name, #{beacons => Beacons}),
  % run particles
  {ok, PidParSup} = relocating_particle_sup:start_link(),
  Ctxs = [#{move => MoveFun, fitness => FitnessFun, env => PidEnv,
            velocity => 2+Index,
            position => relocating_env:around_beacons(PidEnv, 10)}
          || Index <- lists:seq(1, Quantity)],
  {ok, Pids} = relocating_particle_sup:start_children(PidParSup, Ctxs),
  % run particles
  [relocating_particle:beat(Pid, 3, Rounds) || Pid <- Pids],
  PidEnv.

log_move(Fun, Args) ->
  Ctx = #{name := Name, position := Position} = Fun(Args),
  error_logger:info_msg("[~p] ~p", [Name, Position]),
  Ctx.

log_update(Fun, [EnvPid, NewPosition, _NewFitness]=Args) ->
  Ctx = Fun(Args),
  #{best := #{position := BestPosition, fitness := _BestFitness}}
    = relocating_env:debug(EnvPid, ctx),
  relocating_logger:log_msg("~p ~p ~p ~p", [
    erlang:monotonic_time(), self(), NewPosition, BestPosition
  ]),
  Ctx.

%%====================================================================
%% Internal functions
%%====================================================================
