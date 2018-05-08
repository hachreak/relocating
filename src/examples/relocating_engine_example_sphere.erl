-module(relocating_engine_example_sphere).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  run/6
]).

%%====================================================================
%% API
%%====================================================================

run(Name, Dim, InitPosition, Quantity, Rounds, Period) ->
  MoveFun = fun(A,B,C,D,E) -> relocating_pso:move(A,B,C,D,E) end,
  FitnessFun = fun(A, B) -> relocating_opt_sphere:fitness(A,B) end,
  {ok, PidEnvSup} = relocating_env_sup:start_link(),
  {ok, PidEnv} = relocating_env_sup:start_child(
      PidEnvSup, Name, #{dimensions => Dim}, #{}),
  {ok, PidParSup} = relocating_particle_sup:start_link(),
  Ctxs = [#{move => MoveFun, fitness => FitnessFun, env => PidEnv,
            velocity => 2+Index, dimensions => Dim,
            position => relocating_opt_sphere:init_position(10, InitPosition)}
          || Index <- lists:seq(1, Quantity)],
  {ok, Pids} = relocating_particle_sup:start_children(PidParSup, Ctxs),
  [relocating_particle:beat(Pid, Period, Rounds) || Pid <- Pids],
  PidEnv.
