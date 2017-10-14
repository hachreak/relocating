%% Tests

-module(relocating_particle_sup_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

start_child_test() ->
  Beacons = [{{0,0,0}, 50}, {{100,100,100}, 50}, {{100,100,0}, 50}],

  {ok, PidEnvSup} = relocating_env_sup:start_link(),
  {ok, PidEnv} = relocating_env_sup:start_child(
      PidEnvSup, "1", #{beacons => Beacons}),
  {ok, PidParSup} = relocating_particle_sup:start_link(),
  {ok, PidP1} = relocating_particle_sup:start_child(
      PidParSup, "1", #{
        move => fun(A,B,C,D,E) -> relocating_pso:move(A,B,C,D,E) end,
        fitness => fun(A, B) -> relocating_pso:fitness(A,B) end,
        env=>PidEnv, velocity => 5
      }),

  ?assertEqual(Beacons, relocating_env:get_beacons(PidEnv)),
  ?assertEqual({0,0,0}, relocating_env:get_best(PidEnv)),
  lists:foreach(fun(_) -> relocating_particle:move(PidP1), timer:sleep(1) end,
                lists:seq(1, 1000)),

  {X, Y, Z} = relocating_env:get_best(PidEnv),
  ?assert((X > 50) and (X < 60)),
  ?assert((Y > 50) and (Y < 60)),
  ?assert((Z > 50) and (Z < 60)).
