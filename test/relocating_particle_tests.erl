%% Tests

-module(relocating_particle_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

env() ->
  Beacons = [{{0,0,0}, 10}, {{5,5,5}, 7}],
  {ok, PidEnv} = relocating_env:start_link(#{beacons => Beacons}),
  {PidEnv, Beacons}.

move_test() ->
  % run env
  {PidEnv, _} = env(),
  % run particle
  MoveFun = fun({X,Y,Z}, _, _, _, _) ->
    {X+1,Y,Z}
  end,
  FitnessFun = fun({X,_,_}, _) ->
    case X rem 2 of
      1 -> 10 - X;
      0 -> 10 + X
    end
  end,
  {ok, PidPar} = relocating_particle:start_link(#{
    position => {0,0,0}, velocity => 1,
    move => MoveFun, fitness => FitnessFun, env => PidEnv
  }),

  % move and find new best fitness (default fitness = -1)
  relocating_particle:move(PidPar),
  Ctx = relocating_particle:debug(PidPar, ctx),
  ?assertEqual({1,0,0}, maps:get(position, Ctx)),
  ?assertEqual(#{position => {1,0,0}, fitness => 9},
               maps:get(best, Ctx)),
  ?assertEqual({1,0,0}, relocating_env:get_best(PidEnv)),

  % move and don't find a new best for fitness
  relocating_particle:move(PidPar),
  Ctx2 = relocating_particle:debug(PidPar, ctx),
  ?assertEqual({2,0,0}, maps:get(position, Ctx2)),
  ?assertEqual(#{position => {1,0,0}, fitness => 9},
               maps:get(best, Ctx2)),
  ?assertEqual({1,0,0}, relocating_env:get_best(PidEnv)),

  % move and find new best fitness
  relocating_particle:move(PidPar),
  Ctx3 = relocating_particle:debug(PidPar, ctx),
  ?assertEqual({3,0,0}, maps:get(position, Ctx3)),
  ?assertEqual(#{position => {3,0,0}, fitness => 7},
               maps:get(best, Ctx3)),
  ?assertEqual({3,0,0}, relocating_env:get_best(PidEnv)),

  % compute with one beat (after 1 sec)!
  relocating_particle:beat(PidPar, 1, 1),
  timer:sleep(1500),
  Ctx4 = relocating_particle:debug(PidPar, ctx),
  ?assertEqual({4,0,0}, maps:get(position, Ctx4)),
  ?assertEqual(#{position => {3,0,0}, fitness => 7},
               maps:get(best, Ctx4)),
  ?assertEqual({3,0,0}, relocating_env:get_best(PidEnv)),

  ok.

update_test() ->
  {PidEnv, _} = env(),
  % run particle
  {ok, PidPar} = relocating_particle:start_link(#{
    position => {0,0,0}, velocity => 1,
    move => fun() -> ok end, fitness => fun() -> ok end, env => PidEnv
  }),

  ?assertEqual(1, maps:get(velocity, relocating_particle:debug(PidPar, ctx))),
  relocating_particle:update(PidPar, #{velocity => 5}),
  ?assertEqual(5, maps:get(velocity, relocating_particle:debug(PidPar, ctx))).
