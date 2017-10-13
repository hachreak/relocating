%% Tests

-module(relocating_particle_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

move_test() ->
  % run env
  Beacons = [{{0,0,0}, 10}, {{5,5,5}, 7}],
  {ok, PidEnv} = relocating_env:start_link(#{beacons => Beacons}),
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
    position => {0,0,0},
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

  ok.
