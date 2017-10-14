%% Tests

-module(relocating_env_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

get_beacons_test() ->
  Beacons = [{{0,0,0}, 10}, {{5,5,5}, 7}],
  {ok, Pid} = relocating_env:start_link(#{beacons => Beacons}),
  ?assertEqual(Beacons, relocating_env:get_beacons(Pid)).

get_best_test() ->
  {ok, Pid} = relocating_env:start_link(#{}),

  % check default
  Default = #{fitness => -1, position => {0,0,0}},
  ?assertEqual(maps:get(position, Default), relocating_env:get_best(Pid)),
  % check if set new best (when default is -1)
  New = #{fitness => 5, position => {1,2,3}},
  relocating_env:update_best(
    Pid, maps:get(position, New), maps:get(fitness, New)),
  ?assertEqual(maps:get(position, New), relocating_env:get_best(Pid)),

  % check if new fitness is bigger -> should no change
  relocating_env:update_best(Pid, {4,5,6}, 10),
  ?assertEqual(maps:get(position, New), relocating_env:get_best(Pid)),

  % check if set new best
  New2 = #{fitness => 3, position => {6,7,8}},
  relocating_env:update_best(
    Pid, maps:get(position, New2), maps:get(fitness, New2)),
  ?assertEqual(maps:get(position, New2), relocating_env:get_best(Pid)).

around_beacons_test() ->
  Beacons = [{{0,0,0}, 50}, {{100,100,100}, 50}, {{100,100,0}, 50}],
  Radius = 5,

  {ok, Pid} = relocating_env:start_link(#{beacons => Beacons}),

  Results = lists:map(fun(_) ->
      {X, Y, Z} = relocating_env:around_beacons(Pid, Radius),
      lists:any(fun({{Xb, Yb, Zb}, _}) ->
          (X =< Xb+Radius) and (X >= Xb-Radius) and
          (Y =< Yb+Radius) and (Y >= Yb-Radius) and
          (Z =< Zb+Radius) and (Z >= Zb-Radius)
        end, Beacons)
    end, lists:seq(1, 100)),
  ?assert(lists:all(fun(Bool) -> Bool end, Results)).
