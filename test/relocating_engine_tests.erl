%% Tests

-module(relocating_engine_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").

check_fitness_test() ->
  % load mock
  meck:new(relocating_env, [unstick, passthrough]),
  meck:expect(relocating_env, update_best, fun(Pid, Pos, Fitness) ->
      % call original function
      meck:passthrough([Pid, Pos, Fitness])
    end),

  % run test!
  Pid = relocating_engine:run(
    "fuu", [{{0,0,0}, 50}, {{60,60,0}, 35}, {{80,0,0}, 57}], 100, 10),
  timer:sleep(3500),

  % check
  #{best := #{fitness := BestFitness}} = relocating_env:debug(Pid, ctx),
  ListArgs = [Args || {_, {_, Fun, Args}, _}
      <- meck:history(relocating_env), Fun == update_best],
  ?assert(lists:all(fun([_, _, Fit]) -> BestFitness =< Fit end, ListArgs)),

  % unload mock
  ?assert(meck:validate(relocating_env)),
  meck:unload(relocating_env),

  ok.
