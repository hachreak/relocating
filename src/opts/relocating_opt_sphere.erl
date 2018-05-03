%%%-------------------------------------------------------------------
%% @doc Optimization problem: Sphere
%% @end
%%%-------------------------------------------------------------------

-module(relocating_opt_sphere).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  fitness/2
]).

-export([
  init_position/1
]).

-import(relocating_matrix, ['+'/1]).

fitness({X, Y,Z}, _EnvPid) ->
  '+'([X*X + Y*Y + Z*Z]).

init_position(Max) ->
  {rand:uniform(Max), rand:uniform(Max), rand:uniform(Max)}.
