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
  init_position/2
]).

-define(M, relocating_matrix).

fitness(Coordinates, _EnvPid) ->
  ?M:sum(?M:square(Coordinates)).

init_position(Max, Center) ->
  Dim = length(Center),
  ?M:'+'(?M:'.*'(Max, ?M:random_uniform(Dim)), Center).
