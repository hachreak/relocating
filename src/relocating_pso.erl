%%%-------------------------------------------------------------------
%% @doc relocating public API
%% @end
%%%-------------------------------------------------------------------

-module(relocating_pso).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  move/5
]).

-import(relocating_matrix, [
  '.*'/2, '-'/2, fill/2, sum/2
]).

%%====================================================================
%% API
%%====================================================================

move(Position, Velocity, ParticleBestPosition, GlobalBestPosition, Ctx) ->
  Dim = maps:get(dimensions, Ctx),
  InertialWeight = maps:get(inertial_weight, Ctx, 1),
  Cognition = maps:get(cognition, Ctx, 2),
  Social = maps:get(social, Ctx, 2),
  Rand1 = relocating_matrix:random_uniform(Dim),
  Rand2 = relocating_matrix:random_uniform(Dim),

  NewVelocity = sum(column, [
    '.*'(fill(Dim, InertialWeight), fill(Dim, Velocity)),
    '.*'('.*'(Cognition, Rand1), '-'(ParticleBestPosition, Position)),
    '.*'('.*'(Social, Rand2), '-'(GlobalBestPosition, Position))
  ]),

  sum(column, [Position, NewVelocity]).
