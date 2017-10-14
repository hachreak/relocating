%%%-------------------------------------------------------------------
%% @doc relocating public API
%% @end
%%%-------------------------------------------------------------------

-module(relocating_pso).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  move/5,
  fitness/2
]).

-import(relocating_matrix, ['.*'/2, '-'/2, '+'/1, square/1]).

%%====================================================================
%% API
%%====================================================================

move(Position, Velocity, ParticleBestPosition, GlobalBestPosition, Ctx) ->
  InertialWeight = maps:get(inertial_weight, Ctx, 1),
  Cognition = maps:get(cognition, Ctx, 2),
  Social = maps:get(social, Ctx, 2),
  Rand1 = rand:uniform(),
  Rand2 = rand:uniform(),

  NewVelocity = '+'([
    '.*'(InertialWeight, Velocity),
    '.*'('.*'(Cognition, Rand1), '-'(ParticleBestPosition, Position)),
    '.*'('.*'(Social, Rand2), '-'(GlobalBestPosition, Position))
  ]),

  '+'([Position, NewVelocity]).

fitness(Position, Beacons) ->
  '+'([abs('+'(square('-'(Base, Position))) - (Radius * Radius))
       || {Base, Radius} <- Beacons]).
