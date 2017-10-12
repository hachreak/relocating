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
-export([mul/2, sub/2, sum/1, square/1]).
%%====================================================================
%% API
%%====================================================================

move(Position, Velocity, ParticleBestPosition, GlobalBestPosition, Ctx) ->
  InertialWeight = maps:get(inertial_weight, Ctx, 1),
  Cognition = maps:get(cognition, Ctx, 2),
  Social = maps:get(social, Ctx, 2),
  Rand1 = rand:uniform(),
  Rand2 = rand:uniform(),

  NewVelocity = sum([
    mul(InertialWeight, Velocity),
    mul(mul(Cognition, Rand1), sub(ParticleBestPosition, Position)),
    mul(mul(Social, Rand2), sub(GlobalBestPosition, Position))
  ]),

  sum([Position, NewVelocity]).

fitness(Position, Pods) ->
  sum([abs(sum(square(sub(Base, Position))) - (Radius * Radius))
       || {Base, Radius} <- Pods]).

%%====================================================================
%% Internal functions
%%====================================================================

mul({A, B, C}, {D, E, F}) -> {A*D, B*E, C*F};
mul(A, {D, E, F}) -> {A*D, A*E, A*F};
mul({A, B, C}, D) -> {A*D, B*D, C*D};
mul(A, D) -> Val = A*D, {Val, Val, Val}.

sub({A, B, C}, {D, E, F}) -> {A-D, B-E, C-F}.

sum([{_, _, _} | _]=List) ->
  lists:foldl(
    fun({A, B, C}, {D, E, F}) -> {A+D, B+E, C+F} end, {0, 0, 0}, List);
sum({A, B, C}) -> A + B + C;
sum(List) ->
  lists:foldl(fun(A, Sum) -> A + Sum end, 0, List).

square(A) -> mul(A, A).
