%%%-------------------------------------------------------------------
%% @doc Optimization problem: Multilateration
%% @end
%%%-------------------------------------------------------------------

-module(relocating_opt_mlat).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  fitness/2
]).

-export([
  around_beacons/2
]).

-import(relocating_matrix, ['-'/2, '+'/1, square/1]).


fitness(Position, EnvPid) ->
  Beacons = relocating_env:ctx(EnvPid, {get, beacons}),
  '+'([abs('+'(square('-'(Base, Position))) - (Radius * Radius))
       || {Base, Radius} <- Beacons]).

% @doc get a random point around a random beacon. @end
around_beacons(Radius, Beacons) ->
  {Beacon, _} = choose_beacon(rand:uniform(), Beacons),
  Point = {expand(rand:uniform(), Radius),
           expand(rand:uniform(), Radius),
           expand(rand:uniform(), Radius)},
  '+'([Beacon, Point]).

%% Private functions

choose_beacon(0, [Beacon | _]) -> Beacon;
choose_beacon(X, Beacons) -> lists:nth(ceil(X*length(Beacons)), Beacons).

expand(X, Radius) -> (X - 0.5) * Radius / 0.5.
