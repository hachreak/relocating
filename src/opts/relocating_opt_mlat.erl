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

-import(relocating_matrix, [
  '.*'/2, '-'/2, sum/1, sum/2, square/1, random_uniform/1
]).


fitness(Position, EnvPid) ->
  [Beacons] = relocating_env:ctx(EnvPid, [{get, beacons}]),
  sum([abs(sum(square('-'(Base, Position))) - (Radius * Radius))
       || {Base, Radius} <- Beacons]).

% @doc get a random point around a random beacon. @end
around_beacons(Radius, Beacons) ->
  {Beacon, _} = choose_beacon(rand:uniform(), Beacons),
  Dim = length(Beacon),
  Point = '.*'(expand(Radius), random_uniform(Dim)),
  sum(column, [Beacon, Point]).

%% Private functions

choose_beacon(0, [Beacon | _]) -> Beacon;
choose_beacon(X, Beacons) -> lists:nth(ceil(X*length(Beacons)), Beacons).

expand(Radius) ->
  fun(X) -> (X - 0.5) * Radius / 0.5 end.
