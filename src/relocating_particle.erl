%%%-------------------------------------------------------------------
%% @doc Particle
%% @end
%%%-------------------------------------------------------------------

-module(relocating_particle).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(gen_server).

-export([
  start_link/1,
  init/1,
  code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).

%%====================================================================
%% API
%%====================================================================

%% Callbacks gen_server

% -spec start_link(ctx()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ctx) ->
  gen_server:start_link(?MODULE, [Ctx], []).

% -spec init(list(ctx())) -> {ok, ctx()}.
init([Ctx]) ->
  {ok, beat(reset(Ctx))}.

% -spec handle_call(any(), {pid(), term()}, ctx()) -> {reply, ok, ctx()}.
handle_call(Msg, _From, Ctx) ->
  {reply, Msg, Ctx}.

% -spec handle_cast({append, list(event())} | pop, ctx()) -> {noreply, ctx()}.
handle_cast(_Msg, Ctx) ->
  {noreply, Ctx}.

handle_info({timeout, _, beat}, Ctx) ->
  {noreply, beat(move(Ctx))};
handle_info(_Msg, Ctx) ->
  {noreply, Ctx}.

terminate(_Reason, _Ctx) ->
  io:format("Terminate!!~n"),
  ok.

code_change(_OldVsn, Ctx, _Extra) ->
  io:format("code changed !"),
  {ok, Ctx}.

%%====================================================================
%% Internal functions
%%====================================================================

move(#{position := Position, velocity := Velocity,
       move := MoveFun, fitness := FitnessFun, env := EnvPid,
       best := #{fitness := BestFitness, position := BestPosition}
      }=Ctx) ->
  % get global best position
  GlobalBestPosition = relocating_env:get_best(EnvPid),
  % and beacons information
  Beacons = relocating_env:get_beacons(EnvPid),
  % compute new position
  NewPosition = MoveFun(
    Position, Velocity, BestPosition, GlobalBestPosition, Ctx),
  % compute new fitness
  NewFitness = FitnessFun(NewPosition, Beacons),
  % if new fitness is better,
  case NewFitness < BestFitness orelse BestFitness =:= -1 of
    true ->
      % update the global best
      relocating_env:update_best(EnvPid, NewPosition, NewFitness),
      % and the particle best fitness/position
      Ctx#{best => #{fitness => NewFitness, position => NewPosition}};
    false -> Ctx
  end.

beat(#{beat_period := Period}=Ctx) ->
  error_logger:warning_msg("Beat ~p", [self()]),
  erlang:start_timer(Period, self(), beat),
  Ctx.

reset(Ctx) ->
  Ctx#{
    position => {0, 0, 0},
    best => #{
      fitness => -1,
      position => {0, 0, 0}
    },
    velocity => 0,
    % environment pid
    % fitness function
    % move function
    % inertial weight
    % cognition
    % social
    % beat: how much time wait before update the position
    beat_period => 1000
  }.
