%%%-------------------------------------------------------------------
%% @doc Particle
%% @end
%%%-------------------------------------------------------------------

-module(relocating_particle).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(gen_server).

-export([
  beat/3,
  debug/2,
  move/1,
  update/2
]).

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

update(Pid, NewCtx) -> gen_server:cast(Pid, {update, NewCtx}).

move(Pid) -> gen_server:cast(Pid, move).

debug(Pid, Cmd) -> gen_server:call(Pid, {debug, Cmd}).

beat(Pid, Period, Times) -> gen_server:cast(Pid, {beat, Period, Times}).

%% Callbacks gen_server

% -spec start_link(ctx()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ctx) ->
  gen_server:start_link(?MODULE, [Ctx], []).

% -spec init(list(ctx())) -> {ok, ctx()}.
init([Ctx]) ->
  {ok, compute_beat(reset(Ctx))}.

% -spec handle_call(any(), {pid(), term()}, ctx()) -> {reply, ok, ctx()}.
handle_call({debug, ctx}, _From, Ctx) -> {reply, Ctx, Ctx};
handle_call(Msg, _From, Ctx) ->
  {reply, Msg, Ctx}.

% -spec handle_cast({append, list(event())} | pop, ctx()) -> {noreply, ctx()}.
handle_cast({update, NewCtx}, Ctx) -> {noreply, maps:merge(Ctx, NewCtx)};
handle_cast({beat, Period, Times}, Ctx) ->
  {noreply, compute_beat(Ctx#{beat => #{period => Period, times => Times}})};
handle_cast(move, Ctx) -> {noreply, compute_move(Ctx)};
handle_cast(_Msg, Ctx) ->
  {noreply, Ctx}.

handle_info({timeout, _, beat}, Ctx) ->
  {noreply, compute_beat(compute_move(Ctx))};
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

compute_move(#{position := Position, velocity := Velocity,
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
  NewCtx = case NewFitness < BestFitness orelse BestFitness =:= -1 of
    true ->
      % update the global best
      relocating_env:update_best(EnvPid, NewPosition, NewFitness),
      % and the particle best fitness/position
      Ctx#{best => #{fitness => NewFitness, position => NewPosition}};
    false -> Ctx
  end,
  NewCtx#{position => NewPosition}.

schedule_beat(#{beat := #{period := Period}}) ->
  error_logger:warning_msg("Beat ~p", [self()]),
  erlang:start_timer(Period, self(), beat).

compute_beat(#{beat := #{times := Times}}=Ctx) when Times < 0 ->
  % infinite beating
  schedule_beat(Ctx),
  Ctx;
compute_beat(#{beat := #{times := 0}}=Ctx) ->
  % stop beating
  error_logger:warning_msg("Beat ~p [STOP]", [self()]),
  Ctx;
compute_beat(#{beat := #{times := Times}=Beat}=Ctx) ->
  schedule_beat(Ctx),
  Ctx#{beat => Beat#{times => Times - 1}};
compute_beat(Ctx) ->
  % in case no period is specified, don't run periodically!
  Ctx.

reset(Ctx) ->
  Position = maps:get(position, Ctx, {0, 0, 0}),
  Velocity = maps:get(velocity, Ctx, 3),
  Name = maps:get(name, Ctx, pid_to_list(self())),
  Ctx#{
    name => Name,
    position => Position,
    velocity => Velocity,
    best => #{
      fitness => -1,
      position => {0, 0, 0}
    }
    % environment pid
    % fitness function
    % move function
    % inertial weight
    % cognition
    % social
    % beat: how much time wait before update the position
    %   period => 1000, times => -1
   }.
