%%%-------------------------------------------------------------------
%% @doc Environment
%% @end
%%%-------------------------------------------------------------------

-module(relocating_env).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(gen_server).

-export([
  debug/2
]).

-export([
  ctx/2,
  get_best/1,
  subscribe/2,
  unsubscribe/2,
  update_best/3
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

%% API for particles

unsubscribe(Pid, ParticlePid) ->
  gen_server:cast(Pid, {unsubscribe, ParticlePid}).

subscribe(Pid, ParticlePid) ->
  gen_server:cast(Pid, {subscribe, ParticlePid}).

update_best(Pid, Position, Fitness) ->
  gen_server:cast(Pid, {update_best, Position, Fitness}).

get_best(Pid) -> gen_server:call(Pid, get_best).

ctx(Pid, Action) -> gen_server:call(Pid, {ctx, Action}).

debug(Pid, Cmd) -> gen_server:call(Pid, {debug, Cmd}).

%% Callbacks gen_server

% -spec start_link(ctx()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ctx) -> gen_server:start_link(?MODULE, [Ctx], []).

% -spec init(list(ctx())) -> {ok, ctx()}.
init([Ctx]) -> {ok, reset(Ctx)}.

% -spec handle_call(any(), {pid(), term()}, ctx()) -> {reply, ok, ctx()}.
handle_call({debug, ctx}, _From, Ctx) -> {reply, Ctx, Ctx};
handle_call({ctx, {set, beacons, Value}}, _, Ctx) ->
  Ctx2 = reset_best(Ctx),
  {reply, Value, Ctx2#{beacons => Value}};
handle_call({ctx, {set, Var, Value}}, _, Ctx) ->
  {reply, Value, Ctx#{Var => Value}};
handle_call({ctx, {get, Var}}, _, Ctx) ->
  Value = maps:get(Var, Ctx),
  {reply, Value, Ctx};
handle_call(get_best, _From, #{best := #{position := Position}}=Ctx) ->
  {reply, Position, Ctx};
handle_call(Msg, _From, Ctx) ->
  {reply, Msg, Ctx}.

% -spec handle_cast({append, list(event())} | pop, ctx()) -> {noreply, ctx()}.
handle_cast({unsubscribe, ParticlePid}, #{subscribed := Sub}=Ctx) ->
  Sub2 = sets:del_element(ParticlePid, Sub),
  {noreply, Ctx#{subscribed => Sub2}};
handle_cast({subscribe, ParticlePid}, #{subscribed := Sub}=Ctx) ->
  Sub2 = sets:add_element(ParticlePid, Sub),
  {noreply, Ctx#{subscribed => Sub2}};
handle_cast({update_best, {_, _, _}=Position, Fitness},
            #{best := #{fitness := BestFitness}}=Ctx) ->
  % update global fitness if is better
  NewCtx = case Fitness < BestFitness orelse BestFitness =:= -1 of
    true ->
      Ctx#{best => #{fitness => Fitness, position => Position}};
    false -> Ctx
  end,
  {noreply, NewCtx};
handle_cast(_Msg, Ctx) ->
  {noreply, Ctx}.

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

reset(Ctx) ->
  reset_best(Ctx#{
    best => #{position => {0, 0, 0}},
    subscribed => sets:new()
    % beacons matrix
  }).

reset_best(#{subscribed := Sub, best := Best}=Ctx) ->
  lists:foreach(fun(ParticlePid) ->
      relocating_particle:reset(ParticlePid)
    end, sets:to_list(Sub)),
  Ctx#{best => maps:put(fitness, -1, Best)}.
