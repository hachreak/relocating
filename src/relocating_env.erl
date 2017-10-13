%%%-------------------------------------------------------------------
%% @doc Environment
%% @end
%%%-------------------------------------------------------------------

-module(relocating_env).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(gen_server).

-export([
  get_beacons/1,
  get_best/1,
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

update_best(Pid, Position, Fitness) ->
  gen_server:cast(Pid, {update_best, Position, Fitness}).

get_best(Pid) ->
  gen_server:call(Pid, get_best).

get_beacons(Pid) ->
  gen_server:call(Pid, get_beacons).

%% Callbacks gen_server

% -spec start_link(ctx()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ctx) ->
  gen_server:start_link(?MODULE, [Ctx], []).

% -spec init(list(ctx())) -> {ok, ctx()}.
init([Ctx]) ->
  {ok, reset(Ctx)}.

% -spec handle_call(any(), {pid(), term()}, ctx()) -> {reply, ok, ctx()}.
handle_call(get_beacons, _From, #{beacons := Beacons}=Ctx) ->
  {reply, Beacons, Ctx};
handle_call(get_best, _From, #{best := #{position := Position}}=Ctx) ->
  {reply, Position, Ctx};
handle_call(Msg, _From, Ctx) ->
  {reply, Msg, Ctx}.

% -spec handle_cast({append, list(event())} | pop, ctx()) -> {noreply, ctx()}.
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
  Ctx#{
    best => #{
      fitness => -1,
      position => {0, 0, 0}
    }
    % beacons matrix
  }.
