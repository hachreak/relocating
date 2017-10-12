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
  {ok, reset(Ctx)}.

% -spec handle_call(any(), {pid(), term()}, ctx()) -> {reply, ok, ctx()}.
handle_call(Msg, _From, Ctx) ->
  {reply, Msg, Ctx}.

% -spec handle_cast({append, list(event())} | pop, ctx()) -> {noreply, ctx()}.
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
    position => {0, 0, 0},
    best => #{
      fitness => 0,
      position => {0, 0, 0}
    },
    velocity => 0
    % fitness function
    % move function
    % inertial weight
    % cognition
    % social
  }.
