%%%-------------------------------------------------------------------
%% @doc StepFlow Logger
%% @end
%%%-------------------------------------------------------------------

-module(relocating_logger).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(gen_server).

-export([
  log_msg/1,
  log_msg/2
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

-define(TCP_OPTIONS,
        [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).
-define(PORT, 1234).


%%====================================================================
%% API
%%====================================================================

log_msg(Msg) -> gen_server:cast(relocating_logger, {log_msg, Msg}).

log_msg(Msg, Args) -> gen_server:cast(relocating_logger, {log_msg, Msg, Args}).

%% Callbacks gen_server

% -spec start_link(ctx()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ctx) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Ctx], []).

% -spec init(list(ctx())) -> {ok, ctx()}.
init([#{port := Port}=Ctx]) ->
  {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  {ok, Socket} = gen_tcp:accept(LSocket),
  {ok, Ctx#{socket => Socket}}.

% -spec handle_call(any(), {pid(), term()}, ctx()) -> {reply, ok, ctx()}.
handle_call({accept, LSocket}, _From, Ctx) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  {reply, Ctx#{socket => Socket}};
handle_call(Msg, _From, Ctx) ->
  {reply, Msg, Ctx}.

% -spec handle_cast({append, list(event())} | pop, ctx()) -> {noreply, ctx()}.
handle_cast({log_msg, Msg}, #{socket := Socket}=Ctx) ->
  {Time, Name, [X,Y,Z], [BX, BY,BZ]} = Msg,
  NewBody = sf:format(
    "{{time}} {{name}} {{x}} {{y}} {{z}} {{bx}} {{by}} {{bz}}\n",
    [{time, Time}, {name, Name},
     {x, X}, {y, Y}, {z, Z}, {bx, BX}, {by, BY}, {bz, BZ}]),
  gen_tcp:send(Socket, NewBody),
  {noreply, Ctx};
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
