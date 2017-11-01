%%%-------------------------------------------------------------------
%% @doc StepFlow Logger
%% @end
%%%-------------------------------------------------------------------

-module(relocating_logger).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(gen_server).

-export([
  log/1
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

% -import(relocating_matrix, ['+'/1]).

%%====================================================================
%% API
%%====================================================================

log(Msg) -> gen_server:cast(relocating_logger, {log, Msg}).

%% Callbacks gen_server

% -spec start_link(ctx()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ctx) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Ctx], []).

% -spec init(list(ctx())) -> {ok, ctx()}.
init([Ctx]) -> {ok, run_source(Ctx)}.

% -spec handle_call(any(), {pid(), term()}, ctx()) -> {reply, ok, ctx()}.
handle_call(Msg, _From, Ctx) ->
  {reply, Msg, Ctx}.

% -spec handle_cast({append, list(event())} | pop, ctx()) -> {noreply, ctx()}.
handle_cast({log, Msg}, #{source := Pid}=Ctx) ->
  stepflow_source_message:append(Pid, [stepflow_event:new(#{}, Msg)]),
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

run_source(#{filename := Filename}=Ctx) ->
  [{_, {_, PidS, _}}] = stepflow_config:run("
    source FromMsg = stepflow_source_message[]#{}.
    channel Memory = stepflow_channel_memory#{}.
    sink ToFile = stepflow_sink_file[]#{filename => \"" ++ Filename ++ "\"}.

    flow Agent: FromMsg |> Memory |> ToFile.
  "),
  Ctx#{source => PidS}.
