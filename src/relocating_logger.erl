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

% -import(relocating_matrix, ['+'/1]).

%%====================================================================
%% API
%%====================================================================

log_msg(Msg) -> gen_server:cast(relocating_logger, {log_msg, Msg}).

log_msg(Msg, Args) -> gen_server:cast(relocating_logger, {log_msg, Msg, Args}).

%% Callbacks gen_server

% -spec start_link(ctx()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ctx) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Ctx], []).

% -spec init(list(ctx())) -> {ok, ctx()}.
init([Ctx]) -> {ok, run_source(Ctx)}.

% -spec handle_call(any(), {pid(), term()}, ctx()) -> {reply, ok, ctx()}.
handle_call(Msg, _From, Ctx) ->
  {reply, Msg, Ctx}.

% -spec handle_cast({append, list(event())} | pop, ctx()) -> {noreply, ctx()}.
handle_cast({log_msg, Msg}, #{source := Pid}=Ctx) ->
  store(Pid, Msg),
  {noreply, Ctx};
handle_cast({log_msg, Msg, Args}, #{source := Pid}=Ctx) ->
  store(Pid, sf:format(Msg, Args)),
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

store(Pid, Msg) ->
  stepflow_source_message:append(Pid, [stepflow_event:new(#{}, Msg)]).

run_source(#{filename := Filename}=Ctx) ->
  [{_, {_, PidS, _}}] = stepflow_config:run("
    <<<
    Formatter = fun(Events) ->
        EventsFormatted = lists:map(fun(Event) ->
            {Time, Name, {X,Y,Z}, {BX, BY,BZ}} = stepflow_event:body(Event),
            NewBody = sf:format(
              \"{{time}} {{name}} {{x}} {{y}} {{z}} {{bx}} {{by}} {{bz}}\\n\",
              [{time, Time}, {name, Name},
               {x, X}, {y, Y}, {z, Z}, {bx, BX}, {by, BY}, {bz, BZ}]),
            stepflow_event:body(NewBody, Event)
          end, Events),
        {ok, EventsFormatted}
      end.
    >>>

    interceptor Format = stepflow_interceptor_transform#{eval => Formatter}.
    source FromMsg = stepflow_source_message[]#{}.
    channel Memory = stepflow_channel_memory#{}.
    sink ToFile = stepflow_sink_file[Format]#{
      filename => \"" ++ Filename ++ "\"
    }.

    flow Agent: FromMsg |> Memory |> ToFile.
  "),
  Ctx#{source => PidS}.
