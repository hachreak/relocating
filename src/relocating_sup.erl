%%%-------------------------------------------------------------------
%% @doc relocating top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(relocating_sup).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 10, 10}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
