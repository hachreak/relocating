%%%-------------------------------------------------------------------
%% @doc matrix math
%% @end
%%%-------------------------------------------------------------------

-module(relocating_matrix).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  '.*'/2,
  '-'/2,
  '+'/1,
  square/1
]).

%%====================================================================
%% API
%%====================================================================

% @doc A .* B @end
'.*'({A, B, C}, {D, E, F}) -> {A*D, B*E, C*F};
'.*'(A, {D, E, F}) -> {A*D, A*E, A*F};
'.*'({A, B, C}, D) -> {A*D, B*D, C*D};
'.*'(A, D) -> Val = A*D, {Val, Val, Val}.

% @doc A - B @end
'-'({A, B, C}, {D, E, F}) -> {A-D, B-E, C-F}.

% @doc A + B @end
'+'([{_, _, _} | _]=List) ->
  lists:foldl(
    fun({A, B, C}, {D, E, F}) -> {A+D, B+E, C+F} end, {0, 0, 0}, List);
'+'({A, B, C}) -> A + B + C;
'+'(List) ->
  lists:foldl(fun(A, Sum) -> A + Sum end, 0, List).

% @doc A .* A @end
square(A) -> '.*'(A, A).
