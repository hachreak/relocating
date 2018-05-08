%%%-------------------------------------------------------------------
%% @doc matrix math
%% @end
%%%-------------------------------------------------------------------

-module(relocating_matrix).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  '.*'/2,
  '-'/2,
  '+'/2,
  fill/2,
  random_uniform/1,
  square/1,
  sum/1,
  sum/2
]).

%%====================================================================
%% API
%%====================================================================

% @doc A .* B @end
'.*'(A, B) when is_list(A) and is_list(B) ->
  lists:zipwith(fun(Ai, Bi) -> Ai * Bi end, A, B);
'.*'(A, B) when is_number(A) and is_list(B) ->
  lists:map(fun(Bi) -> Bi * A end, B);
'.*'(A, B) when is_list(A) and is_number(B) ->
   '.*'(B, A);
'.*'(A, B) ->
   A*B.

% @doc A - B @end
'-'(A, B) ->
 lists:zipwith(fun(Ai, Bi) -> Ai - Bi end, A, B).

% @doc A + B @end
sum(column, [First | _]=Matrix) ->
  Dim = length(First),
  lists:foldl(fun(A, Sum) ->
      '+'(A, Sum)
    end, fill(Dim, 0), Matrix).

sum(List) ->
  lists:foldl(fun(A, Sum) -> A + Sum end, 0, List).

'+'(A, B) ->
 lists:zipwith(fun(Ai, Bi) -> Ai + Bi end, A, B).

% @doc A .* A @end
square(A) -> '.*'(A, A).

% @doc uniform random vector @end
random_uniform(Dim) ->
  lists:map(fun(_) -> rand:uniform() end, lists:seq(1, Dim)).

fill(Dim, Value) ->
  lists:duplicate(Dim, Value).
