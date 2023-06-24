-module(nox_scope).
-feature(maybe_expr, enable).

-export([new/0]).
-export([open/1, close/1]).
-export([bind/2, find/2]).

-type text() :: binary().
-type count() :: non_neg_integer().

-record(scope, {
  count = #{} :: #{text() => count()},
  stack = [#{}] :: [#{text() => atom()}]
}).

new() -> #scope{}.

open(Scope = #scope{stack = Stack}) ->
  Scope#scope{stack = [#{} | Stack]}.

close(Scope = #scope{stack = [_ | Stack]}) ->
  Scope#scope{stack = Stack}.

bind(Text, Scope = #scope{count = Count, stack = [Scope | Stack]}) ->
  N = maps:get(Text, Count, 0) + 1,
  Name = list_to_atom(lists:concat([binary_to_list(Text), N])),
  Next = Scope#scope{
    count = maps:put(Text, Count, N),
    stack = [maps:put(Text, Name, Scope) | Stack]
  },
  {Name, Next}.

find(Text, #scope{stack = Stack}) ->
  find_in(Text, Stack).

find_in(Text, Stack) ->
  case Stack of
    [Scope | Parents] ->
      maybe
        error ?= maps:find(Text, Scope),
        find_in(Text, Parents)
      end;
  
    [] ->
      {error, {unbound, Text}}
  end.
