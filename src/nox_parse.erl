-module(nox_parse).
-feature(maybe_expr, enable).
-include("nox.hrl").

-export([schema/1, type/1, expression/1]).

-define(BASE(X), ?Atom(_, ?Graph([X]))).

-record(state, {
  types = scope:new(),
  names = scope:new()
}).

schema(Code) ->
  eval(fun schema/2, Code).

type(Code) ->
  eval(fun type/2, Code).

expression(Code) ->
  eval(fun expression/2, Code).

eval(F, Code) ->
  maybe
    {ok, X, _} ?= F(Code, #state{}),
    {ok, X}
  end.

schema(Code, ST = #state{}) ->
  case Code of
    ?Code(R, [?BASE('=>'), CM, CT]) ->
      maybe
        {ok, Ms, S1} ?= traverse(fun type/2, split_and(CM, []), ST),
        {ok, Ty, S2} ?= type(CT, S1),
        {ok, nox_type:schema(R, [Ty | Ms]), S2}
      end;

    CT ->
      maybe
        R = element(2, CT),
        {ok, Ty, S1} ?= type(CT, ST),
        {ok, nox_type:schema(R, [Ty]), S1}
      end
  end.

type(Code, ST = #state{types = Types}) ->
  case Code of
    ?Atom(R, ?Lower([Text])) ->
      case nox_scope:find(Text, Types) of
        {ok, Name} ->
          {ok, ?Var(R, Name)};

        {error, {unbound, _}} ->
          {Name, Scope} = nox_scope:bind(Text, Types),
          {ok, ?Var(R, Name), ST#state{types = Scope}}
      end;

    ?Atom(R, Path) ->
      {ok, ?Ref(R, to_path(Path)), ST};

    ?Code(R, [CF, CX, CY]) ->
      maybe
        {ok, F, S1} ?= type(CF, ST),
        {ok, X, S2} ?= type(CX, S1),
        {ok, Y, S3} ?= type(CY, S2),
        {ok, ?App(R, [F, X, Y]), S3}
      end;

    ?Code(R, [F, ?Code(_, [?BASE('[]'), Xs])]) ->
      maybe
        {ok, Ts, S1} ?= traverse(fun type/2, [F | Xs], ST),
        {ok, ?App(R, Ts), S1}
      end;

    Invalid ->
      {error, {invalid_type, Invalid}}
  end.

expression(Code, ST = #state{names = Names}) ->
  case Code of
    ?Data(R, Data) ->
      {ok, ?Value(R, Data), ST};

    ?Atom(R, Path = ?Lower([Text])) ->
      case nox_scope:find(Text, Names) of
        {ok, Name} ->
          {ok, ?Bound(R, Name), ST};

        {error, {unbound, _}} ->
          {ok, ?Refer(R, to_path(Path), []), ST}
      end;

    ?Atom(R, Path) ->
      {ok, ?Refer(R, to_path(Path), []), ST};

    ?Code(R, [?BASE('=>'), CP, CX]) ->
      case_({R, CP, [CX]}, ST);

    ?Code(R, [?BASE('{}'), ?Code(CR, [?BASE('=>'), CP, CX]) | Tail]) ->
      maybe
        List = group_cases(CR, CP, [CX], Tail, []),
        {ok, Cases, S1} ?= traverse(fun case_/2, List, ST),
        {ok, ?Match(R, Cases), S1}
      end;

    ?Code(_, [?BASE('{}') | List]) ->
      maybe
        {ok, X, S1} ?= block(List, open(ST)),
        {ok, X, close(S1)}
      end;

    ?Code(R, [?BASE(','), CX, CY]) ->
      maybe
        {ok, X, S1} ?= expression(CX, ST),
        {ok, Y, S2} ?= expression(CY, S1),
        {ok, ?Tuple(R, X, Y), S2}
      end;

    ?Code(R, [CF, CX]) ->
      maybe
        {ok, F, S1} ?= expression(CF, ST),
        {ok, X, S2} ?= expression(CX, S1),
        {ok, ?Apply(R, F, X), S2}
      end;

    Invalid ->
      {error, {invalid_expression, Invalid}}
  end.

group_cases(CR, CP, CXs, List, Cases) ->
  case List of
    [?Code(NR, [?BASE('=>'), NP, NX]) | Tail] ->
      group_cases(NR, NP, [NX], Tail, [{CR, CP, lists:reverse(CXs)} | Cases]);
    
    [CX | Tail] ->
      group_cases(CR, CP, [CX | CXs], Tail, Cases);

    [] ->
      lists:reverse([{CR, CP, lists:reverse(CXs)} | Cases])
  end.

case_({Range, CP, CXs}, ST) ->
  maybe
    {ok, P, S1} ?= pattern(CP, open(ST)),
    {ok, X, S2} ?= block(CXs, S1),
    {ok, ?Case(Range, P, X), close(S2)}
  end.

block(List, ST) ->
  case List of
    [CX] ->
      expression(CX, ST);

    [?Code(R, [?BASE('='), CP, CX]) | Tail] ->
      maybe
        {ok, X, S1} ?= expression(CX, ST),
        {ok, P, S2} ?= pattern(CP, S1),
        {ok, Y, S3} ?= block(Tail, S2),
        {ok, ?LetIn(R, P, X, Y), S3}
      end;

    [?Code(R, [CF = ?BASE('<-'), CP, CX]) | Tail] ->
      maybe
        {ok, F, S1} ?= expression(CF, ST),
        {ok, X, S2} ?= expression(CX, S1),
        {ok, P, S3} ?= pattern(CP, S2),
        {ok, Y, S4} ?= block(Tail, S3),
        {ok, ?Apply(R, F, ?Tuple(R, X, ?Match(R, [?Case(R, P, Y)]))), S4}
      end;

    [CX | Tail] ->
      maybe
        R = element(2, CX),
        {ok, X, S1} ?= expression(CX, ST),
        {ok, Y, S2} ?= block(Tail, S1),
        {ok, ?LetIn(R, ?Skip(R), X, Y), S2}
      end
  end.

pattern(Code, ST) ->
  case Code of
    ?Atom(R, ?Lower([Text])) ->
      {Name, S1} = bind(Text, ST),
      {ok, ?Bind(R, Name), S1};

    ?Atom(R, Path) ->
      {ok, ?Pack(R, to_path(Path), []), ST};

    ?Code(R, [?Atom(_, Path = {A, _}) | CP]) when A == 'Upper' orelse A == 'Graph' ->
      maybe
        {ok, Ps, S1} ?= traverse(fun pattern/2, split_comma(CP, []), ST),
        {ok, ?Pack(R, to_path(Path), Ps), S1}
      end;

    Invalid ->
      {error, {invalid_pattern, Invalid}}
  end.

open(State = #state{names = Names}) ->
  State#state{names = nox_scope:open(Names)}.

close(State = #state{names = Names}) ->
  State#state{names = nox_scope:close(Names)}.

bind(Text, State = #state{names = Names}) ->
  State#state{names = nox_scope:bind(Text, Names)}.

to_path({_, Path}) ->
  Path.

split_and(Code, Xs) ->
  case Code of
    ?Code(_, [?BASE('&'), X, Y]) ->
      split_and(Y, [X | Xs]);

    X ->
      lists:reverse([X | Xs])
  end.

split_comma(Code, Xs) ->
  case Code of
    ?Code(_, [?BASE(','), X, Y]) ->
      split_comma(Y, [X | Xs]);

    X ->
      lists:reverse([X | Xs])
  end.

traverse(F, Xs, State) ->
  traverse_to(F, Xs, [], State).

traverse_to(F, Xs, Ys, State) ->
  case Xs of
    [] ->
      {ok, lists:reverse(Ys), State};

    [X | Tail] ->
      maybe
        {ok, Y, Next} ?= F(X, State),
        traverse_to(F, Tail, [Y | Ys], Next)
      end
  end.