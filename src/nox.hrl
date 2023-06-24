% Data = cases {
%   S(String)
%   N(Natural)
%   D(Decimal)
% }
-define(S(X), {'S', X}).
-define(N(X), {'N', X}).
-define(D(X), {'D', X}).

% Atom = cases {
%   Upper(Path)
%   Lower(Path)
%   Graph(Path)
% }
-define(Upper(X), {'Upper', X}).
-define(Lower(X), {'Lower', X}).
-define(Graph(X), {'Graph', X}).

% Code = cases {
%   Data(Range, Data)
%   Atom(Range, Atom)
%   Code(Range, List[Code])
% }
-define(Data(R,X), {'Data', R, X}).
-define(Atom(R,X), {'Atom', R, X}).
-define(Code(R,X), {'Code', R, X}).

% Name is represented by atom
% Path is represented by a reversed list of atoms

% Type[a] = cases {
%   Var(Range, a)
%   Ref(Range, Path)
%   App(Range, List[Tau[a]])
% }
-define(Var(R,X), {'Var', R, X}).
-define(Ref(R,X), {'Ref', R, X}).
-define(App(R,X), {'App', R, X}).

% Schema[a] = cases {
%   ForAll(Range, List[a], List[Tau[a]])
% }
-define(ForAll(R,X,Y), {R, X, Y}).

% Pattern[a] = cases {
%   Skip(a)
%   Bind(a, Name)
%   Pack(a, Path, List[Pattern[a]])
% }
-define(Skip(R),     {'Skip', R}).
-define(Bind(R,X),   {'Bind', R, X}).
-define(Pack(R,X,Y), {'Pack', R, X, Y}).

% Case[a] = {
%   Case(Range, Pattern[a], Expression[a])
% }
-define(Case(R,X,Y), {R, X, Y}).

% Expression[a] = cases {
%   Value(a, Data)
%   Bound(a, Name)
%   Refer(a, Path, List[Type])
%   Match(a, List[Case[a]])
%   Tuple(a, Expression[a], Expression[a])
%   Apply(a, Expression[a], Expression[a])
%   LetIn(a, Pattern[a], Expression[a], Expression[a])
% }
-define(Value(R,X),     {'Value', R, X}).
-define(Bound(R,X),     {'Bound', R, X}).
-define(Refer(R,X,Y),   {'Refer', R, X, Y}).
-define(Match(R,X),     {'Match', R, X}).
-define(Tuple(R,X,Y),   {'Tuple', R, X, Y}).
-define(Apply(R,X,Y),   {'Apply', R, X, Y}).
-define(LetIn(R,X,Y,Z), {'LetIn', R, X, Y, Z}).