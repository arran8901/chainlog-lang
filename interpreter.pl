
% Operators for Chainlog syntax.
:- op(1100, xfy, or).
:- op(900, fy, not).

X or _ :- X.
_ or Y :- Y.

not X :- \+ X.


% Chainlog built-in predicates (ISO Prolog built-ins)
chainlog_builtin(_ =:= _).
chainlog_builtin(_ =\= _).
chainlog_builtin(_ < _).
chainlog_builtin(_ =< _).
chainlog_builtin(_ > _).
chainlog_builtin(_ >= _).
chainlog_builtin(_ is _).
chainlog_builtin(_ @=< _).
chainlog_builtin(_ == _).
chainlog_builtin(_ \== _).
chainlog_builtin(_ @< _).
chainlog_builtin(_ @> _).
chainlog_builtin(_ @>= _).
chainlog_builtin(_ = _).
chainlog_builtin(_ \= _).
chainlog_builtin(compare(_, _, _)).
chainlog_builtin(sort(_, _)).
chainlog_builtin(keysort(_, _)).
chainlog_builtin(true).
chainlog_builtin(fail).
chainlog_builtin(false).
chainlog_builtin(atom(_)).
chainlog_builtin(integer(_)).
chainlog_builtin(float(_)).
chainlog_builtin(atomic(_)).
chainlog_builtin(compound(_)).
chainlog_builtin(nonvar(_)).
chainlog_builtin(number(_)).
chainlog_builtin(ground(_)).

% Library predicates (TODO need defining)
chainlog_builtin(between(_, _, _)).

chainlog_builtin(member(_, _)).
chainlog_builtin(append(_, _, _)).
chainlog_builtin(length(_, _)).
chainlog_builtin(select(_, _, _)).
chainlog_builtin(nth0(_, _, _)).
chainlog_builtin(nth1(_, _, _)).

chainlog_builtin(bagof(_, _, _)).
chainlog_builtin(findall(_, _, _)). % TODO redefine so that it calls query with depth limit
chainlog_builtin(setof(_, _, _)).


% Query interpreter.
chainlog_query(Goal) :-
  chainlog_builtin(Goal), !,
  call(Goal).
chainlog_query((G1, G2)) :-
  !, chainlog_query(G1), chainlog_query(G2).
chainlog_query(not Goal) :-
  !, \+ chainlog_query(Goal).
chainlog_query(G1 or G2) :-
  !, (chainlog_query(G1) ; chainlog_query(G2)).
chainlog_query(Goal) :-
  clause(Goal, Body),
  chainlog_query(Body).

