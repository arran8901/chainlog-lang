
:- op(1100, xfy, or).
:- op(900, fy, not).

X or _ :- X.
_ or Y :- Y.

not X :- \+ X.

age(john, 33).
age(peter, 43).
age(alice, 13).
age(felix, 5).
age(joan, 90).
age(test1, Y) :- number(Y), Y > 100.
age(frank, 55).
age(martin, 41).
age(alex, 24).
age(test2, Y) :- number(Y), Y > 100.
age(myrtle, 69).

old(X) :- age(X, Age), Age >= 40 or member(X, [test1, test2]).

bad(X, Y) :- number(X) -> Y = 3 ; Y = 5.


% Chainlog built-in predicates

% ISO
chainlog_builtin(_ =:= _).
chainlog_builtin(_ =\= _).
chainlog_builtin(_ < _).
chainlog_builtin(_ =< _).
chainlog_builtin(_ > _).
chainlog_builtin(_ >= _).
chainlog_builtin(_ is _).
chainlog_builtin(bagof(_, _, _)).
chainlog_builtin(findall(_, _, _)). % TODO redefine so that it calls query with depth limit
chainlog_builtin(setof(_, _, _)).
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

% Non-ISO (TODO need defining)
chainlog_builtin(between(_, _, _)).

% Lists (TODO need defining)
chainlog_builtin(member(_, _)).
chainlog_builtin(append(_, _, _)).
chainlog_builtin(length(_, _)).
chainlog_builtin(select(_, _, _)).
chainlog_builtin(nth0(_, _, _)).
chainlog_builtin(nth1(_, _, _)).

user_defined_pi(PI) :-
  current_predicate(PI).

% Query interpreter.
query(Goal) :-
  chainlog_builtin(Goal), !,
  call(Goal).
query((G1, G2)) :-
  !, query(G1), query(G2).
query(not Goal) :-
  !, \+ Goal.
query(G1 or G2) :-
  !, (query(G1) ; query(G2)).
query(Goal) :-
  clause(Goal, Body),
  query(Body).

