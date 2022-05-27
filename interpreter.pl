
% Chainlog syntax.
:- op(1100, xfy, or).
:- op(900, fy, not).

X or _ :- X.
_ or Y :- Y.

not X :- \+ X.

:- op(1200, fx, on).
:- op(1150, xfx, :).
:- op(1050, fx, require).
:- op(1050, fx, if).
:- op(1050, fx, do).


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


% Message interpreter
chainlog_msg(MsgTerm, MsgInfo, ActionsList) :-
  % Attempt to unify MsgTerm with a message handler, then call the handler.
  (on MsgTerm: Body),
  call_msg_handler(Body, MsgInfo, ActionsList).
chainlog_msg(MsgTerm, _, _) :-
  % If no message handler
  throw(error(match_error(msg_handler, MsgTerm), 'No matching message handler')).

% call_msg_handler(+Body, +MsgInfo, -ActionsList)
%
% Succeeds if all `require` and `if` conditions succeed and unifies ActionsList with the list
% of actions to be executed.
% Fails if any `if` condition fails.
% Throws `require_error` if any `require` condition fails.
%
% Body: a single do clause `do Actions` or composite (BodyClause ; BodyClauses).
% MsgInfo: TODO.
% ActionsList: a list of actions [Action1, ..., ActionN].
call_msg_handler((do Actions), MsgInfo, ActionsList) :-
  !, collect_actions(Actions, MsgInfo, ActionsList).
%call_msg_handler((require Literals ; Rest), Msg) :- fail.

% collect_actions(+Actions, +MsgInfo, -ActionsList)
%
% Converts ','-separated Actions to a list.
% Always succeeds. Unifies ActionsList with the list of collected actions.
%
% Action: a single action or composite (Action , Actions).
% MsgInfo: TODO.
% ActionsList: a list of actions [Action1, ..., ActionN].
collect_actions((Action, Actions), MsgInfo, [Action | ActionsList]) :-
  !, collect_actions(Actions, MsgInfo, ActionsList).
collect_actions(Action, _, [Action]).



