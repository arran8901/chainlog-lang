
% Chainlog syntax.
:- op(1025, xfy, or).
:- op(900, fy, not).

X or _ :- X.
_ or Y :- Y.

not X :- \+ X.

% Declare `on` dynamic to ensure the predicate is defined, even if no message handlers have been
% declared.
:- dynamic(on/1).
% Declare `dyn` dynamic to ensure the predicate is defined, even if the dynamic KB is empty.
:- dynamic(dyn/1).

:- op(1200, fx, on).
:- op(1150, xfx, :).
:- op(1050, fx, require).
:- op(1050, fx, if).
:- op(1050, fx, do).
:- op(1150, fx, dyn).


% Chainlog built-in predicates (inherited from ISO Prolog built-ins).
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

% Built-in predicate between.
chainlog_builtin(between(_, _, _)).

% List built-in predicates.
chainlog_builtin(member(_, _)).
chainlog_builtin(append(_, _, _)).
chainlog_builtin(length(_, _)).
chainlog_builtin(select(_, _, _)).
chainlog_builtin(nth0(_, _, _)).
chainlog_builtin(nth1(_, _, _)).

% Declare chainlog_lib/1 dynamic to ensure the predicate is defined, even if lib is not loaded.
:- dynamic(chainlog_lib/1).


% chainlog_query(:Goal)
%
% Query interpreter.
%
% Goal: a callable goal.
chainlog_query(Goal) :-
  var(Goal), !,
  throw(error(instantiation_error, _)).
chainlog_query(true) :- !.
chainlog_query(Goal) :-
  chainlog_builtin(Goal), !,
  call(Goal).
chainlog_query(Goal) :-
  chainlog_lib(Goal), !,
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
chainlog_query(Goal) :-
  dyn Goal.

% chainlog_query(:Goal, +QueryCtx)
%
% Query interpreter with context decorator.
%
% Goal: a callable goal.
% QueryCtx: a query context (see chainlog_set_query_ctx).
chainlog_query(Goal, QueryCtx) :-
  chainlog_with_query_ctx(chainlog_query(Goal), QueryCtx).

% chainlog_depth_limit(-Limit)
%
% The default Chainlog depth limit
%
% Limit: integer depth limit.
chainlog_depth_limit(4096).

% chainlog_query_dl(:Goal, +Depth, +Limit, -Max)
%
% Query interpreter with depth limit.
%
% Goal: a callable goal.
% Depth: the current depth.
% Limit: maximum allowed depth of proof tree.
% Max: the maximum depth reached.
chainlog_query_dl(Goal, _, _, _) :-
  var(Goal), !,
  throw(error(instantiation_error, _)).
chainlog_query_dl(_, Depth, Limit, _) :-
  Depth > Limit, !,
  throw(error(depth_limit_exceeded, Limit)).
chainlog_query_dl(true, Depth, _, Depth) :- !.
chainlog_query_dl(Goal, Depth, _, Depth) :-
  chainlog_builtin(Goal), !,
  call(Goal).
chainlog_query_dl(Goal, Depth, _, Depth) :-
  chainlog_lib(Goal), !,
  call(Goal).
chainlog_query_dl((G1, G2), Depth, Limit, Max) :-
  !, chainlog_query_dl(G1, Depth, Limit, Max1),
  chainlog_query_dl(G2, Depth, Limit, Max2),
  Max is max(Max1, Max2).
chainlog_query_dl(not Goal, Depth, Limit, Max) :-
  !, \+ chainlog_query_dl(Goal, Depth, Limit, Max).
chainlog_query_dl(G1 or G2, Depth, Limit, Max) :-
  !, (chainlog_query_dl(G1, Depth, Limit, Max)
      ; chainlog_query_dl(G2, Depth, Limit, Max)).
chainlog_query_dl(Goal, Depth, Limit, Max) :-
  Depth1 is Depth + 1,
  clause(Goal, Body),
  chainlog_query_dl(Body, Depth1, Limit, Max).
chainlog_query_dl(Goal, Depth, _, Depth) :-
  dyn Goal.

% chainlog_query_dl(:Goal, +QueryCtx, -Max)
%
% Query interpreter with depth limit and context decorator.
%
% Goal: a callable goal.
% QueryCtx: a query context (see chainlog_set_query_ctx).
% Max: the maximum depth reached.
chainlog_query_dl(Goal, QueryCtx, Max) :-
  chainlog_depth_limit(Limit),
  chainlog_with_query_ctx(
    chainlog_query_dl(Goal, 0, Limit, Max),
    QueryCtx
  ).

% chainlog_with_query_ctx(:Goal, QueryCtx)
%
% Sets up query context QueryCtx, calls Goal, clears QueryCtx.
% The context is cleared whether Goal succeeds, fails or throws an exception.
%
% Goal: a callable goal.
% QueryCtx: a query context (see chainlog_set_query_ctx).
chainlog_with_query_ctx(Goal, QueryCtx) :-
  chainlog_set_query_ctx(QueryCtx),
  catch(Goal, Error, (chainlog_clear_query_ctx, throw(Error))),
  chainlog_clear_query_ctx.
chainlog_with_query_ctx(_, _) :-
  chainlog_clear_query_ctx,
  fail.

% chainlog_set_query_ctx(+QueryCtx)
%
% Sets the current query context to QueryCtx.
% Throws `type_error` if the query context is malformed.
%
% QueryCtx: a compound term query_ctx(Time: int, Balance: int).
chainlog_set_query_ctx(query_ctx(Time, Balance)) :-
  integer(Time),
  integer(Balance), !,
  assertz(time(Time)),
  assertz(balance(Balance)).
chainlog_set_query_ctx(QueryCtx) :-
  throw(error(type_error(compound, QueryCtx), malformed_query_ctx)).

% chainlog_clear_query_ctx
%
% Clears the current query context.
chainlog_clear_query_ctx :-
  retractall(sender(_)),
  retractall(value(_)),
  retractall(time(_)),
  retractall(balance(_)).


% Message interpreter
chainlog_msg(MsgTerm, MsgCtx, ActionsList) :-
  % Attempt to unify MsgTerm with a message handler, then call the handler.
  (on MsgTerm: Body),
  catch(chainlog_with_msg_ctx(chainlog_call_msg_handler(Body, ActionsList), MsgCtx), Error,
        % For require errors, add MsgTerm and MsgCtx to context and rethrow.
        (  Error = error(require_error, Cond, ErrorCond)
        -> throw(error(require_error, context(MsgTerm, MsgCtx, Cond, ErrorCond)))
        % For all other errors, rethrow as is.
        ;  throw(Error))
  ).
chainlog_msg(MsgTerm, _, _) :-
  % If no matching message handler
  throw(error(match_error(msg_handler), MsgTerm)).

% chainlog_with_msg_ctx(:Goal, MsgCtx)
%
% Sets up message context MsgCtx, calls Goal, clears MsgCtx.
% The context is cleared whether Goal succeeds, fails or throws an exception.
%
% Goal: a callable goal.
% MsgCtx: a message context (see chainlog_set_msg_ctx).
chainlog_with_msg_ctx(Goal, MsgCtx) :-
  chainlog_set_msg_ctx(MsgCtx),
  (  catch(Goal, Error, (chainlog_clear_msg_ctx, throw(Error)))
  -> chainlog_clear_msg_ctx
  ;  chainlog_clear_msg_ctx, fail).

% chainlog_set_msg_ctx(+MsgCtx)
%
% Sets the current message context to MsgCtx.
% Throws `type_error` if the message context is malformed.
%
% MsgCtx: a compound term msg_ctx(Sender: atom, Value: int, Time: int, Balance: int).
chainlog_set_msg_ctx(msg_ctx(Sender, Value, Time, Balance)) :-
  atom(Sender),
  integer(Value),
  integer(Time),
  integer(Balance), !,
  assertz(sender(Sender)),
  assertz(value(Value)),
  assertz(time(Time)),
  assertz(balance(Balance)).
chainlog_set_msg_ctx(MsgCtx) :-
  throw(error(type_error(compound, MsgCtx), malformed_msg_ctx)).

% chainlog_clear_msg_ctx
%
% Clears the current message context.
chainlog_clear_msg_ctx :-
  retractall(sender(_)),
  retractall(value(_)),
  retractall(time(_)),
  retractall(balance(_)).

% chainlog_call_msg_handler(+Body, -ActionsList)
%
% Succeeds if all `require` and `if` conditions succeed and unifies ActionsList with the list
% of actions to be executed.
% Fails if any `if` condition fails.
% Throws `require_error` if any `require` condition fails.
%
% Body: a single do clause `do Actions` or composite (BodyClause ; BodyClauses).
% ActionsList: a list of actions [Action1, ..., ActionN].
chainlog_call_msg_handler((do Actions), ActionsList) :-
  !, chainlog_collect_actions(Actions, ActionsList).
chainlog_call_msg_handler((if Cond ; Rest), ActionsList) :-
  !, chainlog_query(Cond),
  chainlog_call_msg_handler(Rest, ActionsList).
chainlog_call_msg_handler((require Cond ; Rest), ActionsList) :-
  % Use a helper to recurse over Cond instead of just querying it, so that variable substitutions
  % to conditions preceding the failing condition can be reported in the thrown require_error.
  % As a bonus, this also identifies the failing condition in ErrorCond.
  chainlog_check_require(Cond, ErrorCond),
  (  ErrorCond = no_error
  -> chainlog_call_msg_handler(Rest, ActionsList)
  ;  throw(error(require_error, Cond, ErrorCond))).

% chainlog_collect_actions(+Actions, -ActionsList)
%
% Converts ','-separated Actions to a list.
% Always succeeds. Unifies ActionsList with the list of collected actions.
%
% Action: a single action or composite (Action , Actions).
% ActionsList: a list of actions [Action1, ..., ActionN].
chainlog_collect_actions((Action, Actions), [Action | ActionsList]) :-
  !, chainlog_collect_actions(Actions, ActionsList).
chainlog_collect_actions(Action, [Action]).

% chainlog_check_require(+Cond, -ErrorCond)
%
% Checks conditions for the first failure. If there is a failing condition, it is unified with
% ErrorCond. Otherwise, if all conditions pass, ErrorCond is unified with the atom `no_error`.
%
% Cond: a single Chainlog goal or a composite (Cond, Conds)
% ErrorCond: a single Chainlog goal or the atom `no_error`.
chainlog_check_require((Cond, Conds), ErrorCond) :-
  !,
  (  chainlog_query(Cond)
  -> chainlog_check_require(Conds, ErrorCond)
  ;  ErrorCond = Cond).
chainlog_check_require(Cond, ErrorCond) :-
  (  chainlog_query(Cond)
  -> ErrorCond = no_error
  ;  ErrorCond = Cond).

% chainlog_reserved_name(+Name)
%
% Succeeds if Name is a Chainlog reserved predicate name.
%
% Name: an atom.
chainlog_reserved_name(or).
chainlog_reserved_name(not).
chainlog_reserved_name(on).
chainlog_reserved_name(dyn).
% Context predicates are reserved.
chainlog_reserved_name(sender).
chainlog_reserved_name(value).
chainlog_reserved_name(time).
chainlog_reserved_name(balance).
% Names beginning with 'chainlog' are reserved.
chainlog_reserved_name(Name) :- atom_concat(chainlog, _, Name).
% Names beginning with '$' are reserved.
chainlog_reserved_name(Name) :- atom_concat('$', _, Name).

