


% Timestamp utilities.

chainlog_lib(add_time(_, _, _)).

add_time(Time, weeks(W), NewTime) :-
  !, Delta is W * 604800,
  NewTime is Time + Delta.
add_time(Time, days(D), NewTime) :-
  !, Delta is D * 86400,
  NewTime is Time + Delta.
add_time(Time, hours(H), NewTime) :-
  !, Delta is H * 3600,
  NewTime is Time + Delta.
add_time(Time, minutes(M), NewTime) :-
  !, Delta is M * 60,
  NewTime is Time + Delta.
add_time(Time, seconds(S), NewTime) :-
  !, NewTime is Time + S.
add_time(Time, Delta, NewTime) :-
  NewTime is Time + Delta.


% List processing.

chainlog_lib(sum_list(_, _)).
chainlog_lib(max_list(_, _)).
chainlog_lib(min_list(_, _)).

sum_list([], 0).
sum_list([H | T], Sum) :-
	sum_list(T, Sum1),
	Sum is H + Sum1.

max_list([X], X).
max_list([H | T], Max) :-
	max_list(T, Max1),
	Max is max(H, Max1).

min_list([X], X).
min_list([H | T], Min) :-
	min_list(T, Min1),
	Min is min(H, Min1).


% All solutions & aggregation.

chainlog_lib(find_all(_, _, _)).
chainlog_lib(find_all(_, _, _, _)).
chainlog_lib(sum_all(_, _, _)).
chainlog_lib(max_all(_, _, _)).
chainlog_lib(min_all(_, _, _)).

find_all(Template, Goal, Instances) :-
  find_all(Template, Goal, Instances, []).

find_all(Template, Goal, _, _) :-
  asserta('$temp_findall'('$end')),
  chainlog_query(Goal),
  asserta('$temp_findall'(Template)),
  fail.
find_all(_, _, Instances, Remainder) :-
  '$collect'(Instances, Remainder).

'$collect'(Instances, Remainder) :-
  retract('$temp_findall'(Template)),
  !,
  (  Template = '$end'
  -> Instances = Remainder
  ;  '$collect'(Instances, [Template | Remainder])
  ).

sum_all(Template, Goal, Sum) :-
  find_all(Template, Goal, Instances),
  sum_list(Instances, Sum).

max_all(Template, Goal, Max) :-
  find_all(Template, Goal, Instances),
  max_list(Instances, Max).

min_all(Template, Goal, Min) :-
  find_all(Template, Goal, Instances),
  min_list(Instances, Min).

