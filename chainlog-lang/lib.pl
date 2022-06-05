

chainlog_lib(add_time(_, _, _)).
chainlog_lib(duration(_, _)).
chainlog_lib(week(_)).
chainlog_lib(day(_)).
chainlog_lib(hour(_)).
chainlog_lib(minute(_)).
chainlog_lib(second(_)).

chainlog_lib(sum_list(_, _)).
chainlog_lib(max_list(_, _)).
chainlog_lib(min_list(_, _)).

chainlog_lib(find_all(_, _, _)).
chainlog_lib(find_all(_, _, _, _)).
chainlog_lib(sum_all(_, _, _)).
chainlog_lib(max_all(_, _, _)).
chainlog_lib(min_all(_, _, _)).


% Timestamp utilities.

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

duration(weeks(W), Duration) :-
  !, Duration is W * 604800.
duration(days(D), Duration) :-
  !, Duration is D * 86400.
duration(hours(H), Duration) :-
  !, Duration is H * 3600.
duration(minutes(M), Duration) :-
  !, Duration is M * 60.
duration(seconds(S), Duration) :-
  !, Duration is S.

week(W) :- duration(weeks(1), W).
day(D) :- duration(days(1), D).
hour(H) :- duration(hours(1), H).
minute(M) :- duration(minutes(1), M).
second(S) :- S.


% List processing.

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

count_all(Template, Goal, Count) :-
  find_all(Template, Goal, Instances),
  length(Instances, Count).

sum_all(Template, Goal, Sum) :-
  find_all(Template, Goal, Instances),
  sum_list(Instances, Sum).

max_all(Template, Goal, Max) :-
  find_all(Template, Goal, Instances),
  max_list(Instances, Max).

min_all(Template, Goal, Min) :-
  find_all(Template, Goal, Instances),
  min_list(Instances, Min).

