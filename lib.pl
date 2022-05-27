


% Timestamp utilities

:- op(100, xf, seconds).
:- op(100, xf, minutes).
:- op(100, xf, hours).
:- op(100, xf, days).

add_time(Time, D days, NewTime) :-
  Delta is D * 86400,
  NewTime is Time + Delta.
add_time(Time, H hours, NewTime) :-
  Delta is H * 3600,
  NewTime is Time + Delta.
add_time(Time, M minutes, NewTime) :-
  Delta is M * 60,
  NewTime is Time + Delta.
add_time(Time, S seconds, NewTime) :-
  NewTime is Time + S.
add_time(Time, Delta, NewTime) :-
  NewTime is Time + Delta.

