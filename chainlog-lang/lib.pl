


% Timestamp utilities

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

