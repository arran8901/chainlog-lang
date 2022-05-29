% Test utilities.

% expect_query_true(+Query)
% Succeeds if and only if Chainlog query of Query succeeds.
expect_query_true(Query) :-
  chainlog_query(Query), !.
expect_query_true(Query) :-
  throw(error(test_error(expect_query_true), query_fail(Query))).

% expect_query_false(+Query)
% Succeeds if and only if Chainlog query of Query fails.
expect_query_false(Query) :-
  chainlog_query(Query), !,
  throw(error(test_error(expect_query_false), query_success(Query))).
expect_query_false(_).

% expect_query(+Query, +Expectations)
% Succeeds if Chainlog query of Query succeeds, and after which Expectations also succeeds.
expect_query(Query, Expectations) :-
  chainlog_query(Query), !,
  (  call(Expectations)
  -> true
  ;  throw(error(test_error(expect_query), expectations_fail(Query, Expectations)))).
expect_query(Query, _) :-
  throw(error(test_error(expect_query), query_fail(Query))).


% expect_msg(+MsgTerm, +MsgCtx, ?ExpectedActionsList)
% Succeeds if submitting message MsgTerm with context MsgCtx produces actions ExpectedActionsList.
expect_msg(MsgTerm, MsgCtx, ExpectedActionsList) :-
  catch(chainlog_msg(MsgTerm, MsgCtx, GotActionsList),
        error(match_error(msg_handler, _), _),
        throw(error(test_error(expect_msg), no_matching_handler(MsgTerm, MsgCtx)))), !,
  (  subsumes_term(ExpectedActionsList, GotActionsList)
  -> true
  ;  throw(error(test_error(expect_msg),
           actions_mismatch(MsgTerm, MsgCtx, ExpectedActionsList, GotActionsList)))).

% expect_msg_fail(+MsgTerm, +MsgCtx, ?Error)
% Succeeds if submitting message MsgTerm with context MsgCtx fails with an error Error.
expect_msg_fail(MsgTerm, MsgCtx, ExpectedError) :-
  catch(chainlog_msg(MsgTerm, MsgCtx, _), GotError, true), !,
  % GotError remains a variable if message succeeded and no error was caught.
  (  var(GotError)
  -> throw(error(test_error(expect_msg_fail), msg_success(MsgTerm, MsgCtx)))
  ;  \+ subsumes_term(ExpectedError, GotError)
  -> throw(error(test_error(expect_msg_fail),
           error_mismatch(MsgTerm, MsgCtx, ExpectedError, GotError)))
  ;  true).

% mock_msg_ctx(-MsgCtx)
% Gives a mock message context ready for use in expect_msg predicates.
mock_msg_ctx(msg_ctx('0xABCD', 30, 946684800, 120)).

