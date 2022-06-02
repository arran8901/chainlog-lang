# Chainlog Language

Repository for the Chainlog language interpreter and related utilities.

## Language Files
The primary Chainlog language interpreter is written in Prolog, and resides in `chainlog-lang/interpreter.pl`. It performs as much of the interpretation process as is possible in portable Prolog.

A collection of standard library predicates are found in `chainlog-lang/lib.pl`. This includes predicates such as `find_all/3` which are not expressible in pure Chainlog.

The interpreter can be executed using any Prolog implementation by consulting `chainlog-lang/interpreter.pl`, optionally consulting `chainlog-lang/lib.pl`, and optionally consulting a Chainlog file. For example, in SWI-Prolog:
```
swipl chainlog-lang/interpreter.pl
?- ['chainlog-lang/lib.pl'].
```

## Interactive Interpreter
An interactive interpreter written in Go is provided in `cli/gochl.go`.

To start `gochl` from the project root, run:
```
go run cli/gochl.go [<chl-file>...]
```
It is recommended to use `rlwrap` to enable line editing and history.

The interpreter allows you to submit queries such as `age(X, A), A >= 40` as an interactive Prolog interpreter would.
It also allows you to simulate message calls by beginning input with `$` and entering a message term. For example, `$ withdraw(30)`.

## Testing
A testing framework and test suite are provided.

To run all tests, use:
```
go test
```
The `-v` flag is recommended.

The testing framework consists of:
* `tests/testlib.pl`: Test utility functions written in Prolog for asserting expectations about queries and messages (e.g. the query succeeds; the message produces certain expected actions).
* `go_chainlog_test.go`: A test runner written in Go.
* `tests/**/*.chl`: Chainlog test cases.

The Prolog test expectation procedures are designed to be callable from a pure Chainlog test file.
Test directories (such as `tests/query`, `tests/msg`) are registered in the Go test runner by creating a new test method and calling `runTestsForDir` with the directory's path. The Go test runner will search for Chainlog (`.chl`) files within the directory and execute all zero-arity predicates whose predicate symbol begins with `test`.
See the test cases under subdirectories of `tests` for numerous examples.
