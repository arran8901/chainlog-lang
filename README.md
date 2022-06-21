# Chainlog Language

Repository for the Chainlog language interpreter and related utilities.

## Language Files
The primary Chainlog language interpreter is written in Prolog, and resides in `chainlog-lang/interpreter.pl`. It performs as much of the interpretation process as is possible in portable Prolog.

A collection of standard library predicates are found in `chainlog-lang/lib.pl`. This includes predicates such as `find_all/3` which are not expressible in pure Chainlog.

The interpreter can be executed using any Prolog implementation by consulting `chainlog-lang/interpreter.pl`, optionally consulting `chainlog-lang/lib.pl`, and optionally consulting a Chainlog file. From there, one can submit a Chainlog query by calling metapredicate `chainlog_query(:Goal)`, or simulate a message call to see resultant actions by calling `chainlog_msg(+MsgTerm, +MsgCtx, -ActionsList)`.
For example, in SWI-Prolog:
```
swipl chainlog-lang/interpreter.pl
?- ['chainlog-lang/lib.pl'].
?- chainlog_query(3 < 4).
```
Alternatively, use the provided interactive interpreter `gochl`. See below for details.

## Go-Chainlog

Go-Chainlog is an abstraction around a Chainlog interpreter and a Prolog engine. It serves as a library for interpreting, querying and executing Chainlog files from Go code. The implementation is found in `go_chainlog.go`.

Importing:
```go
import chainlog "github.com/arran8901/chainlog-lang"
```

Loading a Chainlog source:
```go
const source string = `
mortal(X) :- man(X).
man(socrates).
`
i := chainlog.NewInterpreter()
if err := i.Consult(source); err != nil {
  panic(err)
}
```

Querying:
```go
itr, err := i.Query("mortal(X).")
if err != nil {
  panic(err)
}
for {
  derivation, err := itr.Next()
  if err != nil {
    panic(err)
  }
  if !derivation.Successful {
    fmt.Println("No more derivations")
    break
  }
  fmt.Println(derivation.Unifications)
}
```

## Interactive Interpreter
An interactive Chainlog interpreter written in Go is provided in `cli/gochl.go`.

To start `gochl` from the project root, run:
```
go run cli/gochl.go [<chl-file>...]
```
Recommended: use `rlwrap` to enable line editing and history.

The interpreter allows you to submit queries such as `age(X, A), A >= 40` as an interactive Prolog interpreter would.

It also allows you to simulate message calls by beginning input with `$` and entering a message term. For example, `$ withdraw(30)`. The actions that would result from such a message will be displayed.

Queries and messages execute in a _query context_ and _message context_ respectively. This includes contextual information about the state of the blockchain and contract at the time of the query or message. A Chainlog program has access to the context by way of _context predicates_. For a query, the context includes the current block timestamp (available via `time/1`) and the contract's balance (available via `balance/1`). A message includes both of these but also the message sender (available via `sender/1`) and the number of tokens sent with the message (available via predicate `value/1`). `gochl` simulates a context for queries and messages. To see the simulated context, use the _context command_ `:` in the prompt:
```
?- :
```
To see a single context variable, enter the variable name after the colon:
```
?- :sender
?- :time
```
To set the value of a context variable, enter the variable name followed by the value you wish to set:
```
?- :balance 300
?- :sender chainlog1krvhlmkhu4c9jgc7w32vetgy9saf6utreh8mya
```

## Testing
A testing framework and test suite are provided.

To run all tests, use:
```
go test
```
Recommended: use the `-v` flag.

The testing framework consists of:
* `tests/testlib.pl`: Test utility functions written in Prolog for asserting expectations about queries and messages (e.g. the query succeeds; the message produces certain expected actions).
* `go_chainlog_test.go`: A test runner written in Go.
* `tests/**/*.chl`: Chainlog test cases.

The Prolog test expectation procedures are designed to be callable from a pure Chainlog test file.
Test directories (such as `tests/query`, `tests/msg`) are registered in the Go test runner by creating a new test method and calling `runTestsForDir` with the directory's path. The Go test runner will search for Chainlog (`.chl`) files within the directory and execute all zero-arity predicates whose predicate symbol begins with `test`.
See the test cases under subdirectories of `tests` for numerous examples.
