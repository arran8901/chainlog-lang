package chainloglang

import (
	_ "embed"
	"fmt"
	"strconv"
	"strings"

	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/engine"
)

// Interpreter is an interpreter for Chainlog.
type Interpreter struct {
	prologInterpreter *prolog.Interpreter
}

// NewInterpreter creates a new Chainlog interpreter
func NewInterpreter() *Interpreter {
	return &Interpreter{baseInterpreter()}
}

// NewInterpreterFromProgram creates a new Chainlog interpreter and loads the
// supplied program source code.
func NewInterpreterFromProgram(program string) *Interpreter {
	var userDefinedPredicates []predicateIndicator = getUserDefinedPredicates(program)
	p := baseInterpreterWithLibrary()

	// Declare all user defined predicates dynamic in order to make them public.
	// This is needed for clause/2 to have permission to access those predicates.
	for _, predicateIndicator := range userDefinedPredicates {
		if err := p.Exec(fmt.Sprintf(`:- dynamic(%s).`, predicateIndicator)); err != nil {
			fmt.Println(predicateIndicator)
			panic(err)
		}
	}

	// Now load the program itself.
	if err := p.Exec(program); err != nil {
		panic(err)
	}

	return &Interpreter{p}
}

// Derivation is the result of a single derivation of a Chainlog query. It
// encapsulates a bool indicating whether the derivation succeeded and a map
// variableName -> term of variable unifications in a successful solution.
type Derivation struct {
	Successful   bool
	Unifications map[string]string
}

// QueryIterator is an iterator for all derivations of a Chainlog query.
type QueryIterator struct {
	prologSolutions *prolog.Solutions
}

// Close closes the iterator.
func (d *QueryIterator) Close() error {
	return d.prologSolutions.Close()
}

// Next attempts to search for the next derivation of the query.
// If another derivation is found, a map of variable unifications is returned.
// If no further derivation is found, a nil map is returned.
// If an error was raised during the derivation, a non-nil error is returned.
func (d *QueryIterator) Next() (*Derivation, error) {
	hasNext := d.prologSolutions.Next()
	if !hasNext {
		if err := d.prologSolutions.Err(); err != nil {
			return nil, err
		}
		// No next and no error means no further derivation found.
		return &Derivation{Successful: false}, nil
	}

	m := make(map[string]engine.Term)
	if err := d.prologSolutions.Scan(m); err != nil {
		return nil, err
	}

	unifications := make(map[string]string)
	for variable, unifiedTerm := range m {
		unifications[variable] = termToString(unifiedTerm)
	}
	return &Derivation{Successful: true, Unifications: unifications}, nil
}

func (i *Interpreter) Query(query string, args ...interface{}) (*QueryIterator, error) {
	sols, err := i.prologInterpreter.Query(query, args...)
	if err != nil {
		return nil, err
	}
	return &QueryIterator{sols}, nil
}

//go:embed chainlog-lang/interpreter.pl
var chainlogInterpreter string

//go:embed chainlog-lang/lib.pl
var chainlogLib string

// predicateIndicator encapsulates a predicate name and arity, e.g. `findall/3`.
type predicateIndicator struct {
	Predicate string
	Arity     int
}

func (pi predicateIndicator) String() string {
	return fmt.Sprintf("%s/%d", pi.Predicate, pi.Arity)
}

// baseInterpreter creates and returns a base Prolog interpreter with the minimal
// Prolog built-ins to support Chainlog.
func baseInterpreter() *prolog.Interpreter {
	var p *prolog.Interpreter = prolog.New(nil, nil)
	if err := p.Exec(chainlogInterpreter); err != nil {
		panic(err)
	}
	return p
}

// baseInterpreterWithLibrary creates and returns a base Prolog interpreter with
// the Chainlog library loaded.
func baseInterpreterWithLibrary() *prolog.Interpreter {
	p := baseInterpreter()
	if err := p.Exec(chainlogLib); err != nil {
		panic(err)
	}
	return p
}

// termToString converts a prolog.engine.Term to a user-friendly display string.
func termToString(term engine.Term) string {
	switch v := term.(type) {
	case engine.Atom:
		return string(v)
	case engine.Variable:
		return string(v)
	case engine.Integer:
		return strconv.FormatInt(int64(v), 10)
	case engine.Float:
		return strconv.FormatFloat(float64(v), 'f', -1, 64)
	case *engine.Compound:
		if v.Functor == "." {
			// Special case for lists: convert as [elems...].
			var listElems []string
			var listSuffix string // Either ']' or '|Term]'.

			var t engine.Term = v
			for {
				if c, ok := t.(*engine.Compound); ok && c.Functor == "." && len(c.Args) == 2 {
					listElems = append(listElems, termToString(c.Args[0]))
					t = c.Args[1]
				} else if a, ok := t.(engine.Atom); ok && a == "[]" {
					listSuffix = "]"
					break
				} else {
					listSuffix = fmt.Sprintf("|%s]", termToString(t))
					break
				}
			}
			return fmt.Sprintf("[%s%s", strings.Join(listElems, ","), listSuffix)

		} else {
			// For all other compounds, convert as compound_name(args...).
			var args []string
			for _, arg := range v.Args {
				args = append(args, termToString(arg))
			}
			return fmt.Sprintf("%s(%s)", v.Functor, strings.Join(args, ","))
		}
	default:
		panic(fmt.Sprintf("Cannot convert %s of type %T to string", v, v))
	}
}

// getUserDefinedPredicates returns the predicates defined in the given program.
func getUserDefinedPredicates(program string) (pis []predicateIndicator) {
	p := baseInterpreter()

	// Load program.
	if err := p.Exec(program); err != nil {
		panic(err)
	}

	// Get user defined predicates using current_predicate/1. Omit reserved
	// names whose predicates belong to the Chainlog interpreter's code.
	sols, err := p.Query(`current_predicate(PI), PI = Name/_, \+ chainlog_reserved_name(Name).`)
	if err != nil {
		panic(err)
	}
	defer sols.Close()

	for sols.Next() {
		var s struct {
			PI engine.Term
		}

		if err := sols.Scan(&s); err != nil {
			panic(err)
		}
		piCompound := s.PI.(*engine.Compound)
		predicate := string(piCompound.Args[0].(engine.Atom))
		arity := int(piCompound.Args[1].(engine.Integer))
		pis = append(pis, predicateIndicator{predicate, arity})
	}

	return
}
