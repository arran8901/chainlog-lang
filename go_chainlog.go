package chainloglang

import (
	_ "embed"
	"fmt"
	"os"
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
	i, _ := baseInterpreterFromSources(chainlogLib)
	return &Interpreter{i}
}

// NewInterpreterWithBuiltins creates a new Chainlog interpreter with the given
// Prolog sources loaded as built-in.
func NewInterpreterWithBuiltins(builtinSources ...string) (*Interpreter, error) {
	i, err := baseInterpreterFromSources(append([]string{chainlogLib}, builtinSources...)...)
	if err != nil {
		return nil, err
	}
	return &Interpreter{i}, nil
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

// Address is the address of an account.
type Address string

// MessageContext contains contextual information about a Chainlog message.
type MessageContext struct {
	Sender Address
	Value  uint64
	Time   int64

	Balance uint64
}

// Action represents a single action term resulting from a message.
type Action interface {
	fmt.Stringer
	Kind() string
}

// AssertAction represents an assert/1 action term, to insert term into the dynamic KB.
type AssertAction struct {
	Term string
}

func (AssertAction) Kind() string {
	return "assert"
}

func (a AssertAction) String() string {
	return fmt.Sprintf("assert(%s)", a.Term)
}

// RetractAction represents a retract/1 action term, to remove term from the dynamic KB.
type RetractAction struct {
	Term string
}

func (RetractAction) Kind() string {
	return "retract"
}

func (a RetractAction) String() string {
	return fmt.Sprintf("retract(%s)", a.Term)
}

// TransferAction represents a transfer/2 action term, to transfer funds to an address.
type TransferAction struct {
	ToAddress string
	Value     uint64
}

func (TransferAction) Kind() string {
	return "transfer"
}

func (a TransferAction) String() string {
	return fmt.Sprintf("transfer(%s, %d)", a.ToAddress, a.Value)
}

// asChainlogTerm formats the message context into a Chainlog term and returns the
// string representation.
func (msgCtx *MessageContext) asChainlogTerm() string {
	return fmt.Sprintf("msg_ctx('%s', %d, %d, %d)", string(msgCtx.Sender), msgCtx.Value, msgCtx.Time, msgCtx.Balance)
}

// Consult loads the supplied program source code.
func (i *Interpreter) Consult(program string) error {
	userDefinedPredicates, err := getUserDefinedPredicates(program)
	if err != nil {
		return err
	}

	// Declare all user defined predicates dynamic in order to make them public.
	// This is needed for clause/2 to have permission to access those predicates.
	for _, predicateIndicator := range userDefinedPredicates {
		if err := i.prologInterpreter.Exec(fmt.Sprintf(`:- dynamic(%s).`, predicateIndicator)); err != nil {
			panic(err)
		}
	}

	// Now load the program itself.
	if err := i.prologInterpreter.Exec(program); err != nil {
		return err
	}

	return nil
}

// ConsultWithDynamicKB loads both program source code and dynamic KB
func (i *Interpreter) ConsultWithDynamicKB(program string, dynamicKB []string) error {
	err := i.Consult(program)
	if err != nil {
		return err
	}
	var sb strings.Builder
	for _, dynamicFact := range dynamicKB {
		sb.WriteString(fmt.Sprintf("dyn(%s).\n", dynamicFact))
	}

	err = i.prologInterpreter.Exec(sb.String())
	if err != nil {
		return err
	}
	return nil
}

// Query submits a Chainlog query for a given single goal term. Returns a
// QueryIterator of derivations.
func (i *Interpreter) Query(goal string) (*QueryIterator, error) {
	// TODO: injection vulnerability
	sols, err := i.prologInterpreter.Query(fmt.Sprintf(`chainlog_query((%s)).`, goal))
	if err != nil {
		return nil, err
	}
	return &QueryIterator{sols}, nil
}

// Message sends a Chainlog message with the given message term and context. Returns
// the sequence of actions to be performed.
func (i *Interpreter) Message(msgTerm string, msgCtx *MessageContext) ([]Action, error) {
	var msgCtxTerm string = msgCtx.asChainlogTerm()

	sol := i.prologInterpreter.QuerySolution(fmt.Sprintf(`chainlog_msg((%s), (%s), ActionsList).`, msgTerm, msgCtxTerm))
	if err := sol.Err(); err != nil {
		return nil, err
	}

	var s struct {
		ActionsList []engine.Term
	}
	sol.Scan(&s)

	var actions []Action
	for _, actionTerm := range s.ActionsList {
		actionCompound, ok := actionTerm.(*engine.Compound)
		if !ok {
			panic(fmt.Sprintf("Expected compound while parsing action: %s", termToString(actionTerm)))
		}

		var action Action
		switch string(actionCompound.Functor) {
		case "assert":
			action = AssertAction{Term: termToString(actionCompound.Args[0])}
		case "retract":
			action = RetractAction{Term: termToString(actionCompound.Args[0])}
		case "transfer":
			var value uint64
			if v, ok := actionCompound.Args[1].(engine.Float); ok {
				value = uint64(v)
			} else if v, ok := actionCompound.Args[1].(engine.Integer); ok {
				value = uint64(v)
			}
			action = TransferAction{
				ToAddress: string(actionCompound.Args[0].(engine.Atom)),
				Value:     value,
			}
		}
		actions = append(actions, action)
	}
	return actions, nil
}

// Assert adds the term as a fact in the dynamic KB.
func (i *Interpreter) Assert(term string) {
	// TODO: injection vulnerability
	sol := i.prologInterpreter.QuerySolution(fmt.Sprintf("assertz(dyn(%s)).", term))
	if err := sol.Err(); err != nil {
		panic(err)
	}
}

// Retract removes from the dynamic KB all facts unifying with the given term.
func (i *Interpreter) Retract(term string) {
	// TODO: injection vulnerability
	sol := i.prologInterpreter.QuerySolution(fmt.Sprintf("retractall(dyn(%s)).", term))
	if err := sol.Err(); err != nil {
		panic(err)
	}
}

// GetDynamicKB returns the current state of the dynamic KB as a source code string.
func (i *Interpreter) GetDynamicKB() (dynamicKB []string) {
	sols, err := i.prologInterpreter.Query(`dyn(Term).`)
	if err != nil {
		panic(err)
	}
	defer sols.Close()

	for sols.Next() {
		var s struct {
			Term engine.Term
		}
		if err := sols.Scan(&s); err != nil {
			panic(err)
		}
		dynamicKB = append(dynamicKB, termToString(s.Term))
	}
	return
}

// DynamicKBAsLogicProgram formats the given dynamic KB as logic program code.
func DynamicKBAsLogicProgram(dynamicKB []string) string {
	var sb strings.Builder
	for _, dynamicFact := range dynamicKB {
		sb.WriteString(fmt.Sprintf("%s.\n", dynamicFact))
	}
	return sb.String()
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
	p.SetUserOutput(os.Stdout)
	return p
}

// baseInterpreterFromSources creates and returns a base Prolog interpreter with
// the given sources loaded.
func baseInterpreterFromSources(sources ...string) (*prolog.Interpreter, error) {
	p := baseInterpreter()
	for _, source := range sources {
		if err := p.Exec(source); err != nil {
			return nil, err
		}
	}
	return p, nil
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
func getUserDefinedPredicates(program string) ([]predicateIndicator, error) {
	p := baseInterpreter()

	// Load program.
	if err := p.Exec(program); err != nil {
		return nil, err
	}

	// Get user defined predicates using current_predicate/1. Omit reserved
	// names whose predicates belong to the Chainlog interpreter's code.
	sols, err := p.Query(`current_predicate(PI), PI = Name/_, \+ chainlog_reserved_name(Name).`)
	if err != nil {
		panic(err)
	}
	defer sols.Close()

	var pis []predicateIndicator
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

	return pis, nil
}
