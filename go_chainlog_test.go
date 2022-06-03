package chainloglang

import (
	_ "embed"
	"fmt"
	"io/fs"
	"io/ioutil"
	"path/filepath"
	"strings"
	"testing"
)

//go:embed tests/testlib.pl
var testlib string

func TestQuery(t *testing.T) {
	runTestsForDir(t, "tests/query")
}

func TestMsg(t *testing.T) {
	runTestsForDir(t, "tests/msg")
}

func TestLib(t *testing.T) {
	runTestsForDir(t, "tests/lib")
}

func TestDynamicKB(t *testing.T) {
	const program = `
human(john).
human(sophie).
	`
	const dynamicKB = `
dyn human(mark).
dyn human(catherine).
	`

	i := newTestInterpreter()
	if err := i.ConsultWithDynamicKB(program, dynamicKB); err != nil {
		panic(err)
	}

	expectQueryDerivations(t, i, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "mark"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "catherine"}},
		&Derivation{Successful: false},
	)
	if dynamicKB := i.GetDynamicKB(); dynamicKB != "human(mark).\nhuman(catherine).\n" {
		t.Fatalf("Expected dynamic KB {human(mark), human(catherine)}, got %s", dynamicKB)
	}

	// Should be able to retract dynamic fact.
	i.Retract(`human(mark)`)

	expectQueryDerivations(t, i, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "catherine"}},
		&Derivation{Successful: false},
	)
	if dynamicKB := i.GetDynamicKB(); dynamicKB != "human(catherine).\n" {
		t.Fatalf("Expected dynamic KB {human(catherine)}, got %s", dynamicKB)
	}

	// Should not be able to retract static fact.
	i.Retract(`human(sophie)`)

	expectQueryDerivations(t, i, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "catherine"}},
		&Derivation{Successful: false},
	)
	if dynamicKB := i.GetDynamicKB(); dynamicKB != "human(catherine).\n" {
		t.Fatalf("Expected dynamic KB {human(catherine)}, got %s", dynamicKB)
	}

	// Should be able to assert dynamic fact.
	i.Assert(`human(kyle)`)

	expectQueryDerivations(t, i, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "catherine"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "kyle"}},
		&Derivation{Successful: false},
	)
	if dynamicKB := i.GetDynamicKB(); dynamicKB != "human(catherine).\nhuman(kyle).\n" {
		t.Fatalf("Expected dynamic KB {human(catherine), human(kyle)}, got %s", dynamicKB)
	}

	// Should be able to retract multiple dynamic facts without retracting static facts.
	i.Retract(`human(_)`)

	expectQueryDerivations(t, i, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: false},
	)
	if dynamicKB := i.GetDynamicKB(); dynamicKB != "" {
		t.Fatalf("Expected empty dynamic KB, got %s", dynamicKB)
	}

	// Dynamic KB from one interpreter should be loadable in a fresh interpreter.
	i.Assert(`human(vincent)`)
	i.Assert(`human(george)`)
	newDynamicKB := i.GetDynamicKB()

	fresh := newTestInterpreter()
	fresh.ConsultWithDynamicKB(program, newDynamicKB)

	expectQueryDerivations(t, fresh, `human(X)`,
		&Derivation{Successful: true, Unifications: map[string]string{"X": "john"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "sophie"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "vincent"}},
		&Derivation{Successful: true, Unifications: map[string]string{"X": "george"}},
		&Derivation{Successful: false},
	)
	if dynamicKB := i.GetDynamicKB(); dynamicKB != newDynamicKB {
		t.Fatalf("Expected dynamic KB {human(vincent), human(george)}, got %s", dynamicKB)
	}
}

func TestParametricDisasterInsurance(t *testing.T) {
	fileBytes, _ := ioutil.ReadFile("tests/full_examples/parametric_disaster_insurance.chl")
	fileSource := string(fileBytes)

	i := newTestInterpreter()
	if err := i.Consult(fileSource); err != nil {
		panic(err)
	}

	// 1656028800 is Jun 24 00:00 GMT
	if err := i.prologInterpreter.Exec(`
rainfall(13, 1654819200).  % Jun 10
rainfall(27, 1654905600).  % Jun 11
rainfall(81, 1654992000).  % Jun 12
rainfall(81, 1655078400).  % Jun 13
rainfall(81, 1655164800).  % Jun 14
rainfall(81, 1655251200).  % Jun 15
rainfall(81, 1655337600).  % Jun 16
rainfall(81, 1655424000).  % Jun 17
rainfall(81, 1655510400).  % Jun 18
rainfall(81, 1655596800).  % Jun 19
rainfall(81, 1655683200).  % Jun 20
rainfall(81, 1655769600).  % Jun 21
rainfall(56, 1655856000).  % Jun 22
rainfall(46, 1655942400).  % Jun 23
rainfall(23, 1656028800).  % Jun 24

seismic_intensity('5+', 1672534801).  % After expiration

wind_speed(99, 1655164800).  % Jun 14
	`); err != nil {
		panic(err)
	}

	// Expect cyclone 80%, flood 100%.
	expectQueryDerivations(t, i, `eligible_for(D, P)`,
		&Derivation{Successful: true, Unifications: map[string]string{"D": "cyclone", "P": "80"}},
		&Derivation{Successful: true, Unifications: map[string]string{"D": "flood", "P": "100"}},
		&Derivation{Successful: false},
	)

	// Attempt payout from wrong address.
	_, err := i.Message(`claimPayout(cyclone)`, &MessageContext{
		Sender:  "0x5555",
		Value:   0,
		Time:    1656288000,
		Balance: 1000,
	})
	if err.Error() != "error(require_error, context(claimPayout(cyclone), msg_ctx('0x5555', 0, 1656288000, 1000), , (sender('0x5555'), policyholder('0x5555')), policyholder('0x5555')))" {
		t.Fatalf("Expected require_error but got %s", err.Error())
	}

	// Make a cyclone payout (from correct address).
	actions, err := i.Message(`claimPayout(cyclone)`, &MessageContext{
		Sender:  "0x1234",
		Value:   0,
		Time:    1656288000, // Jun 27
		Balance: 1000,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !(len(actions) == 2 &&
		actions[0].Kind() == "assert" &&
		actions[0].(AssertAction).Term == "claimed(cyclone,1656288000)" &&
		actions[1].Kind() == "transfer" &&
		actions[1].(TransferAction).ToAddress == "0x1234" &&
		actions[1].(TransferAction).Value == 80) {
		t.Fatalf("Expected actions [assert(claimed(cyclone,1656288000)), transfer(0x1234,80)], got %s", actions)
	}
	i.Assert(actions[0].(AssertAction).Term)

	// Expect flood 100% but no longer cyclone.
	expectQueryDerivations(t, i, `eligible_for(D, P)`,
		&Derivation{Successful: true, Unifications: map[string]string{"D": "flood", "P": "100"}},
		&Derivation{Successful: false},
	)

	// Attempt cyclone payout when not eligible.
	_, err = i.Message(`claimPayout(cyclone)`, &MessageContext{
		Sender:  "0x1234",
		Value:   0,
		Time:    1656374400, // Jun 28
		Balance: 920,
	})
	if !(strings.Contains(err.Error(), "require_error") &&
		strings.Contains(err.Error(), ", eligible_for(cyclone")) {
		t.Fatalf("Expected require_error with eligible_for(cyclone, _) but got %s", err.Error())
	}

	// Dynamic KB should show cyclone has been claimed.
	dynamicKB := i.GetDynamicKB()
	if strings.HasSuffix(dynamicKB, "claimed(cyclone, 1656288000).\n") {
		t.Fatalf("expected dynamic KB of claimed(cyclone, 1656288000) at the end, got %s", dynamicKB)
	}
}

func expectQueryDerivations(t *testing.T, i *Interpreter, query string, expectedDerivations ...*Derivation) {
	itr, err := i.Query(query)
	if err != nil {
		t.Fatal(err)
	}
	defer itr.Close()

	for i, expectedDerivation := range expectedDerivations {
		actualDerivation, err := itr.Next()
		if err != nil {
			t.Fatal(err)
		}

		equal := expectedDerivation.Successful == actualDerivation.Successful &&
			len(expectedDerivation.Unifications) == len(actualDerivation.Unifications)
		for variable, unifiedTerm := range expectedDerivation.Unifications {
			equal = equal && actualDerivation.Unifications[variable] == unifiedTerm
		}

		if !equal {
			t.Errorf("For query %s, derivation %d: expected %+v, actual %+v", query, i, expectedDerivation, actualDerivation)
		}
	}
}

func runTestsForDir(t *testing.T, dir string) {
	// Walk directory, collecting files with extension '.chl' as test files.
	var testFilepaths []string
	err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			t.Fatal(err)
		}
		if !d.IsDir() && filepath.Ext(path) == ".chl" {
			testFilepaths = append(testFilepaths, path)
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}

	// Run tests for each test file.
	for _, path := range testFilepaths {
		runTestsForFile(t, path)
	}
}

func runTestsForFile(t *testing.T, testFilepath string) {
	fileBytes, _ := ioutil.ReadFile(testFilepath)
	fileSource := string(fileBytes)

	// Find the test predicates.
	testPredicates, err := getTestPredicates(fileSource)
	if err != nil {
		t.Errorf("Failed to load test file: %s (%s)", testFilepath, err)
		return
	}

	// If there are no test predicates, we are done.
	if len(testPredicates) == 0 {
		return
	}

	// Otherwise, call each test predicate and report any test failures.
	i := newTestInterpreter()
	i.Consult(fileSource)
	for _, testPredicate := range testPredicates {
		sol := i.prologInterpreter.QuerySolution(fmt.Sprintf(`%s.`, testPredicate))
		if err = sol.Err(); err != nil {
			t.Errorf("In %s [%s]: %s", testFilepath, testPredicate, err)
		}
	}

	if err == nil {
		t.Logf("All tests passed for file: %s %v", testFilepath, testPredicates)
	}
}

func newTestInterpreter() (i *Interpreter) {
	i, err := NewInterpreterWithBuiltins(testlib)
	if err != nil {
		panic(err)
	}
	return
}

func getTestPredicates(program string) ([]string, error) {
	userDefinedPredicates, err := getUserDefinedPredicates(program)
	if err != nil {
		return nil, err
	}

	var pis []string
	for _, userDefinedPredicate := range userDefinedPredicates {
		// Test predicates are the 0-ary predicates whose names start with "test"
		if strings.HasPrefix(userDefinedPredicate.Predicate, "test") &&
			userDefinedPredicate.Arity == 0 {
			pis = append(pis, userDefinedPredicate.Predicate)
		}
	}
	return pis, nil
}
