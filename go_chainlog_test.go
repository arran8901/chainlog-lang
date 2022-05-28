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
			t.Errorf("In %s: %s", testFilepath, err)
		}
	}

	if err == nil {
		t.Logf("All tests passed for file: %s", testFilepath)
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
