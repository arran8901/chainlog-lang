package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"

	chainlog "github.com/arran8901/chainlog-lang"
)

var queryDelimiter *regexp.Regexp = regexp.MustCompile(`\.(\s+|$)`)

func usage() {
	fmt.Fprintf(flag.CommandLine.Output(), "Usage of %s:\n", os.Args[0])
	fmt.Fprintf(flag.CommandLine.Output(), "\tgochl <filename>\n")
	flag.PrintDefaults()
}

func main() {
	flag.Usage = usage
	flag.Parse()

	var i *chainlog.Interpreter

	fmt.Println("Go Chainlog Interactive Interpreter")

	if filename := flag.Arg(0); filename != "" {
		fileBytes, err := ioutil.ReadFile(filename)
		if err != nil {
			panic(err)
		}
		fileSource := string(fileBytes)
		i = chainlog.NewInterpreterFromProgram(fileSource)
		fmt.Printf("Loaded file: %s\n", filename)
	} else {
		i = chainlog.NewInterpreter()
	}

	var scanner *bufio.Scanner = bufio.NewScanner(os.Stdin)
	for fmt.Print("?- "); scanner.Scan(); fmt.Print("?- ") {
		var queryStr string = scanner.Text()

		// Split into queries
		var queries []string = queryDelimiter.Split(queryStr, -1)
		queries = queries[:len(queries)-1]

		for _, query := range queries {
			queryGoal(query, i, scanner)
		}
	}
}

// queryGoal processes a query for a given single goal term.
func queryGoal(goal string, i *chainlog.Interpreter, scanner *bufio.Scanner) {
	itr, err := i.Query(fmt.Sprintf(` chainlog_query(%s).`, goal))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return
	}
	defer itr.Close()

	for {
		derivation, err := itr.Next()
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
		}

		if !derivation.Successful {
			fmt.Println("no")
			return
		}

		if len(derivation.Unifications) == 0 {
			// No variables to display. Simply print 'yes'.
			fmt.Println("yes")
			return
		}
		// Otherwise, print all variable unifications.
		var unifications []string
		for variable, unifiedTerm := range derivation.Unifications {
			unifications = append(unifications, fmt.Sprintf("%s = %s", variable, unifiedTerm))
		}
		fmt.Print(strings.Join(unifications, ", "))
		fmt.Print(" ")

		// Continue looping as long as user enters ';'.
		if !(scanner.Scan() && strings.HasPrefix(scanner.Text(), ";")) {
			return
		}
	}
}