package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"
	"time"

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

	// Create new interpreter.
	i = chainlog.NewInterpreter()

	// Load files if filenames given.
	for _, filename := range flag.Args() {
		fileBytes, err := ioutil.ReadFile(filename)
		if err != nil {
			fmt.Fprintf(os.Stderr, "No such file: %s", filename)
		}
		fileSource := string(fileBytes)
		i.Consult(fileSource)
		fmt.Printf("Loaded file: %s\n", filename)
	}

	// Initialise message context
	msgCtx := &chainlog.MessageContext{
		Sender:  "0x0",
		Value:   0,
		Time:    uint(time.Now().Unix()),
		Balance: 0,
	}

	var scanner *bufio.Scanner = bufio.NewScanner(os.Stdin)
	for fmt.Print("?- "); scanner.Scan(); fmt.Print("?- ") {
		var inputStr string = scanner.Text()

		// Split user input into submissions (queries, messages).
		var submissions []string = queryDelimiter.Split(inputStr, -1)
		if submissions[len(submissions)-1] == "" {
			submissions = submissions[:len(submissions)-1]
		}

		for _, submission := range submissions {
			switch {
			case strings.HasPrefix(submission, ":"):
				// Context directive
				// TODO
			case strings.HasPrefix(submission, "$"):
				// Message
				sendMessage(submission[1:], msgCtx, i)
			default:
				// Query
				queryGoal(submission, i, scanner)
			}
		}
	}
}

// queryGoal processes a query for a given single goal term.
func queryGoal(goal string, i *chainlog.Interpreter, scanner *bufio.Scanner) {
	itr, err := i.Query(goal)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return
	}
	defer itr.Close()

	for {
		derivation, err := itr.Next()
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			return
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

// sendMessage processes a single given message.
func sendMessage(message string, msgCtx *chainlog.MessageContext, i *chainlog.Interpreter) {
	actions, err := i.Message(message, msgCtx)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return
	}

	for _, action := range actions {
		fmt.Println(action)
	}
}
