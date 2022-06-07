package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"

	chainlog "github.com/arran8901/chainlog-lang"
)

var queryDelimiter *regexp.Regexp = regexp.MustCompile(`\.(\s+|$)`)
var whitespace *regexp.Regexp = regexp.MustCompile(`\s+`)

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
			fmt.Fprintf(os.Stderr, "No such file: %s\n", filename)
			continue
		}
		fileSource := string(fileBytes)
		i.Consult(fileSource)
		fmt.Printf("Loaded file: %s\n", filename)
	}

	// Initialise message context
	msgCtx := &chainlog.MessageContext{
		Sender:  "0x0",
		Value:   0,
		Time:    time.Now().Unix(),
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
				// Context command
				splitSubmission := whitespace.Split(submission[1:], -1)
				var components []string
				for _, component := range splitSubmission {
					if component != "" {
						components = append(components, component)
					}
				}
				processContextCommand(components, msgCtx)
			case strings.HasPrefix(submission, "$"):
				// Message
				sendMessage(submission[1:], msgCtx, i)
			default:
				// Query
				queryCtx := &chainlog.QueryContext{
					Sender:  msgCtx.Sender,
					Time:    msgCtx.Time,
					Balance: msgCtx.Balance,
				}
				queryGoal(submission, queryCtx, i, scanner)
			}
		}
	}
}

// queryGoal processes a query for a given single goal term.
func queryGoal(goal string, queryCtx *chainlog.QueryContext, i *chainlog.Interpreter, scanner *bufio.Scanner) {
	itr, err := i.QueryWithContext(goal, queryCtx)
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
	// If message context had value, simulate the transfer to the contract.
	if msgCtx.Value > 0 {
		msgCtx.Balance += msgCtx.Value
		defer func() { msgCtx.Value = 0 }()
	}

	// Submit message to interpreter.
	actions, err := i.Message(message, msgCtx)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return
	}

	updateDynamicKB := false

	for _, action := range actions {
		fmt.Println(action.String())
		switch v := action.(type) {
		case chainlog.AssertAction:
			i.Assert(v.Term)
			updateDynamicKB = true
		case chainlog.RetractAction:
			i.Retract(v.Term)
			updateDynamicKB = true
		case chainlog.TransferAction:
			// Simulate reduction in balance as a result of this transfer.
			msgCtx.Balance -= v.Value
			defer fmt.Printf("Updated balance: %d\n", msgCtx.Balance)
		}
	}

	if updateDynamicKB {
		fmt.Printf("Updated dynamic KB: %s\n", i.GetDynamicKB())
	}
}

// processContextCommand updates the message context as dictated by the context command.
func processContextCommand(args []string, msgCtx *chainlog.MessageContext) {
	if len(args) == 0 {
		fmt.Printf("Context: %+v\n", *msgCtx)
		return
	}

	switch args[0] {
	case "sender", "s":
		if len(args) > 1 {
			msgCtx.Sender = chainlog.Address(args[1])
		}
		fmt.Printf("Sender: %s\n", msgCtx.Sender)

	case "value", "v":
		if len(args) > 1 {
			value, err := strconv.ParseUint(args[1], 10, 64)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Value not convertible to uint: %s\n", args[1])
				return
			}
			msgCtx.Value = value
		}
		fmt.Printf("Value: %d\n", msgCtx.Value)

	case "time", "t":
		if len(args) > 1 {
			time, err := strconv.ParseInt(args[1], 10, 64)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Time not convertible to int: %s\n", args[1])
				return
			}
			msgCtx.Time = time
		}
		fmt.Printf("Time: %d\n", msgCtx.Time)

	case "balance", "b":
		if len(args) > 1 {
			balance, err := strconv.ParseUint(args[1], 10, 64)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Balance not convertible to uint: %s\n", args[1])
			}
			msgCtx.Balance = balance
		}
		fmt.Printf("Balance: %d\n", msgCtx.Balance)

	default:
		fmt.Fprintf(os.Stderr, "Unknown context variable: %s\n", args[0])
	}
}
