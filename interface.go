// dtautom provides corewar-like functionality for decision theory dilemmas
// Spec at alignmentforum.org/posts/qdqYrcGZTh9Lp49Nj
// TODO:
// + Finish implementing agent file parser
// + Write tests for agent file parser
// + Finish environment file parser
// + Write tests for environment file parser

package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Printf("Usage: dtautom ENVFILE [AGENTFILE]...")
		os.Exit(1)
	}
	environmentPath := os.Args[1]
	agentPaths := os.Args[2:]
	fmt.Printf("dtautom called with environment %v and agents %v.\n", environmentPath, agentPaths)
	fmt.Println("dtautom does not actually do anything right now.")
}

func parseEnvFile(filepath string) universe {
	return universe{}
}

func agentFileToTokens(filepath string) []string { // TODO NEEDS TESTING
	file, err := os.Open(filepath)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lines := make([]string, 0)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		line = deleteComment(line)
		if line != "" {
			lines = append(lines, line) // Println will add back the final '\n'
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	tokens := make([]string, 0)
	for _, line := range lines {
		for _, token := range strings.Fields(line) {
			tokens = append(tokens, token)
		}
	}
	return tokens
}

func agentTokensToNumbers(tokens []string) []number { // INCOMPLETE
	// arithmetic is not supported
	i := 0
	tagIndex := make(map[string]int)
	program := make([]number, 0)
	for _, t := range tokens {
		// tokens which end in colons are tags
		if t[len(t)-1] == ':' {
			if _, ok := tagIndex[t]; ok {
				log.Fatalf("Tag %v appears twice in file", t)
			}
			tagIndex[t] = i
			continue
		}
		k := nameToNumber(t)
		if k != -1 {
			program = append(program, k)
			continue
		}
	}
	return program
}

func toWords() {
	// An artificial input source.
	const input = "Now is the winter of our discontent,\nMade glorious summer by this sun of York.\n"
	scanner := bufio.NewScanner(strings.NewReader(input))
	// Set the split function for the scanning operation.
	scanner.Split(bufio.ScanWords)
	// Count the words.
	count := 0
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "reading input:", err)
	}
	fmt.Printf("%v\n", count)
}

func deleteComment(line string) string {
	i := strings.Index(line, "//")
	if i == -1 {
		return line
	}
	return line[:i]
}

func splitOnEquals(s string) (string, string, error) {
	i := strings.IndexRune(s, '=')
	if i == -1 {
		return "", "", fmt.Errorf("string %v has no '='", s)
	}
	return s[:i], s[i+1:], nil
}

func nameToNumber(s string) number {
	switch s {
	case "DIE":
		return DIE
	case "DATA":
		return DATA
	case "COPY":
		return COPY
	case "TRANSFER":
		return TRANSFER
	case "ADD":
		return ADD
	case "SUBTRACT":
		return SUBTRACT
	case "JUMP":
		return JUMP
	case "MAKEMONEY":
		return MAKEMONEY
	case "MAKEPROC":
		return MAKEPROC
	case "ISEQUAL":
		return ISEQUAL
	case "ISLESS":
		return ISLESS
	case "ISZERO":
		return ISZERO
	case "ISMONEY":
		return ISMONEY
	case "ISPROC":
		return ISPROC
	}
	return -1
}
