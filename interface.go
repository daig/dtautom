// dtautom provides corewar-like functionality for decision theory dilemmas
// Spec at alignmentforum.org/posts/qdqYrcGZTh9Lp49Nj

package main

import (
	"fmt"
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

func parseEnvFile(filepath string) Universe {
	return Universe{}
}

func parseAgentFile(filepath string) []number {
	return nil
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
