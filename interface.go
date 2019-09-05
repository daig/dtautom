// dtautom provides corewar-like functionality for decision theory dilemmas
// Spec at alignmentforum.org/posts/qdqYrcGZTh9Lp49Nj

package main

import (
	"fmt"
	"os"
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