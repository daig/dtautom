// dtautom provides corewar-like functionality for decision theory dilemmas
// Spec at alignmentforum.org/posts/qdqYrcGZTh9Lp49Nj

// TODO:
// + check within DeleteProcessor whether it exists yet
// + think of better name that "processor". Chemical reaction as motivating analogy?
// Ideas:
// + Allow the agent to create and destroy money squares
// + Make the booleans part of the number
// + Add ability to destroy remote processors
// + Add ability to mark cells as money but set to 0
// + Allow the agent to read farther than it can write
// + Make universe size and maxstep part of the universe
// + Think of better name for money (diamonds¿)

package main

type number int32 // a number in dtautom
type cell struct {
	n           number
	isProcessor bool // TODO: think of better name
	isMoney     bool
}

const maxStep number = 100 // the largest distance a processor can see, touch, or move
const UniverseSize = 1000
const defaultStep = 3
const ( // all dtautom instructions
	_ number = iota
	// unconditional operations:
	DIE       // processor self destructs
	DATA      // no-op or data
	COPY      // Overwrite B with the value of A
	TRANSFER  // add A to B and set B to 0. You can transfer money.
	ADD       // add A to B
	SUBTRACT  // subtract A from B
	JUMP      // unconditional jump to A
	MAKEMONEY // set A to 0 and mark it as money cell
	MAKEPROC  // mark A as a processor cell
	// conditional operations:
	ISEQUAL // skip ahead if A = B
	ISLESS  // skip ahead if A < B
	ISZERO  // skip ahead if A is zero
	ISMONEY // skip ahead if A is money
	ISPROC  // skip ahead if A is a processor
)

func (u *Universe) AddProcessor(loc number) {
	u.processors[loc] = true
	u.memory[loc].isProcessor = true
}

func (u *Universe) DeleteProcessor(loc number) {
	delete(u.processors, loc)
	u.memory[loc].isProcessor = false
}

func (u *Universe) Move(loc number, distance number) {
	u.DeleteProcessor(loc)
	// processor explodes if it tries to move too far
	if abs(distance) <= maxStep {
		u.AddProcessor((loc + distance) % UniverseSize)
	}
}

func (u *Universe) MoveIf(loc number, b bool) {
	if b {
		u.Move(loc, 6)
	} else {
		u.Move(loc, 3)
	}
}

func (u *Universe) Execute(loc number) {
	instruction := u.memory[loc].n
	Adist := u.memory[(loc+1)%UniverseSize].n
	Bdist := u.memory[(loc+2)%UniverseSize].n

	// Cannot read, write or jump to cells outside of range:
	if abs(Adist) > maxStep || abs(Bdist) > maxStep {
		u.DeleteProcessor(loc)
		return // TODO: It should be fine if B is large if you don't use it.
	}

	A := &u.memory[(loc+Adist)%UniverseSize] // first target of operation
	B := &u.memory[(loc+Bdist)%UniverseSize] // second target of operation

	switch instruction {
	case DIE:
		u.DeleteProcessor(loc)
	case DATA: // do nothing
		u.Move(loc, 3)
	case COPY:
		if B.isMoney {
			u.DeleteProcessor(loc) // forgery is punishable by death
		} else {
			B.n = A.n
			u.Move(loc, 3)
		}
	case TRANSFER:
		if A.isMoney != B.isMoney { // both or neither must be money squares
			u.DeleteProcessor(loc) // forgery is punishable by death
		}
		B.n += A.n
		A.n = 0
	case ADD:
		if B.isMoney {
			u.DeleteProcessor(loc) // forgery is punishable by death
		} else {
			B.n += A.n
			u.Move(loc, 3)
		}
	case SUBTRACT:
		if B.isMoney {
			u.DeleteProcessor(loc) // forgery is punishable by death
		} else {
			B.n -= A.n
			u.Move(loc, 3)
		}
	case JUMP:
		u.Move(loc, Adist)
	case MAKEMONEY:
		A.n = 0
		A.isMoney = true
	case MAKEPROC:
		A.isProcessor = true
	case ISEQUAL:
		u.MoveIf(loc, A.n == B.n)
	case ISLESS:
		u.MoveIf(loc, A.n < B.n)
	case ISZERO:
		u.MoveIf(loc, A.n == 0)
	case ISMONEY:
		u.MoveIf(loc, A.isMoney)
	case ISPROC:
		u.MoveIf(loc, A.isProcessor)
	default:
		// invalid instruction is a DIE
		u.DeleteProcessor(loc) // TODO: this should be logged or something
	}
}

type Universe struct {
	memory [UniverseSize]cell
	// Stored so we don't have to iterate over all of space every iteration:
	// (Doesn't change universe behavior)
	processors map[number]bool
}

func Transition(u Universe) {
	for p := range u.processors {
		u.Execute(p)
	}
}

func abs(x number) number {
	if x > 0 {
		return x
	} else {
		return -x
	}
}
