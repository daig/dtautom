// dtautom provides corewar-like functionality for decision theory dilemmas
// Spec at alignmentforum.org/posts/qdqYrcGZTh9Lp49Nj
package main

type number int32 // a number in dtautom
type cell struct {
	n           number
	isProcessor bool // TODO: think of better name
	isMoney     bool
}

const UniverseSize = 1000
const ( // all dtautom instructions
	DIE number = iota // processor self destructs
	DAT               // no-op or data
	MOV               // copy from A to B
	ADD               // add A to B
	SUB               // subtract A from B
	CMP               // skip next instruction if A = B
	SLT               // skip next instruction if A < B
	JMP               // unconditional jump to A
	JMZ               // jump to A if B is zero
	JMN               // jump to A if B is not zero
	JIM               // jump to A if B is money
	JIP               // jump to A if B is processor
	DJN               // decrement B, then jump to A if B is not 0
	MVM               // move money from one money slot to another money slot
)

func (u *Universe) AddProcessor(loc number) {
	u.pointers[loc] = true
	u.memory[loc].isProcessor = true
}

func (u *Universe) DeleteProcessor(loc number) {
	delete(u.pointers, loc)
	u.memory[loc].isProcessor = false
}

func (u *Universe) Execute(loc number) {
	// the processor always moves, so we delete the current one:
	u.DeleteProcessor(loc)
	instruction := u.memory[loc].n
	Aloc := loc + u.memory[loc+1].n%UniverseSize
	A := &u.memory[Aloc] // first target of operation
	Bloc := loc + u.memory[loc+2].n%UniverseSize
	B := &u.memory[Bloc] // second target of operation
	switch instruction {
	// A `return` prevents the creation of a new processor, so effectively means "die"
	case DIE:
		return
	case DAT: // do nothing
	case MOV:
		if B.isMoney {
			return // can't create money
		}
		B.n = A.n
	case ADD:
		if B.isMoney {
			return // can't create money
		}
		B.n += A.n
	case SUB:
		if B.isMoney {
			return // can't destroy money
		}
		B.n -= A.n
	case CMP:
		if A.n == B.n {
			u.AddProcessor(loc + 6)
			return
		}
	case SLT:
		if A.n < B.n {
			u.AddProcessor(loc + 6)
			return
		}
	case JMP:
		u.AddProcessor(Aloc)
		return
	case JMZ:
		if B.n == 0 {
			u.AddProcessor(Aloc)
			return
		}
	case JMN:
		if B.n != 0 {
			u.AddProcessor(Aloc)
			return
		}
	case JIM:
		if B.isMoney {
			u.AddProcessor(Aloc)
			return
		}
	case JIP:
		if B.isProcessor {
			u.AddProcessor(Aloc)
			return
		}
	case DJN:
		B.n--
		if B.n != 0 {
			u.AddProcessor(Aloc)
			return
		}
	case MVM:
		// Can only move money from one money slot to another money slot
		// Money slots cannot be created or destroyed
		if A.isMoney && B.isMoney {
			B.n += A.n
			A.n = 0
		} else {
			// If you try on non-money then you die
			return
		}
	default: // invalid instruction is a DIE
		return
	}
	u.AddProcessor(loc + 3)
}

type Universe struct {
	memory [UniverseSize]cell
	// Stored so we don't have to iterate over all of space every iteration:
	pointers map[number]bool
}

func Transition(u Universe) {
	for p := range u.pointers {
		u.Execute(p)
	}
}
