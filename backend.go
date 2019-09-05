// dtautom provides corewar-like functionality for decision theory dilemmas
// Spec at alignmentforum.org/posts/qdqYrcGZTh9Lp49Nj
package main

type number int32 // a number in dtautom
type cell struct {
	n                    number
	isInstructionPointer bool // TODO: think of better name
	isMoney              bool
}

const UniverseSize = 1000
const ( // all dtautom instructions
	DIE number = iota // kills the agent
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
	JIP               // jump to A if B is pointer
	DJN               // decrement B, then jump to A if B is not 0
	MVM               // move money from one money slot to another money slot
)

func (u *Universe) AddPointer(loc number) {
	u.pointers[loc] = true
	u.memory[loc].isInstructionPointer = true
}

func (u *Universe) DeletePointer(loc number) {
	delete(u.pointers, loc)
	u.memory[loc].isInstructionPointer = false
}

func (u *Universe) Execute(loc number) {
	// the pointer always moves, so we delete the current one:
	u.DeletePointer(loc)
	instruction := u.memory[loc].n
	Aloc := loc + u.memory[loc+1].n%UniverseSize
	A := &u.memory[Aloc] // first target of operation
	Bloc := loc + u.memory[loc+2].n%UniverseSize
	B := &u.memory[Bloc] // second target of operation
	switch instruction {
	// `return` to skip the final line of this function, which creates a new pointer at the end.
	// Effectively, return means die.
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
			u.AddPointer(loc + 6)
			return
		}
	case SLT:
		if A.n < B.n {
			u.AddPointer(loc + 6)
			return
		}
	case JMP:
		u.AddPointer(Aloc)
		return
	case JMZ:
		if B.n == 0 {
			u.AddPointer(Aloc)
			return
		}
	case JMN:
		if B.n != 0 {
			u.AddPointer(Aloc)
			return
		}
	case JIM:
		if B.isMoney {
			u.AddPointer(Aloc)
			return
		}
	case JIP:
		if B.isInstructionPointer {
			u.AddPointer(Aloc)
			return
		}
	case DJN:
		B.n--
		if B.n != 0 {
			u.AddPointer(Aloc)
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
	u.AddPointer(loc + 3)
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
