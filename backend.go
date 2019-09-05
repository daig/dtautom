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
)

func (u *Universe) Execute(loc number) {
	u.memory[loc].isInstructionPointer = false // the pointer always moves
	instruction := u.memory[loc].n
	A := &u.memory[loc+1].n
	B := &u.memory[loc+2].n
	switch instruction {
	// return when we don't want to put the pointer three spaces ahead
	case DIE:
		return
	case DAT: // do nothing
	case MOV:
		*B = *A
	case ADD:
		*B += *A
	case SUB:
		*B -= *A
	case CMP:
		if *A == *B {
			u.memory[loc+6].isInstructionPointer = true
			return
		}
	case SLT:
		if *A < *B {
			u.memory[loc+6].isInstructionPointer = true
			return
		}
	case JMP:
		u.memory[*A].isInstructionPointer = true
		return
	case JMZ:
		if *B == 0 {
			u.memory[*A].isInstructionPointer = true
			return
		}
	case JMN:
		if *B != 0 {
			u.memory[*A].isInstructionPointer = true
			return
		}
	case JIM:
		if u.memory[loc+2].isMoney {
			u.memory[*A].isInstructionPointer = true
			return
		}
	case JIP:
		if u.memory[loc+2].isInstructionPointer {
			u.memory[*A].isInstructionPointer = true
			return
		}
	case DJN:
		*B--
		if *B != 0 {
			u.memory[*A].isInstructionPointer = true
			return
		}
	default: // invalid instruction is a DIE
		return
	}
	u.memory[loc+3].isInstructionPointer = true
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
