// Package corewar provides corewar-like functionality
// One cell is a triple (int32, isInstructionPointer, isMoney)
// There is no hidden data and each cell is an int32
// An instruction operates on the next two cells after it.
// As an optimization, the live instruction pointers are stored in a seperate array
// The code goes [operation A B operation A B operation A B] and so on.
// A is source and B is destination
// All operations work on locations, not direct values.
// https://corewar.co.uk/perry/evolution.htm
package dtautom

type number int32
type cell struct {
	n                    number
	isInstructionPointer bool
	isMoney              bool
}

const NumAgents = 4
const UniverseSize = 1000
const (
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

func (u *Universe) Execute(loc number) (err error) {
	instruction := u.memory[loc].n
	A := &u.memory[loc+1].n
	B := &u.memory[loc+2].n
	switch instruction {
	case DIE:
	case DAT: // do nothing
	case MOV:
		*B = *A
	case ADD:
		*B += *A
	case SUB:
		*B -= *A
	case CMP:
		if *A == *B {
		}
	case SLT:
		if *A < *B {
		}
	case JMP:
	case JMZ:
		if *B == 0 {
		}
	case JMN:
		if *B != 0 {

		}
	case JIM:
		if u.memory[loc+1].isMoney {
		}
	case JIP:
	case DJN:
	default:
	}
	return nil
}

type Universe struct {
	memory   [UniverseSize]cell
	pointers [NumAgents]number
}

func Transition(u Universe) {
	for _, p := range u.pointers {
		err := u.Execute(p)
		if err != nil {
			// kill the agent or whatever
		}
	}
}
