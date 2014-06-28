package forth

// List of VM traps for Errno
const (
	Bye = Errno(iota)
	EOF
	StackOverflow
	StackUnderflow
	RStackOverflow
	RStackUnderflow
	IllegalInstruction
	IllegalAddress
	UnalignedAddress
	ZeroDivision
	IOError
)

var strError = []string{
	"BYE",
	"EOF",
	"stack overflow",
	"stack underflow",
	"return stack overflow",
	"return stack underflow",
	"illegal instruction",
	"illegal address",
	"unaligned address",
	"zero division",
	"I/O error",
}

// Errno describes the reason for a VM trap.
type Errno int

func (e Errno) Error() string {
	return strError[e]
}

func rstackError(e error) error {
	if e == nil {
		return nil
	}
	return e.(Errno) + (RStackOverflow - StackOverflow)
}

// Error describes the cause and the context of a VM trap.
type Error struct {
	Errno  Errno // nature of the trap
	Err    error // I/O error when Errno is IOError
	PC     Cell  // program counter before the trap
	Instr  Instr // instruction that raised the trap
	Addr   Cell  // address when Errno is IllegalAddress or UnalignedAddress
	Stack  Stack // data stack
	RStack Stack // return stack
}

func (e *Error) Error() string {
	var msg = "forego: "
	if e.Err != nil {
		msg += e.Err.Error()
	} else {
		msg += e.Errno.Error()
		switch e.Errno {
		case IllegalInstruction:
			msg += " " + Cell(e.Instr).String()
		case IllegalAddress, UnalignedAddress:
			msg += " " + e.Addr.String()
		}
	}
	return msg + " at " + e.PC.String()
}

func (vm *VM) newErrorFull(errno Errno, err error, addr Cell) error {
	return &Error{
		Errno:  errno,
		Err:    err,
		PC:     vm.lastpc,
		Addr:   addr,
		Instr:  vm.icell,
		Stack:  append(make([]Cell, 0, stackDepth), vm.stack...),
		RStack: append(make([]Cell, 0, stackDepth), vm.rstack...),
	}
}

func (vm *VM) newErrorAddr(errno Errno, addr Cell) error {
	return vm.newErrorFull(errno, nil, addr)
}

func (vm *VM) newError(errno Errno) error {
	return vm.newErrorAddr(errno, 0)
}

func (vm *VM) newIOError(e error) error {
	return vm.newErrorFull(IOError, e, 0)
}
