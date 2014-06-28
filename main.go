// Copyright 2011 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

/*
Forego is a FORTH system.

Forego runs the Forego virtual machine with the default kernel
using stdin and stdout for I/O.  Unless the VM stops due to BYE
being executed, Forego prints the trap description on stderr and
exits with error code 1.
*/
package main

import (
	"fmt"
	"os"

	"github.com/unixdj/forego/forth"
)

func main() {
	if err := forth.NewVM(os.Stdin, os.Stdout).Run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		if fe := err.(*forth.Error); fe.Errno != forth.EOF {
			fmt.Fprintf(os.Stderr,
				"instruction: %v <%v>\nstack: %v\nrstack: %v\n",
				fe.Instr, forth.Cell(fe.Instr),
				fe.Stack, fe.RStack)
		}
		os.Exit(1)
	}
}
