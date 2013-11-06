// Copyright 2013 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

/*
Usage:
	./bootstrap <../forth/boot.4th >../forth/kern.go
*/
package main

import (
	"fmt"
	"forego/forth"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

func main() {
	code, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		log.Fatalln(err)
	}
	if len(code) > forth.MemSize/4 {
		log.Fatalln("code size must be less than",
			forth.MemSize/4, "bytes.  sorry.")
	}
	var (
		addr = forth.MemSize - 0x200 - len(code)
		eval = fmt.Sprintf("%d %d evaluate\n", addr, len(code))
		dump = "0 here dump2go bye\n"
		in   = strings.NewReader(eval + eval + dump)
		vm   = forth.NewVM(in, os.Stdout)
	)
	copy(vm.Mem[addr:], code)
	vm.Run()
}
