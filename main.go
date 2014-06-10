// Copyright 2011 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

package main

import (
	"github.com/unixdj/forego/forth"
	"os"
)

func main() {
	forth.NewVM(os.Stdin, os.Stdout).Run()
}
