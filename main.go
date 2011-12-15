package main

import (
	"./forth"
	"os"
)

func main() {
	forth.NewVM(os.Stdin, os.Stdout).Run()
}
