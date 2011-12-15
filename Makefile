include $(GOROOT)/src/Make.inc

TARG=forego
GOFILES=main.go
DIRS=forth
PREREQ=$(addsuffix .install, $(DIRS))

%.install:
	$(MAKE) -C $* install

include $(GOROOT)/src/Make.cmd
