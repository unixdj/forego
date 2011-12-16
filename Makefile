# Copyright 2011 Vadim Vygonets. All rights reserved.
# Use of this source code is governed by the Bugroff
# license that can be found in the LICENSE file.

include $(GOROOT)/src/Make.inc

TARG=forego
GOFILES=main.go
DIRS=forth
PREREQ=$(addsuffix .install, $(DIRS))

%.install:
	$(MAKE) -C $* install

include $(GOROOT)/src/Make.cmd
