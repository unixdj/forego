# Copyright 2011 Vadim Vygonets. All rights reserved.
# Use of this source code is governed by the Bugroff
# license that can be found in the LICENSE file.

TARG	= forego
KERNEL	= forth/kern.go
AS	= as/kernel.4as

all: $(KERNEL)
	go build

$(KERNEL): $(AS)
	cd as && go build
	./as/as <$(AS) >$(KERNEL)
