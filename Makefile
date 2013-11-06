# Copyright 2011 Vadim Vygonets. All rights reserved.
# Use of this source code is governed by the Bugroff
# license that can be found in the LICENSE file.

TARG	= forego
KERNEL	= forth/kern.new
AS	= as/kernel.4as
KSRC	= forth/boot.4th

all:
	go build

kernel: $(KERNEL)

$(KERNEL): $(KSRC)
	./bootstrap/bootstrap <$(KSRC) >$(KERNEL)
