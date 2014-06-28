# Copyright 2011 Vadim Vygonets. All rights reserved.
# Use of this source code is governed by the Bugroff
# license that can be found in the LICENSE file.

KERNEL	= forth/kern.new
KSRC	= forth/boot.4th

all:
	go build

kernel: $(KERNEL)

$(KERNEL): $(KSRC)
	go run ./bootstrap/bootstrap.go <$(KSRC) >$(KERNEL)

.PHONY: all kernel $(KERNEL)
