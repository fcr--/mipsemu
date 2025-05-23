export STAGING_DIR := /home/fran/src/onioniot-build/staging_dir
export PATH := $(STAGING_DIR)/toolchain-mipsel_24kc_gcc-8.4.0_musl/bin:$(PATH)

CFLAGS += -Os -fomit-frame-pointer -march=24kc
GCC_PREFIX ?= mipsel-openwrt-linux-musl
CC = $(GCC_PREFIX)-gcc

ALL = test_basic1_nostdlib \
      test_calc \
      test_hello \
      test_hello_asm \
      test_hello_printf
.PRECIOUS: $(ALL)  # <3
all: $(addsuffix .hex, $(ALL))

%.hex: %
	od -A n -v -tx1 $^ | sed 's/^ *//' > $@

# comment the strip commands if you need debug info

%_nostdlib: %_nostdlib.c
	$(CC) $^ -o $@ -nostdlib $(CFLAGS)
	#strip --strip-section-headers $@

%: %.c
	$(CC) $^ -o $@ -static $(CFLAGS)
	#strip --strip-section-headers $@

%: %.S
	$(CC) $^ -o $@ -nostdlib $(CFLAGS)
	#strip --strip-section-headers $@

%.S: %.c
	$(CC) $^ -o $@ -S $(CFLAGS)

.PHONY: clean all tests
clean: 
	rm -f $(ALL) *.hex $(addsuffix .S, $(basename $(wildcard *.c)))

tests: all
	awk -ftest_calc.awk -vname=$(name)
