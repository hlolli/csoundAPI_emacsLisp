# path to the emacs source dir

CSOUND  = $(wildcard /usr/*/include/csound)
EMACS   = $(wildcard /usr/*/share/emacs/*/src)
CC      = gcc
LD      = gcc
CFLAGS  = -ggdb3 -Wall
LDFLAGS = -lcsound64


all: emacscsnd.so
%.so: %.o
	$(LD) -shared $(LDFLAGS) -o $@ $<
%.o: %.c
	$(CC) $(CFLAGS) -I$(CSOUND) -I$(EMACS) -I. -fPIC -c $<
