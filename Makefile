# path to the emacs source dir

CSOUND = /home/hlolli/csound/csound/include/
ROOT    = /usr/local/share/emacs/25.1/src
CC      = gcc
LD      = gcc
CFLAGS  = -ggdb3 -Wall
LDFLAGS = -lcsound64


all: csnd.so

# make shared library out of the object file
%.so: %.o
	$(LD) -shared $(LDFLAGS) -o $@ $<

# compile source file to object file
%.o: %.c
	$(CC) $(CFLAGS) -I$(CSOUND) -I$(ROOT) -I. -fPIC -c $<
