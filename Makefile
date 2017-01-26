# path to the emacs source dir

CSOUND = /home/hlolli/csound/csound/include/
ROOT    = /usr/local/share/emacs/25.1/src
CC      = gcc
LD      = gcc
CFLAGS  = -ggdb3 -Wall
LDFLAGS = -lcsound64


all: emacscsnd.so
%.so: %.o
	$(LD) -shared $(LDFLAGS) -o $@ $<
%.o: %.c
	$(CC) $(CFLAGS) -I$(CSOUND) -I$(ROOT) -I. -fPIC -c $<

install: emacscsnd.so
	cp emacscsnd.so /usr/lib64/

uninstall:
	rm /usr/lib64/emacscsnd.so

clean:
	rm emacscsnd.so
