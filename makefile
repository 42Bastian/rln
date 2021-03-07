#
# Makefile for Renamed Linker for Jaguar
#
RM =/bin/rm -f
CC = gcc
PROGNAME = rln
LIBS =
#
# Figure out which system we're compiling for, and set the appropriate
# variables
#
SYSTYPE    := __GCCUNIX__

ifneq "$(CROSS)" ""
SYSTYPE    := WIN32
else
OSTYPE := $(shell uname -o)
ifeq "$(findstring Msys,$(OSTYPE))" "Msys"
SYSTYPE    := WIN32
endif
endif

CFLAGS = -g -I. -D$(SYSTYPE) -O2 -Wno-format
SRCS = rln.c
OBJS = rln.o

.c.o:
	$(RM) $@
	$(CROSS)$(CC) $(CFLAGS) -c $*.c

all: $(PROGNAME)

$(PROGNAME) : $(OBJS)
	$(CROSS)$(CC) $(CFLAGS) -o $(PROGNAME) $(OBJS) $(LIBS)

clean:
	$(RM) $(OBJS) $(PROGNAME) $(PROGNAME).exe *~

