# Makefile for Reboot's Linker for Jaguar

RM =/bin/rm -f 
CC = gcc 
PROGNAME = rln 
LIBS = 

# Figure out which system we're compiling for, and set the appropriate variables

OSTYPE := $(shell uname -a)

ifeq "$(findstring Msys,$(OSTYPE))" "Msys"			# Win32

OSDEFINE   := WIN32

else											# ???

OSDEFINE   := __GCCUNIX__

endif


# Change this to -DWIN32 for Windows :-)
#CFLAGS = -g -I. -D__GCCUNIX__
#CFLAGS = -g -I. -DWIN32
CFLAGS = -g -I. -D$(OSDEFINE)

SRCS = rln.c 

OBJS = rln.o

.c.o: 
	$(RM) $@ 
	$(CC) $(CFLAGS) -c $*.c 

all: $(PROGNAME) 

$(PROGNAME) : $(OBJS) 
	$(CC) $(CFLAGS) -o $(PROGNAME) $(OBJS) $(LIBS) 

clean: 
	$(RM) $(OBJS) $(PROGNAME) *~ 
