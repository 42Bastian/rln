# Makefile for Reboot's Linker for Jaguar

RM =/bin/rm -f 
CC = gcc 
PROGNAME = rln 
LIBS = 

# Change this to -DWIN32 for Windows :-)
CFLAGS = -g -I. -D__GCCUNIX__
#CFLAGS = -g -I. -DWIN32

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
