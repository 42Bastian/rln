rm=/bin/rm -f 
CC= cc 
PROGNAME= rln 
LIBS= 

CFLAGS= -g -I. -D__GCCUNIX__

SRCS = rln.c 

OBJS = rln.o

.c.o: 
	$(rm) $@ 
	$(CC) $(CFLAGS) -c $*.c 

all: $(PROGNAME) 

$(PROGNAME) : $(OBJS) 
	$(CC) $(CFLAGS) -o $(PROGNAME) $(OBJS) $(LIBS) 

clean: 
	$(rm) $(OBJS) $(PROGNAME) core *~ 
