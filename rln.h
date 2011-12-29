//
// RLN - Reboot's Linker for the Atari Jaguar Console System
// RLN.H - Application Header
// Copyright (C) 199x Allan K. Pratt, 2011 Reboot & Friends
//

#ifndef __RLH_H__
#define __RLH_H__

// Required Include Files

// Macro Definitions

// Requirements for Windows Compilation

#ifdef WIN32
//#define _OPEN_FLAGS  _O_BINARY|_O_RDWR
#define _OPEN_FLAGS  _O_BINARY|_O_RDONLY
#define _BACKSLASH   '\\'
#ifdef _MSC_VER
   #if _MSC_VER > 1000
      #pragma warning(disable:4996)
   #endif
#endif
#include <io.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <setjmp.h>
#endif

// Requirements for Mac OS-X or Linux Compilation

#ifdef __GCCUNIX__
//#define _OPEN_FLAGS  O_RDWR
#define _OPEN_FLAGS  O_RDONLY
#define _BACKSLASH   '/'
#include <sys/fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <setjmp.h>
#include <unistd.h>
#endif

#define MAJOR        1                          // Major version number
#define MINOR        1                          // Minor version number
#define PATCH        0                          // Patch release number

#ifdef WIN32
#define PLATFORM     "Win32"                    // Release platform - Windows
#else
#ifdef __GCCUNIX__
#define PLATFORM     "OSX/Linux"                // Release platform - MAC OSX or Linux
#else
#define PLATFORM     "Unknown"                  // Release platform - Not Specified
#endif
#endif

// Command link flag, warning macro
#define warn(x, f) printf("Warning: repeated flag `%c'%s\n", x, f ? "; previous one(s) ignored." : ".")

// Macro for max: good because longs, shorts, or pointers can be compared
#ifndef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#endif	// max

// Macro to swap the 16-bit words of a 32-bit integer
#define _SWAPWORD(x) (((unsigned)(x) >> 16) | ((unsigned)(x) << 16))

#define FARGSIZE     1024                       // Number of chars in filename argument
#define FNLEN        1024                       // Size of a file name
#define NHANDLES     256                        // Number of open file handles at once
#define OST_BLOCK    0x400000                   // Output symbol table block (4MB)
#define DSTSEG_D     1                          // Include file destination seg (DATA)
#define DSTSEG_T     2                          // Include file destination seg (TEXT)
#define MAXARGS 256                             // Max number of args in a command file

// Headers

// Most of these structures reflect the actual format of the object in
// question, on a 68000: char means one byte, int means two bytes, long means
// four. If the host machine doesn't have this same format (like a VAX), you
// will have to read the file into a buffer and stuff the values into the
// structure (see slongio.c).

// Rather than rely on dodgy compilers for something that's now a C99 standard,
// let's do this:
#include <stdint.h>

struct OHEADER
{
	uint32_t magic;								// $0107 for .o, $601B for abs
	uint32_t tsize;
	uint32_t dsize;
	uint32_t bsize;
	uint32_t ssize;
	union {
		struct {								// For .o 
			uint32_t tsize;						// Text relocation size
			uint32_t dsize;						// Data relocation size
			uint8_t reserved[12];
		} reloc;
		struct {								// For .abs 
			uint32_t stksize;					// Unused 
			uint32_t tstart;					// Start of TEXT 
			uint32_t rbflag;					// -1 if no fixups at all 
			uint32_t dstart;					// Start of DATA 
			uint32_t bstart;					// Start of BSS
		} abs;
	} absrel;
	uint8_t * ostbase;							// Base of output symbol table 
	uint32_t fsize;								// Length of fixups
	uint8_t * fixups;							// Start of fixups 
};

#define new_oheader()   (struct OHEADER *)malloc((uint32_t)sizeof(struct OHEADER))

struct ARHEADER
{
	uint8_t a_fname[14];
	uint32_t a_modti;
	uint8_t a_userid;
	uint8_t a_gid;
	uint16_t a_fimode;
	uint32_t a_fsize;
	uint16_t reserved;							// Two bytes zeroes btw header & file 
};

#define new_arheader()  (struct ARHEADER *)malloc((uint32_t)sizeof(struct ARHEADER))

// Object File Structure and Related Items

struct OFILE
{
	uint8_t o_name[FNLEN];						// Fixed-length names
	uint8_t o_arname[FNLEN];					// Name of archive this is from
	struct OFILE * o_next;						// Next object file
	uint32_t o_tbase, o_dbase, o_bbase;			// Computed bases for this ofile
	uint16_t o_symstart;						// First sym in image is nth in out
	uint16_t o_flags;							// Flags (see O_*)
	struct OHEADER o_header;					// Header of this file
	uint8_t * o_image;							// Image of this file
};

#define new_ofile()  (struct OFILE *)malloc((uint32_t)sizeof(struct OFILE))

// Flags in an Object File's o_flags field
// O_USED: means this ofile is used or is on the command line or in a -x
#define O_USED       0x0001
#define O_ARCHIVE    0x0002                     // This is a dummy archive entry

// Symbol Record

// SYMREC: Used by builddir for the lists of exports and imports, and by the
// linker for the output symbol table (that's why there are type and value
// fields, unused in builddir)

#define SYMLEN       100                        // Symbol name size (incl null)

struct SYMREC
{
	uint8_t s_name[SYMLEN];						// Including null terminator 
	uint16_t s_type;
	uint32_t s_value;
	struct SYMREC * s_next;
};

#define new_symrec() (struct SYMREC *)malloc((uint32_t)sizeof(struct SYMREC))

// Hash Record

// HREC: One item in a hash bucket, including a link to the next item. Commons
// and Globals share a hash table, but their value fields are interpreted
// differently.

struct HREC
{
	uint8_t h_sym[SYMLEN];
	struct HREC * h_next;
	struct OFILE * h_ofile;
	uint32_t h_value;
//Shamus: This was an "int" but as per above, should have been a 16-bit value.
//        Changing it to a 32-bit value (as per compiler warning).
	uint32_t h_type;
};

#define new_hrec()   (struct HREC *)malloc((uint32_t)sizeof(struct HREC))

#define NBUCKETS     1024                       // Number of hash buckets

// Bit definitions for the type field of a symbol.
//
// There is a special case for the linker here: the bit T_OST is a flag
// meaning that the symbol's value field contains an INT (in host-machine
// format, in the first two bytes of the field) which is the index of the
// symbol in the output symbol table.
//
// If that field is -1, it means you have to look this symbol up in the
// ost to get its index.  This happens when the symbol was extern to this
// module and defined by a LATER module.
//
// The upshot is that a symbol which isn't in the ost has its type & value
// fields intact, while a symbol which is in the ost has T_OST set and
// its index in its value field (or -1 if you have to look it up).
// When producing the output fixups, you either output a symbol fixup with
// the new index (for a partial link), or resolve the symbol based on its
// type & value from the output symbol table.

#define ABST_DEFINED    0x8000
#define ABST_EQUATED    0x4000
#define ABST_GLOBAL     0x2000
#define ABST_REGISTER	0x1000
#define ABST_EXTERN     0x0800
#define ABST_DATA       0x0400	/* data-based relocatable */
#define ABST_TEXT       0x0200	/* text-based relocatable */
#define ABST_BSS        0x0100	/* bss-based relocatable  */
#define ABST_FILE       0x0080                  // file symbol
#define ABST_ARCHIVE    0x0040                  // only when FILE set: archive file or no
#define ABST_OST        0x0001                  // private: "symbol is in ost": see above
#define T_COMMON	(T_GLOBAL | T_EXTERN)
#define T_SEG		(T_DATA | T_TEXT | T_BSS)   // segment bits

// Symbol Table - Type Definitions

#define T_UNDF          0x00000000     // Undefined Symbol
#define T_EXT           0x01000000     // External Bit, OR'ed In
#define T_ABS           0x02000000     // Absolute Symbol
#define T_TEXT          0x04000000     // TEXT Segment
#define T_DATA          0x06000000     // DATA Segment
#define T_BSS           0x08000000     // BSS Segment

// These macros are used with the TYPE field of a SYMBOL.

#define iscommon(type) (((type) & T_EXT) == T_EXT)
#define isglobal(type) (((type) & T_EXT) == T_EXT)
#define isextern(type) (((type) & T_EXT) == T_EXT)
#define islocal(type)  (((type) & T_EXT) == 0)

/*
Shamus:
Just look at this. I can't believe that somebody actually wrote this piece of
failure and thought it was a good idea. I'm leaving it here as a testament to
complete, total, and utter failure. :-)
*/

// This macro is used to compare two symbols for equality. It depends on
// symcopy remaining as it is (copies two longs plus a null)

//#define symcmp(a,b) ((*(uint32_t *)(a) == *(uint32_t *)(b)) && \
//					(*(uint32_t *)((a) + sizeof(uint32_t)) == \
//					*(uint32_t *)((b) + sizeof(uint32_t))))

// Function Prototypes

int doargs(int, char *[]);
char * make_string(char *);
void put_name(struct OFILE *);
int flush_handles(void);
void symcopy(char *, char *);
int ost_add(char *,int, long);
int add_fixup(long);
void display_help(void);
void display_version(void);
int pladd(char *, char *);
char * path_tail(char *);
int dolist(void);
int segmentpad(FILE *, long, int);
int ost_lookup(char *);

#endif // __RLH_H__
