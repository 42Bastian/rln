//
// RLN - Reboot's Linker for the Atari Jaguar console system
// Copyright (C) 199x Allan K. Pratt, 2011-2018 Reboot & Friends
//

#ifndef __RLN_H__
#define __RLN_H__


#ifdef WIN32
//#define _OPEN_FLAGS  _O_BINARY|_O_RDWR
#define _OPEN_FLAGS  _O_BINARY|_O_RDONLY
#define PATH_DELIMITER   '\\'
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

#ifdef __GCCUNIX__
//#define _OPEN_FLAGS  O_RDWR
#define _OPEN_FLAGS  O_RDONLY
#define PATH_DELIMITER   '/'
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

#define MAJOR   1			// Major version number
#define MINOR   6			// Minor version number
#define PATCH   1			// Patch release number

#ifdef WIN32
#define PLATFORM     "Win32"		// Release platform - Windows
#else
#ifdef __GCCUNIX__
#define PLATFORM     "OSX/Linux"	// Release platform - MAC OSX or Linux
#else
#define PLATFORM     "Unknown"		// Release platform - Not Specified
#endif
#endif

// Command link flag, warning macro
#define warn(x, f) printf("Warning: repeated flag `%c'%s\n", x, f ? "; previous one(s) ignored." : ".")

// Macro to swap the 16-bit words of a 32-bit integer
#define _SWAPWORD(x) (((uint32_t)(x) >> 16) | ((uint32_t)(x) << 16))

#define FARGSIZE     1024			// Number of chars in filename argument
#define FNLEN        1024			// Size of a file name
#define NHANDLES     256			// Number of open file handles at once
#define OST_BLOCK    0x400000		// Output symbol table block (4MB)
#define DSTSEG_D     1				// Include file destination seg (DATA)
#define DSTSEG_T     2				// Include file destination seg (TEXT)
#define MAXARGS      256			// Max number of args in a command file

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
	uint32_t magic;					// $0107 for .o, $601B for .abs
	uint32_t tsize;
	uint32_t dsize;
	uint32_t bsize;
	uint32_t ssize;
	union {
		struct {					// For .o
			uint32_t tsize;			// Text relocation size
			uint32_t dsize;			// Data relocation size
			uint8_t reserved[12];
		} reloc;
		struct {					// For .abs
			uint32_t stksize;		// Unused
			uint32_t tstart;		// Start of TEXT
			uint32_t rbflag;		// -1 if no fixups at all
			uint32_t dstart;		// Start of DATA
			uint32_t bstart;		// Start of BSS
		} abs;
	} absrel;
	uint8_t * ostbase;				// Base of output symbol table
	uint32_t fsize;					// Length of fixups
	uint8_t * fixups;				// Start of fixups
};

#define new_oheader()   (struct OHEADER *)malloc(sizeof(struct OHEADER))

struct ARHEADER
{
	uint8_t a_fname[14];
	uint32_t a_modti;
	uint8_t a_userid;
	uint8_t a_gid;
	uint16_t a_fimode;
	uint32_t a_fsize;
	uint16_t reserved;				// Two bytes zeroes btwn header & file
};

#define new_arheader()  (struct ARHEADER *)malloc(sizeof(struct ARHEADER))

// Object file structure and related items

enum { TEXT=0, DATA=1, BSS=2 };

struct OFILE
{
	uint8_t o_name[FNLEN];				// Fixed-length names
	uint8_t o_arname[FNLEN];			// Name of archive this is from
	struct OFILE * o_next;				// Next object file
	uint32_t o_tbase, o_dbase, o_bbase;	// Computed bases for this ofile
	uint16_t o_symstart;				// First sym in image is nth in out
	uint16_t o_flags;					// Flags (see O_*)
	struct OHEADER o_header;			// Header of this file
	uint8_t * o_image;					// Image of this file
	uint8_t isArchiveFile;				// Temporary extra flag
//These are likely redundant, and can probably be removed with judicious
//editing of where they are used (in favor of OHEADER vars)
	uint32_t segSize[3];				// Size of TEXT, DATA & BSS (aligned)
	uint32_t segBase[3];				// Accumulated base address of TDB
};

#define new_ofile()  (struct OFILE *)malloc(sizeof(struct OFILE))

// Flags in an Object File's o_flags field
// O_USED: means this ofile is used or is on the command line or in a -x
#define O_USED       0x0001
// N.B.: This is *never* set anywhere in the linker code...
#define O_ARCHIVE    0x0002			// This is a dummy archive entry

// Symbol Record

// SYMREC: Used by builddir for the lists of exports and imports, and by the
// linker for the output symbol table (that's why there are type and value
// fields, unused in builddir)

#define SYMLEN       100			// Symbol name size (incl. null)

struct SYMREC
{
	uint8_t s_name[SYMLEN];			// Including null terminator
	uint16_t s_type;
	uint32_t s_value;
	struct SYMREC * s_next;
};

#define new_symrec() (struct SYMREC *)malloc(sizeof(struct SYMREC))

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

#define new_hrec()   (struct HREC *)malloc(sizeof(struct HREC))

#define NBUCKETS     1024   // Number of hash buckets

// Bit definitions for the type field of a symbol.
//
// There is a special case for the linker here: the bit T_OST is a flag
// meaning that the symbol's value field contains an INT (in host-machine
// format, in the first two bytes of the field) which is the index of the
// symbol in the output symbol table.
//
// If that field is -1, it means you have to look this symbol up in the OST to
// get its index. This happens when the symbol was extern to this module and
// defined by a LATER module.
//
// The upshot is that a symbol which isn't in the OST has its type & value
// fields intact, while a symbol which is in the OST has ABST_OST set and its
// index in its value field (or -1 if you have to look it up).
// When producing the output fixups, you either output a symbol fixup with the
// new index (for a partial link), or resolve the symbol based on its type &
// value from the output symbol table.

#define ABST_DEFINED    0x8000
#define ABST_EQUATED    0x4000
#define ABST_GLOBAL     0x2000
#define ABST_REGISTER	0x1000
#define ABST_EXTERN     0x0800
#define ABST_DATA       0x0400	/* data-based relocatable */
#define ABST_TEXT       0x0200	/* text-based relocatable */
#define ABST_BSS        0x0100	/* bss-based relocatable  */
#define ABST_FILE       0x0080	// file symbol
#define ABST_ARCHIVE    0x0040	// only when FILE set: archive file or no
#define ABST_OST        0x0001	// private: "symbol is in OST": see above
//#define T_COMMON	(T_GLOBAL | T_EXTERN)

// Symbol Table - Type Definitions
// N.B.: T_GLBL can be ORed with any of T_ABS, T_TEXT, TDATA, or T_BSS!
//       Also, these are really a mashup of a struct, consisting of the
//       following items: type (1 byte), other (1 byte), & descr. (2 bytes).
//       Also, the type is not enough to distinguish between external &
//       common symbols; for this,  you need to go to the value field to see
//       what's there (0=external, !0=common).

#define T_UNDF		0x00000000     // Undefined symbol
#define T_GLBL		0x01000000     // Scoping bit, OR'ed in (global)
#define T_ABS		0x02000000     // Absolute symbol (equated)
#define T_TEXT		0x04000000     // TEXT segment
#define T_DATA		0x06000000     // DATA segment
#define T_BSS		0x08000000     // BSS segment
#define T_SEG		(T_DATA | T_TEXT | T_BSS)   // segment bits

// These macros are used with the TYPE field of a SYMBOL.
// They are also mostly WRONG
/*
Absolutes (equates) can't be externals (line 434)
-- they are non-relocatable
*/

#define iscommon(type) (((type) & T_GLBL) == 0)
#define islocal(type)  (((type) & T_GLBL) == 0)
#define isglobal(type) ((type) & T_GLBL)
#define isextern(type) ((type) & T_GLBL)

/*
Shamus:
Just look at this. I can't believe that somebody actually wrote this piece of
failure and thought it was a good idea. I'm leaving it here as a testament to
complete, total, and utter failure. :-)
*/

// This macro is used to compare two symbols for equality. It depends on
// symcopy remaining as it is (copies two longs plus a null)

//#define symcmp(a,b) ((*(long *)(a) == *(long *)(b)) && \
//					(*(long *)((a) + sizeof(long)) == \
//					*(long *)((b) + sizeof(long))))

#endif // __RLN_H__

