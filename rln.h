//
// RLN - Renamed Linker for the Atari Jaguar console system
// Copyright (C) 199x Allan K. Pratt, 2011-2021 Reboot & Friends
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
#define MINOR   7			// Minor version number
#define PATCH   4			// Patch release number

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
#define MAXARGS      4096			// Max number of args in a command file

// Headers

// Most of these structures reflect the actual format of the object in
// question, on a 68000: char means one byte, int means two bytes, long means
// four. If the host machine doesn't have this same format (like a VAX), you
// will have to read the file into a buffer and stuff the values into the
// structure (see slongio.c).

// Rather than rely on dodgy compilers for something that's now a C99 standard,
// let's do this:
#include <stdint.h>
#include <dirent.h>

// Alcyon object file header structures.
//
// Same as an Atari ST/GEMDOS/TOS executable file header.
//
// References:
//  http://cd.textfiles.com/ataricompendium/BOOK/HTML/CHAP2.HTM#processes
//  https://mikro.naprvyraz.sk/docs/GEM/GEMDOS.TXT
//  MADMAC source from Landon Dyer (See below for URL)
//
// Note the above disagree on the last entry in the header layout: In practice
// the files MADMAC produces and that ALN consumes use the header layout from
// the Atari Compendium page.
struct ALCHEADER
{
	uint16_t magic;					// $601A
	uint32_t tsize;					// text segment size
	uint32_t dsize;					// data segment size
	uint32_t bsize;					// BSS segment size
	uint32_t ssize;					// symbol table size
	uint32_t reserved0;				// unused
	uint32_t reserved1;				// unused
	uint16_t absflag;				// Always '0' (relocatable) for obj files
};

// Alcyon/DRI symbol type bits - From size.c:show_dri_symbol_type() in
// https://github.com/cubanismo/jag_utils, as well as
// the GEMDOS Reference Manual, 4/4/86, "Executable Files" section, in the
// table "Values For Symbol Types," available various places, e.g.,
// https://mikro.naprvyraz.sk/docs/GEM/GEMDOS.TXT
#define ALCSYM_DEFINED		0x8000
#define ALCSYM_EQUATED		0x4000
#define ALCSYM_GLOBAL		0x2000
#define ALCSYM_EQUATED_REG	0x1000
#define ALCSYM_EXTERN		0x0800
#define ALCSYM_DATA			0x0400
#define ALCSYM_TEXT			0x0200
#define ALCSYM_BSS			0x0100

// Alcyon/DRI symbol relocation flags - Derived from mark.c in the
// original MADMAC sources released by Landon Dyer on his blog:
//
// https://dadhacker-125488.ingress-alpha.ewp.live/
//
// At this link from his September 2nd, 2008 post:
//
// http://www.dadhacker.com/Downloads/madmac.zip
//
// Downloaded from the Internet Archive Wayback Machine November 2015
// snapshot of the above URL, here:
//
// https://web.archive.org/web/20151120225539/http://www.dadhacker.com/Downloads/madmac.zip
//
// Another valuable source of information on the Alcyon relocation data,
// and the Alcyon/DRI C object file format in general is the Sozobon 2.0
// source code:
//
// https://sourceforge.net/projects/sozobon/files/
//
// Sozobon is a reimplementation of the original Atari Alcyon C compiler
// suite that can generate and link Alcyon/DRI object files compatible
// with the original. Take a look at the jas/cpy.c file for an alternate
// implementation of Alcyon/DRI relocation section data generation than
// the one in MADMAC, as well as an alternate Alcyon/DRI relocation
// data processing implementation in ld/rel.c.
#define ALCREL_ABS			0x0000	// No relocation at this location
#define ALCREL_DATA			0x0001	// Local address from data segment
#define ALCREL_TEXT			0x0002	// Local address from text segment
#define ALCREL_BSS			0x0003	// Local address from BSS segment
#define ALCREL_EXTABS		0x0004	// External fixup: Absolute address
#define ALCREL_LONG			0x0005	// Relocation type is in next word
#define ALCREL_EXTPCREL		0x0006	// External fixup: PC relative address
#define ALCREL_SYMIDX(rval)	((rval) >> 3)	// 0-based index of ext fixup symbol

struct ALCSYM
{
	uint8_t name[8];				// fixed-size, padded with zeros.  NOT NUL-terminated!
	uint16_t type;					// symbol type mask, from ALCSYM_* flags above.
	uint32_t value;					// value
};

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

// BSD/a.out object relocation flags.
#define BSDREL_MOVEI		0x00000001	// Word-swapped (I.e., JRISC movei) long
#define BSDREL_WORD			0x00000002
#define BSDREL_OP			0x00000004	// Object Processor relocation
#define BSDREL_GLOBAL		0x00000010	// AKA external reference
#define BSDREL_ABS			0x00000040
#define BSDREL_PCREL		0x000000A0	// Note this implies BSDREL_GLOBAL
#define BSDREL_SYMIDX_SHIFT	8			// Bits to shift
#define BSDREL_SYMIDX(_rf)	((_rf) >> BSDREL_SYMIDX_SHIFT)
#define BSDREL_SEGMASK		0xFFFFFF00
#define BSDREL_SEG_ABS		0x00000200
#define BSDREL_SEG_TEXT		0x00000400
#define BSDREL_SEG_DATA		0x00000600
#define BSDREL_SEG_BSS		0x00000800

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

// SYMREC: Used by the linker for the output symbol table

#define OST_SIZE_INIT 8				// Half the initial output symbol table size

struct SYMREC
{
	uint32_t s_idx;
	uint32_t s_type;
	uint32_t s_value;
};

#define new_symrec() (struct SYMREC *)malloc(sizeof(struct SYMREC))

// Hash Record

// HREC: One item in a hash bucket, including a link to the next item. Commons
// and Globals share a hash table, but their value fields are interpreted
// differently.

#define SYMLEN       256                       // Symbol name size (incl. null)

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

