//
// RLN - Reboot's Linker for the Atari Jaguar Console System
// RLN.C - Application Code
// Copyright (C) 199x, Allan K. Pratt, 2014 Reboot & Friends
//

#include "rln.h"

unsigned errflag = 0;				// Error flag, goes TRUE on error
unsigned waitflag = 0;				// Wait for any keypress flag
unsigned versflag = 0;				// Version banner has been shown flag
unsigned aflag = 0;					// Absolute linking flag
unsigned bflag = 0;					// Don't remove mulitply def locals flag
unsigned cflag = 0;					// COF executable
unsigned dflag = 0;					// Wait for key after link flag
unsigned gflag = 0;					// Source level debug include flag
unsigned lflag = 0;					// Add local symbols to output flag
unsigned mflag = 0;					// Produce symbol load map flag
unsigned oflag = 0;					// Output filename specified
unsigned rflag = 0;					// Segment alignment size flag
unsigned sflag = 0;					// Output only global symbols
unsigned vflag = 0;					// Verbose flag
unsigned wflag = 0;					// Show warnings flag
unsigned zflag = 0;					// Suppress banner flag
unsigned pflag, uflag;				// Unimplemented flags
unsigned hd = 0;					// Index of next file handle to fill
unsigned secalign = 7;				// Section Alignment (8=phrase)
unsigned tbase = 0;					// TEXT base address
unsigned dbase = 0;					// DATA base address
unsigned bbase = 0;					// BSS base address
unsigned textoffset = 0;			// COF TEXT segment offset
unsigned dataoffset = 0;			// COF DATA segment offset
unsigned bssoffset = 0;				// COF BSS segment offset
unsigned displaybanner = 1;			// Display version banner
unsigned symoffset = 0;				// Symbol table offset in output file
unsigned dosymi = 0;				// Dosym() processing iterator
unsigned dbgsymbase = 0;			// Debug symbol base address
//unsigned symtrunc = 0;			// Symbol truncation -i and -ii
int noheaderflag = 0;				// No header flag for ABS files
int hflags;							// Value of the arg to -h option
int ttype, dtype, btype;			// Type flag: 0, -1, -2, -3, -4
int tval, dval, bval;				// Values of these abs bases
int hflag[NHANDLES];				// True for include files
int handle[NHANDLES];				// Open file handles
int textsize, datasize, bsssize;	// Cumulative segment sizes
char libdir[FARGSIZE * 3];			// Library directory to search
char ofile[FARGSIZE];				// Output file name (.o)
char * name[NHANDLES];				// Associated file names
char * cmdlnexec = NULL;			// Executable name - pointer to ARGV[0]
char * hsym1[SYMLEN];				// First symbol for include files
char * hsym2[SYMLEN];				// Second symbol for include files
struct OFILE * plist = NULL;		// Object image list pointer
struct OFILE * plast;				// Last object image list pointer
struct OFILE * olist = NULL;		// Pointer to first object file in list
struct OFILE * olast;				// Pointer to last object file in list
char obj_fname[16384][FNLEN];		// Object file names
unsigned obj_segsize[16384][3];		// Object file seg sizes; TEXT,DATA,BSS
unsigned obj_index = 0;				// Object file index/count
char * arPtr[512];
uint32_t arIndex = 0;
struct HREC * htable[NBUCKETS];		// Hash table
struct HREC * unresolved = NULL;	// Pointer to unresolved hash list
char * ost;							// Output symbol table
char * ost_ptr;						// Output symbol table; current pointer
char * ost_end;						// Output symbol table; end pointer
char * oststr;						// Output string table
char * oststr_ptr;					// Output string table; current pointer
char * oststr_end;					// Output string table; end pointer
int ost_index = 0;					// Index of next ost addition
int endian;							// Processor endianess
uint8_t nullStr[1] = "\x00";		// Empty string
struct HREC * arSymbol = NULL;		// Pointer to AR symbol table

// Some human readable defines for endianess
#define ENDIANNESS_BIG     1
#define ENDIANNESS_LITTLE  0

// Function prototypes
struct HREC * LookupHREC(char *);
char * path_tail(char *);
void display_help(void);
void display_version(void);


//
// Get a long word from memory
//
uint32_t getlong(uint8_t * src)
{
	uint32_t temp;
	uint8_t * out = (uint8_t *)&temp;

	if (endian == ENDIANNESS_BIG)
	{
		*out++ = src[0];
		*out++ = src[1];
		*out++ = src[2];
		*out = src[3];
	}
	else
	{
		*out++ = src[3];
		*out++ = src[2];
		*out++ = src[1];
		*out = src[0];
	}

	return temp;
}


//
// Put a long word into memory
//
void putlong(uint8_t * dest, uint32_t val)
{
	*dest++ = (uint8_t)(val >> 24);
	*dest++ = (uint8_t)(val >> 16);
	*dest++ = (uint8_t)(val >> 8);
	*dest = (uint8_t)val;
}


//
// Get a word from memory
//
uint16_t getword(uint8_t * src)
{
	uint16_t temp;
	uint8_t * out = (uint8_t *)&temp;

	if (endian == ENDIANNESS_BIG)
	{
		out[0] = src[0];
		out[1] = src[1];
	}
	else
	{
		out[0] = src[1];
		out[1] = src[0];
	}

	return temp;
}


//
// Put a word into memory
//
void putword(uint8_t * dest, uint16_t val)
{
	*dest++ = (uint8_t)(val >> 8);
	*dest = (uint8_t)val;
}


//
// Find passed in file's length in bytes
// N.B.: This also resets the file's pointer to the start of the file
//
long FileSize(int fd)
{
	long size = lseek(fd, 0, SEEK_END);
	lseek(fd, 0, SEEK_SET);

	return size;
}


//
// For this object file, add symbols to the output symbol table after
// relocating them. Returns TRUE if OSTLookup returns an error (-1).
//
int DoSymbol(struct OFILE * ofile)
{
	int type;					// Symbol type
	long value;					// Symbol value
	int index;					// Symbol index
	int j;						// Iterator
	struct HREC * hptr;			// Hash table pointer for globl/extrn
	char sym[SYMLEN];			// String for symbol name/hash search

	// Point to first symbol record in the object file
	char * symptr = (ofile->o_image + 32
		+ ofile->o_header.tsize
		+ ofile->o_header.dsize
		+ ofile->o_header.absrel.reloc.tsize
		+ ofile->o_header.absrel.reloc.dsize);

	// Point to end of symbol record in the object file
	char * symend = symptr + ofile->o_header.ssize;

	// Search through object segment size table to accumulated segment sizes to
	// ensure the correct offsets are used in the resulting COF file.
	int ssidx = -1;
	unsigned tsegoffset = 0, dsegoffset = 0, bsegoffset = 0;

	// Search for object file name
	for(j=0; j<(int)obj_index; j++)
	{
		if (!strcmp(ofile->o_name, obj_fname[j]))
		{
			// Object file name found
			ssidx = j;
			break;
		}

		// Accumulate segment sizes
		tsegoffset += obj_segsize[j][0];
		dsegoffset += obj_segsize[j][1];
		bsegoffset += obj_segsize[j][2];
	}

	if (ssidx == -1)
	{
		printf("DoSymbol(): Cannot get object file segment size: %s\n", ofile->o_name);
		return 1;
	}

	// Process each record in the symbol table
	for(; symptr!=symend; symptr+=12)
	{
		index = getlong(symptr + 0);	// Obtain symbol string index
		type  = getlong(symptr + 4);	// Obtain symbol type
		value = getlong(symptr + 8);	// Obtain symbol value

		// Global/External symbols have a pre-processing stage
		if (type & 0x01000000)
		{
			// Obtain the string table index for the relocation symbol, look
			// for it in the globals hash table to obtain information on that
			// symbol.
#if 0
			memset(sym, 0, SYMLEN);
			strcpy(sym, symend + index);
			hptr = LookupHREC(sym);
#else
			hptr = LookupHREC(symend + index);
#endif

			if (hptr == NULL)
			{
				printf("DoSymbol() : Cannot determine symbol : %s\n", sym);
				return 1;
			}

			// Search through object segment size table to obtain segment sizes
			// for the object that has the required external/global as a local
			// symbol. As each object is interrogated the segment sizes are
			// accumulated to ensure the correct offsets are used in the
			// resulting COF file.  This is effectively 'done again' only as we
			// are working with a different object file.
			ssidx = -1;
			tsegoffset = dsegoffset = bsegoffset = 0;

			// Search for object filename
			for(j=0; j<(int)obj_index; j++)
			{
				if (!strcmp(hptr->h_ofile->o_name, obj_fname[j]))
				{
					ssidx = j;	// Symbol object filename
					break;
				}

				// Accumulate segment sizes
				tsegoffset += obj_segsize[j][0];
				dsegoffset += obj_segsize[j][1];
				bsegoffset += obj_segsize[j][2];
			}

			if (ssidx == -1)
			{
				printf("DoSymbol() : Cannot get object file segment size : %s\n", ofile->o_name);
				return 1;
			}

			// Update type with global type
			type = hptr->h_type;

			// Remove external flag if absolute
			if (type == (T_EXT | T_ABS))
				type = T_ABS;

			// If the global/external has a value then update that value in
			// accordance with the segment sizes of the object file it
			// originates from
			if (hptr->h_value)
			{
				switch (hptr->h_type & 0x0E000000)
				{
				case T_ABS:
				case T_TEXT:
					value = hptr->h_value;
					break;
				case T_DATA:
					value = hptr->h_value - hptr->h_ofile->o_header.tsize;
					break;
				case T_BSS:
					value = hptr->h_value
						- (hptr->h_ofile->o_header.tsize
						+ hptr->h_ofile->o_header.dsize);
					break;
				}
			}
		}

		// Process and update the value dependent on whether the symbol is a
		// debug symbol or not
		// N.B.: These are currently not supported
		if (type & 0xF0000000)
		{
			// DEBUG SYMBOL
			// Set the correct debug symbol base address (TEXT segment)
			dbgsymbase = 0;

			for(j=0; (unsigned)j<dosymi; j++)
				dbgsymbase += obj_segsize[j][0];

			switch (type & 0xFF000000)
			{
			case 0x64000000:
				value = tval + dbgsymbase;
				break;
			case 0x44000000:
			case 0x46000000:
			case 0x48000000:
				value = tval + dbgsymbase + value;
			default:
				break;
			}

			putlong(symptr + 8, value);
		}
		else
		{
			// NON-DEBUG SYMBOL
			// Now make modifications to the symbol value, local or global,
			// based on the segment sizes of the object file currently being
			// processed.
			switch (type & T_SEG)
			{
			case T_ABS:
				break;
			case T_TEXT:
				value = tbase + tsegoffset + value;
				putlong(symptr + 8, value);
				break;
			case T_DATA:
				if (type & T_EXT)
					value = dbase + dsegoffset + value;
				else
					value = dbase + dsegoffset + (value
						- ofile->o_header.tsize);

				putlong(symptr + 8, value);
				break;
			case T_BSS:
				if (type & T_EXT)
					value = bbase + bsegoffset + value;
				else
					value = bbase + bsegoffset
						+ (value - (ofile->o_header.tsize
						+ ofile->o_header.dsize));

				putlong(symptr + 8, value);
				break;
			default:
				break;
			}
		}

		// Add to output symbol table if global/extern, or local flag is set
		if (lflag || !islocal(type))
		{
			index = OSTAdd(symend + index, type, value);

			if (index == -1)
				return 1;
		}
	}

	// Increment dosymi processsing
	dosymi++;

	return 0;
}


//
// Free up hash memory
//
void FreeHashes(void)
{
	int i;

	for(i=0; i<NBUCKETS; i++)
	{
		struct HREC * hptr = htable[i];

		while (hptr)
		{
			struct HREC * htemp = hptr->h_next;
			free(hptr);
			hptr = htemp;
		}
	}
}


//
// Add all global and external symbols to the output symbol table
// [This is confusing--is it adding globals or locals? common == local!
//  but then again, we see this in the header:
//  #define T_COMMON  (T_GLOBAL | T_EXTERN) but that could be just bullshit.]
//
long DoCommon(void)
{
	struct HREC * hptr;
	int i;

	for(i=0; i<NBUCKETS; i++)
	{
		for(hptr=htable[i]; hptr!=NULL; hptr=hptr->h_next)
		{
//NO!			if (iscommon(hptr->h_type))
			if (isglobal(hptr->h_type))// || isextern(hptr->h_type))
			{
// Skip if in *.a file... (does nothing)
//if (hptr->h_ofile->isArchiveFile)
//	continue;

				if (hptr->h_type == (T_EXT | T_ABS))
					hptr->h_type = T_ABS;	// Absolutes *can't* be externals

				if (OSTAdd(hptr->h_sym, hptr->h_type, hptr->h_value) == -1)
					return -1;
			}
		}
	}

	return 0;
}


//
// Add a Symbol's Name, Type, and Value to the OST.
// Return the Index of the Symbol in OST, or -1 for Error.
//
int OSTAdd(char * name, int type, long value)
{
	int ost_offset_p, ost_offset_e = 0;	// OST table offsets for position calcs
	int slen = 0;						// Symbol string length
	int ostresult;						// OST index result

	slen = strlen(name);

	// If the OST or OST String Table has not been initialised then do so
	if (ost_index == 0)
	{
		if ((ost = malloc(OST_BLOCK)) == NULL)
		{
			printf("OST memory allocation error (stringtable).\n");
			return -1;
		}

		ost_ptr = ost;							// Set OST start pointer
		ost_end = ost + OST_BLOCK;				// Set OST end pointer

		if ((oststr = malloc(OST_BLOCK)) == NULL)
		{
			printf("OST memory allocation error (string).\n");
			return -1;
		}

		putlong(oststr, 0x00000004);		// Just null long for now
		oststr_ptr = oststr + 4;			// Skip size of str table long (incl null long)
		putlong(oststr_ptr, 0x00000000);	// Null terminating long
		oststr_end = oststr + OST_BLOCK;
	}
	else
	{
		// If next symbol record exceeds current allocation then expand symbol
		// table.
		ost_offset_p = (ost_ptr - ost);
		ost_offset_e = (ost_end - ost);

		// 3 x int (12)
		if ((ost_ptr + 12) > ost_end)
		{
#warning "!!! Bad logic in realloc call !!!"
			if ((ost = realloc(ost, (ost_end + OST_BLOCK))) == NULL)
			{
				printf("OST memory reallocation error.\n");
				return -1;
			}

			ost_ptr = ost + ost_offset_p;
			ost_end = (ost + ost_offset_e) + OST_BLOCK;
		}

		ost_offset_p = (oststr_ptr - oststr);
		ost_offset_e = (oststr_end - oststr);

		if ((oststr_ptr + (slen + 1 + 4)) > oststr_end)
		{
#warning "!!! Bad logic in realloc call !!!"
			if ((oststr = realloc(oststr, (oststr_end + OST_BLOCK))) == NULL)
			{
				printf("OSTSTR memory reallocation error.\n");
				return -1;
			}

			oststr_ptr = oststr + ost_offset_p;
			oststr_end = (oststr + ost_offset_e) + OST_BLOCK;
		}
	}

	// If this is a debug symbol and the include debug symbol flag (-g) is not
	// set then do nothing
	if ((type & 0xF0000000) && !gflag)
	{
		// Do nothing
	}
	else
	{
		ostresult = OSTLookup(name);		// Get symbol index in OST

		// If the symbol is in the output symbol table and the bflag is set
		// (don't remove multiply defined locals) and this is not an
		// external/global symbol *** OR *** the symbol is not in the output
		// symbol table then add it.
		if (((ostresult != -1) && bflag && !(type & 0x01000000))
			|| ((ostresult != -1) && gflag && (type & 0xF0000000)) || (ostresult == -1))
		{
			if ((type & 0xF0000000) == 0x40000000)
				putlong(ost_ptr, 0x00000000);	// Zero string table offset for dbg line
			else
				putlong(ost_ptr, (oststr_ptr - oststr));	// String table offset of symbol string

			putlong(ost_ptr + 4, type );
			putlong(ost_ptr + 8, value);
			ost_ptr += 12;

			// If the symbol type is anything but a debug line information
			// symbol then write the symbol string to the string table
			if ((type & 0xF0000000) != 0x40000000)
			{
				strcpy(oststr_ptr, name);		// Put symbol name in string table
				*(oststr_ptr + slen) = '\0';	// Add null terminating character
				oststr_ptr += (slen + 1);
				putlong(oststr_ptr, 0x00000000);	// Null terminating long
				putlong(oststr, (oststr_ptr - oststr));	// Update size of string table
			}

			if (vflag > 1)
			{
				printf("OSTAdd: (%s), type=$%08X, val=$%08X\n", name, type, value);
			}

			return ost_index++;
		}
	}

	// Not sure about this as it could affect return indices. Needed to stop
	// return error.
	return 0;
}


//
// Return the index of a symbol in the output symbol table
//
int OSTLookup(char * sym)
{
	int i;
	int stro = 4;		// Offset in string table

	for(i=0; i<ost_index; i++)
	{
		if (strcmp(oststr + stro, sym) == 0)
			return i + 1;

		stro += strlen(oststr + stro) + 1;
	}

	return -1;
}


//
// Add unresolved externs to the output symbol table
//
int DoUnresolved(void)
{
	struct HREC * hptr = unresolved;

	// Add to OST while unresolved list is valid
	while (hptr != NULL)
	{
		if (OSTAdd(hptr->h_sym, T_EXT, 0L) == -1)
			return 1;

		if (vflag > 1)
			printf("DoUnresolved(): added %s\n", hptr->h_sym);

		struct HREC * htemp = hptr->h_next;
		free(hptr);
		hptr = htemp;
	}

	unresolved = NULL;
	return 0;
}


//
// Update object file TEXT and DATA segments based on relocation records. Take
// in an OFILE header and flag (T_TEXT, T_DATA) to process. Return (0) is
// successful or non-zero (1) if failed.
//
int RelocateSegment(struct OFILE * ofile, int flag)
{
	char * symtab;					// Start of symbol table
	char * symbols;					// Start of symbols
	char * sptr;					// Start of segment data
	char * rptr;					// Start of segment relocation records
	unsigned symidx;				// Offset to symbol
	unsigned addr;					// Relocation address
	unsigned rflg;					// Relocation flags
	unsigned olddata;				// Old segment data at reloc address
	unsigned newdata = 0;			// New segment data at reloc address
	unsigned pad;					// Temporary to calculate phrase padding
	int i;							// Iterator
	char sym[SYMLEN];				// String for symbol name/hash search
	int ssidx;						// Segment size table index
	unsigned glblreloc;				// Global relocation flag
	unsigned absreloc;				// Absolute relocation flag
	unsigned relreloc;				// Relative relocation flag
	unsigned swcond;				// Switch statement condition
	unsigned relocsize;				// Relocation record size

	// If there is no TEXT relocation data for the selected object file segment
	// then update the COF TEXT segment offset allowing for the phrase padding
	if ((flag == T_TEXT) && !ofile->o_header.absrel.reloc.tsize)
	{
		// TEXT segment size plus padding
		pad = ((ofile->o_header.tsize + secalign) & ~secalign);
		textoffset += (ofile->o_header.tsize + (pad - ofile->o_header.tsize));

		if (vflag > 1)
			printf("RelocateSegment(%s, TEXT) : No Relocation Data\n", ofile->o_name);

		return 0;
	}

	// If there is no DATA relocation data for the selected object file segment
	// then update the COF DATA and BSS segment offsets allowing for the phrase
	// padding
	if ((flag == T_DATA) && !ofile->o_header.absrel.reloc.dsize)
	{
		// DATA segment size plus padding
		pad = ((ofile->o_header.dsize + secalign) & ~secalign);
		dataoffset += (ofile->o_header.dsize + (pad - ofile->o_header.dsize));
		// BSS segment size plus padding
		pad = ((ofile->o_header.bsize + secalign) & ~secalign);
		bssoffset += (ofile->o_header.bsize + (pad - ofile->o_header.bsize));

		if (vflag > 1)
			printf("RelocateSegment(%s, DATA) : No Relocation Data\n", ofile->o_name);

		return 0;
	}

	// Verbose mode information
	if (vflag > 1)
	{
		printf("RelocateSegment(%s, %s) : Processing Relocation Data\n",
			ofile->o_name, flag == T_DATA ? "DATA" : "TEXT");
	}

	// Obtain pointer to start of symbol table
	symtab = (ofile->o_image + 32 + ofile->o_header.tsize
		+ ofile->o_header.dsize
		+ ofile->o_header.absrel.reloc.tsize
		+ ofile->o_header.absrel.reloc.dsize);

	// Obtain pointer to start of symbols
	symbols = symtab + ofile->o_header.ssize;

	// Obtain pointer to start of TEXT segment
	sptr = ofile->o_image + 32;

	// Obtain pointer to start of TEXT relocation records
	rptr = sptr + (ofile->o_header.tsize + ofile->o_header.dsize);

	relocsize = ofile->o_header.absrel.reloc.tsize;

    if (vflag)
        printf("RELOCSIZE :: %d  Records = %d\n", relocsize, relocsize / 8);

	// Update pointers if DATA relocation records are being processed
	if (flag == T_DATA)
	{
		sptr += ofile->o_header.tsize;              // Start of DATA segment
		rptr += ofile->o_header.absrel.reloc.tsize; // Start of DATA relocation records
		relocsize = ofile->o_header.absrel.reloc.dsize;
	}

	// Process each relocation record for the TEXT segment
	for(i=0; i<(int)relocsize; i+=8)
	{
		// Obtain both the relocation address and the relocation flags from the
		// object file image
		addr = getlong(rptr);
		rflg = getlong(rptr + 4);
		glblreloc = (rflg & 0x00000010 ? 1 : 0);// Set global relocation flag
		absreloc = (rflg & 0x00000040 ? 1 : 0); // Set absolute relocation flag
		relreloc = (rflg & 0x000000A0 ? 1 : 0); // Set relative relocation flag

		// Additional processing required for global relocations
		if (glblreloc)
		{
			// Obtain the string table index for the relocation symbol, look
			// for it in the globals hash table to obtain information on that
			// symbol. For the hash calculation to work correctly it must be
			// placed in a 'clean' string before looking it up.
			symidx = getlong(symtab + ((rflg >> 8) * 12));
			memset(sym, 0, SYMLEN);
			strcpy(sym, symbols + symidx);
			olddata = newdata = 0;   // Initialise old and new segment data
			ssidx = OSTLookup(sym);
			newdata = getlong(ost + ((ssidx - 1) * 12) + 8);
		}

		// Obtain the existing long word segment data and flip words if the
		// relocation flags indicate it relates to a RISC MOVEI instruction
		olddata = getlong(sptr + addr);

		if (rflg & 0x01)
			olddata = _SWAPWORD(olddata);

		// Process record dependant on segment it relates to; TEXT, DATA or
		// BSS. Construct a new relocated segment long word based on the
		// required segment base address, the segment data offset in the
		// resulting COF file and the offsets from the incoming object file.
		swcond = (rflg & 0xFFFFFF00);

		if (!glblreloc)
		{
			switch (swcond)
			{
			case 0x00000200:          // Absolute Value
				break;
			case 0x00000400:          // TEXT segment relocation record
				if (flag == T_TEXT)   // Is this a TEXT section record?
					newdata = tbase + textoffset + olddata;
				else
					newdata = tbase + dataoffset + olddata; // Nope, must be DATA section

				break;
			case 0x00000600:          // DATA segment relocation record
				newdata = dbase + dataoffset
					+ (olddata - ofile->o_header.tsize);

				break;
			case 0x00000800:          // BSS segment relocation record
				newdata = bbase + bssoffset
					+ (olddata - (ofile->o_header.tsize
					+ ofile->o_header.dsize));

				break;
			}
		}
		else
		{
			if (!relreloc)
				newdata += olddata;
		}

		// Set absolute (long) or relative (word) address of symbol
		if (absreloc)
		{
			// Flip the new long word segment data if the relocation record
			// indicated a RISC MOVEI instruction and place the resulting data
			// back in the COF segment
			if (rflg & 0x01)
				newdata = _SWAPWORD(newdata);

			putlong(sptr + addr, newdata);
		}
		else if (relreloc)
		{
			putword(sptr + addr, newdata - tbase - addr - ofile->o_tbase);
		}

		// Shamus: Let's output some info to aid in debugging this crap
		if (vflag > 1)
		{
			char ssiString[128];
			ssiString[0] = 0;

			if (glblreloc)
				sprintf(ssiString, " [ssi:%i]", ssidx);

			printf("RelocateSegment($%08X): %s, $%08X: $%08X => $%08X%s\n", rflg, (glblreloc ? sym : "(LOCAL)"), addr, olddata, getlong(sptr + addr), ssiString);
		}

		rptr += 8;     // Point to the next relocation record
	}

	// Update the COF segment offset allowing for the phrase padding.
	if (flag == T_TEXT)
	{
		// TEXT segment plus padding
		pad = ((ofile->o_header.tsize + secalign) & ~secalign);
		textoffset += (ofile->o_header.tsize + (pad - ofile->o_header.tsize));
	}
	else
	{
		// DATA segment plus padding
		pad = ((ofile->o_header.dsize + secalign) & ~secalign);
		dataoffset += (ofile->o_header.dsize + (pad - ofile->o_header.dsize));
		// BSS segment plus padding
		pad = ((ofile->o_header.bsize + secalign) & ~secalign);
		bssoffset += (ofile->o_header.bsize + (pad - ofile->o_header.bsize));
	}

	// Return value, should always be zero
	return 0;
}


//
// Add a path character to the end of string 's' if it doesn't already end with
// one. The last occurrance of '/' or '\' in the string is assumed to be the
// path character.
//
// This is fucking shit. We know what the path delimiter is, its FUCKING
// DEFINED IN THE FUCKING HEADER. FOR FUCKS SAKE. AND YES, HOPE TO GOD THERE'S
// ENOUGH SPACE IN THE PASSED IN BUFFER TO HOLD THE EXTRA CHARACTER!
//
void AppendPathDelimiter(char * s)
{
#if 0
	// And hope to God that there's enough space in the buffer...
	char pathchar = 0;

	while (*s)
	{
		if (*s == '/' || *s == '\\')
			pathchar = *s;

		s++;
	}

	s--;

	if (*s == pathchar)
		return;

	*++s = pathchar;
	*++s = 0;
#else
	int length = strlen(s);

	if (s[length - 1] != PATH_DELIMITER)
	{
		s[length] = PATH_DELIMITER;
		s[length + 1] = 0;	// BUFFER OVERFLOW!!!! FFFFFFFFUUUUUUUUUUUU
	}
#endif
}


//
// Try to open "name", "name.o", "${libdir}name", "${libdir}name.o". Return the
// handle of the file successfully opened. p_name is updated to point to a
// malloc()'ed string which is the name which actually got opened. p_name will
// return unchanged if the file can't be found.
//
int TryOpenFile(char ** p_name)
{
	char * name = *p_name;

	// Note that libdir will be an empty string if there is none specified
	char * tmpbuf = malloc(strlen(name) + strlen(libdir) + 3);

	if (tmpbuf == NULL)
	{
		printf("TryOpenFile() : out of memory\n");
		return -1;
	}

	strcpy(tmpbuf, name);
	int hasdot = (strrchr(tmpbuf, '.') > strrchr(tmpbuf, PATH_DELIMITER));
	// Try to open file as passed first
	int fd = open(tmpbuf, _OPEN_FLAGS);

	if (fd >= 0)
		goto ok;

	if (!hasdot)
	{
		// Try to open file with '.o' added
		strcat(tmpbuf, ".o");
		fd = open(tmpbuf, _OPEN_FLAGS);

		if (fd >= 0)
			goto ok;
	}

	// Try the libdir only if the name isn't already anchored
	// Shamus: WTH, this makes no sense... Why the ':'? Is this a Macintosh??
//	if (*name != '/' && *name != '\\' && !strchr(name, ':'))
	if ((*name != PATH_DELIMITER) && (strchr(name, ':') == NULL))
	{
		strcpy(tmpbuf, libdir);
		// Add a trailing path char if there isn't one already
		AppendPathDelimiter(tmpbuf);
		strcat(tmpbuf, name);

		if ((fd = open(tmpbuf, _OPEN_FLAGS)) >= 0)
			goto ok;

		if (!hasdot)
		{
			strcat(tmpbuf, ".o");

			if ((fd = open(tmpbuf, _OPEN_FLAGS)) >= 0)
				goto ok;
		}
	}

	// Couldn't open file at all
	return -1;

// There are worse things... :-P
ok:
	tmpbuf = realloc(tmpbuf, strlen(tmpbuf) + 1);

	if (tmpbuf == NULL)
	{
		printf("TryOpenFile() : out of memory\n");
		return -1;
	}

	*p_name = tmpbuf;
	return fd;
}


//
// Archive File Use, Needs to be Removed [Shamus: NON!]
//
void put_name(struct OFILE * p)
{
	int flag = *(p->o_arname);
	printf("%s%s%s", (flag ? (char *)(p->o_arname) : ""), (flag ? ":" : ""), p->o_name);
}


//
// Collect file names and handles in a buffer so there is less disk activity.
// Call DoFile with flag FALSE for normal object files and archives.
// Call it with flag TRUE and a symbol name for include files (-i).
//
int DoFile(char * fname, int incFlag, char * sym)
{
	// Verbose information
	if (vflag)
	{
		printf("DoFile() : `%s' %s", fname, incFlag ? "INCLUDE" : "NORMAL");

		if (incFlag)
			printf(" symbol %s", sym);

		printf("\n");
	}

	// Reached maximum file handles
	if (hd == NHANDLES)
	{
		if (ProcessFiles())
			return 1;
	}

	// Attempt to open input file
	int fd = TryOpenFile(&fname);

	if (fd < 0)
	{
		printf("Cannot find input module %s\n", fname);
		return 1;
	}

	// The file is open; save its info in the handle and name arrays
	handle[hd] = fd;
	name[hd] = fname;		// This is the name from TryOpenFile()
	hflag[hd] = incFlag;

	// Include files
	if (incFlag)
	{
		int temp = strlen(sym);		// Get symbol length

		// 100 chars is max length of a symbol
		if (temp > 99)
		{
			sym[99] = '\0';
			temp = 99;
		}

		// Malloc enough space for two symbols, then build the second one.
		// Second one may be one character longer than first
		if ((hsym1[hd] = malloc((long)temp + 1)) == NULL
			|| (hsym2[hd] = malloc((long)temp + 2)) == NULL)
		{
			printf("DoFile() : out of memory for include-file symbols\n");
			return 1;
		}

		strcpy(hsym1[hd], sym);
		strcpy(hsym2[hd], sym);

		if (temp == 99)
		{
			if (sym[99] == 'x')
			{
				printf("Last char of %s is already 'x': choose another name\n", sym);
				return 1;
			}

			hsym2[hd][99] = 'x';
		}
		else
		{
			hsym2[hd][temp] = 'x';
			hsym2[hd][temp+1] = '\0';
		}
	}

	// Increment next handle index
	hd++;
	// No problems
	return 0;
}


//
// Pad TEXT or DATA segment to the requested boundary
//
int segmentpad(FILE * fd, long segsize, int value)
{
//	long padsize;                            // Number of pad bytes needed
	int i;                                   // Good 'ol iterator
	char padarray[32];                       // Array of padding bytes
	char * padptr;                           // Pointer to array

	// Determine the number of padding bytes that are needed
	long padsize = (segsize + secalign) & ~secalign;
	padsize = padsize - segsize;

	// Fill pad array if padding is required
	if (padsize)
	{
		padptr = padarray;

		for(i=0; i<16; i++)
		{
			putword(padptr, value);
			padptr += 2;
		}

		symoffset += padsize;

		// Write padding bytes
		if (fwrite(padarray, padsize, 1, fd) != 1)
			return 1;
	}

	return 0;
}


//
// Write the output file
//
int write_ofile(struct OHEADER * header)
{
	unsigned osize;						// Object segment size
	struct OFILE * otemp;				// Object file pointer
	int i, j;							// Iterators
	char himage[0x168];					// Header image (COF = 0xA8)
	unsigned tsoff, dsoff, bsoff;		// Segment offset values
	unsigned index, type, value;		// Symbol table index, type and value
	short abstype;						// ABS symbol type
	char symbol[14];					// Symbol record for ABS files
	int slen;							// Symbol string length

	symoffset = 0;						// Initialise symbol offset

	// Add correct output extension if none
	if (strchr(ofile, '.') == NULL)
	{
		if (aflag && cflag)
			strcat(ofile, ".cof");		// COF files
		else if (aflag && !cflag)
			strcat(ofile, ".abs");		// ABS files
		else
			strcat(ofile, ".o");		// Object files (partial linking etc)
	}

	FILE * fd = fopen(ofile, "wb");		// Attempt to open output file

	if (!fd)
	{
		printf("Can't open output file %s\n", ofile);
		return 1;
	}

	// Build the output file header
	// Absolute (COF) header
	if (cflag)
	{
		tsoff = dsoff = bsoff = 0xA8;	// Initialises segment offsets

		// Process each object file segment size to obtain a cumulative segment
		// size for both the TEXT and DATA segments
		for(i=0; i<(int)obj_index; i++)
		{
			dsoff += obj_segsize[i][0];	// Adding TEXT segment sizes
			bsoff += obj_segsize[i][0] + obj_segsize[i][1];	// Adding TEXT and DATA segment sizes
		}

		// Currently this only builds a COF absolute file. Conditionals and
		// additional code will need to be added for ABS and partial linking.

		// Build the COF_HDR
		putword(himage + 0,   0x0150               ); // Magic Number (0x0150)
		putword(himage + 2,   0x0003               ); // Sections Number (3)
		putlong(himage + 4,   0x00000000           ); // Date (0L)
		putlong(himage + 8,   dsoff + header->dsize); // Offset to Symbols Section
		putlong(himage + 12,  ost_index);             // Number of Symbols
		putword(himage + 16,  0x001C               ); // Size of RUN_HDR (0x1C)
		putword(himage + 18,  0x0003               ); // Executable Flags (3)

		// Build the RUN_HDR
		putlong(himage + 20,  0x00000107           ); // Magic/vstamp
		putlong(himage + 24,  header->tsize        ); // TEXT size in bytes
		putlong(himage + 28,  header->dsize        ); // DATA size in bytes
		putlong(himage + 32,  header->bsize        ); // BSS size in bytes
		putlong(himage + 36,  tbase                ); // Start of executable, normally @TEXT
		putlong(himage + 40,  tbase                ); // @TEXT
		putlong(himage + 44,  dbase                ); // @DATA

		// Build the TEXT SEC_HDR
		putlong(himage + 48,  0x2E746578           );
		putlong(himage + 52,  0x74000000           ); // ".text"
		putlong(himage + 56,  tbase                ); // TEXT START
		putlong(himage + 60,  tbase                ); // TEXT START
		putlong(himage + 64,  header->tsize        ); // TEXT size in bytes
		putlong(himage + 68,  tsoff                ); // Offset to section data in file
		putlong(himage + 72,  0x00000000           ); // Offset to section reloc in file (0L)
		putlong(himage + 76,  0x00000000           ); // Offset to debug lines structures (0L)
		putlong(himage + 80,  0x00000000           ); // Nreloc/nlnno (0L)
		putlong(himage + 84,  0x00000020           ); // SEC_FLAGS: STYP_TEXT

		// Build the DATA SEC_HDR
		putlong(himage + 88,  0x2E646174           );
		putlong(himage + 92,  0x61000000           ); // ".data"
		putlong(himage + 96,  dbase                ); // DATA START
		putlong(himage + 100, dbase                ); // DATA START
		putlong(himage + 104, header->dsize        ); // DATA size in bytes
		putlong(himage + 108, dsoff                ); // Offset to section data in file
		putlong(himage + 112, 0x00000000           ); // Offset to section reloc in file (0L)
		putlong(himage + 116, 0x00000000           ); // Offset to debugging lines structures (0L)
		putlong(himage + 120, 0x00000000           ); // Nreloc/nlnno (0L)
		putlong(himage + 124, 0x00000040           ); // SEC_FLAGS: STYP_DATA

		// Build the BSS SEC_HDR
		putlong(himage + 128, 0x2E627373           );
		putlong(himage + 132, 0x00000000           ); // ".bss"
		putlong(himage + 136, bbase                ); // BSS START
		putlong(himage + 140, bbase                ); // BSS START
		putlong(himage + 144, header->bsize        ); // BSS size in bytes
		putlong(himage + 148, bsoff                ); // Offset to section data in file
		putlong(himage + 152, 0x00000000           ); // Offset to section reloc in file (0L)
		putlong(himage + 156, 0x00000000           ); // Offset to debugging lines structures (0L)
		putlong(himage + 160, 0x00000000           ); // Nreloc/nlnno (0L)
		putlong(himage + 164, 0x00000080           ); // SEC_FLAGS: STYP_BSS

		symoffset = 168;                              // Update symbol offset
	}
	// Absolute (ABS) header
	else
	{
		// Build the ABS header
		putword(himage + 0,   0x601B               ); // Magic Number (0x601B)
		putlong(himage + 2,   header->tsize        ); // TEXT segment size
		putlong(himage + 6,   header->dsize        ); // DATA segment size
		putlong(himage + 10,  header->bsize        ); // BSS segment size
		putlong(himage + 14,  ost_index * 14       ); // Symbol table size (?)
		putlong(himage + 18,  0x00000000           ); //
		putlong(himage + 22,  tbase                ); // TEXT base address
		putword(himage + 26,  0xFFFF               ); // Flags (?)
		putlong(himage + 28,  dbase                ); // DATA base address
		putlong(himage + 32,  bbase                ); // BSS base address

		symoffset = 36;                               // Update symbol offset
	}

	// Write the header, but not if noheaderflag
	// Absolute (ABS) header
	if (!cflag)
	{
		if (!noheaderflag)
			if (fwrite(himage, 36, 1, fd) != 1)
				goto werror;
	}
	// Absolute (COF) header
	else
	{
		if (fwrite(himage, 168, 1, fd) != 1)
			goto werror;
	}

	// Write the TEXT segment of each object file
	for(otemp=olist; otemp!=NULL; otemp=otemp->o_next)
	{
		osize = otemp->o_header.tsize;

		// Write only if segment has size
		if (osize)
		{
			if (vflag > 1)
				printf("Writing TEXT Segment of %s\n", otemp->o_name);

			if (fwrite(otemp->o_image + 32, osize, 1, fd) != 1)
				goto werror;

			// Pad to required alignment boundary
			if (segmentpad(fd, osize, 0x0000))
				goto werror;

			symoffset += osize;
		}
	}

	// Write the DATA segment of each object file
	for(otemp=olist; otemp!=NULL; otemp=otemp->o_next)
	{
		osize = otemp->o_header.dsize;

		// Write only if the segment has size
		if (osize)
		{
			if (vflag > 1)
				printf("Writing DATA Segment of %s\n", otemp->o_name);

			if (fwrite((otemp->o_image + 32 + otemp->o_header.tsize), osize, 1, fd) != 1)
				goto werror;

			// Pad to required alignment boundary
			if (segmentpad(fd, osize, 0))
				goto werror;

			symoffset += osize;
		}
	}

	if (!noheaderflag)
	{
		// Write the symbols table and string table
		// Absolute (COF) symbol/string table
		if (cflag)
		{
			if (header->ssize)
			{
				if (fwrite(ost, (ost_ptr - ost), 1, fd) != 1)
					goto werror;

				if (fwrite(oststr, (oststr_ptr - oststr), 1, fd) != 1)
					goto werror;
			}
		}
		// Absolute (ABS) symbol/string table
		else
		{
			// The symbol and string table have been created as part of the
			// DoSymbol() function and the output symbol and string tables are
			// in COF format. For an ABS file we need to process through this
			// to create the 14 character long combined symbol and string
			// table. Format of symbol table in ABS: AAAAAAAATTVVVV, where
			// (A)=STRING, (T)=TYPE & (V)=VALUE

			for(i=0; i<ost_index; i++)
			{
				memset(symbol, 0, 14);		// Initialise symbol record
				abstype = 0;				// Initialise ABS symbol type
				slen = 0;					// Initialise symbol string length
				index = getlong(ost + (i * 12));	// Get symbol index
				type  = getlong((ost + (i * 12)) + 4);	// Get symbol type

				// Not doing debug symbols
				if (type & 0xF0000000)
					continue;

				// Get symbol value
				value = getlong((ost + (i * 12)) + 8);
				slen = strlen(oststr + index);

				// Get symbol string (maximum 8 chars)
				if (slen > 8)
				{
					for(j=0; j<8; j++)
						*(symbol + j) = *(oststr + index + j);
				}
				else
				{
					for(j=0; j<slen; j++)
						*(symbol + j) = *(oststr + index + j);
				}

				// Modify to ABS symbol type
				switch (type)
				{
				case 0x02000000: abstype = (short)ABST_DEFINED;                           break;
				case 0x04000000: abstype = (short)ABST_DEFINED | ABST_TEXT;               break;
				case 0x05000000: abstype = (short)ABST_DEFINED | ABST_GLOBAL | ABST_TEXT; break;
				case 0x06000000: abstype = (short)ABST_DEFINED | ABST_DATA;               break;
				case 0x07000000: abstype = (short)ABST_DEFINED | ABST_GLOBAL | ABST_DATA; break;
				case 0x08000000: abstype = (short)ABST_DEFINED | ABST_BSS;                break;
				case 0x09000000: abstype = (short)ABST_DEFINED | ABST_GLOBAL | ABST_BSS;  break;
				default:
					printf("write_ofile: abs, cannot determine symbol type\n");
					type = 0;
					break;
				}

				putword(symbol + 8, abstype);	// Write back new ABS type
				putlong(symbol + 10, value);	// Write back value

				// Write symbol record
				if (fwrite(symbol, 14, 1, fd) != 1)
					goto werror;
			}
		}
	}

	// Close the file
	if (fclose(fd))
	{
		printf("Close error on output file %s\n",ofile);
		return 1;
	}

	return 0;

werror:
	printf("Write error on output file %s\n", ofile);
	fclose(fd);			// Try to close output file anyway
	return 1;
}


//
// Display the Symbol Load Map
//
int write_map(struct OHEADER * header)
{
	unsigned i, o;			// Inner and outer loop iterators
	unsigned c;				// Column number
	unsigned index;			// Symbol string index
	unsigned type;			// Symbol type
	unsigned value;			// Symbol value
	char * symbol;			// Symbol string value

	if (ost_index == 0)
		return 0;			// Return if no symbols to map

	printf("LOAD MAP\n\n");

	// Outer loop for each of the symbol areas to map out;
	// 0 = NON-RELOCATABLE SYMBOLS
	// 1 = TEXT-SEGMENT RELOCATABLE SYMBOLS
	// 2 = DATA-SEGMENT RELOCATABLE SYMBOLS
	// 3 = BSS-SEGMENT RELOCATABLE SYMBOLS
	for(o=0; o<4; o++)
	{
		// Display the correct map header for the symbols being processed
		switch (o)
		{
		case 0: printf("NON-RELOCATABLE SYMBOLS\n\n");          break;
		case 1: printf("TEXT-SEGMENT RELOCATABLE SYMBOLS\n\n"); break;
		case 2: printf("DATA-SEGMENT RELOCATABLE SYMBOLS\n\n"); break;
		case 3: printf("BSS-SEGMENT RELOCATABLE SYMBOLS\n\n");  break;
		}

		c = 0;				// Initialise column number

		// Inner loop to process each record in the symbol table
		for(i=0; i<(unsigned)ost_index; i++)
		{
			index  = getlong(ost + (i * 12));		// Get symbol string index
			type   = getlong(ost + (i * 12) + 4);	// Get symbol type
			value  = getlong(ost + (i * 12) + 8);	// Get symbol value
			symbol = oststr + index;				// Get symbol string

			// Display only three columns
			if (c == 3)
			{
				printf("\n");
				c = 0;
			}

			// If local symbols not included and the type is local then go to
			// next symbol record
			if (!lflag & !(type & 0x01000000))
				continue;

			// Output each symbol to the display, dependant on type
			switch (o)
			{
			case 0:
				// Non-relocatable symbols
				if (type == 0x02000000 || type == 0x03000000)
				{
					printf("%-8s %c  %08X   ", symbol, (type & 0x01000000) ? 'G' : 'L', value);
					c++;
				}

				break;
			case 1:
				// TEXT segment relocatable symbols
				if (type == 0x04000000 || type == 0x05000000)
				{
					printf("%-8s %c  %08X   ", symbol, (type & 0x01000000) ? 'G' : 'L', value);
					c++;
				}

				break;
			case 2:
				// DATA segment relocatble symbols
				if (type == 0x06000000 || type == 0x07000000)
				{
					printf("%-8s %c  %08X   ", symbol, (type & 0x01000000) ? 'G' : 'L', value);
					c++;
				}

				break;
			case 3:
				// BSS segment relocatable symbols
				if (type == 0x08000000 || type == 0x09000000)
				{
					printf("%-8s %c  %08X   ", symbol, (type & 0x01000000) ? 'G' : 'L', value);
					c++;
				}

				break;
			}
		}

		printf("\n\n");
	}

	return 0;
}


//
// Stuff the (long) value of a string into the value argument. RETURNS TRUE if
// the string doesn't parse.  Parses only as a hexadecimal string.
//
int GetHexValue(char * string, int * value)
{
	*value = 0;

	while (isxdigit(*string))
	{
		if (isdigit(*string))
		{
			*value = (*value << 4) + (*string++ - '0');
		}
		else
		{
			if (isupper(*string))
				*string = tolower(*string);

			*value = (*value << 4) + ((*string++ - 'a') + 10);
		}
	}

	if (*string != '\0')
	{
		printf("Invalid hexadecimal value");
		return 1;
	}

	return 0;
}


//
// Create one big .o file from the images already in memory, returning a
// pointer to an OHEADER. Note that the oheader is just the header for the
// output (plus some other information). The text, data, and fixups are all
// still in the ofile images hanging off the global `olist'.
//
struct OHEADER * make_ofile()
{
	unsigned tptr, dptr, bptr;					// Bases in runtime model
	int ret = 0;								// Return value
	struct OHEADER * header;					// Output header pointer

	// Initialize cumulative segment sizes
	textsize = datasize = bsssize = 0;

	// For each object file, accumulate the sizes of the segments but remove
	// those object files which are unused
	struct OFILE * oprev = NULL;	// Init previous obj file list ptr
	struct OFILE * otemp = olist;	// Set temp pointer to object file list
	int i = 0;

	while (otemp != NULL)
	{
		// UNUSED !!!!! (it's !O_ARCHIVE, so this code executes.)
		if (!(otemp->o_flags & O_ARCHIVE))
		{
			if ((otemp->o_flags & O_USED) == 0)
			{
				if (wflag)
				{
					printf("Unused object file ");
					put_name(otemp);
					printf(" discarded.\n");
				}

				if (oprev == NULL)
					olist = otemp->o_next;
				else
					oprev->o_next = otemp->o_next;

				struct OFILE * ohold = otemp;

				if (!ohold->isArchiveFile)
					free(ohold->o_image);

//				free(ohold);

				// Also need to remove them from the obj_* tables too :-P
				// N.B.: Would probably be worthwhile to remove crap like this
				//       and stuff it into the OFILE structure...
				if (wflag)
					printf("»-> removing %s... (index == %i, len == %i)\n", obj_fname[i], i, obj_index - 1);

				int k;

				for(k=i; k<obj_index-1; k++)
				{
					memcpy(obj_fname[k], obj_fname[k + 1], FNLEN);
					obj_segsize[k][0] = obj_segsize[k + 1][0];
					obj_segsize[k][1] = obj_segsize[k + 1][1];
					obj_segsize[k][2] = obj_segsize[k + 1][2];
				}

				obj_index--;
				i--;
			}
			else
			{
				// Increment total of segment sizes ensuring requested
				// alignment
				textsize += (otemp->o_header.tsize + secalign) & ~secalign;
				datasize += (otemp->o_header.dsize + secalign) & ~secalign;
				bsssize  += (otemp->o_header.bsize + secalign) & ~secalign;
				oprev = otemp;
			}
		}

		// Go to next object file list pointer
		otemp = otemp->o_next;
		i++;
	}

	// Update base addresses and create symbols _TEXT_E, _DATA_E and _BSS_E
	tbase = tval;
	OSTAdd("_TEXT_E", 0x05000000, tval + textsize);

	if (!dval)
	{
		// DATA follows TEXT
		dbase = tval + textsize;
		OSTAdd("_DATA_E", 0x07000000, tval + textsize + datasize);

		if (!bval)
		{
			// BSS follows DATA
			bbase = tval + textsize + datasize;
			OSTAdd("_BSS_E", 0x09000000, tval + textsize + datasize + bsssize);
		}
		else
		{
			// BSS is independent of DATA
			bbase = bval;
			OSTAdd("_BSS_E", 0x09000000, bval + bsssize);
		}
	}
	else
	{
		// DATA is independant of TEXT
		dbase = dval;
		OSTAdd("_DATA_E", 0x07000000, dval + datasize);

		if (!bval)
		{
			// BSS follows DATA
			bbase = dval + datasize;
			OSTAdd("_BSS_E", 0x09000000, dval + datasize + bsssize);
		}
		else
		{
			// BSS is independent of DATA
			bbase = bval;
			OSTAdd("_BSS_E", 0x09000000, bval + bsssize);
		}
	}

	// Place each unresolved symbol in the output symbol table
	if (DoUnresolved())
		return NULL;

	// Initialise base addresses
	tptr = dptr = bptr = 0;

	// For each file, relocate its symbols and add them to the output symbol
	// table
	otemp = olist;
	oprev = NULL;

	while (otemp != NULL)
	{
		otemp->o_tbase = tptr;

		// Do rest only for non-ARCHIVE markers
		if (!(otemp->o_flags & O_ARCHIVE))
		{
			otemp->o_dbase = dptr;
			otemp->o_bbase = bptr;
			tptr += (otemp->o_header.tsize + secalign) & ~secalign;
			dptr += (otemp->o_header.dsize + secalign) & ~secalign;
			bptr += (otemp->o_header.bsize + secalign) & ~secalign;
		}

		// For each symbol, (conditionally) add it to the ost
		// For ARCHIVE markers, this adds the symbol for the file & returns
		// (Shamus: N.B. it does no such thing ATM)
		if (DoSymbol(otemp))
			return NULL;

		if (otemp->o_flags & O_ARCHIVE)
		{
			// Now that the archive is marked, remove it from list
			if (oprev == NULL)
				olist = otemp->o_next;
			else
				oprev->o_next = otemp->o_next;

			struct OFILE * ohold = otemp;

			if (ohold->o_image)
				if (!ohold->isArchiveFile)
					free(ohold->o_image);

			free(ohold);
		}
		else
		{
			oprev = otemp;
		}

		otemp = otemp->o_next;
	}

	// Places all the externs, globals etc into the output symbol table
	if (DoCommon() == -1)
		return NULL;

	// Create a new output file header
	header = new_oheader();

	if (header == NULL)
	{
		printf("make_ofile: out of memory!\n");
		return NULL;
	}

	// Fill in the output header. Does not match the actual output but values
	// used as reference
	header->magic = 0x0150;             // COF magic number
	header->tsize = textsize;           // TEXT segment size
	header->dsize = datasize;           // DATA segment size
	header->bsize = bsssize;            // BSS segment size
	header->ssize = (ost_ptr - ost);    // Symbol table size
	header->ostbase = ost;              // Output symbol table base address

	// For each object file, relocate its TEXT and DATA segments. OR the result
	// into ret so all files get moved (and errors reported) before returning
	// with the error condition
	for(otemp=olist; otemp!=NULL; otemp=otemp->o_next)
	{
		if (!(otemp->o_flags & O_ARCHIVE))
		{
			ret |= RelocateSegment(otemp, T_TEXT); // TEXT segment relocations
			ret |= RelocateSegment(otemp, T_DATA); // DATA segment relocations
		}
	}

	// Done with global symbol hash tables
	FreeHashes();

	return (ret ? (struct OHEADER *)NULL : header);
}


//
// Add symbol to hash list
//
int AddSymbolToHashList(struct HREC ** hptr, char * sym, struct OFILE * ofile,
	long value, int type)
{
	struct HREC * htemp = new_hrec();

	if (htemp == NULL)
	{
		printf("Out of memory\n");
		return 1;
	}

	// Shamus: Moar testing...
	if (vflag > 1)
	{
		printf("AddSymbolToHashList(): hptr=$%08X, sym=\"%s\", ofile=$%08X, value=$%X, type=$%X\n", hptr, sym, ofile, value, type);
	}

	// Populate hash record
	memset(htemp->h_sym, 0, SYMLEN);
	strcpy(htemp->h_sym, sym);
	htemp->h_ofile = ofile;
	htemp->h_value = value;
	htemp->h_type = type;

	// Add new hash to the front of the list (hence the ** for hptr)
	htemp->h_next = *hptr;
	*hptr = htemp;

	return 0;
}


//
// Add symbol to the unresolved symbols hash table (really, it's a linked list)
//
int AddUnresolvedSymbol(char * sym, struct OFILE * ofile)
{
	if (vflag > 1)
		printf("AddUnresolvedSymbol(%s, %s)\n", sym, ofile->o_name);

	return AddSymbolToHashList(&unresolved, sym, ofile, 0L, 0);
}


//
// Remove the HREC from the unresolved symbol list, and pass back a pointer
// to the spot where the HREC was.
//
struct HREC * RemoveUnresolvedSymbol(struct HREC * hrec)
{
	struct HREC * ptr = unresolved;
	struct HREC * previous = NULL;

	while ((ptr != hrec) && (ptr != NULL))
	{
		previous = ptr;
		ptr = ptr->h_next;
	}

	// Not found...!
	if (ptr == NULL)
		return NULL;

	struct HREC * next = ptr->h_next;

	// Remove the head if nothing previous, otherwise, remove what we found
	if (previous == NULL)
		unresolved = next;
	else
		previous->h_next = next;

	free(ptr);
	return next;
}


//
// Add symbol to the unresolved symbols hash table
//
int AddARSymbol(char * sym, struct OFILE * ofile)
{
	if (vflag > 1)
		printf("AddARSymbol(%s, %s)\n", sym, ofile->o_name);

	return AddSymbolToHashList(&arSymbol, sym, ofile, 0L, 0);
}


//
// Generate hash value from the 1st 15 characters of the symbol modulo the
// number of buckets in the hash.
//
int GetHash(char * s)
{
	// For this to be consistent, the symbol MUST be zeroed out beforehand!
	char c[SYMLEN];
	memset(c, 0, SYMLEN);
	strcpy(c, s);

	int i = (c[0] + c[1] + c[2] + c[3] + c[4] + c[5] + c[6] + c[7] + c[8]
		+ c[9] + c[10] + c[11] + c[12] + c[13] + c[14]) % NBUCKETS;
	return i;
}


//
// Lookup a symbol in the hash table.
// Returns either a pointer to the HREC or NULL if not found.
//
struct HREC * LookupHREC(char * symbol)
{
	struct HREC * hptr = htable[GetHash(symbol)];

	while (hptr != NULL)
	{
//This is utter failure...
//		if (symcmp(symbol, hptr->h_sym))  <-- left here for giggles :D  - LinkoVitch
		// Return hash record pointer if found
		if (strcmp(symbol, hptr->h_sym) == 0)
			return hptr;

		hptr = hptr->h_next;
	}

	return NULL;
}


//
// Lookup a symbol in the AR symbol table.
// Returns either a pointer to the HREC or NULL if not found.
//
struct HREC * LookupARHREC(char * symbol)
{
	struct HREC * hptr = arSymbol;//htable[GetHash(symbol)];

	while (hptr != NULL)
	{
		// Return hash record pointer if found
		if (strcmp(symbol, hptr->h_sym) == 0)
			return hptr;

		hptr = hptr->h_next;
	}

	return NULL;
}


//
// Add symbol to the hash table if it doesn't already exist, otherwise decide
// what to do since it's already defined in another unit.
//
int DealWithSymbol(char * sym, long type, long value, struct OFILE * ofile)
{
	if (vflag > 1)
	{
		printf("DealWithSymbol(%s,%s,%lx,", sym, ofile->o_name, value);
		printf("%x,%s)\n", (unsigned int)type, (isglobal(type) ? "GLOBAL" : "COMMON"));
	}

	struct HREC * hptr = LookupHREC(sym);

	if (hptr == NULL)
		return AddSymbolToHashList(&htable[GetHash(sym)], sym, ofile, value, type);

	// Symbol is already in table... Figure out how to handle!
	if (iscommon(type) && !iscommon(hptr->h_type))
	{
		// Mismatch: global came first; warn and keep the global one
		if (wflag)
		{
			printf("Warning: %s: global from ", sym);
			put_name(hptr->h_ofile);
			printf(" used, common from ");
			put_name(ofile);
			printf(" discarded.\n");
		}

		putword(sym + 8, ABST_EXTERN);
		putlong(sym + 10, 0L);
	}
	else if (iscommon(hptr->h_type) && !iscommon(type))
	{
		// Mismatch: common came first; warn and keep the global one
		if (wflag)
		{
			printf("Warning: %s: global from ", sym);
			put_name(ofile);
			printf(" used, common from ");
			put_name(hptr->h_ofile);
			printf(" discarded.\n");
		}

		hptr->h_type = type;
		hptr->h_ofile = ofile;
		hptr->h_value = value;
	}
	// They're both global [WRONG! Passed in one is global]
//	else if (!iscommon(type))
//is this now right??
	else if (!iscommon(type) && !iscommon(hptr->h_type))
	{
		// Global exported by another ofile; warn and make this one extern
		if (wflag)
		{
			printf("Duplicate symbol %s: ", sym);
			put_name(hptr->h_ofile);
			printf(" used, ");
			put_name(ofile);
			printf(" discarded\n");
		}

		putword(sym + 8, ABST_EXTERN);
	}
	// They're both common
	else
	{
		if (hptr->h_value < value)
		{
			hptr->h_value = value;
			hptr->h_ofile = ofile;
		}
	}

	return 0;
}


//
// Add the imported symbols from this file to unresolved, and the global and
// common (???) symbols to the exported hash table.
//
// Change old-style commons (type == T_EXTERN, value != 0) to new-style ones
// (type == (T_GLOBAL | T_EXTERN)). [??? O_o]
//
int AddSymbols(struct OFILE * Ofile)
{
	struct HREC * hptr;			// Hash record pointer
	char symbol[SYMLEN];

	if (vflag > 1)
		printf("Add symbols for file %s\n", Ofile->o_name);

	// Get base pointer, start of sym fixups
	char * ptr = Ofile->o_image + 32
		+ Ofile->o_header.tsize
		+ Ofile->o_header.dsize
		+ Ofile->o_header.absrel.reloc.tsize
		+ Ofile->o_header.absrel.reloc.dsize;
	char * sfix = ptr;							// Set symbol fixup pointer
	char * sstr = sfix + Ofile->o_header.ssize;	// Set symbol table pointer
	long nsymbols = Ofile->o_header.ssize / 12;	// Obtain number of symbols

	while (nsymbols)
	{
		long index = getlong(sfix);				// Get symbol string index
		long type  = getlong(sfix + 4);			// Get symbol type
		long value = getlong(sfix + 8);			// Get symbol value
		memset(symbol, 0, SYMLEN);
		strcpy(symbol, sstr + index);

		if ((Ofile->isArchiveFile) && !(Ofile->o_flags & O_USED))
		{
			if ((type & T_EXT) && (type & (T_SEG | T_ABS)))
				if (AddARSymbol(symbol, Ofile))
					return 1;
		}
		else if (type == T_EXT)
		{
			// External symbal that is *not* in the current unit
			hptr = LookupHREC(symbol);

			if (hptr != NULL)
				hptr->h_ofile->o_flags |= O_USED;	// Mark .o file as used
			// Otherwise add to unresolved list
			else if (AddUnresolvedSymbol(symbol, Ofile))
				return 1;				// Error if addition failed
		}
		else if ((type & T_EXT) && (type & (T_SEG | T_ABS)))
		{
			// Symbol in the current unit that is also EXPORTED
			if (DealWithSymbol(symbol, type, value, Ofile))
				return 1;				// Error if addition failed
		}

		sfix += 12;			// Increment symbol fixup pointer
		nsymbols--;			// Decrement num of symbols to process
	}

	// Success loading symbols
	return 0;
}


//
// Process object file for symbols
//
int DoItem(struct OFILE * obj)
{
	// Allocate memory for object record ptr
	struct OFILE * Ofile = new_ofile();

	if (Ofile == NULL)
	{
		printf("Out of memory while processing %s\n", obj->o_name);
		return 1;
	}

	// Starting after all pathnames, etc., copy .o file name to Ofile
	char * temp = path_tail(obj->o_name);

	// Check filename length
	if (strlen(temp) > FNLEN - 1)
	{
		printf("File name too long: %s\n", temp);
		return 1;
	}

	// Check archive name length
	if (strlen(obj->o_arname) > FNLEN - 1)
	{
		printf("Archive name too long: %s\n", obj->o_arname);
		return 1;
	}

	strcpy(Ofile->o_name, temp);		// Store filename
	strcpy(Ofile->o_arname, obj->o_arname);	// Store archive name

	// Initialise object record information
	Ofile->o_next  = NULL;
	Ofile->o_tbase = 0;
	Ofile->o_dbase = 0;
	Ofile->o_bbase = 0;
	Ofile->o_flags = obj->o_flags;
	Ofile->o_image = obj->o_image;
	Ofile->isArchiveFile = obj->isArchiveFile;
	char * ptr = obj->o_image;

	// Don't do anything if this is just an ARCHIVE marker, just add the file
	// to the olist
	// (Shamus: N.B.: it does no such ARCHIVE thing ATM)
	if (!(obj->o_flags & O_ARCHIVE))
	{
		Ofile->o_header.magic = getlong(ptr);
		Ofile->o_header.tsize = getlong(ptr + 4);
		Ofile->o_header.dsize = getlong(ptr + 8);
		Ofile->o_header.bsize = getlong(ptr + 12);
		Ofile->o_header.ssize = getlong(ptr + 16);
		Ofile->o_header.absrel.reloc.tsize = getlong(ptr + 24);
		Ofile->o_header.absrel.reloc.dsize = getlong(ptr + 28);

		// Round BSS off to alignment boundary
		Ofile->o_header.bsize = (Ofile->o_header.bsize + secalign) & ~secalign;

		if ((Ofile->o_header.dsize & 7) && wflag)
		{
			printf("Warning: data segment size of ");
			put_name(Ofile);
			printf(" is not a phrase multiple\n");
		}

		// Check for odd segment sizes
		if ((Ofile->o_header.tsize & 1) || (Ofile->o_header.dsize & 1)
			|| (Ofile->o_header.bsize & 1))
		{
			printf("Error: odd-sized segment in ");
			put_name(Ofile);
			printf("; link aborted.\n");
			return 1;
		}

		if (AddSymbols(Ofile))
			return 1;
	}

	// Add this file to the olist
	if (olist == NULL)
		olist = Ofile;
	else
		olast->o_next = Ofile;

	olast = Ofile;
	return 0;
}


//
// Handle items in processing list.
//
// After loading all objects, archives & include files, we now go and process
// each item on the processing list (plist). Once this is done, we go through
// any unresolved symbols left and see if they have shown up.
//
int ProcessLists(void)
{
	// Process object file list first (adds symbols from each unit & creates
	// the olist)
	while (plist != NULL)
	{
		if (DoItem(plist))
			return 1;

		struct OFILE * ptemp = plist;
		plist = plist->o_next;
		free(ptemp);
	}

	struct HREC * uptr;

	// Process the unresolved symbols list. This may involve pulling in symbols
	// from any included .a units. Such units are lazy linked by default; we
	// generally don't want everything they provide, just what's referenced.
	for(uptr=unresolved; uptr!=NULL; )
	{
		if (vflag > 1)
			printf("LookupHREC(%s) => ", uptr->h_sym);

		struct HREC * htemp = LookupHREC(uptr->h_sym);

		if (htemp != NULL)
		{
			// Found it in the symbol table!
			if (vflag > 1)
				printf("%s in %s (=$%06X)\n", (isglobal(htemp->h_type) ? "global" : "common"), htemp->h_ofile->o_name, htemp->h_value);

			// Mark the .o unit that the symbol is in as seen & remove from the
			// unresolved list
			htemp->h_ofile->o_flags |= O_USED;
			uptr = RemoveUnresolvedSymbol(uptr);
		}
		else
		{
			if (vflag > 1)
				printf("NULL\n");

			// Check to see if the unresolved symbol is on the AR symbol list.
			htemp = LookupARHREC(uptr->h_sym);

			// If the unresolved symbol is in a .o unit that is unused, we can
			// drop it; same if the unresolved symbol is in the exported AR
			// symbol list. Otherwise, go to the next unresolved symbol.
			if (!(uptr->h_ofile->o_flags & O_USED) || (htemp != NULL))
				uptr = RemoveUnresolvedSymbol(uptr);
			else
				uptr = uptr->h_next;

			// Now that we've possibly deleted the symbol from unresolved list
			// that was also in the AR list, we add the symbols from this .o
			// unit to the symbol table, mark the .o unit as used, and restart
			// scanning the unresolved list as there is a good possibility that
			// the symbols in the unit we're adding has unresolved symbols as
			// well.
			if (htemp != NULL)
			{
				htemp->h_ofile->o_flags |= O_USED;
				AddSymbols(htemp->h_ofile);
				uptr = unresolved;
			}
		}
	}

	// Show files used if the user requests it.
	if (vflag > 1)
	{
		printf("Files used:\n");
		struct OFILE * filePtr = olist;

		while (filePtr != NULL)
		{
			if (filePtr->o_flags & O_USED)
			{
				printf("   %s%s%s\n", filePtr->o_name, (filePtr->isArchiveFile ? ":" : ""), (filePtr->isArchiveFile ? filePtr->o_arname : nullStr));
			}

			filePtr = filePtr->o_next;
		}
	}

	return 0;
}


//
// Extract filename from path
//
char * path_tail(char * name)
{
	char * temp = strrchr(name, PATH_DELIMITER);

	// Return what was passed in if path delimiter was not found
	if (temp == NULL)
		return name;

	return temp + 1;
}


//
// Add input file to processing list
//
int AddToProcessingList(char * ptr, char * fname, char * arname, uint8_t arFile)
{
	if (plist == NULL)
	{
		// First time object record allocation
		plist = new_ofile();
		plast = plist;
	}
	else
	{
		// Next object record allocation
		plast->o_next = new_ofile();
		plast = plast->o_next;
	}

	if (plast == NULL)
	{
		printf("Out of memory.\n");		// Error if memory allocation fails
		return 1;
	}

	fname = path_tail(fname);
	arname = path_tail(arname);

	if (strlen(fname) > FNLEN - 1)
	{
		// Error on excessive filename length
		printf("File name too long: %s (sorry!)\n", fname);
		return 1;
	}

	if (strlen(arname) > FNLEN - 1)
	{
		// Error on excessive filename length
		printf("AR file name too long: %s (sorry!)\n", arname);
		return 1;
	}

	strcpy(plast->o_name, fname);		// Store filename sans path
	strcpy(plast->o_arname, arname);	// Store archive name sans path
	plast->o_image = ptr;				// Store data pointer
	plast->o_flags = (arFile ? 0 : O_USED);	// File is used if NOT in archive
	plast->o_next = NULL;				// Initialise next record pointer
	plast->isArchiveFile = arFile;		// Shamus: Temp until can sort it out

	return 0;							// Return without errors
}


//
// Process in binary include files and add them to the processing list. This
// routine takes in the binary file and creates an 'object' file in memory.
// Sym1/Sym2 point to the start and end of data.
//
// Image size for include files is:
// Header ....... 32 bytes
// Data ......... dsize
// Sym Fixups ... 2 * 12 bytes
// Symbol Size .. 4 bytes (Value to include symbols and terminating null)
// Symbols ...... (strlen(sym1) + 1) + (strlen(sym2) + 1)
// Terminate .... 4 bytes (0x00000000)
//
int LoadInclude(char * fname, int handle, char * sym1, char * sym2, int segment)
{
	long fsize, dsize, size;              // File, DATA segment and image sizes
	char * ptr, * sptr;                   // Data pointers
	int i;                                // Iterators
	int sym1len = 0;                      // Symbol 1 length
	int sym2len = 0;                      // Symbol 2 length
	unsigned symtype = 0;

	fsize = FileSize(handle);             // Get size of include file
	dsize = (fsize + secalign) & ~secalign; // Round up to a alignment boundary

	sym1len = strlen(sym1) + 1;           // Get sym1 length + null termination
	sym2len = strlen(sym2) + 1;           // Get sym2 length + null termination

	size = 32 + dsize + 24 + 4 + sym1len + sym2len + 4;

	// Use calloc so the header & fixups initialize to zero
	// Allocate object image memory
	if ((ptr = calloc(size, 1)) == NULL)
	{
		printf("Out of memory while including %s\n", fname);
		close(handle);
		return 1;
	}

	// Read in binary data
	if (read(handle, ptr + 32, fsize) != fsize)
	{
		printf("File read error on %s\n", fname);
		close(handle);
		free(ptr);
		return 1;
	}

	close(handle);

	strcpy(obj_fname[obj_index], path_tail(fname));

	// Build this image's dummy header
	putlong(ptr, 0x00000107);              // Magic number

	if (segment)
	{
		putlong(ptr+4, dsize);             // Text size
		putlong(ptr+8, 0L);                // Data size
		symtype = 0x05000000;
		obj_segsize[obj_index][0] = dsize;
		obj_segsize[obj_index][1] = 0;
		obj_segsize[obj_index][2] = 0;
	}
	else
	{
		putlong(ptr+4, 0L);                // Text size
		putlong(ptr+8, dsize);             // Data size
		symtype = 0x07000000;
		obj_segsize[obj_index][0] = 0;
		obj_segsize[obj_index][1] = dsize;
		obj_segsize[obj_index][2] = 0;
	}

	obj_index++;                           // Increment object count

	putlong(ptr+12, 0L);                   // BSS size
	putlong(ptr+16, 24);                   // Symbol table size
	putlong(ptr+20, 0L);                   // Entry point
	putlong(ptr+24, 0L);                   // TEXT relocation size
	putlong(ptr+28, 0L);                   // DATA relocation size

	sptr = ptr + 32 + dsize;               // Set sptr to symbol table location

	putlong(sptr,    4L);                  // String offset of symbol1
	putlong(sptr+4,  symtype);             // Symbol type
	putlong(sptr+8,  0x00000000);          // Symbol has no value (START)
	putlong(sptr+12, 4L + (sym2len - 1));  // String offset of symbol2
	putlong(sptr+16, symtype);             // Symbol type
	putlong(sptr+20, dsize);               // Symbol is data size (END)

	sptr = ptr + 32 + dsize + 24;          // Set sptr to symbol table size loc

	putlong(sptr, sym1len + 4L);           // Size of symbol table

	sptr = ptr + 32 + dsize + 24 + 4;      // Set sptr to symbol table location

	for(i=0; i<(sym1len-1); i++)           // Write symbol1 to string table
		sptr[i] = *sym1++;

	sptr += (sym1len - 1);                 // Step past symbol string
	*sptr = '\0';                          // Terminate symbol string
	sptr += 1;                             // Step past termination

	for(i=0; i<(sym2len-1); i++)           // Write symbol2 to string table
		sptr[i] = *sym2++;

	sptr += (sym2len - 1);                 // Step past symbol string
	*sptr = '\0';                          // Terminate symbol string
	sptr += 1;                             // Step past termination

	putlong(sptr, 0L);                     // Terminating long for object file

	return AddToProcessingList(ptr, fname, nullStr, 0);
}


//
// Takes a file name, gets in its image, puts it on plist. The image may
// already be in memory: If so, the ptr arg is non-null.  If so, the file is
// already closed. Note that the file is already open (from DoFile()). RETURNS
// a pointer to the OFILE structure for this file, so you can diddle its flags
// (DoFile sets O_USED for files on the command line).
//
int LoadObject(char * fname, int fd, char * ptr)
{
	if (ptr == NULL)
	{
		long size = FileSize(fd);

		// Allocate memory for file data
		ptr = malloc(size);

		if (ptr == NULL)
		{
			printf("Out of memory while processing %s\n", fname);
			close(fd);
			return 1;
		}

		// Read in file data
		if (read(fd, ptr, size) != size)
		{
			printf("File read error on %s\n", fname);
			close(fd);
			free(ptr);
			return 1;
		}

		// SCPCD: get the name of the file instead of all pathname
		strcpy(obj_fname[obj_index], path_tail(fname));
		obj_segsize[obj_index][0] = (getlong(ptr + 4) + secalign) & ~secalign;
		obj_segsize[obj_index][1] = (getlong(ptr + 8) + secalign) & ~secalign;
		obj_segsize[obj_index][2] = (getlong(ptr + 12) + secalign) & ~secalign;
		obj_index++;

		close(fd);                                            // Close file
	}

	// Now add this image to the list of pending ofiles (plist)
	// This routine is shared by LoadInclude after it builds the image
	return AddToProcessingList(ptr, fname, nullStr, 0);
}


//
// What it says on the tin: check for a .o suffix on the passed in string
//
uint8_t HasDotOSuffix(char * s)
{
	char * temp = strrchr(s, '.');

	if ((temp == NULL) || (strncmp(temp, ".o", 2) != 0))
		return 0;

	return 1;
}


//
// Process an ar archive file (*.a)
//
int LoadArchive(char * fname, int fd)
{
	// Read in the archive file to memory and process
	long size = FileSize(fd);
	char * ptr = malloc(size);
	char * endPtr = ptr + size;
	char * longFilenames = NULL;

	if (ptr == NULL)
	{
		printf("Out of memory while processing %s\n", fname);
		close(fd);
		return 1;
	}

	if (read(fd, ptr, size) != size)
	{
		printf("File read error on %s\n", fname);
		close(fd);
		free(ptr);
		return 1;
	}

	close(fd);

	// Save the pointer for later...
	arPtr[arIndex++] = ptr;
	char objName[FNLEN];
	char objSize[11];
	int i;
//printf("\nProcessing AR file \"%s\"...\n", fname);
	ptr += 8;

	// Loop through all objects in the archive and process them
	do
	{
		memset(objName, 0, 17);
		objSize[10] = 0;

		for(i=0; i<16; i++)
		{
//			if ((ptr[i] == '/') || (ptr[i] == ' '))
			if ((ptr[i] == ' ') && (i != 0))
			{
				objName[i] = 0;
				break;
			}

			objName[i] = ptr[i];
		}

		for(i=0; i<10; i++)
		{
			if (ptr[48 + i] == ' ')
			{
				objSize[i] = 0;
				break;
			}

			objSize[i] = ptr[48 + i];
		}

		// Check to see if a long filename was requested
		if (objName[0] == 0x20)
		{
			uint32_t fnSize = atoi(objName + 1);

			if (longFilenames != NULL)
			{
				i = 0;
				char * currentFilename = longFilenames + fnSize;

				while (*currentFilename != 0x0A)
					objName[i++] = *currentFilename++;

				objName[i] = 0;
			}
		}

		if ((strncmp(objName, "ARFILENAMES/", 12) == 0) || (strncmp(objName, "//", 2) == 0))
		{
			longFilenames = ptr + 60;
		}
		else if (HasDotOSuffix(objName))
		{
//printf("Processing object \"%s\" (size == %i, obj_index == %i)...\n", objName, atoi(objSize), obj_index);
			strcpy(obj_fname[obj_index], objName);
			obj_segsize[obj_index][0] = (getlong(ptr + 60 + 4) + secalign) & ~secalign;
			obj_segsize[obj_index][1] = (getlong(ptr + 60  + 8) + secalign) & ~secalign;
			obj_segsize[obj_index][2] = (getlong(ptr + 60  + 12) + secalign) & ~secalign;
			obj_index++;

			if (AddToProcessingList(ptr + 60, objName, fname, 1))
				return 1;
		}

		uint32_t size = atoi(objSize);
		size += (size & 0x01 ? 1 : 0);
		ptr += 60 + size;
	}
	while (ptr < endPtr);

	return 0;
}


//
// Process files (*.o, *.a) passed in on the command line
//
int ProcessFiles(void)
{
	int i;
	char magic[8];		// Magic header number (4 bytes for *.o, 8 for *.a)

	// Process all file handles
	for(i=0; i<(int)hd; i++)
	{
		// Verbose mode information
		if (vflag == 1)
			printf("Read file %s%s\n", name[i], (hflag[i] ? " (include)" : ""));

		if (!hflag[i])
		{
			// Attempt to read file magic number (OBJECT/ARCHIVE FILES)
			if (read(handle[i], magic, 8) != 8)
			{
				printf("Error reading file %s\n", name[i]);
				close(handle[i]);
				return 1;
			}

			lseek(handle[i], 0L, 0);	// Reset to start of input file

			// Look for RMAC/MAC/GCC ($20107) object files
			if ((getlong(magic) == 0x00000107) || (getlong(magic) == 0x00020107))
			{
				// Process input object file
				if (LoadObject(name[i], handle[i], 0L))
					return 1;
			}
			// Otherwise, look for an object archive file
			else if (strncmp(magic, "!<arch>\x0A", 8) == 0)
			{
				if (LoadArchive(name[i], handle[i]))
					return 1;
			}
			else
			{
				// Close file and error
				printf("%s is not a supported object or archive file\n", name[i]);
				close(handle[i]);
				return 1;
			}
		}
		else
		{
			// INCLUDE FILES
			// If hflag[i] is 1, include this in the data segment; if 2, put it
			// in text segment
			if (LoadInclude(name[i], handle[i], hsym1[i], hsym2[i], hflag[i] - 1))
				return 1;
		}
	}

	// Free include, symbol & object handles
	for(i=0; i<(int)hd; i++)
	{
		free(name[i]);

		if (hflag[i])
		{
			free(hsym1[i]);
			free(hsym2[i]);
		}
	}

	// Reset next handle indicator
	hd = 0;
	return 0;
}


//
// Load newargv with pointers to arguments found in the buffer
//
int parse(char * buf, char * newargv[])
{
	int i = 1;

	if (vflag)
		printf("begin parsing\n");

	while (1)
	{
		while (*buf && strchr(",\t\n\r\14 ", *buf))
			buf++;

		/* test for eof */
		if (*buf == '\0' || *buf == 26)
		{
			if (i == 0)
			{
				printf("No commands in command file\n");
				return -1;
			}
			else
			{
				return i;
			}
		}

		/* test for comment */
		if (*buf == '#')
		{
			/* found a comment; skip to next \n and start over */
			while (*buf && *buf != '\n')
				buf++;

			continue;
		}

		if (i == MAXARGS)
		{
			printf("Too many arguments in command file\n");
			return -1;
		}

		newargv[i] = buf;

		while (!strchr(",\t\n\r\14 ", *buf))
		{
			if (*buf == '\0' || *buf == 26)
			{
				printf("Finished parsing %d args\n", i);
				return i;
			}

			buf++;
		}

		*buf++ = '\0';

		if (vflag)
			printf("argv[%d] = \"%s\"\n", i, newargv[i]);

		i++;
	}
}


//
// Process in a Link Command File
//
int docmdfile(char * fname)
{
	int fd;                                     // File descriptor
	unsigned size;                              // Command file size
	char * ptr;                                 // Pointer
	int newargc;                                // New argument count
	char * (*newargv)[];                        // New argument value array

	// Verbose information
	if (vflag > 1)
		printf("docmdfile(%s)\n", fname);

	// Allocate memory for new argument values
	newargv = malloc((long)(sizeof(char *) * MAXARGS));

	if (!newargv)
	{
		printf("Out of memory.\n");
		return 1;
	}

	// Attempt to open and read in the command file
	if (fname)
	{
		if ((fd = open(fname, _OPEN_FLAGS)) < 0)
		{
			printf("Cannot open command file %s.\n", fname);
			return 1;
		}

		size = FileSize(fd);

		if ((ptr = malloc(size + 1)) == NULL)
		{
			printf("Out of memory.\n");
			close(fd);
			return 1;
		}

		if (read(fd, ptr, size) != (int)size)
		{
			printf("Read error on command file %s.\n", fname);
			close(fd);
			return 1;
		}

		*(ptr + size) = 0;                      // Null terminate the buffer
		close(fd);
	}
	else
	{
		printf("No command filename specified\n");
		return 1;
	}

	// Parse the command file
	if ((newargc = parse(ptr, *newargv)) == -1)
	{
		return 1;
	}

	// Process the inputted flags
	if (doargs(newargc, *newargv))
	{
		printf("docmdfile: doargs returns TRUE\n");
		return 1;
	}

	free(ptr);
	free(newargv);

	return 0;
}


//
// Take an Argument List and Parse the Command Line
//
int doargs(int argc, char * argv[])
{
	int i = 1;					// Iterator
	int c;						// Command line character
	char * ifile, * isym;		// File name and symbol name for -i

	// Parse through option switches & files
	while (i < argc)
	{
		// Process command line switches
		if (argv[i][0] == '-')
		{
			if (!argv[i][1])
			{
				printf("Illegal option argument: %s\n\n", argv[i]);
				display_help();
				return 1;
			}

			c = argv[i++][1];			// Get next character in command line

			// Process command line switch
			switch (c)
			{
			case '?':					// Display usage information
			case 'h':
			case 'H':
				display_version();
				display_help();
				return 1;
			case 'a':
			case 'A':					// Set absolute linking on
				if (aflag)
					warn('a', 1);

				if (i + 2 >= argc)
				{
					printf("Not enough arguments to -a\n");
					return 1;
				}

				aflag = 1;				// Set abs link flag
				// Segment order is TEXT, DATA, BSS
				// Text segment can be 'r', 'x' or a value
				ttype = 0;

				if ((*argv[i] == 'r' || *argv[i] == 'R') && !argv[i][1])
				{
					ttype = -1;			// TEXT segment is relocatable
				}
				else if ((*argv[i] == 'x' || *argv[i] == 'X'))
				{
					printf("Error in text-segment address: cannot be contiguous\n");
					return 1;
				}
				else if ((ttype = 0), GetHexValue(argv[i], &tval))
				{
					printf("Error in text-segment address: %s is not 'r', 'x' or an address.", argv[i]);
					return 1;
				}

				i++;
				// Data segment can be 'r', 'x' or a value
				dtype = 0;

				if ((*argv[i] == 'r' || *argv[i] == 'R') && !argv[i][1])
				{
					dtype = -1;			// DATA segment is relocatable
				}
				else if ((*argv[i] == 'x' || *argv[i] == 'X'))
				{
					dtype = -2;			// DATA follows TEXT
				}
				else if ((dtype = 0), GetHexValue(argv[i], &dval))
				{
					printf("Error in data-segment address: %s is not 'r', 'x' or an address.", argv[i]);
					return 1;
				}

				i++;
				btype = 0;

				// BSS segment can be 'r', 'x' or a value
				if ((*argv[i] == 'r' || *argv[i] == 'R') && !argv[i][1])
				{
					btype = -1;			// BSS segment is relocatable
				}
				else if ((*argv[i] == 'x' || *argv[i] == 'X'))
				{
					btype = -3;			// BSS follows DATA
				}
				else if ((btype = 0), GetHexValue(argv[i], &bval))
				{
					printf("Error in bss-segment address: %s is not 'r', 'x[td]', or an address.", argv[i]);
					return 1;
				}

				i++;
				break;
			case 'b':
			case 'B':					// Don't remove muliply defined locals
				if (bflag)
					warn('b', 1);

				bflag = 1;
				break;
			case 'c':
			case 'C':					// Process a command file
				if (i == argc)
				{
					printf("Not enough arguments to -c\n");
					return 1;
				}

				if (docmdfile(argv[i++]))
				{
					return 1;
				}

				break;
			case 'd':
			case 'D':					// Wait for "return" before exiting
				if (dflag)
					warn('d', 0);

				dflag = 1;
				waitflag = 1;
				break;
			case 'e':
			case 'E':					// Output COFF (absolute only)
				cflag = 1;
				break;
			case 'g':
			case 'G':					// Output source level debugging
				printf("\'g\' flag not currently implemented\n");
				gflag = 0;
				/*
				if (gflag) warn('g', 1);
				gflag = 1;
				*/
				break;
			case 'i':
			case 'I':					// Include binary file
				if (i + 2 > argc)
				{
					printf("Not enough arguments to -i\n");
					return 1;
				}

				ifile = argv[i++];
				isym = argv[i++];

				// handle -ii (No truncation)
				if ((argv[i-3][2] == 'i') || (argv[i-3][2] == 'I'))
				{
					if (!cflag)
						printf("warning: (-ii) COFF format output not specified\n");
				}
				// handle -i (Truncation)
				else
				{
					if (strlen(isym) > 7)
						isym[7] = '\0';
				}

				// Place include files in the DATA segment only
				if (DoFile(ifile, DSTSEG_D, isym))
					return 1;

				break;
			case 'l':
			case 'L':					// Add local symbols
				if (lflag)
					warn('l', 1);

				lflag = 1;
				break;
			case 'm':
			case 'M':					// Produce load symbol map
				if (mflag)
					warn('m', 1);

				mflag = 1;
				break;
			case 'n':
			case 'N':					// Output no header to .abs file
				if (noheaderflag)
					warn('n', 1);

				noheaderflag = 1;
				break;
			case 'o':
			case 'O':					// Specify an output file
				if (oflag)
					warn('o', 1);

				oflag = 1;

				if (i >= argc)
				{
					printf("No output filename following -o switch\n");
					return 1;
				}

				if (strlen(argv[i]) > FARGSIZE - 5)
				{
					printf("Output file name too long (sorry!)\n");
					return 1;
				}

				strcpy(ofile, argv[i++]);
				break;
			case 'r':
			case 'R':					// Section alignment size
				if (rflag)
					warn('r', 1);

				rflag = 1;

				switch (argv[i-1][2])
				{
					case 'w': case 'W': secalign = 1;  break; // Word alignment
					case 'l': case 'L': secalign = 3;  break; // Long alignment
					case 'p': case 'P': secalign = 7;  break; // Phrase alignment
					case 'd': case 'D': secalign = 15; break; // Double phrase alignment
					case 'q': case 'Q': secalign = 31; break; // Quad phrase alignment
					default:            secalign = 7;  break; // Default phrase alignment
				}

				break;
			case 's':
			case 'S':					// Output only global symbols
				if (sflag)
					warn('s', 1);

				sflag = 1;
				break;
			case 'v':
			case 'V':					// Verbose information
				if (!vflag && !versflag)
				{
					display_version();
				}

				vflag++;
				break;
			case 'w':
			case 'W':					// Show warnings flag
				if (wflag)
					warn('w', 1);

				wflag = 1;
				break;
			case 'z':
			case 'Z':					// Suppress banner flag
				if (zflag)
					warn('z', 1);

				zflag = 1;
				break;
			default:
				printf("unknown option argument `%c'\n", c);
				return 1;
			}
		}
		else
		{
			// Not a switch, then process as a file
			if (DoFile(argv[i++], 0, NULL))
				return 1;
		}
	}

	if (!oflag && vflag)
	{
		strcpy(ofile, "output");
		printf("Output file is %s[.ext]\n", ofile);
	}

	if (oflag && vflag)
		printf("Output file is %s\n", ofile);

	if (sflag)
		lflag = 0;

	// No problems encountered
	return 0;
}


//
// Display Version Information
//
void display_version(void)
{
	if (displaybanner)// && vflag)
	{
		printf("\nReboot's Linker for Atari Jaguar\n");
		printf("Copyright (c) 199x Allan K. Pratt, 2014 Reboot\n");
		printf("V%i.%i.%i %s (%s)\n\n", MAJOR, MINOR, PATCH, __DATE__, PLATFORM);
	}
}


//
// Display Command Line Help
//
void display_help(void)
{
	printf("Usage:\n");
	printf("    %s [-options] file(s)\n", cmdlnexec);
	printf("\n");
	printf("Options:\n");
	printf("   -? or -h                display usage information\n");
	printf("   -a <text> <data> <bss>  output absolute file\n");
	printf("                           hex value: segment address\n");
	printf("                           r: relocatable segment\n");
	printf("                           x: contiguous segment\n");
	printf("   -b                      don't remove multiply defined local labels\n");
	printf("   -c <fname>              add contents of <fname> to command line\n");
	printf("   -d                      wait for key after link\n");
	printf("   -e                      output COF absolute file\n");
	printf("   -g                      output source-level debugging\n");
	printf("   -i <fname> <label>      incbin <fname> and set <label>\n");
	printf("   -l                      add local symbols\n");
	printf("   -m                      produce load symbols map\n");
	printf("   -n                      output no file header to .abs file\n");
	printf("   -o <fname>              set output name\n");
	printf("   -r<size>                section alignment size\n");
	printf("                           w: word (2 bytes)\n");
	printf("                           l: long (4 bytes)\n");
	printf("                           p: phrase (8 bytes, default alignment)\n");
	printf("                           d: double phrase (16 bytes)\n");
	printf("                           q: quad phrase (32 bytes)\n");
	printf("   -s                      output only global symbols\n");
	printf("   -v                      set verbose mode\n");
	printf("   -w                      show linker warnings\n");
	printf("   -z                      suppress banner\n");
	printf("\n");
}


//
// Application Exit
//
void rln_exit(void)
{
	char tempbuf[128];

	// Display link status if verbose mode
	if (vflag)
		printf("Link %s.\n", errflag ? "aborted" : "complete");

	// Wait for return key if requested
	if (waitflag)
	{
		printf("\nPress the [RETURN] key to continue. ");
		fgets(tempbuf, 128, stdin);
	}

	exit(errflag);
}


//
// Determine Processor Endianess
//
int get_endianess(void)
{
	int i = 1;
	char * p = (char *)&i;

	if (p[0] == 1)
		return ENDIANNESS_LITTLE;

	return ENDIANNESS_BIG;
}


//
// Application Code Starts Here
//
int main(int argc, char * argv[])
{
	char * s = NULL;				// String pointer for "getenv"
	struct HREC * utemp;			// Temporary hash record pointer
	struct OHEADER * header;		// Pointer to output header structure

	endian = get_endianess();		// Get processor endianess
	cmdlnexec = argv[0];			// Obtain executable name
	s = getenv("RLNPATH");

	if (s)							// Attempt to obtain env variable
		strcpy(libdir, s);			// Store it if found

	// Parse the command line
	if (doargs(argc, argv))
	{
		errflag = 1;
		rln_exit();
	}

	if (!zflag && !vflag)
	{
		display_version();			// Display version information
		versflag = 1;				// We've dumped the version banner
	}

	// Load in specified files/objects and add to processing list
	if (ProcessFiles())
	{
		errflag = 1;
		rln_exit();
	}

	// Work in items in processing list & deal with unresolved list
	if (ProcessLists())
	{
		errflag = 1;
		rln_exit();
	}

	// Check that there is something to link
	if (olist == NULL)
	{
		display_help();
		rln_exit();
	}

	// Report unresolved externals
	if (unresolved != NULL)
	{
		printf("UNRESOLVED SYMBOLS\n");

		// Don't list them if two -u's or more
		if (uflag < 2)
		{
			utemp = unresolved;

			while (utemp != NULL)
			{
				printf("\t%s (",utemp->h_sym);
				put_name(utemp->h_ofile);
				printf(")\n");
				utemp = utemp->h_next;
			}
		}

		if (!uflag)
		{
			errflag = 1;
			rln_exit();
		}
	}

	// Make one output file from input objs
	header = make_ofile();

	if (header == NULL)
	{
		errflag = 1;
		rln_exit();
	}

	// Partial linking
	if (pflag)
	{
		printf("TO DO:Partial linking\n");
		errflag = 1;
	}
	// Relocatable linking
	else if (!aflag)
	{
		printf("TO DO:Relocatable linking\n");
		errflag = 1;
	}
	// Absolute linking
	else
	{
		if (vflag)
		{
			printf("Absolute linking ");

			if (cflag)
				printf("(COF)\n"); 
			else
				printf("(ABS)\n");
		}

		if (vflag > 1)
			printf("Header magic is 0x%04X\n", (unsigned int)header->magic);

		if (write_ofile(header))
			errflag = 1;
	}

	// Display the loaded symbols map
	if (mflag)
		if (write_map(header))
			errflag = 1;

	// Display segment size summary
	if (vflag)
	{
		printf("\n");
		printf("+---------+----------+----------+----------+\n");
		printf("| Segment |     TEXT |     DATA |      BSS |\n");
		printf("| Sizes   |----------+----------+----------|\n");
		printf("| (Hex)   | %8X | %8X | %8X |\n", (unsigned int)header->tsize, (unsigned int)header->dsize, (unsigned int)header->bsize);
		printf("+---------+----------+----------+----------+\n\n");
	}

//printf("BIG_ENDIAN = %i, LITTLE_ENDIAN = %i\n", BIG_ENDIAN, LITTLE_ENDIAN);

	free(header);
	rln_exit();
}

