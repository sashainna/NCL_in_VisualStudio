/********************************************************************* 
**  NAME:  wsgifsave.c
**
**			Routines to create a GIF-file
**			
**	CONTAINS: 
**
**    COPYRIGHT 2002 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       wsgifsave.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:05
*********************************************************************/
#include <stdlib.h>
#include <stdio.h>

typedef unsigned Word; 
typedef unsigned char Byte; 
#ifdef UU_RS6000
typedef unsigned Bit; 
#else
typedef unsigned char Bit; 
#endif
typedef int (*UW_METHOD)();

static FILE *OutFile;

static Byte Buffer[256]; 
static int  Index,         
            BitsLeft;   

#define RES_CODES 2

#define HASH_FREE 0xFFFF
#define NEXT_FIRST 0xFFFF

#define MAXBITS 12
#define MAXSTR (1 << MAXBITS)

#define HASHSIZE 9973
#define HASHSTEP 2039

#define HASH(index, lastbyte) (((lastbyte << 8) ^ index) % HASHSIZE)

static Byte *StrChr = NULL;
static Word *StrNxt = NULL,
            *StrHsh = NULL,
            NumStrings;

typedef struct {
    Word LocalScreenWidth,
         LocalScreenHeight;
    Bit GlobalColorTableSize : 3,
         SortFlag             : 1,
         ColorResolution      : 3,
         GlobalColorTableFlag : 1;
    Byte BackgroundColorIndex;
    Byte PixelAspectRatio;
} ScreenDescriptor;

typedef struct {
    Byte Separator;
    Word LeftPosition,
         TopPosition;
    Word Width,
         Height;
    Bit LocalColorTableSize : 3,
         Reserved            : 2,
         SortFlag            : 1,
         InterlaceFlag       : 1,
         LocalColorTableFlag : 1;
} ImageDescriptor;

static int  BitsPrPrimColor, 
            NumColors;       
static Byte *ColorTable = NULL;
static Word ScreenHeight,
            ScreenWidth,
            ImageHeight,
            ImageWidth,
            ImageLeft,
            ImageTop,
            RelPixX, RelPixY;   /* used by InputByte() -function */
static int  (*GetPixel)();

/***********************************************************************
**
**   FUNCTION:	Create
**				Creates a new file, and enables referencing using the
**				global variable OutFile. This variable is only used
**				by these IO-functions, making it relatively simple to
**				rewrite file IO.
**    PARAMETERS
**		INPUT		filename:	name of file to create
**
**		RETURNS		0       - OK
**					-1		- Error opening the file
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int Create(filename)
char *filename;
{
	if ((OutFile = fopen(filename, "wb")) == NULL)
		return -1;
	return 0;
}



/***********************************************************************
**
**   FUNCTION:	Write
**				Output bytes to the current OutFile.
**
**    PARAMETERS
**		INPUT		buf:	pointer to buffer to write
**					len:	number of bytes to write
**
**		RETURNS		0       - OK
**					-1		- Error opening the file
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int Write(buf, len)
void *buf;
unsigned len;
{
	if (fwrite(buf, sizeof(Byte), len, OutFile) < len)
		return -1;
	return 0;
}



/***********************************************************************
**
**   FUNCTION:	WriteByte
**				Output one byte to the current OutFile.
**
**    PARAMETERS
**		INPUT		b:	byte to write
**
**		RETURNS		0       - OK
**					-1		- Error writing to the file
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int WriteByte(b)
Byte b;
{
	if (putc(b, OutFile) == EOF)
		return -1;
	return 0;
}



/***********************************************************************
**
**   FUNCTION:	WriteWord
**				Output one word to the current OutFile.
**
**    PARAMETERS
**		INPUT		w:	word to write
**
**		RETURNS		0       - OK
**					-1		- Error writing to the file
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int WriteWord(w)
Word w;
{
	if (putc(w & 0xFF, OutFile) == EOF)
		return -1;
	if (putc((w >> 8), OutFile) == EOF)
		return -1;
	return 0;
}

/***********************************************************************
**
**   FUNCTION:	Close
**				Close current OutFile.
**
**    PARAMETERS
**		INPUT	none
**
**		RETURNS	none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void Close()
{
	fclose(OutFile);
}

/***********************************************************************
**
**   FUNCTION:	InitBitFile
**				nitiate for using a bitfile. All output is sent to 
**				the current OutFile using the I/O-routines above.
**
**    PARAMETERS
**		INPUT		w:	word to write
**
**		RETURNS		0       - OK
**					-1		- Error writing to the file
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void InitBitFile()
{
	Buffer[Index = 0] = 0;
	BitsLeft = 8;
}


/***********************************************************************
**
**   FUNCTION:	ResetOutBitFile
**				
**
**    PARAMETERS
**		INPUT	none
**
**		RETURNS		0: success
**					1: error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ResetOutBitFile()
{
	Byte numbytes;

	numbytes = Index + (BitsLeft == 8 ? 0 : 1);

	if (numbytes) {
		if (WriteByte(numbytes) != 0)
			return -1;

		if (Write(Buffer, numbytes) != 0)
			return -1;

		Buffer[Index = 0] = 0;
		BitsLeft = 8;
	}
	return 0;
}



/***********************************************************************
**
**   FUNCTION:	WriteBits
**				
**
**    PARAMETERS
**		INPUT		bits:	bits to write from (right justified) 
**					numbits:	number of bits to write
**
**		RETURNS		bits written, or -1 on error.
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int WriteBits(bits, numbits)
int bits, numbits;
{
	int  bitswritten = 0;
	Byte numbytes = 255;

	do 
	{
		if ((Index == 254 && !BitsLeft) || Index > 254) 
		{
			if (WriteByte(numbytes) != 0)
				return -1;

			if (Write(Buffer, numbytes) != 0)
				return -1;

			Buffer[Index = 0] = 0;
			BitsLeft = 8;
		}

		if (numbits <= BitsLeft) {
			Buffer[Index] |= (bits & ((1 << numbits) - 1)) << (8 - BitsLeft);
			bitswritten += numbits;
			BitsLeft -= numbits;
			numbits = 0;
		} 
		else 
		{
			Buffer[Index] |= (bits & ((1 << BitsLeft) - 1)) << (8 - BitsLeft);
			bitswritten += BitsLeft;
			bits >>= BitsLeft;
			numbits -= BitsLeft;

			Buffer[++Index] = 0;
			BitsLeft = 8;
		}
	} while (numbits);

	return bitswritten;
}

/***********************************************************************
**
**   FUNCTION:	FreeStrtab
**				Free arrays used in string table routines
**
**    PARAMETERS
**		INPUT		none
**
**		RETURNS		none
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void FreeStrtab()
{
	if (StrHsh) 
	{
		free(StrHsh);
		StrHsh = NULL;
	}
	if (StrNxt) 
	{
		free(StrNxt);
		StrNxt = NULL;
	}
	if (StrChr) 
	{
		free(StrChr);
		StrChr = NULL;
	}
}



/***********************************************************************
**
**   FUNCTION:	AllocStrtab
**				Allocate arrays used in string table routines		
**
**    PARAMETERS
**		INPUT		none
**
**		RETURNS		0     - OK
**					-1		- Out of memory
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int AllocStrtab()
{
	FreeStrtab();

	if ((StrChr = (Byte *) malloc(MAXSTR * sizeof(Byte))) == 0) 
	{
		FreeStrtab();
		return -1;
	}
	if ((StrNxt = (Word *) malloc(MAXSTR * sizeof(Word))) == 0) 
	{
		FreeStrtab();
		return -1;
    }
	if ((StrHsh = (Word *) malloc(HASHSIZE * sizeof(Word))) == 0)
	{
		FreeStrtab();
		return -1;
	}
	return 0;
}

/***********************************************************************
**
**   FUNCTION:	AddCharString
**				Add a string consisting of the string of index plus
**				the byte b.
**				If a string of length 1 is wanted, the index should
**				be 0xFFFF.
**    PARAMETERS
**		INPUT		index:	index to first part of string, or 0xFFFF is
**								only 1 byte is wanted
**					b:	last byte in string
**
**		RETURNS		Index to new string, or 0xFFFF if no more room
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static Word AddCharString(index, b)
Word index;
Byte b;
{
	Word hshidx;

	if (NumStrings >= MAXSTR)
		return 0xFFFF;
/* 
......search the string table until a free position is found 
*/
	hshidx = HASH(index, b);
	while (StrHsh[hshidx] != 0xFFFF)
		hshidx = (hshidx + HASHSTEP) % HASHSIZE;

	StrHsh[hshidx] = NumStrings;
	StrChr[NumStrings] = b;
	StrNxt[NumStrings] = (index != 0xFFFF) ? index : NEXT_FIRST;

	return NumStrings++;
}

/***********************************************************************
**
**   FUNCTION:	FindCharString
**				Find index of string consisting of the string of index
**				plus the byte b.
**				If a string of length 1 is wanted, the index should
**				be 0xFFFF.
**
**    PARAMETERS
**		INPUT		index:	index to first part of string, or 0xFFFF is
**								only 1 byte is wanted
**					b:	last byte in string
**
**		RETURNS		Index to string, or 0xFFFF if not found
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static Word FindCharString(index, b)
Word index;
Byte b;
{
	Word hshidx, nxtidx;

/* 
......check if index is 0xFFFF. in that case we need only return b,
......since all one-character strings has their bytevalue as their
......index 
*/
	if (index == 0xFFFF)
		return b;
/* 
......search the string table until the string is found, or we find
......HASH_FREE. in that case the string does not exist. 
*/
	hshidx = HASH(index, b);
	while ((nxtidx = StrHsh[hshidx]) != 0xFFFF) 
	{
		if (StrNxt[nxtidx] == index && StrChr[nxtidx] == b)
			return nxtidx;
		hshidx = (hshidx + HASHSTEP) % HASHSIZE;
	}
/* 
.....no match is found 
*/
	return 0xFFFF;
}

/***********************************************************************
**
**   FUNCTION:	ClearStrtab
**				Mark the entire table as free, enter the 2**codesize
**				one-byte strings, and reserve the RES_CODES reserved codes.
**
**    PARAMETERS
**		INPUT		codesize:	number of bits to encode one pixel
**
**		RETURNS		none
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ClearStrtab(codesize)
int codesize;
{
    int q, w;
    Word *wp;

    NumStrings = 0;

/* 
......mark entire hashtable as free 
*/
    wp = StrHsh;
    for (q = 0; q < HASHSIZE; q++)
        *wp++ = HASH_FREE;

/* 
......insert 2**codesize one-character strings, and reserved codes 
*/
    w = (1 << codesize) + RES_CODES;
    for (q = 0; q < w; q++)
        AddCharString(0xFFFF, q);
}



/***********************************************************************
**
**   FUNCTION:	LZW_Compress
**			Perform LZW compression as specified in the GIF-standard.	
**
**    PARAMETERS
**		INPUT		codesize:	number of bits needed to represent one pixelvalue.
**					inputbyte:	function that fetches each byte to compress. must return -1 
**								when no more bytes.
**
**		RETURNS		0:		- OK
**					-1		- Out of memory
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int LZW_Compress(codesize, inputbyte)
int codesize;
UW_METHOD inputbyte;
{
	register int c;
	register Word index;
	int  clearcode, endofinfo, numbits, limit, errcode;
	Word prefix = 0xFFFF;

	InitBitFile();

	clearcode = 1 << codesize;
	endofinfo = clearcode + 1;

	numbits = codesize + 1;
	limit = (1 << numbits) - 1;

	if ((errcode = AllocStrtab()) != 0)
		return errcode;
	ClearStrtab(codesize);
/* 
......first send a code telling the unpacker to clear the stringtable 
*/
	WriteBits(clearcode, numbits);
/* 
......pack image 
*/
	while ((c = inputbyte()) != -1) 
	{
/* 
......now perform the packing. check if the prefix + the new
......character is a string that exists in the table 
*/
		if ((index = FindCharString(prefix, c)) != 0xFFFF) 
		{
			prefix = index;      
		} 
		else 
		{
/* 
......the string does not exist in the table. first write
......code of the old prefix to the file. 
*/
			WriteBits(prefix, numbits);

/* 
......add the new string (the prefix + the new character) to
......the stringtable 
*/
			if (AddCharString(prefix, c) > limit) 
			{
                if (++numbits > 12) 
				{
					WriteBits(clearcode, numbits - 1);
					ClearStrtab(codesize);
					numbits = codesize + 1;
				}
				limit = (1 << numbits) - 1;
			}
			prefix = c;
		}
	}
/* 
......end of info is reached. write last prefix. 
*/
	if (prefix != 0xFFFF)
		WriteBits(prefix, numbits);

	WriteBits(endofinfo, numbits);
	ResetOutBitFile();
	FreeStrtab();

    return 0;
}

/***********************************************************************
**
**   FUNCTION:	BitsNeeded
**				Calculates number of bits needed to store numbers
**				between 0 and n - 1
**
**    PARAMETERS
**		INPUT	n:	number of numbers to store (0 to n - 1)
**
**		RETURNS		Number of bits needed
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int BitsNeeded(n)
Word n;
{
	int ret = 1;

	if (!n--)
		return 0;
	while (n >>= 1)
		++ret;
	return ret;
}

/***********************************************************************
**
**   FUNCTION:	InputByte
**			Get next pixel from image.	
**
**    PARAMETERS
**		INPUT		none
**
**		RETURNS		Next pixelvalue, or -1 if no more pixels
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int InputByte()
{
	int ret;

	if (RelPixY >= ImageHeight)
		return -1;
	ret = GetPixel(ImageLeft + RelPixX, ImageHeight - (ImageTop + RelPixY) + 1);
	if (++RelPixX >= ImageWidth) 
	{
		RelPixX = 0;
		++RelPixY;
	}
	return ret;
}



/***********************************************************************
**
**   FUNCTION:	WriteScreenDescriptor
**				Output a screen descriptor to the current GIF-file
**
**    PARAMETERS
**		INPUT		sd:	pointer to screen descriptor to output
**
**		RETURNS		0       - OK
**					-1		- Error writing to the file
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int WriteScreenDescriptor(sd)
ScreenDescriptor *sd;
{
	Byte tmp;

	if (WriteWord(sd->LocalScreenWidth) != 0)
		return -1;
	if (WriteWord(sd->LocalScreenHeight) != 0)
		return -1;
	tmp = (sd->GlobalColorTableFlag << 7)
			| (sd->ColorResolution << 4)
			| (sd->SortFlag << 3)
			| sd->GlobalColorTableSize;
	if (WriteByte(tmp) != 0)
		return -1;
	if (WriteByte(sd->BackgroundColorIndex) != 0)
		return -1;
	if (WriteByte(sd->PixelAspectRatio) != 0)
		return -1;

	return 0;
}



/***********************************************************************
**
**   FUNCTION:	WriteImageDescriptor
**				Output an image descriptor to the current GIF-file
**
**    PARAMETERS
**		INPUT		id:	pointer to image descriptor to output
**
**		RETURNS		0       - OK
**					-1		- Error writing to the file
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int WriteImageDescriptor(id)
ImageDescriptor *id;
{
	Byte tmp;

	if (WriteByte(id->Separator) != 0)
		return -1;
	if (WriteWord(id->LeftPosition) != 0)
		return -1;
	if (WriteWord(id->TopPosition) != 0)
		return -1;
	if (WriteWord(id->Width) != 0)
		return -1;
	if (WriteWord(id->Height) != 0)
		return -1;
	tmp = (id->LocalColorTableFlag << 7)
			| (id->InterlaceFlag << 6)
			| (id->SortFlag << 5)
			| (id->Reserved << 3)
			| id->LocalColorTableSize;
	if (WriteByte(tmp) != 0)
		return -1;

	return 0;
}


/***********************************************************************
**
**   FUNCTION:	uw__gifcreate
**				Create a GIF-file, and write headers for both screen
**				and image.
**
**    PARAMETERS
**		INPUT		filename:	name of file to create (including extension)
**					width:	number of horisontal pixels on screen
**					height:	number of vertical pixels on screen
**					numcolors:	number of colors in the colormaps
**					colorres:	color resolution. Number of bits for each
**								primary color
**
**		RETURNS		0        - OK
**					-1			- Couldn't create file
**									or Error writing to the file
**									or Out of memory allocating color table
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw__gifcreate(filename, width, height, numcolors, colorres)
char *filename;
int width, height, numcolors, colorres;
{
    int q, tabsize;
    Byte *bp;
    ScreenDescriptor SD;

	NumColors = numcolors ? (1 << BitsNeeded(numcolors)) : 0;
	BitsPrPrimColor = colorres;
	ScreenHeight = height;
	ScreenWidth = width;

	if (Create(filename) != 0)
		return -1;

	if ((Write("GIF87a", 6)) != 0)
		return -1;

	SD.LocalScreenWidth = width;
	SD.LocalScreenHeight = height;
	if (NumColors) 
	{
		SD.GlobalColorTableSize = BitsNeeded(NumColors) - 1;
		SD.GlobalColorTableFlag = 1;
	} 
	else 
	{
		SD.GlobalColorTableSize = 0;
		SD.GlobalColorTableFlag = 0;
	}
	SD.SortFlag = 0;
	SD.ColorResolution = colorres - 1;
	SD.BackgroundColorIndex = 0;
	SD.PixelAspectRatio = 0;
	if (WriteScreenDescriptor(&SD) != 0)
		return -1;

	if (ColorTable) 
	{
		free(ColorTable);
		ColorTable = NULL;
	}
	if (NumColors) {
		tabsize = NumColors * 3;
		if ((ColorTable = (Byte *) malloc(tabsize * sizeof(Byte))) == NULL)
			return -1;
		else 
		{
			bp = ColorTable;
			for (q = 0; q < tabsize; q++)
				*bp++ = 0;
		}
	}
	return 0;
}



/***********************************************************************
**
**   FUNCTION:	uw_gif_setcolor
**			Set red, green and blue components of one of the
**			colors. The color components are all in the range
**			[0, (1 << BitsPrPrimColor) - 1]	
**
**    PARAMETERS
**		INPUT:	colornum:	color number to set. [0, NumColors - 1]
**					red:	number of bits to write
**					green:	green component of color
**					blue:	blue component of color
**
**		RETURNS		none
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gif_setcolor(colornum, red, green, blue)
int colornum, red, green, blue;
{
	long maxcolor;
	Byte *p;

	maxcolor = (1L << BitsPrPrimColor) - 1L;
	p = ColorTable + colornum * 3;
	*p++ = (Byte) ((red * 255L) / maxcolor);
	*p++ = (Byte) ((green * 255L) / maxcolor);
	*p++ = (Byte) ((blue * 255L) / maxcolor);
}



/***********************************************************************
**
**   FUNCTION:	uw_gif_cmpimg
**				Compress an image into the GIF-file previousely
**				created using uw__gifcreate(). All color values should
**				have been specified before this function is called.
**				
**				The pixels are retrieved using a user defined callback
**				function. This function should accept two parameters,
**				x and y, specifying which pixel to retrieve. The pixel
**				values sent to this function are as follows:
**				
**				x : [ImageLeft, ImageLeft + ImageWidth - 1]
**				y : [ImageTop, ImageTop + ImageHeight - 1]
**				
**				The function should return the pixel value for the
**				point given, in the interval [0, NumColors - 1]
**				
**		PARAMETERS
**				INPUT
**					left:	screen-relative leftmost pixel x-coordinate
**							of the image
**					top     screen-relative uppermost pixel y-coordinate
**							of the image
**					width   width of the image, or -1 if as wide as
**							the screen
**					height  height of the image, or -1 if as high as
**							the screen
**					getpixel
**							address of user defined callback function.
**							
**		RETURNS		0       - OK
**					-1		- Out of memory
**								or Error writing to the file
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int uw_gif_cmpimg(left, top, width, height, getpixel)
int left, top, width, height;
UW_METHOD getpixel;
{
	int codesize, errcode;
	ImageDescriptor ID;

	if (width <= 0) 
	{
		width = ScreenWidth;
		left = 1;
	}
	if (height <= 0) 
	{
		height = ScreenHeight;
		top = 1;
	}
	if (left <= 0)
		left = 1;
	if (top <= 0)
		top = 1;

	if (NumColors)
		if ((Write(ColorTable, NumColors * 3)) != 0)
			return -1;

/* 
......initiate and write image descriptor 
*/
	ID.Separator = ',';
	ID.LeftPosition = ImageLeft = left;
	ID.TopPosition = ImageTop = top;
	ID.Width = ImageWidth = width;
	ID.Height = ImageHeight = height;
	ID.LocalColorTableSize = 0;
	ID.Reserved = 0;
	ID.SortFlag = 0;
	ID.InterlaceFlag = 0;
	ID.LocalColorTableFlag = 0;

	if (WriteImageDescriptor(&ID) != 0)
		return -1;
/* 
......write code size 
*/
	codesize = BitsNeeded(NumColors);
	if (codesize == 1)
		++codesize;
	if (WriteByte(codesize) != 0)
		return -1;
/* 
......perform compression 
*/
	RelPixX = RelPixY = 0;
	GetPixel = getpixel;
	if ((errcode = LZW_Compress(codesize, InputByte)) != 0)
		return errcode;
/* 
......write terminating 0-byte 
*/
	if (WriteByte(0) != 0)
		return -1;

	return 0;
}



/***********************************************************************
**
**   FUNCTION:	uw_gifclose
**				Close the GIF-file
**
**    PARAMETERS
**		INPUT		none
**
**		RETURNS		0       - OK
**					-1		- Error writing to file
**					
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_gifclose()
{
	ImageDescriptor ID;

	ID.Separator = ';';
	if (WriteImageDescriptor(&ID) != 0)
		return -1;

	Close();

	if (ColorTable) 
	{
		free(ColorTable);
		ColorTable = NULL;
	}

	return 0;
}
