/*********************************************************************
**    NAME         : salloc.h 
**       Allocation utilities header 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       salloc.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:50
*********************************************************************/

#ifndef SALLOCH


#include "usysdef.h"

/* conversion from bytes to int units */
#define BYTES_TO_INTS(bytes) (((bytes)+sizeof(int)-1)/sizeof(int))

/* conversion from string dimension to int units */
#define STRING_TO_INTS(dim) BYTES_TO_INTS(dim+1)

/* size of largest string in int units */
#define STR_SIZE	STRING_TO_INTS( STRMAX )

/* number of bits per byte */
#define BITS_PER_BYTE	8

/* number of bits per int unit */
#define BITS_PER_INT		(BITS_PER_BYTE*sizeof(int))

/* conversion from bits to ints */
#define BITS_TO_INTS(bits)		((bits+BITS_PER_INT-1)/BITS_PER_INT)

/* size of UU_REAL in int units */
#define REAL_SIZE (sizeof(UU_REAL)/sizeof(int))

/* size of US_COORD in int units */
#define COORD_SIZE (3*REAL_SIZE)

/* size of US_TRANSF in int units */
#define TRANSF_SIZE (12*REAL_SIZE)

/* size of largest SET in int units */
#define SET_SIZE	BITS_TO_INTS( SETMAX )

/* word index in set given bit index */
#define SET_WORD_INDEX(index) (index >> 5)

/* bit position in word given by WORD_INDEX given bit index */
#define SET_BIT_INDEX(index) (index & 0x001F)


#define SALLOCH
#endif
