/********************************************************************* 
**  NAME:  ubread.c
**
**			uu_bread(fd, add, len)
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       ubread.c , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:52
**
*********************************************************************/

#include <stdio.h>
#include "udebug.h"

/*#ifdef WNT */
#if (UU_COMP == UU_WIN2K)
#include <io.h>
#define write _write
#define read _read
#define close _close
#define open _open
#define creat _creat
#define lseek _lseek
#endif

#define BUFFERSIZE	(4*BUFSIZ+4)		/* Size of our data buffer */
static char buffer[BUFFERSIZE];			/* Buffer data here, read when empty */
static char *bufpos=buffer;				/* Current position in buffer */
static char *bufend=buffer;				/* End of buffered data */

/*********************************************************************
**    I_FUNCTION     :  uu_bread()
**
**		Buffered Unix read.
**       
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_bread(fd, add, len)
int fd;
int *add;
int len;
{
	int i, nbytes;

	uu_denter(UU_GTRC,(us,"uu_bread(%d, %x, %d)", fd, add, len));

	nbytes = len;

/*	for( i=0; i<len; i++ ) {
/*		uu_dprint(-1,(us,"bufpos %x, bufend %x",bufpos, bufend));
/*
/*		if( bufpos == bufend ) {				/* Need to read next block */
/*			nbytes = uu_get_block(fd);			/* Get next block */
/*			if( nbytes <= 0 ) {	 				/* Out of data ? */
/*				break;
/*			}
/*		}
/*
/*		*add++ = *bufpos++;						/* Copy data from buffer */
/*	}
*/

	if( bufpos == bufend ) {				/* Need to read next block */
		nbytes = uu_get_block(fd);			/* Get next block */
	}
	
	if( nbytes > 0 ) {
		*add = *(int *)bufpos;
		bufpos += len;
		nbytes = len;
	}


	uu_dprint(-1,(us,"uu_bread returns %d",nbytes));
	uu_dexit;
/*	return(i);										/* Return # bytes copied */
	return(nbytes);										/* Return # bytes copied */
}


/*********************************************************************
**    I_FUNCTION     :  uu_get_block()
**
**		Read a block of data into the buffer of uu_bread().
**       
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : number of bytes read or -1 (error)
**    SIDE EFFECTS : buffer pointers are updated
**    WARNINGS     : none
*********************************************************************/

static int uu_get_block(fd)
int fd;
{
	int nbytes;

	uu_denter(UU_UITRC,(us,"uu_get_block(%d)",fd));

	/* Read another block of data into buffer */
	nbytes = read(fd, buffer, BUFFERSIZE);

	/* Reset position in the buffer */
	bufpos = buffer;
	bufend = buffer+nbytes;

	uu_dprint(UU_UITRC,(us,"uu_get_block returns %d",nbytes));
	uu_dexit;
	return(nbytes);
}
