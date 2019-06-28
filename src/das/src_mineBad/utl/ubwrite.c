/********************************************************************* 
**  NAME:  ubwrite.c
**
**			uu_bwrite(fd, add, len)
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       ubwrite.c , 25.1
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

static char buffer[BUFSIZ]; 				/* Buffer data here, write when full */
static char *bufpos=buffer;				/* Current position in buffer */
static char *bufend=buffer+BUFSIZ-1;	/* Address at end of buffer */

/*********************************************************************
**    I_FUNCTION     :  uu_bwrite()
**
**		Buffered Unix write.
**       
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : End of line is written into frame buffer.
**    WARNINGS     : none
*********************************************************************/

uu_bwrite(fd, add, len)
int fd;
char *add;
int len;
{
	int i;

	uu_denter(UU_GTRC,(us,"uu_bwrite(%d, %x, %d)", fd, add, len));

	for( i=0; i<len; i++ ) {
		*bufpos++ = *add++;
		if( bufpos > bufend ) {
			uu_bflush(fd);
			bufpos = buffer;
		}
	}
		
	uu_dexit;

}


/*********************************************************************
**    I_FUNCTION     :  uu_bflush()
**
**		Flush the buffer of uu_bwrite().
**       
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : buffer is written to file fd.
**    WARNINGS     : none
*********************************************************************/

uu_bflush(fd)
int fd;
{

	uu_denter(UU_GTRC,(us,"uu_bflush(%d)",fd));

	/* Write the data in buffer */
	write(fd, buffer, bufpos-buffer);

	/* Reset position in the buffer */
	bufpos = buffer;

	uu_dexit;

}
