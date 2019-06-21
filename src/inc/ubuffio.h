
/********************************************************************* 
**  NAME:  ubuffio.h
**
**	 CONTAINS:
**			UU_BREAD(fd, add, len, status)
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       ubuffio.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:02
**
*********************************************************************/

#ifndef BUFSIZ
#include <stdio.h>
#endif

#define BUFFERSIZE	4*BUFSIZ
static char bread_buff[BUFFERSIZE+4];	/* Buffer data here, read when empty */
static char *bread_pos=bread_buff;		/* Current position in bread_buff */
static char *bread_end=bread_buff;		/* End of bread_buffed data */

/*********************************************************************
**    I_DEFINE     :  UU_BREAD()
**
**		Buffered Unix read.  Macro equivalent of function uu_bread
**		found in ubread.c.  Can be used ONLY if all reading is done
**		in one module (.o).
**       
**    PARAMETERS   
**       INPUT  : 
**          fd			file descriptor to read from.
**				useradd	Pointer to storage.
**				len		Length to read.
**				status	Number of bytes read or zero.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : Can only be used if all reading is to be done
**							with UU_BREAD in one module.
*********************************************************************/

#define UU_BREAD(fd, useradd, len, status) \
{ \
	int *add; \
 \
	add = (int *)useradd; \
	status = len; \
	if( bread_pos == bread_end ) { \
		status = uu_get_block(fd); \
	} \
	if( status > 0 ) { \
		*add = *(int *)bread_pos; \
		bread_pos += len; \
		status = len; \
	} \
}


static char bwrite_buff[BUFFERSIZE]; 		/* Buffer data here, write when full */
static char *bwrite_pos=bwrite_buff;				/* Current position in buffer */
static char *bwrite_end=bwrite_buff+BUFFERSIZE;		/* Address at end of buffer */

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

#define UU_BWRITE(fd, useradd, len) { \
	*(int *)bwrite_pos = *(int *)useradd; \
	bwrite_pos += len; \
	if( bwrite_pos >= bwrite_end ) { \
		UU_BFLUSH(fd); \
	} \
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

static UU_BFLUSH(fd)
int fd;
{

	uu_denter(UU_GTRC,(us,"UU_BFLUSH(%d)",fd));

	/* Write the data in buffer */
	write(fd, bwrite_buff, bwrite_pos-bwrite_buff);

	/* Reset position in the buffer */
	bwrite_pos = bwrite_buff;

	uu_dexit;

}


/*********************************************************************
**    I_FUNCTION     :  uu_get_block()
**
**		Read a block of data into the buffer of UU_BREAD().
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
	nbytes = read(fd, bread_buff, BUFFERSIZE);

	/* Reset position in the buffer */
	bread_pos = bread_buff;
	bread_end = bread_buff+nbytes;

	uu_dprint(UU_UITRC,(us,"uu_get_block returns %d",nbytes));
	uu_dexit;
	return(nbytes);
}


