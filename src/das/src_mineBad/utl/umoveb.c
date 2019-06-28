/*********************************************************************
**    NAME         :  umoveb.c
**       CONTAINS:
**       uu_move_byte()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       umoveb.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:54
*********************************************************************/

#include "usysdef.h"
/* #include "umoveb.h" */

/*********************************************************************
**    E_FUNCTION     :  char* uu_move_byte(from_ptr, to_ptr, length)
**       move any bytes - don't look
*********** include umoveb.h in any code using uu_move_byte()!!!!!!!!!
**				portable version of uu_move_byte() will be used on any new
**				machine until a faster machine dependent version is added to
**				umoveb.h for that machine.
**    PARAMETERS   
**       INPUT  : 
**				from_ptr,	pointer to where to move bytes from
**				to_ptr,		pointer to where to mobe bytes to
**				length,		number of bytes to move
**       OUTPUT :  
**          none
**    RETURNS      : to_ptr
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/* #ifndef uu_move_byte */ /* can't do this unless we fix SAL compiler & */
									/* ddltool & ????? to include umoveb.h in the */
									/* files they produce */
char	*
uu_move_byte(from_ptr, to_ptr, length)
register char	*from_ptr;
register char	*to_ptr;
register int	length;				/* unsigned ? */
{
	char	*ptr1;

	ptr1 = to_ptr;			/* save pointer to return */
	if ( !((int)to_ptr & (UU_BYTEPW - 1)) &&	/* chk lo bits for aligned */
	     !((int)from_ptr & (UU_BYTEPW - 1)) )	/* chk lo bits for aligned */
	{
		while (length >= UU_BYTEPW)				/* whole words left? */
		{
			*(int *)to_ptr = *(int *)from_ptr;	/* move a word */
			to_ptr += UU_BYTEPW;
			from_ptr += UU_BYTEPW;
			length -= UU_BYTEPW;
		}
	}
	while (length)							/* move any bytes remaining or unaligned */
	{
		*to_ptr++ = *from_ptr++;		/* move a byte */
		length--;
	}
	return(ptr1);
}
/* #endif */
