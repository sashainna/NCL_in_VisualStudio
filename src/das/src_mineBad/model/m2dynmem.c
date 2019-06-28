
/*********************************************************************
**    NAME         :  mdynalloc.c
**       CONTAINS:
**      				umi_get_buffer 
**						umi_print_alloc_messages
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m2dynmem.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:46
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mdebug.h"

static int totalbytes = 0;
/*********************************************************************
**    I_FUNCTION :  umi_get_buffer(bufptr, bufsize, printmessage)
**				This function mallocs more storage for modelling.  Note, this
**				storage is not freed.
**
**    PARAMETERS   
**       INPUT  : 
**          bufsize			number of bytes to allocate.
**				printmessage	prints messages iff this parameter is TRUE.
**       OUTPUT :  
**          bufptr			pointer to malloced storage.
**    RETURNS      : none
**    SIDE EFFECTS : prints messages to user, and also in the files: trc and 
**							psfile.
**    WARNINGS     : none
*********************************************************************/
umi_get_next_buffer(bufptrptr, bufsize, printmessage)
char *(*bufptrptr);
int bufsize;
UU_LOGICAL  printmessage; /* print messages iff TRUE */
{
	uu_denter(UU_MTRC,(us,"umi_next_buffer(%x, %d)", bufptrptr, bufsize));
	*bufptrptr = (char *) uu_malloc(bufsize);
	totalbytes += bufsize;

	if (printmessage)
	{
		umi_print_alloc_messages();
	}
	uu_dexit;
}

	
/*********************************************************************
**    I_FUNCTION : umi_print_alloc_messages() 
**      		Prints messages to the user, and in the trc and psfile indicating
**				the amount of dynamic memory allocated.
**    PARAMETERS   
**       INPUT  : 
**          totalbytes		total number of bytes recently allocated. 
**									(a static external variable)
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : prints messages and sets totalbytes to zero.
**    WARNINGS     : none
*********************************************************************/
umi_print_alloc_messages()
{
	char s[80];
	uu_denter(UU_MTRC,(us,"umi_print_alloc_messages()"));
	if (totalbytes != 0)
	{
		/* print message to user */
		uu_uerror1(UM_MODEL, 143, totalbytes);
		/* message is: an extra %d bytes have been allocated to draw surface(s) */

		/* print message in trc file */
		uu_denter2(0xffff,(s,"%d bytes allocated to draw surface(s)",totalbytes));
		uu_dexit;

		/* print message in psfile */
		sprintf(UM_sbuf, "%d bytes allocated to draw surface(s)", totalbytes);
		um_pscroll(UM_sbuf);
		totalbytes = 0;
	}
	uu_dexit;
}
