
#include "usysdef.h"
#include "xenv1.h"
#include "udebug.h"

/********************************************************************* 
**  E_FUNCTION:  uu_signal(channel, func)
**
**      Call function func on user interrupt (SIGINT in UNIX).
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       signal.c , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:08
**
**  PARAMETERS   
**      INPUT:  int channel --	Open serial io channel in VMS.
**											Ignored in UNIX land.
**					 int (*func)() --	Function to call on user interrupt.
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#ifdef UU_UNIX 		/* VMS routine is separate */
#include <signal.h>

int uu_signal(channel, func)
int channel;
int (*func)();
{

	signal(SIGINT, func);		/* Set UNIX signal handler */

}

#endif								/* end of ifdef UU_UNIX */

#if UU_COMP == UU_VAXVMS
/*#if UU_OPSYS==UU_VMS || UU_OPSYS==UU_ALPHAVMS 			/* VMS signal handling */
#include <stdio.h>
#include iodef
#include ssdef
#include descrip
#include psldef

int uu_signal(fd, func)
int fd;
int (*func)();
{
	char *p;
	int stat;
  	int channel;  
 	char *ux_getenv();
	static int mask[2] = { 0, 4 };	/* Default mask enabling control-B */

   uu_denter(UU_GTRC,(us,"uu_signal( %d %x )", fd, func));

	p = ux_getenv("UU_VMSINTRP",UX_PRTERRS);

	if( p != NULL )
		mask[1] =  1 << ( *p - '@' );				 /* Set mask */

   uu_dprint(UU_GTRC,(us,"mask %d",mask[1]));

  	channel = uu_ttfd(fd);

	stat = SYS$QIO( 0, channel, (IO$_SETMODE|IO$M_OUTBAND), 0, 0, 0,
						 func, mask, 0, 0, 0, 0);

   uu_dprint(UU_GTRC,(us,"returned from SYS$QIO stat=%d", stat));

	if(stat != SS$_NORMAL) LIB$SIGNAL(stat);
   uu_dexit;
}
#endif
