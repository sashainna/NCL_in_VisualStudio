#include "usysdef.h"
/*********************************************************************
**    NAME         :  ngtimx.c
**       CONTAINS:
**          gtimx()
**          ncl_time()
**
**    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**        ngtimx.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:09:02
*********************************************************************/

#include "nclfc.h"
#include "mfort.h"
#if UU_COMP == UU_IRIS4D && !defined UU_RS6000
#include <sys/times.h>
#include <limits.h>
#include <stdio.h>

/*********************************************************************
**    E_FUNCTION     : int gtimx (sec, ms)
**      timer routine.
**    PARAMETERS   
**       INPUT  : 
**			none
**       OUTPUT :  
**          sec    - current time in seconds
**          ms     - current time in milliseconds
**    RETURNS      : 
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gtimx(sec,ms)
int *sec,*ms;
{
	struct tms buffer;
	*sec = 0;
	*ms = times(&buffer) * CLK_TCK;
}
 
#else

#include <time.h>
#include <sys/timeb.h>
gtimx(sec,ms)
int *sec,*ms;
{
	struct timeb tp;
	ftime(&tp);
	*sec = tp.time;
	*ms = tp.millitm;
}
#endif

/*********************************************************************
**    E_FUNCTION     : ncl_time(itime,label)
**      Calculates the time between calls and prints out this time on
**      the second call to this routine. If *SET/DEBUGG is not set in
**      the part program, then this routine will do nothing.
**    PARAMETERS   
**       INPUT  : 
**          itim   - Should be set to 0 on the first call and left
**                   unchanged on the second call.
**          label  - Text string to output with time message.  If the
**                   text string is blank, then no message will be output.
**       OUTPUT :  
**          itim   - Returns the current time on the first call and
**                   the time between calls on the second call.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_time (itim,label)
int *itim;
char *label;
{
	int tims,timm;
	char lbuf[200];
	static int btim=0;
	UM_int2 idx,ival;

	idx = 5; getlfl(&idx,&ival); if (ival == 0) return;
/*
.....First time here
.....Get the current time
*/
	if (*itim == 0)
	{
		gtimx(&tims,&timm);
		if (btim == 0) btim = tims;
		tims = tims - btim;
		*itim = tims*1000 + timm;
	}
/*
.....Second time here
.....Calculate the delta time and print it out
*/
	else
	{
		gtimx(&tims,&timm);
		tims = tims - btim;
		*itim = (tims*1000 + timm) - *itim;
		if (label[0] != '\0')
		{
			sprintf(lbuf,"%s time = %d",label,*itim);
			NclxDbgPstr(lbuf);
		}
	}
}
		

