#include "usysdef.h"
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#if UU_COMP != UU_WIN2K
#include <sys/time.h>
#endif

/********************************************************************* 
**  E_FUNCTION:  uu_delay(n) 
**
**      Delay n milleseconds. This routine is system dependent. 
**			This implementation is for UNIX.
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       sdelay.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:08
**
**  PARAMETERS   
**      INPUT:  int n -- number of milleseconds to delay.
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
/*
.....WinNT
*/
/*#if UU_COMP != UU_WINNT
/*static int *slep()
/*{
/*	signal(SIGALRM,slep);
/*}
#endif*/

int uu_delay(itim)			/* delay itim  milleseconds */
int itim;
{
#if UU_COMP != UU_VAXVMS
#if UU_COMP == UU_IRIS4D && !defined UU_RS6000
    	sginap(itim);
#else
	int jtim;
/*#if UU_COMP != UU_WINNT
/*	struct itimerval tim,otim;
 /*   	jtim = itim * 10000;
/*	tim.it_interval.tv_sec = 0;
/*	tim.it_interval.tv_usec = jtim;
/*	tim.it_value.tv_sec = 0;
/*	tim.it_value.tv_usec = jtim;
/*	signal(SIGALRM,slep);
/*	setitimer(ITIMER_REAL,&tim,&otim);
/*	pause();
/*#else*/
#if UU_COMP != UU_WIN2K
		jtim = itim * 10000;
		usleep(jtim);
#else
		jtim = itim * 10;
		sleep(jtim);
#endif
/*#endif*/
#endif
#else
        int iflag;
        int itime[2],istat;
        iflag = 2;
        if (itim < 0)     itim = 0;
        if (itim > 21473) itim = 21473;
        itime[0] = itim * (-100000);
        itime[1] = -1;
        istat = sys$setimr(iflag,&itime[0],0,0,0);
        sys$waitfr (iflag);
#endif
}

/********************************************************************* 
**  E_FUNCTION:  uu_delay_milli(n) 
**
**      Delay n milliseconds. This routine is system dependent. 
**
**  PARAMETERS   
**      INPUT:  int n -- number of milleseconds to delay.
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int uu_delay_milli(itim)			/* delay itim  milleseconds */
int itim;
{
	int jtim;
	jtim = itim;
	sleep(jtim);
}
