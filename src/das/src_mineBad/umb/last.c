/*********************************************************************
**	FILENAME: last.c
**	CONTAINS:		Routines to trap CTRL/C & CTRL/Y
**				
**    MODULE NAME AND RELEASE LEVEL 
**       last.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:10
*********************************************************************/

#include "usysdef.h"
#include "lcom.h"
#include "xenv1.h"

#ifndef NUMB
#if UU_COMP == UU_VAXVMS
#include "descrip"
#include "iodef"
#endif

#define LIB$M_CLI_CTRLY 0x02000000
static int oldy,chan,iost,stat;

#if UU_COMP == UU_VAXVMS
static int *yast()
{
   	stat = sys$qiow (1,chan,IO$_SETMODE|IO$M_CTRLYAST,
		&iost,0,0,yast,0,25,0,0,0);
	return;
}

static int *cast()
{
	stat = sys$qiow (1,chan,IO$_SETMODE|IO$M_CTRLCAST,
		&iost,0,0,cast,0,25,0,0,0);
	UL_ctrlc = 1;
	rsttrm();
	return;
}

ul_trap_control()
{
	char dev[3],*str;
	int noctrl=LIB$M_CLI_CTRLY;
	$DESCRIPTOR (devd,dev);

/*	stat = lib$disable_ctrl (&noctrl,&oldy); */
	str = ux_getenv ("gio",UX_NPRTERRS);
	strcpy (dev,str);
	stat = sys$assign (&devd,&chan,0,0);
	stat = sys$qiow (1,chan,IO$_SETMODE|IO$M_CTRLCAST,
		&iost,0,0,cast,0,3,0,0,0);
/*	stat = sys$qiow (1,chan,IO$_SETMODE|IO$M_CTRLYAST,
		&iost,0,0,yast,0,25,0,0,0); */
	return;
}

ul_reset_control()
{
/*	stat = lib$enable_ctrl (&oldy); */
	return;
}
#endif
#endif NUMB
