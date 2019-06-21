/*********************************************************************
**    NAME         :  nclsig.c
**       CONTAINS:
**                  nclstub()      -- stub routine called from interface
**                                    while parsing the interrupt key on sun.
**                  nclsig()       -- called by ncl to send keyboard interrupts
**                                    to NCL subroutine RSTTRM
**                  nrstsg()       -- called by ncl to send keyboard interrupts
**                                    back to unicad handler.
**                  nrstrmx()       -- Call NCL routine RSTTRM to handle 
**                                    a keyboard interrupt.
**                  setintr()
**                  ckintr()       -- called by ncl to set interrupt flag
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nclsig.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       08/31/15 , 11:09:15
*********************************************************************/
#include "gtbl.h"
#include "gdidd.h"
#include "usysg.h"
#include "usignal.h"
#include "driver.h"
#include "nclfc.h"

void nclsig();
static UU_LOGICAL Set_intr=UU_FALSE;

/*********************************************************************
**    E_FUNCTION     : nrstrm()
**       Call NCL routine RSTTRM to handle a keyboard interrupt
**          and re-arm interrupt handler.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void nrstrm(sig,code,svp)
int sig,code;
int *svp;
{
   rsttrm();
   nclsig();
	return;
}
/*********************************************************************
**    E_FUNCTION     : nclsig()
**       Send keyboard interrupts to  NCL subroutine RSTTRM.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void nclsig()
{
   int *status;
   static int trap = 1;

   /* RAH: call uu_trap() to setup unicad signal trapping */
   if (trap)
	{
		trap = 0;
		uu_trap();
	}
   status = (int *)signal(SIGINT,nrstrm);
#if (UU_COMP == UU_SUN)
   status = (int *)signal(SIGURG,nrstrm);
#endif
}
/*********************************************************************
**    E_FUNCTION     : nrstsg()
**       Set keyboard interrupts back to unicad handler.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nrstsg()
{
   uu_trap();
}
/*********************************************************************
**    E_FUNCTION     : setintr()
**       Forces the interrupt flag to be set.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void setintr(ifl_intr, ifl35)
{
	Set_intr = UU_TRUE;
}

/*********************************************************************
**    E_FUNCTION     : ckintr()
**       Called by NCL to set the interrupt flag if interrupt key is 
**       in effect. (SGI ONLY)
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ckintr(ifl_intr, ifl35)
short *ifl_intr;
short *ifl35;
{
	int stat,event,xy[2];
	int cursor;
	if (*ifl35 == 0)
	{
/*
.....Interrupt flag has been forced
*/
		if (Set_intr)
		{
			Set_intr = UU_FALSE;
			rsttrm();
			return;
		}
/*
.....in Window PC, the GetMessage somehow will change the current cursor, so
.....we need to reset the current curor after we done the message loop
.....before we set the cursor in every UW_EVENT routine, nor we have the flag
.....to not reset in UW_EVENT routine
*/
#if UU_COMP == UU_WIN2K
		cursor = uw_ntgetcur_cursor();
#endif
check_event:;
		stat = (*(ug_gksstli.wsopen[0].connid)[UW_EVENT])(&event, 0, xy, 0);
		if ((stat == 1 && event == 27) || (stat == 1 && event == 3) ||
			(stat == 3 && event == 3)) 
			rsttrm();
/*
.....check if there is still window event there since if there is, we need 
.....finished that before we continue
*/
		if (ud_chkwin_event())
			goto check_event;
#if UU_COMP == UU_WIN2K
		uw_ntsetcursor(cursor);
		if (!hostid_ck()) rsttrm();
#endif
/*
.....Flush the graphics
*/
		ud_updatews(UG_SUPPRESS);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclstub()
**       Stub routine to toss erroneous events off que when parsing
**          an interrupt on Sun.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclstub()
	{
	}
