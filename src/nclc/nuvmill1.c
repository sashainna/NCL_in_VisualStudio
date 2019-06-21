/********************************************************************* 
**  NAME:  nuvmill1.cpp
**
**			Interface routines for VoluMill.
**	CONTAINS: 
**       nclu_vmill_open_progress()
**       nclu_vmill_close_progress()
**       nclu_vmill_update_progress()
**    COPYRIGHT 2011 (c) NCCS.  All Rights Reserved. 
**    MODULE NAME AND RELEASE LEVEL
**       nuvmill1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:17
*********************************************************************/
/*
.....NCL headers
*/
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "modef.h"
#include "ncldef.h"
#include "nclfc.h"

#define FPRG 0

extern int NCL_waterline;
/*
.....Internal static routines
*/
static void S_flush_form();
static UD_FSTAT OnInterrupt();
static UD_FSTAT OnClose();

static int Sprogress,Sfrm=-1;
static UU_LOGICAL *Sinterrupt;
void nclu_vmill_close_progress();

/*********************************************************************
**   I_FUNCTION: nclu_vmill_open_progress(interrupt)
**      Displays the VoluMill status form.
**
**   PARAMETERS
**       INPUT  :
**          interrupt = Address of flag to receive user interrupt status.
**                      UU_TRUE = user interrupted calculations.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void nclu_vmill_open_progress(UU_LOGICAL *interrupt)
//UU_LOGICAL *interrupt;
{
	UM_int2 ifl,iflx;
/*
.....Setup form fields
*/
	static char traverse[] = {1,1,1};
	static UD_METHOD methods[] = {UU_NULL,OnInterrupt,OnClose};
	static char called[] = {6,6,6};
	static char display[] = {1,1,1};
	static int *ans[] = {&Sprogress,UU_NULL,UU_NULL};
/*
.....Don't open form in batch mode
*/
	ifl = 35; getifl(&ifl,&iflx);
	if (iflx == 0 && !NCL_waterline)
	{
/*
.....Initialize form
*/
		Sprogress = 0;
		Sinterrupt = interrupt;
/*
.....Display the form
*/
		Sfrm = ud_form_display1("nvmillstat.frm",ans,ans,methods,called,display,
			traverse);
		S_flush_form();
	}
}

/*********************************************************************
**   E_FUNCTION: nclu_vmill_close_progress()
**      Closes the VoluMill status form.
**
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void nclu_vmill_close_progress()
{
/*
.....Close VoluMill status form if it is open
*/
	if (Sfrm != -1) ud_close_dispfrm(Sfrm);
	Sfrm = -1;
}

/*********************************************************************
**   I_FUNCTION: nclu_vmill_update_progress(progress);
**      Receives the STL progress report from the MachineWorks libraries
**      and updates the STL status form.
**
**   PARAMETERS
**       INPUT  :
**          progress  = Progress structure.
**       OUTPUT : none
**   RETURNS: LI_STATUS_INTERRUPT if the user interrupted the STL load.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void nclu_vmill_update_progress(progress)
int progress;
{
/*
.....Performing Waterline roughing
*/
	if (progress != Sprogress)
	{
		if (NCL_waterline)
		{
			ncl_waterline_progress(progress);
		}
/*
.....Update progress field
*/
		else if (Sfrm != -1 && *Sinterrupt != UU_TRUE)
		{
			ud_dispfrm_update_answer(Sfrm,FPRG,&Sprogress);
			S_flush_form();
		}
		Sprogress = progress;
	}
	return;
}

/*********************************************************************
**   I_FUNCTION: OnInterrupt(fieldno,val,stat)
**      User pressed the interrupt button.
**
**   PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnInterrupt(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	*Sinterrupt = UU_TRUE;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnClose()
**      Marks the VoluMill status form as closed.
**
**   PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnClose()
{
	Sfrm = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: S_flush_form()
**      Causes the operating system to update any active forms.
**
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static void S_flush_form()
{
	UM_int2 ifl35,ifl86;
/*
.....Flush the output buffer
.....by checking the input buffer
*/
	if (Sfrm != -1) ud_update_form(Sfrm);
	ifl35 = 0;
	ckintr(&ifl86,&ifl35);
}
