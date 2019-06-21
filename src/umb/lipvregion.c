/*********************************************************************
**	FILENAME: lipvregion.c
**	CONTAINS:   ul_ipv_region_form()
**     MODULE NAME AND RELEASE LEVEL 
**       lipvregion.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:16
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "lcom.h"
#include "gtbl.h"
#include "gobas.h"
#include "mdattr.h"
#include "mpocket.h"
#include "lipv.h"
#include "lipvmach.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"

#define FRLP 1
#define FRLL 2
#define FRUP 3
#define FRUR 4

static UU_LOGICAL Sregfl;
static UU_REAL Sreg[6];

static UD_FSTAT OnText1(),OnRegion(),OnPick();

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_region_form()
**       Handles the NCLIPV Region of Interest form.
**    PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_region_form()
{
	int status,i;
	UU_LOGICAL cmdreject;
	LtDoubleBounds region;
	LtStatus stat;
	LtData stuff;

	static int idum;
	static UD_METHOD methods[] = {OnRegion,OnPick,OnText1,OnPick,OnText1};
	static char called[] = {6,6,6,6,6};
	static int *ans[] = {&Sregfl,&idum,(int *)&Sreg[0],&idum,
		(int *)&Sreg[3]};
	static char traverse[] = {1,1,1,1,1};
/*
.....This routine not supported yet
*/
	return;
/*
.....Set the default values
*/
	Sregfl = LW_region_defined;
	for (i=0;i<6;i++) region[i] = LW_region[i];
/*
.....Setup the traverse fields
*/
	if (Sregfl)
		traverse[1] = traverse[2] = traverse[3] = traverse[4] = 1;
	else
		traverse[1] = traverse[2] = traverse[3] = traverse[4] = 0;
/*
.....Command reject
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject) goto done;
/*
.....Display the form
*/
	status = ud_form1("ipvregion.frm",ans,ans,methods,called,UU_NULL,traverse);
	if (status == -1) goto done;
/*
.....Store values
*/
	LW_region_defined = Sregfl;
	for (i=0;i<6;i++) LW_region[i] = Sreg[i];
/*
.....Set the Region of Interest
*/
	region[LI_MINX] = LW_region[0];
	region[LI_MINY] = LW_region[1];
	region[LI_MINZ] = LW_region[2];
	region[LI_MAXX] = LW_region[3];
	region[LI_MAXY] = LW_region[4];
	region[LI_MAXZ] = LW_region[5];
	LiDataSetDoubleBounds(&stuff,region);
	stat = LiSessionSetProperty(LW_session[LW_mach_mode],
		LI_SESS_PROP_MW_REGION_INTEREST,&stuff);
	if (stat != LI_STATUS_OK)
		ud_wrerr("Could not set Region of Interest.");
/*
.....End of routine
*/
done:;
	UD_UNMARK(cmdreject);
}

/*********************************************************************
**    S_FUNCTION     :  OnPick(filedno, val, stat)
**       Handles the Pick buttons in the Region of Interest form.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnPick(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int inc,button,event;
	char sbuf[80];
	LtPoint orig;
	LtVector norm;
	LtPickedEntityList entlist;
	LW_rv_picked_entity rvent;
/*
.....Set the appropriate prompt
*/
	if (*fieldno == FRLP)
	{
		inc = 0;
		strcpy(sbuf,"Select Lower Left corner of Region");
	}
	else
	{
		inc = 3;
		strcpy(sbuf,"Select Upper Right corner of Region");
	}
/*
.....Get the user selection
*/
	ud_form_invis();
	event = ul_ipv_pick_entity(sbuf,LI_ENTITY_TYPE_FACE,&entlist,&rvent,
		orig,norm,&button);
	ud_form_vis();
/*
.....Exit on middle or right mouse button
.....or no entities picked
*/
	if (event != 3 || button != 1 || !rvent.picked) goto done;
/*
.....Store the picked location
*/
	Sreg[inc] = orig[0];
	Sreg[inc+1] = orig[1];
	Sreg[inc+2] = orig[2];
	if (inc == 0)
		ud_update_answer(FRLL,&Sreg[0]);
	else
		ud_update_answer(FRUR,&Sreg[3]);
/*
.....Get rid of the picking list
*/
	if (LW_mach_mode == LW_VISICUT)
		LiViPickedEntityListDestroy(entlist);
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}


/*********************************************************************
**    S_FUNCTION     :  OnRegion(filedno, val, stat)
**       Handles the Limit Simulation checkbox in the Region of Interest
**       form.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnRegion(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Set the region field masks
.....Based on the Limit Simulation check box
*/
	ud_set_traverse_mask(FRLP,Sregfl);
	ud_set_traverse_mask(FRLL,Sregfl);
	ud_set_traverse_mask(FRUP,Sregfl);
	ud_set_traverse_mask(FRUR,Sregfl);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnText1(filedno, val, stat)
**       Handles the text fields in the Region of Interest form.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnText1(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
	case FRLL:
		um_vctovc(val->frmvec,&Sreg[0]);
		break;
	case FRUR:
		um_vctovc(val->frmvec,&Sreg[3]);
		break;
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}
