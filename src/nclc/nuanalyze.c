/*********************************************************************
**    NAME         :  nuanalyze.c
**       CONTAINS: User interface routines for geometry analysis.
**
**			nclu_geo_measure
**
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nuanalyze.c , 25.4
**    DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 10:50:24
*********************************************************************/
#include "usysdef.h"
#include "wsgl.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "ginqatt.h"
#include "ginqatt2.h"
#include "gtbl.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mdunits.h"
#include "modef.h"
#include "mattr.h"
#include "mdattr.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdeval.h"
#include "udforms.h"
#include "zsysdep.h"

#include "nccs.h"
#include "ncl.h"
#include "nclcmd.h"
#include "nclfc.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclvx.h"
#include "driver.h"
#include "mgeom.h"
#include "lcom.h"

#include "adraft.h"
#include "dselect.h"
#include "uims.h"

#define N_ATTRIB 20
#define GEOM_TOG 2
#define STAT_FLD 7
#define FGRD 0
#define FSFT 1
#define FGRDO 2
#define FRDT 3
#define FRNG 4

#define FCOL1 0
#define FCOL2 1
#define FCOL3 2
#define FCOL4 3

#define PICK_CLOSEST 0
#define PICK_POINT 1
#define PICK_RADIUS 2

static int Stype=0,Sgrid=0,Shilite[2],Ssgl=1,Sunits=0,Smsgl=1,Sclr[]={1,1};
static int Sfrm,Sfrm1=-1,Sfrm2=-1,Ssegid=0;
static UU_LOGICAL Sactive=UU_FALSE,Sactive1=UU_FALSE,Sactive2=UU_FALSE,
						Smotpick[]={UU_FALSE,UU_FALSE},Spick_geo=UU_TRUE;
static UU_LOGICAL Sgrid_flag=UU_FALSE,Sfirst=UU_TRUE;
static char Subuf[10];
static UU_REAL Stoler,Sradtol,Srange,Sconv;
static UD_LIST stat_list;
static int Sncolor=0,Scolor[2];
static struct NCL_fixed_databag Se[2];
static UM_int2 IPT=NCLI_POINT;
static UM_int2 IVE=NCLI_VECTOR;

static UD_FSTAT OnGeoToler(),OnGeoColor(),OnSelect(),OnClose(),OnToler(),OnClose1(),
						OnColor(),OnClose2();
static int S_set_motion();
static void S_restore_colors(),S_init_list(),S_radius(),S_restore_motion();
static void S_distance();
static UN_motseg *Smptr,*Smptr2,*Shilite_strt[]={0,0},*Shilite_end[]={0,0};
static UU_LIST Spoints[2];
static UN_motseg *Smdisp_ptr[]={0,0},Smotatt[2];

/*********************************************************************
**    E_FUNCTION     : nclu_geo_measure()
**       Processes the NCL Measurement form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_geo_measure()
{
	UM_real8 tol;
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1,1,1,1,1,1, 1};
	static UD_METHOD methods[] = {UU_NULL,OnSelect,UU_NULL,UU_NULL,
		OnGeoToler,OnGeoColor,UU_NULL,UU_NULL,OnClose};
	static char called[] = {6,6,6,6,6,6,6, 6};
	static char display[] = {1,1,1,1,1,1,1, 1};
	static int *ans[] = {&Stype, UU_NULL, &Ssgl, &Sunits, UU_NULL,
		UU_NULL, &Smsgl,(int *)&stat_list, UU_NULL};
/*
.....Initialize routine
*/
	if (Sactive) goto done;
	Sactive = UU_TRUE;

	if (UL_measurement_disttol < UM_DFUZZ)
	{
		gettol(&tol);
		UL_measurement_disttol = tol * 10.;
	}
	if (UL_measurement_radtol < UM_DFUZZ)
	{
		gettol(&tol);
		UL_measurement_radtol = tol;
	}
	UM_len_inttoext(UL_measurement_disttol,Stoler);
	UM_len_inttoext(UL_measurement_radtol,Sradtol);
	UM_len_inttoext(UL_measurement_range,Srange);
	Sgrid = UL_measurement_minsfgrid;
	Sgrid_flag = UL_measurement_sfgridonly;
	Shilite[0] = UL_measurement_color1;
	Shilite[1] = UL_measurement_color2;

	stat_list.num_item = 0;
	stat_list.item = UU_NULL;
	stat_list.answer = UU_NULL;

/*
.....Set traverse fields
.......Not needed if all traverse flags are set to 1 and are
.......unchanged - Andrew 10/1/12
*/
/*	traverse[2] = 1;
	traverse[4] = 1;
	traverse[5] = 1;
	traverse[7] = 1;
	*/
/*
.....Display the Form
*/
	Sfirst = UU_TRUE;
	Sfrm = ud_form_display1("ngeomeas.frm", ans, ans, methods, called, display,
		traverse);
	if (Sfrm == -1) goto err1;
	goto done;
/*
.....Could not create the form
*/
err1:;
	ud_wrerr("Could not create measurement form.");
done:;
	S_restore_colors(UU_FALSE);
	ncl_free_trilist();
	return;
}

/*********************************************************************
**    S_FUNCTION     :  OnSelect(filedno, val, stat)
**       Method called when the Select button is pushed.
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
static UD_FSTAT OnSelect(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,numint,pr,dum,status;
	int pick_mask[UD_NMENTWD], *mask;
	UU_REAL tol;
	UM_coord pt[2];
	UM_vector vec[2];
	UD_PLOCREC pick1, pick2;
	UU_LOGICAL cmdreject, markval;
	struct NCL_fixed_databag e;
	UU_REAL ptuv[2];
	
/*
.....Trap Reject Op for go back to form callback, just go back without jump
.....so must set UU_TRUE
*/
/*	UD_MARK(markval,UU_TRUE);*/
/*
.....We changed UD_MARK in OnNCLFunctions/OnAccelFunctions
.....to jump back from CFrame class to C function,
.....(which could happened if we click some menu in the middle
.....of this picking activation, so the CFrame doesn't
.....get back there message, if we use UD_MARK(markval,UU_TRUE)
.....to get dialog/form callback function, the window will think
.....something is missing and can't find something and
.....get a segmentation error, so if we have to use
.....UD_MARK(markval,UU_FALSE); to jump back to top window handler
.....which is CNCLApp::Run() defined in wsntctl.cpp. I check the
.....form function which is not doing anything after the
.....callback (redisplay the form with new value
.....which we didn't need it in case of reject). so we can just ignore and
.....jump back to window top. In case of we do need do something 
.....after return to form callback, we may need define UD_MARK in
.....form callback function like we did in CNCLframe (wsntframe.cpp)
.....Yurong - 01/04/10
*/
	UD_MARK(markval,UU_FALSE);
	if (markval != 0)
	{
		*fieldno = -1;
		goto done2;
	}
/*
.....Initialize routine
*/
	Sconv =  1.;
	tol = UL_measurement_disttol;
	if (Sunits == 1)
	{
		um_linear_units_str(UM_INCH,Subuf);
		if (UM_cpln.length_unit == UM_MM) Sconv = 1./25.4 ;
	}
	else if (Sunits == 2)
	{
		um_linear_units_str(UM_MM,Subuf);
		if (UM_cpln.length_unit == UM_INCH) Sconv =  25.4;
	}
	else
		um_linear_units_str(UM_cpln.length_unit,Subuf);
	S_restore_colors(UU_FALSE);
/*
.....Trap Reject Op for pickinf event
*/
	UD_MARK (cmdreject, UU_FALSE);
	if (cmdreject != 0)
	{
		S_restore_colors(UU_FALSE);
		if (Ssgl) ud_dspfrm_vis(Sfrm);
		goto done;
	}
/*
.....Let user select geometry to analyze
*/
	for(;;)
	{
/*
.....Take down form
.....If in Single Selection mode
*/
		if (Ssgl) 
			ud_dspfrm_invis(Sfrm);
		else
/*
......we need assign active window to main window
*/
#if UU_COMP == UU_WIN2K
			uw_setact_view();
#endif
/*
.....Initialize the status list
*/
		S_init_list(&stat_list,N_ATTRIB);
/*
.....Get user selection
.....UD_ncl_geometry include VECTOR which we don't want to pick
*/
		for (i=0; i<UD_NMENTWD; i++)
		{
			pick_mask[i] = UD_ncl_pt[i] | UD_ncl_ln[i] | UD_ncl_pv[i] |
				UD_ncl_pl[i] | UD_ncl_allcv[i] | UD_ncl_allsfsh[i] | UD_ncl_ci[i];
		}
		mask = (int *)pick_mask;
		do
		{
			if (Spick_geo) 
			{
				ud_setpick_type(UD_PICK_NORMAL);
				ud_lgeo(UU_TRUE,mask);
				if (Stype == PICK_CLOSEST || Stype == PICK_POINT) pr = 653;
				else pr = 655;
				ud_prmerr("ENTITY picking enabled.");
			}
			else
			{
				if (Smsgl == 0) ud_setpick_type(UD_PICK_MOTION);
				else ud_setpick_type(UD_PICK_MOTSEG);
				ud_lgeo(UU_FALSE,mask);
				ud_prmerr("MOTION picking enabled.");
				pr = 710;
			}
			status = ud_ldas(UD_DASPCKLOC,UA_NCL,pr,&pick1,1,&numint,1,UD_NODEFAULT);
			if (status != UA_ALT_ACTION)
			{
				if (Sncolor > 0) S_restore_colors(UU_FALSE);
				else S_restore_motion();
			}
			if (status == UU_TRUE && !Spick_geo) Smotpick[0] = UU_TRUE;
			else if (status == UA_ALT_ACTION) 
			{
				Spick_geo = !Spick_geo;
				stat = UU_FAILURE;
				continue;
			}
/*
.....Get point on entity
*/
			stat = UU_SUCCESS;
			if (numint != 0 && Sactive && pick1.ppath.depth != 0) 
			{
				if (!Smotpick[0])stat = ncl_get_pkpt1(&pick1,&e,tol,pt[0],vec[0]);
				else
				{
					Smptr = (UN_motseg *)pick1.ppath.pickpath[1];
					Smptr2 = (UN_motseg *)pick1.ppath.pickpath[0];
					stat = S_set_motion(&pick1,&e,pt[0],vec[0],0);
					if (stat != UU_SUCCESS) Smotpick[0] = UU_FALSE;
				}
				if (Stype != PICK_POINT) stat = UU_SUCCESS;
				if (stat != UU_SUCCESS)
					ud_wrerr("You did not pick on the entity.  Try again.");
			}
		} while (stat != UU_SUCCESS);
/*
.....Unhighlight previously selected surface
*/
		S_restore_colors(UU_TRUE);
/*
......if there is no selction, continue, or hit reject to cancel
......when we execute anothe menu/key function while we wait for select
......the return numint will be 0, but in that case, we think we want to continue
*/
		if (numint == 0) break;
		if (numint == 0 || pick1.ppath.depth == 0) continue;
		if (!Sactive) break;
/*
.....Highlight selection
*/
		Se[0] = e;
		if (!Smotpick[0])
		{
			um_get_attrib(&Se[0].key,&Scolor[0],&dum,&dum,&dum,&dum);
			j = Shilite[0];
			ncl_update_color(Se[0].key,j);
			uc_display(&Se[0]);
			Sncolor = 1;
		}
/*
.....Get second entity
*/
		if (Stype == PICK_CLOSEST || Stype == PICK_POINT)
		{
			do
			{
				if (Spick_geo) 
				{
					ud_setpick_type(UD_PICK_NORMAL);
					ud_lgeo(UU_TRUE,mask);
					if (Stype == PICK_CLOSEST || Stype == PICK_POINT) pr = 653;
					else pr = 655;
					ud_prmerr("ENTITY picking enabled.");
				}
				else
				{
					if (Smsgl == 0) ud_setpick_type(UD_PICK_MOTION);
					else ud_setpick_type(UD_PICK_MOTSEG);
					ud_lgeo(UU_FALSE,mask);
					ud_prmerr("MOTION picking enabled.");
					pr = 710;
				}			
				status = ud_ldas(UD_DASPCKLOC,UA_NCL,pr,&pick2,1,&numint,1,UD_NODEFAULT);
				if (status == UU_TRUE && !Spick_geo) Smotpick[1] = UU_TRUE;
				else if (status == UA_ALT_ACTION) 
				{
					Smotpick[1] = UU_FALSE;
					Spick_geo = !Spick_geo;
					stat = UU_FAILURE;
					continue;
				}
				if (!Sactive) goto done;
				if (numint > 0 && pick2.ppath.depth != 0)
				{
/*
........Get point on entity
*/
					if (!Smotpick[1])
/* 
......wrong function call 
......Yurong

						stat = ncl_get_pkpt(&pick2,&Se[1],tol,pt[1],vec[1]);
*/						
						stat = ncl_get_pkpt1(&pick2,&Se[1],tol,pt[1],vec[1]); 
					else
					{
						Smptr = (UN_motseg *)pick2.ppath.pickpath[1];
						Smptr2 = (UN_motseg *)pick2.ppath.pickpath[0];
						stat = S_set_motion(&pick2,&Se[1],pt[1],vec[1],1);
						if (stat != UU_SUCCESS) Smotpick[1] = UU_FALSE;
					}
					if (Stype != PICK_POINT) stat = UU_SUCCESS;
					if (stat != UU_SUCCESS)
						ud_wrerr("You did not pick on the entity.  Try again.");
				}
			} while (stat != UU_SUCCESS);
/*
........Highlight second entity
*/
			if (numint > 0 && !Smotpick[1] && pick2.ppath.depth != 0)
			{
				ncl_retrieve_data_fixed(&Se[1]);
				if (Se[1].key == Se[0].key) Scolor[1] = Scolor[0];
				else um_get_attrib(&Se[1].key,&Scolor[1],&dum,&dum,&dum,&dum);
				j = Shilite[1];
				ncl_update_color(Se[1].key,j);
				uc_display(&Se[1]);
				Sncolor = 2;
			}
			else if (!Smotpick[1] || pick2.ppath.depth == 0) 
			{
				S_restore_colors(UU_FALSE);
				continue;
			}
		}
/*
.....Determine type of analyzation to perform
*/
		switch (Stype)
		{
/*
.....Distance
*/
		case PICK_POINT:
		case PICK_CLOSEST:
			S_distance (Se,pt,vec);
			break;
/*
.....Radius
*/
		case PICK_RADIUS:
			S_radius(&pick1,&Se[0],pt[0]);
			break;
/*
.....Unknown type
*/
		default:
			break;
		}
/*
.....Update the form list
*/
		if (Ssgl) ud_dspfrm_vis(Sfrm);
		ud_dispfrm_update_answer(Sfrm,STAT_FLD,(int *)&stat_list);
		if (Ssgl) break;
	}
/*
.....End of routine
*/
done:;
	if (Ssgl) ud_dspfrm_vis(Sfrm);
/*
.....Unhighlight previously selected surface
*/
	if (!Ssgl) S_restore_colors(UU_FALSE);
	UD_UNMARK(cmdreject);
done2:;
	UD_UNMARK(markval);
	if (Sactive == UU_FALSE)
		*fieldno = -1;
	return (UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose()
**       Method called at when form is closed.
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
static UD_FSTAT OnClose()
{
/*
.....Free the lists
*/
	S_restore_colors(UU_FALSE);
	ud_free_flist(&stat_list);
	if (Sactive1) ud_close_dispfrm(Sfrm1);
	if (Sactive2) ud_close_dispfrm(Sfrm2);
	Sfrm1 = -1;
	Sfrm2 = -1;
	Sactive1 = UU_FALSE;
	Sactive2 = UU_FALSE;
	Sactive = UU_FALSE;
	ud_setpick_type(UD_PICK_NORMAL);
	return ((UD_FSTAT)0);
}

/*********************************************************************
**    E_FUNCTION     : OnGeoToler()
**       Controlling routine for the NCLIPV visual compare tolerances form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnGeoToler()
{
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1, 1,1, 1};
	static UD_METHOD methods[] = {OnToler,OnToler, UU_NULL, OnToler,
		OnToler, OnClose1};
	static char called[] = {6,6, 6,6, 6, 6};
	static char display[] = {1,1, 1,1, 1};
	static int *ans[] = {(int *)&Sgrid, (int *)&Stoler,
		(int *)&Sgrid_flag, (int *)&Sradtol,(int *)&Srange};
/*
.....Make sure form is not already active
*/
	if (Sactive1) goto done;
/*
.....Get the Form input
*/
	Sfrm1 = ud_form_display1("ngeomeastol.frm", ans, ans, methods, called,
		display, traverse);
	if (Sfrm1 != -1) Sactive1 = UU_TRUE;
done:;
	return ((UD_FSTAT)0);
}

/*********************************************************************
**    S_FUNCTION     :  OnToler(filedno, val, stat)
**       Method called when the tolerance field is changed.
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
static UD_FSTAT OnToler(fieldno, val)
int *fieldno;
UD_DDATA *val;
{
	switch (*fieldno)
	{
/*
.....Surface Grid
*/
	case FGRD:
		Sgrid = *val->frmint;
		UL_measurement_minsfgrid = Sgrid;
		break;
/*
.....Surface Tolerance
*/
	case FSFT:
		Stoler = *val->frmflt;
		UM_len_exttoint(Stoler,UL_measurement_disttol);
		break;
/*
.....Surface Grid only
*/
	case FGRDO:
		Sgrid_flag = *val->frmint;
		UL_measurement_sfgridonly = Sgrid_flag;
		break;
/*
.....Radius Tolerance
*/
	case FRDT:
		Sradtol = *val->frmflt;
		UM_len_exttoint(Sradtol, UL_measurement_radtol);
		break;
/*
.....Range
*/
	case FRNG:
		Srange = *val->frmflt;
		UM_len_exttoint(Srange,UL_measurement_range);
		break;
	}
	return ((UD_FSTAT)0);
}


/*********************************************************************
**    S_FUNCTION     :  OnClose1()
**       Method called at when modals form is closed.
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
static UD_FSTAT OnClose1()
{
	Sactive1 = UU_FALSE;
	return ((UD_FSTAT)0);
}

/*********************************************************************
**    E_FUNCTION     : OnGeoColor()
**       Controlling routine for the Measure Color form. Sets the
**       highlighting colors and the result display colors.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnGeoColor()
{
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1, 1,1};
	static UD_METHOD methods[] = {OnColor,OnColor,OnColor,OnColor,OnClose2};
	static char called[] = {6,6, 6,6, 6};
	static char display[] = {1,1, 1,1};
	static int *ans[] = {(int *)&Shilite[0], (int *)&Shilite[1],
		(int *)&Sclr[0], (int *)&Sclr[1]};
/*
.....Make sure form is not already active
*/
	if (Sactive2) goto done;
/*
.....Get the Form input
*/
	Sfrm2 = ud_form_display1("ngeomeasclr.frm", ans, ans, methods, called,
		display, traverse);
	if (Sfrm2 != -1) Sactive2 = UU_TRUE;
done:;
	return ((UD_FSTAT)0);
}

/*********************************************************************
**    S_FUNCTION     :  OnColor(filedno, val, stat)
**       Method called when the color fields are traversed.
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
static UD_FSTAT OnColor(fieldno, val)
int *fieldno;
UD_DDATA *val;
{
	switch (*fieldno)
	{
/*
.....First selection color
*/
	case FCOL1:
		Shilite[0] = *val->frmint;
		UL_measurement_color1 = Shilite[0];
		break;
/*
.....Second selection color
*/
	case FCOL2:
		Shilite[1] = *val->frmint;
		UL_measurement_color2 = Shilite[1];
		break;
/*
.....First distance pick color
*/
	case FCOL3:
		Sclr[0] = *val->frmint;
		break;
/*
.....Second distance pick color
*/
	case FCOL4:
		Sclr[1] = *val->frmint;
		break;
	}
	return ((UD_FSTAT)0);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose2()
**       Method called at when color form is closed.
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
static UD_FSTAT OnClose2()
{
	Sactive2 = UU_FALSE;
	return ((UD_FSTAT)0);
}

/*********************************************************************
**    S_FUNCTION     :  S_init_list(list)
**       Initializes a form list field.
**    PARAMETERS
**       INPUT  :
**          list     List to initialize.
**          size     Number of entities to allocate for list.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_init_list(list,size)
UD_LIST *list;
int size;
{
/*
.....Initialize the list
*/
	ud_free_flist(list);
	list->item = (char **)uu_malloc(size*sizeof(char *));
	list->num_item = 0;
	list->answer = (char *)uu_malloc(sizeof(char)*80);
}

/*********************************************************************
**    S_FUNCTION     :  S_put_list(list,sbuf)
**       Stores an entry into a form list field.
**    PARAMETERS
**       INPUT  :
**          list     List to store the entry into.
**          sbuf     Text of entry.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_put_list(list,sbuf)
UD_LIST *list;
char *sbuf;
{
	if (list->num_item >= N_ATTRIB)
		return;
	list->item[list->num_item] = (char *)uu_malloc(81*sizeof(char));
	strcpy(list->item[list->num_item],sbuf);
	list->num_item++;
}

/*********************************************************************
**    S_FUNCTION     :  S_restore_colors(flag)
**       Resets the highlighted entities to their original colors.Also
**       deletes motion point lists when a reject op is caught or the
**       form is closed.
**    PARAMETERS
**       INPUT  :
**          flag  -  keep current allocated motion data
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_restore_colors(flag)
UU_LOGICAL flag;
{
	int j;
	UN_motseg *mfrm;
/*
.....Unhighlight previously selected entities
*/
	for (j=0;j<Sncolor;j++)
	{
		if (!Smotpick[j])
		{
			ncl_update_color(Se[j].key,Scolor[j]);
			uc_display(&Se[j]);
		}
	}
	if (Ssegid != 0) gdeleteseg(Ssegid);
	ud_delete_assist_segs();
/*
.....Delete allocated memory for motion data if flag is set to false
*/
	if (!flag) S_restore_motion();
	ud_setpick_type(UD_PICK_NORMAL);
	Sncolor = 0;
	Ssegid = 0;
}

/*********************************************************************
**    S_FUNCTION     :  S_restore_motion()
**       Resets the highlighted motion to their original colors. Also
**       deletes motion point lists.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_restore_motion()
{
	int j;
	UN_motseg *mfrm;
/*
.....Deallocate memory used for motion point lists and restore colors
*/
	if (Spoints[0].data != UU_NULL) uu_list_free(&Spoints[0]);
	if (Spoints[1].data != UU_NULL) uu_list_free(&Spoints[1]);
	for (j=0;j<2;j++)
	{
		if (j == 1 && Shilite_strt[0] == Shilite_strt[1] && 
			Shilite_end[0] == Shilite_end[1]) break;
		mfrm = Shilite_strt[j];
		while (mfrm != UU_NULL && mfrm != Shilite_end[j])
		{
			mfrm->attr = mfrm->hattr;
			mfrm = (UN_motseg *)uu_lsnext(mfrm);
		}
	}
	Shilite_strt[0] = Shilite_strt[1] = 0;
	Shilite_end[0] = Shilite_end[1] = 0;
	Smotpick[0] = Smotpick[1] = UU_FALSE;
	gredrawsegs();
}

/*********************************************************************
**    S_FUNCTION     :  S_2dcalc(pts,ibeg,npts,inc,bpt,nbpts,cyl)
**       Determines if an array of points or a portion of the array
**       contains a valid circle.
**    PARAMETERS
**       INPUT  :
**          pts      Array of points.
**          ibeg     Starting point in array for analyzation.
**          npts     Ending point in array for analyzation.
**          inc      Index to closest point within 'pts' to selection
**                   point (nrpt).
**          bpt      Array of boundary points for selected face.
**          nbpts    Number of points in 'bpt'.
**          nrpt     Near point which must be included in the circle
**                   check.
**       OUTPUT :
**          cir      Circle record.
**    RETURNS      :
**          UU_TRUE if a circle is present.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_2dcalc(pts,ibeg,npts,inc,bpt,nbpts,nrpt,cir)
UM_coord *pts,*bpt;
int ibeg,npts,inc,nbpts;
UM_coord nrpt;
struct UM_circle_rec *cir;
{
	int i,j,ist,ien,imid,stat;
	UU_LOGICAL status;
	UU_REAL dis,cyl[8];
	UM_real8 tol;
	UM_coord temp;
/*
.....Initialize routine
*/
	gettol(&tol);
	ist = ibeg;
	ien = npts - 1;
	status = UU_FALSE;
/*
.....Attempt to find circle
.....in list of points
*/
	for (;;)
	{
		for (i=ist;i<ien-1;i++)
		{
/*
........Make sure selected point is within cylinder
*/
			if (i > inc) break;
/*
........Calculate middle point
*/
			imid = i + (ien-i+1)/2;
			if (imid <= ist || imid >= ien) break;
/*
........Build the circle
*/
			stat = um_c3_test_3pt(pts[i],pts[imid],pts[ien]);
			if (stat != UU_SUCCESS) continue;
			stat = um_c3_arc3pt(pts[i],pts[imid],pts[ien],cir);
			if (stat != UU_SUCCESS || cir->radius > 10.e+8 ||
				cir->radius <= tol) continue;
/*
........Check selected point to cylinder
*/
			dis = um_dcccc(nrpt,cir->center);
			dis = fabs(dis-cir->radius);
			if (dis > tol) continue;
/*
........Check face boundary points to cylinder
*/
			um_vctovc(cir->center,cyl);
			um_vctovc(cir->nvec,&cyl[3]);
			cyl[6] = cir->radius;
			cyl[7] = cir->radius * fabs(cir->dang);
			for (j=0;j<nbpts;j++)
			{
				stat = um_on_cylinder(cyl,pts[i],pts[imid],pts[ien],bpt[j],
					tol);
				if (!stat) break;
			}
			if (j < nbpts) continue;
/*
........Check slice points to cylinder
*/
			for (j=i;j<ien;j++)
			{
				dis = um_dcccc(pts[j],cir->center);
				dis = fabs(dis-cir->radius);
				if (dis > tol) break;
				um_middlept(pts[j],pts[j+1],temp);
				dis = um_dcccc(temp,cir->center);
				dis = fabs(dis-cir->radius);
				if (dis > tol) break;
			}
/*
........Found our circle
*/
			if (j >= ien)
			{
				status = UU_TRUE;
				goto done;
			}
		}
		ien = ien - 1;
		if (ien < imid || ist+2 > ien) break;
		if (ien < inc) break;
	}
/*
....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    S_FUNCTION     :  S_radius(pick1,e,pt1)
**       Calculate of entity around pick point and display.
**    PARAMETERS
**       INPUT  :
**          pick1    - Picked location structure.
**          e        - Picked entity.
**          pt1      - Point on entity at picked location.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_radius(pick1,e,pt1)
UD_PLOCREC *pick1;
struct NCL_fixed_databag *e;
UM_coord pt1;
{
	int i, j, k, k1,status, isv, n1, unflg=1, ierr,n;
	UU_LOGICAL cfl[2];
	UM_int2 rn1,ietype;
	int cur_mclr,cur_lclr,cur_tclr,cur_type;
	UU_REAL cur_ch;
	Gwpoint3 cpts[100];
	Glntype cur_line,*style_ptr,new_line;
	char sbuf[82], typstr[16], labstr[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char cbuf[2][20];
	struct UM_circle_rec *c1, cc[2];
	struct UM_evsrfout *evsrf;
	struct UM_polyline_rec *poly;
	UM_coord pt2, spt1, ept1, npt1, npt2;
	UM_coord *pts1, *uvp;
	UM_vector vc1, vc2,vpnorm;
	UM_transf tfmat;
	UU_REAL rad[2], arc[2], d1, d1max, d1min, uvlim[2], huv[2], rd2;
	UM_real8 tol, u, v, dir;
	UU_LIST list1, uvlist;
	UU_LOGICAL found = UU_TRUE;
/*
.....Initialize routine
*/
	tol = UL_measurement_radtol;
	cfl[0] = cfl[1] = 0;
	rad[0] = rad[1] = arc[0] = arc[1] = 0.;
	typstr[0] = '\0';
	if (e->rel_num != UM_POLYLINE_REL && !Smotpick[0])
	{
		ncl_get_type(e->rel_num,&rn1);
		ncl_get_label(e,labstr);
	}
	else
	{
		if (e->rel_num == UM_POLYLINE_REL) rn1 = UM_POLYLINE_REL;
		else rn1 = NCLI_LINE;
		sprintf(labstr,e->label);
	}
	um_vpnorm(pick1->pndc.transform,vpnorm);

	switch (rn1)
	{
	case NCLI_CIRCLE:
		strcpy(typstr,"Circle - ");
		c1 = (struct UM_circle_rec *)e;
		cc[0] = *c1;
		cfl[0] = UU_TRUE;
/*
		rad[0] = c1->radius;
		arc[0] = c1->radius * fabs(c1->dang);
*/
		break;
	case UM_POLYLINE_REL:
	case NCLI_CURVE:
		if (rn1 != UM_POLYLINE_REL)
		{
			strcpy(typstr,"Curve - ");
			uu_list_init(&list1,sizeof(UM_coord),100,800);
			if (uc_retrieve_transf(e->key,tfmat) != UU_SUCCESS)
				found = UU_FALSE;
			if (found)
			{
				n1 = ncl_evolve_all_curves(e,tfmat,tol,&list1,UU_NULL,UU_FALSE);
				if (n1 < 3) found = UU_FALSE;
				if (found) pts1 = (UM_coord *)UU_LIST_ARRAY(&list1);
			}
		}
		else
		{
			strcpy(typstr,"Motion - ");
			poly = (struct UM_polyline_rec *)e;
			n1 = poly->no_pt;
			if (n1 < 3) found = UU_FALSE;
			if (found) pts1 = (UM_coord *)poly->pt;
		}
		if (found)
		{
			
			for (i=0;i<n1;i++)
			{
				d1 = um_dot(pts1[i],vpnorm);
				if (i == 0 || d1 > d1max) d1max = d1;
				if (i == 0 || d1 < d1min) d1min = d1;
			}
			if (fabs(d1max-d1min) > UM_FUZZ)
			{
				d1 = um_dot(pt1,vpnorm);
				um_vctmsc(vpnorm,d1max-d1,vc1);
				um_vcplvc(pt1,vc1,spt1);
				um_vctmsc(vpnorm,d1min-d1,vc1);
				um_vcplvc(pt1,vc1,ept1);
			}
			else
			{
				um_vctovc(pt1,spt1);
				um_vcplvc(pt1,vpnorm,ept1);
			}
			d1min = 1.e12;
			for (i=0;i<n1-1 && d1min>0.0;i++)
			{
				um_vcmnvc(pts1[i+1],pts1[i],vc2);
				um_unitvc(vc2,vc2);
				d1 = ncl_lnlndis(spt1,ept1,pts1[i],pts1[i+1],npt1,npt2);
				if (d1 < d1min)
				{
					isv = i;
					d1min = d1;
					um_vctovc(npt1,pt1);
					um_vctovc(npt2,pt2);
				}
			}
			if (S_2dcalc(pts1,0,n1,isv,pts1,0,pts1[isv],&cc[0]))
			{
				cfl[0] = UU_TRUE;
/*
				rad[0] = cyl[6];
				arc[0] = cyl[7];
*/
			}
			else
			{
				if (isv==0) isv = 1;
				i = isv-1;
				j = isv+1;
				if (j < n1-1)
				{
					status = um_c3_test_3pt(pts1[i],pts1[isv],pts1[j]);
					if (status == UU_SUCCESS)
					{
						status = um_c3_arc3pt(pts1[i],pts1[isv],pts1[j],&cc[0]);
						if (status == UU_SUCCESS && cc[0].radius < 10.e+8 &&
							cc[0].radius > tol)
						{
/*
							rad[0] = cc[0].radius;
							arc[0] = cc[0].radius * fabs(cir.dang);
*/
						}
					}
				}
			}
		}
		if (rn1 != UM_POLYLINE_REL) uu_list_free(&list1);
		break;
	case NCLI_SURF:
		strcpy(typstr,"Surface - ");
		uu_list_init(&list1,sizeof(UM_coord),100,800);
		uu_list_init(&uvlist,sizeof(UM_coord),100,800);
		found = UU_FALSE;
		u = .5;
		v = .5;
		dir = 0.;
		ierr = 0;
		d1min = 1.e12;
		if (tol < UM_DFUZZ) tol = UM_DFUZZ;
		for (j=0;j<100 && d1min >= UM_DFUZZ;j++)
		{
			sfpt2(&(e->key), pt1, &u, &v, &unflg, &dir, pt2, vc2, &ierr);
			if (ierr) break;
			um_ilnpln(pt1,vpnorm,pt2,vc2,&n1,spt1);
			if (n1==0)
			{
				um_nptpln(pt1,pt2,vpnorm,spt1);
			}
			d1 = um_dcccc(pt1,spt1);
			if (d1 < d1min)
			{
				d1min = d1;
				huv[0] = u;
				huv[1] = v;
				if (d1 < tol) found = UU_TRUE;
			}
			um_vctovc(spt1,pt1);
		}
		if (found)
		{
			uvlim[0] = 0.;
			uvlim[1] = 1;
			if (uc_retrieve_transf(e->key,tfmat) != UU_SUCCESS)
				found = UU_FALSE;
		}
		if (found)
		{
			evsrf = (struct UM_evsrfout *)uu_malloc (sizeof(struct UM_evsrfout));
			if (!evsrf) found = UU_FALSE;
			if (found)
			{
				if (uc_evsrf (UM_POINT,huv[0],huv[1],e,tfmat,evsrf) == UU_SUCCESS)
					um_vctovc(evsrf->sp,pt1);
				else
					found = UU_FALSE;
				uu_free(evsrf);
			}
		}
		for (k=0;k<2 && found;k++)
		{
			for (k1=0;k1<4;k1++)
			{
				n1 = ncl_evolve_crv_on_srf(e,tfmat,huv[1-k],uvlim,k+1,tol,&list1,
					UU_NULL,&uvlist);
				if (n1 > 2) break;
				tol = tol / 2.;
				UU_LIST_EMPTY(&list1);
				UU_LIST_EMPTY(&uvlist);
			}
			if (n1 > 2)
			{
				uvp = (UM_coord *)UU_LIST_ARRAY(&uvlist);
				pts1 = (UM_coord *)UU_LIST_ARRAY(&list1);
				for (isv=1;isv<n1-1 && huv[k]>uvp[isv][k];isv++) ;
				if (isv>n1-1) isv = n1-1;
				if (um_dcccc(pt1,pts1[isv]) > 4*tol)
				{
					if (um_dcccc(pt1, pts1[isv-1]) < 4*tol)
						isv--;
					else
					{
						uu_list_insert(&list1,isv,pt1);
						n1++;
					}
				}
				pts1 = (UM_coord *)UU_LIST_ARRAY(&list1);
				if (S_2dcalc(pts1,0,n1,isv,pts1,0,pts1[isv],&cc[k]))
				{
					cfl[k] = UU_TRUE;
/*					rad[k] = cyl[6];
					arc[k] = cyl[7];*/
				}
				else
				{
					if (isv==0) isv = 1;
					i = isv-1;
					j = isv+1;
					if (j < n1-1)
					{
						status = um_c3_test_3pt(pts1[i],pts1[isv],pts1[j]);
						if (status == UU_SUCCESS)
						{
							status = um_c3_arc3pt(pts1[i],pts1[isv],pts1[j],&cc[k]);
							if (status == UU_SUCCESS && cc[k].radius < 10.e+8 &&
								cc[k].radius > tol)
							{
								cfl[k] = UU_TRUE;
/*								rad[k] = cir.radius;
								arc[k] = cir.radius * fabs(cir.dang);*/
							}
						}
					}
				}
			}
			UU_LIST_EMPTY(&list1);
			UU_LIST_EMPTY(&uvlist);
		}
		uu_list_free(&list1);
		uu_list_free(&uvlist);
		break;
	case NCLI_LINE:
		found = UU_FALSE;
		if (Smotpick[0]) strcpy(typstr,"Segment - ");
		else strcpy(typstr,"Line - ");
		break;
	default:
		found = UU_FALSE;
		break;
	}
/*
.....List the circles
*/
	if (stat_list.item == UU_NULL)
		S_init_list(&stat_list,N_ATTRIB);
	strcpy(sbuf, typstr);
	strcat(sbuf, labstr);
	S_put_list(&stat_list,sbuf);
	if (rn1 == NCLI_SURF)
	{
		strcpy(cbuf[0],"Center-U");
		strcpy(cbuf[1],"Center-V");
	}
	else
		strcpy(cbuf[0],"Center");
	for (i=0;i<2;i++)
	{
		if (cfl[i])
		{
			ietype = 3;
			um_vctovc(cc[i].center,pt2);
			conpt(pt2,&ietype);
			um_vctmsc(pt2,Sconv,pt2);
			sprintf(sbuf,"%s = %lf,%lf,%lf %s",cbuf[i],pt2[0],pt2[1],pt2[2],Subuf);
			S_put_list(&stat_list,sbuf);

			ietype = 4;
			um_vctovc(cc[i].nvec,vc2);
			conpt(vc2,&ietype);
			um_unitvc(vc2,vc2);
			sprintf(sbuf,"Axis = %lf,%lf,%lf",vc2[0],vc2[1],vc2[2]);
			S_put_list(&stat_list,sbuf);

			rd2 = cc[i].radius;
			conlen(&rd2);
			rd2 = rd2 * Sconv;
			sprintf(sbuf,"Radius = %lf %s",rd2,Subuf);
			S_put_list(&stat_list,sbuf);

			arc[i] = rd2 * fabs(cc[i].dang);
			sprintf(sbuf,"Arclen = %lf %s",arc[i],Subuf);
			S_put_list(&stat_list,sbuf);
			S_put_list(&stat_list," ");
		}
	}
	ud_dispfrm_update_answer(Sfrm,STAT_FLD,(int *)&stat_list);
/*
.....Display the features
*/
	Ssegid = gnseg();
	gcreateseg(Ssegid);
	cur_mclr = gqmarkcolor();
	cur_lclr = gqlinecolor();
	cur_tclr = gqtextcolor();
	cur_ch   = gqcharheight();
	style_ptr= gqlinetype();
	cur_type = gqmarktype();
	zbytecp(cur_line, *style_ptr);
	gsmarkcolor (ur_get_dispattr_featclr());
	gslinecolor (ur_get_dispattr_featclr());
	gstextcolor (ur_get_dispattr_featclr());
	gsmarktype(3);
	new_line.typeno = UM_SOLID_LINE;
	new_line.npatn = 0;
	gslinetype(&new_line);

	for (k=0;k<2;k++)
	{
		if (cfl[k])
		{
			d1 = 4.;
/*			UM_cc_exttoint(cc[k].center,cc[k].center);
			UM_len_exttoint(cc[k].radius,cc[k].radius);*/
			ncl_cutter_nsides(&cc[k],d1,&n);
			if (n > 100) n = 100;
			ncl_cutter_circle(&cc[k],cpts,n);
			gpolyline3(n,cpts);
			gpolymarker3(1,cc[k].center);
			um_arrow(cc[k].center,cc[k].nvec);
		}
	}

	gsmarkcolor (cur_mclr);
	gslinecolor (cur_lclr);
	gstextcolor (cur_tclr);
	gscharheight(cur_ch);
	gsmarktype(cur_type);
	gslinetype  (&cur_line);
	gcloseseg();
}

/*********************************************************************
**    S_FUNCTION     :  S_distance(e,pt,vc)
**       Calculate distances and angles between 2 picked entities.
**    PARAMETERS
**       INPUT  :
**          e      - Fixed entity data.
**          pt     - Point on entity where picked.
**          vc     - Tangent/Normal vector at picked location.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_distance(e,pt,vc)
struct NCL_fixed_databag e[2];
UM_coord pt[2];
UM_vector vc[2];
{
	int k, color[2];
	UM_int2 rn[2];
	char sbuf[82], typstr[2][16];
	char labstr[2][NCL_MAX_LABEL_AND_SUBSCRIPT];
	UM_coord ptx,ptt[2];
	UM_vector vcx,vcc[2],uvc[2];
	UM_plane pln;
	UU_REAL dis1;
	UU_LOGICAL found[2],vcfl[2],ptfl[2];
	Gwpoint3 point, vector;
	struct UM_line_rec *ln1,*ln2;
/*
.....Initialize routine
*/
	ptfl[0] = ptfl[1] = UU_TRUE;
	vcfl[0] = vcfl[1] = UU_TRUE;
	found[0] = found[1] = UU_TRUE;
/*
........Store label and type
*/
	for (k=0;k<2;k++)
	{
		if (!Smotpick[k])
		{
			ncl_get_label(&e[k],labstr[k]);
			ncl_get_type(e[k].rel_num,&rn[k]);
		}
		else
		{
			strcpy(labstr[k],e[k].label);
			if (e[k].rel_num == UM_POLYLINE_REL)
				rn[k] = UM_POLYLINE_REL;
			else
				rn[k] = NCLI_LINE;
		}
	}
/*
.....Find closest points
*/
	if (Stype == PICK_CLOSEST)
		ncl_geom_closest(pt,e,rn,pt,vc, 1,UU_FALSE,Sgrid_flag,Sgrid);
/*
.....Calculate points and vectors on entities
*/
	for (k=0;k<2;k++)
	{
		switch (rn[k])
		{
/*
........Point
*/
		case NCLI_POINT:
			strcpy(typstr[k],"Point - ");
			vcfl[k] = UU_FALSE;
			break;
/*
...........Point-vector
*/
		case NCLI_POINTVEC:
			strcpy(typstr[k],"Pntvec - ");
			break;
/*
...........Vector
*/
		case NCLI_VECTOR:
			strcpy(typstr[k],"Vector - ");
			ptfl[k] = UU_FALSE;
			break;
/*
...........Line or Motion Segment
*/
		case NCLI_LINE:
			if (Smotpick[k]) strcpy(typstr[k],"Segment - ");
			else strcpy(typstr[k],"Line - ");
			break;
/*
...........Plane
*/
		case NCLI_PLANE:
			strcpy(typstr[k],"Plane - ");
			if (Stype == PICK_POINT) ptfl[k] = UU_FALSE;
			break;
/*
...........Circle
*/
		case NCLI_CIRCLE:
			strcpy(typstr[k],"Circle - ");
			break;
/*
...........Curve
*/
		case NCLI_CURVE:
			strcpy(typstr[k],"Curve - ");
			break;
/*
...........Surface
*/
		case NCLI_SURF:
			strcpy(typstr[k],"Surface - ");
			break;
/*
...........Solid
*/
		case NCLI_SOLID:
			strcpy(typstr[k],"Solid - ");
			break;
/*
...........Motion
*/
		case UM_POLYLINE_REL:
			strcpy(typstr[k],"Motion - ");
			break;
/*
...........Unsupported entity
*/
		default:
			strcpy(typstr[k],"Unknown ");
			found[k] = UU_FALSE;
			break;
		}
/*
.....Cannot measure between geometry types
*/
		if ((!ptfl[0] || !ptfl[1]) && !(vcfl[0] && !vcfl[1])) found[k] = UU_FALSE;
	}
/*
.....Cannot measure between geometry types
*/
/*	if ((!ptfl[0] || !ptfl[1]) && !(vcfl[0] && !vcfl[1])) found[k] = UU_FALSE;*/
/*
.....Initialize the list
*/
	if (stat_list.item == UU_NULL)
		S_init_list(&stat_list,N_ATTRIB);
/*
.....Adjust points for MODSYS
*/
	for (k = 0; k < 2; k++)
	{
		if (ptfl[k])
		{
			um_vctovc(pt[k],ptt[k]);
			conpt(ptt[k],&IPT);
		}
		if (vcfl[k])
		{
			um_vctovc(vc[k],vcc[k]);
			conpt(vcc[k],&IVE);
		}
	}
/*
.....Calculate the distances and angle
*/
	if (found[0] && found[1])
	{
		if (ptfl[0] && ptfl[1])
		{
			dis1 = um_dcccc(ptt[0],ptt[1]) * Sconv;
/*			UM_len_inttoext(dis1,dis1);*/
			sprintf(sbuf,"Point Distance = %lf %s",dis1,Subuf);
			S_put_list(&stat_list,sbuf);
			dis1 = fabs(ptt[0][0]-ptt[1][0]) * Sconv;
/*			UM_len_inttoext(dis1,dis1);*/
			sprintf(sbuf,"X Distance = %lf %s",dis1,Subuf);
			S_put_list(&stat_list,sbuf);
			dis1 = fabs(ptt[0][1]-ptt[1][1]) * Sconv;
/*			UM_len_inttoext(dis1,dis1);*/
			sprintf(sbuf,"Y Distance = %lf %s",dis1,Subuf);
			S_put_list(&stat_list,sbuf);
			dis1 = fabs(ptt[0][2]-ptt[1][2]) * Sconv;
/*			UM_len_inttoext(dis1,dis1);*/
			sprintf(sbuf,"Z Distance = %lf %s",dis1,Subuf);
			S_put_list(&stat_list,sbuf);
		}
		if (vcfl[0] && vcfl[1])
		{
			for (k = 0; k < 2; k++)	
				um_unitvc(vcc[k], uvc[k]);

			if (um_vcparall(uvc[0],uvc[1]))
			{
				um_vctovc(ptt[0],pln.p0);
				um_vctovc(uvc[0],pln.n);
				um_proj_pt_on_plane(1,ptt[1],&pln,ptx);
				dis1 = um_dcccc(ptt[1],ptx) * Sconv;
				sprintf(sbuf,"Planar Distance = %lf %s",dis1,Subuf);
				S_put_list(&stat_list,sbuf);
			}
			else
			{
				dis1 = um_angle(vcc[0],vcc[1]) * UM_RADIAN;
				sprintf(sbuf,"Vector Angle = %lf degrees",dis1);
				S_put_list(&stat_list,sbuf);
			}
		}
	}
/*
.....List entities
*/
	for (k=0;k<2;k++)
	{
		S_put_list(&stat_list," ");
		strcpy(sbuf, typstr[k]);
		strcat(sbuf, labstr[k]);
		S_put_list(&stat_list,sbuf);
/*
.....Projection point not found
*/
		if (!found[k])
		{
			S_put_list(&stat_list,"Could not find point on entity.");
		}
/*
.....List Point data
*/
		else
		{
			if (ptfl[k])
			{
				um_vctmsc(ptt[k],Sconv,ptx);
				sprintf(sbuf,"Point = %lf,%lf,%lf %s",ptx[0],ptx[1],ptx[2],
					Subuf);
				S_put_list(&stat_list,sbuf);
			}
/*
.....List Vector data
*/
			if (vcfl[k])
			{
				um_unitvc(vcc[k],vcx);
				sprintf(sbuf,"Vector = %lf,%lf,%lf",vcx[0],vcx[1],
					vcx[2]);
				S_put_list(&stat_list,sbuf);
			}
		}
	}
	ud_dispfrm_update_answer(Sfrm,STAT_FLD,(int *)&stat_list);
/*
..... Display assist vectors
*/
	for (k=0;k<2;k++)
	{
/*		UM_cc_exttoint(pt[k],pt1);*/
		um_vctovc(pt[k], &point);
		if (ptfl[k]) ud_assist_point1(point, Sclr[k]);
		if (vcfl[k])
		{
			um_unitvc(vc[k],&vector);
			ud_assist_vector1(point, vector, Sclr[k]);
		}
	}
}
/*********************************************************************
**    S_FUNCTION     :  S_set_motion()
**       Get the motion segment attributes and create a line segment
**       or polyline to use for distance calculations in place of the
**       motion.
**    PARAMETERS
**       INPUT  :
**          pick - Pick location data
**          ind  - Index of motion pick (0:first or 1:second)
**       OUTPUT :
**          eptr - Fixed data of entity picked.
**          pt   - Point on entity.
**          vec  - Tangent/Normal vector of entity at pick point.
**    RETURNS      : entity generation status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_motion(pick,eptr,pt,vec,mind)
UD_PLOCREC *pick;
struct NCL_fixed_databag *eptr;
UM_coord pt;
UM_vector vec;
int mind;
{
	UU_REAL spt[6],dval;
	UN_motseg *mfrm;
	UN_motseg_attr mattr;
	int i,status,sv_isn,nline,ktype,nc,indx,hatt;
	UM_coord pkpt,coord;
	UM_vector vpnorm;
	UM_real8 tol,uv[2],buf[8];
	struct UM_line_rec *ln;
	struct UM_polyline_rec *poly;
	char sbuf[256];
/*
.....Initialize routine
*/
	tol = UL_measurement_disttol;
	um_vpnorm(pick->pndc.transform,vpnorm);
	gndcw3(&pkpt[0],&pkpt[1],&pkpt[2],
		pick->pndc.cord[0],pick->pndc.cord[1],pick->pndc.cord[2]);
/*
.....Create line segment from motion segment
*/
	ln = (struct UM_line_rec *)eptr;
	ln->rel_num = 2;
	mfrm = (UN_motseg *)uu_lsprev(Smptr);
	if (mfrm == UU_NULL) mfrm = Smptr;
	um_vctovc(&mfrm->tend,spt); um_vctovc(&mfrm->taxis,&spt[3]);
	UM_cc_inttoext(spt,spt);
	um_vctovc(&spt,ln->spt);
	um_vctovc(&Smptr->tend,spt); um_vctovc(&Smptr->taxis,&spt[3]);
	UM_cc_inttoext(spt,spt);
	um_vctovc(&spt,ln->ept);
/*
.....Project picked point onto entity
*/
	status = ncl_pickptonent(ln,pkpt,vpnorm,tol,pt,uv,vec);
/*
.....Get motion command for display
.......The motion display attributes are modified so the motion is highlighted
*/
	indx = 0;
	ncl_motisn_get(Smptr->isn,&nline,&ktype,&dval,&sbuf,&nc,&indx);
	ul_strip_blanks(sbuf,&nc);
/*
.....Store line segment in databag if using a single motion segment
*/
	if (Smsgl == 1)
	{	

		sprintf(ln->label,"%d: %s",nline,sbuf);
		ln->label[63] = 0;
		ln->subscr = 0;
		Shilite_strt[mind] = Smptr;
		Shilite_end[mind] = (UN_motseg *)uu_lsnext(Smptr);
		if (Shilite_strt[0] != Shilite_strt[1])
		{
			ncl_motattr_get(&mattr,Smptr->attr);
			hatt = ncl_motattr_set(Shilite[mind],mattr.lnstyle,mattr.pen);
			Smptr->hattr = Smptr->attr;
			Smptr->attr = hatt;
		}
	}
/*
.....Create a polyline if using the entire motion
*/
	else
	{
/*
.....Get first motion segment and save picked motion isn so the search
.....for segments stops when the next motion is reached
*/
		mfrm = (UN_motseg *)uu_lsprev(Smptr2);
		if (mfrm == UU_NULL) mfrm = Smptr2;
		Shilite_strt[mind] = mfrm;
		sv_isn = Smptr2->isn;
		uu_list_init(&Spoints[mind],sizeof(UM_coord),100,100);
/*
.....Build list of points to make polyline
.......The motion display attributes are modified so the motion is highlighted.
*/ 		
		do
		{
			um_vctovc(&mfrm->tend,spt); um_vctovc(&mfrm->taxis,&spt[3]);
			UM_cc_inttoext(spt,spt);
			um_vctovc(spt,coord);
			uu_list_push(&Spoints[mind],&coord);
			if (Shilite_strt[0] != Shilite_strt[1])
			{
				ncl_motattr_get(&mattr,mfrm->attr);
				hatt = ncl_motattr_set(Shilite[mind],mattr.lnstyle,mattr.pen);
				mfrm->hattr = mfrm->attr;
				mfrm->attr = hatt;
			}
			mfrm = (UN_motseg *)uu_lsnext(mfrm);
			if (mfrm == UU_NULL) break;
		} while (mfrm->isn == sv_isn);
		Shilite_end[mind] = mfrm;
/*
.....Set up polyline data
*/
		poly = (struct UM_polyline_rec *)eptr;
		poly->pt = (UU_REAL *)UU_LIST_ARRAY(&Spoints[mind]);
		poly->rel_num = 42;
		poly->subscr = 0;
		poly->no_pt = Spoints[mind].cur_cnt;
		sprintf(poly->label,"%d: %s",nline,sbuf);
		poly->label[63] = 0;
	}
/*
.....Redisplay entities to update displayed motion color
*/
	gredrawsegs();
	return status;
}

