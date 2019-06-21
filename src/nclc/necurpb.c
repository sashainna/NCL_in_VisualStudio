/*********************************************************************
**    NAME         :  necurpb.c
**       CONTAINS:
**          ncl_plot_clrec
**          ncl_clfind_isn
**          ncl_playback_current
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       necurpb.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:27
*********************************************************************/
#include "usysdef.h"
#include "nclfc.h"
#include "nclstack.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"

#define FVIW 0
#define FREW 1
#define FBCK 2
#define FMOT 3
#define FFWD 4
#define FFST 5
#define FPLY 6

extern UN_motseg *mlist_ptr;

static void Sffwd();
static void Sset_field();

static int Scurrent;
static UU_LOGICAL Sclflag;
static UN_motseg *Smptr;
static UN_mot_stack_struc Smrange;
static char Strav[] = {1, 1,1, 1, 1,1, 1};

/*********************************************************************
**    S_FUNCTION     :  OnBack(fieldno, val, stat)
**       Routine to backspace the motion display.
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
static UD_FSTAT OnBack(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Backspace the motion display
*/
	if (Smptr != Smrange.mbegin && Smptr != UU_NULL)
	{
		ncl_step_motion(&Smptr,-1);
		UN_step_ptr = Smptr;
	}
/*
.....Enable correct fields
*/
	Sset_field(FFWD,UU_TRUE);
	Sset_field(FFST,UU_TRUE);
	Sset_field(FPLY,UU_TRUE);
	if (Smptr == Smrange.mbegin || Smptr == UU_NULL)
	{
		Sset_field(FBCK,UU_FALSE);
		if (Scurrent+1 == UN_mot_stack_num) Sset_field(FREW,UU_FALSE);
	}
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnFfwd(fieldno, val, stat)
**       Routine to fast forward to the next motion.
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
static UD_FSTAT OnFfwd(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Fast forward the motion display
*/
	Sffwd();
/*
.....Point to next motion
*/
	ud_update_answer(FMOT,(int *)&Scurrent);
/*
.....Enable correct fields
*/
	Sset_field(FREW,UU_TRUE);
	if (Sclflag)
	{
		Sset_field(FBCK,UU_FALSE);
		Sset_field(FFWD,UU_TRUE);
		Sset_field(FPLY,UU_TRUE);
	}
	else
	{
		Sset_field(FBCK,UU_TRUE);
		Sset_field(FFWD,UU_FALSE);
		Sset_field(FPLY,UU_FALSE);
		Sset_field(FFST,UU_FALSE);
	}
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  Sffwd()
**       Fast forwards to the next motion.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void Sffwd()
{
	int status;
	UN_motseg *kst;
/*
.....Fast forward the motion display
*/
	if (Sclflag)
	{
		status = ncl_plot_clrec(&Smrange,UU_TRUE,UU_FALSE,UU_FALSE);
		ncl_mot_stack_range_set(Scurrent,&Smrange);
		Smptr = Smrange.mend;
		if (Smptr == 0) goto done;
/*
........Display the cutter
*/
		kst = (UN_motseg *)uu_lsprev(Smrange.mend);
		ncl_display_motion(-1,kst,0,0,UU_TRUE,UU_FALSE,UU_NULL);
		ud_updatews(UG_SUPPRESS);
	}
	else
	{
		if (Smptr != UU_NULL) ncl_step_motion(&Smptr,2);
	}
/*
.....Adjust motion display pointers
*/
	if (Scurrent == 0)
	{
		Sclflag = UU_FALSE;
		Smptr = Smrange.mend;
	}
	else
	{
		Scurrent--;
		ncl_mot_stack_range(Scurrent,&Smrange);
		Smptr = Smrange.mbegin;
		Sclflag = UU_TRUE;
	}
/*
.....End of routine
*/
done:;
	UN_step_ptr = UU_NULL;
}

/*********************************************************************
**    S_FUNCTION     :  OnFwd(fieldno, val, stat)
**       Routine to advance the motion display.
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
static UD_FSTAT OnFwd(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
/*
.....Read from clfile & plot motion
*/
	if (Sclflag && (Smptr == mlist_ptr || Smptr == UU_NULL))
	{
		status = ncl_plot_clrec(&Smrange,UU_FALSE,UU_TRUE,UU_FALSE);
/*
........End of clfile range
*/
		if (status == 1)
		{
			Sclflag = UU_FALSE;
			Smptr = Smrange.mend;
			ncl_mot_stack_range_set(Scurrent,&Smrange);
		}
/*
........Save the current motion display pointer
*/
		else
		{
			Smptr = mlist_ptr;
		}
	}

/*
.....Forward the motion display
*/
	else if (Smptr != Smrange.mend)
	{
		ncl_step_motion(&Smptr,1);
		UN_step_ptr = Smptr;
	}
/*
.....Enable correct fields
*/
	Sset_field(FBCK,UU_TRUE);
	Sset_field(FREW,UU_TRUE);
	if (!Sclflag && Smptr == Smrange.mend)
	{
		Sset_field(FFWD,UU_FALSE);
		if (Scurrent == 0)
		{
			Sset_field(FPLY,UU_FALSE);
			Sset_field(FFST,UU_FALSE);
		}
	}
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnPlay(fieldno, val, stat)
**       Routine to playback the current motion.
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
static UD_FSTAT OnPlay(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
/*
.....At end of motion
.....Start with next one
*/
	if (!Sclflag && Smptr == Smrange.mend)
	{
		Sffwd();
		ud_update_answer(FMOT,(int *)&Scurrent);
	}
/*
.....Playback from the clfile
*/
	if (Sclflag)
	{
		status = ncl_plot_clrec(&Smrange,UU_TRUE,UU_TRUE,UU_FALSE);
		Sclflag = UU_FALSE;
		Smptr = Smrange.mend;
		ncl_mot_stack_range_set(Scurrent,&Smrange);
	}
/*
.....Playback the motion display
*/
	else
	{
		while (Smptr != Smrange.mend && Smptr != UU_NULL)
		{
			ncl_step_motion(&Smptr,1);
		}
	}
/*
.....Enable correct fields
*/
	Sset_field(FBCK,UU_TRUE);
	Sset_field(FREW,UU_TRUE);
	Sset_field(FFWD,UU_FALSE);
	if (Scurrent == 0)
	{
		Sset_field(FFST,UU_FALSE);
		Sset_field(FPLY,UU_FALSE);
	}
/*
.....End of routine
*/
done:;
	UN_step_ptr = UU_NULL;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnRewind(fieldno, val, stat)
**       Routine to rewind to the beginning of the currenr or previous
**       motion.
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
static UD_FSTAT OnRewind(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status,i;
	UN_mot_stack_struc rang;
	UN_motseg *kst;
/*
.....At the beginning of the motion
.....Rewind to previous motion
*/
	if (Smptr == Smrange.mbegin || Smrange.mbegin == UU_NULL)
	{
		if (Scurrent+1 < UN_mot_stack_num)
		{
			Scurrent++;
			ud_update_answer(FMOT,(int *)&Scurrent);
			status = ncl_mot_stack_range(Scurrent,&Smrange);
		}
	}
/*
.....Rewind to current motion
*/
	else
	{
		status = ncl_mot_stack_range(Scurrent,&rang);
		for (i=0;i<6;i++) Smrange.spt[i] = rang.spt[i];
	}
/*
.....Erase the motion
.....From the current location to the end
*/
/*	if (Smrange.mbegin != UU_NULL)*/
	{
		ncl_mot_stack_off();
		kst = Smrange.mbegin;
		if (kst == UU_NULL) kst = (UN_motseg *)-1;
		ncl_erase_motion(kst);
		ncl_mot_stack_on();
	}
/*
.....Display the cutter at the last position
*/
	status = ncl_mot_stack_range(Scurrent+1,&rang);
	if (status == UU_SUCCESS)
	{
		if (rang.mend != UU_NULL)
		{
			kst = (UN_motseg *)uu_lsprev(rang.mend);
			ncl_display_motion(-1,kst,0,0,UU_TRUE,UU_FALSE,UU_NULL);
		}
		else
		{
			status = ncl_plot_clrec(&Smrange,UU_FALSE,UU_TRUE,UU_TRUE);
		}
	}
	else
	{
		status = ncl_plot_clrec(&Smrange,UU_FALSE,UU_TRUE,UU_TRUE);
	}
/*
.....Flush the display
*/
	ud_updatews(UG_SUPPRESS);
/*
.....Reset the clfile range
*/
	Sclflag = UU_TRUE;
	Smrange.crec = Smrange.cbegin;
	Smrange.cpt = 0;
	Smrange.mbegin = UU_NULL;
	Smrange.mend = UU_NULL;
	Smptr = UU_NULL;
	UN_step_ptr = UU_NULL;
	ncl_mot_stack_range_set(Scurrent,&Smrange);
/*
.....Enable correct fields
*/
	Sset_field(FFWD,UU_TRUE);
	Sset_field(FFST,UU_TRUE);
	Sset_field(FPLY,UU_TRUE);
	Sset_field(FBCK,UU_FALSE);
	if (Scurrent+1 == UN_mot_stack_num) Sset_field(FREW,UU_FALSE);
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnView(fieldno, val, stat)
**       Routine to enter dynamic viewing within the Playback form.
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
static UD_FSTAT OnView(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Take down form
*/
	ud_form_invis();
/*
.....Enter dynamic viewing
*/
	uz_dyn_mouse();
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_form_vis();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  Sset_field(fieldno, state)
**       Sets the traversal flag of the specified field if it has changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          state    Traversal state (UU_TRUE/UU_FALSE).
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void Sset_field(fieldno,state)
int fieldno;
char state;
{
/*
.....Set the field's traverse state
*/
	if (Strav[fieldno] != state)
	{
		Strav[fieldno] = state;
		ud_set_traverse_mask(fieldno,state);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_plot_clrec(rang,playfl,movefl,initfl)
**       Plots a location from the clfile.
**    PARAMETERS
**       INPUT  :
**          rang    = Structure that holds the clfile range and
**                    beginning motion attributes for this range.
**          playfl  = UU_TRUE if motion should be played to the end.
**                    UU_FALSE if motion is being stepped through.
**          movefl  = UU_TRUE if cutter should be displayed.
**                    UU_FALSE if just catching up motion (Fast Forward).
**          initfl  = UU_TRUE if only from point with cutter should be plotted.
**                    UU_FALSE if actual clfile plotting is to be done.
**       OUTPUT :
**          rang    = Modified motion display pointers.
**    RETURNS      : 0 = Success, 1 = End of range encountered,
**                   2 = Error processing clfile.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_plot_clrec(rang,playfl,movefl,initfl)
UN_mot_stack_struc *rang;
UU_LOGICAL playfl,movefl,initfl;
{
	static UM_int2 mxcl=0,irapsv,irap;
	static UM_int4 iclw[6];
	static UM_real8 rclw[420];

	int i,j,iret,npt,np,ipt;
	UU_LOGICAL endloop;
	char sym[3][MAXSYMLEN+2];
	UU_REAL tpt[9],spt[6],cnv;
	UM_int2 itra,ifl,ival,kerr,i2v0=0,i2v1=1;
	UM_int4 idc[3],flags[10],trafl;
	UN_clstruc *irec;
/*
.....Initialize routine
*/
	irec = rang->crec;
	ipt = rang->cpt;
	npt = 6;
	if (rang->multax == 0) npt = 3;
	strcpy(sym[0],rang->symbol[0]);
	strcpy(sym[1],rang->symbol[1]);
	strcpy(sym[2],rang->symbol[2]);
	idc[0] = rang->cfl[0];
	idc[1] = rang->cfl[4];
	idc[2] = rang->cfl[5];
	flags[0] = rang->cfl[1];
	flags[1] = rang->cfl[2];
	if (!movefl) flags[1] = -1;
	flags[2] = rang->cfl[3];
	flags[5] = rang->cfl[6];
	flags[6] = rang->cfl[7];
	iret = 0;
	if (ipt == 0) irap = irapsv = rang->rapid;
	ifl = 264;
	getifl(&ifl,&ival);
	if (ival == 0) cnv = 1.;
	else cnv = 25.4;
/*
.....Adjust for TRACUT
*/
	gtrafl(&trafl);
	for (i=0;i<3;i++)
	{
		spt[i] = rang->spt[i] / cnv;
		spt[i+3] = rang->spt[i+3];
	}
	if (rang->trafl == 1 && trafl == 1)
	{
		itra = 3;
		conent(spt,rang->tracut,&itra);
		itra = 4;
		conent(&spt[3],rang->tracut,&itra);
	}
	mcswcs(&i2v0,spt);
	mcswcs(&i2v1,&spt[3]);
/*
.....Plot initial point only
*/
	if (initfl)
	{
		for (i=0;i<6;i++) tpt[i] = spt[i];
		iclw[2] = 5000;
	}
/*
.....Find previous ISN record
*/
	else
		ncl_clfind_isn(&UN_clfile,&irec);
/*
.....Read clfile record
*/
	endloop = UU_FALSE;
	do
	{
		if ((ipt == 0 || ipt >= mxcl) && !initfl)
		{
			if (irec == UU_NULL) break;
			clread(&UN_clfile,&irec,iclw,rclw,&kerr);
			if (kerr == 1) goto failed;
			ipt = 0;
			mxcl = iclw[4];
		}
		switch(iclw[2])
		{
/*
........ISN record
*/
		case 1000:
			motisn(rclw);
			break;
/*
........Post-processor command
*/
		case 2000:
			switch(iclw[3])
			{
/*
...........RAPID
*/
			case 5:
				irap = 1; irapsv = 1;
				break;
/*
...........All other commands
*/
			default:
				irap = 0; irapsv = 0;
			}
			rpset(&irap);
			mxcl = 0; ipt = 1;
			break;
/*
........Motion record
*/
		case 5000:
		case 5200:
			np = npt;
			if (iclw[2] == 5200) np = 21;
			if (!initfl)
			{
				for (j=0;j<npt;j++) tpt[j] = rclw[ipt+j];
				if (rang->trafl == 1 && trafl == 0)
				{
					itra = 3;
					conent(tpt,rang->invtra,&itra);
				}
				tpt[0] = tpt[0] / cnv;
				tpt[1] = tpt[1] / cnv;
				tpt[2] = tpt[2] / cnv;
				mcswcs(&i2v0,tpt);
/*
...........Get tool axis vector
*/
				if (npt == 3)
				{
					tpt[3] = spt[3];
					tpt[4] = spt[4];
					tpt[5] = spt[5];
				}
				else if (rang->trafl == 1 && trafl == 0)
				{
					itra = 4;
					conent(&tpt[3],rang->invtra,&itra);
					mcswcs(&i2v1,tpt[3]);
				}
/*
...........Get forward vector
*/
				if (np == 21)
				{
					tpt[6] = rclw[ipt+6];
					tpt[7] = rclw[ipt+7];
					tpt[8] = rclw[ipt+8];
					if (rang->trafl == 1 && trafl == 0)
					{
						itra = 4;
						conent(&tpt[6],rang->invtra,&itra);
					}
					mcswcs(&i2v1,&tpt[6]);
				}
				else
				{
					um_vcmnvc(tpt,spt,&tpt[6]);
					um_unitvc(&tpt[6],&tpt[6]);
				}
/*
...........From point
*/
				if (iclw[3] == 3)
					for (i=0;i<6;i++) spt[i] = tpt[i];
/*
...........Mark start of motion
*/
				if (ipt == 0) irapsv = irap;
				irap = 0;
				rpset(&irapsv);
				if (rang->mbegin == UU_NULL) rang->mbegin = mlist_ptr;
			}
/*
...........Plot motion
*/
			ncl_cutter_set(rang->cutr,idc,flags,sym[0],sym[1],sym[2],rang->symkey);
			pltmot(spt,tpt,&tpt[6]);
			ipt = ipt + np;
/*
...........Save current location
*/
			for (i=0;i<6;i++) spt[i] = tpt[i];
			break;
/*
........All other records
*/
		default:
			mxcl = 0; ipt = 1;
			break;
		}
/*
.....See if we are done with this loop
*/
		if (playfl)
			endloop = (irec == rang->cend || irec == UU_NULL) && ipt >= mxcl;
		else
			endloop = (iclw[2] == 5000 || iclw[2] == 5200) ||
				(irec == rang->cend || irec == UU_NULL);
	} while (!endloop);
	if (initfl) goto done;
/*
.....Adjust Start point for TRACUT
*/
	if (rang->trafl == 1 && trafl == 1)
	{
		itra = 3;
		conent(spt,rang->invtra,&itra);
		itra = 4;
		conent(&spt[3],rang->invtra,&itra);
	}
	for (i=0;i<3;i++)
	{
		rang->spt[i] = spt[i] * cnv;
		rang->spt[i+3] = spt[i+3];
	}
/*
.....Save clfile pointers
*/
	rang->crec = irec;
	rang->cpt = ipt;
	if ((rang->crec != rang->cend && rang->crec != UU_NULL) || ipt < mxcl)
		goto done;
/*
.....Mark end of motion
*/
endit:;
	rang->mend = mlist_ptr;
	iret = 1;
	goto done;
/*
.....Error reading clfile
*/
failed:;
	iret = 2;
/*
.....End of routine
*/
done:;
	return(iret);
}

/*********************************************************************
**    E_FUNCTION     : ncl_clfind_isn(clf,krec)
**       Finds the ISN record in the clfile prior to the clfile record
**       pointed to by 'irec'.
**    PARAMETERS
**       INPUT  :
**          clf     = Clfile to search.
**          krec    = Pointer to clfile record to search from.
**       OUTPUT : none
**    RETURNS      : 0 = Success, 2 = Error processing clfile.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_clfind_isn(clf,krec)
int *clf;
UN_clstruc *krec;
{
	UM_int2 kerr;
	UM_int4 iclw[6];
	int iret;
	UM_real8 rclw[420];
	UN_clstruc *irec;
/*
.....Initialize routine
*/
	irec = krec;
	iret = 0;
/*
.....Find previous ISN record
*/
	do
	{
		if (irec == UU_NULL) break;
		clprev(clf,irec,iclw,rclw,&kerr);
		if (kerr == 1) goto failed;
/*
........ISN record
*/
		if (iclw[2] == 1000)
		{
			motisn(rclw);
			break;
		}
	} while (irec != UU_NULL && iclw[2] != 1000);
	iret = 0;
	goto done;
/*
.....Error reading clfile
*/
failed:;
	iret = 2;
/*
.....End of routine
*/
done:;
	return(iret);
}

/*********************************************************************
**    E_FUNCTION     : ncl_playback_current()
**       Controlling routine for Playing Back the current motion.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_playback_current()
{
	int i,j,status,iend,mary[3];
	UU_LOGICAL markval=UU_FALSE;
	UN_motseg *mlist[3];
/*
.....Set up form fields
*/
	static UD_METHOD methods[] = {OnView,OnRewind,OnBack,UU_NULL,OnFwd,
		OnFfwd,OnPlay};
	static char called[] = {6, 6,6, 6, 6,6, 6};
	static char display[] = {1, 1,1, 1, 1,1, 1};
	static int *ans[] = {UU_NULL,UU_NULL,UU_NULL,(int *)&Scurrent,UU_NULL,
		UU_NULL,UU_NULL};
/*
.....Make sure a stack is active
*/
	if (UN_mot_stack_num == 0)
	{
		ud_wrerr("There are currently no motions on the stack.");
		return;
	}
/*
.....Initialize routine
*/
	status = ncl_mot_stack_range(0,&Smrange);
	if (status == UU_FAILURE) goto done;
	Smptr = Smrange.mend;
	Smrange.crec = Smrange.cend;
	Sclflag = UU_FALSE;
/*
.....Save the previous motion display pointers
*/
	ncl_get_mlist_ptrs(mlist);
	mary[0] = mary[1] = mary[2] = -1;
	for (i=0;i<UN_mot_stack_num;i++)
	{
		for (j=0;j<3;j++)
			if (UN_mot_stack[i].mbegin == mlist[j]) mary[j] = i;
	}
/*
.....Initialize form
*/
	Scurrent = 0;
	Strav[FREW] = 1;
	Strav[FBCK] = 1;
	Strav[FFWD] = 0;
	Strav[FFST] = 0;
	Strav[FPLY] = 0;
/*
.....Motion is erased
.....Work from the clfile
*/
	if (Smptr == UU_NULL)
	{
		Sclflag = UU_TRUE;
		Smrange.crec = Smrange.cbegin;
		Smrange.cpt = 0;
		Smrange.mbegin = UU_NULL;
		Smrange.mend = UU_NULL;
		Smptr = UU_NULL;
		UN_step_ptr = UU_NULL;
		ncl_mot_stack_range_set(Scurrent,&Smrange);
		Sffwd();
	}
/*
.....Print the stack
*/
	ncl_mot_stack_print(UN_mot_stack_ptr-1,UN_mot_stack_num,"Playback");
/*
.....Command reject
*/
	UD_MARK(markval,UU_FALSE);
	if (markval != 0)
	{
		goto done;
	}
/*
.....Get the Form input
*/
	status = ud_form1("playcur.frm", ans, ans, methods, called,
		display, Strav);
	if (status != UU_SUCCESS) goto done;
/*
.....Redisplay all motion
*/
done:;
	iend = Scurrent;
	for (i=-1;i<iend;i++) Sffwd();
/*
.....Restore the previous motion display pointers
*/
	for (i=0;i<3;i++)
		if (mary[i] != -1) mlist[i] = UN_mot_stack[mary[i]].mbegin;
	ncl_set_mlist_ptrs(mlist);
/*
....End of routine
*/
	UD_UNMARK(markval);
	return;
}
