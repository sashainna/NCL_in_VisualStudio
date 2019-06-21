/*********************************************************************
**    NAME         :  lipvmotstk.c
**       CONTAINS:
**				ul_ipv_mot_stack_init
**				ul_ipv_mot_stack_delete
**				ul_ipv_mot_stack_del_sess
**				ul_ipv_mot_stack_reset
**				ul_ipv_mot_stack_save
**				ul_ipv_mot_stack_restore
**				ul_ipv_mot_stack_push
**				ul_ipv_mot_stack_cutter
**				ul_ipv_mot_stack_step
**
**    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvmotstk.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       07/28/15 , 11:12:44
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "lcom.h"
#include "nclmplay.h"
#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"
#define IPVSTACK
#include "lipvstack.h"
#undef IPVSTACK
#include "nclfc.h"
#include "xenv1.h"
#include "xfsys1.h"

extern int moving_part;

static UU_LOGICAL Sinit=UU_FALSE;
static int Sncutr=-1,Slast_cutter[LW_MAX_SPINDLE],Scutter_stack_num,Sstack_num;
static int Spptr=0;
static LW_mot_stack_struc *Sfirst_ptr=UU_NULL,*Slast_ptr=UU_NULL;
static LW_mot_stack_struc *Sstack_ptr=UU_NULL;
static UU_LIST Scutter_list;

static UU_LOGICAL Ssave_init=UU_FALSE;
static int Ssave_ncutr,Ssave_last_cutter[LW_MAX_SPINDLE],Ssave_cutter_stack_num;
static int Ssave_stack_num;
static int Ssave_pptr;
static LW_mot_stack_struc *Ssave_first_ptr,*Ssave_last_ptr;
static LW_mot_stack_struc *Ssave_stack_ptr;
static UU_LIST Ssave_cutter_list;

static LtSavedMWEnvironment Ssession_start=0,Ssession_end=0;

void ul_ipv_mot_stack_delete();
void ul_ipv_mot_stack_del_sess();
UU_LOGICAL ul_ipv_mot_stack_step();
void S_store_cut_prims();
void S_delete_cut_prims();
void S_calc_cutter();

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_mot_stack_init()
**       Initializes the NCLIPV motion stack.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mot_stack_init()
{
	LtData stuff;
/*
.....Free up the previous stack
*/
	ul_ipv_mot_stack_delete(UU_FALSE);
/*
.....Enable intersect sweep solids to be created
*/
	if (LW_mot_stack_active)
	{
		LiDataSetBoolean(&stuff,TRUE);
		LiSessionSetProperty(LW_session[LW_mach_mode],
			LI_SESS_PROP_VI_CREATE_ISCT_SOL,&stuff);
/*
.....Initialize the motion display stack
*/
		Sfirst_ptr = (LW_mot_stack_struc *)uu_lsnew();
		Sstack_ptr = Sfirst_ptr;
		Slast_ptr = Sfirst_ptr;
		uu_list_init(&Scutter_list,sizeof(LW_cutter_stack_struc),LW_ntool,5);
		Sinit = UU_TRUE;
/*
.....Disable multi-threading
*/
		LiDataSetBoolean(&stuff,FALSE);
		LiSessionSetProperty(LW_session[0],LI_SESS_PROP_VI_STORE_CUTS,&stuff);
	}
/*
.....Disable Motion display stack
*/
	else
	{
		LiDataSetBoolean(&stuff,FALSE);
		LiSessionSetProperty(LW_session[LW_mach_mode],
			LI_SESS_PROP_VI_CREATE_ISCT_SOL,&stuff);
/*
........Initialize multi-threading
*/
		LiDataSetBoolean(&stuff,TRUE);
		LiSessionSetProperty(LW_session[0],LI_SESS_PROP_VI_STORE_CUTS,&stuff);
	}
/*
.....Set the stack pointers
*/
	Sstack_num = 0;
	Scutter_stack_num = 0;
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_mot_stack_delete(reset)
**       Deletes the NCLIPV motion stack.
**    PARAMETERS
**       INPUT  :
**          reset   = UU_TRUE means the session is being reset, so the
**                    the tool should not be stepped to the end.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mot_stack_delete(reset)
UU_LOGICAL reset;
{
/*
.....Been here before
.....Free up the previous stack
*/
	if (Sinit)
	{
/*
........Position tool at end of stack
*/
		if (!reset) ul_ipv_mot_stack_step(2,"");
/*
........Delete all stored primitives
*/
		S_delete_cut_prims(UU_NULL,0);
/*
.....Delete the stacks
*/
		uu_lsdel(Sfirst_ptr);
		uu_list_free(&Scutter_list);
		ul_ipv_mot_stack_del_sess();
		Sfirst_ptr = UU_NULL;
		Sstack_ptr = UU_NULL;
		Slast_ptr = UU_NULL;
		Sinit = UU_FALSE;
	}
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_mot_stack_del_sess()
**       Deletes the NCLIPV motion stack saved start and end sessions.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mot_stack_del_sess()
{
	if (Ssession_start != 0)
	{
		um_delv_axis_ipv();
		LiMWEnvironmentDestroy(Ssession_start);
	}
	if (Ssession_end != 0)
	{
		um_delv_axis_ipv();
		LiMWEnvironmentDestroy(Ssession_end);
	}
	Ssession_start = 0;
	Ssession_end = 0;
	Spptr = 0;
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_mot_stack_reset()
**       Re-Initializes the NCLIPV motion stack.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mot_stack_reset()
{
/*
.....Make sure the user wants to reset the Undo Stack
*/
	if (ud_yesno(0,"Are you sure you want to reset the NCLIPV Undo Stack?",
		"Reset NCLIPV Undo Stack"))
			ul_ipv_mot_stack_init();
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_mot_stack_save()
**       Saves the active motion stack and re-initializes it for
**       Preview type verification.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mot_stack_save()
{
	int i;
/*
.....Save the active stack
*/
	if (LW_mot_stack_active)
	{
		Ssave_ncutr = Sncutr;
		Ssave_cutter_stack_num = Scutter_stack_num;
		Ssave_stack_num = Sstack_num;
		Ssave_pptr = Spptr;
		Ssave_first_ptr = Sfirst_ptr;
		Ssave_last_ptr = Slast_ptr;
		Ssave_stack_ptr = Sstack_ptr;
		Ssave_cutter_list = Scutter_list;
		for (i=0;i<LW_MAX_SPINDLE;i++) Ssave_last_cutter[i] = Slast_cutter[i];
/*
.....Delete any saved sessions
*/
		ul_ipv_mot_stack_del_sess();
/*
.....Reset the motion stack
*/
		ul_ipv_mot_stack_init();
/*
.....Mark the stack as being saved
*/
		Ssave_init = UU_TRUE;
	}
}
		
/*********************************************************************
**    E_FUNCTION     :  ul_ipv_mot_stack_restore()
**       Restores the previously saved motion stack.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mot_stack_restore()
{
	int i;
	if (Ssave_init)
	{
/*
.....Reset the temporary motion stack
*/
		ul_ipv_mot_stack_init();
/*
.....Restore the saved motion stack
*/
		Sncutr = Ssave_ncutr;
		Scutter_stack_num = Ssave_cutter_stack_num;
		Sstack_num = Ssave_stack_num;
		Spptr = Ssave_pptr;
		Sfirst_ptr = Ssave_first_ptr;
		Slast_ptr = Ssave_last_ptr;
		Sstack_ptr = Ssave_stack_ptr;
		Scutter_list = Ssave_cutter_list;
		for (i=0;i<LW_MAX_SPINDLE;i++) Slast_cutter[i] = Ssave_last_cutter[i];
/*
.....Mark the stack as not being saved
*/
		Ssave_init = UU_FALSE;
	}
}
		
/*********************************************************************
**    E_FUNCTION     :  ul_ipv_mot_stack_push(fdata,rdata,nval,cutn,
**                         colors,ctype)
**       Saves the swept volume created by the tool in the current cut
**       onto the motion stack.
**    PARAMETERS
**       INPUT  :
**          fdata  = Previous tool/axes position prior to this cut.
**          rdata  = Parameters required to perform cut.
**          nval   = Number of values provided in 'rdat'.
**          cutn   = Cut number.
**          colors = Cut color attributes.
**          ctype  = Type of cut performed for this motion.
**
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**          In order to remember the cut colors for each spindle, the size
**          of the 'colors' parameter in the motion stack structure and
**          modify the call to this routine to pass in all colors.
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mot_stack_push(fdata,rdata,nval,cutn,colors,ctype)
LtDouble fdata[],rdata[];
int nval,cutn,colors[];
LW_mot_stack_ctype ctype;
{
	int i,nc;
	LW_mot_stack_struc mot_stack,*sptr;
/*
.....If list has not been initialized
.....just return
*/
	if (!Sinit) return;
/*
.....Process stack to its end
*/
	ul_ipv_mot_stack_step(2,"");
/*
.....Delete ending session
*/
	if (Ssession_end != 0)
	{
		LiMWEnvironmentDestroy(Ssession_end);
		Ssession_end = 0;
	}
	Spptr = 0;
/*
.....If first time here and
.....move is a positioning move
.....just ignore it
*/
	if (Sstack_ptr == Sfirst_ptr && ctype == LW_CUT_POSITION) return;
/*
.....If first time here
.....Save session in its initial state
*/
	if (Sstack_ptr == Sfirst_ptr)
		ul_ipv_save_sess(&Ssession_start,LW_tool_zhgt);
/*
.....Stack is full
.....Delete first entry
.....Saved session must be deleted also
.....since it no longer points to the start of the stack
*/
	if (LW_mot_stack_size != 0 && Sstack_num >= LW_mot_stack_size)
	{
		do
		{
			sptr = (LW_mot_stack_struc *)uu_lsnext(Sfirst_ptr);
			S_delete_cut_prims(sptr,Spptr);
			uu_lsdele(sptr);
			if (Ssession_start != 0)
			{
				LiMWEnvironmentDestroy(Ssession_start);
				Ssession_start = 0;
			}
			Sstack_num--;
		} while (Sstack_num >= LW_mot_stack_size);
	}
/*
.....Initialize motion stack structure
*/
	mot_stack.ctype = ctype;
	mot_stack.cutn = cutn;
	for (i=0;i<LW_spindle_num;i++)
		mot_stack.cutter[i] = Scutter_stack_num - (i+1);
	mot_stack.nspin = LW_spindle_num;
	mot_stack.moving_part = moving_part;
	mot_stack.mach_type = LW_mach_type;
	nc = 6;
	if (LW_mach_simul)
	{
		nc = 10;
		if (LW_mach_type == LW_STRINGER) nc = 18;
	}
	for (i=0;i<nc;i++) mot_stack.fdata[i] = fdata[i];
	for (i=0;i<4;i++) mot_stack.colors[i] = colors[i];
	mot_stack.nval = nval;
	for (i=0;i<nval;i++) mot_stack.rdata[i] = rdata[i];
/*
.....Store the cut volume for each stock and fixture
*/
	mot_stack.nprim[Spptr] = 0;
	mot_stack.nprim[1] = 0;
	if (ctype != LW_CUT_CIRCLE) S_store_cut_prims(&mot_stack);
/*
.....Push this motion onto the stack
*/
	Sstack_ptr = (LW_mot_stack_struc *)uu_lsinsrt((char *)Sstack_ptr,
		sizeof(LW_mot_stack_struc));
	*Sstack_ptr = mot_stack;
	Slast_ptr = Sstack_ptr;
	for (i=0;i<mot_stack.nspin;i++) Slast_cutter[i] = mot_stack.cutter[i];
	Sstack_num++;
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_mot_stack_cutter(cutr,mdata,mattr,ofs)
**       Saves the current cutter definition onto the motion stack.
**    PARAMETERS
**       INPUT  :
**          cutr   = Current cutter definition.
**          mdata  = Motion parameters for current move.
**          mattr  = Motion attributes for current move.
**          ofs    = Z-level offset (cutter height).
**          ispin  = Spindle number cutter is loaded into.
**
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mot_stack_cutter(cutr,mdata,mattr,ofs,ispin)
UU_REAL cutr[],ofs;
UN_mot_data *mdata;
UN_mot_attr *mattr;
int ispin;
{
	int i;
	LW_cutter_stack_struc cut_stack;
/*
.....If list has not been initialized
.....just return
*/
	if (!Sinit) return;
/*
.....Initialize cutter stack structure
*/
	cut_stack.tool = LW_act_tool[ispin];
	ncl_cutter_get_ptrs(cut_stack.cutseg);
	cut_stack.toler = LW_toler;
	cut_stack.maxang = LW_maxang;
	cut_stack.fr_mode = mdata->fr_mode;
	cut_stack.fr_val = mdata->fr_val;
	cut_stack.sp_val = mattr->sp_val;
	cut_stack.ofs = ofs;
	cut_stack.spindle = ispin;
	for (i=0;i<3;i++) cut_stack.translucency[i] = LW_translucency[i];
	for (i=0;i<9;i++) cut_stack.cutr[i] = cutr[i];
/*
.....Push this cutter onto the stack
*/
	uu_list_push(&Scutter_list,&cut_stack);
	Scutter_stack_num++;
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_mot_stack_step(dir,nsteps)
**       Steps back and forth in the NCLIPV motion stack, adding and
**       removing cut solids as it does.
**    PARAMETERS
**       INPUT  :
**          dir    = -1 = step backwards.  -2 = step to beginning.
**                   -3 = step to cut number.
**                    1 = step forwards.    2 = step to end.
**          nsteps = Number of steps to make when dir = -1 or 1.  This
**                   is character string that can be blank or contain
**                   an integer number.
**       OUTPUT :
**          none
**    RETURNS      : UU_TRUE if there was a step to be taken on the stack.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ul_ipv_mot_stack_step(dir,nsteps)
int dir;
char *nsteps;
{
	int i,status,ipt,nstep,inc,nc;
	UU_LOGICAL done,idid,fromt;
	char buf[80];
	UM_coord tool_pos;
	UM_vector tool_vec;
	LW_mot_stack_struc *sptr,*eptr,*S_find_cutn();
	LtNat32 cutn;
	LtData stuff;
/*
.....Initialize routine
*/
	if (!Sinit) return(UU_FALSE);
	idid = UU_FALSE;
/*
.....Reset any clash colors
*/
	ul_ipv_reset_clash_colors();
/*
.....Determine how many steps to make
*/
	strcpy(buf,nsteps);
	nc = strlen(buf);
	ul_strip_blanks(buf,&nc);
	nstep = 1;
	if (nc != 0)
	{
		status = ul_to_number(buf,&nstep);
		if (status != UU_SUCCESS || nstep < 1) nstep = 1;
	}
/*
.....If first time here
.....Save session in its current state
.....prior to stepping through motion
*/
	if (Ssession_end == 0 && dir < 0)
	{
		um_delv_axis_ipv();
		ul_ipv_save_sess(&Ssession_end,LW_tool_zhgt);
		Spptr = 1;
	}
/*
.....Step to beginning
*/
	if (dir == -2 && Ssession_start != 0)
	{
		if (Sstack_ptr != Sfirst_ptr)
		{
			ul_ipv_deselect_tool();
			um_delv_axis_ipv();
			LiMWEnvironmentRestore(Ssession_start);
			Sstack_ptr = (LW_mot_stack_struc *)uu_lsnext(Sfirst_ptr);
			S_calc_cutter(Sstack_ptr->cutter,Sstack_ptr->nspin,Sstack_ptr->fdata,
				&Sstack_ptr->fdata[3]);
			if (LW_mach_simul)
				ul_ipv_move_assemblies(Sstack_ptr->rdata,UU_FALSE,UU_TRUE);
			else
			{
				LiViToolSetPosition(LW_tool[0],Sstack_ptr->fdata);
				if (Sstack_ptr->nval == 6)
					LiViToolSetAxis(LW_tool[0],&Sstack_ptr->fdata[3]);
				um_vctovc(Sstack_ptr->fdata,tool_pos);
				um_vctovc(&Sstack_ptr->fdata[3],tool_vec);
			}
			idid = UU_TRUE;
		}
	}
/*
.....Step backwards
*/
	else if (dir < 0)
	{
/*
........Step to cut number
*/
		if (dir == -3)
		{
			do
			{
				status = ul_ipv_pick_cutn("Select face to step back to",&cutn);
				if (status == -2) goto done;
				else if (status == -1)
				{
					ud_wrerr(
						"The selected face does not have an associated cut number");
				}
				else
				{
					eptr = S_find_cutn(cutn);
					if (eptr == UU_NULL)
					{
						status = -1;
						ud_wrerr(
							"Could not find the selected face in the Undo Stack");
					}
				}
			} while (status < 0);
		}
		else
			eptr = Sfirst_ptr;
		done = UU_FALSE;
		sptr = UU_NULL;
		inc = 0;
		do
		{
			if (Sstack_ptr == eptr) break;
			inc++;
			if (Sstack_ptr->ctype == LW_CUT_CIRCLE)
			{
				Sstack_ptr = (LW_mot_stack_struc *)uu_lsprev(Sstack_ptr);
				if (Sstack_ptr == UU_NULL) Sstack_ptr = Sfirst_ptr;
				continue;
			}
			idid = UU_TRUE;
/*
........Set the correct machine type
*/
			if (Sstack_ptr->mach_type != LW_mach_type)
				ul_ipv_set_mach_type(Sstack_ptr->mach_type);
/*
........Add the stock back
*/
			ipt = 0;
			if (Sstack_ptr->nprim[1] != 0) ipt = Spptr;
			for (i=0;i<Sstack_ptr->nprim[ipt];i=i+2)
			{
				status = LiViSolidUnion(Sstack_ptr->prim[ipt][i],
					Sstack_ptr->prim[ipt][i+1]);
			}
			for (i=0;i<Sstack_ptr->nspin;i++)
			{
				if (Sstack_ptr->cutter[i] != Slast_cutter[i])
				{
					S_calc_cutter(Sstack_ptr->cutter,Sstack_ptr->nspin,
						Sstack_ptr->fdata,&Sstack_ptr->fdata[3]);
						break;
				}
			}
/*
.........Set the cut colors
*/
			ul_ipv_set_tool_colors(Sstack_ptr->colors[0],Sstack_ptr->colors[1],
				Sstack_ptr->colors[2],Sstack_ptr->colors[3],0);
/*
........Save the last referenced stack entry
*/
			sptr = Sstack_ptr;
			Sstack_ptr = (LW_mot_stack_struc *)uu_lsprev(Sstack_ptr);
			if (Sstack_ptr == UU_NULL) Sstack_ptr = Sfirst_ptr;
			if (dir == -1 && inc >= nstep) done = UU_TRUE;
		} while (!done);
/*
........Set the machine axes position
*/
		if (sptr != UU_NULL)
		{
			if (LW_mach_simul)
				ul_ipv_move_assemblies(sptr->fdata,UU_FALSE,UU_TRUE);
/*
........Set the tool position
*/
			else
			{
				LiViToolSetPosition(LW_tool[0],sptr->fdata);
				if (sptr->ctype != LW_CUT_LATHE &&
					sptr->ctype != LW_CUT_THREAD &&
					sptr->ctype != LW_CUT_LATHE_ARC)
						LiViToolSetAxis(LW_tool[0],&sptr->fdata[3]);
				um_vctovc(sptr->fdata,tool_pos);
				um_vctovc(&sptr->fdata[3],tool_vec);
/*
........Set the blade angle
*/
				sptr = (LW_mot_stack_struc *)uu_lsprev(sptr);
				if (sptr == UU_NULL) sptr = Sfirst_ptr;
				if (sptr != Sfirst_ptr && sptr->ctype == LW_CUT_BLADE)
					LiViSolidSetAxes(LW_tool[0],&sptr->rdata[3],&sptr->rdata[6]);
			}
		}
	}
/*
.....Step to end
*/
	else if (dir == 2 && Ssession_end != 0)
	{
		if (Sstack_ptr != Slast_ptr)
		{
			ul_ipv_deselect_tool();
			if (Ssession_end != 0)
			{
				um_delv_axis_ipv();
				LiMWEnvironmentRestore(Ssession_end);
				S_delete_cut_prims(UU_NULL,1);
			}
			Sstack_ptr = Slast_ptr;
			S_calc_cutter(Sstack_ptr->cutter,Sstack_ptr->nspin,Sstack_ptr->rdata,
				&Sstack_ptr->rdata[3]);
			if (LW_mach_simul)
				ul_ipv_move_assemblies(Sstack_ptr->rdata,UU_FALSE,UU_TRUE);
			else
			{
				LiViToolSetPosition(LW_tool[0],Sstack_ptr->rdata);
				if (Sstack_ptr->nval == 6) LiViToolSetAxis(LW_tool[0],
					&Sstack_ptr->rdata[3]);
				um_vctovc(&Sstack_ptr->rdata[0],tool_pos);
				um_vctovc(&Sstack_ptr->rdata[3],tool_vec);
			}
			idid = UU_TRUE;
		}
	}
/*
.....Step forwards
*/
	else
	{
		done = UU_FALSE;
		inc = 0;
		do
		{
			if (Sstack_ptr == Slast_ptr) break;
			inc++;
			Sstack_ptr = (LW_mot_stack_struc *)uu_lsnext(Sstack_ptr);
			idid = UU_TRUE;
/*
........Position to beginning of circular record
*/
			if (Sstack_ptr->ctype == LW_CUT_CIREND)
			{
				do
				{
					Sstack_ptr = (LW_mot_stack_struc *)uu_lsprev(Sstack_ptr);
					if (Sstack_ptr == UU_NULL) Sstack_ptr = Sfirst_ptr;
				} while (Sstack_ptr != Sfirst_ptr &&
					Sstack_ptr->ctype == LW_CUT_CIRCLE);
				Sstack_ptr = (LW_mot_stack_struc *)uu_lsnext(Sstack_ptr);
			}
/*
........Define new cutter
*/
			for (i=0;i<Sstack_ptr->nspin;i++)
			{
				if (Sstack_ptr->cutter[i] != Slast_cutter[i])
				{
					S_calc_cutter(Sstack_ptr->cutter,Sstack_ptr->nspin,
						Sstack_ptr->fdata,&Sstack_ptr->fdata[3]);
						break;
				}
			}
/*
.........Restore the cut number
*/
			LiDataSetNat32(&stuff,Sstack_ptr->cutn);
			LiSessionSetProperty(LW_session[LW_mach_mode],
				LI_SESS_PROP_MW_CUT_NUMBER,&stuff);
/*
.........Set the cut colors
*/
			ul_ipv_set_tool_colors(Sstack_ptr->colors[0],Sstack_ptr->colors[1],
				Sstack_ptr->colors[2],Sstack_ptr->colors[3],0);
/*
........Set the correct machine type
*/
				if (Sstack_ptr->mach_type != LW_mach_type)
					ul_ipv_set_mach_type(Sstack_ptr->mach_type);
/*
.........Machine simulation
*/
			if (LW_mach_simul)
			{
/*
...........Circular interpolation
*/
				if (Sstack_ptr->ctype == LW_CUT_CIRCLE)
				{
					do
					{
						ul_ipv_push_axis(Sstack_ptr->rdata);
						if (Sstack_ptr == Slast_ptr) break;
						Sstack_ptr = (LW_mot_stack_struc *)uu_lsnext(Sstack_ptr);
					} while (Sstack_ptr->ctype == LW_CUT_CIRCLE);
				}
/*
...........Threading
*/
				else if (Sstack_ptr->ctype == LW_CUT_THREAD)
				{
					LiViLatheSetOrientation(LW_lathe,0.);
					LiViLatheRotate(LW_lathe,Sstack_ptr->rdata[10]);
				}
/*
...........Move the machine assemblies
*/
				fromt = UU_FALSE;
				if (Sstack_ptr->ctype == LW_CUT_POSITION) fromt = UU_TRUE;
				ul_ipv_move_assemblies(Sstack_ptr->rdata,UU_FALSE,fromt);
			}
/*
.........Positioning move
*/
			else
			{
				switch (Sstack_ptr->ctype)
				{
				case LW_CUT_POSITION:
					LiViToolSetPosition(LW_tool[0],Sstack_ptr->rdata);
					um_vctovc(&Sstack_ptr->rdata[0],tool_pos);
					if (Sstack_ptr->nval == 6)
					{
						LiViToolSetAxis(LW_tool[0],&Sstack_ptr->rdata[3]);
						um_vctovc(&Sstack_ptr->rdata[3],tool_vec);
					}
					break;
/*
.........Standard 5-axis cut
*/
				case LW_CUT_5AXIS:
					LiViToolDefine5AxisLinearCut(LW_tool[0],Sstack_ptr->rdata,
						&Sstack_ptr->rdata[3]);
					um_vctovc(&Sstack_ptr->rdata[0],tool_pos);
					um_vctovc(&Sstack_ptr->rdata[3],tool_vec);
					LiViPerformCut(LW_session[LW_mach_mode]);
					break;
/*
.........Complex blade cut
*/
				case LW_CUT_BLADE:
					LiViToolDefineComplexCut(LW_tool[0],Sstack_ptr->rdata,
					&Sstack_ptr->rdata[3],&Sstack_ptr->rdata[6]);
					um_vctovc(&Sstack_ptr->rdata[0],tool_pos);
					um_vctovc(&Sstack_ptr->rdata[3],tool_vec);
					LiViPerformCut(LW_session[LW_mach_mode]);
					break;
/*
.........Lathe cut
*/
				case LW_CUT_LATHE:
					LiViTurningToolDoSmoothLinearCut(LW_tool[0],Sstack_ptr->rdata);
					um_vctovc(&Sstack_ptr->rdata[0],tool_pos);
					um_vctovc(&Sstack_ptr->fdata[3],tool_vec);
					break;
/*
........Lathe threading cut
*/
				case LW_CUT_THREAD:
					LiViLatheSetOrientation(LW_lathe,0.);
					LiViTurningToolDoLinearCut(LW_tool[0],Sstack_ptr->rdata,
						Sstack_ptr->rdata[3]);
					um_vctovc(&Sstack_ptr->rdata[0],tool_pos);
					um_vctovc(&Sstack_ptr->fdata[3],tool_vec);
					break;
/*
........Circular arc cut
*/
				case LW_CUT_ARC:
					LiViToolDoArcCut(LW_tool[0],Sstack_ptr->rdata,
						Sstack_ptr->rdata[3]);
					um_vctovc(&Sstack_ptr->fdata[0],tool_pos);
					um_vctovc(&Sstack_ptr->fdata[3],tool_vec);
					break;
/*
........Vertical arc cut
*/
				case LW_CUT_VERTICAL_ARC:
					LiViToolDoVerticalArcCut(LW_tool[0],Sstack_ptr->rdata,
						&Sstack_ptr->rdata[3],Sstack_ptr->rdata[6]);
					um_vctovc(&Sstack_ptr->rdata[0],tool_pos);
					um_vctovc(&Sstack_ptr->fdata[3],tool_vec);
					break;
/*
........Lathe arc cut
*/
				case LW_CUT_LATHE_ARC:
					LiViTurningToolDoSmoothArcCut(LW_tool[0],Sstack_ptr->rdata,
						Sstack_ptr->rdata[3]);
					um_vctovc(&Sstack_ptr->rdata[0],tool_pos);
					um_vctovc(&Sstack_ptr->fdata[3],tool_vec);
					break;
				}
			}
/*
........Store the new cut volumes
*/
			S_store_cut_prims(Sstack_ptr);
			if (dir == 1 && inc >= nstep) done = UU_TRUE;
		} while (!done);
	}
/*
.....Display the cut
*/
	if (idid)
	{
/*
........Moving part is active
*/
		if (Sstack_ptr->moving_part == 1)
			ul_ipv_view_taxis(tool_pos,tool_vec);
/*
........Flush the graphics
*/
		else
		{
			ul_ipv_flush();
			um_reset_pocket_graphics(UM_IPV_WINDOW);
		}
	}
/*
.....End of routine
*/
done:;
	return(idid);
}

/*********************************************************************
**    I_FUNCTION     :  S_store_cut_prims(sptr)
**       Stores the current swept volumes in the motion stack entity
**       pointed to by 'sptr'.
**    PARAMETERS
**       INPUT  :
**          sptr   = Pointer to NCLIPV motion stack entry.
**       OUTPUT :
**          sptr   = Updated motion stack entry.
**    RETURNS      : none
**    SIDE EFFECTS :
**          The parameter 'sptr->nprim' must be set to the number of
**          cut volume primitives already stored, as these will be
**          deleted prior to storing the new cut volumes.
**    WARNINGS     : none
*********************************************************************/
void S_store_cut_prims(sptr)
LW_mot_stack_struc *sptr;
{
	int i,j,k,ntim,inc,ifl;
	LW_stock_struc *sd,*sdtmp;
	LtDouble vol;
	LtSessionPrim prim,tprim;
	LtPrim mprim;
	LtData stuff;
/*
.....Free current cut volumes
*/
	S_delete_cut_prims(sptr,Spptr);
/*
.....Get the cut volume for each stock and fixture
*/
	sptr->nprim[Spptr] = 0;
	ntim = LW_mot_stack_fixture ? 2 : 1;
	for (inc=0;inc<LW_spindle_num;inc++)
	{
		for (k=0;k<2+LW_num_holder[inc];k++)
		{
			if (k == 0) tprim = LW_tool[inc];
			else if (k == 1) tprim = LW_shank[inc];
			else tprim = LW_holder[inc][k-2];
			if (tprim != 0)
			{
				for (j=0;j<ntim;j++)
				{
					sd = LW_stock_first[j];
					for (i=0;i<LW_nstock[j];i++)
					{
						ifl = 0;
						do
						{
							ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
							if (ifl == -2) break;
							prim = LiViSolidGetIntersectSolid(tprim,sdtmp->stock);
							if (prim != 0)
							{
								LiViSolidGetVolume(prim,&vol);
								if (vol != 0.)
								{
									sptr->prim[Spptr][sptr->nprim[Spptr]++] =
										sdtmp->stock;
									sptr->prim[Spptr][sptr->nprim[Spptr]++] = prim;
								}
								else
								{
									LiSessionPrimGetProperty(prim,LI_SPRIM_PROP_PRIM,
										&stuff);
									mprim = (LtPrim)LiDataGetGenericPtr(&stuff);
									LiSessionRemovePrim(prim);
/*
.....Cannot destroy the prim right now
.....it causes a fatal error
.....Wait for response from MachineWorks
//								LiPrimitiveDestroy(mprim);
*/
								}
							}
						} while (ifl != -1 && sptr->nprim[Spptr] < MAXPRIM);
						if (sptr->nprim[Spptr] == MAXPRIM) break;
						sd = (LW_stock_struc *)uu_lsnext(sd);
					}
				}
			}
		}
	}
}

/*********************************************************************
**    I_FUNCTION     :  S_delete_cut_prims(sptr,ifl)
**       Deletes the swept volumes stored in the motion stack entity
**       pointed to by 'sptr'.
**    PARAMETERS
**       INPUT  :
**          sptr   = Pointer to NCLIPV motion stack entry.
**          ifl    = 0 = Delete both copies of swept volumes.
**                   1 = Delete only secondary swept volumes.
**       OUTPUT :
**          sptr   = Updated motion stack entry.
**    RETURNS      : none
**    SIDE EFFECTS :
**          The parameter 'sptr->nprim' must be set to the number of
**          cut volume primitives already stored, as these will be
**          deleted.
**    WARNINGS     : none
*********************************************************************/
void S_delete_cut_prims(sptr,ifl)
LW_mot_stack_struc *sptr;
int ifl;
{
	int i,j;
	LW_mot_stack_struc *stkp;
	LtData stuff;
	LtPrim prim;
/*
........Delete all stored primitives
*/
	if (sptr == UU_NULL)
	{
		if (Sfirst_ptr != Slast_ptr)
		{
			stkp = Sfirst_ptr;
			do
			{
				stkp = (LW_mot_stack_struc *)uu_lsnext(stkp);
				S_delete_cut_prims(stkp,ifl);
			} while (stkp != Slast_ptr);
		}
	}
/*
.....Free current cut volumes
*/
	else
	{
		for (j=ifl;j<=1;j++)
		{
			if (sptr->nprim[j] != 0)
			{
				for (i=1;i<sptr->nprim[j];i=i+2)
				{
/*      			LiSessionPrimGetProperty(sptr->prim[j][i],LI_SPRIM_PROP_PRIM,
						&stuff);
      			prim = LiDataGetGenericPtr(&stuff);*/
					LiSessionRemovePrim(sptr->prim[j][i]);
/*					LiPrimitiveDestroy(prim);*/
				}
				sptr->nprim[j] = 0;
			}
		}
	}
}

/*********************************************************************
**    I_FUNCTION     :  S_calc_cutter(cutter,nspin,tend,taxis)
**       Calls the NCLIPV cutter definition routine when the cutter
**       changes while stepping through the motion stack.
**    PARAMETERS
**       INPUT  :
**          cutter = Cutter on stack to define.
**          nspin  = Number of active spindles.
**          tend   = Current position.
**          taxis  = Current tool axis.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_calc_cutter(cutter,nspin,tend,taxis)
int cutter[],nspin;
UU_REAL tend[],taxis[];
{
	int i,inc;
	UU_REAL pos[6],cutr[9];
	UN_mot_data mdata;
	UN_mot_attr mattr;
	LW_cutter_stack_struc *ptr;
/*
.....Initialize routine
*/
	ptr = (LW_cutter_stack_struc *)UU_LIST_ARRAY(&Scutter_list);
/*
.....Load a tool in each spindle
*/
	for (inc=0;inc<nspin;inc++)
	{
/*
.....Set current cutter parameters
*/
		LW_act_tool[ptr[cutter[inc]].spindle] = ptr[cutter[inc]].tool;
		ncl_cutter_set_ptrs(ptr[cutter[inc]].cutseg);
		LW_toler = ptr[cutter[inc]].toler;
		LW_maxang = ptr[cutter[inc]].maxang;
		for (i=0;i<3;i++)
		{
			LW_translucency[i] = ptr[cutter[inc]].translucency[i];
			pos[i] = tend[i]; pos[i+3] = taxis[i];
		}
		for (i=0;i<9;i++) cutr[i] = ptr[cutter[inc]].cutr[i];
		mdata.fr_mode = ptr[cutter[inc]].fr_mode;
		mdata.fr_val = ptr[cutter[inc]].fr_val;
		mattr.sp_val = ptr[cutter[inc]].sp_val;
/*
.....Create cutter for this motion
*/
		ul_ipv_cutter(pos,cutr,&mdata,&mattr,-1,ptr[cutter[inc]].spindle);
/*
.....Store last cutter index
*/
		Slast_cutter[inc] = cutter[inc];
	}
}

/*********************************************************************
**    I_FUNCTION     :  S_find_cutn(cutn)
**       Finds the selected cut number in the Undo stack.  Returns a
**       pointer to the stack entry or UU_NULL.
**    PARAMETERS
**       INPUT  :
**          cutn    = Cut number to search stack for.
**       OUTPUT :
**          none
**    RETURNS      :
**          The pointer within the Undo stack of the entry with the
**          specified cut number or UU_NULL if the cut number is not
**          found.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
LW_mot_stack_struc *S_find_cutn(cutn)
LtNat32 cutn;
{
	UU_LOGICAL idid;
	LW_mot_stack_struc *sptr;
/*
.....Initialize routine
*/
	idid = UU_FALSE;
/*
.....Find the cut number in the Undo stack
*/
	sptr = Sfirst_ptr;
	while (sptr != Slast_ptr)
	{
		sptr = (LW_mot_stack_struc *)uu_lsnext(sptr);
		if (cutn == sptr->cutn)
		{
			idid = UU_TRUE;
			break;
		}
	}
/*
.....End of routine
*/
	if (!idid) sptr = UU_NULL;
	return(sptr);
}
