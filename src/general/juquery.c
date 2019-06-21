/********************************************************************r
**    NAME         : juquery.c
**       CONTAINS:
**				uju_qry_display()
**				uju_qry_motion()
**  COPYRIGHT  2009  NCCS, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       juquery.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       11/22/17 , 10:30:55
**********************************************************************/

#include "usysdef.h"
#include "class.h"
#include "dselect.h"
#include "mdpick.h"
#include "mdcpln.h"
#include "mfort.h"
#include "nconst.h"
#include "nclcmd.h"
#include "nclfc.h"
#include "nclinp.h"
#include "nclmplay.h"
#include "udfdata.h"
#include "udforms.h"
#include "uhep.h"
#include "ulist.h"
#include "msrf.h"
#include "msol.h"
#include "nccs.h"
#include "mattr.h"

#define FLAB 0
#define FKEY 1
#define FREL 2
#define FEXT 3
#define FCOL 4
#define FLAY 5
#define FPEN 6
#define FSTY 7
#define FWGT 8
#define FEDG 9
#define FCLR 10
#define FSHD 11
#define FTRN 12
#define FMKR 13
#define FCMD 14
#define FAPP 15
#define FSEL 16
#define FSRC 17
#define FCAL 18
#define FRES 19
#define FEDT 20
#define FDAT 21

#define FMHGT 0
#define FMSEL 1
#define FMSRC 2
#define FMCAL 3
#define FMRES 4
#define FMEDT 5
#define FMDAT 6

static int Scmd = 0,Sfrm;
static int Smhilite = 0;
static int Sreset = 1;
static int Scolor,Slayer,Spen,Sstyle,Sweight,Sflag,Sisn,Slast_isn;
static int Srel_num,modMkr,choice,Secolor,Slucency,Sshaded,Schoice;
static UU_LOGICAL Sattr_flag,imsk3,imsk4;
static UU_KEY_ID Skey,Skey2;
static char Srelation[32],Slabel[NCL_MAX_LABEL_AND_SUBSCRIPT];
static struct UC_attributedatabag Sattr;
static UD_LIST Ssrc_list,Sstat_list,Scall_list;
static UU_LIST *Slist;
static UN_motseg *Smptr;
static int changeEcolor, changeShaded, changeLucency;
static int ecolor,shaded,lucency,dispEdges,SdispEdges,changeEdges;

static UD_FSTAT OnExtSel(),OnExtTog();
static UD_FSTAT OnApply(),OnSelect(),OnSource(),OnStack();
static UD_FSTAT OnSelectMotion(),OnEdit(),OnSource();
static void S_mod_attribs(),S_fill_lists();
static void S_set_motion(),S_set_traverse();

extern char uw_color_name[64][96];

/*********************************************************************
**    E_FUNCTION     : uju_qry_display(inkey,list,flag)
**			Controls the Entity Attribute and Entity Data forms.
**
**    PARAMETERS   
**       INPUT  :
**				inkey    Key of entity for display.
**				list     List of entity data lines to output to Status field.
**				flag     0 = Use Entity Attributes form, 1 = Entity Data form.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uju_qry_display(inkey,list,flag)
UU_KEY_ID inkey;
UU_LIST *list;
int flag;
{
	UU_KEY_ID key;
	int status;

	static int *ans[22];
	static UD_METHOD methods[] = {UU_NULL,UU_NULL,UU_NULL,OnExtSel,
		UU_NULL,UU_NULL,UU_NULL, UU_NULL,UU_NULL, OnExtTog,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL, UU_NULL,OnApply,OnSelect, OnSource,OnStack,
		UU_NULL,OnEdit, UU_NULL};
	static char called[]  = {6,6,6,6, 6,6,6, 6,6, 6,6,6,6, 6, 6,6,6, 
									6,6, 6,6, 6};
	char traverse[]       = {1,1,1,1, 1,1,1, 1,1, 0,0,0,0, 0, 1,1,1, 
									1,1, 1,1, 1};
	static char display[] = {1,1,1,1, 1,1,1, 1,1, 0,0,0,0, 0, 1,1,1, 
									1,1, 1,1, 1};
/*
.....Initialize routine
*/
	Sflag = flag;
	Skey = inkey;
	Slist = list;
	Sisn = 0;
	getnln(&Slast_isn);
	choice = Schoice = modMkr = dispEdges = 0;
/*
.....Initialize attribute fields
*/
	Sattr_flag = S_set_attribs();
/*
.....Setup the default answers
*/
	changeEcolor = UU_FALSE;
	changeShaded = UU_FALSE;
	changeLucency = UU_FALSE;
	Secolor = ecolor;
	Sshaded = shaded;
	Slucency = lucency;
	modMkr = 0;

	ans[FLAB] = (int *)&Slabel;
	ans[FKEY] = (int *)&Skey;
	ans[FREL] = (int *)Srelation;
	ans[FCOL] = &Scolor;
	ans[FLAY] = &Slayer;
	ans[FPEN] = &Spen;
	ans[FSTY] = &Sstyle;
	ans[FWGT] = &Sweight;
	ans[FCMD] = &Scmd;
	ans[FRES] = &Sreset;
	ans[FEDG] = &dispEdges;
	ans[FCLR] = &ecolor;
	ans[FSHD] = &shaded;
	ans[FTRN] = &lucency;
	ans[FMKR] = &choice;
/*
.....Entity Data fields
........Initialize lists
*/
	if (flag == 1)
	{
		Ssrc_list.num_item = 0;
		Ssrc_list.item = UU_NULL;
		Ssrc_list.answer = UU_NULL;

		Scall_list.num_item = 0;
		Scall_list.item = UU_NULL;
		Scall_list.answer = UU_NULL;

		Sstat_list.num_item = 0;
		Sstat_list.item = UU_NULL;
		Sstat_list.answer = UU_NULL;
/*
.....Propogate the lists
*/
		S_fill_lists();
/*
........Set the default answers
*/
		ans[FSRC] = (int *)&Ssrc_list;
		ans[FCAL] = (int *)&Scall_list;
		ans[FDAT] = (int *)&Sstat_list;
	}
/*
.....Set up traversal flags
*/
	S_set_traverse(Srel_num,traverse,UU_FALSE);

/*
.....Set extended attribute display flags
*/
	display[FMKR] = imsk4;
	display[FEDG] = imsk3;
	display[FCLR] = imsk3;
	display[FSHD] = imsk3;
	display[FTRN] = imsk3;
/*
.....Get the Form input
*/
	if (flag == 1)
		Sfrm = ud_form1("entdata.frm",ans,ans,methods,called,display,
			traverse);
	else
		Sfrm = ud_form1("entattrib.frm",ans,ans,methods,called,display,
			traverse);
/*	if (Sreset) setnln(&Slast_isn);*/
	if (Sfrm == -1) goto done;
/*
.....Output any changed attributes
*/
	if (Sattr_flag) S_mod_attribs();
	goto done;
/*
.....Could not get entity attributes
*/
failed:;
	ud_wrerr("Could not obtain entity attributes");
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : uju_qry_motion()
**			Controls the Motion Data form.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uju_qry_motion()
{
	int status,ival;
	UD_DDATA val;

	static int *ans[7];
	static UD_METHOD methods[] = {UU_NULL,OnSelectMotion, OnSource,OnStack,
		UU_NULL,OnEdit,UU_NULL};
	static char called[] = {6,6, 6,6, 6,6, 6};
	char traverse[] = {1,1, 1,1, 1,1, 1};
	static char display[] = {1,1, 1,1, 1,1, 1};
/*
.....Initialize routine
*/
	Sflag = 2;
	Sisn = 0;
	getnln(&Slast_isn);
/*
.....Initialize lists
*/
	Ssrc_list.num_item = 0;
	Ssrc_list.item = UU_NULL;
	Ssrc_list.answer = UU_NULL;

	Scall_list.num_item = 0;
	Scall_list.item = UU_NULL;
	Scall_list.answer = UU_NULL;

	Sstat_list.num_item = 0;
	Sstat_list.item = UU_NULL;
	Sstat_list.answer = UU_NULL;
/*
.....Select the motion for data display
*/
	ival = -1;
	status = OnSelectMotion(&ival,&val,UD_FLDOK);
	if (status != UD_FLDOK) goto done;
/*
.....Setup the default answers
*/
	ans[FMHGT] = &Smhilite;
/*
........Set the default answers
*/
	ans[FMSRC] = (int *)&Ssrc_list;
	ans[FMCAL] = (int *)&Scall_list;
	ans[FMDAT] = (int *)&Sstat_list;
	ans[FMRES] = &Sreset;
/*
.....Get the Form input
*/
	status = ud_form1("motdata.frm",ans,ans,methods,called,display,
		traverse);
/*
.....Reset line number
*/
/*	if (Sreset) setnln(&Slast_isn);*/
	if (status == -1) goto done;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    S_FUNCTION     :  OnApply(fieldno, val, stat)
**       Method called when the Apply button is pressed.  Modifies
**       any attributes that have changed for the selected entity.
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
static UD_FSTAT OnApply(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Modify requested attributes
*/
	S_mod_attribs();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSelect(fieldno, val, stat)
**       Method called when the Select button is pressed.  Selects
**       a geometry item to display the attributes for.
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
static UD_FSTAT OnSelect(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL cmdreject;
	int nsels,status;
	UM_PLOCREC pick;
/*
.....Take the form down
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Pick geometry for query data display
*/
	um_dl_pldas(UD_DASPCKLOC, UJ_SUPPORT, 16, &pick, 1, &nsels, 1);
	if (nsels <= 0) goto done;
/*
.....Get the key from the pick record
*/
   Skey = um_get_pickkey(&pick.pent, 1);
/*
.....Set attribute fields
*/
	Sattr_flag = S_set_attribs();
/*
.....Propogate the Entity Data lists
*/
	if (Sflag == 1)
	{
/*
........First get the Entity Data
*/
		uj_free_list(Slist);
		uj_init_query(Slist,10);
		status = uc_query(Skey,Slist);
/*
........Now fill the lists
*/
		S_fill_lists();
	}
/*
.....Update the form
*/
	ud_update_answer(FLAB,(int *)Slabel);
	ud_update_answer(FKEY,(int *)&Skey);
	ud_update_answer(FREL,(int *)Srelation);
	ud_update_answer(FCOL,(int *)&Scolor);
	ud_update_answer(FLAY,(int *)&Slayer);
	ud_update_answer(FPEN,(int *)&Spen);
	ud_update_answer(FSTY,(int *)&Sstyle);
	ud_update_answer(FWGT,(int *)&Sweight);
	ud_update_answer(FPEN,(int *)&Spen);
	ud_update_answer(FSTY,(int *)&Sstyle);
	ud_update_answer(FCLR,(int *)&Secolor);
	ud_update_answer(FTRN,(int *)&Slucency);
	ud_update_answer(FSHD,(int *)&Sshaded);
	ud_update_answer(FMKR,(int *)&Schoice);
	if (Sflag == 1)
	{
		ud_update_answer(FSRC,(int *)&Ssrc_list);
		ud_update_answer(FCAL,(int *)&Scall_list);
		ud_update_answer(FDAT,(int *)&Sstat_list);
	}
/*
.....Update the traversal fields
*/
	S_set_traverse(Srel_num,UU_NULL,UU_TRUE);
/*
.....Set extended attribute display flags
*/	
	ud_set_display_mask(UD_INPUTF,FMKR,imsk4);
	ud_set_display_mask(UD_INPUTF,FEDG,imsk3);
	ud_set_display_mask(UD_INPUTF,FCLR,imsk3);
	ud_set_display_mask(UD_INPUTF,FSHD,imsk3);
	ud_set_display_mask(UD_INPUTF,FTRN,imsk3);
/*
.....Display the form
*/
	ud_form_vis();
/*
.....End of routine
*/
done:;
	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSelectMotion(fieldno, val, stat)
**       Method called when the Select button is pressed from the
**       Motion Data form.  Selects the motion to display the
**       data for.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed. -1 = Form is not
**                   displayed yet, just select motion.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSelectMotion(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL cmdreject;
	int nsels,status;
	UD_PLOCREC pick;
	UD_FSTAT fstat;
/*
.....Take the form down
*/
	if (*fieldno != -1) ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto failed;
/*
.....Pick geometry for query data display
*/
	if (Smhilite == 0) ud_setpick_type(UD_PICK_MOTION);
	else ud_setpick_type(UD_PICK_MOTSEG);
	ud_ldas(UD_DASPCKLOC, UJ_SUPPORT, 16, &pick, 1, &nsels, 1);
	if (nsels <= 0 || pick.ppath.depth == 0) goto failed;
/*
.....Get the key from the pick record
*/
	Smptr = (UN_motseg *)pick.ppath.pickpath[1];
	if (Smptr == UU_NULL) goto failed;
/*
.....Set motion data fields
*/
	S_set_motion();
/*
.....Fill the data and source lists
*/
	S_fill_lists();
/*
.....Update the form
*/
	if (*fieldno != -1)
	{
		ud_update_answer(FMSRC,(int *)&Ssrc_list);
		ud_update_answer(FMCAL,(int *)&Scall_list);
		ud_update_answer(FMDAT,(int *)&Sstat_list);
	}
	fstat = UD_FLDOK;
	goto done;
/*
.....User didn't pick anything
*/
failed:;
	fstat = UD_DONE;
/*
.....End of routine
*/
done:;
	ud_setpick_type(UD_PICK_NORMAL);
	if (*fieldno != -1) ud_form_vis();
	UD_UNMARK(cmdreject);
	return(fstat);
}

/*********************************************************************
**    S_FUNCTION     :  OnSource(filedno, val, stat)
**       Method called at when  tool in the tool listbox
**			is selected
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
static UD_FSTAT OnSource(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char temp[81], *tok, *strtok();
/*
.....val.frmstr contains the selected string
....."isn COMMAND"
*/
	if (val->frmstr!=NULL)
	{
		strcpy(Ssrc_list.answer, val->frmstr);
		strcpy(temp, val->frmstr);
		tok = strtok(temp, " ");
		if (tok!=NULL) Sisn = atoi(tok);
	}
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  OnStack(fieldno, val, stat)
**       Updates the Source list when a line is selected in the
**       Call Stack list.
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
static UD_FSTAT OnStack(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int isn;
	char *tok,*strtok();
	char temp[81];
/*
.....Get ISN of call stack line
.....and display appropriate source lines
*/
	if (val->frmstr != UU_NULL)
	{
		strcpy(Scall_list.answer,val->frmstr);
		strcpy(temp,val->frmstr);
		tok = strtok(temp,":");
		if (tok != UU_NULL)
		{
			isn = atoi(tok);
			ncl_motisn_source_list(isn,&Ssrc_list);
		}
		if (Sflag == 1) ud_update_answer(FSRC,(int *)&Ssrc_list);
		else ud_update_answer(FMSRC,(int *)&Ssrc_list);
		Sisn = isn;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnEdit(filedno, val, stat)
**       Method called when the "Edit" button is pushed
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
static UD_FSTAT OnEdit(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int markval;
	if (Sisn > 0)
	{
		UD_MARK(markval,UU_TRUE);
		if (markval != 0) goto done;
		setnln(&Sisn);
		ud_form_invis();
		ncl_cmd_mode();
done:;
		if (Sreset) setnln(&Slast_isn);
		ud_form_vis();
		UD_UNMARK(markval);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  S_set_attribs()
**       Sets the attribute form fields attributes for the selected
**       entity.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_TRUE if attributes can be set for this entity.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_set_attribs()
{
	UU_LOGICAL status;
	char rel_name[32];
   struct UC_entitydatabag e;
	struct UM_surfattr_rec srfattr;
	struct NCL_nclpt_rec pt;
	struct NCL_patern_rec pn;
/*
.....Get entity & attributes
*/
	status = UU_TRUE;
   e.key = Skey;
   if (uc_retrieve_data(&e, sizeof(struct UC_entitydatabag)) != UU_SUCCESS)
      goto done;
   if (uc_retrieve_attr(e.key, &Sattr) != UU_SUCCESS) goto done;
/*
.....Initialize attribute fields
*/
	if (e.rel_num >= UA_LINEAR_DIMS_REL && e.rel_num <= UA_DRWBORDR_REL)
		Slabel[0] = '\0';
	else
		ncl_get_label(&e,Slabel);
	Skey = e.key;
	ur_retrieve_rel_name(e.key,rel_name);
	sprintf(Srelation,"%d - %s",e.rel_num,rel_name);
		
	if (Sattr.color < 0) Scolor = 0;
	else if (Sattr.color >= 64) Scolor = 63;
	else
		Scolor = Sattr.color;
	Slayer = Sattr.layer;
	Spen = Sattr.pen;
	if (Sattr.line_style < 1) Sattr.line_style = 1;
	Sstyle = Sattr.line_style - 1;
	if (Sattr.line_weight < 1) Sattr.line_weight = 1;
	Sweight = Sattr.line_weight - 1;
/*
.....Store surface information if one is selected
*/
	switch (e.rel_num)
	{
	case UM_AGSRF_REL:
	case UM_RBSPLSRF_REL:
	case NCL_SURF_REL:
	case NCL_MESHSURF_REL:
	case NCL_QUILTSURF_REL:
	case NCL_NETSF_REL:
	case NCL_TRIMSF_REL:
	case NCL_REVSURF_REL:
	case UM_SOLID_REL:
		status = uc_retrieve_attr(e.key,&srfattr);
/*
.....To signify the default color is being used the color is set to -1 or 64
*/
		if (status == UU_SUCCESS)
		{
			Secolor = ecolor = (srfattr.ecolor == 64)? -1 : srfattr.ecolor;
			Sshaded = shaded = srfattr.shaded;
			Slucency = lucency = srfattr.lucency;
			SdispEdges = dispEdges = (ecolor >= 0 || srfattr.ecolor == 64)? 1 : 0;
		}
		status = UU_TRUE;
		break;
	case NCL_PATERN_REL:
		pn.key = e.key;
		status = ur_retrieve_data_fixed(&pn);
		Schoice = choice = pn.markertype - 1;
		status = UU_TRUE;
		break;
	case UM_POINT_REL:
		pt.key = e.key;
		status = ur_retrieve_data_fixed(&pt);
		Schoice = choice = pt.markertype - 1;
		status = UU_TRUE;
		break;
	default:
		break;
	}
done:;
	Srel_num = e.rel_num;
	if ((Srel_num >= UA_LINEAR_DIMS_REL && Srel_num <= UA_DRWBORDR_REL) ||
		(Srel_num >= UB_SYMBOL_REL && Srel_num <= UB_CONECTOR_REL))
			status = UU_FALSE;
	return(status);
}

/*********************************************************************
**    S_FUNCTION     :  S_set_traverse(rel_num,traverse,ifl)
**       Sets the form traverse fields depending on the relation type
**       selected.
**    PARAMETERS
**       INPUT  :
**          rel_num   = Relation number of selected entity.
**          ifl       = UU_TRUE = Form is active, update fields.
**                      UU_FALSE = Form is not active, update traverse array.
**       OUTPUT : none
**          traverse  = Updated traverse array when ifl = UU_FALSE.
**    RETURNS      : UU_TRUE if attributes can be set for this entity.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_set_traverse(rel_num,traverse,ifl)
int rel_num;
char traverse[];
UU_LOGICAL ifl;
{
	UU_LOGICAL imsk1,imsk2;
/*
.....Initialize routine
*/
	imsk1 = imsk2 = imsk3 = imsk4 = UU_TRUE;
/*
.....Disable attribute fields for
.....Drafting & Symbol entities
*/
	if ((rel_num >= UA_LINEAR_DIMS_REL && rel_num <= UA_DRWBORDR_REL) ||
		(rel_num >= UB_SYMBOL_REL && rel_num <= UB_CONECTOR_REL))
			imsk1 = UU_FALSE;
	if (rel_num >= UA_LINEAR_DIMS_REL && rel_num <= UA_DRWBORDR_REL)
			imsk2 = UU_FALSE;
/*
.....Additional rel_num checks to ensure fields are only accessible when 
.....appropriate
*/
   if (rel_num != UM_AGSRF_REL && rel_num != UM_RBSPLSRF_REL &&
		rel_num != NCL_SURF_REL && rel_num != NCL_MESHSURF_REL &&
		rel_num != NCL_QUILTSURF_REL && rel_num != NCL_NETSF_REL &&
		rel_num != NCL_TRIMSF_REL && rel_num != NCL_REVSURF_REL &&
		rel_num != UM_SOLID_REL)
			imsk3 = UU_FALSE;
	if (rel_num != UM_POINT_REL && rel_num != NCL_PATERN_REL)
			imsk4 = UU_FALSE;
/*
.....Set field traversals
*/
	
	if (ifl)
	{
		ud_set_traverse_mask(FLAB,imsk2);
		ud_set_traverse_mask(FCOL,imsk1);
		ud_set_traverse_mask(FLAY,imsk1);
		ud_set_traverse_mask(FPEN,imsk1);
		ud_set_traverse_mask(FEXT,imsk1);
		ud_set_traverse_mask(FSTY,imsk1);
		ud_set_traverse_mask(FWGT,imsk1);
		ud_set_traverse_mask(FCMD,imsk1);
		ud_set_traverse_mask(FEDG,imsk3);
		ud_set_traverse_mask(FSHD,imsk3);
		ud_set_traverse_mask(FTRN,imsk3); 
		ud_set_traverse_mask(FMKR,imsk4); 
		ud_set_traverse_mask(FCLR,dispEdges); 
	}
/*
.....Set traversal masks
*/
	else
	{
		traverse[FLAB] = imsk2;
		traverse[FCOL] = imsk1;
		traverse[FLAY] = imsk1;
		traverse[FPEN] = imsk1;
		traverse[FEXT] = imsk1;
		traverse[FSTY] = imsk1;
		traverse[FWGT] = imsk1;
		traverse[FCMD] = imsk1;
		traverse[FAPP] = imsk1;
		traverse[FEDG] = imsk3;
		traverse[FSHD] = imsk3;
		traverse[FTRN] = imsk3;
		traverse[FMKR] = imsk4;
		traverse[FCLR] = dispEdges;
	}
}

/*********************************************************************
**    S_FUNCTION     :  S_set_motion()
**       Formats the Motion Data list with the motion attribute
**       settings for the picked motion.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_set_motion()
{
	int i;
	UU_REAL spt[6];
	char sbuf[132];
	UN_motseg *mfrm;
	UN_mot_attr mattr;
	UN_motseg_cutter cutseg;
	static char *colcd[]={"OFF","FLOOD","MIST","AIR"};
	static char *cdir[]={"OFF","LEFT","RIGHT"};
	static char *cmod[]={"XYPLAN","YZPLAN","ZXPLAN"};
	static char *sdir[]={"CLW","CCLW"};
/*
.....Initialize the Motion Data list
*/
	ul_ipv_init_list(&Sstat_list,20);
/*
.....Get the motion attributes
*/
	ncl_motmattr_get(&mattr,Smptr->mattr);
/*
.....FROM
*/
	mfrm = (UN_motseg *)uu_lsprev(Smptr);
	if (mfrm == UU_NULL) mfrm = Smptr;
	um_vctovc(&mfrm->tend,spt); um_vctovc(&mfrm->taxis,&spt[3]);
	ncl_wcstomcs(0,spt,spt);
	ncl_wcstomcs(1,&spt[3],&spt[3]);
	UM_cc_inttoext(spt,spt);
	sprintf(sbuf,"FROM/%f, %f, %f,  %f, %f, %f",spt[0],spt[1],spt[2],spt[3],
		spt[4],spt[5]);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
.....GOTO
*/
	um_vctovc(&Smptr->tend,spt); um_vctovc(&Smptr->taxis,&spt[3]);
	ncl_wcstomcs(0,spt,spt);
	ncl_wcstomcs(1,&spt[3],&spt[3]);
	UM_cc_inttoext(spt,spt);
	sprintf(sbuf,"GOTO/%f, %f, %f,  %f, %f, %f",spt[0],spt[1],spt[2],spt[3],
		spt[4],spt[5]);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
.....CUTTER
*/
	ncl_cutter_get(&cutseg,Smptr->cutter);
	ncl_format_tool(sbuf,&cutseg);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
.....LOADTL
*/
	if (mattr.loadtl != 0)
	{
		sprintf(sbuf,"LOADTL/%d,LENGTH,%f",mattr.loadtl,mattr.tlen);
		ul_ipv_put_list(&Sstat_list,sbuf);
	}
/*
.....FEDRAT
*/
	if (Smptr->fr_mode == 0)
	{
		ul_ipv_put_list(&Sstat_list,"RAPID");
	}
	else
	{
		if (Smptr->fr_val != 0.)
		{
			if (Smptr->fr_mode == 2)
				sprintf(sbuf,"FEDRAT/FPR,%f",Smptr->fr_val);
			else
				sprintf(sbuf,"FEDRAT/FPM,%f",Smptr->fr_val);
			ul_ipv_put_list(&Sstat_list,sbuf);
		}
	}
/*
........SPINDL
*/
	if (mattr.sp_val != 0.)
	{
		sprintf(sbuf,"SPINDL/%f,%s",mattr.sp_val,sdir[mattr.sp_mode]);
		ul_ipv_put_list(&Sstat_list,sbuf);
	}
/*
........COOLNT
*/
	strcpy(sbuf,"COOLNT/");
	strcat(sbuf,colcd[mattr.coolnt]);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
........CUTCOM
*/
	if (mattr.cc_dir == 0)
		sprintf(sbuf,"CUTCOM/%s",cdir[mattr.cc_dir]);
	else
		sprintf(sbuf,"CUTCOM/%s,%s",cdir[mattr.cc_dir],cmod[mattr.cc_mode]);
	ul_ipv_put_list(&Sstat_list,sbuf);
}

/*********************************************************************
**    S_FUNCTION     :  S_mod_attribs()
**       Modifies the selected entity attributes per the form settings.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS :
**          Outputs an NCL command if the Output Command form
**          field is set.
**    WARNINGS     : none
*********************************************************************/
static void S_mod_attribs()
{
	int i,style,weight,status,color,tcolor,nkeys;
	UU_LOGICAL modfield[5],display;
	UU_KEY_ID key,*kptr;
	UU_REAL rweight;
	char buf[40];
	NCL_cmdbuf cmdbuf;
   struct UC_entitydatabag e;
	struct UM_surfattr_rec srfattr;
	static char *Soffon[]={"OFF","ON"};
	struct NCL_nclpt_rec pt;
	struct NCL_patern_rec pn;
	struct UM_solid_rec solid;
	static char *marks[] = {"DOT","PLUS","STAR","CIRCLE","CROSS",
									"TRIAN","DIMOND","SQUARE","DBLCIR",
									"LRGDOT","CUBE"};
	static char *lines[] = {"SOLID","DASH","DOTTED","CENTER","PHANTM",
									"DASHLN","DASHDT","DASHSP"};
	static char *wghts[] = {"STD","MEDIUM","HEAVY","EXHVY"};
/*
.....See if any attribute changes were made
*/
	style = Sstyle + 1;
	weight = Sweight + 1;
	color = Scolor;
	changeEcolor = Secolor != ecolor;
	changeLucency = Slucency != lucency;
	changeShaded = Sshaded != shaded;
	modMkr = Schoice != choice;
	changeEdges = SdispEdges != dispEdges;

	if (color != Sattr.color || Slayer != Sattr.layer ||
		Spen != Sattr.pen || style != Sattr.line_style ||
		weight != Sattr.line_weight || changeEcolor ||
		changeShaded || changeLucency || modMkr || changeEdges)
	{
/*
........Determine which attributes to update
*/
		Secolor = ecolor;
		Slucency = lucency;
		Sshaded = shaded;
		Schoice = choice;
		SdispEdges = dispEdges;
		modfield[0] = color != Sattr.color;
		modfield[1] = Slayer != Sattr.layer;
		modfield[2] = Spen != Sattr.pen;
		modfield[3] = style != Sattr.line_style;
		modfield[4] = weight != Sattr.line_weight;
/*
........Output NCL command
*/
		if (Scmd)
		{
			ncl_init_cmdbuf(&cmdbuf);
			if (Srel_num == UA_TEXT_REL)
				ncl_add_token(&cmdbuf,"DRAFT/ANOTE,MODIFY=",NCL_nocomma);
			else
				ncl_add_token(&cmdbuf,"DRAFT/MODIFY=",NCL_nocomma);
			ncl_add_token(&cmdbuf,Slabel,NCL_comma);
			if (modfield[0])
			{
				if (color == -1) strcpy(buf,"COLOR=DEFALT");
				else sprintf(buf,"COLOR=%s",uw_color_name[color]);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			if (modfield[1])
			{
				sprintf(buf,"LAYER=%d",Slayer);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			if (modfield[2])
			{
				sprintf(buf,"PEN=%d",Spen);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			if (modfield[3])
			{
				sprintf(buf,"LINTYP=%s",lines[style-1]);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			if (modfield[4])
			{
				sprintf(buf,"LINWGT=%s",wghts[weight-1]);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			if (changeShaded)
			{
				sprintf(buf,"SHADE=%s",Soffon[shaded]);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			if (changeLucency)
			{
				sprintf(buf,"TRANS=%d",lucency);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			if (changeEcolor || changeEdges)
			{
				if (!dispEdges) sprintf(buf,"EDGE=OFF");
				else 
				{
					if (ecolor == -1 || ecolor == 64) sprintf(buf,"EDGE=DEFALT");
					else sprintf(buf,"EDGE=%s",uw_color_name[ecolor]);
				}
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			if (modMkr)
			{
				switch (Srel_num)
				{
				case NCL_PATERN_REL:
				case UM_POINT_REL:
					sprintf(buf,"MARKER=%s",marks[choice]);
					ncl_add_token(&cmdbuf,buf,NCL_comma);
					break;
				default:
					break;
				}
			}
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			getnln(&Slast_isn);
		}
/*
........Just modify attributes
*/
		else
		{
			nkeys = 0;
/*
........Composite solids must have all
........subcomponent attributes modified
*/
			key = Skey;
			if (Srel_num == UM_SOLID_REL)
			{
				solid.key = Skey;
				uc_retrieve_data(&solid,sizeof(solid));
				if (solid.type == UM_COMPOS_SOLID)
				{
					nkeys = solid.no_netkey;
					kptr = solid.netkey;
				}
			}
			display = UU_FALSE;
			for (i=-1;i<nkeys;i++)
			{
				if (i != -1) key = kptr[i];
				if (modfield[0])
				{
					ncl_update_color(key,color);
					display = UU_TRUE;
				}
				if (modfield[1])
				{
					um_set_layer_num(Slayer);
					um_update_layer(key,Slayer);
					display = UU_TRUE;
				}
				if (modfield[2])
				{
					ur_update_pen(key,Spen);
					display = UU_TRUE;
				}
				if (modfield[3])
				{
					ur_update_line_style(key,style);
					display = UU_TRUE;
				}
				if (modfield[4])
				{
					rweight = weight;
					ur_update_line_weight(key,rweight);
					display = UU_TRUE;
				}
				if (changeEcolor || changeShaded || changeLucency || changeEdges)
				{
					e.key = key;
					status = ncl_retrieve_data_fixed (&e);	
					if (changeShaded) ncl_setent_shaded(&e, shaded);
					if (changeLucency) ncl_setent_lucency(&e, lucency);
					if (changeEcolor || changeEdges)
					{
						tcolor = ecolor;
						if (ecolor == -1) tcolor = 64;
						ncl_setent_edge(&e,dispEdges,tcolor);
					}
					display = UU_TRUE;
				}
			}
/*
.....Point and Pattern entities
*/
			if (modMkr)
			{
				switch (Srel_num)
				{
				case NCL_PATERN_REL:
					pn.key = Skey;
					status = ur_retrieve_data_fixed(&pn);
					pn.markertype = choice + 1;
					ur_update_data_fixed(&pn);
					display = UU_TRUE;
					break;
				case UM_POINT_REL:
					pt.key = Skey;
					status = ur_retrieve_data_fixed(&pt);
					pt.markertype = choice + 1;
					ur_update_data_fixed(&pt);
					display = UU_TRUE;
					break;
				default:
					break;
				}
			}
			if (display)
			{
				e.key = Skey;
				status = uc_retrieve_data(&e,sizeof(e));
				if (status == UU_SUCCESS) uc_display(&e);
			}
		}
/*
.....Store the updated attributes
*/
		Sattr.color = color;
		Sattr.layer = Slayer;
		Sattr.pen = Spen;
		Sattr.line_style = style;
		Sattr.line_weight = weight;
	}
}

/*********************************************************************
**    S_FUNCTION     :  S_fill_lists()
**       Propogates the Source, Call, and Entity Data lists with the
**       data associated with the current selection.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fill_lists()
{
	int i,nent,inp;
	char **cptr;
/*
.....Call and Source lists
*/
	if (Smptr != UU_NULL) inp = Smptr->isn;
	else inp = 0;
	ncl_motisn_call_list(inp,Skey,&Scall_list,&Sisn,Sflag);
	ncl_motisn_source_list(Sisn,&Ssrc_list);
/*
.....Entity data list
*/
	if (Sflag == 1)
	{
		nent = UU_LIST_LENGTH(Slist);
		if (nent > 0)
		{
			ul_ipv_init_list(&Sstat_list,nent);
			cptr = (char **)Slist->data;
			for (i=0;i<nent;i++)
				ul_ipv_put_list(&Sstat_list,cptr[i]);
		}
		else
		{
			ul_ipv_init_list(&Sstat_list,1);
			ul_ipv_put_list(&Sstat_list,"");
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : OnExtSel(fieldno,val,stat)
**       Method called when Extract Attributes button is pressed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnExtSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL cmdreject,status,surf,ptpn;
	int nsels,color,pen,layer;
	UM_PLOCREC pick;
	char rel_name[32];
   struct UC_entitydatabag e;
	struct NCL_fixed_databag sf;
	struct UM_surfattr_rec srfattr,srfattr2;
	struct NCL_nclpt_rec pt;
	struct NCL_patern_rec pn;
/*
.....Take the form down
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Pick geometry for query data display
*/
	um_dl_pldas(UD_DASPCKLOC, UJ_SUPPORT, 16, &pick, 1, &nsels, 1);
	if (nsels <= 0) goto done;
/*
.....Get the key from the pick record
*/
   Skey2 = um_get_pickkey(&pick.pent, 1);
/*
.....Get entity & attributes
*/
	status = UU_TRUE;
   e.key = Skey2;
   if (uc_retrieve_data(&e, sizeof(struct UC_entitydatabag)) != UU_SUCCESS)
      goto done;
   if (uc_retrieve_attr(e.key, &Sattr) != UU_SUCCESS) goto done;	
/*
.....Get surface attributes if one was selected
*/
	switch (Srel_num)
	{
	case UM_AGSRF_REL:
	case UM_RBSPLSRF_REL:
	case NCL_SURF_REL:
	case NCL_MESHSURF_REL:
	case NCL_QUILTSURF_REL:
	case NCL_NETSF_REL:
	case NCL_TRIMSF_REL:
	case NCL_REVSURF_REL:
	case UM_SOLID_REL:
		surf = UU_TRUE;
		ptpn = UU_FALSE;
		break;
	case NCL_PATERN_REL:
	case UM_POINT_REL:
		ptpn = UU_TRUE;
		surf = UU_FALSE;
		break;
	default:
		ptpn = UU_FALSE;
		surf = UU_FALSE;
		break;
	}

	sf.key = Skey;
	if (surf)
	{
		switch (e.rel_num)
		{
		case UM_AGSRF_REL:
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_MESHSURF_REL:
		case NCL_QUILTSURF_REL:
		case NCL_NETSF_REL:
		case NCL_TRIMSF_REL:
		case NCL_REVSURF_REL:
		case UM_SOLID_REL:
			status = uc_retrieve_attr(sf.key,&srfattr2);
			status = uc_retrieve_attr(e.key,&srfattr);
			if (status == UU_SUCCESS)
			{
				changeShaded = srfattr.shaded != srfattr2.shaded;
				changeLucency = srfattr.lucency != srfattr2.lucency;
				changeEcolor = srfattr.ecolor != srfattr2.ecolor;
				ecolor = (changeEcolor)? srfattr.ecolor : srfattr2.ecolor;
				dispEdges = (ecolor >= 0)? 1 : 0;
				if (ecolor == 64) ecolor = -1;
				shaded = (changeShaded)? srfattr.shaded : srfattr2.shaded;
				lucency = (changeLucency)? srfattr.lucency : srfattr2.lucency;
				ud_update_answer(FTRN,(int *)&lucency);
				ud_update_answer(FCLR,(int *)&ecolor);
				ud_update_answer(FEDG,(int *)&dispEdges);
				ud_update_answer(FSHD,(int *)&shaded);
				ud_set_traverse_mask(FCLR,dispEdges);
			}
			break;
		}
	}
	if (ptpn)
	{
		switch (e.rel_num)
		{
		case NCL_PATERN_REL:
			pn.key = e.key;
			status = ur_retrieve_data_fixed(&pn);
			choice = pn.markertype - 1;
			modMkr = 1;
			break;
		case UM_POINT_REL:
			pt.key = e.key;
			status = ur_retrieve_data_fixed(&pt);
			choice = pt.markertype - 1;
			modMkr = 1;
			break;
		default:
			break;
		}
	}
	color = Scolor;
	layer = Slayer;
	pen = Spen;
	if (Sattr.line_style < 1) Sattr.line_style = 1;
	Sstyle = Sattr.line_style = Sattr.line_style - 1;
	if (Sattr.line_weight < 1) Sattr.line_weight = 1;
	Sweight = Sattr.line_weight = Sattr.line_weight - 1;

	ud_update_answer(FCOL,(int *)&Sattr.color);
	ud_update_answer(FLAY,(int *)&Sattr.layer);
	ud_update_answer(FPEN,(int *)&Sattr.pen);
	ud_update_answer(FSTY,(int *)&Sstyle);
	ud_update_answer(FWGT,(int *)&Sweight);

	Sattr.color = color;
	Sattr.pen = pen;
	Sattr.layer = layer;

	ud_form_vis();
/*
.....End of routine
*/
done:;
	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnExTog(fieldno,val,stat)
**       Method called when an Extended Attributes form toggle field 
**       is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnExtTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Process Extended Attributes toggle field
*/
	ud_default_method(fieldno, val, stat);

	switch (*fieldno)
	{
	case 9: /* DispEdges field has one associated input field */ 
		ud_set_traverse_mask(10,dispEdges);
		break;
	}
	return(UD_FLDOK);
}
