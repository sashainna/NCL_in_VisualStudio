/*********************************************************************
**    NAME         :  m2uattrf.c
**       CONTAINS: form manipulation for attributes
**    	int umu_attr_form()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m2uattrf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:49
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "mattr.h"
#include "udfconst.h"
#include "udforms.h"
#include "uhep.h"
#include "udfdata.h"
#include "class.h" 
#include "msrf.h"
#include "nccs.h"
#include "mdpick.h"


static UD_FSTAT modattr();
static UD_FSTAT OnExtSel(), OnExtrSel(), OnExtTog(), OnTog();
static wght_to_tog();
static UD_FSTAT OnCloseExt();

/* Data used to convert from toggle number to line weight */
static UU_REAL tog_to_wght[] = {1.0, 2.0, 3.0, 4.0};
static int mod[13], Sfrm, SfrmExt=1;					/* modified field */
static int changeEcolor,changeShaded,changeLucency,disEdges,editSurface;
static int ecolor,shaded,lucency,dispEdges,Scmd,modMkr,choice;
static int Sisn,Slast_isn;
static UU_KEY_ID Skey;
static struct UC_attributedatabag Sattr;
static int Srel_num;
static UU_LOGICAL mdflg=UU_FALSE;
static int Scolor,Slayer,Spen,Sstyle,Sweight;
static int output = 0;

extern char uw_color_name[64][96];
/*********************************************************************
**    E_FUNCTION     : umu_attr_form(lnumber, color, line_styl,
**												pen, line_width, line_weight, modfield, outp,
**                                  extatts)
**			The current default values for layer, color, line style,
**			pen, and line width are used to initialize the MODIFY
**			ATTRIBUTE form and the user is able to change any of
**			the values. These new values are returned but are NOT
**			used to update the current defaults.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          lnumber						layer number
**          color							color
**          line_styl					line style
**          line_weight					line weight
**          pen							logical pen
**          line_width					line width
**				modfield						array: UU_TRUE => corresponding
**																		attribute is to
**																		be modified
**          extatts                 array: Storage for extended attributes
**			outp:			1: output to NCL command (DRAFT)
**							0: No output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umu_attr_form(lnumber, color, line_styl, pen, line_width, line_weight, modfield, outp, extatts)
	int	*lnumber;							/* attribute layer number */
	int	*color;								/* attribute color */
	int 	*line_styl;							/* attribute line style */
	int	*pen;									/* attribute logical pen number */
	UU_REAL	*line_width;					/* attribute line width */
	UU_REAL  *line_weight;					/* attribute line weight	*/
	UU_LOGICAL	modfield[];					/* specifies if field to be modified */
	int *outp;
	int	extatts[];					      /* extended attributes array */
	{
	static int ln;							/* attribute layer number */
	static int at;							/* attribute color */
	static int al;							/* attribute line style */
	static int alg;						/* attribute line weight */
	static int ap;							/* attribute logical pen number */
	static UU_REAL alw;					/* attribute line width UNUSED */
	int i,status,tchoice;
	UU_LOGICAL modatts;

	/* Array used to call ud_form */
	static int *form_ans[] = {	&mod[0],	&ln,
										&mod[1], &at,
										&mod[2], &al,
										&mod[3], &alg,
										&mod[4], &ap,
										&modMkr, &choice,
										&output, 
										&editSurface};

	/* Data used to convert between form toggle number and line style */
	static int tog_to_al[] = { 1, 2, 3, 4, 5, 6, 7, 8 };
/*	static int al_to_tog[] = { -1, 0, -1, -1, -1, 3, 1, 2 };*/

	/* User defined methods for all the Change/No Change fields */
	static UD_METHOD methods[] = {	modattr, NULL,
												modattr, NULL,
												modattr, NULL,
												modattr, NULL,
												modattr, NULL,
												OnTog,   NULL,
												NULL,
												OnTog,   OnExtSel,
															OnExtrSel};
										  
	/* The 'called' array indicates when user-supplied methods are called.
	 * Bitwise or of:
	 *		1 - Invoked on field entry.
	 *		2 - Invoked on field exit.
	 *		4 - Invoked on field toggle.
	 */
	static char called[] = {	6, 6, 
										6, 6, 
										6, 6, 
										6, 6, 
										6, 6,
										6, 6,
										6,
										6, 6,
										   6};

	/* The 'traverse' array indicates which fields are to be traversed.
	 * One indicates traverse this field, 0 don't traverse. This traverse
	 * mask is changed in the user-supplied method.
	 */
	static char traverse[] = {	1, 0,
										1, 0,
										1, 0,
										1, 0,
										1, 0,
										1, 0,
										1,
										1, 0,
										   1};
										  

	uu_denter(UU_MTRC,(us,"umu_attr_form()"));

	/* Set defaults */
	modMkr = editSurface = changeEcolor = changeLucency = changeShaded = disEdges = 0;
	changeEcolor = changeShaded = changeLucency = 0;
	tchoice = choice = 1;
	ln = ur_get_attrmdl_layer();
	at = ncl_get_attrmdl_color();
	changeEcolor = UU_FALSE;
	changeShaded = UU_FALSE;
	changeLucency = UU_FALSE;
/*
.....The following statement appears to only cause errors when
.....setting line_style attributes.  
	al = al_to_tog[ur_get_attrmdl_line_style()];
.....Removing it from the program appears to only improve 
.....the program.
*/
	ap = ur_get_attrmdl_pen();
	alw = ur_get_attrmdl_line_width();
	alg = wght_to_tog(ur_get_attrmdl_line_weight());
	if(alg == -1)
		{
		uu_dprint(UU_MTRC,(us,"umu_attr_form: Bad weight: assuming 1.0"));
		alg = 0;
		}
	for(i=0; i<5; i++)
		mod[i] = 0;
	uu_dprint(UU_MTRC,(us,
	"initial values layer %d, color %d, line style %d, pen %d, line_weight = %d",
		ln, at, al, ap, alg));

	/* Put up form with above defaults, and traverse it */
	status = ud_form1("chgattr.frm",form_ans,form_ans,methods,called,NULL,traverse );
	if (status==-1)
	   return -1;
	uu_dprint(UU_MTRC,(us,
		"final values layer %d, color %d, line style %d, pen %d, weight = %d",
		ln, at, al, ap, alg));

	/* Return user inputs */
	*lnumber   = ln;
	*color     = at;
	*line_styl = tog_to_al[al];
	*pen       = ap;
	*line_width = alw;
	*line_weight = tog_to_wght[alg];
	Scmd = *outp = output;
	for (i=0; i<9; i++) modfield[i] = (mod[i] == 1);
	for (i=0; i<3; i++) extatts[i] = mod[i+9];
	modfield[5] = changeEcolor;
	modfield[6] = changeLucency;
	modfield[7] = changeShaded;
	modfield[8] = dispEdges;
	modfield[9] = modMkr;
	extatts[0] = (ecolor == -1)? 64 : ecolor;
	extatts[1] = shaded;
	extatts[2] = lucency;
	extatts[3] = choice + 1;
	uu_dprint(UU_MTRC,(us,
		"returning layer %d, color %d, line style %d, pen %d, line width %g, line weight = %g",
		*lnumber, *color, *line_styl, *pen, *line_width, *line_weight));

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    I_FUNCTION :  wght_to_tog() -- convert line weight to toggle index
**    PARAMETERS   
**       INPUT  :  weight - weight to convert
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static wght_to_tog(weight)
UU_REAL weight;
{
	int i;

	for(i=0; i<4; i++)
		if(weight == tog_to_wght[i]) break;

	if(i == 4)
	{
		uu_denter(UU_MTRC,(us,"ERROR:wght_to_tog: invalid weight %g",weight));
		uu_dexit;
		return(-1);
	}
	return(i);
}


/*********************************************************************
**    S_FUNCTION     :  modattr(filedno, val, stat)
**       Method called at each change/nochange toggle field
**			in the modify attributes form. (chgattr.frm)
**    PARAMETERS   
**       INPUT  : 
**          fieldno	Field number being changed.
**          val		Current field value.
**          stat		Field status.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form traverse mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT modattr(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{

	uu_denter(UU_MTRC,(us,"modattr(%d)", *fieldno));

	/* Call the default method.  This changes the toggle display, and
	 * causes the answer field to be updated.
	 */
	ud_default_method(fieldno, val, stat);

	/* Switch traverse mask based on the field that changed.  This code
	 * sets the traverse mask so that input fields associated with
	 * change/nochange toggles are not traversed when the toggles value
	 * is nochange.
	 */

	switch(*fieldno) {
		case 0:	/* Layer number */
			if( mod[0] ==0)
				ud_set_traverse_mask(1, UU_FALSE);
			else
				ud_set_traverse_mask(1, UU_TRUE);
			break;

		case 2:	/* Color */
			if( mod[1] ==0)
				ud_set_traverse_mask(3, UU_FALSE);
			else
				ud_set_traverse_mask(3, UU_TRUE);
			break;

		case 4:	/* Line style */
			if( mod[2] ==0)
				ud_set_traverse_mask(5, UU_FALSE);
			else
				ud_set_traverse_mask(5, UU_TRUE);
			break;

		case 6:	/* Line weight */
			if( mod[3] ==0)
				ud_set_traverse_mask(7, UU_FALSE);
			else
				ud_set_traverse_mask(7, UU_TRUE);
			break;

		case 8:	/* Logical pen */
			if( mod[4] ==0)
				ud_set_traverse_mask(9, UU_FALSE);
			else
				ud_set_traverse_mask(9, UU_TRUE);
			break;

		default:
			uu_dprint(UU_MTRC,(us,"modattr: Bad field number: %d",*fieldno));
	}
	uu_dexit;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnExtSel(fieldno,val,stat)
**       Method called when an Extended Attributes box button is 
**       pressed.
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
/*
.....Set up form fields
*/
	static char traverse[] = {1,0,0, 1,0, 1,0, 1};
	static char display[]  = {1,1,1, 1,1, 1,1, 1};
	static char called[]   = {6,6,6, 6,6, 6,6, 6};

	static UD_METHOD methods[] = {OnExtTog,OnExtTog,UU_NULL, OnExtTog,UU_NULL,
											OnExtTog,UU_NULL,OnCloseExt};
	static int *ans[] = {&changeEcolor, &dispEdges, &ecolor, &changeShaded, &shaded,
								&changeLucency, &lucency};
/*
.....Values Extracted, so they should be displayed and traversed
*/
	if (mdflg) 
	{
		if (changeEcolor)  traverse[1] = 1;
		if (changeShaded)  traverse[4] = 1;
		if (changeLucency) traverse[6] = 1;
		traverse[2] = dispEdges;
	}
	else
	{
		dispEdges = UM_srfattr.edge_disp;
		ecolor = (UM_srfattr.edge_color == 64)? -1: UM_srfattr.edge_color;
		shaded = UM_srfattr.shaded;
		lucency = UM_srfattr.lucency;
	}

/*
.....Get the Form input
*/
	SfrmExt = ud_form_display1("extsrfattr.frm",ans,ans,methods,called,
		display,traverse);
	if (SfrmExt == -1)
	{
		SfrmExt = 0;
		goto done;
	}
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnExtrSel(fieldno,val,stat)
**       Method called when Extract Attributes button is pressed. All
**       general attributes will be extracted as well as surface/solid
**       attributes if the picked entity is a surface/solid.
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
static UD_FSTAT OnExtrSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL cmdreject,status;
	int i,nsels,chkbxs=5;
	UM_PLOCREC pick;
	char rel_name[32];
   struct UC_entitydatabag e;
	struct NCL_fixed_databag sf;
	struct UM_surfattr_rec srfattr;
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
   Skey = um_get_pickkey(&pick.pent, 1);
/*
.....Get entity & attributes
*/
	status = UU_TRUE;
   e.key = Skey;
   if (uc_retrieve_data(&e, sizeof(struct UC_entitydatabag)) != UU_SUCCESS)
      goto done;
   if (uc_retrieve_attr(e.key, &Sattr) != UU_SUCCESS) goto done;	
/*
.....Assign values and update display
*/
	if (Sattr.line_style < 1) Sattr.line_style = 1;
	Sstyle = Sattr.line_style = Sattr.line_style - 1;
	if (Sattr.line_weight < 1) Sattr.line_weight = 1;
	Sweight = Sattr.line_weight = Sattr.line_weight - 1;
	Scolor = Sattr.color;
	Spen = Sattr.pen;
	Slayer = Sattr.layer;

	mdflg = UU_TRUE;

	switch (e.rel_num)
	{
	case UM_AGSRF_REL:
	case NCL_TRIMSF_REL:
	case UM_RBSPLSRF_REL:
	case NCL_SURF_REL:
	case NCL_MESHSURF_REL:
	case NCL_REVSURF_REL:
	case UM_SOLID_REL:
		status = uc_retrieve_attr(e.key,&srfattr);
		if (status == UU_SUCCESS)
		{
			changeEcolor = 1;
			changeLucency = 1;
			changeShaded = 1;
			editSurface = 1;
			dispEdges = (srfattr.ecolor >= 0)? 1 : 0;
			ecolor = srfattr.ecolor;
			if (ecolor == 64) ecolor = -1;
			shaded = srfattr.shaded;
			lucency = srfattr.lucency;
			ud_set_traverse_mask(14,UU_TRUE);
		}
		break;
	case NCL_PATERN_REL:
		pn.key = e.key;
		status = ur_retrieve_data_fixed(&pn);
		choice = pn.markertype - 1;
		modMkr = 1;
		ud_set_traverse_mask(11,UU_TRUE);
		break;
	case UM_POINT_REL:
		pt.key = e.key;
		status = ur_retrieve_data_fixed(&pt);
		choice = pt.markertype - 1;
		modMkr = 1;
		ud_set_traverse_mask(11,UU_TRUE);
		break;
	default:
		mdflg = UU_FALSE;
		break;
	}

	ud_update_answer(1,(int *)&Slayer);
	ud_update_answer(3,(int *)&Scolor);
	ud_update_answer(5,(int *)&Sstyle);
	ud_update_answer(7,(int *)&Sweight);
	ud_update_answer(9,(int *)&Spen);

	for (i=0;i<5;i++) 
	{
		mod[i] = 1;
		ud_set_traverse_mask(2*i+1,UU_TRUE);
	}

	ud_form_vis();
/*
.....End of routine
*/
done:;
	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnCloseExt()
**       Method called when Surface Atributes is closed.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnCloseExt()
{
/*
.....Mark the form as closed
*/
	SfrmExt = 0;
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
	case 0: /* changeEcolor field has two associated input fields */
		ud_setfrm_traverse_mask(SfrmExt,1,changeEcolor);  
		ud_setfrm_traverse_mask(SfrmExt,2,changeEcolor && dispEdges);
		break;
	case 1:  /* DispEdges field has one associated input field */
		ud_setfrm_traverse_mask(SfrmExt,2,dispEdges);  
		break;
	case 3:  /* shaded field has one associated input field */
		ud_setfrm_traverse_mask(SfrmExt,4,changeShaded); 
		break;
	case 5:  /* translucency field has one associated input field */
		ud_setfrm_traverse_mask(SfrmExt,6,changeLucency); 
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnTog(fieldno,val,stat)
**       Method called when an Edit Surface Attributes check box or is
**       Change Marker Type chack box are toggled.
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
static UD_FSTAT OnTog(fieldno, val, stat)
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
	case 10: /* edit surface attributes */
		ud_set_traverse_mask(11,modMkr); 
	case 13: /* edit surface attributes */
		ud_set_traverse_mask(14,editSurface);  
	}
	return(UD_FLDOK);
}
