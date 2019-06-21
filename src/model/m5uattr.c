/*********************************************************************
**    NAME         :  m5uattr.c
**       CONTAINS: user interface routines to change surface attributes.
**           umu_set_surf_uv_paths()
**           umu_srf_modals()
**           umu_shape_modals()
**           umu_set_surf_edges()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m5uattr.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:07
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h" 
#include "mdrel.h"
#include "mdclass.h"
#include "mattr.h"
#include "msrf.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "driver.h"
#include "mgeom.h"

int shape_display_mode = 0;

UU_LOGICAL  ud_gnxt();

static int changeNum;         /* Change/NoChange flags for form */
static int changePts;
static int changeColor,changeEcolor,dispEdges;
static int changeMatrl;
static int changeLucency;
static int changeShaded;
static int Scfl = 0;
static int Scmd = 0;
static UD_LIST Smaterial;

static UD_FSTAT S_modattr_m1();
static UD_FSTAT S_modattr_sh();

extern char uw_color_name[64][96];
/*********************************************************************
**    E_FUNCTION     : umu_set_surf_uv_paths()
**       Prompt the user for surfaces for which to change the 
**       surface attributes.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umu_set_surf_uv_paths()
{
	struct NCL_fixed_databag e;
	struct UM_surfattr_rec srfattr;
	struct UM_agsrf_rec *asrf;
	int numint,nent;
	UU_LOGICAL initialize,outflag,disflag;
	UU_LOGICAL found;
	int status,material,i;
	char buf[80];
	NCL_cmdbuf cmdbuf,cmddis;
	static int numu;
	static int numv;
	static int ptsperu;
	static int ptsperv;
	static int color,ecolor,ecolor1;
	static int shaded;
	static int lucency;
	static UM_tess_settype sav_typ;
	static UU_REAL sav_tol;
	static int sav_kupts,sav_kvpts;
	/* Arrays used to call ud_form */
	static int *form_ans[] = {
		&changeNum, &numv, &numu, 
		&changePts, &ptsperv, &ptsperu, 
		&changeMatrl, (int *)&Smaterial,
		&changeColor, &color,
		&changeEcolor, &dispEdges, &ecolor,
		&changeShaded, &shaded,
		&changeLucency, &lucency,
		&Scmd };

	static UD_METHOD methods[] = {
		S_modattr_m1, NULL, NULL,
		S_modattr_m1, NULL, NULL,
		S_modattr_m1, NULL, 
		S_modattr_m1, NULL,
		S_modattr_m1, S_modattr_m1, NULL,
		S_modattr_m1, NULL,
		S_modattr_m1, NULL,
		UU_NULL };

	/* The 'called' array indicates when my method is to be invoked.
	 * Bitwise or of:
	 *    1 - Invoked on field entry.
	 *    2 - Invoked on field exit.
	 *    4 - Invoked on field toggle.
	 */
	static char called[] = {	6, 6, 6,
								6, 6, 6, 
								6, 6,
								6, 6,
								6, 6, 6,
								6, 6,
								6, 6,
								6 };

	/* The 'traverse' array indicates which fields are to be traversed.
	 * One indicates traverse this field, 0 don't traverse.
	 */
	static char traverse[] = {	1, 0, 0,
								1, 0, 0,
								1, 0,
								1, 0,
								1, 0, 0,
								1, 0,
								1, 0,
								1};

	uu_denter(UU_MTRC,(us,"umu_set_surf_uv_paths()"));

	changeNum   = UU_FALSE;
	changePts   = UU_FALSE;
	changeColor = UU_FALSE;
	changeEcolor = UU_FALSE;
	changeMatrl = UU_FALSE;
	changeShaded = UU_FALSE;
	changeLucency = UU_FALSE;
	numu     = UM_srfattr.numupaths; 
	numv     = UM_srfattr.numvpaths; 
	ptsperu  = UM_srfattr.ptsperucrv;
	ptsperv  = UM_srfattr.ptspervcrv;
	color = -1;

	dispEdges = UM_srfattr.edge_disp;
	ecolor = UM_srfattr.edge_color;

	ecolor1 = -1;
	shaded = UM_srfattr.shaded;
	lucency = UM_srfattr.lucency;
/*
.....Get material names
*/
	material = UM_srfattr.material;
	Smaterial.answer = (char *)uu_malloc(80*sizeof(char));
	Smaterial.item = (char**)um_get_material_names(&Smaterial.num_item);
	strcpy(Smaterial.answer,Smaterial.item[material]);

	/* Put up form with correct defaults, and traverse it */
	status = ud_form1( "srfmattr.frm",form_ans,form_ans,methods,called,NULL,traverse );
	if (status == -1)
		return (status);
	uu_dprint(UU_MTRC,(us,
	   "final values upaths %d, vpaths %d, ptsperu %d, ptsperv %d",
	   numu, numv, ptsperu, ptsperv));
	uu_dprint(UU_MTRC,(us,"color %d, material %d", color, material));

	ud_lgeo(UU_TRUE, UD_ncl_allsfsh);
	ud_ldas(UD_DASSELECT,/*pick entity*/UM_MODEL,223, UU_NULL, 1,
		&numint, UD_NODEFAULT);

	if (!Scmd)
	{
		if (changePts && ncl_setver(96))
		{
			ncl_get_tess_parms (&sav_typ,&sav_tol,&sav_kupts,&sav_kvpts);
			ncl_set_tess_parms (1,0.,ptsperu,ptsperv);
		}
		if (changeEcolor)
		{
			if (dispEdges)
			{
				if (ecolor == -1)
/* need check later, we normally use -1 for default color
.....but for edge color, we used to -1 for not draw edge
.....so there is no number for default color, temp set to 64
.....which is 64(max color)
*/
					ecolor1 = 64;
				else
				{
					ecolor1 = ecolor;
				}
			}
			else
				ecolor1 = -1;
		}
		if( changeMatrl )
		{
			for (i=0;i<Smaterial.num_item;i++)
			{
				if (strcmp(Smaterial.answer,Smaterial.item[i]) == 0)
				{
					material = i;
					break;
				}
			}
		}
	}
/*
.....Initialize loop
*/
	initialize = UU_TRUE;
	outflag = changeColor | changeMatrl | changeShaded | changeLucency |
		changeEcolor;
	disflag = changePts || changeNum;
	nent = 0;
	if (Scmd && outflag)
	{
		ncl_init_cmdbuf(&cmdbuf);
		strcpy(buf,"DRAFT/MODIFY=");
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
	}
/*
.....Loop through entities
*/
	while(ud_gnxt(initialize, UU_NULL, &e.key, 1) == UU_TRUE)
	{
		initialize = UU_FALSE;
		found = UU_FALSE;
		status = ncl_retrieve_data_fixed (&e);
/*
........Output DRAFT command
*/
		if (Scmd && status == UU_SUCCESS)
		{
			ncl_get_label(&e,buf);
			if (outflag)
			{
				ncl_add_token(&cmdbuf,buf,NCL_comma);
				nent++;
				if (nent == 20)
				{
					S_output_cmd(color,Smaterial.answer,shaded,lucency,
						dispEdges,ecolor,&cmdbuf);
					ncl_set_cmdmode(UU_TRUE);
					ncl_call(&cmdbuf);
					ncl_init_cmdbuf(&cmdbuf);
					strcpy(buf,"DRAFT/MODIFY=");
					ncl_add_token(&cmdbuf,buf,NCL_nocomma);
					nent = 0;
				}
			}
			if (disflag)
			{
				ncl_init_cmdbuf(&cmddis);
				ncl_add_token(&cmddis,"DISPLY/",NCL_nocomma);
				ncl_add_token(&cmddis,buf,NCL_comma);
				if (changePts)
					sprintf(buf,"%d,%d,%d,%d",ptsperu,numu,ptsperv,numv);
				else
					sprintf(buf,"%d,%d",numu,numv);
				ncl_add_token(&cmddis,buf,NCL_comma);
				ncl_add_token(&cmddis,"REDRAW",NCL_comma);
				ncl_add_cmdbuf(&cmddis);
				ncl_set_cmdmode(UU_TRUE);
				ncl_call(&cmddis);
			}
		}
/*
........Modify attributes directly
*/
		else if (status == UU_SUCCESS)
		{
			found = UU_TRUE;
			switch (e.rel_num)
			{
			case UM_AGSRF_REL:
				asrf = (struct UM_agsrf_rec *) &e;

				if( changeNum ) {
					asrf->numupaths = numu;
					asrf->numvpaths = numv;
				}  

				if( changePts ) {
					asrf->ptsperucrv = ptsperu;
					asrf->ptspervcrv = ptsperv;
				}

				if( changeMatrl ) asrf->material = material;

				break;

			case NCL_TRIMSF_REL:
			case UM_RBSPLSRF_REL:
			case NCL_SURF_REL:
			case NCL_MESHSURF_REL:
			case NCL_REVSURF_REL:
			case UM_SOLID_REL:
				status = uc_retrieve_attr(e.key,&srfattr);
				if (status == UU_SUCCESS)
				{
					if (changeNum)
					{
						srfattr.numupaths = numu;
						srfattr.numvpaths = numv;
					}  

					if (changePts)
					{
						srfattr.ptsperucrv = ptsperu;
						srfattr.ptspervcrv = ptsperv;
					}

					if (changeColor) srfattr.color = color;
					if (changeMatrl) srfattr.material = material;
					if (changeShaded) srfattr.shaded = shaded;
					if (changeLucency) srfattr.lucency = lucency;
					if (changeEcolor) srfattr.ecolor = ecolor1;

					found = UU_TRUE;
					ur_update_attr(&srfattr);
				}
				break;
			default:
				found = UU_FALSE;

				break;
			}
			if (found)
			{
				if (changeColor ) ncl_update_color(e.key, color);
				if (e.rel_num != UM_SOLID_REL) ur_update_data_fixed(&e);
				if (changePts || changeNum) 
				{
					ncl_lst_delete(DISPLAY_LIST, &e.key);
					ncl_lst_delete(TESSELLATION_LIST, &e.key);
				}
				status = ncl_retrieve_data_fixed(&e);
				uc_display(&e);
			}
		}
	}
/*
.....Output commands
*/
	if (nent != 0)
	{
		outflag = S_output_cmd(color,Smaterial.answer,shaded,lucency,
			dispEdges,ecolor,&cmdbuf);
		if (outflag)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_call(&cmdbuf);
		}
	}
	if (changePts && ncl_setver(96))
		ncl_set_tess_parms (sav_typ,sav_tol,sav_kupts,sav_kvpts);

	ud_lgeo(UU_FALSE, UD_surface);
	ud_free_flist(&Smaterial);
	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  S_modattr_m1(filedno, val, stat)
**       Method called at each change/nochange toggle field
**       in the modify surface attributes form.
**    PARAMETERS   
**       INPUT  : 
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form traverse mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT S_modattr_m1(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	uu_denter(UU_MTRC,(us,"S_modattr_m1(%d)", *fieldno));

	/* Call the default method.  This changes the toggle display, and
	 * causes the answer field to be updated.
	 */
	ud_default_method(fieldno, val, stat);

	/* Switch traverse mask based on the field that changed.  This code
	 * sets the traverse mask so that input fields associated with
	 * change/nochange toggles are not traversed when the toggles value
	 * is nochange.
	 */

	uu_dprint(UU_MTRC,(us,"Num = %d, Pts = %d, Matrl = %d, Color = %d",
		changeNum,changePts,changeMatrl,changeColor));
	switch(*fieldno) {
		case 0:  /* ChangeNum field has two associated input fields */
			ud_set_traverse_mask(1, changeNum); /* ChangeNum is traverse flag */
			ud_set_traverse_mask(2, changeNum);
			break;

		case 3:  /* ChangePts field has two associated input fields */
			ud_set_traverse_mask(4, changePts); /* ChangePts is traverse flag */
			ud_set_traverse_mask(5, changePts);
			break;

		case 6:  /* ChangeMatrl field has one associated input field */
			ud_set_traverse_mask(7, changeMatrl);  
/* ChangeMatrl is traverse flag */
			break;

		case 8:  /* ChangeColor field has one associated input field */
			ud_set_traverse_mask(9, changeColor);  
/* ChangeColor is traverse flag */
			break;

		case 10:  /* changeEcolor field has two associated input fields */
			ud_set_traverse_mask(11, changeEcolor);  
			ud_set_traverse_mask(12, changeEcolor && dispEdges);  
			break;
		case 11:  /* DispEdges field has one associated input field */
			ud_set_traverse_mask(12, dispEdges);  
			break;

		case 13:  /* shaded field has one associated input field */
			ud_set_traverse_mask(14, changeShaded); 
			break;

		case 15:  /* translucency field has one associated input field */
			ud_set_traverse_mask(16, changeLucency); 
			break;

		default:
			uu_dprint(UU_MTRC,(us,"S_modattr_m1: Bad field number: %d",*fieldno));
	}
	uu_dexit;
	return (UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  S_output_cmd(color,material,shaded,lucency,
**                         dispEdges,ecolor,cmdbuf);
**       Outputs the DRAFT/MODIFY=sf command.
**    PARAMETERS   
**       INPUT  : 
**          color      Color of entity.
**          material   Material of entity.
**          shaded     UU_TRUE = Shaded.
**          lucency    Translucency of entity.
**          dispEdges  UU_TRUE = display surface edges.
**          ecolor     Edge color.
**          cmdbuf     Active command buffer.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form traverse mask.
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_output_cmd(color,material,shaded,lucency,dispEdges,
	ecolor,cmdbuf)
int color;
char *material;
int shaded,lucency;
UU_LOGICAL dispEdges;
int ecolor;
NCL_cmdbuf *cmdbuf;
{
	int i, outflag;
	char buf[80];
	char Scolor[65][96];
	static char *Soffon[]={"OFF","ON"};

	strcpy(Scolor[0], "DEFALT");
	for (i=0; i<64;i++)
	{
		sprintf(Scolor[i+1], "%s", uw_color_name[i]);
	}
/*
.....Initialize routine
*/
	outflag = UU_FALSE;
/*
.....Format attribute settings
*/
	if (changeColor)
	{
		sprintf(buf,"COLOR=%s",Scolor[color+1]);
		ncl_add_token(cmdbuf,buf,NCL_comma);
		outflag = UU_TRUE;
	}
	if (changeMatrl)
	{
		sprintf(buf,"MATERL=\"%s\"",material);
		ncl_add_token(cmdbuf,buf,NCL_comma);
		outflag = UU_TRUE;
	}
	if (changeShaded)
	{
		sprintf(buf,"SHADE=%s",Soffon[shaded]);
		ncl_add_token(cmdbuf,buf,NCL_comma);
		outflag = UU_TRUE;
	}
	if (changeLucency)
	{
		sprintf(buf,"TRANS=%d",lucency);
		ncl_add_token(cmdbuf,buf,NCL_comma);
		outflag = UU_TRUE;
	}
	if (changeEcolor)
	{
		if (!dispEdges)
			sprintf(buf,"EDGE=OFF");
		else
		{
			sprintf(buf,"EDGE=%s",Scolor[ecolor+1]);
		}
		ncl_add_token(cmdbuf,buf,NCL_comma);
		outflag = UU_TRUE;
	}
	if (outflag) ncl_add_cmdbuf(cmdbuf);
/*
.....End of routine
*/
	return(outflag);
}

/*********************************************************************
**    E_FUNCTION     : umu_srf_modals()
**      Change surface display modals with a form.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umu_srf_modals()
{

	/* Array used to call ud_form */
	static int drive,sdrive, bowtie,sbowtie;
	static int skupts,skvpts;
	static int edge_disp,edge_color,shaded,lucency;

	UM_int2 ifl,val;
	NCL_cmdbuf cmdbuf;
	int stat,status,material,i;
	static char vp_scalar[65], up_scalar[65], vpt_scalar[65], upt_scalar[65];
	UU_REAL value;

	static int *form_ans[] = {
		(int*)vp_scalar, (int*)up_scalar, (int*)vpt_scalar, (int*)upt_scalar,
		(int*)&Smaterial, (int*)&edge_disp, (int*)&edge_color,
		(int*)&shaded, (int*)&lucency,
		(int*)&drive, (int*)&bowtie };

	uu_denter( UU_MTRC,(us,"umu_srf_modals()"));
/*
.....Set drive type flag
.....Bobby  -  5/12/98
*/
	ifl = 340;
	getifl(&ifl,&val);
	drive = val;
	sdrive = drive;
	ifl = 346;
	getifl(&ifl,&val);
	bowtie = val;
	sbowtie = bowtie;
	skupts = UM_srfattr.ptsperucrv;
	skvpts = UM_srfattr.ptspervcrv;

	sprintf (vp_scalar, "%d", UM_srfattr.numvpaths);
	sprintf (up_scalar, "%d", UM_srfattr.numupaths);
	sprintf (vpt_scalar, "%d", UM_srfattr.ptspervcrv);
	sprintf (upt_scalar, "%d", UM_srfattr.ptsperucrv);

	edge_disp = UM_srfattr.edge_disp;
	if (UM_srfattr.edge_color==64)
		edge_color = -1;
	else
		edge_color = UM_srfattr.edge_color;

	shaded = UM_srfattr.shaded;
	lucency = UM_srfattr.lucency;

/*
.....Get material names
*/
	material = UM_srfattr.material;
	Smaterial.answer = (char *)uu_malloc(80*sizeof(char));
	Smaterial.item = (char**)um_get_material_names(&Smaterial.num_item);
	strcpy(Smaterial.answer,Smaterial.item[material]);

	/* Put up form with correct defaults, and traverse it */

	status = ud_form( "srfattr.frm", form_ans, form_ans );
	if (status == -1) return (-1);
/*
.....Added drive type for trimmed surfaces
.....Bobby  -  5/12/98
*/
	if (drive != sdrive)
	{
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf,NCL_set1,NCL_nocomma);
		ncl_add_token(&cmdbuf,"TRIMMED",NCL_comma);
		if (drive == 0) ncl_add_token(&cmdbuf,NCL_defalt,NCL_nocomma);
		else if (drive == 1) ncl_add_token(&cmdbuf,NCL_face,NCL_nocomma);
		else ncl_add_token(&cmdbuf,NCL_base,NCL_nocomma);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
/*
.....Added button to set/reset checking for "bowtie" surfaces
.....Eduard  -  2/19/99
*/
	if (bowtie != sbowtie)
	{
		ncl_init_cmdbuf(&cmdbuf);
		if (bowtie == 0) ncl_add_token(&cmdbuf,NCL_set,NCL_nocomma);
		else ncl_add_token(&cmdbuf,NCL_reset,NCL_nocomma);
		ncl_add_token(&cmdbuf,"SCHECK",NCL_nocomma);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}

	stat = ncl_get_scalar_value(vp_scalar, &value);
	UM_srfattr.numvpaths = (int)value;
	stat = ncl_get_scalar_value(up_scalar, &value);
	UM_srfattr.numupaths = (int)value;
	stat = ncl_get_scalar_value(vpt_scalar, &value);
	UM_srfattr.ptspervcrv = (int)value;
	stat = ncl_get_scalar_value(upt_scalar, &value);
	UM_srfattr.ptsperucrv = (int)value;
	if (skupts != UM_srfattr.ptsperucrv || skvpts != UM_srfattr.ptspervcrv)
	ncl_set_tess_parms (1,0.,UM_srfattr.ptsperucrv,UM_srfattr.ptspervcrv);

	UM_srfattr.edge_disp = edge_disp;
	if ((edge_color==-1)&&(edge_disp))
		UM_srfattr.edge_color = 64;
	else
		UM_srfattr.edge_color = edge_color;

	UM_srfattr.shaded = shaded;
	UM_srfattr.lucency = lucency;
/*
.....Set material
*/
	for (i=0;i<Smaterial.num_item;i++)
	{
		if (strcmp(Smaterial.answer,Smaterial.item[i]) == 0)
		{
			UM_srfattr.material = i;
			break;
		}
	}

	S_save_modfile();

	uu_dprint(UU_MTRC,(us,
		"final values upaths %d, vpaths %d, ptsperu %d, ptsperv %d, material %d",
		UM_srfattr.numupaths, UM_srfattr.numvpaths, 
		UM_srfattr.ptsperucrv, UM_srfattr.ptspervcrv,
		UM_srfattr.material));

	uu_dexit;

	ud_free_flist(&Smaterial);
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : S_save_modfile
**       Save the surface attributes modals properties into modals file.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_modfile()
{
	int i, stat;
	char msg[80];
	UX_pathname fname;
	FILE *fptr;

	static char Secolor[65][96];
	/*
.....changes to array of 10 chars instead of 4 for end of string
*/
   static char yesno[2][10] = {"*NO","*YES"};
/*
.....Initialize routine
*/
	strcpy(Secolor[0], "*DEFAULT");
	for (i=0; i<64;i++)
	{
		sprintf(Secolor[i+1], "*%s", uw_color_name[i]);
	}
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy(fname,"ncl_srfattr.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,fname,
		3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;
/*
.....Store modals
*/
	ux_fputs0("#SRFATTR#\n", fptr);

	sprintf(msg,"/U_PATHS/ %d\n",UM_srfattr.numupaths);
	ux_fputs0(msg, fptr);
	sprintf(msg,"/U_PTS/ %d\n",UM_srfattr.ptsperucrv);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/V_PATHS/ %d\n",UM_srfattr.numvpaths);
	ux_fputs0(msg, fptr);
	sprintf(msg,"/V_PTS/ %d\n",UM_srfattr.ptspervcrv);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/MATERIAL/ %s\n",Smaterial.item[UM_srfattr.material]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/EDGE_DISPLAY/ %s\n",yesno[UM_srfattr.edge_disp]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/EDGE_COLOR/ %s\n",Secolor[UM_srfattr.edge_color+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SHADED/ %s\n",yesno[UM_srfattr.shaded]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/TRANSLUCENCY/ %d\n",UM_srfattr.lucency);
	ux_fputs0(msg, fptr);
/*
.....Close modals file
*/
   ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
   return(stat);
}

/*********************************************************************
**    E_FUNCTION     : umu_shape_modals()
**      Change shape display modals with a form.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umu_shape_modals()
{
	int status;
	UM_int2 iflag;
	UM_int2 ifl,val;
/*
.....Moving the display mode button to the top of the form.
*/
	static int *form_ans[] = { &Scfl, 
										&UM_srfattr.numvpaths,
										&UM_srfattr.numupaths,
										&UM_srfattr.ptspervcrv,
										&UM_srfattr.ptsperucrv };

	static UD_METHOD methods[] = {S_modattr_sh, NULL, NULL, NULL, NULL};
	static char called[] = {6,6,6,6,6};
	static char traverse[] = {1,0,0,0,0};
	ifl = 344;
	getifl(&ifl,&val);
	Scfl = val;
/*
.....The display mode was previously set to either
.....wireframe or shaded, change traverse so that
.....the fields for numupaths, numvpaths, etc, aren't
.....dimmed.  JLS 9/30/99
*/
	if(Scfl>=1)
	{
		traverse[0] = 1;
		traverse[1] = 1;
		traverse[2] = 1;
		traverse[3] = 1;
		traverse[4] = 1;
	}
	status = ud_form1( "shapattr.frm", form_ans, form_ans,methods,called,
					NULL, traverse);
	if (status==-1)
		return (status);
/*
.....Scfl = 0 indicates a 2-D shape.
.....Scfl = 1 indicates a wireframe shape.
.....Scfl = 2 indicates a shaded shape.
*/
	iflag = Scfl;
	setflg(&iflag);
	shape_display_mode = Scfl;
/*
.....Since traverse is a static variable, make sure
.....that the values are set back to the original
*/
	traverse[0] = 1;
	traverse[1] = 0;
	traverse[2] = 0;
	traverse[3] = 0;
	traverse[4] = 0;

	return (status);
}

/*********************************************************************
**
**     FUNCTION  S_modattr_sh
**
**     PURPOSE:  The call back function for the 
**               Display mode button on the shape form.
**               It either dims or undims the fields
**               for numupaths, numvpaths, ptsperucrv
**               and ptspervcrv.
**
*********************************************************************/
static UD_FSTAT S_modattr_sh()
{
/*
.....User selected either wireframe or shaded shape,
.....so undim the fields.  
*/
	if(Scfl>0)
	{
		ud_set_traverse_mask(1,1);
		ud_set_traverse_mask(2,1);
		ud_set_traverse_mask(3,1);
		ud_set_traverse_mask(4,1);
	}
/*
.....User selected a 2-D shape, so dim the fields
*/
	else
	{
		ud_set_traverse_mask(1,0);
		ud_set_traverse_mask(2,0);
		ud_set_traverse_mask(3,0);
		ud_set_traverse_mask(4,0);
	}
	return (UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : umu_set_surf_edges(flag)
**      Sets the surface edge display flag for all defined surfaces
**      in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          flag    = UU_TRUE = Turn on all surface edges with black
**                    color.  UU_FALSE = Turn off all surface edges.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_set_surf_edges(flag)
UU_LOGICAL flag;
{
	int status,ietype;
	struct NCL_fixed_databag ent;
/*
.....Loop through Unibase looking for surfaces
*/
	vxlfst();
	uv_set_defered_mode();
	while (UU_TRUE)
	{
		if (!ncl_vxlnxt(&ent.key,&ietype)) break;
/*
........Modify surface edge attribute
*/
		if (ietype == NCLI_SURF || ietype == NCLI_SOLID)
		{
			status = ncl_retrieve_data_fixed(&ent);
			if (status == UU_SUCCESS)
			{
				if (!flag) ncl_setent_edge(&ent,UU_FALSE,-1);
				else ncl_setent_edge(&ent,UU_TRUE,0);
				uc_display(&ent);
			}
		}
	}
	uv_set_immediate_mode();
}
