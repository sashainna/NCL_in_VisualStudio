/*********************************************************************
**    NAME         :  m2uattr.c
**       CONTAINS: user interface routines to change attributes
**       umu_sea_ent_lnstyl
**       umu_sea_ent_blank
**       umu_swap_ent_blank
**       umu_sea_ent_edit()
**       umu_sea_ent_nedit()
**       umu_sda1_pen_num()
**       umu_sea_ent_pen_num()
**       umu_get_ent_attr()
**       umf_set_ent_attr()
**       umu_get_def_attr()
**       umf_set_def_attr()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m2uattr.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:49
*********************************************************************/

#include "vsegbf.h"
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "udforms.h"
#include "udfdata.h"
#include "dasnog.h"
#include "class.h"
#include "goseg.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdebug.h"
#include "view.h"
#include "nclfc.h"
#include "nccs.h"
#include "lcom.h"
#include "dselect.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nclmodals.h"
#include "msrf.h"
#include "msol.h"

static Stype[27] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

UU_LOGICAL	ud_gnxt();
static UD_FSTAT OnClearAll(),OnSetAll();
extern char uw_color_name[64][96];

/*********************************************************************
**    E_FUNCTION     : umu_sea_ent_lnstyl(lnstyl)
**       Change the line style of any master tuple entity.
**    PARAMETERS   
**       INPUT  : 
**				lnstyl					new line style  for the entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_sea_ent_lnstyl(lnstyl)
	int lnstyl;

	{
	struct UC_entitydatabag e;
	int numint;
	UU_LOGICAL initialize;
	int status;

	uu_denter(UU_MTRC,(us,"umu_sea_ent_lnstyl(%d)",lnstyl));

/*	-- Limit DAS to only editable entities -- */

	ud_leditable(UU_TRUE);
	/* enable DAS to pick all of the entities for which the line style
		attribute is to be modified */
	ud_ldas(UD_DASSELECT,/*pick entity*/UM_MODEL,223, UU_NULL, 1,
		&numint, UD_NODEFAULT);

	/* for each picked entity, change the line style attribute and
		redisplay */
	initialize = UU_TRUE;
	while(ud_gnxt(initialize, UU_NULL, &e.key, 1) == UU_TRUE)
		{
		initialize = UU_FALSE;
		status = ur_update_line_style(e.key,lnstyl);
		if (status == 0) status = uc_retrieve_data(&e, sizeof(e));
		if (status == 0) status = uc_display(&e);
		}

	uu_dexit;
	}


/*********************************************************************
**    E_FUNCTION     : umu_sea_ent_blank()
**       blank/unblank entities.
**    PARAMETERS   
**       INPUT  : 
**          option				0 = Blank filtered
**										1 = Blank all
**										2 = Unblank filtered
**										3 = Unblank all
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_sea_ent_blank(option)
	int option;

	{
	UU_KEY_ID key;                              /* key of retrieved data */
	Gseg dsegid;								/* display segment identifier */
	int status, ur_update_blanked();
	UV_vport	vport;
	int	numint;								/* number of entities picked by DAS */
	UU_LOGICAL initialize;
	UU_LOGICAL blanked;
	int rel_num, stype;
	unsigned long mask[4];					/* Das select mask */

	uu_denter(UU_MTRC,(us,"umu_sea_ent_blank(%d)",option));

	/* Set DAS select mask for all relations other than lights */

	mask[0] = mask[1] = mask[2] = mask[3] = -1;
	uu_clr_bit(mask, UM_LIGHT_REL-1);
	ud_lgeo(UU_TRUE, mask);

	stype = ud_getpick_type();
	switch (option)
		{
		case 0: /* blank filtered */
			{
			/* have user pick entities to blank */
			ud_setpick_type(UD_PICK_NORMAL);
			ud_ldas(UD_DASSELECT, /*pick entities to blank:*/UM_MODEL,
					  259, UU_NULL, 0, &numint,UD_NODEFAULT);
			ud_setpick_type(stype);
			if (numint > 0)
			{
				initialize = UU_TRUE;
				while(ud_gnxt(initialize, UU_NULL, &key, 1) == UU_TRUE)
				{
					initialize = UU_FALSE;

					ur_retrieve_blanked(key, &blanked);
					if (blanked==UU_FALSE)
					{
						if (ur_update_blanked(key, UU_TRUE) == 0)
						{
							ur_retrieve_disp_segid(key, &dsegid);
							uv_blanksegs(dsegid, key);
						}
					}
				}
			}
			break;
			}
		case 1: /* blank all */
			
			/* Select entities in vport */
			ud_alldisp(UU_FALSE, UU_FALSE, UU_FALSE);

			initialize = UU_TRUE;
			while(ud_gnxt(initialize, UU_NULL, &key, 1) == UU_TRUE)
			{
				initialize = UU_FALSE;
				ur_retrieve_blanked(key, &blanked);
				if (blanked==UU_FALSE)
				{
					if (ur_update_blanked(key, UU_TRUE) == 0)
					{
						ur_retrieve_disp_segid(key,&dsegid);
						uv_blanksegs(dsegid, key);
					}
				}
			}
			break;
		
		case 2: /* unblank filtered */
			{

/*			if (uvu_pickvp(&vport) != UU_SUCCESS)
			{
				uu_dexit;
				return;
			}
			uv_getvid(vport.cur_view, &view);
			if (view.vtype!=UV_INVISIBLE_VIEW)
				ud_calsel(UU_TRUE, 1, vport.xform, UU_FALSE);
			else
			{
				ud_setpick_type(UD_PICK_INVISIBLE);
				ud_ldas(UD_DASSELECT, UM_MODEL,
							  346, UU_NULL, 0, &numint,UD_NODEFAULT);
				ud_setpick_type(stype);
				if (numint <= 0)
					return;
			}
*/
			if (uv_invisv_act())
			{
				ud_setpick_type(UD_PICK_INVISIBLE);
				ud_ldas(UD_DASSELECT, UM_MODEL,
							  346, UU_NULL, 0, &numint,UD_NODEFAULT);
				ud_setpick_type(stype);
				if (numint <= 0)
					return;
			}
			else
			{
				if (uvu_pickvp(&vport) != UU_SUCCESS)
				{
					uu_dexit;
					return;
				}
				ud_calsel(UU_TRUE, 1, vport.xform, UU_FALSE);
			}
			initialize = UU_TRUE;
			while(ud_gnxt(initialize, UU_NULL, &key, 1) == UU_TRUE)
			{
				initialize = UU_FALSE;

				ur_retrieve_blanked(key, &blanked);
				if (blanked==UU_TRUE)
				{
					if (ur_update_blanked(key,UU_FALSE) == 0)
					{
						ur_retrieve_disp_segid(key,&dsegid);
						uv_unblanksegs(dsegid, key);
					}
				}
			}
			break;
			}
		case 3: /* unblank all */
			{
			initialize = UU_TRUE;
			while (uv_getobjs(initialize, &key, UU_FALSE) == 0)
			{
				initialize = UU_FALSE;
				ur_retrieve_blanked(key, &blanked);
				if (blanked == UU_TRUE)
				{
					ur_retrieve_data_relnum(key, &rel_num);
					if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
					{
						status = ur_update_blanked(key, UU_FALSE);
						ur_retrieve_disp_segid(key,&dsegid);
						uv_unblanksegs(dsegid, key);
					}
				}
			}
			break;
			}
		}
	uu_dexit;

	}

/*********************************************************************
**
**    E_FUNCTION        :  umu_swap_ent_blank()
**       Open a geometry type form and swap the visible/invisible
**       setting of the selected entity types.
**
**    PARAMETERS
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
umu_swap_ent_blank()
{
	int i,status,mask[UD_NMENTWD],ietype;
	UU_LOGICAL blanked;
	struct NCL_fixed_databag e;
	Gseg dsegid;

	static char traverse[]     = {1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
	static UD_METHOD methods[] = 
		{ UU_NULL,UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, 
			UU_NULL,UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, 
			UU_NULL,UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, 
			UU_NULL,UU_NULL, UU_NULL, UU_NULL, UU_NULL,
			OnClearAll,OnSetAll};
	static char called[] = {6,6,6,6,6,6,6,6,6,6,6,6,6,6,
			6,6,6,6,6,6,6,6,6,6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
						1,1,1,1,1,1,1,1};
	int *ans[28];
/*
.....Set the default answers
*/
	for (i=0; i<26; i++)
		ans[i] = (int *)&(Stype[i]);
	ans[26] = ans[27] = UU_NULL;
/*
.....Get the form input
*/
	status = ud_form1("swapgeo.frm", ans, ans, methods, called,
		display, traverse);
	if (status == -1) goto done;
/*
.....Set the filter mask
*/
	nclu_set_geommask(Stype,mask);
/*
.....Loop through all geometry
*/
	vxlfst();
	while (ncl_vxlnxt(&e.key,&ietype))
	{
		if (e.key == 0 || ietype == 2 || (ietype >= 11 && ietype <= 17) ||
			ietype == 19 || ietype == 22 || ietype == 24) continue;
		ncl_retrieve_data_fixed(&e);
/*
........Swap entity's visibilty
*/
		if (uu_tst_bit(mask,e.rel_num-1))
		{
			ur_retrieve_blanked(e.key,&blanked);
			ur_update_blanked(e.key,!blanked);
			ur_retrieve_disp_segid(e.key,&dsegid);
			if (blanked)
				uv_unblanksegs(dsegid,e.key);
			else
				uv_blanksegs(dsegid,e.key);
		}
	}
/*
.....End of routine
*/
done:;
	return status;
}

/*********************************************************************
**    I_FUNCTION        :  OnClearAll()
**       Clear all geometry type selection
**
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
static UD_FSTAT OnClearAll(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i, type = 0;
	for (i=0; i<26;i++)
	{
		Stype[i] = 0;
		ud_dispfrm_update_answer(0, i,(int *)&Stype[i]);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION        :  OnSetAll()
**       Set all geometry type selection
**
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
static UD_FSTAT OnSetAll(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i, type = 0;
	for (i=0; i<26;i++)
	{
		Stype[i] = 1;
		ud_dispfrm_update_answer(0, i,(int *)&Stype[i]);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : umu_sea_ent_edit()
**       change entities to editable.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_sea_ent_edit()
{
	Gseg dsegid;								/* display segment identifier */
	UU_KEY_ID key;								/* key of retrieved data */
	int ur_update_editability();
	int	numint;								/* number of entities picked by DAS */
	UU_LOGICAL initialize;
	uv_segbuff(buff);							/* define a user data buffer */

	uu_denter(UU_MTRC,(us,"umu_sea_ent_edit()"));

	/* have user pick entities to blank */
	ud_leditable(UU_FALSE);
	ud_ldas(UD_DASSELECT, /*pick entities to make editable:*/UM_MODEL, 
			  329, UU_NULL, 0, &numint,UD_NODEFAULT);
	if (numint > 0)
	{
		initialize = UU_TRUE;
		while(ud_gnxt(initialize, UU_NULL, &key, 1) == UU_TRUE)
		{
			initialize = UU_FALSE;
			if (ur_update_editability(key, UU_TRUE) == 0)
			{
				ur_retrieve_disp_segid(key, &dsegid);
				while (dsegid != -1)
				{
					gsegrud(dsegid, buff);			/* read user data */
					uv_seteditable(buff);			/* make it editable */
					gsegwud(dsegid, buff);			/* write back to segment */
					dsegid = uv_getsegid(buff); 	/* get the next view */
				}
			}
		}
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : umu_sea_ent_nedit()
**       change entities to non-editable.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_sea_ent_nedit()
{
	Gseg dsegid;								/* display segment identifier */
	UU_KEY_ID key;								/* key of retrieved data */
	int ur_update_editability();
	int	numint;								/* number of entities picked by DAS */
	UU_LOGICAL initialize;
	uv_segbuff(buff);							/* define a user data buffer */

	uu_denter(UU_MTRC,(us,"umu_sea_ent_nedit()"));

	/* have user pick entities to blank */
	ud_leditable(UU_FALSE);
	ud_ldas(UD_DASSELECT, /*pick entities to make editable:*/UM_MODEL, 
			  330, UU_NULL, 0, &numint,UD_NODEFAULT);
	if (numint > 0)
	{
		initialize = UU_TRUE;
		while(ud_gnxt(initialize, UU_NULL, &key, 1) == UU_TRUE)
		{
			initialize = UU_FALSE;
			if (ur_update_editability(key, UU_FALSE) == 0)
			{
				ur_retrieve_disp_segid(key, &dsegid);
				while (dsegid != -1)
				{
					gsegrud(dsegid, buff);			/* read user data */
					uv_reseteditable(buff);			/* make it non-editable */
					gsegwud(dsegid, buff);			/* write back to segment */
					dsegid = uv_getsegid(buff); 	/* get the next view */
				}
			}
		}
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : umu_sda1_pen_num()
**       Set the default pen number attribute .
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_sda1_pen_num()
	{
	int num;									/* new pen number */
	int cont;								/* continue until the user gets it right,
													or (s)he wants to quit trying */
	int numint;								/* number of inputs returned from DAS */

	uu_denter(UU_MTRC,(us,"umu_sda1_pen_num()"));
	cont = 1;
	while (cont)
		{
		ud_ldas(UD_DASINT,/*pen number*/UM_MODEL,225,&num,1,
			&numint,UD_NODEFAULT);
		if (numint != 0)
			{
			if (num <= 0 || num >= 257)
				{
				uu_uerror0(/* pen number must be in the range 1 - 256 */
								UM_MODEL,198);
				}
			else
				{
				ur_put_attrmdl_pen(num);
				break;
				}
			}
		else
			break;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_sea_ent_pen_num()
**       Change the pen number of any master tuple entity.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_sea_ent_pen_num()

	{
	struct UC_entitydatabag e;						/* picked entity */
	int numint;
	UU_LOGICAL initialize;							/* initialize get next entity */
	int status;											/* -1 iff error, 0 otherwise */
	int cont;											/* continue */
	int pen;												/* pen number */

	uu_denter(UU_MTRC,(us,"umu_sea_ent_pen_num()"));
	cont = 1;
	while (cont)
		{
		ud_ldas(UD_DASINT,/*pen number*/UM_MODEL,225,&pen,1,
			&numint,UD_NODEFAULT);
		if (numint != 0)
			{
			if (pen <= 0 || pen >= 257)
				{
				uu_uerror0(/* pen number must be in the range 1 - 256 */
								UM_MODEL,198);
				}
			else
				{

/*	-- Limit DAS to only editable entities -- */

				ud_leditable(UU_TRUE);
				ud_ldas(UD_DASSELECT,/*pick entity*/UM_MODEL,223,
							UU_NULL,1,&numint,UD_NODEFAULT);
				initialize = UU_TRUE;
				while(ud_gnxt(initialize, UU_NULL, &e.key, 1) == UU_TRUE)
					{
					initialize = UU_FALSE;
					status = ur_update_pen(e.key, pen);
					if (status == 0)
						{
						uc_retrieve_data(&e, sizeof(e));
						uc_display(&e);
						}
					}
				}
			}
		else
			break ;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_get_ent_attr()
**       Get the new entity attributes
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_get_ent_attr()
	{
	int	i, layer;								/* attribute layer number */
	int	color;								/* attribute color */
	int 	line_styl;							/* attribute line style */
	int	pen;									/* attribute logical pen number */
	UU_REAL	line_width;						/* attribute line width */
	UU_REAL line_weight;
	int 	rel_num;
	int	numint;
	int 	status;
	struct UC_entitydatabag e,e1;				/* entity picked */
	struct  UC_attributedatabag attr;  
	int output, modify, extatts[4];
	char buf[256];
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL outmod, found, lstatus, initialize, modfield[10],display;	
	struct NCL_patern_rec pn;
	struct NCL_nclpt_rec pt;
	struct UM_surfattr_rec srfattr;
	UU_LOGICAL writecmd;
	int nkeys;
	UU_KEY_ID *kptr,key;
	struct UM_solid_rec *solid;
	static char *Soffon[]={"OFF","ON"};
	static char *marks[] = {"DOT","PLUS","STAR","CIRCLE","CROSS",
									"TRIAN","DIMOND","SQUARE","DBLCIR",
									"LRGDOT","CUBE"};
	static char *lines[] = {"SOLID","DASH","DOTTED","CENTER","PHANTM",
									"DASHLN","DASHDT","DASHSP"};
	static char *wghts[] = {"STD","MEDIUM","HEAVY","EXHVY"};

	uu_denter(UU_MTRC,(us,"umu_get_ent_attr()"));

	/* set up defaults */
	outmod = 0;
	output = 0;
	status = umu_attr_form(&layer, &color, &line_styl, &pen, &line_width,
						&line_weight, modfield, &output, extatts);
	if (status==-1)
		return;
	modify = 0;	
	for (i=0; i<10;i++)
	{
		if (modfield[i]) modify = 1;
	}
	/* create a new layer record (without a name) if needed */
	if (modfield[0]) um_set_layer_num(layer);

/*	-- Limit DAS to only editable entities -- */

	ud_leditable(UU_TRUE);

	ud_ldas(UD_DASSELECT,/*pick entity*/UM_MODEL,223, UU_NULL,1,
		&numint,UD_NODEFAULT);
	lstatus = ud_gnxt(UU_TRUE, UU_NULL, &e.key, 1);
	outmod = output && modify;
	while (lstatus == UU_TRUE)
	{
		if (outmod)
		{
			ncl_init_cmdbuf(&cmdbuf);
			strcpy(buf,"DRAFT/MODIFY=");	
			ncl_add_token(&cmdbuf, buf, NCL_nocomma);
		}

/*       
.....get names of picked geometry to modify 
*/
		found = UU_FALSE;
		initialize = UU_TRUE;
		display = UU_FALSE;
		while((lstatus == UU_TRUE)&&(!outmod || cmdbuf.num_cmd < 3))
		{
			initialize = UU_FALSE;
			uc_retrieve_data(&e, sizeof(e));
			found = UU_TRUE;
			nkeys = 0;
			key = e.key;
			if (e.rel_num == UM_SOLID_REL)
			{
				solid = (struct UM_solid_rec *)&e;
				if (solid->type == UM_COMPOS_SOLID)
				{
					nkeys = solid->no_netkey;
					kptr = solid->netkey;
				}
			}

			if (outmod)
			{
				ncl_get_label(&e, buf);
				ncl_add_token(&cmdbuf, buf, NCL_comma);
			}
/*
.....Loop through component entities
*/
			for (i=-1;i<nkeys;i++)
			{
				if (i != -1) key = kptr[i];
			/*	update line style	*/
				if (modfield[2])
				{
					status = ur_update_line_style(key,line_styl);
					display = UU_TRUE;
				}
			/*	update line weight	*/
				if (modfield[3])
				{
					status = ur_update_line_weight(key,line_weight);
					display = UU_TRUE;
				}
			/*	update color	*/
				if (modfield[1])
				{
					status = ncl_update_color(key,color);
					ur_retrieve_data_relnum(e.key, &rel_num);
					if (rel_num == UM_BODY_REL)
						um_set_rom_color(&e, color);
					display = UU_TRUE;
				}
			/* update pen number	*/
				if (modfield[4])
					status = ur_update_pen(key,pen);
			/* update layer	*/
				if (modfield[0])
					status = um_update_layer(key,layer);
			/* update surface attributes if applicable */
				if (modfield[5] || modfield[6] || modfield[7] || modfield[8])
				{
					e1.key = key;
					status = ncl_retrieve_data_fixed (&e1);	
					if (modfield[7]) ncl_setent_shaded(&e1, extatts[1]);
					if (modfield[6]) ncl_setent_lucency(&e1, extatts[2]);
					if (modfield[5]) ncl_setent_edge(&e1,modfield[8],extatts[0]);
					display = UU_TRUE;
				}
			/* update point marker if applicable */
				if (modfield[9])
				{
					switch (e.rel_num)
					{
					case NCL_PATERN_REL:
						pn.key = e.key;
						status = ur_retrieve_data_fixed(&pn);
						pn.markertype = extatts[3];
						ur_update_data_fixed(&pn);
						display = UU_TRUE;
						break;
					case UM_POINT_REL:
						pt.key = e.key;
						status = ur_retrieve_data_fixed(&pt);
						pt.markertype = extatts[3];
						ur_update_data_fixed(&pt);
						display = UU_TRUE;
						break;
					default:
					break;
					}
				}
			}
			if (display && status == 0)
			{
				uc_retrieve_data(&e, sizeof(e));
/*
..... If label display is on
*/
				attr.key = e.key;
				status = ur_retrieve_attr(&attr);
/*
..... do bit operation to check if label display is on
*/
				if (status == UU_SUCCESS && (ncl_get_label_on(attr.label_on)))
				{
					if (UW_label_clr==0)
					{
						gstextcolor(attr.color);
					}
					else
						gstextcolor(UW_label_clr);
				}
				uc_display(&e);
			}
			lstatus = ud_gnxt(UU_FALSE, UU_NULL, &e.key, 1);
		}
		if (outmod  && (found == UU_TRUE))
		{
			/*	update line style	*/
			if (modfield[2])
			{
				sprintf (buf, "LINTYP=%s", lines[line_styl-1]);
				ncl_add_token(&cmdbuf, buf, NCL_comma);
			}
			/*	update line weight	*/
			if (modfield[3])
			{
				i = line_weight-1;
				sprintf (buf, "LINWGT=%s", wghts[i]);
				ncl_add_token(&cmdbuf, buf, NCL_comma);
			}
			/*	update color	*/
			if (modfield[1])
			{
				if (color == -1) strcpy(buf,"COLOR=DEFALT");
				else sprintf(buf,"COLOR=%s",uw_color_name[color]);
				ncl_add_token(&cmdbuf, buf, NCL_comma);
			}
			/* update pen number	*/
			if (modfield[4])
			{
				sprintf (buf, "PEN=%d", pen);
				ncl_add_token(&cmdbuf, buf, NCL_comma);
			}
			/* update layer	*/
			if (modfield[0])
			{
				sprintf (buf, "LAYER=%d", layer);
				ncl_add_token(&cmdbuf, buf, NCL_comma);
			}
			if (modfield[5] || modfield[6] || modfield[7] || modfield[8] || modfield[9])
			{
				if (modfield[5] || modfield[6] || modfield[7] || modfield[8])
				{
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
							if (modfield[5]) 
							{
								writecmd = UU_FALSE;
								if (!modfield[8]) sprintf(buf,"EDGE=OFF");
								else
								{
									if (extatts[0] > -1 && extatts[0] < 64)
										sprintf(buf,"EDGE=%s",uw_color_name[extatts[0]]);
									else
										sprintf(buf,"EDGE=DEFALT");
								}
								ncl_add_token(&cmdbuf,buf,NCL_comma);
							}
							if (modfield[6])
							{
								sprintf(buf,"TRANS=%d",extatts[2]);
								ncl_add_token(&cmdbuf,buf,NCL_comma);
							}
							if (modfield[7])
							{
								sprintf(buf,"SHADE=%s",Soffon[extatts[1]]);
								ncl_add_token(&cmdbuf,buf,NCL_comma);
							}
						}
						break;
					default:
						break;
					}
				}
				/* update point marker if applicable */
				if (modfield[9])
				{
					sprintf(buf,"MARKER=%s",marks[extatts[3]-1]);
					ncl_add_token(&cmdbuf,buf,NCL_comma);
				}
			}
			ncl_del_token(&cmdbuf,"", UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_set_cmdmode(UU_TRUE);
			ncl_call(&cmdbuf);
		}
		if (lstatus != UU_TRUE) break;
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : umf_set_ent_attr()
**       Sets the new attributes for a geometry entity.  Fortran callable
**       routine.
**    PARAMETERS
**       INPUT  :
**          newcolor   = Entity color.
**          newpen     = Entity pen.
**          newlintyp  = Entity line type.
**          newlinwgt  = Entity line weight.
**          newlayer   = Entity latyer.
**          newshd     = Surface shade flag.
**          newtrans   = Surface translucency.
**          newmat     = Surface material.
**          newedg     = Surface edge setting/color.
**          newmkr     = Point marker type.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umf_set_ent_attr(nclkey,newcolor,newpen,newlintyp,newlinwgt,newlayer,
	newshade,newtrans,newmat,newedg,newmkr)
int *nclkey,*newcolor,*newpen,*newlintyp,*newlayer;
int *newshade, *newtrans,*newmat,*newedg,*newmkr;
UU_REAL *newlinwgt;
{
	int     rel_num,i,nkeys;
	int     status;
	UU_LOGICAL display;
	UU_KEY_ID key,*kptr;
	struct UC_entitydatabag e;              /* entity picked */
	struct NCL_fixed_databag ent;
	struct NCL_patern_rec pn;
	struct NCL_nclpt_rec pt;
	struct UM_solid_rec solid;
 
	display = UU_FALSE;

	/* create a new layer record (without a name) if needed */
	if (*newlayer >= 0) um_set_layer_num(*newlayer);
 
/*
.....Composite solids must have all
.....subcomponent attributes modified
*/
	nkeys = 0;
	key = *nclkey;
	ur_retrieve_data_relnum(key,&rel_num);
	if (rel_num == UM_SOLID_REL)
	{
		solid.key = key;
		uc_retrieve_data(&solid,sizeof(solid));
		if (solid.type == UM_COMPOS_SOLID)
		{
			nkeys = solid.no_netkey;
			kptr = solid.netkey;
		}
	}
	for (i=-1;i<nkeys;i++)
	{
		if (i != -1) key = kptr[i];
	/*  update line style   */
		if (*newlintyp > 0)
		{
			status = ur_update_line_style(key,*newlintyp);
			display = UU_TRUE;
		}
 
	/*  update line weight  */
		if (*newlinwgt > 0)
		{
			status = ur_update_line_weight(key,*newlinwgt);
			display = UU_TRUE;
		}
 
	/*  update color    */
		if (*newcolor >= -1)
		{
			e.key = key;
			status = ncl_update_color(key,*newcolor);
			ur_retrieve_data_relnum(key, &rel_num);
			if (rel_num == UM_BODY_REL)
				um_set_rom_color(&e, *newcolor);
			display = UU_TRUE;
		}

	/* update pen number    */
		if (*newpen > 0) status = ur_update_pen(key,*newpen);

	/* update layer */
		if (*newlayer >= 0) status = um_update_layer(key,*newlayer);
/*
.....Update surface attributes
*/
		if (*newshade >= 0 || *newtrans >= 0 || *newmat >= 0 || *newedg >= 0)
		{
			ent.key = key;
			status = ncl_retrieve_data_fixed (&ent);	
			if (*newshade >= 0)
				ncl_setent_shaded(&ent, *newshade);
			if (*newtrans >= 0)
				ncl_setent_lucency(&ent, *newtrans);		
			if (*newmat >= 0)
				ncl_setent_material(&ent, *newmat);
	
			if (*newedg == 0)
/*
......off
*/
				ncl_setent_edge(&ent,UU_FALSE,-1);
			else if (*newedg == 1)
/*
......default color, use 64 (max color) for default
*/
				ncl_setent_edge(&ent,UU_TRUE,64);
			else if (*newedg > 0)
				ncl_setent_edge(&ent,UU_TRUE, *newedg-2);
			display = UU_TRUE;
		}
	}

	if (*newmkr > 0)
	{
		ur_retrieve_data_relnum(*nclkey, &rel_num);
		switch (rel_num)
		{
		case NCL_PATERN_REL:
			pn.key = *nclkey;
			status = ur_retrieve_data_fixed(&pn);
			pn.markertype = *newmkr;
			ur_update_data_fixed(&pn);
			display = UU_TRUE;
		case UM_POINT_REL:
			pt.key = *nclkey;
			status = ur_retrieve_data_fixed(&pt);
			pt.markertype = *newmkr;
			ur_update_data_fixed(&pt);
			display = UU_TRUE;
			break;
		default:
			break;
		}
	}

	if (status == 0 && display)
	{
		e.key = *nclkey;
		uc_retrieve_data(&e, sizeof(e));
		uc_display(&e);
	}
}

/*********************************************************************
**    E_FUNCTION     : umu_get_def_attr()
**       Get new default attributes for layer, color, line style,
**			line width, and pen number, and update the modal tables in UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_get_def_attr()
	{
	NCL_cmdbuf cmdbuf;
	int	i, layer;								/* attribute layer number */
	int	color;								/* attribute color */
	int 	line_styl;							/* attribute line style */
	int	pen, status;									/* attribute logical pen number */
	UU_REAL	line_width;						/* attribute line width */
	UU_REAL  line_weight;					/* attribute line weight */
	UU_LOGICAL modfield[10];
	int output, modify, extatts[4];
	char buf[256];
	char Scolor[65][96];
	static char *Soffon[]={"OFF","ON"};
	static char *marks[] = {"DOT","PLUS","STAR","CIRCLE","CROSS",
									"TRIAN","DIMOND","SQUARE","DBLCIR",
									"LRGDOT","CUBE"};
	static char *lines[] = {"SOLID","DASH","DOTTED","CENTER","PHANTM",
									"DASHLN","DASHDT","DASHSP"};
	static char *wghts[] = {"STD","MEDIUM","HEAVY","EXHVY"};

	uu_denter(UU_MTRC,(us,"umu_get_def_attr()"));

	strcpy(Scolor[0], "DEFALT");
	for (i=0; i<64;i++)
	{
		sprintf(Scolor[i+1], "%s", uw_color_name[i]);
	}
	/* set up defaults */
	output = 0;
	status = umu_attr_form(&layer, &color, &line_styl, &pen, &line_width,
						&line_weight, modfield, &output, extatts);
	if (status==-1)
		return;
	if (extatts[0] == 64) extatts[0] = -1;
	modify = 0;	
	for (i=0; i<10;i++)
	{
		if (modfield[i]) modify = 1;
	}			
	if ((output) && (modify))
	{
		ncl_init_cmdbuf(&cmdbuf);
		strcpy(buf,"DRAFT/MODIFY");	
		ncl_add_token(&cmdbuf, buf, NCL_comma);
	}
	if (modfield[0]) 
	{
		um_set_layer_num(layer);
		um_set_active_layer(layer);
		if (output)
		{
			sprintf (buf, "LAYER=%d", layer);
			ncl_add_token(&cmdbuf, buf, NCL_comma);
		}
	}
	if (modfield[1]) 
	{
		ncl_set_color_attr(color); 
		if (output)
		{
			sprintf (buf, "COLOR=%s", Scolor[color+1]);
			ncl_add_token(&cmdbuf, buf, NCL_comma);
		}
	}
	if (modfield[2]) 
	{
		ur_put_attrmdl_line_style(line_styl);
		if (output)
		{
			sprintf (buf, "LINTYP=%s", lines[line_styl-1]);
			ncl_add_token(&cmdbuf, buf, NCL_comma);
		}
	}
	if (modfield[3]) 
	{
		ur_put_attrmdl_line_weight(line_weight);
		if (output)
		{
			i = line_weight-1;
			sprintf (buf, "LINWGT=%s", wghts[i]);
			ncl_add_token(&cmdbuf, buf, NCL_comma);
		}
	}
	if (modfield[4]) 
	{
		ur_put_attrmdl_pen(pen);
		if (output)
		{
			sprintf (buf, "PEN=%d", pen);
			ncl_add_token(&cmdbuf, buf, NCL_comma);
		}
	}
	/* update surface attributes if applicable */
	if (modfield[5] || modfield[6] || modfield[7] || modfield[8] || modfield[9])
	{
		UM_srfattr.edge_color =  (modfield[5])? extatts[0] : 0;
		UM_srfattr.lucency    =  (modfield[6])? extatts[2] : 0;
		UM_srfattr.shaded     =  (modfield[7])? extatts[1] : 0;
		UM_srfattr.edge_disp  =  (modfield[8])? 1 : 0;
		if (modfield[9]) UM_ptattr.markertype = extatts[3];
		else UM_ptattr.markertype = 2;
		if ((output) && (modify))
		{
			if (modfield[5]) 
			{
				if (!modfield[8])
					sprintf(buf,"EDGE=OFF");
				else
				{
					sprintf(buf,"EDGE=%s",Scolor[extatts[0]+1]);
				}
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			if (modfield[6])
			{
				sprintf(buf,"TRANS=%d",extatts[2]);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			if (modfield[7])
			{
				sprintf(buf,"SHADE=%s",Soffon[extatts[1]]);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			if (modfield[9])
			{
				sprintf(buf,"MARKER=%s",marks[extatts[3]-1]);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
		}
	}
	if ((output) && (modify))
	{
		ncl_del_token(&cmdbuf,"", UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf);
	}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umf_set_def_attr()
**       Sets the new default attributes for geometry.  Fortran callable
**       routine.
**    PARAMETERS
**       INPUT  :
**          newcolor   = Default color.
**          newpen     = Default pen.
**          newlintyp  = Default line type.
**          newlinwgt  = Default line weight.
**          newlayer   = Default latyer.
**          newshd     = Default surface shade flag.
**          newtrans   = Default surface translucency.
**          newmat     = Default surface material.
**          newedg     = Default surface edge setting/color.
**          newmkr     = Point marker type.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umf_set_def_attr(newcolor,newpen,newlintyp,newlinwgt,newlayer,
	newshd,newtrans,newmat,newedg,newmkr)
int *newcolor,*newpen,*newlintyp,*newlayer,*newshd,*newtrans,*newmat,*newedg;
int *newmkr;
UU_REAL *newlinwgt;
{
	if (*newlayer >= 0) um_set_layer_num(*newlayer);
	if (*newlayer >= 0) um_set_active_layer(*newlayer);
	if (*newlayer >= 0) uz_actlayer(ur_get_attrmdl_layer()); 
	if (*newcolor >= -1) ncl_set_color_attr(*newcolor);
	if (*newlintyp > 0) ur_put_attrmdl_line_style(*newlintyp);
	if (*newlinwgt > 0) ur_put_attrmdl_line_weight(*newlinwgt);
	if (*newpen > 0) ur_put_attrmdl_pen(*newpen);
	if (*newshd >= 0) UM_srfattr.shaded = *newshd;
	if (*newtrans >= 0) UM_srfattr.lucency = *newtrans;
	if (*newmat >= 0) UM_srfattr.material = *newmat;
	if (*newedg == 0) UM_srfattr.edge_disp = 0;
	else if (*newedg == 1) UM_srfattr.edge_disp = 1;
	else if (*newedg > 0)
	{
		UM_srfattr.edge_disp = 1;
		UM_srfattr.edge_color = *newedg - 2;
	}
	if (*newmkr > 0) UM_ptattr.markertype = *newmkr;
}

/*********************************************************************
**    E_FUNCTION     : umu_ubfn_ent_blank(option)
**       blank/unblank entities from second unibase
**    PARAMETERS   
**       INPUT  : 
**          option				0 = Blank filtered
**										1 = Blank all
**										2 = Unblank filtered
**										3 = Unblank all
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_ubfn_ent_blank(option)
	int option;

	{
	UU_KEY_ID key;                              /* key of retrieved data */
	Gseg dsegid;								/* display segment identifier */
	int i, status, ur_update_blanked();
	UV_vport	vport;
	UV_view view;
	int	numint, second_view;
	UU_LOGICAL initialize;
	UU_LOGICAL blanked;
	int rel_num, stype;
	unsigned long mask[4];					/* Das select mask */

	uu_denter(UU_MTRC,(us,"umu_sea_ent_blank(%d)",option));

	/* Set DAS select mask for all relations other than lights */
	second_view = 0;
	for (i=0; i<UV_act_screen[0].nvports; i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_getvid(vport.cur_view, &view);
		if (view.vtype==UV_SECONDARY_VIEW)
		{
			second_view = 1;
			break;
		}
	}
	if (second_view==0)
	{
		ud_wrerr("There is no second unibase view yet.");
		return;
	}
	mask[0] = mask[1] = mask[2] = mask[3] = -1;
	uu_clr_bit(mask, UM_LIGHT_REL-1);
	ud_lgeo(UU_TRUE, mask);

	stype = ud_getpick_type();
	ud_setpick_type(UD_PICK_SECONDARY);
	switch (option)
	{
		case 0: /* blank filtered */
		{
			/* have user pick entities to blank */
			ud_ldas(UD_DASSELECT, /*pick entities to blank:*/UM_MODEL,
					  259, UU_NULL, 0, &numint,UD_NODEFAULT);
			if (numint > 0)
			{
				initialize = UU_TRUE;
				while(ud_gnxt(initialize, UU_NULL, &key, 1) == UU_TRUE)
				{
					initialize = UU_FALSE;

					ur_retrieve_blanked(key, &blanked);
					if (blanked==UU_FALSE)
					{
						if (ur_update_blanked(key, UU_TRUE) == 0)
						{
							ur_retrieve_disp_segid(key, &dsegid);
							uv_blanksegs(dsegid, key);
						}
					}
				}
			}
			break;
		}
		case 1: /* blank all */
		{
				/* Select entities in vport */
			ud_alldisp(UU_FALSE, UU_FALSE, UU_FALSE);

			initialize = UU_TRUE;
			while(ud_gnxt(initialize, UU_NULL, &key, 1) == UU_TRUE)
			{
				initialize = UU_FALSE;
				ur_retrieve_blanked(key, &blanked);
				if (blanked==UU_FALSE)
				{
					if (ur_update_blanked(key, UU_TRUE) == 0)
					{
						ur_retrieve_disp_segid(key,&dsegid);
						uv_blanksegs(dsegid, key);
					}
				}
			}
			break;
		}
		case 2: /* unblank filtered */
		{
			ud_calsel(UU_TRUE, 1, vport.xform, UU_FALSE);
			initialize = UU_TRUE;
			while(ud_gnxt(initialize, UU_NULL, &key, 1) == UU_TRUE)
			{
				initialize = UU_FALSE;

				ur_retrieve_blanked(key, &blanked);
				if (blanked==UU_TRUE)
				{
					if (ur_update_blanked(key,UU_FALSE) == 0)
					{
						ur_retrieve_disp_segid(key,&dsegid);
						uv_unblanksegs(dsegid, key);
					}
				}
			}
			break;
		}
		case 3: /* unblank all */
			{
			initialize = UU_TRUE;
			while (uv_getobjs(initialize, &key, UU_FALSE) == 0)
			{
				initialize = UU_FALSE;
				ur_retrieve_blanked(key, &blanked);
				if (blanked == UU_TRUE)
				{
					ur_retrieve_data_relnum(key, &rel_num);
					if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
					{
						status = ur_update_blanked(key, UU_FALSE);
						ur_retrieve_disp_segid(key,&dsegid);
						uv_unblanksegs(dsegid, key);
					}
				}
			}
			break;
			}
	}
	ud_setpick_type(stype);
	uu_dexit;
}
