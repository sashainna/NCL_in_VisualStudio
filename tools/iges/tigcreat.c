/*********************************************************************
**    NAME         :  tigcreate.c
**       CONTAINS:
**               uig_init_surf_attr
**               uig_create_geom
**               uig_create_text(eptr,t,attptr)
**               uig_create_symbol(eptr,t,attptr)
**               uig_update_attr(dblk)
**               uig_create_draft
**               uig_update_text_attr(dblk)
**               uig_update_sym_attr(dblk, attr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigcreat.c , 25.2
**     DATE AND TIME OF LAST  MODIFICATION
**       10/27/16 , 14:50:19
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdattr.h"
#include "mxxx.h"
#include "tigsupp.h"
#include "adrfcom.h"
#include "mdgenent.h"
#include "bsym.h"
#include "udebug.h"
#include "class.h"
#include "atext.h"
#include "nccs.h"
#include "msrf.h"

extern int UG_def_line_wt;

/*********************************************************************
**    I_FUNCTION     :  uig_init_surf_attr (key)
**      Initializes a surface attribute record and associates
**      it with the given entity.
**    PARAMETERS   
**       INPUT  :
**          key         Key of surface entity to initialize attribute with.
**       OUTPUT : none 
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_init_surf_attr (key,attr)
UU_KEY_ID key;
struct UM_surfattr_rec *attr;
{
	UM_int2 itype,lblst;
	int color,ecolor,ecolor1,status;
/*
.....Initialize standard attributes
*/
	attr->key = key;
	attr->rel_num = UM_SURFATTR_REL;
	attr->label_on = UU_FALSE;

	attr->use_count = UM_attrmdl.use_count;
	attr->color = UM_attrmdl.color;
	attr->layer = UM_attrmdl.layer;
	attr->pen = UM_attrmdl.pen;
	attr->line_style = UM_attrmdl.line_style;
	attr->line_weight = UM_attrmdl.line_weight;
	attr->line_width = UM_attrmdl.line_width;
	attr->displayable = UM_attrmdl.displayable;
	attr->selectable = UM_attrmdl.selectable;
/*
.....Initialize surface attributes
*/
	attr->material = 0;
	attr->numupaths = 5;
	attr->numvpaths = 5;
	attr->ptsperucrv = 0;
	attr->ptspervcrv = 0;
	attr->shaded = shade_set;
	attr->lucency = UIG_lucency;
/*
......use UIG_edge_color
*/
	if (UIG_srf_edge == 0)
		attr->ecolor = -1;
	else
		attr->ecolor = UIG_edge_color;
/*
.....Update attribute record
*/
	status = ur_update_attr(attr);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_create_geom(eptr,t,attptr, view_key)
**				Create a Unibase geometry record.
**    PARAMETERS   
**       INPUT  : 
**				eptr							pointer to the entity structure
**				t								transformation if rquired
**				attptr						pointer to attribute bundle
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_create_geom(eptr,t,attptr, view_key)
struct NCL_fixed_databag *eptr;
UU_REAL t[12];
struct UM_attrdata_rec *attptr;
int view_key;
{

	struct UM_transf_rec tran;
	struct UM_surfattr_rec sfattr;
	int status, i, j, k;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_create_geom"));

/*
...if labeled geometry type initialize label location
*/
   um_init_lablocal (eptr);
 
	/* create unibase entry */

	status = ur_create_data(eptr);

	/* update display segment id */

	status = ur_update_disp_segid(eptr->key,0);

	/* define default attributes */

	if (eptr->rel_num == UM_RBSPLSRF_REL ||
		eptr->rel_num == NCL_REVSURF_REL ||
		eptr->rel_num == NCL_TRIMSF_REL)
	{
		status = uig_init_surf_attr (eptr->key,&sfattr);
	}
	else
	{
		ur_put_attrmdl_key(eptr->key);
		ur_put_attrmdl_rel_num(UM_ATTR_REL);
		ur_put_attrmdl_label_on(UU_FALSE);

		status = ur_update_attr(&UM_attrmdl);
	}

	/* define default transformation */

	tran.key = eptr->key;
	tran.rel_num = UM_TRANSFORM_REL;
	if (t == NULL)
		um_tftotf(UM_idmat,tran.tfmat);
	else
		{
		k = 0;
		for (i=0;i<3;i++)
			{
			for (j=0;j<4;j++) tran.tfmat[j][i] = t[k++];
/* 			tran.tfmat[3][i] = t[k++]; */
			}
		}
	status = ur_update_transf(&tran);

	/* define view priority */
	status = uig_update_view (eptr->key, view_key);

	uu_dexit;
	return (status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_update_view (key, view_key)
**            Update entity view.
**    PARAMETERS   
**       INPUT  : 
**            key        - key of entity.
**            view_key   - key of view.
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_update_view (key, view_key)
   UU_KEY_ID key;
   int view_key;
   {

   int found, status, i;

   uu_denter(UU_MTRC,(us,"uig_update_view "));

   status = UU_SUCCESS;
   /* define view priority */
   if(view_key == 0)
      {
      ur_update_view_key(key, 0);
      }
   else
      {
      found = UU_FALSE;
      for(i=0;i<no_of_views && !found;i++)
         {
         if(view_key == view_keys[i][0])
            {
            ur_update_view_key(key, view_keys[i][1]);
            found = UU_TRUE;
            }
         }
      if (!found) ur_update_view_key(key, 0);
      }
   uu_dexit;
   return(status);
   }
/*********************************************************************
**    I_FUNCTION     :  uig_create_text(eptr,t,attptr)
**      		Create a Unibase text record.
**    PARAMETERS   
**       INPUT  : 
**				eptr							pointer to the entity structure
**				t								transformation if rquired
**				attptr						pointer to attribute bundle
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
uig_create_text(eptr , t, attr, view_key)
	struct UC_entitydatabag *eptr;
	UU_REAL t[4][3];
	struct UA_txtattr_rec *attr;
	int view_key;
	{

	struct UM_transf_rec tran;
	int status, i;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_create_text"));

	/* create unibase entry */

	status = ur_create_data(eptr);

	/* update display segment id */

	status = ur_update_disp_segid(eptr->key,0);

	/* define default attributes */

	attr->key = eptr->key;
	attr->rel_num = UA_TEXTATTR_REL;
	status = ur_update_attr(attr);

	/* define default transformation */

	tran.key = eptr->key;
	tran.rel_num = UM_TRANSFORM_REL;
	um_tftotf(UM_idmat,tran.tfmat);
	status = ur_update_transf(&tran);

	/* define view priority */

	if(view_key == 0)
		{
		ur_update_view_key(eptr->key, 0);
		uu_dexit;
		return;
		}
	else
		{
		for(i=0;i<no_of_views;i++)
			{
			if(view_key == view_keys[i][0])
				{
				ur_update_view_key(eptr->key, view_keys[i][1]);
				uu_dexit;
				return;
				}
			}
		}
	ur_update_view_key(eptr->key, 0);
	uu_dexit;
	}
/*********************************************************************
**    I_FUNCTION     :  uig_create_symbol(eptr,t,attptr)
**				Create a Unibase symbol/instance record.
**    PARAMETERS   
**       INPUT  : 
**				eptr							pointer to the entity structure
**				t								transformation if rquired
**				attptr						pointer to attribute bundle
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
uig_create_symbol(eptr,t,attr, view_key)
	struct UC_entitydatabag *eptr;
	UU_REAL t[4][3];
	struct UA_txtattr_rec *attr;
	int    view_key;
	{

	struct UM_transf_rec tran;
	int status, i;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_create_symbol"));

	/* create unibase entry */

	status = ur_create_data(eptr);

	/* update display segment id */

	status = ur_update_disp_segid(eptr->key,0);

	/* define default attributes */

	attr->key = eptr->key;
	attr->rel_num = UB_SYMATTR_REL;
	status = ur_update_attr(attr);

	/* define default transformation */

	tran.key = eptr->key;
	tran.rel_num = UM_TRANSFORM_REL;
	um_tftotf(UM_idmat,tran.tfmat);
	status = ur_update_transf(&tran);

	/* define view priority */
	if(view_key == 0)
		{
		ur_update_view_key(eptr->key, 0);
		uu_dexit;
		return;
		}
	else
		{
		for(i=0;i<no_of_views;i++)
			{
			if(view_key == view_keys[i][0])
				{
				ur_update_view_key(eptr->key, view_keys[i][1]);
				uu_dexit;
				return;
				}
			}
		}
	ur_update_view_key(eptr->key, 0);
	uu_dexit;

	}

/*********************************************************************
**    I_FUNCTION     :  uig_create_draft(eptr,t,attptr)
**				Create a Unibase drafting entity record.
**    PARAMETERS   
**       INPUT  : 
**				eptr							pointer to the entity structure
**				t								transformation if rquired
**				attptr						pointer to attribute bundle
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
uig_create_draft(eptr,t,attptr, view_key)
	struct UA_generic_draft *eptr;
	UU_REAL t[4][3];
	struct UM_attrdata_rec *attptr;
	int view_key;
	{

	struct UM_transf_rec tran;
	int i;
	int status, key;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_create_draft"));

	/* create unibase entry */

	ua_create_entity(eptr, &key);

	/* update display segment id */

	status = ur_update_disp_segid(eptr->key,0);

	/* define view priority */
	if(view_key == 0)
		{
		ur_update_view_key(eptr->key, 0);
		uu_dexit;
		return;
		}
	else
		{
		for(i=0;i<no_of_views;i++)
			{
			if(view_key == view_keys[i][0])
				{
				ur_update_view_key(eptr->key, view_keys[i][1]);
				uu_dexit;
				return;
				}
			}
		}
	ur_update_view_key(eptr->key, 0);
	uu_dexit;

	}

/*********************************************************************
**    I_FUNCTION     :  uig_update_attr(dblk)
**				Update attribute bundle for this entity.
**    PARAMETERS   
**       INPUT  : 
**				dblk							entity directory block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_update_attr(dblk)
	struct dir_rec *dblk;
	{
	int ln_sty;
	int ln_weight;
	int i,color;
	int lev;
	UU_REAL ln_wt;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_update_attr"));

	/* check layer number */

 	lev = dblk->level;
	uig_set_layer_num(lev);
	if(lev > IG_LAYER) lev = 0;
	if(lev != ur_get_attrmdl_layer())
				ur_put_attrmdl_layer(lev);

	/* check line_style */


	switch (dblk->line_font)
	{
	 case	1:
		ln_sty = UM_SOLID_LINE;	break;
	 case 2:
		ln_sty = UM_DASHED_LINE;	break;
	 case 3:
		ln_sty = UM_PHANTOM_LINE;	break;
	 case 4:
		ln_sty = UM_CENTER_LINE;	break;
/*
.....Added UM_DOT_LINE.  JLS 11/17/98
*/
	 case 5:
		ln_sty = UM_DOT_LINE;  break;
	 default:
		ln_sty = UM_SOLID_LINE;	break;
	}
	if(ln_sty != ur_get_attrmdl_line_style()) 
				ur_put_attrmdl_line_style(ln_sty);

	/* check color number */
	color = -1;
	if(dblk->pen_no < 0)
	{
		for(i=0;i<UIG_ncolor_iges;i++)
		{
			if(UIG_color_array[i].irec == abs(dblk->pen_no))
			{
				color = UIG_color_array[i].color;
				break;
			}
		}
	}
	else
		color = dblk->pen_no;
	if (color == -1)
	{
/*
.....use default color used by iges
*/
		switch(dblk->rel_type)
		{
			case GFACE:
			case GSPLSURF:
			case GBDSRF:
			case GRSPLSRF:
			case GRULEDSRF:
			case GTRIMSRF:
			case GREVSRF:
				color = UM_MAGENTA;
				break;
			case GLINE:
				color = UM_DARKGREEN;
				break;
			case GPOLY:
			case GPOLY6D:
				if(entity_mask[36] ==1 && dblk->form_no ==13)
					color = UM_CYAN;
				else
					color = UM_DARKRED;
				break;
			case GPOLY3D:
				if(entity_mask[36] ==1 && dblk->form_no ==12)
					color = UM_CYAN;
				else
					color = UM_YELLOW;
				break;
			case GPOINT:
				color = UM_DARKRED;
				break;
			case GPLANE:
				color = UM_DARKGREEN;
				break;
			case GARC:
				color = UM_DARKBLUE;
				break;
			case GCONIC:
				color = UM_YELLOW;
				break;
			case GSPLINE:
			case GRSPLINE:
				color = UM_CYAN;
				break;
			default:
				color = UM_YELLOW; 
				break;
		}
	}
	if(color != ur_get_attrmdl_color()) 
		ur_put_attrmdl_color(color);

	/* check displayable flag */

	if(DDC_displayable_flag != ur_get_attrmdl_displayable()) 
				ur_put_attrmdl_displayable(DDC_displayable_flag);

	/* check line_weight */

	if (!UG_def_line_wt)
		{
		ln_wt = ur_get_attrmdl_line_weight();
		ln_weight = ln_wt;
		if(dblk->line_wt != ln_weight)
			{
			ln_weight = dblk->line_wt;
			ln_wt = ln_weight;
			ur_put_attrmdl_line_weight(ln_wt);
			}
		}
	uu_dexit;
	}
/*********************************************************************
**    I_FUNCTION     :  uig_update_text_attr(dblk)
**				Update text attribute bundle for this entity.
**    PARAMETERS   
**       INPUT  : 
**				dblk							entity directory block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_update_text_attr(dblk, t, attr)
	struct dir_rec *dblk;
	struct UA_txtattr_rec *attr;
	UU_REAL t[4][3];
	{
	int ln_sty;
	int ln_weight = 0;
	int i,color;
	UU_REAL ln_wt;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_update_text_attr"));

	attr->layer = dblk->level;
	uig_set_layer_num(attr->layer);

	switch (dblk->line_font)
	{
	 case	1:
		ln_sty = UM_SOLID_LINE;	break;
	 case 2:
		ln_sty = UM_DASHED_LINE;	break;
	 case 3:
		ln_sty = UM_PHANTOM_LINE;	break;
	 case 4:
		ln_sty = UM_CENTER_LINE;	break;
/*
.....Added UM_DOT_LINE.  JLS 11/17/98
*/
	 case 5:
		ln_sty = UM_DOT_LINE;  break;
	 default:
		ln_sty = UM_SOLID_LINE;	break;
	}
	attr->line_style = ln_sty;

	attr->pen = 0;

	attr->color = -1;
	if(dblk->pen_no < 0)
	{
		for(i=0;i<UIG_ncolor_iges;i++)
		{
			if(UIG_color_array[i].irec == abs(dblk->pen_no))
			{
				attr->color = UIG_color_array[i].color;
				break;
			}
		}
	}
	else
		attr->color = dblk->pen_no;
	if (attr->color==-1)
		attr->color = UM_BLACK;
	attr->displayable = DDC_displayable_flag;
	ln_wt = ln_weight;
	attr->line_weight = ln_wt;
	attr->line_width = 1.0;
	attr->selectable = UU_TRUE;
	attr->font = UA_txt_fontnum;
	attr->prec = (int ) UA_txt_precision;
	attr->expn = UA_char_expansion;
	attr->spacing = UA_char_space;
	attr->path = 0;
	attr->align_hor = 0;
	attr->align_ver = 0;
	uig_trans_comp(t,2,attr->up);
	uig_trans_comp(t,3,attr->plane);
	uu_dexit;
	}
/*********************************************************************
**    I_FUNCTION     :  uig_update_sym_attr(dblk, attr)
**				Update symbol attribute bundle for this entity.
**    PARAMETERS   
**       INPUT  : 
**				dblk							entity directory block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_update_sym_attr(dblk, attr)
	struct dir_rec *dblk;
	struct UB_symattr_rec *attr;
	{
	int i,ln_sty;
	int ln_weight = 0;
	UU_REAL ln_wt = 0;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_update_sym_attr"));

	attr->layer = dblk->level;
	uig_set_layer_num(attr->layer);

	switch (dblk->line_font)
	{
	 case	1:
		ln_sty = UM_SOLID_LINE;	break;
	 case 2:
		ln_sty = UM_DASHED_LINE;	break;
	 case 3:
		ln_sty = UM_PHANTOM_LINE;	break;
	 case 4:
		ln_sty = UM_CENTER_LINE;	break;
/*
.....Added UM_DOT_LINE. JLS 11/17/98
*/
	 case 5:
		ln_sty = UM_DOT_LINE;  break;
	 default:
		ln_sty = UM_SOLID_LINE;	break;
	}
	attr->line_style = ln_sty;

	attr->pen = 0;
	attr->color = -1;
	if(dblk->pen_no < 0)
	{
		for(i=0;i<UIG_ncolor_iges;i++)
		{
			if(UIG_color_array[i].irec == abs(dblk->pen_no))
			{
				attr->color = UIG_color_array[i].color;
				break;
			}
		}
	}
	else
		attr->color = dblk->pen_no;
	attr->displayable = DDC_displayable_flag;
	if (attr->color==-1)
		attr->color = UM_YELLOW;
	ln_wt = ln_weight;
	attr->line_weight = ln_wt;

	attr->line_width = 0.0;
	attr->selectable = UU_TRUE;
	attr->see_snod = UU_TRUE;
	attr->see_tnod = 0;
	uu_dexit;
	}
