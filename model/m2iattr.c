/*********************************************************************
**    NAME         :  m2iattr.c
**       CONTAINS:
**    um_sda1_geom_lnstyl(lnstyl)
**    um_set_disp_attr(attrptr)
**    um_retrieve_material( surface, material)
**    um_set_material( surface, material)
**    um_get_attrib(nclkey,color,pen,lintyp,linwgt,layer)
**    umf_get_attrib(nclkey,color,pen,lintyp,linwgt,layer)
**    umf_get_marker_type(nclkey,marker)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m2iattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:47
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "class.h"
#include "mdattr.h"
#include "mattr.h"
#include "mdrel.h"
#include "mxxx.h"
#include "mdgenent.h"
#include "msrf.h"
#include "mdebug.h"
#include "nclfc.h"
#include "mfort.h"
#include "nccs.h"
#include "mcrv.h"

/*********************************************************************
**    E_FUNCTION     : um_sda1_geom_lnstyl(lnstyl)
**       Set the default line style attribute for geometry.
**    PARAMETERS   
**       INPUT  : 
**          lnstyl                  set the default line style 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_sda1_geom_lnstyl(lnstyl)
   int lnstyl;                      /* new line style */
   {
   uu_denter(UU_MTRC,(us,"um_sda1_geom_lnstyl(%d)",lnstyl));
   ur_put_attrmdl_line_style(lnstyl);
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : um_set_disp_attr(attrptr)
**       If ATTRPTR is not UU_NULL, set DIGS display attributes
**       for line color, line style, and line width. Otherwise,
**       do nothing.
**    PARAMETERS   
**       INPUT  : 
**          attrptr              pointer to attribute record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_set_disp_attr(attrptr)
   struct UM_attrdata_rec *attrptr;

   {
   Glntype line_style;

   uu_denter(UU_MTRC,(us,"um_set_disp_attr(attr=%x)",attrptr));

   if (attrptr != UU_NULL)
      {
      gsfillcolor(attrptr->color);
      gslinecolor(attrptr->color);
      line_style.typeno = attrptr->line_style;
      line_style.npatn = 0;
      gslinetype(&line_style);
      gslinewidth(attrptr->line_weight);
      }

   uu_dexit;
   }


/******************************************************************************
**    E_FUNCTION     :  um_retrieve_material( surface, material)
**       Returns material index of surface.
**    PARAMETERS   
**       INPUT  : 
**          UM_srfdatabag  *surface;            Surface record
**          int *material;                      Material index returned
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : Assumes that all surf types have material field in same
**                   position in record.
******************************************************************************/
void um_retrieve_material(srf, mat, kd, ks, spec_exp)
   struct UC_entitydatabag *srf;
   int *mat;
   UU_REAL *kd, *ks, *spec_exp;

   {
   struct UM_rbsplsrf_rec *dsrf;

   uu_denter(UU_MTRC,(us,"um_retrieve_material(%x, rel_num %d)",
      srf, srf->rel_num));

   dsrf = (struct UM_rbsplsrf_rec *) srf;
	ncl_retrieve_material(srf,mat);

   if( *mat >= 0 )
      {
      *kd = UM_mtrlmdl.kd[*mat];
      *ks = UM_mtrlmdl.ks[*mat];
      *spec_exp = UM_mtrlmdl.spec_exp[*mat];
      uu_dprint(UU_MTRC,(us,"kd %f, ks %f, spec_exp %f", *kd, *ks, *spec_exp));
      }

   uu_dprint(UU_MTRC,(us,"um_retrieve_material returns %d)", *mat));
   uu_dexit;
   }


/******************************************************************************
**    E_FUNCTION     :  um_set_material( surface, material)
**       Sets a new material index for a surface.
**    PARAMETERS   
**       INPUT  : 
**          UM_srfdatabag  *surface;            Surface record
**          int material;                       Material index to set.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : Assumes that all surf types have material field in same
**                   position in record.
******************************************************************************/
void um_set_material(srf, mat)
   struct UC_entitydatabag *srf;
   int mat;

   {
   struct UM_rbsplsrf_rec *dsrf;
   int status;

   uu_denter(UU_MTRC,(us,"um_set_material(%x, %d)", srf, mat));

   dsrf = (struct UM_rbsplsrf_rec *) srf;
	ncl_setent_material(dsrf,mat);
   status = ur_update_data_fixed(dsrf);

   uu_dexit;
   }

/******************************************************************************
**    E_FUNCTION     :  um_get_attrib(nclkey,color,pen,lintyp,linwgt,layer)
**       Returns the display attributes for the requested entity.
**    PARAMETERS   
**       INPUT  : 
**          nclkey   = Key of entity to retrieve attributes for.
**          color    = Entity's color.
**          pen      = Entity's pen number.
**          lintyp   = Entity's line type.
**          linwgt   = Entity's line weight.
**          layer    = Entity's layer number.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
******************************************************************************/
void um_get_attrib(nclkey,color,pen,lintyp,linwgt,layer)
UU_KEY_ID *nclkey;
int *color,*pen,*lintyp,*linwgt,*layer;
{
   int linwid, displayable, selectable, label_on;
	struct UC_attributedatabag attr1;
/*
.....Calling uc_retrieve_attr will get the current
.....entity's attributes.
*/

	uc_retrieve_attr(*nclkey,&attr1);
	*color = attr1.color;
	*pen   = attr1.pen;
	*lintyp= attr1.line_style;
	*linwgt=attr1.line_weight;
	*layer = attr1.layer;
   linwid=attr1.line_width;
	displayable=attr1.displayable;
	selectable =attr1.selectable;
	label_on = attr1.label_on;
	

	return;
}

/**********************************************************************
**
**  E_FUNCTION: umf_get_attrib(nclkey,color,pen,lintyp,linwgt,layer)
**
**     Retrieves the entity's attributes.
**     Fortran interface.
**
**********************************************************************/
void umf_get_attrib(nclkey,color,pen,lintyp,linwgt,layer)
UU_KEY_ID *nclkey;
UM_int2 *color,*pen,*lintyp,*linwgt,*layer;
{
	int jcolor,jpen,jlintyp,jlinwgt,jlayer;
	um_get_attrib(nclkey,&jcolor,&jpen,&jlintyp,&jlinwgt,&jlayer);
	*color = jcolor;
	*pen = jpen;
	*lintyp = jlintyp;
	*linwgt = jlinwgt;
	*layer = jlayer;
}

/*********************************************************************
**    E_FUNCTION     : void umf_get_marker_type (nclkey,marker)
**       Fortran callable routine that gets the marker type for a 
**       point or pattern.
**    PARAMETERS   
**       INPUT  : 
**          nclkey - point/pattern key
**       OUTPUT :  
**          marker - marker type for given point/pattern
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umf_get_marker_type (nclkey,marker)
UU_KEY_ID *nclkey;
UM_int2 *marker;
{
	struct NCL_fixed_databag e;
	struct UM_point_rec *pt;
	struct NCL_patern_rec *ptn;

	e.key = *nclkey;
	ncl_retrieve_data_fixed(&e);
	*marker = -1;
	switch(e.rel_num)
	{
	case NCL_PATERN_REL:
		ptn = (struct NCL_patern_rec *)&e;
		*marker = ptn->markertype;
		break;
	case UM_POINT_REL:
		pt = (struct UM_point_rec *)&e;
		*marker = pt->markertype;
		break;
	}
}
