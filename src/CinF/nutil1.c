/*********************************************************************
**    NAME         :  nutil1.c
**       CONTAINS: utility routines
**				UU_LOGICAL ncl_legal_relation(rel_num)
**				void nclu_repaint (geo,nc)
**				void nclu_list_repaint (slist,color)
**				void nclu_add_list(sflst,geo,color)
**				void nclu_push_sf_key(keylst,geo)
**				void nclu_push_new_sfkey(keylst,geo,nsurf,geolst)
**				nclu_init_sgeo (geb)
**				nclu_swap_sgeo(slist,i,j)
**          ncl_show_layer
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nutil1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:16
*********************************************************************/
#include "usysdef.h"
#include "mdrel.h"
#include "go.h"
#include "udforms.h"
#include "mdpick.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclvx.h"

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL ncl_legal_relation(rel_num)
**       Return UU_TRUE if the given relation (rel_num) is a
**       legal "NCL" relation. (This includes UNICAD geometry)
**    PARAMETERS
**       INPUT  :
**          rel_num              relation number
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_TRUE iff this is a legal NCL relation; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
ncl_legal_relation(rel_num)
   int rel_num;

   {
   UU_LOGICAL status;

   switch (rel_num)
      {
      case UM_POINT_REL:
      case UM_LINE_REL:
      case UM_POLYLINE_REL:
      case UM_CIRCLE_REL:
      case UM_CONIC_REL:
      case UM_COMPCRV_REL:
      case UM_RBSPLCRV_REL:
      case UM_UVCVONSF_REL:
      case UM_AGCRV_REL:
      case UM_AGSRF_REL:
      case UM_AGSHELL_REL:
      case NCL_VECTOR_REL:
      case NCL_POINT_REL:
      case NCL_LINE_REL:
      case NCL_CIRCLE_REL:
      case NCL_PLN_REL:
      case NCL_CURVE_REL:
      case NCL_SURF_REL:
      case NCL_REVSURF_REL:
      case NCL_MATRIX_REL:
      case NCL_MESHSURF_REL:
      case NCL_QUILTSURF_REL:
      case NCL_NETSF_REL:
      case NCL_EVALCV_REL:
      case NCL_EVALSF_REL:
      case UM_RBSPLSRF_REL:
      case NCL_SHAPE_REL:
      case NCL_PATERN_REL:
      case NCL_SCALAR_REL:
      case NCL_POINTVEC_REL:
      case NCL_TRIMSF_REL:
      case NCL_DATAST_REL:
      case NCL_TEXTVAR_REL:
      case UA_TEXT_REL:
      case UB_SYMBOL_REL:
      case UB_INSTANCE_REL:
      case UM_SOLID_REL:
         status = UU_TRUE;
         break;
      default:
         status = UU_FALSE;
         break;
      }
   return (status);
   }

/*********************************************************************
**    I_FUNCTION     : void nclu_repaint (geo,nc,color)
**			Redisplay entities with their saved colors, if color = -1,
**       otherwise with the provided color.
**    PARAMETERS
**       INPUT  :
**						geo   = entities to repaint
**						nc    = number of entities
**						color = valid color number or -1
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_repaint (geo,nc,color)
UM_sgeo *geo;
int nc,color;
{
	int i;
	struct NCL_fixed_databag e;

	for (i = 0; i < nc; i++)
	{
		if (geo[i].key == (UU_KEY_ID)0) continue;
		if (color == -1 && (geo[i].color < 0 || geo[i].color > 63)) continue;
		e.key = geo[i].key;
		if (color == -1)
			ncl_update_geo_color(e.key,geo[i].color,UU_FALSE);
		else
			ncl_update_geo_color(e.key,color,UU_TRUE);
		if (ncl_retrieve_data_fixed (&e) == UU_SUCCESS)
			uc_display(&e);
	}
	ud_updatews(UG_SUPPRESS);
}

/*********************************************************************
**    I_FUNCTION     : void nclu_list_repaint (slist,color)
**       Redisplay entities with their saved colors, if color = -1,
**       otherwise with the provided color.
**    PARAMETERS
**       INPUT  :
**						slist = list of entities to repaint
**						color = valid color number or -1
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_list_repaint (slist,color)
UU_LIST *slist;
int color;
{
	int nc;
	UM_sgeo *geo;

	geo = (UM_sgeo *) UU_LIST_ARRAY (slist);
	nc = slist->cur_cnt;
	nclu_repaint (geo,nc,color);
}

/*********************************************************************
**    I_FUNCTION     : nclu_add_list(sflst,geo,color)
**			Adds a new entity to the surface list.  If the entity
**			already exists, then it will remove it from the list instead.
**    PARAMETERS
**       INPUT  :
**						sflst   = List of already selected entities.
**						geo     = Entity to add/remove from list.
**						color   = Entity display color when added to the list.
**       OUTPUT :
**						sflst  = Updated list.
**						color  = -1 if the entity was removed, and so should not
**						         be redisplayed; unchanged otherwise.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_add_list(sflst,geo,color)
UU_LIST *sflst;
UM_sgeo *geo;
int *color;
{
	int i,nc;
	UU_LOGICAL found;
	UM_sgeo *list_geo;
	char sbuf[100];
/*
.....Point to the geometry list
*/
	list_geo = (UM_sgeo *) UU_LIST_ARRAY(sflst);
/*
.....Search for the selected entity
*/
	found = UU_FALSE;
	nc = UU_LIST_LENGTH(sflst);
	for (i = 0; i < nc; i++)
	{
/*
.....Entity found
.....Remove it from the list
*/
		if (geo->key == list_geo[i].key)
		{
			*color = -1;
			geo->color = list_geo[i].color; /* return the stored color */
/*
..... qar 95236: added a message to work as in nucontour.c
*/
			sprintf(sbuf,"Entity %s removed.",geo->label);
			ud_prmerr(sbuf);
			uu_list_delete(sflst,i,1);
			found = UU_TRUE;
			break;
		}
	}
/*
.....Entity not found
*/
	if (!found)
		uu_list_push(sflst,geo);
	return;
}

/*********************************************************************
**    I_FUNCTION     : nclu_push_sf_key(keylst,geo)
**			Adds an entity to the surfaces keylist. If a net surface,
**			keys of component surfaces are added instead.
**    PARAMETERS
**       INPUT  :
**						keylst  - List of keys.
**						geo     = Entity to add to list.
**       OUTPUT :
**						keylst  = Updated list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_push_sfkey(keylst,geo)
UU_LIST *keylst;
UM_sgeo *geo;
{
	int numsf,status;
	struct NCL_fixed_databag e;
	struct NCL_netsf_rec *netsf;

	if (geo->relnum == NCL_NETSF_REL)
	{
		e.key = geo->key;
		status = ncl_retrieve_data_fixed (&e);
		if (status != UU_SUCCESS) return;
		netsf = (struct NCL_netsf_rec*) &e;
		numsf = netsf->no_netkey;
		if (numsf > 0)
			uu_list_push_multiple (keylst,numsf,netsf->netkey);
	}
	else
		uu_list_push (keylst,&geo->key);
	return;
}

/*********************************************************************
**    I_FUNCTION     : nclu_push_new_sfkey(keylst,geo,nsurf,geolst)
**       Adds a new entity to the surfaces keylist - new means not already
**       present in geolst. If a net surface, keys of component surfaces are
**       added instead.
**    PARAMETERS
**       INPUT  :
**						keylst  - List of keys.
**						geo     = Entity to add to list.
**						geolst  - array of surfaces to match against.
**						nsurf   - number of surfaces in geolst.
**       OUTPUT :
**						keylst  = Updated list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_push_new_sfkey(keylst,geo,nsurf,geolst)
UU_LIST *keylst;
UM_sgeo *geo,*geolst;
int nsurf;
{
	int j;
	UU_LOGICAL found = UU_FALSE;

	if (nsurf > 0)
	{
		for (j = 0; j < nsurf && !found; j++)
			found = (geolst[j].key == geo->key);
		if (!found) nclu_push_sfkey (keylst, geo);
	}
	else
		nclu_push_sfkey (keylst,geo);
}

/*********************************************************************
**    I_FUNCTION     : nclu_init_sgeo (geb)
*********************************************************************/
void nclu_init_sgeo (geb)
UM_sgeo *geb;
{
	geb->key = NULLKEY;
	geb->relnum = 0;
	geb->color = -1;
	geb->inout = 0;
	geb->label[0] = '\0';
	geb->thick = 0;
}

/*********************************************************************
**    I_FUNCTION     : nclu_copy_sgeo (gea,geb)
*********************************************************************/
static void nclu_copy_sgeo (gea,geb)
UM_sgeo *gea,*geb;
{
	geb->key = gea->key;
	geb->relnum = gea->relnum;
	geb->color = gea->color;
	geb->inout = gea->inout;
	geb->thick = gea->thick;
	strncpy(geb->label,gea->label,NCL_MAX_LABEL_AND_SUBSCRIPT);
}

/*********************************************************************
**    I_FUNCTION     : nclu_swap_sgeo(slist,i,j)
*********************************************************************/
void nclu_swap_sgeo(slist,i,j)
UU_LIST *slist;
int i,j;
{
	UM_sgeo *geo,tmp;

	geo = (UM_sgeo *) UU_LIST_ARRAY(slist);
	nclu_copy_sgeo (&geo[i],&tmp);
	nclu_copy_sgeo (&geo[j],&geo[i]);
	nclu_copy_sgeo (&tmp,&geo[j]);
}

/*********************************************************************
**    I_FUNCTION     : ncl_show_layer(xlaylist,xcolor,xlayer,xlistlayer,flag)
**			Displays all surfaces in the layer.
**    PARAMETERS   
**       INPUT  :
**          xlaylist    list to hold the layer entities
**          xcolor      color to highlight entities
**          xlayer      current layer
**          xlistlayer  layer number for list entities (if the list is filled)
**          flag        0 = Include surfaces, 1 = solids, 2 = surfaces & solids,
**                      3 = all geometry.
**       OUTPUT :  
**          xlaylist    list holding the layer entities
**          xlistlayer  layer number for list entities
**
**    RETURNS      : UD_DONE iff CTRL-C pressed, else UD_FLDOK
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT ncl_show_layer(xlaylist,xcolor,xlayer,xlistlayer,flag)
UU_LIST *xlaylist;
int xcolor,xlayer,*xlistlayer,flag;
{
	UM_sgeo geo;
	int layer,color,i,dum,nc,ietype;
	struct NCL_fixed_databag e;
	UM_sgeo *geoptr;
	UM_int2 idx,ifl86,ifl35;
	UU_KEY_ID nclkey;

	color = xcolor;

	nc = xlaylist->cur_cnt;
	if (nc > 0)
	{
		geoptr = (UM_sgeo *) UU_LIST_ARRAY(xlaylist);
/*
..... if same layer, redisplay the surfaces in the list with xcolor
*/
		if (xlayer == *xlistlayer)
		{
			for (i = 0; i < nc && color != -1; i++)
			{
				e.key = geoptr[i].key;
				if (ncl_retrieve_data_fixed (&e) == UU_SUCCESS)
				ncl_update_geo_color(e.key,color,UU_TRUE);
				uc_display(&e);
			}
			return (UD_FLDOK);
		}
/*
..... redisplay all surfaces in the list with original colors, reset the list
*/
		nclu_repaint (geoptr,nc,-1);
		xlaylist->cur_cnt = 0;
	}
/*
..... get all surfaces with the xlayer attribute from the ranfile, store
..... them in the list with original colors, then redisplay them with xcolor
*/
	vxlfst();
	while (UU_TRUE)
	{
		if (!ncl_vxlnxt (&nclkey,&ietype)) break;
		if (nclkey == 0) continue;

		if (((flag == 0 || flag == 2) && ietype == NCLI_SURF) ||
			((flag == 1 || flag == 2) && ietype == NCLI_SOLID) ||
			(flag == 3 && (ietype != NCLI_MPARM && ietype != NCLI_LABEL)))
		{
			color = xcolor;
			um_get_attrib (&nclkey,&geo.color,&dum,&dum,&dum,&layer);
			if (layer == xlayer)
			{
				e.key = nclkey;
				if (ncl_retrieve_data_fixed(&e) != 0) continue;
				geo.relnum = e.rel_num;
				geo.key = nclkey;
				nclu_add_list(xlaylist,&geo,&color);
/*
.....Update the entities color
*/
				if (color != -1)
				{
					ncl_update_geo_color(e.key,color,UU_TRUE);
					uc_display(&e);
				}
			}
		}
		ifl35 = 0; ckintr(&ifl86,&ifl35);
		idx = 86; getifl(&idx,&ifl86);
		if (ifl86 != 0) return (UD_DONE);
	}
	if (xlaylist->cur_cnt > 0)
		*xlistlayer = xlayer;
	else
		*xlistlayer = -1;

	return (UD_FLDOK);
}
