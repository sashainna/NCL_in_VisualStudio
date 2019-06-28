/*********************************************************************
**    NAME         :  ymdlattrib.c
**    CONTAINS:
**			int NclxMdlGetAttr(rec)
**			NclxMdlGetLabel(rec,lab,sub,vis,pos)
**			NclxMdlSetLabel(rec,lab,sub,vis,pos)
**			NclxMdlStoreAttr(rec)
**			NclxMdlStoreSurfAttr(rec)
**			NclxMdlSetSurfAttr(rec,attr)
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       ymdlattrib.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:10:59
*********************************************************************/

#include "usysdef.h"
#include "class.h"
#include "goseg.h"
#include "mattrddl.h"
#include "mdrel.h"
#include "mfort.h"
#include "msrf.h"
#include "nccs.h"
#include "nclxmdl.h"

void NclxMdlSetSurfAttr();

/*********************************************************************
**		E_FUNCTION     : int NclxMdlGetAttr(rec)
**			Get the geometric entity's attribute record.
**		PARAMETERS
**		INPUT  :
**			rec                  Geometric entity to retrieve attribute
**			                     record for.
**		OUTPUT :
**			rec                  Returns attribute record within.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxMdlGetAttr(rec)
NCLX_mdl_struct *rec;
{
	struct UC_attributedatabag attr;
/*
.....Get the Attribute record
*/
	attr.key = rec->key;
	ur_retrieve_attr(&attr);
/*
.....Store the attributes
*/
	(*rec).attrib.color = attr.color;
	(*rec).attrib.layer = attr.layer;
	(*rec).attrib.pen = attr.pen;
	(*rec).attrib.line_style = attr.line_style;
	(*rec).attrib.line_weight = attr.line_weight;
	ur_retrieve_blanked(rec->key,&(rec->attrib.visible));
	(*rec).attrib.label_on = attr.label_on;
	(*rec).attrib.displayable = attr.displayable;
}

/*********************************************************************
**		E_FUNCTION     : NclxMdlGetLabel(rec,lab,sub,vis,pos)
**			Get the geometric entity's label.
**		PARAMETERS
**		INPUT  :
**			rec                  Geometric entity to retrieve label for.
**		OUTPUT :
**			lab                  Label.
**			sub                  Subscript.
**			vis                  Is label displayed.
**			pos                  Label position.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxMdlGetLabel(rec,lab,sub,vis,pos)
NCLX_mdl_struct *rec;
char *lab;
int sub,vis;
double pos[3];
{
	strcpy(lab,(*rec).label);
	sub = (*rec).subscript;
	vis = (*rec).attrib.label_on;
	pos[0] = (*rec).label_pos[0];
	pos[1] = (*rec).label_pos[1];
	pos[2] = (*rec).label_pos[2];
}

/*********************************************************************
**		E_FUNCTION     : NclxMdlSetLabel(rec,lab,sub,vis,pos)
**			Set the geometric entity's label.
**		PARAMETERS
**		INPUT  :
**			rec                  Geometric entity to retrieve label for.
**			lab                  Label.
**			sub                  Subscript.
**			vis                  Is label displayed.
**			pos                  Label position.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxMdlSetLabel(rec,lab,sub,vis,pos)
NCLX_mdl_struct *rec;
char *lab;
int sub,vis;
double pos[3];
{
   char strlab[10];
   sprintf(strlab, "%s", lab);
	strcpy((*rec).label,strlab);
	(*rec).subscript = sub;
	(*rec).attrib.label_on = vis;
	(*rec).label_pos[0] = pos[0];
	(*rec).label_pos[1] = pos[1];
	(*rec).label_pos[2] = pos[2];
}

/*********************************************************************
**		E_FUNCTION     : int NclxMdlStoreAttr(rec)
**			Store the geometric entity's attribute record.
**		PARAMETERS
**		INPUT  :
**			rec                  Geometric entity to store.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxMdlStoreAttr(rec)
NCLX_mdl_struct *rec;
{
	struct UC_attributedatabag attr;
	struct UC_entitydatabag e;
	Gseg dsegid;
/*
.....Get the Attribute record
*/
	attr.key = rec->key;
	ur_retrieve_attr(&attr);
/*
.....Update attribute block
*/
	attr.color = (*rec).attrib.color;
	attr.layer = (*rec).attrib.layer;
	attr.pen = (*rec).attrib.pen;
	attr.line_style = (*rec).attrib.line_style;
	attr.line_weight = (*rec).attrib.line_weight;
	attr.label_on = (*rec).attrib.label_on;
	attr.displayable = (*rec).attrib.displayable;
	ur_update_attr(&attr);
/*
.....Invisible entity
*/
	if ((*rec).attrib.visible == UU_TRUE)
	{
		if (ur_update_blanked(rec->key,UU_TRUE) == 0)
		{
			ur_retrieve_disp_segid(rec->key,&dsegid);
			uv_blanksegs(dsegid, rec->key);
		}
	}
/*
.....Display entity with new attributes
*/
	else
	{
		ur_update_blanked(rec->key,UU_FALSE);
		e.key = rec->key;
		uc_retrieve_data(&e,sizeof(e));
		uc_display(&e);
	}
}

/*********************************************************************
**		E_FUNCTION     : int NclxMdlStoreSurfAttr(rec,sfhead)
**			Store the surface's attribute record.
**		PARAMETERS
**		INPUT  :
**			rec                  Surface entity to store attribute for.
**			sfhead               Surface header record.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxMdlStoreSurfAttr(rec,sfhead)
NCLX_mdl_struct *rec;
NCLX_mdl_sfhead *sfhead;
{
	struct UM_surfattr_rec attr;
	struct UC_entitydatabag e;
	Gseg dsegid;
/*
.....Get the Attribute record
*/
	attr.key = rec->key;
	ur_retrieve_attr(&attr);
/*
.....Update attribute block
*/
	NclxMdlSetSurfAttr(rec,sfhead,&attr);
	ur_update_attr(&attr);
/*
.....Invisible entity
*/
	if ((*rec).attrib.visible == UU_TRUE)
	{
		if (ur_update_blanked(rec->key,UU_TRUE) == 0)
		{
			ur_retrieve_disp_segid(rec->key,&dsegid);
			uv_blanksegs(dsegid, rec->key);
		}
	}
/*
.....Display entity with new attributes
*/
	else
	{
		ur_update_blanked(rec->key,UU_FALSE);
		e.key = rec->key;
		uc_retrieve_data(&e,sizeof(e));
		uc_display(&e);
	}
}

/*********************************************************************
**		E_FUNCTION     : int NclxMdlSetSurfAttr(rec,sfhead,attr)
**			Sets the surface's attribute record.
**		PARAMETERS
**		INPUT  :
**			rec                  Surface entity to store attribute for.
**			sfhead               Surface header record.
**		OUTPUT :
**			attr                 Filled attribute record.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxMdlSetSurfAttr(rec,sfhead,attr)
NCLX_mdl_struct *rec;
NCLX_mdl_sfhead *sfhead;
struct UM_surfattr_rec *attr;
{
	struct UC_entitydatabag e;
	Gseg dsegid;
/*
.....Set the Attribute relation number
*/
	attr->rel_num = UM_SURFATTR_REL;
/*
.....Update attribute block
*/
	attr->use_count = 0;
	attr->color = (*rec).attrib.color;
	attr->layer = (*rec).attrib.layer;
	attr->pen = (*rec).attrib.pen;
	attr->line_style = (*rec).attrib.line_style;
	attr->line_weight = (*rec).attrib.line_weight;
	attr->line_width = 0.;
	attr->label_on = (*rec).attrib.label_on;
	attr->displayable = (*rec).attrib.displayable;
	attr->selectable = 1;
	attr->material = sfhead->material;
	attr->numupaths = sfhead->upaths;
	attr->numvpaths = sfhead->vpaths;
	attr->ptsperucrv = sfhead->upts;
	attr->ptspervcrv = sfhead->vpts;
	attr->ecolor = -1;
	attr->shaded = sfhead->shaded;
	attr->lucency = sfhead->lucency;
}
