/*********************************************************************
**    NAME         :  axhclas.c
**       CONTAINS:
**			dispatch functions for drafting class, xhatch relations
**	
**			this code must be in
**			C rather than SAL because SAL RECORDs can't contain pointers
**			or unbounded ARRAYs, one of which is necessary to 
**			represent the variable number of lines in each xhatch entity
**
**							ua_disp_xhatch()
**							ua_copy_xhatch()
**							ua_retrieve_xhatch()
**							ua_transf_xhatch()
**							ua_create_xhatch()
**							ua_translate_xhatch()
**							ua_rotate_xhatch()
**
**							ua_update_xhatch() - support for the above
**							ua_xh_update_attr() - support for the above
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**         axhclas.c  ,  25.1
**    DATE AND TIME OF LAST  MODIFICATION
**         04/29/15  ,  15:05:42
*********************************************************************/

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) axhclas.c 3.3 8/14/87 09:00:12 single"};
#else
static char uu_sccsident[]={"@(#) axhclas.c 3.3 8/14/87 09:00:12 double"};
#endif


#include "udebug.h"
#include "usysdef.h"
#include "zsysdep.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdattr.h"
#include "mdcoord.h"
#include "adrf.h"
#include "adraft.h"
#include "gobas.h"
#include "goatt.h"
#include "axhatch.h"

#define NONE 0 	/* xhatch material */

/*********************************************************************
**    I_FUNCTION :  ua_disp_xhatch(entity, tfmat)
**					strokes out xhatch lines
**
**    PARAMETERS   
**       INPUT  : 
**						struct UA_generic_draft *entity
**						transformation
**       OUTPUT :  
**						none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_disp_xhatch(entity, tfmat)
struct UA_generic_draft *entity;
UU_REAL                 tfmat[4][3];
{
	struct UA_hatchlin_rec	hatchrec;
	UU_REAL *p;
	struct UM_attrdata_rec	attr;	
	Glntype 	line_style;
	int key;

	uu_denter(UU_STRC,(us,"entering ua_disp_xhatch"));

	/* retrieve data from Unibase */

	hatchrec.key = entity->asso_blk[entity->asso_blk_use - 1].key;
	if(ua_retrieve_xhatch(&hatchrec, sizeof(struct UA_hatchlin_rec)) ==
									UU_FAILURE)
		{
		uu_denter2(UU_STRC,(us,"ua_disp_xhatch: uc_retrieve_xhatch ERROR"));
		uu_dexit;
		goto fexit;
		}
	if (uc_retrieve_attr(hatchrec.key, &attr) != UU_SUCCESS)
	{
		uu_denter2(UU_STRC,(us,"ua_disp_xhatch: uc_retrieve_attr ERROR"));
		uu_dexit;
	}

	/* transform data */

	ua_transf_xhatch(&hatchrec, tfmat, UU_FALSE);

	/* draw lines */

	/* force linestyle to solid for all materials except none */
	if (entity->subtype == NONE)
		line_style.typeno = attr.line_style;
	else
		line_style.typeno = UM_SOLID_LINE;

	line_style.npatn = 0;
	uu_denter2(UU_STRC,(us,"ua_disp_xhatch: setting linestyle %d",
		line_style.typeno));
	uu_dexit;
/*
.....Changed key=0 to key=hatchrec.key
.....To allow plotting to select the
.....correct pen for crosshatching
.....Bobby  -  7/19/91
*/
	key = hatchrec.key;
	us_set_dispattr(key, attr.color, line_style.typeno, (UU_REAL) 1.0, 2);

	for (p=hatchrec.wt; p < (hatchrec.wt + hatchrec.no_wt); p+=6)
	{
		if (gpolyline3(2,p) != 0)
		{
			uu_denter2(UU_STRC,(us,"ua_disp_xhatch: gpolyline3() ERROR!!"));
			uu_dexit;
		}
		/* uu_denter2(UU_STRC,(us,"ua_disp_xhatch: 1st pt %g %g %g",*p,*(p+1),*(p+2)));
		uu_dexit;
		uu_denter2(UU_STRC,(us,"ua_disp_xhatch: 2nd pt %g %g %g",*(p+3),*(p+4),*(p+5)));
		uu_dexit; */
	}
	uu_free(hatchrec.wt);
fexit:
	uu_dexit;
}

	
/*********************************************************************
**    E_FUNCTION :  ua_copy_xhatch(entity1, entity2)
**       Copy a hatchline entity.
**    PARAMETERS   
**       INPUT  : 
**          entity1 - hatchline entity to be copied
**       OUTPUT :  
**          entity2  - copy of the hatchline entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_copy_xhatch(e1, e2)
	struct UA_hatchlin_rec	*e1;	/*	entity to copy */
	struct UA_hatchlin_rec	*e2;	/* db copy */
	{
	UU_KEY_ID 	key;
	struct UM_attrdata_rec	attr;

	uu_denter(UU_STRC,(us,"ua_copy_xhatch(%x)",e1));

	/* setup target entity record */
	e2->rel_num = e1->rel_num;
	e2->no_wt = e1->no_wt;

	/* copy varlist contents into local storage */
	e2->wt = (UU_REAL*) uu_malloc(e2->no_wt * sizeof(UU_REAL));
	if (ur_retrieve_data_varlist(e1->key, 1, e2->wt, 1, e2->no_wt) == -1)
	{
		uu_denter2(UU_STRC,(us,"ua_copy_xhatch: retrieve_data_varlist ERROR"));
		uu_dexit;
	}

	/* put e2 in database */
	ua_create_xhatch(e2, &key);
	uu_free(e2->wt);

	/* update attributes of e2 */
	if (uc_retrieve_attr(e1->key, &attr) != UU_SUCCESS)
	{
		uu_denter2(UU_STRC,(us,"ua_copy_xhatch: uc_retrieve_attr ERROR"));
		uu_dexit;
	}
	attr.key = e2->key;
	ur_update_attr(&attr); 

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ua_retrieve_xhatch(e, size)
**       retrieve a hatchline entity from Unibase
**    PARAMETERS   
**       INPUT  : 
**          	e - hatchline entity to be retrieved
**				size - size of entity
**       OUTPUT :  
**				none
**    RETURNS      : UU_SUCCESS if OK
**    SIDE EFFECTS : none
**    WARNINGS     : calling routine should call uu_free(e.wt)
**				before returning
*********************************************************************/
int ua_retrieve_xhatch(e, size)
struct UA_hatchlin_rec *e;
int size;
{
	int status;

	uu_denter(UU_STRC,(us,"entering ua_retrieve_xhatch(%x)",
		e));

	/* get fixed part of hatchlin_rec out of Unibase */
	if ((status = ur_retrieve_data_fixed(e)) != UU_SUCCESS)
	{
		uu_denter2(UU_STRC,(us,"ua_retrieve_xhatch: retrieve_data_fixed ERROR"));
		uu_dexit;
	}

	/* set up for coordinate array */
	e->wt = (UU_REAL*) uu_malloc(e->no_wt * sizeof(UU_REAL));

	/* fill it up */
	if (ur_retrieve_data_varlist(e->key, 1, e->wt, 1, e->no_wt) == UU_FAILURE)
	{
		status = UU_FAILURE;
		uu_denter2(UU_STRC,(us,"ua_retrieve_xhatch: retrieve_data_varlist ERROR"));
		uu_dexit;
	}
	uu_denter2(UU_STRC,(us,"ua_retrieve_xhatch: e.key=%d .rel_num=%d .no_wt=%d", e->key, e->rel_num, e->no_wt));
	uu_dexit;

	uu_dexit;
	return (status);
}


/*********************************************************************
**    E_FUNCTION :  ua_transf_xhatch (entity, tf, upd)
**       Transform a cross hatch entity
**    PARAMETERS   
**       INPUT  : 
**          entity			entity record
**          tf				transformation matrix
**          upd				update flag (TRUE - update Unibase)
**       OUTPUT :  
**          entity			modified entity record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_transf_xhatch(e, tf, upd)
struct UA_hatchlin_rec	*e;
UU_REAL						tf[4][3];
UU_LOGICAL					upd;
{
	UU_REAL	pt[3];
	UU_REAL  *p;

	uu_denter(UU_STRC,(us,"ua_transf_xhatch"));

	for (p=e->wt; p < (e->wt + e->no_wt); p+=3)
	{
		/* uu_denter2(UU_STRC,(us,"transf_xhatch: before %g", *p));
		uu_dexit; */
		pt[0] = *p; 
		pt[1] = *(p + 1);
		pt[2] = *(p + 2);
		um_cctmtf(pt, tf, pt);
		*p = pt[0];
		*(p + 1) = pt[1];
		*(p + 2) = pt[2];
		/* uu_denter2(UU_STRC,(us,"transf_xhatch: after %g", *p));
		uu_dexit; */
	}

	/* check update flag */

	if( upd == UU_TRUE)
		ua_update_xhatch(e);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ua_create_xhatch(entity, key)
**       Creates the entity given in Unibase.
**    PARAMETERS   
**       INPUT  : 
**          entity - hatchline entity to save in Unibase
**       OUTPUT :  
**          key   - entity key
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_create_xhatch(entity, key)
struct UA_hatchlin_rec 	*entity;
UU_KEY_ID	*key;	
{
	extern UU_KEY_ID UA_drafting_view;
	struct UM_attrdata_rec attr;
	int status;

	uu_denter(UU_STRC,(us,"ua_create_xhatch(%x,)",entity));

	uu_denter2(UU_STRC,(us, "ua_create_xhatch: relnum %d no_wt %d coord1 %g %g %g", entity->rel_num, entity->no_wt, *(entity->wt), *(entity->wt + 1),
	*(entity->wt + 2)));
	uu_dexit; 

	if (ur_create_data(entity) != 0)
	{
		uu_denter2(UU_STRC,(us,"ua_create_xhatch: ur_create_data ERROR"));
		uu_dexit;
	}

	/* get a copy of current attributes */
	/* zbytecp(attr, UM_attr); */
	ur_get_attrmdl(&attr);

	/* update attr with info from UA_xh_defaults */
	attr.key = entity->key;
	ua_xh_update_attr(&attr);

	/* update in Unibase */
	status = ur_update_attr(&attr); 

	status = ur_update_view_key(entity->key, UA_drafting_view);

	*key = entity->key;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ua_translate_xhatch (entity, offset)
**       Translate a cross hatch entity
**    PARAMETERS   
**       INPUT  : 
**          	entity	entity record
**				offset	translation offset
**       OUTPUT :  
**          entity			modified entity record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_translate_xhatch(e, offset)
struct UA_hatchlin_rec	*e;
UM_vector offset;
{
	UU_REAL	pt[3];
	UU_REAL  *p;

	uu_denter(UU_STRC,(us,"ua_translate_xhatch"));
	
	for (p=e->wt; p < (e->wt + e->no_wt); p+=3)
	{
		/* uu_denter2(UU_STRC,(us,"translate_xhatch: before %g", *p));
		uu_dexit; */
		pt[0] = *p; 
		pt[1] = *(p + 1);
		pt[2] = *(p + 2);
		um_vcplvc(pt, offset, pt);
		*p = pt[0];
		*(p + 1) = pt[1];
		*(p + 2) = pt[2];
		/* uu_denter2(UU_STRC,(us,"translate_xhatch: after %g", *p));
		uu_dexit; */
	}
	/* update entity in Unibase */
	ua_update_xhatch(e);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ua_rotate_xhatch (entity, pt, dir, angle, rotmat)
**       rotate a cross hatch entity
**    PARAMETERS   
**       INPUT  : 
**          	entity - entity record
**				pt	- point on rotation axis
**				dir - direction vector
**				angle - rotation angle
**				rotmat - rotation matrix
**       OUTPUT :  
**          entity			modified entity record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_rotate_xhatch(e, pt, dir, angle, rotmat)
struct UA_hatchlin_rec	*e;
UM_coord pt;
UM_vector dir;
UM_angle angle;
UM_transf rotmat;
{
	UU_REAL	point[3];
	UU_REAL  *p;

	uu_denter(UU_STRC,(us,"ua_rotate_xhatch"));
	
	for (p=e->wt; p < (e->wt + e->no_wt); p+=3)
	{
		/* uu_denter2(UU_STRC,(us,"rotate_xhatch: before %g", *p));
		uu_dexit; */
		point[0] = *p; 
		point[1] = *(p + 1);
		point[2] = *(p + 2);
		um_cctmtf(point, rotmat, point);
		*p = point[0];
		*(p + 1) = point[1];
		*(p + 2) = point[2];
		/* uu_denter2(UU_STRC,(us,"rotate_xhatch: after %g", *p));
		uu_dexit; */
	}

	/* update entity in Unibase */
	ua_update_xhatch(e);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ua_update_xhatch (entity)
**       Updates entity in unibase.
**    PARAMETERS   
**       INPUT  : 
**          entity - entity to save
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_update_xhatch(entity)
struct UA_hatchlin_rec 	*entity;		/*	entity to save	*/
{
	struct UM_transf_rec	transfpacket;

	uu_denter(UU_STRC,(us,"ua_update_xhatch(%x)",entity));

	if (ur_update_data(entity) != UU_SUCCESS)
	{
		uu_denter2(UU_STRC,(us,"update_xhatch: ur_update_data ERROR!!"));
		uu_dexit;
	}
	transfpacket.key = entity->key;
	transfpacket.rel_num = UM_TRANSFORM_REL;
	um_tftotf(UM_idmat, transfpacket.tfmat);
	ur_update_transf(&transfpacket);

	uu_dexit;
}


/*********************************************************************
**
**    I_FUNCTION   :   ua_xh_update_attr()
**
**       updates a UM_attrdata_rec structure using the contents of 
**			UA_xh_defaults
**
**    PARAMETERS   
**
**       INPUT  :  UM_attrdata_rec *attr;
**
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : may change contents of the structure ref'd by attr
**
**    WARNINGS     : none
**
*********************************************************************/

ua_xh_update_attr(attr)
struct UM_attrdata_rec *attr;
{
	extern UA_xh_attr_rec UA_xh_defaults;
	char us[150];

	attr->rel_num = UM_ATTR_REL;
	attr->displayable = UM_NEVERDISPLAYABLE;
	attr->selectable = UU_FALSE;
	attr->color = UA_xh_defaults.color[0];
	attr->pen = UA_xh_defaults.pen;

	switch (UA_xh_defaults.linestyle[0])
	{
		case 0:	attr->line_style = 1;	/* Solid */
					break;
		case 1:  attr->line_style = 6;	/* Dashed */
					break;
		case 2:	attr->line_style = 7;	/* Center */
					break;
		case 3:  attr->line_style = 5;	/* Phantom */
					break;
	}
	uu_denter2(UU_STRC,(us,"ua_xh_update_attr: key %d color %d pen %d ls %d disp %d select %d",
		attr->key, attr->color, attr->pen, 
		attr->line_style, attr->displayable, attr->selectable));
	uu_dexit;
}
