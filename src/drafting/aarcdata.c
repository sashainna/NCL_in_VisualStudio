/*********************************************************************
**    NAME         :  aarcdata.c
**       CONTAINS:
**			ua_get_arcdata
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       aarcdata.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:30
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mattr.h"
#include "mcrv.h"
/*********************************************************************
**    I_FUNCTION :  ua_get_arcdata(key,radius,center,spt,ept,length,arcang,svec,nvec)
**       Get information about arc entity.
**    PARAMETERS   
**       INPUT  : 
**				key			unibase key of entity
**       OUTPUT :  
**				radius		arc radius
**				center		arc center point in model coord
**				spt			start point or acr in model coord
**				ept			end point of arc in model coord
**				length		arc length
**				arcang		arc angle from start vector (svec)
**				svec			arc start vector
**				nvec			arc normal vector
**    RETURNS      : UU_FAILURE if any routine called fails
**							UU_SUCCESS if all OK
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_get_arcdata(key,radius,cpt,spt,ept,length,arcang,svec,nvec)
UU_KEY_ID		key;					/* arc entity key */
UM_length		*radius;				/* arc radius */
UM_coord		cpt,spt,ept;		/* center start end points */
UM_length		*length;				/* arc length */
UM_angle		*arcang;				/* arc angle */
UM_vector		svec;					/* arc start vector */
UM_vector		nvec;					/* arc normal vector */
{
	int			status;
	UU_REAL		um_getarclen();
	struct		UM_circle_rec	e;
	UM_transf	tfmat;
	/*----------- begin function code -----------------------------*/
	uu_denter(UU_STRC,(us,"ua_get_arcdata(key:%d)",key));
	e.key = key;
	status = um_get_all_geom(&e,sizeof(struct UM_circle_rec));
	if (status!=UU_SUCCESS)
		return(status);
	*radius = e.radius;						/* get entity record data */
	um_vctovc(e.center,cpt);
	status = um_get_transformation(e.key,tfmat);
	if (status!=UU_SUCCESS)
		return(status);
	status = um_get_endpts(&e,tfmat,spt,ept);	/* get arc end points */
	if (status!=UU_SUCCESS)
		return(status);
	*length = um_getarclen(&e,tfmat);	/* get arc length */
	*arcang = e.dang;							/* get arc angle */
	um_vctovc(e.svec,svec);					/* arc start vector */
	um_vctovc(e.nvec,nvec);					/* arc normal vector */
	uu_dexit;
}
