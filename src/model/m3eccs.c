
/*********************************************************************
**    NAME         :  m3eccs.
**       CONTAINS: routine to handle coordinate system entities
**			int um_drw43_coordsys(eptr, tfmat, attr)
**			int um_p43_coordsys(eptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3eccs.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:50
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "dasnog.h"
#include "modef.h"
#include "mxxx.h"
#include "mattr.h"
#include "mdattr.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     : int um_drw43_coordsys(eptr, tfmat, attr)
**       Draw a coordinate system in the currently open DIGS
**			display segment.
**    PARAMETERS   
**       INPUT  : 
**          eptr						pointer to a coordinate system entity
**				tfmat						transformation matrix to apply
**				attr						attributes to apply (not used)
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_drw43_coordsys(eptr, tfmat, attr)
	struct UM_coordsys_rec *eptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attr;

	{
	UM_coord origin;
	UM_coord xaxis;
	UM_coord yaxis;
	UM_coord zaxis;

	uu_denter(UU_MTRC,(us,"um_drw43_coordsys(key=%d, tfmat=%x, attr=%x)",
		eptr->key, tfmat, attr));

	um_cctmtf(eptr->origin, tfmat, origin);
	um_vctmtf(eptr->xaxis, tfmat, xaxis);
	um_vctmtf(eptr->yaxis, tfmat, yaxis);
	um_vctmtf(eptr->zaxis, tfmat, zaxis);

	um_set_disp_attr(attr);

	um_drwaxis(origin, xaxis, yaxis, zaxis, (UU_REAL) 1.0);

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : um_p43_coordsys(eptr)
**       Print a coordinate system entity
**    PARAMETERS   
**       INPUT  : 
**          eptr						coordinate system entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_p43_coordsys(eptr)
	struct UM_coordsys_rec *eptr;

	{
	uu_denter(UU_MTRC,(us,"um_p43_coordsys(key=%x)",eptr->key));

	sprintf(UM_sbuf, "COORDSYS: key=%x", eptr->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "name %s", eptr->name);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PFLOAT, "origin", 3, eptr->origin);
	um_p_ary(UM_PFLOAT, "xaxis", 3, eptr->xaxis);
	um_p_ary(UM_PFLOAT, "yaxis", 3, eptr->yaxis);
	um_p_ary(UM_PFLOAT, "zaxis", 3, eptr->zaxis);
	um_p_ary(UM_PFLOAT, "z_depth", 1, &eptr->z_depth);

	uu_dexit;
	return (UU_SUCCESS);
	}

