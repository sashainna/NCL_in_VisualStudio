/*********************************************************************
**    NAME         :  netrimsf3.c
**       CONTAINS:
**             ncl_basesf_to_trimsf
**
**    COPYRIGHT 2014 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       netrimsf3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:55
*********************************************************************/

#include "class.h"
#include "nccs.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdeval.h"
#include "mdattr.h"
#include "mgeom.h"
#include "udebug.h"
#include "nconst.h"

static UU_LOGICAL Scflag=UU_FALSE;
static struct UM_compcrv_rec Scomp;

/*********************************************************************
**    E_FUNCTION     :  ncl_basesf_to_trimsf(basesf,trimsf)
**				Converts a base surface to a trimmed surface, creating
**          both the UV and XYZ boundary curves.
**    PARAMETERS   
**       INPUT  : 
**          basesf   Base surface to create a trimmed surface from.
**       OUTPUT : 
**          trimsf   Created trimmed surface.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID ncl_basesf_to_trimsf(basesf,trimsf)
struct UM_rbsplsrf_rec *basesf;
struct NCL_trimsf_rec *trimsf;
{
	int i;
	UU_KEY_ID uvkey,cvkey;
	UU_REAL uv[4],tol;
	UM_coord spt,ept;
	UM_transf tfmat;
	struct UM_compcrv_rec comp;
	struct UM_cid_rec cid[4];

	static UU_REAL ptloc[5][2]={0.,0., 1.,0., 1.,1., 0.,1., 0.,0.};
/*
.....Initialize composite curve structure
*/
	comp.closdinu = 1;
	comp.arclen = 4.;
	comp.planar = UU_TRUE;
	comp.open = UU_FALSE;
	comp.continuity = 0;
	comp.fcolor = 0;
	comp.t0 = 0.;
	comp.t1 = 1.;
	comp.addflg = 0;
/*
.....Create UV boundary curve as a
.....4 lines of 0,0  1,0  1,1  0,1  0,0
*/
	spt[2] = ept[2] = 0.;
	for (i=0;i<4;i++)
	{
		spt[0] = ptloc[i][0]; spt[1] = ptloc[i][1];
		ept[0] = ptloc[i+1][0]; ept[1] = ptloc[i+1][1];
		cid[i].crvid = utp_store_line("@UN",spt,ept);
		cid[i].reverse = 0;
		cid[i].endparam = (i+1) * .25;
	}
/*
.....Create UV boundary curve
*/
	uvkey = utp_store_composite(&comp,"@UN",cid,4);
	if (uvkey == 0) goto done;
/*
.....Create Trim surf record
*/
	uv[0] = uv[2] = 0.;
	uv[1] = uv[3] = 1.;
	cvkey = 0;
	trimsf->key = utp_store_trimsf(basesf->label,0,basesf->key,uv,&uvkey,&cvkey,
		1,UU_TRUE);
	if (trimsf->key == 0) goto done;
	ncl_retrieve_data_fixed(trimsf);
/*
.....Create XYZ boundary curve
*/
	uc_retrieve_transf(trimsf->key,tfmat);
	tol = .001;
	uio_uvcrv_to_crv(trimsf,tfmat,0,tol,&trimsf->cv_key,0);
	ur_update_data_fixed(trimsf);
/*
.....End of routine
*/
done:;
	return(trimsf->key);
}
