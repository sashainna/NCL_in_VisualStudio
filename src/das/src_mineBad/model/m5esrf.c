/*********************************************************************
**    NAME         :  m5esrf.c
**       CONTAINS: AG surface support routines
**			int um_agsrf_delete(key)
**			int um_agsrf_feature(eptr, tfmat, feature, dploc)
**			int um_agsrf_print(eptr)
**			int um_agsrf_draw(eptr, tfmat, attrptr)
**			int um_agsrf_transform(eptr, tfmat, store)
**			int um_agsrf_copy(e1ptr, e2ptr, bagsize)
**			int um_agsrf_evaluate(evflag, u, v, eptr, tfmat, evsrf)
**			int um_agsrf_revsrf(full_srf, axis_point, axis_vector, 
**				start_angle, end_angle, unicrv, agsrf)
**			int um_agsrf_tabcyl(dir, unicrv)
**			int um_agsrf_rulsrf(unicrv1, unicrv2, agsrf)
**			int um_agsrf_tessellate(srf)
**			int um_tessellate(n, pt0, norm0, pt1, norm1, rev_normal)
**			int um_agsrf_4bndycrvs(unicrv1, unicrv2, agsrf)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       m5esrf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:06
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "go.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mdeval.h"
#include "modef.h"
#include "mdebug.h"

extern UM_transf UM_bez_draw_mat;

/*********************************************************************
**    E_FUNCTION     : int um_tessellate(n, pt0, norm0, pt1, norm1, rev_normal)
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tessellate(n, pt0, norm0, pt1, norm1, rev_normal)
	int n;
	UU_REAL pt0[][3];
	UU_REAL norm0[][3];
	UU_REAL pt1[][3];
	UU_REAL norm1[][3];
	UU_LOGICAL rev_normal;

	{
	int i, j;
	Gwpoint3 points[4];
	Gwpoint3 norms[4];

	uu_denter(UU_MTRC, (us, "um_tessellate(n=%d, pt0=%x, norm0=%x, pt1=%x, norm1=%x)",
		n, pt0, norm0, pt1, norm1));

	for (i=0; i<(n-1); i++)
		{
		points[0].x = pt0[i][0];
		points[0].y = pt0[i][1];
		points[0].z = pt0[i][2];

		points[1].x = pt0[i+1][0];
		points[1].y = pt0[i+1][1];
		points[1].z = pt0[i+1][2];
	
		points[2].x = pt1[i+1][0];
		points[2].y = pt1[i+1][1];
		points[2].z = pt1[i+1][2];

		points[3].x = pt1[i][0];
		points[3].y = pt1[i][1];
		points[3].z = pt1[i][2];
	
		norms[0].x = norm0[i][0];
		norms[0].y = norm0[i][1];
		norms[0].z = norm0[i][2];

		norms[1].x = norm0[i+1][0];
		norms[1].y = norm0[i+1][1];
		norms[1].z = norm0[i+1][2];
	
		norms[2].x = norm1[i+1][0];
		norms[2].y = norm1[i+1][1];
		norms[2].z = norm1[i+1][2];

		norms[3].x = norm1[i][0];
		norms[3].y = norm1[i][1];
		norms[3].z = norm1[i][2];

		if (rev_normal) for (j=0; j<4; j++)
			{
			norms[j].x = -norms[j].x;
			norms[j].y = -norms[j].y;
			norms[j].z = -norms[j].z;
			}

/*
		for (j=0; j<4; j++)
			{
			sprintf(UM_sbuf, "pt[%d]=(%f,%f,%f)", j,
				points[j].x, points[j].y, points[j].z);
			um_pscroll(UM_sbuf);
			sprintf(UM_sbuf, "norms[%d]=(%f,%f,%f)", j,
				norms[j].x, norms[j].y, norms[j].z);
			um_pscroll(UM_sbuf);
			}
*/
	
		gfillareanorm3( 4, points, norms );

		}

	uu_dexit;
	}

