
/*********************************************************************
**    NAME         :  m2lnwid.c
**       CONTAINS: routines to draw wide lines
**			um_gpolyline3(attrptr, npt, points)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2lnwid.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:47
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "go.h"
#include "mattr.h"
#include "mdcoord.h"
#include "modef.h"

/*********************************************************************
**    E_FUNCTION     : um_gpolyline3(attrptr, npt, points)
**			Draw a wide polyline if the attribute linewidth is > 0.0;
**			otherwise, draw a normal polyline.
**    PARAMETERS   
**       INPUT  : 
**				attrptr				attribute bundle
**          npt					number of points in polyline
**				points				array of points
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_gpolyline3(attrptr, npt, points)
	struct UM_attrdata_rec *attrptr;
	int npt;
	UM_coord points[];

	{
	UU_REAL linewidth;
	char buf[256];
	UM_coord spt1, ept1;
	UM_coord spt2, ept2;
	UM_vector vec1;
	UM_vector vec2;
	UM_vector bivec;
	UM_vector vpnorm;
	UM_vector widthvec;
	int nint1, nint2;
	int i;
	Gwpoint3 gpt[4];

	uu_denter(UU_MTRC, (us,"um_gpolyline3(attrptr=%x, npt=%d, points=%x)",
		attrptr, npt, points));

	linewidth = attrptr->line_width;
	if (linewidth <= UM_FUZZ)
		{
		gpolyline3(npt, points);
		}
	else
		{
		gsfillcolor(attrptr->color);
		/* calculate the vector perpendicular to the first line segment and
			the view plane normal and of length 1/2 the line width */
	
		um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, vpnorm);
		um_vcmnvc(points[0], points[1], vec1);
		um_unitvc(vec1, vec1);
		um_cross(vec1, vpnorm, widthvec);
		um_unitvc(widthvec, widthvec);
		um_vctmsc(widthvec, linewidth/2.0, widthvec);
	
		/* calculate the start points of the first pair of offset lines */
	
		um_vcplvc(points[0], widthvec, spt1);
		um_vcmnvc(points[0], widthvec, spt2);
	
		/* for each subsequent line segment, calculate the line bisecting the
			two line adjacent line segments, intersect this line with the two
			offset lines, and fill the resulting polygon */
	
		for (i=1; i<npt; i++)
			{
			if (i==(npt-1))
				{
				um_cross(vec1, vpnorm, bivec);
				um_unitvc(bivec, bivec);
				}
			else
				{
				um_vcmnvc(points[i+1], points[i], vec2);
				um_unitvc(vec2, vec2);
				if (um_vcparall(vec1, vec2))
					{
					um_cross(vec2, vpnorm, bivec);
					um_unitvc(bivec, bivec);
					}
				else
					{
					um_vcplvc(vec1, vec2, bivec);
					um_unitvc(bivec, bivec);
					}
				}
			um_ilnln(spt1, vec1, points[i], bivec, &nint1, ept1);
			um_ilnln(spt2, vec1, points[i], bivec, &nint2, ept2);
			gpt[0].x = spt1[0]; gpt[0].y = spt1[1]; gpt[0].z = spt1[2];
			gpt[1].x = ept1[0]; gpt[1].y = ept1[1]; gpt[1].z = ept1[2];
			gpt[2].x = ept2[0]; gpt[2].y = ept2[1]; gpt[2].z = ept2[2];
			gpt[3].x = spt2[0]; gpt[3].y = spt2[1]; gpt[3].z = spt2[2];
			gfillarea3(4, gpt);
			um_vctovc(ept1, spt1);
			um_vctovc(ept2, spt2);
			um_vctmsc(vec2, (UU_REAL) -1.0, vec1);
			}
		}

	uu_dexit;
	}

