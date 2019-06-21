/*********************************************************************
**    NAME         :  glnwdt.c
**       CONTAINS: routines to draw wide lines
**			ug_dlinewidth(ws,width, line_offset, npt, points)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       glnwdt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:20
*********************************************************************/
#include "gobas.h"
#include "udebug.h"
#include "gtblst.h"
#include "gtblvar4.h"
#include "gtblvar6.h"
#include "gmat4.h"
#include "gdidd.h"
#include "umath.h"


#define VCMNVC(v1,v2,vr) \
 vr[0]  =  v1.x - v2[0];\
 vr[1]  =  v1.y - v2[1];\
 vr[2]  =  v1.z - v2[2];

#define VCMNVC1(v1,v2,vr) \
 vr[0]  =  v1.x - v2.x;\
 vr[1]  =  v1.y - v2.y;\
 vr[2]  =  v1.z - v2.z;

#define VCMNVC2(v1,v2,vr) \
	vr[0]  =  v1[0] - v2[0];\
	vr[1]  =  v1[1] - v2[1];\
	vr[2]  =  v1[2] - v2[2];

#define VCMNVC3(v1,v2,vr) \
	vr[0]  =  v1[0] - v2.x;\
	vr[1]  =  v1[1] - v2.y;\
	vr[2]  =  v1[2] - v2.z;

#define MAG(v1) sqrt(v1[0]*v1[0] + v1[1]*v1[1] + v1[2]*v1[2])

#define CROSS(v1,v2,vr)\
	{\
	int im;\
	Gfloat rv[3];\
\
	rv[0] = (v1[1] *v2[2]) - (v1[2] *v2[1]);\
	rv[1] = (v1[2] *v2[0]) - (v1[0] *v2[2]);\
	rv[2] = (v1[0] *v2[1]) - (v1[1] *v2[0]);\
	for (im = 0; im < 3; im++) vr[im]  =  rv[im];\
	}

#define VCPLVC(v1,v2,vr)\
	vr[0]  =  v1[0] + v2[0];\
	vr[1]  =  v1[1] + v2[1];\
	vr[2]  =  v1[2] + v2[2];

#define VCPLVC1(v1,v2,vr)\
	vr[0]  =  v1.x + v2[0];\
	vr[1]  =  v1.y + v2[1];\
	vr[2]  =  v1.z + v2[2];

#define VCTMSC(vci,sca,vco)\
	{\
	int im;\
	for (im = 0; im < 3; im++) \
		vco[im]  =  sca * vci[im];\
	}

#define TRUE 1
#define FALSE 0

typedef Gfloat point3[3];

/*********************************************************************
**    I_FUNCTION     : ug_dlinewidth(ws, width, line_offset, npt, points)
**			Draw a wide polyline 
**    PARAMETERS   
**       INPUT  : 
**				ws						workstation id
**				width					width of line
**				line_offset       distance between offset lines - on plotters this
**											is set by plotting system.  On all other 
**											devices, this is the nominal line width.
**          npt					number of points in polyline
**				points				array of points
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_dlinewidth(ws,width, line_offset, npt, points)
int ws;
Gfloat width;
Gfloat line_offset;
int npt;
Gnpoint3 *points;
{
	Gfloat spt1[3], ept1[3];
	Gfloat spt2[3], ept2[3];
	Gfloat vec1[3];
	Gfloat vec2[3];
	Gfloat bivec[3];
	Gfloat vpnorm[3];
	Gfloat widthvec[3];
	int nint1, nint2;
	int i,j,num=0;
	Gfloat linewidth;
	Gfloat proj;
	int cnt=0,lno=0;
	Gnpoint3 **opts;
	struct {int id; Gws ws; int n; Gwpoint  *points;} prms;
	Gnpoint3 *wpoints;

	uu_denter(UU_GITRC, (us,"ug_dlinewidth(width=%g, npt=%d, points=%x)",
		width, npt, points));

	linewidth = (width -1.0) * line_offset;

	num = 2 * ((int)(width/2.0));

	opts = (Gnpoint3**)uu_malloc(((int)width) * sizeof(Gnpoint3 *));
	for(i=0;i<num;i++)
		opts[i] = (Gnpoint3*)uu_malloc(npt*sizeof(Gnpoint3));

	/* for each set of two polylines that need to be drawn	*/
	for(lno=0;lno < ((int)width -1) ;lno +=2)
		{


	/* calculate the vector perpendicular to the first line segment and
		the view plane normal and of length 1/2 the line width */

	vpnorm[0] = 0.0; vpnorm[1]=0.0; vpnorm[2]=1.0;

	widthvec[0]=0.0; widthvec[1]=0.0; widthvec[2]=0.0;
	i=0;
	while((i<npt-1)&&(widthvec[0]==0)&&(widthvec[1]==0)&&(widthvec[2]==0))
		{
		VCMNVC1(points[i], points[i+1], vec1);
		vec1[2] = 0.0;
		ug_unitvc(vec1, vec1);
		CROSS(vec1, vpnorm, widthvec);
		ug_unitvc(widthvec, widthvec);
		VCTMSC(widthvec, linewidth/2.0, widthvec);
		i++;
		}

	if(i == npt-1) 
		{
		wpoints = (Gnpoint3 *) uu_malloc(npt * sizeof(Gnpoint3));
		cnt = npt;
		for(i=0;i<npt;i++)
			{
			wpoints[i].x = points[i].x;
			wpoints[i].y = points[i].y;
			wpoints[i].z = points[i].z;
			}
		gslinewidth((UU_REAL) 0.0);
		prms.ws = ws;
		prms.n = cnt;
		output(&prms,wpoints,cnt);
		goto retn;
		}
	/* calculate the start points of the first pair of offset lines */
	VCPLVC1(points[0], widthvec, spt1);
	VCMNVC(points[0], widthvec, spt2);

	opts[lno][0].x = spt1[0]; opts[lno][0].y = spt1[1];opts[lno][0].z = spt1[2];
	opts[lno+1][0].x=spt2[0]; opts[lno+1][0].y=spt2[1];opts[lno+1][0].z=spt2[2];

	/* for each subsequent line segment, calculate the line bisecting the
		two line adjacent line segments, intersect this line with the two
		offset lines */

	for (i=0 ; i<npt; i++)
		{
		if (i==(npt-1))
			{
			CROSS(vec1, vpnorm, bivec);
			ug_unitvc(bivec, bivec);
			}
		else
			{
			int tmp;

			VCMNVC1(points[i+1], points[i], vec2);
			vec2[2] = 0.0;
			ug_unitvc(vec2, vec2);
			proj = vec1[0]*vec2[0]+vec1[1]*vec2[1]+vec1[2]*vec2[2]; /* dot prod */
			if ( (1.0 - fabs(proj)) < 0.001) tmp = TRUE; else tmp = FALSE;
			if (tmp)
				{
				CROSS(vec2, vpnorm, bivec);
				ug_unitvc(bivec, bivec);
				}
			else
				{
				VCPLVC(vec1, vec2, bivec);
				ug_unitvc(bivec, bivec);
				}
			}

		ug_ilnln(spt1, vec1, points[i], bivec, &nint1, ept1);
		ug_ilnln(spt2, vec1, points[i], bivec, &nint2, ept2);

		opts[lno][i].x=ept1[0]; opts[lno][i].y=ept1[1];opts[lno][i].z=ept1[2];
		opts[lno+1][i].x=ept2[0]; opts[lno+1][i].y=ept2[1];
				opts[lno+1][i].z=ept2[2];

		spt1[0] = ept1[0]; spt1[1] = ept1[1]; spt1[2] = ept1[2];
		spt2[0] = ept2[0]; spt2[1] = ept2[1]; spt2[2] = ept2[2];
		VCTMSC(vec2, (UU_REAL) -1.0, vec1);
		}

	linewidth += 2.0*line_offset;
	}

	i= -1;
	cnt=0;

	wpoints = (Gnpoint3 *)uu_malloc((npt * (int)width + 1) * sizeof(Gnpoint3));
	gslinewidth((UU_REAL) 0.0);
	prms.ws = ws;
	if(((int)width) % 2)	/* Draw original line if needed	*/
			{
			for (i=0; i<npt; i++)	
				{
				wpoints[cnt].x=points[i].x;
				wpoints[cnt].y =points[i].y;
				wpoints[cnt++].z = points[i].z;
				}
			output(&prms,wpoints,cnt);
			cnt = 0;
			}

	for(j=0;j<num;j++)
		{
		if(i== -1)
			{
			for (i=0; i<npt; i++)	
				{
				wpoints[cnt].x=opts[j][i].x;
				wpoints[cnt].y =opts[j][i].y;
				wpoints[cnt++].z = opts[j][i].z;
				}
			output(&prms,wpoints,cnt);
			cnt = 0;
			}
		else
			{
			for (i=npt-1; i>=0; i--)	
				{
				wpoints[cnt].x=opts[j][i].x;
				wpoints[cnt].y =opts[j][i].y;
				wpoints[cnt++].z = opts[j][i].z;
				}
			output(&prms,wpoints,cnt);
			cnt = 0;
			}
		uu_free(opts[j]);
		}
	uu_free(opts);
retn:;
	uu_free(wpoints);
	gslinewidth(width);
	uu_dexit;
}

static int output(prms,wpoints, n)
	struct {int id; Gws ws; int n; Gwpoint  *points;} *prms;
	Gnpoint3 *wpoints;
	int n;
	{
	int rasn = 0;
	int reply;
	int i;
	Gnpoint3 p1,p2;

	prms->points = (Gwpoint *) uu_malloc(n * sizeof(Gwpoint));

	for(i=0;i<n-1;i++)
		{
		int onscrn, chg2;
		Gnpoint3 np1,np2;

		ug_clipexp(&wpoints[i], &wpoints[i+1], &np1, &np2, &onscrn, &chg2);

		if (onscrn!=0)          /* at least partially on scrn */
			{
			/* add another raster point (or two) */
			if (rasn==0)           /* pick up 1st point also */
				{
				/* Convert to raster */
/*				(*(ug_gksstli.wsopen[prms->ws].connid)[UG_DNDCDEV])
/*															(&np1,&prms->points[0]);
*/
				prms->points[0].x = np1.x;
				prms->points[0].y = np1.y;
				rasn = 1;
				}
/*			(*(ug_gksstli.wsopen[prms->ws].connid)[UG_DNDCDEV])
/*														(&np2,&prms->points[rasn]);
*/

			prms->points[rasn].x = np2.x;
			prms->points[rasn].y = np2.y;
			rasn++;
			}
		if (chg2==1) /* pos2!=np2, flush the raster polyline buffer*/
			{
			if (rasn>0) 
				{
/*				prms->n = rasn;
/*				(*(ug_gksstli.wsopen[prms->ws].connid)[UG_DPOLYLNRAS])
/*																				(prms, &reply);
*/
				p1.x=prms->points[0].x; p1.y=prms->points[0].y; p1.z=0.0;
				for (i=1; i<rasn; i++)
					{
					p2.x=prms->points[i].x; p2.y=prms->points[i].y; p2.z=0.0;
					ug_linndc(&p1,&p2);		
					p1=p2;
					}
/*				ug_dpolyln(prms,&reply);	*/
				}
			rasn = 0;
			}
		}
	if (rasn>0)            /* dump the polyline raster buffer */
		{
		prms->n = rasn;
/*		(*(ug_gksstli.wsopen[prms->ws].connid)[UG_DPOLYLNRAS])(prms, &reply);
*/
				p1.x=prms->points[0].x; p1.y=prms->points[0].y; p1.z=0.0;
				for (i=1; i<rasn; i++)
					{
					p2.x=prms->points[i].x; p2.y=prms->points[i].y; p2.z=0.0;
					ug_linndc(&p1,&p2);		
					p1=p2;
					}
/*				ug_dpolyln(prms,&reply);		*/
		}
	uu_free(prms->points);
}


/*********************************************************************
**    E_FUNCTION     : ug_ilnln(pt1,uvc1,pt2,uvc2,nint,pt)
**      Intersect two lines which are known to lie in the same plane.
**    PARAMETERS   
**       INPUT  : 
**				pt1            point on line 1
**          uvc1           unit vector along line 1
**          pt2            point on line 2
**          uvc2           vector along line 2
**       OUTPUT :  
**				nint           number of intersection points (=0,1)
**          pt             intersection points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_ilnln(pt1,uvc1,pt2,uvc2,nint,pt)
	Gfloat pt1[3];
	Gfloat uvc1[3];
	Gnpoint3 pt2;
	Gfloat uvc2[3];
	int  *nint;
	Gfloat pt[3];

	{
	Gfloat proj;
	Gfloat t1[3], t2[3];
	Gfloat t;
	int tmp;

	proj = uvc1[0]*uvc2[0]+uvc1[1]*uvc2[1]+uvc1[2]*uvc2[2]; /* dot prod */
	if ( (1.0 - fabs(proj)) < 0.001) tmp = TRUE; else tmp = FALSE;
	if(tmp ==  TRUE)  *nint = 0;
	else
		{
		proj = uvc1[0]*uvc2[0]+uvc1[1]*uvc2[1]+uvc1[2]*uvc2[2]; /* dot prod */
		VCMNVC3(pt1, pt2, t1);
		VCTMSC(uvc2, proj, t2);
		VCMNVC2(t2, uvc1, t2);
		t = t1[0]*t2[0]+t1[1]*t2[1]+t1[2]*t2[2]; /* dot prod */
		t = t / (1 - proj *proj);
		*nint = 1;
		VCTMSC(uvc1, t, pt);
		VCPLVC(pt1, pt, pt);
		}
	}
