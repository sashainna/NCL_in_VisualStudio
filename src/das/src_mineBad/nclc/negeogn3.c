/*********************************************************************
**    NAME         :  negeogn3.c
**       CONTAINS:  Fillet routines (not needed by IGES).
**
**        int ncl_filpts(spt,wpt2,wpt3,fpt,pl,ofd,dis,ier)
**        int ncl_filcvpt(u,pt)
**
**    COPYRIGHT 2000 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       negeogn3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:34
*********************************************************************/

#include "udebug.h"
#include "msrf.h"
#include "mcrv.h"
#include "mgeom.h"
#include "nccs.h"
#include "nclfc.h"

#define NCL_MAX 100
static UM_vector NCL_cpt[NCL_MAX];
static UU_REAL NCL_dpt[NCL_MAX];

/*********************************************************************
**    I_FUNCTION     : S_trouble (ts1,vts,ds,vperps,tf1,vtf,df,vperpf,pt,tolsq)
**       Check if a point is on the "wrong" side of either of two lines.
**       The "right" side of a line is defined by a provided perpendicular.
**    PARAMETERS   
**       INPUT  : 
**          ts1      - Start point of the first line
**          vts      - First line vector.
**          ds       - First line squared length.
**          vperps   - First line horizontal perpendicular.
**          tf1      - Start point of the second line
**          vtf      - second line vector.
**          df       - second line squared length.
**          vperpf   - second line horizontal perpendicular.
**          pt       - point to check
**          tolsq    - squared tolerance.
**       OUTPUT :  none
**    RETURNS      : 
**       UU_TRUE if bad position detected; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_trouble (ts1,vts,ds,vperps,tf1,vtf,df,vperpf,pt,tolsq)
UM_vector vts,vtf,vperps,vperpf;
UM_coord ts1,tf1,pt;
UU_REAL ds,df,tolsq;
{
	UM_vector vdel;
	UU_REAL d,c;

	um_vcmnvc (pt,ts1,vdel);
	d = UM_DOT (vdel,vperps);

	if (d < 0)
	{
		c = UM_DOT (vdel,vts);
		if (c > 0)
		{
			c = c*c;
			if (c < ds)
			{
				d = UM_DOT (vdel,vdel) - c/ds;
				if (d > tolsq) return (UU_TRUE);
			}
		}
	}

	um_vcmnvc (pt,tf1,vdel);
	d = UM_DOT (vdel,vperpf);

	if (d < 0)
	{
		c = UM_DOT (vdel,vtf);
		if (c > 0)
		{
			c = c*c;
			if (c < df)
			{
				d = UM_DOT (vdel,vdel) - c/df;
				if (d > tolsq) return (UU_TRUE);
			}
		}
	}

	return (UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : ncl_filpts(spt,wpt2,wpt3,fpt,pl,ofd,dis,ier)
**       Create B-spline curve offset from a plane between 2 points
**       and slope points. Then calculate points and distances along the
**       curve and store in global arrays NCL_cpt and NCL_dpt.
**    PARAMETERS   
**       INPUT  : 
**          spt      - Start point and vector.
**          wpt1     - First slope point and vector.
**          wpt2     - Second slope point and vector.
**          wpt3     - Third slope point and vector.
**          wpt4     - Fourth slope point and vector.
**          fpt      - Final point and vector.
**          ofd      - offset distance.
**          hgt      - tool height.
**          cpt      - fillet points at the bottom.
**          icnt     - number of fillet points.
**          dtol     - tolerance.
**       OUTPUT :  
**          ier     - Non-zero if error.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_filpts(spt,wpt1,wpt2,wpt3,wpt4,fpt,ofd,hgt,cpt,icp,dtol,ier)
UM_real8 *spt,*wpt1,*wpt2,*wpt3,*wpt4,*fpt,*ofd,*hgt,*cpt,*dtol;
UM_int2 *icp,*ier;
	{
	UU_REAL *pptr=UU_NULL, *tptr=UU_NULL, ofdis, u, du, wt,dis;
	UU_REAL wta[8],pt3[6],a,h,tol,tolsq,hdel,dh,d,ds,df;
	int n1,n2,i,k,m,M,npt,status;
	UU_LOGICAL done;
	UM_coord pp1, pp2, pt1, pt2, px1, px2, *cptr;
	UM_coord tp,bp,ts1,ts2,tf1,tf2;
	UM_vector v1, vv1, vv2, vv3;
	UM_vector vts,vtf,vsf,nvec,vperps,vperpf;
	struct NCL_crvgen_rec seg[2], *segp;
	UM_real8 pt[3],uu;
	UM_int2 i2v0=0, i2v3=3, i2v8=8;
	UM_plane plane;

	*ier = 0;
	status = UU_SUCCESS;
	ofdis = *ofd;
	wta[0] = wta[1] = wta[2] = wta[3] = 
	wta[4] = wta[5] = wta[6] = wta[7] = 1.0;

	h = *hgt;
	tol = *dtol;
	tolsq = tol*tol;
/*
..... calculate data for tool top checking:
..... M - number of heights to check, hdel - height increment
*/
	npt = *icp;
	M = m = 0;
	if (npt > 2)
	{
		hdel = 0.04*(h - ofdis);
		if (hdel < 20*tol)
			hdel = 20*tol;
		else if (hdel > ofdis)
			hdel = ofdis;
		M = (h - ofdis) / hdel + 0.5;
		if (M > 0)
			hdel = (h - ofdis) / M;
	}
/*
.....Make sure that all points lie on the same plane
.....spt,wpt2, and fpt should already be on an XY-plane
.....(due to rotpts routine)
.....So we'll adjust wpt3 to be on the same plane.
.....If not, then the Z-axis value of wpt3 can
.....affect the tool axis vector
.....Bobby - 4/12/02
*/
	nvec[0] = nvec[1] = 0; nvec[2] = 1;
	n1 = 0;
	if (um_plane1(spt,wpt2,fpt,&plane))
	{
		um_proj_pt_on_plane(1,wpt3,&plane,pt3);
		n1 = 1;
		um_vctovc (plane.n,nvec);
/*		um_ilnpln(wpt3,&wpt3[3],plane.p0,plane.n,&n1,pt3);*/
	}
	if (n1 == 0)
	{
		um_vctovc(wpt3,pt3);
	}
	um_vctovc(&wpt3[3],&pt3[3]);
/*
..... calculate data for tool top checking
*/
	if (M > 0)
	{
		for (k = 0; k < 3; k++)
		{
			/* first couple of tool top points */
			ts1[k] = wpt1[k] + h*wpt1[k+3];
			ts2[k] = wpt2[k] + h*wpt2[k+3];
			/* second couple of tool top points */
			tf1[k] = wpt3[k] + h*pt3[k+3];
			tf2[k] = wpt4[k] + h*wpt4[k+3];
			/* tool top vectors */
			vts[k] = ts2[k] - ts1[k];
			vtf[k] = tf2[k] - tf1[k];
		}
		ds = UM_DOT (vts,vts);
		df = UM_DOT (vtf,vtf);
/*
..... vectors perpendicular to tool top lines, projected to the bottom plane and
..... directed inside the fillet
*/
		um_vcmnvc (fpt,spt,vsf);

		um_cross (nvec,vts,vperps);
		d = UM_DOT (vperps,vsf);
		if (d < 0) um_negvc (vperps,vperps);

		um_cross (nvec,vtf,vperpf);
		d = UM_DOT (vperpf,vsf);
		if (d > 0) um_negvc (vperpf,vperpf);
	}
/*
..... Create B_Spline in up tool axis vectors offset by ofd 
*/
Again:
	done = UU_FALSE;
	n1 = 0;
	do
	{
/*
.....Create offset points from all input points
.....(up tool axis vectors)
*/
		um_vctmsc (&spt[3],ofdis,v1);
		um_vcplvc (spt,v1,pt1);
		um_vctmsc (&wpt1[3],ofdis,v1);
		um_vcplvc (wpt1,v1,px1);
		um_vctmsc (&wpt2[3],ofdis,v1);
		um_vcplvc (wpt2,v1,pp1);
		um_vctmsc (&pt3[3],ofdis,v1);
		um_vcplvc (pt3,v1,pp2);
		um_vctmsc (&wpt4[3],ofdis,v1);
		um_vcplvc (wpt4,v1,px2);
		um_vctmsc (&fpt[3],ofdis,v1);
		um_vcplvc (fpt,v1,pt2);
/*
.....Get tangent vectors using work points
.....Since fillet can actually start outside of line segments
.....Thereby reversing tangent vector
*/
		um_vcmnvc (pp1,px1,vv1);
		um_vcmnvc (px2,pp2,vv2);
		um_vcmnvc (pp2,px1,vv3);
		um_unitvc(vv1,vv1);
		um_unitvc(vv2,vv2);
		um_unitvc(vv3,vv3);
		a = um_dot(vv1,vv3);
		if (a >= -.001) done = UU_TRUE;
		else
		{
			n1++;
			if (n1 > 10) done = UU_TRUE;
			ofdis = ofdis * .9;
		}
	} while (!done);
	if (n1 > 10) goto Err;

	segp = seg;
	segp->x = pt1[0];
	segp->y = pt1[1];
	segp->z = pt1[2];
	segp->a = vv1[0];
	segp->b = vv1[1];
	segp->c = vv1[2];
	segp->inv = 1;
	segp++;
	segp->x = pt2[0];
	segp->y = pt2[1];
	segp->z = pt2[2];
	segp->a = vv2[0];
	segp->b = vv2[1];
	segp->c = vv2[2];
	segp->inv = 1;

	status = ncl_interp_rbsp1 (2, seg, 0, &n2, &tptr, &pptr);
	if (status != UU_SUCCESS) goto Err;
/*
..... Create point and distance arrays.
*/
	u = 0.;
	du = NCL_MAX - 1;
	du = 1./du;
	cptr = NCL_cpt;
	NCL_dpt[0] = 0.;
	dis = 0.;
	for (i=0;i<NCL_MAX;i++)
	{
		evrbsp(&i2v3,&i2v8,tptr,wta,pptr,&u,&i2v0,tptr,&tptr[7],pt,&wt);
		if (i>0)
		{
			dis += um_dcccc(pt,cptr);
			NCL_dpt[i] = dis;
			cptr++;
		}
		um_vctovc (pt,cptr);
		u += du;
	}
/*
..... Check that the resulting tool top positions do not lean away from
..... the fillet. If some do, recalculate the curve at a higher level.
*/
	if (n1 == 0 && m < M)
	{
		dh = h/ofdis;
		du = 1./npt;
		uu = 0;

		for (i = 0; i < npt-1; i++)
		{
			uu += du;
			ncl_filcvpt(&uu,pt);
			for (k = 0; k < 3; k++)
			{
				bp[k] = cpt[6*i+k];
				tp[k] = bp[k] + dh*(pt[k] - bp[k]);
			}
/*
..... tp is the tool top corresponding to the tool position that will be
..... calculated by fillin if this curve is accepted
*/
			if (S_trouble (ts1,vts,ds,vperps,tf1,vtf,df,vperpf,tp,tolsq))
			{
				ofdis += hdel;
				m++;
				if (pptr)
				{
					uu_free(pptr); pptr = UU_NULL;
				}
				if (tptr)
				{
					uu_free(tptr); tptr = UU_NULL;
				}
				goto Again;
			}
		}
	}

	goto Done;

Err:;
	if (*ier == 0) *ier = 1;
	status = UU_FAILURE;

Done:;

	if (pptr) uu_free(pptr);
	if (tptr) uu_free(tptr);
	return(status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_filcvpt (u,pt)
**       Return point U ratio along fillet axis slope curve using points
**       and distances in NCL_cpt and NCL_dpt arrays calculated by
**       ncl_filpts().
**    PARAMETERS   
**       INPUT  : 
**          u        - u value on curv3
**       OUTPUT :  
**          pt       - Point at u.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_filcvpt(u,pt)
UM_real8 *u, *pt;
	{
	int i, status;
	UU_REAL d1, den;
	UM_vector vt1;

	status = UU_SUCCESS;
	d1 = *u * NCL_dpt[NCL_MAX - 1];
	for (i=0; i < NCL_MAX && NCL_dpt[i] < d1; i++) ;
	if (i==0) i=1;
	if (i>NCL_MAX-1) i=NCL_MAX-1;
	um_vcmnvc (NCL_cpt[i],NCL_cpt[i-1],vt1);
	den = NCL_dpt[i] - NCL_dpt[i-1];
	if (den <= 0.) den = 1.0;
	d1 = (d1-NCL_dpt[i-1])/den;
	um_vctmsc (vt1,d1,vt1);
	um_vcplvc (NCL_cpt[i-1],vt1,pt);

	return(status);
   }
