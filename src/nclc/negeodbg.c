/*********************************************************************
**    NAME         :  negeodbg.c
**    CONTAINS: routines used to offset curves
**       ncl_print_curve (cvpoint,cvtang,title)
**       ncl_print_trian
**       ncl_print_triangles
**       ncl_print_tri
**       ncl_print_ln_2d
**       ncl_print_ln_3d
**       ncl_print_pt_2d
**       ncl_print_pt_3d
**       ncl_print_ln4
**       ncl_print_ln6
**       ncl_print_pv
**       ncl_print_pl
**       ncl_print_gt
**       ncl_print_pol
**    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       negeodbg.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:33
*********************************************************************/

#include "mgeom.h"
#include "nclclip.h"

/*********************************************************************
**    E_FUNCTION     : ncl_print_pt_2d (p0)
**       Prints out a 2D point
**          iact    - commented statement if 0; active if 1
*********************************************************************/
void ncl_print_pt_2d (iact,p0)
int iact;
UM_2Dcoord p0;
{
	UU_REAL xx,yy;

	xx = p0[0]; yy = p0[1];
	if (fabs(xx)<0.0001) xx=0.;
	if (fabs(yy)<0.0001) yy=0.;
	if (iact == 0)
	printf ("$$pt/%g,%g\n",xx,yy);
	else
	printf ("pt/%g,%g\n",xx,yy);
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_pt_3d(pp)
**       Prints out a point
**          iact    - commented statement if 0; active if 1
*********************************************************************/
void ncl_print_pt_3d (iact,pp)
int iact;
UM_coord pp;
{
	UU_REAL xx,yy,zz;

	xx=pp[0];
	yy=pp[1];
	zz=pp[2];
	if (fabs(xx)<0.0001) xx=0.;
	if (fabs(yy)<0.0001) yy=0.;
	if (fabs(zz)<0.0001) zz=0.;
	if (iact == 0)
	printf ("$$pt/%g,%g,%g\n",xx,yy,zz);
	else
	printf ("pt/%g,%g,%g\n",xx,yy,zz);
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_ln6 (xx,yy,zz,xx1,yy1,zz1)
**       Prints out a line
*********************************************************************/
void ncl_print_ln6 (xx,yy,zz,xx1,yy1,zz1)
UU_REAL xx,yy,zz,xx1,yy1,zz1;
{
	UU_REAL dd;

	dd = (xx-xx1)*(xx-xx1)+(yy-yy1)*(yy-yy1)+(zz-zz1)*(zz-zz1);
	if (dd < 0.000001)
	{
		printf ("pt/%g,%g,%g\n",xx,yy,zz);
		printf ("pt/%g,%g,%g\n",xx1,yy1,zz1);
	}
	else
		printf ("ln/%g,%g,%g,%g,%g,%g\n",xx,yy,zz,xx1,yy1,zz1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_ln4 (xx,yy,zz,xx1,yy1,zz1)
**       Prints out a line
*********************************************************************/
void ncl_print_ln4 (xx,yy,xx1,yy1)
UU_REAL xx,yy,xx1,yy1;
{
	UU_REAL dd;

	dd = (xx-xx1)*(xx-xx1)+(yy-yy1)*(yy-yy1);
	if (dd < 0.000001)
	{
		printf ("pt/%g,%g\n",xx,yy);
		printf ("pt/%g,%g\n",xx1,yy1);
	}
	else
		printf ("ln/%g,%g,%g,%g\n",xx,yy,xx1,yy1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_ln_3d (p0,p1)
**       Prints out a 3D line
*********************************************************************/
void ncl_print_ln_3d (p0,p1)
UM_coord p0,p1;
{
	UU_REAL xx,yy,zz,xx1,yy1,zz1;

	xx=p0[0];
	yy=p0[1];
	zz=p0[2];
	if (fabs(xx)<0.0001) xx=0.;
	if (fabs(yy)<0.0001) yy=0.;
	if (fabs(zz)<0.0001) zz=0.;
	xx1=p1[0];
	yy1=p1[1];
	zz1=p1[2];
	if (fabs(xx1)<0.0001) xx1=0.;
	if (fabs(yy1)<0.0001) yy1=0.;
	if (fabs(zz1)<0.0001) zz1=0.;

	ncl_print_ln6 (xx,yy,zz,xx1,yy1,zz1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_ln_2d (p0,p1)
**       Prints out a 2D line
*********************************************************************/
void ncl_print_ln_2d (p0,p1)
UM_2Dcoord p0,p1;
{
	UU_REAL xx,yy,xx1,yy1;

	xx = p0[0]; yy = p0[1];
	xx1 = p1[0]; yy1 = p1[1];
	if (fabs(xx)<0.0001) xx=0.;
	if (fabs(yy)<0.0001) yy=0.;
	if (fabs(xx1)<0.0001) xx1=0.;
	if (fabs(yy1)<0.0001) yy1=0.;

	ncl_print_ln4 (xx,yy,xx1,yy1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_curve (cvpoint,cvtang,title)
**       Prints out the requested curve.
**    PARAMETERS
**       INPUT  :
**          cvpoint    list of points
**          cvtang     list of tangent vectors
**          title      Text to print prior to curve.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_print_curve (cvpoint,cvtang,title)
UU_LIST *cvpoint,*cvtang;
char *title;
{
	UU_REAL xx,yy,zz,xx1,yy1,zz1,aa,bb,cc,dd;
	UM_coord *pp;
	UM_vector *vv;
	int i,npts;
/*
.....Print out the curve
*/
	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	if (cvtang != UU_NULL)
	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
	npts = cvpoint->cur_cnt;
	printf ("\n\n%% %s \n",title);

	for (i = 0; i < npts-1; i++)
	{
		xx=pp[i][0];
		yy=pp[i][1];
		zz=pp[i][2];
		if (fabs(xx)<0.0001) xx=0.;
		if (fabs(yy)<0.0001) yy=0.;
		if (fabs(zz)<0.0001) zz=0.;
		xx1=pp[i+1][0];
		yy1=pp[i+1][1];
		zz1=pp[i+1][2];
		if (fabs(xx1)<0.0001) xx1=0.;
		if (fabs(yy1)<0.0001) yy1=0.;
		if (fabs(zz1)<0.0001) zz1=0.;
		if (cvtang != UU_NULL)
		{
			dd = UM_DOT(vv[i],vv[i]);
			if (dd > 0.0001)
			{
				dd = sqrt (dd);
				aa = vv[i][0]/dd;
				bb = vv[i][1]/dd;
				cc = vv[i][2]/dd;
				if (fabs(aa)<0.0001) aa=0.;
				if (fabs(bb)<0.0001) bb=0.;
				if (fabs(cc)<0.0001) cc=0.;
				if (dd > 10000)
				{
					printf ("$$LONG VECTOR\n");
					printf ("draft/modify,color=cyan\n");
					printf ("pv/%g,%g,%g,%g,%g,%g\n",xx,yy,zz,aa,bb,cc);
					printf ("draft/modify,color=defalt\n");
				}
				else
				{
					printf ("$$pv/%g,%g,%g,%g,%g,%g\n",xx,yy,zz,aa,bb,cc);
				}
			}
		}
		ncl_print_ln6 (xx,yy,zz,xx1,yy1,zz1);
	}
			
	if (cvtang != UU_NULL)
	{
		i = npts - 1;
		dd = UM_DOT(vv[i],vv[i]);
		if (dd > 0.0001)
		{
			dd = sqrt (dd);
			aa = vv[i][0]/dd;
			bb = vv[i][1]/dd;
			cc = vv[i][2]/dd;
			if (fabs(aa)<0.0001) aa=0.;
			if (fabs(bb)<0.0001) bb=0.;
			if (fabs(cc)<0.0001) cc=0.;
			if (dd > 10000)
			{
				printf ("$$LONG VECTOR\n");
				printf ("draft/modify,color=cyan\n");
				printf ("pv/%g,%g,%g,%g,%g,%g\n",xx1,yy1,zz1,aa,bb,cc);
				printf ("draft/modify,color=defalt\n");
			}
			else
			{
				printf ("$$pv/%g,%g,%g,%g,%g,%g\n",xx1,yy1,zz1,aa,bb,cc);
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_curve (cvpoint,cvtang,title)
**       Prints out the requested curve.
**    PARAMETERS
**       INPUT  :
**          cvpoint    list of points
**          cvtang     list of tangent vectors
**          title      Text to print prior to curve.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_print_curve_2d (cvpoint,title)
UU_LIST *cvpoint;
char *title;
{
	UM_2Dcoord *pp;
	int i,npts;
/*
.....Print out the curve
*/

	pp = (UM_2Dcoord *) UU_LIST_ARRAY(cvpoint);
	npts = cvpoint->cur_cnt;
	printf ("\n\n%% %s \n",title);
	printf ("$$%d points\n",npts);

	for (i = 0; i < npts-1; i++)
	{
		ncl_print_ln_2d (pp[i],pp[i+1]);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_closed (pp,npts,title)
**       Prints out a closed polyline.
**    PARAMETERS
**       INPUT  :
**          cvpoint    list of points
**          cvtang     list of tangent vectors
**          title      Text to print prior to curve.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_print_closed (pp,npts,title)
UM_coord *pp;
int npts;
char *title;
{
	UU_REAL xx,yy,zz,xx1,yy1,zz1;
	int i;
/*
.....Print out the curve
*/
	printf ("\n\n%% %s \n",title);
	for (i = 0; i < npts-1; i++)
	{
		xx=pp[i][0];
		yy=pp[i][1];
		zz=pp[i][2];
		if (fabs(xx)<0.0001) xx=0.;
		if (fabs(yy)<0.0001) yy=0.;
		if (fabs(zz)<0.0001) zz=0.;
		xx1=pp[i+1][0];
		yy1=pp[i+1][1];
		zz1=pp[i+1][2];
		if (fabs(xx1)<0.0001) xx1=0.;
		if (fabs(yy1)<0.0001) yy1=0.;
		if (fabs(zz1)<0.0001) zz1=0.;
		ncl_print_ln6 (xx,yy,zz,xx1,yy1,zz1);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_closed_2d (pp,npts)
**       Prints out a closed 2D polyline.
*********************************************************************/
void ncl_print_closed_2d (pp,npts)
UM_2Dcoord *pp;
int npts;
{
	int i,i1;
/*
.....Print out the curve
*/
	printf ("\n\n%% %d points\n",npts);
	for (i = 0; i < npts; i++)
	{
		i1 = (i + 1 + npts)%npts;
		ncl_print_ln_2d (pp[i],pp[i1]);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_pv (p0,p1)
**       Prints out a point-vector
*********************************************************************/
void ncl_print_pv (p0,p1)
UM_coord p0;
UM_vector p1;
{
	UU_REAL xx,yy,zz,xx1,yy1,zz1;

	xx=p0[0];
	yy=p0[1];
	zz=p0[2];
	if (fabs(xx)<0.0001) xx=0.;
	if (fabs(yy)<0.0001) yy=0.;
	if (fabs(zz)<0.0001) zz=0.;
	xx1=p1[0];
	yy1=p1[1];
	zz1=p1[2];
	if (fabs(xx1)<0.0001) xx1=0.;
	if (fabs(yy1)<0.0001) yy1=0.;
	if (fabs(zz1)<0.0001) zz1=0.;
	printf ("pv/%g,%g,%g,%g,%g,%g\n",xx,yy,zz,xx1,yy1,zz1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_pl (p,v)
**       Prints out a plane
*********************************************************************/
void ncl_print_pl (p0,vc)
UM_coord p0,vc;
{
	UU_REAL xx,yy,zz,xx1,yy1,zz1;

	xx=p0[0];
	yy=p0[1];
	zz=p0[2];
	if (fabs(xx)<0.0001) xx=0.;
	if (fabs(yy)<0.0001) yy=0.;
	if (fabs(zz)<0.0001) zz=0.;
	xx1=vc[0];
	yy1=vc[1];
	zz1=vc[2];
	if (fabs(xx1)<0.0001) xx1=0.;
	if (fabs(yy1)<0.0001) yy1=0.;
	if (fabs(zz1)<0.0001) zz1=0.;
	printf ("$$pl/(pv/%g,%g,%g,%g,%g,%g)\n",xx,yy,zz,xx1,yy1,zz1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_print_gt (p0,p1)
**       Debug: Print out a GOTO statement
*********************************************************************/
void ncl_print_gt (p0,p1)
UM_coord p0;
UM_vector p1;
{
	UU_REAL xx,yy,zz,xx1,yy1,zz1;

		xx=p0[0];
		yy=p0[1];
		zz=p0[2];
		if (fabs(xx)<0.0001) xx=0.;
		if (fabs(yy)<0.0001) yy=0.;
		if (fabs(zz)<0.0001) zz=0.;
		xx1=p1[0];
		yy1=p1[1];
		zz1=p1[2];
		if (fabs(xx1)<0.0001) xx1=0.;
		if (fabs(yy1)<0.0001) yy1=0.;
		if (fabs(zz1)<0.0001) zz1=0.;

		printf ("gt/%g,%g,%g,%g,%g,%g\n",xx,yy,zz,xx1,yy1,zz1);
}

/*********************************************************************
** FUNCTION : void ncl_print_trian (trian)
**   Debug routine - print the triangle
*********************************************************************/
void ncl_print_trian (trian)
UM_trian *trian;
{
	printf ("\n");
	ncl_print_ln_3d (trian->p1,trian->p2);
	ncl_print_ln_3d (trian->p2,trian->p3);
	ncl_print_ln_3d (trian->p3,trian->p1);
}

/*********************************************************************
** FUNCTION : void ncl_print_trians (trians)
**   Debug routine - print the list of triangles (short struct - only points)
*********************************************************************/
void ncl_print_trians (trians)
UU_LIST *trians;
{
	UM_trian *trian;
	int n,i;

	n = trians->cur_cnt;

	trian = (UM_trian *) UU_LIST_ARRAY (trians);

	printf ("\n\n%% %d triangles\n",n);

	for (i = 0; i < n; i++)
	{
		ncl_print_trian (&trian[i]);
	}
}

/*********************************************************************
** FUNCTION : void ncl_print_triangle (triangle)
**   Debug routine - print the triangle
*********************************************************************/
void ncl_print_triangle (itsk,trian)
int itsk;
UM_triangle *trian;
{
	printf ("\n");
	ncl_print_ln_3d (trian->p1,trian->p2);
	ncl_print_ln_3d (trian->p2,trian->p3);
	ncl_print_ln_3d (trian->p3,trian->p1);

	if (itsk == 1)
	{
		printf ("\n");
		ncl_print_pv (trian->p1,trian->norm1);
		ncl_print_pv (trian->p2,trian->norm2);
		ncl_print_pv (trian->p3,trian->norm3);
	}
}

/*********************************************************************
** FUNCTION : void ncl_print_triangles (triangles)
**   Debug routine - print the list of triangles (long struct - points + norms)
*********************************************************************/
void ncl_print_triangles (itsk,triangles)
int itsk;
UU_LIST *triangles;
{
	UM_triangle *trian;
	int n,i;

	n = triangles->cur_cnt;

	trian = (UM_triangle *) UU_LIST_ARRAY (triangles);

	printf ("\n\n%% %d triangles\n",n);

	for (i = 0; i < n; i++)
	{
		ncl_print_triangle (itsk,&trian[i]);
	}
}

/*********************************************************************
** FUNCTION : void ncl_print_trian_uv (tript,uv)
**   Debug routine - print the triangle
*********************************************************************/
void ncl_print_trian_uv (tript,uv)
UM_tript *tript;
UM_2Dcoord *uv;
{
	int n1,n2,n3;
	UM_2Dcoord uv0;

	n1 = tript->n1; n2 = tript->n2; n3 = tript->n3;
	uv0[0] = (uv[n1][0] + uv[n2][0] + uv[n3][0])/3;
	uv0[1] = (uv[n1][1] + uv[n2][1] + uv[n3][1])/3;

	printf ("\n");
	ncl_print_ln_2d (uv[n1],uv[n2]);
	ncl_print_ln_2d (uv[n2],uv[n3]);
	ncl_print_ln_2d (uv[n3],uv[n1]);
	ncl_print_pt_2d (1,uv0);
}

/*********************************************************************
** FUNCTION : void ncl_print_pol (q)
**   Debug routine - print polygon contours
*********************************************************************/
void ncl_print_pol (q)
ncl_polygon *q;
{
	int nc,c,nv,iv,nv1;
	UU_REAL xx,yy,zz,xx1,yy1,zz1,dd;
	UM_2Dcoord *pp;

	zz = zz1 = 0;

	nc = q->num_contours;
	printf ("Num_contours =  %d\n",nc);

	if (nc > 0)
	{
		pp = (UM_2Dcoord *) UU_LIST_ARRAY (q->contour);
		for (c = 0; c < nc; c++)
		{
			nv = abs(q->np[c]);
			dd = UM_SQDIS_2D (pp[0],pp[nv-1]);
			if (dd < 0.000001)
				nv1 = nv-1;
			else
				nv1 = nv;

			printf ("\n");
			printf ("$$%d points\n",q->np[c]);

			for (iv = 0; iv < nv1; iv++)
			{
				xx=pp[iv][0];
				yy=pp[iv][1];
				if (fabs(xx)<0.0001) xx=0.;
				if (fabs(yy)<0.0001) yy=0.;
				if (iv < nv-1)
				{
					xx1=pp[iv+1][0];
					yy1=pp[iv+1][1];
					if (fabs(xx1)<0.0001) xx1=0.;
					if (fabs(yy1)<0.0001) yy1=0.;
				}
				else
				{
					xx1=pp[0][0];
					yy1=pp[0][1];
					if (fabs(xx1)<0.0001) xx1=0.;
					if (fabs(yy1)<0.0001) yy1=0.;
				}
				ncl_print_ln4 (xx,yy,xx1,yy1);
			}
			pp += nv;
		}
		printf ("\n");
	}

	return;
}
