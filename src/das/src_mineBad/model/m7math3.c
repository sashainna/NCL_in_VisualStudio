/*********************************************************************
**    NAME         :  m7math3.c
**       CONTAINS:
**           int QLAlgorithm (A,diag,subd)
**           void Tridiagonal3 (A,diag,subd,ldet)
**           int um_eigen (A,w)
**           int um_ptstopln (npt,pts,plpt,plvc,tol)
**           int um_gcd(a,b)
**           int um_lcm(a,b)
**	          um_transform_evcrvout
**           um_polygon_orient3D
**           um_polygon_orientation3D
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       m7math3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:09
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "mdcoord.h"
#include "modef.h"
#include "udebug.h"
#include "mdeval.h"
#include "ulist.h"

typedef UU_REAL UM_sqmatr[3][3];

/*********************************************************************
**    E_FUNCTION     : QLAlgorithm (A,diag,subd)
**       For a symmetric tridiagnal matrix T calculate eigenvalues and
**       corresponding eigenvectors.
**    PARAMETERS
**       INPUT  :
**          A                  initial orthogonal matrix
**          diag               the diagonal elements of T
**          subd               the subdiagonal elements of T
**       OUTPUT :
**          A                  final orthogonal matrix
**          diag               the diagonal elements of D
**    RETURNS      : 0 iff success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int QLAlgorithm (A,diag,subd)
UM_sqmatr A;
UU_REAL diag[],subd[];
{
	int i0,i1,i2,i3,i4;
	int MaxIter = 32;
	UU_REAL si,co,B,F,G,P,R;

	for (i0 = 0; i0 < 3; i0++)
	{
		for (i1 = 0; i1 < MaxIter; i1++)
		{
			for (i2 = i0; i2 <= 1; i2++)
			{
				if (fabs(subd[i2]) < UM_DFUZZ) break;
			}
			if (i2 == i0) break;

			G = (diag[i0+1] - diag[i0])/(2.0 * subd[i0]);
			R = sqrt(G*G+1.0);
			if (G < 0.0)
			{
				G = diag[i2]-diag[i0]+subd[i0]/(G-R);
			}
			else
			{
				G = diag[i2]-diag[i0]+subd[i0]/(G+R);
			}
			si = 1.0, co = 1.0, P = 0.0;
			for (i3 = i2-1; i3 >= i0; i3--)
			{
				F = si*subd[i3];
				B = co*subd[i3];
				if (fabs(F) >= fabs(G))
				{
					co = G/F;
					R = sqrt(co*co+1.0);
					subd[i3+1] = F*R;
					si = 1.0/R;
					co *= si;
				}
				else
				{
					si = F/G;
					R = sqrt(si*si+1.0);
					subd[i3+1] = G*R;
					co = 1.0/R;
					si *= co;
				}
				G = diag[i3+1]-P;
				R = (diag[i3]-G)*si+2.0*B*co;
				P = si*R;
				diag[i3+1] = G+P;
				G = co*R-B;

				for (i4 = 0; i4 < 3; i4++)
				{
					F = A[i4][i3+1];
					A[i4][i3+1] = si*A[i4][i3]+co*F;
					A[i4][i3] = co*A[i4][i3]-si*F;
				}
			}
			diag[i0] -= P;
			subd[i0] = G;
			subd[i2] = 0.0;
		}
		if (i1 == MaxIter) return (-1);
	}

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : void Tridiagonal3 (A,diag,subd,ldet)
**       Reduce a symmetric matrix A to the tridiagonal form.
**       We calculate A = Q * T * Q^(-1), where T is symmetric tridiagonal,
**       and Q is orthogonal.
**    PARAMETERS
**       INPUT  :
**          A                  symmetric matrix
**       OUTPUT :
**          A                  the orthogonal matrix (overwrites the original A)
**          diag               the diagonal elements of T
**          subd               the subdiagonal elements of T
**          ldet               the determinant of Q (1 or -1)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void Tridiagonal3 (A,diag,subd,ldet)
UM_sqmatr A;
UU_REAL diag[],subd[];
int *ldet;
{
	UU_REAL a00,a01,a02,a11,a12,a22;
	UU_REAL d,dinv,u,v,q;

	a00 = A[0][0];
	a01 = A[0][1];
	a02 = A[0][2];
	a11 = A[1][1];
	a12 = A[1][2];
	a22 = A[2][2];

	diag[0] = a00;
	subd[2] = 0.0;
	if (fabs(a02) > UM_DFUZZ)
	{
		d = sqrt(a01*a01+a02*a02);
		dinv = 1.0/d;
		u = a01 * dinv;
		v = a02 * dinv;
		q = 2.0*u*a12 + v*(a22-a11);

		diag[1] = a11+v*q;
		diag[2] = a22-v*q;

		subd[0] = d;
		subd[1] = a12-u*q;

		A[0][0] = 1.0;
		A[0][1] = 0.0;
		A[0][2] = 0.0;
		A[1][0] = 0.0;
		A[1][1] = u;
		A[1][2] = v;
		A[2][0] = 0.0;
		A[2][1] = v;
		A[2][2] = -u;

		*ldet = -1;
	}
	else
	{
		diag[1] = a11;
		diag[2] = a22;
		subd[0] = a01;
		subd[1] = a12;

		A[0][0] = 1.0;
		A[0][1] = 0.0;
		A[0][2] = 0.0;
		A[1][0] = 0.0;
		A[1][1] = 1.0;
		A[1][2] = 0.0;
		A[2][0] = 0.0;
		A[2][1] = 0.0;
		A[2][2] = 1.0;

		*ldet = 1;
	}
}

/*********************************************************************
**    E_FUNCTION     : um_eigen (A,w)
**       For a symmetric nonnegative matrix A calculate eigenvalues and
**       corresponding eigenvectors.
**    PARAMETERS
**       INPUT  :
**          A                  matrix
**       OUTPUT :
**          A                  the matrix whose columns are eigenvectors
**          w                  the vector of eigenvalues
**    RETURNS      : 0 iff success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int um_eigen (A,w)
UM_sqmatr A;
UM_vector w;
{
	UU_REAL subd[3];
	int status,ldet;

/*
..... We calculate the represenation A = U * D * U^(-1),
..... where U is orthogonal, and D = diag(w0,w1,w2).
..... Then we overwrite the matrix A by U.
*/
	Tridiagonal3(A,w,subd,&ldet);
	status = QLAlgorithm(A,w,subd);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : um_ptstopln (npt,pts,plpt,plvc,tol)
**       Calculate the best fitting plane for a set of points.
**    PARAMETERS
**       INPUT  :
**          pts                points
**          npt                number of points
**          tol                tolerance
**       OUTPUT :
**          plpt               coordinates of the center point
**          plvc               plane normal
**    RETURNS      :   0 iff success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_ptstopln (npt,pts,plpt,plvc,tol)
UU_REAL tol;
int npt;
UM_coord *pts, plpt;
UM_vector plvc;
{
	UU_REAL tpn,xx,xy,xz,yy,yz,zz,d;
	UM_sqmatr A;
	int i,j,k,status;
	UM_vector pve,vt;

	um_nullvc (plpt);
	um_nullvc (pve);
/*
..... calculate the center point
*/
	for (i = 0; i < npt; i++)
	{
		um_vcplvc (plpt,pts[i],plpt);
	}

	tpn = 1./npt;
	um_vctmsc (plpt,tpn,plpt);

	xx = xy = xz = yy = yz = zz = 0;
	for (i = 0; i < npt; i++)
	{
		um_vcmnvc (pts[i],plpt,vt);
		xx += vt[0]*vt[0];
		xy += vt[0]*vt[1];
		xz += vt[0]*vt[2];
		yy += vt[1]*vt[1];
		yz += vt[1]*vt[2];
		zz += vt[2]*vt[2];
	}

	xx *= tpn;
	xy *= tpn;
	xz *= tpn;
	yy *= tpn;
	yz *= tpn;
	zz *= tpn;

	A[0][0] = xx;
	A[0][1] = A[1][0] = xy;
	A[0][2] = A[2][0] = xz;
	A[1][1] = yy;
	A[1][2] = A[2][1] = yz;
	A[2][2] = zz;

	status = um_eigen (A,vt);
	if (status != 0) return (status);

	if (vt[0] <= vt[1] && vt[0] <= vt[2])
		j = 0;
	else if (vt[1] <= vt[0] && vt[1] <= vt[2])
		j = 1;
	else
		j = 2;

	if (vt[j] < -UM_FUZZ) return (-1);
	if (vt[j] < UM_FUZZ)
	{
		for (k = 0; k < 3; k++)
		{
			if (k != j && vt[k] < UM_DFUZZ) 
				return (-1);
		}
	}

	for (k = 0; k < 3; k++)
		pve[k] = A[k][j];

	d = UM_DOT (pve,pve);

	if (d < tol*tol) return (-1);

	d = 1./sqrt(d);
	if (j == 1 && !ncl_setver(95)) d = -d;

	um_vctmsc (pve,d,plvc);

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_gcd(a,b)
**       Calculate the greatest common denominator of two numbers.
********************************************************************/
int um_gcd(a,b)
int a,b;
{
	int r;

	if (a < 2 || b < 2) return (1);
	r = a%b;
	while (r != 0)
	{
		a = b;
		b = r;
		r = a%b;
	}
	return (b);
}

/*********************************************************************
**    E_FUNCTION     : um_lcm(a,b)
**       Calculate the least common multiple of two numbers.
********************************************************************/
int um_lcm(a,b)
int a,b;
{
	int m = 1;

	if (a < 1 || b < 1) return (m);
	m = a*b/um_gcd(a,b);
	return (m);
}

/*********************************************************************
**    E_FUNCTION : um_transform_evcrvout(evflag, eptr, tfmat, crvoutptr)
**			Apply the transformation matrix (TFMAT) to all of the defined
**			fields in the curve evaluator record (EVOUTPTR).
**    PARAMETERS   
**       INPUT  : 
**          evflag		UM_POINT=>		point
**								UM_FRSTFERIV=> point, 1st deriv
**								UM_SECDETIV=>	point, 1st deriv, 2nd deriv
**								UM_CURVATURE=>	point, 1st deriv, 2nd deriv,
**													curvature
**				u				parameter value to evaluate curve function
**				crvptr		pointer to curve entity
**				tfmat			transformation matrix
**       OUTPUT :  
**          crvoutptr	curve evaluator record to put results
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_transform_evcrvout(evflag, eptr, tfmat, crvoutptr)
	int evflag;
	struct UM_entitydatabag *eptr;
	UM_transf tfmat;
	struct UM_evcrvout *crvoutptr;

	{
	UM_transf temptf;
	UU_REAL numerator, denom;
	UM_vector top;
	UU_LOGICAL um_is_idmat();

	uu_denter(UU_MTRC,
		(us,"um_transform_evcrvout(evflag:%d,eptr->key:%d,tfmat:%x,crvoutptr:%x)",
		evflag, eptr->key, tfmat, crvoutptr));
		
	if (!um_is_idmat(tfmat))
		{
		um_cctmtf(crvoutptr->cp, tfmat, crvoutptr->cp); 
		if (evflag >= UM_FRSTDERIV)
			{
			um_nodisptf(tfmat, temptf);
			um_vctmtf(crvoutptr->dcdu, temptf, crvoutptr->dcdu);
			if (evflag >= UM_SECDERIV)
				{
				um_vctmtf(crvoutptr->d2cdu2, temptf, crvoutptr->d2cdu2);
				if (evflag >= UM_CURVATURE)
				/* we calculate curvature directly here in terms of the 
				 * transformed components of the first and second derivative; 
				 * namely, by using the formula: 
			 	 *               |FRSTDERIV X SECONDERIV|
			 	 *  curvature =  ------------------------
			 	 *     (sum of the squares of the components of FRSTDERIV)**(3/2)
			 	 */
/*
..... vp 11/22/96: here I fixed the problem which is obvious if you look at 
.....              old source line.  
*/
					{
/*					um_cross(crvoutptr->dcdu, crvoutptr->d2cdu2, numerator);  */
					um_cross(crvoutptr->dcdu, crvoutptr->d2cdu2, top);
					numerator = um_mag(top);
					denom = crvoutptr->dcdu[0]*crvoutptr->dcdu[0] +
							  crvoutptr->dcdu[1]*crvoutptr->dcdu[1] +
							  crvoutptr->dcdu[2]*crvoutptr->dcdu[2];
					denom = sqrt(denom) * denom;
					crvoutptr->curv = numerator / denom;
					}
				}
			}
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_polygon_orient3D(crv,nvec)
**     Determine the orientation of a 3D curve.  The curve is
**     converted to a polygon and translated into the base coordinate
**     system before the orientation is determined. Note that the
**     vector does not need to be at the origin.  This routine will
**     rotate the curve's plane so it is parallel to the xy-plane.
**    INPUT:
**       crv     Curve to evaluate.
**       pt_list Polygon to evaluate.
**       flag    0: Use curve data.
**               1: Use point list data.
**       nvec    Plane normal vector.
**    OUTPUT:
**       none
**    RETURNS     : +1 if CCLW orientation.
**                   0 if no solution found.
**                  -1 if CLW orientation.
**    SIDE EFFECTS: none
**    WARNINGS    : none
********************************************************************/
int um_polygon_orient3D(crv,pt_list,flag,nvec,sftf)
struct UM_crvdatabag *crv;
UU_LIST *pt_list;
int flag;
UM_vector nvec;
UM_transf sftf;
{
	int i,npts,orient;
	UM_transf tf,tfi;
	UM_vector xvec;
	UM_coord *pts,ppt;
	UM_2Dcoord *pts2d,p2d;
	UU_REAL tol;
	UU_LOGICAL reverse;
	UU_LIST ptlist,ptlist2d;
/*
.....Find transformation matrix and invert to move to base origin.
*/
	um_nullvc(ppt);
	um_perpvc (nvec,xvec); um_unitvc(xvec,xvec);
	um_ptzx_tf(ppt,nvec,xvec,tf); um_inverttf(tf,tfi);
	uu_list_init0(&ptlist);
	uu_list_init0(&ptlist2d);
/*
.....Generate the points for the orientation routine.
*/
	orient = 0;
	if (flag == 0)
	{
		gettol(&tol);
		uu_list_init(&ptlist,sizeof(UM_coord),200,200);
		uc_retrieve_transf (crv->key,tf);
		npts = ncl_evolve_curve(crv,tf,tol,&ptlist,UU_NULL,UU_NULL,0);
		if (npts < 3) goto error;
		pts = (UM_coord *)UU_LIST_ARRAY(&ptlist);
	}
	else if (flag == 1 && pt_list != UU_NULL)
	{
		npts = pt_list->cur_cnt;
		if (npts < 3) goto error;
		pts = (UM_coord *)UU_LIST_ARRAY(pt_list);
	}
	else
		return(0);
	uu_list_init(&ptlist2d,sizeof(UM_2Dcoord),npts,1);
	for (i=0;i<npts;i++) 
	{
		if (sftf != UU_NULL) um_cctmtf(pts[i],sftf,ppt);
		else um_vctovc(pts[i],ppt);
		um_cctmtf(ppt,tfi,ppt);
		um_vctovc_2d(ppt,p2d);
		uu_list_push(&ptlist2d,&p2d);
	}
	pts2d = (UM_2Dcoord *)UU_LIST_ARRAY(&ptlist2d);
	orient = um_polygon_orientation(npts,pts2d);
error:
	if (flag == 0) uu_list_free(&ptlist);
	uu_list_free(&ptlist2d);
	return(orient);
}

/*********************************************************************
**    E_FUNCTION     : um_polygon_orientation3D(crv,nvec)
**     Determine the orientation of a 3D curve.  The curve is
**     converted to a polygon and translated into the base coordinate
**     system before the orientation is determined. Note that the
**     vector does not need to be at the origin.  This routine will
**     rotate the curve's plane so it is parallel to the xy-plane.
**    INPUT:
**       crv     Curve to evaluate.
**       nvec    Plane normal vector.
**    OUTPUT:
**       none
**    RETURNS     : +1 if CCLW orientation.
**                   0 if no solution found.
**                  -1 if CLW orientation.
**    SIDE EFFECTS: none
**    WARNINGS    : none
********************************************************************/
int um_polygon_orientation3D(crv,nvec)
struct UM_crvdatabag *crv;
UM_vector nvec;
{
	int orient = um_polygon_orient3D(crv,UU_NULL,0,nvec,UU_NULL);
	return(orient);
}
