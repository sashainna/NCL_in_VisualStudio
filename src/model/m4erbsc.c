/*********************************************************************
**    NAME         :  m4erbsc.c
**       CONTAINS:
**			int um_p7_rbsplcrv(ptr)
**			int um_c7_frmcirc(cptr,rptr)
**			    um_solv(xx,tt)
**			int um_c7_frmline(lptr,rptr)
**			int um_c7_frmpoly(pptr,rptr)
**			int um_c7_frmbsplcrv(bptr,rptr)
**			int um_c7_frmrbsplcrv(bptr,rptr)
**			int um_c7_frmconic(cptr, rptr)
**			int um_drw7_rbsplcrv(ptr,tfmat,attrptr)
**			int um_dcp7_rbsplcrv(ptr)
**			int um_tr7_tranrbsplcrv(eptr, offset)
**			int um_tf7_tranfrbsplcrv(eptr,tranfmat,store)
**			int um_cp7_copyrbsplcrv(eptr1, eptr2, bagsize)
**			int um_sc7_scalrbsplcrv
**			int um_c7_flip(eptr, r2)
**			    um_m7_addknot(r1ptr,mul,tnew,r2ptr)
**       int um_c7_splitrbsplcrv(rptr,tsplit,r1ptr,r2ptr)
**       int um_c7_trimrbsplcrv(rptr,tsplit,udel,ptr1,ptr2)
**       int um_redef_rbsplcrv (eptr)
**       int um_c7_endpoints(eptr, u, udel)
**       int um_c7_frmline_extension (lptr,eptr1,plane,r0)
**       int um_c7_frmcirc_extension (cptr,eptr1,plane,r0)
**       int um_c7_frmconic_extension (cptr,eptr1,plane,r0)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**        m4erbsc.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:08:03
*********************************************************************/

#include "nccs.h"
#include "umath.h"
#include "udebug.h"
#include "mdebug.h"
#include "mdeval.h"
#include "uhep.h"
#include "go.h"
#include "mattr.h"
#include "mcrv.h"
#include "mcvh.h"
#include "modef.h"
#include	"mdrel.h"
#include "nclvx.h"

/*
...vp 10/10/97 one good realy true zaxis, I hope to keep it
...protected from changes by anysys.  Other area of troubles
...with using UM_zaxis can be in the following files: 
...m3ecpln2.c m3idaxis.c m4e2da.c m8irsol6.c & m9eview.c.
*/

/*********************************************************************
**    E_FUNCTION     : um_p7_rbsplcrv(ptr)
**       Print the contents of a rational bspline record.
**    PARAMETERS   
**       INPUT  : 
**				ptr							pointer to rational bspline 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_p7_rbsplcrv(ptr)
	struct UM_rbsplcrv_rec *ptr;

	{
	int npts;

	uu_denter(UU_MTRC,(us,"um_p7_rbsplcrv(%d)",ptr->key));
	sprintf(UM_sbuf,"RBSPLINE %d",ptr->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "label %7.7s", ptr->label);
	um_pscroll(UM_sbuf);
	npts = ptr->n + ptr->k - 1;
	um_p_ary(UM_PINT,"planar",1,&ptr->planar);
	um_p_ary(UM_PINT,"open",1,&ptr->open);
	um_p_ary(UM_PINT,"order (k)",1,&ptr->k);
	um_p_ary(UM_PINT,"spans (n)",1,&ptr->n);
	um_p_ary(UM_PFLOAT,"t0",1,&ptr->t0);
	um_p_ary(UM_PFLOAT,"t1",1,&ptr->t1);
	um_p_ary(UM_PINT,"no_t",1,&ptr->no_t);
	um_p_ary(UM_PFLOAT,"t=",ptr->no_t,ptr->t);
	um_p_ary(UM_PINT,"no_pt",1,&ptr->no_pt);
	um_p_ary(UM_PFLOAT,"pt=",3*npts,ptr->pt);
	um_p_ary(UM_PINT,"no_wt",1,&ptr->no_wt);
	um_p_ary(UM_PFLOAT,"wt=",npts,ptr->wt);
	umi_print_transf(ptr->key);
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_c7_frmcirc(cptr,rptr)
**       Convert a circle into rational b-spline form.
**    PARAMETERS   
**       INPUT  : 
**				cptr    			pointer to circle record
**       OUTPUT :  
**				rptr				pointer to  entity record for rational bspline
**    RETURNS      : 
**				0					if no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

	static UU_REAL w[7] = {2.0,1.0,2.0,1.0,2.0,1.0,2.0};
	static UU_REAL t[10] = {0.0,0.0,0.0,1.0,1.0,2.0,2.0,3.0,3.0,3.0};
	static UU_REAL fact1[6] = {1.0,1.0,-0.5,-2.0,-0.5,1.0};
	static UU_REAL fact2[6] = {0.0,1.7320508,0.8660254,0.0,-.8660254,-1.7320528};

int
um_c7_frmcirc(cptr,rptr)

	struct  UM_circle_rec  *cptr;
	struct  UM_rbsplcrv_rec  *rptr;

	{
	UM_vector yaxis;			/* vector normal to nvec and svec */
	UM_vector xcomp;			/* component in "x" direction */
	UM_vector ycomp;			/* component in "y" direction */
	UM_coord pt;				/* scratch array */
	UU_REAL rr;					/* radius of the arc */
	UU_REAL ang;				/* sweep angle of the arc */
	UU_REAL tmin;				/* minimum knot value of arc */
	UU_REAL tmax;				/* maximum knot value of arc */
	UU_REAL xleng;				/* length of x projection of radius */
	UU_REAL dumm=.5;
	int i,j;
	int status;

	struct UM_rbsplcrv_rec r1;
	struct UM_rbsplcrv_rec r2;
	struct UM_rbsplcrv_rec *r;
	UU_REAL tnew;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter( UU_MTRC,(us,"um_c7_frmcirc(%8x)",cptr)); 
	status = UU_SUCCESS;
	ur_setup_data(UM_RBSPLCRV_REL, rptr, um_curve_size(cptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (rptr->label, "");
	rptr->subscr = 0;
	rr = cptr->radius;									/* get radius */

	um_cross(cptr->nvec, cptr->svec, yaxis);    /* find axis in plane */
															/* calc control points */
	for(j=0;j<6;j++)
		{
		um_vctmsc(cptr->svec,fact1[j]*rr,xcomp);
		um_vctmsc(yaxis,fact2[j]*rr,ycomp);
		um_vcplvc(xcomp,ycomp,pt);
		um_vcplvc(pt,cptr->center,&rptr->pt[3*j]);
		}
	um_vctovc(&rptr->pt[0],&rptr->pt[18]);

	ang = cptr->dang;
	tmin = 0.0;
	if(ang < 0.0) ang = UM_TWOPI + ang;       /* compensate for negative ang */

	if(fabs(ang - UM_TWOPI) < .005)				/* branch on circle or arc */
		{
		tmax = 3.0;										/* circle */
		}
	else if(ang < UM_TWOPI/3.0)
		{													/* arc with ang < 120 deg */
		xleng = cos(ang);
		um_solv(xleng,&tmax);
		}
	else if(ang >= UM_TWOPI/3.0 && ang < UM_TWOPI * 2.0/3.0)
		{
		ang = ang - UM_TWOPI/3.0;					/* arc with ang > 120 < 240 */
		xleng = cos(ang);
		um_solv(xleng,&tmax);
		tmax = tmax + 1.0;
		}
	else
		{
		ang = ang - UM_TWOPI * 2.0/3.0;			/* arc with ang > 240 */
		xleng = cos(ang);
		um_solv(xleng,&tmax);
		tmax = tmax + 2.0;
		}
															/* swap limits if ang negative */
	if(cptr->dang < 0.0)
		{
		tmin = tmax;
		tmax = 3.0;
		}

	rptr->rel_num = UM_RBSPLCRV_REL;
	rptr->planar = UU_TRUE;
	rptr->open = UU_FALSE;
	rptr->k = 3;
	rptr->n = 5;
	rptr->t0 = 0.0;
	rptr->t1 = 3.0;
	rptr->no_t = 10;
	for (i=0; i<10; i++)
		rptr->t[i] = t[i];
	rptr->no_pt = 7;
	rptr->no_wt = 7;
	for (i=0; i<7; i++)
		rptr->wt[i] = w[i];
	if ((tmin > 0.0) ||(tmax < 3.0))
		{
		if (tmin > 0.0) tnew = tmin/3.0; else tnew = tmax/3.0;
		um_c7_splitrbsplcrv(rptr,&tnew,&dumm,&r1,&r2);
		if (tmin > 0.0) r = &r2; else r = &r1;
		rptr->t0 = r->t0;
		rptr->t1 = r->t1;
		rptr->k = r->k;
		rptr->n = r->n;
		rptr->no_t = r->no_t;
		um_cparray(r->no_t, r->t, rptr->t);
		rptr->no_pt = r->no_pt;
		um_cparray(3*(r->no_pt), r->pt, rptr->pt);
		rptr->no_wt = r->no_wt;
		um_cparray(r->no_wt, r->wt, rptr->wt);
		}
	uu_dexitstatus("um_c7_frmcirc",status);
	return (status);
	}

/*********************************************************************
**    I_FUNCTION     : um_solv(xx,tt)
**       Solve quadratic equation for max knot value in rational
**			bspline representation of an arc.
**    PARAMETERS   
**       INPUT  : 
**				xx					x-coordinate of the end point.
**       OUTPUT :  
**				tt					knot value at the end point.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_solv(xx,tt)
	UU_REAL xx;								/* x-coordinate */
	UU_REAL *tt;							/* max knot value */

	{
	UU_REAL coef;							/* coefficient of the quadratic eq. */

	/*-------------------------------------------------------------------
	** Start of executable code
	**-----------------------------------------------------------------*/

	coef = (1.0 - xx)/(xx + 0.5);
	if (fabs(coef) < 1.0e-8)
/*		*tt = 0.0; */
  *tt = sqrt(coef) - .5*coef;
	else
		*tt = (coef/2.0) * (sqrt(1.0 + (4.0/coef)) - 1.0);

	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_c7_frmline(lptr,rptr)
**       Convert a line into rational b-spline form.
**    PARAMETERS   
**       INPUT  : 
**				lptr    			pointer to line record
**       OUTPUT :  
**				rptr				pointer to  entity record for rational bspline
**    RETURNS      : 
**				0					if no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int
um_c7_frmline(lptr,rptr)

	struct  UM_line_rec  *lptr;
	struct  UM_rbsplcrv_rec  *rptr;

	{
	int i;
	int status;

	uu_denter( UU_MTRC,(us,"um_c7_frmline(%8x)",lptr));
	status = UU_SUCCESS;

	ur_setup_data(UM_RBSPLCRV_REL, rptr, um_curve_size(lptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (rptr->label, "");
	rptr->subscr = 0;
	rptr->rel_num = UM_RBSPLCRV_REL;
	rptr->planar = UU_TRUE;
	rptr->open = UU_TRUE;
	rptr->k = 2;
	rptr->n = 1;
	rptr->t0 = 0.0;
	rptr->t1 = 1.0;
	rptr->no_t = 4;
	rptr->t[0] = 0.0;
	rptr->t[1] = 0.0;					
	rptr->t[2] = 1.0;				
	rptr->t[3] = 1.0;			
	rptr->no_pt = 2;
	um_vctovc(&lptr->spt[0],&rptr->pt[0]);
	um_vctovc(&lptr->ept[0],&rptr->pt[3]);
	rptr->no_wt = 2;
	for (i=0; i<2; i++)
		rptr->wt[i] = 1.0;
	uu_dexitstatus("um_c7_frmline",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_c7_frmpoly(pptr,rptr)
**       Convert a polyline into rational b-spline form.
**    PARAMETERS   
**       INPUT  : 
**				pptr    			pointer to polyline record
**       OUTPUT :  
**				rptr				pointer to  entity record for rational bspline
**    RETURNS      : 
**				0					if no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_c7_frmpoly(pptr,rptr)
struct  UM_polyline_rec  *pptr;
struct  UM_rbsplcrv_rec  *rptr;
{
	int i;
	int status;
	UU_REAL *t, len,magvec;
	int numpts,j;
	UM_vector vec;

	uu_denter( UU_MTRC,(us,"um_c7_frmpoly(%8x)",pptr));
	status = UU_SUCCESS;

	numpts = pptr->no_pt;
	t = (UU_REAL *) uu_toolmalloc ((numpts+3) * sizeof(UU_REAL));
	len = 0.0;
/*
.....Use length of the polyline at each vertice as
.....the t values.
*/
	for (i=1, j=0; i<numpts; i++, j+=3)
	{
		um_vcmnvc (&pptr->pt[j+3], &pptr->pt[j], vec);
		magvec = um_mag(vec);
		len += magvec;
		t[i+1] = len;
	}
/*
.....Since the order of the polyline is 2 (same as a line)
.....make sure the initial and final t values are repeated.
*/
	t[0]=0.0;
	t[1]=0.0;
	t[numpts+1]=len;


	ur_setup_data(UM_RBSPLCRV_REL, rptr, um_curve_size(pptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (rptr->label, "");
	rptr->subscr = 0;
	rptr->rel_num = UM_RBSPLCRV_REL;
	rptr->planar = UU_TRUE;
	rptr->open = UU_TRUE;
	rptr->k = 2;
	rptr->n = numpts -1;
	rptr->t0 = 0.0;
	rptr->t1 = len;
	rptr->no_t =numpts+2;
	rptr->t =t;
	rptr->no_pt = numpts;
	rptr->pt = pptr->pt;
	rptr->no_wt = numpts;
	for (i=0; i<numpts; i++)
		rptr->wt[i] = 1.0;
	uu_dexitstatus("um_c7_frmpoly",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : 	int um_c7_frmbsplcrv(bptr,rptr)
**       Convert a bspline curve into rational b-spline form.
**    PARAMETERS   
**       INPUT  : 
**				bptr    			pointer to bspline record
**       OUTPUT :  
**				rptr				pointer to  entity record for rational bspline
**    RETURNS      : 
**				0					if no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c7_frmbsplcrv(bptr,rptr)
	struct  UM_bsplcrv_rec  *bptr;
	struct  UM_rbsplcrv_rec  *rptr;

	{
	int status;
	int i,j;
	int order;
	int nspan;
	int npts;
	int minknot,maxknot;
   UU_REAL u0 = 0.0;
   UU_REAL u1 = 1.0;
   UU_REAL dumm = 0.5;
	struct UM_rbsplcrv_rec r11;
	struct UM_rbsplcrv_rec r12;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter( UU_MTRC,(us,"um_c7_frmbsplcrv(%8x)",bptr));
	status = UU_SUCCESS;
	ur_setup_data(UM_RBSPLCRV_REL, rptr, um_curve_size(bptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (rptr->label, "");
	rptr->subscr = 0;
	order = bptr->k;
	nspan = bptr->n;
	npts = nspan + order - 1;
	minknot = 0;
	maxknot = nspan;
	rptr->key = -1;
	rptr->rel_num = UM_RBSPLCRV_REL;
	rptr->planar = bptr->planar;
	rptr->open = bptr->open;
	rptr->k = order;
	rptr->n = nspan;
	rptr->t0 = bptr->t0;
	rptr->t1 = bptr->t1; 
	rptr->no_t = maxknot + (2*order) - 1;
	for (i=0; i<order; i++) rptr->t[i] = 0.0;
	for (i=order,j=1; i<maxknot+order-1; i++,j++) rptr->t[i] = j;
	for (i=maxknot+order-1; i<maxknot+(2*order)-1; i++) rptr->t[i] = maxknot;
	rptr->no_pt = bptr->no_pt;
	um_cparray(3*npts, bptr->pt, rptr->pt);
	rptr->no_wt = npts;
	for (i=0; i<npts; i++) rptr->wt[i] = 1.0;
	if ((rptr->t0 != minknot) || (rptr->t1 != maxknot))
		{
		um_c7_splitrbsplcrv(rptr,&u0,&dumm,&r11,&r12);
		um_c7_splitrbsplcrv(&r12,&u1,&dumm,rptr,&r11);
		}
	uu_dexitstatus("um_c7_frmbsplcrv",status);
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : 	int um_c7_frmrbsplcrv(bptr,rptr)
**       Convert a rational bspline curve into a rational bspline 
**			curve (i.e. copy original).
**    PARAMETERS   
**       INPUT  : 
**				eptr    			rational bspline curve
**       OUTPUT :  
**				rptr				copy of original rational bspline curve
**    RETURNS      : 
**				0					if no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c7_frmrbsplcrv(eptr,rptr)
	struct  UM_rbsplcrv_rec  *eptr;
	struct  UM_rbsplcrv_rec  *rptr;

	{
	int status;

	uu_denter( UU_MTRC,(us,"um_c7_frmrbsplcrv(%x,%x)",eptr,rptr));
	status = UU_SUCCESS;
	ur_setup_data(UM_RBSPLCRV_REL, rptr, um_curve_size(eptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (rptr->label, "");
	rptr->subscr = 0;

	rptr->key = -1;
	rptr->rel_num = UM_RBSPLCRV_REL;

	rptr->planar = eptr->planar;
	rptr->open = eptr->open;
	rptr->closdinu = eptr->closdinu;

	rptr->n = eptr->n;
	rptr->k = eptr->k;
	rptr->t0 = eptr->t0;
	rptr->t1 = eptr->t1;
	rptr->no_t = eptr->no_t;
	um_cparray(eptr->no_t, eptr->t, rptr->t);

	rptr->no_pt = eptr->no_pt;
	um_cparray(3*eptr->no_pt, eptr->pt, rptr->pt);

	rptr->no_wt = eptr->no_wt;
	um_cparray(eptr->no_wt, eptr->wt, rptr->wt);

	uu_dexitstatus("um_c7_frmrbsplcrv",status);
	return (status);
	}
/*********************************************************************
**    E_FUNCTION :  um_c7_frmconic(cptr, rptr)
**       convert conic curve to rational bspline form
**    PARAMETERS   
**       INPUT  : 
**          cptr	--	pointer to conic entity rec
**          rptr	--	points to allocated rat. bspl. record
**       OUTPUT :  
**          rptr	--	filled in with converted conic
**    RETURNS      : 0 if OK
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static	UU_REAL	cparam[3] = { 0.0, 0.5, 1.0};	/* fed to evaluator	*/

um_c7_frmconic(cptr, rptr)
	struct	UM_conic_rec		*cptr;
	struct	UM_rbsplcrv_rec	*rptr;

	{
	int		status;
	struct	UM_evcrvout	evout;	/* evaluator record for conic			*/

	/** automatics for hyperbola and parabola cases	**/
	UM_coord P[3];						/* points on curve at u = 0, .5, 1	*/
	UM_vector T[3]						/* tngnts on curve at u = 0, .5, 1	*/;
	UM_coord ipoints[3];				/* i'section points of three tangents */
	UM_transf tfmat;					/* transformation for conic, from unibase	*/
	int		numisects;				/* passed to um_ilnln() */
	int		i;

	/** automatics for ellipse case	**/
	struct	UM_circle_rec	circ;
	UM_coord	epts[2];				/* endpoints in definition space	*/
	UM_transf stretch;	/* stretch composed with  conic transformation	*/
	UU_REAL	*p;						/* pointer to next control point in list	*/
	UU_REAL	scale;
	UM_vector svec;
	UM_vector evec;

	/**	Strategy for converting conics to rat. bsplines
	 * Follows Faux and Pratt, pp. 138-141, with necessary relabeling.
	 * Weights are set to ratios g1 and g2 directly.  Since these have
	 * been calculated for conic parameter u = .5, the rbspl and conic
	 * parameterization agree (at least) at u = 0, .5, 1
	 *
	 * An ellipse is handled differently: it is squished to a circle
	 * in definition space.  The circle is converted to an rbspline,
	 * via the existing um_c7_frmcirc().  The resulting rbspline is
	 * still in conic definition space; then the rbspline is transformed
	 * by the compositon of the unsquish followed by the conic tfmat.
	 **/

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_c7_frmconic()"));

	ur_setup_data(UM_RBSPLCRV_REL, rptr, um_curve_size(cptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (rptr->label, "");
	rptr->subscr = 0;
	if (cptr->rel_num != UM_CONIC_REL)
		{
		um_p_ary(UM_PINT, "not a conic.", 1, &cptr->rel_num);
		status = UU_FAILURE;
		goto Done;
		}

	switch (cptr->type)
		{
	 case	UM_HYPERBOLA:
	 case	UM_PARABOLA:
		um_get_transformation(cptr->key, tfmat);
		for ( i = 0; i < 3; ++i)
			{
			um_ev4_conic(UM_FRSTDERIV, cparam[i], cptr, tfmat, &evout);
			um_vctovc(evout.cp, P[i]);
			um_unitvc(evout.dcdu, T[i]);
			}

		/**	--	find intersections of tangents	--	**/
		um_ilnln(P[0], T[0], P[1], T[1], &numisects, ipoints[0]);
		if (numisects != 1)
			{
			status = 2;
			goto Done;
			}
		um_ilnln(P[0], T[0], P[2], T[2], &numisects, ipoints[1]);
		if (numisects != 1)
			{
			status = 2;
			goto Done;
			}
		um_ilnln(P[1], T[1], P[2], T[2], &numisects, ipoints[2]);
		if (numisects != 1)
			{
			status = 2;
			goto Done;
			}
		rptr->rel_num	=	UM_RBSPLCRV_REL;
		rptr->planar	=	UU_TRUE;
		rptr->open		=	UU_TRUE;
		rptr->k			=	3;						/* quadratic	*/
		rptr->n			=	1;
		rptr->t0			=	0.0;
		rptr->t1			=	1.0;
		rptr->no_t		=	6;
		rptr->t[0]		=	0.0;
		rptr->t[1]		=	0.0;
		rptr->t[2]		=	0.0;
		rptr->t[3]		=	1.0;
		rptr->t[4]		=	1.0;
		rptr->t[5]		=	1.0;
		rptr->no_pt		= 3;

		/* rptr->pt is not really an array of points	*/
		um_vctovc(P[0], rptr->pt);
		um_vctovc(ipoints[1], rptr->pt + 3);
		um_vctovc(P[2], rptr->pt + 6);

		rptr->no_wt		= 3;
		rptr->wt[0]	= um_dcccc(ipoints[0], ipoints[1])/um_dcccc(ipoints[0], P[0]);
		rptr->wt[1]		=	1.0;
		rptr->wt[2]	= um_dcccc(ipoints[2], ipoints[1])/um_dcccc(ipoints[2], P[2]);

		status = UU_SUCCESS;

		break;
	 case	UM_ELLIPSE:

		/** -- get ellipse endpoints in definition space	--	**/
		uc_init_evcrvout(cptr, &evout);
		um_val4_conic(UM_POINT, cptr->t0, cptr, &evout);
		um_vctovc(evout.cp, epts[0]);
		um_val4_conic(UM_POINT, cptr->t1, cptr, &evout);
		um_vctovc(evout.cp, epts[1]);
		scale = cptr->invariants[1]/cptr->invariants[0];

		/** -- fill in circle rec (with "squished" conic)	--	**/
		circ.rel_num = UM_CIRCLE_REL;
		circ.radius  = cptr->invariants[0];			/* semi-minor length	*/
		um_vctovc(UM_zerovec, circ.center);			/* centered	*/
		um_vctovc(UM_zvector, circ.nvec);				/* set normal	*/
		um_vctovc(epts[0], svec);						/* start vector	*/
		svec[1] = svec[1] / scale;
		um_unitvc(svec, circ.svec);
		um_unitvc(epts[1], evec);
		evec[1] = evec[1] / scale;
		um_unitvc(evec, evec);
																/* arc angle		*/
		circ.dang = um_angle2p(circ.svec, evec, circ.nvec);
		if (fabs(circ.dang) < UM_DFUZZ) circ.dang = UM_TWOPI;
/*
		circ.dang  = atan2(epts[1][1],epts[1][0]) - atan2(epts[0][1],epts[0][0]);
		if (circ.dang < UM_DFUZZ)
			circ.dang += UM_TWOPI;
*/

		/** -- convert to rat. bspline	--	**/
		status = um_c7_frmcirc(&circ, rptr);
		if (status != UU_SUCCESS)
			{
			um_p_ary(UM_PINT, " frmconic: frmcirc returned bad status:", 1,
							&status);
				goto Done;
			}

		/**	--	transform geometry of ratbspline --	**/

		/* compose internal tfmat with stretching function	*/
		um_tftotf(cptr->tfmat, stretch);
		um_vctmsc(stretch[1],cptr->invariants[1]/cptr->invariants[0],stretch[1]);

		for ( i = 0, p = rptr->pt; i < rptr->no_pt; i++, p = p + 3)
			{
			um_cctmtf(p, stretch, p);
			}

		status = UU_SUCCESS;
		break;

	 default:
		um_p_ary(UM_PINT, "unknown conic type.", 1, &cptr->type);
		status = UU_FAILURE;
	}

Done:
	uu_dexitstatus("um_c7_frmconic",status);
	return (status);
	}
/*********************************************************************
**    I_FUNCTION     :  um_c7_frmnclcrv (bptr,rptr)
**            Map an NCL Bezier curve to a Unicad rational bspline.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c7_frmnclcrv (bptr,rptr)
   struct NCL_curve_rec  *bptr;
   struct UM_rbsplcrv_rec *rptr;
   {

   int i, j, num, k, m, ns, nt, n, lst, status;
   UU_REAL pt_cc[3], pt[3], rho, rr, dlt[3], (*cpt)[3];
   struct  NCL_segment_rec *segment;      /* NCL curve segment record */


   uu_denter(UU_MTRC,(us,"um_c7_frmnclcrv"));
   
   status = UU_SUCCESS;
   ur_setup_data(UM_RBSPLCRV_REL, rptr, um_curve_size(bptr));

   strncpy(rptr->label,bptr->label, NCL_MAX_LABEL);   
   rptr->subscr = bptr->subscr;       

   rptr->key = -1;
   rptr->rel_num = UM_RBSPLCRV_REL;
   rptr->planar = 0;   
   rptr->open = bptr->closdinu;
   rptr->closdinu = bptr->closdinu;

   m     = 3;
   ns    = bptr->no_segment;
   k     = (ns - 1)*m;
   nt    = ns * m + 2;

   rptr->k = m + 1;
   rptr->n = k + 1 - m;
   rptr->no_t = nt;
   rptr->no_wt = k + 1;
   rptr->no_pt = k + 1;

   cpt = (UU_REAL (*)[3]) rptr->pt;
   rr  = 0;

   segment = bptr->segment;

/* Calculate Bsplane points and store then in rbsplane record  */

   num   = bptr->no_param;
   j     = 0;
   for (i=0; i<num; i++)
     {
      if (i == 0)
         {
          for (n=0; n<3; n++)
            {
             pt[n]  = segment->point[n];
             dlt[n] = segment->delta[n];
            }
         }
      rho = segment->rho;  
      um_vctovc(pt, &rptr->pt[j]);         /* I control point    */
      pt[0] = pt[0] + dlt[0]; 
      pt[1] = pt[1] + dlt[1]; 
      pt[2] = pt[2] + dlt[2]; 
      j     = j + 3;
      um_vctovc(pt, &rptr->pt[j]);         /* II control point    */

/*  Get next segment point  */

      segment++;
      for (n=0; n<3; n++)
        {
         pt[n]  = segment->point[n];
         dlt[n] = segment->delta[n];
        }
      pt_cc[0] = pt[0] - rho*dlt[0]; 
      pt_cc[1] = pt[1] - rho*dlt[1]; 
      pt_cc[2] = pt[2] - rho*dlt[2]; 
      j     = j + 3;
      um_vctovc(pt_cc, &rptr->pt[j]);      /* III control point    */
      j     = j + 3;

/*  For last segment store 4-th point  */

      if (i == num-1) um_vctovc(pt, &rptr->pt[j]);      
     }

/* Generate weights and knots for Bsplane record */

   nt     = ns * m + 2;
   rptr->no_t = nt;
   rptr->no_wt = k + 1;
   rptr->no_pt = k + 1;

   for (i=0; i<=k; i++) rptr->wt[i] = 1.0;

   for (j=0; j<4; j++) rptr->t[j] = 0.0;
   j     = 3;
   lst   = 3;
   for (i=0; i<num; i++)
     {
      if (i == num-1) lst = 4;
      for (n=0; n<lst; n++)
        {
         j   = j + 1;
         rptr->t[j] = bptr->param[i];
        }
     }

   rptr->t0 = bptr->t0;          /* starting value    */
   rptr->t1 = bptr->t1;          /* ending value      */

   rho = bptr->t_end;
   for (j=4; j<rptr->no_t; j++) rptr->t[j] = rho * rptr->t[j];
   rptr->t0 = rho * bptr->t0;         
   rptr->t1 = rho * bptr->t1;        

   uu_dexit;
   return(status);
   }   

/*********************************************************************
**    E_FUNCTION		: um_drw7_rbsplcrv(ptr,tfmat,attrptr)
**			Draws a rational bspline curve.
**			PARAMETERS   
**				INPUT:
**					ptr			pointer to the rational bspline curve to be drawn. 
**					tfmat			transformation matrix
**					attrptr		pointer to attribute record
**				OUTPUT :      
**					none 
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
um_drw7_rbsplcrv(ptr,tfmat,attrptr)
	struct UM_rbsplcrv_rec *ptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;

	{
	UU_REAL u;							 /* parameter to evaluate bspline at */
	UU_REAL step;						 /* step size in parameter value */
	int nstep;							 /* number of steps to take */
	int i;								  /* index */
	struct UM_evcrvout evout;			/* evaluator output */
	Gwpoint3 gpt[90];					/* points for graphic um_display */
	Gint j;								/* number of points to um_display */
	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_drw7_rbsplcrv(key:%d,tfmat:%x,%x)", 
								ptr->key,tfmat,attrptr)); 

	um_set_disp_attr(attrptr);

	if (ptr->k == 2)
		nstep = ptr->n;
	else
		nstep = UM_srfattr.ptsperucrv * ptr->n;
	step = 1.0  / nstep;
	u = 0.0; 
	i = 0; 
	for (j = 0; j < nstep + 1; j++, i++)/* i = the nbr of pts in gks buffer */
		{
		if (u > 1.0) u = 1.0;
		um_ev7_rbsplcrv(UM_POINT, u, ptr, tfmat, &evout);
		if (i == 90) /* 98 = max nbr of pts to pass to gpolyline */
			{
			um_gpolyline3(attrptr, i, gpt);
			gpt[0].x = gpt[89].x;
			gpt[0].y = gpt[89].y;
			gpt[0].z = gpt[89].z;
			i = 1;
			}
		gpt[i].x = evout.cp[0];
		gpt[i].y = evout.cp[1];
		gpt[i].z = evout.cp[2];
		u = u + step;
		}
	um_gpolyline3(attrptr, i, gpt);
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION		: um_dcp7_rbsplcrv(ptr)
**			Draws the control polygon for a rational bspline curve.
**			PARAMETERS   
**				INPUT:
**					ptr			pointer to the rational bspline curve
**				OUTPUT :      
**					none 
**			RETURNS      : 
**				UU_SUCCESS iff no error; else UU_FAILURE
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
int
um_dcp7_rbsplcrv(ptr)
	struct UM_rbsplcrv_rec *ptr;

	{
	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_dcp7_rbsplcrv()")); 
	gpolyline3(ptr->no_pt,ptr->pt);
	uu_dexit;
	return(UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION		: um_tr7_tranrbsplcrv(eptr, offset)
**			Translate a rational bspline curve and update unibase.
**			PARAMETERS   
**				INPUT: 
**					eptr          pointer to the bspline entity record.
**					offset        direction vector for translation.
**				OUTPUT : none.  
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
um_tr7_tranrbsplcrv(eptr, offset)
	struct UM_rbsplcrv_rec *eptr;
	UM_vector offset;

	{
	int i,j;				/*loop counters							  */

	uu_denter(UU_MTRC, (us, "um_tr7_tranrbsplcrv(%x, %x)", eptr, offset));

	/* um_trxx_trangenent(eptr, offset); */
	for (i=0,j=0; i<eptr->no_pt; i++,j=j+3) 
		{
		um_vcplvc(&eptr->pt[j], offset, &eptr->pt[j]);
		}
	ur_update_data_varlist(eptr->key, 2,eptr->pt,1,eptr->no_pt);
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION		: um_tf7_tranfrbsplcrv(eptr,tranfmat,store)
**			Transform a rational bspline curve with a 
**			transformation matrix
**			PARAMETERS   
**				INPUT: 
**					eptr         pointer to a rational bspline entity to transform.
**					tranfmat     transformation matrix.
**					store			 TRUE iff the transformed entity is to be store
**									 in UNIBASE.
**				OUTPUT :  none.  
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
um_tf7_tranfrbsplcrv(eptr,tranfmat,store)
	struct UM_rbsplcrv_rec *eptr;
	UM_transf tranfmat;
	UU_LOGICAL store;
	{
	int i,j;						/*loop counters						  */

	uu_denter(UU_MTRC,(us,"um_tf7_tranfrbsplcrv(key:%d,tranfmat:%x,store:%d)",
					eptr->key, tranfmat, store));

	/* um_tfxx_tranfgenent(eptr, tranfmat, store); */
	for (i=0,j=0; i<eptr->no_pt; i++,j=j+3)	
		{
		um_cctmtf(&eptr->pt[j], tranfmat, &eptr->pt[j]);
		}
	if (store)
		ur_update_data_varlist(eptr->key, 2, eptr->pt, 1, eptr->no_pt);
	uu_dexit;
	return(UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION		: um_cp7_copyrbsplcrv(eptr1, eptr2, bagsize)
**			Copies the data for one rational bspline entity into 
**			another rational bspline entity. 
**			PARAMETERS   
**				INPUT: 
**					eptr1         entity pointer to a rational bspline curve.
**					eptr2         entity pointer the storage for the new
**                           rational bspline entity.
**					bagsize		  size of storage for entity.
**				OUTPUT :  
**					eptr2         pointer to a copy of the original rational
**                           bspline data pointed to by "eptr1".
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/

um_cp7_copyrbsplcrv(eptr1, eptr2, bagsize)
	struct UM_rbsplcrv_rec *eptr1;
	struct UM_rbsplcrv_rec *eptr2;
	int bagsize;

	{
	UU_KEY_ID temp_key;						/* temporary entity mtid */
	struct UM_attrdata_rec attr;

	uu_denter(UU_MTRC, (us, "um_cp7_copyrbsplcrv(key:%d, %x, %d)", 
						eptr1->key, eptr2, bagsize));

	/* um_cpxx_copygenent(eptr1, eptr2, bagsize); */
	temp_key = eptr1->key;
	ur_setup_data(eptr1->rel_num, eptr2, um_curve_size(eptr1));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (eptr2->label, "");
	eptr2->subscr = 0;
	um_get_disp_attr(eptr1->key, &attr);
	if (um_create_geom(eptr1, UM_DEFAULT_TF, &attr) != 0)
		{
		eptr1->key = temp_key;
		uu_uerror0(/*from cp7_rbsplcrv, error on um_create_geom*/ UM_MODEL,166); 
		}
	else
		{ 
		eptr2->key = eptr1->key;
		eptr2->rel_num = UM_RBSPLCRV_REL;
		eptr1->key = temp_key;
		um_get_all_geom(eptr2,sizeof(struct UM_rbsplcrv_rec));
		}
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION		: um_cp7_rbsplstruct(eptr1, eptr2)
**			Copies the data for one rational bspline entity into 
**			another rational bspline entity. 
**			PARAMETERS   
**				INPUT: 
**					eptr1         entity pointer to a rational bspline curve.
**					eptr2         entity pointer the storage for the new
**                           rational bspline entity.
**				OUTPUT :  
**					eptr2         pointer to a copy of the original rational
**                           bspline data pointed to by "eptr1".
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
int
um_cp7_rbsplstruct (rptr,r2ptr)
	struct UM_rbsplcrv_rec *rptr;
	struct UM_rbsplcrv_rec *r2ptr;
   {
   r2ptr->key = -1;
   r2ptr->rel_num = UM_RBSPLCRV_REL;
   r2ptr->k = rptr->k;
   r2ptr->n = rptr->n;
   r2ptr->planar = rptr->planar;
   r2ptr->open = UU_TRUE;
   r2ptr->t0 = rptr->t0;
   r2ptr->t1 = rptr->t1;

   r2ptr->no_t = rptr->no_t;
   um_cparray(3*(rptr->no_t), &rptr->t[0],&r2ptr->t[0]);

   r2ptr->no_pt = rptr->no_pt;
   um_cparray(3*(rptr->no_pt), &rptr->pt[0],&r2ptr->pt[0]);

   r2ptr->no_wt = rptr->no_wt;
   um_cparray(rptr->no_wt, &rptr->wt[0],&r2ptr->wt[0]);

	uu_dexit;
	return(UU_SUCCESS);
   }
/*********************************************************************
**    E_FUNCTION     : um_sc7_scalrbsplcrv
**      Scale a rational bspline curve around a point
**    PARAMETERS   
**       INPUT  : 
**				eptr        pointer to the entity to be scaled
**          scalpt      point to be scaled about
**          scalmat     the 4x3 scaling matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**   SIDE EFFECTS  : none
**    WARNINGS     : none
*********************************************************************/
um_sc7_scalrbsplcrv(eptr,fpt,scalmat)
	struct  UM_rbsplcrv_rec *eptr;
	UM_coord fpt;
	UM_transf scalmat;

	{
	int i,j;              /*loop counters                       */
	UM_coord temppt;       /*temporary storage                   */

	uu_denter(UU_MTRC, (us, "um_sc7_scalrbsplcrv(%x, %x, %x)", 
					eptr, fpt, scalmat));

	/* um_scxx_scalgenent(eptr, fpt, scalmat); */
	for (i=0,j=0; i<eptr->no_pt; i++,j=j+3)
		{
		um_vcmnvc(&eptr->pt[j], fpt, temppt);
		um_cctmtf(temppt, scalmat, temppt);
		um_vcplvc(temppt, fpt, &eptr->pt[j]);
		}
	ur_update_data_varlist(eptr->key, 2, eptr->pt, 1, eptr->no_pt);
	uu_dexit;
	return(UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c7_flip(eptr, r2)
**       Given a curve EPTR, determine an equivalent rational bspline
**			curve (R2) which is a "flipped" parameterization of R1.
**    PARAMETERS   
**       INPUT  : 
**          eptr						pointer to curve
**       OUTPUT :  
**          r2							pointer to equivalent curve which
**											has a "flipped" parameterization
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c7_flip(eptr, r2)
	struct UM_crvdatabag *eptr;
	struct UM_rbsplcrv_rec *r2;

	{
	struct UM_rbsplcrv_rec r1;
	int status;
	int i,j;

	uu_denter(UU_MTRC,(us,"um_c7_flip(key=%x)", eptr->key));

	/* convert curve to rational bspline form */
	status = uc_crv_to_unirbsc(eptr, &r1);

	/* reparameterize (flip) curve: tnew = 1 - told */
	if (status == UU_SUCCESS)
		{
		ur_setup_data(UM_RBSPLCRV_REL, r2, um_curve_size (eptr));
		/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
		strcpy (r2->label, "");
		r2->subscr = 0;

		r2->key = -1;
		r2->rel_num = UM_RBSPLCRV_REL;

		r2->planar = r1.planar;
		r2->open = r1.open;

		r2->n = r1.n;
		r2->k = r1.k;

		r2->t0 = 1.0 - r1.t1;
		r2->t1 = 1.0 - r1.t0;
		r2->no_t = r1.no_t;
		for (i=0, j=(r1.no_t-1); i<r1.no_t; i++, j=j-1)
			r2->t[j] = 1.0 - r1.t[i];

		r2->no_pt = r1.no_pt;
		for (i=0, j=(r1.no_pt-1)*3; i<r1.no_pt; i++, j=j-3)
			um_vctovc(&r1.pt[3*i], &r2->pt[j]);

		r2->no_wt = r1.no_wt;
		for (i=0, j=(r1.no_wt-1); i<r1.no_wt; i++, j=j-1)
			r2->wt[j] = r1.wt[i];
		}

	uu_dexit;
	uu_dexitstatus("um_c7_flip",status);
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : um_m7_addknot(r1ptr,mul,tnew,r2ptr)
**			Create a rational bspline curve (R2PTR) which represents
**			the same curve as the given rational bspline (R1PTR) but which
**			has a knot vector (and associated control polygon and
**			weight vector) with the given knot value (TNEW) included
**			the specified number of times (MUL).
**    PARAMETERS   
**       INPUT  : 
**          r1ptr							pointer to rational bspline
**				mul							desired multiplicity
**				tnew							new knot value t
**													0.0 <= tnew <= 1.0
**       OUTPUT :  
**          r2ptr							new rational bspline
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_m7_addknot(r1ptr,mul,tnew,r2ptr)
	struct UM_rbsplcrv_rec *r1ptr;
	int mul;
	UU_REAL tnew;
	struct UM_rbsplcrv_rec *r2ptr;

	{
	UU_REAL tval;

	uu_denter(UU_MTRC,(us,"um_m7_addknot(%8x,%d,%f,%8x)",r1ptr,mul,tnew,r2ptr));
	tval = r1ptr->t0 + (tnew*(r1ptr->t1 - r1ptr->t0));
	r2ptr->planar = r1ptr->planar;
	r2ptr->open = r1ptr->open;
	r2ptr->k = r1ptr->k;
	r2ptr->t0 = r1ptr->t0;
	r2ptr->t1 = r1ptr->t1;
	r2ptr->no_t = r1ptr->no_t;
	um_cparray(r1ptr->no_t,r1ptr->t,r2ptr->t);
	um_addknot(mul,tval,&r2ptr->no_t,r2ptr->t);
	r2ptr->no_pt = r2ptr->no_t - r2ptr->k;
	r2ptr->no_wt = r2ptr->no_t - r2ptr->k;
	r2ptr->n = r2ptr->no_pt - r2ptr->k + 1;
	um_crvoslo(r1ptr,r2ptr);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : int um_c7_splitrbsplcrv(rptr,tsplit,r1ptr,r2ptr)
**       Split a rational bspline curve (RPTR) at parameter value
**			TSPLIT into two rational bspline curves (R1PTR and R2PTR)
**			where R1PTR goes from the start to TSPLIT and R2PTR goes
**			from TSPLIT to the end of the curve. Works with extended curve.
**    PARAMETERS   
**       INPUT  : 
**				rptr								original rational bspline curve
**				tsplit							parameter value to split at
**														0.0 < tsplit < 1.0
**          dumm                       not used
**       OUTPUT :  
**				r1ptr								first part of split curve
**				r2ptr								last part of split curve
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c7_splitrbsplcrv(rptr,tsplit,dumm, r1ptr,r2ptr)
	struct UM_rbsplcrv_rec *rptr;
	UU_REAL *tsplit, *dumm;
	struct UM_rbsplcrv_rec *r1ptr;
	struct UM_rbsplcrv_rec *r2ptr;

	{
	UU_REAL tval,tmin,tmax;
   struct UM_evcrvout evcrv;
	struct UM_rbsplcrv_rec *cptr;
	int	ret_val = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"um_c7_splitrbsplcrv(%8x,%f,%8x,%8x)",rptr,
				tsplit,r1ptr,r2ptr));

	ur_setup_data(UM_RBSPLCRV_REL, r1ptr, um_curve_size (rptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (r1ptr->label, "");
	r1ptr->subscr = 0;
	if (rptr->wt == UU_NULL)
	{
		r1ptr->no_wt = 0;
		r1ptr->wt = UU_NULL;
	}
	ur_setup_data(UM_RBSPLCRV_REL, r2ptr, um_curve_size (rptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (r2ptr->label, "");
	if (rptr->wt == UU_NULL)
	{
		r2ptr->no_wt = 0;
		r2ptr->wt = UU_NULL;
	}
/*
...check if curve is extended, and
...if split parameter is at extention make a line 
...segment for that (extention) part of curve 
*/
	tval = rptr->t0 + (*tsplit*(rptr->t1 - rptr->t0));
/*
...check if the spline segemnt is tiny smaller than UM_FUZZ
*/
	if (rptr->t1 - rptr->t0 < UM_FUZZ)
	{
		tmin = rptr->t[0];
		tmax = rptr->t[rptr->no_t-1];
	}
	else
	{
		tmin = rptr->t[0] + .5*UM_FUZZ;
		tmax = rptr->t[rptr->no_t-1] - .5*UM_FUZZ;
	}
    if (tval > tmax || tval < tmin)
     {
      uc_init_evcrvout(rptr,&evcrv);
      if (tval > tmax)
        {
         ret_val = um_ev7_rbsplcrv(UM_POINT,tsplit[0],rptr,UM_DEFAULT_TF,&evcrv);
		 if (ret_val == UU_FAILURE)
			 return ret_val;
         um_vctovc (evcrv.cp,&r2ptr->pt[0]);
         ret_val = um_ev7_rbsplcrv(UM_POINT,1.0,rptr,UM_DEFAULT_TF,&evcrv); 
		 if (ret_val == UU_FAILURE)
			 return ret_val;
		 um_vctovc (evcrv.cp,&r2ptr->pt[3]);
         cptr = r2ptr;
         um_cp7_rbsplstruct (rptr,r1ptr);
         r1ptr->t1 = tval;
         r1ptr->key = rptr->key;
        }
      else if (tval < tmin)
        {
         ret_val = um_ev7_rbsplcrv(UM_POINT,0.0,rptr,UM_DEFAULT_TF,&evcrv);		
		 if (ret_val == UU_FAILURE)
			 return ret_val;
         um_vctovc (evcrv.cp,&r1ptr->pt[0]);
         ret_val = um_ev7_rbsplcrv(UM_POINT,tsplit[0],rptr,UM_DEFAULT_TF,&evcrv);		 
		 if (ret_val == UU_FAILURE)
			 return ret_val;
         um_vctovc (evcrv.cp,&r1ptr->pt[3]);
         cptr = r1ptr;
         um_cp7_rbsplstruct (rptr,r2ptr);
         r2ptr->t0 = tval;
         r1ptr->key = rptr->key;
        }
/*
...set linear segment of spline
*/
      cptr->k = 2;
      cptr->n = 1;
      cptr->t0 = 0.0;
      cptr->t1 = 1.0;
      cptr->no_t = 4;
      cptr->t[0] = 0.0;
      cptr->t[1] = 0.0;
      cptr->t[2] = 1.0;
      cptr->t[3] = 1.0;
      cptr->no_pt = 2;
      cptr->no_wt = 0;
      cptr->wt    = UU_NULL;
     }
/*
...if split is in the midle of spline definition
...split it by creating additional t knot
...(use old um_c7_splitrbsplcrv routine) 
*/
   else
     {
       ret_val = um_c7_splitrbsplcrv1(rptr,tsplit,dumm, r1ptr,r2ptr);
     }

	uu_dexit;
	return(ret_val);
	}
/*********************************************************************
**    E_FUNCTION: int um_c7_splitrbsplcrv1(rptr,tsplit,r1ptr,r2ptr)
**       Previously 'um_c7_splitrbsplcrv'.
**       Split a rational bspline curve (RPTR) at parameter value
**       TSPLIT into two rational bspline curves (R1PTR and R2PTR)
**       where R1PTR goes from the start to TSPLIT and R2PTR goes
**       from TSPLIT to the end of the curve. 
**    PARAMETERS
**       INPUT  :
**          rptr                       original rational bspline curve
**          tsplit                     parameter value to split at
**                                        0.0 < tsplit < 1.0
**          dumm                       not used
**       OUTPUT :
**          r1ptr                      first part of split curve
**          r2ptr                      last part of split curve
**    RETURNS      :
**       0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c7_splitrbsplcrv1(rptr,tsplit,dumm, r1ptr,r2ptr)
   struct UM_rbsplcrv_rec *rptr;
   UU_REAL *tsplit, *dumm;
   struct UM_rbsplcrv_rec *r1ptr;
   struct UM_rbsplcrv_rec *r2ptr;

   {
   int i,j;
   UU_REAL tval;

   uu_denter(UU_MTRC,(us,"um_c7_splitrbsplcrv1(%8x,%f,%8x,%8x)",rptr,
            tsplit,r1ptr,r2ptr));
/*
...16-jul-96 vp
...ur_setup_data is in calling routine now
*/
   tval = rptr->t0 + (*tsplit*(rptr->t1 - rptr->t0));

   um_m7_addknot(rptr, rptr->k, tsplit[0], r1ptr);
   for (i=0; (i<r1ptr->no_t) && (fabs(r1ptr->t[i] - tval) > UM_EQPARM); i++);
/*
...16-jul-96 vp - if we need split closed curve same way as circle
...see um_c3_splitcircle. Note that we must check if curve rptr is closed,
...if f'(0) == f'(1) and add knot at tsplit[1] before connecting segments. 
*/
   r2ptr->key = -1;
   r2ptr->rel_num = UM_RBSPLCRV_REL;
   r2ptr->k = rptr->k;
   r2ptr->planar = rptr->planar;
   r1ptr->planar = rptr->planar;
   r2ptr->open = UU_TRUE;
   r1ptr->open = UU_TRUE;
   r2ptr->t0 = 0.;
   r2ptr->t1 = r1ptr->t1 - tval;
   r1ptr->t1 = tval;
   r2ptr->no_t = r1ptr->no_t - i;
/*
...16-jul-96 vp
...added shift parameter values for the second part of curve
...so it starts from 0 (replaces um_cparray)
*/
   r1ptr->no_t = i + r1ptr->k;
   for (j=0; j<r2ptr->no_t; j++) r2ptr->t[j] = r1ptr->t[i+j] - tval;

   r2ptr->no_pt = r2ptr->no_t - r2ptr->k;
   r1ptr->no_pt = r1ptr->no_pt - r2ptr->no_pt;
   um_cparray(3*(r2ptr->no_pt), &r1ptr->pt[3*(r1ptr->no_pt)],r2ptr->pt);
	if (r1ptr->wt)
	{
   	r2ptr->no_wt = r2ptr->no_pt;
   	r1ptr->no_wt = r1ptr->no_wt - r2ptr->no_wt;
   	um_cparray(r2ptr->no_wt, &r1ptr->wt[r1ptr->no_wt],r2ptr->wt);
	}
   r2ptr->n = r2ptr->no_pt - r2ptr->k + 1;
   r1ptr->n = r1ptr->no_pt - r1ptr->k + 1;

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION: int um_c7_trimrbsplcrv(rptr,tsplit,udel,ptr1,ptr2)
**       Split a rational bspline curve (RPTR) at parameter value
**			TSPLIT into two rational bspline curves (R1PTR and R2PTR)
**			where R1PTR goes from the start to TSPLIT and R2PTR goes
**			from TSPLIT to the end of the curve.
**    PARAMETERS   
**       INPUT  : 
**				rptr					original rational bspline curve
**				tsplit				parameter value to split at
**										0.0 < tsplit < 1.0
**       OUTPUT :  
**				ptr1					left part of curve rptr (u < tsplit)
**				ptr2					right part of curve rptr (u > tsplit)
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c7_trimrbsplcrv(rptr,tsplit,udel,ptr1,ptr2)
	struct UM_rbsplcrv_rec *rptr,*ptr1,*ptr2;
	UM_param *tsplit, *udel;

  {
   UM_param urng,t;

   uu_denter(UU_MTRC,(us,"um_c7_trimrbsplcrv(%8x,%f,%f)",rptr,
              *tsplit,*udel));

   urng = rptr->t1 - rptr->t0;
   t    = rptr->t0 + *tsplit * urng; 

   um_c7_frmrbsplcrv (rptr, ptr1);
   ptr1->t1 = t;
   um_c7_frmrbsplcrv (rptr, ptr2);
   ptr2->t0 = t;
   
   return (UU_SUCCESS);
  }

/*********************************************************************
**    E_FUNCTION     : int um_redef_rbsplcrv(eptr)
**       Restore original bspline curve if it was trimmed. 
**    PARAMETERS   
**       INPUT  : 
**				eptr								trimmed rational bspline curve
**       OUTPUT :  
**				eptr								original curve.
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_redef_rbsplcrv (eptr)
	struct UM_rbsplcrv_rec *eptr;
  {
/*
...restore original curve length
*/
   eptr->t0 = eptr->t[0];
   eptr->t1 = eptr->t[eptr->no_t - 1];

   return (UU_SUCCESS);
  }

/*********************************************************************
**    E_FUNCTION     : int um_c7_endpoints (eptr, u, udel)
**       Extend trimmed RBspl curve to new parameter limits. 
**       ? It can be outside the orginal curve definition ? 
**    PARAMETERS   
**       INPUT  : 
**				eptr   - trimmed rational bspline curve
**          u      - parameter value outside the input RBspl limits
**          udel   - not used
**       OUTPUT :  
**				eptr   - extended RBspl curve.
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c7_endpoints(eptr, u, udel)
	struct UM_rbsplcrv_rec *eptr;
   UM_param u, *udel;
  {
   UM_param urng, t;

   urng = eptr->t1 - eptr->t0;
   t = eptr->t0 + u * urng;

   if (u < 0.0) 
     {
      eptr->t0 = t;
     }
   else if (u > 1.0)
     {
      eptr->t1 = t;
     }

   return(UU_SUCCESS);
  }
/*********************************************************************
**    E_FUNCTION: int um_c7_frmline_extension (lptr,eptr1,plane,r0);
**       Convert input line into RBspl curve which represents the 
**       extended line so its def space contains the etire def space 
**       of the second curve (eptr1).
**    PARAMETERS   
**       INPUT  : 
**          lptr    - pointer to input line record
**				eptr1   - pointer to rational bspline curve record
**          plane   - common plane data
**       OUTPUT :  
**          r0      - pointer to the RBspl obtained from the expanded
**                    input line
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  um_c7_frmline_extension (lptr,eptr1,plane,r0)
  struct UM_line_rec *lptr;
  struct UM_rbsplcrv_rec *eptr1, *r0;
  UU_REAL     plane[2][3];
  {
   C2vh cvh1;
   UM_coord ppt, maxpt;
   UU_REAL vec1[3], vec2[3], dmin,dmax,d;
   UU_REAL (*pt0)[3];    
   UU_REAL (*pt1)[3];    
   int i,mx,mn;
/*
...convert line to RBspline
*/
   um_rbcrv_frmnclcrv (lptr,r0);
   pt0 = (UU_REAL (*)[3]) r0->pt;
/*
...get convex hulls for second RBspline  
*/
   cvh1.no_pt  =  eptr1->no_pt;
   pt1 = cvh1.pt = (UU_REAL (*)[3]) eptr1->pt;
   cvh1.cv = (int *) uu_toolmalloc ((cvh1.no_pt+1)*sizeof(int));
   um_plncvh (&cvh1, plane);
   
   um_vcmnvc (pt0[1],pt0[0],vec1);
   um_unitvc (vec1,vec1);
/*
...project all points of hull to line and
...get the most distant points
*/
   dmin = 1.e10;
   dmax = -1.e10;
   for (i=0; i<cvh1.no_cv-1; i++)
     {
      um_nptln (pt1[cvh1.cv[i]],pt0[0],vec1,ppt);
      um_vcmnvc (ppt,pt0[0],vec2);
      d = um_dot (vec2,vec1);
      if (d > dmax)
        {
         mx = i;
         dmax = d;
        }
      if (d < dmin)
        {
         mn = i;
         dmin = d;
        }
     }
/*
...replace points in RBspl line using extremal points
*/
   um_nptln (pt1[cvh1.cv[mn]],pt0[0],vec1,ppt);
   um_nptln (pt1[cvh1.cv[mx]],pt0[0],vec1,maxpt);
   um_vctovc (ppt,pt0[0]);
   um_vctovc (maxpt,pt0[1]);
/*
...make sure the line is not a point
*/
   if (dmax-dmin < UM_FUZZ)
      {
       um_vctmsc (vec1,UM_FUZZ,vec1);
       um_vcplvc (pt0[1],vec1,pt0[1]);
       um_vcmnvc (pt0[0],vec1,pt0[0]);
      }

   uu_toolfree (cvh1.cv);
   uu_dexit;
   return(UU_SUCCESS);
  }
/*********************************************************************
**    E_FUNCTION: int um_c7_frmcirc_extension (cptr,eptr1,plane,r0);
**       Convert input arc into RBspl curve which represents the 
**       full circle.
**    PARAMETERS   
**       INPUT  : 
**          cptr    - pointer to input cicrcle record
**				eptr1   - pointer to rational bspline curve record
**          plane   - common plane data
**       OUTPUT :  
**          r0      - pointer to the RBspl obtained from the expanded
**                    circle record
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  um_c7_frmcirc_extension (cptr,eptr1,plane,r0)
  struct UM_circle_rec *cptr;
  struct UM_rbsplcrv_rec *eptr1, *r0;
  UU_REAL     plane[2][3];
  {
   UU_REAL svt1;

   svt1 = cptr->dang;
/*
...close it to make full & convert to RBspl
*/
   cptr->dang = UM_TWOPI;
   um_rbcrv_frmnclcrv (cptr,r0);
   cptr->dang = svt1;

   uu_dexit;
   return (UU_SUCCESS);
  }
/*********************************************************************
**    E_FUNCTION: int um_c7_frmconic_extension (cptr,eptr1,plane,r0);
**       Convert input conic into RBspl curve which represents the 
**       extended conic so its def space contains the etire def space 
**       of the second curve (eptr1).
**    PARAMETERS   
**       INPUT  : 
**          cptr    - pointer to input conic record
**				eptr1   - pointer to rational bspline curve record
**          plane   - common plane data
**       OUTPUT :  
**          r0      - pointer to the RBspl obtained from the expanded
**                    conic record
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  um_c7_frmconic_extension (cptr,eptr1,plane,r0)
  struct UM_conic_rec *cptr;
  struct UM_rbsplcrv_rec *eptr1, *r0;
  UU_REAL     plane[2][3];
  {
   C2vh cvh1;
   UM_coord pt0, ppt, maxpt;
   UU_REAL vec1[3], vec2[3], dd, dmin,dmax,d,svt0,svt1,intrv,t;
   UU_REAL p1,p0;    
   UU_REAL (*pt1)[3];    
   int status,i,mx,mn;

   status = UU_SUCCESS;

   svt0 = cptr->t0;
   svt1 = cptr->t1;
   p0   = cptr->invariants[0];
   p1   = cptr->invariants[1];
   um_conic_parameter ((UU_REAL) 0., cptr, &t, &intrv);

   switch (cptr->type)
     {
/*
...ellipse - close it to make full & convert to RBspl
*/
      case UM_ELLIPSE:
        {
         if (intrv < 0) intrv += 4;
         cptr->t0 = -2.0;
         cptr->t1 = 2.0;
         um_rbcrv_frmnclcrv (cptr,r0);
         cptr->t0 = svt0;
         cptr->t1 = svt1;
        }
        break;
/*
...open conic - get conic axis & project RBspl to it to get
...the last possible point of conic in the RBspl space
*/
      case UM_PARABOLA:
      case UM_HYPERBOLA:
        {
/*
...get convex hull for second RBspline  
*/
         cvh1.no_pt  =  eptr1->no_pt;
         cvh1.cv = (int *) uu_toolmalloc ((cvh1.no_pt+1)*sizeof(int));
         pt1 = cvh1.pt = (UU_REAL (*)[3]) eptr1->pt;
         um_plncvh (&cvh1, plane);

         um_vctovc (cptr->tfmat[3],pt0);
         um_vctovc (cptr->tfmat[0],vec1);
/*
...project hull on conic axis
*/
         dmin = 1.e10;
         dmax = -1.e10;
         for (i=0; i<cvh1.no_cv-1; i++)
           {
            um_nptln (pt1[cvh1.cv[i]],pt0,vec1,ppt);
            um_vcmnvc (ppt,pt0,vec2);
            d = um_dot (vec2,vec1);
            if (d > dmax)
              {
               mx = i;
               dmax = d;
              }
            if (d < dmin)
              {
               mn = i;
               dmin = d;
              }
           }
/*
...check if intersection is possible
*/
         if (dmax < p1)
            status = UU_FAILURE;
         else
           {
            um_nptln (pt1[cvh1.cv[mn]],pt0,vec1,ppt);
            um_nptln (pt1[cvh1.cv[mx]],pt0,vec1,maxpt);
            if (cptr->type == UM_PARABOLA)
               {
                cptr->t1 = sqrt(dmax/p0);    
                cptr->t0 = -cptr->t1;
                dd = cptr->t1;
               }
            else
               {
                d = dmax/p0;
                maxpt[1] = p1 * sqrt(d*d - 1.);
                maxpt[0] = dmax;
                um_slv4_hyperbola (maxpt,cptr,&dd);
                cptr->t0 = dd;
                cptr->t1 = -dd;
               }  
           }
/*
...convert extended conic to RBspl
*/
         if (status == UU_SUCCESS)
            {
             um_rbcrv_frmnclcrv (cptr,r0);
             cptr->t0 = svt0;
             cptr->t1 = svt1;
            }
         uu_toolfree (cvh1.cv);
        }
        break;
     }
   uu_dexit;
   return (status);
  }

