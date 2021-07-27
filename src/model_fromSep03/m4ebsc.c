
/*********************************************************************
**    NAME         : mebsc 
**       CONTAINS:
**			   um_ev6_bsplcrv
**			   um_drw6_bsplcrv
**			   um_p6_bsplcrv
**			   um_valid6_cntlpt
**				umi_get_inverted_cntlpts
**			   um_tr6_tranbsplcrv
**			   um_tf6_tranfbsplcrv
**			   um_cp6_copybsplcrv
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4ebsc.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:01
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "uhep.h"
#include "udebug.h"
#include "go.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdmatrix.h"
#include "mdebug.h"


static UU_LOGICAL trace = UU_FALSE;
#define UM_MAX_CNTL_PTS 200
#define BYTES_IN_MAX_CNTL_PTS 3*sizeof(UU_REAL)*MAX_CNTL_PTS


#define C(i,j) c[i*(3)+j]
/*********************************************************************
**    E_FUNCTION: int um_ev6_bsplcrv(evflag, u, eptr, tfmat, evoutptr)
**
**			DESCRIPTION:Evaluate a bspline curve at a parameter u in the 
**					range [0,1].
**       
**			PARAMETERS   
**				INPUT: 
**		      	evflag	 flag specifying data to evaluate
**									>= UM_POINT => calculate (X,Y,Z) 
**									>= UM_FRSTDERIV => calculate 
**														(DX/DU,DY/DU,DZ/DU)
**									>= UM_SECDERIV => calculate 
**													(D2X/DU2,D2Y/DU2,D2Z/DU2)
**									>= UM_CURVATURE => calculate curvature
**
**					u			 parameter to evaluate at
**
**					eptr      pointer to the bspline entity
**          
**					tfmat		 transformation matrix.
**				OUTPUT :  
**					evoutptr  pointer to a curve evaluator record containing 
**                       the requested information.
**          
**			RETURNS      : none
**
**			SIDE EFFECTS : none
**
**			WARNINGS     : none
*********************************************************************/
int
um_ev6_bsplcrv(evflag, u, eptr, tfmat, evoutptr)
	int evflag;  
	UU_REAL u;
	struct UM_bsplcrv_rec *eptr;
	UU_REAL tfmat[4][3];
	struct UM_evcrvout *evoutptr;
	{
	int order;
	int npts;	         /* nbr of control points */
	UU_REAL *c;          /* coefficients of control points */
	int i,j;				   /* indicies */
	UU_REAL uu;				/* actual parameter value */
	UU_REAL bu;				/* basis evaluated at u */
	UU_REAL den;			/* denominator for curvature calculation */
	UU_REAL temp;			/* temporary used in curvature calculation */
	UU_REAL vect[3];		/* used in curvature calculation */
	UU_REAL um_basis();	/* basis evaluated at a parameter */
	UU_REAL um_dbasis();  /* first parametric derivative */
	UU_REAL um_d2basis();	/* second parametric derivative */
	int maxknot;/* max knot value=nbr of nondegenerate spans */
	UU_REAL um_dot();

	uu_denter(UU_MTRC,
		(us,"um_ev6_bsplcrv(evflag:%d,u:%g,key:%d,tfmat:%x,evoutptr:%x)",
		evflag,u,eptr,tfmat,evoutptr));
	order = eptr->k;
	npts = eptr->n + eptr->k - 1;
	c = eptr->pt;
	/* note, we use a define statement to access "c" as a dim 2 array 
	 * called "C".  
	 */
	maxknot = eptr->n;/* =max knot value */

	/* convert u to a value in the subrange of [0, maxknot]: [t0, t1].
	 */
	uu = u * (eptr->t1 - eptr->t0) + eptr->t0; 

	for (j = 0; j < 3; j++)															
		{
		evoutptr->cp[j]	= 0.0;
		evoutptr->dcdu[j]	= 0.0;
		evoutptr->d2cdu2[j] = 0.0;
		}
	evoutptr->curv = 0.0;
	if (evflag >= UM_POINT)
		{
		for (i = 0; i < npts; i++)
			{
			bu = um_basis(uu, i + 1, order, maxknot);
			for (j = 0; j < 3; j++)
				evoutptr->cp[j] = evoutptr->cp[j] + (C(i,j) * bu);
			}
		if (evflag >= UM_FRSTDERIV)
			{
			for (i = 0; i < npts; i++)
				{
				bu = um_dbasis(uu, i + 1, order, maxknot);
				for (j = 0; j < 3; j++)
					evoutptr->dcdu[j]  = evoutptr->dcdu[j] + (C(i,j) * bu);
				}

			/* now scale tan vec for parameter values 0 thru 1 */
			um_vctmsc(evoutptr->dcdu, (UU_REAL)maxknot, evoutptr->dcdu);

			if (evflag >= UM_SECDERIV) 
				{
				for (i = 0; i < npts; i++)
					{
					bu = um_d2basis(uu, i + 1, order, maxknot);
					for (j = 0; j < 3; j++)
						evoutptr->d2cdu2[j] = evoutptr->d2cdu2[j] + (C(i,j) * bu);
					}
				if (evflag >= UM_CURVATURE)
					{
					den = um_dot(evoutptr->cp, evoutptr->cp); 
					den = sqrt(den); 
					den = den * den * den; 
					temp = um_dot(evoutptr->d2cdu2, evoutptr->d2cdu2);
					if ( (den < UM_FUZZ) || (temp < UM_FUZZ) )
						evoutptr->curv = 0;
					else
						{
						um_cross(evoutptr->cp, evoutptr->d2cdu2, vect);
						temp = um_dot(vect, vect);
						evoutptr->curv = sqrt(temp) / den;
						}
					}
				}
			}
		}
	/* position results in evaluator record according to the transform, tfmat */
	um_transform_evcrvout(evflag, eptr, tfmat, evoutptr);

	uu_dexit;
	return (UU_SUCCESS);
	}


/*********************************************************************
**    E_FUNCTION: int um_drw6_bsplcrv(eptr,tfmat,attrptr)
**			DESCRIPTION:
**				Draws a bspline curve.
**			PARAMETERS   
**				INPUT:
**					eptr			pointer to entity record
**					tfmat			transformation matrix
**					attrptr		pointer to attribute record
**				OUTPUT :       none. 
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
int
um_drw6_bsplcrv(eptr,tfmat,attrptr)
	struct UM_bsplcrv_rec *eptr;
	UU_REAL tfmat[4][3];
	struct UM_attrdatabag *attrptr;

	{
	UU_REAL u;							 /* parameter to evaluate bspline at */
	UU_REAL step;						 /* step size in parameter value */
	int nstep;						 /* number of steps to take */
	int i;							  /* index */
	int npts;						  /* number of points in control polygon */
	struct UM_evcrvout evout;			/* evaluator output */
	Gwpoint3 gpt[90];				/* points for graphic um_display */
	Gint j;							/* number of points to um_display */

	uu_denter(UU_MTRC,(us,"um_drw6_bsplcrv(key=%d,tfmat=%x,attr=%x)", 
		eptr->key, tfmat, attrptr));

	um_set_disp_attr(attrptr);

	if (eptr->k == 2)
		nstep = eptr->n;
	else
		nstep = 10 * eptr->n;
	step = 1.0  / nstep;
	npts = eptr->k + eptr->n - 1;
	u = 0.0; 
	i = 0; 
	for (j = 0; j < nstep + 1; j++, i++)/* i = the nbr of pts in gks buffer */
		{
		if (u > 1.0) u = 1.0;
		um_ev7_rbsplcrv(UM_POINT, u, eptr, tfmat, &evout);
		if (i == 90) /* 98 = max nbr of pts to pass to gpolyline */
			{
			gpolyline3(i,gpt);
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
	gpolyline3(i,gpt);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION: int um_p6_bsplcrv(ptr)
**
**			DESCRIPTION: Print the parameters defining a bspline curve.
**       
**			PARAMETERS   
**				INPUT: 
**
**					ptr          pointer to the bspline record.
**          
**				OUTPUT :  
**          
**			RETURNS      : none
**
**			SIDE EFFECTS : none
**
**			WARNINGS     : none
*********************************************************************/
int
um_p6_bsplcrv(ptr)
	struct UM_bsplcrv_rec  *ptr;
	{
	int i,j;
	int npts;
	UU_REAL *coefficient;

	uu_denter(UU_MTRC,(us,"um_p6_bsplcrv(%8x)",ptr));
	sprintf(UM_sbuf,"BSPLINE %d",ptr->key); 
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "label %7.7s", ptr->label);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PINT,	"inverted",	  1, &ptr->inverted);
	um_p_ary(UM_PINT,	"planar",		 1, &ptr->planar);
	um_p_ary(UM_PINT,	"open",			1, &ptr->open);
	um_p_ary(UM_PINT,	"order (k)",	 1, &ptr->k);
	um_p_ary(UM_PINT,	"number spans (n)", 1, &ptr->n);
	um_p_ary(UM_PFLOAT, "start param (t0)", 1, &ptr->t0);
	um_p_ary(UM_PFLOAT, "end param  (t1)",	1, &ptr->t1);
	um_p_ary(UM_PINT,	"atom count (no_pt)",	1, &ptr->no_pt);
	um_p_ary(UM_PINT,	"pt ptr",	 1, &ptr->pt);
	npts = ptr->n + ptr->k - 1;
	coefficient = ptr->pt;
	for (i=0, j=0; i < npts; i++, j=j+3)
		um_p_ary(UM_PFLOAT, "pt", 3, &(coefficient[j]));
	umi_print_transf(ptr->key);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION: um_valid6_cntlpt(cntlpts, i)
**
**			DESCRIPTION: Determines if a control point for a bspline is valid.
**       
**			PARAMETERS   
**				INPUT: 
**					cntlpts        control point array.
**
**					i              index in "cntlpts" of the next prospective 
**                            control point.
**          
**				OUTPUT :  none.  
**          
**			RETURNS      : TRUE if the new control point is valid.
**
**			SIDE EFFECTS : none
**
**			WARNINGS     : none
*********************************************************************/
um_valid6_cntlpt(cntlpts, i)
	UU_REAL cntlpts[][3]; /*control point array */
	int i; /*index in "cntlpts" of nxt prospective control pt */
	{
	int npts;				/* current nbr of control pts */
	UU_REAL vec[3];
	UU_REAL vec1[3];
	UU_REAL vec2[3];
	UU_REAL *lstcntlpt;
	UU_REAL *nxt2lstcntlpt;
	UU_REAL ptonln[3];
	UU_REAL len;
	UU_REAL len1;
	UU_REAL len2;
	UU_REAL um_mag();

	uu_denter(UU_MTRC,(us,"um_valid6_cntlpt(%x,%d)",cntlpts,i));
	npts = i;
	if (npts > 0) 
	/* determine if the new cntl pt is compatible with the previous cntl pts */
		{				
		lstcntlpt = (cntlpts[i-1]);

		/* determine if new cntl pt is too close to previous one */
		um_vcmnvc(cntlpts[i], lstcntlpt, vec);
		len = um_mag(vec);
		if (len < UM_FUZZ)
			{
			uu_uerror0(/*new control point not accepted; 
						too close to last control point*/UM_MODEL,122);
			uu_dexit;
			return(UU_FALSE);
			}
		else /* new cntl pt is not too close */
			{ 
			if (npts >= 2)
				{ 	/* npts >= 2 */
				/* determine if new cntl pt causes the spline to fold 
				 * back on itself too sharply 
				 */
					nxt2lstcntlpt = (cntlpts[i-2]);
					um_vcmnvc(lstcntlpt, nxt2lstcntlpt, vec);
					len = um_mag(vec);
					um_unitvc(vec, vec);
					/*find the nearest pt to the new cntl pt that lies 
					 * on the line between "lstcntpt" and "nxt2lstcntlpt"
					 */
					um_nptln(cntlpts[i], lstcntlpt, vec, ptonln);
					/*we compare the  coordinates of lstcnlpt-ptonln 
					 * and nxt2lstcntlpt - ptonln to determine if ptonln
					 * lies between lstcnlpt and nxt2lstcntlpt.
					 */
					um_vcmnvc(lstcntlpt, ptonln, vec1);
					um_vcmnvc(nxt2lstcntlpt, ptonln, vec2);
					len1 = um_mag(vec1);
					len2 = um_mag(vec2);
					if ((len1 <= len) && (len2 <= len)) 
						{  /* "ptonln" is between */
						um_vcmnvc(cntlpts[i], ptonln, vec);
						len = um_mag(vec);
						if (len < UM_FUZZ)
							{
							uu_uerror0(/*new control point not accepted; spline
							would not have continuous 1st derivative*/UM_MODEL,123);
							uu_dexit;
							return(UU_FALSE);
							}
						else /* new cntl pt is ok, 1st derivative exists */
							{
							uu_dexit;
							return(UU_TRUE);
							}
						}  /* end "ptonln" is between */
					else 
					/* new cntl pt is ok, its not between previous 2 cntl pts */
						{
						uu_dexit;
						return(UU_TRUE);
						}
				} /* end npts >= 2 */
			else /* npts = 1 */
				{
				uu_dexit;
				return(UU_TRUE);
				}/* end npts = 1 */
			} /* end not too close */		
		} /* end npts > 0 */ 
	else /* npts = 0, so contl pt is ok */
		{
		uu_dexit;
		return(UU_TRUE);
		}
	}

							
#define B(i,j) b[i*(ncol)+j]
#define UM_MAT(i,j) mat.ary[i*(ncol)+j]
/*********************************************************************
**    I_FUNCTION :  umi_get_inverted_cntlpts
**							(cntlpts, extracntlpts,	order, npts, maxknot, incptr)
**       Get the inverted control points for the bspline.
**    PARAMETERS   
**       INPUT  : 
**         cntlpts			contains the original control points specified
**								by the user.
**			extracntlpts	the number of extra control to be added to each
**								end of the knot sequence for smoothing.
**			order				order of spline.
**			npts				number of control points needed.
**       maxknot			the max knot value.
**       OUTPUT :  
**			cntlpts			new inverted control points.
**			incptr			pointer to increment between consecutive "u" values 
**								associated with user data points to be interpolated
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umi_get_inverted_cntlpts(cntlpts, extracntlpts, order, npts, maxknot,incptr)
	UU_REAL cntlpts[UM_MAX_CNTL_PTS][3];  /* space for control points */
	int extracntlpts; /* nbr of extra control pts to add at the beginning and
							 * end of the control pt sequence before inverting.
							 */
	int npts;				/* number of control points needed */
	int order;				/* order of spline */
	int maxknot;			/* pointer to max knot */
	UU_REAL *incptr;	/* increment between consecutive "u" values associated
							 * with user data points to be interpolated
							 */
	{
	UU_REAL coeff[UM_MAX_CNTL_PTS][4];	/* for use when inverting */
	struct UM_matrix mat; 	/*data type defined in sal001.h, used for*/
								 	/*solving linear sys when inverting */
	UU_REAL *b;					 /*pointer to matrix to be inverted
										if inverted bsplines wanted must add extra
										cntl pts to the beginning and end  */
	UU_REAL um_basis(); 		/*bspline basis function evaluator */ 
	UU_REAL u;					/*parameter value to evaluate basis functions */
									/*when inverting */
	int cur_basis_maxknot_index; /*temporary*/
	int cur_basis_maxknot; /*the max knot value in the support of */
									/*the current basis function being used*/
	int cur_basis_minknot; /*the min value in the support of */
								  /*the current basis function being used*/
	int nbrzeros; 			/*nbr of times zero has been assigned to */
					  			/*"cur_basis_minknot" */
	int user_lst_cntlpt_index; /* index of last control point user entered */
	UU_REAL startvec[3];/* start vector for adding control pts to the 
								* beginning of the control pt sequence before inverting
								*/
	UU_REAL endvec[3];  /* end vector for adding control pts to the beginning of
								* the control pt sequence before inverting.
								*/
	UU_REAL eps = 1.0e-4; /*espilon for "um_eval_matrix"; does not change*/
	UU_REAL deter; 		/*determinant value returned by "um_eval_matrix"  */
	int nrow,ncol;			/* number of rows and col in the matrix */
	int i,j,k;


	/*------------------------------------------------------------------------
	**  Start of Executable Code
	**-----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"umi_get_inverted_cntlpts(%x,%d,%d,%d,%d)",
					cntlpts, extracntlpts, order, npts, maxknot));

	user_lst_cntlpt_index = npts - extracntlpts;
	um_vcmnvc(cntlpts[extracntlpts], cntlpts[extracntlpts + 1], startvec);
	um_vctmsc(startvec, (UU_REAL) 2.0, startvec);
	um_vcmnvc(cntlpts[user_lst_cntlpt_index-1], 
					cntlpts[user_lst_cntlpt_index-2], endvec);
	um_vctmsc(endvec, (UU_REAL) 2.0, endvec);
	for (i=0; i<extracntlpts; i++)
		{
		um_vcplvc
			(cntlpts[extracntlpts-i], startvec, cntlpts[extracntlpts-i-1]); 
		um_vcplvc(cntlpts[user_lst_cntlpt_index+i-1], endvec, 
				cntlpts[user_lst_cntlpt_index+i]); 
		}

	/* The coefficients of the linear eqns to be solved are the
	 * values of the basis functions at certain vals; i.e.
	 * we must solve B * x[i] = c[i] for i=1,2,3, where c[i]=
	 * the vector of the ith  coordinates of the control points 
	 * entered above.  Before describing the entries of B, note
	 * that the knot value range is: [0,maxknot] where maxknot=
	 * npts-order + 1.											
	 * We assume the coordinates given by the user are to be	
	 * values on the curve, equally spaced in this range, with 
	 * the first  coordinate corresponding to 0 and the last	
	 * coordinate corresponding to the maxknot value.  The inc-
	 * rement for this is: inc=maxknot/(npts-1).  Given this, 
	 * the entries of B can be described as follows:			
	 * 																	
	 *  f[1](0)		 f[2](0)	    f[3](0)	   ...f[npts](0)		 
	 *  f[1](inc)	 f[2](inc)	 f[3](inc)  ...f[npts](inc)	
	 *  f[1](2*inc) f[2](2*inc) f[3](2*inc)...f[npts](2*inc)	
	 * 							...									
	 *  f[1](maxknot)			...				  f[npts](maxknot) 
	 * 																	
	 * where f[1](u) is the bspline basis function with finite 
	 * support on the knot values: <0,...,0,1> (order nbr of	
	 * zeros) and where f[2](u) is the bspline basis function  
	 * with finite support on the knot values: <0,...,0,1,2>	
	 * (order-1 nbr of zeros).										
	 * Note1: The knot sequence is:									
	 * <0,...,0,1,2,...,n-order,n-order+1,...,n-order+1>		
	 *   with n=nbr of control pts="npts" above and				
	 *   where there are "order" 0's and "order" (n-order+1)'s 
	 * Note2: the matrix is likely to be sparse.					
	 */
		 	
	/* get storage for the matrix b described above */
	mat.nrow = nrow = npts;
	mat.ncol = ncol =  npts + 3;

	/* note 2 copies of the matrix are needed for the inversion;
	 * the "mat.ary" matrix is denoted by "MAT" and the "b" matrix
	 * is denoted by "B".
	 */
	mat.ary = (UU_REAL *) uu_malloc(npts*(ncol)*sizeof(UU_REAL));
	b = (UU_REAL *) uu_malloc(npts*(ncol)*sizeof(UU_REAL));

	*incptr = maxknot / (UU_REAL)(npts - 1);
	cur_basis_minknot = 0;
	cur_basis_maxknot = cur_basis_maxknot_index = 1;
	nbrzeros = 1; /* nbr of times zero has been assigned to 
					   * "cur_basis_minknot" 
						*/
	for (j=0; j<npts; j++)
		{					 
		/* for each basis function;i.e. each col. of B corresponding 
		 * to a basis function 
		 */
		u = 0.0;
		i = 0; 
		/* start at top of column put zeros in the column up to, and 
		 * including "cur_basis_minknot" 
		 */
		while (u < cur_basis_minknot)
			{
			B(i,j) = 0.0;
			i++;  /* go down column */
			u += *incptr;
			}
		/* calculate the values for this basis function where
		 * it is nonzero; note, u is in [0,maxknot].
		 */
		while (u <= cur_basis_maxknot + UM_FUZZ)  
			{
			B(i,j) = um_basis(u,cur_basis_maxknot_index,order,maxknot);
			if (trace)
				{
				sprintf(UM_sbuf,"u=%g, B(%d,%d)=%g cur_basis_maxknot_index=%d",
						u,i,j,B(i,j),cur_basis_maxknot_index);
				um_pscroll(UM_sbuf);
				}
			i++;
			u += *incptr;
			}
		for ( ; i<npts; i++) /* fill the rest of the col with 0's */
			B(i,j) = 0.0;

		/* now recalc "cur_basis_maxknot" and "cur_basis_minknot"
	 	 * for next um_basis function  
	 	 */
		if (cur_basis_maxknot < maxknot)
			cur_basis_maxknot++;
		if (nbrzeros == order) 		/* "order" zeros used as knots */
			cur_basis_minknot++;
		else nbrzeros++;					

		cur_basis_maxknot_index++; /* this is needed because of 
											 * the way "um_basis" works	
											 */
	}										/* end, constructing each column of B */
					
	/* solve system of linear eqns, bx=c, */
	/* augment B with control pt  coordinates */

	for (i=0; i<npts; i++)
		{
		for (k=0; k<3; k++) 
			{
			j = npts + k;
			B(i,j) = cntlpts[i][k];
			}
		}
	if (trace)  /* for debugging only */
		{
		for (i=0; i<npts; i++)
			{
			sprintf(UM_sbuf,"row=%d, of matrix %d",i,j);
			um_pscroll(UM_sbuf);
			for (k=0; k<ncol; k++)
				{
				sprintf(UM_sbuf," %g\0",B(i, k));
				um_pscroll(UM_sbuf);
			}
		}
	}

	/* copy B into MAT, "um_eval_matrix" changes the matrix */
	for (i=0; i<npts; i++)
		for (k=0; k<ncol; k++)
			UM_MAT(i,k) = B(i,k);

	if (um_eval_matrix(UM_SOLVEQNS,eps,&mat,coeff,&deter) 
		== UM_ILLCONDITIONED)
		{
		um_pscroll("from um_c6_bsplcrv, matrix is ill-conditioned,");
		um_pscroll("the inverted bspline may be a poor representation");
		}

	/* now replace original points with new control points */
	for (i=0; i<npts; i++)
		{
		for (j=0; j<3; j++)
			cntlpts[i][j] = coeff[i][j];
		}

	uu_free(mat.ary);
	uu_free(b);
	uu_dexit;
	}/* end, getting inverted contlpts */


/* the following 2 "undef"s apply to umi_get_inverted_cntlpts above */
#undef B
#undef UM_MAT


#define UM_MAXNUMATOMS  12  /* the nbr of atoms to be retrieved from unibase; */
								/*each atom = 3 real nbrs in this case, and each  */
#define BSPLBUFSZ	UM_MAXNUMATOMS * 3
/*********************************************************************
**    E_FUNCTION: int um_tr6_tranbsplcrv(eptr, offset)
**
**			DESCRIPTION: Translate a bspline curve and update unibase.
**       
**			PARAMETERS   
**				INPUT: 
**					eptr          pointer to the bspline entity record.
**
**					offset        direction vector for translation.
**          
**				OUTPUT : none.  
**          
**			RETURNS      : none
**
**			SIDE EFFECTS : Replaces the control points in the variable list
**            with translated control points.
**
**			WARNINGS     : none
*********************************************************************/
int
um_tr6_tranbsplcrv(eptr, offset)
	struct UM_bsplcrv_rec *eptr;
	UM_vector offset;
	{
	int i,j;				/*loop counters							  */

	uu_denter(UU_MTRC, (us, "um_tr6_tranbsplcrv(%x, %x)", eptr, offset));

	um_trxx_trangenent(eptr, offset);
	/*
	for (i=0,j=0; i<eptr->no_pt; i++,j=j+3) 
		{
		um_vcplvc(&eptr->pt[j], offset, &eptr->pt[j]);
		}
	ur_update_data_varlist(eptr->key, 1, eptr->pt, 1, eptr->no_pt);
	*/
	uu_dexit;
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION: int um_tf6_tranfbsplcrv(eptr,tranfmat,store)
**			 Transform a bspline curve with a transformation matrix
**			PARAMETERS   
**				INPUT: 
**					eptr         pointer to a bspline entity to transform.
**					tranfmat     transformation matrix.
**					store			 TRUE iff the transformed entity is to be stored
**									 in UNIBASE.
**				OUTPUT :  none.  
**			RETURNS      : none
**			SIDE EFFECTS : Replaces the variable list of control points with
**					new control points that have been tranformed.
**			WARNINGS     : none
*********************************************************************/
int
um_tf6_tranfbsplcrv(eptr,tranfmat,store)
	struct UM_bsplcrv_rec *eptr; /*ptr to bspline entity */
	UM_transf tranfmat;	/*transformation matrix	*/
	UU_LOGICAL store;
	{
	int i,j;					/*loop counters						  */

	uu_denter(UU_MTRC, (us, 
		"um_trt6_tranfbsplcrv(key:%d, tranfmat:%x,store:%d)", 
		eptr->key, tranfmat, store));

	um_tfxx_tranfgenent(eptr, tranfmat, store);
	/*
	for (i=0,j=0; i<eptr->no_pt; i++,j=j+3)	
		{
		um_cctmtf(&eptr->pt[j], tranfmat, &eptr->pt[j]);
		}
	if (store)
		ur_update_data_varlist(eptr->key, 1, eptr->pt, 1, eptr->no_pt);
	*/
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION: int um_cp6_copybsplcrv(eptr1, eptr2, bagsize)
**
**			DESCRIPTION: Copies the data for one bspline entity into 
**					another bspline entity. 
**       
**			PARAMETERS   
**				INPUT: 
**					eptr1         entity pointer to a bspline curve.
**
**					bagsize		  size of storage for entity.
**
**					eptr2         entity pointer the storage for the new
**                           bspline entity.
**          
**				OUTPUT :  
**					eptr2         pointer to a copy of the original bspline
**                           data pointed to by "eptr1".
**          
**			RETURNS      : none
**
**			SIDE EFFECTS : none
**
**			WARNINGS     : none
*********************************************************************/
int
um_cp6_copybsplcrv(bsplptr1, bsplptr2, bagsize)
	struct UM_bsplcrv_rec *bsplptr1;
	struct UM_bsplcrv_rec *bsplptr2;
	int bagsize;
	{
	int temp_key;
	struct UM_attrdata_rec attrbag;

	uu_denter(UU_MTRC, (us, "um_cp6_copybsplcrv(%x, %x, %d)", 
						bsplptr1, bsplptr2, bagsize));

	um_cpxx_copygenent(bsplptr1, bsplptr2, bagsize);
	/*
	ur_setup_data(bsplptr1->rel_num, bsplptr2, bagsize);
	strcpy (bsplptr2->label, "");
	bsplptr2->subscr = 0;
	temp_key = bsplptr1->key;
	um_get_disp_attr(bsplptr1->key, &attrbag);
	if (um_create_geom(bsplptr1, UM_DEFAULT_TF, &attrbag) != 0)
		uu_uerror0(/*from cp6_bsplcrv, error on create_data*@UM_MODEL,8);
	else
		{
		ur_setup_data(UM_BSPLCRV_REL, bsplptr2, sizeof(struct UM_bsplcrv_rec));
		strcpy (bsplptr2->label, "");
		bsplptr2->subscr = 0;
		bsplptr2->key = bsplptr1->key;
		um_get_all_geom(bsplptr2,sizeof(struct UM_bsplcrv_rec));
		}
	bsplptr1->key = temp_key;
	*/
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_sc6_scalbsplcrv(eptr,pt,scalmat)
**      Scale a bspline curve around a point
**    PARAMETERS   
**       INPUT  : 
**				eptr        pointer to the entity to be scaled
**          scalpt      point to be scaled about
**          scalmat     the 4x3 scaling matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**   SIDE EFFECTS  : replaces the variable list of control points with
**        				new control points that have been scaled.
**    WARNINGS     : none
*********************************************************************/
int
um_sc6_scalbsplcrv(eptr,pt,scalmat)
	struct  UM_bsplcrv_rec *eptr;
	UM_coord pt;
	UM_transf scalmat;

	{
	int i,j;
	UM_coord temppt;

	uu_denter(UU_MTRC, (us,"um_sc6_scalbsplcrv(%x, %x, %x)", eptr, pt, scalmat));

	um_scxx_scalgenent(eptr, pt, scalmat);
	/*
	for (i=0,j=0; i<eptr->no_pt; i++,j=j+3)
		{
		um_vcmnvc(&eptr->pt[j], pt, temppt);
		um_cctmmx(temppt, scalmat, temppt);
		um_vcplvc(temppt, pt, &eptr->pt[j]);
		}
	ur_update_data_varlist(eptr->key, 1, eptr->pt, 1, eptr->no_pt);
	*/
	uu_dexit;
	return (UU_SUCCESS);
	}
