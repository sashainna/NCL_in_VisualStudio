
/*********************************************************************
**    NAME         :  m7math2.c
**       CONTAINS:
**			um_icircir(cen1,unvc1,rad1,cen2,unvc2,rad2,nint,pt)
**			UU_REAL um_getarclen(eptr, tfmat)
**			umi_calcarclen(su, eu, eptr, tfmat, resultptr)
**			int umi_arclenfunc(u, resultptr)
**			int umi_integrate(a, b, func, tol, valptr)
**			int	um_quadratslv(a, b, c, degen, xary)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m7math2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:08
*********************************************************************/
#include <errno.h>
#include "usysdef.h"
#include "ustdio.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdeval.h"
#include "modef.h"
#include	"misect.h"

/*********************************************************************
**    E_FUNCTION     : um_icircir(cen1,unvc1,rad1,cen2,unvc2,rad2,nint,pt)
**      Intersect two circles known to lie in the same plane.
**    PARAMETERS   
**       INPUT  : 
**				cen1                center of circle 1
**          unvc1               unit normal vector of circle 1
**          rad1                radius of circle 1
**          cen2                center of circle 2
**          unvc2               unit normal vector of circle 2
**          rad2                radius of circle 2
**       OUTPUT :  
**				nint                number of intersection points
**          pt                  array of intersection points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_icircir(cen1,unvc1,rad1,cen2,unvc2,rad2,nint,pt)
	UM_coord cen1;
	UM_vector unvc1;
	UM_length rad1;
	UM_coord cen2;
	UM_vector unvc2;
	UM_length rad2;
	int  *nint;
	UM_coord pt[];

	{
	UU_REAL x, y;				/* legs of right triangle */
	UU_REAL d;					/* distance between circle centers */
	UU_REAL lr, sr;			/* largest and smallest radius */
	UM_vector v;				/* unit vector from center1 to center2 */
	UM_vector q, w;		/* temporary vectors */

	if (rad1 > rad2)
		{
		lr = rad1;
		sr = rad2;
		}
	else
		{
		lr = rad2;
		sr = rad1;
		}
	d = um_dcccc(cen1, cen2);
	um_vcmnvc(cen2, cen1, v);
	um_unitvc(v, v);
	if ( (( (lr - sr) - UM_FUZZ)<d) && (d<( (lr + sr) - UM_FUZZ) ))
		{
		x = d / 2.0 + (rad1 *rad1 - rad2 *rad2) / (2.0 *d);
		y = sqrt(rad1 *rad1 - x *x);
		um_cross(unvc1, v, w);
		um_vctmsc(w, y, w);
		um_vctmsc(v, x, q);
		um_vcplvc(q, cen1, q);
		 *nint = 2;
		um_vcplvc(q, w, pt[0]);
		um_vcmnvc(q, w, pt[1]);
		}
	else if ( (fabs(d - (lr - sr) )<UM_FUZZ) || (fabs(d - (lr + sr) )<UM_FUZZ) )
		{
		um_vctmsc(v, rad1, q);
		 *nint = 1;
		um_vcplvc(cen1, q, pt[0]);
		}
	else
		{
		 *nint = 0;
		}

	return (0);
	}

/****************************************************************************
**		E_FUNCTION: int um_getarclen1 (su, eu, eptr, tfmat, dlen)
**
**			DESCRIPTION: returns the arc length of a curve by calling
**			umi_calcarclen (just an interface)
****************************************************************************/
int um_getarclen1 (su, eu, eptr, tfmat, dlen)
UM_param su,eu;
struct UM_crvdatabag *eptr;
UM_transf tfmat;
UU_REAL *dlen;
{
	return (umi_calcarclen(su, eu, eptr, tfmat, dlen));
}

/****************************************************************************
**		E_FUNCTION: UU_REAL um_getarclen(eptr, tfmat)
**
**			DESCRIPTION: returns the arc length of a curve.
**
**			PARAMETERS:
**				INPUT: 
**          PARAMETER                   MEANING
**          eptr               pointer to a curve entity whose
**                             arc length is desired.
**				tfmat					 transformation matrix.
**				OUTPUT: none.
**
**			RETURNS: a real value indicating the arc length.
**
**			SIDE EFFECTS: none.
**
**			WARNINGS: none.
**
****************************************************************************/
UU_REAL um_getarclen(eptr, tfmat)
	struct UM_crvdatabag *eptr;
	UM_transf tfmat;

	{
	UM_coord scc;
	UM_coord ecc;
	UU_REAL len,t0,t1;
	UM_vector vec;
	int i;
	UU_LOGICAL first = UU_TRUE;
	struct UM_crvdatabag tptr;
                 
	uu_denter(UU_MTRC,(us,"um_getarclen(eptr->key:%d, tfmat:%x)",
					eptr->key, tfmat));
	if (uc_super_class(eptr->rel_num) == UM_CURVE_CLASS)
		{
		switch(eptr->rel_num)
			{
			case UM_LINE_REL:
				{
				struct UM_line_rec *ptr;
				ptr = (struct UM_line_rec *) eptr;
				if (tfmat != UM_DEFAULT_TF)
					{
					um_cctmtf(ptr->spt, tfmat, scc);
					um_cctmtf(ptr->ept, tfmat, ecc);
					um_vcmnvc(ptr->spt, ptr->ept, vec);
					}
				else
					um_vcmnvc(ptr->spt, ptr->ept, vec);
				len = um_mag(vec);
				}
				break;
			case UM_CIRCLE_REL:
				{
				struct UM_circle_rec *ptr;
				ptr = (struct UM_circle_rec *) eptr;
				if (tfmat != UM_DEFAULT_TF)
					{
					um_vctmsc(ptr->svec, ptr->radius, vec);
					um_vctmtf(vec, tfmat, vec);
					len = um_mag(vec);
					len = len * fabs(ptr->dang);
					}
				else
					len = ptr->radius * fabs(ptr->dang); 
				}
				break;
			case UM_COMPCRV_REL:
				{
				struct UM_compcrv_rec *ptr;
				struct UM_crvdatabag subcrv; /* used to get arc length */
				UM_transf subcrv_tfmat;

				ptr = (struct UM_compcrv_rec *) eptr;
				if (tfmat != UM_DEFAULT_TF)
					/* check to see if the transformation has a scale factor */
					if (um_scale_in_tf(tfmat)) 
						{	/* find new length of composite crv */
						len = 0;
						t0 = ptr->t0;
						t1 = ptr->t1;
						for (i=0; i<ptr->no_cid; i++) 
							{	/* get length of each subcrv */
							if (t0 > ptr->cid[i].endparam) continue;
							subcrv.key = ptr->cid[i].crvid;
							um_get_all_geom(&subcrv, sizeof(struct UM_crvdatabag));
							um_get_transformation(subcrv.key, subcrv_tfmat);
							um_tftmtf(tfmat, subcrv_tfmat, subcrv_tfmat);
							um_c5_trimpart(&subcrv,ptr->cid,ptr->no_cid,t0,t1,i,
								&first,subcrv_tfmat);
							tptr.key = subcrv.key;
							ncl_retrieve_data_fixed(tptr);
							len = len + um_getarclen(tptr, subcrv_tfmat);
							ur_update_data_fixed(&subcrv);
							if (t1 - ptr->cid[i].endparam < UM_DFUZZ) break;
							}
						}
					else len = ptr->arclen;
				else len = ptr->arclen;
				}
				break;
			default:
/* 
... aak 13-jan-1998: used to be:
				umi_calcarclen((UU_REAL) 0.0, (UU_REAL) 1.0, eptr, tfmat, &len);	
... this is not very accurate for big curves -> divided [0,1] into 4 intervals
... length of each interval below is calculated via 32-point Gaussian quadr.
*/
				{
				int nint = 4;
				UU_REAL len1, u0, du;

				du = 1./nint;
				len = 0.; u0 = 0.;
				for (i=0; i<nint; i++, u0 += du)
					{
					umi_calcarclen(u0, u0 + du, eptr, tfmat, &len1);	
            	len += len1;
					}
				break;
				}
			}
		}
	else
		uu_uerror1(UM_MODEL, 141, eptr->rel_num);
			/* message is: um_getarclen: undefined relation of %d */
	uu_dexit;
	return(len);
	}

/* used in the integration functions below */
static struct UM_crvdatabag *UM_arclen_eptr;
static UU_REAL *UM_arclen_tfmat;
static struct UM_evcrvout *UM_arclen_evout;

/*********************************************************************
**    I_FUNCTION: umi_calcarclen(su, eu, eptr, tfmat, resultptr)
**			DESCRIPTION: This function calculates the arc length of a
**					curve entity.
**			PARAMETERS   
**				INPUT: 
**					su            start parameter for the portion of the curve
**				                 to have its arc length computed.
**
**					eu            end parameter for the portion of the curve
**                           to have its arc length computed.
**
**					eptr          pointer to the curve entity whose length is
**                           desired.
**					tfmat			  transformation that positions the curve entity in
**									  space.
**				OUTPUT :  
**					resultptr     pointer to the computed arc length.
**			RETURNS      : UM_VALID if the arc length was computed;
**							   UM_INVALID if the arc length could not be computed.
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
umi_calcarclen(su, eu, eptr, tfmat, resultptr)
	UM_param su;
	UM_param eu;
	struct UM_crvdatabag *eptr;
	UM_transf tfmat;
	UU_REAL *resultptr;

	{						
	int status;
	struct UM_evcrvout evout;
	int umi_arclenfunc(); 

	uu_denter(UU_MTRC,(us,"umi_calcarclen(%g,%g,eptr->key:%d,tfmat:%x,%x)",
							su,eu,eptr->key,tfmat,resultptr));

	/* assign globals for umi_arclenfunc (i.e. function to integrate)  */
	UM_arclen_eptr = eptr;
	UM_arclen_tfmat = (UU_REAL *) tfmat;
	UM_arclen_evout = &evout;

	/* initialize evaluator and calculate the arclength of the entity */
	uc_init_evcrvout(UM_arclen_eptr, UM_arclen_evout);
	status = umi_integrate(su, eu, umi_arclenfunc, resultptr);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    I_FUNCTION: int umi_arclenfunc(u, resultptr)
**			DESCRIPTION: This procedure returns the value of the math function 
**					that must be integrated in order to find the arc length of 
**					the curve associated with the entity pointed to by "eptr"
**					in "um_calcarclen".
**			PARAMETERS   
**				INPUT: 
**					u             parameter value at which the arc length function
**                           is to be evaluated.
**				OUTPUT :  
**					resultptr     pointer to the computed value of the arc length
**                           function when evaluated at "u".
**			RETURNS      : 
**				status returned by uc_evcrv
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
int
umi_arclenfunc(u, resultptr)
	UU_REAL u;
	UU_REAL *resultptr;

	{													
	UU_REAL *derv;
	int status;

	uu_denter(UU_MTRC,(us,"umi_arclenfunc(%g,%x)",u,resultptr));
	status = uc_evcrv(UM_FRSTDERIV, u, UM_arclen_eptr, UM_arclen_tfmat,
		UM_arclen_evout);
	derv = UM_arclen_evout->dcdu;
	*resultptr = 
		sqrt(derv[0] * derv[0] + derv[1] * derv[1] + derv[2] * derv[2]);
	uu_dexit;
	return (status);
	}

static UU_REAL x[32] = {
  0.02435029266342443, 0.07299312178779904, 0.12146281929612055,
  0.16964442042399282, 0.21742364374000708, 0.26468716220876742,
  0.31132287199021096, 0.35722015833766812, 0.40227015796399160,
  0.44636601725346409, 0.48940314570705296, 0.53127946401989455,
  0.57189564620263403, 0.61115535517239325, 0.64896547125465734,
  0.68523631305423324, 0.71988185017161083, 0.75281990726053190,
  0.78397235894334141, 0.81326531512279756, 0.84062929625258036,
  0.86599939815409282, 0.88931544599511411, 0.91052213707850281,
  0.92956917213193958, 0.94641137485840282, 0.96100879965205372,
  0.97332682778991096, 0.98333625388462596, 0.99101337147674432,
  0.99634011677195528, 0.99930504173577214
  };
static UU_REAL w[32] = {
  0.04869095700913972, 0.04857546744150343, 0.04834476223480296,
  0.04799938859645831, 0.04754016571483031, 0.04696818281621002,
  0.04628479658131442, 0.04549162792741814, 0.04459055816375656,
  0.04358372452932345, 0.04247351512365359, 0.04126256324262353,
  0.03995374113272034, 0.03855015317861563, 0.03705512854024005,
  0.03547221325688238, 0.03380516183714161, 0.03205792835485155,
  0.03023465707240248, 0.02833967261425948, 0.02637746971505466,
  0.02435270256871087, 0.02227017380838325, 0.02013482315353021,
  0.01795171577569734, 0.01572603047602472, 0.01346304789671864,
  0.01116813946013113, 0.00884675982636395, 0.00650445796897836,
  0.00414703326056247, 0.00178328072169643
  };

/*********************************************************************
**    I_FUNCTION: int umi_integrate(a, b, func, tol, valptr)
**			DESCRIPTION: This function integrates the real valued function, func,
**					on the interval [a, b].
**			PARAMETERS   
**				INPUT: 
**					a             starting point of the domain of integration.
**					b             ending point of the domain of integration.
**					func          real valued function to be integrated.
**				OUTPUT :  
**					valptr        pointer to the value of the integral.
**			RETURNS : Returns UM_VALID if the integral could be computed,
**             otherwise UM_INVALID is returned.         
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
int
umi_integrate(a, b, func, valptr)
	UU_REAL a;		/*lower bound of integral	 */
	UU_REAL b;		/*upper bound of integral	 */
	int (*func)();  /*function to be integrated  */
	UU_REAL *valptr;	/*ptr to value to be returned*/		

	{
	UU_REAL da;
	UU_REAL db;
	UU_REAL sum;
	UU_REAL alpha;
	UU_REAL beta;
	UU_REAL y1;
	UU_REAL y2;
	UU_REAL fy1;
	UU_REAL fy2;
	int i;
	int status;

	da = a;
	db = b;
	sum = 0.0;
	alpha = 0.5 * (db - da);
	beta = 0.5 * (da + db);
	/* in general: z = (2.0 * u - (da + db))/(db - da) */
	status = UM_VALID;
  for (i=0; i<32 && status == UM_VALID; i++)
  		{
		/* z = (2.0 * (x[i] * (db - da) + da) - (da + db)) / (db - da); */
		y1 = (alpha * x[i]) + beta;
		y2 = beta - (alpha * x[i]);
		status = (*func)(y1, &fy1);
		status = (*func)(y2, &fy2);
		sum = sum + w[i] * (fy1 + fy2);
		}
	*valptr = sum * alpha;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION :  int	um_quadratslv(a, b, c, degen, xary)
**       solve one variable quadratic equation
**    PARAMETERS   
**       INPUT  : 
**          a, b, c	-- 	axx + bx + c = 0
**       OUTPUT :  
**				*degen	--		0 for normal quad, two roots
**									1 for quad really a line, one root
**									2 for quad, double root (first element of xary)
**									-1 for no solution
**									-2 for completely degen. equation, c = 0.
**          xary[2]	--		roots
**    RETURNS      : 0 for first cases of *degen. If *degen < 0, returns
**							degen.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_quadratslv(a, b, c, degen, xary)
	UU_REAL	a;
	UU_REAL	b;
	UU_REAL	c;
	int		*degen;
	UU_REAL	xary[2];
	{
	int	ret_val;
	UU_REAL	discr;

	uu_denter(UU_MTRC,(us,"um_quadratslv(%f, %f, %f)", a, b, c));

	if ( UM_ZILCH(a) )	/* degenerates to linear	bx + c = 0 */
		{
		ret_val = 1;
		if ( UM_ZILCH(b) )	/* c = 0	*/
			{
			*degen = -2;
			ret_val = -2;
			}
		else
			{
			*degen = 1;
			ret_val = 0;
			xary[0] = -c/b;
			}
		}
	else /* solve axx + bx + c = 0 */
		{
		discr = b*b - 4*a*c;				/* discriminant	*/
	
		if (discr < -UM_FUZZ)			/* no real solution	*/
			{
			*degen = -1;
			ret_val = -1;
			}
		else if (UM_ZILCH (discr) )	/* double root	*/
			{
			*degen = 1;
			ret_val = 0;
			xary[0] = -b / (2*a);
	
			/* (in case caller doesn't test *degen)	*/
			xary[1] = -b / (2*a);
			}
		else									/* two roots	*/
			{
			UU_REAL	sqrtdiscr;
	
			sqrtdiscr = sqrt(discr);
			*degen  = 0;
			ret_val = 0;
			xary[0] = (-b + sqrtdiscr)/ (2*a);
			xary[1] = (-b - sqrtdiscr)/ (2*a);
			}
		}

	uu_dexit;
	return (ret_val);
	}
