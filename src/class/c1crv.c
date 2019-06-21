/*********************************************************************
**    NAME         :  c1crv.c
**       CONTAINS: class method dispatchers for curve operations
**			int uc_evcrv(evflag, u, crvptr, tfmat, crvout) 
**			int uc_init_evcrvout(crvptr, crvout)
**			int uc_cctou(crvptr, tfmat, pt, u, distp)
**			int uc_utot(crvptr, u, t)
**			int uc_altou(crvptr, ua, u)
**			int uc_crv_intersect(eptr0, tfmat0, eptr1, tfmat1,
**			int uc_crv_intersect_sp(eptr0, tfmat0, eptr1, tfmat1,
**			int uc_crv_to_unirbsc(crvptr, unirbsc)
**			int uc_crv_to_agrbsc(crv, agrbsc)
**			int uc_reverse_curve(crv)
**			int uc_tan_line_thru_pt(eptr, tfmat, pt, ploc, lptr)
**			int uc_tan_tan_line(eptr1, tfmat1, ploc1,
**			int uc_trim_extend_curve(eptr1, tfmat, ploc1, 
**				eptr2, tfmat, ploc1) 
**			int uc_crv_fillet(radius, eptr1, ploc1,
**				eptr2, ploc2, isect1, isect2, fillet)
**			int uc_split_curve(eptr, u, eptr1, eptr2) 
**			int uc_midtrim_curve(ploc, isect1, isect2, eptr)
**			int uc_dissolve(eptr)
**			int uc_project_to_plane(eptr, pt, normal, neweptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       c1crv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:57
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "class.h"
#include "canbe.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mdebug.h"
#include "misect.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*******************************************************************
**  E_FUNCTION: int uc_evcrv1(evflag, u, crvptr, tfmat, crvout) 
**		Given a curve entity (CRVPTR) with associated transformation
**		matrix (TFMAT which may be UM_DEFAULT_TF), calculate the
**		data requested by EVFLAG at the logical parameter value U
**		[0.0 <= U <= 1.0] and return it in CRVOUT.
**    PARAMETERS   
**       INPUT  : 
**          evflag				UM_POINT=>		point
**										UM_FRSTDERIV=> 1st deriv plus above
**										UM_SECDERIV=>	2nd deriv plus above
**										UM_CURVATURE=> curvature plus above
**				u						parameter value to evaluate curve function
**				crvptr				pointer to curve entity
**				tfmat					transformation matrix
**       OUTPUT :  
**          crvout				curve evaluator record to put results
**    RETURNS      : 
**			UM_VALID only; ultimately will return extended diagnostic
**    SIDE EFFECTS : 
**			The curve evaluator record (CRVOUT) may be updated with
**			information which makes subsequent evaluator calls faster
**    WARNINGS     : 
**			The curve evaluator record (CRVOUT) must be initialized
**			before the first call to uc_evcrv() with a new curve.
*********************************************************************/
int
uc_evcrv1(evflag, u, crvptr, tfmat, crvout)
	int evflag;
	UM_param u;
	struct UC_entitydatabag  *crvptr;
	UM_transf tfmat;
	struct UM_evcrvout *crvout;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC, 
		(us,"uc_evcrv(evflag:%d,u:%g,key:%d,tfmat:%x,crvout:%x)",
		evflag,u,crvptr->key,tfmat,crvout));

	class = (*crvptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_EVCRV);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(evflag, u, crvptr, tfmat, crvout);
		}

	uu_dexitstatus("uc_evcrv", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int uc_init_evcrvout(crvptr, crvout)
**			Initialize an evaluator record (CRVOUT) for curves to handle
**			the evaluation of the specified curve (CRVPTR) entity.
**    PARAMETERS   
**       INPUT: 
**				crvptr				the curve entity to have an evaluator record
**										set up for.
**       OUTPUT:  
**				crvout				pointer to the set up evaluator record. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_init_evcrvout(crvptr, crvout)
	struct UC_entitydatabag *crvptr;
	struct UM_evcrvout *crvout;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC,(us, "uc_init_evcrvout(crvptr->rel_num:%d, crvout:%x)", 
											crvptr->rel_num, crvout));

	class = (*crvptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_INIT_EVCRVOUT);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(crvptr, crvout);
		}

	uu_dexitstatus("uc_init_evcrvout", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int uc_cctou(crvptr, tfmat, pt, u, distp)
**			Given a point (PT) and a curve (CRVPTR), determine the parameter
**			value U such that the curve evaluated at U will give the
**			point. The point MUST be on the curve. The distance of the
**			point on the curve evaluated at U and the given point PT is
**			returned
**    PARAMETERS   
**       INPUT: 
**				crvptr				curve that the point lies on
**				pt						point lying on curve
**       OUTPUT:  
**				u						logical parameter [0.0 <= u <= 1.0]
**				distp					distance of PT to CURVE(U)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uc_cctou(crvptr, tfmat, pt, u, distp)
	struct UC_entitydatabag *crvptr;
	UM_transf tfmat;
	UM_coord pt;
	UM_param *u;
	UM_length *distp;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC,(us, "uc_cctou(key=%x)", crvptr->key));

	class = (*crvptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_CCTOU);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(crvptr, tfmat, pt, u, distp);
		}

	uu_dexitstatus("uc_cctou", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int uc_utot(crvptr,u, t)
**			Given a parameter (U) and a curve (CRVPTR), determine the parameter
**			value T such that the curve evaluated at T will give the same
**			point as that evaluated at U. The point MUST be on the curve.
**			The distance of the point on the curve evaluated at U and
**		 	the point at T is returned.
**    PARAMETERS   
**       INPUT: 
**				crvptr				curve that the point lies on
**				u						logical parameter [0.0 <= u <= 1.0]
**       OUTPUT:  
**				t						physical parameter
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uc_utot(crvptr,u, t)
	struct UC_entitydatabag *crvptr;
	UM_param u;
	UM_param *t;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC,(us, "uc_utot(key=%x)", crvptr->key));

	class = (*crvptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_UTOT);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(crvptr, u, t);
		}

	uu_dexitstatus("uc_utot", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int uc_altou(crvptr, ua, u)
**			Given a "parameter" specified as a percenatage of arc
**			length (UA) and a curve (CRVPTR), determine the logical
**			parameter value U such that the curve evaluated at U
**			will result in a point that is the percentage of arc 
**			length along the curve.
**    PARAMETERS   
**       INPUT: 
**				crvptr				curve that the point lies on
**				ua						arclen parameter [0.0 <= ua <= 1.0]
**       OUTPUT:  
**				u						logical parameter [0.0 <= u <= 1.0]
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_altou(crvptr, ua, u)
	struct UC_entitydatabag *crvptr;
	UM_param ua;
	UM_param *u;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC,(us, "uc_altou(key=%x, ua=%g)", crvptr->key, ua));

	class = (*crvptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_ALTOU);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(crvptr, ua, u);
		}

	uu_dexitstatus("uc_altou", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int uc_pt_on_crv_at_pal(crvptr)
**			Create points on a curve at percentage of arc length along
**			the curve.
**    PARAMETERS   
**       INPUT: 
**				crvptr				pointer to curve
**       OUTPUT:  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_pt_on_crv_at_pal(crvptr)
	struct UC_entitydatabag *crvptr;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC,(us, "uc_pt_on_crv_at_pal(key=%x)", crvptr->key));

	class = (*crvptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_PT_ON_CRV_AT_PAL);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(crvptr);
		}

	uu_dexitstatus("uc_pt_on_crv_at_pal", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_crv_intersect(eptr0, tfmat0, eptr1, tfmat1,
**								nintp, no_ibuf, ibuf)
**       Intersect two curves (EPTR0, TFMAT0) and (EPTR1, TFMAT1).
**			The number of intersection points (NINITP) and the intersection
**			points (IBUF) are returned.  NO_IBUF specifies the maximum
**			number of intersection points which may be stored in IBUF.
**    PARAMETERS   
**       INPUT  : 
**          eptr0						pointer to curve entity
**				tfmat0					transformation matrix for eptr0
**											(may be UM_DEFAULT_TF)
**          eptr1						pointer to curve entity
**				tfmat1					transformation matrix for eptr1
**											(may be UM_DEFAULT_TF)
**				no_ibuf					maximum number of intersection
**											points to put in IBUF
**       OUTPUT :  
**          nintp						number of intersection points found
**				ibuf						array of point intersection records
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_crv_intersect(eptr0, tfmat0, eptr1, tfmat1, nintp, no_ibuf, ibuf)
	struct UC_entitydatabag	*eptr0;
	UM_transf	tfmat0;
	struct UC_entitydatabag	*eptr1;
	UM_transf	tfmat1;
	int	*nintp;
	int	no_ibuf;
	UM_isect	ibuf[];

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class0, class1;
	int i;
	UM_param t,u;
	
	uu_denter(UU_MTRC,(us,"uc_crv_intersect(e0(r,k,t)=(%d,%x,%x), e1(r,k,t)=(%d,%x,%x), bufsiz=%d)",
				eptr0->rel_num, eptr0->key, tfmat0,
				eptr1->rel_num, eptr1->key, tfmat1, no_ibuf));
		  
	/* try to intersect curve 0 with curve 1 */
	class0 = (*eptr0).rel_num;
	class0 = CONVERT_REL_TO_CLASS(class0);
	function = ucu_validate(&UC_cot_descriptor, class0, UC_CRV_INTERSECT);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(eptr0, tfmat0, eptr1, tfmat1, 
			nintp, no_ibuf, ibuf);
		}

	/* if can't intersect 0 with 1, then try to intersect 1 with 0 */
	if (status != UU_SUCCESS)
		{
		class1 = (*eptr1).rel_num;
		class1 = CONVERT_REL_TO_CLASS(class1);
		function = ucu_validate(&UC_cot_descriptor, class1, UC_CRV_INTERSECT);
		if(function != UC_UNDEFINED_METHOD)
			{
			status = (*function)(eptr1, tfmat1, eptr0, tfmat0, 
				nintp, no_ibuf, ibuf);
			}
		if (status == UU_SUCCESS)
			{
			for (i=0; i<*nintp; i++)
				{
				t = ibuf[i].t0;
				ibuf[i].t0 = ibuf[i].t1;
				ibuf[i].t1 = t;
				u = ibuf[i].u0;
				ibuf[i].u0 = ibuf[i].u1;
				ibuf[i].u1 = u;
				}
			}
		}

	uu_dexitstatus("uc_crv_intersect", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_crv_intersect_sp(eptr0, tfmat0, eptr1, tfmat1,
**								common_plane, nintp, no_ibuf, ibuf)
**       Intersect two curves (EPTR0, TFMAT0) and (EPTR1, TFMAT1) which
**			are known to lie in the same plane (COMMON_PLANE).  The number
**			of intersection points (NINITP) and the intersection points
**			(IBUF) are returned.  NO_IBUF specifies the maximum number of 
**			intersection points which may be stored in IBUF.
**    PARAMETERS   
**       INPUT  : 
**          eptr0						pointer to curve entity
**				tfmat0					transformation matrix for eptr0
**											(may be UM_DEFAULT_TF)
**          eptr1						pointer to curve entity
**				tfmat1					transformation matrix for eptr1
**											(may be UM_DEFAULT_TF)
**				common_plane			plane definition entities lie in
**				no_ibuf					maximum number of intersection
**											points to put in IBUF
**       OUTPUT :  
**          nintp						number of intersection points found
**				ibuf						array of point intersection records
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_crv_intersect_sp(eptr0, tfmat0, eptr1, tfmat1, common_plane,
	nintp, no_ibuf, ibuf)
	struct UC_entitydatabag	*eptr0;
	UM_transf	tfmat0;
	struct UC_entitydatabag	*eptr1;
	UM_transf	tfmat1;
	UU_REAL common_plane[2][3];
	int	*nintp;
	int	no_ibuf;
	UM_isect	ibuf[];

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class0, class1;
	int i;
	UM_param t,u;
	
	uu_denter(UU_MTRC,(us,"uc_crv_intersect_sp(e0(r,k,t)=(%d,%x,%x), e1(r,k,t)=(%d,%x,%x), bufsiz=%d)",
				eptr0->rel_num, eptr0->key, tfmat0,
				eptr1->rel_num, eptr1->key, tfmat1, no_ibuf));
		  
	/* try to intersect curve 0 with curve 1 */
	class0 = (*eptr0).rel_num;
	class0 = CONVERT_REL_TO_CLASS(class0);
	function = ucu_validate(&UC_cot_descriptor, class0, UC_CRV_INTERSECT_SP);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(eptr0, tfmat0, eptr1, tfmat1, common_plane,
			nintp, no_ibuf, ibuf);
		}

	/* if can't intersect 0 with 1, then try to intersect 1 with 0 */
	if (status != UU_SUCCESS)
		{
		class1 = (*eptr1).rel_num;
		class1 = CONVERT_REL_TO_CLASS(class1);
		function = ucu_validate(&UC_cot_descriptor, class1, UC_CRV_INTERSECT_SP);
		if(function != UC_UNDEFINED_METHOD)
			{
			status = (*function)(eptr1, tfmat1, eptr0, tfmat0, common_plane,
				nintp, no_ibuf, ibuf);
			}
		if (status == UU_SUCCESS)
			{
			for (i=0; i<*nintp; i++)
				{
				t = ibuf[i].t0;
				ibuf[i].t0 = ibuf[i].t1;
				ibuf[i].t1 = t;
				u = ibuf[i].u0;
				ibuf[i].u0 = ibuf[i].u1;
				ibuf[i].u1 = u;
				}
			}
		}

	uu_dexitstatus("uc_crv_intersect_sp", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_crv_to_unirbsc(crvptr, unirbsc)
**       Convert a curve to an equivalent UNICAD non-uniform rational
**       bspline curve.
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
uc_crv_to_unirbsc(crvptr, unirbsc)
	struct UC_entitydatabag *crvptr;
	struct UM_rbsplcrv_rec *unirbsc;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC, (us,"uc_crv_to_unirbsc(key=%x)", crvptr->key));

	class = (*crvptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_CRV_TO_UNIRBSC);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(crvptr, unirbsc);
		}

	uu_dexitstatus("uc_crv_to_unirbsc", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_crv_to_agrbsc(crv, agrbsc)
**       Convert a curve to an equivalent APPLIED GEOMETRY non-uniform
**			rational bspline curve.
**    PARAMETERS   
**       INPUT  : 
**          crv					any curve entity
**       OUTPUT :  
**          agrbsc				equivalent AG non-uniform rational bspline
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_crv_to_agrbsc(crvptr, agrbsc)
	struct UC_entitydatabag *crvptr;
	struct UM_agcrv_rec *agrbsc;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC, (us,"uc_crv_to_agrbsc(key=%x)", crvptr->key));

	class = (*crvptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_CRV_TO_AGRBSC);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(crvptr, agrbsc);
		}

	uu_dexitstatus("uc_crv_to_agrbsc", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_reverse_curve(crv)
**			Reverse the parameterization of the curve.
**    PARAMETERS   
**       INPUT  : 
**          crv					any curve entity
**       OUTPUT :  
**          crv					any curve entity
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_reverse_curve(crvptr)
	struct UC_entitydatabag *crvptr;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC, (us,"uc_reverse_curve(key=%x)", crvptr->key));

	class = (*crvptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_REVERSE_CRV);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(crvptr);
		}

	uu_dexitstatus("uc_reverse_curve", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_tan_line_thru_pt(eptr, tfmat, pt, ploc, lptr)
**       Create a line through a given point (PT) and tangent to a 
**			entity (EPTR, TFMAT). 
**    PARAMETERS   
**       INPUT  : 
**          eptr				circle entity
**				tfmat				transformation matrix (may be UM_DEFAULT_TF)
**				pt					point line is to go through
**				ploc				pick location for discriminating between
**									multiple tangent points
**       OUTPUT :  
**          lptr				line entity
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_tan_line_thru_pt(eptr, tfmat, pt, ploc, lptr)
	struct UC_entitydatabag *eptr;
	UM_transf tfmat;
	UM_coord pt;
	UD_NDCLOCREC *ploc;
	struct UM_line_rec *lptr;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter( UU_MTRC,(us,"uc_tan_line_thru_pt(key=%x)",eptr->key));

	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_TAN_LINE_THRU_PT);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(eptr, tfmat, pt, ploc, lptr);
		}

	uu_dexitstatus("uc_tan_line_thru_pt", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_tan_tan_line(eptr1, tfmat1, ploc1,
**									eptr2, tfmat2, ploc2, lptr);
**			Create a line (LPTR) tangent to two entities (EPTR1, TFMAT1)
**			and (EPTR2, TFMAT2) near to where the entities were picked
**			(PLOC1 and PLOC2).
**    PARAMETERS   
**       INPUT  : 
**          eptr1				first entity
**				tfmat1			transformation matrix for first entity
**										(may be UM_DEFAULT_TF)
**				ploc1				picked location on first entity
**          eptr2				second entity
**				tfmat2			transformation matrix for second entity
**										(may be UM_DEFAULT_TF)
**				ploc2				picked location on second entity
**       OUTPUT :  
**          lptr				line tangent to two entities
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_tan_tan_line(eptr1, tfmat1, ploc1, eptr2, tfmat2, ploc2, lptr)
	struct UC_entitydatabag *eptr1;
	UM_transf tfmat1;
	UD_NDCLOCREC *ploc1;
	struct UC_entitydatabag *eptr2;
	UM_transf tfmat2;
	UD_NDCLOCREC *ploc2;
	struct UM_line_rec *lptr;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter( UU_MTRC,(us,"uc_tan_tan_line(key1=%x, key2=%x)",
		eptr1->key, eptr2->key));

	if ((eptr1->rel_num == UM_CIRCLE_REL) && (eptr2->rel_num == UM_CIRCLE_REL))
		status = um_c2_tt(eptr1, ploc1, eptr2, ploc2, lptr);
	else
		{
		class = (*eptr1).rel_num;
		class = CONVERT_REL_TO_CLASS(class);
		function = ucu_validate(&UC_cot_descriptor, class, UC_TAN_TAN_LINE);
		if(function != UC_UNDEFINED_METHOD)
			{
			status = (*function)(eptr1, tfmat1, ploc1, eptr2, tfmat2, ploc2, lptr);
			}
		}

	uu_dexitstatus("uc_tan_tan_line", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_crv_fillet(radius, eptr1, ploc1,
**									eptr2, ploc2, isect1, isect2, fillet)
**			Create a fillet (FILLET) tangent to two entities (EPTR1)
**			and (EPTR2) near to where the entities were picked
**			(PLOC1 and PLOC2). No trimming will be performed.
**    PARAMETERS   
**       INPUT  : 
**				radius			fillet radius
**          eptr1				first entity
**				ploc1				picked location on first entity
**          eptr2				second entity
**				ploc2				picked location on second entity
**       OUTPUT :  
**				isect1			fillet tangency point on first entity
**				isect1			fillet tangency point on second entity
**          fillet			fillet between the two entities
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_crv_fillet(radius, eptr1, ploc1, eptr2, ploc2, 
	isect1, isect2, fillet)
	UM_length radius;
	struct UC_entitydatabag *eptr1;
	UD_NDCLOCREC *ploc1;
	struct UC_entitydatabag *eptr2;
	UD_NDCLOCREC *ploc2;
	UM_isect *isect1;
	UM_isect *isect2;
	struct UC_entitydatabag *fillet;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter( UU_MTRC,(us,"uc_crv_fillet(key1=%x, key2=%x, radius=%f)",
		eptr1->key, eptr2->key, radius));

	class = (*eptr1).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_FILLET_CURVE);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function) (radius, eptr1, ploc1, eptr2, ploc2,
			isect1, isect2, fillet); 
		}

	uu_dexitstatus("uc_crv_fillet", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_trim_extend_curve(eptr1, tfmat, ploc1, 
**									eptr2, tfmat, ploc1) 
**			Trim/extend the first curve (EPTR1, TFMAT1, PLOC1) to the
**			second curve (EPTR2, TFMAT2, PLOC2).
**			
**    PARAMETERS   
**       INPUT  : 
**          eptr1				first curve entity
**				tfmat1			transformation matrix for first curve 
**										(may be UM_DEFAULT_TF)
**				ploc1				location picked on first curve
**          eptr2				second curve entity
**				tfmat2			transformation matrix for second curve 
**										(may be UM_DEFAULT_TF)
**				ploc2				location picked on second curve
**       OUTPUT :  
*8				none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			If successful, the definition of the first entity will change
**			in UNIBASE.
*********************************************************************/
int
uc_trim_extend_curve(eptr, tfmat, ptio, uall, uend)
	struct UC_entitydatabag *eptr;
	UM_transf *tfmat;
   UM_param *uall, uend;
   UM_coord ptio;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter( UU_MTRC,(us,"uc_trim_extend_curve(trimkey=%x, trimtokey=%x)",
		eptr[0].key,eptr[1].key));

	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_TRIM_EXTEND_CURVE);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function) (eptr, tfmat, ptio, uall, uend);
		}

	uu_dexitstatus("uc_trim_extend_curve", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_split_curve(eptr, u, eptr1, eptr2) 
**			Split a curve (EPTR) at a logical parameter U into
**			two pieces (EPTR1, EPTR2).
**    PARAMETERS   
**       INPUT  : 
**          eptr				curve entity
**       OUTPUT :  
**          eptr1				first part  entity
**          eptr2				second part entity
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_split_curve(eptr, u, udel, eptr1, eptr2)
	struct UC_entitydatabag *eptr;
	UM_param *u, *udel;
	struct UC_entitydatabag *eptr1;
	struct UC_entitydatabag *eptr2;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter( UU_MTRC,(us,"uc_split_curve(key=%x, u=%f)", eptr->key, u));

	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_SPLIT_CURVE);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(eptr, &u[0], udel, eptr1, eptr2);
		}

	uu_dexitstatus("uc_split_curve", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_midtrim_curve(ploc, isect1, isect2, eptr)
**			Use the intersection buffers (ISECT1 and ISECT2) to split the
**			curve entity (EPTR) into three pieces (note: the t0 parameter
**			value is assumed to be on the curve EPTR). Then use the picked
**			location (PLOC) to determine if the middle piece or the two
**			end pieces of the curve should be discarded (i.e. throw away
**			the middle piece if the user picked on that segment).
**    PARAMETERS   
**       INPUT  : 
**          ploc						picked location
**				isect1					intersection buffer
**				isect2					intersection buffer
**				eptr						curve entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
uc_midtrim_curve(eptr, tfmat, pt1, pt2, uall, uend)
  struct UC_entitydatabag *eptr;
  UM_transf *tfmat;
  UM_param *uall, uend;
  UM_coord pt1, pt2;
 {
  UC_METHOD function, ucu_validate();
  int status = UU_FAILURE;
  int class;

	uu_denter(UU_MTRC,(us,"uc_midtrim_curve(key=%x)", eptr->key));

	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_MIDTRIM_CURVE);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(eptr, tfmat, pt1, pt2, uall, uend);
		}
	uu_dexitstatus("uc_midtrim_curve", status);
	return (status);
  }

/*********************************************************************
**    E_FUNCTION     : int uc_dissolve(eptr)
**			Dispatch to the appropriate routine to dissolve the data
**			associated with the given record. 
**    PARAMETERS   
**       INPUT  : 
**				eptr		pointer to an entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_dissolve(eptr)
struct UC_entitydatabag  *eptr;
	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;

	uu_denter(UU_MTRC,(us,"uc_dissolve(e(r,k)=(%d,%x)",
		(*eptr).key,(*eptr).rel_num));
	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_DISSOLVE);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		status = (*function)(eptr);
	uu_dexitstatus("uc_dissolve", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_project_to_plane(eptr, pt, normal, neweptr)
**			Project an entity (EPTR) onto a plane (PT, NORMAL).
**    PARAMETERS   
**       INPUT  : 
**				eptr		pointer to an entity
**				pt			point definint plane
**				normal	normal defining plane
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_project_to_plane(eptr, pt, normal, neweptr)
struct UC_entitydatabag  *eptr;
UM_coord pt;
UM_vector normal;
struct UC_entitydatabag  *neweptr;
	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;

	uu_denter(UU_MTRC,(us,"uc_project_to_plane(e(r,k)=(%d,%x)",
		(*eptr).rel_num,(*eptr).key));
	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_PROJECT_TO_PLANE);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		status = (*function)(eptr, pt, normal, neweptr);
	uu_dexitstatus("uc_project_to_plane", status);
	return(status);
	}

