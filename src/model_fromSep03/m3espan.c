
/*********************************************************************
**    NAME         :  mespan.c
**       CONTAINS:
**			um_ents_coplanar()	-	two entities coplanar?
**			um_ents_colinear()	-	two entities collinear?
**			-- NOTE: above two routines are almost identical	--			*
**       um_span_keylist()	-	gets affine span of list of mtid's
**       um_span_cidlist()	-	gets affine span of list of cid structures
**       um_span_ptlist()	-	gets affine span of list of points
**       um_span_elist()	-	gets affine span of list of entity pointers
**			um_span_entity()	-	gets span for single entity (a method)
**			um_netspan()		-	accumulates span of two affine spaces
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3espan.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:55
*********************************************************************/

/*********************************************************************
**
**	NOTE:	In these routines, the term "affine space" is used to mean a
**		linear object in 3-space which does not necessarily go through
**		the origin.  These are the spaces that entities share to be
**		colinear or coplanar.  The "span" of an entity (or set of entities)
**		is the minimal affine space which it occupies (they all occupy). 
**
**		Affine spaces are defined by two variables
**			int	dim;					* space dimension	*
**			UU_REAL	space[2][3];	* dimensioned space definition	*
**		as follows:
**				dim = 0	(point)	:	space = point coordinates/null
**				dim = 1	(line)	:	space = point on line, direction vector
**				dim = 2	(plane)	:	space = point on plane, normal vector
**				dim = 3	(skew)	:	space = null/null
**		where "null" means not defined, not referenced by any function
**		NOTE: vector space[1] will be unit length in all routines
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "modef.h"
#include "mdebug.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION :  UU_LOGICAL	um_ents_coplanar(e1, t1, e2, t2)
**       determines if two entities are coplanar (after transformation)
**    PARAMETERS   
**       INPUT  : 
**          e1 (e2)	-	first	(second) entity
**				t1 (t2)	-	first (second) transformation to common space
**						if t is UM_DEFAULT_TF, identity transformation is assumed
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE	if coplanar, else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL
um_ents_coplanar(e1, t1, e2, t2)
	struct	UC_entitydatabag	*e1;
	UM_transf	t1;
	struct	UC_entitydatabag	*e2;
	UM_transf	t2;

	{
	struct	UC_entitydatabag	*ents[2];
	UU_REAL	*tfmats[2];
	int		dim;
	UU_REAL	space[2][3];
	int		badent;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_ents_coplanar()"));

	ents[0] =  e1;
	ents[1] =  e2;

	tfmats[0] = (UU_REAL *) t1;
	tfmats[1] = (UU_REAL *) t2;

	um_span_elist(2, ents, tfmats, &dim, space, &badent);
	
	uu_dexit;
	return ( (badent == -1 && dim < 3)? UU_TRUE: UU_FALSE);
	}
/*********************************************************************
**    E_FUNCTION :  UU_LOGICAL	um_ents_colinear(e1, t1, e2, t2)
**       determines if two entities are colinear (after transformation)
**    PARAMETERS   
**       INPUT  : 
**          e1 (e2)	-	first	(second) entity
**				t1 (t2)	-	first (second) transformation to common space
**						if t is UM_DEFAULT_TF, identity transformation is assumed
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE	if colinear, else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL
um_ents_colinear(e1, t1, e2, t2)
	struct	UC_entitydatabag	*e1;
	UM_transf	t1;
	struct	UC_entitydatabag	*e2;
	UM_transf	t2;

	{
	struct	UC_entitydatabag	*ents[2];
	UU_REAL	*tfmats[2];
	int		dim;
	UU_REAL	space[2][3];
	int		badent;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_ents_colinear()"));

	ents[0] =  e1;
	ents[1] =  e2;

	tfmats[0] = (UU_REAL *) t1;
	tfmats[1] = (UU_REAL *) t2;

	um_span_elist(2, ents, tfmats, &dim, space, &badent);
	
	uu_dexit;
	return ( (badent == -1 && dim < 2)? UU_TRUE: UU_FALSE);
	}
/*********************************************************************
**    E_FUNCTION :  um_span_keylist(no_keys, keys, dim, space, badent)
**       determines affine span (point, line, plane, space) for 
**			a list of geometric entities (by mtid)
**    PARAMETERS   
**       INPUT  : 
**          no_keys	-	number of entities listed
**				keys		-	pointer to list of MTID's
**       OUTPUT :  
**          *dim	-	dimension of spanned space
**				space	-	point/vector combination defining spanned space
**				badent-	if no errors, -1, else, index into list for element
**							whose processing caused error
**    RETURNS      : 0 if AOK, -1 if couldn't get geom, -2 if span of
**							some entity could not be obtained
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_span_keylist(no_keys, keys, dimptr, space, badentptr)
	int	no_keys;
	UU_KEY_ID	*keys;
	int	*dimptr;
	UU_REAL	space[2][3];
	int	*badentptr;

	{
	int	i;
	int	errorbuf;
	int	tmpdim;
	UU_REAL	tmpspace[2][3];

	/* keylist	*/
	struct	UC_entitydatabag	e;
	UM_transf	tfmat;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_span_keylist()"));


	for (i = 0; i < no_keys; ++i)
		{
		/** get geometry, transformation, and span **/
		e.key= keys[i];
		uc_retrieve_data(&e, sizeof (e));
		uc_retrieve_transf(e.key, tfmat);

		if ( errorbuf = uc_span_entity(&e, &tmpdim, tmpspace))
			{
			*badentptr = i;
			goto Done;
			}

		/** transform span	**/
		if (tfmat != UM_DEFAULT_TF && tmpdim != 3)
			{
			um_cctmtf(tmpspace[0], tfmat, tmpspace[0]);
			if (tmpdim != 0)
				{
				um_vctmtf(tmpspace[1], tfmat, tmpspace[1]);
				um_unitvc(tmpspace[1], tmpspace[1]);
				}
			}

		if (i == 0)
			{	/* initialize accumulation with first span	*/
			*dimptr = tmpdim;
			um_vctovc(tmpspace[0], space[0]);
			um_vctovc(tmpspace[1], space[1]);
			}
		else
			{	/* accumulate span	*/
			if (um_netspan(*dimptr, space, tmpdim, tmpspace, dimptr, space ) == 3)
				break;	/* no point in going further	*/
			}
		}

	/* successful completion	*/
	*badentptr = -1;
	errorbuf = 0;

Done:
	uu_dexit;
	return (errorbuf);
	}
/*********************************************************************
**    E_FUNCTION :  um_span_cidlist(no_cid, cids, dim, space, badent)
**       determines affine span (point, line, plane, space) for 
**			a list of geometric entities (by mtid)
**    PARAMETERS   
**       INPUT  : 
**          no_cid	-	number of cid's listed
**				cids		-	pointer to list of cid's
**       OUTPUT :  
**          *dim	-	dimension of spanned space
**				space	-	point/vector combination defining spanned space
**				badent-	if no errors, -1, else, index into list for element
**							whose processing caused error
**    RETURNS      : 0 if AOK, -1 if couldn't get geom, -2 if span of
**							some entity could not be obtained
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_span_cidlist(no_cid, cids, dimptr, space, badentptr)
	int	no_cid;
	struct	UM_cid_rec	*cids;
	int	*dimptr;
	UU_REAL	space[2][3];
	int	*badentptr;

	{
	int	i;
	int	errorbuf;
	int	tmpdim;
	UU_REAL	tmpspace[2][3];

	/* cidlist	*/
	struct	UC_entitydatabag	e;
	UM_transf	tfmat;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_span_cidlist()"));

	for (i = 0; i < no_cid; ++i)
		{
		/** get geometry, transformation, and span **/
		e.key= cids[i].crvid;
		uc_retrieve_data(&e, sizeof(e));
		uc_retrieve_transf(e.key, tfmat);

		if ( errorbuf = uc_span_entity(&e, &tmpdim, tmpspace))
			{
			*badentptr = i;
			goto Done;
			}

		/** transform span	**/
		if (tfmat != UM_DEFAULT_TF && tmpdim != 3)
			{
			um_cctmtf(tmpspace[0], tfmat, tmpspace[0]);
			if (tmpdim != 0)
				{
				um_vctmtf(tmpspace[1], tfmat, tmpspace[1]);
				um_unitvc(tmpspace[1], tmpspace[1]);
				}
			}

		if (i == 0)
			{	/* initialize accumulation with first span	*/
			*dimptr = tmpdim;
			um_vctovc(tmpspace[0], space[0]);
			um_vctovc(tmpspace[1], space[1]);
			}
		else
			{	/* accumulate span	*/
			if (um_netspan(*dimptr, space, tmpdim, tmpspace, dimptr, space ) == 3)
				break;	/* no point in going further	*/
			}
		}

	/* successful completion	*/
	*badentptr = -1;
	errorbuf = 0;

Done:
	uu_dexit;
	return (errorbuf);
	}
/*********************************************************************
**    E_FUNCTION :  um_span_ptlist(no_pts, pts, dim, space, badent)
**       determines affine span (point, line, plane, space) for 
**			a list of geometric entities (by mtid)
**    PARAMETERS   
**       INPUT  : 
**          no_pts	-	number of pt's listed
**				pts		-	list of pt's
**       OUTPUT :  
**          *dim	-	dimension of spanned space
**				space	-	point/vector combination defining spanned space
**				badent-	if no errors, -1, else, index into list for element
**							whose processing caused error
**    RETURNS      : 0 if AOK
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_span_ptlist(no_pts, pts, dimptr, space, badentptr)
	int	no_pts;
	UM_coord	pts[];
	int	*dimptr;
	UU_REAL	space[2][3];
	int	*badentptr;

	{
	int	i;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_span_ptlist()"));

	for (i = 0; i < no_pts; ++i)
		{
		if (i == 0)
			{
			/* initialize accumulation with first span	*/
			*dimptr = 0;
			um_vctovc(pts[0], space[0]);
			}
		else
			{
			/* accumulate span	*/
			if (um_netspan(*dimptr, space, 0, pts[i], dimptr, space ) == 3)
				break;	/* no point in going further	*/
			}

		}

	/* successful completion	*/
	*badentptr = -1;

	uu_dexit;
	return (0);
	}
/*********************************************************************
**    E_FUNCTION :  int um_span_elist(no_ents, eptrs, tfmats, dim, space, badent)
**       determines affine span (point, line, plane, space) for 
**			a list of geometric entities (by entity address);
**    PARAMETERS   
**       INPUT  : 
**          no_ents	-	number of entities listed
**				eptrs		-	list of pointers to entities
**				tptrs		-	list of pointers to transformation
**							(NULL means identity)
**		NOTE: transformation pointers MUST BE SUPPLIED.
**       OUTPUT :  
**          *dim	-	dimension of spanned space
**				space	-	point/vector combination defining spanned space
**				badent-	if no errors, -1, else, index into list for element
**							whose processing caused error
**    RETURNS      : 0 if AOK, -1 if couldn't get geom, -2 if span of
**							some entity could not be obtained
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_span_elist(no_ents, eptrs, tfmats, dimptr, space, badentptr)
	int	no_ents;
	struct	UC_entitydatabag	*eptrs[];
	UU_REAL	*tfmats[];
	int	*dimptr;
	UU_REAL	space[2][3];
	int	*badentptr;

	{
	int	i;
	int	errorbuf;
	int	tmpdim;
	UU_REAL	tmpspace[2][3];


	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_span_elist()"));


	for (i = 0; i < no_ents; ++i)
		{

		if ( errorbuf = uc_span_entity(eptrs[i], &tmpdim, tmpspace))
			{
			*badentptr = i;
			goto Done;
			}

		/** transform span	**/
		if (tfmats[i] != UM_DEFAULT_TF && tmpdim != 3)
			{
			um_cctmtf(tmpspace[0], tfmats[i], tmpspace[0]);
			if (tmpdim != 0)
				{
				um_vctmtf(tmpspace[1], tfmats[i], tmpspace[1]);
				um_unitvc(tmpspace[1], tmpspace[1]);
				}
			}

		if (i == 0)
			{	/* initialize accumulation with first span	*/
			*dimptr = tmpdim;
			um_vctovc(tmpspace[0], space[0]);
			um_vctovc(tmpspace[1], space[1]);
			}
		else
			{	/* accumulate span	*/
			if (um_netspan(*dimptr, space, tmpdim, tmpspace, dimptr, space ) == 3)
				break;	/* no point in going further	*/
			}
		}

	/* successful completion	*/
	*badentptr = -1;
	errorbuf = 0;

Done:
	uu_dexit;
	return (errorbuf);
	}
/*********************************************************************
**    E_FUNCTION :  um_span_entity(eptr, dimptr, space)
**       A method-- returns the affine span of an entity
**    PARAMETERS   
**       INPUT  : 
**          eptr	-	entity 
**       OUTPUT :  
**          *dimptr-	dimension of affine space
**				space	--	definition of affine space (point/vector)
**				NOTE:	vector space[1] will be made unit length
**    RETURNS      : 0, of no problem, -2 if not defined 
**    SIDE EFFECTS : posts an error message if not defined
**    WARNINGS     : none
*********************************************************************/

um_span_entity(eptr, dimptr, space)
	struct	UC_entitydatabag	*eptr;
	int		*dimptr;
	UU_REAL	space[2][3];

	{
	int	ret_val = 0;
	int	badent;
	UM_coord *pts;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_span_entity()"));
	switch (eptr->rel_num)
		{
		case UM_POINT_REL:
			*dimptr = 0;
			um_vctovc(((struct UM_point_rec *) eptr)->pt, space[0]);
			break;

		case UM_LINE_REL:
			*dimptr = 1;
			um_vctovc( ((struct UM_line_rec *) eptr)->spt, space[0]);
			um_vcmnvc( ((struct UM_line_rec *) eptr)->ept,
				((struct UM_line_rec *) eptr)->spt, space[1]);
			um_unitvc(space[1], space[1]);
			break;

		case UM_CIRCLE_REL:
			*dimptr = 2;
			um_vctovc( ((struct UM_circle_rec *) eptr)->center, space[0]);
			um_vctovc( ((struct UM_circle_rec *) eptr)->nvec, space[1]);
			um_unitvc(space[1], space[1]);
			break;

		case UM_CONIC_REL:
			*dimptr = 2;
			um_vctovc( ((struct UM_conic_rec *) eptr)->tfmat[3], space[0]);
			um_vctovc( ((struct UM_conic_rec *) eptr)->tfmat[2], space[1]);
			um_unitvc(space[1], space[1]);
			break;

		case UM_COMPCRV_REL:
			ret_val = um_span_cidlist( ((struct UM_compcrv_rec *) eptr)->no_cid,
				((struct UM_compcrv_rec *) eptr)->cid, dimptr, space, &badent);
			break;
			
		case UM_RBSPLCRV_REL:
			pts = (UM_coord *) ((struct UM_rbsplcrv_rec *) eptr)->pt;
			ret_val = um_span_ptlist( ((struct UM_rbsplcrv_rec *) eptr)->no_pt,
				pts, dimptr, space, &badent);
			break;

		case UM_POLY_REL:
			ret_val = um_span_ptlist( ((struct UM_poly_rec *) eptr)->numvtx,
				((struct UM_poly_rec *) eptr)->vertex, dimptr, space, &badent);
			break;

		case UM_BODY_REL:
			*dimptr = 3;
			break;

		default:
			/** FIX: error message	**/
			um_p_ary(UM_PINT,
				"um_span_entity: unimplemented relation:", 1, &(eptr->rel_num));
			ret_val = -2;
		}

	uu_dexit;
	return(ret_val);
	}
/*********************************************************************
**    E_FUNCTION :  um_netspan(dim1, space1, dim2, space2, netdim, netspace )
**       Accumulates the affine dimension spanned by geometric objects
**    PARAMETERS   
**       INPUT  : 
**          dim1 (dim2)			-	the dimension of first (second) affine space
**				space1 (space2)	-	definition of the first (second) space
**       OUTPUT :  
**          netdim_ptr
**				netspace				-	as above for inputs, these define the 
**				span of the two inputs.  For example, if two intersecting lines are
**				input, this routine will provide the definition of the plane they
**				span.  Passing skew lines, for example, will result in netdim == 3.
**				In such a case, the netspace data is not defined.
**
**				NOTE: netspace[1], when defined, will be unit length
**
**				NOTE: if dimensions passed are invalid (not from 0 to 3),
**					*netdim will be -1. (but see next note.)
**				NOTE:	dim1 is allowed to be -1, in which case, netdim and
**					netspace will be copies of dim2 and space2, resp.
**    RETURNS      : netdim, for your convenience
**    SIDE EFFECTS : none
**    WARNINGS     : Unit vectors are both required in input, and
**							insured for output.
*********************************************************************/

/* matrix of cases	*/
#define	UM_PAIRING(A, B)	( ((A) << 4) + (B) )

#define	UM_PT_PT_SPAN			UM_PAIRING( 0, 0)
#define	UM_PT_LINE_SPAN		UM_PAIRING( 0, 1)
#define	UM_PT_PLANE_SPAN		UM_PAIRING( 0, 2)
#define	UM_LINE_LINE_SPAN		UM_PAIRING( 1, 1)
#define	UM_LINE_PLANE_SPAN	UM_PAIRING( 1, 2)
#define	UM_PLANE_PLANE_SPAN	UM_PAIRING( 2, 2)

um_netspan(dim1, space1, dim2, space2, netdim_ptr, netspace )
	int		dim1;
	UU_REAL	space1[][3];
	int		dim2;
	UU_REAL	space2[][3];
	int		*netdim_ptr;
	UU_REAL	netspace[][3];

	{
	UU_REAL	(*s1)[3];	/* s1 will be space of lower dimension	*/
	UU_REAL	(*s2)[3];
	UU_REAL tol,rnum;
	int		pairing;		/* a number made of the two input dimensions
								 * takes on values of symbolic constants such
								 * as UM_PT_LINE_SPAN
								 */
	UM_coord	nearpt[3];
	UM_vector tempvec[3];

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_netspan()"));

/*
.....Get machining tolerance
*/
	getsct(&tol);
	/* special handling, used in first iteration of a loop	*/
	if (dim1 == -1)
		{
		*netdim_ptr =  dim2;
		um_vctovc(space2[0], netspace[0]);
		if (dim2 == 1 || dim2 == 2)
			um_vctovc(space2[1], netspace[1]);
		goto Done;
		}
	if (dim1 <= dim2)
		{
		s1 = space1;
		s2 = space2;
		pairing = UM_PAIRING(dim1, dim2);
		}
	else
		{
		s1 = space2;
		s2 = space1;
		pairing = UM_PAIRING(dim2, dim1);
		}

	switch (pairing)
		{
	 case	UM_PT_PT_SPAN:
		if ( um_cceqcc_tol(s1[0], s2[0],tol) )	/* same point	*/
			{
			*netdim_ptr = 0;
			um_vctovc(s1[0], netspace[0]);
			}
		else												/* two different points	*/
			{
			*netdim_ptr = 1;
			um_vctovc(s1[0], netspace[0]);		/* first point	*/
			um_vcmnvc(s2[0], s1[0], netspace[1]);	/* vector between two	*/
			um_unitvc(netspace[1], netspace[1]);
			}
		break;

	 case	UM_PT_LINE_SPAN:
		um_nptln(s1[0], s2[0], s2[1], nearpt);	/* get nearest point on line	*/
		if ( um_cceqcc_tol(nearpt, s1[0],tol) )	/* point on line	*/
			{
			*netdim_ptr = 1;
			um_vctovc(s2[0], netspace[0]);	/* pass back original line	*/
			um_vctovc(s2[1], netspace[1]);
			}
		else											/* point not on line		*/
			{
			*netdim_ptr = 2;
			um_vctovc(s1[0], netspace[0]);	/* point on plane is point s1	*/
			um_vcmnvc(s1[0], nearpt, tempvec);	/* independent vector	*/
			um_cross(tempvec, s2[1], netspace[1]);
			um_unitvc(netspace[1], netspace[1]);
			}
		break;

	 case	UM_PT_PLANE_SPAN:
		um_nptpln(s1[0], s2[0], s2[1], nearpt);
		if ( um_cceqcc_tol(nearpt, s1[0],tol) )	/* point on plane	*/
			{
			*netdim_ptr = 2;					/* copy him his plane	*/
			um_vctovc(s2[0], netspace[0]);
			um_vctovc(s2[1], netspace[1]);
			}
		else		/* point not on plane	*/
			{
			*netdim_ptr = 3;				/* no space definition needed	*/
			}
		break;

	 case	UM_LINE_LINE_SPAN:
		um_nptln(s1[0], s2[0], s2[1], nearpt); /* between basepoint1 and line2 */
		if (um_vcparall(s1[1], s2[1]))	/* parallel directions?	*/
			{
			if (um_cceqcc_tol(s1[0], nearpt,tol))			/* conicident lines	*/
				{
				*netdim_ptr = 1;
				um_vctovc(s1[0], netspace[0]);	/* pass back first line	*/
				um_vctovc(s1[1], netspace[1]);
				}
			else							/* distinct parallel lines	*/
				{
				*netdim_ptr = 2;
				um_vctovc(s1[0], netspace[0]);		/* point from first line	*/
				um_vcmnvc(nearpt, s1[0], tempvec);	/* independent vector	*/
				um_cross(tempvec, s2[1], netspace[1]);
				um_unitvc(netspace[1], netspace[1]);
				}
			}
		else						/* not parallel	*/
			{
			if (um_cceqcc_tol(s1[0], nearpt,tol))		/* intersect at s1[0]	*/
				{
				*netdim_ptr = 2;
				um_vctovc(s1[0], netspace[0]);			/* intersection point	*/
				um_cross(s1[1], s2[1], netspace[1]);	/*  cross direction vc's */
				um_unitvc(netspace[1], netspace[1]);
				}
			else													/* s2[0] not on s1	*/
				{
				um_vcmnvc(nearpt, s1[0], tempvec);		/* independent of s2[1]	*/
				um_cross(tempvec, s2[1], tempvec);
				um_unitvc(tempvec, tempvec);
				um_cross(s1[1], s2[1], netspace[1]);
				um_unitvc(netspace[1], netspace[1]);
				rnum = um_dot(netspace[1],tempvec);

				if (fabs(rnum) >= .999999)	/* coplanar	*/
					{
					*netdim_ptr = 2;
					um_vctovc(s1[0], netspace[0]);
					/* netspace[1] already set */
					}
				else											/* not coplanar	*/
					{
					*netdim_ptr = 3;
					/* netspace is to be ignored	*/
					}
				}
			}
		break;

	 case	UM_LINE_PLANE_SPAN:
		um_nptpln(s1[0], s2[0], s2[1], nearpt);
		if (um_cceqcc_tol(s1[0], nearpt,tol) && um_vcperp_tol(s1[1],s2[1],1.e-5))
		/* line in plane condition */
			{
			*netdim_ptr = 2;
			um_vctovc(s2[0], netspace[0]);
			um_vctovc(s2[1], netspace[1]);
			}
		else
			{
			*netdim_ptr = 3;
			}
		break;

	 case	UM_PLANE_PLANE_SPAN:
		um_nptpln(s1[0], s2[0], s2[1], nearpt);
		if (um_cceqcc_tol(s1[0], nearpt,tol) && um_vcparall(s1[1], s2[1]))
		/* congruent planes condition */
			{
			*netdim_ptr = 2;
			um_vctovc(s2[0], netspace[0]);
			um_vctovc(s2[1], netspace[1]);
			}
		else
			{
			*netdim_ptr = 3;
			}
		break;

	 default:
		*netdim_ptr = -1;
		/** FIX: proper error message here?	**/
		}

Done:
	uu_dexit;
	return(*netdim_ptr);
	}
