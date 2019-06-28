
/*********************************************************************
**    NAME         :  axhcsup.c
**       CONTAINS:
**			 Interfaces to C routines needed for cross hatching
**			ua_isect_line
**			ua_xh_evcrv
**			ua_floor
**			ua_xhtype()
**			ua_loadcache
**			ua_flushcache
**			ua_span_entity
**			ua_xherrortoint
**			ua_select()
**			ua_init_select()
**			ua_gnxt()
**			ua_next_entity()
**			ua_next_comp()
**
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       axhcsup.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:43
**		NOTE: These routines use a single-deep cache for retrieved entity
**		geometry.  Until further work, they are not suitable for mixing in
**		with other routines which pass a geometry key
*********************************************************************/
#include	"usysdef.h"
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) axhcsup.c 3.5 9/30/87 13:44:03 single"};
#else
static char uu_sccsident[]={"@(#) axhcsup.c 3.5 9/30/87 13:44:03 double"};
#endif

#include	"udebug.h"
#include	"mdebug.h"
#include	"mcvh.h"
#include	"mcrv.h"
#include	"modef.h"
#include	"mdrel.h"
#include	"mdcoord.h"
#include	"mdeval.h"
#include	"misect.h"
#include	"mderror.h"
#include	"mdclass.h"
#include "mdgenent.h"
#include	"mdeval.h"
#include	"umath.h"
#include "dasnog.h"
#include "adraft.h"
#include "adrf.h"
#include "gobas.h"

#define	UM_NOKEY	0			/* invalid value for cache_key	*/
static	int	cache_key = UM_NOKEY;
									/* should be proper type: used drafting precedent */
static	struct	UM_entitydatabag	cache_entity;
static	UM_transf	cache_transf;
static   UM_transf  *cache_tfmat;

/** this structure insulates code from structure def'n changes	**/
typedef	struct	ua_isect_clone {
	UU_REAL	pt[3];	/* intersection point */
	UU_REAL	t0;		/* user parameter for intersection point on curve 0	*/
	UU_REAL	t1;		/* user parameter for intersection point on curve 1	*/
	int		order;	/* 0 means transverse, 1 means tangent, etc.				*/
	};

UU_LOGICAL ua_next_comp();
UU_LOGICAL	select_initialize = UU_FALSE;
extern UU_LOGICAL UA_xh_regeneration;

/* these are for ua_next_entity() & ua_next_comp() */
UU_LOGICAL not_comp_curve = UU_TRUE;
int ith_curve = 0;
struct UM_compcrv_rec comp_rec;


/*********************************************************************
**    E_FUNCTION :  int ua_isect_line(pt, vec, key, nintp, no_ibuf, ibuf, cpln)
**       SAL interface: intersects line with geometry
**    PARAMETERS   
**       INPUT  : 
**          UM_coord		pt		-	point on line
**				UM_vector	vec	-	UNIT direction of line
**				int			key	-	geometry key
**				int			*nintp-	buffer for answer
**				int			no_ibuf- number of slots allocated in ibuf
**				ua_isect_clone ibuf-	intersection data buffer
**				UA_cpln		cpln - 	construction plane of boundary geometry
**       OUTPUT :  
**          *nintp		number of intersections found (a tangent intersection
**								counts as one intersection, with order = 1).
**				ibuf			filled up with intersection data for each intersection
**    RETURNS      : zero if OK, else, passes back error from modeling
**    SIDE EFFECTS : Uses and may change the geometry cache.
**    WARNINGS     : vec must be UNIT length if inverted parameters are to be
**							correct
*********************************************************************/
int
ua_isect_line(pt, vec, key,  nintp, no_ibuf, ibuf)
	UM_coord	pt;
	UM_coord	vec;
	int	key;
	int	*nintp;
	int	no_ibuf;
	struct	ua_isect_clone	ibuf[];
{
	UM_isect	*isectp;
	int	i;
	int	ret_val = 0;
	int	status;
	UM_coord off_set;
	extern UU_REAL UA_hatch_common_plane[2][3];
	extern UU_REAL UA_xh_size;

	struct	UM_line_rec			l;

	uu_denter(UU_STRC,(us,"ua_isect_line(pt <%g,%g,%g>, vec <%g,%g,%g>, key %d)",
		pt[0], pt[1], pt[2], vec[0], vec[1], vec[2], key));

	/* if cache doesn't hold geometry for key...	*/
	if (key != cache_key)
		if ((ret_val = ua_loadcache(key)) != 0)
		{
			goto Done;
		}

	/* allocate UM_isect buffer	*/
	isectp = (UM_isect *) uu_malloc (no_ibuf * sizeof(UM_isect));

	/* create entity out of line data	*/
	ur_setup_data(UM_LINE_REL, &l, sizeof (l));	/*	FIX: see comment below	*/
	um_vctmsc(vec, UA_xh_size, off_set);
	um_vcmnvc(pt, off_set, l.spt);
	um_vcplvc(pt, off_set, l.ept);
	uu_denter2(UU_STRC,(us,"ua_isect_line: spt = (%g %g %g) ept( %g %g %g)",
	l.spt[0], l.spt[1], l.spt[2], l.ept[0], l.ept[1], l.ept[2]));
	uu_dexit;
	ret_val = uc_crv_intersect_sp(&l, UM_DEFAULT_TF, &cache_entity, cache_tfmat,
							UA_hatch_common_plane,nintp, no_ibuf, isectp);

	if (ret_val != UM_OK)
	{
		uu_free(isectp);
		goto Done;
	}

	/* copy intersection data	*/
	for (i = 0; i < *nintp; ++i)
		{
		if(vec[0] != 0.0)
			{
			ibuf[i].t0 = ((l.spt[0] - pt[0]) + 2.0*isectp[i].u0*off_set[0])/vec[0];
			}
		else
			{
			if (vec[1] != 0.0)
				{
				ibuf[i].t0 = ((l.spt[1] - pt[1]) + 2.0*isectp[i].u0*off_set[1])/vec[1];
				}
			else
				{
				ibuf[i].t0 = ((l.spt[2] - pt[2]) + 2.0*isectp[i].u0*off_set[2])/vec[2];
				}
			}
		ibuf[i].t1 = isectp[i].u1;
		ibuf[i].order = isectp[i].order;
		um_vctovc(isectp[i].pt, ibuf[i].pt);
		uu_denter2(UU_STRC,(us,"ua_isect_line: t0 %g t1 %g order %d pt %g %g %g",
			ibuf[i].t0, ibuf[i].t1, ibuf[i].order, ibuf[i].pt[0], 
			ibuf[i].pt[1], ibuf[i].pt[2]));
		uu_dexit;
		}

	uu_free(isectp);
Done:
	uu_dexit;
	return (ret_val);
}

/*********************************************************************
**    E_FUNCTION :  ua_xh_evcrv(evflag, key, t, pt)
**			xhatch interface to evaluator; point or 1st derivative
**    PARAMETERS   
**       INPUT  : 
**				int 	evflag - UA_POINT or UA_FRSTDERIV
**				int		key	-	geometry key
**				UU_REAL	t		-	parameter value ([0,1])
**       OUTPUT :  
**          	UU_REAL	pt[3] -	answer
**    RETURNS      : 0 if OK, else various errors for model failures
**    SIDE EFFECTS : uses and may change the geometry cache
**    WARNINGS     : none
*********************************************************************/
int
ua_xh_evcrv(evflag, key, t, pt)
	int 		evflag;
	int		key;
	UU_REAL	t;
	UU_REAL	pt[3];
{
	struct UM_evcrvout	ev;
	UM_transf	tfmat;
	int	ret_val = 0;
	int	status;
	int	rel_num;

	uu_denter(UU_STRC,(us,"ua_xh_evcrv(evflag %d key %d, t %g)", evflag, key, t));

	if (key != cache_key)
	{
		if (ur_retrieve_data_relnum(key, &rel_num) != UU_SUCCESS)
			return ( 0 );
	}
	else
	{
		rel_num = cache_entity.rel_num;
	}

	if (UM_CURVE_CLASS != uc_super_class(rel_num))
	{
		ret_val = -1;
		goto Done;
	}

	/* if cache doesn't hold geometry for key...	*/
	if (key != cache_key)
		if ((ret_val = ua_loadcache(key)) != 0)
			goto Done;

	uc_init_evcrvout(&cache_entity, &ev);

	if (uc_evcrv(evflag, t, &cache_entity, cache_tfmat, &ev) != UM_VALID)
	{
		ret_val = -4;
		goto Done;
	}

	switch (evflag)
	{
		case UM_POINT: um_vctovc(ev.cp, pt);
							break;
		case UM_FRSTDERIV: um_vctovc(ev.dcdu, pt);
							break;
		default:			uu_denter2(UU_STRC,(us,"xh_evcrv: bad evflag %d",
								evflag));
							uu_dexit;
	}

Done:
	uu_dexit;
	return (ret_val);
}

/*********************************************************************
**    E_FUNCTION :  int ua_floor(num)
**       interface to math floor() routine
**    PARAMETERS   
**       INPUT  : 
**          UU_REAL	num
**       OUTPUT :  
**          none
**    RETURNS      : largest integer not greater than num
**    SIDE EFFECTS : none
**    WARNINGS     : none
**		REMARKS		 : should not be necessary.  SAL  should allow assignment
**							with truncation or rounding automatically
*********************************************************************/

int	ua_floor(num)
	UU_REAL	num;
{
	int	val;

	uu_denter(UU_STRC,(us,"ua_floor(num %g)", num));
	val = (int) floor(num);
	uu_dexit;
	return (val);
}

/*********************************************************************
**    E_FUNCTION :  ua_xhtype(key)
**       returns type value to be loaded into asso_block.
**			this type is now used to determine if an entity is a boundary curve,
**			or a mask (which will be various drafting entities).
**			If one returns type UA_BOUNDARY, xh_exec() will try to call
**			the evaluator to get a point on the curve.  If this isn't always
**			the proper approach, change xh_exec in addition to thi routine.
**    PARAMETERS   
**       INPUT  : 
**          int	key:	geometry key
**       OUTPUT :  
**          none
**    RETURNS      : UA_ASSO_BOUNDARY, UA_ASSO_XHMASK
**							-1 if error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_xhtype(key)
	int key;
{
	int	rel_num;
	int	type;

	uu_denter(UU_STRC,(us,"ua_xhtype(key %d)", key));

	if (key != cache_key)
	{
		if (ur_retrieve_data_relnum(key, &rel_num) != UU_SUCCESS)
			return ( 0 );
	}
	else
	{
		rel_num = cache_entity.rel_num;
	}

	switch (uc_super_class(rel_num))
	{
	case UM_CURVE_CLASS:
		type = UA_ASSO_BOUNDARY;
		break;
	
	default:
		type = -1;
	}

	uu_dexit;
	return (type);
}

/*********************************************************************
**    E_FUNCTION :  ua_loadcache(key)
**       loads the geometry cache used in this file with entity for key
**    PARAMETERS   
**       INPUT  : 
**          int	key:	unibase key
**       OUTPUT :  
**          none
**    RETURNS      : 0 if OK, else error code depending on failed model
**						routine: -3 for failed uc_retrieve_data()
**									-4 for failed uc_retrieve_transf()
**    SIDE EFFECTS : loads cache
**    WARNINGS     : none
*********************************************************************/
ua_loadcache(key)
	int	key;
{
	int	status;
	int	ret_val = 0;
	UU_LOGICAL um_is_idmat();

	uu_denter(UU_STRC,(us,"ua_loadcache(key %d)", key));

	cache_key = key;
	cache_entity.key = key;

	status = uc_retrieve_data(&cache_entity, sizeof (cache_entity));
	if (status != UM_OK)
	{
		ret_val = -3;
		cache_key = UM_NOKEY;
		goto Done;
	}
	status = uc_retrieve_transf(key, cache_transf);
	if (status != UM_OK)
	{
		ret_val = -4;
		cache_key = UM_NOKEY;
		goto Done;
	}

	/* use UM_DEFAULT_TF if possible for modeling code efficiency */
	if (um_is_idmat(cache_transf))
		cache_tfmat = UM_DEFAULT_TF;
	else 
		cache_tfmat = (UM_transf*)cache_transf;

Done:
	uu_dexit;
	return (ret_val);
}

/*********************************************************************
**    E_FUNCTION :  ua_flushcache()
**       invalidates the geometry cache, so it will be reloaded before 
**			next use.  Call this routine when done with routines using
**			the cache, to make sure you don't use out of date entity data
**			which might otherwise hang around the cache
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : invalidates cache
**    WARNINGS     : none
*********************************************************************/
ua_flushcache()
{
	uu_denter(UU_STRC,(us,"ua_flushcache(): current cache_key = %d", cache_key));
	cache_key = UM_NOKEY;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ua_span_entity()
**       (caching) SAL interface to uc_span_entity()
**    PARAMETERS   
**			SEE uc_span_entity()
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_span_entity(key, dim, space)
	int	key;
	int	*dim;
	UU_REAL	space[2][3];
{
	int	ret_val = 0;

	uu_denter(UU_STRC,(us,"ua_span_entity(key %d)", key));

	/* if cache doesn't hold geometry for key...	*/
	if (key != cache_key)
		if ((ret_val = ua_loadcache(key)) != 0)
			goto Done;

	if ((ret_val = uc_span_entity(&cache_entity, dim, space)) != 0)
	{
		uu_denter2(UU_STRC,(us,"ua_span_entity: uc_span_entity ERROR %d",
			ret_val));
		uu_dexit;
		goto Done;
	}

	/* transform span	*/
	if (*dim != 3)
	{
		um_cctmtf(space[0], cache_tfmat, space[0]);
		if (*dim != 0)
		{
			um_vctmtf(space[1], cache_tfmat, space[1]);
			um_unitvc(space[1], space[1]);
		}
	}
	
Done:
	uu_dexit;
	return (ret_val);
}

/*** THIS ROUTINE SHOULD BE REPLACED BY SOME MORE GENERAL CASTING OR
 ** SOMETHING IN SAL
 */
int ua_xherrortoint(err)
	int	err;
{
	return (err);
}


/*********************************************************************
**    E_FUNCTION :  ua_select(msgnumber, numpicks)
**       drafting interface to select subsystem
**    PARAMETERS   
**       INPUT  : 
**          int:	msgnumber:	prompt number in draft subsection
**       OUTPUT :  
**				int:	*numpicks	number of entities actually picked
**    RETURNS      : none
**    SIDE EFFECTS : loads the system default select buffer for
**							uz_gnxt() usage
**    WARNINGS     : none
*********************************************************************/

ua_select(msgnumber, numpicks)
	int	msgnumber;
	int	*numpicks;
{
	ud_ldas(UD_DASSELECT, UA_DRAFTING, msgnumber, 
		 			UU_NULL,  UM_MAXPICK, numpicks, UD_NODEFAULT);

	select_initialize = UU_TRUE;
}
/*********************************************************************
**    E_FUNCTION :  ua_init_select()
**       Initilize select logic.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_init_select()
{
	select_initialize = UU_TRUE;
}

/*********************************************************************
**    E_FUNCTION :  ua_gnxt(keyptr)
**       after ua_select(), this gets succesive selected entities from DAS
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          int	*keyptr;	 key of next picked entity	
**    RETURNS      : 0 if no more entities
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL
ua_gnxt(keyptr)
	int	*keyptr;
	{
	UU_LOGICAL ret_val;

	uu_denter(UU_STRC,(us,"entering ua_gnxt"));
	ret_val = ud_gnxt(select_initialize, UU_NULL, keyptr, 1);
	select_initialize = UU_FALSE;
	uu_dexit;
	return (ret_val);
	}


/*********************************************************************
**    I_FUNCTION :  ua_next_entity(keyptr)
**       after ua_select(), this gets successive selected entities from DAS
**			and breaks down composite curves into their constituent curves 
**			before returning the key id
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          int	*keyptr;	 key of next picked entity	
**    RETURNS      : UU_FALSE if no more entities or an invalid entity; 
**			otherwise UU_TRUE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL
ua_next_entity(keyptr)
	int	*keyptr;
	{
	UU_LOGICAL ret_val;
	int rel_num;

/* If we are NOT breaking down a composite curve, get the next DAS entity.  
** If this entity is a composite, return the first constituent entity. If not, 
** return the original DAS entity.  If we ARE breaking down a composite curve,
** return the next constituent entity. */

	uu_denter(UU_STRC,(us,"entering ua_next_entity"));
	if (not_comp_curve)
		{
		if (ret_val = ua_gnxt(keyptr));
			{
			if (ur_retrieve_data_relnum(*keyptr,&rel_num) == UU_SUCCESS)
				{
				if (rel_num == UM_COMPCRV_REL)
					{
					comp_rec.key = *keyptr;
					if (uc_retrieve_data(&comp_rec, sizeof(comp_rec)) == UU_SUCCESS)
						{
						ret_val = ua_next_comp(keyptr);
						}
					else
						{
						ret_val = UU_FALSE;
						uu_denter2(UU_STRC,(us,"ua_next_entity: uc_retrieve_data() returns FAILURE"));
						uu_dexit;
						}
					}	/* end of if UM_COMPCRV_REL */
				}	/* end of if ur_retrieve_data_relnum() */
			else
				{
				/* ur_retrieve_data_relnum = UU_FAILURE */
				ret_val =  UU_FALSE;
				uu_denter2(UU_STRC,(us,"ua_next_entity: ur_retrieve_data_relnum(%d) returns FAILURE", *keyptr));
				uu_dexit;
				}
			} 	/* end of if ret_val = ua_gnxt() */
		}	/* end of if not_comp_curve */
	else
		{
		/* not_comp_curve = UU_FALSE */
		ret_val = ua_next_comp(keyptr);
		}
	uu_dexit;
	return (ret_val);
	}


/*********************************************************************
**    I_FUNCTION :  ua_next_comp(keyptr)
**			gets the next constituent of the composite stored in the external
**			structure UM_compcrv_rec comp_rec
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          int	*keyptr;	 key of next picked entity	
**    RETURNS      : UU_FALSE if no more constituents or an invalid entity; 
**			otherwise UU_TRUE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL ua_next_comp(keyptr)
int *keyptr;
	{
	UU_LOGICAL ret_val;

	uu_denter(UU_STRC,(us,"entering ua_next_comp()"));

	/* this is a redundant test, but why not be safe ? */
	if (ith_curve < comp_rec.no_cid)
		{
		*keyptr = comp_rec.cid[ith_curve].crvid;
		ret_val = UU_TRUE;
		}
	else
		{
		/* should never reach this point */
		ret_val = UU_FALSE;
		uu_denter2(UU_STRC,(us,"ua_next_entity: bad ith_curve %d",
			ith_curve));
		uu_dexit;
		}
	if (++ith_curve < comp_rec.no_cid)
		{
		/* there are constituent entities remaining */
		not_comp_curve = UU_FALSE;
		}
	else
		{
		/* finished with this composite */
		not_comp_curve = UU_TRUE;
		ith_curve = 0;
		}
	uu_dexit;
	return (ret_val);
	}
