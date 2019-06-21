/*********************************************************************
**    NAME         :  c1srf.c
**       CONTAINS: class method dispatchers for surfaces
**			int uc_evsrf1(evflag, u, v, srfptr, tfmat, srfout)
**			int uc_altouv(srfptr, ua, va, u, v)
**			int uc_init_evsrfout(srcptr, srcout)
**			int uc_srf_tesselate(srf)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       c1srf.c , 25.1
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
#include "mgeom.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION     : int uc_evsrf1(evflag, u, v, srfptr, tfmat, srfout)
**		Given a surface entity (SRFPTR) with associated transformation
**		matrix (TFMAT which may be UM_DEFAULT_TF), calculate the
**		data requested by EVFLAG at the logical parameter value U,V
**		[0.0 <= U,V <= 1.0].
**    PARAMETERS   
**       INPUT  : 
**          evflag					UM_POINT		=> point on surface
**											UM_NORM		=> surface normal, plus above
**											UM_FRSTDERIV=> first cross derivatives,
**																plus above
**											UM_SECDERIV => second cross derivatives,
**																plus above
**											UM_CURVATURE=> curvature, plus above
**				u,v						logical parameter at which to evaluate 
**											surface equation (i.e. 0.0 <= U,V <= 1.0)
**				srfptr					pointer to surface entity
**				tfmat						transformation matrix positioning surface
**											in MCS (may be UM_DEFAULT_TF)
**       OUTPUT :  
**          srfout					pointer to surface evaluator record
**    RETURNS      : 
**       UM_VALID:			all requested fields are valid;
**			UM_BADFIELDS:		at least one requested fields is invalid;
**       UM_BADRECORDS:		at least one entire record is invalid;
**       UM_INVALID:			all output is suspect.
**    SIDE EFFECTS : 
**			The surface evaluator record (SRFOUT) may be updated with
**			information which makes subsequent evaluator calls faster
**    WARNINGS     : 
**			The surface evaluator record (SRFOUT) must be initialized
**			before the first call to um_evsrf() with a new surface.
*********************************************************************/
int
uc_evsrf1(evflag, u, v, srfptr, tfmat, srfout)
	int evflag;
	UM_param u;            
	UM_param v;            
	struct UC_entitydatabag *srfptr;
	UM_transf tfmat;
	struct UM_evsrfout *srfout;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC, (us,"uc_evsrf1(evflag=%d,u=%g,v=%g,key=%d,tfmat=%x,%x)",
		evflag, u, v, srfptr, tfmat, srfout));

	class = (*srfptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_EVSRF);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(evflag, u, v, srfptr, tfmat, srfout);
		}

	uu_dexitstatus("uc_evsrf1", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int uc_altouv(srfptr, ua, va, u, v)
**			Given a "parameter" specified as a percenatage of arc
**			length (UA,VA) and a surface (SRFPTR), determine the logical
**			parameter value U,V such that the surface evaluated at U,V
**			will result in a point that is the percentage of arc 
**			length along the surface.
**    PARAMETERS   
**       INPUT: 
**				srfptr				surface that the point lies on
**				ua						arclen parameter [0.0 <= ua <= 1.0]
**				va						arclen parameter [0.0 <= va <= 1.0]
**       OUTPUT:  
**				u						logical parameter [0.0 <= u <= 1.0]
**				v						logical parameter [0.0 <= v <= 1.0]
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_altouv(srfptr, ua, va, u, v)
	struct UC_entitydatabag *srfptr;
	UM_param ua;
	UM_param va;
	UM_param *u;
	UM_param *v;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC,(us, "uc_altouv(key=%x, ua=%g, va=%g)",
		 srfptr->key, ua, va));

	class = (*srfptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_ALTOUV);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(srfptr, ua, va, u, v);
		}

	uu_dexitstatus("uc_altouv", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int uc_init_evsrfout(srcptr, srcout)
**			Initialize an evaluator record (SRFOUT) for surfaces to handle
**			the evaluation of the specified surface (SRFPTR) entity.
**    PARAMETERS   
**       INPUT: 
**				srfptr				the surface entity to have an evaluator record
**										set up for.
**       OUTPUT:  
**				srfout				pointer to the set up evaluator record. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uc_init_evsrfout(srfptr, srfout)
	struct UC_entitydatabag *srfptr;
	struct UM_evsrfout *srfout;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC,(us, "uc_init_evsrfout(srfptr->rel_num:%d, srfout:%x)", 
											srfptr->rel_num, srfout));

	class = (*srfptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_INIT_EVSRFOUT);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(srfptr, srfout);
		}

	uu_dexitstatus("uc_init_evsrfout", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_srf_tessellate(srf)
**       Tessalate the specified surface (SRF).
**    PARAMETERS   
**       INPUT  : 
**          srf				pointer to surface entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_srf_tessellate(srf, tessellation)
struct UC_entitydatabag *srf;
UM_tessellation *tessellation;
{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	class = (*srf).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_SRF_TESSELLATE);

	if(function != UC_UNDEFINED_METHOD)
	{
		status = (*function)(srf,tessellation);
	}

	return (status);
}
