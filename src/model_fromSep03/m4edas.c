/*********************************************************************
**    NAME         :  m4edas.c
**       CONTAINS: routines to interface AG curves to DAS
**			int um_agcrv_ploc_to_coord(level,pickpath,pickloc,pt)
**			int um_agcrv_ploc_to_vector(level,pickpath,pickloc,vec)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4edas.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:02
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mdpick.h"
#include "mdeval.h"

#include "ag_incl.h"

/*********************************************************************
**    E_FUNCTION    : int um_agcrv_ploc_to_coord(level,pickpath,pickloc,pt)
**			Determine a cartesian coordinate (PT) given a picked location
**			on an AG curve (LEVEL, PICKPATH, PICKLOC).
**    PARAMETERS   
**       INPUT  : 
**				level					level of entity picked within pickpath
**				pickpath				DAS pick path record
**				pickloc				DAS pickloc record
**       OUTPUT :  
**				pt						cartesian coordinate (MCS) corresponding
**										to picked location on entity
**    RETURNS      : 
**			UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_ploc_to_coord(level, pickpath, pickloc, pt)
	int level;
	UD_PPICKREC *pickpath;
	UD_NDCLOCREC *pickloc;
	UM_coord pt;

	{
	UM_PICKENT pent;
	struct UM_evcrvout evcrv;
	struct UM_agcrv_rec crv;
	UM_coord p[4];
	int closest;
	int status;

	uu_denter(UU_MTRC,(us,"um_agcrv_ploc_to_coord(level=%d,pickpath=%x,pickloc=%x)",
		level, pickpath, pickloc));

	status = um_d_pickresolve(pickpath, level, &pent);
	if (status != UU_SUCCESS) goto done;

	crv.key = um_get_pickkey(&pent, 1);
	status = uc_retrieve_data(&crv, sizeof(crv));
	if (status != UU_SUCCESS) goto done;

	um_init_evcrvout(&crv, &evcrv);
	status = um_agcrv_evaluate(UM_POINT, (UU_REAL) 0.0, &crv, UM_DEFAULT_TF, &evcrv);
	um_vctovc(evcrv.cp, p[0]);
	status = um_agcrv_evaluate(UM_POINT, (UU_REAL) 1.0, &crv, UM_DEFAULT_TF, &evcrv);
	um_vctovc(evcrv.cp, p[1]);
	closest = um_nearest_to_ploc(pickloc, 2, p);
	um_vctovc(p[closest], pt);

done:;
	uu_dexitstatus("um_agcrv_ploc_to_coord",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION    : int um_agcrv_ploc_to_vector(level,pickpath,pickloc,vec)
**			Determine a vector (VEC) given a picked location
**			on an AG curve (LEVEL, PICKPATH, PICKLOC).
**    PARAMETERS   
**       INPUT  : 
**				level					level of entity picked within pickpath
**				pickpath				DAS pick path
**				pickloc				DAS pickloc record
**       OUTPUT :  
**				vec					vector
**    RETURNS      : 
**			UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_ploc_to_vector(level, pickpath, pickloc, vec)
	int level;
	UD_PPICKREC *pickpath;
	UD_NDCLOCREC *pickloc;
	UM_vector vec;

	{
	UM_PICKENT pent;
	struct UM_agcrv_rec crv;
	struct UM_evcrvout evcrv;
	int status;
	UM_coord p[2];
	UM_vector pvec[2];
	int closest;

	uu_denter(UU_MTRC,(us,"um_agcrv_ploc_to_vector(level=%d,pickpath=%x,pickloc=%x)",
		level, pickpath, pickloc));

	status = um_d_pickresolve(pickpath, level, &pent);
	if (status != UU_SUCCESS) goto done;

	crv.key = um_get_pickkey(&pent, 1);
	status = uc_retrieve_data(&crv, sizeof(crv));
	if (status != UU_SUCCESS) goto done;


	um_init_evcrvout(&crv, &evcrv);
	status = um_agcrv_evaluate(UM_FRSTDERIV, (UU_REAL) 0.0, &crv, UM_DEFAULT_TF, &evcrv);
	um_vctovc(evcrv.cp, p[0]);
	um_vctovc(evcrv.dcdu, pvec[0]);
	status = um_agcrv_evaluate(UM_FRSTDERIV, (UU_REAL) 1.0, &crv, UM_DEFAULT_TF, &evcrv);
	um_vctovc(evcrv.cp, p[1]);
	um_vctovc(evcrv.dcdu, pvec[1]);
	closest = um_nearest_to_ploc(pickloc, 2, p);
	um_vctovc(pvec[closest], vec);
	if (closest == 1) um_vctmsc(vec, (UU_REAL) -1.0, vec);

done:;
	uu_dexitstatus("um_agcrv_ploc_to_vector",status);
	return (status);
	}

