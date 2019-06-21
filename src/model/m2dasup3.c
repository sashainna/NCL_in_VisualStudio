/*********************************************************************
**    NAME         :  m2dasup3.c
**       CONTAINS: class suppport
**			int um_curve_ploc_to_coord(level,pickpath,pickloc,pt)
**			int um_surface_ploc_to_coord(level,pickpath,pickloc,pt)
**			int um_curve_ploc_to_vector(level,pickpath,pickloc,vec)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2dasup3.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:45
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mdebug.h"
#include "mdpick.h"
#include "mdeval.h"

/*********************************************************************
**    E_FUNCTION    : int um_curve_ploc_to_coord(level,pickpath,pickloc,pt)
**			Determine a cartesian coordinate (PT) given a picked location
**			on a curve (LEVEL, PICKPATH, PICKLOC).
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
int um_curve_ploc_to_coord(level, pickpath, pickloc, pt)
	int level;
	UD_PPICKREC *pickpath;
	UD_NDCLOCREC *pickloc;
	UM_coord pt;

	{
	UM_PICKENT pent;
	struct UC_entitydatabag e;
	UM_transf tfmat;
	struct UM_evcrvout evcrv;
	UM_coord p[2];
	int closest;
	int status;

	uu_denter(UU_MTRC,(us,"um_curve_ploc_to_coord(level=%d,pickpath=%x,pickloc=%x)",
		level, pickpath, pickloc));

	status = um_d_pickresolve(pickpath, level, &pent);
	if (status != UU_SUCCESS) goto done;

	e.key = um_get_pickkey(&pent, level);
	status = uc_retrieve_data(&e, sizeof(e));
	if (status != UU_SUCCESS) goto done;
	status = uc_retrieve_transf(e.key, tfmat);
	if (status != UU_SUCCESS) goto done;

	status = uc_init_evcrvout(&e, &evcrv);
	if (status != UU_SUCCESS) goto done;

	status = uc_evcrv(UM_POINT, (UU_REAL) 0.0, &e, tfmat, &evcrv);
	if (status != UU_SUCCESS) goto done;
	um_vctovc(evcrv.cp, p[0]);

	status = uc_evcrv(UM_POINT, (UU_REAL) 1.0, &e, tfmat, &evcrv);
	if (status != UU_SUCCESS) goto done;
	um_vctovc(evcrv.cp, p[1]);

	closest = um_nearest_to_ploc(pickloc, 2, p);
	um_vctovc(p[closest], pt);

done:;
	uu_dexitstatus("um_curve_ploc_to_coord", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION    : int um_surface_ploc_to_coord(level,pickpath,pickloc,pt)
**			Determine a cartesian coordinate (PT) given a picked location
**			on a surface (LEVEL, PICKPATH, PICKLOC).
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
int um_surface_ploc_to_coord(level, pickpath, pickloc, pt)
	int level;
	UD_PPICKREC *pickpath;
	UD_NDCLOCREC *pickloc;
	UM_coord pt;

	{
	UM_PICKENT pent;
	struct UC_entitydatabag *e;
	UM_transf tfmat;
	struct UM_evsrfout *evsrf;
	UM_coord p[4];
	int closest;
	int status;

	uu_denter(UU_MTRC,(us,"um_surface_ploc_to_coord(level=%d,pickpath=%x,pickloc=%x)",
		level, pickpath, pickloc));

	status = um_d_pickresolve(pickpath, level, &pent);
	if (status != UU_SUCCESS) goto done;

	e = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
/*
.....Key of surface is always at level 1, level 2 is used to store which
.....part of surface display segment represents (1-4 for sides, 5 for center).
.....IJD 21-AUG-98
*/
	e->key = um_get_pickkey(&pent, 1);
	status = uc_retrieve_data(e, sizeof(struct UC_entitydatabag));
	if (status != UU_SUCCESS) goto done;
	status = uc_retrieve_transf(e->key, tfmat);
	if (status != UU_SUCCESS) goto done;

	status = uc_init_evsrfout(e, evsrf);
	if (status != UU_SUCCESS) goto done;

	status = uc_evsrf(UM_POINT, (UU_REAL) 0.0, (UU_REAL) 0.0, e, tfmat, evsrf);
	if (status != UU_SUCCESS) goto done;
	um_vctovc(evsrf->sp, p[0]);

	status = uc_evsrf(UM_POINT, (UU_REAL) 0.0, (UU_REAL) 1.0, e, tfmat, evsrf);
	if (status != UU_SUCCESS) goto done;
	um_vctovc(evsrf->sp, p[1]);

	status = uc_evsrf(UM_POINT, (UU_REAL) 1.0, (UU_REAL) 0.0, e, tfmat, evsrf);
	if (status != UU_SUCCESS) goto done;
	um_vctovc(evsrf->sp, p[2]);

	status = uc_evsrf(UM_POINT, (UU_REAL) 1.0, (UU_REAL) 1.0, e, tfmat, &evsrf);
	if (status != UU_SUCCESS) goto done;
	um_vctovc(evsrf->sp, p[3]);

	closest = um_nearest_to_ploc(pickloc, 4, p);
	um_vctovc(p[closest], pt);

done:;
	uu_free(e);
	uu_free(evsrf);

	uu_dexitstatus("um_surface_ploc_to_coord",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION    : int um_curve_ploc_to_vector(level,pickpath,pickloc,vec)
**			Determine a vector (VEC) given a picked location
**			on an curve (LEVEL, PICKPATH, PICKLOC).
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
int um_curve_ploc_to_vector(level, pickpath, pickloc, vec)
	int level;
	UD_PPICKREC *pickpath;
	UD_NDCLOCREC *pickloc;
	UM_vector vec;

	{
	UU_KEY_ID um_get_pickkey();
	UM_PICKENT pent;
	struct UC_entitydatabag e;
	UM_transf tfmat;
	struct UM_evcrvout evcrv;
	UM_coord p[2];
	UM_vector pvec[2];
	int closest;
	int status;

	uu_denter(UU_MTRC,(us,"um_curve_ploc_to_vector(level=%d,pickpath=%x,pickloc=%x)",
		level, pickpath, pickloc));

	status = um_d_pickresolve(pickpath, level, &pent);
	if (status != UU_SUCCESS) goto done;

	e.key = um_get_pickkey(&pent, level);
	status = uc_retrieve_data(&e, sizeof(e));
	if (status != UU_SUCCESS) goto done;
	status = uc_retrieve_transf(e.key, tfmat);
	if (status != UU_SUCCESS) goto done;


	status = uc_init_evcrvout(&e, &evcrv);
	if (status != UU_SUCCESS) goto done;

	status = uc_evcrv(UM_FRSTDERIV, (UU_REAL) 0.0, &e, tfmat, &evcrv);
	if (status != UU_SUCCESS) goto done;
	um_vctovc(evcrv.cp, p[0]);
	um_vctovc(evcrv.dcdu, pvec[0]);

	status = uc_evcrv(UM_FRSTDERIV, (UU_REAL) 1.0, &e, tfmat, &evcrv);
	if (status != UU_SUCCESS) goto done;
	um_vctovc(evcrv.cp, p[1]);
	um_vctovc(evcrv.dcdu, pvec[1]);

	closest = um_nearest_to_ploc(pickloc, 2, p);
	um_vctovc(pvec[closest], vec);
	if (closest == 1) um_vctmsc(vec, (UU_REAL) -1.0, vec);

done:;
	uu_dexitstatus("um_curve_ploc_to_vector",status);
	return (status);
	}

