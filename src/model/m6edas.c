/*********************************************************************
**    NAME         :  m6edas.c
**       CONTAINS: routines to interface AG shells to DAS
**			int um_agshell_ploc_to_coord(level,pickpath,pickloc,pt)
**			int um_agshell_ploc_to_vector(level,pickpath,pickloc,vec)
**			int um_ploc_to_agcrv_on_agshell(level, pickpath,
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m6edas.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:07
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msol.h"
#include "mdebug.h"
#include "mdpick.h"
#include "mdeval.h"

#include "ag_incl.h"

/*********************************************************************
**    E_FUNCTION    : int um_agshell_ploc_to_coord(level,pickpath,pickloc,pt)
**			Determine a cartesian coordinate (PT) given a picked location
**			on an AG shell (LEVEL, PICKPATH, PICKLOC).
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
int um_agshell_ploc_to_coord(level, pickpath, pickloc, pt)
	int level;
	UD_PPICKREC *pickpath;
	UD_NDCLOCREC *pickloc;
	UM_coord pt;

	{
	struct UM_agcrv_rec curve;
	struct UM_evcrvout evcrv;
	UM_coord p[2];
	int closest;
	int status;

	uu_denter(UU_MTRC,(us,"um_agshell_ploc_to_coord(level=%d,pickpath=%x,pickloc=%x)",
		level, pickpath, pickloc));

	status = um_ploc_to_agcrv_on_agshell(level, pickpath, pickloc, &curve);
	if (status != UU_SUCCESS) goto done;

	um_init_evcrvout(&curve, &evcrv);
	status = um_agcrv_evaluate(UM_POINT, (UU_REAL) 0.0, &curve, 
										UM_DEFAULT_TF, &evcrv);
	um_vctovc(evcrv.cp, p[0]);
	status = um_agcrv_evaluate(UM_POINT, (UU_REAL) 1.0, &curve, 
										UM_DEFAULT_TF, &evcrv);
	um_vctovc(evcrv.cp, p[1]);
	closest = um_nearest_to_ploc(pickloc, 2, p);
	um_vctovc(p[closest], pt);

	status = UU_SUCCESS;

done:;
	uu_dexitstatus("um_agshell_ploc_to_coord",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION    : int um_agshell_ploc_to_vector(level,pickpath,pickloc,vec)
**			Determine a vector (VEC) given a picked location
**			on an AG shell (LEVEL, PICKPATH, PICKLOC).
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
um_agshell_ploc_to_vector(level, pickpath, pickloc, vec)
	int level;
	UD_PPICKREC *pickpath;
	UD_NDCLOCREC *pickloc;
	UM_vector vec;

	{
	int status;
	struct UM_agcrv_rec curve;
	struct UM_evcrvout evcrv;
	UM_coord p[2];
	UM_vector pvec[2];
	int closest;

	uu_denter(UU_MTRC,(us,"um_agshell_ploc_to_vector(level=%d,pickpath=%x,pickloc=%x)",
		level, pickpath, pickloc));

	status = um_ploc_to_agcrv_on_agshell(level, pickpath, pickloc, &curve);
	if (status != UU_SUCCESS) goto done;

	um_init_evcrvout(&curve, &evcrv);
	status = um_agcrv_evaluate(UM_FRSTDERIV, (UU_REAL) 0.0, &curve,
					 					UM_DEFAULT_TF, &evcrv);
	um_vctovc(evcrv.cp, p[0]);
	um_vctovc(evcrv.dcdu, pvec[0]);
	status = um_agcrv_evaluate(UM_FRSTDERIV, (UU_REAL) 1.0, &curve,
										UM_DEFAULT_TF, &evcrv);
	um_vctovc(evcrv.cp, p[1]);
	um_vctovc(evcrv.dcdu, pvec[1]);
	closest = um_nearest_to_ploc(pickloc, 2, p);
	um_vctovc(pvec[closest], vec);
	if (closest == 1) um_vctmsc(vec, (UU_REAL) -1.0, vec);

	status = UU_SUCCESS;

done:;
	uu_dexitstatus("um_agshell_ploc_to_vector",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION    : int um_ploc_to_agcrv_on_agshell(level, pickpath,
**										pickloc, curve)
**			Determine a curve on the shell (CURVE) given a picked location
**			on the shell (LEVEL, PICKPATH, PICKLOC).
**    PARAMETERS   
**       INPUT  : 
**				level					level of entity picked within pickpath
**				pickpath				DAS pick path record
**				pickloc				DAS pickloc record
**       OUTPUT :  
**				curve					curve on shell which was picked
**    RETURNS      : 
**			UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_ploc_to_agcrv_on_agshell(level, pickpath, pickloc, curve)
	int level;
	UD_PPICKREC *pickpath;
	UD_NDCLOCREC *pickloc;
	struct UM_agcrv_rec *curve;


	{
	int status;
	int curvenum1;
	struct UM_agshell_rec shell;
	UM_PICKENT pent;

	uu_denter(UU_MTRC,(us,"um_ploc_to_agcrv_on_agshell(level=%d,pickpath=%x,pickloc=%x)",
		level, pickpath, pickloc));

	status = UU_FAILURE;

	um_d_pickresolve(pickpath, level, &pent);
	shell.key = um_get_pickkey(&pent, 1);
	uc_retrieve_data(&shell, sizeof(shell));
	curvenum1 = pickpath->pickpath[1];

	status = um_pickid_to_agcrv_on_agshell(&shell, curvenum1, curve);

done:;
	uu_dexitstatus("um_ploc_to_agcrv_on_agshell",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION    : int um_pickid_to_agcrv_on_agshell(shell, pickid,
**										 curve)
**			Determine a curve (CURVE) on the shell (SHELL) given a pickid
**			(PICKID).
**    PARAMETERS   
**       INPUT  : 
**				shell					shell entity
**				pickid				pickid
**       OUTPUT :  
**				curve					curve on shell which was picked
**    RETURNS      : 
**			UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_pickid_to_agcrv_on_agshell(shell, pickid, curve)
	struct UM_agshell_rec *shell;
	int pickid;
	struct UM_agcrv_rec *curve;


	{
	int status;
	int curvenum2;
	AG_SHELLP sh;
	AG_FACEP face;
	AG_BOUNDARYP bndy;
	AG_TEDGEP twin;
	AG_CURVEP crv;
	UM_PICKENT pent;

	uu_denter(UU_MTRC,(us,"um_pickid_to_agcrv_on_agshell(key=%x, pickid=%d)",
		shell->key, pickid));

	status = UU_FAILURE;

	curvenum2 = 0;

	sh = (AG_SHELLP) shell->shelladdr;

	uc_setup_data(UM_AGCRV_REL, curve, sizeof(struct UM_agcrv_rec));

	if (sh != NULL)
		{
		face = sh->f;
		if (face != NULL) do
			{
			bndy = face->ob;
			if (bndy != NULL) do
				{
				twin = bndy->te0;
				if (twin != NULL) do
					{
					crv = twin->edge;
					if (crv == NULL) crv = twin->twin->edge;
					if (crv != NULL)
						{
						curvenum2++;
						if (pickid == curvenum2)
							{
							curve->crvaddr = (int) crv;
							status = UU_SUCCESS;
							goto done;
							}
						}
					twin = twin->next;
					} while ((twin != bndy->te0) && (twin != NULL));
				bndy = bndy->next;
				} while ((bndy != face->ob) && (bndy != NULL));
			face = face->next;
			} while ((face != sh->f) && (face != NULL));
		}

done:;
	uu_dexitstatus("um_pickid_to_agcrv_on_agshell",status);
	return (status);
	}
