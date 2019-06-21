

/*********************************************************************
**    NAME         :  m3ucn2.c
**       CONTAINS: user interface routines for creating hyperbolas
**			umu_c4_hyptax()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3ucn2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:57
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "modef.h"
#include "mcrv.h"
#include "mdpick.h"
#include "mdcpln.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION :  umu_c4_hyptax()
**			Prompt the user for the following information:
**				1. center of hyperbola (intersection of assymptotes)
**				2. length of transverse axis (along x axis)
**				3. length of conjugate axis (along y axis)
**				4. orientation angle (relative to x axis)
**				5. a curve to limit the extent of the hyperbola
**			The hyperbola will lie in a plane through the center and
**			parallel to the xy construction plane.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : creates and displays new entity
**    WARNINGS     : none
*********************************************************************/
umu_c4_hyptax()

	{
	int status;
	struct UM_conic_rec e;
	struct UC_entitydatabag c;
	int numint;
/**	UM_coord	center; **/
    UD_NDCLOCREC center;

	UM_vector vector;
	UU_REAL	trans_axis;
	UU_REAL	conj_axis;
	UU_REAL	range;
	UM_PICKENT pick;
	UM_transf tfmat;
	UM_angle rotangle;

	uu_denter(UU_MTRC,(us,"umu_c4_hyptax()"));

	ur_setup_data(UM_CONIC_REL, &e, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	while (UU_TRUE)
		{
		/* get center	*/
		ud_ldas(UD_DASCART, /* center of hyperbola */UM_MODEL, 201, &center, 1,
			&numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		/* get transverse axis length	*/
		ud_ldas(UD_DASDISTANCE, /* Length of transverse axis */UM_MODEL, 202,
			&trans_axis, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		if (trans_axis < UM_FUZZ)
			{
			uu_uerror0(/* axis must be longer	*/UM_MODEL, 179);
			continue;
			}

		/* get conjugate axis length	*/
		ud_ldas(UD_DASDISTANCE, /* Length of conjugate axis */UM_MODEL, 203,
			&conj_axis, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		if (conj_axis < UM_FUZZ)
			{
			uu_uerror0(/* axis must be longer	*/UM_MODEL, 179);
			continue;
			}

		rotangle = 0.0;
		ud_ldas(UD_DASANGLE, /* orientation mngle */UM_MODEL, 255,
			&rotangle, 1, &numint, UD_DEFAULT);
		if (numint < 1) goto repeat;

		/* get limiting curve */
		ud_lgeo(UU_TRUE, UD_vircurves);
		um_dl_pdas(UD_DASPICK, /* pick a curve to trim conic to */
			UM_MODEL, 316, &pick, 1, &numint, 2);
		if (numint < 1) goto repeat;

		c.key = um_get_pickkey(&pick, 2);
		status = uc_retrieve_data(&c, sizeof(c));
		if (status != UU_SUCCESS) goto repeat;

		/* initialize hyperbola */
		e.type = UM_HYPERBOLA;
		e.invariants[0] = trans_axis;
		e.invariants[1] = conj_axis;
		um_rottf(UM_cpln.zaxis, rotangle, tfmat);
		um_vctmtf(UM_cpln.xaxis, tfmat, vector);
		um_unitvc(vector, e.tfmat[0]);
		um_vctovc(UM_cpln.zaxis, e.tfmat[2]);
		um_cross(UM_cpln.zaxis, e.tfmat[0], e.tfmat[1]);
		um_vctovc(&center, e.tfmat[3]);
		range = 10000.0 / e.invariants[0];
		e.t0 = sqrt((range - 1)/(range + 1));
		e.t1 =  -e.t0;
		
		/* create hyperbola in UNIBASE */
		um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);

		/* limit conic to extend to the intersection of the curves */
		status = um_limit_conic(&e, &c);
	
		/* update trimmed hyperbola and display */
		um_update_geom(&e, UM_DEFAULT_TF);
		uc_display(&e);

repeat:;
	}
	
done:
	uu_dexit;
	}

