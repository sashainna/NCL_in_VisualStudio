
/*********************************************************************
**    NAME         :  m3ucirc5.c
**       CONTAINS: user interface routines to create circle or arc
**			umu_c3_3tan()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ucirc5.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:57
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdrel.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdpick.h"
#include "mdebug.h"

#define ERROR_BASE 216
#define PROMPT_BASE 256
/*********************************************************************
**    E_FUNCTION     : umu_c3_3tan(option)
**			Create a circle or arc through three points.
**			The three points are determined by three tangent lines.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

umu_c3_3tan(option)
	UU_LOGICAL option;
	{
	UD_NDCLOCREC ploc[3];			/* picked location on entities picked */
	struct UM_circle_rec c;			/* new tangent circle */
	struct UM_crvdatabag e[3];		/* three picked entities */
	UM_PLOCREC pick;					/* pick information */
	int numint;
	UU_LOGICAL err;					/* error flag */
	struct UM_crvdatabag *ents[3];/* to check for coplanar */
	UU_REAL *tfmats[3];				/* to check for coplanar */
	int dim,bad;
	UU_REAL space[2][3];
	UU_REAL p[3][3];
	int i;

	uu_denter( UU_MTRC,(us,"umu_c3_3tan()"));

	while (UU_TRUE) {
					/*Pick first tangent line*/ 
		um_dl_pldas(UD_DASPCKLOC, UM_MODEL, PROMPT_BASE, &pick, 1, &numint, 2);
		if (numint <= 0) break;
		e[0].key = um_get_pickkey(&pick.pent, 2);
		um_copyploc(&pick.ploc, &ploc[0]);
		ur_retrieve_data_relnum(e[0].key, &e[0].rel_num);
		if (e[0].rel_num != UM_LINE_REL) {
			uu_uerror0(UM_MODEL,/*entity not a line*/131);
			continue; /* try again */
			}

					/*Pick second tangent line*/
		for (e[1].key=0 ; e[1].key == 0 ; ) {
			um_dl_pldas(UD_DASPCKLOC, UM_MODEL, PROMPT_BASE+1, &pick, 1, &numint, 2);
			if (numint <= 0) break; /* nothing picked so exit */
			e[1].key = um_get_pickkey(&pick.pent, 2);
			if (e[0].key == e[1].key) {
				uu_uerror0(/*picked entities are same*/UM_MODEL,35);
				e[1].key = 0;
				}
			else {
				um_copyploc(&pick.ploc, &ploc[1]);
				ur_retrieve_data_relnum(e[1].key, &e[1].rel_num);
				if (e[1].rel_num != UM_LINE_REL) {
					uu_uerror0(UM_MODEL,/*entity not a line*/131);
					e[1].key = 0;
					}
				}
			}
		if (e[1].key == 0) break; /* nothing picked so exit */

					/*Pick third tangent line*/ 
		for (e[2].key=0 ; e[2].key == 0 ; ) {
			um_dl_pldas(UD_DASPCKLOC, UM_MODEL, PROMPT_BASE+2, &pick, 1, &numint, 2);
			if (numint <= 0) break; /* nothing picked so exit */
			e[2].key = um_get_pickkey(&pick.pent, 2);
			if ((e[0].key == e[2].key) || (e[1].key == e[2].key)) {
				uu_uerror0(/*picked entities are same*/UM_MODEL,35);
				e[2].key = 0;
				}
			else {
				um_copyploc(&pick.ploc, &ploc[2]);
				ur_retrieve_data_relnum(e[2].key, &e[2].rel_num);
				if (e[2].rel_num != UM_LINE_REL) {
					uu_uerror0(UM_MODEL,/*entity not a line*/131);
					e[2].key = 0;
					}
				}
			}
		if (e[2].key == 0) break; /* nothing picked so exit */

		um_get_all_geom(&e[0], sizeof(struct UM_crvdatabag));
		um_get_all_geom(&e[1], sizeof(struct UM_crvdatabag));
		um_get_all_geom(&e[2], sizeof(struct UM_crvdatabag));
		ur_setup_data(UM_CIRCLE_REL, &c,sizeof(struct UM_circle_rec));
		/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
		strcpy (c.label, "");
		c.subscr = 0;

		/* check for colinear lines */
		tfmats[0] = (UU_REAL *)UM_idmat;
		tfmats[1] = (UU_REAL *)UM_idmat;
		tfmats[2] = (UU_REAL *)UM_idmat;
		/*   line 2 - line 3 */
		ents[0] = &e[1];
		ents[1] = &e[2];
		um_span_elist(2, ents, tfmats, &dim, space, &bad);
		if ((bad != -1) || (dim != 2)) {
			uu_uerror0(UM_MODEL,ERROR_BASE+2);
			continue;
			}
		/*   line 1 - line 3 */
		ents[0] = &e[0];
		um_span_elist(2, ents, tfmats, &dim, space, &bad);
		if ((bad != -1) || (dim != 2)) {
			uu_uerror0(UM_MODEL,ERROR_BASE+2);
			continue;
			}
		/*   line 1 - line 2 */
		ents[1] = &e[1];
		um_span_elist(2, ents, tfmats, &dim, space, &bad);
		if ((bad != -1) || (dim != 2)) {
			uu_uerror0(UM_MODEL,ERROR_BASE+2);
			continue;
			}

		/* make sure they are coplanar */
		ents[2] = &e[2];
		um_span_elist(3, ents, tfmats, &dim, space, &bad);
		if ((bad != -1) || (dim != 2)) {
			uu_uerror0(UM_MODEL,ERROR_BASE);
			continue;
			}

		/* project the picked points to the plane */
		for(i=0 ; i < 3 ; i++)
			um_projploctopln(&ploc[i],space[0],space[1],p[i]);

		err = um_cir_3tan(option,e,p,&c);
		if (!err) {
			um_create_geom(&c, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&c);
			}
		}
	uu_dexit;
	}

