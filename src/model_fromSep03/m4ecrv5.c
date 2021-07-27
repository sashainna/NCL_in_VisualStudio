
/********************************************************************r
**    NAME: m4ecrv5.c
**       CONTAINS: query routines for AG curves
**			int um_agcrv_query(key, list)
**  COPYRIGHT  1986  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       m4ecrv5.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:02
**********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mderror.h"
#include "mdebug.h"
#include "ulist.h"
#include "mdgenent.h"
#include "mdrel.h"
#include "mdeval.h"
#include "mcrv.h"
#include "mattr.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "class.h"
#include "jquery.h"
#include "nclfc.h"
#include "mfort.h"

#include "ag_incl.h"

/*********************************************************************
**    I_FUNCTION : int um_agcrv_query(key, list)
**			This function produces the query output for an agcrv.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_query(key, list)
	UU_KEY_ID	key;
	UU_LIST 	*list;

	{
     UM_int2 idx, ival;
	int status = UU_FAILURE;
	struct UM_agcrv_rec crv;
	struct UM_attrdata_rec attr;
	UM_transf tfmat;
	char	 buf[256];
	UM_coord pt;
	UU_REAL wt;
	UU_REAL t;
	int dim, rat;
	int i,j;
	AG_CURVEP cv;
	AG_SPLINEP bs;
	AG_CNODEP node;

	uu_denter(UU_MTRC,(us, "um_agcrv_query(key=%d)", key));

	/* get the entity data */
	crv.key = key;
	if (uc_retrieve_data(&crv, sizeof(struct UM_agcrv_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS)
		goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)
		goto failed;

	/* print curve specific information */

	cv = (AG_CURVEP) crv.crvaddr;
	bs = cv->bs0;

    idx = 264;
    getifl (&idx, &ival);
	for (i=1; i<=cv->nbs; i++)
		{
		dim = bs->dim;
		rat = bs->rat;

		sprintf(buf, "bspline segment %d", i);
		uj_putstr(list, buf);
		sprintf(buf, "  dimension=%d", dim);
		uj_putstr(list, buf);
		sprintf(buf, "  degree=%d", bs->m);
		uj_putstr(list, buf);
		sprintf(buf, "  number spans=%d", bs->n);
		uj_putstr(list, buf);
		if (bs->rat == 0)
			sprintf(buf, "  non-rational");
		else if (bs->rat == 1)
			sprintf(buf, "  rational");
		else if (bs->rat == -1)
			sprintf(buf, "  homgeneous");
		else
			sprintf(buf, "  unknown rat flag");
		uj_putstr(list, buf);

		node = bs->node0;
		do
			{
			for (j=0; j<dim; j++) pt[j] = node->Pw[j];
			if (dim == 2) pt[2] = 0.0;
			UM_cc_inttoext(pt, pt);

			if (rat != 1)
				wt = 1.0;
			else
				{
				wt = node->Pw[dim];
				UM_len_inttoext(wt, wt);
				}

			t = *(node->t);

            if (ival == 0) /* inches */
			    sprintf(buf,"  pt=<%10.5f,%11.6f,%10.5f>     wt=%10.5f     t=%10.5f",
				pt[0], pt[1], pt[2], wt, t);
            else
                sprintf(buf,"  pt=<%10.4f,%11.5f,%10.4f>     wt=%10.4f     t=%10.4f",
                pt[0], pt[1], pt[2], wt, t);

			uj_putstr(list, buf);

			node = node->next;
			}
		while ((node != NULL) && (node != bs->node0));

		bs = bs->next;
		}

	status = UU_SUCCESS;
	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_agcrv_query", status);
	return(status);
	}	
