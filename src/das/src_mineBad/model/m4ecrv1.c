
/*********************************************************************
**    NAME         :  m4ecrv1.c
**       CONTAINS: AG curve graphics support routines
**			int um_agcrv_draw(eptr, tfmat, attrptr)
**			void ag_drw_bs_Bez(bz)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4ecrv1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:02
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "go.h"
#include "ginq.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdebug.h"

#include "ag_incl.h"
#include "ag_global.h"

UM_transf UM_bez_draw_mat;

UU_LOGICAL UM_display_control_polygon = UU_FALSE;

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_draw(eptr, tfmat, attrptr)
**			Stroke an AG curve.
**    PARAMETERS   
**       INPUT  : 
**				eptr					pointer to entity data
**				tfmat					transformation matrix
**				attrptr				pointer to attribute data
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_draw(eptr, tfmat, attrptr)
	struct UM_agcrv_rec *eptr;
	UM_transf tfmat;
	struct UM_attdata_rec *attrptr;

	{
	int status;
	int markertype;
	Gwpoint3 gpt;
	AG_CURVEP crv;
	AG_SPLINEP bs;
	AG_CNODEP node;
	UU_REAL a,b;
	int nbs;
	int i, j, dim;
	UM_coord pt;

	uu_denter(UU_MTRC,(us,"um_agcrv_draw(key=%d,tfmat=%x,attrptr=%x)",
					eptr->key, tfmat, attrptr));

	status = UU_SUCCESS;

	um_set_disp_attr(attrptr);

	if (tfmat == UU_NULL)
		um_tftotf(UM_idmat, UM_bez_draw_mat);
	else
		um_tftotf(tfmat, UM_bez_draw_mat);

	crv = (AG_CURVEP) eptr->crvaddr;
	if (crv != NULL)
		{
		nbs = crv->nbs;
		dim = crv->dim;

		/* draw the control polygon */
		if (UM_display_control_polygon)
			{
			markertype = gqmarktype();
			gsmarktype(2);
			j = 0;
			for (i=0, bs=crv->bs0; i<nbs; i++, bs=bs->next)
				{
				node = bs->node0;
				do
					{
					um_vctovc(node->Pw, pt);
					if (bs->rat) um_vctmsc(pt, 1.0/node->Pw[dim], pt);
					j++;
					gspickid(j);
					gpt.x = pt[0];
					gpt.y = pt[1];
					gpt.z = pt[2];
					gpolymarker3(1, &gpt);
					node = node->next;
					}
				while ((node != NULL) && (node != bs->node0));
				}
			sprintf(UM_sbuf,"%d pickids set for control polygon",j);
			um_pscroll(UM_sbuf);
			gsmarktype(markertype);
			}

		/*  draw the curve */
		for (i=0, bs=crv->bs0; i<nbs; i++, bs=bs->next)
			{
			a = *bs->node0->t;
			b = *bs->noden->t;
			ag_dr_bs_ab(bs, a, b);
			}

		}

	uu_dexitstatus("um_agcrv_draw",status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ag_drw_bs_Bez(bz)
**       Call DIGS to draw an AG Bezier curve. If the dimension of 
**			BZ is 2, the z coordinate will be set to 0.0.
**    PARAMETERS   
**       INPUT  : 
**          bz						pointer to Bezier curve
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ag_drw_bs_Bez(bz)
	AG_SPLINEP bz;

	{
	Gwpoint3 gpt[90];
	AG_CPOINT V;
	int i;
	UU_REAL  t0, t1, del, t, q[3];

	uu_denter(UU_MTRC,(us,"ag_drw_bs_Bez(bz=%x, dim=%d, type=%d)",
		bz, bz->dim, bz->ctype));

	if (bz->ctype == AG_LINE)
		{
		/* draw a single line segment */
		um_vctovc(bz->node0->Pw, &gpt[0]);
		um_vctovc(bz->noden->Pw, &gpt[1]);
		um_cctmtf(&gpt[0], UM_bez_draw_mat, &gpt[0]);
		um_cctmtf(&gpt[1], UM_bez_draw_mat, &gpt[1]);
		gpolyline3(2, gpt);
		}
	else
		{
		/* calculate the parameter range and step size for 10 evaluations */
		t0 = *bz->node0->t;
		t1 = *bz->noden->t;
		del = (t1 - t0)/10.0;
		t = t0;
	
		/* evaluate the bspline and save data in DIGS polyline structure */
		for(i=0; i<=10; i++)
			{
			V.P = (UU_REAL *) &gpt[i];
			ag_eval_bs(t, 0, bz, &V);
			if (bz->dim == 2) gpt[i].z = 0.0;
			um_cctmtf(&gpt[i], UM_bez_draw_mat, &gpt[i]);
			t = t + del;
			}

		/* finally, display the polyline */
		gpolyline3(11, gpt);
		}

	uu_dexit;
	}

