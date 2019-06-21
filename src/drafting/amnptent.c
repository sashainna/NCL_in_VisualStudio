/*********************************************************************
**    NAME         :  amnptent.c
**       CONTAINS:
**			um_nptent
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       amnptent.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:36
*********************************************************************/
#include "umath.h"
#include "usysdef.h"
#include "modef.h"
#include "mcrv.h"
#include "dasnog.h"
#include "mdgenent.h"
#include "mdrel.h"
/*********************************************************************
**    E_FUNCTION     :  um_nptent(key,pickloc,pt)
**      Find point on entity key nearest the curser location  in pickloc
**		and return the point in pt.
**    PARAMETERS   
**       INPUT  : 
**          key							entity key_id
**          pickloc					pick location(ndc) part of plocrec
**       OUTPUT :  
**          pt								returned point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_nptent(key,pickloc,pt)
	UU_KEY_ID key;								/* entity key_id to find curve on*/
	UD_NDCLOCREC *pickloc;					/* ndc portion of plocrec */
	UU_REAL pt[3];								/* returned model coord point */
	{
	struct UM_entitydatabag e;
	UU_REAL tfmat[4][3];
	UU_REAL v1[3],v2[3],vpnorm[3];
	UU_REAL pv_cc[3],pv_spt[3],pv_ept[3];
	UU_REAL cc[3],ptc[2][3],cdot;
	int nint;
	UU_REAL um_dot();

	/*------------------------------------------------------------------------
	**  Start of Executable Code
	**-----------------------------------------------------------------------*/

	/* get entity data */

	e.key = key;
	um_get_all_geom(&e,sizeof(struct UM_entitydatabag));
	um_get_transformation(key,tfmat);

	/* switch on type */

	switch(e.rel_num)
		{
		case UM_POINT_REL:
			{
			struct UM_point_rec *ptr;
			ptr = (struct UM_point_rec *) &e;
			uc_transform(ptr,tfmat,UU_FALSE);
			um_vctovc(ptr->pt,pt);
			break;
			}
		case UM_LINE_REL:
			{
			struct UM_line_rec *ptr;
			ptr = (struct UM_line_rec * ) &e;
			uc_transform(ptr,tfmat,UU_FALSE);

			/* convert location from ndc to world */

			uv_ndctocc(pickloc->cord,cc,pickloc->transform);

			/* project loc and line endpoints onto view plane */

			uv_projvpln(cc,pv_cc,pickloc->transform);
			uv_projvpln(ptr->spt,pv_spt,pickloc->transform);
			uv_projvpln(ptr->ept,pv_ept,pickloc->transform);

			/* create vectors between loc and line */

			um_vcmnvc(pv_cc,pv_spt,v1);
			um_vcmnvc(pv_ept,pv_spt,v2);
			um_unitvc(v2,v2);

			/* calculate closest point on projected line */

			cdot = um_dot(v1,v2);
			um_vctmsc(v2,cdot,v2);
			um_vcplvc(ptr->spt,v2,pt);

			/* now project back onto original line */

			um_vcmnvc(ptr->ept,ptr->spt,v2);
			um_unitvc(v2,v2);
			um_vpnorm(pickloc->transform,vpnorm);
			um_ilnln(ptr->spt,v2,pt,vpnorm,&nint,cc);
			um_vctovc(cc,pt);
			break;
			}
		case UM_CIRCLE_REL:
			{
			struct UM_circle_rec *ptr;
			ptr = (struct UM_circle_rec *) &e;

			/* convert location from ndc to world */

			uv_ndctocc(pickloc->cord,cc,pickloc->transform);

			/* project point onto the plane of the circle */

			um_vpnorm(pickloc->transform,vpnorm);
			um_ilnpln(cc,vpnorm,ptr->center,ptr->nvec,&nint,pv_cc);

			/* form line with projected point and circle center */

			um_vcmnvc(pv_cc,ptr->center,v1);
			um_unitvc(v1,v1);

			/* intersect line and circle */

			um_ilncir(ptr->center,v1,ptr->center,ptr->nvec,ptr->radius,
						&nint,ptc);

			/* return intersection point */

			pt[0] = ptc[0][0];
			pt[1] = ptc[0][1];
			pt[2] = ptc[0][2];
			break;
			}

		}
	}
