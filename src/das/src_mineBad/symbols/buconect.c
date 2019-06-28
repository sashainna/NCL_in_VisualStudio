/*********************************************************************
**    NAME:  bconect.c
**       CONTAINS:
**			ubu_connector(lnwidth)
**			int ub_cr64_connect(eptr, tfmat, attrptr)
**			int ub_dl64_connect(key)
**			int ub_disp64_connect(con)
**			int ub_drw64_connect(con, tfmat, attrptr)
**			int ub_transf64_connect(con, tfmat, store)
**			int ub_tran64_connect(con, offset)
**			int ub_rot64_connect(con, pt, dir, ang, tfmat)
**			int ub_scale64_connect(con, pt, tfmat, scalefactor)
**			int ub_mir64_connect(con, pt, normal, tfmat)
**			int ub_p64_connect(con, tfmat)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       buconect.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:05
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
/* #include "dasnog.h" */
#include "dselmask.h"
#include "dmark.h"
#include "drubber.h"
#include "dasg.h" 	/* UD_nxhair.position.y */
#include "class.h"	/* for UC_attributedatabag */
#include "mdrel.h"
#include "mdcoord.h"
#include "mcrv.h"
/* #include "mattr.h"	/* for UM_transf_rec and UM_attrmdl */ 
#include "mattrddl.h"
#include "mdattr.h"
#include "mdpick.h"
#include "modef.h" 

#include "bsym.h"
/*********************************************************************
**    E_FUNCTION: ubu_connector(lnwidth)
**			Add a connector.
**    PARAMETERS   
**       INPUT  : 
**          lnwidth						width of connector
**       OUTPUT :  
**          none
**    RETURNS: UU_SUCCESS if no problems; UB_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ubu_connector(lnwidth)
	UU_REAL lnwidth;
{
	struct UM_polyline_rec pline;	/* polyline curve */
	struct UB_conector_rec con;		/* connector record */
	UM_coord fstpt, lstpt;			/* first point and last point of connector */
	UM_ndc ndcfstpt;					/* ndc coordinate for first point */
	UU_REAL oldwidth;					/* save old line width */
	int oldstyle;						/* save old line style */
	int n;								/* segment number for temp segment */
	UM_PLOCREC pick1;					/* pick location on first symbol */
	UM_PLOCREC pick2;					/* pick location on second symbol/connector */
	UD_RUBBER rubber;					/* rubber band control block */
	UU_LOGICAL snapFound;
	int cmdreject, numint, i, status = UU_SUCCESS;
	UU_REAL z;

    UD_NDCLOCREC tmp;

	uu_denter(UU_BTRC, (us, "ubu_connector(lnwidth=%g)",lnwidth));
	UD_MARK(cmdreject, UU_FALSE);
	if (cmdreject) goto done;

	oldwidth = ur_get_attrmdl_line_width();
	ur_put_attrmdl_line_width(lnwidth);

	oldstyle = ur_get_attrmdl_line_style();
	if (lnwidth < UM_FUZZ)
		ur_put_attrmdl_line_style(UM_DASHED_LINE);
	else
		ur_put_attrmdl_line_style(UM_SOLID_LINE);

	RUBBER_LINE(&rubber);
	while (UU_TRUE)
	{
		/* setup internal record storage for connector entity */
		ur_setup_app_data(UB_CONECTOR_REL, &con, sizeof(con));

		/* pick pin on symbol to start connector */
		ud_lgeo(UU_TRUE, UD_symbol);

		snapFound = UU_FALSE;
		while (!snapFound)
		{
			um_dl_pldas(UD_DASPCKLOC, UB_SYMBOL, 100, &pick1, 1, &numint, 1);
			if (numint <= 0) goto done;

			/* calculate starting point of connector */
			if (ub_snapnode_of_instance(&pick1, fstpt, &snapFound) 
						!= UU_SUCCESS) goto failed;
		}

		/* pick connector or pin on symbol to end connector */
		snapFound = UU_FALSE;
		while (!snapFound)
		{
			um_dl_pldas(UD_DASPCKLOC, UB_SYMBOL, 101, &pick2, 1, &numint, 1);
			if (numint <= 0) goto done;

			/* calculate end point of connector */
			if (ub_snapnode_of_instance(&pick2, lstpt, &snapFound) 
						!= UU_SUCCESS) goto failed;
		}

		/* initialize internal storage for polyline entity */
		ur_setup_data(UM_POLYLINE_REL, &pline, sizeof(pline));

		um_vctovc(fstpt, &pline.pt[0]);
		pline.no_pt = 1;
		i = 3;

		/* 	-- get intermediate coordinates for connector -- */

		uv_cctondc(fstpt, ndcfstpt, pick1.ploc.transform);
		UD_nxhair.transform = pick1.ploc.transform;
		UD_nxhair.position.x = ndcfstpt[0];
		UD_nxhair.position.y = ndcfstpt[1];
		UD_nxhair.position.z = ndcfstpt[2];
		UD_GLOC3_NDC2W(&UD_wxhair, &UD_nxhair);
		RUBBER_ON(&rubber);

		/* initialize a segment for the exho of rubber band line */
		n = gnseg();
		gcreateseg(n);
		gcloseseg();

		while (UU_TRUE)
		{
			/* enter intermediate coordinate of connector*/
			ud_ldas(UD_DASCART, UB_SYMBOL, 102, &tmp, 1, &numint,
				UD_NODEFAULT);
            pline.pt[i]   = tmp.cord[0];
            pline.pt[i+1] = tmp.cord[1];
            pline.pt[i+2] = tmp.cord[2];

			if (numint <= 0) goto repeat;

			/* temporarily draw the rubber band line in non-retained segment */
			gopenseg(n);
			gsnormtran(pick1.ploc.transform);
			gpolyline3(2, &pline.pt[i-3]);
			gcloseseg();

			i = i + 3;
			pline.no_pt++;
		}
repeat:
		/* turn rubber banding off */
		ud_endrub(&rubber);

		um_vctovc(lstpt, &pline.pt[i]);
		pline.no_pt++;

		/* create polyline entity in UNIBASE but make sure it is never
			displayed */
		um_create_geom(&pline, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		ur_update_displayable(pline.key, UM_NEVERDISPLAYABLE);
		con.pline = pline.key;

		/* associate instance with connector */
		con.ainst[0] = um_get_pickkey(&pick1, 1);
		con.ainst[1] = um_get_pickkey(&pick2, 1);
		sprintf(UB_sbuf,"inst1=%x, inst2=%x",con.ainst[0], con.ainst[1]);
		ubi_pscroll(UB_sbuf);
		con.no_ainst = 2;

		/* delete the temp segment of the rubber-banding lines */
		gdeleteseg(n);

		/* create and display connector */
		ub_cr64_connect(&con, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		ub_disp64_connect(&con);

		/* free application data areas */
		ur_free_app_data(&con);
	}/* end while */

failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:
	UD_UNMARK(cmdreject);
	RUBBER_OFF(&rubber);
	ur_put_attrmdl_line_width(oldwidth);
	ur_put_attrmdl_line_style(oldstyle);
	uu_dexit;
	return(status);
}
