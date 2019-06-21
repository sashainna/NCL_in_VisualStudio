/*********************************************************************
**    NAME:  bconinst.c
**       CONTAINS:
**				ubu_insert_connector(inst, pick, instmat)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       bconinst.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:02
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "dmark.h"
#include "dasg.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "class.h"			/* for UC_attributedatabag */
/* #include "mattr.h" */
#include "mdattr.h"
#include "mdpick.h"
#include "mdebug.h"
#include "modef.h"

#include "bsym.h"
/*********************************************************************
**    E_FUNCTION: ubu_insert_connector(inst, pick, tfmat)
**			Orient the given symbol instance (INST) to lie on the
**			the picked connector (PICK).
**    PARAMETERS   
**       INPUT  : 
**				inst						pointer to instance record
**				pick						pick loc (on connector)
**       OUTPUT :  
**				instmat					transformation matrix
**    RETURNS: UU_SUCCESS iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ubu_insert_connector(inst, pick, instmat)
	struct UB_instance_rec *inst;
	UM_PLOCREC *pick;
	UM_transf instmat;
{
	struct UB_conector_rec con;
	struct UB_conector_rec newcon;
	struct UM_polyline_rec pline;
	struct UM_polyline_rec newpline;
	struct UM_point_rec pin1;
	struct UM_point_rec pin2;
	struct UC_attributedatabag attr;
	UM_coord pt1;
	UM_coord pt2;
	UM_coord mcs_pickloc;
	UM_coord new_origin;
	UM_coord old_origin;
	UM_vector temp;
	UM_vector inst_vec;
	UM_vector con_vec;
	UM_transf rotmat;
	UM_transf tranmat;
	UM_angle new_angle;
	UU_REAL dist, min_dist;
	int status, i, j, k, from, to;

	uu_denter(UU_BTRC, (us, "ubu_insert_connector()"));

	/* get the UNIBASE key of the picked entity, check if it is a connector,
		and retrieve the data if it was */
	con.key = um_get_pickkey(pick, 1);
	status = ur_retrieve_data_relnum(con.key, &con.rel_num);
	if (status != UU_SUCCESS) goto failed;
	if (con.rel_num != UB_CONECTOR_REL) goto failed;
	ur_retrieve_app_data(&con, sizeof(con));
	um_get_disp_attr(con.key, &attr);

	/* determine the unit vector from the first pin to the second pin
		of the instance */
	if (inst->no_snap_nod < 2) goto failed;
	pin1.key = inst->snap_nod[0].snap_key;
	um_get_all_geom(&pin1, sizeof(pin1));
	pin2.key = inst->snap_nod[1].snap_key;
	um_get_all_geom(&pin2, sizeof(pin2));
	um_vcmnvc(pin2.pt, pin1.pt, inst_vec);
	um_vctmsc(inst_vec, (UU_REAL) 0.5, temp);
	um_vcplvc(pin1.pt, temp, old_origin);
	um_vctmsc(old_origin, (UU_REAL)  -1.0, old_origin);
	um_unitvc(inst_vec, inst_vec);
	sprintf(UB_sbuf, "inst vec = (%g, %g, %g)", inst_vec[0], 
		inst_vec[1], inst_vec[2]);
	ubi_pscroll(UB_sbuf);

	/* determine the coordinate of the picked location (MCS) on the connector */
	um_ploctocc(&(pick->ploc), mcs_pickloc);
	sprintf(UB_sbuf, "msc pickloc = (%g, %g, %g)", mcs_pickloc[0], 
		mcs_pickloc[1], mcs_pickloc[2]);
	ubi_pscroll(UB_sbuf);

	/* retrieve the polyline data of the connector and determine which segment
		was picked */
	pline.key = con.pline;
	ur_retrieve_data(&pline, sizeof(pline));
	ubi_pscroll("original connector polyline");
	uc_print(&pline);
	min_dist = 9999999.0;
	for (i=0, j=0; i<(pline.no_pt-1); i++, j=j+3)
	{
		um_vcmnvc(&(pline.pt[j+3]), &(pline.pt[j]), con_vec);
		um_unitvc(con_vec, con_vec);
		um_nptln(mcs_pickloc, &(pline.pt[j]), con_vec, new_origin);
		um_vcmnvc(mcs_pickloc, new_origin, temp);
		dist = um_mag(temp);
		if (dist < min_dist)
		{
			min_dist = dist;
			k = i;
		}
	}
	i = k;
	j = 3*k;
	sprintf(UB_sbuf,"on seg: i=%d, j=%d", i, j);
	ubi_pscroll(UB_sbuf);

	/* the new origin is the nearest point on the picked segment of
		the polygon to the picked location */
	um_vcmnvc(&(pline.pt[j+3]), &(pline.pt[j]), con_vec);
	um_unitvc(con_vec, con_vec);
	um_nptln(mcs_pickloc, &(pline.pt[j]), con_vec, new_origin);
	sprintf(UB_sbuf, "new origin = (%g,%g,%g)", new_origin[0], new_origin[1],
		new_origin[2]);
	ubi_pscroll(UB_sbuf);

	/* the angle of rotation is the smallest angle to rotate the unit
		vector between the pins into the unit vector along the segment
		of the polyline picked */
	new_angle = um_angle2p(inst_vec, con_vec, UM_zaxis);
	sprintf(UB_sbuf, "new angle = %g", new_angle);
	ubi_pscroll(UB_sbuf);

	/* determine the transformation which places the instance along
		the connector */
	um_disptf(old_origin, tranmat);
	um_rottf(UM_zaxis, new_angle, rotmat);
	um_tftmtf(tranmat, rotmat, instmat);
	um_disptf(new_origin, tranmat);
	um_tftmtf(instmat, tranmat, instmat);

	/* determine the coordinates of the two pins along the split
		connector */
	um_cctmtf(pin1.pt, instmat, pt1);
	um_cctmtf(pin2.pt, instmat, pt2);
	sprintf(UB_sbuf, "new pt1 = (%g,%g,%g)", pt1[0], pt1[1], pt1[2]);
	ubi_pscroll(UB_sbuf);
	sprintf(UB_sbuf, "new pt2 = (%g,%g,%g)", pt2[0], pt2[1], pt2[2]);
	ubi_pscroll(UB_sbuf);

	/* delete the picked connector (this removes it from its associated
		instances */
	ub_dl64_connect(con.key);

	/* create a polyline for the first part of the split connector */
	ur_setup_data(UM_POLYLINE_REL, &newpline, sizeof(newpline));
	newpline.no_pt = i + 2;
	for (k=0, to=0, from=0; k<(newpline.no_pt-1); k++, to=to+3, from=from+3)
		um_vctovc(&pline.pt[from], &newpline.pt[to]);
	um_vctovc(pt1, &newpline.pt[to]);
	um_create_geom(&newpline, UM_DEFAULT_TF, UM_CURRENT_ATTR);
	ur_update_displayable(newpline.key, UM_NEVERDISPLAYABLE);
	ubi_pscroll("first polyline");
	uc_print(&newpline);

	/* create a connector for the first part of the split connector */
	ur_setup_app_data(UB_CONECTOR_REL, &newcon, sizeof(newcon));
	newcon.pline = newpline.key;
	newcon.no_ainst = 2;
	newcon.ainst[0] = con.ainst[0];
	newcon.ainst[1] = inst->key;
	ub_cr64_connect(&newcon, UM_DEFAULT_TF, &attr);
	ub_disp64_connect(&newcon);

	/* create a polyline for the second part of the split connector */
	ur_setup_data(UM_POLYLINE_REL, &newpline, sizeof(newpline));
	newpline.no_pt = pline.no_pt - i;
	um_vctovc(pt2, &newpline.pt[0]);
	for (k=0, to=3; k<(newpline.no_pt-1); k++, to=to+3, from=from+3)
		um_vctovc(&pline.pt[from], &newpline.pt[to]);
	um_create_geom(&newpline, UM_DEFAULT_TF, UM_CURRENT_ATTR);
	ur_update_displayable(newpline.key, UM_NEVERDISPLAYABLE);
	ubi_pscroll("second polyline");
	uc_print(&newpline);

	/* create a connector for the second part of the split connector */
	ur_setup_app_data(UB_CONECTOR_REL, &newcon, sizeof(newcon));
	newcon.pline = newpline.key;
	newcon.no_ainst = 2;
	newcon.ainst[0] = inst->key;
	newcon.ainst[1] = con.ainst[1];
	ub_cr64_connect(&newcon, UM_DEFAULT_TF, &attr);
	ub_disp64_connect(&newcon);

	/* free application data areas */
	ur_free_app_data(&con);
	ur_free_app_data(&newcon);

	status = UU_SUCCESS;
	goto done;

failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:
	uu_dexit;
	return (status);
}

