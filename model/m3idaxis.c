/*********************************************************************
**    NAME         :  m3idaxis.c
**       CONTAINS:
**			um_modaxisinit()
**			um_drwaxis
**			um_drw_mod_axis(turnon)
**			um_idrwmodals()
**			um_reset_mod_axis(key) - MILLS!!!
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       m3idaxis.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:56
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "g.h"
#include "gtbl.h"
#include "dasnog.h"
#include "lcom.h"
#include "math.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mdattr.h"
#include "mattr.h"
#include "mxxx.h"
#include "mdebug.h"
#include "nclfc.h"
#include "nccs.h"
#include "mcrv.h"

struct {
	UU_KEY_ID modaxiskey;			/* UNIBASE key of model axis */
	UU_REAL length;					/* length of axis */
	int color;							/* color of axis */
	}  UM_Mod_axis;
UU_LOGICAL UM_modax_disp = UU_FALSE;
/*********************************************************************
**    I_FUNCTION     : um_modaxisinit()
**       Initialize model axis. Create a "model axis" coordinate 
**			system in UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_modaxisinit()

	{
	struct UM_coordsys_rec ccs;		/* construction coordinate system */
	struct UM_attrdata_rec attr;		/* display attributes */

	struct UM_point_rec *e1;

	uu_denter( UU_MTRC,(us,"um_modaxisinit()"));

	/* assign initial values to construction coordinate system in UNIBASE */
/*
.....Assigned initial value to label to avoid the UMR error in purify
*/
	e1 = (struct UM_point_rec *) &ccs;
	e1->label[0]='\0';
	um_vctovc(UM_zerovec, ccs.origin);
	ccs.xaxis[0] = 1.0;
	ccs.xaxis[1] = 0.0;
	ccs.xaxis[2] = 0.0;
	ccs.yaxis[0] = 0.0;
	ccs.yaxis[1] = 1.0;
	ccs.yaxis[2] = 0.0;
	ccs.zaxis[0] = 0.0;
	ccs.zaxis[1] = 0.0;
	ccs.zaxis[2] = 1.0;
	ccs.z_depth = 0.0;
	strcpy(ccs.name, "MCS");

	attr.key = -1;
	attr.rel_num = UM_ATTR_REL;
	attr.color = UL_color_mod.maxis;
	attr.layer = 1;
	attr.line_style = UM_SOLID_LINE;
	attr.line_width = 0.0;
	attr.line_weight = 1.0;
	attr.pen = 1;
	attr.displayable = UM_DISPLAYABLE;
	attr.selectable = UU_FALSE;
	attr.label_on = UU_FALSE;

	ur_setup_data(UM_COORDSYS_REL, &ccs, sizeof(ccs));
	um_create_geom(&ccs, UM_DEFAULT_TF, &attr);
	ur_update_blanked(ccs.key, UU_TRUE);

	UM_Mod_axis.modaxiskey = ccs.key;
	UM_Mod_axis.length = 50.0;
	UM_Mod_axis.color =  UM_RED;

	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION: um_drwaxis
**						(origin,xaxis,yaxis,zaxis,length)
**      Draw a  coordinate system defined by an origin and unit vectors
**			defining the X, Y, and Z axes. The length of the axis is
**			specified by a length.
**    PARAMETERS   
**       INPUT  : 
**				origin				origin of  coordinate system
**				xaxis					unit vector defining x axis
**				yaxis					unit vector defining yaxis
**				zaxis					unit vector defining zaxis
**				length				length to draw axis
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_drwaxis(origin,xaxis,yaxis,zaxis,length)
	UM_coord origin;
	UM_vector xaxis;
	UM_vector yaxis;
	UM_vector zaxis;
	UM_length length;
	{
	Gwpoint3 gpt[2];					/* points to send to GKS */
	UM_coord temp[3];				/* position of tip of axis vector */
	static char *label[] = {		/* axis labels */
		"X","Y","Z"};
	int i;								/* index */
	int cur_text_color;
	Gfloat cur_text_height;
	UU_REAL axis_length;
	UU_REAL text_height;
	UM_coord spt, ept;
	UM_ndc ndcspt, ndcept;

	uu_denter( UU_MTRC,(us,"um_drwaxis(?,length=%f)",length));

	/* deterimine the size of the axis lines and text under the current
		normtran */
	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 0.0, ndcspt);
	um_xyztovc((UU_REAL) 0.075, (UU_REAL) 0.0, (UU_REAL) 0.0, ndcept);
	gndcw3(&spt[0],&spt[1],&spt[2],ndcspt[0],ndcspt[1],ndcspt[2]);
	gndcw3(&ept[0],&ept[1],&ept[2],ndcept[0],ndcept[1],ndcept[2]);
	um_vcmnvc(ept, spt, temp[0]);
	axis_length = fabs(length);
	if (length > 0.)
	{
		axis_length = length * um_mag(temp[0]);
		if (length != 1.0)			/* then um_drwaxis was called from viewing to*/
			length = length * 12.0;	/*	put up the view axis - and the axis length*/
											/* is so small the characters are unreadable */
	}
	text_height = length * axis_length/10.0;

	/* get current text attributes */
	cur_text_color = gqtextcolor();
	cur_text_height = gqcharheight();
	
	/* set text attributes */
	gstextcolor(UM_WHITE);

	gscharheight(text_height);

	/* draw axis and labels */
	gpt[0].x = origin[0];
	gpt[0].y = origin[1];
	gpt[0].z = origin[2];
	um_vctmsc(xaxis,axis_length,temp[0]);
	um_vcplvc(origin,temp[0],temp[0]);
	um_vctmsc(yaxis,axis_length,temp[1]);
	um_vcplvc(origin,temp[1],temp[1]);
	um_vctmsc(zaxis,axis_length,temp[2]);
	um_vcplvc(origin,temp[2],temp[2]);
	for (i=0; i<3; i++)
		{
		gpt[1].x = temp[i][0];
		gpt[1].y = temp[i][1];
		gpt[1].z = temp[i][2];
		gpolyline3(2,gpt);
		gtext(&gpt[1],label[i]);
		}

	/* finally, restore text attributes */
	gstextcolor (cur_text_color);
	gscharheight(cur_text_height);

	uu_dexit;
	}
/*********************************************************************
**
**     FUNCTION:  umf_drw_mod_axis
**
**      Buffer function from disply to um_drw_mod_axis.
**      To be used when user would like to disply modeling axes.
**
**********************************************************************/
umf_drw_mod_axis()

	{
	uu_denter(UU_MTRC,(us,"umf_drw_mod_axis()"));
   um_drw_mod_axis(UU_TRUE);

	return 0;
	}
/**********************************************************************
**
**      FUNCTION:   umf_drw_mod_axis_off
**
**      Buffer function from delgeo to um_drw_mod_axis.
**      To be used when user would like to no longer display
**      modeling axes.
**
**********************************************************************/
umf_drw_mod_axis_off()

	{
	uu_denter(UU_MTRC,(us,"umf_drw_mod_axis_off()"));
   um_drw_mod_axis(UU_FALSE);

	return 0;
	}
/*********************************************************************
**    I_FUNCTION     : um_drw_mod_axis(turnon)
**      Display model  coordinate axis.
**    PARAMETERS   
**       INPUT  : 
**				turnon			UU_TRUE => turn on axis
**									UU_FALSE => turn off axis
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_drw_mod_axis(turnon)
	UU_LOGICAL turnon;

{
	struct NCL_fixed_databag e;
	struct UM_coordsys_rec *ccsp;
	int status;
	int dsegid;

	ccsp = (struct UM_coordsys_rec *)&e;
	ccsp->key = UM_Mod_axis.modaxiskey;
	status = ncl_retrieve_data_fixed(ccsp);
	if (status == UU_SUCCESS && ccsp->rel_num == UM_COORDSYS_REL)
	{
		if (turnon)
		{
			ur_update_blanked(ccsp->key, UU_FALSE);
			uc_display(ccsp);
			UM_modax_disp = UU_TRUE;
		}
		else
		{
			ur_retrieve_disp_segid(UM_Mod_axis.modaxiskey, &dsegid);
			if (dsegid != -1)
			{
				uv_delsegs(dsegid);
				ur_update_disp_segid(ccsp->key, -1);
			}
			ur_update_blanked(ccsp->key, UU_TRUE);
		   UM_modax_disp = UU_FALSE;
		}
	}
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : um_idrwmodals()
**      Initialize all of the display modals.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_idrwmodals()

	{

	uu_denter(UU_MTRC,(us,"um_idrwmodals"));

	/*---------- entity display attributes ---------------------------*/
	ur_put_attrmdl_color(UM_YELLOW);
	ur_put_attrmdl_line_style(UM_SOLID_LINE);
	ur_put_attrmdl_pen(1);
	ur_put_attrmdl_line_width((UU_REAL) 0.0);
	ur_put_attrmdl_line_weight((UU_REAL) 1.0);
	ur_put_attrmdl_displayable(UM_DISPLAYABLE);
	ur_put_attrmdl_selectable(UU_TRUE);
	ur_put_attrmdl_label_on(UU_FALSE);
	um_set_active_layer(1);

	/*---------- point display attributes ---------------------------*/
	UM_ptattr.snap_node = UU_FALSE;	/* default: point is not a snap node */
	UM_ptattr.markertype = 2;			/* default marker type */

	/*---------- curve display attributes ---------------------------*/
	UM_crvattr.relcirerr = .01; 		/* circle cording accuracy as % of radius */
	UM_crvattr.maxpts = 400;			/* maximum number of pts per curve */

	/*---------- surface display attributes ---------------------------*/
	UM_srfattr.numupaths = 5;			/* number of u paths for surface */
	UM_srfattr.numvpaths = 5;			/* number of v paths for surface */
	UM_srfattr.ptsperucrv = 0;			/* number of points per u curve */
	UM_srfattr.ptspervcrv = 0;			/* number of points per v curve */
	UM_srfattr.maxpaths = 200;			/* maximum number of paths */
	UM_srfattr.edge_disp = UU_FALSE;
	UM_srfattr.edge_color = 8;
	UM_srfattr.shaded = UU_TRUE;
	UM_srfattr.lucency = 100;

	/*---------- solids display attributes ---------------------------*/
	UM_solattr.pitch = 3.;				/* default planar hatching pitch */

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : um_reset_mod_axis(key)
**      MILLS: reset UM_Mod_axis.modaxiskey after load of Unibase without
**             viewing and coordsys entities.
**    PARAMETERS   
**       INPUT  : 
**          key 	key of UM_dispattr 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : reset UM_Mod_axis.modaxiskey to key+1
**    WARNINGS     : none
*********************************************************************/
um_reset_mod_axis(key)
UU_KEY_ID key;
	{
	UM_Mod_axis.modaxiskey = key + 1;

	return 0;
	}
/*********************************************************************
**    E_FUNCTION     : um_set_mod_axis(origin, xaxis, yaxis, zaxis)
**      Set model coordinate system.
**    PARAMETERS   
**       INPUT  : 
**          origin   - Origin.
**          xaxis    - Xaxis.
**          yaxis    - Yaxis.
**          zaxis    - Zaxis.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_set_mod_sys (origin, xaxis, yaxis, zaxis)
UM_coord origin;
UM_vector xaxis;
UM_vector yaxis;
UM_vector zaxis;
   {
   struct NCL_fixed_databag e;
   struct UM_coordsys_rec *ccsp;
   int status;
   UM_int2 ifl, ifl35=35;

   ccsp = (struct UM_coordsys_rec *)&e;
   um_vctovc(origin, UM_origin);
   um_vctovc(xaxis, UM_xaxis);
   um_vctovc(yaxis, UM_yaxis);
   um_vctovc(zaxis, UM_zaxis);

   ccsp->key = UM_Mod_axis.modaxiskey;
   status = ncl_retrieve_data_fixed(ccsp);
	if (status == UU_SUCCESS && ccsp->rel_num == UM_COORDSYS_REL)
	{
		um_vctovc(origin, ccsp->origin);
		um_vctovc(xaxis, ccsp->xaxis);
		um_vctovc(yaxis, ccsp->yaxis);
		um_vctovc(zaxis, ccsp->zaxis);
/*    ccs.z_depth = 0.0; */
		ur_update_data_fixed (ccsp);
/*
.....Do not draw mod axis in batch
*/
		getifl(&ifl35,&ifl);
		if (ifl == 0)
		{
			if (UM_modax_disp) um_drw_mod_axis(UU_TRUE);
		}
	}

   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : um_get_axis(cname,*key)
**      Set model coordinate system.
**    PARAMETERS   
**       INPUT  : 
**          cname    - coordinate' name.
**       OUTPUT :  
**          key      - unibase key
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_get_axis (cname,key)
char *cname;
UU_KEY_ID *key;
{
	int status, entnum, ifl;
	struct UM_coordsys_rec cpln;

	*key = 0;
	entnum = status = ifl = 0;

	while(status == 0)
	{
		entnum++;
		status = ur_get_next_new_data_key(UM_COORDSYS_REL, &entnum, &cpln.key);
		if (status == 0)
		{
			uc_retrieve_data(&cpln, sizeof(cpln));
			status = ifl = !strcmp(cpln.name,cname);
		}
	}
	if (ifl) *key = cpln.key;
}

/*********************************************************************
**    E_FUNCTION     : um_get_modaxis_key()
**      Returns the key value of the modelling axis.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID um_get_modaxis_key()
{
	return(UM_Mod_axis.modaxiskey);
}
