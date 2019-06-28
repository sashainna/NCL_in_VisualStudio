/*********************************************************************
**    NAME         :  adisparc.c
**       CONTAINS:
**			ua_disp1_arc
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       adisparc.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:33
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mdeval.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
/*********************************************************************
**    I_FUNCTION :  ua_disp1_arc(key,cpt,radius,sang,eang,tcolor,tstyle,
**											porig,pxaxis,pyaxis,pzaxis)
**			Display a drafting arc using the modeling circular arc
**			routines- create a dummy circle(arc) entity and pass to
**			um_drw_geometry.
**			Direction of arc is always positive angle from start angle
**				to end angle. If the start is greater than end, we must
**				sweep around through the xaxis (normally this will give
**				us the long arc angle rather than the shortest angle).
**    PARAMETERS   
**       INPUT  : 
**				cpt				center point of arc
**				radius			radius of arc
**				sang				arc start angle
**				eang				arc end angle
**				tcolor		   arc color
**				tstyle			arc line style
**				porig,pxaxis,pyaxis,pzaxis		dimen plane for arc.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_disp1_arc(key,cpt,radius,sang,eang,tcolor,tstyle,porig,pxaxis,pyaxis,pzaxis)
UU_KEY_ID   key;
UM_coord		cpt;					/* arc center point */
UM_length		radius;				/* arc radius */
UM_angle		sang;					/* arc start angle */
UM_angle		eang;					/* arc end angle */
int			tcolor;
int         tstyle;
UM_coord		porig;				/* dimension plane origin pt */
UM_vector		pxaxis,pyaxis,pzaxis;	/* dimen plane axis */
{
	UM_angle		dang;					/* angle from start to end */
	struct UM_circle_rec		e;		/* circle entity */
	struct UM_rotmatrix		m;		/* for rotate point routine */
	struct UM_attrdata_rec attrptr;
	int status;
	int uj_setpen();
	UU_LOGICAL uj_miplotting();

	/*----------- begin function code ----------------------------------*/

	uu_denter(UU_STRC,
		(us,"ua_disp1_arc(cpt=<%g,%g,%g>,rad=%g,sang=%g,eang=%g,)",
		cpt[0],cpt[1],cpt[2],radius,sang,eang));
	uu_denter2(UU_STRC,
		(us,"ua_disp1_arc(porig=<%g,%g,%g>,pxaxis=<%g,%g,%g>,)",
		porig[0], porig[1], porig[2],
		pxaxis[0], pxaxis[1], pxaxis[2]));
	uu_dexit;
	uu_denter2(UU_STRC,
		(us,"ua_disp1_arc(pyaxis=<%g,%g,%g>,pzaxis=<%g,%g,%g>)",
		pyaxis[0], pyaxis[1], pyaxis[2],
		pzaxis[0], pzaxis[1], pzaxis[2]));
	uu_dexit;
	/*-------- fill in dummy circle entity for display ---------------*/
	e.key = 0;
	e.rel_num = UM_CIRCLE_REL;
	e.radius = radius;
	if (sang>=eang)					/* sweep around through xaxis */
		e.dang = (UM_TWOPI - sang) + eang;
	else									/* arc angle is shortest */
		e.dang = eang - sang;
	um_vctovc(cpt,e.center);
	um_vctovc(pxaxis,e.svec);		/* start vec is xaxis rotated */
	um_rotatept(e.svec,				/* rotate xaxis to start angle */
					pzaxis,porig,sang,UU_TRUE,&m);
	um_vctovc(pzaxis,e.nvec);
	status = uc_retrieve_attr(key, &attrptr);
	if(uj_miplotting())
			attrptr.color = uj_setpen(attrptr.pen);
	else
		attrptr.color = tcolor;
	attrptr.line_style = tstyle;
	attrptr.line_width = 0.0;
	attrptr.line_weight = 1.0;
	/*--------- go display arc using drawing dispatcher ---------------*/
	uc_draw(&e,UM_DEFAULT_TF,&attrptr); /* unicad drawer*/
	uu_dexit;
}
