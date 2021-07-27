/*********************************************************************
**    NAME         :  mucirc4
**       CONTAINS:
**			umu_m3_modcircen(key)
**			umu_m3_modcirrad(key)
**			umu_m3_breakcirc
**			umu_m3_mvcirend
**			umu_m3_chgcirang
**			umu_m3_closecir(key)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ucirc4.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:57
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "mfort.h"
#include "mdclass.h"
#include "mdrel.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdpick.h"
#include "nclvx.h"

/*********************************************************************
**    E_FUNCTION     : umu_m3_modcircen(key)
**			Modify the center of a circle. If KEY is UU_NULL, the user
**			is prompted to pick a circle, otherwise, the given key is
**			assumed to be the circle.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_m3_modcircen(key)
	UU_KEY_ID key;

	{
	UM_PICKENT	pent;						/* pick path to picked entity */
	int			numint;					/* the number of interactions input
													through das								*/
	struct		UM_circle_rec	c1;	/* the first circle entity				*/
    UD_NDCLOCREC tmp;
    int i;


	uu_denter(UU_MTRC,(us,"umu_m3_modcircen(key=%x)",key));

	if (key != 0)
		{
		c1.key = key;
		}
	else
		{
		/* limit DAS to pick only circles/arcs */
		ud_lgeo(UU_TRUE, UD_circle);
		um_dl_pdas(UD_DASPICK, UM_MODEL, 190, &pent, 1, &numint, 1);
		/* message is: Pick circles to have their centers changed */
		if (numint <= 0) goto done;
		c1.key = um_get_pickkey(&pent, 1);
		}

	/* get entity */
	um_get_all_geom(&c1, sizeof(c1));

	ud_ldas(UD_DASCART, /*New center point: */UM_MODEL, 108,
				&tmp, 1, &numint, UD_DEFAULT);
    for (i=0; i<3; i++) c1.center[i] = tmp.cord[i];

	if (numint <= 0) goto done;

	/* move circle */
	um_update_geom(&c1, UM_DEFAULT_TF);
	uc_display(&c1);

done:
	ud_lgeo(UU_FALSE, UD_circle);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_m3_modcirrad(key)
**       Modify the radius of a circle. If KEY is UU_NULL, prompt the
**			user to pick the circle to be modified, otherwise, KEY is
**			assumed to be a circle.  In either case, the user is prompted
**			to enter the new radius.
**    PARAMETERS   
**       INPUT  : 
**          key							UU_NULL => prompt user to pick circle
**												otherwise, this is the key of a circle
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_m3_modcirrad(key)
	UU_KEY_ID key;

	{
	UM_PICKENT pent;						/* pick path to picked entity */
	int numint;								/* the number of interactions input
													through das */
	struct UM_circle_rec	c1;			/* the first line entity */
	UU_REAL newrad;						/* new center location */

	uu_denter( UU_MTRC,(us,"umu_m3_modcirrad(key=%x)",key));

	if (key != 0)
		{
		c1.key = key;
		}
	else
		{
		ud_lgeo(UU_TRUE, UD_circle);
		um_dl_pdas(UD_DASPICK, UM_MODEL, 107, &pent, 1, &numint, 1);
		/* message is: Pick circle to be modified */
		if (numint <= 0) goto done;
		c1.key = um_get_pickkey(&pent, 1);
		}

	um_get_all_geom(&c1, sizeof(c1));	

	/* get new radius */
	newrad = c1.radius;
	ud_ldas(UD_DASDISTANCE, /*New radius: */UM_MODEL, 
				109, &newrad, 1, &numint, UD_DEFAULT);
	if (numint <= 0) goto done;

	if (newrad > UM_FUZZ)
		{
		c1.radius = newrad;
		um_update_geom(&c1, UM_DEFAULT_TF);
		uc_display(&c1);
		}
	else
		/* radius is too small */
		uu_uerror0(UM_MODEL, 19);

done:
	ud_lgeo(UU_FALSE, UD_circle);
	uu_dexit;
	}

#define UM_BRKANGLE (UU_REAL) 0.200
/*********************************************************************
**    E_FUNCTION     : umu_m3_breakcirc()
**			Break a circle or arc so that endpoints can be moved.  An 
**			arc is split into two arcs
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_m3_breakcirc()

	{
	UM_PLOCREC		pick;					/* the pick id and location */
	int				numint;				/* the number of interactions input
													through das */
	struct			UM_circle_rec c1;	/* the picked circle entity */
	UM_vector		breakvec;			/* vector to break point */
	UM_transf		rotmat;				/* rotation matrix */
	int				status;

	uu_denter( UU_MTRC,(us,"umu_m3_breakcirc()"));

	ud_lgeo(UU_TRUE, UD_circle);
	um_dl_pldas(UD_DASPCKLOC, /*Pick circle at point to be broken:*/UM_MODEL, 162, 
					&pick, 1, &numint, 1);
	if (numint <= 0) goto done;

	/* retrieve entity info */
	c1.key = um_get_pickkey(&pick.pent, 1);
	um_get_all_geom(&c1, sizeof(c1));

	/*-------------------------------------------------------------
	**
	**	Find unit vector to breakpt that lines in plane of circle
	*/

	/* project picked location onto circle plane along view normal */
	status = um_projploctopln(&pick.ploc, c1.center, c1.nvec, breakvec);
	
	if (status != 0)
		{
		/* Arc is parallel to view normal, i.e. its normal is perpendicular to view normal */
		uu_uerror0(UM_MODEL, 135);
		goto done;
		}

	um_vcmnvc(breakvec, c1.center, breakvec);
	um_unitvc(breakvec, breakvec);

	/* if entity is a full circle, then rotate svec by UM_BRK_ANGLE
		and reduce dang */
	if (fabs(fabs(c1.dang) - UM_TWOPI) < UM_FUZZ)
		{
		um_rottf(c1.nvec, UM_BRKANGLE, rotmat);
		um_cctmtf(breakvec, rotmat, c1.svec);
		c1.dang = UM_TWOPI - 2.0 * UM_BRKANGLE;
		um_update_geom(&c1, UM_DEFAULT_TF);
		uc_display(&c1);
		}
	else
		/* Arc with end points cannot be broken */
		uu_uerror0(UM_MODEL, 135);

done:
	ud_lgeo(UU_FALSE, UD_circle);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_m3_mvcirend()
**       Move the endpoint of a circular arc.  The user picks arc near end
**			to be moved, then picks point for new end.  Radius of arc will
**			not be changed so the endpt will be on vector between center of
**			arc and user picked point.  The new dang of the arc will be the
**			same sign as the old dang.
**			This is a semantic action.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_m3_mvcirend()

	{
	UM_PLOCREC		pick;					/* the pick id and location */
	int				numint;				/* the number of interactions input
													through das */
	struct			UM_circle_rec	c1;/* the first line entity */
	UU_REAL			angle;				/* new angle of arc */
	UM_coord			pickpt;				/* point used to pick arc */
/**	UM_coord			newpt;	**/			/* new end point location */
    UD_NDCLOCREC        newpt;

	UM_coord			startpt;				/* start point of arc */
	UM_coord			endpt;				/* end point of arc */
	UM_vector		newvec;				/* vector to new end point location */
	UM_vector		endvec;				/* vector to end point */
	int				status;
	char label[NCL_MAX_LABEL];
	int subscr;
	UU_KEY_ID key;

	uu_denter(UU_MTRC,(us,"umu_m3_mvcirend()"));

	ud_lgeo(UU_TRUE, UD_circle);
	um_dl_pldas(UD_DASPCKLOC, /*Pick arc near end to be moved:*/UM_MODEL, 
			163, &pick, 1, &numint, 1);
	if (numint <= 0) goto done;

	/* retrieve entity info */
	c1.key = um_get_pickkey(&pick.pent, 1);
	ur_retrieve_data_relnum(c1.key, &c1.rel_num);
	um_get_all_geom(&c1, sizeof(c1));	

	/* allow movement only of arc ends */
	if (fabs(c1.dang - UM_TWOPI) < UM_FUZZ)
		{
		/* No end point; break circle first. */
		uu_uerror0(UM_MODEL,134);
		goto done;
		}

	/*-------------------------------------------------------------
	**
	**	Find which end of arc is to be moved
	*/
	/* save off label, key, subscr for later */
	strncpy(label,c1.label, NCL_MAX_LABEL);
	subscr = c1.subscr;
	key = c1.key;
	
	/* compute end points of arc */
	um_get_endpts(&c1, UM_idmat, startpt, endpt);
	um_vcmnvc(endpt, c1.center, endvec);
	um_unitvc(endvec, endvec);

	/* project picked location onto circle plane along view normal */
	status = um_projploctopln(&pick.ploc, c1.center, c1.nvec, pickpt);

	if (status != 0)
		{
		/* Arc is parallel to view normal, i.e. its normal is perpendicular to view normal */
		uu_uerror0(UM_MODEL, 135);
		goto done;
		}

	/* if startpt is closer, switch ends */
	if (um_closept(startpt, endpt, pickpt) == 1)
		{
		um_vctovc(endvec, c1.svec);
		c1.dang = -c1.dang;
		}

	um_vctovc(endpt, &newpt);

	/* get pt to move endpoint to */
	ud_ldas(UD_DASCART, /*New endpoint of arc: */UM_MODEL, 110, &newpt, 1, &numint, UD_DEFAULT);
	if (numint <= 0) goto done;

	/* compute vector in plane of circle to newpt */
	um_vcmnvc(&newpt, c1.center, newvec);
	um_projvect(newvec, c1.nvec, newvec);

	/* compute angle between start and newvec */
	angle = um_angle2p(c1.svec, newvec, c1.nvec);
	if (c1.dang < 0.0)
		{
		if (angle > 0.0)
			angle -= UM_TWOPI;
		}
	else
		if (angle < 0.0)
			angle += UM_TWOPI;
	if (fabs(angle*c1.radius) < UM_FUZZ)
		{
		/* startpt and endpt are identical */
		uu_uerror0(UM_MODEL, 55);
		goto done;
		}
	c1.dang = angle;

	/*reset label and subscr */
	strncpy(c1.label,label, NCL_MAX_LABEL);
	c1.subscr = subscr;
	c1.key = key;

	um_update_geom(&c1, UM_DEFAULT_TF);
	uc_display(&c1);

done:
	ud_lgeo(UU_FALSE, UD_circle);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_m3_chgcirang()
**			Change the angle subtended by an arc to the value specified
**			by the user. Prompt the user for the endpoint to be moved.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_m3_chgcirang()

	{
	UM_PLOCREC		pick;					/* the pick id and location */
	int				numint;				/* the number of interactions input
													through das */
	struct			UM_circle_rec	c1;/* the first circle entity */
	UM_coord			pickpt;				/* point used to pick arc */
	UM_angle			newangle;			/* new angle value */
	UM_coord			startpt;				/* start point of arc */
	UM_coord			endpt;				/* end point of arc */
	UM_vector		endvec;				/* vector to end point */
	int				status;
	char label[NCL_MAX_LABEL];
	int subscr;
	UU_KEY_ID key;

	uu_denter(UU_MTRC,(us,"umu_m3_chgcirang()"));

	ud_lgeo(UU_TRUE, UD_circle);
	um_dl_pldas(UD_DASPCKLOC, /*Pick arc near end to be moved:*/UM_MODEL, 
					163, &pick, 1, &numint, 1);
	if (numint <= 0) goto done;

	/* retrieve entity info */
	c1.key = um_get_pickkey(&pick.pent, 1);
	um_get_all_geom(&c1, sizeof(c1));

	/* allow movement only of arc ends */
	if (fabs(c1.dang - UM_TWOPI) < UM_FUZZ)
		{
		/* No end point; break circle first. */
		uu_uerror0(UM_MODEL,134);
		goto done;
		}

	/*-------------------------------------------------------------
	**
	**	Find which end of arc is to be moved
	*/
	/* save off label, key, subscr for later */
	strncpy(label,c1.label, NCL_MAX_LABEL);
	subscr = c1.subscr;
	key = c1.key;

	/* compute end points of arc */
	um_get_endpts(&c1, UM_idmat, startpt, endpt);
	um_vcmnvc(endpt, c1.center, endvec);
	um_unitvc(endvec, endvec);

	/* project picked location onto circle plane along view normal */
	status = um_projploctopln(&pick.ploc, c1.center, c1.nvec, pickpt);

	if (status != 0)
		{
		/* Arc is parallel to view normal, i.e. its normal is 
			perpendicular to view normal */
		uu_uerror0(UM_MODEL, 135);
		goto done;
		}

	/* if startpt is closer, switch ends */
	if (um_closept(startpt, endpt, pickpt) == 1)
		{
		um_vctovc(endvec, c1.svec);
		c1.dang = -c1.dang;
		}

	/* get pt to move endpoint to */
	ud_ldas(UD_DASANGLE, /*new angle: */UM_MODEL, 112, &newangle, 1, &numint, UD_NODEFAULT);
	if (numint <= 0) goto done;

	newangle = fabs(newangle);
	if (newangle > UM_TWOPI)
		{
		/* resulting angle too large */
		uu_uerror0(UM_MODEL, 274);
		goto done;
		}
	else if (newangle < UM_FUZZ)
		{
		/* resulting angle too small */
		uu_uerror0(UM_MODEL, 55);
		goto done;
		}

	if (c1.dang < 0.0) c1.dang = -newangle; else  c1.dang = newangle ;

	/*reset label and subscr */
	strncpy(c1.label,label, NCL_MAX_LABEL);
	c1.subscr = subscr;
	c1.key = key;

	um_update_geom(&c1, UM_DEFAULT_TF);
	uc_display(&c1);

done:
	ud_lgeo(UU_FALSE, UD_circle);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_m3_closecir(key)
**       Close a circular arc.  If KEY is UU_NULL the user is prompted
**			to pickan arc, otherwise, KEY  is assumed to be a cirle. 
**    PARAMETERS   
**       INPUT  : 
**          key						UU_NULL => prompt user to pick circle,
**											otherwise, this is key of circle to 
**											close
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_m3_closecir(key)
	UU_KEY_ID key;

	{
	int			numint;					/* the number of interactions input
													through das */
	struct	UM_circle_rec	c1;		/* the first line entity */
	UM_PICKENT	pent;						/* pick path to picked entity */

	uu_denter( UU_MTRC,(us,"umu_m3_closecir(key=%x)",key));

	if (key != 0)
		{
		c1.key = key;
		}
	else
		{
		ud_lgeo(UU_TRUE, UD_circle);
		um_dl_pdas(UD_DASPICK, /*Pick circle to be closed:*/UM_MODEL, 111, &pent, 
				 1, &numint,UD_NODEFAULT);
		if (numint <= 0) goto done;
		c1.key = um_get_pickkey(&pent, 1);
		}
	/* retrieve geometry information */
	um_get_all_geom(&c1, sizeof(c1));

	c1.dang = UM_TWOPI;
	um_update_geom(&c1, UM_DEFAULT_TF);
	uc_display(&c1);

done:
	ud_lgeo(UU_FALSE, UD_circle);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_m3_modcircomplement(key)
**			Change the angle of the specified circle to define the 
**			complementary arc and parameterized in the same direction.
**
**    PARAMETERS   
**       INPUT  : 
**          key				key of circle
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_m3_modcircomplement(key)
	UU_KEY_ID key;

	{
	struct UM_circle_rec	c1;
	UM_transf rottf;

	uu_denter(UU_MTRC,(us,"umu_m3_modcircomplement(key=%x)", key));

	/* retrieve entity info */
	c1.key = key;
	um_get_all_geom(&c1, sizeof(c1));

	/* complementary arc defined only for arcs */
	if (fabs(c1.dang - UM_TWOPI) < UM_FUZZ)
		{
		/* Not an arc; break circle first. */
		uu_uerror0(UM_MODEL,134);
		goto done;
		}

	/* redefine the start vector and the delta angle */
	um_rotlntf(c1.center, c1.nvec, c1.dang, rottf);
	um_vctmtf(c1.svec, rottf, c1.svec);
	um_unitvc(c1.svec, c1.svec);
	if (c1.dang < 0.0)
		 c1.dang = -UM_TWOPI - c1.dang;
	else
		 c1.dang =  UM_TWOPI - c1.dang;

	/* update UNIBASE and DIGS */
	um_update_geom(&c1, UM_DEFAULT_TF);
	uc_display(&c1);

done:
	uu_dexit;
	}
