/*********************************************************************
**    NAME         :  neclass.c
**       CONTAINS: class dispatchers for NCL entities
**			int ncl_draft_type(rel_num, type)
**			int ncl_get_line_data(key, spt, ept)
**			int ncl_get_circle_data(key, center, radius, dang, normal, spt, ept)
**			int ncl_d_endpts(level, pickpath, pickloc, rel_num, cpt, opt)
**			int ncl_near_on_entity(e, pickloc, pt)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nclclass.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:22
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mfort.h"
#include "mdebug.h"
#include "mdmatrix.h"
#include "mcrv.h"
#include "modef.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mdeval.h"
#include "go.h"
#include "dasnog.h"
#include "mdclass.h"
#include "mdpick.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "adraft.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_draft_type(rel_num, type)
**       Call the viewing system to display an NCL entity.
**    PARAMETERS   
**       INPUT  : 
**          rel_num						NCL entity relation number
**       OUTPUT :  
**          type                    drafting type
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			The only data which must be initialized in the entity 
**			are the UNIBASE key and relation number.
*********************************************************************/
int
ncl_draft_type(rel_num, type)
	int rel_num;
	int *type;

	{
	int status;
	int kind;

	uu_denter(UU_MTRC,(us,"ncl_draft_type(rel_num=%d)", rel_num));
	status = UU_SUCCESS;
	switch(rel_num)
		{
		case  NCL_POINT_REL:
				kind = UA_DRAFT_POINT;
				break;
		case  NCL_LINE_REL:
				kind = UA_DRAFT_LINE;
				break;
		case  NCL_CIRCLE_REL:
				kind = UA_DRAFT_ARC;
				break;
		case  NCL_CURVE_REL:
				kind = UA_DRAFT_CURVE;
				break;
		default:
				kind = UA_DRAFT_UNKNOWN;
				status = UU_FAILURE;
				break;
		}
	*type = kind;
	uu_dexit;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : int ncl_get_circle_data(key, center, radius, dtheta,
								normal, start_pt, end_pt)
**       Return the center, radius, and normal for an NCL circle/arc having
**			the specified key.
**    PARAMETERS   
**       INPUT  : 
**          key							key of entity in UNIBASE
**       OUTPUT :  
**          center						center of circle
**          radius						radius of circle
**          dtheta						sweep angle of circle
**          normal						normal of circle
**          s_pt							start point
**          e_pt							end point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_circle_data(key, center, radius, dtheta, normal, spt, ept, length)
	UU_KEY_ID  key;
	UM_coord center;
	UM_length *radius;
	UM_angle *dtheta;
	UM_vector normal;
	UM_coord spt;
	UM_coord ept;
	UM_length *length;

	{
	struct UC_entitydatabag e;
	struct UM_circle_rec c;
	int rel_num;
	int status;
	UM_transf tfmat;
	UU_REAL um_getarclen();

	uu_denter(UU_MTRC,(us,"ncl_get_circle_data(%d,,,)",key));
	e.key = key;
	if (ur_retrieve_data(&e, sizeof(e)) != 0)
		status = UU_FAILURE;
	else
		{
		if (e.rel_num != NCL_CIRCLE_REL) status = -1;
		else
			{
			ncl_nclci_to_unici(&e, &c);
			*radius = c.radius;
			*dtheta = c.dang;
			um_vctovc(c.center, center);
			um_vctovc(c.nvec, normal);
			um_tftotf(UM_idmat, tfmat);
			um_get_endpts(&c, tfmat, spt, ept);
			*length = um_getarclen(&c, tfmat);
			status = UU_SUCCESS;
			}
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int ncl_get_line_data(key, spt, ept)
**       Return end points of the line specified by key
**    PARAMETERS   
**       INPUT  : 
**          key							key of entity in UNIBASE
**       OUTPUT :  
**          spt							start point
**          ept							end point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_line_data(key, spt, ept)
	UU_KEY_ID  key;
	UM_coord spt;
	UM_coord ept;

	{
	struct UC_entitydatabag e;
	struct UM_line_rec l;
	int rel_num;
	int status;
	UM_transf tfmat;

	uu_denter(UU_MTRC,(us,"ncl_get_line_data(%d,,,)",key));
	e.key = key;
	if (ur_retrieve_data(&e, sizeof(e)) != 0)
		status = UU_FAILURE;
	else
		{
		if (e.rel_num != NCL_LINE_REL) status = -1;
		else
			{
			ncl_nclln_to_uniln(&e, &l);
			um_tftotf(UM_idmat, tfmat);
			um_get_endpts(&l, tfmat, spt, ept);
			status = UU_SUCCESS;
			}
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int ncl_d_endpts(level,pickpath,pickloc,
**													rel_num,cpt,opt)
**      Get the cartesian  coordinates of the end points of an NCL
**      curve. The closest point to the pick location is the
**	   	first point.
**    PARAMETERS   
**       INPUT  : 
**				levl							level of entity picked
**				pickpath						pick path 
**				pickloc                 pick location
**       OUTPUT :  
**				rel_num						relation number of entity picked
**				cpt							closest end point to pick
**				opt							other end point
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_d_endpts(level,pickpath,pickloc,rel_num,cpt,opt)
	int level;
	UD_PPICKREC *pickpath;
	UD_NDCLOCREC *pickloc;
	int *rel_num;
	UM_coord cpt;
	UM_coord opt;

	{
	static struct UC_entitydatabag e;				/* data for picked entity */
	static struct UM_entitydatabag unientity;	/* UNICAD representation */
	UM_transf tfmat;							/* transformation matrix */
	UM_coord spt[2], ept[2];
	UM_coord c_spt, c_ept;
	UU_REAL distspt, distept;
	UU_REAL um_dploccc();
	UU_KEY_ID um_get_pickkey();
	UM_PICKENT pent;
	int status;
	int kind;

	uu_denter(UU_MTRC,(us,"ncl_d_endpts(level=%d,pickpath=%x,pickloc=%d)",
		level,pickpath,pickloc));

	status = um_d_pickresolve(pickpath, level, &pent);
	if (status == 0)
		{
		e.key = um_get_pickkey(&pent, level);
		ur_retrieve_data_relnum(e.key, &e.rel_num);
		switch(e.rel_num)
			{
			case NCL_POINT_REL:
			case NCL_LINE_REL:
			case NCL_CIRCLE_REL:
				if (ur_retrieve_data(&e, sizeof(e)) != 0)
					status = UU_FAILURE;
				else
					um_get_transformation(e.key, tfmat);
					*rel_num = e.rel_num;
					switch(e.rel_num)
						{
						case NCL_POINT_REL:
							ncl_nclpt_to_unipt(&e, &unientity);
							break;
						case NCL_LINE_REL:
							ncl_nclln_to_uniln(&e, &unientity);
							break;
						case NCL_CIRCLE_REL:
							ncl_nclci_to_unici(&e, &unientity);
							break;
						}
					status = um_n1_nrendpt(&unientity,tfmat,pickloc,cpt,opt);
					break;
			case NCL_CURVE_REL:
				cvepts(&e.key, spt, ept);
				um_vctovc(ept[0],c_ept); 
				um_vctovc(spt[0],c_spt); 
				distspt = um_dploccc(pickloc,c_spt);
				distept = um_dploccc(pickloc,c_ept);
				if (distspt > distept)
					{
					um_vctovc(ept[0],cpt); 
					um_vctovc(spt[0],opt); 
					}
				else 
					{
					um_vctovc(spt[0],cpt);
					um_vctovc(ept[0],opt);
					}
				break;
			default:
				status = -1;
				break;
			}
		}
	if (status != 0)
		uu_uerror0(/*Selected entity not a curve*/ UM_MODEL,12);

	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     :  ncl_near_on_entity(e,pickloc,pt)
**      Find point on entity key nearest the curser location  in pickloc
**		and return the point in pt.
**    PARAMETERS   
**       INPUT  : 
**          e				entity (key & rel_num filled in)
**          pickloc			pick location(ndc) part of plocrec
**       OUTPUT :  
**          pt				returned point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_near_on_entity(e,tfmat, pickloc,pt)
	struct UC_entitydatabag *e;
	UD_NDCLOCREC *pickloc;			/* ndc portion of plocrec */
	UM_coord pt;				/* returned model coord point */
	UM_transf tfmat;
	{
	UM_vector v1,v2,vpnorm;
	UM_coord pv_cc,pv_spt,pv_ept;
	UU_REAL cc[3],ptc[2][3],cdot;
	int nint;
	UU_REAL um_dot();

	/*---------------------------------------------------------------------
	**  Start of Executable Code
	**--------------------------------------------------------------------*/

	/* get entity data */

	ur_retrieve_data(e, sizeof(*e));
	um_get_transformation(e->key,tfmat);

	/* switch on type */

	switch(e->rel_num)
		{
		case NCL_POINT_REL:
			{
			struct NCL_nclpt_rec *ptr;
			ptr = (struct NCL_nclpt_rec *) e;
			um_cctmtf(ptr->pt, tfmat, pt);
			break;
			}
		case NCL_LINE_REL:
			{
			struct UM_line_rec l;

			ncl_nclln_to_uniln(e, &l);
/*    NCL entities don't use transform (?). If they did, need to call */
/*    uc_transform(&l,tfmat,UU_FALSE);                 ijd 1-feb-89   */
/*			um_transform_geom(&l,tfmat,UU_FALSE); */

			/* convert location from ndc to world */
			uv_ndctocc(pickloc->cord,cc,pickloc->transform);

			/* project loc and line endpoints onto view plane */
			uv_projvpln(cc,pv_cc,pickloc->transform);
			uv_projvpln(l.spt,pv_spt,pickloc->transform);
			uv_projvpln(l.ept,pv_ept,pickloc->transform);

			/* create vectors between loc and line */
			um_vcmnvc(pv_cc,pv_spt,v1);
			um_vcmnvc(pv_ept,pv_spt,v2);
			um_unitvc(v2,v2);

			/* calculate closest point on projected line */
			cdot = um_dot(v1,v2);
			um_vctmsc(v2,cdot,v2);
			um_vcplvc(l.spt,v2,pt);

			/* now project back onto original line */
			um_vcmnvc(l.ept,l.spt,v2);
			um_unitvc(v2,v2);
			um_vpnorm(pickloc->transform,vpnorm);
			um_ilnln(l.spt,v2,pt,vpnorm,&nint,cc);
			um_vctovc(cc,pt);
			break;
			}
		case NCL_CIRCLE_REL:
			{
			struct UM_circle_rec c;

			ncl_nclci_to_unici(e, &c);

			/* convert location from ndc to world */
			uv_ndctocc(pickloc->cord,cc,pickloc->transform);

			/* project point onto the plane of the circle */
			um_vpnorm(pickloc->transform,vpnorm);
			um_ilnpln(cc,vpnorm,c.center,c.nvec,&nint,pv_cc);

			/* form line with projected point and circle center */
			um_vcmnvc(pv_cc,c.center,v1);
			um_unitvc(v1,v1);

			/* intersect line and circle */
			um_ilncir(c.center,v1,c.center,c.nvec,c.radius,
						&nint,ptc);

			/* return intersection point */
			um_vctovc(ptc[0], pt);
			break;
			}

		}
	}
/*********************************************************************
**    E_FUNCTION     : int ncl_get_endpts(key,spt,ept,ierr)
**      Get the cartesian  coordinates of the end points of a curve/
**      point vector.
**    PARAMETERS   
**       INPUT  : 
**				key 							Key of entity to evaluate.
**       OUTPUT :  
**				spt    						Starting point.
**				ept							Ending point.
**			   ierr							0 if no error.
**    RETURNS      : 
**				none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_endpts(key,spt,ept,ierr)
UM_int4 *key;
UM_real8 spt[],ept[];
UM_int2 *ierr;
{
	struct UM_entitydatabag e;
	UM_transf tfmat;
	int status;
/*
.....Get the entities end points
*/
	e.key = *key;
	status = ncl_retrieve_data_fixed(&e);
	if (status != UU_SUCCESS) goto failed;
	status = uc_retrieve_transf(e.key, tfmat);
	if (status != UU_SUCCESS) goto failed;
	status = um_get_endpts(&e,tfmat,spt,ept);
	if (status != UU_SUCCESS) goto failed;
	*ierr = 0;
	goto done;
/*
.....Failure
*/
failed:;
	*ierr = 1;
done:;
	return;
}
