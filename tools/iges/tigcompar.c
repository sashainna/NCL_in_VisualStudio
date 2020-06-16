/*********************************************************************
**    NAME         :  tigcompar.c
**       CONTAINS:  Routines to compare iges translated entities
**                  with those in the secondary unibase to see if
**                  they are the same and should be labeled the
**                  same.  The routines that start with uig_match
**                  look for exact matches.  The routines that
**                  start with uig_comp try and match remaining
**                  geometries with something close to it.
**
**          uig_comp_point(eptr,num)
**          uig_comp_line(eptr,num)
**          uig_comp_circle(eptr,num)
**          uig_comp_plane(eptr,num)
**          uig_comp_conic(eptr,num)
**          uig_comp_polyline(eptr,num)
**          uig_comp_patern(eptr,num)
**          uig_comp_pointvec(eptr,num)
**          uig_comp_rbsplcrv(eptr,num)
**          uig_comp_uvcvonsf(eptr,num)
**          uig_comp_compcrv(eptr,num)
**          uig_comp_rbsplsrf(eptr,num)
**          uig_comp_meshsurf(eptr,num)
**          uig_comp_trimsrf(eptr,num)
**          uig_comp_revsf(eptr,num)
**          uig_secondary_unmatched()
**			uig_create_sec_unmatch()
**			uig_creat_sec_pt(eptr)
**			uig_reg_ent(chkey,factor,pos)
**
**    COPYRIGHT 1999 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			tigcompar.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			10/27/16 , 14:47:49
*********************************************************************/
#include "tiges.h"
#include "tigdefs.h"
#include "tigsupp.h"
#include "umath.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdattr.h"
#include "mdrel.h"
#include "mlab.h"
#include "mdeval.h"
#include "mxxx.h"
#include "modef.h"
#include "mgeom.h"
#include "udebug.h"
#include "usysdef.h"
#include "rbase.h"
#include "nccs.h" 
#include "ncl.h" 
#include "nclfc.h" 
#include "ag_incl.h"
#include "ag_global.h"
#include "nclfc.h"
#include "tigglobal.h"

#define IGES_LARGE (UU_REAL) 1.0e+5
extern int tig_max_ptlab;
extern int tig_max_lnlab;
extern int tig_max_cilab;
extern int tig_max_pllab;
extern int tig_max_cnlab;
extern int tig_max_pnlab;
extern int tig_max_pvlab;

extern int uig_ev13_uvcvonsf();
extern int uc_evcrv();
extern int uc_evsrf();
extern int ncl_ev7_nclcrv();
extern int ncl_retrieve_data_fixed();
extern UU_LOGICAL um_cceqcc();
extern UU_LOGICAL um_cceqcc_tol();
extern UU_REAL um_dcccc();
extern int um_vctovc();
extern int um_unitvc();
extern int um_vctovc_2d();
extern int um_ev4_conic();
extern int um_ev5_compcrv();
extern int um_isect_boxes();
extern int um_pre_srf_bndr_box();
extern int um_3dbox_around();
extern int um_translate_point();
extern void ur_getu_second();
extern void ur_getu_work();
extern int ur_get_next_key();
extern int ur_get_next_data_key();
extern int ur_update_data_fixed();
extern int ur_retrieve_transf();
extern char* uu_malloc();
extern void uu_free();
extern void uu_list_init();
extern void uu_list_push();
extern void uu_list_free();

UU_REAL uig_isect_area();
UU_REAL empty2 = 0.;

/*********************************************************************
**    I_FUNCTION     :  uig_comp_point(eptr,num)
**       Compare translated point data with existing point data
**       to see if there is a point that is close to it. These
**       routines are called after all exact matches have been made.
**    PARAMETERS
**       INPUT  :
**          eptr                   point record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_point(eptr,num)
struct UM_point_rec *eptr;
int num;
{
	struct UM_point_rec check,close,prev;
	int cnt, pos,i=0,status,next_tupleid;
	UU_REAL dist_low,check_dist;
	UU_KEY_ID matchkey,savematch;

	status = UU_FAILURE;
	next_tupleid = 1;
	dist_low = IGES_LARGE;
	matchkey = eptr->key;
	savematch = 0;
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve points from the unibase.
*/
	while(ur_get_next_data_key(UM_POINT_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't bother to compare any entities that have @UN as their
.....label, it is either an underlying entity, which translated
.....underlying entities have already been labeled and won't enter
.....this routine, or it has been match already.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
/*
.....Find the closest point.
*/
		else 
		{
			check_dist = um_dcccc(eptr->pt,check.pt);
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
			cnt = uig_reg_ent(check.key,check_dist,&i);
			if(cnt) continue;
			if (check_dist < dist_low)
			{
/*
.....If this point is closer than the previous closest
.....point, save the key number in close, and the key position in check keys.
*/
				pos =i;
				dist_low = check_dist;
				close.key = check.key;	
			}
		}
	}
	if (dist_low < IGES_LARGE)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = dist_low;
		}
		else
		{
/*
.....If the label is the standard ptXX, then we need to
.....keep track of the highest number used.  The remaining
.....points that were not matched will be labeled starting
.....at one higher than the highest.
*/
			uig_match_check_maxnum(&close, &tig_max_ptlab);
			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
.....We have used this point so mark it with
.....a label of @UN.
*/
			strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
		}
		status = UU_SUCCESS;
	}
/*
.....If we found a match decrease the number of mis-matched points.
*/
	if (!UIG_regressive && status == UU_SUCCESS)
		tig_unlabeled_ent[0] --;
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
/*
.....find another match for the entity which previously matched with check
*/
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_point(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_comp_line(eptr,num)
**       Compare translated line data with existing line data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   line record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_line(eptr,num)
struct UM_line_rec *eptr;
int num;
{
	struct UM_line_rec check,close,prev;
	int cnt,pos=0,status,i,next_tupleid;
	UM_vector vec1,vec2;
	UU_REAL dist_low, check_dist;
	UU_KEY_ID matchkey,savematch;

	matchkey = eptr->key;
	savematch = 0;
	status = UU_FAILURE;
	next_tupleid = 1;
	dist_low = IGES_LARGE;
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Get lines from unibase.
*/
	while(ur_get_next_data_key(UM_LINE_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't bother comparing eptr with entities that have @UN label
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
		else 
		{
			for (i=0;i<3; i++) vec1[i] = eptr->spt[i] - eptr->ept[i];
			for (i=0;i<3; i++) vec2[i] = check.spt[i] - check.ept[i];
/*
.....For num = 1 we want to make sure the lines has the same length
.....and the same direction.
*/
			if (num ==1)
			{
				if (fabs(um_mag(vec1) - um_mag(vec2)) < UM_FUZZ)
				{
					um_unitvc(vec1,vec1);
					um_unitvc(vec2,vec2);
					if (um_vcparall(vec1,vec2))
					{
						check_dist = um_dcccc(eptr->spt,check.spt);
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
						cnt = uig_reg_ent(check.key,check_dist,&i);
						if(cnt) continue;
						if (check_dist< dist_low)
						{
							pos =i;
							close.key = check.key;
							dist_low = check_dist;
						}
					}
				}
			}
/*
.....If num =2 just check to see that they have the same 
.....direction.
*/
			else if (num ==2)
			{
				um_unitvc(vec1,vec1);
				um_unitvc(vec2,vec2);
				if (um_vcparall(vec1,vec2))
				{
					check_dist = um_dcccc(eptr->spt,check.spt);
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
					cnt = uig_reg_ent(check.key,check_dist,&i);
					if(cnt) continue;
					if (check_dist< dist_low)
					{
						pos =i;
						close.key = check.key;
						dist_low = check_dist;
					}
				}
			}
/*
.....For anything else look for the line with the
.....closest starting point.
*/
			else
			{
				check_dist = um_dcccc(eptr->spt,check.spt);
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
				cnt = uig_reg_ent(check.key,check_dist,&i);
				if(cnt) continue;
				if (check_dist< dist_low)
				{
					pos =i;
					close.key = check.key;
					dist_low = check_dist;
				}
			}
		}
	}
	if (dist_low < IGES_LARGE)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = dist_low;
			
		}
		else
		{
			uig_match_check_maxnum(&close, &tig_max_lnlab);
			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
......We have used this line, so mark it with an @UN.
*/
			strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
		}
		status = UU_SUCCESS;
	}
	if(!UIG_regressive && status == UU_SUCCESS)
		tig_unlabeled_ent[1] --;
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_line(&prev,num);
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_comp_circle(eptr,num)
**       Compare translated circle data with existing circle data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   circle record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_circle(eptr,num)
struct UM_circle_rec *eptr;
int num;
{
	struct UM_circle_rec check,close,prev;
	int cnt,i,pos=0,status,next_tupleid;
	UU_REAL dist_low,check_dist, d1, d2;
	UM_vector evec;
	UM_transf rotmat;
	UU_KEY_ID matchkey,savematch;

	status = UU_FAILURE;
	next_tupleid = 1;
	dist_low = IGES_LARGE;
	matchkey = eptr->key;
	savematch = 0;
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
......Get circles from unibase.
*/
	while(ur_get_next_data_key(UM_CIRCLE_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....No need to compare eptr to entities with @UN labels.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
/*
.....Check if circle is reversed
*/
		else if (num == 1)
		{
			d1 = um_dot(eptr->nvec, check.nvec);
			if (um_cceqcc(eptr->center, check.center) &&
				fabs(eptr->radius - check.radius) < UM_FUZZ &&
				fabs(d1) > 0.9999 && 
				fabs(fabs(eptr->dang) - fabs(check.dang)) < UM_FUZZ)
				{
					d2 = eptr->dang * check.dang;
					if (d1 * d2 < 0.0)
					{
						um_rotlntf(eptr->center, eptr->nvec, eptr->dang, rotmat);
						um_vctmtf(eptr->svec, rotmat, evec);
					}
					else
					{
						um_vctovc(eptr->svec, evec);
					}
					if (um_dot(evec, check.svec) > 0.9999)
					{
						if (d1 * d2 < 0.0)
						{
							um_vctovc(evec, eptr->svec);
							eptr->dang = -eptr->dang;
						}
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
						cnt = uig_reg_ent(check.key,0.,&i);
						if(cnt) continue;
						pos =i;
						dist_low = 0.0;
						close.key = check.key;
						goto done;
					}
				}
		}
/*
.....Compare angles and normal vectors.
*/
		else if (fabs(eptr->dang - check.dang) < UM_FUZZ)
		{
			if (um_cceqcc(eptr->nvec,check.nvec))
			{
/*
.....For num = 2 we will allow same center point but different
.....radii.
*/
				if (num == 2)
				{
					if (um_cceqcc(eptr->center,check.center))
					{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
						cnt = uig_reg_ent(check.key,0.,&i);
						if(cnt) continue;
						dist_low = 0.0;
						pos =i;
						close.key = check.key;
						goto done;
					}
				}
/*
.....For num = 3, we will allow same radius but different
.....center points, find the one with the closest center point.
*/
				else if (num == 3)
				{
					if(fabs(eptr->radius - check.radius)<UM_FUZZ)
					{
						check_dist = um_dcccc(eptr->center,check.center);
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
.......Changed distance passed in so check_dist is actually considered - ASF 11/07/13.
*/
						cnt = uig_reg_ent(check.key,check_dist,&i);
						if(cnt) continue;
						if (check_dist < dist_low)
						{
							pos =i;
							close.key = check.key;
							dist_low = check_dist;
						}
					}
				}
/*
.....For all return visits, find the circle with the closest
.....center point. 
*/
				else
				{
					check_dist = um_dcccc(eptr->center,check.center);
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
.......Changed distance passed in so check_dist is actually considered - ASF 11/07/13.
*/
					cnt = uig_reg_ent(check.key,check_dist,&i);
					if(cnt) continue;
					if (check_dist < dist_low)
					{
						pos =i;
						close.key = check.key;
						dist_low = check_dist;
					}
				}
			}
		}
	}
/*
.....We found our match, now copy the label and if the
.....label is the standard ciXX keep track of highest XX.
*/
done:;
	if (dist_low < IGES_LARGE)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = dist_low;
		}
		else
		{
			uig_match_check_maxnum(&close, &tig_max_cilab);
			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
.....We have used this circle so mark it with an @UN.
*/
			strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
		}
		status = UU_SUCCESS;
	}
	if(!UIG_regressive && status == UU_SUCCESS)
		tig_unlabeled_ent[2] --;
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_circle(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_comp_plane(eptr,num)
**       Compare translated plane data with existing plane data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   plane record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_plane(eptr,num)
struct NCL_nclpl_rec *eptr;
int num;
{
	struct NCL_nclpl_rec check,close,prev;
	int cnt,pos=0,i ,status,next_tupleid;
	UU_REAL dist_low,check_dist;
	UU_KEY_ID matchkey,savematch;

	status = UU_FAILURE;
	next_tupleid = 1;
	dist_low = IGES_LARGE;
	matchkey = eptr->key;
	savematch = 0;
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve planes from the unibase.
*/
	while(ur_get_next_data_key(NCL_PLN_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....If check.label is @UN don't continue this comparision.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
/*
.....Compare the normal vectors of the planes.
*/
		else if (um_cceqcc(eptr->nvec,check.nvec))
		{
			check_dist = um_dcccc(eptr->pt,check.pt);
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
			cnt = uig_reg_ent(check.key,check_dist,&i);
			if(cnt) continue;
			if (check_dist < dist_low)
			{
/*
.....If this point is closer than the previous closest
.....point, save the key number in close.
*/
				pos =i;
				dist_low = check_dist;
				close.key = check.key;
			}
		}
	}
	if (dist_low < IGES_LARGE)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = dist_low;
		}
		else
		{
			uig_match_check_maxnum(&close, &tig_max_pllab);
			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
.....We have used the plane so label it with @UN.
*/
			strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
		}
		status = UU_SUCCESS;
	}
	if(!UIG_regressive && status == UU_SUCCESS)
		tig_unlabeled_ent[3] --;
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_plane(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_comp_conic(eptr,num)
**       Compare translated conic data with existing conic data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   conic record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_conic(eptr,num)
struct UM_conic_rec *eptr;
int num;
{
	struct UM_conic_rec check,close,prev;
	struct UM_evcrvout evout1, evout2;
	struct UM_transf_rec trans1,trans2;
	int cnt,status,next_tupleid,i,pos=0;
	UM_3D_box box1, box2;
	UM_coord *pts1, *pts2,trans_pt;
	UM_vector vector;
	UU_LIST points1, points2;
	UU_REAL uparm,tol,um_dcccc();
	UU_REAL bwdth1,bwdth2,blen1,blen2,bhgt1,bhgt2,dist_low;
	UU_KEY_ID matchkey,savematch;

	matchkey = eptr->key;
	savematch = 0;

	uu_list_init(&points1, sizeof(UM_coord),25,25);
	uu_list_init(&points2, sizeof(UM_coord),25,25);
	status = UU_FAILURE;
	next_tupleid = 1;
	if (num <=2)
		tol = UM_FUZZ;
	else if (num == 3)
		tol = .001;
	else
		tol = .005;

	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);

/*
.....Evaluate eptr at 25 points and put those points
.....into pts1, then calculate the box that surrounds the curve
.....and determine the width, height, and length of the box.
*/
	for (i=0;i<=24;i++)
	{
		uparm = i/24.0;
		um_ev4_conic(UM_POINT,uparm,eptr,trans1.tfmat,&evout1);
		uu_list_push(&points1,evout1.cp);
	}
	pts1 = (UM_coord *)UU_LIST_ARRAY(&points1);
	status = um_3dbox_around(25,pts1,&box1);
	bwdth1 = um_dcccc(box1.ver[0],box1.ver[1]);
	bhgt1 = um_dcccc(box1.ver[0],box1.ver[2]);
	blen1 = um_dcccc(box1.ver[0],box1.ver[4]);
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve conics from the unibase.
*/
	while(ur_get_next_data_key(UM_CONIC_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't bother comparing entity that has @UN for its label.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
/*
.....Compare the conic data, first make sure they
.....are the same type of conic.
*/
		else if(eptr->type == check.type)
		{
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
			for (i=0; i<=24; i++)
			{
				uparm = i/24.0;
				um_ev4_conic(UM_POINT,uparm,&check,trans2.tfmat,&evout2);
				uu_list_push(&points2,evout2.cp);
			}
			pts2 = (UM_coord *)UU_LIST_ARRAY(&points2);
			status = um_3dbox_around(25,pts2,&box2);
			bwdth2 = um_dcccc(box2.ver[0],box2.ver[1]);
			bhgt2 = um_dcccc(box2.ver[0],box2.ver[2]);
			blen2 = um_dcccc(box2.ver[0],box2.ver[4]);
			status = UU_FAILURE;
			pts1 = (UM_coord *)UU_LIST_ARRAY(&points1);
/*
.....Calculate the vector that goes from pts2 to pts1
*/
			for (i=0;i<3;i++) vector[i] = pts1[0][i] - pts2[0][i];
/*
.....Determine if the box is the same size as the box surrounding eptr,
.....Or if num is greater than two compare the ratios of the box to
.....see if it might be a scaled version of the curve.
*/
			if ((fabs(bwdth1-bwdth2) < tol) &&
				(fabs(blen1-blen2) < tol) &&(fabs(bhgt1-bhgt2) < tol))
			{
				status = UU_SUCCESS;
/*
.....Translate the points in pts2, by the vector and then
.....see if the points match the points in pts1, if all the
.....points match, it is the same curve, just translated to
.....a new location.
*/
				if (num == 1)
				{
					for(i=0;i<=24 && status == UU_SUCCESS;i++)
					{
						um_translate_point(pts2,1.0,vector,trans_pt);
						if (!um_cceqcc(trans_pt,pts1))
							status = UU_FAILURE;
						pts1++;
						pts2++;
					}
				}
/*
.....Just check endpoints
*/
				else 
				{
					um_translate_point(pts2,1.0,vector,trans_pt);
					if (!um_cceqcc(trans_pt,pts1))
						status = UU_FAILURE;
					um_translate_point(&pts2[24],1.0,vector,trans_pt);
					if (!um_cceqcc(trans_pt,&pts1[24]))
						status = UU_FAILURE;
				}
					
			}
			else
			{
				um_translate_point(pts2,1.0,vector,trans_pt);
				if (!um_cceqcc(trans_pt,pts1))
					status = UU_FAILURE;
				um_translate_point(&pts2[24],1.0,vector,trans_pt);
				if (!um_cceqcc(trans_pt,&pts1[24]))
					status = UU_FAILURE;
			}
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
			if (status == UU_SUCCESS)
			{
				cnt = uig_reg_ent(check.key,0.,&i);
				if(cnt) continue;
			}
			if (status == UU_SUCCESS)
			{
				pos =i;
				dist_low =0.0;
				close.key = check.key;
				goto done;
			}
			uu_list_free(&points2);
			uu_list_init(&points2, sizeof(UM_coord),25,25);
		}
	}
	status = UU_FAILURE;
done:;
	if (status == UU_SUCCESS)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = dist_low;
		}	
		else
		{
			if (close.label[1] == 'N')
				uig_match_check_maxnum(&close, &tig_max_cnlab);
			else if (close.label[1] == 'V')
				uig_match_check_maxnum(&close, &tig_max_cvlab);

			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
.....We have used this conic so label it with @UN.
*/
			strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
			tig_unlabeled_ent[4] --;
		}
	status = UU_SUCCESS;
	}

/*
.....Switch back to primary unibase.
*/	uu_list_free(&points1);
	uu_list_free(&points2);
	ur_getu_work();
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_conic(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_comp_polyline(eptr,num)
**       Compare translated polyline data with existing polyline data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   polyline record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_polyline(eptr,num)
struct UM_polyline_rec *eptr;
int num;
{
	struct UM_polyline_rec check,prev;
	int cnt,status,next_tupleid,i,pos=0;
	UU_REAL *pts1, *pts2,trans_pt[3];
	UM_vector vec;
	UU_KEY_ID matchkey,savematch;

	matchkey = eptr->key;
	savematch = 0;

	status = UU_FAILURE;
	next_tupleid = 1;

	pts1 = (UU_REAL *)uu_malloc(3*eptr->no_pt*sizeof(*pts1));
	pts2 = (UU_REAL *)uu_malloc(3*eptr->no_pt*sizeof(*pts2));
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve polyline from the unibase.
*/
	while(ur_get_next_data_key(UM_POLYLINE_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't compare entities if check.label is @UN.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
/*
.....Compare the number of points.
*/
		else if (eptr->no_pt == check.no_pt)
		{
/*
.....If num is greater than 1, then having the same number
.....of points is good enough.
*/
			if (num > 1)
				status = UU_SUCCESS;
			else
			{
/*
.....Compare the points, if they are the same points
.....just moved then we have a match.
*/
				status = UU_SUCCESS;
				pts1 = eptr->pt;
				pts2 = check.pt;
/*
.....Determine the vector that moves the first point
.....of pts1 to the first point of pts2. 
*/
				for (i=0;i<3;i++) vec[i] = pts2[i] - pts1[i];
				i = 0;
				while(i<eptr->no_pt&& status == UU_SUCCESS)
				{
/*
.....If all the points in the polyline are translated
.....by the same vector, then we have a match.
*/
					um_translate_point(pts1,1.0,vec,trans_pt);
					if(!um_cceqcc(trans_pt,pts2))
						status = UU_FAILURE;
					pts1+=3;
					pts2+=3;
					i++;
				}
			}
			if (status == UU_SUCCESS)
			{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
				cnt = uig_reg_ent(check.key,0.0,&i);
				if(cnt) continue;
				pos =i;
			}
			if (!UIG_regressive && status == UU_SUCCESS)
			{
/*
.....Found a match, so copy label into eptr, and check to see
.....if the label is the standard pnXX and keep track of 
.....highest XX.
*/
				uig_match_check_maxnum(&check, &tig_max_pnlab);
				strcpy(eptr->label,check.label);
				eptr->subscr = check.subscr;
/*
.....Set check.label to @UN so we know not to use it again.
*/
				strcpy(check.label,"@UN");
				check.subscr = 0;
				ur_update_data_fixed(&check);
				goto done;
			}
		}
	}
done:;
	if (status == UU_SUCCESS)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&check);
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = 0.0;
		}
		else
			tig_unlabeled_ent[5] --;
		status = UU_SUCCESS;
	}
	uu_free(pts1);
	uu_free(pts2);
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_polyline(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_comp_patern(eptr,num)
**       Compare translated patern data with existing patern data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   patern record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_patern(eptr,num)
struct NCL_patern_rec *eptr;
int num;
{
	struct NCL_patern_rec check,prev;
	int cnt,pos=0,status,next_tupleid,i,numpts;
	UM_vector vec;
	UU_REAL *pts1,*pts2,trans_pt[3];
	UU_KEY_ID matchkey,savematch;

	matchkey = eptr->key;
	savematch = 0;

	status = UU_FAILURE;
	next_tupleid = 1;
	pts1 = (UU_REAL *)uu_malloc(eptr->no_patpnt*sizeof(*pts1));
	pts2 = (UU_REAL *)uu_malloc(eptr->no_patpnt*sizeof(*pts2));
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve paterns from the unibase.
*/
	while(ur_get_next_data_key(NCL_PATERN_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't compare entities that have @UN labels.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
/*
.....Compare the patern data.
*/
		else if (eptr->markertype == check.markertype)
		{
/*
.....Compare if these are points or point vectors.
*/
			if(eptr->pntype == check.pntype)
			{
				numpts = check.no_patpnt/3.0;
					
				if(eptr->no_patpnt == check.no_patpnt)
				{
					if (num > 1)
						status = UU_SUCCESS;
					else
					{
						pts1 = eptr->patpnt;
						pts2 = check.patpnt;
/*
.....Create a vector that maps the first point in pts1 to
.....the first point in pts2.
*/
						for (i=0;i<3;i++) vec[i] = pts2[i] - pts1[i];
	
						status = UU_SUCCESS;
						i = 0;
/*
......Compare the point data.
*/
						while(i<numpts&& status == UU_SUCCESS)
						{
							um_translate_point(pts1,1.0,vec,trans_pt);
							if(!um_cceqcc(trans_pt,pts2))
								status = UU_FAILURE;
							pts1 +=3;
							pts2 +=3;
							i++;
						}
					}
/*
.....A match was found so copy the label into eptr and check
.....to see if the standard label pnXX was being used, if so
.....keep track of the highest XX.
*/
					if (status == UU_SUCCESS)
					{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
						cnt = uig_reg_ent(check.key,0.,&i);
						if(cnt) continue;
						pos =i;
					}
/*
.....A match was found so copy the label into eptr and check
.....to see if the standard label pnXX was being used, if so
.....keep track of the highest XX.
*/
					if (!UIG_regressive && status == UU_SUCCESS)
					{
						uig_match_check_maxnum(&check, &tig_max_pnlab);

						strcpy(eptr->label,check.label);
						eptr->subscr = check.subscr;
/*
.....Update check.label with @UN so we know that it has
.....been matched already.
*/
						strcpy(check.label,"@UN");
						check.subscr = 0;
						ur_update_data_fixed(&check);
						goto done;
					}
				}
			}
		}
	}
done:;
	if (status == UU_SUCCESS)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&check);
		if (UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = 0.0;
		}
		else
			tig_unlabeled_ent[5] --;
		status = UU_SUCCESS;
	}
	uu_free(pts1);
	uu_free(pts2);
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
	if (UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_patern(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_comp_pointvec(eptr,num)
**       Compare translated pointvec data with existing pointvec data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   pointvec record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_pointvec(eptr,num)
struct NCL_nclpv_rec *eptr;
int num;
{
	struct NCL_nclpv_rec check,close,prev;
	int cnt,i,pos=0,status,next_tupleid;
	UM_vector eptr_vec, check_vec;
	UU_REAL dist_low,check_dist;
	UU_KEY_ID matchkey,savematch;

	matchkey = eptr->key;
	savematch = 0;

	status = UU_FAILURE;
	next_tupleid = 1;
	dist_low = IGES_LARGE;
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve point-vectors from the unibase.
*/
	while(ur_get_next_data_key(NCL_POINTVEC_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't bother comparing entities that have @UN labels.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
		else
		{

/*
.....If num is greater than one, then unitize the vector and
.....just make sure the vectors are going in the same direction.
.....Otherwise just copy the vectors for comparison.
*/
			if (num > 1)
			{
				um_unitvc(eptr->ve,eptr_vec);
				um_unitvc(check.ve,check_vec);
			}
			else
			{
				um_vctovc(eptr->ve,eptr_vec);
				um_vctovc(check.ve,check_vec);
			}
/*
.....Compare the vector data.
*/
			if(um_cceqcc(eptr_vec,check_vec))
			{
/*
.....Find the pointvector with the same vector and the closest
.....point.
*/
				check_dist = um_dcccc(eptr->pt, check.pt);
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
				cnt = uig_reg_ent(check.key,check_dist,&i);
				if(cnt) continue;
				if (check_dist < dist_low)
				{
					pos =i;
					close.key = check.key;
					dist_low = check_dist;
				}
			}
		}
	}
	if (dist_low < IGES_LARGE)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = dist_low;
		}
		else
		{
			uig_match_check_maxnum(&close, &tig_max_pvlab);
			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
.....Update close.label with an @UN label to indicate 
.....it has been used already.
*/
				strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
		}
		status = UU_SUCCESS;
	}
	if(!UIG_regressive & status == UU_SUCCESS)
		tig_unlabeled_ent[6] --;
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
/*
.....find another match for the entity which previously matched with check
*/
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_pointvec(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_comp_rbsplcrv(eptr,num)
**       Compare translated curve data with existing curve data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   curve record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_rbsplcrv(eptr,num)
struct UM_rbsplcrv_rec *eptr;
int num;
{
	struct UM_evcrvout evout1,evout2;
	struct UM_transf_rec trans1,trans2;
	struct NCL_fixed_databag close,check,prev;
	int cnt,pos=0,status,next_tupleid,i;
	UM_3D_box box1,box2;
	UM_coord *pts1,*pts2,trans_pt;
	UM_vector vector;
	UU_LIST points1,points2;
	UU_REAL dist_low,uparm,tol,blen1,blen2,bhgt1,bhgt2,bwdth1,bwdth2;
	UU_KEY_ID matchkey,savematch;

	matchkey = eptr->key;
	savematch = 0;
/*
.....Initialize
*/
	uu_list_init(&points1, sizeof(UM_coord),25,25);
	uu_list_init(&points2, sizeof(UM_coord),25,25);
	status = UU_FAILURE;
	next_tupleid = 1;
	if (num <=2)
		tol = UM_FUZZ;
	else if (num ==3)
		tol = .001;
	else
		tol = .005;
/*
.....Get the transformation matrix associated with this curve.
*/
	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);
/*
.....Evaluate eptr and put the list of points
.....into pts1, then calculate the box that surrounds the
.....curve and determine the width,height,and length of the
.....box.
*/
	for (i=0;i<=24;i++)
	{
		uparm = i/24.0;
		uc_evcrv(UM_POINT,uparm,eptr,trans1.tfmat,&evout1);
		uu_list_push(&points1,evout1.cp);
	}
	pts1 = (UM_coord *)UU_LIST_ARRAY(&points1);
	status = um_3dbox_around(25,pts1,&box1);
	bwdth1 = um_dcccc(box1.ver[0],box1.ver[1]);	
	bhgt1 = um_dcccc(box1.ver[0],box1.ver[2]);	
	blen1 = um_dcccc(box1.ver[0],box1.ver[4]);	
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve the curves from the unibase.
*/
	while(ur_get_next_data_key(UM_RBSPLCRV_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't bother comparing entities with @UN labels.
*/
		if (check.label[0] =='@')
			status = UU_FAILURE;
/*
.....Create a box around the curve and compare
.....boxes to see if it is the same curve, but in a different
.....location.
*/
		else
		{
/*
.....Get the transformation matrix associated with check
*/
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
			uparm = 0;
/*
.....Evaluate curve at 25 points and put those points into pts2
.....Then calculate the box that surrounds this curve and determine
.....the width,height,and length of the box
*/
			for (i=0;i<=24;i++)
			{
				uparm = i/24.0;
				uc_evcrv(UM_POINT,uparm,&check,trans2.tfmat,&evout2);
				uu_list_push(&points2,evout2.cp);
			}
			pts2 = (UM_coord *)UU_LIST_ARRAY(&points2);
			status = um_3dbox_around(25,pts2,&box2);
			bwdth2 = um_dcccc(box2.ver[0],box2.ver[1]);	
			bhgt2 = um_dcccc(box2.ver[0],box2.ver[2]);	
			blen2 = um_dcccc(box2.ver[0],box2.ver[4]);	
			status = UU_FAILURE;
			pts1 = (UM_coord *)UU_LIST_ARRAY(&points1);
/*
.....Calculate a vector that goes from pts2 to pts1.
*/
			for (i=0;i<3;i++) vector[i] = pts1[0][i] - pts2[0][i];
/*
.....Determine if the box is the same size as the box surrounding eptr,
.....Or if num is greater than two compare the ratios of the box to 
.....see if it might be a scaled version of the curve.
*/
			if ((fabs(bwdth1-bwdth2) < tol) &&
				(fabs(blen1-blen2) < tol) &&(fabs(bhgt1-bhgt2) < tol)) 
			{
				status = UU_SUCCESS;
/*
.....Translate the points in pts2, by the vector and then
.....see if the points match the points in pts1, if all the
.....points match, it is the same curve, just translated to
.....a new location.
*/
				if (num ==1)
				{
					for(i=0;i<=24 && status == UU_SUCCESS;i++)
					{
						um_translate_point(pts2,1.0,vector,trans_pt);
						if (!um_cceqcc(trans_pt,pts1))
							status = UU_FAILURE;
						pts1++;
						pts2++;
					}
				}
/*
.....Just check the end points.
*/
				else if (num >1)
				{
					um_translate_point(pts2,1.0,vector,trans_pt);
					if (!um_cceqcc_tol(trans_pt,pts1,tol))
						status = UU_FAILURE;
					else
					{
						um_translate_point(&pts2[24],1.0,vector,trans_pt);
						if (!um_cceqcc_tol(trans_pt,&pts1[24],tol))
							status = UU_FAILURE;
					}
				}
			}
			else if (num > 2)
			{
				status = UU_SUCCESS;
				um_translate_point(pts2,1.0,vector,trans_pt);
				if (!um_cceqcc_tol(trans_pt,pts1,tol))
					status = UU_FAILURE;
				else
				{
					um_translate_point(&pts2[24],1.0,vector,trans_pt);
					if (!um_cceqcc_tol(trans_pt,&pts1[24],tol))
						status = UU_FAILURE;
				}
			}
			if (status == UU_SUCCESS)
			{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
				cnt = uig_reg_ent(check.key,0.,&i);
				if(cnt) continue;
				dist_low = 0.;
				pos =i;
				close.key = check.key;
				goto done;
			}
			uu_list_free(&points2);
			uu_list_init(&points2, sizeof(UM_coord),25,25);
		}
	}
	next_tupleid = 1;
/*
.....If there are any NCL curves in the unibase we need to check
.....those also.
*/
	while(ur_get_next_data_key(NCL_CURVE_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't bother comparing entities with @UN labels.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
		else
		{
/*
.....Get the transformation matrix associated with check
*/
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
			uparm = 0;
/*
.....Evaluate curve at 25 points and put those points into pts2
.....Then calculate the box that surrounds this curve and determine
.....the width,height,and length of the box
*/
			for (i=0;i<=24;i++)
			{
				uparm = i/24.0;
				ncl_ev7_nclcrv(UM_POINT,uparm,&check,trans2.tfmat,&evout2);
				uu_list_push(&points2,evout2.cp);
			}
			pts2 = (UM_coord *)UU_LIST_ARRAY(&points2);
			status = um_3dbox_around(25,pts2,&box2);
			bwdth2 = um_dcccc(box2.ver[0],box2.ver[1]);	
			bhgt2 = um_dcccc(box2.ver[0],box2.ver[2]);	
			blen2 = um_dcccc(box2.ver[0],box2.ver[4]);	
			status = UU_FAILURE;
			pts1 = (UM_coord *)UU_LIST_ARRAY(&points1);
/*
.....Calculate a vector that goes from pts2 to pts1.
*/
			for (i=0;i<3;i++) vector[i] = pts1[0][i] - pts2[0][i];
/*
.....Determine if the box is the same size as the box surrounding eptr,
*/
			if ((fabs(bwdth1-bwdth2) < UM_FUZZ) && (fabs(blen1-blen2) < UM_FUZZ) && 
				(fabs(bhgt1-bhgt2) < UM_FUZZ)) 
			{
				status = UU_SUCCESS;
/*
.....Translate the points in pts2, by the vector and then
.....see if the points match the points in pts1, if all the
.....points match, it is the same curve, just translated to
.....a new location.
*/
				if (num ==1)
				{
					for(i=0;i<=24 && status == UU_SUCCESS;i++)
					{
						um_translate_point(pts2,1.0,vector,trans_pt);
						if (!um_cceqcc(trans_pt,pts1))
							status = UU_FAILURE;
						pts1++;
						pts2++;
					}
				}
/*
.....Just check the endpoints.
*/
				else if (num > 1)
				{
					um_translate_point(pts2,1.0,vector,trans_pt);
					if (!um_cceqcc(trans_pt,pts1))
						status = UU_FAILURE;
					else
					{
						um_translate_point(&pts2[24],1.0,vector,trans_pt);
						if (!um_cceqcc(trans_pt,&pts1[24]))
							status = UU_FAILURE;
					}
				}
			}
			else if (num >2)
			{
				status = UU_SUCCESS;
				um_translate_point(pts2,1.0,vector,trans_pt);
				if (!um_cceqcc_tol(trans_pt,pts1,tol))
					status = UU_FAILURE;
				else
				{
					um_translate_point(&pts2[24],1.0,vector,trans_pt);
					if (!um_cceqcc_tol(trans_pt,&pts1[24],tol))
						status = UU_FAILURE;
				}
			}
			if (status == UU_SUCCESS)
			{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
				cnt = uig_reg_ent(check.key,0.,&i);
				if(cnt) continue;
				dist_low = 0.;
				pos =i;
				close.key = check.key;
				goto done;
			}
			uu_list_free(&points2);
			uu_list_init(&points2, sizeof(UM_coord),25,25);
		}
	}
	status = UU_FAILURE;
/*
.....If all points matched, we found our curve.
*/
done:;
	if (status == UU_SUCCESS)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = dist_low;
		}
		else
		{
			uig_match_check_maxnum(&close, &tig_max_cvlab);
			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
.....Update close.label with @UN, since we found its match.
*/
			strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
		}
		status = UU_SUCCESS;
	}
	uu_list_free(&points1);
	uu_list_free(&points2);
	if (!UIG_regressive && status == UU_SUCCESS)
		tig_unlabeled_ent[7] --;
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
/*
.....find another match for the entity which previously matched with check
*/
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_rbsplcrv(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_comp_uvcvonsf(eptr,num)
**       Compare translated curve data with existing curve data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   curve record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_uvcvonsf(eptr,num)
struct UM_uvcvonsf_rec *eptr;
int num;
{
	struct UM_uvcvonsf_rec check,close,prev;
	struct UM_evcrvout evout1,evout2;
	struct UM_transf_rec trans1, trans2;
	int cnt,pos=0,status,next_tupleid,i;
	UM_3D_box box1,box2;
	UM_coord *pts1, *pts2,trans_pt;
	UM_vector vector;
	UU_LIST points1,points2;
	UU_REAL dist_low,uparm,blen1, blen2,bhgt1,bhgt2,bwdth1,bwdth2;
	UU_KEY_ID matchkey,savematch;

	matchkey = eptr->key;
	savematch = 0;
		
	uu_list_init(&points1, sizeof(UM_coord),25,25);
	uu_list_init(&points2, sizeof(UM_coord),25,25);
	status = UU_FAILURE;
	next_tupleid = 1;
/*
.....Get the transformation matrix associated with this curve.
*/
	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);
/*
.....Evaluate eptr at 25 points and put the list of points
.....into pts1, then calculate the box that surrounds the
.....curve and determine the width,height,and length of the
.....box.
*/
	for (i=0;i<=24;i++)
	{
		uparm = i/24.0;
		uig_ev13_uvcvonsf(UM_POINT,uparm,eptr,trans1.tfmat,&evout1);
		uu_list_push(&points1,evout1.cp);
	}
	pts1 = (UM_coord *)UU_LIST_ARRAY(&points1);
	status = um_3dbox_around(25,pts1,&box1);
	bwdth1 = um_dcccc(box1.ver[0],box1.ver[1]);
	bhgt1 = um_dcccc(box1.ver[0],box1.ver[2]);
	blen1 = um_dcccc(box1.ver[0],box1.ver[4]);

/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve surface splines from the unibase.
*/
	while(ur_get_next_data_key(UM_UVCVONSF_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't bother comparing entities that have @UN labels.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
		else
		{
/*
.....Get the transformation matrix associated with check
*/
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
			uparm = 0;
/*
.....Evaluate curve at 25 points and put those points into pts2
.....Then calculate the box that surrounds this curve and determine
.....the width,height,and length of the box
*/
			for (i=0;i<=24;i++)
			{
				uparm = i/24.0;
				uig_ev13_uvcvonsf(UM_POINT,uparm,&check,trans2.tfmat,&evout2);
				uu_list_push(&points2,evout2.cp);
			}
			pts2 = (UM_coord *)UU_LIST_ARRAY(&points2);
			status = um_3dbox_around(25,pts2,&box2);
			bwdth2 = um_dcccc(box2.ver[0],box2.ver[1]);
			bhgt2 = um_dcccc(box2.ver[0],box2.ver[2]);
			blen2 = um_dcccc(box2.ver[0],box2.ver[4]);
			status = UU_FAILURE;
			pts1 = (UM_coord *)UU_LIST_ARRAY(&points1);
/*
.....Calculate a vector that goes from pts2 to pts1.
*/
			for (i=0;i<3;i++) vector[i] = pts1[0][i] - pts2[0][i];
/*
.....Determine if the box is the same size as the box surrounding eptr,
*/
			if ((fabs(bwdth1-bwdth2) < UM_FUZZ) &&
				(fabs(blen1-blen2) < UM_FUZZ) &&
				(fabs(bhgt1-bhgt2) < UM_FUZZ))
			{
				status = UU_SUCCESS;
/*
.....Translate the points in pts2, by the vector and then
.....see if the points match the points in pts1, if all the
.....points match, it is the same curve, just translated to
.....a new location.
*/
				if (num == 1)
				{
					for(i=0;i<=24 && status == UU_SUCCESS;i++)
					{
						um_translate_point(pts2,1.0,vector,trans_pt);
						if (!um_cceqcc(trans_pt,pts1))
							status = UU_FAILURE;
						pts1++;
						pts2++;
					}
				}
				else 
				{
					um_translate_point(pts2,1.0,vector,trans_pt);
					if (!um_cceqcc(trans_pt,pts1))
						status = UU_FAILURE;
					else
					{
						um_translate_point(&pts2[24],1.0,vector,trans_pt);
						if (!um_cceqcc(trans_pt,&pts1[24]))
							status = UU_FAILURE;
					}
				}
			}
			else if (num > 2)
			{
				status = UU_SUCCESS;
				um_translate_point(pts2,1.0,vector,trans_pt);
				if (!um_cceqcc(trans_pt,pts1))
					status = UU_FAILURE;
				else
				{
					um_translate_point(&pts2[24],1.0,vector,trans_pt);
					if (!um_cceqcc(trans_pt,&pts1[24]))
						status = UU_FAILURE;
				}
			}
			if (status == UU_SUCCESS)
			{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
				cnt = uig_reg_ent(check.key,0.,&i);
				if(cnt) continue;
				dist_low = 0.;
				pos =i;
				close.key = check.key;
				goto done;
			}
			uu_list_free(&points2);
			uu_list_init(&points2, sizeof(UM_coord),25,25);
		}
	}
	status = UU_FAILURE;
done:;
	if (status == UU_SUCCESS)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		if(UIG_regressive)
		{		
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = dist_low;
		}
		else
		{
			uig_match_check_maxnum(&close, &tig_max_cvlab);
			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
.....Update close.label since we have used this entity already
*/
			strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
			tig_unlabeled_ent[7] --;
		}
		status = UU_SUCCESS;
	}
	uu_list_free(&points1);
	uu_list_free(&points2);
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
/*
.....find another match for the entity which previously matched with check
*/
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_uvcvonsf(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_comp_compcrv(eptr,num)
**       Compare translated curve data with existing curve data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   curve record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_compcrv(eptr,num)
struct UM_compcrv_rec *eptr;
int num;
{
	struct UM_compcrv_rec check,close, prev;
	struct UM_transf_rec trans1,trans2;
	struct UM_evcrvout evout1, evout2;
	int cnt, pos=0, status,next_tupleid,i;
	UM_3D_box box1,box2;
	UM_coord *pts1,*pts2,trans_pt;
	UM_vector vector;
	UU_LIST points1,points2;
	UU_REAL dist_low, uparm,blen1,blen2,bhgt1,bhgt2,bwdth1,bwdth2;
	UU_KEY_ID matchkey,savematch;

	matchkey = eptr->key;
	savematch = 0;

	uu_list_init(&points1, sizeof(UM_coord),25,25);
	uu_list_init(&points2, sizeof(UM_coord),25,25);
	status = UU_FAILURE;
	next_tupleid = 1;
/*
.....Get the transformation matrix associated with this curve.
*/
	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);
	for (i=0;i<=24;i++)
	{
		uparm = i/24.0;
		um_ev5_compcrv(UM_POINT, uparm, eptr, trans1.tfmat, &evout1);
		uu_list_push(&points1,evout1.cp);
	}
	pts1 = (UM_coord *)UU_LIST_ARRAY(&points1);
	status = um_3dbox_around(25,pts1,&box1);
	bwdth1 = um_dcccc(box1.ver[0],box1.ver[1]);
	bhgt1 = um_dcccc(box1.ver[0],box1.ver[2]);
	blen1 = um_dcccc(box1.ver[0],box1.ver[4]);
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve composite curves from unibase.
*/
	while(ur_get_next_data_key(UM_COMPCRV_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't compare entities that have @UN labels.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
		else
		{
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
			uparm = 0;
/*
.....Evaluate curve at 25 points and put those points into pts2
.....Then calculate the box that surrounds this curve and determine
.....the width,height,and length of the box
*/
			for (i=0;i<=24;i++)
			{
				uparm = i/24.0;
				um_ev5_compcrv(UM_POINT, uparm, &check, trans2.tfmat, &evout2);
				uu_list_push(&points2,evout2.cp);
			}
			pts2 = (UM_coord *)UU_LIST_ARRAY(&points2);
			status = um_3dbox_around(25,pts2,&box2);
			bwdth2 = um_dcccc(box2.ver[0],box2.ver[1]);
			bhgt2 = um_dcccc(box2.ver[0],box2.ver[2]);
			blen2 = um_dcccc(box2.ver[0],box2.ver[4]);
			status = UU_FAILURE;
			pts1 = (UM_coord *)UU_LIST_ARRAY(&points1);
/*
.....Calculate a vector that goes from pts2 to pts1.
*/
			for (i=0;i<3;i++) vector[i] = pts1[0][i] - pts2[0][i];
/*
.....Determine if the box is the same size as the box surrounding eptr,
*/
			if ((fabs(bwdth1-bwdth2) < UM_FUZZ) &&
				(fabs(blen1-blen2) < UM_FUZZ) &&
				(fabs(bhgt1-bhgt2) < UM_FUZZ))
			{
				status = UU_SUCCESS;
/*
.....Translate the points in pts2, by the vector and then
.....see if the points match the points in pts1, if all the
.....points match, it is the same curve, just translated to
.....a new location.
*/
				if (num == 1)
				{
					for(i=0;i<=24 && status == UU_SUCCESS;i++)
					{
   					um_translate_point(pts2,1.0,vector,trans_pt);
   					if (!um_cceqcc(trans_pt,pts1))
							status = UU_FAILURE;
						pts1++;
						pts2++;
					}
				}
				else 
				{
   				um_translate_point(pts2,1.0,vector,trans_pt);
   				if (!um_cceqcc(trans_pt,pts1))
						status = UU_FAILURE;
					else
					{
   					um_translate_point(pts2,1.0,vector,trans_pt);
   					if (!um_cceqcc(trans_pt,pts1))
							status = UU_FAILURE;
					}
				}
			}
			else if (num >2)
			{
				status = UU_SUCCESS;
   			um_translate_point(pts2,1.0,vector,trans_pt);
   			if (!um_cceqcc(trans_pt,pts1))
					status = UU_FAILURE;
				else
				{
   				um_translate_point(pts2,1.0,vector,trans_pt);
   				if (!um_cceqcc(trans_pt,pts1))
						status = UU_FAILURE;
				}
			}
			if (status == UU_SUCCESS)
			{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
				cnt = uig_reg_ent(check.key,0.0,&i);
				if(cnt) continue;
				dist_low = 0.;
				pos =i;
				close.key = check.key;
				goto done;
			}
			uu_list_free(&points2);
			uu_list_init(&points2, sizeof(UM_coord),25,25);
		}
	}
	status = UU_FAILURE;
done:;
	if (status == UU_SUCCESS)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = dist_low;
		}
		else
		{
			uig_match_check_maxnum(&close, &tig_max_cvlab);
			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
.....Indicate that this entity has been used by
.....setting the label to @UN.
*/
			strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
			tig_unlabeled_ent[7] --;
		}

		status = UU_SUCCESS;
	}
	uu_list_free(&points1);
	uu_list_free(&points2);
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
/*
.....find another match for the entity which previously matched with check
*/
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_compcrv(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_comp_rbsplsrf(eptr,num)
**       Compare translated surface data with existing surface data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   surface record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_rbsplsrf(eptr,num)
struct UM_rbsplsrf_rec *eptr;
int num;
{
	struct UM_rbsplsrf_rec check;
	struct UM_transf_rec trans1,trans2;
	struct NCL_surface_rec ncheck;
	struct NCL_fixed_databag close,prev;
	int pos=0,i,cnt,status,next_tupleid,revflg;
	UM_3D_box *boxp1,*boxp2;
	UM_coord *pts1,*pts2;
	UU_REAL check_rad, check_dist, radius_low, dist_low;
	UU_REAL tol = .001;
	int uig_match_prim();
	int uig_comp_2srf();
	UU_KEY_ID matchkey,savematch;

	matchkey = eptr->key;
	savematch = 0;
	next_tupleid = 1;
	dist_low = IGES_LARGE;
/*
.....Used to get cylindrical surface with closest radius
*/
	radius_low = IGES_LARGE;
/*
.....Get the transformation matrix associated with this curve.
*/
   trans1.key = eptr->key;
   ur_retrieve_transf(&trans1);

	status = uig_box_sf (eptr,trans1.tfmat,1,&boxp1,&pts1);
	if (status != UU_SUCCESS) goto done;
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve surfaces from the unibase.
*/
	while(ur_get_next_data_key(UM_RBSPLSRF_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't compare surfaces that have @UN labels, this means
.....that check has already been matched or check is an underlying
.....surface and underlying entities have already been taken 
.....care of.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
		else
		{
			status = UU_SUCCESS;

			ncl_retrieve_data_fixed(&check);
			if (eptr->primitive != check.primitive)
				status = UU_FAILURE;
/*
.....If the surface is one of the special surfaces use
.....uig_match_prim to determine if it is a match, otherwise
.....calculate points.
*/
			else if (eptr->primitive > 2)
			{
				status = uig_match_prim(eptr,&check,num,1,&check_rad,&check_dist);
				if (status == UU_SUCCESS)
				{
						if((eptr->primitive==NCLSF_CONE || 
								eptr->primitive==NCLSF_CYLINDER) && num==4)
					{
						if (status == UU_SUCCESS)
						{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
							cnt = uig_reg_ent(check.key,check_rad + tol,&i);
							if(cnt) continue;
						}
						if (status == UU_SUCCESS  && 
							((check_rad < radius_low - tol)||
							(( fabs(check_rad - radius_low)<tol)&&
							(check_dist < dist_low))))  
						{
							pos =i;
							dist_low = check_dist;
							radius_low = check_rad;
							close.key = check.key;
						}
						continue;
					}
					else
					{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
						cnt = uig_reg_ent(check.key,0.,&i);
						if(cnt) continue;
						pos =i;
						radius_low = 0;
						dist_low =0;
						check_rad = 0;
					}
/*
.....Keep track of the closest surface
*/
					if (check_dist < IGES_LARGE)
					{
						close.key = check.key;
						goto done;
					}
					else
						status = UU_FAILURE;
				}
			}
			else
			{
/*
.....Get transformation matrix for check
*/
				trans2.key = check.key;
				ur_retrieve_transf(&trans2);
		
				status = uig_box_sf (&check,trans2.tfmat,0,&boxp2,&pts2);
				
				if (status == UU_SUCCESS)
				{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
					cnt = uig_reg_ent(check.key,0.,&i);
					if(cnt) continue;
				}
				if (status == UU_SUCCESS)
					status = uig_comp_2srf(pts1,boxp1,pts2,boxp2,num,&revflg);

				if (status == UU_SUCCESS)
				{
					if (revflg) uig_reverse_rbsf(eptr);
					close.key = check.key;
					radius_low = 0.;
					pos =i;
					dist_low =0;
					goto done;
				}
			}
		}
	}
/*
.....Check to see if there are any NCL surfaces that match.
*/
   dist_low = IGES_LARGE;
/*
.....Used to get cylindrical surface with closest radius
*/
   radius_low = IGES_LARGE;
	next_tupleid = 1;
	while(ur_get_next_data_key(NCL_SURF_REL,&next_tupleid, &ncheck.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&ncheck);
/*
.....Don't compare surfaces that have @UN labels, this means
.....that ncheck has already been matched or ncheck is an underlying
.....surface and underlying entities have already been taken 
.....care of.
*/
		if (ncheck.label[0] == '@')
			status = UU_FAILURE;
		else
		{
			status = UU_SUCCESS;

			ncl_retrieve_data_fixed(&ncheck);
			if (eptr->primitive != ncheck.primitive)
				status = UU_FAILURE;
			else if (eptr->primitive > 2)
			{
				status = uig_match_prim(eptr,&ncheck,num,1,&check_rad,&check_dist);
				if (status == UU_SUCCESS)
				{
               if((eptr->primitive==NCLSF_CONE ||
                        eptr->primitive==NCLSF_CYLINDER) && num==4)
					{
						if (status == UU_SUCCESS)
						{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
							cnt = uig_reg_ent(check.key,check_rad + tol,&i);
							if(cnt) continue;
						}
						if (status == UU_SUCCESS  && 
							((check_rad < radius_low - tol)||
							(( fabs(check_rad - radius_low)<tol)&&
							(check_dist < dist_low)))) 
						{
							pos =i;
							dist_low = check_dist;
							radius_low = check_rad;
							close.key = check.key;
						}
						continue;
					}
					else
					{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
						cnt = uig_reg_ent(check.key,0.,&i);
						if(cnt) continue;
						check_rad = 0;
						pos =i;
						radius_low = 0;
						dist_low =0;
					}
					if (check_dist < IGES_LARGE)
					{
						close.key = ncheck.key;
						goto done;
					}
					else
						status = UU_FAILURE;
				}
			}
			else
			{
				trans2.key = ncheck.key;
				ur_retrieve_transf(&trans2);
		
				status = uig_box_sf (&ncheck,trans2.tfmat,0,&boxp2,&pts2);
				if (status == UU_SUCCESS)
				{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
					cnt = uig_reg_ent(check.key,0.,&i);
					if(cnt) continue;
				}
				if (status == UU_SUCCESS)
					status = uig_comp_2srf(pts1,boxp1,pts2,boxp2,num,&revflg);

				if (status == UU_SUCCESS)
				{
					if (revflg) uig_reverse_rbsf(eptr);
					radius_low = 0.;
					pos =i;
					dist_low =0;
					close.key = ncheck.key;
					goto done;
				}
			}
		}
	}
done:;
	if (status == UU_SUCCESS)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			if((eptr->primitive==NCLSF_CONE ||
							eptr->primitive==NCLSF_CYLINDER) && num==4)
				UIG_regfactor[pos] = radius_low;
			else
				UIG_regfactor[pos] = dist_low;
		}
		else
		{
			uig_match_check_maxnum(&close, &tig_max_sflab);
			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
.....Update close.label with @UN to indicate that
.....a match has been found for this surface.
*/
			strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
			tig_unlabeled_ent[8] --;
		}
		status = UU_SUCCESS;
	}
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
/*
.....find another match for the entity which previously matched with check
*/
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_rbsplsrf(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_box_sf (eptr,tf,lstflg,boxp,ptsp)
**       Evaluate a surface at 625 points, calculate a containing box,
**       save in unibase and return pointers to them. If they already
**       exist in unibase, just return pointers to them.
**    PARAMETERS
**       INPUT  :
**          eptr          surface record
**          tf            transformation matrix.
**          lstflg        If true, save key of sf in global list.
**       OUTPUT :
**          boxp          Pointer to box around the points.
**          ptsp          Pointer to points on surface.
**    RETURNS      : UU_SUCCESS if successful, else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none 
*********************************************************************/
int
uig_box_sf (eptr,tf,lstflg,boxp,ptsp)
struct NCL_fixed_databag *eptr;
UM_transf tf;
int lstflg;
UM_3D_box **boxp;
UM_coord **ptsp;

{
	UU_REAL r,uparm,vparm;
	UM_coord ptsl[625];
	UM_3D_box boxl;
	struct UM_evsrfout evout;
	int i,j,k,ix,n,status;
	int lst;

	status = ncl_surflist_data(BOX_LIST, eptr, boxp);
	if (status == UU_SUCCESS)
		status = ncl_surflist_data(WHOLE_BOUNDARY_LIST, eptr, ptsp);

	if (status != UU_SUCCESS)
	{
		k = 25;
		r = 24.0;
		ix = 0;

		for (i=0;i<k; i++)
		{
			uparm = i/r;
			for (j=0; j<k;j++)
			{
				vparm = j/r;
				status = uc_evsrf(UM_POINT,uparm,vparm,eptr,tf,&evout);
				if (status != UU_SUCCESS) return (status);
				um_vctovc(evout.sp,ptsl[ix++]);
			}
		}

		status = um_3dbox_around(625,ptsl,&boxl);
		if (status == 0 || status == 1)
			status = UU_SUCCESS;
		else
			status = UU_FAILURE;

		if (status == UU_SUCCESS)
		{
			lst = ncl_surflist_number (BOX_LIST, eptr->rel_num);
			n = 3*UM_3DBOX_NUM_VERT;
			status = ur_update_data_varlist(eptr->key, lst, boxl.ver, 1, n);
		}

		if (status == UU_SUCCESS)
		{
			lst = ncl_surflist_number (WHOLE_BOUNDARY_LIST, eptr->rel_num);
			n = 3*625;
			status = ur_update_data_varlist(eptr->key, lst, ptsl, 1, n);
		}

		if (status == UU_SUCCESS)
		{
			if (lstflg) uu_list_push(&UIG_sflist_keys,&eptr->key);
			status = ncl_retrieve_data_fixed(eptr);
		}

		if (status == UU_SUCCESS)
		{
			status = ncl_surflist_data(BOX_LIST, eptr, boxp);
		}

		if (status == UU_SUCCESS)
		{
			status = ncl_surflist_data(WHOLE_BOUNDARY_LIST, eptr, ptsp);
		}
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_comp_2srf(points1,box1,points2,box2,num)
**       Compare two surfaces represented by 625 points each, at the 
**       level defined by num.
**    PARAMETERS
**       INPUT  :
**          points1,box1           surface 1 points and box around
**          points2,box2           surface 2 points and box around
**          num                    level of matching
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none 
*********************************************************************/
uig_comp_2srf(pts1,box1,pts2,box2,num,revflg)
UM_coord *pts1,*pts2;
UM_3D_box *box1, *box2;
int num, *revflg;
{
	UM_coord trans_pt;
	UM_vector vector;
	int i,status;
	UU_REAL tol;

	status = UU_FAILURE;
	*revflg = 0;
	tol = UIG_match_tol * 10.0;

/*
.....Calculate a vector that goes from pts2 to pts1.
*/
	for (i=0;i<3;i++) vector[i] = pts1[0][i] - pts2[0][i];
/*
.....Compare the boxes sizes
*/
	if (num <= 3)
	{
		UU_REAL blen1,blen2,bhgt1,bhgt2,bwdth1,bwdth2;

		bwdth1 = um_dcccc(box1->ver[0],box1->ver[1]);
		bhgt1 = um_dcccc(box1->ver[0],box1->ver[2]);
		blen1 = um_dcccc(box1->ver[0],box1->ver[4]);

		bwdth2 = um_dcccc(box2->ver[0],box2->ver[1]);
		bhgt2 = um_dcccc(box2->ver[0],box2->ver[2]);
		blen2 = um_dcccc(box2->ver[0],box2->ver[4]);

		if ((fabs(bwdth1-bwdth2) < UM_FUZZ) &&
 			(fabs(blen1-blen2) < UM_FUZZ) &&
 			(fabs(bhgt1-bhgt2) < UM_FUZZ))
		{
			status = UU_SUCCESS;
/*
..... check for reversed surface.
*/
			if (num == 1)
			{
				pts2 += 624;
				for(i=0;i<=624 && status == UU_SUCCESS;i++,pts1++,pts2--)
				{
					if (um_dcccc(pts2,pts1) > tol) status = UU_FAILURE;
				}
				if (status == UU_SUCCESS) *revflg = 1;
			}
/*
.....Translate the points in pts2, by the vector and then
.....see if the points match the points in pts1, if all the
.....points match, it is the same surface, just translated to
.....a new location.
*/
			else if (num == 2)
			{
				for(i=0;i<=624 && status == UU_SUCCESS;i++)
				{
					um_translate_point(pts2,1.0,vector,trans_pt);
					if (!um_cceqcc(trans_pt,pts1))
						status = UU_FAILURE;
					pts1++;
					pts2++;
				}
			}
			else 
			{
/*
.....Just check the corners of the surfaces to see if they match.
.....u = 0, v = 0
*/
				um_translate_point(pts2,1.0,vector,trans_pt);
				if (!um_cceqcc(trans_pt,pts1))
					status = UU_FAILURE;
/*
.....u = 0, v = 1
*/
				um_translate_point(&pts2[24],1.0,vector,trans_pt);
				if (!um_cceqcc(trans_pt,&pts1[24]))
					status = UU_FAILURE;
/*
.....u = 1, v = 0;
*/
				um_translate_point(&pts2[600],1.0,vector,trans_pt);
				if (!um_cceqcc(trans_pt,&pts1[600]))
					status = UU_FAILURE;
/*
.....u = 1, v = 1;
*/
				um_translate_point(&pts2[624],1.0,vector,trans_pt);
				if (!um_cceqcc(trans_pt,&pts1[624]))
					status = UU_FAILURE;
			}
		}
	}

	else /* if (num > 3) */
	{
		if (num > 4) tol *= 5.0;
/*
.....Check the corners and center of the surfaces to see if they match.
*/
		status = UU_SUCCESS;
		um_translate_point(pts2,1.0,vector,trans_pt);
		if (!um_cceqcc_tol(trans_pt,pts1,tol))
			status = UU_FAILURE;
		if (status == UU_SUCCESS)
		{
			um_translate_point(&pts2[24],1.0,vector,trans_pt);
			if (!um_cceqcc_tol(trans_pt,&pts1[24],tol))
				status = UU_FAILURE;
		}
		if (status == UU_SUCCESS)
		{
			um_translate_point(&pts2[312],1.0,vector,trans_pt);
			if (!um_cceqcc_tol(trans_pt,&pts1[312],tol))
				status = UU_FAILURE;
		}
		if (status == UU_SUCCESS)
		{
			um_translate_point(&pts2[600],1.0,vector,trans_pt);
			if (!um_cceqcc_tol(trans_pt,&pts1[600],tol))
				status = UU_FAILURE;
		}
		if (status == UU_SUCCESS)
		{
			um_translate_point(&pts2[624],1.0,vector,trans_pt);
			if (!um_cceqcc_tol(trans_pt,&pts1[624],tol))
				status = UU_FAILURE;
		}
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_comp_meshsurf(eptr,num)
**       Compare translated surface data with existing surface data
**       to see if there is a close match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   surface record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none 
*********************************************************************/
uig_comp_meshsurf(eptr,num)
struct NCL_meshsf_rec *eptr;
int num;
{
	struct NCL_meshsf_rec check,prev;
	struct UM_transf_rec trans1,trans2;
	int pos=0,i,cnt,status,next_tupleid, revflg;
	UM_3D_box *boxp1,*boxp2;
	UM_coord *pts1,*pts2;
	UU_KEY_ID matchkey,savematch;
	UU_REAL dist_low;

	matchkey = eptr->key;
	savematch = 0;

	status = UU_FAILURE;
	next_tupleid = 1;
/*
.....Get the transformation matrix associated with this curve.
*/
	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);

/*
..... Eduard 071700. I see from the previous version that mesh surfaces
..... are always compared at level = 1, no matter what num is. So I
..... am doing it the same way. 
*/

	status = uig_box_sf ((struct NCL_fixed_databag *)eptr,trans1.tfmat,1,&boxp1,&pts1);
	if (status != UU_SUCCESS) goto done;
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve meshsurfaces from the unibase.
*/
	while(ur_get_next_data_key(NCL_MESHSURF_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't check surfaces that have @UN for a label.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
/*
.....Evaluate the surface at a few points to see if we can
.....find a match.
*/
		else
		{
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
	
			status = uig_box_sf ((struct NCL_fixed_databag *)&check,trans2.tfmat,0,&boxp2,&pts2);
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
			if (status == UU_SUCCESS)
			{
				cnt = uig_reg_ent(check.key,0.,&i);
				if(cnt) continue;
			}
			if (status == UU_SUCCESS)
				status = uig_comp_2srf(pts1,boxp1,pts2,boxp2,num,&revflg);

			if (status == UU_SUCCESS)
			{
				pos =i;
				dist_low =0.;
				goto done;
			}
		}
	}
/*
.....Found a match
*/
done:;
	if (status == UU_SUCCESS)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&check);
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			UIG_regfactor[pos] = dist_low;
		}
		else
		{
			uig_match_check_maxnum(&check, &tig_max_sflab);
			strcpy(eptr->label,check.label);
			eptr->subscr = check.subscr;
			strcpy(check.label,"@UN");
			check.subscr = 0;
			ur_update_data_fixed(&check);
			tig_unlabeled_ent[8] --;
		}
		status = UU_SUCCESS;
	}
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
/*
.....find another match for the entity which previously matched with check
*/
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_meshsurf(&prev,num);
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_comp_trimsrf(eptr,num)
**       Compare translated trimmed surface data with existing 
**       trimmed surface data to see if there is a close match 
**       for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   trimmed surface record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_trimsrf(eptr,num)
struct NCL_trimsf_rec *eptr;
int num;
{
	struct NCL_trimsf_rec check,close, prev;
	struct UM_rbsplsrf_rec base1,base2;
	struct UM_transf_rec trans1,trans2;
	struct NCL_revsurf_rec *revsfp1, *revsfp2;
	int numcv1, numcv2, iprim1, iprim2, revflg;
	int cnt,pos=0, i,status,next_tupleid,prim_check;
	UM_3D_box *boxp1,*boxp2;
	UM_coord *pts1, *pts2;
	UM_2Dcoord ummn1, vmmn1, ummn2, vmmn2;
	UM_srf_boundary boundary_box1, boundary_box2;
	UU_REAL tol,check_rad,check_dist,radius_low,dist_low,uparm, vparm;
	UU_REAL xm1[2],xm2[2],ym1[2],ym2[2];
	UU_REAL d1,d2,d3,d4, *primp1;
	UU_KEY_ID matchkey,savematch;

	matchkey = eptr->key;
	savematch = 0;

	status = UU_FAILURE;
/*
.....Used to get cylindrical surface with closest radius
*/
	dist_low = IGES_LARGE;
	radius_low = IGES_LARGE;
/*
.....If eptr doesn't not yet have a uvkey, exit now, otherwise
.....a fatal error will occur when checking the boundary box.
*/
	if (eptr->uv_key == 0)
		goto done;

	revsfp1 = (struct NCL_revsurf_rec *)&base1;
	revsfp2 = (struct NCL_revsurf_rec *)&base2;

	next_tupleid = 1;
	tol = .001;
	prim_check = num;
	uparm = vparm = 0.0;
/*
.....Get the base surface, we will compare these first.
*/
	base1.key = eptr->bs_key;
	if (base1.key > 0) status = UU_SUCCESS;
	if (status == UU_SUCCESS)
		status = ncl_retrieve_data_fixed(&base1);
/*
.....Get the transformation matrix associated with the surface.
*/
	trans1.key = eptr->key;
	if (trans1.key <= 0) status = UU_FAILURE;
	if (status == UU_SUCCESS)
		status = ur_retrieve_transf(&trans1);

	if (status == UU_SUCCESS)
	{
		status = uig_box_sf ((struct NCL_fixed_databag *)&base1,trans1.tfmat,1,&boxp1,&pts1);
	}
	if (status != UU_SUCCESS) goto done;

	if (base1.rel_num == NCL_REVSURF_REL)
	{
		iprim1 = revsfp1->primitive;
		primp1 = revsfp1->prim_param;
	}
	else
	{
		iprim1 = base1.primitive;
		primp1 = base1.prim_param;
	}

	ncl_set_boundary_toler(tol);
	ncl_get_boundary(WHOLE_BOUNDARY_LIST,eptr,&boundary_box1);
	numcv1 = boundary_box1.nb;


/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve trimmed surfaces from the unibase.
.......Make sure keys are initialized so a check can be made later to see if
.......a key was actuall found - ASF 11/07/13.
*/
	check.key = close.key = prev.key = 0;
	while(ur_get_next_data_key(NCL_TRIMSF_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't compare surfaces that have @UN labels, they
.....have already been matched.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
		else
		{
			status = UU_SUCCESS;
			base2.key = check.bs_key;
			ncl_retrieve_data_fixed(&base2);
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
/*
.....Compare base surface primitives to see if we have a match
.....If the primitive is less than two, then we need to evaluate
.....the surface and compare those points.
*/
			if (base2.rel_num == NCL_REVSURF_REL)
			{
				iprim2 = revsfp2->primitive;
			}
			else
			{
				iprim2 = base2.primitive;
			}

			if (iprim1 != iprim2)
				status = UU_FAILURE;
			else if (iprim1 > 2) 
			{
				status = uig_match_prim(&base1,&base2,prim_check,1,&check_rad,
							&check_dist);
				if (status == UU_SUCCESS && check_dist >= IGES_LARGE) 
					status = UU_FAILURE;
				if (status == UU_SUCCESS)
				{
					if (iprim1 == NCLSF_PLANE)
					{
						if ((num == 1 || num == 3) && check_dist > tol)
						{
							status = UU_FAILURE;
						}
						if (status == UU_SUCCESS && num <=2)
						{
							boundary_box2.toler = tol;
							ncl_get_boundary(WHOLE_BOUNDARY_LIST,&check,&boundary_box2);
							check_dist = -uig_isect_area(primp1, &boundary_box1, &boundary_box2);
						}
						if(status == UU_SUCCESS)
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
						{
							cnt = uig_reg_ent(check.key,check_dist,&i);
							if(cnt) continue;
						}
						if (status == UU_SUCCESS && check_dist < dist_low)
						{
							pos =i;
							dist_low = check_dist;
							close.key = check.key;
						}
						continue;

					}
					if ((iprim1 == NCLSF_CONE || iprim1 == NCLSF_CYLINDER)&& num ==4)
					{
/*
..... Find the most similar radius.
..... If two or more similar radii are found compare the start points
..... Himani
*/
						if (status == UU_SUCCESS)
						{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
							cnt = uig_reg_ent(check.key,check_rad + tol,&i);
							if(cnt) continue;
													}
						if (status == UU_SUCCESS  && 
							((check_rad < radius_low - tol)||
							(( fabs(check_rad - radius_low)<tol)&&
							(check_dist < dist_low)))) 
						{
							pos =i;
							dist_low = check_dist;
							radius_low = check_rad;
							close.key = check.key;
						}
						continue;
					}
					else
					{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
						cnt = uig_reg_ent(check.key,0.,&i);
						if(cnt) continue;
						pos =i;
						radius_low= 0.;
						dist_low= 0.;
						check_rad = 0;
					}
				}
			}
			else
			{
				status = uig_box_sf ((struct NCL_fixed_databag *)&base2,trans2.tfmat,0,&boxp2,&pts2);
				
				if (status == UU_SUCCESS)
					status = uig_comp_2srf(pts1,boxp1,pts2,boxp2,num,&revflg);

/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
				if (status == UU_SUCCESS)
				{
					cnt = uig_reg_ent(check.key,0.,&i);
					if(cnt) continue;
				}
				if (status == UU_SUCCESS)
				{
					pos =i;
					radius_low = 0.;
					dist_low= 0.;
					close.key = check.key;
					goto checkbox;
				}
			}
checkbox:;
			if (status == UU_SUCCESS)
			{
				boundary_box2.toler = tol;

				um_pre_srf_bndr_box(&check,&boundary_box2);
				numcv2 = boundary_box2.nb;

				if (numcv1 != numcv2 && num < 2)
					status = UU_FAILURE;
				else
				{
					status = UU_SUCCESS;
/*
					um_vctovc_2d (boundary_box1.ummx, &ummn1);
					um_vctovc_2d (boundary_box1.vmmx, &vmmn1);
					um_vctovc_2d (boundary_box2.ummx, &ummn2);
					um_vctovc_2d (boundary_box2.vmmx, &vmmn2);
*/
					um_vctovc_2d (*(boundary_box1.ummx), ummn1);
					um_vctovc_2d (*(boundary_box1.vmmx), vmmn1);
					um_vctovc_2d (*(boundary_box2.ummx), ummn2);
					um_vctovc_2d (*(boundary_box2.vmmx), vmmn2);
/*
.....Just check to see if bounding boxes intersect;
*/
					xm1[0] = ummn1[0];
					xm1[1] = ummn1[1];
					ym1[0] = vmmn1[0];
					ym1[1] = vmmn1[1];
					xm2[0] = ummn2[0];
					xm2[1] = ummn2[1];
					ym2[0] = vmmn2[0];
					ym2[1] = vmmn2[1];
					d1 = fabs(xm2[0]-xm1[0]);
					d2 = fabs(1.0-xm2[0]-xm1[0]);
					d3 = fabs(xm2[1]-xm1[0]);
					d4 = fabs(1.0-xm2[1]-xm1[0]);
					if (d2 < d1) 
					{
						d1 = d2;
						xm2[0] = 1.0 - ummn2[0];
						xm2[1] = 1.0 - ummn2[1];
					}
					if (d3 < d1) 
					{
						d1 = d3;
						xm2[0] = ummn2[1];
						xm2[1] = ummn2[0];
					}
					if (d4 < d1) 
					{
						xm2[0] = 1.0 - ummn2[1];
						xm2[1] = 1.0 - ummn2[0];
					}
					d1 = fabs(ym2[0]-ym1[0]);
					d2 = fabs(1.0-ym2[0]-ym1[0]);
					d3 = fabs(ym2[1]-ym1[0]);
					d4 = fabs(1.0-ym2[1]-ym1[0]);
					if (d2 < d1) 
					{
						d1 = d2;
						ym2[0] = 1.0 - vmmn2[0];
						ym2[1] = 1.0 - vmmn2[1];
					}
					if (d3 < d1) 
					{
						d1 = d3;
						ym2[0] = vmmn2[1];
						ym2[1] = vmmn2[0];
					}
					if (d4 < d1) 
					{
						ym2[0] = 1.0 - vmmn2[1];
						ym2[1] = 1.0 - vmmn2[0];
					}
					status =  um_isect_boxes(&xm1,&ym1,&xm2, &ym2,tol);
					if (status == 1)
					{
						close.key = check.key;
						status = UU_SUCCESS;
						goto done;
					}
				}
			}
		}
	}
	status = UU_FAILURE;
done:;
/*
.....Added check to make sure close was actually found - ASF 11/07/13.
*/
	if((status == UU_SUCCESS || dist_low < IGES_LARGE) && close.key > 0)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			if ((iprim1 == NCLSF_CONE || iprim1 == NCLSF_CYLINDER)&& num ==4)
				UIG_regfactor[pos] = radius_low;
			else
				UIG_regfactor[pos] = dist_low;
		}
		else
		{
			uig_match_check_maxnum(&close, &tig_max_sflab);
			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
.....Change close.label to @UN to indicate that this surface
.....has been matched.
*/
			strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
			tig_unlabeled_ent[8] --;
		}
		status = UU_SUCCESS;
	}
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
/*
.....find another match for the entity which previously matched with check
*/
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_trimsrf(&prev,num);
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_comp_revsf(eptr,num)
**       Compare translated surface of revolution data with existing 
**       surfaces dwdata to see if there is a close match 
**       for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   surface of revolution record
**          num                    level of matching
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_comp_revsf(eptr,num)
struct NCL_revsurf_rec *eptr;
int num;
{
	struct NCL_revsurf_rec check;
	struct UM_rbsplsrf_rec ncheck;
	struct UM_transf_rec trans1,trans2;
	struct NCL_fixed_databag close,prev;
	int pos=0,cnt,i,status,next_tupleid, revflg;
	UM_3D_box *boxp1,*boxp2;
	UM_coord *pts1,*pts2;
	UU_REAL check_dist,check_rad, radius_low,	dist_low;
	UU_REAL tol = .001;
	UU_KEY_ID matchkey,savematch;

	matchkey = eptr->key;
	savematch = 0;

	dist_low = IGES_LARGE;
/*
.....Used to get cylindrical surface with closest radius
*/
	radius_low = IGES_LARGE;
   next_tupleid = 1;
/*
.....Get the transformation matrix associated with this curve.
*/
   trans1.key = eptr->key;
   ur_retrieve_transf(&trans1);

	status = uig_box_sf ((struct NCL_fixed_databag *)eptr,trans1.tfmat,1,&boxp1,&pts1);
	if (status != UU_SUCCESS) goto done;
/*
.....Switch to secondary unibase.
*/
	ur_getu_second();
/*
.....Retrieve surfaces from the unibase.
*/
	while(ur_get_next_data_key(NCL_REVSURF_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't compare surfaces that have @UN labels, this means
.....that check has already been matched or check is an underlying
.....surface and underlying entities have already been taken 
.....care of.
*/

		if (check.label[0] == '@')
			status = UU_FAILURE;
		else
		{
			status = UU_SUCCESS;

			ncl_retrieve_data_fixed(&check);
			if (eptr->primitive != check.primitive)
				status = UU_FAILURE;
/*
.....If the surface is one of the special surfaces use
.....uig_match_prim to determine if it is a match, otherwise
.....calculate points.
*/
			else if (eptr->primitive > 2)
			{
				status = uig_match_prim(eptr,&check,num,1,&empty2,&check_dist);

            if (status == UU_SUCCESS)
            {
               if((eptr->primitive==NCLSF_CONE ||
                        eptr->primitive==NCLSF_CYLINDER) && num==4)
					{
						if (status == UU_SUCCESS)
						{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
							cnt = uig_reg_ent(check.key,check_rad + tol,&i);
							if(cnt) continue;
						}
						if (status == UU_SUCCESS  && 
							((check_rad < radius_low - tol)||
							(( fabs(check_rad - radius_low)<tol)&&
							(check_dist < dist_low))))  
						{
							pos =i;
							dist_low = check_dist;
							radius_low = check_rad;
							close.key = check.key;
						}
						continue;
					}
					else
					{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
						cnt = uig_reg_ent(check.key,0.,&i);
						if(cnt) continue;
						check_rad = 0;
						pos =i;
						radius_low = 0;
						dist_low =0;
					}
               if (check_dist < IGES_LARGE)
               {
                  close.key = check.key;
                  goto done;
               }
               else
                  status = UU_FAILURE;
            }
			}
			else
			{
/*
.....Get transformation matrix for check
*/
				trans2.key = check.key;
				ur_retrieve_transf(&trans2);

				status = uig_box_sf ((struct NCL_fixed_databag *)&check,trans2.tfmat,0,&boxp2,&pts2);
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
				if (status == UU_SUCCESS)
				{
					cnt = uig_reg_ent(check.key,0.,&i);
					if(cnt) continue;
				}
				if (status == UU_SUCCESS)
					status = uig_comp_2srf(pts1,boxp1,pts2,boxp2,num,&revflg);

				if (status == UU_SUCCESS)
				{
					pos =i;
					radius_low =0.0;
					dist_low =0;
					close.key = check.key;
					goto done;
				}
			}
		}
	}
/*
.....Check to see if there are any rbspl surfaces that match.
*/
   dist_low = IGES_LARGE;
/*
.....Used to get cylindrical surface with closest radius
*/
   radius_low = IGES_LARGE;
	next_tupleid = 1;
	while(ur_get_next_data_key(UM_RBSPLSRF_REL,&next_tupleid, &ncheck.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&ncheck);
/*
.....Don't compare surfaces that have @UN labels, this means
.....that ncheck has already been matched or ncheck is an underlying
.....surface and underlying entities have already been taken 
.....care of.
*/

		if (ncheck.label[0] == '@')
			status = UU_FAILURE;
		else
		{
			status = UU_SUCCESS;

			ncl_retrieve_data_fixed(&ncheck);
			if (eptr->primitive != ncheck.primitive)
				status = UU_FAILURE;
			else if (eptr->primitive > 2)
			{
				status = uig_match_prim(eptr,&ncheck,num,1,&check_rad,&check_dist);
            if (status == UU_SUCCESS)
            {
               if((eptr->primitive==NCLSF_CONE ||
                        eptr->primitive==NCLSF_CYLINDER) && num==4)
					{
						if (status == UU_SUCCESS)
						{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
							cnt = uig_reg_ent(check.key,check_rad + tol,&i);
							if(cnt) continue;
						}
						if (status == UU_SUCCESS  && 
							((check_rad < radius_low - tol)||
							(( fabs(check_rad - radius_low)<tol)&&
							(check_dist < dist_low)))) 
						{
							pos =i;
							dist_low = check_dist;
							radius_low = check_rad;
							close.key = check.key;
						}
						continue;
					}
					else
					{
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
						cnt = uig_reg_ent(check.key,0.,&i);
						if(cnt) continue;
						pos =i;
						check_rad = 0;
						radius_low = 0;
						dist_low =0;
					}
               if (check_dist < IGES_LARGE)
               {
                  close.key = ncheck.key;
                  goto done;
               }
               else
                  status = UU_FAILURE;
            }
			}
			else
			{
				trans2.key = ncheck.key;
				ur_retrieve_transf(&trans2);
		
				status = uig_box_sf ((struct NCL_fixed_databag *)&ncheck,trans2.tfmat,0,&boxp2,&pts2);			
/*
.....if this ent has already been matched in this level with a better factor 
.....do not consider it.
*/
				if (status == UU_SUCCESS)
				{
					cnt = uig_reg_ent(check.key,0.,&i);
					if(cnt) continue;
				}
				if (status == UU_SUCCESS)
					status = uig_comp_2srf(pts1,boxp1,pts2,boxp2,num,&revflg);

				if (status == UU_SUCCESS)
				{
					pos =i;
					radius_low = 0.;
					dist_low = 0.;
					close.key = ncheck.key;
					goto done;
				}
			}
		}
	}
done:;
	if (status == UU_SUCCESS)
	{
/*
.....if this entity has been matched in the same level with a worse factor
.....then unmatch the previous and match it with this entity
*/
		ncl_retrieve_data_fixed(&close);
		if(UIG_regressive)
		{
			if(UIG_matchkeys[pos] != 0)
				savematch = UIG_matchkeys[pos];
			UIG_matchkeys[pos] = matchkey;
			if((eptr->primitive==NCLSF_CONE ||
				eptr->primitive==NCLSF_CYLINDER) && num==4)
				UIG_regfactor[pos] = radius_low;
			else
				UIG_regfactor[pos] = dist_low;
		}
		else
		{
			uig_match_check_maxnum(&close, &tig_max_sflab);
			strcpy(eptr->label,close.label);
			eptr->subscr = close.subscr;
/*
.....Update close.label with @UN to indicate that
.....a match has been found for this surface.
*/
			strcpy(close.label,"@UN");
			close.subscr = 0;
			ur_update_data_fixed(&close);
			tig_unlabeled_ent[8] --;
		}
		status = UU_SUCCESS;
	}
/*
.....Switch back to primary unibase.
*/
	ur_getu_work();
/*
.....find another match for the entity which previously matched with check
*/
	if(UIG_regressive && status == UU_SUCCESS && savematch)
	{
		prev.key = savematch;
		ncl_retrieve_data_fixed(&prev);
		uig_comp_revsf(&prev,num);
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_create_sec_unmatch()
**				Creates entities in new unibase from the unmatched
**          entities in the secondary unibase.
**          Also updates the highest label number for each of 
**          the standard labels so that unmatched entities in the new 
**          unibase will not accidentally be given the same name
**          as an unused entity.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
uig_create_sec_unmatch()
{
	struct NCL_fixed_databag eptr;
	int i, next_tupleid, status, flag;
	char name[64], label[80];
	int j, k, rel_num,num;
	UU_LOGICAL copied;
	void uig_error();

/*
........Initialize Unibase lists
*/
   ncl_ublist_init();
	status = UU_SUCCESS;
	flag =0;

	ur_getu_second();
	i = 0;
	next_tupleid = 1;
	while(ur_get_next_key(&next_tupleid, &eptr.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&eptr);
/*
.....Do not create entities whose label starts with an '@'
*/
		if (eptr.label[0] != '@')
		{
			i++;
/*
.....Updated parameters to match changes to ncl_ubcopy_ent - ASF 11/07/13.
*/
			ncl_ubcopy_ent (&eptr,0,0,&copied);
/*
..... Update the highest label number for each of the standard labels
..... so that unmatched entities in the new unibase will not accidentally
..... be given the same name as an unmatched entity in the secondary unibase.
.....
.....Commented out this code as it unnecessarily changes
.....the beginning number of the output entities labels
.....by checking the maximum secondary unibase used
.....This is especiall evident when the naming modals
.....are set to Subscript and the secondary unibase
.....has prefix style labels
.....There seems to be similar code in other routines
.....that will catch duplicate labels anyway
.....Bobby - 9/12/07
*/
/*
			rel_num = eptr.rel_num;
			strncpy(name,eptr.label,64);
			for (j=0;j<64;j++)
				if (name[j] == ' ' || name[j] == '\0') break;
			name[j] = '\0';
			if (eptr.subscr == 0)
			{
				strcpy(label,name);
				for (k=j-1;k>=0;k--)
					if (!isdigit(name[k])) break;
				num = 0;
				if (k < j-1)
				{
					sscanf(&name[k+1],"%d",&num);
					name[k+1] = '\0';
				}
			}
			else
			{
				num = eptr.subscr;
			}
			switch(rel_num)
			{
				case UM_POINT_REL:
					if (num>tig_max_ptlab) 
						tig_max_ptlab = num;
					break;
				case UM_LINE_REL:
					if (num>tig_max_lnlab) 
						tig_max_lnlab = num;
					break;
				case UM_CIRCLE_REL:
					if (num>tig_max_cilab) 
						tig_max_cilab = num;
					break;
				case NCL_PLN_REL:
					if (num>tig_max_pllab) 
						tig_max_pllab = num;
					break;
				case UM_POLYLINE_REL:
				case NCL_PATERN_REL:
					if (num>tig_max_pnlab) 
						tig_max_pnlab = num;
					break;
				case NCL_POINTVEC_REL:
					if (num>tig_max_pvlab) 
						tig_max_pvlab = num;
					break;
				case NCL_CURVE_REL:
				case UM_RBSPLCRV_REL:
				case UM_UVCVONSF_REL:
				case UM_COMPCRV_REL:
				case UM_CONIC_REL:
					if (num>tig_max_cvlab) 
						tig_max_cvlab = num;
					break;
				case NCL_SURF_REL:
				case UM_RBSPLSRF_REL:
				case NCL_MESHSURF_REL:
				case NCL_TRIMSF_REL:
					if (num>tig_max_sflab) 
						tig_max_sflab = num;
					break;

				default:
					status = UU_FAILURE;
			}
*/
		}
	}
	
	ur_getu_work();
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_secondary_unmatched()
**          Prints out the labels in the secondary unibase that
**          are unused.  Also updates the highest label number
**          for each of the standard labels so that unmatched 
**          entities will not accidentally be given the same name
**          as an unused entity if the unmatched entitites are to be 
**			labeled from the next higest matched entity.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
uig_secondary_unmatched()
{
	struct NCL_fixed_databag eptr;
	int next_tupleid,num,status, flag;
	char  buf[80];
	struct labstruct {
		int rel_num;
		int num;
		char name[64];
		char label[80];
	};
	struct labstruct *lsp, *p1, *p2;
	char name[64], label[80];
	int j, k, rel_num;
	void uig_error();

	status = UU_SUCCESS;
	flag =0;
	lsp = (struct labstruct *)uu_lsnew();
	if (!lsp) return(UU_FAILURE);

	ur_getu_second();

	next_tupleid = 1;
	while(ur_get_next_key(&next_tupleid, &eptr.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&eptr);
/*
.....Do not print out any label that starts with an '@'
*/
		if (eptr.label[0] != '@')
		{
			rel_num = eptr.rel_num;
			strncpy(name,eptr.label,64);
			for (j=0;j<64;j++)
				if (name[j] == ' ' || name[j] == '\0') break;
			name[j] = '\0';
			if (eptr.subscr == 0)
			{
				strcpy(label,name);
				for (k=j-1;k>=0;k--)
					if (!isdigit(name[k])) break;
				num = 0;
				if (k < j-1)
				{
					sscanf(&name[k+1],"%d",&num);
					name[k+1] = '\0';
				}
			}
			else
			{
				num = eptr.subscr;
				sprintf(label,"%s(%d)", name, num);
			}
			status = UU_SUCCESS;

			switch(rel_num)
			{
/*
.....If the option for labeling the unmatched entities is from the secondary 
.....unibase then update the highest label number for each of the standard 
.....labels so that unmatched entities will not accidentally be given the same 
.....name as an unused entity 
*/
				case UM_POINT_REL:
					if (UIG_start_unmatch && num>tig_max_ptlab) 
						tig_max_ptlab = num;
					break;
				case UM_LINE_REL:
					if (UIG_start_unmatch && num>tig_max_lnlab) 
						tig_max_lnlab = num;
					break;
				case UM_CIRCLE_REL:
					if (UIG_start_unmatch && num>tig_max_cilab) 
						tig_max_cilab = num;
					break;
				case NCL_PLN_REL:
					if (UIG_start_unmatch && num>tig_max_pllab) 
						tig_max_pllab = num;
					break;
				case UM_POLYLINE_REL:
				case NCL_PATERN_REL:
					if (UIG_start_unmatch && num>tig_max_pnlab) 
						tig_max_pnlab = num;
					break;
				case NCL_POINTVEC_REL:
					if (UIG_start_unmatch && num>tig_max_pvlab) 
						tig_max_pvlab = num;
					break;
				case NCL_CURVE_REL:
				case UM_RBSPLCRV_REL:
				case UM_UVCVONSF_REL:
				case UM_COMPCRV_REL:
				case UM_CONIC_REL:
					if (UIG_start_unmatch && num>tig_max_cvlab) 
						tig_max_cvlab = num;
					break;
				case NCL_SURF_REL:
				case UM_RBSPLSRF_REL:
				case NCL_MESHSURF_REL:
				case NCL_TRIMSF_REL:
					if (UIG_start_unmatch && num>tig_max_sflab) 
						tig_max_sflab = num;
					break;

				default:
					status = UU_FAILURE;
			}
			if (status == UU_SUCCESS)
			{
				p1 = lsp;
				while (1)
				{
					p2 = p1;
					p1 = (struct labstruct *)uu_lsnext(p1);
					if (!p1) break;
/*					if (rel_num < p1->rel_num) break;*/
/*					if (rel_num > p1->rel_num) continue;*/
					j = strcmp(name,p1->name);
					if (j < 0) break;
					if (j > 0) continue;
					if (num < p1->num) break;
				}
				if (p2)
				{
					p1=(struct labstruct *)uu_lsinsrt(p2,(sizeof(struct labstruct)));
					p1->rel_num = rel_num;
					p1->num = num;
					strcpy(p1->name, name);
					strcpy(p1->label, label);
				}
			}
		}
	}
	if (lsp)
	{
		p1 = (struct labstruct *)uu_lsnext(lsp);
		if (p1)
			strcpy(buf," Labels not used from labeling unibase \n");
		else
			strcpy(buf," All labels used from labeling unibase \n");
		uig_error(buf);
		while (p1)
		{
			sprintf(buf," %s\n", p1->label);
			uig_error(buf);
			p1 = (struct labstruct *)uu_lsnext(p1);
		}
		uu_lsdel(lsp);
	}
	ur_getu_work();
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  uig_revers_rbsf(eptr)
**          Reverse a rational B-spline surface.
**    PARAMETERS
**       INPUT  :
**            eptr  - Surface to reverse.
**       OUTPUT :
**            eptr  - Reversed surface.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : 1. Changes variable lists in place.
**                   2. Does not reverse primitive data
************************************************************************/
uig_reverse_rbsf(eptr)
struct UM_rbsplsrf_rec *eptr;
{
	int j, n1;
	UU_REAL *ptr0, *ptr1, t1, ht;
	UM_vector hpt;

	n1 = eptr->no_tu - 2;
	ptr0 = eptr->tu;
	ptr1 = &eptr->tu[n1-1];
	t1   = *ptr1;
	for (j=1; j<=n1/2; j++, ptr0++, ptr1--)
	{
		ht = t1 - *ptr0;
		*ptr0 = t1 - *ptr1;
		*ptr1 = ht;
	}
	if (n1%2) *ptr0 = t1-*ptr0;

	n1 = eptr->no_tv - 2;
	ptr0 = eptr->tv;
	ptr1 = &eptr->tv[n1-1];
	t1   = *ptr1;
	for (j=1; j<=n1/2; j++, ptr0++, ptr1--)
	{
		ht = t1 - *ptr0;
		*ptr0 = t1 - *ptr1;
		*ptr1 = ht;
	}
	if (n1%2) *ptr0 = t1-*ptr0;

	n1 = eptr->no_pt;
	ptr0 = eptr->pt;
	ptr1 = &eptr->pt[(n1-1)*3];
	for (j=1; j<=n1/2; j++, ptr0+=3, ptr1-=3)
	{
		um_vctovc(ptr0, hpt);
		um_vctovc(ptr1, ptr0);
		um_vctovc(hpt, ptr1);
	}

	n1 = eptr->no_wt;
	ptr0 = eptr->wt;
	ptr1 = &eptr->wt[n1-1];
	for (j=1; j<=n1/2; j++, ptr0++, ptr1--)
	{
		ht    = *ptr0;
		*ptr0 = *ptr1;
		*ptr1 = ht;
	}

	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : UU_REAL uig_isect_area(primp, box1, box2)
**          Calculate the area of intersection of planar boxes.
**    PARAMETERS
**       INPUT  :
**            primp  - Pointer to planar primitive.
**            box1   - First box.
**            box2   - Second box.
**       OUTPUT :
**            none
**    RETURNS      : Intersection area.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
************************************************************************/
UU_REAL uig_isect_area(primp, box1, box2)
UU_REAL *primp;
UM_srf_boundary *box1, *box2;
{
	int j, ix;
	UU_REAL a1, *xp1, *xp2, x1, y1, x2, y2;
	UU_REAL xmin1 =  1.0e15, ymin1 =  1.0e15, xmin2 =  1.0e15, ymin2 =  1.0e15;
	UU_REAL xmax1 = -1.0e15, ymax1 = -1.0e15, xmax2 = -1.0e15, ymax2 = -1.0e15;
	UM_vector v1,v2;

	um_perpvc(primp,v1);
	um_cross(primp,v1,v2);
	xp1 = (UU_REAL *)UU_LIST_ARRAY(box1->cvpts);
	xp2 = (UU_REAL *)UU_LIST_ARRAY(box2->cvpts);

	for (j=0,ix=0;j<box1->np[0];j++,ix+=3)
	{
		x1 = um_dot(v1,&xp1[ix]);
		y1 = um_dot(v2,&xp1[ix]);
		if (x1 < xmin1) xmin1 = x1;
		if (x1 > xmax1) xmax1 = x1;
		if (y1 < ymin1) ymin1 = y1;
		if (y1 > ymax1) ymax1 = y1;
	}

	for (j=0,ix=0;j<box2->np[0];j++,ix+=3)
	{
		x2 = um_dot(v1,&xp2[ix]);
		y2 = um_dot(v2,&xp2[ix]);
		if (x2 < xmin2) xmin2 = x2;
		if (x2 > xmax2) xmax2 = x2;
		if (y2 < ymin2) ymin2 = y2;
		if (y2 > ymax2) ymax2 = y2;
	}

	x1 = (xmin1 > xmin2) ? xmin1 : xmin2;
	x2 = (xmax1 < xmax2) ? xmax1 : xmax2;
	y1 = (ymin1 > ymin2) ? ymin1 : ymin2;
	y2 = (ymax1 < ymax2) ? ymax1 : ymax2;

	a1 = 0.0;

	if (x2 > x1 && y2 > y1)
	{
		a1 = (x2-x1)*(y2-y1);
	}
return(a1);
}

/*********************************************************************
**    I_FUNCTION     :  uig_creat_sec_pt(eptr)
**				Create a point entity from the secondary unibase in the 
**				new unibase 
**    PARAMETERS
**       INPUT  :
**				  eptr	- point record
**       OUTPUT :
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
************************************************************************/
uig_creat_sec_pt(eptr)
struct UM_point_rec *eptr;
{
	struct UM_point_rec new;

	new.key = eptr->key;
	ncl_retrieve_data_fixed(&new);
/*
.....Switch to new unibase.
*/
	ur_getu_work();
/*
.....Setup unibase storage space
*/
   ur_setup_data(UM_POINT_REL,&new,sizeof(struct UM_point_rec));
/*
.....Create unibase record
*/
   uig_create_geom(&new,0,0,0);
   UIG_unibase_entities++;
   ur_getu_second();		
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_reg_ent(chkey,factor,pos)
**			Determine if this entity has already been matched in this level with 
**			a better factor.
**
**    PARAMETERS
**       INPUT  :
**				  chkey		- key of macthing entity
**				  factor	- factor to compare with
**       OUTPUT :
**				  pos		- positon of the chkey in checkkey array.
**    RETURNS      : 0: entity not used or used with a worse factor
**					 0: entity has been used with a better factor
**    SIDE EFFECTS : none
**    WARNINGS     : none.
************************************************************************/
int uig_reg_ent(chkey,factor,pos)
UU_KEY_ID chkey;
UU_REAL factor;
int *pos;
{
	int i, cnt =0;

	if(UIG_regressive)
	{
		for (i=0; i<UIG_regcount;i++)
		{
			if (UIG_checkkeys[i] == chkey)
			{
				if (UIG_matchkeys[i] != 0 &&UIG_regfactor[i]<=factor) cnt=1;
				break;
			}
		}
		*pos = i;
	}
	return (cnt);
}
