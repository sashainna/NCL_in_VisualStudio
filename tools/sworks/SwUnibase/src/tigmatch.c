/*********************************************************************
**    NAME         :  tigmatch.c
**       CONTAINS:  Routines to compare iges translated entities
**                  with those in the secondary unibase to see if
**                  they are the same and should be labeled the
**                  same.  The routines that start with uig_match
**                  look for exact matches.  The routines that
**                  start with uig_comp try and match remaining
**                  geometries with something close to it.
**
**          uig_match_point(eptr)
**          uig_match_line(eptr)
**          uig_match_circle(eptr)
**          uig_match_plane(eptr)
**          uig_match_conic(eptr,labflg)
**          uig_match_conic2(eptr,cp,labflg)
**          uig_match_polyline(eptr)
**          uig_match_patern(eptr)
**          uig_match_pointvec(eptr)
**          uig_match_rbsplcrv(eptr)
**          uig_match_uvcvonsf(eptr)
**          uig_match_compcrv(eptr)
**          uig_match_surface(eptr)
**          uig_match_rbsplsrf(eptr)
**          uig_match_meshsurf(eptr)
**          uig_match_trimsrf(eptr)
**          uig_match_revsf(eptr)
**          uig_update_surface_prim()
**          uig_match_prim(eptr1,eptr2,num,labflg,rad,dist)
**          uig_match_remaining(num,unlabeled)
**          uig_label_remaining(unlabeled)
**          uig_match_check_maxnum(eptr,maxnum)
**          uig_exact_match(eptr, func)
**          uig_match_updatts(eptr,lev)
**          uig_set_layer_num(num)
**			uig_unused_sec()
**			uig_change_labels(num,unlab)
**			uig_match_check_maxnum1(eptr)
**
**    COPYRIGHT 1999 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			tigmatch.c , 24.3
**    DATE AND TIME OF LAST  MODIFICATION
**			01/27/14 , 13:38:57
*********************************************************************/
#include "tiges.h"
#include "tigdefs.h"
#include "tigsupp.h"
#include "class.h"
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
#include "tigglobal.h"

#define IGES_LARGE (UU_REAL) 1.0e+5
#define NCVPT 25

int tig_max_ptlab =0;
int tig_max_lnlab =0;
int tig_max_cilab =0;
int tig_max_pllab =0;
int tig_max_cnlab =0;
int tig_max_pnlab =0;
int tig_max_pvlab =0;
int UIG_color_sec= -1;
int UIG_layer_sec= -1;
/*
.....tig_unlabeled_ent holds how many of each type don't
.....have a match. The order of the geometries in the
.....array are  points, lines, circles, planes, conics,
.....paterns, pointvecs, curves, and surfaces.
*/

UU_LOGICAL UIG_matching_trimsf = UU_FALSE;
UU_REAL empty = 0.0;
void uig_match_updatts();
void uig_set_layer_num();

/*********************************************************************
**    I_FUNCTION     :  uig_match_point(eptr,labflg)
**       Compare translated point data with existing point data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   point record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_point(eptr,labflg)
struct UM_point_rec *eptr;
int labflg;
{
	struct UM_point_rec check;
	int status,next_tupleid;
	UU_LOGICAL um_cceqcc_tol();

	status = UU_FAILURE;
	next_tupleid = 1;
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
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
.....Compare the point data.
*/
		else if (um_cceqcc_tol(eptr->pt,check.pt,UIG_match_tol))
		{
/*
.....If the label is the standard ptXX, then we need to
.....keep track of the highest number used.  The remaining
.....points that were not matched will be labeled starting
.....at one higher than the highest.
*/
			if (labflg)
			{
				uig_match_check_maxnum(&check, &tig_max_ptlab);

				strcpy(eptr->label,check.label);
				eptr->subscr = check.subscr;
/*
.....We have used this point so mark it with
.....a label of @UN.
*/
				strcpy(check.label,"@UN");
				check.subscr = 0;
				ur_update_data_fixed(&check);
			}
			status = UU_SUCCESS;
			goto done;
		}
	}
done:;
	if (labflg)
	{
		if (status == UU_FAILURE)
			tig_unlabeled_ent[0] ++;
/*
.....Switch back to primary unibase.
*/
		ur_getu_work();
	}
	return(status);
}            
/*********************************************************************
**    I_FUNCTION     :  uig_match_line(eptr,labflg)
**       Compare translated line data with existing line data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr                   line record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_line(eptr,labflg)
struct UM_line_rec *eptr;
int labflg;
{
	struct UM_line_rec check;
	int status,next_tupleid;
	UU_LOGICAL um_cceqcc_tol();

	status = UU_FAILURE;
	next_tupleid = 1;
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
/*
.....Get lines from unibase.
*/
	while(ur_get_next_data_key(UM_LINE_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't bother comparing eptr with entities that have @UN label.
*/
		if (check.label[0] == '@')
			status = UU_FAILURE;
/*
.....Compare start and end points of the two lines to 
.....see if the are the same.
*/
		else if ((um_cceqcc_tol(eptr->spt,check.spt,UIG_match_tol) && 
			um_cceqcc_tol(eptr->ept,check.ept,UIG_match_tol)) ||
			(um_cceqcc_tol(eptr->ept,check.spt,UIG_match_tol) && 
			um_cceqcc_tol(eptr->spt,check.ept,UIG_match_tol)))
		{
			if (labflg)
			{
				uig_match_check_maxnum(&check, &tig_max_lnlab);
				strcpy(eptr->label,check.label);
				eptr->subscr = check.subscr;
/*
......We have used this line, so mark it with an @UN.
*/
				strcpy(check.label,"@UN");
				check.subscr = 0;
				ur_update_data_fixed(&check);
			}
			status = UU_SUCCESS;
			goto done;
		}
	}
done:;
	if (labflg)
	{
		if (status == UU_FAILURE)
			tig_unlabeled_ent[1] ++;
/*
.....Switch back to primary unibase.
*/
		ur_getu_work();
	}
	return(status);
}            
/*********************************************************************
**    I_FUNCTION     :  uig_match_circle(eptr,labflg)
**       Compare translated circle data with existing circle data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      circle record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_circle(eptr,labflg)
struct UM_circle_rec *eptr;
int labflg;
{
	struct UM_circle_rec check;
	int status,next_tupleid,same_dir;
	UU_LOGICAL um_cceqcc_tol();
	UM_coord spt_ci1,spt_ci2,ept_ci1,ept_ci2,tpt;
	UM_vector nnvec;
	struct UM_rotmatrix mx;
	UU_REAL tol = UIG_match_tol;
	UM_transf tfmat,tfmat2;

	status = UU_FAILURE;
	next_tupleid = 1;
/*
.....Moved transformation retrieval so it is called before
.....the unibase switch - ASF 11/07/13.
*/
	uc_retrieve_transf(eptr->key, tfmat);
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
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
.....Compare the circle data, starting with radius
*/
		else if(fabs(eptr->radius - check.radius)<UIG_match_tol)
		{
/*
.....Compare angles.
*/
			if(fabs(eptr->dang - check.dang)<UM_FUZZ)
			{	
/*
.....Compare center points.
*/
				if(um_cceqcc_tol(eptr->center,check.center,UIG_match_tol))
				{
/*
.....Compare the normal vectors.
*/
					um_negvc(eptr->nvec,nnvec);
					if (um_cceqcc_tol(eptr->nvec,check.nvec,UM_FUZZ)) same_dir=1;
					else if(um_cceqcc_tol(nnvec,check.nvec,UM_FUZZ)) same_dir=2;
					else same_dir = 0;

					if (same_dir > 0)
					{
/*
.....Compare the endpoints.
*/
						um_get_endpts(eptr,tfmat,spt_ci1,ept_ci1);
						uc_retrieve_transf(check.key, tfmat2);
						um_get_endpts(&check,tfmat2,spt_ci2,ept_ci2);
						if ((um_cceqcc_tol(spt_ci1,spt_ci2,tol) && same_dir==1) ||
							(um_cceqcc_tol(spt_ci1,ept_ci2,tol) && same_dir==2))
						{

/*
.....We found our match, now copy the label and if the
.....label is the standard ciXX keep track of highest XX.
*/
							if (labflg)
							{
								uig_match_check_maxnum(&check, &tig_max_cilab);

								strcpy(eptr->label,check.label);
								eptr->subscr = check.subscr;
/*
.....We have used this circle so mark it with an @UN.
*/
								strcpy(check.label,"@UN");
								check.subscr = 0;
								ur_update_data_fixed(&check);
							}
							status = UU_SUCCESS;
							goto done;
						}
					}
				}
			}
		}
	}
done:;
	if (labflg)
	{
		if (status == UU_FAILURE)
			tig_unlabeled_ent[2] ++;
/*
.....Switch back to primary unibase.
*/
		ur_getu_work();
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_plane(eptr,labflg)
**       Compare translated plane data with existing plane data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      plane record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_plane(eptr,labflg)
struct NCL_nclpl_rec *eptr;
int labflg;
{
	struct NCL_nclpl_rec check;
	int status,next_tupleid;
	UU_LOGICAL um_cceqcc_tol();

	status = UU_FAILURE;
	next_tupleid = 1;
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
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
.....Compare the point that defines the plane.
*/
		else if(um_cceqcc_tol(eptr->pt,check.pt,UIG_match_tol))
		{
/*
.....Compare the normal vectors of the planes.
*/
			if(um_cceqcc_tol(eptr->nvec,check.nvec,UIG_match_tol))
			{
/*
.....Found a match, copy label into eptr, and check to see
.....if the label is the standard plXX and keep track of
.....the highest XX.
*/
				if (labflg)
				{
					uig_match_check_maxnum(&check, &tig_max_pllab);

					strcpy(eptr->label,check.label);
					eptr->subscr = check.subscr;
/*
.....We have used the plane so label it with @UN.
*/
					strcpy(check.label,"@UN");
					check.subscr = 0;
					ur_update_data_fixed(&check);
				}
				status = UU_SUCCESS;
				goto done;
			}
		}
	}
done:;
	if (labflg)
	{
		if (status == UU_FAILURE)
			tig_unlabeled_ent[3] ++;
/*
.....Switch back to primary unibase.
*/
		ur_getu_work();
	}
	return(status);
}
 
/*********************************************************************
**    I_FUNCTION     :  uig_match_conic(eptr,labflg)
**       Compare translated conic data with existing conic data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      conic record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_conic(eptr,labflg)
struct UM_conic_rec *eptr;
int labflg;
{
	struct UM_evcrvout evout1;
	struct UM_transf_rec trans1;
	int status,i;
	UU_REAL uparm;
	UM_coord cp[25];

	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);

	um_init_evcrvout(&eptr,&evout1);
/*
.....Evaluate points on the curves.
*/
	for (i=0;i<=24; i++)
	{
		uparm = i/24.0;
		um_ev4_conic(UM_POINT,uparm,eptr,trans1.tfmat,&evout1);
		um_vctovc(evout1.cp,cp[i]);
	}
	status = uig_match_conic2(eptr,cp,labflg);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_conic2(eptr,cp,labflg)
**       Compare translated conic data with existing conic data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      conic record
**				cp			 evaluated points on curve
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_conic2(eptr,cp,labflg)
struct UM_conic_rec *eptr;
UM_coord *cp;
int labflg;
{
	struct UM_conic_rec ncheck,check;
	/*struct NCL_curve_rec ncheck;*/
	struct NCL_fixed_databag found;
	struct UM_evcrvout evout2;
	struct UM_transf_rec trans2;
	int status,next_tupleid,i;
	UU_REAL uparm;
	UU_LOGICAL um_cceqcc_tol();
	UM_coord spt,ept;
	int same_dir;

	status = UU_FAILURE;
	next_tupleid = 1;

/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
/*
.....Retriev conics from the unibase.
*/
	while(ur_get_next_data_key(UM_CONIC_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't bother comparing entity that has @UN for its label.
*/
		if (check.label[0] !='@' && (labflg || eptr->key != check.key))
		{
      	ncl_retrieve_data_fixed(&check);
   		um_init_evcrvout(&check,&evout2);
/*
.....Compare the conic data, first make sure they
.....are the same type of conic.
*/
			if( eptr->type == check.type)
			{
				status = UU_SUCCESS;
				trans2.key = check.key;
				ur_retrieve_transf(&trans2);
				um_get_endpts(&check,trans2.tfmat,spt,ept);
/*
.....Determine if same direction should be used for comparison.
*/
				if (um_cceqcc_tol(cp[0],spt,UIG_match_tol) &&
					um_cceqcc_tol(cp[24],ept,UIG_match_tol)) same_dir=1;
				else if(um_cceqcc_tol(cp[0],ept,UIG_match_tol) &&
					um_cceqcc_tol(cp[24],spt,UIG_match_tol)) same_dir=2;
				else same_dir = 0;
/*
.....Evaluate points on the curves.
*/
				for (i=0;i<=24 && status == UU_SUCCESS; i++)
				{
					if (same_dir < 2)
						uparm = i/24.0;
					else
						uparm = (((UU_REAL)(24.0 - i))/24.0);
					um_ev4_conic(UM_POINT,uparm,&check,trans2.tfmat,&evout2);
					if(!um_cceqcc_tol(cp[i],evout2.cp,UIG_match_tol))
						status = UU_FAILURE;
				}
				if (same_dir > 0 && status == UU_FAILURE)
				{
					same_dir = 3 - same_dir;
					for(i=0;i<=24 && status == UU_SUCCESS;i++)
					{
						if (same_dir < 2)
							uparm = i/24.0;
						else
							uparm = (((UU_REAL)(24.0 - i))/24.0);
						um_ev4_conic(UM_POINT,uparm,&check,trans2.tfmat,&evout2);
						if(!um_cceqcc_tol(cp[i],evout2.cp,UIG_match_tol))
							status = UU_FAILURE;
					}
				}
/*
.....If all points matched, we found our curve.
*/
				if (status == UU_SUCCESS)
				{
					found.key = check.key;
					goto done;
				}	
			}
		}
	}
	next_tupleid = 1;
/*
.....Look to see if there is an NCL curve that matches.
*/
	while(ur_get_next_data_key(NCL_CURVE_REL,&next_tupleid, &ncheck.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&ncheck);
		if (ncheck.label[0] != '@' && (labflg || eptr->key != ncheck.key))
		{
			if( eptr->type == ncheck.type)
			{
				status = UU_SUCCESS;
				trans2.key = ncheck.key;
				ur_retrieve_transf(&trans2);
				um_get_endpts(&ncheck,trans2.tfmat,spt,ept);
/*
.....Determine if same direction should be used for comparison.
*/
				if (um_cceqcc_tol(cp[0],spt,UIG_match_tol) &&
					um_cceqcc_tol(cp[24],ept,UIG_match_tol)) same_dir=1;
				else if(um_cceqcc_tol(cp[0],ept,UIG_match_tol) &&
					um_cceqcc_tol(cp[24],spt,UIG_match_tol)) same_dir=2;
				else same_dir = 0;
/*
.....Check the 25 points to see if they are the same.
*/
				for(i=0;i<=24 && status == UU_SUCCESS;i++)
				{
					if (same_dir < 2)
						uparm = i/24.0;
					else
						uparm = (((UU_REAL)(24.0 - i))/24.0);
					um_ev4_conic(UM_POINT,uparm,&ncheck,trans2.tfmat,&evout2);
					if(!um_cceqcc_tol(cp[i],evout2.cp,UIG_match_tol))
   					status = UU_FAILURE;			
				}
/*
.....Check reversed direction for closed curves.
*/
				if (same_dir > 0 && status == UU_FAILURE)
				{
					same_dir = 3 - same_dir;
					for(i=0;i<=24 && status == UU_SUCCESS;i++)
					{
						if (same_dir < 2)
							uparm = i/24.0;
						else
							uparm = (((UU_REAL)(24.0 - i))/24.0);
						um_ev4_conic(UM_POINT,uparm,&ncheck,trans2.tfmat,&evout2);
						if(!um_cceqcc_tol(cp[i],evout2.cp,UIG_match_tol))
							status = UU_FAILURE;
					}
				}
				if (status == UU_SUCCESS)
				{
					found.key = ncheck.key;
					goto done;
				}
			}
		}
 	}

	done:;
		 if(labflg)
		 {
		 	if (status == UU_SUCCESS)
			{
				ncl_retrieve_data_fixed(&found);
				if (found.label[0] == 'C')
				{
					if (found.label[1] == 'N')
						uig_match_check_maxnum(&found, &tig_max_cnlab);
					else if (found.label[1] == 'V')
						uig_match_check_maxnum(&found, &tig_max_cvlab);
				}
				strcpy(eptr->label,found.label);
				eptr->subscr = found.subscr;
/*
.....We have used this conic so label it with @UN.
*/
				strcpy(found.label,"@UN");
				found.subscr = 0;
				ur_update_data_fixed(&found);
		  }
		  else
			tig_unlabeled_ent[4] ++;
/*
.....Switch back to primary unibase.
*/
		ur_getu_work();
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_polyline(eptr,labflg)
**       Compare translated polyline data with existing polyline data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      polyline record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_polyline(eptr,labflg)
struct UM_polyline_rec *eptr;
int labflg;
{
	struct UM_polyline_rec check;
	int status,next_tupleid,i;
	UU_REAL *pts1, *pts2;
	UU_LOGICAL um_cceqcc_tol();

	status = UU_FAILURE;
	next_tupleid = 1;

	pts1 = (UU_REAL *)uu_malloc(3*eptr->no_pt*sizeof(*pts1));
	pts2 = (UU_REAL *)uu_malloc(3*eptr->no_pt*sizeof(*pts2));
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
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
.....Compare the data for the points.
*/
			i = 0;
			status = UU_SUCCESS;
			pts1 = eptr->pt;
			pts2 = check.pt;
			while(i<eptr->no_pt&& status == UU_SUCCESS)
			{
				if(!um_cceqcc_tol(pts1,pts2,UIG_match_tol))
					status = UU_FAILURE;
				pts1+=3;
				pts2+=3;
				i++;
			}
			if (status == UU_SUCCESS)
			{
				if (labflg)
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
				}
				goto done;
			}
		}
	}
done:;
	if (labflg)
	{
		if (status == UU_FAILURE)
			tig_unlabeled_ent[5] ++;
/*
.....Switch back to primary unibase.
*/
		ur_getu_work();
	}
	uu_free(pts1);
	uu_free(pts2);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_patern(eptr,labflg)
**       Compare translated patern data with existing patern data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      patern record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_patern(eptr,labflg)
struct NCL_patern_rec *eptr;
int labflg;
{
	struct NCL_patern_rec check;
	int status,next_tupleid,i,numpts;
	UU_REAL *pts1,*pts2;
	UU_LOGICAL um_cceqcc_tol();

	status = UU_FAILURE;
	next_tupleid = 1;
	pts1 = (UU_REAL *)uu_malloc(eptr->no_patpnt*sizeof(*pts1));
	pts2 = (UU_REAL *)uu_malloc(eptr->no_patpnt*sizeof(*pts2));
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
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
		if ((check.label[0] == '@') || (!labflg && (eptr->key == check.key)))
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
					i = 0;
					pts1 = eptr->patpnt;
					pts2 = check.patpnt;
					status = UU_SUCCESS;
/*
......Compare the point data.
*/
					while(i<numpts&& status == UU_SUCCESS)
					{
						if(!um_cceqcc_tol(pts1,pts2,UIG_match_tol))
							status = UU_FAILURE;
						pts1 +=3;
						pts2 +=3;
						i++;
					}
					if (status == UU_SUCCESS)
/*
.....A match was found so copy the label into eptr and check
.....to see if the standard label pnXX was being used, if so
.....keep track of the highest XX.
*/
					{
						if (labflg)
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
						}
						goto done;
					}
				}
			}
		}
	}
done:;
	if (labflg)
	{
		if (status == UU_FAILURE)
			tig_unlabeled_ent[5] ++;
/*
.....Switch back to primary unibase.
*/
		ur_getu_work();
	}
	uu_free(pts1);
	uu_free(pts2);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_pointvec(eptr,labflg)
**       Compare translated pointvec data with existing pointvec data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      pointvec record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_pointvec(eptr,labflg)
struct NCL_nclpv_rec *eptr;
int labflg;
{
	struct NCL_nclpv_rec check;
	int status,next_tupleid;
	UU_LOGICAL um_cceqcc_tol();

	status = UU_FAILURE;
	next_tupleid = 1;
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
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
/*
.....Compare the point data.
*/
		else if (um_cceqcc_tol(eptr->pt,check.pt,UIG_match_tol))
		{
/*
.....Compare the vector data.
*/
			if(um_cceqcc_tol(eptr->ve,check.ve,UIG_match_tol))
			{
				if (labflg)
				{
/*
.....Found a match, so copy label and keep track of the 
.....highest label number check.label used the standard pvXX
.....as its label.
*/
					uig_match_check_maxnum(&check, &tig_max_pvlab);

					strcpy(eptr->label,check.label);
					eptr->subscr = check.subscr;
/*
.....Update check.label with an @UN label to indicate 
.....it has been used already.
*/
					strcpy(check.label,"@UN");
					check.subscr = 0;
					ur_update_data_fixed(&check);
				}
				status = UU_SUCCESS;
				goto done;
			}
		}
	}
/*
.....Finished, switch back to primary unibase.
*/
done:;
	if (labflg)
	{
		if (status == UU_FAILURE)
			tig_unlabeled_ent[6] ++;
		ur_getu_work();
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_rbsplcrv(eptr,labflg)
**       Compare translated rbsplcrv data with existing rbsplcrv data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      rbsplcrv record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_rbsplcrv(eptr,labflg)
struct UM_rbsplcrv_rec *eptr;
int labflg;
{
	struct UM_evcrvout evout1;
	struct UM_transf_rec trans1;
	UM_coord cp[NCVPT];
	int status,i;
	UU_REAL uparm;

	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);

   um_init_evcrvout(&eptr,&evout1);
	
/*
.....Evaluate the supplied curve
*/
	for (i=0;i<NCVPT;i++)
	{
		uparm = (UU_REAL)i/((UU_REAL)NCVPT-1.);
		um_ev7_rbsplcrv(UM_POINT,uparm,eptr,trans1.tfmat,&evout1);
		um_vctovc(evout1.cp,cp[i]);
	}
	status = uig_match_rbsplcrv2(eptr,cp,labflg);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_rbsplcrv2(cp,labflg)
**       Compare translated rbsplcrv data with existing rbsplcrv data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      pointer to curve.
**          cp        evaluated points on curve.
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_rbsplcrv2(eptr,cp,labflg)
struct UM_rbsplcrv_rec *eptr;
UM_coord *cp;
int labflg;
{
	struct UM_rbsplcrv_rec check;
	struct NCL_curve_rec ncheck;
	struct NCL_fixed_databag found;
	struct UM_evcrvout evout2;
	struct UM_transf_rec trans2;
	int status,next_tupleid,i,same_dir;
	UU_REAL uparm;
	UU_LOGICAL um_cceqcc_tol();
	UM_coord spt,ept;

	status = UU_FAILURE;
	next_tupleid = 1;
	
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
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
		if (check.label[0] !='@' && (labflg || eptr->key != check.key))
/*
.....Compare the curve data by evaluating both curves at 25
.....different u paramters and comparing the points.
*/
		{
			status = UU_SUCCESS;
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
      	ncl_retrieve_data_fixed(&check);
   		um_init_evcrvout(&check,&evout2);
			um_get_endpts(&check,trans2.tfmat,spt,ept);
/*
.....Determine if same direction should be used for comparison.
*/
			if (um_cceqcc_tol(cp[0],spt,UIG_match_tol) &&
				um_cceqcc_tol(cp[NCVPT-1],ept,UIG_match_tol)) same_dir=1;
			else if(um_cceqcc_tol(cp[0],ept,UIG_match_tol) &&
				um_cceqcc_tol(cp[NCVPT-1],spt,UIG_match_tol)) same_dir=2;
			else 
			{
				status = UU_FAILURE;
				same_dir = 0;
			}
			for(i=0;i<NCVPT && status == UU_SUCCESS;i++)
			{
				if (same_dir == 1)
					uparm = (UU_REAL)i/((UU_REAL)NCVPT-1.);
				else
					uparm = ((UU_REAL)(NCVPT-i-1))/((UU_REAL)NCVPT-1.);
				um_ev7_rbsplcrv(UM_POINT,uparm,&check,trans2.tfmat,&evout2);
				if(!um_cceqcc_tol(cp[i],evout2.cp,UIG_match_tol))
					status = UU_FAILURE;
			}
/*
.....Check reversed direction for closed curves.
*/
			if (same_dir > 0 && status == UU_FAILURE && eptr->closdinu &&
				check.closdinu)
			{
				same_dir = 3 - same_dir;
				for(i=0;i<NCVPT && status == UU_SUCCESS;i++)
				{
					if (same_dir == 1)
						uparm = (UU_REAL)i/((UU_REAL)NCVPT-1.);
					else
						uparm = ((UU_REAL)(NCVPT-i-1))/((UU_REAL)NCVPT-1.);
					um_ev7_rbsplcrv(UM_POINT,uparm,&check,trans2.tfmat,&evout2);
					if(!um_cceqcc_tol(cp[i],evout2.cp,UIG_match_tol))
						status = UU_FAILURE;
				}
			}
/*
.....If all points matched, we found our curve.
*/
			if (status == UU_SUCCESS)
			{
				found.key = check.key;
				goto done;
			}
		}
	}
	next_tupleid = 1;
/*
.....Look to see if there is an NCL curve that matches.
*/
	while(ur_get_next_data_key(NCL_CURVE_REL,&next_tupleid, &ncheck.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&ncheck);
		if (ncheck.label[0] != '@' && (labflg || eptr->key != ncheck.key))
		{
			status = UU_SUCCESS;
			trans2.key = ncheck.key;
			ur_retrieve_transf(&trans2);
			um_get_endpts(&ncheck,trans2.tfmat,spt,ept);
/*
.....Determine if same direction should be used for comparison.
*/
			if (um_cceqcc_tol(cp[0],spt,UIG_match_tol) &&
				um_cceqcc_tol(cp[NCVPT-1],ept,UIG_match_tol)) same_dir=1;
			else if(um_cceqcc_tol(cp[0],ept,UIG_match_tol) &&
				um_cceqcc_tol(cp[NCVPT-1],spt,UIG_match_tol)) same_dir=2;
			else
			{
				same_dir = 0;
				status = UU_FAILURE;
			}			
/*
.....Check the 25 points to see if they are the same.
*/
			for(i=0;i<NCVPT && status == UU_SUCCESS;i++)
			{
				if (same_dir == 1)
					uparm = (UU_REAL)i/((UU_REAL)NCVPT-1.);
				else
					uparm = ((UU_REAL)(NCVPT-i-1))/((UU_REAL)NCVPT-1.);
				ncl_ev7_nclcrv(UM_POINT,uparm,&ncheck,trans2.tfmat,&evout2);
				if(!um_cceqcc_tol(cp[i],evout2.cp,UIG_match_tol))
					status = UU_FAILURE;
			}
			if (same_dir > 0 && status == UU_FAILURE && eptr->closdinu &&
				check.closdinu)
			{
				same_dir = 3 - same_dir;
				for(i=0;i<NCVPT && status == UU_SUCCESS;i++)
				{
					if (same_dir == 1)
						uparm = (UU_REAL)i/((UU_REAL)NCVPT-1.);
					else
						uparm = ((UU_REAL)(NCVPT-i-1))/((UU_REAL)NCVPT-1.);
					um_ev7_rbsplcrv(UM_POINT,uparm,&ncheck,trans2.tfmat,&evout2);
					if(!um_cceqcc_tol(cp[i],evout2.cp,UIG_match_tol))
						status = UU_FAILURE;
				}
			}
			if (status == UU_SUCCESS)
			{
				found.key = ncheck.key;
				goto done;
			}
		}
	}
done:;
	if (labflg)
	{
		if (status == UU_SUCCESS)
		{
			ncl_retrieve_data_fixed(&found);

			uig_match_check_maxnum(&found, &tig_max_cvlab);

			strcpy(eptr->label,found.label);
			eptr->subscr = found.subscr;
/*
.....Update found.label with @UN, since we found its match.
*/
			strcpy(found.label,"@UN");
			found.subscr = 0;
			ur_update_data_fixed(&found);
		}
		else
			tig_unlabeled_ent[7] ++;
/*
.....Finished, switch back to primary unibase.
*/
		ur_getu_work();
	}
	return(status);
}            

/*********************************************************************
**    I_FUNCTION     :  uig_match_uvcvonsf(eptr,labflg)
**       Compare translated uvcvonsf data with existing uvcvonsf data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      uvcvonsf record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_uvcvonsf(eptr,labflg)
struct UM_uvcvonsf_rec *eptr;
int labflg;
{
	struct UM_uvcvonsf_rec check;
	struct UM_evcrvout evout1,evout2;
	struct UM_transf_rec trans1,trans2;
	int status,next_tupleid,i;
	UU_REAL uparm;
	UU_LOGICAL um_cceqcc_tol();
	UM_transf tfmat;
	UM_coord spt1,spt2,ept1,ept2;
	int same_dir;

	status = UU_FAILURE;
	next_tupleid = 1;
	ncl_retrieve_data_fixed(eptr);
	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);
   um_init_evcrvout(&eptr,&evout1);
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
/*
.....Retrieve surface splines from the unibase.
*/
	while (ur_get_next_data_key(UM_UVCVONSF_REL,&next_tupleid, &check.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't bother comparing entities that have @UN labels.
*/
		if (check.label[0] != '@' && (labflg || eptr->key != check.key))
/*
.....Evaluate both curves at 25 different u values,
.....if the points match, then we have the same curve.
*/
		{
			status = UU_SUCCESS;
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
      	ncl_retrieve_data_fixed(&check);
   		um_init_evcrvout(&check,&evout2);
			if (labflg) ur_getu_work();
			uc_retrieve_transf(eptr->key, tfmat);
			um_get_endpts(eptr,tfmat,spt1,ept1);
			if (labflg) ur_getu_second();
			uc_retrieve_transf(check.key, tfmat);
			um_get_endpts(&check,tfmat,spt2,ept2);
/*
.....Determine if same direction should be used for comparison.
*/
			if (um_cceqcc_tol(spt1,spt2,UIG_match_tol) &&
				um_cceqcc_tol(ept1,ept2,UIG_match_tol)) same_dir=1;
			else if(um_cceqcc_tol(spt1,ept2,UIG_match_tol) &&
				um_cceqcc_tol(ept1,spt2,UIG_match_tol)) same_dir=2;
			else 
			{
				status = UU_FAILURE;
				same_dir = 0;
			}
			for(i=0;i<=24 && status == UU_SUCCESS;i++)
			{
				if (same_dir == 1)
					uparm = i/24.0;
				else
					uparm = ((UU_REAL)(24-i))/24.0;
				uig_ev13_uvcvonsf(UM_POINT,uparm,&check,trans2.tfmat,&evout2);
/*
.....Switch back to the working unibase because the evaluator
.....will need to get the base surface.
*/
				if (labflg) ur_getu_work();
				uig_ev13_uvcvonsf(UM_POINT,uparm,eptr,trans1.tfmat,&evout1);
/*
.....Now return back to the secondary unibase.
*/
				if (labflg) ur_getu_second();

				if(!um_cceqcc_tol(evout1.cp,evout2.cp,UIG_match_tol))
					status = UU_FAILURE;
			}
/*
.....Check reversed direction.
*/
			if (status == UU_FAILURE && eptr->closdinu && check.closdinu &&
				same_dir > 0)
			{
				same_dir = 3 - same_dir;
				for(i=0;i<=24 && status == UU_SUCCESS;i++)
				{
					if (same_dir == 1)
						uparm = i/24.0;
					else
						uparm = ((UU_REAL)(24-i))/24.0;
					uig_ev13_uvcvonsf(UM_POINT,uparm,&check,trans2.tfmat,&evout2);
					if (labflg) ur_getu_work();
					uig_ev13_uvcvonsf(UM_POINT,uparm,eptr,trans1.tfmat,&evout1);
					if (labflg) ur_getu_second();
					if(!um_cceqcc_tol(evout1.cp,evout2.cp,UIG_match_tol))
						status = UU_FAILURE;
				}
			}
/*
.....If all points matched, we found our curve.
*/
			if (status == UU_SUCCESS)
			{
				if (labflg)
				{
					uig_match_check_maxnum(&check, &tig_max_cvlab);

					strcpy(eptr->label,check.label);
					eptr->subscr = check.subscr;
/*
.....Update check.label since we have used this entity already.
*/
					strcpy(check.label,"@UN");
					check.subscr = 0;
					ur_update_data_fixed(&check);
				}
				goto done;
			}
		}
	}
/*
.....Finished, switch back to primary unibase.
*/
done:;
	if (labflg)
	{
		if (status == UU_FAILURE)
			tig_unlabeled_ent[7] ++;
		ur_getu_work();
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_match_compcrv(eptr,labflg)
**       Compare translated compcrv data with existing compcrv data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      composite curve record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_compcrv(eptr,labflg)
struct UM_compcrv_rec *eptr;
int labflg;
{
	struct UM_compcrv_rec check;
	struct UM_evcrvout evout1, evout2;
	struct UM_transf_rec trans1,trans2;
	int status,next_tupleid,i;
	UU_REAL uparm;
	UU_LOGICAL um_cceqcc_tol();
	UM_coord spt1,ept1,spt2,ept2;
	int same_dir;

	status = UU_FAILURE;
	next_tupleid = 1;
	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);
	um_init_evcrvout(&eptr,&evout1);
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
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
		if (check.label[0] != '@' && (labflg || eptr->key != check.key))
		{
			if (labflg) ur_getu_work();
			trans2.key = eptr->key;
			ur_retrieve_transf(&trans2);
			um_get_endpts(eptr,trans2.tfmat,spt1,ept1);
			if (labflg) ur_getu_second();
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
			status = UU_SUCCESS;
      	ncl_retrieve_data_fixed(&check);
			um_get_endpts(&check,trans2.tfmat,spt2,ept2);
   		um_init_evcrvout(&check,&evout2);
			if (um_cceqcc_tol(spt1,spt2,UIG_match_tol) &&
				um_cceqcc_tol(ept1,ept2,UIG_match_tol)) same_dir=1;
			else if(um_cceqcc_tol(spt1,ept2,UIG_match_tol) &&
				um_cceqcc_tol(ept1,spt2,UIG_match_tol)) same_dir=2;
			else status = UU_FAILURE;
/*
.....Compare each curve at 25 different points.
*/
			for(i=0;i<=24 && status == UU_SUCCESS;i++)
			{
				if (same_dir == 1)
					uparm = i/24.0;
				else
					uparm = ((UU_REAL)(24-i))/24.0;
				um_ev5_compcrv(UM_POINT, uparm, &check, trans2.tfmat, &evout2);
/*
.....Switch to the working unibase so that the evaluator can
.....get the information about each component of the composite curve.
*/
				if (labflg) ur_getu_work();
				um_ev5_compcrv(UM_POINT, uparm, eptr, trans1.tfmat, &evout1);
/*
.....Switch to secondary unibase.
*/
				if (labflg) ur_getu_second();
				if(!um_cceqcc_tol(evout1.cp,evout2.cp,UIG_match_tol))
					status = UU_FAILURE;
			}
			if (status == UU_FAILURE && eptr->closdinu && check.closdinu &&
				same_dir > 0)
			{
				same_dir = 3 - same_dir;
				for(i=0;i<=24 && status == UU_SUCCESS;i++)
				{
					if (same_dir == 1)
						uparm = i/24.0;
					else
						uparm = ((UU_REAL)(24-i))/24.0;
					um_ev5_compcrv(UM_POINT, uparm, &check, trans2.tfmat, &evout2);
					if (labflg) ur_getu_work();
					um_ev5_compcrv(UM_POINT, uparm, eptr, trans1.tfmat, &evout1);
					if (labflg) ur_getu_second();
					if(!um_cceqcc_tol(evout1.cp,evout2.cp,UIG_match_tol))
						status = UU_FAILURE;
				}
			}
/*
.....If all the points matched, we have a matching curve.
*/
			if (status == UU_SUCCESS)
			{
				if (labflg)
				{
					uig_match_check_maxnum(&check, &tig_max_cvlab);

					strcpy(eptr->label,check.label);
					eptr->subscr = check.subscr;
/*
.....Indicate that this entity has been used by
.....setting the label to @UN.
*/
					strcpy(check.label,"@UN");
					check.subscr = 0;
					ur_update_data_fixed(&check);
				}
				goto done;
			}
		}
	}
/*
.....Finished, switch back to primary unibase.
*/
done:;
	if (labflg)
	{
		if (status == UU_FAILURE)
			tig_unlabeled_ent[7] ++;
		ur_getu_work();
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_surface(eptr,labflg)
**       Dispatch various surfaces to the appropriate surface-matching
**       function.
**    PARAMETERS
**       INPUT  :
**          eptr      surface record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_match_surface(eptr,labflg)
struct NCL_fixed_databag *eptr;
int labflg;
{
	int found;

	switch(eptr->rel_num)
	{
		case UM_RBSPLSRF_REL:
			found = uig_match_rbsplsrf(eptr,labflg);
			break;
		case NCL_MESHSURF_REL:
			found = uig_match_meshsurf(eptr,labflg);
			break;
		case NCL_TRIMSF_REL:
			found = uig_match_trimsrf(eptr,labflg);
			break;
		case NCL_REVSURF_REL:
			found = uig_match_revsf(eptr,labflg);
			break;
		default:
			found = UU_FAILURE;
			break;
	}

	return (found);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_2sf(eptr1,tf1,eptr2,tf2,pts)
**       Determine if 2 surfaces match.
**    PARAMETERS
**       INPUT  :
**          eptr1   - First surface record
**          tf1     - First surface transformation.
**          eptr2   - Second surface record
**          tf2     - Second surface transformation.
**          pts     - grid of points from 1st sf
**          labflg  - labeling flag
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_match_2sf(eptr1,tf1,eptr2,tf2,pts,labflg)
struct NCL_fixed_databag *eptr1, *eptr2;
UM_transf tf1, tf2;
UM_coord pts[25][25];
int labflg;
{
	int found;

	if (eptr1->rel_num != eptr2->rel_num)
		found = UU_FAILURE;
	else
	{
		switch(eptr1->rel_num)
		{
			case UM_RBSPLSRF_REL:
				found = uig_match_2rbsplsrf(eptr1,tf1,eptr2,tf2,pts,labflg);
				break;
			case NCL_REVSURF_REL:
				found = uig_match_2revsf(eptr1,tf1,eptr2,tf2,labflg);
				break;
			default:
				found = uig_match_2srf_points(eptr2,tf2,pts);
				break;
		}
	}

	return (found);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_rbsplsrf(eptr,labflg)
**       Compare translated rbsplsrf data with existing rbsplsrf data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      rbsplsrf record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_rbsplsrf(eptr,labflg)
struct UM_rbsplsrf_rec *eptr;
int labflg;
{
	struct UM_rbsplsrf_rec check;
	struct UM_transf_rec trans1,trans2;
	struct NCL_surface_rec ncheck;
	struct NCL_fixed_databag found;
	int status,next_tupleid;
	UM_coord pts[25][25];

	status = UU_FAILURE;
	next_tupleid = 1;
	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);
	uig_match_get_srf_points(eptr,trans1.tfmat,pts);
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
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
		if (check.label[0] != '@' && (labflg || eptr->key != check.key))
		{
			status = UU_SUCCESS;
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
			if (eptr->primitive != check.primitive)
				status = UU_FAILURE;
/*
.....If the primitives match and they are greater than two,
.....check to see if the parameters match.
*/
			else if (eptr->primitive > 2)
				status = uig_match_prim(eptr,&check,0,labflg,&empty,&empty);
			else
			{
/*
.....If the surface isn't one of the special primitive
.....types, check the surfaces at 625 different points
.....to see if they match.
*/
				status = uig_match_2srf_points(&check,trans2.tfmat,pts);
			}
			if (status == UU_SUCCESS)
			{
				found.key = check.key;
				goto done;
			}
		}
	}
	next_tupleid = 1;
/*
.....See if there is an NCL surface to match this rbsplsrf.
*/
	while(ur_get_next_data_key(NCL_SURF_REL,&next_tupleid, &ncheck.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&ncheck);
/*
.....Don't compare surfaces that have @UN labels, this means
.....that check has already been matched or check is an underlying
.....surface and underlying entities have already been taken 
.....care of.
*/

		if (ncheck.label[0] != '@' && (labflg || eptr->key != ncheck.key))
		{
			status = UU_SUCCESS;
			ncl_retrieve_data_fixed(&ncheck);
			if (eptr->primitive != ncheck.primitive)
				status = UU_FAILURE;
			else if (eptr->primitive > 2)
				status = uig_match_prim(eptr,&ncheck,0,labflg,&empty,&empty);
			else
			{
/*
.....If the surfaces are not any of the special primitive
.....types, then check at 625 different points on the surface
.....to see if they match.
*/
				trans2.key = ncheck.key;
				ur_retrieve_transf(&trans2);
				status = uig_match_2srf_points(&ncheck,trans2.tfmat,pts);
			}
			if (status == UU_SUCCESS)
			{
				found.key = ncheck.key;
				goto done;
			}
		}
	}
done:;
	if (labflg)
	{
		if(status == UU_SUCCESS)
		{
/*
.....We found a match so copy over the label, then check to
.....see if the standard sfXX label is being used and keep
.....track of the highest number used.
*/
			ncl_retrieve_data_fixed(&found);
			uig_match_check_maxnum(&found, &tig_max_sflab);

			strcpy(eptr->label,found.label);
			eptr->subscr = found.subscr;
/*
.....Update check.label with @UN to indicate that
.....a match has been found for this surface.
*/
			strcpy(found.label,"@UN");
			found.subscr = 0;
			ur_update_data_fixed(&found);
		}
		else
			tig_unlabeled_ent[8] ++;
/*
.....Finished, switch back to primary unibase.
*/
		ur_getu_work();
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_2rbsplsrf(eptr1,tf1,eptr,tf2,pts)
**       Compare translated rbsplsrf data with existing rbsplsrf data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr1     - First rbsplsrf record
**          tf1       - First rbsplsrf transformation.
**          eptr2     - Second rbsplsrf record
**          tf2       - Second rbsplsrf transformation.
**          pts       - Grid of points from 1st sf.
**          labflg    - labeling flag
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_2rbsplsrf(eptr1,tf1,eptr2,tf2,pts,labflg)
struct UM_rbsplsrf_rec *eptr1, *eptr2;
UM_transf tf1, tf2;
UM_coord pts[25][25];
int labflg;
{
	int status;

	if (eptr1->primitive != eptr2->primitive)
		status = UU_FAILURE;
/*
.....If the primitives match and they are greater than two,
.....check to see if the parameters match.
*/
	else if (eptr1->primitive > 2)
		status = uig_match_prim(eptr1,eptr2,0,labflg,&empty,&empty);
	else
	{
/*
.....If the surface isn't one of the special primitive
.....types, check the surfaces at 625 different points
.....to see if they match.
*/
		status = uig_match_2srf_points(eptr2,tf2,pts);
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_2srf_points(eptr1,tf1,pts)
**       Compare translated (generic) srf data with existing srf data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr1, tf1     - 1st srf record & transformation
**          pts            - Grid of points from second sf.
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_2srf_points(eptr1,tf1,pts)
struct NCL_fixed_databag *eptr1;
UM_transf tf1;
UM_coord pts[25][25];
{
	int status,i,j,k;
	struct UM_evsrfout evout1;
	UU_REAL ust,vst,udir,vdir;
	static UU_REAL ua[] = {0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0};
	static UU_REAL va[] = {0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0};
	UU_LOGICAL um_cceqcc_tol();
	
	for (k=0;k<8;k++)
	{
		status = UU_SUCCESS;
		ust = ua[k];
		vst = va[k];
		udir = 1.0-ust*2.0;
		vdir = 1.0-vst*2.0;
		for(i=0;i<=24 && status == UU_SUCCESS; i++)
		{
			for(j=0;j<=24 && status == UU_SUCCESS; j++)
			{
				uc_evsrf(UM_POINT,ust,vst,eptr1,tf1,&evout1);
				if(!um_cceqcc_tol(evout1.sp,pts[i][j],UIG_match_tol))
					status = UU_FAILURE;
				if (k%2)
					ust += udir/24.0;
				else
					vst += vdir/24.0;
			}
			if (k%2)
			{
				vst += vdir/24.0;
				ust = ua[k];
			}
			else
			{
				ust += udir/24.0;
				vst = va[k];
			}
		}
		if (status == UU_SUCCESS) break;
	}

	return (status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_match_get_srf_points(eptr1,tf1,pts)
**       Evaluate a 25 by 25 grid of points on a surface.
**    PARAMETERS
**       INPUT  :
**          eptr1, tf1           surface record & transformation
**       OUTPUT :
**          pts        - Grid of points.
**    RETURNS      : UU_SUCCESS if no error, otherwise UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_get_srf_points(eptr1,tf1,pts)
struct NCL_fixed_databag *eptr1;
UM_transf tf1;
UM_coord pts[25][25];
{
	int status,i,j;
	struct UM_evsrfout evout1;
	UU_REAL uparm,vparm;

	status = UU_SUCCESS;
	for(i=0;i<=24 && status == UU_SUCCESS; i++)
	{
		uparm = i/24.0;
		for(j=0;j<=24 && status == UU_SUCCESS; j++)
		{
			vparm = j/24.0;
			status = uc_evsrf(UM_POINT,uparm,vparm,eptr1,tf1,&evout1);
			um_vctovc(evout1.sp,pts[i][j]);
		}
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_meshsurf(eptr,labflg)
**       Compare translated meshsurf data with existing meshsurf data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      meshsurf record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_meshsurf(eptr,labflg)
struct NCL_meshsf_rec *eptr;
int labflg;
{
	struct NCL_meshsf_rec check;
	struct UM_transf_rec trans1, trans2;
	int status,next_tupleid;
	UM_coord pts[25][25];

	status = UU_FAILURE;
	next_tupleid = 1;
	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);
	uig_match_get_srf_points((struct NCL_fixed_databag *)eptr,trans1.tfmat,pts);
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
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
		if (check.label[0] != '@' && (labflg || eptr->key != check.key))
/*
.....Compare the surfaces at 625 different points.
*/
		{
			status = UU_SUCCESS;
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
			status = uig_match_2srf_points((struct NCL_fixed_databag *)&check,trans2.tfmat,pts);
/*
.....Found a match.
*/
			if (status == UU_SUCCESS)
			{
				if (labflg)
				{
					uig_match_check_maxnum(&check, &tig_max_sflab);

					strcpy(eptr->label,check.label);
					eptr->subscr = check.subscr;

					strcpy(check.label,"@UN");
					check.subscr = 0;
					ur_update_data_fixed(&check);
				}
				goto done;
			}
		}
	}
/*
.....Finished, switch back to primary unibase.
*/
done:;
	if (labflg)
	{
		if (status == UU_FAILURE)
			tig_unlabeled_ent[8] ++;
		ur_getu_work();
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_trimsrf(eptr,labflg)
**       Compare translated trimsrf data with existing trimsrf data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      trimsrf record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_trimsrf(eptr,labflg)
struct NCL_trimsf_rec *eptr;
int labflg;
{
	struct NCL_trimsf_rec check;
	struct UM_rbsplsrf_rec base1,base2;
	struct UM_transf_rec trans1,trans2;
	int status,next_tupleid, numcv1, numcv2;
	UM_2Dcoord ummn1, vmmn1, ummn2, vmmn2;
	UM_srf_boundary boundary_box1, boundary_box2;
	UU_REAL tol;
	UM_coord pts[25][25],p1,p2,p3,p4;
	struct UM_evsrfout evout1;
	UU_REAL u1,u2,u3,u4,v1,v2,v3,v4;

	tol = UIG_match_tol;
	status = UU_FAILURE;
	UIG_matching_trimsf = UU_TRUE;
	next_tupleid = 1;
	
/*
.....Get the base surface, we will compare these first.
*/
	base1.key = eptr->bs_key;
	ncl_retrieve_data_fixed(&base1);
/*
.....Use the transformation matrix associated with the
.....trimmed surface not the base surface.
*/
	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);
/*
.....Get array of points and trim boundary max and min points
*/
	uig_match_get_srf_points((struct NCL_fixed_databag *)&base1,trans1.tfmat,pts);
	boundary_box1.toler = tol;
	um_pre_srf_bndr_box(eptr,&boundary_box1);
	numcv1 = boundary_box1.nb;
	um_vctovc_2d (boundary_box1.ummx, &ummn1);
	um_vctovc_2d (boundary_box1.vmmx, &vmmn1);
	u1 = ummn1[0];
	u2 = ummn1[1];
	v1 = vmmn1[0];
	v2 = vmmn1[1];
	status = uc_evsrf(UM_POINT,u1,v1,&base1,trans1.tfmat,&evout1);
	um_vctovc(evout1.sp,p1);
	status = uc_evsrf(UM_POINT,u2,v2,&base1,trans1.tfmat,&evout1);
	um_vctovc(evout1.sp,p2);
/*
.....Switch to secondary unibase.
*/
	if (labflg) 
		ur_getu_second();
/*
.....If eptr doesn't not yet have a uvkey, exit now, otherwise
.....a fatal error will occur when checking the boundary box.
*/
	if (eptr->uv_key == 0)
		goto done;
/*
.....Retrieve trimmed surfaces from the unibase.
*/
	while(ur_get_next_data_key(NCL_TRIMSF_REL,&next_tupleid, &check.key)>-1)
	{
		status = UU_FAILURE;
		next_tupleid++;
		ncl_retrieve_data_fixed(&check);
/*
.....Don't compare surfaces that have @UN labels, they
.....have already been matched.
*/
		if (check.label[0] != '@' && (labflg || eptr->key != check.key))
		{
			base2.key = check.bs_key;
/*
.....get the transformation matrix associated with check
*/
			ncl_retrieve_data_fixed(&base2);
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);

			if (uig_match_2sf((struct NCL_fixed_databag *)&base1,trans1.tfmat,(struct NCL_fixed_databag *)&base2,trans2.tfmat,pts,labflg) == UU_SUCCESS)
			{
/*
.....The trimmed surface evaluator evaluates the base surface, so
.....check the boundary box to make sure this is the same trimmed surface
.....not just the same base surface.
*/
				boundary_box2.toler = tol;

				um_pre_srf_bndr_box(&check,&boundary_box2);
				numcv2 = boundary_box2.nb;

				if (numcv1 == numcv2)
				{
					um_vctovc_2d (boundary_box2.ummx, &ummn2);
					um_vctovc_2d (boundary_box2.vmmx, &vmmn2);

					if ((((fabs(ummn1[0] - ummn2[0]) < UM_FUZZ)  &&
					      (fabs(ummn1[1] - ummn2[1]) < UM_FUZZ)) ||
					      (fabs(1.0 - ummn1[0] - ummn2[0]) < UM_FUZZ)  &&
					      (fabs(1.0 - ummn1[1] - ummn2[1]) < UM_FUZZ)) &&
					    (((fabs(vmmn1[0] - vmmn2[0]) < UM_FUZZ)  &&
					      (fabs(vmmn1[1] - vmmn2[1]) < UM_FUZZ)) ||
					      (fabs(1.0 - vmmn1[0] - vmmn2[0]) < UM_FUZZ)  &&
					      (fabs(1.0 - vmmn1[1] - vmmn2[1]) < UM_FUZZ)))

					{
						u3 = ummn2[0];
						u4 = ummn2[1];
						if ((fabs(1.0 - ummn1[0] - ummn2[0]) < UM_FUZZ)  &&
						    (fabs(1.0 - ummn1[1] - ummn2[1]) < UM_FUZZ))
						{
							u3 = ummn2[1];
							u4 = ummn2[0];
						}
						v3 = vmmn2[0];
						v4 = vmmn2[1];
						if ((fabs(1.0 - vmmn1[0] - vmmn2[0]) < UM_FUZZ)  &&
						    (fabs(1.0 - vmmn1[1] - vmmn2[1]) < UM_FUZZ))
						{
							v3 = vmmn2[1];
							v4 = vmmn2[0];
						}
						uc_evsrf(UM_POINT,u3,v3,&base2,trans2.tfmat,&evout1);
						um_vctovc(evout1.sp,p3);
						uc_evsrf(UM_POINT,u4,v4,&base2,trans2.tfmat,&evout1);
						um_vctovc(evout1.sp,p4);
						if(um_cceqcc_tol(p1,p3,tol) &&
						   um_cceqcc_tol(p2,p4,tol))
							status = UU_SUCCESS;
					}
				}
			}
			if(status == UU_SUCCESS)
			{
				if (labflg)
				{
/*
......Found a match, so copy label.
*/
					uig_match_check_maxnum(&check, &tig_max_sflab);

					strcpy(eptr->label,check.label);
					eptr->subscr = check.subscr;
/*
.....Change check.label to @UN to indicate that this surface
.....has been matched.
*/
					strcpy(check.label,"@UN");
					check.subscr = 0;
					ur_update_data_fixed(&check);
				}
				goto done;
			}
		}
	}
/*
.....Finished, switch back to primary unibase.
*/
done:;
	if (labflg)
	{
		if (status == UU_FAILURE)
			tig_unlabeled_ent[8] ++;
		ur_getu_work();
	}
	UIG_matching_trimsf = UU_FALSE;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_revsf(eptr,labflg)
**       Compare translated revsf data with existing revsf data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr      revsf record
**          labflg    1=labelling from secondary unibase,
**                    0=match only
**                    (not used)
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_revsf(eptr,labflg)
struct NCL_revsurf_rec *eptr;
int labflg;
{
	struct NCL_revsurf_rec check;
	struct UM_rbsplsrf_rec ncheck;
	struct UM_transf_rec trans1,trans2;
	struct NCL_fixed_databag found;
	int status,next_tupleid;
	UM_coord pts[25][25];

	status = UU_FAILURE;
	next_tupleid = 1;
	trans1.key = eptr->key;
	ur_retrieve_transf(&trans1);
/*
.....Switch to secondary unibase.
*/
	if (labflg) ur_getu_second();
/*
.....Retrieve sufaces from the unibase.
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
		if (check.label[0] != '@' && (labflg || eptr->key != check.key))
		{
			status = UU_SUCCESS;
			trans2.key = check.key;
			ur_retrieve_transf(&trans2);
			if (eptr->primitive != check.primitive)
				status = UU_FAILURE;
/*
.....If the primitives match and they are greater than three,
.....check to see if the parameters match.
*/
			else if (eptr->primitive > 2)
				status = uig_match_prim(eptr,&check,0,labflg,&empty,&empty);
			else
				status = uig_match_2revsf(eptr,trans1.tfmat,&check,trans2.tfmat,labflg);

			if(status == UU_SUCCESS)
			{
				found.key = check.key;
				goto done;
			}
		}
	}
	if (labflg) ur_getu_work();
	uig_match_get_srf_points((struct NCL_fixed_databag *)eptr,trans1.tfmat,pts);
	if (labflg) ur_getu_second();
	next_tupleid = 1;
/*
.....See if there is a rbsplsrf to match this surface of revolution.
*/
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
		if (ncheck.label[0] != '@' && (labflg || eptr->key != ncheck.key))
		{
			status = UU_SUCCESS;
			trans2.key = ncheck.key;
			ur_retrieve_transf(&trans2);
			if (eptr->primitive != ncheck.primitive)
				status = UU_FAILURE;
/*
.....If the primitives match and they are greater than two,
.....check to see if the parameters match.
*/
			else if (eptr->primitive > 2)
				status = uig_match_prim(eptr,&ncheck,0,labflg,&empty,&empty);
			else
			{
/*
.....If the surfaces are not any of the special primitive
.....types, then check at 625 different points on the surface
.....to see if they match.
*/
				trans2.key = ncheck.key;
				ur_retrieve_transf(&trans2);

				status = uig_match_2srf_points((struct NCL_fixed_databag *)&ncheck,trans2.tfmat,pts);
			}

			if(status == UU_SUCCESS)
			{
				found.key = ncheck.key;
				goto done;
			}
		}
	}

done:;
	if (labflg)
	{
		if(status == UU_SUCCESS)
		{
/*
......Found a match, so copy label.
*/
			ncl_retrieve_data_fixed(&found);
			uig_match_check_maxnum(&found, &tig_max_sflab);

			strcpy(eptr->label,found.label);
			eptr->subscr = found.subscr;
/*
.....Change check.label to @UN to indicate that this surface
.....has been matched.
*/
			strcpy(found.label,"@UN");
			found.subscr = 0;
			ur_update_data_fixed(&found);
		}
		else
			tig_unlabeled_ent[8] ++;
/*
.....Finished, switch back to primary unibase.
*/
		ur_getu_work();
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_2revsf(eptr1,tf1,eptr2,tf2,labflg)
**       Compare translated revsf data with existing revsf data
**       to see if there is a match for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          eptr1, tf1           1st revsf record & transformation
**          eptr2, tf2           2nd revsf record & transformation
**          labflg               labeling flag
**       OUTPUT :
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_2revsf(eptr1,tf1,eptr2,tf2,labflg)
struct NCL_revsurf_rec *eptr1, *eptr2;
UM_transf tf1, tf2;
int labflg;
{
	int status,i;
	UU_REAL tol,um_dcccc(),uparm;
	UM_coord pt1,pt2,npt;
	UM_vector vc1,vc2;
	UU_LOGICAL um_vcparall();
	
	status = UU_SUCCESS;

	if (eptr1->cvkey <= 0 || eptr1->cvkey <= 0)
		return (UU_FAILURE);

	tol = UIG_match_tol;
/*
..... compare the rotation angles
*/
	if (fabs (eptr1->offdist - eptr2->offdist) > tol) 
		return (UU_FAILURE);

	if (fabs (eptr1->sa - eptr2->sa) > tol) 
		return (UU_FAILURE);

	if (fabs (eptr1->ta - eptr2->ta) > tol) 
		return (UU_FAILURE);
/*
..... check if the rotation axes are same
*/
	um_cctmtf(eptr1->pta,tf1,pt1);
	um_cctmtf(eptr2->pta,tf2,pt2);
	um_vctmtf(eptr1->vca,tf1,vc1);
	um_vctmtf(eptr2->vca,tf2,vc2);
	if (!um_vcparall(vc1, vc2)) return (UU_FAILURE);
	um_nptln(pt1,pt2,vc2,npt);
	if (um_dcccc (pt1,npt) > tol) 
		return (UU_FAILURE);
	else
	{
/*
..... compare the generating curves
*/
		struct NCL_fixed_databag crv;
		struct UM_transf_rec trans;
		struct UM_evcrvout evout;
		UM_coord pts1[25];

		if (labflg) ur_getu_work();
		crv.key = eptr1->cvkey;
		if (ncl_retrieve_data_fixed (&crv) != 0)
			return (UU_FAILURE);
		trans.key = crv.key;
		ur_retrieve_transf(&trans);
		um_tftmtf (trans.tfmat, tf1, trans.tfmat);
   	um_init_evcrvout(&crv,&evout);
		for(i = 0; i < 25; i++)
		{
			uparm = i/24.0;
			uc_evcrv (UM_POINT,uparm,&crv,trans.tfmat,&evout);
			um_vctovc (evout.cp, pts1[i]);
		}
		
		if (labflg) ur_getu_second();
		crv.key = eptr2->cvkey;
		if (ncl_retrieve_data_fixed (&crv) != 0)
			return (UU_FAILURE);
		trans.key = crv.key;
		ur_retrieve_transf(&trans);
		um_tftmtf (trans.tfmat, tf2, trans.tfmat);
   	um_init_evcrvout(&crv,&evout);
		for(i = 0; i < 25; i++)
		{
			uparm = i/24.0;
			uc_evcrv (UM_POINT,uparm,&crv,trans.tfmat,&evout);
			if (um_dcccc (pts1[i],evout.cp) > tol) return (UU_FAILURE);
		}
	}

	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_prim(eptr1,eptr2,num,labflg,rad,dist)
**          Check to see if two surfaces of the same type
**          are actually the same surface.
**    PARAMETERS
**       INPUT  :
**          eptr1                  surface 1 record
**          eptr2                  surface 2 record
**          num                    indicates degree of matching to be done:
**                                 0 - exact matching (including corners)
**                                 1 - exact matching of the primitives
**                                 2 -
**                                 3 -
**          labflg                 labeling flag
**       OUTPUT :
**				rad						  Only meaningful if num is equal to 4
**	  										  Then it is used to indicate how
**											  close the current eptr2 radius is from
**											  the eptr1 radius.
**          dist                   Only meaningful if num is greater than
**                                 one.  Then it is used to indicate how
**                                 far away the current eptr2 point is from
**                                 eptr1 point.
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
uig_match_prim(eptr1,eptr2,num,labflg,rad,dist)
struct NCL_fixed_databag *eptr1;
struct NCL_fixed_databag *eptr2;
int num, labflg;
UU_REAL *rad;
UU_REAL *dist;
{

	int status, i;
	UM_int2 type1, type2;
	UM_real8 param1[16], param2[16];
	UM_vector v1,v2,vec_in1, vec_in2;
	UM_coord pt1, pt2, pt_in, pt_out;
	UU_REAL distance, um_dot(),dot;
	UU_REAL diff_ht, diff_rad, comp_fac;
/*
.....Since eptr1 is from the working unibase, we need to switch
.....unibases.  Get the primitive parameters of each surface.
*/
	if (labflg) ur_getu_work();
	ncl_get_sf_primdat(&eptr1->key,&type1,&param1);
	if (labflg) ur_getu_second();
	ncl_get_sf_primdat(&eptr2->key,&type2,&param2);
	status = UU_FAILURE;
	distance = 0.;

	switch(type1)
	{
		case NCLSF_PLANE:
			status = UU_SUCCESS;
/*
.....If num is zero or one, check all parameters
*/
			for (i = 0; i < 3; i++)
			{
				vec_in1[i] = param1[i];
				vec_in2[i] = param2[i];
			}
			um_unitvc (vec_in1,v1);
			um_unitvc (vec_in2,v2);
			dot = um_dot(v1, v2); 
			if (1.0 - fabs (dot) > UM_FUZZ)
				status = UU_FAILURE;
			else
			{
				distance = fabs (param1[3] - dot*param2[3]);
				if (num <= 1 && distance > UM_FUZZ)
					status = UU_FAILURE;
			}
			break;
		case NCLSF_SPHERE:
			status = UU_SUCCESS;
/*
.....Num equal to zero or one, then check all parameters
*/
			if (num <= 1)
			{
				for (i=0; i<4 && status == UU_SUCCESS; i++)
				{
					if (fabs(param1[i] - param2[i]) > UM_FUZZ)
					status = UU_FAILURE;
				}
			}
/*
.....Num equal to 2 match same center point, but can have different
.....radius.
*/
			else if (num == 2)
			{
				for (i=0; i<3 && status == UU_SUCCESS; i++)
				{
					if(fabs(param1[i] - param2[i])>UM_FUZZ)
					status = UU_FAILURE;
				}
			}
/*
.....look for closest center point
*/
			else
			{
				for (i = 0; i < 3; i++)
				{
					pt1[i] = param1[i];
					pt2[i] = param2[i];
				}
				distance = um_dcccc(pt1,pt2);
			}
			break;
/*
.....Cylinder
.....Cone
*/
		case NCLSF_CYLINDER:
		case NCLSF_CONE:
			status = UU_SUCCESS;
			for (i = 0; i < 3; i++)
			{
				vec_in1[i] = param1[i+3];
				vec_in2[i] = param2[i+3];
			}
			um_unitvc (vec_in1,v1);
			um_unitvc (vec_in2,v2);
			dot = um_dot(v1, v2); 
			if (type1 == NCLSF_CYLINDER && dot < 0.)
			{
				for (i = 0; i < 3; i++)
				pt_in[i] = param2[i];
				dot = -1.0 * dot;
				um_translate_point(pt_in,param2[7],vec_in2,pt_out);
				for (i = 0; i < 3; i++)
					param2[i] = pt_out[i];
			}
			if (1.0 - dot > UM_FUZZ)
				status = UU_FAILURE;
/*
..... Check for same start point if num is less than four
*/
			else
			{
				for (i = 0; i < 3; i++)
				{
					pt1[i] = param1[i];
					pt2[i] = param2[i];
				}
				distance = um_dcccc(pt1,pt2);
				if(num < 4)
				{
/*
.....cheack all parameters if num is one or zero
*/
					if (distance > UM_FUZZ)
						status = UU_FAILURE;
/*
..... if the level is 2 or less:
.....      check height or radius for a cylinder; 
.....      check height or angle to normal for a cone.
*/
					else if (num < 3)
					{
						if(fabs(param1[6] - param2[6])>UM_FUZZ)
							status = UU_FAILURE;

						else if (num <2)
						{
							if(fabs(param1[7] - param2[7])>UM_FUZZ)
   							status = UU_FAILURE;
					   }	
					}
				}
				else if (num == 4 )
				{
/*
.....Do not consider a surface whose center is further than its diameter 
.....from the center of the first cylinder.
*/
					if (distance > 2*param1[6])
						status = UU_FAILURE;
/*
...... check the factor calculated from the height and the radius.
...... for regressive matching consider the difference as a fraction 
*/
					diff_ht =fabs(param1[7] - param2[7]);
					diff_rad =fabs(param1[6] - param2[6]);
					if(UIG_regressive)
					{
						diff_ht = 0.;
						diff_rad = diff_rad/param1[6];
					}
					comp_fac = (diff_ht * diff_ht) + (diff_rad * diff_rad);
					*rad =sqrt(comp_fac);
				}
			}
			break;
/*
.....Torus
*/
		case NCLSF_TORUS:
			status = UU_SUCCESS;
			for (i = 0; i < 3; i++)
			{
				vec_in1[i] = param1[i+3];
				vec_in2[i] = param2[i+3];
			}
			um_unitvc (vec_in1,v1);
			um_unitvc (vec_in2,v2);
			dot = fabs(um_dot(v1, v2)); 
			if (1.0 - dot > UM_FUZZ)
				status = UU_FAILURE;
/*
..... Check for same start point if num is less than four
*/
			else
			{
				for (i = 0; i < 3; i++)
				{
					pt1[i] = param1[i];
					pt2[i] = param2[i];
				}
				distance = um_dcccc(pt1,pt2);
/*
.....cheack all parameters if num is one or zero
*/
				if(num < 4)
				{
					if (distance > UM_FUZZ)
						status = UU_FAILURE;
/*
.....if the level is 2 or less:
.....check large and small radii
*/
					else if (num < 3)
					{
						if(fabs(param1[6] - param2[6])>UM_FUZZ)
							status = UU_FAILURE;

						else if (num <2)
						{
							if(fabs(param1[7] - param2[7])>UM_FUZZ)
   							status = UU_FAILURE;
					   }	
					}
				}
				else if (num == 4 )
				{
/*
.....Do not consider a surface whose center is further than its diameter 
.....from the center of the first torus.
*/
					if (distance > 2*(param1[6]+param1[7]))
						status = UU_FAILURE;
/*
...... check the factor calculated from the radii
...... for regressive matching consider the difference as a fraction 
*/
					diff_ht =fabs(param1[7] - param2[7]);
					diff_rad =fabs(param1[6] - param2[6]);
					if(UIG_regressive)
					{
						diff_ht = 0.;
						diff_rad = diff_rad/param1[6];
					}
					comp_fac = (diff_ht * diff_ht) + (diff_rad * diff_rad);
					*rad =sqrt(comp_fac);
				}
			}
			break;
		default:
			status = UU_FAILURE;
			break;
	}

	if (status == UU_SUCCESS && num == 0 && !UIG_matching_trimsf)
		status = uig_match_corners(eptr1,eptr2,labflg);

	if (status == UU_SUCCESS)
		{
			*dist = distance;
		}
	else
		{
			*dist = IGES_LARGE;
		}

	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_update_surface_prim()
**          Analyzes and updates the surfaces in the secondary
**          unibase.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
uig_update_surface_prim()
{
	struct NCL_fixed_databag eptr;
	int next_tupleid;
	UM_int2 primtyp;
	UM_real8 primdata[16];

	ur_getu_second();
	next_tupleid =1;
	while(ur_get_next_data_key(UM_RBSPLSRF_REL,&next_tupleid, &eptr.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&eptr);
		ncl_sf_prim_analyz(&eptr.key,&primtyp,&primdata);
	}
	next_tupleid =1;
	while(ur_get_next_data_key(NCL_SURF_REL,&next_tupleid, &eptr.key)>-1)
	{
		next_tupleid++;
		ncl_retrieve_data_fixed(&eptr);
		ncl_sf_prim_analyz(&eptr.key,&primtyp,&primdata);
	}
	ur_getu_work();
	return(0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_match_corners(eptr1,eptr2)
**          Check to see if two surfaces of the same type
**          are actually the same surface.
**    PARAMETERS
**       INPUT  :
**          eptr1                  surface 1 record
**          eptr2                  surface 2 record
**          labflg                 labeling flag
**       OUTPUT :
**          none          
**    RETURNS      : UU_SUCCESS if a match
**                   UU_FAILURE if no match
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
uig_match_corners(eptr1,eptr2,labflg)
struct NCL_fixed_databag *eptr1;
struct NCL_fixed_databag *eptr2;
int labflg;
{
	int status, i,j;
	struct UM_evsrfout evout;
	struct UM_transf_rec trans;
	UM_coord pt1[4], pt2[4];
	UU_REAL uv[2],tol;
	UU_LOGICAL um_cceqcc_tol();
	static int ix1[] = {2,3,0,1};
	static int ix2[] = {1,0,3,2};
	static int ix3[] = {0,3,2,1};

	tol = UIG_match_tol;
	uv[0] = 0.; uv[1] = 1.;
/*
.....Since eptr1 is from the working unibase, we need to switch
.....unibases. 
*/
	if (labflg) ur_getu_work();
	trans.key = eptr1->key;
	ur_retrieve_transf(&trans);
	for (i = 0; i < 2; i++)
	{
		for (j = 0; j < 2; j++)
		{
			status=uc_evsrf(UM_POINT,uv[i],uv[j],eptr1,trans.tfmat,&evout);
			if (status != UU_SUCCESS) return (UU_FAILURE);
			um_vctovc (evout.sp,pt1[2*i+j]);
		}
	}
	
	if (labflg) ur_getu_second();
	trans.key = eptr2->key;
	ur_retrieve_transf(&trans);
	for (i = 0; i < 2; i++)
	{
		for (j = 0; j < 2; j++)
		{
			status=uc_evsrf(UM_POINT,uv[i],uv[j],eptr2,trans.tfmat,&evout);
			if (status != UU_SUCCESS) return (UU_FAILURE);
			um_vctovc (evout.sp,pt2[2*i+j]);
		}
	}
	
	for (i = 0; i < 4 && status == UU_SUCCESS; i++)
	{
		if (!um_cceqcc_tol(pt1[i],pt2[i],tol)) status= UU_FAILURE;
	}

	if (status == UU_FAILURE)
	{
		status = UU_SUCCESS;
		for (i = 0; i < 4 && status == UU_SUCCESS; i++)
		{
			j = ix1[i];
			if (!um_cceqcc_tol(pt1[i],pt2[j],tol)) status= UU_FAILURE;
		}
	}

	if (status == UU_FAILURE)
	{
		status = UU_SUCCESS;
		for (i = 0; i < 4 && status == UU_SUCCESS; i++)
		{
			j = ix2[i];
			if (!um_cceqcc_tol(pt1[i],pt2[j],tol)) status= UU_FAILURE;
		}
	}
	
	if (status == UU_FAILURE)
	{
		status = UU_SUCCESS;
		for (i = 0; i < 4 && status == UU_SUCCESS; i++)
		{
			j = ix3[i];
			if (!um_cceqcc_tol(pt1[i],pt2[j],tol)) status= UU_FAILURE;
		}
	}
	
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_label_remaining()
**       Label the remaining entities that had no match in the
**       secondary unibase.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_label_remaining(unlabeled)
int *unlabeled;
{
	struct NCL_fixed_databag eptr;
	int i, count, rel;
	char buf[80], name_str[9];
	char prefix[9][2];

	prefix[0][0] = 'p'; prefix[0][1] = 't';
	prefix[1][0] = 'l'; prefix[1][1] = 'n';
	prefix[2][0] = 'c'; prefix[2][1] = 'i';
	prefix[3][0] = 'p'; prefix[3][1] = 'l';
	prefix[4][0] = 'c'; prefix[4][1] = 'n';
	prefix[5][0] = 'p'; prefix[5][1] = 'n';
	prefix[6][0] = 'p'; prefix[6][1] = 'v';
	prefix[7][0] = 'c'; prefix[7][1] = 'v';
	prefix[8][0] = 's'; prefix[8][1] = 'f';

/*
.....Set the UM_labelmdl.next to tig_max for each type of entity
.....plus one.  This will start the auto labeling at the next
.....number after the highest one used.
*/
	UM_labelmdl.next[UM_labelmdl.rel[UM_POINT_REL]] = tig_max_ptlab +1;
	UM_labelmdl.next[UM_labelmdl.rel[UM_LINE_REL]] = tig_max_lnlab +1;
	UM_labelmdl.next[UM_labelmdl.rel[UM_CIRCLE_REL]] = tig_max_cilab +1;
	UM_labelmdl.next[UM_labelmdl.rel[NCL_PLN_REL]] = tig_max_pllab +1;
	UM_labelmdl.next[UM_labelmdl.rel[UM_CONIC_REL]] = tig_max_cnlab +1;
	UM_labelmdl.next[UM_labelmdl.rel[NCL_PATERN_REL]] = tig_max_pnlab +1;
	UM_labelmdl.next[UM_labelmdl.rel[NCL_POINTVEC_REL]] = tig_max_pvlab +1;
	UM_labelmdl.next[UM_labelmdl.rel[UM_RBSPLCRV_REL]] = tig_max_cvlab +1;
	UM_labelmdl.next[UM_labelmdl.rel[UM_RBSPLSRF_REL]] = tig_max_sflab +1;
	if (*unlabeled >0)
	{
		sprintf(buf,"%d entities were unmatched \n",*unlabeled);
		uig_error(buf);
		for (i=0; i<9; i++)
		{
			if(tig_unlabeled_ent[i] >0)
			{
				sprintf(buf," %d %c%c's were unmatched. \n",tig_unlabeled_ent[i],
					prefix[i][0],prefix[i][1]);
				uig_error(buf);
			}
		}
	}
/*
.....Go through, get each key and give it a label.
*/
	for(i=0; i< tig_unlabeled; i++)
	{
		eptr.key = tig_unlabeled_keys[i];
		if(eptr.key != 0)
		{
			ur_retrieve_data_fixed(&eptr);
			rel = eptr.rel_num;
			if (rel == UM_CONIC_REL) rel = UM_RBSPLCRV_REL;
			um_auto_label(rel,&eptr.label);
/*
..... Every entity that failed to match must have its label gererated.
..... If subscripted labels are wanted for generated names, must
..... use subscripts for that geometry type.
..... Also, be sure to get rid of trailling spaces in the name.
*/
			count = 0;
			switch(eptr.rel_num)
			{
				case 1:		/* UM_POINT_REL */
					if (lab_flag[0]==1)
					{
						ipt++;
						tig_max_ptlab++;
						count = tig_max_ptlab;
					}
					break;
				case 98:	/* NCL_POINTVEC_REL */
					if (lab_flag[1]==1)
					{
						ipn++;
						tig_max_pvlab++;
						count = tig_max_pvlab;
					}
					break;
				case 2:		/* UM_LINE_REL */
					if (lab_flag[2]==1)
					{
						iln++;
						tig_max_lnlab++;
						count = tig_max_lnlab;
					}
					break;
				case 91:	/* NCL_PLN_REL */
					if (lab_flag[4]==1)
					{
						ipl++;
						tig_max_pllab++;
						count = tig_max_pllab;
					}
					break;
				case 3:		/* UM_CIRCLE_REL */
					if (lab_flag[5]==1)
					{
						ici++;
						tig_max_cilab++;
						count = tig_max_cilab;
					}
					break;
				case 4:		/* UM_CONIC_REL */
				case 5:		/* UM_COMPCRV_REL */
				case 7:		/* UM_RBSPLCRV_REL */
				case 13:	/* UM_UVCVONSF_REL */
				case 82:	/* NCL_CURVE_REL */
					if (lab_flag[6]==1)
					{
						icv++;
						tig_max_cvlab++;
						count = tig_max_cvlab;
					}
					break;
				case 11:	/* UM_RBSPLSRF_REL */
				case 83:	/* NCL_SURF_REL */
				case 85:	/* NCL_MESHSURF_REL */
				case 86:	/* NCL_QUILTSURF_REL */
				case 93:	/* NCL_NETSF_REL */
				case 99:	/* NCL_TRIMSF_REL */
				case 100:	/* NCL_REVSURF_REL */
					if (lab_flag[7]==1)
					{
						isf++;
						tig_max_sflab++;
						count = tig_max_sflab;
					}
					break;
				case 92:	/* NCL_PATERN_REL */
					if (lab_flag[10]==1)
					{
						ipn++;
						tig_max_pnlab++;
						count = tig_max_pnlab;
					}
					break;
				default:
					break;
			}
			eptr.subscr = count;
			if(count > 0)
			{
				sscanf(eptr.label, "%s", name_str);
				sprintf(eptr.label, "%s", name_str);
			}
/*
.....Update color and layer of unmatched entity if requested.
*/
			uig_match_updatts(&eptr,5);
			ur_update_data_fixed(&eptr);
			if (count == 0)
				sprintf (buf, "  generated label is %s\n", eptr.label);
			else
				sprintf (buf, "  generated label is %s(%d)\n", \
				                                    eptr.label, count);
			uig_error(buf);
		}
	}

	return(UU_SUCCESS);
}
/*********************************************************************
**    I_FUNCTION     :  uig_match_remaining(num,unlabeled)
**       Take unmatched entities and see if there are entities
**       in the secondary unibase that are close but not close enough
**       to have been matched during the first round.
**    PARAMETERS
**       INPUT  :
**          num                Indicates the level of matching to do
**                             The higher the number, the less exact
**                             we will be in selecting a match.
**          unlabeled          Used to indicate how many entities
**                             are still unmatched.
**       OUTPUT :
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_match_remaining(num,unlabeled)
int num;
int *unlabeled;
{
	struct NCL_fixed_databag eptr;
	int status,i,found, nlab;
	int percent, match_percent;
	char buf[80];

	nlab = *unlabeled;
	match_percent = 0;
/*
.....for regressive matching,
......make a list of all the keys in the secondary unibase with label not equal
..... to @UN and make its corresponding matchkey 0 and reggression factor 0
*/
	if(UIG_regressive) status = uig_unused_sec();
	
	for (i = 0; i<tig_unlabeled; i++)
	{
		eptr.key = tig_unlabeled_keys[i];
		if (eptr.key !=0)
		{
			ncl_retrieve_data_fixed(&eptr);
			switch(eptr.rel_num)
			{
				case UM_POINT_REL:
					found = uig_comp_point(&eptr,num);
				break;
				case UM_LINE_REL:
					found = uig_comp_line(&eptr,num);
					break;
				case UM_CIRCLE_REL:
					found = uig_comp_circle(&eptr,num);
					break;
				case NCL_PLN_REL:
					found = uig_comp_plane(&eptr,num);
					break;
				case UM_CONIC_REL:
					found = uig_comp_conic(&eptr,num);
					break;
				case UM_POLYLINE_REL:
					found = uig_comp_polyline(&eptr,num);
					break;
				case NCL_PATERN_REL:
					found = uig_comp_patern(&eptr,num);
					break;
				case NCL_POINTVEC_REL:
					found = uig_comp_pointvec(&eptr,num);
					break;
				case UM_RBSPLCRV_REL:
					found = uig_comp_rbsplcrv(&eptr,num);
					break;
				case UM_UVCVONSF_REL:
					found = uig_comp_uvcvonsf(&eptr,num);
					break;
				case UM_COMPCRV_REL:
					found = uig_comp_compcrv(&eptr,num);
					break;
				case UM_RBSPLSRF_REL:
					found = uig_comp_rbsplsrf(&eptr,num);
					break;
				case NCL_MESHSURF_REL:
					found = uig_comp_meshsurf(&eptr,num);
					break;
				case NCL_TRIMSF_REL:
					found = uig_comp_trimsrf(&eptr,num);
					break;
				default:
					break;
			}
/*
.....If a match is found,  update the entity with its new
.....label, put 0 for the key in the tig_unlabeled_keys
.....array decrease nlab by one and update color and layer
.....if requested.
*/

			if (!UIG_regressive && found == UU_SUCCESS)
			{
				ur_update_data_fixed(&eptr);
				tig_unlabeled_keys[i] = 0;
				nlab--;
/*
.....If a match was found, update color and layer if requested.
*/
				uig_match_updatts(&eptr,num);
			}
		}
		percent = i*100/tig_unlabeled;
		if (percent % 2 == 0)
		{
			if (percent != match_percent && percent > 0 && percent < 100)
			{
				sprintf(buf, "%d%% of level%d matched\n", percent, num);
				match_percent = percent;
				iges_disply_as_percent(percent);
			}	
		}
	}
/*
.....Regressive matching,
.....for all unlabeled ent ,check if its key is in matchkeys,
.....delete the key from tig_unlabeled_keys decrease unlabeled by 1
.....decrease corresponding tig_unlabeled_ent by 1 
.....copy label of check to label of match
.....for all keys in checkkeys with matchkey not 0 make label of check key @UN
*/
	if(UIG_regressive)
	{
		uig_change_labels(num,&nlab);	
/*
.....empty lists
*/
		UIG_regcount = 0 ;
		if (UIG_checkkeys)
		{
			uu_free(UIG_checkkeys);
			UIG_checkkeys = NULL;
		} 
		if (UIG_matchkeys)
		{
			uu_free(UIG_matchkeys);
			UIG_matchkeys = NULL;
		} 
		if (UIG_regfactor)
		{
			uu_free(UIG_regfactor);
			UIG_regfactor = NULL;
		} 
	}
	*unlabeled = nlab;
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION   : uig_match_check_num(eptr,maxnum)
**       Determine if the integer portion or subscript of a label is 
**       greater than the current maximum for this entity type.
**    PARAMETERS
**       INPUT  :
**          eptr          - pointer to entity.
**          maxnum        - Current maximum for this entity type.
**       OUTPUT :
**          maxnum        - maximum updated if necessary.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_match_check_maxnum(eptr,maxnum)
struct NCL_fixed_databag *eptr;
int *maxnum;
{
	int i, idx, num;
	char labc, lab[6];

	idx = UM_labelmdl.rel[eptr->rel_num];
/*
.....If this label is not subscripted and label mode
.....is not subscripted and this label prefix is the 
.....same as the labeling prefix, extract the number 
.....following the prefix and set the maximum to it 
.....if necessary.
*/
	if (eptr->subscr == 0)
	{
		if (!UM_labelmdl.issub[idx])
		{
			if (!strncmp(eptr->label,UM_labelmdl.pf[idx],2))
			{
				for (i=0;i<5;i++)
				{
					labc = eptr->label[i+2];
					if (!labc || labc==' ')
					{
						labc = lab[i] = '\0';
						break;
					}
					if (!isdigit(labc)) break;
					lab[i] = labc;
				}
				if (!labc)
				{
					sscanf(lab,"%d",&num);
					if (num>*maxnum) *maxnum = num;
				}
			}
		}
	}
/*
.....If this label is subscripted and label mode
.....is subscripted and this label prefix is the 
.....same as the labeling prefix, set the maximum
.....to the subscript if necessary.
*/
	else
	{
		if (UM_labelmdl.issub[idx])
		{
			for (i=0; eptr->label[i] && eptr->label[i] != ' ' && i<6; i++)
			{
				lab[i] = eptr->label[i];
			}
			lab[i] = '\0';
			if (!strcmp(UM_labelmdl.pfs[idx],lab) && eptr->subscr>*maxnum)
				*maxnum = eptr->subscr;
		}
	}
	return(UU_SUCCESS);
}
/*********************************************************************
**    I_FUNCTION   : uig_exact_match(key,func)
**       Determine if an entity is an exact match of an entity in the
**       label matching unibase and label it if it is.
**    PARAMETERS
**       INPUT  :
**          eptr     - Pointer to entity.
**          func     - Match function for this entity type.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS if labeled, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_exact_match(eptr, func)
struct NCL_fixed_databag *eptr;
int (*func)();
{
	int status;

	status = ncl_retrieve_data_fixed(eptr);
	if (status == UU_SUCCESS)
	{
/*
.....If it is an underlying element, give it an @UN label.
*/
		if (label_comp_element == UU_TRUE)
			strcpy(eptr->label, "@UN");
		else
		{
/*
.....Otherwise call the exact match function for this entity
*/
			status = (*(func)) (eptr, 1);
/*
.....If a match was not found, update global match variables
.....and mark with special label.
*/
			if (status == UU_FAILURE)
			{
				tig_unlabeled++;
				sprintf(eptr->label, "IG_MAT(%d)",tig_unlabeled);
				tig_unlabeled_keys[tig_unlabeled -1] = eptr->key;
			}
			else
			{
/*
.....If a match was found, update color and layer if requested.
*/
				uig_match_updatts(eptr,0);
			}
		}
/*
.....Update the entity with its label.
*/
		ur_update_data_fixed(eptr);
	}
	return (status);
}
/*********************************************************************
**    I_FUNCTION   : uig_match_updatts(eptr,lev)
**       Update the color and layer of a matched entity
**    PARAMETERS
**       INPUT  :
**          eptr       - Pointer to entity.
**          lev        - Matching level.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS if labeled, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_match_updatts(eptr,lev)
struct NCL_fixed_databag *eptr;
int lev;
{
	int color, layer;
	struct UC_attributedatabag attrbag;
	struct UM_attrdata_rec *attr;

	attr = (struct UM_attrdata_rec *)&attrbag;

	if(UIG_from_sw)
	{
		color = UIG_color_sec;
		if (color <= 0) color = -1;
		layer = UIG_layer_sec;
	}
	else
	{
		color = UIG_match_color_array[lev];
		layer = UIG_match_layer_array[lev];
	}
	if (color >= 0 || layer >=0)
	{
		attr->key=eptr->key;
		ur_retrieve_attr(attr);
		if (color >= 0)  attr->color = color;
		if (layer >= 0)
		{
			attr->layer = layer;
			uig_set_layer_num(layer);
		}
		ur_update_attr(attr);
	}
}

/*********************************************************************
**    E_FUNCTION     : uig_set_layer_num(num)
**       Set the layer number attribute .
**    PARAMETERS   
**       INPUT  : 
**				num				layer number
**       OUTPUT :  
**          none
**    RETURNS      : status		0 if success , -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_set_layer_num(num)
	int num;									/* layer number */	

	{
	/*int numint;							number of entities returned from DAS */
	int entnum;							/* next tuple index */
	int status;							/* status returned from UNIBASE */
	struct UM_layer_rec oldlayer;
	struct UM_layer_rec newlayer;
	char   buff[10];

	status = 0;
	entnum = 0;
	oldlayer.rel_num = UM_LAYER_REL;

	while (status == 0)
		{
		entnum++;
		status = ur_get_next_tuple_index(oldlayer.rel_num, &entnum);
		if (status == 0)
			{
			ur_retrieve_tuple(oldlayer.rel_num, entnum, &oldlayer);
			if (oldlayer.num == num)
				{
				break;	
				}
			}
		}
	if (status == -1)
		{
		newlayer.rel_num = UM_LAYER_REL;
		newlayer.num = num;
		newlayer.key = 0;
		sprintf(buff, "%d", num);
		strcpy(newlayer.name, "layer" );
		strcat(newlayer.name, buff);
		newlayer.displayable = UU_TRUE;
		newlayer.selectable  = UU_TRUE;
		newlayer.no_layers = 0;
		newlayer.layers = UU_NULL;
		ur_create_tuple(newlayer.rel_num, &entnum, &newlayer);
		}

	}

/*********************************************************************
**    I_FUNCTION     :  uig_unused_sec()
**       Take entities with labels other than @UN from the secondary unibase
**		 and push their keys in the checkkey list, also push a 0 for each of
**		these entities into the matchkey list and 0 into the regfactor list
**    PARAMETERS
**       INPUT  :
**          none                
**       OUTPUT :
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_unused_sec()
{
	int count,next_tupleid;
	struct NCL_fixed_databag eptr;
/*
.....initialize the three lists to store the keys from the existing unibase,
......keys from the secondary unibase and their corresponding  refactors
*/
	ur_getu_second();

	next_tupleid = 1;
	count =0;
	while(ur_get_next_key(&next_tupleid, &eptr.key)>-1)
	{
		next_tupleid++;
		ur_retrieve_data_relnum(eptr.key,&eptr.rel_num);
		if(eptr.rel_num == UM_POINT_REL || eptr.rel_num == UM_LINE_REL || 
			eptr.rel_num == UM_CIRCLE_REL || eptr.rel_num == NCL_PLN_REL ||
			eptr.rel_num == UM_CONIC_REL || eptr.rel_num == UM_POLYLINE_REL ||
			eptr.rel_num == NCL_PATERN_REL || eptr.rel_num == NCL_POINTVEC_REL ||
			eptr.rel_num == UM_RBSPLCRV_REL || eptr.rel_num == UM_UVCVONSF_REL ||
			eptr.rel_num == UM_COMPCRV_REL || eptr.rel_num == UM_RBSPLSRF_REL ||
			eptr.rel_num == NCL_MESHSURF_REL|| eptr.rel_num == NCL_TRIMSF_REL)
		count++;
	}
	if (UIG_checkkeys)
	{
		uu_free(UIG_checkkeys);
		UIG_checkkeys = NULL;
	} 
	if (UIG_matchkeys)
	{
		uu_free(UIG_matchkeys);
		UIG_matchkeys = NULL;
	} 
	if (UIG_regfactor)
	{
		uu_free(UIG_regfactor);
		UIG_regfactor = NULL;
	} 
	UIG_regcount = 0;
	UIG_checkkeys = (UU_KEY_ID *)uu_malloc(count*sizeof(UU_KEY_ID));
	if (!UIG_checkkeys) return(UU_FAILURE);
	UIG_matchkeys = (UU_KEY_ID *)uu_malloc(count*sizeof(UU_KEY_ID));
	if (!UIG_matchkeys) return(UU_FAILURE);
	UIG_regfactor = (UU_REAL *)uu_malloc(count*sizeof(UU_REAL));
	if (!UIG_regfactor) return(UU_FAILURE);
	
	next_tupleid = 1;
	while(ur_get_next_key(&next_tupleid, &eptr.key)>-1)
	{
		next_tupleid++;
		ur_retrieve_data_relnum(eptr.key,&eptr.rel_num);
		if(eptr.rel_num == UM_POINT_REL || eptr.rel_num == UM_LINE_REL || 
			eptr.rel_num == UM_CIRCLE_REL || eptr.rel_num == NCL_PLN_REL ||
			eptr.rel_num == UM_CONIC_REL || eptr.rel_num == UM_POLYLINE_REL ||
			eptr.rel_num == NCL_PATERN_REL || eptr.rel_num == NCL_POINTVEC_REL ||
			eptr.rel_num == UM_RBSPLCRV_REL || eptr.rel_num == UM_UVCVONSF_REL ||
			eptr.rel_num == UM_COMPCRV_REL || eptr.rel_num == UM_RBSPLSRF_REL ||
			eptr.rel_num == NCL_MESHSURF_REL|| eptr.rel_num == NCL_TRIMSF_REL)
		{
/*
.....Do not push entities whose label starts with an '@'
*/
			if (eptr.label[0] != '@')
			{
				UIG_checkkeys[UIG_regcount] = eptr.key;
				UIG_matchkeys[UIG_regcount] = 0;
				UIG_regfactor[UIG_regcount] = 0.;
				UIG_regcount ++;
			}
		}
	}
	ur_getu_work();

	return(UU_SUCCESS);
}
/*********************************************************************
**    I_FUNCTION     :  uig_change_labels()
**       For all tig_unlabeled_keys check if they lie in matchkeylist
**		 copy label of check from secondary  to match in the existing unibase
**		 delete key from tig_unlabeled_keys decrease unlabeled by 1
**		 decrease corresponding tig_unlabeled_ent by 1 
**		 for all keys in the UIG_checkkeys with corresponding UIG_macthkey
**		 not 0 make label of check key @UN
**    PARAMETERS
**       INPUT  :
**          num                Indicates the level of matching to do     
**                             The higher the number, the less exact
**                             we will be in selecting a match. 
**          nlab	           Used to indicate how many entities
**                             are still unmatched.         
**       OUTPUT :
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_change_labels(num,unlab)
int num;
int *unlab;
{
	int nlab, i, j;
	struct NCL_fixed_databag eptr,check;

	nlab = *unlab;

	for (i = 0; i<tig_unlabeled; i++)
	{
		eptr.key = tig_unlabeled_keys[i];
		if (eptr.key !=0)
		{
			ncl_retrieve_data_fixed(&eptr);
			for (j = 0; j<UIG_regcount; j++)
			{
				if(UIG_matchkeys[j] == eptr.key)
				{
					ur_getu_second();
					check.key = UIG_checkkeys[j];
					ncl_retrieve_data_fixed(&check);
/*
.....If the label is the standard ptXX, then we need to
.....keep track of the highest number used.  The remaining
.....points that were not matched will be labeled starting
.....at one higher than the highest.
*/
					uig_match_check_maxnum1(&check);

					strcpy(eptr.label,check.label);
					eptr.subscr = check.subscr;
/*
.....We have used this ent so mark it with
.....a label of @UN.
*/
					strcpy(check.label,"@UN");
					check.subscr = 0;
					ur_update_data_fixed(&check);
/*
.....Switch back to primary unibase.
*/
					ur_getu_work();
/*
.....put 0 for the key in the tig_unlabeled_keys
.....array decrease nlab by one and update color and layer
.....if requested.
*/
					ur_update_data_fixed(&eptr);
					tig_unlabeled_keys[i] = 0;
					nlab--;
/*
.....If a match was found, update color and layer if requested.
*/
					uig_match_updatts(&eptr,num);
					break;
				}
			}
		}
	}
	*unlab = nlab;
	return 0;
}
/*********************************************************************
**    I_FUNCTION   : uig_match_check_num1(eptr)
**       Determine if the integer portion or subscript of a label is 
**       greater than the current maximum for this entity type.Also 
**		decrease the corresponding tig_unlabeled_ent by 1
**    PARAMETERS
**       INPUT  :
**          eptr          - pointer to entity.
**       OUTPUT :
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_match_check_maxnum1(eptr)
struct NCL_fixed_databag *eptr;
{
	
	switch(eptr->rel_num)
	{
		case 1:		/* UM_POINT_REL */
			uig_match_check_maxnum(eptr,&tig_max_ptlab);
			tig_unlabeled_ent[0] --;
			break;
		case 98:	/* NCL_POINTVEC_REL */
			uig_match_check_maxnum(eptr,&tig_max_pvlab);
			tig_unlabeled_ent[6] --;
			break;
		case 2:		/* UM_LINE_REL */
			uig_match_check_maxnum(eptr,&tig_max_lnlab);
			tig_unlabeled_ent[1] --;
			break;
		case 91:	/* NCL_PLN_REL */
			uig_match_check_maxnum(eptr,&tig_max_pllab);
			tig_unlabeled_ent[2] --;
			break;	
		case 3:		/* UM_CIRCLE_REL */
			uig_match_check_maxnum(eptr,&tig_max_cilab);
			tig_unlabeled_ent[2] --;
			break;
		case 4:		/* UM_CONIC_REL */
			uig_match_check_maxnum(eptr,&tig_max_cnlab);
			tig_unlabeled_ent[4] --;
			break;
		case 5:		/* UM_COMPCRV_REL */
		case 7:		/* UM_RBSPLCRV_REL */
		case 13:	/* UM_UVCVONSF_REL */
		case 82:	/* NCL_CURVE_REL */
			uig_match_check_maxnum(eptr,&tig_max_cvlab);
			tig_unlabeled_ent[4] --;
			break;
		case 11:	/* UM_RBSPLSRF_REL */
		case 83:	/* NCL_SURF_REL */
		case 85:	/* NCL_MESHSURF_REL */
		case 86:	/* NCL_QUILTSURF_REL */
		case 93:	/* NCL_NETSF_REL */
		case 99:	/* NCL_TRIMSF_REL */
		case 100:	/* NCL_REVSURF_REL */
			uig_match_check_maxnum(eptr,&tig_max_sflab);
			tig_unlabeled_ent[8] --;
			break;
		case 92:	/* NCL_PATERN_REL */
			uig_match_check_maxnum(eptr,&tig_max_pnlab);
			tig_unlabeled_ent[5] --;
			break;
		default:
			break;
	}
	return 0;
}
