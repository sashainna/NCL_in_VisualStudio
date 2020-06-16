/*********************************************************************
**    NAME         :  tspmcrv.c
**       CONTAINS:
**					utp_in_outer_bound
**					utp_in_edge_loop
**					utp_in_oriented_edge
**					utp_in_edge_curve
**					utp_in_curve
**					utp_in_comp_curve
**					utp_in_surface_curve
**					utp_in_trimmed_curve
**					utp_in_vertex_loop
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tspmcrv.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/17/15 , 17:55:16
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "tstep.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mdrel.h"
#include "modef.h"
#include "nconst.h"
#include "udebug.h"

UU_KEY_ID utp_in_edge_curve();
UU_KEY_ID utp_in_curve();
static UU_KEY_ID S_in_curve();
static void S_trim_curve();

static int Sreverse=UU_FALSE;
static UM_coord Spt[2];

/*********************************************************************
**    I_FUNCTION     :  utp_in_outer_bound(ptr,relnum)
**				FACE_OUTER_BOUND handling routine.  Typically a top level
**          designator for surface boundary curves.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_outer_bound(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	UU_KEY_ID key;
/*
.....Get the pointer to the actual curve
*/
	if (ptr->parm[1].type == UTP_RECNO)
		key = utp_in_dispat_recno(ptr->parm[1].ptype.recno,relnum);
/*
.....Could not reference boundary curve
*/
	else
	{
		key = 0;
		utp_syntax_error(ptr);
	}
	return(key);
}

/*********************************************************************
**    I_FUNCTION     :  utp_in_edge_loop(ptr,relnum)
**				EDGE_LOOP (composite) curve handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_edge_loop(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i,ind,ncvs;
	UU_KEY_ID key,tkey;
	struct UM_compcrv_rec comp;
	struct UM_cid_rec *cvid;
	struct UM_crvdatabag cv;
	UU_REAL rnum;
	UU_REAL um_getarclen();

	UU_REAL start_u,end_u;
	UM_coord start_pt,end_pt;
	UM_transf tfmat;
	struct UM_crvdatabag crv,cons;
	struct UM_evcrvout evout;
/*
.....Initialize composite curve structure
*/
	comp.closdinu = 1;
	comp.arclen = 0.;
	comp.planar = UU_TRUE;
	comp.open = UU_FALSE;
	comp.continuity = 0;
	comp.fcolor = 0;
	comp.t0 = 0.;
	comp.t1 = 1.;
	comp.addflg = 0;
	*relnum = 0;
/*
.....Allocate memory for sub-curves
*/
	ncvs = ptr->nparm-1;
	cvid = (struct UM_cid_rec *)uu_malloc(sizeof(struct UM_cid_rec)*ncvs);
/*
.....Loop through and define
.....individual curves
*/
	ind = 0;
	for (i=1;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			Sreverse = UU_FALSE;
			cvid[ind].crvid = utp_in_dispat_recno(ptr->parm[i].ptype.recno,relnum);
			if (cvid[ind].crvid == 0)
			{
				ncvs--;
				if (ncvs < 1) goto failed;
			}
			else
			{
				cvid[ind].reverse = Sreverse;
/*
		crv.key = cvid[ind].crvid;
		ncl_retrieve_data_fixed(&crv);
		uc_retrieve_transf (crv.key,tfmat);
		uc_init_evcrvout (&crv,&evout);
		if (cvid[ind].reverse) start_u = 1.0;
		else start_u = 0.0;
		end_u   = 1. - start_u;
		uc_evcrv (UM_POINT, start_u, &crv, tfmat, &evout);
		uc_evcrv (UM_FRSTDERIV, end_u, &crv, tfmat, &evout);
*/
				ind++;
			}
		}
		else
		{
			key = 0;
			utp_syntax_error(ptr);
			goto done;
		}
	}
/*
.....Repair composite curve
.......Error if "fixing" curve results in no components - ASF 7/2/13.
*/
	S_fix_composite(&comp,cvid,&ncvs);
	if (ncvs < 1) goto failed;
/*
.....Create composite curve
*/
	key = utp_store_composite(&comp,ptr->parm[0].ptype.str,cvid,ncvs);
	goto done;
/*
.....Could not create Edge loop
*/
failed:;
	key = 0;
	utp_boundary_error(ptr);
	goto done;
done:;
	if (key != 0) *relnum = comp.rel_num;
	if (cvid != UU_NULL) uu_free(cvid);
	return(key);
}

/*********************************************************************
**    I_FUNCTION     :  utp_in_oriented_edge(ptr,relnum)
**				ORIENTATED_EDGE curve handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_oriented_edge(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int status,reverse;
	UU_KEY_ID key;
	UTPs_step_record *tptr;
/*
.....Get Reverse flag
*/
   status = utp_get_logicals(&ptr->parm[4],&reverse,1);
   Sreverse = !reverse;
/*
.....Get the pointer to the curve
*/
	if (ptr->parm[3].type == UTP_RECNO)
	{
/*
........Record points to EDGE_CURVE
........Process the EDGE_CURVE
*/
		tptr = utp_get_record(ptr->parm[3].ptype.recno);
		if (tptr->command == EDGE_CURVE)
			key = utp_in_dispat(tptr,relnum);
/*
........Record does not point to EDGE_CURVE
........Process ORIENTED_EDGE as an EDGE_CURVE
*/
		else
			key = utp_in_edge_curve(ptr,relnum);
	}
		
/*
.....Could not reference geometry
*/
	else
	{
		key = 0;
		utp_syntax_error(ptr);
	}
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_edge_curve(ptr,relnum)
**				EDGE_CURVE curve handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_edge_curve(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i,status;
	char label[NCL_MAX_LABEL+1];
	UU_KEY_ID key;
	UTPs_step_record *tptr;
	struct UM_circle_rec eptr;
	struct UM_conic_rec cptr;
	UU_LOGICAL cclw;
/*
.....Invalid record syntax
*/
	if (ptr->parm[1].type != UTP_RECNO || ptr->parm[2].type != UTP_RECNO ||
		ptr->parm[3].type != UTP_RECNO)
	{
		tptr = ptr;
		goto failed;
	}
	
/*
.....Get the start and end points of the curve
*/
	else
	{
		for (i=0;i<2;i++)
		{
			tptr = utp_get_record(ptr->parm[i+1].ptype.recno);
			if (tptr == UU_NULL) goto failed;
			status = utp_map_point(tptr,Spt[i]);
			if (status == UU_FAILURE) goto done;
		}
/*
.....Get the curve itself
*/
		if (ptr->recno == 10643)
		{
			i = i;
		}
		tptr = utp_get_record(ptr->parm[3].ptype.recno);
		if (tptr == UU_NULL) goto failed;
/*
.....Set reversal flag
*/
		status = utp_get_logicals(&ptr->parm[4],&cclw,1);
/*
.....Define the curve
*/
		key = S_in_curve(tptr,Spt[0],Spt[1],cclw,relnum);
	}
	goto done;
/*
.....Could not reference face
*/
failed:;
	key = 0;
	utp_syntax_error(tptr);
/*
.....End of routine
*/
done:;
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_curve(ptr,relnum)
**				Processes a b-spline curve and stores it in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS :
**          Stores the curve in the Unibase.
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_curve(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int status;
	char label[NCL_MAX_LABEL+1];
	UU_KEY_ID key;
	UTPs_step_record uptr;
	struct UM_rbsplcrv_rec cv;
/*
.....Define the curve's label
*/
	key = 0;
	strcpy(label,"@UN");
/*
.....B-spline Curve 
.......Added QUASI_UNIFORM_CURVE case, which is handled the same as
.......B_SPLINE_CURVE case - ASF 10/17/13.
*/
	if (ptr->command == B_SPLINE_CURVE ||
		ptr->command == QUASI_UNIFORM_CURVE ||
		ptr->command == B_SPLINE_CURVE_WITH_KNOTS)
	{
		status = utp_map_bspline_curve(ptr,&cv);
		if (status == UU_SUCCESS)
		{
			key = utp_store_curve(&cv,label,UU_TRUE);
			*relnum = cv.rel_num;
		}
	}
/*
.....Bounded curve
*/
	else if (ptr->command == BOUNDED_CURVE)
	{
		if (ptr->parm[0].type != UTP_COMMAND) goto failed;
		uptr.recno = ptr->recno;
		uptr.command = ptr->parm[0].ptype.cmd;
		uptr.nparm = ptr->nparm - 1;
		uptr.parm = &ptr->parm[1];
/*
........B-spline curve
*/
		if (uptr.command == B_SPLINE_CURVE ||
			uptr.command == B_SPLINE_CURVE_WITH_KNOTS)
			status = utp_map_bspline_curve(&uptr,&cv);
		else
			goto failed;
/*
........Store curve
*/
		if (status == UU_SUCCESS)
		{
			key = utp_store_curve(&cv,label,UU_TRUE);
			*relnum = cv.rel_num;
		}
	}
/*
.....Unknown curve type
*/
	else
		key = utp_in_dispat(ptr,relnum);
	goto done;
/*
.....Could not reference face
*/
failed:;
	key = 0;
	utp_syntax_error(ptr);
/*
.....End of routine
*/
done:;
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_comp_curve(ptr)
**				COMPOSITE_CURVE handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT :
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS :
**          Stores the curve in the Unibase.
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_comp_curve(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i,ncvs,status;
	UU_KEY_ID key;
	struct UM_compcrv_rec comp;
	struct UM_cid_rec *cvid;
	UTPs_step_record *tptr;
/*
.....Initialize routine
*/
	key = 0;
	*relnum = UM_COMPCRV_REL;
	cvid = UU_NULL;
	ncvs = ptr->nparm - 2;
/*
.....Initialize composite curve structure
*/
	if (ncvs > 1)
	{
		
		comp.closdinu = 1;
		comp.arclen = 0.;
		comp.planar = UU_TRUE;
		comp.open = UU_FALSE;
		comp.continuity = 0;
		comp.fcolor = 0;
		comp.t0 = 0.;
		comp.t1 = 1.;
		comp.addflg = 0;
	}
	cvid = (struct UM_cid_rec *)uu_malloc(sizeof(struct UM_cid_rec)*ncvs);
	if (cvid == UU_NULL) goto failed;
/*
.....Loop through and define
.....individual curves
*/
	ncvs = 0;
	for (i=1;i<ptr->nparm-1;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			tptr = utp_get_record(ptr->parm[i].ptype.recno);
			if (tptr->command != COMPOSITE_CURVE_SEGMENT) goto failed;
			if (tptr->parm[tptr->nparm-1].type != UTP_RECNO) goto failed;
			cvid[ncvs].crvid =
				utp_in_dispat_recno(tptr->parm[tptr->nparm-1].ptype.recno,relnum);
/*
.....Ignore bad segments
*/
			if (cvid[ncvs].crvid != 0) ncvs++;
		}
		else
		{
			key = 0;
			utp_syntax_error(ptr);
		}
	}
/*
.....Repair composite curve
*/
	if (ncvs > 1)
	{
		S_fix_composite(&comp,cvid,&ncvs);
		if (ncvs < 1) goto failed;
/*
.....Create composite curve
*/
		key = utp_store_composite(&comp,ptr->parm[0].ptype.str,cvid,ncvs);
		*relnum = UM_COMPCRV_REL;
	}
/*
.....Create single entity
*/
	else
		key = cvid[0].crvid;
	goto done;
/*
.....Could not create composite curve
*/
failed:;
	key = 0;
	utp_wireframe_error(ptr);
	goto done;
/*
.....End of routine
*/
done:;
	if (cvid != UU_NULL) uu_free(cvid);
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_surface_curve(ptr,relnum)
**				SURFACE_CURVE curve handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_surface_curve(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i,status;
	char label[NCL_MAX_LABEL+1];
	UU_KEY_ID key;
	UM_coord pt[2];
	UTPs_step_record *tptr;
	struct UM_circle_rec eptr;
	struct UM_conic_rec cptr;
	UU_LOGICAL cclw;
/*
.....Invalid record syntax
*/
	if (ptr->parm[1].type != UTP_RECNO)
	{
		tptr = ptr;
		goto failed;
	}
/*
.....Get the curve itself
*/
	tptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (tptr == UU_NULL) goto failed;
/*
.....Define the curve
*/
	key = S_in_curve(tptr,Spt[0],Spt[1],UU_TRUE,relnum);
	goto done;
/*
.....Could not reference face
*/
failed:;
	key = 0;
	utp_syntax_error(tptr);
/*
.....End of routine
*/
done:;
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_trimmed_curve(ptr)
**				TRIMMED_CURVE handling routine.
**             Currently only support u-paramater for trimming.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT :
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS :
**          Stores the curve in the Unibase.
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_trimmed_curve(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i,inc,np,ncvs,status;
	UU_KEY_ID key;
	UU_REAL t[2];
	UM_coord pt[2];
	UM_transf tfmat;
	struct UM_compcrv_rec comp;
	struct UM_cid_rec *cvid;
	UTPs_step_record *sptr,*tptr;
/*
.....Initialize routine
*/
	key = 0;
	np = ptr->nparm;
	if (np < 6 || ptr->parm[np-1].type != UTP_STRING) goto done;
	if (ptr->parm[1].type != UTP_RECNO) goto done;
/*
.....Get curve end points &
.....parameter values
*/
	t[0] = 0.; t[1] = 1.;
	inc = 0;
	for (i=2;i<np;i++)
	{
		if (ptr->parm[i].type == UTP_STRING)
		{
			if (strcmp(ptr->parm[i].ptype.str,"PARAMETER_VALUE") == 0)
			{
				if (ptr->parm[i+1].type != UTP_REAL) goto done;
				t[inc] = ptr->parm[i+1].ptype.value;
				i++;
				if (ptr->parm[i+1].type == UTP_RECNO) i++;
			}
			else goto done;
		}
		if (ptr->parm[i].type == UTP_RECNO)
		{
			tptr = utp_get_record(ptr->parm[i].ptype.recno);
			if (utp_map_point(tptr,pt[inc]) != UU_SUCCESS) goto done;
		}
		inc++;
		if (inc == 2) break;
	}
	if (inc != 2) goto done;
/*
.....Get reversal flag
*/
	status = utp_get_logicals(&ptr->parm[i+1],&Sreverse,1);
/*
.....Define the curve
*/
	if (ptr->parm[1].type != UTP_RECNO) goto done;
	sptr = utp_get_record(ptr->parm[1].ptype.recno);
	key = S_in_curve(sptr,pt[0],pt[1],Sreverse,relnum);
/*
.....Composite curve must be built
.....for trimmed surfaces boundaries
........Initialize composite curve structure
*/
	if (UIG_from_trimsrf && key != 0)
	{
		comp.closdinu = 1;
		comp.arclen = 0.;
		comp.planar = UU_TRUE;
		comp.open = UU_FALSE;
		comp.continuity = 0;
		comp.fcolor = 0;
		comp.addflg = 0;
		ncvs = 1;
		cvid = (struct UM_cid_rec *)uu_malloc(sizeof(struct UM_cid_rec)*ncvs);
/*
........Store base curve
*/
		cvid[0].crvid = key;
		cvid[0].endparam = 1.;
		comp.t0 = t[0];
		comp.t1 = t[1];
		cvid[0].reverse = Sreverse;
/*
........Store composite curve
*/
		S_fix_composite(&comp,cvid,&ncvs);
		key = utp_store_composite(&comp,ptr->parm[0].ptype.str,cvid,ncvs);
		if (key != 0) *relnum = comp.rel_num;
	}
/*
.....End of routine
*/
done:;
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_vertex_loop(ptr,relnum)
**				VERTEX_LOOP curve handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_vertex_loop(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int status;
	char label[NCL_MAX_LABEL+1];
	UU_KEY_ID key;
	UM_coord spt,ept;
	UTPs_step_record *tptr;
/*
.....Invalid record syntax
*/
	if (ptr->parm[1].type != UTP_RECNO)
	{
		tptr = ptr;
		goto failed;
	}
/*
.....Define the curve's label
*/
	key = 0;
	strcpy(label,"@UN");
/*
.....Get the start and end points of the curve
*/
   tptr = utp_get_record(ptr->parm[1].ptype.recno);
   if (tptr == UU_NULL) goto failed;
   status = utp_map_point(tptr,spt);
   if (status == UU_FAILURE) goto failed;
/*
.....Create zero length line
*/
	um_vctovc(spt,ept);
	utp_transform_line(spt,ept);
	key = utp_store_line(label,spt,ept);
	*relnum = UM_LINE_REL;
	goto done;
/*
.....Could not reference face
*/
failed:;
	key = 0;
	utp_syntax_error(tptr);
/*
.....End of routine
*/
done:;
	return(key);
}

/*********************************************************************
**    S_FUNCTION     :  S_fix_composite(comp,cvid,ncvs)
**       Sets the closed, reversal, and endparm settings for composite
**       curves.
**    PARAMETERS   
**       INPUT  : 
**          comp     Composite curve record.
**          cvid     Curve components.
**          ncvs     Number of curves in 'cvid'.
**       OUTPUT : 
**          ncvs     Updated number of curves in case one was deleted.
**    RETURNS      :
**          UU_SUCCESS on success, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_fix_composite(comp,cvid,ncvs)
struct UM_compcrv_rec *comp;
struct UM_cid_rec *cvid;
int *ncvs;
{
	int i,j,stat,num,test_flags[4],rev;
	UU_LOGICAL reversed;
	UU_REAL rnum,start_u,end_u,tol,arclen,dis;
	UU_REAL um_getarclen(),um_dot();
	UM_coord lpt,fpt,start_pt,end_pt,spt,ept;
	UM_transf tfmat;
	struct UM_crvdatabag crv,cons;
	struct UM_evcrvout evout;
/*
....Initialize routine
*/
	stat = UU_FAILURE;
	num = *ncvs;
	comp->arclen = 0.;
	if (UIG_from_trimsrf)
		tol = UIG_comp_tol;
	else 
		tol = 0.008;
/*
.....Verify that the composite curve is continuous
.....The last point on the previous subcurve should
.....be close to the first point on the current subcurve
.....(subcurves should be joined, end-to-end).
*/
	for(i=0;i<num;i++)
	{
/*
.....Get the subcurve
*/
		crv.key = cvid[i].crvid;
		ncl_retrieve_data_fixed(&crv);
		uc_retrieve_transf (crv.key,tfmat);
		uc_init_evcrvout (&crv,&evout);
/*
.....Get the start and ending points
*/
		reversed = UU_FALSE;
		if (cvid[i].reverse) start_u = 1.0;
		else start_u = 0.0;
		end_u   = 1. - start_u;
		uc_evcrv (UM_POINT, start_u, &crv, tfmat, &evout);
		um_vctovc(evout.cp, start_pt);
		uc_evcrv (UM_FRSTDERIV, end_u, &crv, tfmat, &evout);
		um_vctovc(evout.cp, end_pt);
/*
.....Ignore any subcurves that equate to a point
*/ 
		cons.key = cvid[i].crvid;
		uc_retrieve_data(&cons,sizeof(cons));
		arclen = um_getarclen(&cons,UM_DEFAULT_TF);
/*
		if (i>0 && i==num-1 && arclen<=UM_FUZZ)
		{
			num--;
			i--;
			continue;
		}
*/
		if (arclen<=UM_FUZZ)
		{
			num--;
			for (j=i;j<num;j++)
				cvid[j] = cvid[j+1];
			i--;
			continue;
		}
/*
.....For first subcurve, don't care about its starting point
.....being close to anything.  Just get the endpoint.
*/
		if (i == 0)
		{
			um_vctovc(start_pt, fpt);
			um_vctovc(end_pt, lpt);
/*			cvid[i].reverse = UU_FALSE;*/
		}
/*
.....Check rest of curves
*/
		else
		{
			rev = uig_compcv_contin_check(fpt, lpt, start_pt,
				end_pt, test_flags, tol);
/*
........Both endpoints of 1st and 2nd curves are too far away.
........The curves are separated (not continuous).
........See if this curve just needs to be dropped
*/
			if (rev == -1)
			{
/*
...........Last curve, just ignore it
*/
				if (i == num-1)
				{
					num--;
					i--;
					continue;
				}
/*
...........See if curve can be dropped
...........when end point of next curve
...........matches previous curve
*/
				else
				{
					crv.key = cvid[i+1].crvid;
					ncl_retrieve_data_fixed(&crv);
					uc_retrieve_transf (crv.key,tfmat);
					if (cvid[i+1].reverse) start_u = 1.0;
					else start_u = 0.0;
					end_u   = 1. - start_u;
					uc_evcrv (UM_POINT, start_u, &crv, tfmat, &evout);
					um_vctovc(evout.cp, spt);
					uc_evcrv (UM_POINT, end_u, &crv, tfmat, &evout);
					um_vctovc(evout.cp, ept);
					rev = uig_compcv_contin_check(fpt,lpt,spt,ept,test_flags,tol);
					if (rev != -1)
					{
						num--;
						for (j=i;j<num;j++)
							cvid[j] = cvid[j+1];
						i--;
						continue;
					}
/*
...........See if previous curve can be dropped
*/
					else if (i > 1)
					{
						crv.key = cvid[i-2].crvid;
						ncl_retrieve_data_fixed(&crv);
						uc_retrieve_transf (crv.key,tfmat);
						if (cvid[i-2].reverse) start_u = 1.0;
						else start_u = 0.0;
						end_u   = 1. - start_u;
						uc_evcrv (UM_POINT, start_u, &crv, tfmat, &evout);
						um_vctovc(evout.cp, spt);
						uc_evcrv (UM_POINT, end_u, &crv, tfmat, &evout);
						um_vctovc(evout.cp, ept);
						rev = uig_compcv_contin_check(spt,ept,start_pt,end_pt,
							test_flags,tol);
/*
...........Drop previous curve
*/
						if (rev != -1)
						{
							num--;
							for (j=i-1;j<num;j++)
								cvid[j] = cvid[j+1];
							i -= 2;
/*
...........Reset end points of previous curve
*/
							if (i >= 0)
							{
								crv.key = cvid[i].crvid;
								ncl_retrieve_data_fixed(&crv);
								uc_retrieve_transf (crv.key,tfmat);
								if (cvid[i].reverse) start_u = 1.0;
								else start_u = 0.0;
								end_u   = 1. - start_u;
								uc_evcrv (UM_POINT, start_u, &crv, tfmat, &evout);
								um_vctovc(evout.cp, fpt);
								uc_evcrv (UM_POINT, end_u, &crv, tfmat, &evout);
								um_vctovc(evout.cp, lpt);
							}
							continue;
						}
//						else
//							goto done;
					}
//					else
//						goto done;
				}
			}
/*
........The first subcurve is reversed.  Change the orientation of
........composite curve (first point <-> last point).
*/
			if (i == 1)
			{
				if ((rev == 3) || (rev == 4))
				{
					cvid[0].reverse = !cvid[0].reverse;
					um_vctovc(lpt,fpt);
					rev -= 2;
				}
			}
/*
........Current subcurve will need to be reversed if rev = 2 or 4.
*/
			rev = rev % 2;
			if (rev == 1)
			{
/*
...........If current subcurve is the last subcurve and
...........If rev = 3 and
...........If End of subcurve is within tol of end of comp
...........then reverse the subcurve.
*/
				if ((i==(num-1)) && test_flags[0]==0 && test_flags[1]==1 &&
						test_flags[2]==1 && test_flags[3]==0)
				{
					reversed = UU_TRUE;
					cvid[i].reverse = !cvid[i].reverse;
				}
			}
			else
			{
				reversed = UU_TRUE;
				cvid[i].reverse = !cvid[i].reverse;
			}
/*
.....Store end point of current curve
*/
			if (reversed) um_vctovc(start_pt,lpt);
			else um_vctovc(end_pt,lpt);
		}
/*
.....Store end parameter for component
*/
		cvid[i].endparam = arclen;
		comp->arclen = comp->arclen + cvid[i].endparam;
	}
/*
.....Adjust end parameters
*/
	*ncvs = num;
	if (comp->arclen > 0.)
	{
		rnum = 0;
		for (i=0;i<num;i++)
		{
			rnum = rnum + cvid[i].endparam;
			cvid[i].endparam = rnum / comp->arclen;
		}
		stat = UU_SUCCESS;
	}
	else
		stat = UU_FALSE;
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    S_FUNCTION     :  S_in_curve(ptr,spt,ept,revfl,relnum)
**       Defines and stores a curve record.  The following geometry
**       types are supported.
**
**          Lines, Circles, Ellipses, Curves
**
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**          spt      Start point of curve.
**          ept      End point of curve.
**          revfl    Reversal flag of entity.  UU_TRUE = CCLW for circle.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of created entity or 0 on failure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_KEY_ID S_in_curve(ptr,spt,ept,revfl,relnum)
UTPs_step_record *ptr;
UM_coord spt,ept;
UU_LOGICAL revfl;
int *relnum;
{
	int status;
	UU_KEY_ID key;
	char label[NCL_MAX_LABEL+1];
	struct UM_circle_rec eptr;
	struct UM_conic_rec cptr;
/*
.....Define the curve's label
*/
	key = 0;
	strcpy(label,"@UN");
/*
.....Curve is a simple line
*/
	if (ptr->command == LINE)
	{
		utp_transform_line(spt,ept);
		key = utp_store_line(label,spt,ept);
		*relnum = UM_LINE_REL;
	}
/*
.....Curve is a simple circle
*/
	else if (ptr->command == CIRCLE)
	{
		status = utp_map_circle(ptr,spt,ept,revfl,&eptr);
		if (status == 0)
		{
			key = utp_store_circle(&eptr,label);
			*relnum = UM_CIRCLE_REL;
		}
		else if (status == -2)
		{
			utp_transform_line(spt,ept);
			key = utp_store_line(label,spt,ept);
			*relnum = UM_LINE_REL;
		}
	}
/*
.....Curve is an ellipse
*/
	else if (ptr->command == ELLIPSE)
	{
		status = utp_map_ellipse(ptr,spt,ept,revfl,&cptr);
		if (status == UU_SUCCESS)
			key = utp_store_conic(&cptr,label);
		*relnum = UM_CONIC_REL;
	}
/*
.....Trimmed Curve
*/
	else if (ptr->command == TRIMMED_CURVE)
	{
		key = utp_in_trimmed_curve(ptr,relnum);
//		Sreverse = revfl;
	}
/*
.....B-spline Curve 
*/
	else
	{
		key = utp_in_curve(ptr,relnum);
//		Sreverse = revfl;
/*
.....Trim curve if necessary
*/
		if (key != 0)
		{
			utp_transform_point(spt);
			utp_transform_point(ept);
			S_trim_curve(key,spt,ept);
		}
	}
/*
.....End of routine
*/
	return(key);
}

/*********************************************************************
**    S_FUNCTION     :  S_trim_curve(key,spt,ept)
**       Determines if a curve needs to be trimmed (end points do not
**       match supplied end points) and then trims it if necessary.
**
**    PARAMETERS   
**       INPUT  : 
**          key      Key of curve to test.
**          spt      Start point of curve.
**          ept      End point of curve.
**       OUTPUT :  none
**    RETURNS      :
**          Key of created entity or 0 on failure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_trim_curve(key,spt,ept)
UU_KEY_ID key;
UM_coord spt,ept;
{
	int i,j,k,n,ist,ien;
	UU_REAL u,inc,uprm[2],udis[2],dis;
	UM_coord pt[2];
	UM_transf tfmat;
	struct UM_rbsplcrv_rec crv;
	struct UM_evcrvout evout;
/*
.....Retrieve the curve from the Unibase
*/
	crv.key = key;
	ncl_retrieve_data_fixed(&crv);
	uc_retrieve_transf (crv.key,tfmat);
	uc_init_evcrvout (&crv,&evout);
/*
.....Initialize routine
*/
	um_vctovc(spt,pt[0]);
	um_vctovc(ept,pt[1]);
/*
.....Get the start and ending points
.....of the curve and compare them
.....to the provided start and ending points
*/
	for (i=0;i<2;i++)
	{
		uprm[i] = 0.;
		uc_evcrv(UM_POINT, 0., &crv, tfmat, &evout);
		udis[i] = um_dcccc(evout.cp,pt[i]);
		uc_evcrv (UM_POINT, 1., &crv, tfmat, &evout);
		dis = um_dcccc(evout.cp,pt[i]);
		if (dis < udis[i])
		{
			uprm[i] = 1.;
			udis[i] = dis;
		}
	}
/*
.....Points to not match
.....find the curve projection of the point
*/
	if (udis[0] > UM_FUZZ || udis[1] > UM_FUZZ)
	{
		ist = 0; ien = 1;
		if (udis[0] <= UM_FUZZ) ist++;
		if (udis[1] <= UM_FUZZ) ien--;
		for (i=ist;i<=ien;i++)
		{
			u = 0.;
			inc = .01;
			n = 1. / inc + 1;
			for (k=0;k<2;k++)
			{
				for (j=0;j<n;j++)
				{
					uc_evcrv (UM_POINT, u, &crv, tfmat, &evout);
					dis = um_dcccc(evout.cp,pt[i]);
					if (dis < udis[i])
					{
						uprm[i] = u;
						udis[i] = dis;
						if (dis <= UM_FUZZ) break;
					}
					u += inc;
				}
/*
........Refine the projection
*/
				if (udis[i] > UM_FUZZ)
				{
					u = uprm[i] - inc; if (u < 0.) u = 0.;
					dis = uprm[i] + inc; if (dis > 1.) dis = 1.;
					inc = inc / 50.;
					n = (dis-u) / inc + 1;
				}
			}
		}
/*
.....Trim the curve
*/
		if (uprm[0] > uprm[1])
		{
			dis = uprm[0]; uprm[0] = uprm[1]; uprm[1] = dis;
		}
		if (uprm[0] != 0. || uprm[1] != 1.)
		{
			dis = crv.t1 - crv.t0;
			crv.t0 = uprm[0]*dis; crv.t1 = uprm[1]*dis;
			ur_update_data_fixed(&crv);
		}
	}
}
