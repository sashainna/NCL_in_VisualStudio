/*********************************************************************
**    NAME         :  m3ecomp3.c
**       CONTAINS:
**           um_check_connected()
**           um_c5_connect_comp()
**           um_c5_splitcompcrv()
**           um_c5_isectcomp()
**           um_c5_trimpart()
**           um_c5_endpoints()
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ecomp3.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:53
*********************************************************************/
#include "nccs.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "class.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdebug.h"
#include "nclfc.h"

#include "misect.h"

/*********************************************************************
**    E_FUNCTION     : um_check_connected(numkeys,keys)
**			Checks if entities to be in composite curve are connected and
**       whether connecting will make a closed curve.
**    PARAMETERS   
**       INPUT  : 
**          numkeys - number of keys in array
**				keys    - array of keys
**       OUTPUT :
**          closed  - UU_TRUE : curve will be closed
**                    UU_FALSE: otherwise
**    RETURNS      : UU_TRUE iff connected
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_check_connected(numkeys,keys,closed)
int numkeys;
UU_KEY_ID keys[];
UU_LOGICAL *closed;
{
	int i,nint;
	struct UM_crvdatabag comp1,comp2;
	UU_REAL dis1,dis2,dis3,dis4;
	UU_REAL tolr=1.e-3;
	UM_transf tfmat1,tfmat2;
	UM_coord cpt11,cpt12,cpt21,cpt22;
	UM_int2 ierr;
	UM_isect ibuff[UM_MAXISECT];
	UU_LOGICAL connected = UU_TRUE;
/*
.....See if end points match up
.......Key lists with uv curves will be handled in composite routine.
*/
	if (numkeys == 1) return (UU_TRUE);
	for (i=0;i<numkeys-1;i++)
	{
		comp1.key = keys[i];
		uc_retrieve_data(&comp1, sizeof(comp1));
		if (comp1.rel_num == UM_UVCVONSF_REL) return(UU_TRUE);
		uc_retrieve_transf(keys[i], tfmat1);
		um_get_endpts(&comp1,tfmat1,cpt11,cpt12);
		comp1.key = keys[i+1];
		uc_retrieve_data(&comp1, sizeof(comp1));
		if (comp1.rel_num == UM_UVCVONSF_REL) return(UU_TRUE);
		uc_retrieve_transf(keys[i+1], tfmat1);
		um_get_endpts(&comp1,tfmat1,cpt21,cpt22);
		dis1 = um_dcccc(cpt11,cpt21); dis2 = um_dcccc(cpt11,cpt22);
		dis3 = um_dcccc(cpt12,cpt21); dis4 = um_dcccc(cpt12,cpt22);
		if (dis1 > tolr && dis2 > tolr && dis3 > tolr && dis4 > tolr)
			connected = UU_FALSE;
	}
/*
.....Check to see if curve is closed or if first and last entities isect
*/
	comp1.key = keys[0];
	uc_retrieve_data(&comp1, sizeof(comp1));
	uc_retrieve_transf(keys[0], tfmat1);
	comp2.key = keys[numkeys-1];
	uc_retrieve_data(&comp2, sizeof(comp2));
	uc_retrieve_transf(keys[numkeys-1], tfmat2);
	ierr = 0;
	um_get_endpts(&comp1,tfmat1,cpt11,cpt12);
	um_get_endpts(&comp2,tfmat2,cpt21,cpt22);
	dis1 = um_dcccc(cpt11,cpt21); dis2 = um_dcccc(cpt11,cpt22);
	dis3 = um_dcccc(cpt12,cpt21); dis4 = um_dcccc(cpt12,cpt22);
	if (dis1 > tolr && dis2 > tolr && dis3 > tolr && dis4 > tolr)
		*closed = UU_FALSE;
	else
		*closed = UU_TRUE;
	nint = 0;
	ncl_isect_3d(&comp1,tfmat1,&comp2,tfmat2,UU_FALSE,cpt11,&nint,
		ibuff,&ierr);
	if (nint > 0 && ierr == 0)
	{
		for (i=0;i<nint;i++)
		{
			um_cctou(&comp1,tfmat1,ibuff[i].pt,&ibuff[i].u0,&dis1);
			um_cctou(&comp2,tfmat2,ibuff[i].pt,&ibuff[i].u1,&dis2);
			if (dis1<UM_DFUZZ && dis2<UM_DFUZZ && UM_DFUZZ<ibuff[i].u0
				&& fabs(ibuff[i].u0-1.)>UM_DFUZZ && UM_DFUZZ<ibuff[i].u1
				&& fabs(ibuff[i].u1-1.)>UM_DFUZZ)
				connected = UU_FALSE; 
		}
	}
	return(connected);
}
/*********************************************************************
**    E_FUNCTION     : um_c5_connect_comp(numkeys,keys)
**			Adds connecting geometry when needed and returns keys list
**       with all entites in it.
**    PARAMETERS   
**       INPUT  : 
**          numkeys  - number of keys in array
**				keys     - array of keys
**          type     - connection type to use (See um_compcrv_connect)
**       OUTPUT :  
**          key_list - list containing original and connectig geometry
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_c5_connect_comp(numkeys,keys,key_list,type)
int *numkeys,type;
UU_KEY_ID keys[];
UU_LIST *key_list;
{
	int i,num_in,status,firstfl,added=0,no_keys=*numkeys;
	UU_REAL tol=1.e-3;
	UU_KEY_ID *key,okey1,okey2,ikeys[3],okeys[2];
	UM_coord conpt,firstpt;
	UU_LOGICAL lrevs[2],use_conpt = UU_FALSE;
	UU_LOGICAL cut_int,closefl = UU_FALSE;

	firstfl = 0;
	cut_int = UU_TRUE;
	lrevs[0] = lrevs[1] = UU_FALSE;
	if (type < 0)
	{
		closefl = UU_TRUE;
		type *= -1;
		firstfl = 1;
	}

	for (i=0;i<no_keys;i++)
	{
/*
.....Break if not closing curve and all entities have been connected
*/
		if (!closefl && i == no_keys - 1) break;
/*
.....Do connecting for all except to close the curve
*/
		ikeys[0] = keys[i]; 
		if (i < no_keys - 2 || (i < no_keys - 1 && closefl))
		{
			ikeys[1] = keys[i+1];
			if (i < no_keys - 2)
				ikeys[2] = keys[i+2];
			else
				ikeys[2] = keys[0];
			num_in = 3;
		}
		else if (closefl)
		{
			firstfl = -1;
			ikeys[1] = keys[0];
			if (no_keys > 2)
			{
				ikeys[2] = keys[1];
				num_in = 3;
			}
			else
			{
				cut_int = UU_FALSE;
				num_in = 2;
			}
		}
		else
		{
			ikeys[1] = keys[i+1];
			num_in = 2;
		}
		status = um_compcrv_connect(ikeys,lrevs,num_in,tol,type,
			okeys,UU_FALSE,cut_int,conpt,use_conpt,firstpt,firstfl);
		use_conpt = UU_TRUE;
		firstfl = 0;
		uu_list_push(key_list,&keys[i]);
		if (status == UU_SUCCESS)
		{
			if (okeys[0] > NULLKEY)
			{
				added++;
				uu_list_push(key_list,&okeys[0]);
			}
			if (okeys[1] > NULLKEY)
			{
				added++;
				uu_list_push(key_list,&okeys[1]);
			}
		}
	}
	if (!closefl)
		uu_list_push(key_list,&keys[no_keys-1]);
	*numkeys += added;
}

/*********************************************************************
**    E_FUNCTION :  int um_c5_splitcompcrv(eptr, u, udel, crv1, crv2)
**			Split composite curve into two sections at a given u
**       parameter.
**    PARAMETERS   
**       INPUT  : 
**      	  eptr - Pointer to original composite curve
**			  u    - u parameter to split curve at
**			  udel - Unused
**			  crv1 - Copy of first half
**         crv2 - Copy of second half
**       OUTPUT : none.
**          
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : The original curve is copied twice along with
**                   all its component geometry
**    WARNINGS     : none
*********************************************************************/
int um_c5_splitcompcrv(eptr, u, udel, crv1, crv2)
struct UM_crvdatabag *eptr, *crv1, *crv2;
UM_param *u, *udel;
{
	int addflg;
	UM_transf tfmat;
	UU_REAL t0,t1,tu;
	struct UM_compcrv_rec *comp;
	UM_int2 lfl_77;
/*
.....Copy original curve structure into output curves
*/
	lfl_77 = 1;
	stunlb (&lfl_77);
	um_cp5_copycompcrv(eptr,crv2,sizeof(eptr));
	um_cp5_copycompcrv(eptr,crv1,sizeof(eptr));
	comp = (struct UM_compcrv_rec *)eptr;
	uc_retrieve_transf(eptr->key, tfmat);
	addflg = comp->addflg;
	t0 = comp->t0;
	t1 = comp->t1;
/*
.....Set t0 and t1 for first copy
*/
	comp = (struct UM_compcrv_rec *)crv1;
	if (*u < t0)
	{
		comp->t0 = *u;
		comp->t1 = t1;
	}
	else
	{
		comp->t0 = t0;
		comp->t1 = *u;
	}
	comp->addflg = addflg;
	if (um_is_curve_closed(comp, tfmat))
		comp->open = 0;
	else
		comp->open = 1;
	ur_update_data_fixed(comp);
	um_c5_update_len(comp,tfmat,comp->t0,comp->t1);
/*
.....Set t0 and t1 for second copy
*/
	comp = (struct UM_compcrv_rec *)crv2;
	if (*u < t1)
	{
		comp->t0 = *u;
		comp->t1 = t1;
	}
	else
	{
		comp->t0 = t0;
		comp->t1 = *u;
	}
	comp->addflg = addflg;
	if (um_is_curve_closed(comp, tfmat))
		comp->open = 0;
	else
		comp->open = 1;
	ur_update_data_fixed(comp);
	um_c5_update_len(comp,tfmat,comp->t0,comp->t1);
	stunlb (&lfl_77);

	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION :  um_c5_isectcomp (crv1p,tfmat,crv2p,ipt,npt,nint,
**                                     no_ibuf,ibuf)
**			Finds intersections between a composite curve's end
**       component and another curve.  The end to used is determined
**       based on the near point.  This intersection routine is used
**       if the additional curve does not intersect the composite
**       curve, but may intersect its extension.
**    PARAMETERS   
**       INPUT  : 
**      	  crv1p   - Pointer to composite curve
**         tfmat   - Composite curve transformation matrix
**         crv2p   - Pointer to curve to intersect
**         ipt     - Intersection type used in function calls
**         npt     - Near point
**         no_ibuf - Length of ibuf
**       OUTPUT :
**         nint    - Number of intersections found
**         ibuf    - Intersection records
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_c5_isectcomp (crv1p,tfmat,crv2p,ipt,npt,nint,no_ibuf,ibuf)
struct UM_compcrv_rec *crv1p;
UM_transf tfmat;
struct NCL_fixed_databag *crv2p;
int *ipt;
UM_coord npt;
int *nint, no_ibuf;
UM_isect ibuf[];
{
	int i,ind,status;
	UM_coord spt,ept;
	struct UM_crvdatabag crv;
	struct UM_evcrvout evout;
	struct UM_line_rec ln;
	UU_REAL dis1,dis2,u0,u1,t0,t1,len;
	UM_coord t0pt,t1pt,pt1,pt2;
	UM_vector t0vc,t1vc;
	UM_transf tfmat2;
	UM_int2 lfl_77;
	
	lfl_77 = 1;
	stunlb (&lfl_77);
/*
.....Set up compcrv to get ends
*/
	t0 = crv1p->t0;
	t1 = crv1p->t1;
	crv1p->t0 = 0.;
	crv1p->t1 = 1.;
	um_c5_update_len(crv1p,tfmat,crv1p->t0,crv1p->t1);
	ur_update_data_fixed(crv1p);
/*
.....Get points and tangent vectors at ends of curve
*/
	uc_init_evcrvout(crv1p, &evout);
	uc_evcrv(UM_FRSTDERIV,0.,crv1p,tfmat,&evout);
	um_vctovc(evout.cp,t0pt);
	um_vctovc(evout.dcdu,t0vc);
	um_unitvc(t0vc,t0vc);
	uc_evcrv(UM_FRSTDERIV,1.,crv1p,tfmat,&evout);
	um_vctovc(evout.cp,t1pt);
	um_vctovc(evout.dcdu,t1vc);
	um_unitvc(t1vc,t1vc);
	dis1 = um_dcccc(npt,t0pt);
	dis2 = um_dcccc(npt,t1pt);
	if (dis1 < dis2)
	{
		ind = 0;
		u0 = 0.;
	}
	else
	{
		ind = crv1p->no_cid-1;
		u0 = crv1p->cid[ind-1].endparam;
	}
	u1 = crv1p->cid[ind].endparam;
	crv.key = crv1p->cid[ind].crvid;
	uc_retrieve_transf(crv.key, tfmat2);
	ncl_retrieve_data_fixed(&crv);
	len = um_getarclen(&crv, tfmat2);
/*
.....Use a small length line so the u parameter for the intersection
.....will be closer to the parameter for the original component
*/
	if (ind == 0)
	{
		um_vctovc(t0pt,pt2);
		um_translate_point(pt2,-len,t0vc,pt1);
	}
	else
	{
		um_vctovc(t1pt,pt1);
		um_translate_point(pt1,len,t1vc,pt2);
	}
/*
.....Set up extension line and get intersection
*/
	ln.key = 0;
	ln.rel_num = 2;
	ln.no_displst = 0;
	um_vctovc(pt1,ln.spt);
	um_vctovc(pt2,ln.ept);
	strcpy (ln.label,"");
	um_create_geom(&ln,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	status = um_proj_isect_curves (&ln,crv2p,ipt,npt,nint,no_ibuf,ibuf);
	for (i=0;i<*nint;i++)
	{
/*
.....Add/Subtract 1. to get u parameter closer to what it would be
.....for the actual component when intersection is beyond u=1. on
.....component.
*/
		if (ind == 0) ibuf[i].u0 -= 1.;
		else ibuf[i].u0 += 1.;
		ibuf[i].u0 = u0 + ibuf[i].u0 * (u1 - u0);
	}
	uc_delete(ln.key);
	stunlb (&lfl_77);
	crv1p->t0 = t0;
	crv1p->t1 = t1;
	um_c5_update_len(crv1p,tfmat,crv1p->t0,crv1p->t1);
	ur_update_data_fixed(crv1p);
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  um_c5_trimpart(eptr,cid,no_cid,t0,t1,ind,tfmat)
**			Trims a component of a composite curve for display.
**    PARAMETERS   
**       INPUT  : 
**      	  eptr   - Pointer to component curve
**         cid    - Point to composite curve cid record
**         no_cid - Number of cid records
**         t0     - t0 value for composite curve
**         t1     - t1 value for composite curve
**         ind    - index of component in cid record
**         tfmat  - Transformation matrix for component curve
**       OUTPUT :
**         none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : Additional geometry is defined to create the
**                   trimmed version of the component.
**    WARNINGS     : none
*********************************************************************/
int um_c5_trimpart(eptr,cid,no_cid,t0,t1,ind,tfmat)
struct UM_crvdatabag *eptr;
struct UM_cid_rec *cid;
int no_cid,ind;
UU_REAL t0,t1;
UM_transf tfmat;
{
	struct UM_crvdatabag *tptr,*eptr1,*eptr2;
	int status = UU_SUCCESS;
	int size;
	UU_REAL uu,u0,u1,uu0,uu1;
	UU_LOGICAL allocated = UU_FALSE,opp,rev;
	UM_param inpara;
	UU_KEY_ID key;
	struct UM_evcrvout evout;
	UM_int2 lfl_77;

	lfl_77 = 1;
	stunlb (&lfl_77);
	key = eptr->key;
	size = um_curve_size (eptr);
	eptr1 = eptr2 = UU_NULL;
	tptr = eptr;

	if (t0!=0. && (ind>0 && cid[ind-1].endparam<=t0 || (ind==0)))
	{
		um_allocate_curve (&eptr1,size);
		um_allocate_curve (&eptr2,size);
		allocated = UU_TRUE;
		if (ind == 0) u0 = 0.;
		else u0 = cid[ind-1].endparam;
		u1 = cid[ind].endparam;
		rev = cid[ind].reverse;
		uu = (t0 - u0) / (u1 - u0);
		if (rev)
			uu = 1. - uu;
		inpara = (cid[ind].reverse)? 1. : 0.;
		if (uu > UM_DFUZZ && uu < 1. - UM_DFUZZ)
		{
			um_splitcurve(eptr,&uu,&inpara,eptr1,eptr2);
			if (inpara < uu) 
			{
				tptr = eptr2;
				u0 = u0 + uu * (u1 - u0);
			}
			else 
			{
				tptr = eptr1;
				u1 = u0 + uu * (u1 - u0);
			}
		}
		else
		{
			uc_copy(eptr,eptr1,size);
			uc_init_evcrvout(eptr1, &evout);
			uc_evcrv(UM_FRSTDERIV,uu,eptr1,tfmat,&evout);
			um_extend_curve(UU_NULL,evout.cp,uu,&inpara,eptr1);
			tptr = eptr1;
		}
		if(ncl_itis_uvcv(tptr))
		{
			struct UM_uvcvonsf_rec uvcv;
			status = ncl_conv_trim_rbcv_uvcv(tptr,&uvcv);
			tptr = (struct UM_crvdatabag *)(&uvcv);
		}
		tptr->key = key;
		ur_update_data_fixed(tptr);
	}
	if ((t1 != 1. && t1 < cid[ind].endparam) ||
		(t1 > 1. && ind == no_cid - 1))
	{
		if (!allocated)
		{
			um_allocate_curve (&eptr1,size);
			um_allocate_curve (&eptr2,size);
			if (ind == 0) uu0 = 0.;
			else uu0 = cid[ind-1].endparam;
			uu1 = cid[ind].endparam;
			uu = (t1 - uu0) / (uu1 - uu0);
			allocated = UU_TRUE;
		}
		else 
		{
			uu0 = u0;
			uu1 = u1;
		}
		uu = (t1 - uu0) / (uu1 - uu0);
		rev = cid[ind].reverse;
		if (rev)
			uu = 1. - uu;
		inpara = (cid[ind].reverse)? 0. : 1.;
		if (uu > UM_DFUZZ && uu < 1. - UM_DFUZZ)
		{
			um_splitcurve(tptr,&uu,&inpara,eptr1,eptr2);
			if (inpara < uu) tptr = eptr2;
			else tptr = eptr1;
		}
		else
		{
			uc_copy(tptr,eptr1,size);
			uc_init_evcrvout(eptr1, &evout);
			uc_evcrv(UM_FRSTDERIV,uu,eptr1,tfmat,&evout);
			um_extend_curve(UU_NULL,evout.cp,uu,&inpara,eptr1);
			tptr = eptr1;
		}
		if(ncl_itis_uvcv(tptr))
		{
			struct UM_uvcvonsf_rec uvcv;
			status = ncl_conv_trim_rbcv_uvcv(tptr,&uvcv);
			tptr = (struct UM_crvdatabag *)(&uvcv);
		}
		tptr->key = key;
		ur_update_data_fixed(tptr);
	}
	if (eptr1 != UU_NULL) uu_toolfree ((struct UM_crvdatabag *)eptr1);
	if (eptr2 != UU_NULL) uu_toolfree ((struct UM_crvdatabag *)eptr2);
	stunlb(&lfl_77);
	return(0);
}

/*********************************************************************
**    E_FUNCTION :  int um_c5_endpoints(eptr, u, pt, udel)
**			Calculate the t0 or t1 value for the composite curve based
**       on the given parameter.
**    PARAMETERS   
**       INPUT  : 
**      	  eptr - Pointer to original composite curve
**			  u    - u parameter to end curve at
**         pt   - Intersection point
**			  udel - Unused
**       OUTPUT : none.
**          
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_c5_endpoints(eptr, u, pt, udel)
struct UM_compcrv_rec *eptr;
UM_coord pt;
UM_param u, *udel;
{
	int i,ind,no_cids;
	UM_param urng,t,uu,t0,t1;
	UU_LIST cid_list;
	struct UM_cid_rec tcid,*cids;
	struct UM_line_rec ln;
	struct UM_evcrvout evout;
	struct UM_crvdatabag crv;
	UM_int2 lfl_77;
	UM_transf tfmat;
	UU_LOGICAL start;
	UU_REAL len1,len2,dist;
	UM_coord t0pt,t1pt;

	t0 = eptr->t0;
	t1 = eptr->t1;
/*
.....Get original curve arclen
*/
	len1 = 0.;
	for (i=0;i<eptr->no_cid;i++)
	{
		crv.key = eptr->cid[i].crvid;
		uc_retrieve_transf(crv.key, tfmat);
		ncl_retrieve_data_fixed(&crv);
		len1 += um_getarclen(&crv, UM_idmat);
	}
	no_cids = 0;
	urng = t1 - t0;
	t = t0 + u * urng;
	uu_list_init(&cid_list,sizeof(struct UM_cid_rec),eptr->no_cid,1);
/*
.....Get original t0 and t1 points on curve
*/
	uc_init_evcrvout(eptr, &evout);
	uc_retrieve_transf(eptr->key, tfmat);
	uc_evcrv(UM_FRSTDERIV,0.,eptr,tfmat,&evout);
	um_vctovc(evout.cp,t0pt);
	uc_evcrv(UM_FRSTDERIV,1.,eptr,tfmat,&evout);
	um_vctovc(evout.cp,t1pt);
/*
.....Get original cid record
*/
	for (i=0;i<eptr->no_cid;i++)
	{
		tcid.crvid = eptr->cid[i].crvid;
		tcid.endparam = eptr->cid[i].endparam;
		tcid.reverse = eptr->cid[i].reverse;
		if (tcid.reverse)
			tcid.crvid = -tcid.crvid;
		uu_list_push(&cid_list,&tcid);
		no_cids++;
	}
/*
.....Create new extension
*/
	if (eptr->addflg == 0 || ((u < 0. && eptr->addflg == 2) ||
		(u > 1. && eptr->addflg == 1)))
	{
		lfl_77 = 1;
		stunlb (&lfl_77);
		if (u < 0.0) 
		{
			ind = 0;
			uu = (eptr->cid[0].reverse)? 1. : 0.;
			start = UU_TRUE;
		}
		else if (u > 1.0)
		{
			ind = eptr->no_cid - 1;
			uu = (eptr->cid[ind].reverse)? 0. : 1.;
			start = UU_FALSE;
		}
		crv.key = eptr->cid[ind].crvid;
		ncl_retrieve_data_fixed(&crv);
		uc_init_evcrvout(&crv, &evout);
		uc_retrieve_transf(eptr->cid[ind].crvid, tfmat);
		uc_evcrv(UM_FRSTDERIV,uu,&crv,tfmat,&evout);
		if (start)
		{
			um_vctovc(pt,ln.spt);
			um_vctovc(evout.cp,ln.ept);
			eptr->addflg += 1;
		}
		else
		{
			um_vctovc(pt,ln.ept);
			um_vctovc(evout.cp,ln.spt);
			eptr->addflg += 2;
		}
		ln.key = 0;
		ln.rel_num = 2;
		ln.no_displst = 0;
		strcpy (ln.label,"");
		um_create_geom(&ln,UM_DEFAULT_TF,UM_CURRENT_ATTR);
		ur_update_displayable(ln.key, UM_NEVERDISPLAYABLE);
		len2 = um_getarclen(&ln, UM_idmat);
		stunlb (&lfl_77);
		tcid.crvid = ln.key;
		if (start) uu_list_insert (&cid_list,0,&tcid);
		else uu_list_push(&cid_list,&tcid);
		no_cids++;
	}
	else
	{
		if (u < UM_DFUZZ) 
		{
			ln.key = eptr->cid[0].crvid;
			ncl_retrieve_data(&ln,sizeof(ln));
			um_vctovc(pt,ln.spt);
			start = UU_TRUE;
		}
		else
		{
			ln.key = eptr->cid[eptr->no_cid-1].crvid;
			ncl_retrieve_data(&ln,sizeof(ln));
			um_vctovc(pt,ln.ept);
			start = UU_FALSE;
		}
		ur_update_data(&ln);
	}
/*
.....Update curve data
*/
	cids = (struct UM_cid_rec *) UU_LIST_ARRAY (&cid_list);
	ur_update_data_varlist (eptr->key, 1, cids, 1, no_cids);
	eptr->no_cid = no_cids;
	ur_update_data_fixed(eptr);
	uc_retrieve_transf(eptr->key, tfmat);
	ncl_retrieve_data_fixed(eptr);
	eptr->t0 = 0.;
	eptr->t1 = 1.;
	umi_fix_subcurve_fields(eptr, tfmat);
	um_c5_update_len(eptr,tfmat,eptr->t0,eptr->t1);
	ur_update_data_fixed(eptr);
/*
.....Update t0 and t1 based on new curve geometry
*/
	if (fabs(t0) > UM_DFUZZ)
		um_cctou_compcrv(eptr,tfmat,t0pt,&t0,&dist);
	if (fabs(t1 - 1.) > UM_DFUZZ)
		um_cctou_compcrv(eptr,tfmat,t1pt,&t1,&dist);
	if (start)
	{
		eptr->t0 = 0.;
		eptr->t1 = t1;
	}
	else
	{
		eptr->t0 = t0;
		eptr->t1 = 1.;
	}
	um_c5_update_len(eptr,tfmat,eptr->t0,eptr->t1);
	ur_update_data_fixed(eptr);
	uu_list_free(&cid_list);
	if (um_is_curve_closed(eptr, tfmat))
		eptr->open = 0;
	else
		eptr->open = 1;
	ur_update_data_fixed(eptr);

	return(UU_SUCCESS);
}
