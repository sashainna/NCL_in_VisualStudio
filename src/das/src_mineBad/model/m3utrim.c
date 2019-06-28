/*********************************************************************
**    NAME         :  m3utrim.c
**       CONTAINS: user interface routines for trim/extend, midtrim,
**              fillet, and split 
**        umu_trim_curve()
**        umu_split_curve()
**        umu_midtrim_curve()
**        umu_fillet()
**        um_get_trim_curves (keys, e, tfmat)
**        um_prep_trim_curve (e,tfmat,ploc,pt,uall,ptp,uend)
**        um_prep_midtrim_curve (e,tfmat,ploc,pt1,pt2,uall,ptp,uend)
**        um_trim_modifier(ibuff,nint,which,lmod)
**        um_trim_direction(pt1,pt2,lmod)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**        m3utrim.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:08:00
*********************************************************************/

#include "nccs.h"
#include "ncldef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h"
#include "modef.h"
#include "mdclass.h"
#include "mdgenent.h"
#include "mattr.h"
#include "mdpick.h"
#include "misect.h"
#include "mdebug.h"
#include "mdeval.h"
#include "nclvx.h"

/*
.....Set the DAS level for picking composite curves
.....so we do not get the underlying curves (@UN)
.....Vadim  -  6/20/97
*/
static int daslev=1;
extern char UM_RD_name[32];
char *UU_ldir[8] = {"XSMALL","XLARGE","YSMALL","YLARGE","ZSMALL","ZLARGE",
						  "CENTER","OUT"};
UM_transf ref_tm, invref_tm, mod_tm, invmod_tm;
UM_transf *REFtm, *MODtm;
 
/*********************************************************************
**    E_FUNCTION: umu_trim_curve(keys, eloc)
**      Prompt the user for a curve to trim to and then repeatedly
**      prompt for a curve to trim.
**    PARAMETERS   
**       INPUT  : 
**          keys   -  list of keys of picked entities
**          eloc   -  list of pick locations
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_trim_curve (keys,eloc)
UU_KEY_ID *keys;
UD_NDCLOCREC *eloc;             /* picked location */

{
	struct NCL_fixed_databag *elst[3];   /*  pointers to e structures */
	struct NCL_fixed_databag *rbcv, *ptrcv[3];   /*  pointers to e structures */
	struct NCL_fixed_databag *e;         /*  e[2] data for curve to trim 
 													     e[0] data for curve to trim to */
	UM_transf *tfmat;               /* transformation for curves to trim */
	UM_param uall[10], uend;
	UM_coord  pt, pt2, nrpt;
	int  status, type, i, inp = 0;
	UM_int2 ifl;
	UU_LOGICAL isuvcv;
	char lmod1[9], lmod2[9], ldir[9];
	struct NCL_fixed_databag  *projcrv0, *projcrv1;

	uu_denter(UU_MTRC,(us,"umu_trim_curve()"));

	projcrv0 = UU_NULL;
	projcrv1 = UU_NULL;

	strcpy(lmod1,"        ");
	strcpy(lmod2,"        ");
	strcpy(ldir ,"        ");
/*
.....vp 4-mar-97 
...Check if REFSYS (MODSYS) is active and get tfmats
...once for single trim.
*/
	gtref (ref_tm,&ifl);
	REFtm = (ifl)? (UM_transf *) invref_tm: UU_NULL;
	if (REFtm) um_inverttf (ref_tm,invref_tm);
	gtmod (mod_tm,&ifl);
	MODtm = (ifl)? (UM_transf *) invmod_tm: UU_NULL;
	if (MODtm) um_inverttf (mod_tm,invmod_tm);
/* 
...allocate storage for entities & trans mx
...and get entity  
*/
	tfmat = (UM_transf *) uu_malloc(3*sizeof(UM_transf));
	e = (struct NCL_fixed_databag *) uu_malloc(3*sizeof(struct NCL_fixed_databag));
   for(i=0; i<3;i++) elst[i] = &e[i];
	status = um_get_trim_curves (keys,elst,tfmat);
	if (status != UU_SUCCESS) goto done;
/*
...Prepare data to trim & trim/extend curve 
... aak 06-nov-1997: if uv-curve on surface, copy to a rbspl curve
... and use intersectiopn routines for rbspl's
... aak 01-dec-1997: disable trimming of CVonSF to anything else but CVonSF
*/
	rbcv = (struct NCL_fixed_databag *) uu_malloc(3*sizeof(struct NCL_fixed_databag));
	isuvcv = ncl_itsa_uvcv_onsf(elst[0]);
	for(i=0 ;i<3 && elst[i]->key > 0 ; i++)
	{
		ptrcv[i] = elst[i];
		if (isuvcv != ncl_itsa_uvcv_onsf(elst[i]) )
		{
			status = UU_FAILURE;
			goto done;
		}
   	else if(ncl_itsa_uvcv_onsf(elst[i]))
		{
         elst[i] = &rbcv[i];
   		status    = ncl_cp_struct_uvcv_rbcv(ptrcv[i],&elst[i]);
			if (status != UU_SUCCESS) goto done;
		}
	}

	strcpy (UM_RD_name,"");
	type = 1;
	if (keys[0] > 0)
	{
		if (keys[1] == 0)
		{

			status = um_prep_trim_curve (elst,tfmat,eloc,pt,lmod1,uall,&inp,nrpt,ldir,&uend,ptrcv);
			if (status == UU_SUCCESS)
			{
				if (UU_application == UU_NCLCAM)
         			nclu_put_cmd (type,elst,pt,lmod1,pt2,lmod2,inp,nrpt,ldir);
/* 
... aak 06-nov-1997: was
...         status = uc_trim_extend_curve(&e[0], tfmat, pt, uall, uend);
*/
				else
					status = uc_trim_extend_curve(elst[0], tfmat, pt, uall, uend);
			}
		}
/*
...midtrim curve
*/
		else {
			type = 2;
			status = um_prep_midtrim_curve (elst,tfmat,eloc,pt,lmod1,pt2,lmod2,uall,
						nrpt,ldir,&uend,ptrcv);
			if (status == UU_SUCCESS)
      		if (UU_application == UU_NCLCAM)
         		nclu_put_cmd (type,elst,pt,lmod1,pt2,lmod2,inp,nrpt,ldir);
/*
... aak 06-nov-1997: was
...            status = uc_midtrim_curve(&e[0], tfmat, pt, pt2, uall, uend); 
*/
				else
					status = uc_midtrim_curve(elst[0], tfmat, pt, pt2, uall, uend); 
		} 
	}
	else if (UU_application == UU_NCLCAM)
		nclu_put_cmd (type,elst,pt,lmod1,pt2,lmod2,inp,nrpt,ldir);
	else
		status = um_redef_curve (elst,tfmat);
/* 
...Write command in pp file
*/

done:;
	if (status != UU_SUCCESS)
		uu_uerror0(/* unable to trim this curve */UM_MODEL, 282);
	/*else
		if (UU_application == UU_NCLCAM)
			nclu_put_cmd (type,elst,pt,lmod1,pt2,lmod2,inp,nrpt,ldir);
*/
/*  ud_lgeo(UU_FALSE, UD_circle); */
	uu_free(e);
	uu_free(rbcv);
	uu_free(tfmat);
	uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION: umu_split_curve()
**      Prompt the user for a curve to split to and then repeatedly
**      prompt for a curve to split.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_split_curve()

{
	UM_PLOCREC pick;             /* pick information */
	struct UM_crvdatabag *e0;    /* data for curve to trim */
	UM_transf tfmat0;            /* transformation for curve to trim */
	struct UM_crvdatabag *e1;    /* data for curve to trim to */
	UM_transf tfmat1;            /* transformation for curve to trim to */
	UD_NDCLOCREC ploc[2];        /* picked location */
	int nint;                    /* number of intersections of trim
											 element with control element */
	UM_param uparam[20];
	UM_coord ccsplit[20];
	UM_param u, udel, uu[2];
	UU_REAL dist;
	UM_isect ibuff[10];          /* intersection points */
	struct UM_crvdatabag *eptr1;
	struct UM_crvdatabag *eptr2;
	struct UM_crvdatabag *eptr3;
	struct UM_crvdatabag *esplit;
	struct UM_crvdatabag *part1;
	struct UM_crvdatabag *part2;
	struct UM_crvdatabag *part3;
	struct UM_attrdata_rec attr;
	UU_REAL l_bound, u_bound;
	int numint;
	int status;
	int i,j,k,l;
	int bagsize;
	int errflag;
	int dsegid;

	uu_denter(UU_MTRC,(us,"umu_split_curve()"));
/*
  bagsize = sizeof(struct UC_entitydatabag);
  e0 = (struct UC_entitydatabag *) uu_malloc(bagsize);
  e1 = (struct UC_entitydatabag *) uu_malloc(bagsize);
  eptr1 = (struct UC_entitydatabag *) uu_malloc(bagsize);
  eptr2 = (struct UC_entitydatabag *) uu_malloc(bagsize);
  eptr3 = (struct UC_entitydatabag *) uu_malloc(bagsize);
*/
	bagsize = sizeof(struct UM_crvdatabag);
	e0 = (struct UM_crvdatabag *) uu_toolmalloc(bagsize);
	e1 = (struct UM_crvdatabag *) uu_toolmalloc(bagsize);

	udel = -1.0;

	ud_lgeo(UU_TRUE, UD_splitable_curves);

	while (UU_TRUE)
	{
		ud_leditable(UU_FALSE);
		um_dl_pldas(UD_DASPCKLOC,/*pick spliting curve */
						UM_MODEL, 267, &pick, 1, &numint, daslev);
		if (numint <= 0) goto done;

		e1->key = um_get_pickkey(&pick.pent, daslev);
		um_copyploc(&pick.ploc, &ploc[1]);
		um_retrieve_data_relnum(e1->key, &e1->rel_num);

/*    status = uc_retrieve_data(e1, bagsize);   */
		status = ncl_retrieve_data_fixed(e1, bagsize);
		if (status != UU_SUCCESS) goto repeat0;
		status = uc_retrieve_transf(e1->key, tfmat1);
		if (status != UU_SUCCESS) goto repeat0;

		while (UU_TRUE)
		{
			ud_leditable(UU_TRUE);
			um_dl_pldas(UD_DASPCKLOC, /*pick curve to split */UM_MODEL, 
						  268, &pick, 1, &numint, daslev);
			if (numint <=0) goto repeat1;

			e0->key = um_get_pickkey(&pick.pent, daslev);
			um_copyploc(&pick.ploc, &ploc[0]);
			um_retrieve_data_relnum(e0->key, &e0->rel_num);

			if (e0->key == e1->key)
			{
				uu_uerror0(/*trim element and trim entity are the same*/
							  UM_MODEL,103);
				goto repeat0;
			}

/*      status = uc_retrieve_data(e0, bagsize);   */
			status = ncl_retrieve_data_fixed(e0, bagsize);
			if (status != UU_SUCCESS) goto repeat0;
			status = uc_retrieve_transf(e0->key, tfmat0);
			if (status != UU_SUCCESS) goto repeat0;
			status = uc_retrieve_attr(e0->key, &attr);
			if (status != UU_SUCCESS) goto repeat0;

			status = uc_crv_intersect(e0, tfmat0, e1, tfmat1, 
							  &nint, 10, ibuff, &errflag);
			if (status != UU_SUCCESS) goto repeat0;

			if (nint > 0)
			{
/* 
...remove intersections outside of curve segment 
*/
				l_bound = UM_FUZZ;
				u_bound = 1.0 - UM_FUZZ;
				for (i=0, k=0; i<nint; i++)
				{
					if ((l_bound < ibuff[i].u0) && (ibuff[i].u0 < u_bound))
					{
						ibuff[k].u0 = ibuff[i].u0;
						um_vctovc(ibuff[i].pt, ibuff[k].pt);
						k++;
					}
				}
				nint = k;
			}
			if (nint  ==  0)
			{
				uu_uerror0(/*entity to trim doesn't intersect trim element*/
						UM_MODEL,112);
				goto repeat1;
			}
/*
...sort intersections in increasing parameter
*/
			else
			{
				for (i=0, k=0; i<nint; i++)
				{
					for (j=0; (j<k) && (ibuff[i].u0 >= ibuff[j].u0); j++);
						if (j == k)
						{
							uparam[k] = ibuff[i].u0;
							um_vctovc(ibuff[i].pt, ccsplit[k]);
							k++;
						}
						else
						{
							for (l=k; j<l; l--)
							{
								uparam[l] = uparam[l-1];
								um_vctovc(ccsplit[l-1], ccsplit[l]);
							}
							uparam[j] = ibuff[i].u0;
							um_vctovc(ibuff[i].pt, ccsplit[j]);
							k++;
						}
				}
				nint = k;
				uparam[nint] = 1.0;
				um_p_ary(UM_PFLOAT,"split at", nint+1, uparam);
				um_alloc_eq_curve (e0,&eptr1);
				um_alloc_eq_curve (e0,&eptr2);
				um_alloc_eq_curve (e0,&eptr3);
				part1 = eptr1;
				part2 = eptr2;
				part3 = eptr3;
				esplit = e0;
				ur_retrieve_disp_segid(e0->key, &dsegid);
				uv_blanksegs(dsegid, e0->key);
/*
...RAH: to preserve labels ... make first segment label same as
...     original curve 
*/
				if (nint > 0)
				{
					i=nint;
/*
...build first segment, name it to original curve and update 
*/
					uc_cctou(esplit, UM_DEFAULT_TF, ccsplit[i-1], &uu[0], &dist);
					uu[1] = uparam[0]; 
					u  = udel;
					status = uc_split_curve(esplit, &uu[0], &udel, part1, part2);
					part2->key = esplit->key;
					strncpy(part2->label, esplit->label, NCL_MAX_LABEL);
					part2->subscr = esplit->subscr;
					um_update_geom(part2, tfmat0);
					uc_display(part2);
					esplit = part1;
					part1 = part2;
					part2 = part3;
					part3 = esplit;
/*
...for closed curve the first segment is connected to
...to the last, don't output it (only CI is supported).
*/
					if (u != udel) i--;
					i--;

/*
...now continue as before for remaining segments if any 
*/
					for (; i>0; i--)
					{
				/*u = uparam[i-1] / uparam[i];*/
						uc_cctou(esplit, UM_DEFAULT_TF, ccsplit[i-1], &uu[0], &dist);
						uu[1] = uparam[i]; 
						status = uc_split_curve(esplit, &uu[0], &udel, part1, part2);
						if (status != UU_SUCCESS) goto repeat0;
						uc_create_data(part2, tfmat0, &attr);
						uc_display(part2);
						esplit = part1;
						part1 = part2;
						part2 = part3;
						part3 = esplit;
					}
				}
				uc_create_data(esplit, tfmat0, &attr);
				uc_display(esplit);
/*RAH: we made the first segment the same as the original-
			DON'T DELETE THE ORIGINAL! */
			/* uc_delete(e0->key); */
			}
repeat0:;
		}
repeat1:;
	}
done:;
	ud_lgeo(UU_FALSE, UD_splitable_curves);
	uu_toolfree(e0);
	uu_toolfree(e1);
	uu_toolfree(eptr1);
	uu_toolfree(eptr2);
	uu_toolfree(eptr3);
	uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : umu_midtrim_curve(keys,eloc)
**
**      !!! not used, see umu_trim_curve !!! vp 28-mar-95 !!! 
**
**      Perform a "mid trim" operation on a picked curve by splitting
**      this curve into three parts between the intersection of the
**      curve and two other curves and keeping either the middle 
**      section or the two exterior sections depending upon where the
**      user picked the curve to mid trim.
**    PARAMETERS   
**       INPUT  : 
**          keys   - List of kyes of the picked entities.
**          eloc   - Pick locations of entities / points.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_midtrim_curve(keys,eloc)
UU_KEY_ID *keys;
UD_NDCLOCREC *eloc;                /* picked location */

{
	int        numint;                 /* number of DAS items returned */
	UM_PLOCREC    pick;                /* structure for picking */
	UD_NDCLOCREC  ploc[3];             /* pick points */
	struct        UM_crvdatabag *et[3];/* 0 => entity to trim
													 1 => first entity to trim to
													 2 => second entity to trim to */
	UM_transf  tfmat[3];               /* entity transformations */
	int        nint;                   /* number of intersection points */
	UM_isect   ibuff[10];              /* intersection points */
	int        closest;                /* index of closest intersection point */
	UM_isect   cipt[2];                /* intersection points closest to
													 picked point */
	int        status;
	int        i;
	int        bagsize;
	int        errflag;

	uu_denter( UU_MTRC, (us, "umu_midtrim_curve()"));

	bagsize = sizeof(struct UM_crvdatabag);
	et[0] = (struct UM_crvdatabag *) uu_malloc(bagsize);
	et[1] = (struct UM_crvdatabag *) uu_malloc(bagsize);
	et[2] = (struct UM_crvdatabag *) uu_malloc(bagsize);

	while (UU_TRUE)
	{
/*
... aak 31-oct-1997: added uv curves on surfaces; was
      ud_lgeo(UU_TRUE, UD_vircurves); 
*/
		ud_lgeo(UU_TRUE, UD_allcurvess_pts);  

		ud_leditable(UU_FALSE);
		um_dl_pldas(UD_DASPCKLOC, /*pick first element to trim to*/UM_MODEL,
		167, &pick, 1, &numint, daslev);
		if (numint == 0) break;
		et[1]->key = um_get_pickkey(&pick.pent, daslev);
		um_copyploc(&pick.ploc, &ploc[1]);
		um_retrieve_data_relnum(et[1]->key, &et[1]->rel_num);

		ud_leditable(UU_FALSE);
		um_dl_pldas(UD_DASPCKLOC, /*pick second element to trim to*/UM_MODEL,
						168, &pick, 1, &numint, daslev);
		if (numint == 0) continue;
		et[2]->key = um_get_pickkey(&pick.pent, daslev);
		um_copyploc(&pick.ploc, &ploc[2]);
		um_retrieve_data_relnum(et[2]->key, &et[2]->rel_num);

		while (UU_TRUE)
		{
			ud_lgeo(UU_TRUE, UD_midtrimcurves);
			ud_leditable(UU_TRUE);
			um_dl_pldas(UD_DASPCKLOC, /*pick part of entity to keep*/UM_MODEL,
						  166, &pick, 1, &numint, daslev);
			if (numint == 0) break;

			et[0]->key = um_get_pickkey(&pick.pent, daslev);
			um_copyploc(&pick.ploc, &ploc[0]);
			ur_retrieve_data_relnum(et[0]->key, &et[0]->rel_num);

			if (et[1]->key == et[0]->key || et[2]->key == et[0]->key)
			{
				uu_uerror0(/*trim entity and trim element are the same*/ UM_MODEL,103);
				continue;
			}

			for (i=0; i<3; i++)
			{
				uc_retrieve_data(et[i], sizeof(struct UC_entitydatabag));
				uc_retrieve_transf(et[i]->key, tfmat[i]);
			}
	 
		/* Intersect the curves and determine the intersection point
		  closest to the pick locations */
			for (i=1; i<3; i++)
			{
				status = uc_crv_intersect(et[0], tfmat[0], et[i], tfmat[i],
								&nint, 10, ibuff, &errflag);
				if ((status != UU_SUCCESS) || (nint == 0))
				{
					uu_uerror0(/*entity to trim does not intersect trim element %d*/ 
									UM_MODEL,112);
					goto next_entity;
				}
				closest = um_nearest_isect_to_ploc(&ploc[i],nint,ibuff);
				um_vctovc(ibuff[closest].pt,cipt[i-1].pt);
				cipt[i-1].t0 = ibuff[closest].t0;
				cipt[i-1].t1 = ibuff[closest].t1;
				cipt[i-1].u0 = ibuff[closest].u0;
				cipt[i-1].u1 = ibuff[closest].u1;
				cipt[i-1].order = ibuff[closest].order;
			}

			if (um_cceqcc(cipt[0].pt,cipt[1].pt))
			{
				uu_uerror0(/*the intersection points are the same*/UM_MODEL,113);
				continue;
			}

			uc_midtrim_curve(&ploc[0], &cipt[0], &cipt[1], et[0]);

			next_entity:;
		}  /* end of inner while */  
	}  /* end of outer while */    
/*  ud_lgeo(UU_FALSE, UD_circle); */
	uu_free(et[0]);
	uu_free(et[1]);
	uu_free(et[2]);
	uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : umu_fillet()
**      Fillet lines.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_fillet()

{
	UM_PLOCREC pick;              /* pick information */
	UD_NDCLOCREC ploc[3];          /* picked location on entities picked */
	UU_REAL radius;              /* radius of tangent circle */
	struct UM_circle_rec c;          /* new tangent circle */
	struct UM_crvdatabag e[2];        /* two picked entities */
	UM_coord npt[2];              /* nearest point to center of tangent 
									 circle on each picked entity */
	UM_vector tan;                /* tangent on fillet circle */
	UM_vector vec;                /* vector along line */
	UU_REAL temp[3];
	UM_angle dang;
	UM_length arclen;              /* arc length of curve */
	UM_length dist1,dist2;          /* distances of endpoints from near point */
	int replace;                /* indicates which endpoint to replace */
	UU_REAL signtest;              /* sign test for dot product */
	UU_LOGICAL err;              /* error flag */
	int i;                    /* index */
	int numint;

	uu_denter( UU_MTRC,(us,"umu_fillet()"));
	ud_ldas(UD_DASDISTANCE, /*enter fillet radius*/UM_MODEL, 64, &radius, 
		 1, &numint, UD_NODEFAULT);

	ud_leditable(UU_TRUE);
	ud_lgeo(UU_TRUE, UD_lncir);
	while (numint != 0)
	{
		um_dl_pldas(UD_DASPCKLOC, /*pick first line / circle*/UM_MODEL, 157, &pick, 1, 
				&numint, daslev);
		if (numint != 0) 
		{
			e[0].key = um_get_pickkey(&pick.pent, daslev);
			um_copyploc(&pick.ploc, &ploc[0]);
			um_dl_pldas(UD_DASPCKLOC, /*pick second line / circle*/UM_MODEL, 158, &pick, 1,
							&numint, daslev);
			if (numint != 0)
			{
				e[1].key = um_get_pickkey(&pick.pent, daslev);
				um_copyploc(&pick.ploc,&ploc[1]);
				if (e[0].key == e[1].key)
					uu_uerror0(/*picked entities are the same*/UM_MODEL,95);
				else
				{
					ur_retrieve_data_relnum(e[0].key, &e[0].rel_num);
					ur_retrieve_data_relnum(e[1].key, &e[1].rel_num);
					if (radius < UM_FUZZ)
						uu_uerror0(/*radius is too small*/UM_MODEL,19);
					else
					{
						if ((e[0].rel_num == UM_CIRCLE_REL) && (e[1].rel_num == UM_CIRCLE_REL))
						{
							ud_ldas(UD_DASNDC, /*approximate endpoint*/UM_MODEL, 34, &ploc[2], 
										 1, &numint, UD_NODEFAULT);
						}
						if (numint > 0)
						{
							um_get_all_geom(&e[0], sizeof(struct UM_crvdatabag));
							um_get_all_geom(&e[1], sizeof(struct UM_crvdatabag));
							ur_setup_data(UM_CIRCLE_REL, &c, sizeof(struct UM_circle_rec));
						  /* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
							strcpy (c.label, "");
							c.subscr = 0;
							um_cir_ttr(UU_TRUE, ploc, e, radius, &c, npt, &err);
							if (!err) 
							{
								for (i = 0; i < 2; i++)
								{
									um_vcmnvc(npt[i],c.center,tan);
									um_cross(c.nvec,tan,tan);
									um_unitvc(tan,tan);
									switch (e[i].rel_num)
									{
										case  UM_LINE_REL:
										{
											struct UM_line_rec *ptr;
											ptr = (struct UM_line_rec *) &e[i];
											if (um_ptbtwn(ptr->spt, npt[i], ptr->ept))
											{
												um_vcmnvc(npt[i], ptr->spt, vec);
												signtest = um_dot(vec, tan);
												if (signtest < 0) replace = 1; else replace = 2;
												if (i == 1)
												{
													if (replace == 1) replace = 2; else replace = 1;
												}
											}
											else
											{
												dist1 = um_dcccc(npt[i], ptr->spt);
												dist2 = um_dcccc(npt[i], ptr->ept);
												if (dist1 < dist2) replace = 1; else replace = 2;
											}
											if (replace == 1)
												um_vctovc(npt[i], ptr->spt);
											else
												um_vctovc(npt[i], ptr->ept);
											arclen = um_dcccc(ptr->spt, ptr->ept);
											if (arclen < UM_FUZZ)
												uu_uerror0(/*auto trim line too small*/UM_MODEL,106);
											else
											{
												um_update_geom(&e[i], UM_DEFAULT_TF);
												uc_display(&e[i]);
											}
										}
										break;
										case  UM_CIRCLE_REL:
										{
											struct UM_circle_rec *ptr;
											ptr = (struct UM_circle_rec *) &e[i];
											if ((fabs(fabs(ptr->dang) - UM_TWOPI)) < UM_FUZZ)
											{
												uu_uerror0(/*can not midtrim a circle*/UM_MODEL,107);
											}
											else
											{
												um_vcmnvc(npt[i], ptr->center, vec);
												um_unitvc(vec, vec);
												dang = um_angle(vec, ptr->svec);
												um_cross(ptr->svec, vec, temp);
												um_unitvc(temp, temp);
												signtest = um_dot(ptr->nvec, temp);
												if (signtest < 0.0) dang = UM_TWOPI - dang;
												if (ptr->dang < 0.0) dang = dang - UM_TWOPI;
												if (fabs(dang) < fabs(ptr->dang))
												{
													um_cross(ptr->nvec, vec, temp);
													signtest = um_dot(temp, tan);
													if (signtest < 0.0) replace = 1; else replace = 2;
													if (i == 1)
													{
														if (replace == 1) replace = 2; else replace = 1;
													}
													if (replace == 1)
													{
														um_vctovc(vec, ptr->svec);
														ptr->dang = ptr->dang - dang;
													}
													else
													{
														ptr->dang = dang;
													}
													arclen = ptr->radius * fabs(ptr->dang);
													if (arclen < UM_FUZZ)
														uu_uerror0(/*auto trim circle too small*/
															UM_MODEL,108);
													else
													{
														um_update_geom(&e[i], UM_DEFAULT_TF);
														uc_display(&e[i]);
													}
												}
												else
												{
													uu_uerror0(/*can't auto trim entity*/UM_MODEL,109);
												}
											}
										}
										break;
										default:
											uu_uerror0(/*can't trim this entity*/UM_MODEL,110);
										break;
									}
								}
								um_create_geom(&c, UM_DEFAULT_TF, UM_CURRENT_ATTR);
								uc_display(&c);
							}
						}
					}
				}
			}
		}
	}
	ud_lgeo(UU_FALSE, UD_circle);
	uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION: um_get_trim_curves(keys, e, tfmat)
**      Get entities data used to trim/extend or midtrim operation.
**    PARAMETERS   
**       INPUT  : 
**          keys  - [3] keys of curves: 0,1 - trimming curves,
**                  2 - CV to trim/extend or midtrim.
**       OUTPUT :  
**          e     - [3] curves data: 0 - CV to trim, 1(,2) - trimming
**                  curves data.
**          tfmat - [3] associate matrices of entities.
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_get_trim_curves (keys, elst, tfmat)
UU_KEY_ID *keys;
struct NCL_fixed_databag *elst[3];   /* elst[0] data for curve to trim 
							 						 elst[2] data for curve to trim to */
UM_transf *tfmat;                    /* transformation for curves to trim */
{
	int i, j, status;
	struct NCL_fixed_databag *e;
	int ixx[3];

	ixx[0] = 2;
	ixx[1] = 0;
	ixx[2] = 1;
	status = UU_FAILURE;
/*
...i = 0 - CV to trim,
...  = 1 - trimming curve (or pt)
...  = 2 - second trimming curve (or pt) for mid trim
*/
	for (i=0; i<3; i++)
	{
		e = (struct NCL_fixed_databag *) elst[i];
		j = ixx[i];
		if (keys[j] != 0)
		{
			e->key = keys[j];
			status = ncl_retrieve_data_fixed(e, sizeof(struct UM_crvdatabag));
			if (status != UU_SUCCESS) goto done;
			status = uc_retrieve_transf(e->key, tfmat[i]);
			if (status != UU_SUCCESS) goto done;
		}
		else e->key = 0;
	}
done:;
	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION: um_prep_trim_curve(elst,tfmat,ploc,pt,lmod,uall,ptp,ldir,uend,ptrcv)
**      Intersect CV with trimmer and prompt user for a near point if 
**      more than 1 intersection exists. 
**    PARAMETERS   
**       INPUT  : 
**          elst  - [3] pointers to curves data: 0 - CV to trim, 1(,2) - trimming
**                  curves data.
**          tfmat - [3] associate matrices of entities.
**          ploc  - [3] location records: 0,1 - trimming picks (not used),
**                      2 - CV pick location.
**       OUTPUT :  
**          pt    - IO point of CV & trimming geo
**          lmod  - Modifier associated with 'pt'.
**          inp   - 1 = use nearpt in statement.
**          ptp   - Nearest point to pick location on CV.
**          ldir  - Modifier associated with 'ptp'.
**          uall  - pointer to all u values of IOs CV & Trimming geo.
**          uend  - u value of 'ptp' point. 
**
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_prep_trim_curve (elst, tfmat, ploc, pt, lmod, uall, inp, ptp, ldir, uend,ptrcv)
struct UM_crvdatabag  *elst[3],*ptrcv[3];     
UM_transf *tfmat;               /* transformation for curves to trim */
UD_NDCLOCREC *ploc;
int *inp;
UM_coord pt, ptp;
UM_param *uall, *uend;
char *lmod, *ldir;
{
	struct UM_crvdatabag *crv1, *e, *uvcrv;     
	struct UM_rbsplcrv_rec *rbcv;
	UM_isect ibuff[20];
	UM_coord tmpt;
	UM_coord nppt;
	UM_param u, utmp[20];
	int status, nint, closest, i, j;
	UU_REAL  dist, udel;
	int *ptr;
	UM_param uu;
	struct UM_evcrvout evout;

	crv1  = (struct UM_crvdatabag *) elst[0];
	e     = (struct UM_crvdatabag *) elst[1];
	um_ploctocc(ploc,nppt);
	*inp = 2;
	
/*
.....Project both the curves to the plane of the curved to be trimmed if planar
.....then find the intersections
	status = um_isect_curves (crv1,tfmat[0],e,tfmat[1],inp,nppt,&nint,10,ibuff);
*/
	status = um_proj_isect_curves (crv1,e,inp,nppt,&nint,10,ibuff);
	if ((status!=UU_SUCCESS||nint==0)&&crv1->rel_num==UM_COMPCRV_REL)
		status = um_c5_isectcomp (crv1,tfmat[0],e,inp,nppt,&nint,10,ibuff);
	if ((status != UU_SUCCESS) || (nint  ==  0))
	{
		uu_uerror0(/*entity to trim doesn't intersect trim element*/
						UM_MODEL,112);
		goto failed;
	}
/*
...if there is more than one, prompt the user to pick near the point
...of intersection to trim to and determine
...the closest intersection point to the picked location
*/
	if (nint == 1)
	{
		closest = 0;
		if (*inp)
			strcpy(lmod,"      ");
		else
			strcpy(lmod,"#");
	}
	else
	{
/*
.....Use the location from the "trim to" entity
.....Saving the user a screen locate
.....Bobby  -  7/17/96
*/
		closest = um_nearest_isect_to_ploc(&ploc[0], nint, ibuff);

/*    
... ud_ldas(UD_DASNDC, UM_MODEL, 81, &ioloc, 1, &numint,UD_NODEFAULT);
... if ( numint <= 0 ) goto failed;
... closest = um_nearest_isect_to_ploc(&ioloc, nint, ibuff);

.....Get modifier for near point
.....Bobby  -  7/17/96
*/
		um_trim_modifier(ibuff,nint,closest,lmod);
	}
	um_vctovc(ibuff[closest].pt, pt);
/*
... status = um_cctou(crv1, tfmat[0], pt, &u, &dist);
... if (status != UU_SUCCESS) goto failed;

...determine the parameter of the pickloc used to pick the curve
...to be trimed
*/
	uc_near_on_entity(crv1->key, &ploc[2], ptp);
/*
... aak 06-nov-1997: if uv-curve on surface, use the pointer to the original uv_curve,
... not that to its rbspl copy
*/
   rbcv = (struct UM_rbsplcrv_rec *) crv1;
	uvcrv = (struct UM_crvdatabag *) ptrcv[0];
   if(ncl_itis_uvcv(rbcv)) 
		um_cctou(uvcrv, tfmat[0], ptp, &udel, &dist);
	else if (crv1->rel_num == UM_COMPCRV_REL)
		um_cctou_compcrv(crv1, tfmat[0], ptp, &udel, &dist);
	else
		um_cctou(crv1, tfmat[0], ptp, &udel, &dist);

	*uend = udel;
/*
.....If intersection point is
.....beyond the curve's boundaries
.....then determine direction from
.....ending points of curve
.....Bobby  -  5/19/98
*/
   if(ncl_itis_uvcv(rbcv)) 
	{
		um_cctou(uvcrv, tfmat[0], pt, &uu, &dist);
		ptr = (int *)uvcrv;
	}
	else if (crv1->rel_num == UM_COMPCRV_REL)
	{
		um_cctou_compcrv(crv1, tfmat[0], pt, &uu, &dist);
		ptr = (int *)crv1;
	}
	else
	{
		um_cctou(crv1, tfmat[0], pt, &uu, &dist);
		ptr = (int *)crv1;
	}
	if (uu <= 0.0 || uu >= 1.0)
	{
		uc_evcrv (UM_POINT, (UU_REAL) 0.0, ptr, tfmat[0], &evout);
		um_vctovc (evout.cp,tmpt);
		uc_evcrv (UM_POINT, (UU_REAL) 1.0, ptr, tfmat[0], &evout);
		if (um_dcccc(tmpt,ptp) < um_dcccc(evout.cp,ptp))
			um_trim_direction(tmpt,evout.cp,ldir);
		else
			um_trim_direction(evout.cp,tmpt,ldir);
	}
	else
	{
		um_trim_direction(ptp,ibuff[closest].pt,ldir);
	}
/*
...Get all i/o's u values.  Problem exists with CI and conics where u 
...from ibuff is not exactly  what is if original definition is used, 
...so for this reason coordinates of point are used to get u value.
*/
	for (i = 0; i < nint; i++)
	{
		if (crv1->rel_num == UM_RBSPLCRV_REL ||
			 crv1->rel_num == NCL_CURVE_REL ||
			 crv1->rel_num == UM_COMPCRV_REL)
		{
			utmp[i] = ibuff[i].u0; 
		}
		else
		{
			um_cctou(crv1, tfmat[0], ibuff[i].pt, &u, &dist);
			utmp[i] = u; 
		} 
	}
/*
...'uall' contains all i/o's but the first is the closest
...to pick of crv1.  
*/
	j = 0;
	for (i = closest; i < nint; i++) uall[j++] = utmp[i];
	if (j < nint)
		for (i = 0; i < closest; i++) uall[j+i] = utmp[i];

	if (*inp) um_vctovc (nppt, ptp);
	goto done;

failed:;
	status = UU_FAILURE;
/*
...End of routine
*/
done:;
	uu_dexit;
	return (status);
}
/*********************************************************************
**    E_FUNCTION: um_prep_midtrim_curve(elst,tfmat,ploc,pt1,lmod1,pt2,lmod2,
**                               uall,ptp,ldir,uend)
**      Prepare data to midtrim curve process.
**    PARAMETERS   
**       INPUT  : 
**          elst  - [3] pointers to curves data: 0 - CV to trim, 1,2 - trimming
**                  curves data.
**          tfmat - [3] associate matrices of entities.
**          ploc  - [3] location records: 0,1 - trimming picks (used),
**                      2 - CV pick location.
**       OUTPUT :  
**          pt1   - IO point of CV & I trimming geo
**          lmod1 - Modifier associated with 'pt1'.
**          pt2   - IO point of CV & II trimming geo
**          lmod2 - Modifier associated with 'pt2'.
**          ptp   - Nearest point to pick location on CV.
**          ldir  - Modifier associated with 'ptp'.
**          uall  - pointer to all u values of IOs CV & Trimming geo.
**          uend  - u value of 'ptp' point. 
**
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_prep_midtrim_curve (elst,tfmat,ploc,pt1,lmod1,pt2,lmod2,uall,ptp,ldir,uend,ptrcv)
struct UM_crvdatabag *elst[3],*ptrcv[3];
UM_transf *tfmat;               /* transformation for curves to trim */
UD_NDCLOCREC *ploc;
UM_coord pt1, pt2, ptp;
UM_param *uall, *uend;
char *lmod1,*lmod2,*ldir;
{
	struct UM_crvdatabag *crv1, *e, *uvcrv;     
	struct UM_rbsplcrv_rec *rbcv;
	struct UM_evcrvout evcrv;
	UM_isect ibuff[10];
	UM_param u, du;
	int status, nint, closest, i, j, k;
	UU_REAL dist, udel;
	UM_coord nppt;
	int inp;
	UM_int2 idx = 169;
	UM_real8 ver;
	UU_LOGICAL NCL_lv96;

	getsc(&idx, &ver);
	NCL_lv96 = (ver < 9.549);

	crv1 = (struct UM_crvdatabag *) elst[0];
	rbcv = (struct UM_rbsplcrv_rec *) crv1;
	uvcrv = (struct UM_crvdatabag *) ptrcv[0];
/*
...Check if trimmer 1 is same as 2 then use pick location of
...trimers to get correct intersections
*/
	j    = 2;
	strcpy(lmod1,"#");
	strcpy(lmod2,"#");
	strcpy(ldir," ");
/*
...Intersect first trimmer with curve
*/
	for (i=1; i<3; i++)
	{
		e    = (struct UM_crvdatabag *) elst[i];
		um_ploctocc(&ploc[i-1],nppt);
		inp = 2;
/*
.....match the trim-extend logic for all 9.6
.....Project both the curves to the plane of the curved to be trimmed if planar
.....then find the intersections
*/
		if(NCL_lv96)
			status = uc_crv_intersect(crv1, tfmat[0], e, tfmat[i], &nint, 10, ibuff);
		else
		{
			status = um_proj_isect_curves (crv1,e,&inp,nppt,&nint,10,ibuff);
			if (((status!=UU_SUCCESS)||(nint==0))&&crv1->rel_num==UM_COMPCRV_REL)
			{
				status = um_c5_isectcomp (crv1,tfmat[0],e,&inp,nppt,&nint,10,ibuff);
				if (nint > 0)
				{
					for (k=0;k<nint;k++)
						um_cctou_compcrv(crv1,tfmat[0],ibuff[k].pt,&ibuff[k].u0,&dist);
				}
			}
			else if (crv1->rel_num==UM_COMPCRV_REL)
			{
				for (k=0;k<nint;k++)
					if (ibuff[k].u0 < 0. || ibuff[k].u0 > 1.)
						um_cctou_compcrv(crv1,tfmat[0],ibuff[k].pt,&ibuff[k].u0,&dist);
			}
		}
		if ((status != UU_SUCCESS) || (nint  ==  0))
		{
			uu_uerror0(/*entity to trim doesn't intersect trim element*/
			UM_MODEL,112);
			goto failed;
		}
/*
...if there is more than one, select that closest to the pick location
...of the first trimmer
*/
		j  = i - 1;
		if (nint == 1)
			closest = 0;
		else
			closest = um_nearest_isect_to_ploc(&ploc[j], nint, ibuff);

/*
... aak 06-nov-1997: was
...    status = um_cctou(crv1, tfmat[0], ibuff[closest].pt, &u, &dist);
... see corresponding place in um_prep_trim_curve.
*/
		if (crv1->rel_num == UM_RBSPLCRV_REL || crv1->rel_num == NCL_CURVE_REL)
         u = ibuff[closest].u0;
		else if (crv1->rel_num == UM_COMPCRV_REL)
			um_cctou_compcrv(crv1,tfmat[0],ibuff[closest].pt,&u,&dist);
		else
			status = um_cctou(crv1, tfmat[0], ibuff[closest].pt, &u, &dist);

		if (status != UU_SUCCESS) goto failed;

		if (i == 1)
		{
			um_vctovc(ibuff[closest].pt, pt1);
/*
.....Get modifier for near point
.....Bobby  -  7/17/96
*/
			if (nint > 1) um_trim_modifier(ibuff,nint,closest,lmod1);
		}
		else
		{
			um_vctovc(ibuff[closest].pt, pt2);
			if (nint > 1) um_trim_modifier(ibuff,nint,closest,lmod2);
		}
		uall[i-1] = u;
	}
/*
...determine the parameter of the pickloc used to pick the curve
...to be trimed
*/
	uc_near_on_entity(crv1->key, &ploc[2], ptp);
   status = UU_SUCCESS;

/*
... aak 07-nov-1997: changes for uv-curves on surfaces
*/
	if(ncl_itis_uvcv(rbcv))
		um_cctou(uvcrv, tfmat[0], ptp, &udel, &dist);
	else if (crv1->rel_num == UM_COMPCRV_REL)
		um_cctou_compcrv(crv1,tfmat[0],ptp,&udel,&dist);
	else
		um_cctou(crv1, tfmat[0], ptp, &udel, &dist);

	*uend = udel;
	dist  = fabs(.5*(uall[1] - uall[0])); 
	if (um_is_curve_closed(crv1, UM_DEFAULT_TF))
	{
		du = 2 * UM_FUZZ * (uall[1]-uall[0]) / dist;
		if (fabs(udel-.5*(uall[1]+uall[0])) < dist) du = -du;
		uc_init_evcrvout (crv1,&evcrv);
		uc_evcrv (UM_POINT,uall[0]+du,crv1,tfmat[0],&evcrv);
		um_trim_direction (pt1,evcrv.cp,ldir);
	}
	else
	{   
		if (fabs(udel-.5*(uall[1]+uall[0])) < dist)
			strcpy (ldir,UU_ldir[6]);
		else
			strcpy (ldir,UU_ldir[7]);
	}

	goto done;

failed:;
	status = UU_FAILURE;
/*
...End of routine
*/
done:;
	uu_dexit;
	return (status);
}
/*********************************************************************
**    E_FUNCTION: um_trim_modifer(isect,nint,which,lmod)
**      Return the optimum modifier (XL,YS,etc.) which corresonds to the
**      chosen intersection.
**    PARAMETERS   
**       INPUT  : 
**          isect - All intersection points.
**          nint  - Number of intersection points.
**          which - Selected intersection.
**       OUTPUT :  
**          lmod  - Best modifier (XL,YS, etc.).
**
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_trim_modifier(ibuff,nint,which,lmod)
UM_isect *ibuff;
int nint,which;
char *lmod;
{
	int i,inc,j;
	UU_REAL max;
	UM_vector vec;
/*
.....Don't use modifiers with more than 2 intersections
*/
	if (nint > 2)
	{
		strcpy(lmod,"      ");
	}
/*
.....Loop thru intersections to determine best direction modifier
*/
	else
	{
		max = -1.;
		for (i=0;i<nint;i++)
		{
			um_vcmnvc(ibuff[which].pt, ibuff[i].pt, vec);
/*
.....vp 4-mar-97 fix modifiers in NCL command
.....transform modifiers thru REFSYS/MODSYS system
*/
			if (REFtm != UU_NULL) um_vctmtf (vec,REFtm,vec);
			if (MODtm != UU_NULL) um_vctmtf (vec,MODtm,vec);
			for (j=0;j<3;j++)
			{
				if (fabs(vec[j]) > max)
				{
					inc = j * 2;
					if (vec[j] > 0.) inc++;
					max = fabs(vec[j]);
				}
			}
		}
		strcpy(lmod,UU_ldir[inc]);
	}
	return 0;
}
/*********************************************************************
**    E_FUNCTION: um_trim_direction(pt1,pt2,lmod)
**      Return the optimum modifier (XL,YS,etc.) which corresonds to the
**      chosen direction.
**    PARAMETERS   
**       INPUT  : 
**          pt1   - Origin point.
**          nint  - Direction point.
**       OUTPUT :  
**          lmod  - Best modifier (XL,YS, etc.).
**
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_trim_direction(pt1,pt2,lmod)
UU_REAL *pt1,*pt2;
char *lmod;
{
	int i,inc;
	UU_REAL max;
	UM_vector vec;
/*
.....Calculate the optimum direction modifier
*/
	max = -1.;
	um_vcmnvc(pt1, pt2, vec);
/*
.....vp 4-mar-97 fix modifiers in NCL command
.....transform modifiers thru REFSYS/MODSYS system
*/
	if (REFtm != UU_NULL) um_vctmtf (vec,REFtm,vec);
	if (MODtm != UU_NULL) um_vctmtf (vec,MODtm,vec);
	for (i=0;i<3;i++)
	{
		if (fabs(vec[i]) > max)
		{
			inc = i * 2;
			if (vec[i] > 0.) inc++;
			max = fabs(vec[i]);
		}
	}
	if (max == 0.) strcpy(lmod,"      ");
	else strcpy(lmod,UU_ldir[inc]);
	return 0;
}
/*********************************************************************
**    E_FUNCTION: um_set_modsys_refsys()
**      Set the global modsys and refsys pointers.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : status
**    SIDE EFFECTS : Sets global modsys and refsys pointers.
**    WARNINGS     : none
*********************************************************************/
um_set_modsys_refsys()
{
	UM_int2 ifl;

	gtref (ref_tm,&ifl);
	REFtm = (ifl)? (UM_transf *) invref_tm: UU_NULL;
	if (REFtm) um_inverttf (ref_tm,invref_tm);
	gtmod (mod_tm,&ifl);
	MODtm = (ifl)? (UM_transf *) invmod_tm: UU_NULL;
	if (MODtm) um_inverttf (mod_tm,invmod_tm);
	return 0;
}

/*********************************************************************
**    E_FUNCTION: um_proj_planar_crv(elst,projkeys,projflg)
**      If the triming entity is a curve and the curve to trim is planar, then
**		project both the curves on the curve plane to find the intersections.
**    PARAMETERS   
**       INPUT  : 
**          elst   -  pointers to the curve entities
**       OUTPUT :  
**       projkeys   -  keys of the projected curves 
**		 projflg	-  0 : if planar
**					   1 : if not planar
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_proj_planar_crv(crv1p, crv2p,projcrv0,projcrv1,projflg)
struct NCL_fixed_databag *crv1p,*crv2p;
struct NCL_fixed_databag *projcrv0;
struct NCL_fixed_databag *projcrv1;
int *projflg;
{
	UM_real8 tol;
	UU_LIST ptlist;
	UM_coord ppt, p0, *p1;
	int npts, status, i;
	UM_transf tf1;
	UM_vector xaxis,yaxis;
	UU_REAL plane[4];
	UM_int2 lfl_77;
	struct UM_circle_rec ci;
	
	status = ncl_retrieve_data_fixed(crv1p,sizeof(struct NCL_fixed_databag));
	status = ncl_retrieve_data_fixed(crv2p,sizeof(struct NCL_fixed_databag));
/*
.....If the triming entity is a curve and the curve to trim is planar, then
.....project both the curves on the curve plane to find the intersections.
*/	
	if (crv2p->rel_num == UM_POINT_REL) return 0;
/*
.....get plane pt and normal form cirle entity data.
*/
	if (crv1p->rel_num == UM_CIRCLE_REL)
	{
		ci.key = crv1p->key;
		status = ncl_retrieve_data_fixed(&ci);
		um_vctovc(ci.center,ppt);
		um_vctovc(ci.nvec,plane);
	}
	else
	{
		gettol(&tol);
		uu_list_init (&ptlist, sizeof(UM_coord), 200, 200);
/*
.....Evolve points on curve.
*/
		status = uc_retrieve_transf(crv1p->key, tf1);
		if (status == UU_SUCCESS) 
		{
			npts=ncl_evolve_curve(crv1p,tf1,tol,&ptlist,UU_NULL,UU_NULL,0);
     		if (npts < 4) status = UU_FAILURE;
		}
   		if (status != UU_SUCCESS)return 0;
		p1 = (UM_coord *)UU_LIST_ARRAY(&ptlist);
/*
.....Determine if the curve is planar.
*/
		if(!um_planar_curve(p1,npts,plane,xaxis,yaxis))
			return(0);
		for (i=0; i<3;i++) p0[i] =0;
		um_translate_point(p0,plane[3],plane,ppt);
	}
/*
.....set ranfile label flag to temp unknown
*/
	lfl_77 = 1;
	stunlb (&lfl_77);
/*
.....project the curves to the plane of the curve to be trimmed.
*/
	um_proj_geom_to_plane(crv1p, ppt, plane, projcrv0);
	um_proj_geom_to_plane(crv2p, ppt, plane, projcrv1);
	status = um_create_geom(projcrv0,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	status = um_create_geom(projcrv1,UM_DEFAULT_TF,UM_CURRENT_ATTR);
/*
.....Reset ranfile label flag.
*/
	if (status != UU_SUCCESS)return 0;
	*projflg = 1;
   	stunlb (&lfl_77);
	return 0;
}
/*********************************************************************
**    E_FUNCTION: um_proj_isect_curves (crv1p,crv2p,ipt,npt,nint,no_ibuf,ibuff);
**      If the triming entity is a curve and the curve to trim is planar, then
**		project both the curves on the curve plane to find the intersections
**    PARAMETERS
**       INPUT  :
**          crv1p   - First curve.
**          crv2p   - Second curve.
**          ipt     - =0, use planar routine to intersect line/curve
**                    =1, use non-planar routine to intersect line/curve
**                    =2, use planar routine, if it fails use non-planar
**          no_ibuf - Maximum number of intersectionbuf can hold.
**       OUTPUT :
**          ipt     - =0 if planar routine to intersect line/curve was used
**                    =1 if non-planar routine to intersect line/curve was used
**          npt     - near point used in non-planar line/curve intersection
**          nint    - Number of intersections.
**          ibuf    - Intersections.
**    RETURNS      : UU_SUCCESS for successful intersection; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_proj_isect_curves (crv1p, crv2p, ipt, npt, nint, no_ibuf, ibuf)
struct NCL_fixed_databag *crv1p, *crv2p;
int *ipt;
UM_coord npt;
int *nint, no_ibuf;
UM_isect ibuf[];
{
	struct NCL_fixed_databag *projcrv1,*projcrv2;
	struct UM_crvdatabag *crv1, *e;     
	int status, projflg =0;
	UM_transf tf1,tf2;
/*
.....If the triming entity is a curve and the curve to trim is planar, then
.....project both the curves on the curve plane to find the intersections.
*/
	projcrv1 = UU_NULL;
	projcrv2 = UU_NULL;
				
	projcrv1 = (struct NCL_fixed_databag *) uu_toolmalloc (um_curve_size(crv1p));
	projcrv2 = (struct NCL_fixed_databag *) uu_toolmalloc (um_curve_size(crv2p));

/*
.....avoid this step for NCL curves
*/
	if(crv1p->rel_num!=NCL_CURVE_REL && crv2p->rel_num!=NCL_CURVE_REL)
		um_proj_planar_crv(crv1p,crv2p,projcrv1,projcrv2,&projflg);
/*
...Intersect curves
*/
	if(projflg)
	{
		status = ncl_retrieve_data_fixed(projcrv1, sizeof(struct UM_crvdatabag));
		if (status != UU_SUCCESS) goto done;
		status = uc_retrieve_transf(projcrv1->key, tf1);
		if (status != UU_SUCCESS) goto done;
		status = ncl_retrieve_data_fixed(projcrv2, sizeof(struct UM_crvdatabag));
		if (status != UU_SUCCESS) goto done;
		status = uc_retrieve_transf(projcrv2->key, tf2);
		if (status != UU_SUCCESS) goto done;
		crv1  = (struct UM_crvdatabag *) projcrv1;
		e     = (struct UM_crvdatabag *) projcrv2;
	}
	else
	{
		crv1  = (struct UM_crvdatabag *) crv1p;
		status = uc_retrieve_transf(crv1p->key, tf1);
		e     = (struct UM_crvdatabag *) crv2p;
		status = uc_retrieve_transf(crv2p->key, tf2);
	}
/*
.....Now find the intersections
*/
	status = um_isect_curves (crv1,tf1,e,tf2,ipt,npt,nint,no_ibuf,ibuf);
	if ((status != UU_SUCCESS) || (*nint  ==  0))
		status = UU_FAILURE;
done:
/*
.....delete temporary curves used to calculate intersections
*/
	if(projflg)
	 {
		uc_delete(projcrv1->key);
		uc_delete(projcrv2->key);
	 }
	 if (projcrv1 != NULL)	uu_toolfree (projcrv1);
	 if (projcrv2 != NULL)	uu_toolfree (projcrv2);
	return (status);
}
