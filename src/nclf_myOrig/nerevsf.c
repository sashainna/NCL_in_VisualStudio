/*********************************************************************
**    NAME         :  nerevsf.c
**       CONTAINS:
**           int revsf
**           nclf_is_revsf
**           int ncl_eval_revsf
**           int ncl_offset_revsf
**           int ncl_class_copy_revsf
**           int ncl_copy_revsf
**           int ncl_net_revsf (key1, key2)
**           int ncl_get_apex(sfkey,uvpt,ptuv,nuv)
**           UU_LOGICAL ncl_gen_cv_planar (eptr,ptlst,uvlst,tol)
**           void ncl_revsf_axis (eptr,tfmat,pta,vca)
**
**    COPYRIGHT 2000 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nerevsf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:46
*********************************************************************/

#include "udebug.h"
#include "nccs.h"
#include "class.h"
#include "mdrel.h"
#include "mattr.h"
#include "ncl.h"
#include "nclvx.h"
#include "mdeval.h"
#include "nclfc.h"
#include "modef.h"
#include "mdattr.h"
#include "mcrv.h"

/*********************************************************************
**    E_FUNCTION     : int revsf (keycv, ptbuf, vebuf, sa, ea, ifull,
**                                                            nclkey)
**       Create surface of revolution.
**    PARAMETERS
**       INPUT  :
**          keycv   - key of curve to revolve
**          ptbuf   - Coordinates of point on axis of revolution.
**          vebuf   - Vector defining axis of revolution.
**          sa      - start angle.
**          ea      - end angle.
**          ifull   - 1 if 360 degrees rotation, 0 if not
**       OUTPUT :
**          nclkey  - key of created surface, or 0 if error.
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int revsf (keycv, ptbuf, vebuf, sa, ea, ifull, nclkey)
UM_int4 *keycv;
UM_real8 ptbuf[3], vebuf[3], *sa, *ea;
UM_int2 *ifull;
UM_int4 *nclkey;
{
	int status;
	struct NCL_fixed_databag crv,crv1;
	struct NCL_revsurf_rec sf;
	UM_int2 nstat;
	UM_int2 lfl_77,typ = 0;
	UM_real8 param[16],um_angle(),ang;
	nclsf_prim_type primtyp = NCLSF_UNKNOWN;
	UU_KEY_ID ckey;
	UM_vector vca,v;
	UM_coord pta;

	uu_denter(UU_MTRC,(us,"revsf ()"));

	status = UU_SUCCESS;
	sf.key = *nclkey;
	*nclkey = 0;
	crv.key = ckey = *keycv;

	if (ckey == 0)
	{
		int i;
		UU_REAL d,h,r1,r2,tol;
		UM_real8 tol8;
		struct UM_circle_rec ci1;
		struct UM_line_rec ln1;
		UM_coord ptb,vr;


		gettol (&tol8);
		tol = tol8;
		r1 = *sa; r2 = *ea;
		if (r1 < tol && r2 < tol) goto Done;
		if (r1 < tol) r1 = 0;
		if (r2 < tol) r2 = 0;

		for (i = 0; i < 3; i++)
		{
			pta[i] = ptbuf[i]; vca[i] = vebuf[i];
		}
		h = UM_MAG(vca);
		if (h < tol)
		{
			vca[0] = vca[2] = 0; vca[1] = 1;
			primtyp = NCLSF_SPHERE;
		}
		else
		{
			if (*ifull == 2)
			{
				for (i = 0; i < 3; i++)
				{
					ptb[i] = pta[i] + vca[i];
					vca[i] /= h;
				}
				primtyp = NCLSF_PLANE;
				*ifull = 1;
			}
			else
			{
				for (i = 0; i < 3; i++)
				{
					ptb[i] = pta[i] + vca[i];
					vca[i] /= h;
				}
				primtyp = (fabs(r1 - r2) < tol)? NCLSF_CYLINDER: NCLSF_CONE;
			}
			um_perpvc(vca,vr);
		}
/* set ranfile label flag to temp unknown */
		lfl_77 = 1;
		stunlb (&lfl_77);
		typ = primtyp;
		for (i = 0; i < 16; i++) param[i] = 0.;

		if (primtyp == NCLSF_SPHERE)
		{
			ur_setup_data(UM_CIRCLE_REL,&ci1,sizeof(ci1));
			ci1.key = NULLKEY;
			ci1.radius = r1;
			ci1.dang = UM_PI;
			um_vctovc (pta,ci1.center);
			ci1.svec[0] = ci1.svec[2] = 0; ci1.svec[1] = -1;
			ci1.nvec[0] = ci1.nvec[1] = 0; ci1.nvec[2] = 1;
			ci1.displst = UU_NULL;

			ncl_create_entity(&ci1,UM_CIRCLE_REL);
			if (ci1.key == NULLKEY) goto Done;

			ur_update_displayable(ci1.key, UM_NEVERDISPLAYABLE);
			ncl_set_label_attr(UU_FALSE, ci1.key);
			ckey = ci1.key;
			for (i = 0; i < 3; i++) param[i] = pta[i];
			param[3] = r1;
		}
		else
		{
			ur_setup_data(UM_LINE_REL,&ln1,sizeof(ln1));
			ln1.key = NULLKEY;
			if (primtyp == NCLSF_PLANE)
			{
				for (i = 0; i < 3; i++)
				{
					ln1.spt[i] = pta[i];
					ln1.ept[i] = pta[i] + r1*vr[i];
				}
			}
			else
			{
				for (i = 0; i < 3; i++)
				{
					ln1.spt[i] = pta[i] + r1*vr[i];
					ln1.ept[i] = ptb[i] + r2*vr[i];
				}
			}
			ln1.displst = UU_NULL;

			ncl_create_entity(&ln1,UM_LINE_REL);
			if (ln1.key == NULLKEY) goto Done;

			ur_update_displayable(ln1.key, UM_NEVERDISPLAYABLE);
			ncl_set_label_attr(UU_FALSE, ln1.key);
			ckey = ln1.key;

			if (primtyp == NCLSF_PLANE)
			{
				for (i = 0; i < 3; i++)
				{
					param[i] = vca[i]; param[i+4] = pta[i];
					param[3] = UM_DOT (pta,vca);
				}
			}
			else if (primtyp == NCLSF_CYLINDER)
			{
				for (i = 0; i < 3; i++)
				{
					param[i] = pta[i]; param[i+3] = vca[i];
				}
				param[6] = r1;
				param[7] = h;
			}
			else
			{
				d = r1 - r2;
				if (d > 0)
				{
					param[6] = atan2 (d,h);
					param[8] = (r2*h)/d;
					for (i = 0; i < 3; i++)
					{
						param[i+3] = -vca[i];
						param[i] = ptb[i] + param[8]*vca[i];
					}
				}
				else
				{
					d = -d;
					param[6] = atan2 (d,h);
					param[8] = (r1*h)/d;
					for (i = 0; i < 3; i++)
					{
						param[i+3] = vca[i];
						param[i] = pta[i] - param[8]*vca[i];
					}
				}
				param[7] = h;
			}
/*
.....If line and axis are parallel within tolerance
.....make sure they are exactly the same
*/
			um_vcmnvc(ln1.ept,ln1.spt,v); um_unitvc(v,v);
			if (um_vcparall(vca,v))
			{
				ang = um_angle(v,vca);
				if (abs(ang) > 1) um_vctmsc(v,-1.,vca);
				else um_vctovc(v,vca);
			}
		}

/* reset ranfile label flag */
		stunlb (&lfl_77);
	}
	else
	{
		crv.key = *keycv;
		if (ncl_retrieve_data_fixed (&crv) != 0) goto Done;
/*
.....If line and axis are parallel within tolerance
.....make sure they are exactly the same
*/
		um_unitvc(vebuf,vca);
		if (ncl_itsa_line(&crv))
		{
			struct UM_line_rec *ln1;
			ln1 = (struct UM_line_rec *)&crv;
			um_vcmnvc(ln1->ept,ln1->spt,v); um_unitvc(v,v);
			if (um_vcparall(vca,v))
			{
				ang = um_angle(v,vca);
				if (abs(ang) > 1) um_vctmsc(v,-1.,vca);
				else um_vctovc(v,vca);
			}
		}
	}
	ur_setup_data (NCL_REVSURF_REL, &sf, sizeof(struct NCL_revsurf_rec));

	sf.rel_num = NCL_REVSURF_REL;
	um_nullvc (sf.labloc);
/*
.....initialize the default label location
*/
	um_nullvc (sf.ldrloc);
	sf.key = 0;
	sf.rldnu = -1;
	sf.swapuv = 0;
	sf.rev_normal = UU_FALSE;

	sf.closdinu = 0;
/*
..... closdinu will be set either in crvcls or in uig_srfcls
*/
	if (*ifull == 1)
		sf.closdinv = 1;
	else
		sf.closdinv = 0;

	sf.offdist = 0.0;

	sf.no_sskey = 0;
	sf.sskey = NULL;
	sf.no_displst = 0;
	sf.displst = NULL;
	sf.no_tesslst = 0;
	sf.tesslst = NULL;
	sf.no_boxlst = 0;
	sf.boxlst = NULL;
	sf.no_xyzbylst = 0;
	sf.xyzbylst = NULL;

	if (primtyp == NCLSF_UNKNOWN)
	{
/* set ranfile label flag to temp unknown */
		lfl_77 = 1;
		stunlb (&lfl_77);
		crv1.key = 0;
		status = uc_copy (&crv, &crv1, sizeof(struct NCL_fixed_databag));
/* reset ranfile label flag */
		stunlb (&lfl_77);

		if (status == UU_SUCCESS)
		{
			if (crv1.key > NULLKEY && crv1.key != *keycv)
			{
				ur_update_displayable(crv1.key, UM_NEVERDISPLAYABLE);
				strncpy (crv1.label,"@UN    ",7);
				ncl_set_label_attr(UU_FALSE, crv1.key);
				sf.cvkey = crv1.key;
			}
			else
				sf.cvkey = *keycv;
			sf.sa = *sa;
			sf.ta = *ea;
			um_vctovc (ptbuf, sf.pta);
			um_unitvc (vca, sf.vca);
		}
	}
	else
	{
		sf.cvkey = ckey;

		um_vctovc (pta, sf.pta);
		um_unitvc (vca, sf.vca);
		sf.sa = 0;
		sf.ta = 360;
	}


	if (status == UU_SUCCESS)
		status = ncl_label_wf(sf.rel_num,sf.label,&sf.subscr,sf.key,&nstat);

	if (status == UU_SUCCESS)
		status = ncl_create_entity(&sf, 9);

	if (status == UU_SUCCESS)
	{
		*nclkey = sf.key;
		ncl_def_color (sf.key);
	}

	if (status == UU_SUCCESS)
/*
..... we call the general primitive type analyzing routine if a special one
..... for surfaces of revolution failed to determine a type.
*/
	{
		if (typ <= 0 && crv.key > NULLKEY)
		status = ncl_revsf_primdat (sf.pta,sf.vca,&crv,&typ,param);

		if (typ <= 0)
			status = ncl_sf_prim_analyz(&sf.key,&typ,param);
		else
			status = ncl_put_sf_primdat(&sf.key,&typ,param);
	}

Done:;

	uu_dexitstatus("revsf ()", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : nclf_is_revsf (label,isrevsf)
**       Determines if the given entity is a Surface of Revolution.
**    PARAMETERS
**       INPUT  :
**          label      - Label of surface to test.
**       OUTPUT :
**          isrevsf    - 1 = Entity is a surface of revolution.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_is_revsf (label,isrevsf)
UM_f77_str *label;
int *isrevsf;
{
	UU_KEY_ID key;
	int relnum;
/*
.....Get relnum of entity and
.....determine if it is a Surface of Revolution
*/
	*isrevsf = 0;
	getkey(label,&key);
	if (key != 0)
	{
		ur_retrieve_data_relnum(key,&relnum);
		if (relnum == NCL_REVSURF_REL) *isrevsf = 1;
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_eval_revsf (evflag, u, v, eptr, tfmat,
**                                                             evsrf)
**       Evaluate an NCL surface of revolution.
**    PARAMETERS
**       INPUT  :
**          evflag     - evaluation flag
**          u          - evaluation parameter
**          v          - evaluation parameter
**          eptr       - ptr to surface
**          tfmat      - transformation
**       OUTPUT :
**          evsrf      - ptr to surface evaluation record
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_eval_revsf (evflag, u, v, eptr, tfmat, evsrf)
int evflag;
UM_param u, v;
struct NCL_revsurf_rec *eptr;
UM_transf tfmat;
struct UM_evsrfout *evsrf;
{
	int status, lflg;
	struct UM_evcrvout evout,evout1;
	struct NCL_fixed_databag crv;
	UM_transf rottf, tfa;
	UU_REAL offd,sa,ta,theta,del,rad,radsq;
	UM_coord pta;
	UM_vector vca,svc,voff,vrgt,vrgt1,svc1,norm1;
	UM_param u1;
	UU_REAL EPSSQ = 1.e-12;

	uu_denter(UU_MTRC,(us,"ncl_eval_revsf(key=%x, tfmat=%x)",eptr->key,tfmat));

	status = UU_FAILURE;

	crv.key = eptr->cvkey;
	if (crv.key <= NULLKEY || ncl_retrieve_data_fixed (&crv) != 0)
		return (status);

	status = uc_retrieve_transf (crv.key, tfa);
	if (status == UU_SUCCESS)
		status = uc_init_evcrvout (&crv, &evout);

	offd = eptr->offdist;
/*
..... need derivatives to evaluate normals; need normals if the offset field
..... is not zero (then points are translated along normals by offdist)
*/
	lflg = (evflag == UM_NORM || evflag < UM_FRSTDERIV && offd != 0.)?
				UM_FRSTDERIV: evflag;

	status = uc_evcrv(lflg, u, &crv, tfa, &evout);
	if (status == UU_SUCCESS)
	{
		um_vctovc (eptr->pta,pta);
		um_unitvc (eptr->vca,vca);
		sa = eptr->sa / UM_RADIAN;
		ta = eptr->ta / UM_RADIAN;

		del = ta - sa;
		theta = sa + v*del;

		um_rotlntf (pta, vca, theta, rottf);
		um_cctmtf(evout.cp, rottf, evsrf->sp);

		if (lflg > UM_POINT)
		{
			um_vctmtf(evout.dcdu, rottf, evsrf->dsdu);
			um_vcmnvc (evsrf->sp, pta, svc);
			um_cross (vca,svc,vrgt);
			radsq = UM_DOT (vrgt,vrgt);
			um_vctmsc (vrgt,del,evsrf->dsdv);
/*
..... If the surface vector (svc) is collinear with the axis of
..... revolution, use the axis as the surface normal; set the direction
..... by the normal at a near point.
*/
			if (radsq < EPSSQ)
			{
/*
.....Initialized curve evaluator, caught in Purify.
*/
				status = uc_init_evcrvout (&crv, &evout1);

				u1 = (u < 0.5)? u + 0.0005: u - 0.0005;
				status = uc_evcrv(UM_FRSTDERIV, u1, &crv, tfa, &evout1);
				if (status != UU_SUCCESS) return (status);
				um_cctmtf(evout1.cp, rottf, evout1.cp);
				um_vctmtf(evout1.dcdu, rottf, evout1.dcdu);
				um_vcmnvc (evout1.cp, pta, svc1);
				um_cross (vca,svc1,vrgt1);
				radsq = UM_DOT (vrgt1,vrgt1);
				if (radsq < EPSSQ)
				{
					um_nullvc (evsrf->snorm);
					if (offd != 0.) return (UU_FAILURE);
				}
				else
				{
					if (ncl_setver(96))
					{
						um_cross (evout1.dcdu,vrgt1,norm1);

						if (um_dot (norm1,vca) < 0.)
							um_vctmsc (vca,-1.,evsrf->snorm);
						else
							um_vctovc (vca,evsrf->snorm);
/*
..... QAR 92158: if the surface point is on the axis, we put
..... dsdv = snorm x dsdu, so that the normal snorm = dsdu x dsdv -
..... for use in usrfpn
*/
						um_cross (evsrf->snorm,evsrf->dsdu,vrgt1);
						um_vctmsc(vrgt1,0.1e-5/UM_MAG(vrgt1), evsrf->dsdv);
					}
					else
					{
						um_vctmsc (vrgt1,del,evsrf->dsdv);
						um_cross (evsrf->dsdu,evsrf->dsdv,evsrf->snorm);
					}
				}
			}
			else
			{
				um_cross (evsrf->dsdu,evsrf->dsdv,evsrf->snorm);
			}
			um_unitvc (evsrf->snorm,evsrf->snorm);
			if (offd != 0.)
			{
				um_vctmsc (evsrf->snorm,offd,voff);
				um_vcplvc (evsrf->sp,voff,evsrf->sp);
			}

			if (lflg > UM_FRSTDERIV)
			{
				um_vctmtf(evout.d2cdu2, rottf, evsrf->d2sdu2);
				um_cross (vca,vrgt,evsrf->d2sdv2);
				um_vctmsc (evsrf->d2sdv2,del*del,evsrf->d2sdv2);

				if (lflg > UM_SECDERIV)
				{
					evsrf->ucurv = evout.curv;
					rad = sqrt(radsq);
					if (ncl_setver(95))
						evsrf->vcurv = rad;
					else
						evsrf->vcurv = 1./rad;
				}
			}
		}
	}

	if (status == UU_SUCCESS)
		status = ncl_transform_evsfout (evflag, tfmat, evsrf);

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_class_copy_revsf (e1, e2, bagsize)
**       Copy an NCL surface of revolution with updated label & store in ranfile.
**    PARAMETERS
**       INPUT  :
**          e1         - pointer to surface to copy
**          bagsize    - size of surface record.
**       OUTPUT :
**          e2         - pointer to copied surface.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_class_copy_revsf (e1, e2, bagsize)
struct NCL_revsurf_rec *e1, *e2;
int bagsize;
{
	int status;
	UM_int2 nclstatus;

	uu_denter(UU_MTRC,(us,"ncl_class_copy_revsf(key=%x)", e1->key));

	status = ncl_label_wf(NCL_REVSURF_REL, e2->label, &e2->subscr, 0,
                         &nclstatus);

	if (status == UU_SUCCESS) status = ncl_copy_revsf(e1, e2);

	if (status == UU_SUCCESS) status = ncl_store_wf1(e2->key);

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_copy_revsf (e1, e2)
**       Copy an NCL surface of revolution.
**    PARAMETERS
**       INPUT  :
**          e1         - pointer to surface to copy
**       OUTPUT :
**          e2         - pointer to copied surface.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_copy_revsf (e1, e2)
struct NCL_revsurf_rec *e1, *e2;
{
	UM_int2 lfl_77;
	int status, isub, isze;
	char labl[NCL_MAX_LABEL];
	struct NCL_fixed_databag c1, c2;
	struct UM_transf_rec tran;
	struct UC_attributedatabag attr1;

	uu_denter(UU_MTRC,(us,"ncl_copy_revsf(key=%x)", e1->key));

	if (e1->cvkey <= 0) return (UU_FAILURE);

	status = UU_SUCCESS;

	strncpy(labl,e2->label,NCL_MAX_LABEL);
	isub = e2->subscr;
	ur_setup_data (NCL_REVSURF_REL, e2, sizeof(struct NCL_revsurf_rec));
	*e2 = *e1;
	e2->key = 0;
	strncpy(e2->label,labl,NCL_MAX_LABEL);
	e2->subscr = isub;
	e2->no_sskey = 0;
	e2->sskey = NULL;
	e2->no_displst = 0;
	e2->displst = NULL;
	e2->no_tesslst = 0;
	e2->tesslst = NULL;
	e2->no_boxlst = 0;
	e2->boxlst = NULL;
	e2->no_xyzbylst = 0;
	e2->xyzbylst = NULL;

/* set ranfile label flag to temp unknown */
	lfl_77 = 1;
	stunlb (&lfl_77);

	isze = sizeof(struct NCL_fixed_databag);

	c1.key = e1->cvkey;
	c2.key = 0;
	status = ncl_retrieve_data (&c1, isze);
	if (status == UU_SUCCESS)
		status = uc_copy (&c1, &c2, isze);
	if (status == UU_SUCCESS)
	{
		ur_update_displayable(c2.key, UM_NEVERDISPLAYABLE);
		e2->cvkey = c2.key;
	}

/* reset ranfile label flag */
	stunlb (&lfl_77);

	if (status == UU_SUCCESS)
	{   /* get its transform */
		tran.key = e1->key;
		tran.rel_num = UM_TRANSFORM_REL;
		status = ur_retrieve_transf(&tran);
	}
	if (status == UU_SUCCESS)
	{
		status = ncl_create_entity(e2, 9);
		tran.key = e2->key;
		if (status == UU_SUCCESS)
			status = ur_update_transf(&tran);
	}
	if (status == UU_SUCCESS)
	{
		uc_retrieve_attr(e1->key, &attr1);
		attr1.key = e2->key;
		ur_update_attr(&attr1);
	}

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL S_verify_2D (pta,vca,cvpoint,npp,tol,nvec)
**       Check that the generating curve belongs to a plane through the axis.
**    PARAMETERS
**       INPUT  :
**          pta,vca        - axis of revolution
**          cvpoint        - evolved curve points
**          npp            - number of points
**          tol            - tolerance
**       OUTPUT :
**          nvec        - plane normal
**    RETURNS      :
**       UU_TRUE iff the curve is planar; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_verify_2D (pta,vca,cvpoint,npp,tol,nvec)
UM_coord pta;
UM_vector vca,nvec;
UU_LIST *cvpoint;
int npp;
UU_REAL tol;
{
	int i,imax;
	UM_coord *pp;
	UM_vector vci,vcmax;
	UU_REAL b,c,di,dmax,tolsq;

	tolsq = tol*tol;
	pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
	imax = -1;

	for (i = 0, dmax = 0; i < npp; i++)
	{
		um_vcmnvc (pp[i],pta,vci);
		c = UM_DOT (vci,vca);
		b = UM_DOT (vci,vci);
		di = b - c*c;
		if (di > dmax)
		{
			imax = i;
			um_vctovc (vci,vcmax);
			dmax = di;
		}
	}

	if (imax < 0 || dmax < tolsq) return (UU_FALSE);

	um_cross (vca,vcmax,nvec);
	um_unitvc (nvec,nvec);
	for (i = 0; i < npp; i++)
	{
		um_vcmnvc (pp[i],pta,vci);
		c = UM_DOT (vci,nvec);
		if (fabs (c) > tol) return (UU_FALSE);
	}

	return (UU_TRUE);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL ncl_gen_cv_planar (eptr,ptlst,uvlst,tol)
**       Check that the generating curve belongs to a plane through the axis.
**    PARAMETERS
**       INPUT  :
**          eptr          - pointer to surface of revolution
**          ptlst,uvlst   - UM_coord initialized lists to use
**          tol           - tolerance
**       OUTPUT : none
**    RETURNS      :
**       UU_TRUE iff the curve is planar; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_gen_cv_planar (eptr,ptlst,uvlst,tol)
struct NCL_revsurf_rec *eptr;
UU_LIST *ptlst,*uvlst;
UU_REAL tol;
{
	int status,n1;
	struct UM_evcrvout evout;
	struct NCL_fixed_databag crv;
	UM_transf tfa;
	UM_coord pta;
	UM_vector vca,nvec;
	UU_LOGICAL planar;

	planar = UU_FALSE;

	crv.key = eptr->cvkey;
	if (crv.key <= NULLKEY || ncl_retrieve_data_fixed (&crv) != 0)
		return (planar);

	status = uc_init_evcrvout (&crv, &evout);
	if (status == UU_SUCCESS)
	status = uc_retrieve_transf (crv.key, tfa);
	if (status != UU_SUCCESS) return (planar);

	um_vctovc (eptr->pta,pta);
	um_unitvc (eptr->vca,vca);

	n1 = ncl_evolve_curve (&crv,tfa,tol,ptlst,NULLST,uvlst,1);
	if (n1 < 2) return (planar);

	planar = S_verify_2D (pta,vca,ptlst,n1,2*tol,nvec);

	return (planar);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_offset_gen_cv (eptr)
**       Offset a surface of revolution by offseting the generating curve.
**    PARAMETERS
**       INPUT  :
**          eptr       - pointer to surface of revolution
**          offd       - offset distance
**       OUTPUT :
**          eptr       - original curve replaced by its offset
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_offset_gen_cv (eptr,offd,voff)
struct NCL_revsurf_rec *eptr;
UU_REAL offd;
UM_vector voff;
{
	int status,i,n1;
	int lr = 0;
	struct UM_evcrvout evout;
	struct NCL_fixed_databag crv;
	struct UM_rbsplcrv_rec crv1;

	UM_transf tfa;
	UU_REAL tol,d;
	UM_real8 tol8;
	UM_coord pta;
	UM_vector vca,nvec,vc;
	UM_vector *vv;
	UU_LOGICAL planar;

	UU_LIST cvpoint,cvtang,uptr;

	uu_denter(UU_MTRC,(us,"ncl_offset_gen_cv(key=%x)",eptr->key));

/*
..... the offset parameter is already correct, so that if we can not
..... offset the curve we just exit and leave the surface offset the old
..... way. if the curve is offset successfully, the surface offset parameter
..... is reset to zero.
*/
	crv.key = eptr->cvkey;
	if (crv.key <= NULLKEY || ncl_retrieve_data_fixed (&crv) != 0)
		return;

	status = uc_init_evcrvout (&crv, &evout);
	if (status == UU_SUCCESS)
	status = uc_retrieve_transf (crv.key, tfa);
	if (status != UU_SUCCESS) return;

	um_vctovc (eptr->pta,pta);
	um_unitvc (eptr->vca,vca);

	gettol (&tol8);
	tol = tol8;

	uu_list_init (&cvpoint, sizeof(UM_coord), 100, 100);
	uu_list_init (&cvtang,  sizeof(UM_coord), 100, 100);
	uu_list_init (&uptr,  sizeof(UM_coord), 100, 100);

	n1 = ncl_evolve_curve (&crv,tfa,tol,&cvpoint,&cvtang,&uptr,1);
	if (n1 < 2) goto Done;

	planar = S_verify_2D (pta,vca,&cvpoint,n1,2*tol,nvec);
	if (!planar) goto Done;

	status = ncl_fix_evol (1,&crv,tfa,&evout,&n1,tol,&cvpoint,&cvtang,&uptr);

	if (status != UU_SUCCESS || n1 < 3) goto Done;

	vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
	um_cross (nvec,vv[0],vc);
	d = UM_DOT (vc,voff);
	if (d < 0) um_negvc (nvec,nvec);
	lr = 1;

	n1 = ncl_cv_offset (NULLKEY,&cvpoint,&cvtang,n1,lr,nvec,offd,0,0.,0.,tol,
		0,0);
	if (n1 < 2) goto Done;

	vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
	for (i = 0; i < n1; i++)
		um_unitvc (vv[i],vv[i]);

	ncl_fix_corners (&cvpoint, &cvtang, tol, 1, &n1);
	ncl_fix_tol (&n1, tol, &cvpoint, &cvtang);
	if (n1 < 2) goto Done;

	vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
	for (i = 1; i < n1-1; i++)
	{
		d = UM_DOT(vv[i],vv[i]);
		if (d < 9.e9)
			um_nullvc (vv[i]);
		else
			um_unitvc (vv[i],vv[i]);
	}

	status = ncl_geogn2 (n1, &cvpoint, &cvtang, (UU_REAL *) UU_NULL, UU_FALSE,
		UU_NULL, 1, &crv1);

	if (status != UU_SUCCESS) goto Done;

	uc_delete (crv.key);

	ur_update_displayable(crv1.key, UM_NEVERDISPLAYABLE);
	eptr->offdist = 0;
	eptr->cvkey = crv1.key;

	ur_update_data_fixed (eptr);

Done:
	uu_list_free (&cvpoint);
	uu_list_free (&cvtang);
	uu_list_free (&uptr);
}

/*********************************************************************
**    E_FUNCTION     : void S_offset_revsf (eptr)
**       Offset a surface of revolution.
**    PARAMETERS
**       INPUT  :
**          bsf        - pointer to surface
**          cdis       - offset distance
**       OUTPUT :
**          bsf        - pointer to surface with offset field updated
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_offset_revsf (eptr)
struct NCL_revsurf_rec *eptr;
{
	UU_REAL offd,u,v,sa;
	int istat;
	struct UM_evsrfout evout;
	UM_transf tff;
	UM_vector voff;
	UM_int2 lfl_77;

	offd = eptr->offdist;
	if (fabs(offd) > 0.0001)
	{
		istat = uc_retrieve_transf(eptr->key, tff);
		if (istat == UU_SUCCESS)
		{
			istat = uc_init_evsrfout (eptr,&evout);
			if (istat == UU_SUCCESS)
			{
				u = v = 0.;
				eptr->offdist = 0;
				sa = eptr->sa;
				eptr->sa = 0;

				istat = ncl_eval_revsf (UM_FRSTDERIV, u, v, eptr, tff, &evout);

				eptr->offdist = offd;
				eptr->sa = sa;

				if (istat == UU_SUCCESS)
				{
					if (offd < 0)
					{
						um_negvc (evout.snorm,voff);
						offd  = -offd;
					}
					else
						um_vctovc (evout.snorm,voff);

/* set ranfile label flag to temp unknown */
					lfl_77 = 1;
					stunlb (&lfl_77);

					S_offset_gen_cv (eptr,offd,voff);

					/* reset ranfile label flag */
					stunlb (&lfl_77);
				}
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_offset_revsf (lcopy, eptr, cdis)
**       Offset a surface of revolution.
**    PARAMETERS
**       INPUT  :
**          bsf        - pointer to surface
**          cdis       - offset distance
**       OUTPUT :
**          bsf        - pointer to surface with offset field updated
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_offset_revsf (lcopy, eptr, cdis)
UU_LOGICAL lcopy;
struct NCL_revsurf_rec *eptr;
UU_REAL cdis;
{
	int status = UU_SUCCESS;
	uu_denter(UU_MTRC,(us,"ncl_offset_revsf (key=%x)",eptr->key));

	eptr->offdist = eptr->offdist + cdis;
	status = ur_update_data_fixed (eptr);

	if (status == UU_SUCCESS)
	{
		if (eptr->primitive >= NCLSF_PLANE && fabs(cdis) > 0.0001)
		{
			ncl_offset_primdat (eptr->key,eptr->primitive,eptr->prim_param,
				cdis);
			status = ur_update_data_fixed (eptr);
		}
		else if (lcopy)
			S_offset_revsf (eptr);
	}

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_revsf_axis (eptr,tfmat,pta,vca)
**       Return axis of a surface of revolution.
**    PARAMETERS
**       INPUT  :
**          eptr       - pointer to surface of revolution
**          tfmat      - surface matrix
**       OUTPUT :
**          pta        - Rev surf rotation point.
**          vca        - Rev surf axis unit vector.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_revsf_axis (eptr,tfmat,pta,vca)
struct NCL_revsurf_rec *eptr;
UM_transf tfmat;
UM_coord pta;
UM_vector vca;
{
	UU_LOGICAL um_is_idmat();

	um_vctovc (eptr->pta,pta);
	um_vctovc (eptr->vca,vca);
	if (um_is_idmat(tfmat) == UU_FALSE)
	{
		um_cctmtf (pta,tfmat,pta);
		um_vctmtf (vca,tfmat,vca);
	}
	um_unitvc (vca,vca);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_net_revsf (key1, key2)
**       Copy a surface of revolution to a net surface.
**    PARAMETERS
**       INPUT  :
**          key1       - key of entity to copy.
**       OUTPUT :
**          key2       - key of copied entity.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_net_revsf(key1, key2)
UU_KEY_ID key1, *key2;

{
   int status;
	struct NCL_revsurf_rec e1, e2;

	uu_denter(UU_MTRC,(us,"ncl_net_revsf (key=%x)", key1));

	*key2 = 0;
	e1.key = key1;
	status = ncl_retrieve_data_fixed (&e1);
	if (status == UU_SUCCESS)
		status = ncl_copy_revsf (&e1, &e2);
	if (status == UU_SUCCESS)
	{
 		*key2 = e2.key;
		strncpy (e2.label,"@UN    ",7);
		status = ur_update_data_fixed(&e2);
		ur_update_displayable(e2.key, UM_NEVERDISPLAYABLE);
	}

	uu_dexit;
	return (status);
}
/*********************************************************************
**    FUNCTION     :  ncl_get_apex(sfkey,uvpt,ptuv,nuv)
**			get the uv projection of a point on a spherical surface by
**			extending the vector formed by the previous 2 points on the crv
**    PARAMETERS
**       INPUT  :
**			 	sfkey		 : key id of base surface
**			 	uvpt		 : uv parameter of point from usrfpn
**			 	ptuv		 : projected points
**				nuv			 : no. of projected points
**			OUTPUT :
**				uvpt		 : new uv point at apex
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_apex(sfkey,uvpt,ptuv,nuv)
UU_KEY_ID sfkey;
UM_coord uvpt;
UM_coord *ptuv;
int nuv;
{
	int status,trfl;
	UU_REAL tol=.001;
	UM_coord p1,p2;
	struct NCL_fixed_databag sf;
/*
.....Determine if surface has an apex point
.....(is a triangular surface)
*/
	sf.key = sfkey;
	status = ncl_retrieve_data_fixed(&sf);
	if (status != 0) return(status);
	ncl_get_sf_treflag(&sf,&trfl,tol);
	if (trfl == 0) return(UU_FAILURE);
/*
.....get coordinates of the new point by extending the vector formed by the
.....previous two points.
*/
	um_vctovc(ptuv[nuv-2],p1);
	um_vctovc(ptuv[nuv-1],p2);
	if (trfl <= 2)
	{
		uvpt[0] = 0.;
		if (trfl == 2) uvpt[0] = 1.;
		if (fabs(p1[0]-p2[0]) < UM_FUZZ || fabs(p1[1]-p2[1])<UM_FUZZ)
			uvpt[1] = p2[1];
		else
			uvpt[1] = p1[1] - p1[0] * ((p1[1]-p2[1])/(p1[0]-p2[0]));
	}
	else
	{
		uvpt[1] = 0.;
		if (trfl == 4) uvpt[1] = 1.;
		if (fabs(p1[0]-p2[0]) < UM_FUZZ || fabs(p1[1]-p2[1])<UM_FUZZ)
			uvpt[0] = p2[0];
		else
			uvpt[0] = p1[0] - p1[1] * ((p1[0]-p2[0])/(p1[1]-p2[1]));
	}

	status = UU_SUCCESS;
	return (status);
}
