/*********************************************************************
**    NAME         :  necv.c
**       CONTAINS: routines to handle NCL Bezier curves
**
**       int ncl_curve_to_nclcrv
**       int ncl_nclcrv_to_curve
**       ncl_p_nclcrv
**       ncl_p80_curve
**       ncl_put_crvhead
**       ncl_put_crvseg
**       ncl_get_crvhead
**       ncl_get_crvseg
**       int ncl_crvheadsize
**       cmpdef
**       int ncl_nclcmpcrv_to_curve
**       revwf
**       ofswf
**       um_c7_trimnclcrv
**       int ncl_c7_frmnclcrv
**       int ncl_disp_curve
**       int ncl_redef_curve
**       int ncl_c7_endpoints
**       int ncl_copy_nclcrv
**       int ncl_maxim_crv1
**       int ncl_get1_tpar1
**       int ncl_put1_tpar1
**       int ncl_get_tpar
**       int ncl_put_tpar
**       int ncl_rbsp_get_tpar
**       int ncl_rbsp_put_tpar
**       int ncl_ssonsf1
**       int ncl_cvonsf_func
**       int ncl_cvon_revsf
**       int ncl_sson_revsf
**		   ncl_nclcv_feat
**       ncl_cv_isclosed
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       necv.c , 25.2
**     DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 17:13:15
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mdattr.h"
#include "mdeval.h"
#include "modef.h"
#include "ulist.h"
#include "uminmax.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclxmdl.h"
#include "dasnog.h"


extern int NCLX_internal_geom;

static int NCL_COMPCRV = 0;
/*********************************************************************
**    E_FUNCTION     : int ncl_curve_to_nclcrv(e, buf)
**       Convert the UNIBASE representation of an NCL Bezier curve
*       to an internal NCLI curve representation.
**    PARAMETERS   
**       INPUT  : 
**          e                 NCL Bezier curve
**       OUTPUT :  
**          buf               buffer to place NCLI curve
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_curve_to_nclcrv(e, buf)
struct NCL_curve_rec *e;
UM_real8 buf[];

{
	int status;
	struct NCLI_crvhead_rec *crvhead;
	struct NCLI_crvseg_rec *crvseg;
	int i;

	uu_denter(UU_MTRC,(us,"ncl_curve_to_nclcrv(key=%x, buf=%x)",
		e->key, buf));

	status = ncl_retrieve_data(e, sizeof(struct NCL_fixed_databag));

	/* move the header information */
	crvhead = (struct NCLI_crvhead_rec *) buf;
	ncl_get_crvhead(e->no_param, e->param, crvhead);

	/* move each of the curve segments */
	crvseg = (struct NCLI_crvseg_rec *) (buf + ncl_crvheadsize(crvhead));
	for (i=0; i<e->no_segment; i++)
	{
		ncl_get_crvseg(&e->segment[i], crvseg);
		crvseg++;
	}

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_nclcrv_to_curve(buf, e)
**       Convert an internally represented NCLI curve into its 
**       UNIBASE equivalent.
**    PARAMETERS   
**       INPUT  : 
**          buf                  buffer holding NCL curve
**       OUTPUT :  
**          e                    UNIBASE representation of curve entity
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_nclcrv_to_curve(buf, e)
UM_real8 buf[];
struct NCL_curve_rec *e;

{
	int status;
	struct NCLI_crvhead_rec *crvhead;
	struct NCLI_crvseg_rec *crvseg;
	int i;

	uu_denter(UU_MTRC,(us,"ncl_nclcrv_to_curve(buf=%x, e=%x)",
		buf, e));

	if (NCL_COMPCRV)
	{
		status = ncl_nclcmpcrv_to_curve(buf, e);
		uu_dexit;
		return(status);
	}

	status = UU_SUCCESS;
	ur_setup_data(NCL_CURVE_REL, e, sizeof(struct NCL_curve_rec));

	/* move the header information */
	crvhead = (struct NCLI_crvhead_rec *) buf;
	ncl_put_crvhead(crvhead, e);

	/* move each of the curve segments */
	crvseg = (struct NCLI_crvseg_rec *) (buf + ncl_crvheadsize(buf));
	for (i=0; i<crvhead->nosegs; i++)
	{
		e->no_segment++;
		if (i == crvhead->nosegs - 1) /* zero out unused values in last seg */
		{
			crvseg->duds0 = 0.;
			crvseg->duds1 = 0.;
			crvseg->rho   = 0.;
		}
		ncl_put_crvseg(crvseg, &e->segment[i]);
		crvseg++;
	}
	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_p_nclcrv(buf)
**       Print an NCLI curve.
**    PARAMETERS   
**       INPUT  : 
**          buf               buffer holding NCL curve.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p_nclcrv(buf)
UM_real8 buf[];

{
	struct NCLI_crvhead_rec *crvhead;
	struct NCLI_crvseg_rec *crvseg;
	int i;
	int nosegs;

	uu_denter(UU_MTRC,(us,"ncl_p_nclcrv(buf=%x)",buf));
	sprintf(UM_sbuf,"NCLCRV:");
	um_pscroll(UM_sbuf);

	/* print the header information */
	crvhead = (struct NCLI_crvhead_rec *) buf;
	nosegs = crvhead->nosegs;
	sprintf(UM_sbuf, "nosegs = %d", nosegs);
	um_pscroll(UM_sbuf);
	for (i=0; i<nosegs-1; i++)
	{
		sprintf(UM_sbuf, "param[%d] = %g",
			i+1, crvhead->param[i]);
		um_pscroll(UM_sbuf);
	}

	/* print each of the curve segments */
	crvseg = (struct NCLI_crvseg_rec *) (buf + ncl_crvheadsize(crvhead));
	for (i=0; i<nosegs; i++)
	{
		sprintf(UM_sbuf, "crvsegadr = %x", crvseg);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf, "segment %d", i+1);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf, "point=(%g,%g,%g)",
			crvseg->point[0], crvseg->point[1], crvseg->point[2]);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf, "delta=(%g,%g,%g)",
			crvseg->delta[0], crvseg->delta[1], crvseg->delta[2]);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf, "(duds0, duds1, rho) =(%g,%g,%g)",
			crvseg->duds0, crvseg->duds1, crvseg->rho);
		um_pscroll(UM_sbuf);
		crvseg++;
	}

	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_p80_curve(e)
**       Print the contents of a UNIBASE NCL Bezier curve.
**    PARAMETERS   
**       INPUT  : 
**          e                       NCL Bezier curve
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p80_curve(e)
struct NCL_curve_rec *e;

{
	int i;

	uu_denter(UU_MTRC,(us,"ncl_p80_curve(key=%x)",e->key));

	sprintf(UM_sbuf, "NCL BEZIER CURVE: %x",e->key);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf, "label %s",e->label);
	um_pscroll(UM_sbuf);

	um_p_ary(UM_PINT, "no_param", 1, &e->no_param);
	um_p_ary(UM_PINT, "no_segment", 1, &e->no_segment);
	for (i=0; i<e->no_param; i++)
	{
		sprintf(UM_sbuf, "  param[%d]=%g",i,e->param[i]);
		um_pscroll(UM_sbuf);
	}
	for (i=0; i<e->no_segment; i++)
	{
		sprintf(UM_sbuf, "  point=(%g,%g,%g)",
			e->segment[i].point[0], e->segment[i].point[1],e->segment[i].point[2]);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf, "  delta=(%g,%g,%g)",
			e->segment[i].delta[0], e->segment[i].delta[1],e->segment[i].delta[2]);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf, "  duds0=%g",e->segment[i].duds0);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf, "  duds1=%g",e->segment[i].duds1);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf, "  rho  =%g",e->segment[i].rho);
		um_pscroll(UM_sbuf);
	}

	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_put_crvhead(crvhead, e)
**       Move the internal NCL representation of a curve header into
**       a UNIBSE representation.
**    PARAMETERS   
**       INPUT  : 
**          crvhed               NCLI crvhead record
**       OUTPUT :  
**          e                    NCL curve record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_put_crvhead(crvhead, e)
struct NCLI_crvhead_rec *crvhead;
struct NCL_curve_rec *e;

{
	int i;

	uu_denter(UU_MTRC,(us,"ncl_put_crvhead(crvhead=%x, e=%x)",
		crvhead, e));
	e->closdinu = 0;
	e->no_param = crvhead->nosegs-1;
	e->t0 = 0.0;
	e->t1 = crvhead->param[e->no_param-1];
/*
...initial curve length < 0, we set it when entity is created in  
...unibase and um_arclen will be able to calculate this value
*/ 
	e->t_end = -1;
	for (i=0; i<e->no_param; i++)
		e->param[i] = crvhead->param[i];
	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_put_crvseg(crvseg, e)
**       Move the internal NCL representation of a curve segment into
**       a UNIBSE representation.
**    PARAMETERS   
**       INPUT  : 
**          crvseg               NCLI crvseg record
**       OUTPUT :  
**          segdata              NCL segment record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_put_crvseg(crvseg, segdata)
struct NCLI_crvseg_rec *crvseg;
struct NCL_segment_rec *segdata;

{
	int i;

	uu_denter(UU_MTRC,(us,"ncl_put_crvseg(crvseg=%x, segdata=%x)",
		crvseg, segdata));
	for (i=0; i<3; i++) segdata->point[i] = crvseg->point[i];
	for (i=0; i<3; i++) segdata->delta[i] = crvseg->delta[i];
	segdata->duds0 = crvseg->duds0;
	segdata->duds1 = crvseg->duds1;
	segdata->rho = crvseg->rho;
	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_crvhead(n, param, crvhead)
**       Move the UNIBASE representation of a curve header into an
**       NCL internal representation.
**    PARAMETERS   
**       INPUT  : 
**          n                    number of parameters
**          param                array of parameter values
**       OUTPUT :  
**          crvhed               NCLI crvhead record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_crvhead(n, param, crvhead)
int n;
float param[];
struct NCLI_crvhead_rec *crvhead;

{
	int i;

	uu_denter(UU_MTRC,(us,"ncl_get_crvhead(n=%d, param=%x, crvhead=%x)",
		n, param, crvhead));
	crvhead->nosegs = n+1;
	for (i=0; i<n; i++) crvhead->param[i] = param[i];
	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_crvseg(segdata, crvseg)
**       Move the UNIBASE representation of a curve segment into an
**       internal NCl representation.
**    PARAMETERS   
**       INPUT  : 
**          segdata              NCL segment record
**       OUTPUT :  
**          crvseg               NCLI crvseg record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_crvseg(segdata, crvseg)
struct NCL_segment_rec *segdata;
struct NCLI_crvseg_rec *crvseg;

{
	int i;

	uu_denter(UU_MTRC,(us,"ncl_get_crvseg(segdata=%x, crvseg=%x)",
		segdata, crvseg));
	for (i=0; i<3; i++) crvseg->point[i] = segdata->point[i];
	for (i=0; i<3; i++) crvseg->delta[i] = segdata->delta[i];
	crvseg->duds0 = segdata->duds0;
	crvseg->duds1 = segdata->duds1;
	crvseg->rho = segdata->rho;
	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_crvheadsize(crvhead)
**       Calculate the size (in bytes) of a curve head record.
**    PARAMETERS   
**       INPUT  : 
**          crvhead              NCLI curve header record
**       OUTPUT :  
**          none
**    RETURNS      : 
**       actual size of curve header (in bytes)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_crvheadsize(crvhead)
struct NCLI_crvhead_rec *crvhead;

{
	int crvheadsize;
	int nosegs;

	uu_denter(UU_MTRC,(us,"ncl_crvheadsize(crvhead=%x)", crvhead));
	nosegs = crvhead->nosegs;
	crvheadsize = sizeof(UM_real4) + ((nosegs-1) * sizeof(UM_real4));
	/*um_p_ary(UM_PINT, "byte size", 1, &crvheadsize);*/
	crvheadsize = crvheadsize / sizeof(UM_real8);
	/*um_p_ary(UM_PINT, "real8 size", 1, &crvheadsize);*/
	if (nosegs & 1) crvheadsize++;
	/*um_p_ary(UM_PINT, "upreal8 size", 1, &crvheadsize);*/
	uu_dexit;
	return (crvheadsize);
}

/*********************************************************************
**    E_FUNCTION     : cmpdef()
**       Fortran callable routine to create and store COMPOSITE type curves.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          crvkey: Integer containing the key of the defined curve.
**                  If a zero (0) value is returned, the curve 
**                  definition completed in error.
**    RETURNS      : See "crvkey" above.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int cmpdef (crvkey)
UU_KEY_ID *crvkey;                /* key of new curve */
{
	int i,j,status;
	int numpicks;                  /* number of entities in curve */
	UU_KEY_ID *ids,*sskeys;        /* keys of defining entities */
	struct UM_compcrv_rec comp;    /* new composite entity */
	UM_int2 type = NCLI_CURVE;
	UM_int2 iflg;
	char *uu_malloc();
	UU_REAL *ptr;
	union {
		UM_real8 asn;
		UM_int4 key[2];
	} val;

	*crvkey = 0;
	NCL_COMPCRV = 1;
	ids = UU_NULL;
	status = ncl_get_asn_ptr(&ptr,&numpicks);
	if (numpicks <= 0)
	{
		status = UU_FAILURE;
		goto Done;
	}
	ids = (UU_KEY_ID *)uu_malloc(numpicks*sizeof(UU_KEY_ID));
	if (!ids)
	{
		status = UU_FAILURE;
		goto Done;
	}

	for (i = 0; i < numpicks; i++)
	{
		val.asn = ptr[i];
		ids[i] = val.key[0];
	}

	comp.key = 0;
/*
..... create duplicate geometry for composite curve
*/
	iflg = 1;
	stunlb (&iflg);
	status = um_c5_mergecrv (numpicks, ids, &comp);
	stunlb (&iflg);

	if (status == UU_SUCCESS)
	{
		struct NCL_fixed_databag bsrf;

		ptgeo(&type, &comp, &comp.key);
/*
...... aak 13-apr-1998: if it's a composite crv composed of CVonSF's, add
...... the composite crv key to the sskey table of the base SF
.....Support CVonSF's on multiple surfaces
.....Bobby - 09/29/14
*/
		sskeys = (UU_KEY_ID *)uu_malloc(sizeof(UU_KEY_ID)*comp.no_cid);
		for (i=0;i<comp.no_cid;i++)
		{
			ncl_cvonsf_get_bskeys(&comp,i,&bsrf.key);
			sskeys[i] = bsrf.key;
			for (j=0;j<i;j++)
				if (bsrf.key == sskeys[i]) continue;
			if (bsrf.key != 0)
			{
				status = ncl_retrieve_data_fixed (&bsrf);
				if (status == UU_SUCCESS) ncl_update_sskey (&bsrf, comp.key,1);
			}
		}
		uu_free(sskeys);
	}
	*crvkey = comp.key;

Done:;
	NCL_COMPCRV = 0;
	if (ids) uu_free(ids);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_nclcmpcrv_to_curve(cptr, eptr)
**       COPY a CAM generated COMPOSITE CURVE to the target Unibase ptr 'e'.
**    PARAMETERS   
**       INPUT  : 
**          cptr                  input CAM generated COMPOSITE CURVE
**       OUTPUT :  
**          eptr                  UNIBASE representation of curve entity
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_nclcmpcrv_to_curve(cptr, eptr)
struct UM_compcrv_rec *cptr;
struct UM_compcrv_rec *eptr;
{
	int status;
	int i;

	uu_denter(UU_MTRC,(us,"ncl_nclcmpcrv_to_curve(buf=%x, e=%x)",
		buf, e));

	ur_setup_data(UM_COMPCRV_REL, eptr, sizeof(struct UM_compcrv_rec));
	eptr->rel_num = cptr->rel_num;
	eptr->arclen = cptr->arclen;
	eptr->planar = cptr->planar;
	eptr->open = cptr->open;
	eptr->continuity = cptr->continuity;
	eptr->fcolor = cptr->fcolor;
	eptr->closdinu = cptr->closdinu;
	eptr->no_cid = cptr->no_cid;
	eptr->t0 = cptr->t0;
	eptr->t1 = cptr->t1;
	eptr->addflg = cptr->addflg;
	for (i = 0; i < eptr->no_cid; i++)
		eptr->cid[i] = cptr->cid[i];

	status = UU_SUCCESS;

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : revwf (nclkey, irev)
**       Copy & optionally reverse a cad curve.
**    PARAMETERS
**       INPUT  :
**          nclkey            Key of curve to reverse.
**          irev              = 1 copy & reverse, = 0 just copy,
**                            = 2 just revers
**       OUTPUT :
**          nclkey            Key of new reversed curve or 0 if error
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int revwf (nclkey, irev)
UM_int4 *nclkey;
UM_int2 *irev;
{
	int isize, status;
	struct NCL_fixed_databag crv1,crv2,*ptr2;

	uu_denter(UU_MTRC,(us,"revwf(%d)", *nclkey));

	crv1.key = *nclkey;
	*nclkey = 0;
	crv2.key = (UU_KEY_ID) 0;
	ptr2 = &crv2;
	isize = sizeof(crv1);
	status = ncl_retrieve_data_fixed (&crv1);
	if (status == UU_SUCCESS)
	{
/*
.....vp 9/30/97.  Do not copy entity when reversing only 
.....splines and ssplines are reversed directly in unibase
.....and um_update_geom is not required (can damage entity)
*/
		if (*irev != 2)
			status = uc_copy (&crv1, ptr2, isize);
		else
			ptr2 = &crv1;
	}
	if (*irev > 0 && status == UU_SUCCESS) status = uc_reverse_curve(ptr2);

	if (status == UU_SUCCESS)
	{
		if (ptr2->rel_num != UM_RBSPLCRV_REL && 
						ptr2->rel_num != UM_UVCVONSF_REL &&
						ptr2->rel_num != UM_COMPCRV_REL)
			status = um_update_geom (ptr2, UM_DEFAULT_TF);
/*
.....There is a possibility that the parameters have changed, make sure 
.....that they are saved. Due the the above comment, I am a little 
.....worried that saving the changes may cause problems, but during
.....my testing of the changes, no such problems occured. JLS 4/23/99
*/
		else
			ur_update_data_fixed(ptr2);
	}
	if (status == UU_SUCCESS)
	{
		*nclkey = ptr2->key;
/*
.....vp 3/25/98 set default color only if it is new entity
*/
		if (crv1.key != *nclkey) ncl_def_color (ptr2->key);
	}

	uu_dexitstatus("revwf", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ofswf (nclkey, delx,dely,delz,err)
**       Translate a rbsp curve 
**    PARAMETERS
**       INPUT  :
**          nclkey            Key of curve to reverse.
**       OUTPUT :
**          nclkey            Key of new curve or 0 if error
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ofswf (nclkey, delx,dely,delz,err)
UM_int4 *nclkey;
UM_real8 *delx,*dely,*delz;
UM_int2 *err;
{
	int isize, i, nn, status;
	struct NCL_fixed_databag crv1,crv2,*ptr2;
	struct UM_rbsplcrv_rec *eptr;
	UU_REAL *pts;
	UM_vector dv;

	uu_denter(UU_MTRC,(us,"ofswf(%d)", *nclkey));

	crv1.key = *nclkey;
	*nclkey = 0;
	ptr2 = &crv2;
	isize = sizeof(crv1);
	status = ncl_retrieve_data_fixed (&crv1);
	if (status == UU_SUCCESS)
		status = uc_copy (&crv1, ptr2, isize);
	if (status == UU_SUCCESS) 
	{
		eptr = (struct UM_rbsplcrv_rec *) ptr2;
		nn = eptr->no_pt;
		um_xyztovc (*delx,*dely,*delz, dv);
		for (i=0, pts = eptr->pt; i<nn; i++, pts+=3)
			um_vcplvc (pts,dv,pts);
	}

	if (status == UU_SUCCESS && ptr2->rel_num == UM_RBSPLCRV_REL)
	{
		ur_update_data_fixed(ptr2);
		*nclkey = ptr2->key;
		if (crv1.key != *nclkey) ncl_def_color (ptr2->key);
	}
	else
	{
/* quit with error flag */
		*err = 480;
		return (0);
	}

	uu_dexitstatus("ofswf", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: um_c7_trimnclcrv(rptr, tsplit, udel, ptr1, ptr2)
**       Split  NCL curve (RPTR) at parameter value
**			TSPLIT into two NCL curves (R1PTR and R2PTR)
**			where R1PTR goes from the start to TSPLIT and R2PTR goes
**			from TSPLIT to the end of the curve.
**    PARAMETERS   
**       INPUT  : 
**				rptr					original rational bspline curve
**				tsplit				parameter value to split at
**										0.0 < tsplit < 1.0
**       OUTPUT :  
**				ptr1					left part of curve rptr (u < tsplit)
**				ptr2					right part of curve rptr (u > tsplit)
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_c7_trimnclcrv(rptr, tsplit, udel, ptr1, ptr2)
struct NCL_curve_rec *rptr,*ptr1,*ptr2;
UM_param *tsplit, *udel;

{
	UM_param urng,t;

	uu_denter(UU_MTRC,(us,"um_c7_trimnclcrv(%8x,%f,%f)",rptr,
				  *tsplit,*udel));

	urng = rptr->t1 - rptr->t0;
	t    = rptr->t0 + *tsplit * urng; 

	ncl_c7_frmnclcrv (rptr, ptr1);
	ptr1->t1 = t;
	ncl_c7_frmnclcrv (rptr, ptr2);
	ptr2->t0 = t;
	
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     :  int ncl_c7_frmnclcrv(bptr,rptr)
**       Convert an NCL curve into an NCL curve, 
**       (i.e. copy original).
**    PARAMETERS
**       INPUT  :
**          eptr           NCL curve
**       OUTPUT :
**          rptr           copy of original NCL curve
**    RETURNS      :
**          0              if no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_c7_frmnclcrv(eptr,rptr)
struct  NCL_curve_rec  *eptr;
struct  NCL_curve_rec  *rptr;
{
	int status, i;
	struct NCL_segment_rec;

	status = UU_SUCCESS;
	ur_setup_data(NCL_CURVE_REL, rptr, um_curve_size(eptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (rptr->label, "");
	rptr->subscr = 0;

	rptr->key = -1;
	rptr->rel_num = NCL_CURVE_REL;
	rptr->closdinu = eptr->closdinu;
	rptr->t0 = eptr->t0;
	rptr->t1 = eptr->t1;
	rptr->t_end = eptr->t_end;
	rptr->no_param = eptr->no_param;
	rptr->no_segment = eptr->no_segment;

	for (i=0; i<eptr->no_param; i++)
		rptr->param[i] = eptr->param[i];
	for (i=0; i<eptr->no_segment; i++)
		rptr->segment[i] = eptr->segment[i];

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_disp_curve (eptr, tfmat, attptr)
**       Display an evaluated curve.
**    PARAMETERS
**       INPUT  :
**          eptr       - ptr to curve
**          tfmat      - transformation
**          attptr     - ptr to attribute bundle
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int
ncl_disp_curve (eptr, tfmat, attrptr)
struct NCL_curve_rec *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attrptr;

{
	UU_REAL u, tol;
	UU_LIST cvpoint; 
	int status, i, j, nu;
	struct UM_evcrvout evout;
	UM_int2 idx, ival;
	UU_REAL *ptcv;


	uu_denter(UU_MTRC,(us,"ncl_disp_rbsp(key=%x, tfmat=%x, attrptr=%x)",
		eptr->key, tfmat, attrptr));

	status = UU_FAILURE;
	um_set_disp_attr(attrptr);

	if (ncl_retrieve_data_fixed (eptr) == 0)
	{
		status = UU_SUCCESS;
/*
...replaced 11-jul-95 vp. using display with tolerance routine
...and dynamic memory allocation.
...Get tolerance, version flag & number of points to display
*/
		idx = 136;
		getifl(&idx, &ival);
		nu  = ival;
		idx = 175;
		getsc (&idx,&tol);
		j   = (tol < .005)? 100: 50; 
		if (nu > j) j = nu; 
		uu_list_init (&cvpoint, sizeof(UM_coord), j, j);
		
/*
...initialize list for points (slop not used here)
...and evolve curve
*/
		if (nu > 0) 
			nu   = ncl_evolve1_curve (eptr,tfmat, nu, &cvpoint);
		else
			nu   = ncl_evolve_curve (eptr,tfmat,tol,&cvpoint,UU_NULL,UU_NULL,0);
/*
...display curve
*/
		ptcv = (UU_REAL *) UU_LIST_ARRAY (&cvpoint);
		for (i=0, j=0; i<nu; i++, j+=3)
			 glina3 (&ptcv[j],&ptcv[j+1],&ptcv[j+2]);  

		gdraw();
/*
...draw label if necessary  
*/
		cvlbl(&ival);
		if (ival == 1)
		{
			u = .5;
			uc_evcrv (UM_POINT, u, eptr, tfmat, &evout); 
			drwlab(&evout.cp[0], &evout.cp[1], &evout.cp[2], &eptr->key);
		}
		uu_list_free (&cvpoint);
	}

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_redef_curve(eptr)
**       Restore original bspline curve if it was trimmed. 
**    PARAMETERS   
**       INPUT  : 
**				eptr								trimmed rational bspline curve
**       OUTPUT :  
**				eptr								original curve.
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_redef_curve (eptr)
struct NCL_curve_rec *eptr;
{
/*
...restore original curve length
*/
	eptr->t0 = 0.;
	eptr->t1 = eptr->param[eptr->no_param-1];

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_c7_endpoints(eptr, u, udel)
**       Extends display parameter of NCL curve over current
**       limit to untrim trimmed curve.. 
**    PARAMETERS   
**       INPUT  : 
**				eptr	  - trimmed NCL curve
**          u       - parameter value specified relative to the
**                    previous definition.
**       OUTPUT :  
**				eptr								original curve.
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_c7_endpoints(eptr, u, udel)
struct NCL_curve_rec *eptr;
UM_param u, *udel;
{
	UM_param urng, t;

	urng = eptr->t1 - eptr->t0;
	t = eptr->t0 + u * urng;

	if (u < 0.0)
	{
		eptr->t0 = t;
	}
	else if (u > 1.0)
	{
		eptr->t1 = t;
	}

	return(UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_copy_nclcrv (e1, e2, bagsize)
**       Copy a NCL curve.
**    PARAMETERS
**       INPUT  :
**          e1       - entity to copy.
**          bagsize  - size of entity.
**       OUTPUT :
**          e2       - copied entity.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_copy_nclcrv(e1, e2, bagsize)
struct NCL_curve_rec *e1, *e2;
int bagsize;
{
	int status = UU_FAILURE;

	if (ncl_copy_geom (e1, e2, bagsize) == UU_SUCCESS)
	{
		status = ur_update_data_varlist(e2->key,1,e1->param,1,e1->no_param);
		status = ur_update_data_varlist(e2->key,2,e1->segment,1,e1->no_segment);
		status = ncl_retrieve_data (e2, bagsize);
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_tpar (nclkey, buf)
**       Get a NCL curve t end values.
**    PARAMETERS
**       INPUT  :
**          e1       - entity to copy.
**       OUTPUT :
**          e2       - copied entity.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_tpar(nclkey, buf)
UM_real8 buf[];
UU_KEY_ID *nclkey;
{
	UU_KEY_ID key;
	struct NCL_curve_rec e;
	int rel_num;
	key = *nclkey;
	if (ur_retrieve_data_relnum(key, &rel_num) == 0)
	{
		e.key = key;
		if (ur_retrieve_data_fixed(&e) == 0)
		{ 
			buf[0] = e.t0;
			buf[1] = e.t1;
			buf[2] = e.t_end;
		}
	}
	return(0);  
} 

/*********************************************************************
**    E_FUNCTION     : int ncl_put_tpar (nclkey, buf)
**       Put NCL curve t end values into unibase.
**    PARAMETERS
**       INPUT  :
**          e1       - entity to copy.
**       OUTPUT :
**          e2       - copied entity.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_put_tpar(nclkey, buf)
UM_real8 buf[];
UU_KEY_ID *nclkey;
{
	UU_KEY_ID key;
	struct NCL_curve_rec e;
	int rel_num;
	key = *nclkey;
	if (ur_retrieve_data_relnum(key, &rel_num) == 0)
	{
		e.key = key;
		if (ur_retrieve_data_fixed(&e) == 0)
		{ 
			e.t0  = buf[0];
			e.t1  = buf[1];
			e.t_end  = buf[2];
			ur_update_data_fixed (&e);
		}
	}
	return(0);  
} 

/*********************************************************************
**    E_FUNCTION     : int ncl_rbsp_get_tpar (key,tpar)
**       Get rbspline curve end trim values from unibase bundle.
**    PARAMETERS   
**       INPUT  : 
**        key      curve nclkey
**       OUTPUT :  
**        buf      curve end parameters (3 double)
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_rbsp_get_tpar(nclkey, buf)
UM_real8 buf[];
UU_KEY_ID *nclkey;
{
	UU_KEY_ID key;
	struct UM_rbsplcrv_rec e;
	int rel_num, ncl_retrieve_data_fixed(),status = UU_FAILURE;
	key = *nclkey;
	if (ur_retrieve_data_relnum(key, &rel_num) == 0)
	{
		e.key = key;
		if ((status=ncl_retrieve_data_fixed(&e)) == 0)
		{
			buf[0] = e.t0;
			buf[1] = e.t1;
			buf[2] = e.t[e.no_t-1];
		}
	}
	return(status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_rbsp_put_tpar (key,tpar)
**       Put rbspline curve end trim values into unibase bundle.
**    PARAMETERS   
**       INPUT  : 
**        key      curve nclkey
**       OUTPUT :  
**        tpar      curve end trim parameters (3 double)
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_rbsp_put_tpar(nclkey, buf)
UM_real8 buf[];
UU_KEY_ID *nclkey;
{
	UU_KEY_ID key;
	struct UM_rbsplcrv_rec e;
	int rel_num, ncl_retrieve_data_fixed(),status = UU_FAILURE;
	key = *nclkey;
	if (ur_retrieve_data_relnum(key, &rel_num) == 0)
	{
		e.key = key;
		if ((status=ncl_retrieve_data_fixed(&e)) == 0)
		{
			e.t0 = buf[0];
			e.t1 = buf[1];
			ur_update_data_fixed (&e);
		}
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncvevl1 (nclkey, evflg, u, pt, vec)
**       Fortran callable evaluation of NCL curve.
**    PARAMETERS
**       INPUT  :
**          nclkey  - key to NCL curve.
**          evflag  - evaluation option (all supported but output 
**                    parameters are for point and first derivative only.
**          u       - evaluation parameter value [0,1.0].
**     
**       OUTPUT :
**          pt      - point at u.
**          vec     - first derivative (slope vector) at u.
**    RETURNS      :
**         UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncvevl1 (nclkey, evflg, u, pt, vec)
UM_real8 *pt, *vec, *u;
UU_KEY_ID *nclkey;
short *evflg;
{
	UM_real8 uu;
	int evflag; 
	struct UM_crvdatabag e;
	UM_transf tfmat;
	struct UM_evcrvout evout;

	e.key = *nclkey;
	ncl_retrieve_data_fixed(&e);
	uc_retrieve_transf(e.key,tfmat);
	uu     = *u;
	evflag = *evflg;
	uc_evcrv (evflag,uu,&e,tfmat,&evout);
	um_vctovc (evout.cp,pt);
	um_vctovc (evout.dcdu,vec);

	return(0);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL ncl_cv_isclosed (cv,tol)
**       Check if a curve is closed.
**    PARAMETERS
**       INPUT  :
**          cv      - pointer to curve structure
**          tol     - tolerance
**       OUTPUT : none
**    RETURNS      :
**         UU_TRUE if closed; UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_cv_isclosed (cv,tol)
struct NCL_fixed_databag *cv;
UU_REAL tol;
{
	struct UM_evcrvout evout;
	UM_coord pt0;
	UU_REAL dis;
	UU_LOGICAL lclosed;

	uc_evcrv (UM_POINT,(UU_REAL)0.,cv,UM_idmat,&evout);
	um_vctovc (evout.cp,pt0);
	uc_evcrv (UM_POINT,(UU_REAL)1.,cv,UM_idmat,&evout);
	dis = UM_SQDIS (pt0,evout.cp);
	lclosed = (dis < tol*tol);

	return (lclosed);
}
 
/*********************************************************************
**    E_FUNCTION : int ncl_ssonsf1 (sfkey,bnum,cvkey,type,ibnum,ienum,idir)
**       Fortran callable function to extract surface spline from
**       trimmed surface.
**    PARAMETERS
**       INPUT  :
**			sfkey	- key to trimmed/rbspline surface.
**                    parameters are for point and first derivative only.
**			bnum	- boundary curve number to retrieve.
**			type	- surface type
**			ibnum	- start sub crvid of the composite curve to retrieve.
**			ienum	- end sub crvid of the composite curve to retrieve.
**			idir	- direction of crvid, 1:CLW, -1:CCLW
**       OUTPUT :
**          cvkey	- nclkey of created curve.
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ssonsf1(sfkey, bnum, cvkey, type, ibnum, ienum, idir)
UU_KEY_ID *sfkey, *cvkey;
int *bnum, *ibnum, *ienum, *idir;
UM_int2 *type;
{
	struct NCL_trimsf_rec surf;
	struct NCL_fixed_databag ccrv;
	struct  UC_entitydatabag c2;
	struct  NCL_nclattr_rec *nattr;
	UM_transf tran, tmat;
	UU_KEY_ID keyin;
	struct UC_attributedatabag tatr;
	int status, crvdir, crvid, subid, subid1, color;
/*
.....Initialize routine
*/
	*cvkey = 0;
	surf.key = *sfkey;
	crvid = *bnum;
	subid = *ibnum;
	subid1 = *ienum;
	crvdir = *idir;
/*
.....Get the surface
*/
	status = ncl_retrieve_data_fixed(&surf);

	if (status == UU_SUCCESS)
	{
		if (surf.rel_num == NCL_TRIMSF_REL)
		{
/*
.....Get the surface transformation
*/
			uc_retrieve_transf (surf.key,tran);
/*
.....Get the appropriate curve
*/
			if (crvid == 0)
				keyin = surf.uv_key;
			else 
			{
				if (crvid >0 && crvid <= surf.no_ibndykey/2)
					keyin = surf.ibndykey[2*crvid-1];
				else
				{
					status = UU_FAILURE;
					return(UU_FAILURE);
				}
			}
			if (keyin != 0)
			{
				int size = sizeof(struct NCL_fixed_databag);
				ccrv.key = keyin;
				status = ncl_retrieve_data(&ccrv,size);
				if (status == UU_SUCCESS)
				{
					status = UU_FAILURE;
/*
........Composite curve
*/
					if(ccrv.rel_num == UM_COMPCRV_REL)
					{
						status = um_tboundary_to_sscomp(sfkey,&ccrv,&c2,
													subid-1,subid1-1,crvdir);
					}
					else
					{
/*
........Bspline curve
*/
						if (ccrv.rel_num == UM_RBSPLCRV_REL)
							status = um_cp_struct_rbcv_uvcv (&ccrv,sfkey,&c2);
/*
........Line
*/
						if (ccrv.rel_num == UM_LINE_REL)
							status = um_cp_struct_line_uvcv (&ccrv,sfkey,&c2);
					}  
/*
.....Create the surface spline
*/
					if (status == UU_SUCCESS)
					{			
						ncl_retrieve_ent(&c2,&tatr,&tmat);
						ncl_get_entity_color(c2.key, &color);
						tatr.color = color;
						nattr  = (struct  NCL_nclattr_rec *) &(tatr);
						nattr->label_on = 0;
						status = ur_update_attr(&tatr);
						ur_update_displayable(c2.key, UM_DISPLAYABLE);
						ncl_get_type(c2.rel_num,type);
						*cvkey = c2.key;
					}
				}
				else
				{
					return(UU_FAILURE);
				}
			}
		}
		else
			status = UU_FAILURE;
	}
/*
.....Copy the surface transformation
.....to the surface spline
*/
	if (status == UU_SUCCESS)
		uc_transform(&c2,tran,UU_TRUE);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_cvonsf_func (sfkey,bnum,cvkey,type,ibnum,ienum,idir)
**       Fortran callable function to extract B-spline from
**       trimmed surface.
**    PARAMETERS
**       INPUT  :
**          sfkey   - key to trimmed surface.
**                    parameters are for point and first derivative only.
**          bnum    - boundary curve number to retrieve.
**          ibnum   - start sub crvid of the composite curve to retrieve.
**          ienum   - end sub crvid of the composite curve to retrieve.
**			idir	- direction of crvid,1:CLW, -1:CCLW
**
**       OUTPUT :
**          cvkey   - nclkey of created curve.
**          type    - curve type
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**                 Changed the name of the function from ncl_cvonsf
**                 to ncl_cvonsf_func to eliminate problems with
**                 VMS reading ncl_cvonsf and NCL_cvonsf as the 
**                 same.  JLS 8/2/99
*********************************************************************/
int
ncl_cvonsf_func (sfkey, bnum, cvkey, type, ibnum,ienum,idir)
UU_KEY_ID *sfkey, *cvkey;
int *bnum, *ibnum, *ienum,*idir;
UM_int2 *type;
{
	struct NCL_trimsf_rec surf;
	struct NCL_fixed_databag ccrv,c2;
	struct  NCL_nclattr_rec *nattr;
	UM_transf tran;
	UU_KEY_ID keyin;
	struct UC_attributedatabag tatr;
	int crvdir,crvid,numcvs,status,subid,subid1,id,color;
	UM_int2 ier472 = 472;

/*
.....initialize the key to 0 to avoid UMR in um_create_geom
*/
	c2.key = 0;
	*cvkey = 0;
	surf.key = *sfkey;
	crvid = *bnum;
	subid = *ibnum;
	subid1 = *ienum;
	crvdir = *idir;
	
	status = ncl_retrieve_data_fixed(&surf);
	if (status != UU_SUCCESS || surf.rel_num != NCL_TRIMSF_REL)
		return (UU_FAILURE);
	uc_retrieve_transf (surf.key,tran);

	if (crvid < 0 || crvid > surf.no_ibndykey/2) 
		return (ier472);

	if (crvid == 0) 
		keyin = surf.cv_key;
	else 
		keyin = surf.ibndykey[2*crvid-2];

	if (keyin != 0)
	{
		int size = sizeof(struct NCL_fixed_databag);
		ccrv.key = keyin;
		status = ncl_retrieve_data(&ccrv,size);
		if (status == UU_SUCCESS)
		{
			if (!ncl_itsa_compcrv(&ccrv))
			{
				if (uc_super_class(ccrv.rel_num) != UC_CURVE_CLASS)
					status = UU_FAILURE;
				else
					status = uc_copy (&ccrv,&c2,size);
			}
			else if (subid <= 0)
				status = um_cp5_copycompcrv1(&ccrv,0,0,0,&c2);
			else if (fabs(subid1 - subid) >= 1 && subid > 0 && subid1 > 0)	
			{
				status = um_cp5_copycompcrv1(&ccrv,subid-1,subid1-1,crvdir,&c2);	
				umi_fix_subcurve_fields(&c2,tran);
			}
			else
			{
				int ncv,rev;
				struct NCL_fixed_databag ccrv1;
			
				status = ncl_compcrv_getnents (&ccrv, &ncv);
				if (subid > ncv)
					return (ier472);
				if (status == UU_SUCCESS)
				{
					status = ncl_compcrv_getelm (&ccrv, subid-1, &ccrv1, &rev);
					if (status == UU_SUCCESS)
					{
						if (uc_super_class(ccrv1.rel_num) != UC_CURVE_CLASS)
							status = UU_FAILURE;
						else
						{
							if (ccrv1.rel_num == UM_RBSPLCRV_REL)
								status = uc_copy (&ccrv1,&c2,size);
							else
							{
								struct UM_rbsplcrv_rec *crv2;
								int npt,k,nwt;
								UU_REAL *t,*pt,*wt;
								UU_REAL t0,t1;

								size = um_curve_size (&ccrv1);
								um_allocate_curve (&crv2,size);
								status = um_rbcrv_frmnclcrv (&ccrv1,crv2);
								if (status == UU_SUCCESS)
								{
									t = crv2->t;
									pt = crv2->pt;
									wt = crv2->wt;
									npt = crv2->no_pt;
									k = crv2->k; 
									nwt =crv2->no_wt;
									t0 = crv2->t0; 
									t1 = crv2->t1;
									status = ncl_create_rbsp1 (k,npt,nwt,
										t,pt,wt,t0,t1,&c2);
								}
								if (size > 0) uu_toolfree (crv2);
							}
						}
					}
				}
			}

			if (status == UU_SUCCESS)
			{
				uc_transform(&c2,tran,UU_TRUE);
				*cvkey = c2.key;
				ncl_def_color (c2.key);
			}
		}
	}

	if (keyin == 0 || status != UU_SUCCESS)
	{
		UM_real8 tol8;
		UU_REAL tol;

		gettol (&tol8);
		tol = tol8;

		if (subid <= 0)	
			status = ncl_uvcrv_to_crv (&surf,tran,crvid,tol,cvkey,0,0,0);
		else if (subid > 0 && subid1 >= 0)	
			status = ncl_uvcrv_to_crv (&surf,tran,crvid,tol,cvkey,
									subid-1,subid1-1,crvdir);

		if (status != UU_SUCCESS || *cvkey <= 0)
		{
			if (status == 472) return (ier472);
			else return (UU_FAILURE);
		}

		c2.key = *cvkey;
	}

	if (status == UU_SUCCESS)
	{
		ncl_retrieve_ent(&c2,&tatr,&tran);
		ncl_get_entity_color(c2.key, &color);
		tatr.color = color;
		nattr = (struct  NCL_nclattr_rec *) &(tatr);
		nattr->label_on = 0;
		status = ur_update_attr(&tatr);
		ur_update_displayable(c2.key, UM_DISPLAYABLE);
		ncl_get_type(c2.rel_num,type);
	}
	else
		status = UU_FAILURE;

   return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_maxim_crv1 (key,tpar)
**       Dispatching routine to maximize curve if it is trimmed.
**        
**    PARAMETERS   
**       INPUT  : 
**        key      curve nclkey
**       OUTPUT :  
**        tpar     buffer for 3 trimm t (t0,t1,t[n])
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_maxim_crv1 (key,tpar)
int *key;
UU_REAL *tpar;
{
	int status, relnum;
	UU_REAL t[3];

	status = UU_SUCCESS;
	if (ur_retrieve_data_relnum(*key, &relnum) == 0)
	{
		switch (relnum)
		{
			case UM_RBSPLCRV_REL:
				if (tpar[0] != 0. || tpar[1] != tpar[2]) 
				{
					t[0] = MIN2 (tpar[0],0.);
					t[1] = MAX2 (tpar[1],tpar[2]);
					status = ncl_rbsp_put_tpar(key,t);
				}
				break;    
			case NCL_CURVE_REL:
				if (tpar[0] != 0. || tpar[1] != 1.) 
				{
					t[0] = MIN2 (tpar[0],0.0);
					t[1] = MAX2 (tpar[1],1.0);
					t[2] = tpar[2];
					status = ncl_put_tpar(key,t);
				}
				break;    
			default:
				status = UU_FAILURE;
				break;
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get1_tpar1 (key,tpar)
**       Dispatching routine to get trim parameters from the 
**       general curve 'eptr'.
**    PARAMETERS   
**       INPUT  : 
**        key      curve nclkey
**       OUTPUT :  
**        tpar     buffer for 3 trimm t (t0,t1,t[n])
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get1_tpar1 (key,tpar)
int *key;
UU_REAL *tpar;
{
	int status, relnum;

	if (ur_retrieve_data_relnum(*key, &relnum) == 0)
	{
		switch (relnum)
		{
			case UM_RBSPLCRV_REL:
				status = ncl_rbsp_get_tpar(key,tpar);
				break;    
			case NCL_CURVE_REL:
				status = ncl_get_tpar(key,tpar);
				break;    
			default:
				status = UU_FAILURE;
				break;
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_put1_tpar1 (key,tpar)
**       Dispatching routine to get end trimm parameters from a 
**       general curve 'eptr'.
**    PARAMETERS   
**       INPUT  : 
**        key      curve nclkey
**       OUTPUT :  
**        tpar      curve end parameters (3 double)
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_put1_tpar1 (key,tpar)
int *key;
UU_REAL *tpar;
{
	int status, relnum;

	if (ur_retrieve_data_relnum(*key, &relnum) == 0)
	{
		switch (relnum)
		{
			case UM_RBSPLCRV_REL:
				status = ncl_rbsp_put_tpar(key,tpar);
				break;    
			case NCL_CURVE_REL:
				status = ncl_put_tpar(key,tpar);
				break;    
			default:
				status = UU_FAILURE;
				break;
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_cvon_revsf (sfkey,cvkey,type)
**       Fortran callable function to extract generating curve from
**       surface.
**    PARAMETERS
**       INPUT  :
**          sfkey   - key to trimmed surface.
**                    parameters are for point and first derivative only.
**
**       OUTPUT :
**          cvkey   - nclkey of created curve.
**          type    - curve type
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_cvon_revsf (sfkey, cvkey, type)
UU_KEY_ID *sfkey, *cvkey;
UM_int2 *type;
{
	struct NCL_trimsf_rec surf;
	struct NCL_revsurf_rec rev;
	struct NCL_fixed_databag e,ccrv,c2;
	struct  NCL_nclattr_rec *nattr;
	UM_transf tran;
	UU_KEY_ID keyin;
	struct UC_attributedatabag tatr;
	int status, color;
	UM_int2 primtyp;
	UM_int2 edge,np,lpercnt;
	UM_real8 offset, tol8;
	UU_REAL tol;

/*
.....initialize the key 
*/
	c2.key = 0;
	*cvkey = 0;
	e.key = *sfkey;
	
/*
.....consider base surface for trimsrf
*/
	status = ncl_retrieve_data_fixed(&e);
	uc_retrieve_transf (e.key,tran);
	if (e.rel_num == NCL_TRIMSF_REL)
	{
		surf.key = *sfkey;
		status = ncl_retrieve_data_fixed(&surf);
		e.key = surf.bs_key;
		status = ncl_retrieve_data_fixed(&e);
	}
/*
.....check if cone/cylinder/revsf
*/
	ncl_get_sf_primtyp(&e.key,&primtyp);
	if(primtyp != NCLSF_CYLINDER && primtyp !=NCLSF_CONE &&
		e.rel_num != NCL_REVSURF_REL)
		return (UU_FAILURE);

/*
.....create curve along generatrix for revsf
*/
	if(e.rel_num == NCL_REVSURF_REL)
	{
		rev.key = e.key;
		status = ncl_retrieve_data_fixed(&rev);
		keyin = rev.cvkey;
	}
/*
.....Create a curve along the surface edge u=0 for cone/cylinder
*/
	else if(primtyp == NCLSF_CYLINDER || primtyp == NCLSF_CONE )
	{
		gettol (&tol8);
		tol = tol8;
		edge = 3;
		np = 0;
		lpercnt = 0;
		offset = 0;
		ncl_cvsfsd (&e.key, cvkey, &edge, &np, &lpercnt, &offset, &tol);
		c2.key = *cvkey;
		status = ncl_retrieve_data_fixed(&c2);
		ncl_get_type(c2.rel_num,type);
		goto done;
	}

	if (keyin != 0)
	{
		int size = sizeof(struct NCL_fixed_databag);
		ccrv.key = keyin;
		status = ncl_retrieve_data(&ccrv,size);
		if (status == UU_SUCCESS)
		{
			if (!ncl_itsa_compcrv(&ccrv))
			{
				if (uc_super_class(ccrv.rel_num) != UC_CURVE_CLASS)
					status = UU_FAILURE;
				else
					status = uc_copy (&ccrv,&c2,size);
			}
			else 
				status = um_cp5_copycompcrv1(&ccrv,0,0,0,&c2);
			

			if (status == UU_SUCCESS)
			{
				uc_transform(&c2,tran,UU_TRUE);
				*cvkey = c2.key;
				ncl_def_color (c2.key);
			}
		}
	}

	if (status == UU_SUCCESS)
	{
		ncl_retrieve_ent(&c2,&tatr,&tran);
		ncl_get_entity_color(c2.key, &color);
		tatr.color = color;
		nattr = (struct  NCL_nclattr_rec *) &(tatr);
		nattr->label_on = 0;
		status = ur_update_attr(&tatr);
		ur_update_displayable(c2.key, UM_DISPLAYABLE);
		ncl_get_type(c2.rel_num,type);
	}
	else
		status = UU_FAILURE;
done:
   return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_sson_revsf (sfkey, cvkey, type)
**       Fortran callable function to extract surface spline from
**       a rev surface.
**    PARAMETERS
**       INPUT  :
**          sfkey   - key id of surface.
**
**       OUTPUT :
**          type    - curve type
**          cvkey   - nclkey of created curve.
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sson_revsf(sfkey,cvkey, type)
UU_KEY_ID *sfkey, *cvkey;
UM_int2 *type;
{
	struct NCL_trimsf_rec surf;
	struct NCL_fixed_databag e,ccrv,*sscv;
	struct NCL_revsurf_rec rev;
	UM_int2 primtyp;
	struct  UC_entitydatabag c2;
	struct  NCL_nclattr_rec *nattr;
	UM_transf  tmat;
	UU_KEY_ID keyin,lkey;
	struct UC_attributedatabag tatr;
	int i,npt,status,  color;
	UM_real8  tol8;
	UU_REAL s[4],tol;
	UM_coord pts[3];
	UM_real4 uv[2];
	UM_int2 itsk, *jerr, i0=0;
/*
.....initialize the key to 0 to avoid UMR in um_create_geom
*/
	c2.key = 0;
	*cvkey = 0;
	e.key = *sfkey;
	status = ncl_retrieve_data_fixed(&e);
	uc_retrieve_transf (e.key,tmat);
/*
.....consider base surface in the case of a trimmed surface
*/
	if (e.rel_num == NCL_TRIMSF_REL)
	{
		surf.key = *sfkey;
		status = ncl_retrieve_data_fixed(&surf);
		e.key = surf.bs_key;
		status = ncl_retrieve_data_fixed(&e);
	}

	ncl_get_sf_primtyp(&e.key,&primtyp);
	if(primtyp != NCLSF_CYLINDER && primtyp !=NCLSF_CONE &&
		e.rel_num != NCL_REVSURF_REL)
		return (UU_FAILURE);
/*
.....For surfaces of revolution get the generating curve
*/
	if(e.rel_num == NCL_REVSURF_REL)
	{
		rev.key = e.key;
		status = ncl_retrieve_data_fixed(&rev);
		keyin = rev.cvkey;
		if (keyin != 0)
		{
			int size = sizeof(struct NCL_fixed_databag);
			ccrv.key = keyin;
			status = ncl_retrieve_data(&ccrv,size);
			itsk = 2;
			uv[0]=uv[1]=0.5;
			status = ncl_create_sspline(itsk,e.key,keyin,uv,&lkey,&jerr);
/*
.....Create the surface spline
*/
			if (status == UU_SUCCESS)
			{
				c2.key=lkey;
				status = ncl_retrieve_data_fixed(&c2);
				uc_transform(&c2,tmat,UU_TRUE);
			}
		}
		else
			status = UU_FAILURE;
	}
/*
.....For cylindrical or conical surfaces get the curve at u=0
*/
	else if(status == UU_SUCCESS &&
		(primtyp == NCLSF_CYLINDER || primtyp == NCLSF_CONE))
	{
		gettol (&tol8);
		tol = tol8;
		s[0]=s[1]=0.0;
		s[2]=s[3]=1.0;
		for(i=0;i<3;i++)
			um_nullvc(pts[i]);
		pts[1][1]=1;
		npt =2;
		ncl_create_ssplin (&e.key,s,pts,&npt,cvkey);
		c2.key = *cvkey;
	}
	
done:
	if (status == UU_SUCCESS)
	{	
		status = ncl_retrieve_data_fixed(&c2);
		sscv = (struct NCL_fixed_databag *)&c2;
		ncl_label_wf(sscv->rel_num,sscv->label,&sscv->subscr,sscv->key,&i0);
		ncl_store_wf1(sscv->key);
		ncl_get_type(c2.rel_num,type);
		ncl_retrieve_ent(&c2,&tatr,&tmat);
		ncl_get_entity_color(c2.key, &color);
		tatr.color = color;
		nattr  = (struct  NCL_nclattr_rec *) &(tatr);
		nattr->label_on = 0;
		status = ur_update_attr(&tatr);
		ur_update_displayable(c2.key, UM_DISPLAYABLE);
		ncl_get_type(c2.rel_num,type);
		*cvkey = c2.key;
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_nclcv_feat (eptr, tfmat, feature_order, dploc)
**       Calculate features for NCL curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr              pointer to NCl curve
**          tfmat             transformation matrix
**          feature_order     order of feature to display
**          dploc             picked location on Cv
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff on error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    NOTE         : Created from feature/f7slpine.c um_f7_spline()
*********************************************************************/
ncl_nclcv_feat (eptr, tfmat, feature_order, dploc)
struct NCL_curve_rec *eptr;
UM_transf tfmat;
int feature_order;
UD_PLOCREC *dploc;
{
	int status;
	UM_vector vec;
	UU_REAL len;
	struct UM_evcrvout evout;
	int i,j;
	struct NCL_segment_rec *segp;

	UU_REAL pt[3],pt1[3],pt2[3],ptn1[3],ptn2[3], rho;

	/* transform geometry to model space */
	ncl_transform_nclcv(eptr, tfmat, UU_FALSE);

	/* calculate desired features */
	status = UU_SUCCESS;
	switch (feature_order)
	{
		case 1:
			/* show control points */

			segp = eptr->segment;
			status = um_feacoord(&segp->point);
			if (status!=0)return UU_FAILURE;
			um_vctovc(segp->point,pt1);
			for (i=1;i<eptr->no_segment;i++)
			{
				for (j=0;j<3;j++) pt[j] = segp->point[j]+segp->delta[j];
				status = um_feacoord(&pt);
				if(i==1) um_vctovc(pt,pt2);
				if (status!=0)return UU_FAILURE;
				rho = segp->rho;
				segp++;
				for (j=0;j<3;j++) pt[j] = segp->point[j]-rho*segp->delta[j];
				if(i==eptr->no_segment-1) um_vctovc(pt,ptn1);
				if(i==eptr->no_segment-1) um_vctovc(segp->point,ptn2);
				status = um_feacoord(&pt);
				if (status!=0)return UU_FAILURE;
				status = um_feacoord(&segp->point);
				if (status!=0)return UU_FAILURE;
			}
			/* show derivatives */
			uc_init_evcrvout(eptr, &evout);

			/* scale tangent vector at end points */
			uc_evcrv(UM_FRSTDERIV, (UU_REAL) 0.0, eptr, UM_idmat, &evout);

			um_vcmnvc(pt1, pt2, vec);
			len = um_mag(vec);
			um_unitvc(evout.dcdu, vec);
			um_vctmsc(vec, 2.0*len, vec);
			status = um_feavect(evout.cp, vec);
			if (status!=0)
			return UU_FAILURE;

			/* scale tangent vector at end points */
			uc_evcrv(UM_FRSTDERIV, (UU_REAL) 1.0, eptr, UM_idmat, &evout);

			um_vcmnvc(ptn1, ptn2, vec);
			len = um_mag(vec);
			um_unitvc(evout.dcdu, vec);
			um_vctmsc(vec, 2.0*len, vec);
			status = um_feavect(evout.cp, vec);
			if (status!=0)
				return UU_FAILURE;
			break;

		case 2:
			/* tangents at knot values */
			/* length of spline */
			break;

		case 3:
			break;

		default:
			status = UU_FAILURE;

	}
	uu_dexit;
	return(status);
}
