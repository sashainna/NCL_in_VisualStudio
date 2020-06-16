/*********************************************************************
**    NAME         :  tiomsrf.c
**    CONTAINS:
**       uio_agsrf_rbspl
**       uio_rbsrf
**       uio_revsrf
**       uio_nclsrf
**       uio_mshsrf 
**       gtspan   
**       nclsrf_bspdef 
**       uio_map_mpatch 
**       uio_trimsrf
**       uio_offset_srf
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tiomsrf.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/17/15 , 17:57:47
*********************************************************************/

#include    "usysdef.h"
#include    "mdeval.h"
#include    "udebug.h"
#include    "tiges.h"
#include    "tigdefs.h"
#include    "rbase.h"
#include    "class.h" 
#include    "mdrel.h" 
#include    "mdattr.h"
#include    "mattrddl.h"
#include    "mcrv.h"
#include    "msrf.h"
#include    "mdcoord.h"
#include    "tioconv.h"
#include    "modef.h"
#include    "mxxx.h"
#include    "view.h"
#include    "nccs.h"
#include    "ulist.h"
#include    "nclvx.h"
#include		"nclfc.h"
#include		"ncl.h"

#include "ag_incl.h"

extern int UIO_sub_swt;
extern int UIO_use_flg;
extern UU_LOGICAL UIO_drawing;
extern UU_LOGICAL UIO_uvcurve;
extern UU_LOGICAL bump_u, bump_v;
char  *uu_toolmalloc();

int doing_trimsrf = UU_FALSE;
UM_2Dcoord trim_center;

static int ixa[16] = {0,4,8,12,1,5,9,13,2,6,10,14,3,7,11,15};

/*********************************************************************
**    I_FUNCTION     :  uio_agsrf_rbspl(fd1,fd2,agcrv,attr,dcount,pcount)
**          Map a UNICAD AG rational bspline batch to a IGES rational 
**          bspline batch.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_agsrf_rbspl(fd1,fd2,agsrf,attr,dcount,pcount)
int   fd1, fd2;
struct UM_agsrf_rec  *agsrf;
struct UR_attr    *attr;
int   *dcount;    /* directory section sequence number */
int   *pcount; /* parameter section sequence number */

{
	int status;
	int  i, j, k, iu, iv, ku, kv, mu, mv, multu, multv, nu, nv, prop[5],
		 rat, numknots, ratu, ratv, stype, u_top, u_bot, v_top, v_bot;
	AG_SNODEP   nptr, nptr0;
	AG_SURFACEP srf;
	UU_REAL pt_cc[3], pt[3];
	struct IG_igesrssf_rec rsplsrf;  /* IGES rational bspline surface record */
	struct  dir_rec dblk;      /* directory record */
	UU_LOGICAL  blanked;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	status = UU_SUCCESS;

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, agsrf->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(agsrf->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GRSPLSRF;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"RSPLSRF");  
	dblk.subsno = agsrf->subscr;      /*721vp*/

	srf = (AG_SURFACEP) agsrf->srfaddr;

	multu = ag_q_srf_mek_u(srf);
	multv = ag_q_srf_mek_v(srf);
	stype = srf->stype;
	dblk.form_no = stype;
	mu = srf->mu;
	mv = srf->mv;
	nu = srf->nu;
	nv = srf->nv;
	ratu = srf->ratu;  ratv = srf->ratv;
	rat = (ratu || ratv);
	ku =  nu;
	kv =  nv;

	rsplsrf.key = GRSPLSRF;
	rsplsrf.indx1 = ku;
	rsplsrf.indx2 = kv;
	rsplsrf.degree1 = mu;
	rsplsrf.degree2 = mv;
	rsplsrf.open1 = 0;
	rsplsrf.open2 = 0;
	rsplsrf.type = 0;
	rsplsrf.period1 = 0;
	rsplsrf.period2 = 0;

	rsplsrf.no_t1 = ku + 2*(mu - 1) + 1;
	rsplsrf.no_t2 = kv + 2*(mv - 1) + 1;
	rsplsrf.no_w =  (ku + mu)*(kv + mv);
	rsplsrf.no_pt3 =  rsplsrf.no_w;
	rsplsrf.no_rspara =  1;


	rsplsrf.t1 = (UU_REAL *) uu_toolmalloc(rsplsrf.no_t1*sizeof(UU_REAL));
	rsplsrf.t2 = (UU_REAL *) uu_toolmalloc(rsplsrf.no_t2*sizeof(UU_REAL));
	rsplsrf.w  = (UU_REAL *) uu_toolmalloc(rsplsrf.no_w*sizeof(UU_REAL));
	rsplsrf.pt3  = (UU_REAL *) uu_toolmalloc(3*rsplsrf.no_pt3*sizeof(UU_REAL));
	rsplsrf.rspara = (struct IG_rspara_rec *)
									uu_toolmalloc(sizeof(struct IG_rspara_rec));

	/* output knots */
	nptr0 = srf->node0;
	if (!multu) for (iu=1; iu<mu; iu++)   nptr0 = nptr0->uprev;
	if (!multv) for (iv=1; iv<mv; iv++)   nptr0 = nptr0->vprev;
	u_bot = (multu) ? 0  : 1 - mu;
	u_top = (multu) ? nu : ku;
	v_bot = (multv) ? 0  : 1 - mv;
	v_top = (multv) ? nv : kv;
 
	/* first u direction */
	nptr = nptr0;
	j = 0;
	numknots = u_top - u_bot + 1;
	if(multu)
	{
		for(i=0,j=0;i<(mu-1);i++,j++) rsplsrf.t1[j] = *nptr->u;
		for (i=0; i<numknots; i++,j++) 
		{
			rsplsrf.t1[j] = *nptr->u;
			nptr = nptr->unext;
		}
		for(i=0;i<(mu-1);i++,j++) rsplsrf.t1[j] = *nptr->u;
	}
	else
	{
		for (i=0; i<numknots; i++,j++) 
		{
			rsplsrf.t1[j] = *nptr->u;
			nptr = nptr->unext;
		}
	}

	/* now v direction */
	nptr = nptr0;
	numknots = v_top - v_bot + 1;
	j = 0;
	if(multv)
	{
		for(i=0,j=0;i<(mv-1);i++,j++) rsplsrf.t2[j] = *nptr->u;
		for (i=0; i<numknots; i++,j++) 
		{
			rsplsrf.t2[j] = *nptr->v;
			nptr = nptr->vnext;
		}
		for(i=0;i<(mv-1);i++,j++) rsplsrf.t2[j] = *nptr->v;
	}
	else
	{
		for (i=0; i<numknots; i++,j++) 
		{
			rsplsrf.t2[j] = *nptr->v;
			nptr = nptr->vnext;
		}
	}

	/* now output weights and control points */
	nptr0 = srf->node0;
	j = 0;
	k = 0;
	for (iv=0; iv<(nv+mv); iv++) 
	{
		nptr = nptr0;
		for (iu=0; iu<(nu+mu); iu++) 
		{
			if(rat)
				rsplsrf.w[j] = nptr->Pw[3];            /* weight */
			else
				rsplsrf.w[j] = 1.0;           /* weight */
			j++;

			pt_cc[0] = nptr->Pw[0];
			pt_cc[1] = nptr->Pw[1];
			pt_cc[2] = nptr->Pw[2];
			UIO_CC_INTTOEXT(pt_cc,pt);
			um_vctovc(pt, &rsplsrf.pt3[k]);     /* control point   */
			k = k+3;
			nptr = nptr->unext;
		}
		nptr0 = nptr0->vnext;
	}

	/* set parameter range */
	rsplsrf.rspara[0].u0 = 0.;
	rsplsrf.rspara[0].u1 = 1.;
	rsplsrf.rspara[0].v0 = 0.;
	rsplsrf.rspara[0].v1 = 1.;
/*
.....put label in properties
*/
	uio_label (fd1,fd2,agsrf->label,dcount,pcount);
	rsplsrf.no_bptr = 0;
	dblk.par_ptr = *pcount;
	rsplsrf.no_prop = 1;
	prop[0] = *dcount - 2;
	rsplsrf.prop = &prop[0];

	uio_put_para(GRSPLSRF,&rsplsrf,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);

done:;
	uu_toolfree(rsplsrf.t1);
	uu_toolfree(rsplsrf.t2);
	uu_toolfree(rsplsrf.w);
	uu_toolfree(rsplsrf.pt3);
	uu_toolfree(rsplsrf.rspara);

	return (status);
}

/*********************************************************************
**    I_FUNCTION     :  uio_rbsrf (fd1,fd2,agcrv,attr,dcount,pcount)
**          Map a UNICAD rational bspline surface to a IGES rational bspline
**          surface.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_rbsrf (fd1,fd2,e,attr,dcount,pcount)
int   fd1, fd2;
struct UM_rbsplsrf_rec  *e;
struct UR_attr    *attr;
int   *dcount;    /* directory section sequence number */
int   *pcount; /* parameter section sequence number */
{
	int status, prop[5];
	int  i, j, ku, kv, mu, mv, nu, nv,num;
	UU_REAL pt_cc[3], pt[3], fct;
	struct IG_igesrssf_rec rsplsrf;  /* IGES rational bspline surface record */
	struct  dir_rec dblk;      /* directory record */
	UU_LOGICAL  blanked;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	status = UU_SUCCESS;

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GRSPLSRF;
	dblk.par_ptr = *pcount;
    if (e->offdist != 0.0)
		dblk.sub_swt = 1;
	else	
		dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"RSPLSRF");  
	dblk.subsno = e->subscr;      /*721vp*/

	dblk.form_no = 0;
	ku = e->ku;
	kv = e->kv;
	nu = e->nu;
	nv = e->nv;
	mu = ku - 1;
	mv = kv - 1;

	rsplsrf.key = GRSPLSRF;
	rsplsrf.indx1 = nu + mu - 1;
	rsplsrf.indx2 = nv + mv - 1;
	rsplsrf.degree1 = mu;
	rsplsrf.degree2 = mv;
	rsplsrf.open1 = 0;
	rsplsrf.open2 = 0;
	rsplsrf.type = 0;
	rsplsrf.period1 = 0;
	rsplsrf.period2 = 0;

	nu = nu + 2*mu + 1;
	nv = nv + 2*mv + 1;
	rsplsrf.no_t1 = nu;
	rsplsrf.no_t2 = nv;
	rsplsrf.no_w =  e->no_pt;
	rsplsrf.no_pt3 =  e->no_pt;
	rsplsrf.no_rspara =  1;

	rsplsrf.t1 = (UU_REAL *) uu_toolmalloc(nu*sizeof(UU_REAL));
	rsplsrf.t2 = (UU_REAL *) uu_toolmalloc(nv*sizeof(UU_REAL));
	rsplsrf.w  = (UU_REAL *) uu_toolmalloc(rsplsrf.no_pt3*sizeof(UU_REAL));
	rsplsrf.pt3  = (UU_REAL *) uu_toolmalloc(3*rsplsrf.no_pt3*sizeof(UU_REAL));
	rsplsrf.rspara = (struct IG_rspara_rec *)
									uu_toolmalloc(sizeof(struct IG_rspara_rec));

	/* output knots */
	/* first u direction */
/*
.....If surface has u min/max stored in tu array, use it to scale tu values
*/
	fct = 1.0;
	if (e->no_tu == nu+2)
		fct = e->tu[nu+1] - e->tu[nu];
	else
/*
.....Added the else statement, because there are times when the
.....additional start and end tu values are not placed in the
.....tu array, and max tu - min tu does not equal 1.0.  We
.....need to divide the current values in tu by the difference
.....otherwise the surface will be distorted. JLS 11/19/99
*/
	{
		fct = e->tu[nu-1] - e->tu[0];
		fct = 1.0/fct;
	}

	for (i=0; i<nu; i++) 
	{
		rsplsrf.t1[i] = e->tu[i]*fct;
	}

	/* now v direction */
/*
.....If surface has v min/max stored in tv array, use it to scale tv values
*/
	fct = 1.0;
	if (e->no_tv == nv+2)
		fct = e->tv[nv+1] - e->tv[nv];
/*
.....Same reason for adding this else statement as the addition
.....of the else statement for the tu's.
*/
	else
	{
		fct = e->tv[nv-1] - e->tv[0];
		fct = 1.0/fct;
	}

	for (i=0; i<nv; i++) 
	{
		rsplsrf.t2[i] = e->tv[i]*fct;
	}

	/* now output weights and control points */

	num = e->no_pt;
	j   = 0;
	for (i=0; i<num; i++) 
	{
		if (e->wt && e->no_wt > 0)
			rsplsrf.w[i] = e->wt[i];            /* weight */
		else
			rsplsrf.w[i] = 1.;
		pt_cc[0] = e->pt[j];
		pt_cc[1] = e->pt[j+1];
		pt_cc[2] = e->pt[j+2];
		UIO_CC_INTTOEXT(pt_cc,pt);
		um_vctovc(pt, &rsplsrf.pt3[j]);  /* control point   */
		j  = j + 3;
	}

/*
.....set parameter range. If it was saved in tu and tv arrays, use it,
.....otherwise use tu and tv start and end values
*/

	if (e->no_tu == nu+2)
	{
		rsplsrf.rspara[0].u0 = e->tu[nu];
		rsplsrf.rspara[0].u1 = e->tu[nu+1];
	}
	else
	{
		rsplsrf.rspara[0].u0 = e->tu[0];
		rsplsrf.rspara[0].u1 = e->tu[e->no_tu-1];
	}
	if (e->no_tv == nv+2)
	{
		rsplsrf.rspara[0].v0 = e->tv[nv];
		rsplsrf.rspara[0].v1 = e->tv[nv+1];
	}
	else
	{
		rsplsrf.rspara[0].v0 = e->tv[0];
		rsplsrf.rspara[0].v1 = e->tv[e->no_tv-1];
	}
/*
.....put label in properties
*/
	if (e->offdist != 0.0) strcpy(e->label,"@UN");
	uio_label (fd1,fd2,e->label,dcount,pcount);
	rsplsrf.no_bptr = 0;
	dblk.par_ptr = *pcount;
	rsplsrf.no_prop = 1;
	prop[0] = *dcount - 2;
	rsplsrf.prop = &prop[0];

	uio_put_para(GRSPLSRF,&rsplsrf,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
/*
.....vp 1/12/98 save reference in list for any possible cv on sf
*/ 
	update_sflist (*dcount-2,e->key);
    if (e->offdist != 0.0)
		uio_offset_srf(dblk, e->offdist, e->key, fd1, fd2, dcount, pcount);

done:;
	uu_toolfree(rsplsrf.t1);
	uu_toolfree(rsplsrf.t2);
	uu_toolfree(rsplsrf.w);
	uu_toolfree(rsplsrf.pt3);
	uu_toolfree(rsplsrf.rspara);

	return(status);
}  

/*********************************************************************
**    I_FUNCTION     :  uio_revsrf (fd1,fd2,agcrv,attr,dcount,pcount)
**          Map a NCL surface of revolution to an IGES surface of revolution
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_revsrf (fd1,fd2,e,attr,dcount,pcount)
int   fd1, fd2;
struct NCL_revsurf_rec  *e;
struct UR_attr    *attr;
int   *dcount;    /* directory section sequence number */
int   *pcount; /* parameter section sequence number */
{
	int status, prop[5];
	int savsub;
	struct IG_igesrvsf_rec revsrf;  /* IGES surface of revolution record */
	struct  dir_rec dblk;      /* directory record */
	UU_LOGICAL  blanked;
	struct NCL_fixed_databag eptr;
	struct UM_rbsplcrv_rec  *rcrv;
	struct UM_line_rec axis;
	UU_KEY_ID crv_key,axis_key;
	struct UR_attr attr1;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	status = UU_SUCCESS;
	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GREVSRF;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"REVSRF");  
	dblk.subsno = e->subscr;      /*721vp*/
	dblk.form_no = 0;

	savsub = UIO_sub_swt;
	UIO_sub_swt = 1;
/*
..... Make sure the angles are in radians.
*/
	revsrf.key = GREVSRF;
	while (e->sa < 0.) e->sa += 360.;
	while (e->ta < e->sa) e->ta += 360.;
	revsrf.sa = (3.1415926 / 180.0) * e->sa;
	revsrf.ea = (3.1415926 / 180.0) * e->ta;

	eptr.key = e->cvkey;
	ur_retrieve_data_relnum(eptr.key, &eptr.rel_num);
	rcrv = (struct UM_rbsplcrv_rec *) &eptr;
	if (uio_getdata(&eptr, &attr1))
	{
		uio_geometry(fd1,fd2,rcrv,&attr1,dcount,pcount);
		crv_key = *dcount - 2;
	}
	revsrf.crv = crv_key;

	axis.key = 0;
	axis.rel_num = UM_LINE_REL;
	strcpy(axis.label,"@UN");
	axis.subscr = 0;
	um_vctovc (e->pta, axis.spt);
	um_vcplvc (e->pta,e->vca, axis.ept);
	uio_line(fd1,fd2,&axis,&attr1,dcount,pcount);
	axis_key = *dcount - 2;
	revsrf.axis = axis_key;

	UIO_sub_swt = savsub;
/*
.....put label in properties
*/
	uio_label (fd1,fd2,e->label,dcount,pcount);
	revsrf.no_bptr = 0;
	dblk.par_ptr = *pcount;
	revsrf.no_prop = 1;
	prop[0] = *dcount - 2;
	revsrf.prop = &prop[0];

	uio_put_para(GREVSRF,&revsrf,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
/*
.....vp 1/12/98 save reference in list for any possible cv on sf
*/ 
	update_sflist (*dcount-2,e->key);

done:;

	return(status);
}  

/*********************************************************************
**    I_FUNCTION     :  uio_nclsrf (fd1,fd2,agcrv,attr,dcount,pcount)
**          Map a NCL surface to a IGES rational bspline
**          surface.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_nclsrf (fd1,fd2,e,attr,dcount,pcount)
int   fd1, fd2;
struct NCL_surface_rec  *e;
struct UR_attr    *attr;
int   *dcount;    /* directory section sequence number */
int   *pcount; /* parameter section sequence number */
{
	int status, prop[5];
	int  i, j, ku, kv, mu, mv, nu, nv, ispan, num;
	UM_int2 typ;
	UM_real8 primdata[16];
	UU_REAL pt_cc[3], pt[3], *up, *vp, u, v;
	UU_LIST slist;
	struct IG_igesrssf_rec rsplsrf;  /* IGES rational bspline surface record */
	struct  dir_rec dblk;      /* directory record */
	struct NCL_panel_rec panel;
	struct NCL_fixed_databag *crv, *tc1;
	struct UM_evsrfout evsrf;
	struct UM_rbsplsrf_rec sf;
	struct NCL_crvgen_rec *pi, *pj, *usegp, *vsegp;
	UM_transf tfp, *tfa;
	UU_LOGICAL  blanked;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	status = UU_SUCCESS;

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GRSPLSRF;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"RSPLSRF");  
/*   strcpy(dblk.label,e->label);  */
	dblk.subsno = e->subscr;     

	vp = 0;
	vsegp = 0;
	usegp = 0;
	sf.key = 0;

	dblk.form_no = 0;

	rsplsrf.key = GRSPLSRF;
	rsplsrf.no_rspara =  1;
	status = uc_retrieve_transf (e->key, tfp);

/*   Build Bsplane surf using NCL surf panels.
	  1. Allocate memory for (ispan+1) curves   */

	ispan = e->no_panelkey;
	crv = 0;
	crv   = (struct NCL_fixed_databag *)
					uu_toolmalloc((ispan+1)*sizeof(struct NCL_fixed_databag));
	if (crv == 0) status = UU_FAILURE;
	tfa   = (UM_transf *) uu_malloc ((ispan+1)*sizeof(UM_transf));
	if (tfa == 0) status = UU_FAILURE;

/*   create curve for each panel at the edge of panel */

	tc1   = crv;
	if (status == UU_SUCCESS) for (j=1; j<=ispan; j++,tc1++) tc1->key = 0;

	tc1 = crv;
	for (j=1; j<=ispan && status == UU_SUCCESS; j++)
	{
		status = gtspan (e->key, j, &panel);
		if (status==UU_SUCCESS) status = nclsrf_bspdef (0,&panel, tc1);
		tc1++;
	}

/*   create last curve at outer edge of the last panel */

	if (status==UU_SUCCESS)
	{
		status = nclsrf_bspdef (1,&panel, tc1);
		ispan = ispan + 1;
	}

	if (status==UU_SUCCESS)
	{
		status = ncl_presrf (ispan, crv, tfa, &slist);
		nu = UU_LIST_LENGTH(&slist);
		up = (UU_REAL *) UU_LIST_ARRAY(&slist);
		nv = ispan;
	}

/*   delete all curves from database & deallocate memory  */

	tc1  = crv;
	for (j=0; j<ispan; j++)
	{
		if (tc1 && tc1->key) uc_delete (tc1->key);
		tc1++;
	}
	if (crv) uu_toolfree(crv);

	if (status == UU_SUCCESS)
	{
		vsegp = (struct NCL_crvgen_rec *)
					uu_toolmalloc(nv*nu*sizeof(*vsegp));
		if (vsegp == 0) status = UU_FAILURE;
		usegp = (struct NCL_crvgen_rec *)
					uu_toolmalloc(nv*nu*sizeof(*usegp));
		if (usegp == 0) status = UU_FAILURE;
		vp    = (UU_REAL *) uu_toolmalloc (ispan*sizeof(UU_REAL));
		if (vp == 0) status = UU_FAILURE;
		pi    = usegp;
	}

	for (j=0; j<ispan && status == UU_SUCCESS; j++)
	{
		v     = (UU_REAL)(j)/(UU_REAL)(ispan-1);
		vp[j] = v;
		pj    = vsegp + j;
		for (i=0; i<nu && status == UU_SUCCESS; i++)
		{
			u = up[i];
			status = uc_evsrf (UM_FRSTDERIV,u,v,e,tfp,&evsrf);
			if (status==UU_SUCCESS)
			{
				pi->x = pj->x = evsrf.sp[0];
				pi->y = pj->y = evsrf.sp[1];
				pi->z = pj->z = evsrf.sp[2];
				um_unitvc (evsrf.dsdu, &pi->a);
				um_unitvc (evsrf.dsdv, &pj->a);
				pi->inv = pj->inv = 1;
				pi++;
				pj += ispan;
			}
		}
	}

/* create Bspline surface using points and slopes from NCL surface  */

	sf.key = 0;
	typ = -1;
	if (status == UU_SUCCESS)
		status = ncl_srfpre (nu-1,ispan-1,usegp,vsegp,up,vp,typ,primdata, &sf);

/* map UNIbase Bspline surf to IGES Bspline surf */

	rsplsrf.t1 = 0;
	rsplsrf.t2 = 0;
	rsplsrf.w  = 0;
	rsplsrf.pt3  = 0;
	rsplsrf.rspara  = 0;

	if (status == UU_SUCCESS)
	{
		ku = sf.ku;
		kv = sf.kv;
		nu = sf.nu;
		nv = sf.nv;
		mu = ku - 1;
		mv = kv - 1; 
		rsplsrf.indx1 = nu + mu - 1;
		rsplsrf.indx2 = nv + mv - 1;
		rsplsrf.degree1 = mu;
		rsplsrf.degree2 = mv;
		rsplsrf.open1 = 0;
		rsplsrf.open2 = 0;
		rsplsrf.type = 0;
		rsplsrf.period1 = 0;
		rsplsrf.period2 = 0;

		rsplsrf.no_t1 = sf.no_tu;
		rsplsrf.no_t2 = sf.no_tv;
		rsplsrf.no_w =  sf.no_wt;
		rsplsrf.no_pt3 =  sf.no_pt;

		rsplsrf.t1 = (UU_REAL *) uu_toolmalloc(rsplsrf.no_t1*sizeof(UU_REAL));
		rsplsrf.t2 = (UU_REAL *) uu_toolmalloc(rsplsrf.no_t2*sizeof(UU_REAL));
		rsplsrf.w  = (UU_REAL *) uu_toolmalloc(rsplsrf.no_w*sizeof(UU_REAL));
		rsplsrf.pt3  = (UU_REAL *) uu_toolmalloc(3*rsplsrf.no_pt3*sizeof(UU_REAL));
		rsplsrf.rspara = (struct IG_rspara_rec *)
									uu_toolmalloc(sizeof(struct IG_rspara_rec));
		if (rsplsrf.t1 == 0) status = UU_FAILURE;
		if (rsplsrf.t2 == 0) status = UU_FAILURE;
		if (rsplsrf.w == 0) status = UU_FAILURE;
		if (rsplsrf.pt3 == 0) status = UU_FAILURE;
		if (rsplsrf.rspara == 0) status = UU_FAILURE;
	}

	/* output knots */

	if (status == UU_SUCCESS)
	{
		num = sf.no_tu;
 
	/* first u direction */

		for (i=0; i<num; i++) 
		{
			rsplsrf.t1[i] = sf.tu[i];
		}

	/* now v direction */

		num = sf.no_tv;
		for (i=0; i<num; i++) 
		{
			rsplsrf.t2[i] = sf.tv[i];
		}

	/* now output weights and control points */

		num = sf.no_pt;
		j   = 0;
		for (i=0; i<num; i++) 
		{
			rsplsrf.w[i] = sf.wt[i];            /* weight */
			pt_cc[0] = sf.pt[j];
			pt_cc[1] = sf.pt[j+1];
			pt_cc[2] = sf.pt[j+2];
			UIO_CC_INTTOEXT(pt_cc,pt);
			um_vctovc(pt, &rsplsrf.pt3[j]);  /* control point   */
			j  = j + 3;
		}

/* set parameter range */

		rsplsrf.rspara[0].u0 = sf.tu[0];
		rsplsrf.rspara[0].u1 = sf.tu[sf.no_tu-1];
		rsplsrf.rspara[0].v0 = sf.tv[0];
		rsplsrf.rspara[0].v1 = sf.tv[sf.no_tv-1];
/*
.....put label in properties
*/
		uio_label (fd1,fd2,e->label,dcount,pcount);
		rsplsrf.no_bptr = 0;
		dblk.par_ptr = *pcount;
		rsplsrf.no_prop = 1;
		prop[0] = *dcount - 2;
		rsplsrf.prop = &prop[0];

		uio_put_para(GRSPLSRF,&rsplsrf,fd2,pcount,*dcount);
		uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
/*
.....vp 1/12/98 save reference in list for any possible cv on sf
*/ 
		update_sflist (*dcount-2,e->key);
	}


done:;
	if ((int)sf.key > 0) uc_delete (sf.key);
	if (vp ) uu_toolfree(vp);
	if (status == UU_SUCCESS) uu_list_free (&slist);
	if (usegp) uu_toolfree(usegp);
	if (vsegp) uu_toolfree(vsegp);
	if (rsplsrf.t1) uu_toolfree(rsplsrf.t1);
	if (rsplsrf.t2) uu_toolfree(rsplsrf.t2);
	if (rsplsrf. w) uu_toolfree(rsplsrf.w);
	if (rsplsrf.pt3) uu_toolfree(rsplsrf.pt3);
	if (rsplsrf.rspara) uu_toolfree(rsplsrf.rspara);

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int gtspan(nclkey, ispan, panel)
**       Retrieve the panel header data for the specified span (ISPAN)
**       of the surface (NCLKEY).
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the surface entity
**          ispan             span to get panel header
**       OUTPUT :
**          panel             struct to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gtspan (nclkey, ispan, panel)

UU_KEY_ID nclkey;
int  ispan;
struct NCL_panel_rec *panel;

{
	int status;
	int rel_num;
	UU_KEY_ID panelkey;

	uu_denter(UU_MTRC,(us,"gtspan(nclkey=%x, ispan=%d)",
		nclkey, ispan ));

	if (ur_retrieve_data_relnum(nclkey, &rel_num) != 0)
		status = UU_FAILURE;
	else if (rel_num != NCL_SURF_REL)
		status = UU_FAILURE;
	else if (ur_retrieve_data_varlist(nclkey, 1, &panelkey, ispan, 1) != 0)
		 status = UU_FAILURE;
	else if (panelkey == 0)
		 status = UU_FAILURE;
	else
	{
		status = UU_SUCCESS;
		panel->key = panelkey;
		if (ncl_retrieve_data_fixed(panel) != 0)
			status = UU_FAILURE;
		else if (panel->rel_num  != NCL_PANEL_REL)
			status = UU_FAILURE;
	}
	uu_dexit;
	return (status);
}
/*********************************************************************
**    E_FUNCTION     : nclsrf_bspdef (panel, crv)
**       Create B-spline curve.
**    PARAMETERS   
**       INPUT  : 
**          nenfl   - input flag to specify the edge where to make
**                    curve; 0 - inner edge of panel, 
**                           1 - outer edge of panel.
**          panel   - ncl surface panel data 
**       OUTPUT :  
**          crv     - created B-spline data structure
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclsrf_bspdef (nenfl, panel, crv)

int nenfl;
struct UM_rbsplcrv_rec *crv;
struct NCL_panel_rec *panel;

{
	int i, i3, i4, i7, nents, status;
	int npts, itsk;
	struct NCL_crvgen_rec *segp, *p2;
	struct NCL_patch_rec *p1;

	uu_denter(UU_MTRC,(us,"nclsrf_bspdef ()"));

	status = UU_SUCCESS;
	nents  = panel->no_patch;
	npts   = 0;
	if (panel->type == 0) i4 = 3; else i4 = 1;
	if (nenfl == 1)
		if (panel->type == 0) 
		{
			i3 = 2;
			i7 = 6;
		}
		else
		{
			i3 = 0;
			i7 = 2;
		}
		 
	segp   = (struct NCL_crvgen_rec *)
									uu_toolmalloc(nents*sizeof(struct NCL_crvgen_rec));
	p2     = segp;
	p1     = panel->patch;
	for (i=0; i<nents; i++)
	{
		if (nenfl == 0)
		{
			p2->x = p1->pt[0];
			p2->y = p1->pt[1];
			p2->z = p1->pt[2];
			p2->a = p1->delta[i4][0];
			p2->b = p1->delta[i4][1];
			p2->c = p1->delta[i4][2];
		}
		else
		{
			p2->x = p1->pt[0] + p1->delta[i3][0];
			p2->y = p1->pt[1] + p1->delta[i3][1];
			p2->z = p1->pt[2] + p1->delta[i3][2];
			p2->a = p1->delta[i7][0] - p1->delta[i3][0];
			p2->b = p1->delta[i7][1] - p1->delta[i3][1];
			p2->c = p1->delta[i7][2] - p1->delta[i3][2];
		}
		p2->inv = 1;
		npts++;
		p1++;
		p2++;
	}

	itsk   = 0;
	status = ncl_interp_rbsp (npts, segp, itsk, crv);
	uu_toolfree (segp);

	uu_dexitstatus("nclsrf_bspdef ()", status);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uio_mshsrf (fd1,fd2,agcrv,attr,dcount,pcount)
**          Map a UNICAD mesh surface to an IGES parametric spline
**          surface.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_mshsrf (fd1,fd2,e,attr,dcount,pcount)
int   fd1, fd2;
struct NCL_meshsf_rec  *e;
struct UR_attr    *attr;
int   *dcount;    /* directory section sequence number */
int   *pcount;    /* parameter section sequence number */
{
	struct NCL_mpatch_rec  *p1;
	int status, prop[5];
	UU_REAL a[51];
	int m, n, i, j, k, l, cx, ix, ia, num;
	struct IG_igesplsf_rec splsrf;  /* IGES pspline surface record */
	struct dir_rec dblk;            /* directory record */
	UU_LOGICAL blanked;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	status = UU_SUCCESS;

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GSPLSURF;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"SPLSURF");  
/*   strcpy(dblk.label,e->label);  */
	dblk.subsno = e->subscr;     

	dblk.form_no = 0;
	m = e->m;
	n = e->n;

	splsrf.key = GSPLSURF;
	splsrf.sub_type = 3;
	splsrf.patch_type = 0;
	splsrf.no_u_seg = m;
	splsrf.no_v_seg = n;
	splsrf.no_tu = m + 1;
	splsrf.no_tv = n + 1;
	splsrf.no_patc = 48 * (m+1) * (n+1);

/* allocate memory for variable arrays */

	splsrf.tu = (UU_REAL *) uu_toolmalloc(splsrf.no_tu*sizeof(UU_REAL));
	splsrf.tv = (UU_REAL *) uu_toolmalloc(splsrf.no_tv*sizeof(UU_REAL));
	splsrf.patc = (UU_REAL *) uu_toolmalloc(splsrf.no_patc*sizeof(UU_REAL));

	/* output knots */

	num = splsrf.no_tu;
 
	/* first u direction */

	for (i=0; i<num; i++) 
	{
		splsrf.tu[i] = i;
	}

	/* now v direction */

	num = splsrf.no_tv;

	for (i=0; i<num; i++) 
	{
		splsrf.tv[i] = i;
	}

/* now get Unibase mesh patches */

/*    cx    = 0; */
	p1   = e->mpatch;
/*   ------   FIX   ------   */
/*    num = m; */

	for (j=0; j<=n; j++)
	{
		for (i=0; i<=m; i++)
		{
			cx = (i*(n+1)+j)*48;
			if (i != m && j != n)
			{
				uio_map_mpatch (p1,a);
				ia    = 1;
/*            ix    = 0; */
				for (k=0; k<3; k++)
				{
				 	for (l=0; l<16; l++)
					{
						ix = ixa[l]+16*k;
						splsrf.patc[cx+ix] = a[ia+l];
/*                  ix++; */
					}
					ia   = ia + 17;
				}
				p1++;
			}
			else
				for (k=0; k<48; k++) splsrf.patc[cx+k] = 0.0;
/*         cx   = cx + 48; */
		}
	}
/*
.....put label in properties
*/
	uio_label (fd1,fd2,e->label,dcount,pcount);
	splsrf.no_bptr = 0;
	dblk.par_ptr = *pcount;
	splsrf.no_prop = 1;
	prop[0] = *dcount - 2;
	splsrf.prop = &prop[0];

/* Store IGES entity */

	uio_put_para(GSPLSURF,&splsrf,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
/*
.....vp 1/12/98 save reference in list for any possible cv on sf
*/ 
	update_sflist (*dcount-2,e->key);

done:;
	uu_toolfree(splsrf.tu);
	uu_toolfree(splsrf.tv);
	uu_toolfree(splsrf.patc);

	return(status);
}

/*********************************************************************
**    E_SUBROUTINE     : uio_map_mpatch (p1,a) 
**          prepare IGES psplane patch from NCL mesh patch data. 
**    PARAMETERS   
**       INPUT  : 
**          p1      - NCL patch structure 
**       OUTPUT :  
**          a[3,17] - array (3,16) of coefficients of pspline patch,
**                    where a(*,1) is not used.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
uio_map_mpatch (p1,a)
struct NCL_mpatch_rec *p1;  /*  Unibase mesh p-spline surface rec */
UU_REAL a[3][17];
{
	UU_REAL w[3], p2[17][3];
	UU_REAL pt[3], *cx;
	int i, j;

/* Get patch initial point  */

	UIO_CC_INTTOEXT(p1->pt,pt);
	p2[1][0] = pt[0];
	p2[1][1] = pt[1];
	p2[1][2] = pt[2];

/* Convert deltas of patch into points */

	cx   = p1->delta[0];
	for (i=2; i<=16; i++)
	{
		UIO_CC_INTTOEXT(cx,w);
		p2[i][0] = w[0] + pt[0];
		p2[i][1] = w[1] + pt[1];
		p2[i][2] = w[2] + pt[2];
		cx    = cx + 3;
	}

/*  Get coefficients for each point of patch  */

	for (j=0; j<3; j++)
	{
		a[j][1] = p2[1][j]; 
		a[j][2] = 3.*(p2[ 2][j]-a[j][1]);
		a[j][3] = 3.*(p2[ 3][j]-a[j][1])-a[j][2]*2.;
		a[j][4] = p2[ 4][j]-a[j][1]-a[j][2]-a[j][3];
		a[j][5] = 3.*(p2[ 5][j]-a[j][1]);
		a[j][6] = 9.*(p2[ 6][j]-p2[2][j])-a[j][5]*3.;
		a[j][7] = 9.*(p2[ 7][j]-p2[3][j])-a[j][5]*3.-a[j][6]*2.;
		a[j][8] = 3.*(p2[ 8][j]-p2[4][j])-a[j][5]-a[j][6]-a[j][7];
		a[j][9] = 3.*(p2[ 9][j]-a[j][1])-a[j][5]*2.;
		a[j][10] = 9.*(p2[10][j]-p2[2][j])-a[j][5]*6.-a[j][6]*2.-a[j][9]*3.;
		a[j][11] = 9.*(p2[11][j]-p2[3][j])-a[j][5]*6.-a[j][6]*4.-a[j][7]*2.
						 -a[j][9]*3.-a[j][10]*2.;
		a[j][12] = 3.*(p2[12][j]-p2[4][j])-2.*(a[j][5]+a[j][6]+a[j][7]
						 +a[j][8])-a[j][9]-a[j][10]-a[j][11];  
		a[j][13] = p2[13][j]-a[j][1]-a[j][5]-a[j][9];
		a[j][14] = 3.*(p2[14][j]-p2[2][j]-a[j][5]-a[j][9]-a[j][13])
						 -a[j][6]-a[j][10];
		a[j][15] = 3.*(p2[15][j]-p2[3][j]-a[j][5]-a[j][9]-a[j][13])
						 -2.*(a[j][6]+a[j][10]+a[j][14])-a[j][7]-a[j][11];
		a[j][16] = p2[16][j]-p2[4][j]-a[j][5]-a[j][6]-a[j][7]-a[j][8]-a[j][9]
						 -a[j][10]-a[j][11]-a[j][12]-a[j][13]-a[j][14]-a[j][15];
	}
done:; 

	return(UU_SUCCESS);
}
/*********************************************************************
**    I_FUNCTION     :  uio_trimsrf (fd1,fd2,tsf,attr,dcount,pcount)
**          Map a NCL trimmed surface to a IGES trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_trimsrf (fd1,fd2,tsf,attr,dcount,pcount)
int   fd1, fd2;
struct NCL_trimsf_rec  *tsf;
struct UR_attr    *attr;
int   *dcount;    /* directory section sequence number */
int   *pcount; /* parameter section sequence number */

{
	int status;
	int  i, j, num_ents, prop[5];
	struct IG_igestrsf_rec igtsf;  /* IGES trimmed surface record */
	struct  dir_rec dblk;      /* directory record */
	struct NCL_fixed_databag    e1;
	UU_KEY_ID key1, key2;
	struct UR_attr    attr1;
	UU_KEY_ID srf_ptr, *ent_ptr;
	UU_LOGICAL  blanked;
	int savsub;
	int mtrid;
	UM_transf tf;
	UU_REAL  t1[12];

	uu_denter(UU_MTRC,(us,"enter uio_trimsrf"));

if (tsf->key == 905)
{
i = 0;
}
	doing_trimsrf = UU_TRUE;
	trim_center[0] = 0.5 * (tsf->u_max + tsf->u_min);
	trim_center[1] = 0.5 * (tsf->v_max + tsf->v_min);
	status = UU_SUCCESS;

	savsub = UIO_sub_swt;
	UIO_sub_swt = 1;
	e1.key = tsf->bs_key;
	ur_retrieve_data_relnum(e1.key, &e1.rel_num);
	if(uio_getdata(&e1, &attr1)) {
		uio_geometry(fd1,fd2,&e1,&attr1,dcount,pcount);
		srf_ptr = *dcount - 2;
	}
	num_ents = tsf->no_ibndykey/2+1;
	ent_ptr = 0;
	ent_ptr = (UU_KEY_ID *) uu_toolmalloc((num_ents+1)*sizeof(UU_KEY_ID));
	j = 0;
	key1 = tsf->cv_key;
	key2 = tsf->uv_key;
	for(i=0;i<num_ents;i++)
	{
		uio_cvonsf (fd1,fd2,tsf,srf_ptr,key1,key2,attr,dcount,pcount);
		ent_ptr[i] = *dcount - 2;
		if (i<num_ents-1)
		{
			key1 = tsf->ibndykey[j++];
			key2 = tsf->ibndykey[j++];
		}
	}

	UIO_sub_swt = savsub;

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, tsf->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(tsf->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GTRIMSRF;
/*
.....Comment out for FSR61234
	if (tsf->offdist != 0.0)
		dblk.sub_swt = 1;
	else
		dblk.sub_swt = UIO_sub_swt;
*/
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"TRIMSRF");
	dblk.subsno = tsf->subscr;
	dblk.matrix_ptr = 0;
	status = uc_retrieve_transf (tsf->key, tf);
	if ((mtrid=uio_chk_matrix(tf,*dcount))<0)
	{
		uio_mtrtomtr(tf,t1);    /* Convert Unicad matrix to iges matrix */
		dblk.matrix_ptr = *dcount;
		uio_tran(fd1,fd2,t1,dcount,pcount);
	}
	else     /* Matrix exists in IGES file, point the pointer to it */
	{
		dblk.matrix_ptr = mtrid;
	}

	dblk.par_ptr = *pcount;
	dblk.form_no = 0;

	igtsf.key = GTRIMSRF;
	igtsf.srf = srf_ptr;
	igtsf.n1 = 1;
	igtsf.n2 = num_ents-1;
	igtsf.crv = ent_ptr[0];
	igtsf.no_cid = num_ents-1;
	igtsf.cid = &ent_ptr[1];
/*
.....put label in properties
*/
/*
.....Comment out for FSR61234
	if (tsf->offdist != 0.0) strcpy(tsf->label,"@UN");
*/
	uio_label (fd1,fd2,tsf->label,dcount,pcount);
	igtsf.no_bptr = 0;
	dblk.par_ptr = *pcount;
	igtsf.no_prop = 1;
	prop[0] = *dcount - 2;
	igtsf.prop = &prop[0];

	uio_put_para(GTRIMSRF,&igtsf,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
/*
.....vp 1/12/98 save reference in list for any possible cv on sf
*/ 
	update_sflist (*dcount-2,tsf->key);
/*
.....Comment out for FSR61234
	if (tsf->offdist != 0.0)
		uio_offset_srf(dblk, tsf->offdist, tsf->key, fd1, fd2, dcount, pcount);
*/
	if (ent_ptr) uu_toolfree(ent_ptr);

	doing_trimsrf = UU_FALSE;
	uu_dexit;
	return (status);
}  
/*********************************************************************
**    I_FUNCTION     :  uio_cvonsf (fd1,fd2,tsf,srf,key1,key2,attr,dcount,pcount)
**          Create an IGES curve on a surface from the trimming curves of a
**          NCL trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_cvonsf (fd1,fd2,tsf,srf,key1,key2,attr,dcount,pcount)
int   fd1, fd2;
struct NCL_trimsf_rec  *tsf;
UU_KEY_ID srf, key1, key2;
struct UR_attr    *attr;
int   *dcount;    /* directory section sequence number */
int   *pcount; /* parameter section sequence number */

{
	int  status;
	struct IG_igescvsf_rec igcvsf;  /* IGES curve on surface record */
	struct  dir_rec dblk;      /* directory record */
	struct NCL_fixed_databag e,e1, bsf;
	struct UR_attr  attr1;
	UM_transf tf, tfmat;
	struct UM_rbsplsrf_rec *rsf;
	struct NCL_revsurf_rec *vsf;
	struct UM_circle_rec *circle;
	UU_LOGICAL  blanked, store = UU_TRUE;
	int savsub, savuse, prop[4];
	UU_REAL tol, savunit,sa,ea,ns,ne,dang,rang,scl;
	char label[NCL_MAX_LABEL];
	UU_KEY_ID cvkey;
	UM_real8 tol8;

	uu_denter(UU_MTRC,(us,"enter uio_cvonsf"));

	status = UU_SUCCESS;

	UIO_uvcurve = 0;
	savsub = UIO_sub_swt;
	UIO_sub_swt = 1;
/*
.....If the curve does not have a xyz component we create one.
*/
	if (key1 == 0)
	{
   	gettol (&tol8);
   	tol = tol8;
   	status = uc_retrieve_transf (tsf->key, tfmat);
   	status = uio_uvcrv_to_crv (tsf,tfmat,0,tol,&cvkey,0);
   	if (status == 0) key1 = cvkey;
		else
			igcvsf.crv = 0;
	}
	if (key1 !=0)
	{
		e1.key = key1;
		ur_retrieve_data_relnum(e1.key, &e1.rel_num);
		if(uio_getdata(&e1, &attr1))
		{
			strcpy (label,e1.label);
			uio_geometry(fd1,fd2,&e1,&attr1,dcount,pcount);
			igcvsf.crv = *dcount - 2;
		}
	}
	savuse = UIO_use_flg;
	UIO_use_flg = 5;
	if (key2 == 0)
	{
		igcvsf.b_ptr = 0;
	}
	else
	{
		e1.key = key2;
		ur_retrieve_data_relnum(e1.key, &e1.rel_num);
		if(uio_getdata(&e1, &attr1)) 
		{
			strcpy (label,e1.label);
			bsf.key = tsf->bs_key;
			status = ncl_retrieve_data_fixed (&bsf);
			rsf = (struct UM_rbsplsrf_rec *)&bsf;
			vsf = (struct NCL_revsurf_rec *)&bsf;
/*
.....Surfaces of revolution parameterization
.....in IGES is based on the starting and ending angles
.....and is parameterized in radians
*/
			if (bsf.rel_num == NCL_REVSURF_REL)
			{
				um_identtf(tf);
/*
........If genetrix is a circle then
........adjust U-parameterization
*/
				e.key = vsf->cvkey;
				ncl_retrieve_data_fixed(&e);
				if (e.rel_num == UM_CIRCLE_REL)
				{
					circle = (struct UM_circle_rec *)&e;
					sa = tsf->u_min; ea = tsf->u_max;
					ns = 0.; ne = circle->dang;
					rang = ea - sa;
					dang = ne - ns;
					scl = dang/rang;
					tf[0][0] = scl;
					tf[3][0] = ns;
				}
/*
........Adjust V-parameterization
........for revolved surface angles
*/
				sa = tsf->v_min; ea = tsf->v_max;
				ns = vsf->sa / UM_RADIAN; ne = vsf->ta / UM_RADIAN;
				while (ns < 0.) ns += 360.;
				while (ne <= ns) ne += 360.;
				rang = ea - sa;
				dang = ne - ns;
				scl = dang/rang;
				tf[1][1] = scl;
				tf[3][1] = ns;
				status = uig_transform (&e1, tf, store);
			}
			else if ((bsf.rel_num == NCL_MESHSURF_REL) ||
				 (bsf.rel_num == UM_RBSPLSRF_REL &&
				  rsf->tu[0] == 0.0 && rsf->tu[rsf->no_tu-1] == 1.0 &&
				  rsf->tv[0] == 0.0 && rsf->tv[rsf->no_tv-1] == 1.0))
			{
				um_identtf (tf);
			   if (bsf.rel_num != NCL_REVSURF_REL)
				{
					tf[0][0] = 1.0/(tsf->u_max-tsf->u_min);
					tf[1][1] = 1.0/(tsf->v_max-tsf->v_min);
					tf[3][1] = -tsf->v_min*tf[1][1];
				}
				tf[3][0] = -tsf->u_min*tf[0][0];
				status = uig_transform (&e1, tf, store);
			}
			savunit = uio_units;
			uio_units = 1.0;
/*
.....vp 12/10/96 
.....when unicad uv curve is polyline map to iges polyline type 1 (2D) 
*/
			UIO_uvcurve = 1;
			bump_u = bump_v = UU_TRUE;
			uio_geometry(fd1,fd2,&e1,&attr1,dcount,pcount);
			bump_u = bump_v = UU_FALSE;
			uio_units = savunit ;
			UIO_uvcurve = 0;
			igcvsf.b_ptr = *dcount - 2;

		}
	}
	UIO_sub_swt = savsub;
	UIO_use_flg = savuse;

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, tsf->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(tsf->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GCRVONSRF;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
	dblk.use_flg = 0;
	strcpy(dblk.label,"CRVONSRF");
/*   strcpy(dblk.label,tsf->label);   */
	dblk.subsno = tsf->subscr;

	dblk.form_no = 0;

	igcvsf.key = GCRVONSRF;
	igcvsf.crtn = 0;
	igcvsf.srf = srf;
	igcvsf.pref = 1;
/*
.....put label in properties
*/
	uio_label (fd1,fd2,label,dcount,pcount);
	igcvsf.no_bptr = 0;
	dblk.par_ptr = *pcount;
	igcvsf.no_prop = 1;
	prop[0] = *dcount - 2;
	igcvsf.prop = &prop[0];

	uio_put_para(GCRVONSRF,&igcvsf,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);

	uu_dexit;
	return (status);
}  
/*********************************************************************
**    I_FUNCTION     :  update_counts(key,irec)
**          Update translated surfaces list.
**    PARAMETERS
**       INPUT  :
**          key:        key of translated surface.
**     	  irec:       directory record number.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
update_sflist(irec, key)
int irec;
UU_KEY_ID key;
{
	surf_list rec;

	rec.bskey = key;
	rec.drec = irec;

	uu_list_push (UIO_surf_list,&rec);

	return(UU_SUCCESS);
}


/*********************************************************************
**    I_FUNCTION     :  uio_offset_srf(base_dir_rec, of_dist, base_sf_key, \
**                                      fd1, fd2, dcount, pcount)
**         Translates offset surfaces from Unibase to IGES. 
**    PARAMETERS
**       INPUT  :
**          base_dir_rec:   diretory entry for the base surfaces
**          of_dist:        distance to offset the surface.
**          base_sf_key:    Unibase key to base surface.
**                             Used to evaluate surface normal.
**                             Needed to give orientation to of_dist
**          fd1:            temporary file for directory entries
**          fd2:            temporary file for parameter entries
**          dcount:         pointer to directory sequence number
**          pcount:         pointer to parameter sequence number
**       OUTPUT :
**          dcount:         pointer to next directory entry
**          pcount:         pointer to next parameter entry
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_offset_srf(base_dir_rec, of_dist, base_sf_key, fd1, fd2, dcount, pcount)
struct  dir_rec base_dir_rec;      /* directory record of base surface */
UU_REAL of_dist;
UU_KEY_ID base_sf_key;
int fd1, fd2;
int *dcount;        /* directory section sequence number */
int *pcount;    /* parameter section sequence number */
{
	struct  dir_rec dblk;      /* directory record */
	struct IG_igesofsf_rec ig_off_srf;  /* IGES offset surface record */
	int status, prop[5];
	struct NCL_fixed_databag srf;
	UM_param u, v;
	struct UM_evsrfout evsrf;
	UM_transf tfmat;
	UM_vector base_sf_norm, trim_norm, surf_norm;
	UU_REAL flip_dir;

	srf.key = base_sf_key;
	status = ncl_retrieve_data_fixed (&srf);
    uc_init_evsrfout (&srf, &evsrf);
    status = uc_retrieve_transf(srf.key,tfmat);
	if (doing_trimsrf == UU_TRUE)
	{
		u = trim_center[0]; v = trim_center[1];
		status = uc_evsrf(UM_NORM, u, v, &srf, tfmat, &evsrf);
		um_vctovc (evsrf.snorm, trim_norm);	
		
		u = 0.5; v = 0.5;
		status = uc_evsrf(UM_NORM, u, v, &srf, tfmat, &evsrf);
		um_vctovc (evsrf.snorm, base_sf_norm);	

		if (um_dot(base_sf_norm, trim_norm) < 0.0)
			flip_dir = -1.0;
		else
			flip_dir = 1.0;
		
		um_vctovc(trim_norm, surf_norm);	
	}
	else
	{
		u = 0.5; v = 0.5;
		status = uc_evsrf(UM_NORM, u, v, &srf, tfmat, &evsrf);
		um_vctovc (evsrf.snorm, surf_norm);	
		flip_dir = 1.0;
	}	
	of_dist *= flip_dir;

    ig_off_srf.key = GOFFSTSRF;            /* entity type */
	ig_off_srf.rel_num = GOFFSTSRF;
	UIO_LEN_INTTOEXT(of_dist, ig_off_srf.of_dist);
	UIO_CC_INTTOEXT(surf_norm, ig_off_srf.of_vec);
/*
..... Must set the surface pointer in the parameter block.
..... The base surface for this offset sf was called immediately
..... before this function.  Therefore, the directory entry for
..... the base surface is (*dcount) - 2.  The '- 2' part comes 
..... from the fact that each entity is processed and then a 
..... property entity is created.  That takes 2 lines in the 
..... directory section.
*/
	ig_off_srf.srf = (*dcount) - 2;


	dblk.rel_type = GOFFSTSRF;
	dblk.par_ptr = *pcount;
	dblk.line_font = base_dir_rec.line_font;
	dblk.level = base_dir_rec.level;
	dblk.view_ptr = base_dir_rec.view_ptr;
	dblk.matrix_ptr = 0;
	dblk.blank = base_dir_rec.blank;
/* 
..... Base surf is now physically dependent (sub_swt = 1) and
..... this offset surface gets the UIO_sub_swt.  If base surface
..... was independent entity bafore, now the offset surf is
..... independent, and the base surf is dependent.  If base
..... was dependent (eg. base of a trimmed surface), now 
..... offset surface is dependent.
*/ 
	dblk.sub_swt = UIO_sub_swt;
	dblk.use_flg = base_dir_rec.use_flg;
	dblk.hier = base_dir_rec.hier;
	dblk.line_wt = base_dir_rec.line_wt;
	dblk.pen_no = base_dir_rec.pen_no;
	dblk.par_cnt = 1;
	dblk.form_no = 0;
	strcpy(dblk.label,"OFFSTSF");
/*
..... Subscript should be distinct from base surface, yet meaningful.
..... Don't know how to do it yet, so use a random number.
*/
	dblk.subsno = *dcount;
/*
.....put label in properties
*/
    uio_label (fd1,fd2,srf.label,dcount,pcount);
    ig_off_srf.no_bptr = 0;
    dblk.par_ptr = *pcount;
    ig_off_srf.no_prop = 1;
    prop[0] = *dcount - 2;
    ig_off_srf.prop = &prop[0];

/*
..... Create the parameter entry in its file, fd2.
..... The parameter sequence number starts off pointing to the
..... parameter, but returns pointing to the next line in the 
..... parameter section.  Since the parameter section should 
..... only be one line for an offset surface (pointer to directory
..... entry for base surface, and 4 numbers -> definitely should
..... fit on one line), the returned pcount should be +1 from the
..... input pcount.
*/ 
	uio_put_para(GOFFSTSRF, &ig_off_srf, fd2, pcount, *dcount);
/*
..... Puts the directory entry in its file, fd1.
..... Set up the directory entry.  Use the parameter sequence
..... number from BEFORE calling uio_put_para; after that function
..... is called, the sequence number points at the parameter FOLLOWING
..... the offset surface.
*/
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);

	return(status);
}
