/*********************************************************************
**   NAME:  nefmill1.c
**    CONTAINS:
**      ncl_copy_pos (posa,posb)
**      ncl_fm_ptstor (pte,iret)
**      ncl_fm_ptget (ix,pte,iret)
**      ncl_fm_chkpts (kcs4,npts4,tol8,thk8,ier)
**      ncl_fm_csdis1 (pos,cpl,tool)
**      ncl_fm_csdis (lproj,init,isf,ier,tol8,cpos,cpl,tool)
**      ncl_interp (lproj,isf,ier,tol8,posa,posb,posc,cpl,tool,tol)
**      ncl_toolparams (tool)
**		ncl_copy_cspln()
**      ncl_fm_csdis0 ()
**      ncl_fm_csdismin ()
**		ncl_fm_chkpts1()
**		ncl_fmptlst_copy()
**		ncl_fmptlst_free()
**		ncl_fmptlst_paste()
**
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nefmill1.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       04/05/18 , 14:54:57
*********************************************************************/

#include "ncldef.h"
#include "modef.h"
#include "mgeom.h"
#include "mdeval.h"
#include "nccs.h"
#include "nclfc.h"
#include "uminmax.h"
#include "nclpsmult.h"

static UU_LIST Stoolpts;
static int Stoolpt_init = 0;
static UU_LOGICAL Slsurf = UU_FALSE;
static UM_cylinder Scyl;
static int Sncsf = 1;
static UU_REAL Sthk[40];
static UU_REAL Stol;
static UU_REAL Skcs[40];
static UU_LIST Sfmptlst;

typedef struct
{
	UM_coord pte;
	UM_vector vta;
	UM_real8 up;
	UM_real8 vp;
	UM_real4 uvh[3];
	UU_REAL dis;
	UU_REAL ptdis;
	int iret;
	int icsf;
} toolpt;

typedef struct
{
	UU_LOGICAL lproj;
	UM_int2 isf;
	UM_int2 init;
	UM_real8 tol;
	UM_coord pt;
	UM_vector ve;
	UU_REAL thk;
} cspln;

typedef struct
{
	UU_REAL rad;
	UU_REAL crad;
	UU_REAL height;
	UU_REAL cute;
	UU_REAL cutn;
	UU_REAL sbe;
	UU_REAL cbe;
} cutter;

#define MAX_INTERP 25
#define MAX_ITER 25

/*********************************************************************
**       Free the list Stoolpts. Called from ncl_fmfin.
*********************************************************************/
void ncl_free_toolpts()
{
	if (Stoolpt_init == 1)
	{
		uu_list_free(&Stoolpts);
		Stoolpt_init = 0;
	}
}

/*********************************************************************
**       Debug routine
*********************************************************************/
static void S_print_csdis (pos,cpl)
toolpt *pos;
cspln *cpl;
{
	printf ("\n");
	ncl_print_gt (pos->pte,pos->vta);
	printf ("%%");
	ncl_print_pv (pos->pte,pos->vta);

	printf ("%% dis %g \n",pos->dis);
/*	printf ("%% ptdis %g \n",pos->ptdis); */
/*
	printf ("%%");
	ncl_print_pv (cpl->pt,cpl->ve);
*/
}
	
/*********************************************************************
**    E_FUNCTION     : ncl_copy_cspln (cspla,csplb)
**       Copy all fields from cspla to csplb.
*********************************************************************/
void ncl_copy_cspln (cspla,csplb)
cspln *cspla,*csplb;
{
	int k;
	for (k = 0; k < 3; k++)
	{	
		csplb->pt[k] = cspla->pt[k];
		csplb->ve[k] = cspla->ve[k];
	}
	csplb->lproj = cspla->lproj;
	csplb->isf = cspla->isf;
	csplb->init = cspla->init;
	csplb->tol = cspla->tol;
	csplb->thk = cspla->thk;
}

/*********************************************************************
**    E_FUNCTION     : ncl_copy_pos (posa,posb)
**       Copy all fields from posa to posb.
*********************************************************************/
void ncl_copy_pos (posa,posb)
toolpt *posa,*posb;
{
	int k;

	for (k = 0; k < 3; k++)
	{	
		posb->pte[k] = posa->pte[k];
		posb->vta[k] = posa->vta[k];
		posb->uvh[k] = posa->uvh[k];
	}
	posb->up = posa->up;
	posb->vp = posa->vp;
	posb->iret = posa->iret;
	posb->dis = posa->dis;
	posb->ptdis = posa->ptdis;
	posb->icsf = posa->icsf;
}

/*********************************************************************
*********************************************************************/
static void S_udir(vec,dvec)
UM_vector vec;
UU_REAL *dvec;
{
	UU_REAL d;

	*dvec = 0;
	d = UM_DOT (vec,vec);
	if (d < UM_DFUZZ) return;

	d = sqrt (d);
	vec[0] /= d;
	vec[1] /= d;
	vec[2] /= d;
	*dvec = d;
}

/*********************************************************************
**    E_FUNCTION     : ncl_fm_ptstor (pte,u,v,iret)
**       Store FMILL tool position into the list.
**    PARAMETERS
**       INPUT  :
**             pte    - tool end, tool axis
**             u,v    - Part Surface parameters
**             iret   - retract flag
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fm_ptstor (pte,u,v,iret)
UM_real8 pte[6],*u,*v;
UM_int2 *iret;
{
	int k,npts;
	toolpt pos;

	if (Stoolpt_init == 0)
	{
		npts = ncl_fml_uvlist_num();
		uu_list_init (&Stoolpts,sizeof(toolpt),npts,npts);
		Stoolpt_init = 1;
	}

	for (k = 0; k < 3; k++)
	{
		pos.pte[k] = pte[k];
		pos.vta[k] = pte[k+3];
		pos.uvh[k] = -1;
	}
	pos.up = *u; pos.vp = *v;
	pos.iret = *iret;
	pos.dis = pos.ptdis = 1.e+9;
	pos.icsf = 0;

	uu_list_push (&Stoolpts,&pos);

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_fm_ptget (ix,pte,iret)
**       Get FMILL tool position from the list.
**    PARAMETERS
**       INPUT  :
**             pte    - tool end, tool axis
**             iret   - retract flag
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fm_ptget (ix,pte,iret)
UM_int4 *ix;
UM_real8 pte[6];
UM_int2 *iret;
{
	int k;
	toolpt *pos;
	int i = *ix-1;

	*iret = -1;

	if (Stoolpt_init == 0 || i >= Stoolpts.cur_cnt)
		return;

	pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);
	pos += i;

	for (k = 0; k < 3; k++)
	{
		pte[k] = pos->pte[k];
		pte[k+3] = pos->vta[k];
	}
	*iret = pos->iret;

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_fm_csdis1 (pos,cpl,tool)
**       Calculate the distance from a tool position to a plane
**    PARAMETERS
**       INPUT  :
**             pos     - tool position
**             tool    - tool structure
**             cpl     - plane
**       OUTPUT :
**             pos.dis    -  distance calculated
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fm_csdis1 (pos,cpl,tool)
cutter *tool;
cspln *cpl;
toolpt *pos;
{
	UM_vector vec;
	UU_REAL d,ted,sal,cal,dang,h,hc,dc;

/*
..... the distance is calculated as in csrel
*/
	um_vcmnvc (cpl->pt,pos->pte,vec);
	d = UM_DOT (vec,cpl->ve);
	ted = d - cpl->thk;

	sal = UM_DOT (pos->vta,cpl->ve);
	cal = 1 - sal*sal;
	if (cal > 0)
		cal = sqrt (cal);
	else
		cal = 0;

	dang = tool->sbe + sal;
	if (fabs(dang) < 0.001)
		pos->dis = ted - tool->cutn;
	else if (dang < 0)
		pos->dis = ted - (tool->crad + tool->cute*cal) - tool->crad*sal;
	else
		pos->dis = ted - tool->rad*cal - tool->height*sal;

	if (Slsurf)
	{
		h = UM_DOT (vec,pos->vta);
		d = UM_DOT (vec,vec);
		d = sqrt (d - h*h);
		if (d!=d)	// Sasha, Dec.06, 2017, due to bad angle between vec and pos-> vta d is negative
		{
			return;
		}

		if (h > tool->height)
		{
			hc = h - tool->height;
			dc = d - tool->rad;

			if (dc <= 0)
				pos->ptdis = hc;
			else
				pos->ptdis = sqrt (hc*hc + dc*dc);
		}
		else
		{
			hc = tool->crad - h;
			dc = d - tool->cute;

			if (hc > 0)
			{
				if (dc <= 0)
					pos->ptdis = -h;
				else
					pos->ptdis = sqrt (hc*hc + dc*dc) - tool->crad;
			}
			else
				pos->ptdis = d - tool->rad;
		}
		pos->ptdis -= cpl->thk;

		pos->dis = pos->ptdis;
	}
	else
		pos->ptdis = pos->dis;

	return;
}
		
/*********************************************************************
**    E_FUNCTION     : ncl_fm_csdis (cpos,cpl,tool,ier)
**       Project tool to the Check Surface, calculate the distance.
**       INPUT  :
**             cpos    - tool position
**             tool    - tool structure
**             cpl     - CS structure
**       OUTPUT :
**             cpl        - CS structure with current CS plane
**             cpos.dis   -  distance calculated
**             ier        -  error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fm_csdis (cpos,cpl,tool,ier)
toolpt *cpos;
cspln *cpl;
cutter *tool;
UM_int2 *ier;
{
	int k;
	UM_real4 te[3],ta[3],cpt[3],cve[3],cuv[3],side;

	if (cpl->lproj)
	{
		for (k = 0; k < 3; k++)
		{
			te[k] = cpos->pte[k];
			ta[k] = cpos->vta[k];
			cuv[k] = cpos->uvh[k];
		}
/*
..... project the tool to the CS entity
*/
		fmilcs (te,ta,cpt,cve,cuv,&cpl->tol,&cpl->isf,&cpl->init,&side,ier);
		if (*ier != 0) return;

		for (k = 0; k < 3; k++)
		{
			cpl->pt[k] = cpt[k];
			cpl->ve[k] = cve[k];
			cpos->uvh[k] = cuv[k];
		}
	}
/*
..... calculate the tool distance to the projection plane
*/
	ncl_fm_csdis1 (cpos,cpl,tool);
	if (cpos->dis!=cpos->dis)	//Sasha, Dec06-07, 2017, dis==NaN
		{
			*ier = 11;
		}

}

/*********************************************************************
**    E_FUNCTION     : ncl_fm_csdis0 (cpos,cpl,tool,ier)
**       Project tool to the cpos->icsf Check Surface, calculate the distance.
**       INPUT  :
**             cpos    - tool position with icsf check surface
**             tool    - tool structure
**             cpl     - CS structure
**       OUTPUT :
**             cpl        - CS structure with current CS plane
**             cpos.dis   -  distance calculated
**             ier        -  error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fm_csdis0(cpos,cpl,tool,ier)
toolpt *cpos;
cspln *cpl;
cutter *tool;
UM_int2 *ier;
{
	int icsf,i,j,k,l,ncsf,status;
	struct NCL_fixed_databag eptr;
	struct NCL_nclpl_rec *pln;
	UU_LOGICAL lfintype;
	int iext;

	UM_int2 rn;
	UM_vector vec;
	UU_REAL d, tol;
	UU_KEY_ID cskey;
	UU_REAL csasw;
	cspln cspl;
	UU_REAL dis;
	UM_int2 nwds, ietype;

/*
..... initialize CS entity
*/
	cspl.tol = Stol;

	icsf = cpos->icsf;
	cspl.thk = Sthk[icsf-1];
	csasw = Skcs[icsf-1];

	gtdesc(&csasw,&cskey,&nwds,&ietype);

	eptr.key = cskey;
	seticsf(&icsf);

	status = ncl_retrieve_data_fixed(&eptr);
	if (status != UU_SUCCESS)
		goto done;

	ncl_get_type(eptr.rel_num,&rn);
	Slsurf = (rn == NCLI_SURF);
	cspl.lproj = (rn != NCLI_PLANE);

	if (cspl.lproj)
	{
		cspl.isf = 2;
		status = evstup1 (&cskey,&cspl.isf);
		if (status != UU_SUCCESS)
			goto done;
	}
	else
	{
		pln = (struct NCL_nclpl_rec *)&eptr;
		um_vctovc (pln->pt,cspl.pt);
		um_unitvc(pln->nvec,cspl.ve);
	}

	cspl.init = 1;
	if (cspl.lproj)
	{
		cpos->uvh[0] = 0.5;
		cpos->uvh[1] = 0.5;
		cpos->uvh[2] = 0.5*(tool->crad+tool->height);
	}
	else
	{
/*
..... plane normal should point away from the tool
*/
		um_vcmnvc (cspl.pt,cpos->pte,vec);
		d = UM_DOT (vec,cspl.ve);
		if (d < 0)
			um_negvc (cspl.ve,cspl.ve);
	}
/*
..... compute CS distance 
*/
	ncl_fm_csdis (cpos,&cspl,tool,ier);
	if (*ier != 0) 
		return;
	cspl.init = 0;

	ncl_copy_cspln(&cspl,cpl);

done:
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_fm_csdismin (cpos,cpl,tool,ier)
**       Project tool to all Check Surfaces, calculate the minmum distance.
**       INPUT  :
**             cpos    - tool position 
**             tool    - tool structure
**             cpl     - CS structure
**       OUTPUT :
**             cpl        - CS structure with current CS plane
**             cpos.dis   -  distance calculated
**             ier        -  error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fm_csdismin (cpos,cpl,tool,ier)
toolpt *cpos;
cspln *cpl;
cutter *tool;
UM_int2 *ier;
{
	int icsf,i,j,k,l,ncsf,icsf_min,status;
	struct NCL_fixed_databag eptr;
	struct NCL_nclpl_rec *pln;
	UU_LOGICAL lfintype;
	int iext;

	int irot,itsk,iprcnt,insf,ier1,insf1;
	UM_coord pt,uv;
	UM_srf_boundary bndr;
	UU_LIST cvlst,uvlst;
	UU_REAL u1,v1;
	UM_transf tfmat;
	NCL_surf_struct s1;

	UM_int2 rn,iret;
	UM_vector vec;
	UU_REAL d, tol,tol10,dissq,toltk;
	UU_KEY_ID cskey;
	UU_REAL csasw;
	cspln cspl,cspl_min;
	UU_REAL dis, dis_min = 1000.0,ptdis_min = 1000.0;
	UM_int2 nwds, ietype;
	iret = -1;

/*
.....Initialize sf boundary
*/	
	tol = Stol;
	ncl_set_boundary_toler (tol);
	um_init_boundary (&bndr);
	uu_list_init (&cvlst, sizeof(UM_coord), 200, 200);
	uu_list_init (&uvlst, sizeof(UM_coord), 200, 200);
	bndr.uvpts = &uvlst;
	bndr.cvpts = &cvlst;
/*
..... initialize CS entity
*/
	cspl.tol = Stol;
	tol10 = 0.1 * tool->crad;
	toltk = tool->crad;

	for (icsf = 1; icsf <= Sncsf; icsf++)
	{
		cspl.thk = Sthk[icsf-1];
		csasw = Skcs[icsf-1];

		gtdesc(&csasw,&cskey,&nwds,&ietype);

		eptr.key = cskey;
		
		seticsf(&icsf);

		status = ncl_retrieve_data_fixed(&eptr);
		if (status != UU_SUCCESS)
			goto done;
	
		ncl_get_type(eptr.rel_num,&rn);
		Slsurf = (rn == NCLI_SURF);
		cspl.lproj = (rn != NCLI_PLANE);

		if (status == UU_SUCCESS)
		{
			status = uc_retrieve_transf (eptr.key, tfmat);

			ncl_free_bndry (&bndr);
			UU_LIST_EMPTY (bndr.uvpts);
			UU_LIST_EMPTY (bndr.cvpts);
			status = ncl_get_bndry (&eptr,tfmat,&bndr,tol,UU_TRUE);
			if (bndr.nb < 1) status = UU_FAILURE;
		}

		if (cspl.lproj)
		{
			cspl.isf = 2;
			status = evstup1 (&cskey,&cspl.isf);
			if (status != UU_SUCCESS)
				goto done;
		}
		else
		{
			pln = (struct NCL_nclpl_rec *)&eptr;
			um_vctovc (pln->pt,cspl.pt);
			um_unitvc(pln->nvec,cspl.ve);
		}

		cspl.init = 1;
		if (cspl.lproj)
		{
			cpos->uvh[0] = 0.5;
			cpos->uvh[1] = 0.5;
			cpos->uvh[2] = 0.5*(tool->crad+tool->height);
		}
		else
		{
/*
..... plane normal should point away from the tool
*/
			um_vcmnvc (cspl.pt,cpos->pte,vec);
			d = UM_DOT (vec,cspl.ve);
			if (d < 0)
				um_negvc (cspl.ve,cspl.ve);
		}
/*
..... compute CS distance 
*/
		ncl_fm_csdis (cpos,&cspl,tool,ier);
		if (*ier != 0) 
			return;
		cspl.init = 0;
/*
.....Check if cpos inside the trimmed surface.
*/
		iret = 0;
		if (cpos->dis < tool->crad + cspl.thk)	
		{
			itsk = 0;
			iprcnt= 0;
			u1 = cpos->uvh[0];
			v1 = cpos->uvh[1];
			ncl_pt_on_sf(&itsk,&eptr.key,&iprcnt,&u1,&v1,&pt,&ier1);

			uv[0] = u1;
			uv[1] = v1;
			insf = um_inside_bndry (uv,pt,&bndr,&tol);

			dissq = um_sqdis(pt,cspl.pt);
			if (dissq > toltk * toltk)
				insf = -1;

			dissq = 1000;
			if (insf == -1)
			{
				iret = -1;
				status = ncl_proj_on_srf_bndry (pt,&eptr,&bndr,&s1,UU_TRUE);
				dissq = um_sqdis(s1.pt,cspl.pt);
				toltk = toltk * (1-fabs(UM_DOT(cpos->vta,s1.normal)));
			}
			if(iret == -1 && dissq < (toltk + tol10) * (toltk + tol10))
				iret = 0;
		}

		if (cpos->dis < dis_min && iret == 0)
		{
			dis_min = cpos->dis;
			ptdis_min = cpos->ptdis;
			icsf_min = icsf;
			ncl_copy_cspln(&cspl,&cspl_min);
		}
	}

	cpos->dis = dis_min;
	cpos->ptdis = ptdis_min;
	cpos->icsf = icsf_min;
	ncl_copy_cspln(&cspl_min,cpl);

done:

/*
.....Free memory allocated
*/
	ncl_free_bndry (&bndr);
	uu_list_free (&uvlst);
	uu_list_free (&cvlst);

	return;
}

/*********************************************************************
**    E_FUNCTION     : S_interp_dis (a,b,posa,posb,cpos,cpl,tool,ier)
**       Interpolate between two tool positions in the a:(b-a) ratio.
**       Project new tool position 
**    PARAMETERS
**       INPUT  :
**             a,b       - interpolation parameters
**             posa,posb - original tool positions.
**             tool      - tool structure
**             cpl       - CS structure
**       OUTPUT :
**             cpos      -  interpolated position
**             cpl       -  CS projection plane for cpos
**             ier       -  error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_interp_dis (a,b,posa,posb,cpos,cpl,tool,ier)
toolpt *posa,*posb,*cpos;
cspln *cpl;
cutter *tool;
UU_REAL a,b;
UM_int2 *ier;
{
	int k;
	UU_REAL s,t;

	if (b < UM_DFUZZ)
	{
		*ier = 1111;
		return;
	}

	t = a / b;
	s = 1-t;

	for (k = 0; k < 3; k++)
	{
		cpos->pte[k] = s*posa->pte[k] + t*posb->pte[k];
		cpos->vta[k] = s*posa->vta[k] + t*posb->vta[k];
		cpos->up = s*posa->up + t*posb->up;
		cpos->vp = s*posa->vp + t*posb->vp;
		if (cpl->lproj)
			cpos->uvh[k] = s*posa->uvh[k] + t*posb->uvh[k];
	}
	um_unitvc (cpos->vta,cpos->vta);
	cpos->iret = 0;
/*
..... Project tool to the Check Surace, calculate the distance.
*/
	if (posa->icsf == posb->icsf)
	{
		cpos->icsf = posa->icsf;
		if (Sncsf == 1 || posa->icsf == 0)
			ncl_fm_csdis (cpos,cpl,tool,ier);
		else
			ncl_fm_csdis0 (cpos,cpl,tool,ier);
	}
	else
	{
/*
..... Project tool to the closest Check Surface
*/
		ncl_fm_csdismin (cpos,cpl,tool,ier);
	}
}

/*********************************************************************
**    E_FUNCTION     : S_move_ab1 (itsk,cpos,sn,pcon,cpl,da,db)
**       Calculate a repositioning move to the intersection of PS and CS planes
**    PARAMETERS
**       INPUT  :
**             itsk        - reposition the tool iff 1
**             cpos        - original tool position
**             sn          - PS normal
**             cpl         - CS structure
**             pcon        - PS distance
**       OUTPUT :
**             cpos        - new tool position
**             da          - repositioning move alond PS normal
**             db          - repositioning move alond CS normal
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_move_ab1 (itsk,cpos,sn,pcon,cpl,da,db)
int itsk;
toolpt *cpos;
UM_vector sn;
cspln *cpl;
UU_REAL pcon,*da,*db;
{
	UU_REAL p,q,si,a,b;
	int k;
	UM_coord pte;

	q = -cpos->dis;
	p = UM_DOT (sn,cpos->pte) - pcon;
	si = UM_DOT (sn,cpl->ve);

	a  = (p-si*q)/(1.-si*si);
	b  = q - a*si;

	for (k = 0; k < 3; k++)
	{
		pte[k] = cpos->pte[k] - a*sn[k] - b*cpl->ve[k];
	}

	if (itsk == 1)
		um_vctovc (pte,cpos->pte);

	*da = a; *db = b;

	return;
}

/*********************************************************************
**    E_FUNCTION     : S_move_ab (itsk,cpos,cpl,a,b,ier)
**       Project the cutter on PS and CS, calculate the repositioning move
**       to the PS and CS planes
**    PARAMETERS
**       INPUT  :
**             itsk        - reposition the tool iff 1
**             cpos        - original tool position
**             cpl         - CS structure
**             pcon        - PS distance
**       OUTPUT :
**             cpos        - new tool position
**             a           - repositioning move alond PS normal
**             b           - repositioning move alond CS normal
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_move_ab (itsk,cpos,cpl,a,b,ier)
int itsk;
toolpt *cpos;
cspln *cpl;
UU_REAL *a,*b;
UM_int2 *ier;
{
	UM_real8 sn[4];
	UM_vector ps;
	UU_REAL pcon,dpl;
	UM_real4 sdis,te[3],ta[3];
	int k;

		for (k = 0; k < 3; k++)
		{
			te[k] = cpos->pte[k];
			ta[k] = cpos->vta[k];
		}

		fmprj1 (te,ta,&cpos->up,&cpos->vp,sn,&sdis,ier);
		if (*ier != 0) return;

		for (k = 0; k < 3; k++)
		{
			ps[k] = sn[k];
		}

		pcon = sdis;
		dpl = sn[3];

		S_move_ab1 (itsk,cpos,ps,pcon,cpl,a,b);
}

/*********************************************************************
**    E_FUNCTION     : S_move2 (cpos,sn,pcon,cpl,tool,da,db)
**       Reposition a tool so that it touches PS and CS planes
**    PARAMETERS
**       INPUT  :
**             cpos        - original tool position
**             tool        - tool structure
**             sn          - PS normal
**             cpl         - CS structure
**             pcon        - PS distance
**             del         - step size
**       OUTPUT :
**             cpos        - new tool position
**             da          - repositioning move alond PS normal
**             db          - repositioning move alond CS normal
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_move2 (cpos,sn,pcon,cpl,tool,da,db)
toolpt *cpos;
UM_vector sn;
cspln *cpl;
cutter *tool;
UU_REAL pcon,*da,*db;
{
	UU_REAL p,q,si,a,b;
	int k;
	
	q = -cpos->dis;
	p = UM_DOT (sn,cpos->pte) - pcon;
	si = UM_DOT (sn,cpl->ve);

	a  = (p-si*q)/(1.-si*si);
	b  = q - a*si;

	for (k = 0; k < 3; k++)
	{
		cpos->pte[k] = cpos->pte[k] - a*sn[k] - b*cpl->ve[k];
	}

	*da = a; *db = b;

	return;
}

/*********************************************************************
**    E_FUNCTION     : S_move1 (posa,cpos,cpl,tool,tol,vfw,del,ier)
**       Move along the Check Surface between two good positions.
**    PARAMETERS
**       INPUT  :
**             posa        - original tool positions.
**             tool        - tool structure
**             tol         - tolerance
**             cpl         - CS structure
**             vfw         - forward vector
**             del         - step size
**       OUTPUT :
**             cpos        - next tool positions.
**             ier         - error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_move1 (posa,cpos,cpl,tool,tol,vfw,del,ier)
toolpt *posa,*cpos;
cutter *tool;
cspln *cpl;
UM_vector vfw;
UU_REAL del,tol;
UM_int2 *ier;
{
	UM_real8 sn[3];
	UM_vector ps,vec;
	UU_REAL d,a,b,dtol,pcon;
	UM_real4 sdis,te[3],ta[3];
	int icsfb,j,k,status;

	status = UU_FAILURE;
	dtol = tol;
/*
..... compute the forward direction along the initial forward and
..... at the intersection of the initial PS, CS planes
*/
	if (Sncsf == 1 || posa->icsf == 0)
	   ncl_fm_csdis (posa,cpl,tool,ier);
	else
	   ncl_fm_csdis0 (posa,cpl,tool,ier);
	if (*ier != 0) return (status);
		
	fmevsf (&posa->up,&posa->vp,sn,ier);

	if (*ier != 0) return (status);
	for (k = 0; k < 3; k++)
		ps[k] = sn[k];

	um_cross (ps,cpl->ve,vec);
	d = UM_DOT (vec,vfw);
	if (d < 0) um_negvc (vec,vec);
/*
..... move forward by the step distance
*/
	icsfb = cpos->icsf;
	ncl_copy_pos (posa,cpos);
	for (k = 0; k < 3; k++)
		cpos->pte[k] = posa->pte[k] + del*vec[k];
/*
..... nest to the two surfaces: PS and CS
*/
	for (j = 0; j < MAX_ITER; j++)
	{
		for (k = 0; k < 3; k++)
		{
			te[k] = cpos->pte[k];
			ta[k] = cpos->vta[k];
		}

		fmprj1 (te,ta,&cpos->up,&cpos->vp,sn,&sdis,ier);
		if (*ier != 0) 
			return (status);

		for (k = 0; k < 3; k++)
		{
			cpos->vta[k] = ta[k];
			ps[k] = sn[k];
		}

		pcon = sdis;
		
		if (Sncsf == 1 || cpos->icsf == 0)
			ncl_fm_csdis (cpos,cpl,tool,ier);
		else
			ncl_fm_csdismin (cpos,cpl,tool,ier);

		if (*ier != 0)
			return (status);

		S_move2 (cpos,ps,pcon,cpl,tool,&a,&b);

		if (fabs(a) < dtol && fabs(b) < dtol)
			return (UU_SUCCESS);
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : S_move (i0,cpl,tool,tol,ier)
**       Move along the Check Surface between two good positions.
**    PARAMETERS
**       INPUT  :
**             Stoolpts - list of tool positions
**             i0          - location of the positions in the list
**             tool        - tool structure
**             tol         - tolerance
**             cpl         - CS structure
**       OUTPUT :
**             Stoolpts - list updated
**             ier         -  error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_move (i0,cpl,tool,tol,ier)
int i0;
cutter *tool;
cspln *cpl;
UU_REAL tol;
UM_int2 *ier;
{
	toolpt *pos,*posa,*posb,cpos,pcur,plast;
	UU_REAL c,d,co,dp,eps,tol2,d1;
	UM_vector vfw,csn,vfw1;
	UU_REAL A = 0.5, B = 1.;
	UU_REAL da,db;
	int M = 25;
	int MMAX = 100;
	int i,j,l,m,n,status;

	eps = 5.*tol;
	tol2 = 2.*tol;
	
	l = 0;
	i = i0;
				
	pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);
	posa = pos + i0 - 1;
	posb = pos + i0;

/*
..... if the middle point is good just return - the one-step jump is OK
*/
	S_interp_dis (A,B,posa,posb,&cpos,cpl,tool,ier);
	if (*ier != 0)
		return (l);
	if (fabs(cpos.dis) < tol)
	{
		S_move_ab (0,&cpos,cpl,&da,&db,ier);
		if (*ier != 0 || (fabs(da) < tol && fabs(db) < tol))
			return (l);
	}

	ncl_copy_pos (posa,&pcur);
	ncl_copy_pos (posb,&plast);
/*
..... project the final position on CS
*/
	if (Sncsf == 1 || plast.icsf == 0)
	    ncl_fm_csdis (&plast,cpl,tool,ier);
	else
		ncl_fm_csdis0 (&plast,cpl,tool,ier);
	if (*ier != 0) return (l);
/*
..... nest the final position to PS and CS
*/
	S_move_ab (1,&plast,cpl,&da,&db,ier);
	if (*ier != 0) return (l);

	um_vcmnvc (plast.pte,pcur.pte,vfw);
	S_udir(vfw,&d);
	if (d < tol) return (l);
/*
..... final plane, through the final position perpendicular to CS
*/
	um_cross (plast.vta,cpl->ve,csn);
	S_udir(csn,&c);
	if (c < tol) return (l);

	co = UM_DOT (csn,vfw);
	if (co < 0) um_negvc (csn,csn);

/*
..... calculate a step size
*/
	dp = d/M;
	if (dp < 5*tol) dp = 5*tol;
	m = (d/dp + 0.5);
	n = m-1;
	if (n < 1)
	{
		if (cpos.dis < 0) l = -1;
		return (l);
	}
	dp = d / m;

	for (j = 0; j < MMAX; j++)
	{
		status = S_move1 (&pcur,&cpos,cpl,tool,tol2,vfw,dp,ier);
		if (*ier != 0) return (l);
		if (status != UU_SUCCESS) break;

		uu_list_insert(&Stoolpts,i,&cpos);
		i++; l++;
				
		um_vcmnvc (plast.pte,cpos.pte,vfw);
/*
..... if almost at the final plane - return
*/	
		c = UM_DOT (csn,vfw);
		if (!Slsurf && c < eps) return (l);
/*
..... if the middle point between current and last is good - return
*/	
		ncl_copy_pos (&cpos,&pcur);
		S_interp_dis (A,B,&pcur,&plast,&cpos,cpl,tool,ier);
		if (*ier != 0)
			return (l);
		if (fabs(cpos.dis) < tol2)
		{
			S_move_ab (0,&cpos,cpl,&da,&db,ier);

			um_vcmnvc (plast.pte,pcur.pte,vfw1);
			S_udir(vfw1,&d1);

			if (*ier != 0 || (fabs(da) < tol2 && fabs(db) < tol2 && d1 < 2.* dp))
				return (l);
		}

		if (j >= MMAX-1)
		{
			status = UU_FAILURE; break;
		}

		S_udir(vfw,&d);
		if (d < tol) return (l);
	}

	if (status != UU_SUCCESS)
	{
		S_interp_dis (A,B,&pcur,&plast,&cpos,cpl,tool,ier);

		if (*ier == 0 && cpos.dis < -tol) *ier = 256;
	}

	return (l);
}

/*********************************************************************
**    E_FUNCTION     : ncl_interp0 (posa,posb,cpos,cpl,tool,ier)
**       Find a position between posa and posb, approximately right on
**       the Check Surface. Project new tool position.
**       Purpose: to check for a CS extension (if the new position does
**       not gouge CS it must be an extension).
**    PARAMETERS
**       INPUT  :
**             posa,posb - original tool positions.
**             tool      - tool structure
**             cpl       - CS structure
**       OUTPUT :
**             cpos      -  interpolated position
**             ier       -  error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_interp0 (posa,posb,cpos,cpl,tool,ier)
cutter *tool;
cspln *cpl;
toolpt *posa,*posb,*cpos;
UM_int2 *ier;
{
	UU_REAL co,a,b;
	UM_vector vec,vca;

	um_vcmnvc (posb->pte,posa->pte,vec);

	co = UM_DOT (vec,cpl->ve);
	if (co < UM_DFUZZ) return (UU_FAILURE);

	um_vcmnvc (cpl->pt,posa->pte,vca);
	a = UM_DOT (vca,cpl->ve);
	a = a/co;
	b = 1.;

	if (a < 0 || a > b) return (UU_FAILURE);

	S_interp_dis (a,b,posa,posb,cpos,cpl,tool,ier);
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_interp1 (iknt,posa,posb,posc,cpl,tool,tol,ier)
**       Finds a new position between posa and posb, just at
**       the Check Surface.
**       Calls itself recursively.
**    PARAMETERS
**       INPUT  :
**             iknt      - recursive calls conter
**             posa,posb - original tool positions.
**             tool      - tool structure
**             tol       - tolerance
**             cpl       - CS structure
**       OUTPUT :
**             posc      -  interpolated position
**             ier       -  error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_interp1 (iknt,posa,posb,posc,cpl,tool,tol,ier)
int *iknt;
toolpt *posa,*posb,*posc;
cspln *cpl;
cutter *tool;
UU_REAL tol;
UM_int2 *ier;
{
	UU_REAL b;
	toolpt cpos;

	(*iknt)++;
	if (*iknt > MAX_INTERP || posa->dis - posb->dis < tol)
	{
		if (posc->dis < -tol || posc->dis > posa->dis)
			ncl_copy_pos (posa,posc);
	
		if (Sncsf == 1 || posc->icsf == 0)
			ncl_fm_csdis (posc,cpl,tool,ier);
		else
			ncl_fm_csdis0 (posc,cpl,tool,ier);

		return;
	}

	b = posa->dis - posb->dis;
	S_interp_dis (posa->dis,b,posa,posb,&cpos,cpl,tool,ier);

	if (*ier != 0) return;

	if (fabs(cpos.dis) < tol)
	{
		ncl_copy_pos (&cpos,posc);
		return;
	}

	if (*iknt == 1)
		ncl_copy_pos (&cpos,posc);

	if (cpos.dis < 0)
		ncl_interp1 (iknt,posa,&cpos,posc,cpl,tool,tol,ier);
	else
		ncl_interp1 (iknt,&cpos,posb,posc,cpl,tool,tol,ier);
}

/******************************************************************************
**    E_FUNCTION   : ncl_interp (lfintype,posa,posb,posc,cpl,tool,tol,iext,ier)
**       At input, posa and posb are two consecutive fmill tool positions,
**       with posa not gouging and posb gouging the Check Surface.
**       The routine finds a new position between posa and posb,
**       just at the Check Surface.
**    PARAMETERS
**       INPUT  :
**             lfintype  - finite type flag: check for CS extension iff ON
**             posa,posb - original tool positions.
**             tool      - tool structure
**             tol       - tolerance
**             cpl       - CS structure
**       OUTPUT :
**             posc      -  interpolated position
**             iext      -  the extension flag: incremented by 1 if the move
**                          between posa and posb crosses CS extension only
**             ier       -  error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
******************************************************************************/
void ncl_interp (lfintype,posa,posb,posc,cpl,tool,tol,iext,ier)
UU_LOGICAL lfintype;
cutter *tool;
cspln *cpl;
toolpt *posa,*posb,*posc;
UM_int2 *ier;
UU_REAL tol;
int *iext;
{
	int iknt = 0;
	int status;
	toolpt cpos;
	UU_REAL a,b;
	UU_REAL ctol = tol/4;


/*
..... The two checking below to skip NaN values
..... Sasha, Oct.18, 2017
*/
	if (posa->dis!=posa->dis)
	{
		*ier = 11;
		return;
	}
	if (posb->dis!=posb->dis)
	{
		*ier = 11;
		return;
	}

	ncl_interp1 (&iknt,posa,posb,posc,cpl,tool,ctol,ier);
	if (*ier != 0) return;
/*
..... set the extension flag if moving from posa to posb does not gouge
..... the CS itself
*/
	if (lfintype)
	{
		if (posc->dis > -tol)
		{
			status = ncl_interp0 (posc,posb,&cpos,cpl,tool,ier);
			if (*ier != 0) return;
			
			if (status == UU_SUCCESS)
			{
				um_vctovc (cpos.vta,Scyl.axis.n);
				um_vctovc (cpos.pte,Scyl.axis.p0);

				if (!um_point_is_in_cylinder(cpl->pt,&Scyl))
				{
					*iext = *iext + 1;
				}
			}
			else
			{
				for (a = 0.1, b = 1.; a < b; a += 0.1)
				{
					S_interp_dis (a,b,posc,posb,&cpos,cpl,tool,ier);
					if (*ier != 0) return;
					um_vctovc (cpos.vta,Scyl.axis.n);
					um_vctovc (cpos.pte,Scyl.axis.p0);

					if (um_point_is_in_cylinder(cpl->pt,&Scyl))
						return;

				}
				*iext = *iext + 1;
			}
		}
		else
		{
			um_vctovc (posc->vta,Scyl.axis.n);
			um_vctovc (posc->pte,Scyl.axis.p0);
			if (!um_point_is_in_cylinder(cpl->pt,&Scyl))
			{
				*iext = *iext + 1;
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_toolparams (tool)
**       Get various tool parameters into the tool structure.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**             tool      - tool structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_toolparams (tool)
cutter *tool;
{
	UM_int2 id;
	UM_real8 toolpar;

	id = 1;
	gettool (&id ,&toolpar);
	tool->rad = toolpar/2.;
	id = 2;
	gettool (&id ,&toolpar);
	tool->crad = toolpar;
	id = 3;
	gettool (&id ,&toolpar);
	tool->height = toolpar;
	id = 6;
	gettool (&id ,&toolpar);
	tool->cute = toolpar;
	id = 63;
	gettool (&id ,&toolpar);
	tool->sbe = toolpar;
	id = 64;
	gettool (&id ,&toolpar);
	tool->cbe = toolpar;
	id = 65;
	gettool (&id ,&toolpar);
	tool->cutn = toolpar;
}

/*********************************************************************
**    E_FUNCTION     : ncl_fm_chkpts (kcs4,npts4,tol8,thk8,ier)
**       Change fmill tool path to avoid a Check Surface.
**    PARAMETERS
**       INPUT  :
**             kcs4     - Key of Check Surface.
**             npts4    - number of points created.
**             tol8     - tolerance
**             thk8     - CS thick
**       OUTPUT :
**             npts4    - number of points possibly changed.
**             ier      - error number if something went wrong
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fm_chkpts (kcs4,npts4,thk8,tol8,ier)
UM_int2 *ier;
UM_int4 *kcs4,*npts4;
UM_real8 *tol8,*thk8;
{
	int i,j,k,l,npts,status;
	struct NCL_fixed_databag eptr;
	struct NCL_nclpl_rec *pln;
	UU_LOGICAL lfintype,lmovestart;
	int iext;

	UM_int2 rn;

	UM_vector vec;
	UU_REAL d;

	UU_REAL tol;
	UU_KEY_ID cskey;
	toolpt *pos,posi,posj;
	cutter tool;
	cspln cpl;

	npts = *npts4;
	if (npts < 2 || Stoolpt_init == 0 || npts > Stoolpts.cur_cnt)
		return;
/*
..... initialize CS entity
*/
	cpl.thk = *thk8;
	cpl.tol = *tol8;
	cskey = *kcs4;
	tol = *tol8;
	eptr.key = cskey;
	status = ncl_retrieve_data_fixed(&eptr);

	if (status != UU_SUCCESS) goto done;
	ncl_get_type(eptr.rel_num,&rn);

	lfintype = (rn != NCLI_PLANE && rn != NCLI_LINE);
	Slsurf = (rn == NCLI_SURF);
	cpl.lproj = (rn != NCLI_PLANE);

	if (cpl.lproj)
	{
		cpl.isf = 2;
		status = evstup1 (kcs4,&cpl.isf);
		if (status != UU_SUCCESS) goto done;
	}
	else
	{
		pln = (struct NCL_nclpl_rec *)&eptr;
		um_vctovc (pln->pt,cpl.pt);
		um_unitvc(pln->nvec,cpl.ve);
	}

	pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);

	ncl_toolparams (&tool);

	Scyl.radius = tool.rad + cpl.thk + tol;

	cpl.init = 1;
	if (cpl.lproj)
	{
		pos[0].uvh[0] = 0.5;
		pos[0].uvh[1] = 0.5;
		pos[0].uvh[2] = 0.5*(tool.crad+tool.height);
	}
	else
	{
/*
..... plane normal should point away from the tool
*/
		um_vcmnvc (cpl.pt,pos[0].pte,vec);
		d = UM_DOT (vec,cpl.ve);
		if (d < 0)
		{
			um_negvc (cpl.ve,cpl.ve);
		}
	}
/*
..... compute CS distance for all points
*/
	ncl_fm_csdis (&pos[0],&cpl,&tool,ier);
	if (*ier != 0) return;
	cpl.init = 0;

	lmovestart = UU_FALSE;
	if (pos[0].dis < -tol)
	{
		um_vctovc (pos[0].vta,Scyl.axis.n);
		um_vctovc (pos[0].pte,Scyl.axis.p0);
		lmovestart = um_point_is_in_cylinder(cpl.pt,&Scyl);
	}

	for (i = 1; i < npts; i++)
	{
		if (cpl.lproj)
		{
			for (k = 0; k < 3; k++)
			{
				pos[i].uvh[k] = pos[i-1].uvh[k];
			}
		}
		/*if (i==56)
				{
					printf ("%% dis %g \n",pos->dis);;
				}*/
		ncl_fm_csdis (&pos[i],&cpl,&tool,ier);
		if (*ier != 0) return;
	}

	posi.iret = posj.iret = 0;

		if (lmovestart)
		{
			for (j = 1; j < npts; j++)
			{
				if (pos[j].dis >= 0)
					break;
			}
/*
..... delete gouging positions at start
*/
			if (j < npts)
			{
				iext = 0;
				ncl_interp (lfintype,&pos[j],&pos[j-1],&posj,&cpl,&tool,tol,&iext,ier);
				if (*ier != 0) return;

				l = j - 1;
				if (l > 0)
				{
					uu_list_delete (&Stoolpts,0,l);
					npts -= l;
					pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);
				}

				ncl_copy_pos (&posj,&pos[0]);

			}
			else
				npts = 0;
		}

	for (i = 1; i < npts; i++)
	{
/*
..... find next gouging position, then find a good position before it
..... and right at the CS
*/
		if (pos[i].dis < -tol)
		{
			for (j = i+1; j < npts; j++)
			{
				if (pos[j].dis >= 0)
					break;
			}

			iext = 0;
			ncl_interp (lfintype,&pos[i-1],&pos[i],&posi,&cpl,&tool,tol,&iext,ier);
			if (*ier != 0) 
				return;
/*
..... find next non-gouging position, then find a good position before it
..... and right at the CS
*/
			if (j < npts)
			{

				ncl_interp (lfintype,&pos[j],&pos[j-1],&posj,&cpl,&tool,tol,&iext,ier);
				if (*ier != 0) 
				{
					*ier = 0;
					return;
				}
			}

			if (iext == 2)
			{
				i = j-1;
				continue;
			}

			ncl_copy_pos (&posi,&pos[i]);
/*
..... delete gouging positions and insert/(replace with) good positions 
*/
			if (j < npts)
			{
				l = j - i - 2;
				if (l < 0)
					uu_list_insert(&Stoolpts,j,&posj);
				else
					ncl_copy_pos (&posj,&pos[j-1]);

				if (l > 0)
					uu_list_delete (&Stoolpts,i+1,l);

				npts -= l;
				i = j - 1 - l;

				if (lfintype)
				{
/*
..... move along CS between the two good positions
*/
					l = S_move (i,&cpl,&tool,tol,ier);
					if (*ier != 0) 
						return;
					if (l > 0)
					{
						i = i+l;
						npts = npts+l;
					}
				}

				pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);
			}
			else
				npts = i+1;
		}
	}

	*npts4 = npts;
done:

	if (status != UU_SUCCESS && *ier == 0)
		*ier = 138;

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_fm_chkpts1 (kcs4,ncsf4,npts4,tol8,thk8,ier)
**       Change fmill tool path to avoid a Check Surface.
**    PARAMETERS
**       INPUT  :
**             kcs4     - Key of Check Surface.
**			   ncsf4	- numbe rof check surfaces
**             npts4    - number of points created.
**             tol8     - tolerance
**             thk8     - CS thick
**       OUTPUT :
**             npts4    - number of points possibly changed.
**             ier      - error number if something went wrong
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fm_chkpts1 (kcs8,ncsf4,npts4,thk8,tol8,ier)
UM_int2 *ier;
UM_int4 *npts4;
UM_int2 *ncsf4;
UM_real8 *tol8,*kcs8,*thk8;
{
	int icsf,i,j,k,l,npts,ncsf,icsf_min,status;
	struct NCL_fixed_databag eptr;
	struct NCL_nclpl_rec *pln;
	UU_LOGICAL lfintype,lmovestart,loutside0;
	int iext;
	UM_int2 rn,iret;
	UM_vector vec;
	UU_REAL d,dp;

	UU_REAL tol,tol10,toltk,dissq;
	UU_KEY_ID cskey;
	UU_REAL csasw;
	toolpt *pos,posi,posj;
	cutter tool;
	cspln cpl;
	UU_REAL dis,dmin,ptdmin;
	UM_int2 nwds, ietype;
	UM_real4 uvh0,uvh1,uvh2;

	int irot,itsk,iprcnt,insf,ier1;
	UM_coord pt,uv;
	UM_srf_boundary bndr;
	UU_LIST cvlst,uvlst;
	UU_REAL u1,v1;
	UM_transf tfmat;
	NCL_surf_struct s1;

	npts = *npts4;
	ncsf = *ncsf4;

	Sncsf = ncsf;
	if (npts < 2 || Stoolpt_init == 0 || npts > Stoolpts.cur_cnt)
		return;

	pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);

	ncl_toolparams (&tool);

/*
..... initialize CS entity
*/
	cpl.tol = *tol8;
	tol = *tol8;
    tol10 = 0.1 * tool.rad;
	Stol = *tol8;
	toltk = tool.rad;

/*
.....Initialize sf boundary
*/	
	ncl_set_boundary_toler (tol);
	um_init_boundary (&bndr);
	uu_list_init (&cvlst, sizeof(UM_coord), 200, 200);
	uu_list_init (&uvlst, sizeof(UM_coord), 200, 200);
	bndr.uvpts = &uvlst;
	bndr.cvpts = &cvlst;

	lmovestart = UU_FALSE;

	for (icsf = 1; icsf <= ncsf; icsf++)
	{
		Sthk[icsf-1] = thk8[icsf-1];
		Skcs[icsf-1]= kcs8[icsf-1];

		cpl.thk = thk8[icsf-1];
		csasw = kcs8[icsf-1];
		gtdesc(&csasw,&cskey,&nwds,&ietype);

		seticsf(&icsf);

		eptr.key = cskey;
		status = ncl_retrieve_data_fixed(&eptr);
		if (status != UU_SUCCESS) goto done;
		ncl_get_type(eptr.rel_num,&rn);

		if (status == UU_SUCCESS)
		{
     		status = uc_retrieve_transf (eptr.key, tfmat);

			ncl_free_bndry (&bndr);
			UU_LIST_EMPTY (bndr.uvpts);
			UU_LIST_EMPTY (bndr.cvpts);
			status = ncl_get_bndry (&eptr,tfmat,&bndr,tol10,UU_TRUE);
			if (bndr.nb < 1) status = UU_FAILURE;
		}

		lfintype = (rn != NCLI_PLANE && rn != NCLI_LINE);
		Slsurf = (rn == NCLI_SURF);
		cpl.lproj = (rn != NCLI_PLANE);

		if (cpl.lproj)
		{
			cpl.isf = 2;
			status = evstup1 (&cskey,&cpl.isf);
			if (status != UU_SUCCESS)
				goto done;
		}
		else
		{
			pln = (struct NCL_nclpl_rec *)&eptr;
			um_vctovc (pln->pt,cpl.pt);
			um_unitvc(pln->nvec,cpl.ve);
		}

		Scyl.radius = tool.rad + cpl.thk + tol;
		cpl.init = 1;
		if (cpl.lproj)
		{
			pos[0].uvh[0] = 0.5;
			pos[0].uvh[1] = 0.5;
			pos[0].uvh[2] = 0.5*(tool.crad+tool.height);
		}
		else
		{
/*
..... plane normal should point away from the tool
*/
			um_vcmnvc (cpl.pt,pos[0].pte,vec);
			d = UM_DOT (vec,cpl.ve);
			if (d < 0)
			{
				um_negvc (cpl.ve,cpl.ve);
			}
		}
/*
..... compute CS distance for all points
*/
		icsf_min = pos[0].icsf;
		dmin = pos[0].dis;
		ptdmin = pos[0].ptdis;
		uvh0 = pos[0].uvh[0];
		uvh1 = pos[0].uvh[1];
		uvh2 = pos[0].uvh[2];
		toltk = tool.rad + cpl.thk;

		ncl_fm_csdis (&pos[0],&cpl,&tool,ier);
		if (*ier != 0) 
			return;
		cpl.init = 0;
		ier1 = 0;
		loutside0 = UU_FALSE;

		itsk = 0;
		iprcnt= 0;
		u1 = pos[0].uvh[0];
		v1 = pos[0].uvh[1];
		ncl_pt_on_sf(&itsk,&eptr.key,&iprcnt,&u1,&v1,&pt,&ier1);

		uv[0] = u1;
		uv[1] = v1;
		insf = um_inside_bndry (uv,pt,&bndr,&tol10);			
		dissq = um_sqdis(pt,cpl.pt);

		if (insf == -1 && dissq > toltk*toltk)
		{
			pos[0].dis = dmin;
			pos[0].ptdis = ptdmin;
			pos[0].icsf = icsf_min;
			pos[0].uvh[0] = uvh0;
			pos[0].uvh[1] = uvh1;
			pos[0].uvh[2] = uvh2;
			loutside0 = UU_TRUE;
		}

		if (!loutside0)
		{
			pos[0].icsf = icsf;
			dis = pos[0].dis;
			if (dis > dmin)
			{
				pos[0].dis = dmin;
				pos[0].ptdis = ptdmin;
				pos[0].icsf = icsf_min;
				pos[0].uvh[0] = uvh0;
				pos[0].uvh[1] = uvh1;
				pos[0].uvh[2] = uvh2;
			}
		}

		if (pos[0].dis < -tol)
		{
			um_vctovc (pos[0].vta,Scyl.axis.n);
			um_vctovc (pos[0].pte,Scyl.axis.p0);
			lmovestart = um_point_is_in_cylinder(cpl.pt,&Scyl);
		}

		for (i = 1; i < npts; i++)
		{
			if (cpl.lproj)
			{
				for (k = 0; k < 3; k++)
					pos[i].uvh[k] = pos[i-1].uvh[k];
			}

			icsf_min = pos[i].icsf;
			dmin = pos[i].dis;
			ptdmin = pos[i].ptdis;
			uvh0 = pos[i].uvh[0];
			uvh1 = pos[i].uvh[1];
			uvh2 = pos[i].uvh[2];

			ncl_fm_csdis (&pos[i],&cpl,&tool,ier);
			if (*ier != 0)
				return;
/*
.....Check if the cpl pt inside the trimmed surface or not
*/
			itsk = 0;
			iprcnt= 0;
			u1 = pos[i].uvh[0];
			v1 = pos[i].uvh[1];
			ncl_pt_on_sf(&itsk,&eptr.key,&iprcnt,&u1,&v1,&pt,&ier1);

			uv[0] = u1;
			uv[1] = v1;
			insf = um_inside_bndry (uv,pt,&bndr,&tol10);

			dissq = 0.0;
			if (insf == -1)
			{
				iret = -1;
				status = ncl_proj_on_srf_bndry (pt,&eptr,&bndr,&s1,UU_TRUE);
				dissq = um_sqdis(s1.pt,cpl.pt);		
			}
			if (insf == -1 && dissq > toltk*toltk)
			{
				pos[i].dis = dmin;
				pos[i].ptdis = ptdmin;
				pos[i].icsf = icsf_min;
				pos[i].uvh[0] = uvh0;
				pos[i].uvh[1] = uvh1;
				pos[i].uvh[2] = uvh2;
				continue;
			}

			pos[i].icsf = icsf;
			dis = pos[i].dis;
			if (dis > dmin)
			{
				pos[i].dis = dmin;
				pos[i].ptdis = ptdmin;
				pos[i].icsf = icsf_min;
				pos[i].uvh[0] = uvh0;
				pos[i].uvh[1] = uvh1;
				pos[i].uvh[2] = uvh2;
			}
		}
	}

/*
.....Free memory allocated
*/
	ncl_free_bndry (&bndr);
	uu_list_free (&uvlst);
	uu_list_free (&cvlst);

	posi.iret = posj.iret = 0;

	if (lmovestart)
	{
		for (j = 1; j < npts; j++)
		{
			if (pos[j].dis >= 0)
				break;
		}
/*
..... delete gouging positions at start
*/
		if (j < npts)
		{
			iext = 0;
			ncl_interp (lfintype,&pos[j],&pos[j-1],&posj,&cpl,&tool,tol,&iext,ier);
			if (*ier != 0) return;

			l = j - 1;
			if (l > 0)
			{
				uu_list_delete (&Stoolpts,0,l);
				npts -= l;
				pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);
			}

			ncl_copy_pos (&posj,&pos[0]);

		}
		else
			npts = 0;
	}

	for (i = 1; i < npts; i++)
	{
/*
..... find next gouging position, then find a good position before it
..... and right at the CS
*/
		if (pos[i].dis < -tol)
		{
			for (j = i+1; j < npts; j++)
			{
				if (pos[j].dis >= 0)
					break;
			}

			icsf = pos[i].icsf;
			seticsf(&icsf);

			iext = 0;
			ncl_interp (lfintype,&pos[i-1],&pos[i],&posi,&cpl,&tool,tol,&iext,ier);
			if (*ier != 0)
				return;
/*
..... find next non-gouging position, then find a good position before it
..... and right at the CS
*/
			icsf = pos[j].icsf;
			seticsf(&icsf);

			if (j < npts)
			{
				ncl_interp (lfintype,&pos[j],&pos[j-1],&posj,&cpl,&tool,tol,&iext,ier);
				if (*ier != 0)
					return;
			}

			if (iext == 2)
			{
				i = j-1;
				continue;
			}

			ncl_copy_pos (&posi,&pos[i]);
/*
..... delete gouging positions in between
*/
			if (j < npts)
			{
				l = j - i - 2;
				if (l < 0)
					uu_list_insert(&Stoolpts,j,&posj);
				else
					ncl_copy_pos (&posj,&pos[j-1]);

				if (l > 0)
					uu_list_delete (&Stoolpts,i+1,l);

				npts -= l;
				i = j - 1 - l;

				if (lfintype)
				{
/*
..... move along CS between the two good positions
*/
					l = S_move (i,&cpl,&tool,tol,ier);
					if (*ier != 0)
						return;
					if (l > 0)
					{
						i = i+l;
						npts = npts+l;
					}
				}

				pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);
			}
			else
				npts = i+1;
		}
	}

	*npts4 = npts;
done:

	if (status != UU_SUCCESS && *ier == 0)
		*ier = 138;

	return;
}

/*******************************************************************
**    E_FUNCTION     : void ncl_find_closer (pp,pt1,pt2,qt1,qt2,ZVEC,
**                                   pint,qint,fact,tol,dd,ipts,jpts)
*******************************************************************/
static void S_find_closer (pos,pt1,pt2,qt1,qt2,ZVEC,pint,qint,fact,tol,
dd,ipts,jpts)
toolpt *pos;
UM_coord pt1,pt2,qt1,qt2;
UU_REAL *ZVEC,fact,tol,dd;
UM_coord pint,qint;
int ipts,*jpts;
{
	int j,k,nint;
	UU_REAL dp;
	UM_coord pk,qk,qk1,qk2;

	j = *jpts;

	for (k = j-1; k > ipts+1; k--)
	{
		um_vctovc (pos[k].pte, qk1);
		um_vctovc (pos[k+1].pte, qk2);

		um_isegseg1 (pt1,pt2,qk1,qk2,ZVEC,&nint,pk,qk,fact,tol);
		if (nint > 0)
		{
			dp = UM_SQDIS (pt1,pk);
			if (dp < dd)
			{
				*jpts = k;
				um_vctovc (pk,pint);
				um_vctovc (qk,qint);
				um_vctovc (qk1,qt1);
				um_vctovc (qk2,qt2);
				return;
			}
		}
	}
}

/*******************************************************************
**    E_FUNCTION     : void S_interp_uv (pos,pint,ipts,ui)
*******************************************************************/
static void	S_interp_uv (pos,pint,ipts,ui)
toolpt *pos;
UM_coord pint;
UU_REAL *ui;
int ipts;
{
	UU_REAL a,b,s,t,u0,u1;

	u0 = pos[ipts].up;
	u1 = pos[ipts+1].up;

	a = UM_DCCCC (pos[ipts].pte,pint);
	b = UM_DCCCC (pos[ipts].pte,pos[ipts+1].pte);

	if (a >= b || b < UM_DFUZZ)
		t = 1;
	else
		t = a / b;
	s = 1-t;

	*ui = s*u0 + t*u1;
}

/*********************************************************************
**    E_FUNCTION     : S_deloop (npts,i0,ntot,fact,tol)
**       Change fmill tool path to avoid a Check Surface.
**    PARAMETERS
**       INPUT  :
**             kcs4     - Key of Check Surface.
**             npts4    - number of points created.
**             tol8     - tolerance
**             thk8     - CS thick
**       OUTPUT :
**             npts4    - number of points possibly changed.
**             ier      - error number if something went wrong
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/			
static void S_deloop (npts,i0,ntot,fact,tol)
int *i0,*ntot,npts;
UU_REAL fact,tol;
{
	int is;
	int nint,ipts,jpts,k;
	UU_REAL dd,upi,uqi;
	UM_coord pint,qint,pt1,pt2,qt1,qt2,pqi;

	toolpt *pos;
	UU_REAL tolsq = tol*tol;
	UU_REAL *ZVEC = UU_NULL;

	is = *i0;

	if (npts < 2) goto Done;

	pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);
	pos += is;

	for (ipts = 0; ipts < npts - 3; ipts++)
	{
		um_vctovc (pos[ipts].pte, pt1);
		um_vctovc (pos[ipts+1].pte, pt2);

		for (jpts = npts - 2; jpts > ipts+1; jpts--)
		{
			um_vctovc (pos[jpts].pte, qt1);
			um_vctovc (pos[jpts+1].pte, qt2);
			um_isegseg1 (pt1,pt2,qt1,qt2,ZVEC,&nint,pint,qint,fact,tol);
			if (nint > 0)
			{
				dd = UM_SQDIS (pt1,pint);
				if (dd < tolsq)
				{
					if (ipts <= 0) continue;
					ipts--;
					um_vctovc (pos[ipts].pte, pt1);
					um_vctovc (pos[ipts+1].pte, pt2);
					dd = UM_SQDIS (pt1,pint);
				}

				S_find_closer (pos,pt1,pt2,qt1,qt2,ZVEC,pint,qint,fact,tol,dd,ipts,&jpts);

				dd = UM_SQDIS (qt2,qint);
				if (dd < tolsq)
				{
					jpts++;
					um_vctovc (pos[jpts].pte, qt1);
					um_vctovc (pos[jpts+1].pte, qt2);
				}

				ncl_aver_iseg (pt1,pint,qt2,qint,ZVEC,tolsq,pqi);
				S_interp_uv (pos,pint,ipts,&upi);
				S_interp_uv (pos,qint,jpts,&uqi);

				um_vctovc (pqi, pos[jpts].pte);
				pos[jpts].up = (upi + uqi) / 2;

				k = jpts - ipts - 1;
				if (k > 0)
				{
					npts -= k;
					uu_list_delete (&Stoolpts,is+ipts+1,k);

					pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);
					pos += is;
				}
				break;
			}
		}
	}

Done:
		is = is + npts;
		*i0 = is;
		*ntot = Stoolpts.cur_cnt;
		return;
}

/*********************************************************************
**    E_FUNCTION     : fmdelp (npts4,crad8,tol8)
**       Remove loops from FMILL flowlines (used with fixed tool axis).
**    PARAMETERS
**       INPUT  :
**             npts4    - number of points created.
**             tol8     - tolerance
**             crad8    - corner radius
**       OUTPUT :
**             npts4    - number of points possibly changed.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void fmdelp (npts4,crad8,tol8)
UM_int4 *npts4;
UM_real8 *crad8,*tol8;
{
	int i,npts,n,i0;
	UU_REAL v,tol,fact;
	toolpt *pos;

	npts = *npts4;
	if (npts < 2 || Stoolpt_init == 0 || npts > Stoolpts.cur_cnt)
		return;

	tol = *tol8;
	fact = *crad8/(30.*tol);

	pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);

	v = pos[0].vp;
	i0 = 0;
	n = 1;

	for (i = 1; i < npts; i++)
	{
		if (pos[i].iret == 0 && pos[i].vp == v)
			n++;
		else
		{
			if (pos[i].vp == v) n++;
			S_deloop (n,&i0,&npts,fact,tol);

			i = i0;
			n = 1;
			v = pos[i0].vp;
		}
	}

	if (n > 2) S_deloop (n,&i0,&npts,fact,tol);

	*npts4 = npts;
}

/*********************************************************************
**       Copy the list Stoolpts to the list Sfmptlst.
*********************************************************************/
void ncl_fmptlst_create()
{
	int npts;
    toolpt *pos;

	npts = UU_LIST_LENGTH (&Stoolpts);
	pos = (toolpt *) UU_LIST_ARRAY (&Stoolpts);

	uu_list_init (&Sfmptlst,sizeof(toolpt),npts,npts);
	uu_list_push_multiple (&Sfmptlst,npts,pos);
}

/*********************************************************************
**       Free the list Sfmptlst
*********************************************************************/
void ncl_fmptlst_free()
{
	uu_list_free(&Sfmptlst);
}

/*********************************************************************
**       Copy the list Sfmptlst to the list Stoolpts
*********************************************************************/
void ncl_fmptlst_copy()
{
	int npts;
    toolpt *pos;

	npts = UU_LIST_LENGTH (&Sfmptlst);
	pos = (toolpt *) UU_LIST_ARRAY (&Sfmptlst);

	UU_LIST_EMPTY (&Stoolpts);
	uu_list_push_multiple (&Stoolpts,npts,pos);

	ncl_fmptlst_free();
}

