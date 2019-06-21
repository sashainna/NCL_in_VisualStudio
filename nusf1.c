/*********************************************************************
**    NAME         :  nusf1.c
**       CONTAINS: User interface routines for surface creation.
**
**		 ncl_near_bndry_edge
**		 nclu_sfedge_chain
**		 nclu_sf_chain
**		 nclu_pt_int_bndry
**		 nclu_alldisp_surf 
**		 ncl_get_pkpt
**		 nclu_pt_int_bndry_rot
**		 ncl_new_chain_vec
**		 nclu_list_in_buf
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nusf1.c , 25.6
**    DATE AND TIME OF LAST MODIFICATION
**       02/28/17 , 15:49:06
*********************************************************************/

#include "mgeom.h"
#include "usysdef.h"
#include "wsgl.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "dselmask.h"
#include "mcrv.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mdcpln.h"
#include "nclfc.h"
#include "nccs.h"
#include "dselect.h"
#include "mattr.h"
#include "mdeval.h"
#include "gsegac.h"
#include "vsegbf.h"

#define UD_NULLPICKID -1               /* invalid pick id */

static UU_REAL Srftol ;
static UU_LIST Schain_sflist;           /* list of chained surfaces */

static void S_get_endpts();

/*********************************************************************
**    FUNCTION     : ncl_near_bndry_edge (nv,pts,pt,spt,ept,segno,pt2)
**       Get a boundary edge closest to given point
**    PARAMETERS
**       INPUT  :
**			nv		:	number of boundary points
**			pts		:	boundary points
**			pt		:	reference point	
**       OUTPUT :
**          spt		:	start of nearest bndry edge
**			ept		:	end of nearest bndry edge
*			segno	:	boundary edge index
**			pt2		:	pt projection on edge
**    RETURNS		:	UU_SUCCESS : nearest boundary within tol
**						UU_FAILURE : nearest boundary not within tol
**    SIDE EFFECTS	: none
**    WARNINGS		: none
*********************************************************************/
int ncl_near_bndry_edge(nv,pts,pt,spt,ept,segno,pt2)
int nv,*segno;
UM_coord *pts,pt,spt,ept,pt2;
{
	int i,index=0;
	UM_vector npt,vc1,vc2;
	UU_REAL min=30,d1,d2,d3,dist;

	for (i=0; i <= nv -1; i++)
	{
		if(i == nv-1)	um_vctovc(pts[0],ept);
       	else	um_vctovc(pts[i+1],ept);
		um_vctovc(pts[i],spt);
		um_vcmnvc(ept, spt, vc1);
		um_unitvc(vc1,vc2);
		um_nptln(pt,spt,vc2,pt2);
		d1 = UM_DOT(pt,vc2);
		d2 = UM_DOT(spt,vc2);
		d3 = UM_DOT(ept,vc2);
		if (d1 < d2)
			um_vctovc(spt,pt2);
		else if (d1 > d3)
			um_vctovc(ept,pt2);
		um_vcmnvc(pt,pt2,npt);
		dist = um_mag(npt);
		if(dist < min)
		{
			min = dist;
			index =i;
		}
	}
	if(index == nv-1)	um_vctovc(pts[0],ept);
    else	um_vctovc(pts[index+1],ept);
	um_vctovc(pts[index],spt);
	um_vcmnvc(ept, spt, vc1);
	um_unitvc(vc1,vc2);
	um_nptln(pt,spt,vc2,pt2);
	*segno = index;

	if(min > Srftol) return UU_FAILURE;
	else return UU_SUCCESS;
}

/*********************************************************************
**    FUNCTION     : nclu_sfedge_chain(sflist,flag)
**       Chain select surfaces in select subsystem by following sf edge 
**		boundaries
**    PARAMETERS
**       INPUT  :	 
**			   flag = UU_TRUE = Store boundary curve numbers with selected
**                 surfaces.  The surface list will be returned in
**                 'sflist' instead of being placed on the Selection
**                 buffer.
**       OUTPUT :
**          sflist = List of surfaces and boundary curve numbers when
**                   flag = UU_TRUE;
**    RETURNS      : UU_SUCCESS if picks were successful.
**    SIDE EFFECTS : Adds surfaces to the global select buffer.
**    WARNINGS     : none
*********************************************************************/
nclu_sfedge_chain(sflist,flag)
UU_LIST *sflist;
UU_LOGICAL flag;
{
	UU_LIST keylst;
	int eval,j,ierr,unflg,back, segno=0,irtn,nv,numint,status,nkey,*mask;
	int fp,k,ncsf,ncvj,nvj,segnoj=0,ipt,incsf,*eplst,ncv,lstpt,ipt_last;
	int *eplstj,retstat, start_vseg;
	UU_KEY_ID *kyj,ckey1,ckey2,first, last, *ky,pshkey;
	char sbuf[80];
	struct NCL_fixed_databag sfj,e,en;
	UM_PLOCREC pick;
	UM_coord ptsf,uv, *ptsj,*pts,ept, spt, ptuv,pt,ptn,pt2,prev,next,pt2j;
	UM_coord sptj,eptj;
	UM_vector norm,norm2,vp,vn,v1,v2,vec,assist;
	Gwpoint3 point, vector;
	UM_srf_boundary bound,boundj;
	struct NCL_trimsf_rec sf;
	UM_angle ang;
	UU_REAL proj,dir;
/*
.....Create a list of all surfaces displayed.
*/
	retstat = UU_SUCCESS;
	UM_len_exttoint(UD_chain_mod.surftoler,Srftol);
	uu_list_init (&keylst,sizeof(UU_KEY_ID),100,100);
	um_init_boundary(&bound); um_init_boundary(&boundj);
/*
.....Create a list of chain surfaces
*/
	uu_list_init (&Schain_sflist,sizeof(UU_KEY_ID),100,100);
/*
.....Limit geometry to surfaces, to get a list of all surfaces.
*/
	ud_lgeo(UU_TRUE, UD_ncl_netentity);
	status = nclu_alldisp_surf(&keylst);
	if (status == 0)
	{
		nkey = keylst.cur_cnt;
		ky = (UU_KEY_ID *) UU_LIST_ARRAY (&keylst);
	
	}
	else goto failed;
	ud_unlimit ();
	e.key = NULLKEY;

/*
.....Limit geometry selection to surfaces.
*/
srf1:;
	mask = (int *)UD_ncl_netentity;
	ud_lgeo(UU_TRUE, mask);
/*
.....Prompt user to select the first surface
*/
	ud_ldas(UD_DASPCKLOC, UM_MODEL, 140, &pick, 1,
            		&numint,1, UD_NODEFAULT);
	if (numint > 0)
  	{
/*
.....Get point on first surface
*/
		status = ncl_get_pkpt(&pick,&e,&pt,&vec,&ptuv);
		if (status != UU_SUCCESS)
		{
			ud_wrerr("Could not select surface boundary curve.");
			goto srf1;
		}
/*
		if (pick.pent.key[1]==pick.pent.key[0]) 
		{
			ud_wrerr("CVonSF picked is not a boundary CV.");
			ud_unlimit ();
			goto srf1;
		}
		sf.key = e.key;
		status = ncl_retrieve_data_fixed(&sf);
		if (status != UU_SUCCESS) goto exit;

		ckey1 = pick.pent.key[1]/100;
		ckey2 = pick.pent.key[1];
		if (ckey1 != sf.uv_key && ckey2 != sf.uv_key)
		{
			ud_wrerr("CVonSF picked is not a boundary CV.");
			ud_unlimit ();
			goto srf1;
		}
*/
	}
	else goto failed;
	
	first = e.key;
/*
.....Project this point on the nearest sf bndry edge
..... display assist vectors in the two directions ablong this bndry edge
*/
	um_free_boundary (&bound);
	status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,&e,&bound);
	if (status == UU_SUCCESS)
	{
		nv = bound.np[0];
		pts = (UM_coord *) UU_LIST_ARRAY (bound.cvpts);
		S_get_endpts(&e,pts,nv,&eplst,&ncv);
		status = ncl_near_bndry_edge(nv,pts,pt,spt,ept,&segno,pt2);
		point.x = pt2[0];
		point.y = pt2[1];
		point.z = pt2[2];
		um_vcmnvc(ept,pt2,assist);
		um_unitvc(assist,assist);
		vector.x = assist[0];
		vector.y = assist[1];
		vector.z = assist[2];
		start_vseg = ud_assist_vector(point,vector);
		um_vcmnvc(spt,pt2,assist);
		um_unitvc(assist,assist);
		vector.x = assist[0];
		vector.y = assist[1];
		vector.z = assist[2];
		ud_assist_vector(point,vector);
	}
/*
..... Prompt user to pick one of the assist vectors to determine direction.
*/
	ud_unlimit ();
	irtn = ud_pick_assist_seg("Pick an assist display segment:");	
/*
.....adjust the number
*/
	irtn = irtn-start_vseg+1;
	if(irtn < 1 || irtn > 2) goto srf1;
	uu_list_push(&Schain_sflist,&e.key);	
/*
.....SET direction of the chaining according to the assist vector picked.
*/
	switch(irtn)
	{
	case 1:
		back = 1;
		segno++;
		um_vctovc(pts[segno],prev);
		break;
	case 2:
		back = -1;
		um_vctovc(spt,prev);
		break;
	}
	um_vctovc(prev,next);
	fp = segno;
	um_vcmnvc(prev,pt2,vp);
/*
.....Push curve number onto list
*/
	incsf = 1;
	if (flag)
	{
		ipt = S_get_cvindex(eplst,ncv,back,segno);
		uu_list_push(&Schain_sflist,&ipt);
		incsf = 2;
		lstpt = ipt;
	}
/*
.....Prompt user to select the last surface (optional)
*/
srf2:;
	ud_lgeo(UU_TRUE, mask);
	ud_ldas(UD_DASPCKLOC, UA_NCL, 501, &pick, 1,
                  &numint,1, UD_NODEFAULT);

	if (numint != 0 ) 
	{
		status = ncl_get_pkpt(&pick,&en,&ptn,&vec,&ptuv);
		if (status != UU_SUCCESS)
		{
			ud_wrerr("Could not select surface boundary curve.");
			goto srf2;
		}
		last = en.key;
	}
/*
.....If last surface is not specified then assign the last surface as the 
.....first one.
*/
	else
		last = first;
/*
.....Project this point on the nearest sf bndry edge
.....If chain selecting boundary curves
.....This will be the last curve processed in the surface list
*/
	if (flag)
	{
		en.key = last;
		um_free_boundary (&boundj);
		status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,&en,&boundj);
		if (status == UU_SUCCESS)
		{
			nvj = boundj.np[0];
			ptsj = (UM_coord *) UU_LIST_ARRAY (boundj.cvpts);
			status = ncl_near_bndry_edge(nvj,ptsj,ptn,sptj,eptj,&segnoj,pt2j);
			S_get_endpts(&en,ptsj,nvj,&eplstj,&ncvj);
			ipt_last = S_get_cvindex(eplstj,ncvj,-back,segnoj);
			uu_free(eplstj);
		}
	}
/*
..... follow edge boundary till there is a sharp ccorner and a corner of another
..... surface boundary is found within tol.
*/
sf:
/*
NclxDbgPstr("");
sprintf(sbuf,"sf: segno = %d",segno);
NclxDbgPstr(sbuf);
sprintf(sbuf,"sf: key = %d",e.key);
NclxDbgPstr(sbuf);
sprintf(sbuf,"  prev = %10.3f,%10.3f,%10.3f",prev[0],prev[1],prev[2]);
NclxDbgPstr(sbuf);
sprintf(sbuf,"  next = %10.3f,%10.3f,%10.3f",next[0],next[1],next[2]);
NclxDbgPstr(sbuf);
sprintf(sbuf,"  pts[segno] = %10.3f,%10.3f,%10.3f",pts[segno][0],pts[segno][1],
	pts[segno][2]);
NclxDbgPstr(sbuf);
*/
/*
.....See if another subcurve has been encountered
*/
	if (flag)
	{
		ipt = S_get_cvindex(eplst,ncv,back,segno);
		if (ipt != lstpt)
		{
			uu_list_push(&Schain_sflist,&e.key);
			uu_list_push(&Schain_sflist,&ipt);
			lstpt = ipt;
			if (e.key == last && ipt == ipt_last) goto done;
/*
sprintf(sbuf,"Pushed   key = %d, ipt = %d",e.key,ipt);
NclxDbgPstr(sbuf);
*/
		}
	}
	um_unitvc(vp,vp);

	segno += back;
	if(segno == nv) 
	{
		if(um_dcccc(pts[0],prev)<UM_FUZZ)
			segno = 1;
		else
			segno = 0;
	}
	if(segno < 0) 
	{
		if(um_dcccc(pts[nv-1],prev)<UM_FUZZ)
			segno = nv-2;
		else
			segno = nv-1;
	}
	um_vctovc(pts[segno],next);
	if(fp == segno && !flag) goto done;
	um_vcmnvc(next,prev,vn);
	um_unitvc(vn,vn);
/*
.....if the next edge bndry of the same sf is along the previous, continue with
..... this sf
*/
	if(um_vcparall(vp,vn) == UU_TRUE)
	{
		um_vctovc(vn,vp);
		um_vctovc(next,prev);
		goto sf;
	}
/*
..... Get the surface normal at the edge endpoint
*/
 	dir = 0.;
	unflg = 1;
	uv[0] = uv[1] = 0;
	sfpt2(&e,prev,&uv[0],&uv[1],&unflg,&dir,&ptsf,&norm,&ierr);
/*
.....find a surface whose edge lies within tolerance to this edge.
*/
	for (j = 0; j < nkey; j++)
	{
//sprintf(sbuf,"j = %d",j);
//ud_wrerr(sbuf);
/*
.....do not consider a surface that has already been used in chaining
*/
		eval = 0;
		ncsf = Schain_sflist.cur_cnt / incsf;
		kyj = (UU_KEY_ID *) UU_LIST_ARRAY (&Schain_sflist);
		for (k = 0; k <ncsf; k=k+incsf)
		{
			if(ky[j] == kyj[k])
			{
				if (UD_chain_mod.mult_surf == 0) eval = 1;
				else
				{
					if (k > 1 && kyj[k-incsf] == kyj[ncsf-incsf]) eval = 1;
				}
				if (eval == 1) break;
			}
			if (k != 0 && ky[j] == kyj[ncsf-incsf]) eval = 1;
		}
		if (eval) continue;
/*
.....do not consider the first surface again for chaining
*/
		if(ky[j] == e.key) continue;
	 	sfj.key = ky[j];
		ncl_retrieve_data_fixed(&sfj);	
/*
.....consider this surface if it has a bndry edge endpoint within tol
..... of the bndry edge endpoint of the previous sf
*/	
/*
if (j == 29)
{
sprintf(sbuf,"j = %d",j);
ud_wrerr(sbuf);
}
*/
		um_free_boundary (&boundj);
		status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,&sfj,&boundj);
/*
if (j == 29)
{
sprintf(sbuf,"j = %d",j);
ud_wrerr(sbuf);
}
*/
		if (status != UU_SUCCESS) continue;
		nvj = boundj.np[0];
		ptsj = (UM_coord *) UU_LIST_ARRAY (boundj.cvpts);
		status = ncl_near_bndry_edge(nvj,ptsj,prev,spt,ept,&segnoj,pt2j);
		if(status != UU_SUCCESS) continue;
		if(um_dcccc(prev,pt2j)>Srftol) continue;
/*
..... Get the surface normal at the edge endpoint.
..... compare the 2 sf normalsif the change in angle is small select this
..... as the next sf.
*/
 		dir = 0.;
		uv[0]=uv[1] = 0;
		sfpt2(&sfj,pt2j,&uv[0],&uv[1],&unflg,&dir,&ptsf,&norm2,&ierr);
		ang = um_angle(norm,norm2);
		if(fabs(ang - UM_PI) >= 1.0e-2)
			if(ang> UM_PI/3) 
			{
				if(fabs(ang - UM_PI) >UM_PI/3)
				continue;
			}
		if(um_dcccc(ept,pt2j)<Srftol)
			segnoj++;
		if(segnoj == nvj-1)
		{
			if(um_dcccc(ptsj[0],ptsj[nvj-1])<UM_FUZZ)
				um_vcmnvc(ptsj[1],pt2j,v1);
			else
				um_vcmnvc(ptsj[0],pt2j,v1);
		}
		else
			um_vcmnvc(ptsj[segnoj+1],pt2j,v1);
		um_unitvc(v1,v1);
		if(segnoj == 0) 
		{
			if(um_dcccc(ptsj[0],ptsj[nvj-1])<UM_FUZZ)
				um_vcmnvc(ptsj[nvj-2],pt2j,v2);
			else
				um_vcmnvc(ptsj[nvj-1],pt2j,v2);
		}
		else
			um_vcmnvc(ptsj[segnoj-1],pt2j,v2);
		um_unitvc(v2,v2);
		proj = UM_DOT(v1, vn);
		if ( (1.0 - fabs(proj)) < UM_FUZZ) back = -1;
		else 
		{
			proj = UM_DOT(v2, vn);
			if ( (1.0 - fabs(proj)) < UM_FUZZ) back = 1;
			else continue;
		}
		segno = segnoj;
		um_move_boundary(&boundj,&bound);
		nv = bound.np[0];
		pts = (UM_coord *) UU_LIST_ARRAY (bound.cvpts);
		if (e.key == last && sfj.key != last) goto done;
		e.key = sfj.key;
		ncl_retrieve_data_fixed(&e);

		switch(back)
		{
		case 1:
			um_vctovc(v1,vp);
			if(segno == nv-1)	
			{
				if(um_dcccc(pts[nv-1],pts[0])<UM_FUZZ)
					segno = 1;
				else
					segno = 0;
			}
			else
				segno++;
			um_vctovc(pts[segno],prev);
			break;
		case -1 :
			um_vctovc(v2,vp);
			if(segno == 0)
			{
				if(um_dcccc(pts[nv-1],pts[0])<UM_FUZZ)
					segno = nv-2;
				else
					segno = nv-1;
			}
			else
				segno--;
			um_vctovc(pts[segno],prev);
			break;
		}
		fp = segno;
/*
........Push surface key
*/
		if (sfj.key == first && flag) goto done;
		uu_list_push(&Schain_sflist,&sfj.key);
/*
........Push curve number onto list
*/
		if (flag)
		{
			uu_free(eplst);
			S_get_endpts(&sfj,pts,nv,&eplst,&ncv);
			ipt = S_get_cvindex(eplst,ncv,back,segno);
			uu_list_push(&Schain_sflist,&ipt);
			lstpt = ipt;
/*
sprintf(sbuf,"EOL   key = %d, ipt = %d",sfj.key,ipt);
NclxDbgPstr(sbuf);
*/
		}
		if(sfj.key == first) goto done;
		if (sfj.key == last && !flag) goto done;
		goto sf;
	}
	um_vctovc(vn,vp);
	um_vctovc(next,prev);
	goto sf;
failed:
	retstat = UU_FAILURE;
done:
	if (!flag) nclu_list_in_buf();
	else uu_free(eplst);
exit:
	um_free_boundary (&boundj);
	um_free_boundary (&bound);
	uu_list_free (&keylst);
	if (flag)
		*sflist = Schain_sflist;
	else
		uu_list_free (&Schain_sflist);
	return(retstat);
}

/*********************************************************************
**    FUNCTION     : nclu_sf_chain ()
**       Chain select surfaces in select subsystem
**    PARAMETERS
**       INPUT  :	 
**			none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Adds surfaces to the global select buffer.
**    WARNINGS     : none
*********************************************************************/
nclu_sf_chain()
{
	int minseg, adjno, bndryno, inn2, status, i, ierr, nvj, ix, ic;
	int segno, minsf, irtn, eval, nkey, unflg = 1, ncsf, j, numint, k, *mask;
	int vectors,count, start_vseg;
	UM_PLOCREC pick, pickn;
	struct NCL_fixed_databag e, en, sfj;
	Gwpoint3 point, vector;
	UM_coord minpt, npt, pt2, cvint, ptsf, uvint, pt, ptn,*ptuvsj, *ptsj,ptext;
	UM_coord evpt, euv, suv, ept, spt, ptuv,ptuvn,adjpt,uvext,pkpt;
	UM_vector uevsrf, vpos, vc1, vc2, vec, vecn, vecsf, intvec, segvec;
	UM_vector usegdir, segdir, upos, adjvec, uadjvec, minvec,newvec;
	UU_REAL min , dist = 0.,tol5,tol10, u2, dir, v2, sign,du, dv, tol;
	UU_REAL d1,d2, d3,d4;
	UM_real8 tol8;
	struct UM_transf_rec tran;
	struct UM_evsrfout evsrf;
	UM_srf_boundary boundj;
	UU_LIST keylst;
	UU_KEY_ID first, last, *ky, *kyj;
	UM_angle ang,delang;
	UM_transf tf;

/*
.....Initialize routine
*/
	gettol (&tol8);
	tol = tol8;
	tol5 = tol * 5;
	tol10 = tol *10;
	ncl_set_boundary_toler (tol);

	um_nullvc (upos);
	um_nullvc (vpos);
/*
.....initialize the tolerance to the tolerance from the chain.mod file
.....convert the tolerance for the units used
*/
	UM_len_exttoint(UD_chain_mod.surftoler,Srftol);
	
	if(UD_chain_mod.direction == 3)
	{
		nclu_sfedge_chain(UU_NULL,UU_FALSE);
		return 0;
	}
	if(!UD_chain_mod.direction)
		vectors = 4;
	else
		vectors = UD_chain_mod.vectors;
	um_init_boundary(&boundj);
/*
.....Create a list of all surfaces displayed.
*/
	uu_list_init (&keylst,sizeof(UU_KEY_ID),100,100);
/*
.....Create a list of chain surfaces
*/
	uu_list_init (&Schain_sflist,sizeof(UU_KEY_ID),100,100);
/*
.....Limit geometry to surfaces, to get a list of all surfaces.
*/
	ud_lgeo(UU_TRUE, UD_ncl_netentity);
	status = nclu_alldisp_surf (&keylst);
	if (status == 0)
	{
		nkey = keylst.cur_cnt;
		ky = (UU_KEY_ID *) UU_LIST_ARRAY (&keylst);
	
	}
	ud_unlimit ();
	e.key = NULLKEY;
/*
.....Limit geometry selection to surfaces.
*/
srf1:;
	mask = (int *)UD_ncl_netentity;
	ud_lgeo(UU_TRUE, mask);
/*
.....Prompt user to select the first surface
*/
	ud_ldas(UD_DASPCKLOC, UM_MODEL, 140, &pick, 1,
            		&numint,1, UD_NODEFAULT);
/*
.....Get point on first surface
*/
	if (numint != 0 )
	{
		status = ncl_get_pkpt(&pick,&e,&pt,&vec,&ptuv);
		if (status != UU_SUCCESS)
		{
			ud_wrerr("Could not select surface boundary curve.");
			goto exit;
		}
	}
	else goto exit;
	first = e.key;
/*
.....Display all assist vectors in the +/- u/v direction from the point picked
.....on the surface.
*/
	tran.key = e.key;
	tran.rel_num = UM_TRANSFORM_REL;
	if (ur_retrieve_transf(&tran) == 0)
	{
   		uc_init_evsrfout (&e, &evsrf);
		uc_evsrf(UM_FRSTDERIV,ptuv[0],ptuv[1], &e, tran.tfmat, &evsrf);
		point.x = evsrf.sp[0];
		point.y = evsrf.sp[1];
		point.z = evsrf.sp[2];
		um_unitvc(evsrf.dsdu,uevsrf);
		vector.x = uevsrf[0];
		vector.y = uevsrf[1];
		vector.z = uevsrf[2];
		start_vseg = ud_assist_vector(point,vector);
		if(vectors >1)
		{
			delang = 2 * UM_PI / vectors;
			ang = 0;
			um_rottf(evsrf.snorm,delang,tf);
			count =vectors;
			while(count>1)
			{
				um_vctmtf(uevsrf,tf,uevsrf);
				vector.x = uevsrf[0];
				vector.y = uevsrf[1];
				vector.z = uevsrf[2];
				ud_assist_vector(point, vector);
				ang +=delang;
				count--;
			}
		}
/*
..... Prompt user to pick one of the assist vectors to determine direction.
*/
		ud_unlimit ();
		irtn = ud_pick_assist_seg("Pick an assist display segment:");	
		irtn = irtn-start_vseg+1;
		if(irtn < 1 || irtn > vectors) goto srf1;
		uu_list_push(&Schain_sflist,&e.key);
	}	
/*
.....Prompt user to select the last surface (optional)
*/
	ud_lgeo(UU_TRUE, mask);
	ud_ldas(UD_DASPCKLOC, UA_NCL, 501, &pickn, 1,
                  &numint,1, UD_NODEFAULT);

	if (numint != 0 ) 
	{
		ncl_get_pkpt(&pickn,&en,&ptn,&vecn,&ptuvn);
		if (status != UU_SUCCESS)
		{
			ud_wrerr("Could not select surface boundary curve.");
			goto exit;
		}
		last = en.key;
	}
/*
.....If last surface is not specified then assign the last surface as the 
.....first one.
*/
	else
		last = first;
/*
.....SET direction of the chaining according to the assist vector picked.
*/
	um_vctovc(evsrf.sp,pkpt);
	um_unitvc(evsrf.dsdu,uevsrf);
	ang = (irtn - 1) * delang;
	um_rottf(evsrf.snorm,ang,tf);
	um_vctmtf(uevsrf,tf,newvec);
	um_translate_point (evsrf.sp,tol10,newvec,ptext);
	dir = 0.;
	sfpt2(&e,ptext,&uvext[0],&uvext[1],&unflg,&dir,&ptsf,&vecsf,&ierr);	
/*
.....find the intersection on the surface boundary in this selected direction,
.....to determine adjacent surface.
*/	
	um_free_boundary (&boundj);
	status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,&e,&boundj,UU_NULL);	
	nvj = boundj.np[0];
	ptsj = (UM_coord *) UU_LIST_ARRAY (boundj.uvpts);
	status=nclu_pt_int_bndry_rot(&e,nvj,ptsj,ptuv,uvext,&uvint,&cvint,&intvec);
/*
..... Pick the a surface from the unibase whose boundary lies within 
..... tolerance to this spatial point.
*/
loop:
	min = 30.0;
	for (j = 0; j < nkey; j++)
	{
/*
.....do not consider a surface that has already been used in chaining
*/
		eval = 0;
		ncsf = Schain_sflist.cur_cnt;
		kyj = (UU_KEY_ID *) UU_LIST_ARRAY (&Schain_sflist);
		for (k = 0; k <ncsf; k++)
		{
			if(ky[j] == kyj[k])
			{
				if (UD_chain_mod.mult_surf == 0) eval = 1;
				else
				{
					if (k > 1 && kyj[k-1] == kyj[ncsf-1]) eval = 1;
				}
				if (eval == 1) break;
			}
			if (k != 0 && ky[j] == kyj[ncsf-1]) eval = 1;
		}
		if (eval) continue;
/*
.....do not consider the first surface again for chaining
*/
		if (UD_chain_mod.mult_surf == 0 && ky[j] == e.key) continue;
		if (ky[j] == e.key && ncsf == 1) continue;
/*
...... For every segment formed by the cv points of the boundary  determine if, 
...... it lies within tolerance of the point.
*/		
	 	sfj.key = ky[j];
		ncl_retrieve_data_fixed(&sfj);	
		um_free_boundary (&boundj);
		status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,&sfj,&boundj,UU_NULL);
		if (status == UU_SUCCESS)
		{
			nvj = boundj.np[0];
			ptsj = (UM_coord *) UU_LIST_ARRAY (boundj.cvpts);
			ptuvsj = (UM_coord *) UU_LIST_ARRAY (boundj.uvpts);
			for (i=0; i <= nvj -1; i++)
			{
				if(i == nvj-1)	um_vctovc(ptsj[0],ept);
              	else	um_vctovc(ptsj[i+1],ept);
				um_vctovc(ptsj[i],spt);
				um_vcmnvc(ept, spt, vc1);
				um_unitvc(vc1,vc2);
				um_nptln(cvint,spt,vc2,pt2);
				d1 = UM_DOT(cvint,vc2);
				d2 = UM_DOT(spt,vc2);
				d3 = UM_DOT(ept,vc2);
				if (d1 < d2)
					um_vctovc(spt,pt2);
				else if (d1 > d3)
					um_vctovc(ept,pt2);
				um_vcmnvc(cvint,pt2,npt);
				dist = um_mag(npt);	
				segno = i;
/*
.....find the closest segment to the point which is out of tolerance 
*/
				if(dist > Srftol)
				{
					if(dist<min) 
					{
						min=dist;
						minsf = sfj.key;
						minseg = i;
						um_vctovc(pt2,minpt);
						um_vctovc(vc2,minvec);
											 
					}
					continue;
				}
				u2 = ptuvsj[i][0];
				v2 = ptuvsj[i][1];
				dir = 0.;	
				sfpt2(&(sfj),pt2,&u2,&v2,&unflg,&dir,&ptsf,&vecsf,&ierr);
				goto found;
			}	
		}
	}
/*
.....if no segment is within tolerance then find the closest segment 
..... within one and a half times tolerance to the point
*/
	if (min < 1.5 * Srftol)
	{
		u2 = ptuvsj[minseg][0];
		v2 = ptuvsj[minseg][1];
		dir = 0.;
		sfj.key = minsf;
		status = ncl_retrieve_data_fixed(&sfj);
		segno = minseg;
		um_free_boundary (&boundj);
		status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,&sfj,&boundj,UU_NULL);
		nvj = boundj.np[0];
		ptsj = (UM_coord *) UU_LIST_ARRAY (boundj.cvpts);
		ptuvsj = (UM_coord *) UU_LIST_ARRAY (boundj.uvpts);
		um_vctovc(minpt,pt2);
		um_vctovc(minvec,vc2);
		sfpt2(&(sfj),minpt,&u2,&v2,&unflg,&dir,&ptsf,&vecsf,&ierr);
		goto found;
	}		
	goto done;
found:;
	if(sfj.key == first) 
		goto done;
/*
.....put this surface in the chaining list
*/
	uu_list_push(&Schain_sflist,&sfj.key);
	if (sfj.key == last)
		goto done;
		
/*
.....Determine if the point of intersection lies on either of the endpoints.
*/
	if(um_dcccc(cvint,spt) < tol)
	{
		if(segno == 0)
		{
			um_vctovc(ptsj[nvj-1],adjpt);
			adjno = nvj-1;
		}
		else 
		{
			um_vctovc(ptsj[i-1],adjpt);
			adjno = i-1;
		}
		um_vcmnvc(spt,adjpt,adjvec);
		um_unitvc(adjvec,uadjvec);
	}
	else 
		if(um_dcccc(cvint,ept) < tol) 
		{
			if(segno == nvj-2)
			{
				um_vctovc(ptsj[0],adjpt);
				adjno = 0;
			}
			else
			{
				um_vctovc(ptsj[i+2],adjpt);
				adjno = i+2;
			}
			um_vcmnvc(adjpt,ept,adjvec);
			um_unitvc(adjvec,uadjvec);
		}
		else 
		{
			um_vctovc(vc2,segvec);
			adjno = bndryno = segno;
		}
		
/*
.....determine which boundary segment is to be considered.
*/
	if(adjno != segno)
	{
		d4 = UM_DOT(uadjvec,intvec);
		d2 = UM_DOT(vc2,intvec);
		if(fabs(d2) > fabs(d4)) 
		{
			um_vctovc(uadjvec,segvec);
			bndryno = adjno;
		}
		else 
			bndryno = segno;
	}
	if (bndryno == nvj-1)
		um_vctovc(ptuvsj[0],euv);
	else
		um_vctovc(ptuvsj[bndryno+1],euv);
	um_vctovc(ptuvsj[bndryno],suv);
	
	um_vcmnvc(euv,suv,segdir);
	um_unitvc(segdir,usegdir);
	ptuv[0] = u2;
	ptuv[1] = v2;
	ptuv[2] = 0;
	e.key = sfj.key;
	if(UD_chain_mod.direction == 2)
	{
		um_perpvc_2d(segdir,upos);
		um_translate_point (ptuv,tol10,upos,uvext);
		inn2 = um_cshape_inschk1(ptuvsj,nvj,uvext,boundj.ummx,boundj.vmmx);	
		if (inn2 ==-1) 
		{
			um_negvc(upos,upos);
			um_translate_point (ptuv,tol10,upos,uvext);
		}
	}
	else 
		if(!UD_chain_mod.direction || !ncl_new_chain_vec(&sfj,nvj,ptuvsj,intvec,pt2,ptuv,segvec,uvext))
		{
			upos[0] = 1;
			vpos[1] = 1;
			upos[1] = 0;
			vpos[0] = 0;

			du = UM_DOT(segdir,upos);
			dv = UM_DOT(segdir,vpos); 
	/*
	.....determine the if the - or + side of the u/v direction is to be considered.
	*/		
			if (fabs(du) <= fabs(dv))
			{
				ix = 0;ic = 1;
				evpt[0] = u2 + tol5;
				evpt[1] = v2;
			}
			else
			{	
				ix = 1;ic = 0;
				evpt[0] = u2;
				evpt[1] = v2 + tol5;
			}
			inn2 = um_cshape_inschk1(ptuvsj,nvj,evpt,boundj.ummx,boundj.vmmx);	
			if (inn2 ==-1) sign = -1.0;
			else sign = 1.0;
		
/*
.....find the intersection on the surface boundary in this selected direction,
....to determine adjacent surface.
*/
			status=nclu_pt_int_bndry(&sfj,nvj,ptuvsj,boundj.ummx,boundj.vmmx,ptuv,ic,ix,sign,&uvint,&cvint,&intvec,0);
			if(status) goto done;
			else goto loop;
		}
	status=nclu_pt_int_bndry_rot(&sfj,nvj,ptuvsj,ptuv,uvext,&uvint,&cvint,&intvec);

	if(status) goto done;
	else goto loop;


done:
	nclu_list_in_buf();
exit:
	um_free_boundary (&boundj);
	uu_list_free (&keylst);
	uu_list_free (&Schain_sflist);
	uu_dexit;
	return 0;
}

/*********************************************************************
**    S_FUNCTION     :  ncl_get_pkpt(pick,eptr,pt,vec,uv)
**       Calculate the coordinates of the
**       point on a surface from a screen location pick.
**    PARAMETERS
**       INPUT  :
**			pick	- First pick point.
**       OUTPUT :
**			eptr	- Fixed data of entity picked.
**			pt		- Point on the surface.
**			vec		- Tangent/Normal vector of teh surface at pick point.
**			uv		- U and V value of point on the surface.
**    RETURNS      : UU_SUCCESS if the surface point was calculated.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_pkpt(pick,eptr,pt,vec,uv)
UD_PLOCREC *pick;
struct NCL_fixed_databag *eptr;
UM_coord pt;
UM_vector vec;
UU_REAL uv[2];
{
   UM_coord pkpt;
   UM_vector vpnorm;
   UU_REAL tol;
   UM_PICKENT pent;
	int status;
/*
.....Initialize routine
*/
/*   tol = 0.0008; */
	gettol(&tol);
/*
.....Calculate viewport normal
*/
   um_vpnorm(pick->pndc.transform,vpnorm);
/*
.....Project pick locations onto view plane
*/
   gndcw3(&pkpt[0],&pkpt[1],&pkpt[2],
      pick->pndc.cord[0],pick->pndc.cord[1],pick->pndc.cord[2]);
/*
.....Get picked entity
*/
   um_d_pickresolve(&(pick->ppath),1,&pent);
   eptr->key = um_get_pickkey(&pent,1);
   ncl_retrieve_data_fixed(eptr, sizeof(*eptr));
/*
.....Project picked point onto entity
*/
    status = ncl_pickptosf(eptr, pkpt, vpnorm, tol, UU_FALSE, pt, uv, vec);
	return(status);
}

/*********************************************************************
**    FUNCTION     : nclu_alldisp_surf (keylst)
**       Calls routines analyze surface primitive type , returns all the
**			 surfaces in the unibase
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          keylst = list of keys of all  surafaces
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_alldisp_surf (keylst)
UU_LIST *keylst;
{
	UG_segstli *p;
	int status;
	UU_KEY_ID key;
	struct NCL_fixed_databag sf;

	ug_seginitscan();

	while( (p=ug_segscan()) != NULL ) 
	{
		if ((p->segatts.gvis == UG_VISIBLE) && (p->segatts.gdtect == UG_DETECTABLE))
		{
			status = ud_glpick_alldisp (p->segid,&key);
			if (status == 0)
			{
				sf.key = key;
				status = ncl_retrieve_data_fixed(&sf);
				if (status == 0)
					uu_list_push (keylst,&key);
			}
		}
	}

	return(UU_SUCCESS);
}

/*********************************************************************
**    FUNCTION     : nclu_pt_int_bndry(e,nv,pts,ummx,vmmx,ptuv,ic,ix,sign,uvint,cvint, intvec,rotflg)
**			Returns the point of intersectin of the boundary of the surface
**			and the vector from the point on surface.
**    PARAMETERS
**       INPUT  :
**			 	e		 : key id of surface
**				nv		 : number of boundary points
**				pts		 : boundary points
**				ummx	 : u min/max of boundary
**				vmmx	 : v min/max of bpundary
**				ptuv	 : point on the surface
**				ic		 : axis along which the vector is constant
**				ix	    : axis along which the vector changes
**				sign	 : direction of the axis
**				rotflg	 : 0 called form nclu_sf_chain
**						   1 called from nclu_pt_int_bndry_rot
**			OUTPUT :
**				uvint	 : uv coordinates of the point of intersection
**				cvint  : xyz coordinates of the point of intersection
** 				intvec : vector at the point of intersection
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_int_bndry(e,nv,pts,ummx,vmmx,ptuv,ic,ix,sign,uvint,cvint, intvec,rotflg)
struct NCL_fixed_databag *e;
int nv;
UM_coord pts[],ummx,vmmx;
UM_coord ptuv, uvint, cvint, intvec;
int ic, ix,rotflg;
UU_REAL sign;
{
	int inn1, boundary_shift = 0, i, minflag = 0;
	UM_coord  seg1, seg2, npt;
	UM_real8 tol8;
	UU_REAL diff1[2], diff2[2], minint, tol;
	UU_REAL diff, intsec;
	struct UM_transf_rec tran;
	struct UM_evsrfout evsrf;

	if(sign == 1.0) 
		if(rotflg) minint = 1000;
			else minint = 1.0;
	else
		if(rotflg) minint = -1000;
			else minint = 0.0;
 
	gettol (&tol8);
	tol = tol8;
	
/*
......Get the outer boundary of the surface as a list of boundary coordinates
......Consider line segments formed by the consecutive points and determine 
......if they intersect the selected axis.
*/
	{
/*
.....We consider only the outer boundary of the suraface , hence np[0]
*/
		for (i=0; i <= nv -1; i++)
		{
/*
......For the last point in the boundary form a segment with the first point
*/
			if(i == nv-1)
				um_vctovc(pts[0],seg2);
			else
				um_vctovc(pts[i+1],seg2);
			um_vctovc(pts[i],seg1);					

/*
..... Calculate the difference in the u and v direction of the end and
..... start point of the segemnt from the selected point on surface.
*/
			diff1[ic] = ptuv[ic] - seg1[ic];
			diff2[ic] = ptuv[ic] - seg2[ic];
			diff1[ix] = ptuv[ix] - seg1[ix];
			diff2[ix] = ptuv[ix] - seg2[ix];

			if(boundary_shift ==1)
			{
				if ((((sign == 1.0) && (diff2[ix] < tol)) ||
					  ((sign == -1.0) && (diff2[ix] > sign * tol))) &&
					    (fabs(diff2[ic]) < 2 *tol)) 
				{
					ptuv[0] = seg2[0];
					ptuv[1] = seg2[1];
					continue;
				}
				else	
				{
					npt[ic] = seg1[ic];
					if (sign == 1.0)
						npt[ix] = seg1[ix] + (5 * tol);
					else
						npt[ix] = seg1[ix] - (5 * tol);
					inn1 = um_cshape_inschk1(pts,nv,npt,ummx,vmmx);
					switch(inn1)
					{
						case -1:
							um_vctovc(seg1,uvint);
							minflag = 1;
							goto endloop;
						case 0 :
						case 1 :
							um_vctovc(seg1,ptuv);
							boundary_shift = 0;
							continue;
							break;
					}	
				}	
			}
/*
.....Consider the segment only if  the endpoints are on either side of the
.....selected axis.
*/
			if((diff1[ic] > 0 && diff2[ic] > 0) || 
				(diff1[ic] < 0 && diff2[ic] < 0))  continue;

/*
.....for the segment to intersect the axis atleast one of the end points must 
.....lie on the positive/negative side.
*/
			if((diff1[ix] > tol && diff2[ix] > tol && sign == 1.0) || 
				(diff1[ix] < -1.0*tol && diff2[ix] < -1.0*tol && sign == -1.0)) 
				continue;

/*
.....Find the point of intersection of the segment and the  axis only if the
.....slope of the segment is not below a predifined value
*/
			if(fabs(seg1[ic] - seg2[ic]) > UM_DFUZZ)
			{
				if(fabs(seg1[ix] - seg2[ix]) < tol )
					intsec = seg1[ix];
				else
					intsec =((seg1[ix] - seg2[ix]) * (ptuv[ic] - seg2[ic])/  
							(seg1[ic] - seg2[ic])) + seg2[ix];
					
			} 
/* 
.....If both the end points lie within tolerance then we check for 2 cases:
.....1. If both the end points lie on the positive section of the axis 
.....		then
.....			If seg2 is closer to the ptuv then consider seg1 as intsec
.....       else shift ptuv on boundary.
.....2. If one lies on the positive and the other lies on the negative side
.....   of the axis
.....		then
.....			If seg2 lies on the positve side then shift ptuv on boundary 
.....			else consider seg 1 as intvec. 	
*/
				if (((sign == 1.0) && (fabs(diff1[ic]) < tol) && 
					  (fabs(diff2[ic]) < tol)) ||
					 ((sign == -1.0) && (fabs(diff1[ic]) < tol) &&
                 (fabs(diff2[ic]) < tol)))		
				{
					if(((sign == 1.0) && (diff1[ix] <=  0) && (diff2[ix] <= 0)) ||
						((sign == -1.0) && (diff1[ix] >=  0) && (diff2[ix] >= 0)))
					{
						if(fabs(diff1[ix]) <= fabs(diff2[ix]))
						{
							boundary_shift = 1;
							um_vctovc(seg2,ptuv);
							continue;
						}					
						else 
							intsec = seg1[ix];
					}
					else
					{
						if(((sign == 1.0) && (diff1[ix] > 0)) ||
							((sign == -1.0) && (diff1[ix] < 0)))
						{
							boundary_shift = 1;
							um_vctovc(seg2,ptuv);
							continue;
						}
						else
						{	
							intsec = seg1[ix];
						}
					}
				}
/*
..... If just one of the end points lies within tolerance then we consider this 
..... one as the point of intersection or shift boundary depending on the 
..... direction of the boundary.
*/
				else if((fabs(diff1[ic]) < tol) &&
						  (((sign == 1.0) && (diff1[ix] <= tol)) ||
						   ((sign == -1.0) && (diff1[ix] >= -1.0*tol))))
						 intsec = seg1[ix];
					  else if((fabs(diff2[ic]) < tol) && 
						  (((sign == 1.0) && (diff2[ix] <= tol)) ||
						   ((sign == -1.0) && (diff2[ix] >= -1.0 * tol)))) 
					  {
							boundary_shift = 1;
							um_vctovc(seg2,ptuv);
							continue;
					  }
/*
..... If the point of intersection is within tolerance of the initial point,
..... then we ignore this boundary segment and go to the next.
*/
            diff = intsec- ptuv[ix];
            if (((sign == 1.0) && (diff < tol)) ||
                ((sign == -1.0) && (diff > sign * tol))) continue;
/*
.....check if the point of intersection is outside the uv bound
*/
				if(((sign == 1.0) && ((minint + tol)>= intsec)) ||
					((sign == -1.0) && ((minint - tol)<= intsec)))
  				{
					minint = intsec;
					minflag = 1;
				}
		}
/*
..... uvint is the uv point of intersection of the surface boundary.
*/
      uvint[ix] = minint;
      uvint[ic] = ptuv[ic];
		if ((((sign == 1.0) && ( uvint[ix] == 1.0)) ||
				((sign == -1.0) && ( uvint[ix] == 0.0)))
 				&& (minflag == 0)) 
			goto end;
endloop:;
	}
/*
..... Get the spatial coordiantes of the intersecting point on the surface
*/
	if(rotflg ==0)
	{
 		tran.key = e->key;
 		tran.rel_num = UM_TRANSFORM_REL;
 		if (ur_retrieve_transf(&tran) == 0)
		{
    		uc_init_evsrfout (e, &evsrf);
			uc_evsrf(UM_FRSTDERIV,uvint[0], uvint[1], e, tran.tfmat, &evsrf);
			um_vctovc(evsrf.sp,cvint);
			if (ic)
			{	
				intvec[0] = sign * evsrf.dsdu[0];
				intvec[1] = sign * evsrf.dsdu[1];
				intvec[2] = sign * evsrf.dsdu[2];
  			}
			else
			{
				intvec[0] = sign * evsrf.dsdv[0];
				intvec[1] = sign * evsrf.dsdv[1];
				intvec[2] = sign * evsrf.dsdv[2];
			}
		}
	}
end:;
	if ((((sign == 1.0) && ( uvint[ix] == 1.0)) ||
      ((sign == -1.0) && ( uvint[ix] == 0.0)))
      && (minflag == 0))
		return 1;
	else
		return 0;
}

/*********************************************************************
**
**    E_FUNCTION        :  nclu_list_in_buf()
**       all surfaces in UD_chain_list in select subsystem
**
**    PARAMETERS
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/
nclu_list_in_buf()
{
	int i,segid,baseptr,ncsf;
	UU_KEY_ID *kychlst;
/*
.....Put all surface entity type select handler in select subsystem.
*/
	ncsf = Schain_sflist.cur_cnt;
	kychlst = (UU_KEY_ID *) UU_LIST_ARRAY (&Schain_sflist);	
/*
.....Add surfaces to the select buffer
*/
	for (i=0;i<ncsf;i++)
	{
		ur_retrieve_disp_segid(kychlst[i],&segid);
		baseptr = UD_Select_ptr;
		UD_Select_buf[UD_Select_ptr] = 2;
		UD_Select_buf[UD_Select_ptr+1] = segid;
		UD_Select_buf[UD_Select_ptr+2] = kychlst[i];
		UD_Select_ptr = UD_Select_ptr + 3;
		UD_Select_cnt++;
		ud_spstat(UU_FALSE,baseptr);
	}
	return 0;
}

/*********************************************************************
**
**    E_FUNCTION        :  ncl_new_chain_vec(sfj,nv,pts,intvec,pt2,ptuv,segvec,uvext)
**       get the direction of the uv direction on the new surface to be chained
**
**    PARAMETERS
**       INPUT  :  
**				sfj		:	pointer to the new surface
**				nv		:	no of uv boundary points
**				pts		:   uv boundary points
**				intvec	:	vector in the chaining direction for the previous surface
**				pt2		:   point on the new surface
**				ptuv	:   uv point on the new surface
**				segvec	:	vector in the direction of the boundary segment 
**       OUTPUT :  
**				uvext	:   point within tol5 in he new direction
**    RETURNS      : 0 : use uv direction
**				     1 : use new direction
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/
int ncl_new_chain_vec(sfj,nv,pts,intvec,pt2,ptuv,segvec,uvext)
struct NCL_fixed_databag *sfj;
int nv;
UM_vector intvec;
UM_coord pts[],pt2,ptuv,uvext;
UM_vector segvec;
{
	int inn1,inn2,unflg = 1, status, ierr ;
	UM_vector uintvec, uevsrf, vevsrf, newvec,newvec1,newvec2, vecsf;
	UM_coord uvext1,uvext2,ptsf, ptext2,ptext1;
	struct UM_transf_rec tran;
	struct UM_evsrfout evsrf;
	UU_REAL d1,d2,dir, tol,tol10,du,dv,um_angle2p_acc();
	UM_angle alpha;
	UM_transf matrix;
	UM_real8 tol8;
	UM_2Dcoord ummx,vmmx;

	gettol (&tol8);
	tol = tol8;
	tol10 = tol * 10;

/*
.....unit vector along direction of chaining
*/
	um_vctovc(intvec,uintvec);

/*
.....xyz vectors along the u and v direction at the point on the next surface
*/
	tran.key = sfj->key;
	tran.rel_num = UM_TRANSFORM_REL;
	status = ur_retrieve_transf(&tran);
   	uc_init_evsrfout (sfj, &evsrf);
	uc_evsrf(UM_FRSTDERIV,ptuv[0],ptuv[1], sfj, tran.tfmat, &evsrf);
	um_unitvc(evsrf.dsdu,uevsrf);
	um_unitvc(evsrf.dsdv,vevsrf);
	du = UM_DOT(uintvec,uevsrf);
	dv = UM_DOT(uintvec,vevsrf);
/*
.....If the old direction vector lies along u or v of the new surface
.....u/v is used as the new direction vector otherwise we calculate the new direction
*/
	if ((fabs(1.0 - fabs(du)) < tol)  ||  (fabs(0.0 - fabs(du)) < tol))
		return 0;
/*
..... we caculate the new vector such that the angle made by this vector and the 
.....boundary edge and the angle made by the the old vector and the boundary 
.....edge add up to 180 degrees.
*/

	alpha = um_angle(uintvec,segvec);
/*
.....rotate the edge to get the new vector
*/
	um_rotlntf(evsrf.sp, evsrf.snorm, alpha, matrix);
	
	um_vctmtf(segvec,matrix,newvec1);
/*
...... step out in the direction of this vector to get a point on this vector
*/

/*
.....project this point on to the surface to get the new uv direction
*/
	um_translate_point (evsrf.sp,tol10,newvec1,ptext1);
	um_vctovc(ptuv,uvext1);
	dir = 0.;
	sfpt2(sfj,ptext1,&uvext1[0],&uvext1[1],&unflg,&dir,&ptsf,&vecsf,&ierr);
	um_cshape_dirchk1 (pts,nv,ummx,vmmx);
	inn1 = um_cshape_inschk1(pts,nv,uvext1,ummx,vmmx);
	
/*
.....rotate the edge to get the new vector in the other direction
*/
	um_rotlntf(evsrf.sp, evsrf.snorm, -alpha, matrix);
	um_vctmtf(segvec,matrix,newvec2);
/*
..... step out in the direction of this vector to get a point on this vector
.....project this point on to the surface to get the new uv direction
*/
	um_translate_point (evsrf.sp,tol10,newvec2,ptext2);
	um_vctovc(ptuv,uvext2);
	dir = 0.;
	sfpt2(sfj,ptext2,&uvext2[0],&uvext2[1],&unflg,&dir,&ptsf,&vecsf,&ierr);
	inn2 = um_cshape_inschk1(pts,nv,uvext2,ummx,vmmx);
	
	if (inn1 == 1 && inn2 == 1) 
	{
		d1 = um_dist_2d(ptuv,uvext1);
		d2 = um_dist_2d(ptuv,uvext2);
		if (d1 > d2) goto first;
		else goto second;
	}
	else 
		if (inn1 == 1) goto first;
		else
			if (inn2 == 1) goto second;
			else  if(inn1==0) goto first;
				  else if (inn2 == 0) goto second;
					   else goto none;

first:
	um_vctovc(newvec1,newvec);
	um_vctovc(uvext1,uvext);
	goto done;
second:
	um_vctovc(newvec2,newvec);
	um_vctovc(uvext2,uvext);
	goto done;
none:
	return 0;

done:
	return 1;	
}

/*********************************************************************
**    FUNCTION     : nclu_pt_int_bndry_rot(e,nv,pts,ptuv,uvext,uvint,cvint, intvec)
**			Returns the point of intersectin of the boundary of the surface
**			and the vector from the point on surface.
**    PARAMETERS
**       INPUT  :
**			 	e		 : key id of surface
**				nv		 : number of boundary points
**				pts		 : boundary points
**				ptuv	 : point on the surface
**				uvext	 :  point within tol5 in the new direction
**			OUTPUT :
**				uvint	 : uv coordinates of the point of intersection
**				cvint  : xyz coordinates of the point of intersection
** 				intvec : vector at the point of intersection
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_int_bndry_rot(e,nv,pts,ptuv,uvext,uvint,cvint, intvec)
struct NCL_fixed_databag *e;
int nv;
UM_coord pts[];
UM_coord ptuv, uvext,uvint,cvint, intvec;
{
	int i, status;
	UU_REAL sign =1.0, alpha,beta , um_angle_2d();
	UM_2Dtransf matrix;
	UM_2Dcoord ummx, vmmx,axis;
	struct UM_transf_rec tran;
	struct UM_evsrfout evsrf;

/*
.....calculate the angel between the new vector and + u axis
.....rotate this newvector to u axis.
.....Use the same rotation on the boundary points.
*/
	axis[0] = -1;
	axis[1] = 0;
	if ((uvext[0] - ptuv[0]) == 0)
		if (uvext[1] > ptuv[1]) alpha = UM_PI /2;
		else alpha = -UM_PI/2;
	else
	{
		
		alpha = atan ((uvext[1] - ptuv[1])/(uvext[0] - ptuv[0]));
		if( uvext[0] < ptuv[0]) 
		{
			sign = -1.0;
			beta = -alpha;
		}
	}
	um_rottf_2d(ptuv,-alpha,matrix);
	
	for (i=0; i<nv; i ++)
		um_cctmtf_2d(pts[i],matrix,pts[i]);

	um_cshape_dirchk1 (pts,nv,ummx,vmmx);
	status = nclu_pt_int_bndry(e,nv,pts,ummx,vmmx,ptuv,1,0,sign,uvint,cvint,intvec,1);
	um_rottf_2d(ptuv,alpha,matrix);
	um_cctmtf_2d(uvint,matrix,uvint);

/*
..... Get the spatial coordiantes of the intersecting point on the surface
*/
 	tran.key = e->key;
 	tran.rel_num = UM_TRANSFORM_REL;
	status = ur_retrieve_transf(&tran);
 	if (status == UU_SUCCESS)
	{
    	uc_init_evsrfout (e, &evsrf);
		uc_evsrf(UM_FRSTDERIV,uvint[0], uvint[1], e, tran.tfmat, &evsrf);
		um_vctovc(evsrf.sp,cvint);	
		for (i=0; i<3; i++)
			if (sign == -1.0 )
				intvec[i] = cos(UM_PI + alpha) * evsrf.dsdu[i] + sin(UM_PI+alpha)*evsrf.dsdv[i];
			else
				intvec[i] = cos(alpha) * evsrf.dsdu[i] + sin(alpha)*evsrf.dsdv[i];
	}	

	return status;
}

/*********************************************************************
**    S-FUNCTION   : S_get_endpts(tsf,pts,npts,epts,ncv)
**			Returns the indexes into the boundary array of where each subcurve
**       begins.
**    PARAMETERS
**       INPUT  :
**			 	tsf  	 : Trimmed surface that contains boundary curve.
**			 	pts  	 : Boundary points.
**			 	npts 	 : Number of boundary points;
**			OUTPUT : 
**          epts   : Indexes into boundary points array for start of
**                   each subcurve.
**          ncv    : Number of subcurves in outer boundary.
**    RETURNS      : none
**    SIDE EFFECTS : 
**          The calling routine must free the memory allocated in 'epts'.
**    WARNINGS     : none
*********************************************************************/
static void S_get_endpts(tsf,pts,npts,epts,ncv)
struct NCL_trimsf_rec *tsf;
UM_coord *pts;
int npts;
int **epts;
int *ncv;
{
	int i,j,*tmp;
	UM_coord pt1,pt2;
	UM_transf tfmat;
	struct UC_entitydatabag crv;
	struct NCL_fixed_databag e;
	struct UM_compcrv_rec *comp;
/*
.....Get outer boundary curve
*/
	crv.key = tsf->cv_key;
	ncl_retrieve_data(&crv);
	uc_retrieve_transf(tsf->key,tfmat);
/*
.....Composite curve
.....Get start of each subcurve
*/
	if (crv.rel_num == UM_COMPCRV_REL)
	{
		comp = (struct UM_compcrv_rec *)&crv;
		tmp = (int *)uu_malloc(sizeof(int)*comp->no_cid);
		tmp[0] = 0;
		*epts = tmp;
		*ncv = comp->no_cid;
		for (i=1;i<comp->no_cid;i++)
		{
			e.key = comp->cid[i].crvid;
			ncl_retrieve_data_fixed(&e);
			um_get_endpts(&e,tfmat,pt1,pt2);
			if (comp->cid[i].reverse) um_vctovc(pt2,pt1);
			tmp[i] = 0;
			for (j=tmp[i-1]+1;j<npts;j++)
			{
				if (um_dcccc(pts[j],pt1) <= Srftol)
				{
					tmp[i] = j;
					break;
				}
			}
		}
	}
/*
.....Single curve
*/
	else
	{
		tmp = (int *)uu_malloc(sizeof(int));
		tmp[0] = 0;
		*epts = tmp;
		*ncv = 1;
	}
}

/*********************************************************************
**    FUNCTION     : S_get_cvindex(bncrv,indx,dir,segno)
**			Returns the boundary subcurve number where the point index resides.
**    PARAMETERS
**       INPUT  :
**			 	bncrv	 : Indexes to start of each boundary curve.
**			 	ncv  	 : Number of boundary subcurves.
**			 	dir  	 : 1 = Travelling forward along curve, -1 = Back.
**				segno	 : index of input point to match to boundary subcurve
**			OUTPUT : none
**    RETURNS      : Curve number that the 'indx' point resides on.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_cvindex(bncrv,ncv,back,segno)
int *bncrv,ncv,back,segno;
{
	int i,ipt;
/*
.....Find curve that point index lies on
*/
	ipt = ncv - 1;
	for (i=1;i<ncv;i++)
	{
		if ((segno <= bncrv[i] && back == 1) ||
			(segno < bncrv[i] && back == -1))
		{
			ipt = i - 1;
			if (ipt == -1) ipt = ncv - 1;
			break;
		}
	}
	return(ipt);
}

