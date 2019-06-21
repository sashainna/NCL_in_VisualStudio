/*********************************************************************
**    NAME         :  m3ecomp1.c
**   NOTE - This file contains routines which are used by both NCL & IGES.
**       CONTAINS: composite curve support routines
**           um_compcrv_connect
**           um_update_norm
**           um_compcrv_deloop
**           umf_offset_compcrv_comps
**           um_offset_compcrv_comps
**
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ecomp2.c , 25.1 
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:52 
*********************************************************************/
#include "usysdef.h"
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
#include "mfort.h"
#include "nccs.h"
#include "tzmdrel.h"
#include "misect.h"
#include "nconst.h"
#include "nclmplay.h"
#include "mdpick.h"
#include "mxxx.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclvx.h"
#include "nclupok.h"
#include "vsegbf.h"

#include "mgeom.h"

/*********************************************************************
**   E_FUNCTION : void um_compcrv_connect(ikeys,lrevs,nkeys,tol,type,
**          okeys,use_lrevs,cut_int,conpt,use_conpt,firstpt,firstfl)
**		 Connect two curves using either a smooth (spline) connection,
**     a chamfer connection or a linear connection.  The linear
**     connection will extend lines to meet the other curve or the
**     extension of the other curve if possible.  Smooth and chamfer
**     will join the ends of the curves directly.  Use lrevs if you
**     know the ends to join and will not be needing conpt.
**   PARAMETERS   
**      INPUT  : 
**         ikeys     - curve key list
**         lrevs     - reverse flag list
**         nkeys     - number of keys in ikeys
**         tol       - tolerance for curve evaluation
**         type      - connection type
**                     0: Linear
**                     1: Smooth
**                     2: Chamfer
**         use_lrevs - Denotes how connection points are determined
**                     UU_TRUE : Connect end of ikeys[0] to start of
**                               ikeys[1]
**                     UU_FALSE: Connect closest ends
**         cut_int   - UU_TRUE : Trim curves that intersect
**                     UU_FALSE: Do not trim curves
**         conpt     - point on ikeys[0] not used yet
**         use_conpt - UU_TRUE : Use conpt to ensure no end point is
**                               used twice
**                     UU_FALSE: Ignore conpt
**         firstpt   - Starting point of curve to use when connecting
**                     ends of curve
**         firstfl   - -1: Use firstpt to close curve
**                      0: Ignore firstpt
**                      1: Set firstpt
**      OUTPUT :  
**         okeys     - key list for added connecting curves
**         conpt     - point on ikeys[1] not used yet
**         firstpt   - Starting point of curve
**   RETURNS      : UU_SUCCESS iff successful (connection added/made)
**   SIDE EFFECTS : none
**   WARNINGS     : none
*********************************************************************/
int um_compcrv_connect(ikeys,lrevs,nkeys,tol,type,okeys,
	use_lrevs,cut_int,conpt,use_conpt,firstpt,firstfl)
UU_KEY_ID ikeys[3],okeys[2];
UU_LOGICAL lrevs[2],use_lrevs,cut_int,use_conpt;
UU_REAL tol;
UM_coord conpt,firstpt;
int nkeys,type,firstfl;
{
	int k,status,n1,ix,rel1,rel2,dsegid,ival,nint=0,ind,ind1,ind2;
	struct UM_crvdatabag comp1,comp2;
	struct UM_line_rec ln1,ln2;
	struct UM_circle_rec circ;
   struct UM_rbsplcrv_rec rcrv;
	struct UM_evcrvout evcrv;
	struct NCL_nclpv_rec pv1,pv2;
	UM_coord ipt,ipt1,ipt2,cpt11,cpt12,cpt21,cpt22,*pp,sopt,flippt,tpt1,tpt2;
	UM_vector itv1,itv2,tv11,tv12,tv21,tv22,*vv,tvec1,tvec2;
	UM_transf tfmat1,tfmat2;
	UU_REAL u1,u2,u3,dis1,dis2,dis3,dis4,tolr=1.e-3;
	UU_REAL tdis,dmin1,dmin2,umin=0.001;
	UU_LIST seglist,cvpoint,cvtang;
	struct NCL_crvgen_rec seg, *segp;
	UM_param inpara1,inpara2;
	UM_isect ibuff[UM_MAXISECT];
	UU_LOGICAL use_u3,use_spt,use_ept,swapped,is_spt,ext1,ext2;
	UM_int2 ierr;
	char sbuf[80];

	okeys[0] = 0;
	okeys[1] = 0;
	use_u3 = use_spt = use_ept = swapped = UU_FALSE;
	comp1.key = ikeys[0];
	ncl_retrieve_data_fixed(&comp1);
	uc_retrieve_transf(ikeys[0],tfmat1);
	um_get_endpts(&comp1,tfmat1,cpt11,cpt12);
	if (use_conpt)
		is_spt = (um_dcccc(cpt11,conpt) < UM_DFUZZ);
	else
		is_spt = UU_FALSE;
/*
.....Determine which ends of curves to keep if they are trimmed.
*/
	if (use_lrevs)
	{
		inpara1 = (lrevs[0])? 0. : 1.;
		inpara2 = (lrevs[1])? 1. : 0.;
	}
	else
	{
		inpara1 = (is_spt)? 0. : 1.;
		inpara2 = 0.;
		if (nkeys == 3)
		{
			comp1.key = ikeys[1];
			ncl_retrieve_data_fixed(&comp1);
			uc_retrieve_transf(ikeys[1],tfmat1);
			comp2.key = ikeys[2];
			ncl_retrieve_data_fixed(&comp2);
			uc_retrieve_transf(ikeys[2],tfmat2);
			nint = 0;
			ncl_isect_3d(&comp1,tfmat1,&comp2,tfmat2,UU_FALSE,ipt,&nint,
				ibuff,&ierr);
/*
.....Keep the end closest to the other neighbor of ikeys[1]
*/
			if (nint <= 0)
			{
				um_get_endpts(&comp1,tfmat1,cpt11,cpt12);
				um_get_endpts(&comp2,tfmat2,cpt21,cpt22);
				dis1 = um_dcccc(cpt11,cpt21);
				dis2 = um_dcccc(cpt11,cpt22);
				dis3 = um_dcccc(cpt12,cpt21);
				dis4 = um_dcccc(cpt12,cpt22);
				
				if ((dis1 <= dis3 && dis1 <= dis4)|| 
					(dis2 <= dis3 && dis2 <= dis4))
					inpara2 = 1.;
				else
					inpara2 = 0.;
			}
			else
			{
				use_u3 = UU_TRUE;
				u3 = 10000.;
				for (k=0;k<nint;k++)
				{
					um_cctou(&comp1,tfmat1,ibuff[k].pt,&ibuff[k].u1,&dis1);
					if (dis1 > tol)
						continue;
					if ((u3 > 0. && ibuff[k].u1 < u3)||
						(u3 < 0. && ibuff[k].u1 > u3))
						u3 = ibuff[k].u1;
				}
				if (u3 <= 0.)
					inpara2 = 1.;
				else
					inpara2 = 0.;
			}
		}
	}
/*
.....Check for intersection
*/
	if (cut_int)
	{
		comp1.key = ikeys[0];
		ncl_retrieve_data_fixed(&comp1);
		uc_retrieve_transf(ikeys[0],tfmat1);
		um_get_endpts(&comp1,tfmat1,cpt11,cpt12);
		comp2.key = ikeys[1];
		ncl_retrieve_data_fixed(&comp2);
		uc_retrieve_transf(ikeys[1],tfmat2);
		ierr = 0;
		nint = 0;
		ncl_isect_3d(&comp1,tfmat1,&comp2,tfmat2,UU_FALSE,ipt,&nint,
			ibuff,&ierr);
		dis2 = 1.e25;
		u1 = u2 = 0.;
		if (ierr == 0 && nint > 0)
		{
			if (is_spt) um_vctovc(cpt12,ipt);
			else um_vctovc(cpt11,ipt);
			for (k=0;k<nint;k++)
			{
				um_cctou(&comp1,tfmat1,ibuff[k].pt,&ibuff[k].u0,&dis1);
				um_cctou(&comp2,tfmat2,ibuff[k].pt,&ibuff[k].u1,&dis1);
				dis1 = um_dcccc(ipt,ibuff[k].pt);
				if (dis1 < dis2)
				{
					if (0. < ibuff[k].u0 && 0. < ibuff[k].u1 &&
					ibuff[k].u0 < 1. && ibuff[k].u1 < 1.)
					{
						dis2 = dis1;
						u1 = ibuff[k].u0;
						u2 = ibuff[k].u1;
						um_vctovc(ibuff[k].pt,ipt);
					}
				}
			}
			if (u1 > 0.)
			{
/*
.....Keep larger section of first curve if making closed curve
*/
				if (firstfl == 1 && ((u1 <= 0.5 && inpara1 == 1.) ||
					(u1 > 0.5 && inpara1 == 0.)))
					inpara1 = 1. - inpara1;
				if (fabs(u1) > umin && fabs(1.- u1) > umin)
					uc_trim_extend_curve(&comp1,tfmat1,ipt,&u1,inpara1);
				if (use_u3 && nkeys == 3)
				{
					if (u2 < u3 && u3 < 10000.) inpara2 = 0.;
					else inpara2 = 1.;
				}
				if (fabs(u2) > umin && fabs(1.- u2) > umin)
					uc_trim_extend_curve(&comp2,tfmat2,ipt,&u2,inpara2);
				if (firstfl == 1)
				{
					if (inpara1 == 1.)
						um_vctovc(cpt11,firstpt);
					else
						um_vctovc(cpt12,firstpt);
				}
				if (!use_lrevs)
				{
					ncl_retrieve_data_fixed(&comp2);
					um_get_endpts(&comp2,tfmat2,cpt21,cpt22);
					if (inpara2 == 0.)
						um_vctovc(cpt22,conpt);
					else
						um_vctovc(cpt21,conpt);
				}
				return(2);
			}
		}
	}
/*
.....Get end points and tangent vectors
.......Reverse tv11 and tv22 so they point in the correct
.......direction for creating smooth connection.
*/
	ix = 3;
	evstup (&ikeys[0], &ix);
	ix = 2;
	n1 = ncevolvF (ix, tol, &cvpoint, &cvtang);
	pp = (UM_coord *) UU_LIST_ARRAY(&cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
	if (lrevs[0])
	{
		ncl_revers1_list (n1,0,pp,1);
		ncl_revers1_list (n1,0,vv,2);
		pp = (UM_coord *) UU_LIST_ARRAY(&cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
	}
	um_vctovc(pp[0],cpt11); um_vctovc(vv[0],tv11);
	um_vctovc(pp[n1-1],cpt12); um_vctovc(vv[n1-1],tv12);
	um_negvc(tv11,tv11);
	uu_list_free(&cvpoint); uu_list_free(&cvtang);
	ix = 3;
	evstup (&ikeys[1], &ix);
	ix = 2;
	n1 = ncevolvF (ix, tol, &cvpoint, &cvtang);
	pp = (UM_coord *) UU_LIST_ARRAY(&cvpoint);
	vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
	if (lrevs[1])
	{
		ncl_revers1_list (n1,0,pp,1);
		ncl_revers1_list (n1,0,vv,2);
		pp = (UM_coord *) UU_LIST_ARRAY(&cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
	}
	um_vctovc(pp[0],cpt21); um_vctovc(vv[0],tv21);
	um_vctovc(pp[n1-1],cpt22); um_vctovc(vv[n1-1],tv22);
	um_negvc(tv22,tv22);
	uu_list_free (&cvpoint); uu_list_free (&cvtang);
/*
.....Find closest end points
*/
	if (!use_lrevs)
	{
/*
.....is_spt is already known so we only need to find ipt2.
*/
		if (use_conpt)
		{
			if (is_spt)
			{
				um_vctovc(cpt11,cpt12);
				um_vctovc(tv11,tv12);
			}
			else
			{
				um_vctovc(cpt12,cpt11);
				um_vctovc(tv12,tv11);
			}
		}
		if (firstfl == -1)
		{
			if (um_dcccc(firstpt,cpt22) < UM_DFUZZ)
			{
				um_vctovc(cpt22,cpt21);
				um_vctovc(tv22,tv21);
			}
			else
			{
				um_vctovc(cpt21,cpt22);
				um_vctovc(tv21,tv22);
			}
		}
		dis1 = um_dcccc(cpt11,cpt21); dis2 = um_dcccc(cpt11,cpt22);
		dis3 = um_dcccc(cpt12,cpt21); dis4 = um_dcccc(cpt12,cpt22);
/*
.....Already connected at one end
*/
		if (dis1 < tolr || dis2 < tolr || dis3 < tolr || dis4 < tolr)
		{
			if (!use_lrevs)
			{
				if (dis1 < tolr || dis3 < tolr)
					um_vctovc(cpt22,conpt);
				else
					um_vctovc(cpt21,conpt);
			}
			if (firstfl == 1)
			{
				if (dis1 < tolr || dis2 < tolr)
				{
					um_vctovc(cpt12,firstpt);
					um_vctovc(cpt11,flippt);
				}
				else
				{
					um_vctovc(cpt11,firstpt);
					um_vctovc(cpt12,flippt);
				}
			}
			return(UU_FAILURE);
		}
/*
.....If closest ends are same distance away, choose the one that is closest
.....to the intersection of the curves or thier extensions - ASF 2/6/14.
*/
		if ((fabs(dis1-dis3) < UM_FUZZ && dis1 <= dis2 && dis1 <= dis4) ||
			(fabs(dis2-dis4) < UM_FUZZ && dis2 <= dis1 && dis2 <= dis3))
		{
			nint = ierr = 0; ival = 1;
			um_isect_curves(&comp1,tfmat1,&comp2,tfmat2,&ival,ipt,
				&nint,10,ibuff);
			if (nint > 0 && ierr == 0)
			{
				dmin1 = dmin2 = 1.e12;
				for (k=0;k<nint;k++)
				{
					tdis = um_dcccc(ibuff[k].pt,cpt11);
					if (tdis < dmin1)
					{
						ind1 = 1;
						dmin1 = tdis;
					}
					tdis = um_dcccc(ibuff[k].pt,cpt12);
					if (tdis < dmin1)
					{
						ind1 = 2;
						dmin1 = tdis;
					}
					tdis = um_dcccc(ibuff[k].pt,cpt21);
					if (tdis < dmin2)
					{
						ind2 = 3;
						dmin2 = tdis;
					}
					tdis = um_dcccc(ibuff[k].pt,cpt22);
					if (tdis < dmin2)
					{
						ind2 = 5;
						dmin2 = tdis;
					}
				}
				if (ind1 + ind2 == 4) {dis1 = 0.; dis2 = dis3 = dis4 = 1.e6;}
				else if (ind1 + ind2 == 6) {dis2 = 0.; dis1 = dis3 = dis4 = 1.e6;}
				else if (ind1 + ind2 == 5) {dis3 = 0.; dis1 = dis2 = dis4 = 1.e6;}
				else if (ind1 + ind2 == 7) {dis4 = 0.; dis1 = dis2 = dis3 = 1.e6;}
			}
		}
		if (dis1 <= dis2 && dis1 <= dis3 && dis1 <= dis4)
		{
			um_vctovc(cpt11,ipt1); um_vctovc(cpt21,ipt2);
			um_vctovc(tv11,itv1); um_vctovc(tv21,itv2);
			use_spt = UU_TRUE;
		}
		else if (dis2 <= dis1 && dis2 <= dis3 && dis2 <= dis4)
		{
			um_vctovc(cpt11,ipt1); um_vctovc(cpt22,ipt2);
			um_vctovc(tv11,itv1); um_vctovc(tv22,itv2);
			use_ept = UU_TRUE;
		}
		else if (dis3 <= dis1 && dis3 <= dis2 && dis3 <= dis4)
		{
			um_vctovc(cpt12,ipt1); um_vctovc(cpt21,ipt2);
			um_vctovc(tv12,itv1); um_vctovc(tv21,itv2);
			use_spt = UU_TRUE;
		}
		else
		{
			um_vctovc(cpt12,ipt1); um_vctovc(cpt22,ipt2);
			um_vctovc(tv12,itv1); um_vctovc(tv22,itv2);
			use_ept = UU_TRUE;
		}
		if (!use_lrevs)
		{
			if (use_spt)
				um_vctovc(cpt22,conpt);
			else
				um_vctovc(cpt21,conpt);
		}
		if (firstfl == 1)
		{
			if (um_dcccc(ipt1,cpt12) < UM_DFUZZ)
			{
				um_vctovc(cpt11,firstpt);
				um_vctovc(cpt12,flippt);
			}
			else
			{
				um_vctovc(cpt12,firstpt);
				um_vctovc(cpt11,flippt);
			}
		}
	}
/*
.....Use reverse flags to set end points
*/
	else
	{
		um_vctovc(cpt12,ipt1); um_vctovc(tv12,itv1);
		um_vctovc(cpt21,ipt2); um_vctovc(tv21,itv2);
		dis1 = um_dcccc(ipt1,ipt2);
		if (dis1 < tolr)
			return(2);
	}
/*
.....Make connection
*/
	if (type == 1) /* LINEAR */
	{
		um_retrieve_data_relnum(ikeys[0],&rel1);
		um_retrieve_data_relnum(ikeys[1],&rel2);
		um_unitvc(itv1,itv1); um_unitvc(itv2,itv2);
		um_translate_point(ipt1,10000.,itv1,tpt1);
		um_negvc (itv2,itv2);
		um_translate_point(ipt2,10000.,itv2,tpt2);
		um_isegseg(ipt1,tpt1,ipt2,tpt2,&nint,ipt,tol);
		comp1.key = ikeys[0];
		ncl_retrieve_data_fixed(&comp1);
		uc_retrieve_transf(ikeys[0],tfmat1);
		comp2.key = ikeys[1];
		ncl_retrieve_data_fixed(&comp2);
		uc_retrieve_transf(ikeys[1],tfmat2);
/*
.....Use chamfer if no intersection
*/
		if (nint <= 0)
		{
/*
.....Extend lines instead of adding new ones if possible.
*/
			if (rel1 == UM_LINE_REL || rel2 == UM_LINE_REL)
			{
				ierr = 0;
				ival = 1;
				um_isect_curves(&comp1,tfmat1,&comp2,tfmat2,&ival,ipt,
					&nint,10,ibuff);
				if (nint > 0 && ierr == 0)
				{
					dis2 = 1.e25;
					u1 = u2 = 10000.;
					for (k=0;k<nint;k++)
					{
/*
.....Make sure extension can be made. Only extend lines in the direction
.....of the end being connected to.
*/
						um_cctou(&comp1,tfmat1,ibuff[k].pt,&ibuff[k].u0,&dis1);
						um_cctou(&comp2,tfmat2,ibuff[k].pt,&ibuff[k].u1,&dis3);
						um_vcmnvc(ibuff[k].pt,ipt1,tvec1); um_unitvc(tvec1,tvec1);
						um_vcmnvc(ibuff[k].pt,ipt2,tvec2); um_unitvc(tvec2,tvec2);
						ext1 = (UM_DOT(itv1,tvec1)>0 && 0.<ibuff[k].u1 &&
							ibuff[k].u1<1. && rel1 == UM_LINE_REL && dis3 < tolr);
						ext2 = (UM_DOT(itv2,tvec2)>0 && 0.<ibuff[k].u0 &&
							ibuff[k].u0<1. && rel2 == UM_LINE_REL && dis1 < tolr);
						dis1 = um_dcccc(ibuff[k].pt,ipt1);
						if (dis1 < dis2 && (ext1 || ext2))
						{
							dis2 = dis1;
							u1 = ibuff[k].u0;
							u2 = ibuff[k].u1;
							um_vctovc(ibuff[k].pt,ipt);
						}
					}
					if (u1<10000.)
					{
/*
.....Make sure the side closest to the next curve is kept
*/
						if (use_u3 && 0. < u2 && u2 < 1.)
						{
							if (u2 < u3) inpara2 = 0.;
							else inpara2 = 1.;
						}
						if (fabs(u1) > umin && fabs(1.- u1) > umin)
							uc_trim_extend_curve(&comp1,tfmat1,ipt,&u1,inpara1);
/*
.....Flip the first point if the connection will be made using the wrong end
.....causing the "extension" to connect to the other end - ASF 2/11/14.
*/
						else if (firstfl == 1 && um_dcccc(firstpt,ipt) < UM_FUZZ)
							um_vctovc(flippt,firstpt);
						if (fabs(u2) > umin && fabs(1.- u2) > umin)
							uc_trim_extend_curve(&comp2,tfmat2,ipt,&u2,inpara2);
						if (!use_lrevs)
						{
							ncl_retrieve_data_fixed(&comp2);
							um_get_endpts(&comp2,tfmat2,cpt21,cpt22);
							if (inpara2 == 0.)
								um_vctovc(cpt22,conpt);
							else
								um_vctovc(cpt21,conpt);
						}
						goto done;
					}
				}
			}
			ln1.key = 0;
			ln1.rel_num = 2;
			ln1.no_displst = 0;
			strcpy (ln1.label,"");
			um_vctovc(ipt1,ln1.spt); um_vctovc(ipt2,ln1.ept);
			status = um_create_geom(&ln1,UM_DEFAULT_TF,UM_CURRENT_ATTR);
			okeys[0] = ln1.key;
			okeys[1] = 0;
			goto done;
		}
		if (rel1 == UM_LINE_REL)
		{
			um_cctou(&comp1,tfmat1,ipt,&u1,&dis1);
			if (fabs(u1) > umin && fabs(1.- u1) > umin)
				uc_trim_extend_curve(&comp1,tfmat1,ipt,&u1,inpara1);
			okeys[0] = 0;
		}
		else
		{
			ln1.key = 0;
			ln1.rel_num = 2;
			ln1.no_displst = 0;
			strcpy (ln1.label,"");
			um_vctovc(ipt1,ln1.spt); um_vctovc(ipt,ln1.ept);
			status = um_create_geom(&ln1,UM_DEFAULT_TF,UM_CURRENT_ATTR);
			okeys[0] = ln1.key;
		}
		if (rel2 == UM_LINE_REL)
		{
			um_cctou(&comp2,tfmat2,ipt,&u1,&dis1);
			if (fabs(u1) > umin && fabs(1.- u1) > umin)
				uc_trim_extend_curve(&comp2,tfmat2,ipt,&u1,inpara2);
			okeys[1] = 0;
		}
		else
		{
			ln1.key = 0;
			ln1.rel_num = 2;
			ln1.no_displst = 0;
			strcpy (ln1.label,"");
			um_vctovc(ipt,ln1.spt); um_vctovc(ipt2,ln1.ept);
			status = um_create_geom(&ln1,UM_DEFAULT_TF,UM_CURRENT_ATTR);
			okeys[1] = ln1.key;
		}
	}
	else if (type == 2) /* SMOOTH */
	{
		uu_list_init (&seglist, sizeof(struct NCL_crvgen_rec), 2, 1);
		ncl_init_seg (&seg);
		seg.x = ipt1[0]; seg.y = ipt1[1]; seg.z = ipt1[2];
		seg.a = itv1[0]; seg.b = itv1[1]; seg.c = itv1[2];
		if (seg.a*seg.a + seg.b*seg.b + seg.c*seg.c > 0.009)
			seg.inv = 1;
		uu_list_push (&seglist, &seg);
		ncl_init_seg (&seg);
		seg.x = ipt2[0]; seg.y = ipt2[1]; seg.z = ipt2[2];
		seg.a = itv2[0]; seg.b = itv2[1]; seg.c = itv2[2];
		if (seg.a*seg.a + seg.b*seg.b + seg.c*seg.c > 0.009)
			seg.inv = 1;
		uu_list_push (&seglist, &seg);
		segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);
		rcrv.key = 0;
		strcpy (rcrv.label,"");
		status = ncl_interp_rbsp(2,segp,1,&rcrv);
		uu_list_free (&seglist);
		okeys[0] = rcrv.key;
		okeys[1] = 0;
	}
	else if (type == 3) /* CHAMFER */
	{
		ln1.key = 0;
		ln1.rel_num = 2;
		ln1.no_displst = 0;
		strcpy (ln1.label,"");
		um_vctovc(ipt1,ln1.spt); um_vctovc(ipt2,ln1.ept);
		status = um_create_geom(&ln1,UM_DEFAULT_TF,UM_CURRENT_ATTR);
		okeys[0] = ln1.key;
		okeys[1] = 0;
	}
done:
	if (okeys[0] > 0) ur_update_displayable(okeys[0], UM_NEVERDISPLAYABLE);
	if (okeys[1] > 0) ur_update_displayable(okeys[1], UM_NEVERDISPLAYABLE);
	return (status);
}
/*********************************************************************
**   E_FUNCTION : int um_update_norm (cvtang,npt,up,vmod,vnorm,vvq,
**                                       lperp2d,l2d)
**		Updates the normal vector so it will be in the correct position
**    when defining the normal vector for the next subcurve.  
**   PARAMETERS   
**      INPUT  :
**         cvtang  - curve tangent vectors
**         npt     - number of input vectors
**         up      - perp flag
**         vmod    - offset direction
**         vnorm   - offset vector
**         vvq     - offset plane vector
**         lperp2d - offset perp to curve plane flag
**         l2d     - left/right direction modifier
**      OUTPUT :
**         vnorm   - offset vector
**         vvq     - offset plane vector
**   RETURNS      : 0 iff no error
**   SIDE EFFECTS : none
**   WARNINGS     : none
*********************************************************************/
int um_update_norm (cvtang,npt,up,vmod,vnorm,vvq,lperp2d,l2d)
UU_LIST *cvtang;
UM_vector vmod,vnorm,vvq;
int npt,up;
UU_LOGICAL lperp2d,l2d;
{
	int i;
	UM_vector *vv,vv1;

	vv = (UM_vector *) UU_LIST_ARRAY(cvtang);

	for (i=1;i<npt;i++)
	{
		um_vctovc (vv[i], vv1);
		if (up == 1)
		{
			um_triple_cross (vv1,vmod,vv1, vnorm);
			if (UM_MAG (vnorm) < UM_FUZZ)
				return (-1);
			um_unitvc (vnorm, vnorm);
		}
		else
		{
/*  May need to add in corner offset logic from necvoffs.c  */
			if (!lperp2d)
			{
				um_cross (vvq, vv1, vnorm);
				um_unitvc (vnorm, vnorm);
				if (!l2d) um_cross (vv1, vnorm, vvq);
			}
			else
			{		
				um_vctovc (vvq,vnorm);
				um_unitvc (vnorm, vnorm);
			}
		}
	}
	return(0);
}
/*********************************************************************
**   E_FUNCTION : void um_compcrv_deloop(key_list,rev_list,no_cid,
**                                         closed,lplanar,single,tol)
**		 Remove loops from offset composite curve.
**   PARAMETERS   
**      INPUT  : 
**         key_list - component key list
**         rev_list - component reverse flag list
**         no_cid   - input number of components
**         closed   - curve closed flag
**         lplanar  - UU_TRUE is curve is planar
**         single   - UU_TRUE if single component was offset
**         tol      - curve tolerance
**      OUTPUT :  
**         key_list - updated key list
**         rev_list - updated reverse flag list
**         no_cid   - updated number of components
**   RETURNS      : none
**   SIDE EFFECTS : none
**   WARNINGS     : none
*********************************************************************/
void um_compcrv_deloop(key_list,rev_list,no_cid,closed,lplanar,single,
								tol)
UU_LIST *key_list,*rev_list;
UU_LOGICAL closed,lplanar,single;
int *no_cid;
UU_REAL tol;
{
	int i,j,k,inp,nint,offst,offval,err,nocv=*no_cid;
	struct UM_crvdatabag comp1,comp2;
	struct UM_line_rec *ln1,*ln2;
	UM_isect ibuff[10];
	UM_coord svpt,spt,ept,ipt,npt,tpt1,tpt2,tpt3,tpt4;
	UM_vector tvec1,tvec2;
	UM_param inpara;
	UM_transf tfmat1,tfmat2;
	UU_KEY_ID *keys;
	UU_LOGICAL *revs,lrev1,lrev2,idel;
	UU_LOGICAL s1,e1,s2,e2,start,end,extend1,extend2;
	UU_REAL u1,u2,su1,su2,dist1,dist2,dist_min,umin=0.001;
	UM_int2 ierr;
	char sbuf[80];

	keys = (UU_KEY_ID *)UU_LIST_ARRAY(key_list);
	revs = (UU_LOGICAL *)UU_LIST_ARRAY(rev_list);
	offst = (closed)? 1 : 0;
	i = 0;
	while(i<nocv-1)
	{
/*
.....Look for intersections
*/
		comp1.key = keys[i];
		lrev1 = revs[i];
		ncl_retrieve_data_fixed(&comp1);
		uc_retrieve_transf(comp1.key,tfmat1);
		um_get_endpts(&comp1,tfmat1,spt,ept);
		if (lrev1) um_vctovc(ept,ipt);
		else um_vctovc(spt,ipt);
		for (j=nocv-1;j>i;j--)
		{
			nint = 0;
			idel = UU_FALSE;
			su1 = su2 = -1.;
			comp2.key = keys[j];
			lrev2 = revs[j];
			ncl_retrieve_data_fixed(&comp2);
			uc_retrieve_transf(comp2.key,tfmat2);
			if (!lplanar || single)
			{
				ncl_isect_3d(&comp1,tfmat1,&comp2,tfmat2,UU_FALSE,ipt,&nint,ibuff,&err);
				for (k=0;k<nint;k++)
				{
					um_cctou(&comp1,tfmat1,ibuff[k].pt,&ibuff[k].u0,&dist1);
					um_cctou(&comp2,tfmat2,ibuff[k].pt,&ibuff[k].u1,&dist2);
/*
.....Intersection point is not on the curves
*/
					if (dist1 > tol || dist2 > tol)
					{
						ibuff[k].u0 = -1.; ibuff[k].u1 = -1.;
					}
				}
			}
			else
			{
/*
.....Using line intersection routine so colinear lines can still have their
.....intersection found if they have one
*/
				if (comp1.rel_num == UM_LINE_REL && comp2.rel_num == UM_LINE_REL)
				{
					ln1 = (struct UM_line_rec *)&comp1; ln2 = (struct UM_line_rec *)&comp2;
					if (!lrev1)
					{
						um_vctovc(ln1->spt,tpt1);
						um_vctovc(ln1->ept,tpt3);
					}
					else
					{
						um_vctovc(ln1->ept,tpt1);
						um_vctovc(ln1->spt,tpt3);
					}
					if (!lrev2)
					{
						um_vctovc(ln2->spt,tpt2);
						um_vctovc(ln2->ept,tpt4);
					}
					else
					{
						um_vctovc(ln2->ept,tpt2);
						um_vctovc(ln2->spt,tpt4);
					}
					um_vcmnvc(tpt3,tpt1,tvec1); um_vcmnvc(tpt4,tpt2,tvec2);
					um_unitvc(tvec1,tvec1); um_unitvc(tvec2,tvec2);
					um_ilnln1(tpt1,tvec1,tpt2,tvec2,&nint,ibuff[0].pt);
					if (nint > 0)
					{
/*
.....Get the midpoint of the overlap if there is one.
*/
						if (um_vcparall(tvec1,tvec2))
						{
							um_vcplvc(tpt3,tpt2,ibuff[0].pt); 
							um_vctmsc(ibuff[0].pt,(UU_REAL)0.5,ibuff[0].pt);
						}
						um_cctou(&comp1,tfmat1,ibuff[0].pt,&ibuff[0].u0,&dist1);
						um_cctou(&comp2,tfmat2,ibuff[0].pt,&ibuff[0].u1,&dist1);
					}
				}
				else
					ncl_isect_2d(&comp1,tfmat1,&comp2,tfmat2,UU_FALSE,ipt,&nint,ibuff,&ierr);
			}
			if (nint <= 0) continue;
			dist_min = 1.e25;
			for (k=0;k<nint;k++)
			{
				dist1 = um_dcccc(ipt,ibuff[k].pt);
				u1 = ibuff[k].u0; u2 = ibuff[k].u1;
				start = ((lrev1 && fabs(1.-u1)<umin)||(!lrev1 && fabs(u1)<umin));
				end = ((!lrev2 && fabs(1.-u2)<umin)||(lrev2 && fabs(u2)<umin));
				s1 = ((lrev1 && fabs(1.-u1)<umin)||(!lrev1 && fabs(u1)<umin));
				e1 = ((!lrev1 && fabs(1.-u1)<umin)||(lrev1 && fabs(u1)<umin));
				s2 = ((lrev2 && fabs(1.-u2)<umin)||(!lrev2 && fabs(u2)<umin));
				e2 = ((lrev2 && fabs(u2)<umin)||(!lrev2 && fabs(1.-u2)<umin));
				extend1 = (u1<0.||u1>1.); extend2 = (u2<0.||u2>1.);
/*
.....Ignore any intersection on an extension or where a closed curve
.....has its ends meet
*/
				if ((closed && start && end && j-i == nocv-1) || extend1 ||
					extend2 || e1 && s2 && j-i == 1) 
					continue;
				else if (dist1 < dist_min)
				{
					su1 = u1; su2 = u2;
					um_vctovc(ibuff[k].pt,svpt);
					dist_min = dist1;
				}
			}
			if (fabs(su1) > umin && su1 < 0.)
				continue;
/*
.....Intersection in interior of curve
*/
			if (j-i < nocv-1)
			{
/*
.....The smaller section of the curve is removed
*/
				if (j - i <= nocv/2)
				{
					uu_list_delete(key_list,i+1,j-i-1);
					uu_list_delete(rev_list,i+1,j-i-1);
					nocv -= (j-i-1);
					inpara = (lrev2)? 1. : 0.;
					if (fabs(su2) > umin && fabs(1.- su2) > umin)
						uc_trim_extend_curve(&comp2, tfmat2, svpt, &su2, inpara);
					inpara = (lrev1)? 0. : 1.;
					if (fabs(su1) > umin && fabs(1.- su1) > umin)
						uc_trim_extend_curve(&comp1, tfmat1, svpt, &su1, inpara);
					keys = (UU_KEY_ID *)UU_LIST_ARRAY(key_list);
					revs = (UU_LOGICAL *)UU_LIST_ARRAY(rev_list);
					break;
				}
				else
				{
					if (i > 0)
					{
						uu_list_delete(key_list,0,i);
						uu_list_delete(rev_list,0,i);
					}
					if (j < nocv - 1)
					{
						if (i > 0)
						{
							uu_list_delete(key_list,j,nocv-j-1);
							uu_list_delete(rev_list,j,nocv-j-1);
						}
						else
						{
							uu_list_delete(key_list,j+1,nocv-j-1);
							uu_list_delete(rev_list,j+1,nocv-j-1);
						}
						nocv -= nocv-j-1;
					}
					if (i > 0) nocv -= i;
					inpara = (lrev1)? 1. : 0.;
					if (fabs(su1) > UM_DFUZZ && fabs(1.- su1) > umin)
						uc_trim_extend_curve(&comp1, tfmat1, svpt, &su1, inpara);
					inpara = (lrev2)? 0. : 1.;
					if (fabs(su2) > UM_DFUZZ && fabs(1.- su2) > umin)
						uc_trim_extend_curve(&comp2, tfmat2, svpt, &su2, inpara);
					keys = (UU_KEY_ID *)UU_LIST_ARRAY(key_list);
					revs = (UU_LOGICAL *)UU_LIST_ARRAY(rev_list);
					i = -1;
					break;
				}
			}
/*
.....Remove the ends of the curve
*/
			else if (j > 0)
			{
				if ((su1 < UM_DFUZZ && lrev1) || (su1 > 1. && !lrev1))
				{
					uu_list_delete(key_list,i,1);
					uu_list_delete(rev_list,i,1);
					idel = UU_TRUE;
					nocv--;
				}
				else
				{
					inpara = (lrev1)? 1. : 0.;
					if (fabs(su1) > umin && fabs(1.- su1) > umin)
						uc_trim_extend_curve(&comp1, tfmat1, svpt, &su1, inpara);
				}
				if ((su2 < UM_DFUZZ && !lrev2) || (su2 > 1. && lrev2))
				{
					offval = (idel)? 1 : 0;
					uu_list_delete(key_list,j-offval,1);
					uu_list_delete(rev_list,j-offval,1);
					nocv--;
				}
				else
				{
					inpara = (lrev2)? 0. : 1.;
					if (fabs(su2) > umin && fabs(1.- su2) > umin)
						uc_trim_extend_curve(&comp2, tfmat2, svpt, &su2, inpara);
				}
				keys = (UU_KEY_ID *)UU_LIST_ARRAY(key_list);
				revs = (UU_LOGICAL *)UU_LIST_ARRAY(rev_list);
				if (idel) {i--; break;}
			}
		}
		i++;
	}
	*no_cid = nocv;
}
/*********************************************************************
**   E_FUNCTION : int umf_offset_compcrv_comps(cvkey,dir,allfl,scomp,
**                     ecomp,dist,err_flg,lab,sub,labfl,dtol,type)
**		 Fortran callable routine to offset components of a composite
**     curve.
*********************************************************************/
int umf_offset_compcrv_comps(cvkey,dir,allfl,scomp,ecomp,dist,err_flg,
										lab,sub,labfl,dtol,type)
UM_int4 *cvkey, *scomp, *ecomp, *sub;
UM_real8 dir[], *dist, *dtol;
UM_int2 *err_flg,*allfl,*labfl,*type;
UM_f77_str_ptr lab;
{
	int first,last,subsc,nc,typ,status,err;
	UU_REAL dis,tol;
	UU_KEY_ID key;
	UM_vector dvec;
	UU_LOGICAL lall,flabel;
	char *tlab,label[NCL_MAX_LABEL];

	key = *cvkey;
	dvec[0] = dir[0]; dvec[1] = dir[1]; dvec[2] = dir[2];
	lall = *allfl;
	first = *scomp - 1;
	last = *ecomp -1;
	subsc = *sub;
	dis = *dist;
	flabel = (*labfl == 1)? UU_TRUE : UU_FALSE;
	if (flabel)
	{
		tlab = UM_cstr_of_f77_str(lab);
		nc = NCL_MAX_LABEL-1;
		strncpy(label,tlab,nc);
		ul_strip_blanks(label,&nc);
	}
	tol = *dtol;
	typ = *type;
	status = um_offset_compcrv_comps(&key,dvec,first,last,lall,dis,
					&err,&label,subsc,flabel,tol,typ);
	*cvkey = key;
	*err_flg = err;

	return(status);
}
/*********************************************************************
**   E_FUNCTION : int um_offset_compcrv_comps(cvkey,dvec,first,last,
**                     allfl,dis,err_flg,label,sub,flabel,tol,contype)
**		 Offset components of a composite curve
**   PARAMETERS   
**      INPUT  :
**         cvkey   - key of curve to offset
**         dvec    - offset direction vector
**         first   - index of starting component to offset
**         last    - index of final component to offset
**         allfl   - UU_TRUE : Offset all components
**         dis     - UU_FALSE: Use first, last for range
**         label   - label to assign to curve
**         sub     - subscript to assign to curve
**         flabel  - UU_TRUE : if curve label is provided
**                   UU_FALSE: use autonaming for curve
**         tol     - tolerance to use when creating geometry
**         contype - 1: use lines to connect components
**                   2: use bspline to connect components
**                   3: use chamfers to connect components
**      OUTPUT :
**         cvkey   - key of new curve
**         err_flg - error number
**   RETURNS      : UU_SUCCESS
**   SIDE EFFECTS : none
**   WARNINGS     : none
*********************************************************************/
int um_offset_compcrv_comps(cvkey,dvec,first,last,allfl,dis,err_flg,
										label,sub,flabel,tol,contype)
UU_KEY_ID *cvkey;
UM_vector dvec;
int first,last,sub,contype,*err_flg;
UU_REAL dis,tol;
UU_LOGICAL allfl,flabel;
char *label;
{
	int no_cid,i,j,k,status,ix,n1,nc,npts=0,UP=0,count;
	int nfl=-1,removed=0,added;
	UU_KEY_ID key,*keys,new_key1,new_key2,*olds,ikeys[2],okeys[2];
	struct UM_compcrv_rec crv;
	struct UM_line_rec ln;
	struct UM_circle_rec circ;
   struct UM_rbsplcrv_rec rcrv;
	struct NCL_fixed_databag comp,comp1,*crvs;
	UU_LOGICAL lperp=UU_FALSE,lrev,closed,single=UU_FALSE;
	UU_LOGICAL *revs,tfl=UU_FALSE,offst,lplanar,stunfl=UU_FALSE;
	UU_LOGICAL *rev_old,use_keys=UU_TRUE,init_done=UU_FALSE;
	UM_vector vv1,vnorm,vvq,tvec1,tvec2,*vv;
	UM_vector cvec,xaxis,yaxis;
	UU_REAL ddp,dot,dot1,tdis,tol1,tu;
	UU_REAL plane[4],cls_dist,sig,rtemp;
	UM_coord *pp,circpt,circpt2,ipt,spt,ept;
	struct UM_evcrvout evout;
	UM_transf tfmat;
	UU_LIST cvpoint,cvtang,tangs;
	UM_int2 itype,nwds,lfl_77;
   struct NCL_crvgen_rec seg, *segp;
   UU_LIST seglist,key_list,rev_list,del_list,kold_list;
	UU_LIST rold_list,ind_list, crv_list;
	UU_LOGICAL lrevs[2];
	UU_REAL t0,t1;
	int del,*inds;
	char sbuf[80];

	*err_flg = 0;
/*
.....Initialize curve data
*/
	crv.key = *cvkey;
	ncl_retrieve_data_fixed(&crv);
	uc_retrieve_transf(crv.key,tfmat);
	if (crv.rel_num != UM_COMPCRV_REL)
	{
		*err_flg = 163;
		return(0);
	}
	um_get_endpts(&crv,tfmat,spt,ept);
	closed = (um_dcccc(spt,ept) < tol);
	no_cid = crv.no_cid;
	t0 = crv.t0;
	t1 = crv.t1;
	if (allfl)
	{
		first = 0;
		last = no_cid-1;
	}
	else
	{
		if (last < 0) last = first;
		if (no_cid - 1 < last || no_cid - 1 < first)
		{
			*err_flg = 445;
			return(0);
		}
	}
	uu_list_init(&kold_list,sizeof(UU_KEY_ID),no_cid,1);
	uu_list_init(&rold_list,sizeof(UU_LOGICAL),no_cid,1);
	uu_list_init(&key_list,sizeof(UU_KEY_ID),no_cid,1);
	uu_list_init(&rev_list,sizeof(UU_LOGICAL),no_cid,1);
	uu_list_init(&ind_list,sizeof(int),no_cid,1);
	uu_list_init(&tangs,sizeof(UM_vector),50,50);
	uu_list_init(&crv_list,sizeof(struct NCL_fixed_databag),no_cid,1);
/*
.....Store keys of component geometry. The offset section will be the
.....first added to the list.
*/
	inds = (int *)UU_LIST_ARRAY(&ind_list);
	olds = (UU_KEY_ID *)UU_LIST_ARRAY(&kold_list);
	rev_old = (UU_LOGICAL *)UU_LIST_ARRAY(&rold_list);
	i = 0; 
	if (closed) j = first;
	else j = 0;
	del = 0;
	while (i < no_cid)
	{
		if ((t0 > crv.cid[j].endparam) || 
			(j > 0 && crv.cid[j-1].endparam > t1))
		{
			no_cid--;
			del++;
			j++;
			continue;
		}
		olds[i] = crv.cid[j].crvid;
		rev_old[i] = crv.cid[j].reverse;
		inds[i] = j;
		j++;
		if (j >= no_cid+del) j = 0;
		i++;
	}
	if (closed)
	{
		if (first > last)
			last += (no_cid - first);
		else
			last -= first;
		first = 0;
	}
	if (fabs (dis) < tol) tol1 = tol;
	else tol1 = 2.*tol;
/*
.....Set up offset data to determine lperp
*/
	for (i=first;i<=last;i++)
	{
		key = olds[i];
		ix = 3;
		evstup (&key, &ix);
		ix = 2;
		if (i == first || i == last)
		{
			comp.key = key;
			uc_retrieve_transf(key, tfmat);
			ncl_retrieve_data_fixed(&comp);
			um_c5_trimpart(&comp,crv.cid,crv.no_cid,t0,t1,inds[i],tfmat);
		}
		n1 = ncevolvF (ix, tol1, &cvpoint, &cvtang);
		if (i == first || i == last) ur_update_data_fixed(&comp);
		npts += n1;
		vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
		lrev = rev_old[i];
		if (lrev) ncl_revers1_list (n1,0,vv,2);
		uu_list_push_multiple(&tangs,n1,vv);
	}
//	ncl_evolve_composite_curve(&crv,tol,&cvpoint,&cvtang,UU_NULL,0);
	lperp = ncl_offset_perp2d(&tangs,npts,dvec);
	uu_list_free(&cvpoint); uu_list_free(&cvtang);
	uu_list_free(&tangs);
	if (lperp) nfl *= -1;
	npts = 0;
	ix = 3;
	evstup (&crv.key, &ix);
	ix = 2;
	n1 = ncevolvF (ix, tol1, &cvpoint, &cvtang);
	pp = (UM_coord *) UU_LIST_ARRAY(&cvpoint);
	lplanar = um_planar_curve (pp,n1,plane,xaxis,yaxis);
	uu_list_free(&cvpoint); uu_list_free(&cvtang);
/*
.....Modify curve components and store data for later use.
*/
	if (first == last) single = UU_TRUE;
	lfl_77 = 1;
	stunlb (&lfl_77);
	stunfl = UU_TRUE;
	count = k = 0;
	for (i=0;i<no_cid;i++)
	{
		if (i < first || i > last)
		{
			comp.key = olds[i];
			comp1.key = 0;
			uc_retrieve_data(&comp, sizeof(struct UC_entitydatabag));
			status = uc_copy(&comp, &comp1, sizeof(struct UC_entitydatabag));
			um_c5_trimpart(&comp1,crv.cid,crv.no_cid,t0,t1,inds[i],tfmat);
			uu_list_push(&rev_list,&rev_old[i]);
			uu_list_push(&key_list,&comp1.key);
			uu_list_push(&crv_list,&comp);
			count++;
		}
		if (i >= first && i <= last)
		{
/*
.....Set up component data
*/
			comp.key = key = olds[i];
			uc_retrieve_transf(key, tfmat);
			ncl_retrieve_data_fixed(&comp);
			uu_list_push(&crv_list,&comp);
			count++;
			um_c5_trimpart(&comp,crv.cid,crv.no_cid,t0,t1,inds[i],tfmat);
			ix = 3;
			evstup (&key, &ix);
			ix = 2;
			n1 = ncevolvF (ix, tol1, &cvpoint, &cvtang);
			npts = n1;
			pp = (UM_coord *) UU_LIST_ARRAY(&cvpoint);
			vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
			lrev = rev_old[i];
			if (lrev)
			{
				ncl_revers1_list (n1,0,pp,1);
				ncl_revers1_list (n1,0,vv,2);
				pp = (UM_coord *) UU_LIST_ARRAY(&cvpoint);
				vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
			}
			um_vctovc(vv[0],vv1);
/*
.....Get next normal vector at beginning of subcurve
*/
			if (i == first && !init_done)
			{
				init_done = UU_TRUE;
				ddp = UM_SQDIS (pp[1],pp[0]);
				status = ncl_1st_offset (key,pp,vv,n1,ddp,tol1,vv1,0,lperp,
										dvec,vnorm,vvq,&UP);
				if (status != UU_SUCCESS)
				{
					*err_flg = 476;
					goto error;
				}
			}
			else
			{
				if (UP == 1)
				{
					um_triple_cross (vv[0],dvec,vv[0], vnorm);
					if (UM_MAG (vnorm) < UM_FUZZ)
					{
						*err_flg = 476;
						goto error;
					}
					um_unitvc (vnorm, vnorm);
				}
				else if (!lperp) 
				{
					um_vctovc(vvq,tvec1); um_unitvc(tvec1,tvec1);
					um_vctovc(vv[0],tvec2); um_unitvc(tvec2,tvec2);
					rtemp = UM_DOT(tvec1,tvec2);
					if (fabs(rtemp) < UM_DFUZZ)
						um_cross(vvq,vv[0],vnorm);
					else
/*
.....Since vnorm is perp to vvq we know the plane defined by vv[0] and vnorm
.....will be well defined if/when vvq and vv[0] and approximately parallel
*/
						um_triple_cross (vv[0],vnorm,vv[0], vnorm);
					um_unitvc(vnorm,vnorm);
					um_cross (vv[0], vnorm, vvq);
				}
				else
				{		
					um_vctovc (vvq,vnorm);
					um_unitvc (vnorm, vnorm);
				}
			}
/*
.....Offset component
*/
			switch(comp.rel_num)
			{
				case UM_LINE_REL:
					ln.key = key;
					ncl_retrieve_data_fixed(&ln);
					ln.key = 0;
					strcpy (ln.label,"");
					status = um_create_geom(&ln,UM_DEFAULT_TF,UM_CURRENT_ATTR);
					um_translate_point(ln.spt,dis,vnorm,ln.spt);
					um_translate_point(ln.labloc,dis,vnorm,ln.labloc);
					um_translate_point(ln.ept,dis,vnorm,ln.ept);
					key = ln.key;
					ur_update_data_fixed(&ln);
					break;
				case UM_CIRCLE_REL:
					circ.key = key;
					ncl_retrieve_data_fixed(&circ);
					um_vcmnvc(pp[0],circ.center,tvec1);
					um_unitvc(tvec1,tvec1);
					dot = um_dot(vnorm,tvec1);
					if (dot < 0. && fabs(dot) > UM_DFUZZ &&
						circ.radius-dis < UM_DFUZZ && !lperp)
					{
						first++; removed++;
						status = um_update_norm (&cvtang,n1,UP,dvec,vnorm,vvq,
							lperp,UU_FALSE);
						goto skip;
					}
					circ.key = 0;
					strcpy (circ.label,"");
					status = um_create_geom(&circ,UM_DEFAULT_TF,UM_CURRENT_ATTR);
					sig = 1.;
					if (dot < 0.) sig = -1.; 
					um_translate_point(circ.center,dis,vnorm,circpt);
					um_nptpln(circpt,circ.center,circ.nvec,circpt2);
					dot = um_dcccc(circpt,circpt2);
					if (dot > UM_DFUZZ)
					{
						um_vcmnvc(circpt,circpt2,cvec);
						um_translate_point(pp[0],dis,vnorm,ipt);
						um_vcplvc(circ.center,cvec,circ.center);
						circ.radius = um_dcccc(ipt,circ.center);
					}
					else
						circ.radius += sig*dis;
					key = circ.key;
					ur_update_data_fixed(&circ);
					break;
				case UM_RBSPLCRV_REL:
					if (lperp)
						n1 = ncl_cv_offset (NULLKEY,&cvpoint,&cvtang,npts,0,dvec,
								dis,0,0.,0.,tol1,0,vnorm);
					else
						n1 = ncl_cv_offset (NULLKEY,&cvpoint,&cvtang,npts,0,dvec,
								dis,0,0.,0.,tol1,nfl,vnorm);
					if (n1 < 0)
					{
						*err_flg = 409;
						goto error;
					}
					else if(n1 <= 1)
					{
						first++; removed++;
						goto skip;
					}
					rcrv.key = key;
					ncl_retrieve_data_fixed(&rcrv);
					rcrv.key = 0;
					strcpy (rcrv.label,"");
					status = um_create_geom(&rcrv,UM_DEFAULT_TF,UM_CURRENT_ATTR);
					pp = (UM_coord *) UU_LIST_ARRAY(&cvpoint);
					vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
					uu_list_init (&seglist,sizeof(struct NCL_crvgen_rec),n1,n1);
					for (j=0;j<n1;j++)
					{
						seg.x = pp[j][0];
						seg.y = pp[j][1];
						seg.z = pp[j][2];
						if (UM_DOT(vv[i],vv[i]) > 0.009)
						{
							seg.a = vv[i][0];
							seg.b = vv[i][1];
							seg.c = vv[i][2];
						}
						seg.inv = 0;
						uu_list_push (&seglist, &seg);
					}
					segp = (struct NCL_crvgen_rec *)UU_LIST_ARRAY(&seglist);
					status = ncl_interp_rbsp (n1,segp,1,&rcrv);
					key = rcrv.key;
					uu_list_free (&seglist);
					if (status != UU_SUCCESS)
					{
						*err_flg = 409;
						goto error;
					}
					break;
			}
			uu_list_push(&key_list,&key);
			if (!um_is_idmat(tfmat))
			{
				if (um_update_transformation(key,tfmat) != UU_SUCCESS)
				{
					*err_flg = 409;
					goto error;
				}
			}
			if (comp.rel_num != UM_RBSPLCRV_REL)
				uu_list_push(&rev_list,&lrev);
			else
				uu_list_push(&rev_list,&tfl);
			ur_update_displayable(key, UM_NEVERDISPLAYABLE);
/*
.....Get next normal vector at end of subcurve
.......The rbsplcrv case already handles vnorm and updates it.
*/
			if (comp.rel_num != UM_RBSPLCRV_REL)
			{
				status = um_update_norm (&cvtang,n1,UP,dvec,vnorm,vvq,
					lperp,UU_FALSE);
				if (status != UU_SUCCESS)
				{
					*err_flg = 163;
					goto error;
				}
			}
skip:
//			if (i == first || i == last) ur_update_data_fixed(&comp);
			uu_list_free(&cvpoint); uu_list_free(&cvtang);
		}
	}
/*
.....If any components were eliminated from the curve the indices
.....will need to be updated to offset this in later logic.
.......First was increased each time an entity was omitted so the
.......offset vector would be recalculated if the first entity
.......was one of the components left out.
*/
	no_cid -= removed;
	first -= removed;
	last -= removed;
	if (no_cid < 1 || first < 0 || last < 0)
	{
		*err_flg = 476;
		goto error;
	}
/*
.....Make sure all subcurves are connected
*/
	keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
	revs = (UU_LOGICAL *)UU_LIST_ARRAY(&rev_list);
	for (i=0;i<no_cid && i < 1000;i++)
	{
		if (i < no_cid - 1)
		{
			ikeys[0] = keys[i]; ikeys[1] = keys[i+1];
			lrevs[0] = revs[i]; lrevs[1] = revs[i+1];
		}
		else if (closed)
		{
			ikeys[0] = keys[no_cid-1];	ikeys[1] = keys[0];
			lrevs[0] = revs[no_cid-1]; lrevs[1] = revs[0];
		}
		else
			break;

		added = um_compcrv_connect(ikeys,lrevs,2,tol1,contype,
			okeys,UU_TRUE,UU_TRUE,0,UU_FALSE,0,0);
		if (added == UU_FAILURE)
		{
			*err_flg = 409;
			break;
		}
		else if (added == UU_SUCCESS)
		{
			if (i < no_cid - 1)
			{
				if (okeys[0] > NULLKEY)
				{
					uu_list_insert(&key_list,i+1,&okeys[0]);
					uu_list_insert(&rev_list,i+1,&tfl);
					keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
					revs = (UU_LOGICAL *)UU_LIST_ARRAY(&rev_list);
					i++; no_cid++;
				}
				if (okeys[1] > NULLKEY)
				{
					uu_list_insert(&key_list,i+1,&okeys[1]);
					uu_list_insert(&rev_list,i+1,&tfl);
					keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
					revs = (UU_LOGICAL *)UU_LIST_ARRAY(&rev_list);
					i++; no_cid++;
				}
			}
			else
			{
				if (okeys[0] > NULLKEY)
				{
					uu_list_insert(&key_list,0,&okeys[0]);
					uu_list_insert(&rev_list,0,&tfl);
					keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
					revs = (UU_LOGICAL *)UU_LIST_ARRAY(&rev_list);
					no_cid++;
				}
				if (okeys[1] > NULLKEY)
				{
					uu_list_push(&key_list,&okeys[1]);
					uu_list_push(&rev_list,&tfl);
					keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
					revs = (UU_LOGICAL *)UU_LIST_ARRAY(&rev_list);
					no_cid++;
				}
			}
		}
	}
	keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
	if (*err_flg != 0) goto error;
	uu_list_init(&del_list,sizeof(UU_KEY_ID),no_cid,1);
	keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
	for (i=0;i<no_cid;i++) uu_list_push(&del_list,&keys[i]);
	use_keys = UU_FALSE;
/*
.....Remove loops
.......There is no need to check for loops if the offset was perpendicular
.......to the curve's plane since no loops will be added.
*/
	if (!lperp || single) 
		um_compcrv_deloop(&key_list,&rev_list,&no_cid,closed,lplanar,
			single,tol);
	keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
	crv.key = 0;
	if (no_cid <= 0)
	{
		*err_flg = 409;
		goto error;
	}
	status = um_c5_mergecrv(no_cid, keys, &crv);
	if (status != UU_SUCCESS) 
	{
		*err_flg = 409;
		goto error;
	}
	if (!flabel) stunlb(&lfl_77);
	status = um_create_geom(&crv,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	if (flabel) 
	{
		stunlb (&lfl_77);
		strcpy(crv.label,label);
		crv.subscr = sub;
	}
/*
.....Update curve data
*/
	crv.arclen = 0;
	for (i=0;i<crv.no_cid;i++)
	{
		comp.key = crv.cid[i].crvid;
		ncl_retrieve_data_fixed(&comp);
		crv.arclen += um_getarclen(&comp,UM_DEFAULT_TF);
	}
	*cvkey = crv.key;
	ur_update_data_fixed(&crv);
	goto done;
error:
	if (stunfl) stunlb (&lfl_77);
done:
	crvs = (struct NCL_fixed_databag *)UU_LIST_ARRAY(&crv_list);
	for (i=0;i<count;i++)
		ur_update_data_fixed(&crvs[i]);
/*
.....Delete the new component geometry since a copy is made when the
.....new curve is defined.
*/
	if (use_keys)
	{
		no_cid = key_list.cur_cnt;
		keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
	}
	else
	{
		no_cid = del_list.cur_cnt;
		keys = (UU_KEY_ID *)UU_LIST_ARRAY(&del_list);
	}
	for (i = 0; i < no_cid; i++)
	{
		if (keys[i] > 0) ur_delete_all(keys[i]);
	}
	if (!use_keys) uu_list_free(&del_list);
	uu_list_free(&kold_list);
	uu_list_free(&rold_list);
	uu_list_free(&key_list);
	uu_list_free(&rev_list);
	uu_list_free (&cvpoint);
	uu_list_free (&cvtang);
	return(1);
}
