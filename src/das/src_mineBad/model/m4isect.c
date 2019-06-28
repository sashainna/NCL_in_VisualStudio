/*********************************************************************
**    NAME         :  m4isect.c
**       CONTAINS:
**			int um_isect(eptr0, tfmat0, eptr1, tfmat1, nintp, no_ibuf, ibuf)
**			int um_isect_sp(eptr0, tfmat0, eptr1, tfmat1, common_plane,
**			int um_isect_line(pt, vec, eptr, tfmat, nintp, no_ibuf, ibuf)
**			int um_isect_comp(eptr0, tfmat0, eptr1, tfmat1, nintp, no_ibuf, ibuf)
**			int um_icoplnrbsplcrv(r0, r1, plane, no_isect, max_isect, isect_buff)
**			int umi_isect1(depth, t0, intrv0, r0, cvh0, t1, intrv1, r1, cvh1)
**			int um_isect_refine(t0, i0, r0,cvh0, t1, i1, r1,cvh1)
**			int um_cctou(eptr, tfmat, pt, u, distp)
**			int um_u_equals_pal(eptr, pal, u)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m4isect.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:05
*********************************************************************/

#include	"usysdef.h"
#include	"udebug.h"
#include	"mdebug.h"
#include	"mcvh.h"
#include	"mcrv.h"
#include	"modef.h"
#include	"mdrel.h"
#include	"mdcoord.h"
#include	"mdeval.h"
#include	"misect.h"
#include	"mderror.h"
#include	"mdclass.h"
#include "mdgenent.h"
#include "umath.h"
#include "zsysdep.h"
#include "nccs.h"
#include "ulist.h"

/*
#define	UM_ISECTTRC 1
*/

/*********************************************************************
** E_FUNCTION :  um_isect(eptr0, tfmat0, eptr1, tfmat1, nintp, no_ibuf, ibuf)
**       intersects two curves.  Fills in user supplied buffer of UM_isect
**			records.  curves must be coplanar for success, but this routine
**			will check.
**    PARAMETERS   
**       INPUT  : 
**          eptr0, eptr1		--	pointers to alleged curves
**				tfmat0, tfmat1		--	transformations for each to some common
**											model space.  UM_DEFAULT_TF is OK
**				nintp					--	pointer to integer output
**				no_ibuf				--	number of intersections allocated in ibuf
**				ibuf					--	pointer to UM_isect buffers to hold output
**       OUTPUT :  
**          *nintp				--	number of intersections found
**				ibuf					--	list of UM_isect entries filled in
**    RETURNS      :
**					0	 if OK
**					UM_BADCLASS, UM_NOTPLANAR, UM_BUFFULL, UM_BADENTITY,
**					UM_BADTFMAT, UM_UNIMPLEMENTED, UM_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
**		WARNING:	colinear entities are no longer considered an error. 
**			Instead, no intersections are declared as found.
*********************************************************************/
um_isect(eptr0, tfmat0, eptr1, tfmat1, nintp, no_ibuf, ibuf)
	struct UM_rbsplcrv_rec	*eptr0;
	UM_transf	tfmat0;
	struct UM_rbsplcrv_rec	*eptr1;
	UM_transf	tfmat1;
	int	*nintp;
	int	no_ibuf;
	UM_isect	ibuf[];

	{
	int	ret_val = 0;
	
	/** used in um_span_elist()	**/
	int			dimension;
	UU_REAL		common_plane[2][3];	/* plane of curves	*/
	int			badent;					/* in um_span has problems	*/
	struct UM_rbsplcrv_rec	*elist[2];
	struct UM_rbsplcrv_rec *ptr0, *rb0=0;
	struct UM_rbsplcrv_rec *ptr1, *rb1=0;
	UU_REAL		*tlist[2];

	uu_denter(UU_MTRC,(us,"um_isect(e0(r,k,t)=(%d,%x,%x), e1(r,k,t)=(%d,%x,%x),no_ubuf=%d)",
				eptr0->rel_num, eptr0->key, tfmat0,
				eptr1->rel_num, eptr1->key, tfmat1, no_ibuf));

	*nintp	=	0;

	/**	test classes	**/
	if (uc_super_class(eptr0->rel_num) != UM_CURVE_CLASS 
		|| uc_super_class(eptr1->rel_num) != UM_CURVE_CLASS &&
     uc_super_class(eptr1->rel_num) != UM_POINT_REL)
		{
		ret_val = UM_BADCLASS;
		goto Done;
		}
/*
...make sure that curves are Unicad types,
...but do not convert to RBsplines now if it is not NCL curve 
*/
   if (eptr0->rel_num == NCL_CURVE_REL)
     {
      ptr0 = rb0 = (struct UM_rbsplcrv_rec *) uu_toolmalloc (um_curve_size(eptr0));
      um_rbcrv_frmnclcrv (eptr0,ptr0);
     }
   else
     ptr0 = (struct UM_rbsplcrv_rec *) eptr0;

   if (eptr1->rel_num == NCL_CURVE_REL)
     {
      ptr1 = rb1 = (struct UM_rbsplcrv_rec *) uu_toolmalloc (um_curve_size(eptr1));
      um_rbcrv_frmnclcrv (eptr1,ptr1);
     }
   else
     ptr1 = (struct UM_rbsplcrv_rec *) eptr1;

	elist[0] = ptr0;
	elist[1] = ptr1;
	tlist[0] = (UU_REAL *) tfmat0;
	tlist[1] = (UU_REAL *) tfmat1;

	if (um_span_elist(2, elist, tlist, &dimension, common_plane, &badent))
		{
#ifdef	UM_ISECTTRC
		sprintf(UM_sbuf, "um_isect: um_span_elist returned error.");
		NclxDbgPstr(UM_sbuf);
#endif
		ret_val = UM_FAILURE;	/* general error	*/
		goto Done;
		}
	if (dimension == 3)
		{
#ifdef	UM_ISECTTRC
		sprintf(UM_sbuf, "um_isect: curves not in plane.");
		NclxDbgPstr(UM_sbuf);
#endif
		ret_val = UM_NOTPLANAR;
		goto Done;
		}
	else if (dimension == 1 &&
            uc_super_class(ptr0->rel_num) == 
            uc_super_class(ptr1->rel_num)) 
		{
#ifdef	UM_ISECTTRC
		sprintf(UM_sbuf, "um_isect: colinear lines.");
		NclxDbgPstr(UM_sbuf);
#endif
		*nintp = 0;
		ret_val = 0;
		goto Done;
		}

	ret_val = um_isect_sp(ptr0, tfmat0, ptr1, tfmat1, common_plane, 
									nintp, no_ibuf, ibuf);

Done:
#ifdef	UM_ISECTTRC
		sprintf(UM_sbuf, "um_isect: returning %d, numint %d", ret_val, *nintp);
		NclxDbgPstr(UM_sbuf);
#endif
	if (rb0 != 0) uu_toolfree(rb0);
	if (rb1 != 0) uu_toolfree(rb1);
	uu_dexitstatus("um_isect", ret_val);
	return (ret_val);
	}

/*********************************************************************
** E_FUNCTION :  um_isect_sp(eptr0, tfmat0, eptr1, tfmat1, common_plane,
**										nintp, no_ibuf, ibuf)
**       intersects two curves.  Fills in user supplied buffer of UM_isect
**			records.  curves must be coplanar for success, this routine
**			WILL NOT check.
**    PARAMETERS   
**       INPUT  : 
**          eptr0, eptr1		--	pointers to alleged curves
**				tfmat0, tfmat1		--	transformations for each to some common
**											model space.  UM_DEFAULT_TF is OK
**				common_plane		-- definition of common_plane
**				nintp					--	pointer to integer output
**				no_ibuf				--	number of intersections allocated in ibuf
**				ibuf					--	pointer to UM_isect buffers to hold output
**       OUTPUT :  
**          *nintp				--	number of intersections found
**				ibuf					--	list of UM_isect entries filled in
**    RETURNS      :
**					0	 if OK
**					UM_BADCLASS, UM_NOTPLANAR, UM_BUFFULL, UM_BADENTITY,
**					UM_BADTFMAT, UM_UNIMPLEMENTED, UM_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : Some entities (all but line/circle), SOME of the time,
**		need to be copied, to be transformed to a common space.  Until changes
**		are made in the copy routines, those entities need to be installed into
**		the database (and probably deleted later).
**
**		WARNING:	colinear entities are no longer considered an error. 
**			Instead, no intersections are declared as found.
*********************************************************************/
int um_isect_sp(eptr0, tfmat0, eptr1, tfmat1, common_plane, nintp, no_ibuf, ibuf)
struct UM_crvdatabag	*eptr0;
UM_transf tfmat0;
struct UM_crvdatabag	*eptr1;
UM_transf tfmat1;
UU_REAL common_plane[2][3];
int *nintp;
int no_ibuf;
UM_isect	ibuf[];

{
	UU_LOGICAL	switched;	/* if UU_TRUE, the order we handle
							 		 * curves is opposite that passed
							 		 */
	UU_REAL		*ttinv;		/* transformation from one definition space to other */
	UM_transf	ttinv_space;/* transformation from one definition space to other */
	UU_REAL		dist[2];		/* distance from int point to curves	*/
	int			pairing;		/* holds UM_ISECTPAIR() of two entity types	*/
	int			i,k;
	int	ret_val = 0;
	struct UM_rbsplcrv_rec	*elist[2];
	struct UM_evcrvout ev0;
	UU_REAL		*tlist[2];

	uu_denter(UU_MTRC,(us,"um_isect_sp(e0(r,k,t)=(%d,%x,%x), e1(r,k,t)=(%d,%x,%x),no_ubuf=%d)",
				eptr0->rel_num, eptr0->key, tfmat0,
				eptr1->rel_num, eptr1->key, tfmat1, no_ibuf));

	*nintp	=	0;

	/**	test classes	**/
	if (uc_super_class(eptr0->rel_num) != UM_CURVE_CLASS
		|| uc_super_class(eptr1->rel_num) != UM_CURVE_CLASS &&
     uc_super_class(eptr1->rel_num) != UM_POINT_REL)
	{
		ret_val = UM_BADCLASS;
		goto Done;
	}

	/* composite curve recursion is handled by a separate routine	*/
	if (eptr0->rel_num == UM_COMPCRV_REL || eptr1->rel_num == UM_COMPCRV_REL )
	{
		ret_val = um_isect_comp_sp(eptr0, tfmat0, eptr1, tfmat1,
											 common_plane, nintp, no_ibuf, ibuf);
		goto Done;
	}

	/** note whether curves were passed in our favorite order	**/
	switched = ! UM_IORDER(eptr0->rel_num, eptr1->rel_num);

	/* get transformation between definition spaces.  use the definition 
	 * space of the second entity (respecting UM_IORDER) as the working space.
	 * Also get the common plane in the working space.
	 */

	if (switched)
		{
		/* get transformation to common space	*/
		if (tfmat0 == UM_DEFAULT_TF && tfmat1 == UM_DEFAULT_TF)
			/* both entities in model space	*/
			ttinv = UM_DEFAULT_TF;
		else
			{
			ttinv = (UU_REAL *) ttinv_space;
			um_inverttf(tfmat0, ttinv_space);
			um_cctmtf(common_plane[0], ttinv_space, common_plane[0]);
			um_vctmtf(common_plane[1] ,ttinv_space, common_plane[1]);
			um_tftmtf(tfmat1, ttinv_space, ttinv_space);
			}

		pairing = UM_ISECTPAIR(eptr1->rel_num, eptr0->rel_num);
		}
	else
		{
		if (tfmat0 == UM_DEFAULT_TF && tfmat1 == UM_DEFAULT_TF)
			/* both entities in model space	*/
			ttinv = UM_DEFAULT_TF;
		else
		{
			ttinv = (UU_REAL *) ttinv_space;
			um_inverttf(tfmat1, ttinv_space);
			um_cctmtf(common_plane[0], ttinv_space, common_plane[0]);
			um_vctmtf(common_plane[1], ttinv_space, common_plane[1]);
			um_tftmtf(tfmat0, ttinv_space, ttinv_space);
		}
		pairing = UM_ISECTPAIR(eptr0->rel_num, eptr1->rel_num);
	}

	switch (pairing)
	{
		/*		analytic methods:
		 * transform geometry
		 * call analytic method
		 * invert intersection points (transform if needed);
		 */

		case UM_POINT_LINE:		/* assume switched == UU_FALSE	*/
		{
			UM_vector	uvc0;
			UM_vector	uvc1;
			UM_coord		newpt0;
			UM_coord		ipt;
			struct	UM_line_rec	*e0;
			struct	UM_point_rec	*e1;

			e0 = (struct UM_line_rec *) eptr0;
			e1 = (struct UM_point_rec *) eptr1;

			/* unit vector (transformed) for e0	*/
			um_vcmnvc(e0->ept, e0->spt, uvc0);
			um_vctmtf(uvc0, ttinv, uvc0);
			um_unitvc(uvc0, uvc0);

			/* basepoint (transformed) for e0	*/
			um_cctmtf(e0->spt, ttinv, newpt0);

			um_nptln (e1->pt, newpt0, uvc0, ipt);
			*nintp= 1;
			/* invert point to u	*/
			if (*nintp == 1)
			{
				um_vctovc(ipt, ibuf[0].pt);
				um_cctou(e0, ttinv, ipt, &(ibuf[0].u0), dist);
				um_cctou(e1, UM_DEFAULT_TF, ipt, &(ibuf[0].u1), dist + 1);
				ibuf[0].order = 0;
			}
		}
		break;
/* 
... aak 29-SEP-97: support added for curve-to-point trimmming.
... assumed switched = UU_FALSE; ttinv = UM_DEFAULT_TF;
... trim-extend is done to the closest point on CV to PT.
*/
		case UM_POINT_CURVE:
			ret_val = UM_UNIMPLEMENTED;
		break;
		case UM_CURVE_POINT:
		{
			UU_LIST crv_pts, tanvc;
			int n, nproj;
			UU_REAL u0 = 0., cvlen, d,u1,dm,du;
			UM_coord *pts, pt, vc, vec, pt0;
			struct	UM_rbsplcrv_rec	*e0;
			struct	UM_point_rec	*e1;

			e0 = (struct UM_rbsplcrv_rec *) eptr0;
			e1 = (struct UM_point_rec *)    eptr1;
			uu_list_init (&crv_pts, sizeof(UM_coord), 200, 200);
			uu_list_init (&tanvc,  sizeof(UM_coord), 200, 200);

			n = ncl_evolve_curve(e0,ttinv,0.1*UM_ISECTTOLERANCE,
				&crv_pts,&tanvc,UU_NULL,0);
			if(!n) return(UU_FAILURE);
			n = UU_LIST_LENGTH(&crv_pts);
			pts = (UM_coord *)UU_LIST_ARRAY(&crv_pts);

			for(i=0,cvlen=0.; i<n-1;i++) cvlen += um_dcccc(pts[i],pts[i+1]);

			nproj = norm_to_cvonsf1(e1->pt,&u0,n,pts,cvlen,pt,vc);
			*nintp = 1;
			ibuf[0].order = 0;
			um_vcmnvc(e1->pt,pt,vec);
			d = fabs(um_dot(vc,vec));
			if( d < UM_FUZZ )
			{
/*
.....PT projects on the inner part of CV
*/
				um_vctovc(pt, ibuf[0].pt);
				ibuf[0].u0 = u0;
/*
.....Fine tune the projection by
.....evaluating the actual curve
.....Bobby - 10/20/14
*/
				du = .001;
				um_ev7_rbsplcrv(UM_POINT,u0,e0,tfmat0,&ev0);
				dm = um_dcccc(ev0.cp,e1->pt);
				um_vctovc(e1->pt,pt0);
				for (k=0;k<2;k++)
				{
					u1 = u0;
					um_vctovc(ibuf[0].pt,pt);
					for (i=1;i<=10;i++)
					{
						if (dm <= UM_FUZZ*.1) break;
						u1 += du;
						um_ev7_rbsplcrv(UM_POINT,u1,e0,tfmat0,&ev0);
						d = um_dcccc(ev0.cp,e1->pt);
						if (d <= UM_FUZZ*.1)
						{
							dm = d; u0 = u1;
							um_vctovc(ev0.cp,pt0);
							break;
						}
						else if (d < dm)
						{
							dm = d; u0 = u1;
							um_vctovc(ev0.cp,pt0);
						}
						else
						{
							du = -du/2.;
						}
					}
					ibuf[0].u0 = u0;
					du = -.001;
					u0 = ibuf[0].u0;
				}
				um_vctovc(pt0,ibuf[0].pt);
			}
			else
			{
/*
... PT projects on the extension of CV
*/
				um_unitvc(vc,vc);
				um_nptln(e1->pt,pt,vc,ibuf[0].pt);
				d = um_dcccc(ibuf[0].pt,pt);
				ibuf[0].u0 = (u0 < 0.5) ? -d/cvlen : 1. + d/cvlen;
			}
			ret_val = UU_SUCCESS;
			uu_list_free (&crv_pts);
			uu_list_free (&tanvc);
/*
			um_cctou_rbcrv(e0,tfmat0,e1->pt,&ibuf[0].u0,&d);
			um_ev7_rbsplcrv(UM_POINT,ibuf[0].u0,e0,tfmat0,&ev0);
			um_vctovc(ev0.cp,ibuf[0].pt);
			*nintp = 1;
			ibuf[0].order = 0;
*/
		}
		break;

		case UM_LINE_LINE:		/* assume switched == UU_FALSE	*/
		{
			UM_vector	uvc0;
			UM_vector	uvc1;
			UM_coord		newpt0;
			UM_coord		ipt;
			struct	UM_line_rec	*e0, *e1;

			e0 = (struct UM_line_rec *) eptr0;
			e1 = (struct UM_line_rec *) eptr1;

			/* unit vector (untransformed) for eptr1	*/
			um_vcmnvc(e1->ept, e1->spt, uvc1);
			um_unitvc(uvc1, uvc1);

			/* unit vector (transformed) for e0	*/
			um_vcmnvc(e0->ept, e0->spt, uvc0);
			um_vctmtf(uvc0, ttinv, uvc0);
			um_unitvc(uvc0, uvc0);

			/* basepoint (transformed) for e0	*/
			um_cctmtf(e0->spt, ttinv, newpt0);

			um_ilnln(newpt0, uvc0, e1->spt, uvc1, nintp, ipt);

			/** invert point to u	**/
			if (*nintp == 1)
			{
				um_vctovc(ipt, ibuf[0].pt);
				um_cctou(e0, ttinv, ipt, &(ibuf[0].u0), dist);
				um_cctou(e1, UM_DEFAULT_TF, ipt, &(ibuf[0].u1), dist + 1);
				ibuf[0].order = 0;
			}
		}
		break;

		case	UM_POINT_CIRC:		/* assuming switched == UU_FALSE	*/
		case	UM_LINE_CIRC:		/* assuming switched == UU_FALSE	*/
		{
			struct	UM_line_rec	*vp;		/* tmp ptr	*/
			struct	UM_line_rec	dp;
			struct	UM_point_rec	*e1;
			struct	UM_circle_rec	*hostp;	/* IORDER second	*/
			UM_coord	newpt0, cen0;
			UM_vector	uvc0;
			UM_coord	ipt[2];
	
			hostp = (struct UM_circle_rec *) ((switched)? eptr0: eptr1);
	
			/* get visitor data in host definition space	*/
	if (pairing == UM_POINT_CIRC)
	{
		e1 = (struct UM_point_rec *) ((switched)? eptr1: eptr0);
      vp = &dp;
	   um_cctmtf(e1->pt, ttinv, newpt0);
	   um_cctmtf(hostp->center, ttinv, cen0);
      uvc0[0] = cen0[0] - newpt0[0];
      uvc0[1] = cen0[1] - newpt0[1];
      uvc0[2] = cen0[2] - newpt0[2];
      vp->rel_num = UM_LINE_REL;
      um_vctovc (newpt0,vp->spt);
      um_vctovc (cen0,vp->ept);
	   ttinv = UM_DEFAULT_TF;
	}
   else
	{
		vp = (struct UM_line_rec *) ((switched)? eptr1: eptr0);
	
	  /* unit direction vector (transformed) for line	*/
	   um_vcmnvc(vp->ept, vp->spt, uvc0);
	  	um_vctmtf(uvc0, ttinv, uvc0);
	
		/* basepoint (transformed) for e0	*/
  		um_cctmtf(vp->spt, ttinv, newpt0);
	}	
 	um_unitvc(uvc0, uvc0);
	um_ilncir(newpt0, uvc0, hostp->center, hostp->nvec, 
	hostp->radius, nintp, ipt);
	
			/* set intersection multiplicity	*/
			if (*nintp == 1)
			{
				ibuf[0].order = 1;		/* tangent intersection	*/
			}
			else
			{
				ibuf[0].order = 0;		/* transverse intersections	*/
				ibuf[1].order = 0;
			}
	
			for ( i = 0; i < *nintp; ++i)
			{
				um_vctovc(ipt[i], ibuf[i].pt);
	
				if (switched)	/* line is second entity	*/
				{
					um_cctou(vp, ttinv,ipt[i], &(ibuf[i].u1), dist + 1);
					um_cctou(hostp, UM_DEFAULT_TF, ipt[i], &(ibuf[i].u0), dist);
				}
				else
				{
					um_cctou(vp, ttinv, ipt[i], &(ibuf[i].u0), dist);
					um_cctou(hostp, UM_DEFAULT_TF, ipt[i], &(ibuf[i].u1), dist + 1);
				}
			}
		}
		break;
	
		case	UM_LINE_CONIC:
		{
			struct	UM_line_rec	*vp;		/* tmp ptr	*/
			struct	UM_conic_rec	*hostp;	/* IORDER second	*/
			UM_coord	newpt0;
			UM_vector	uvc0;
			UM_coord	ipt[2];
			int mult,k;
	
			hostp = (struct UM_conic_rec *) ((switched)? eptr0: eptr1);
	
			/* get visitor in host definition space	*/
			vp = (struct UM_line_rec *) ((switched)? eptr1: eptr0);
	
			/* unit direction vector (transformed) for line	*/
			um_vcmnvc(vp->ept, vp->spt, uvc0);
			um_vctmtf(uvc0, ttinv, uvc0);
			um_unitvc(uvc0, uvc0);
	
			/* basepoint (transformed) for e0	*/
			um_cctmtf(vp->spt, ttinv, newpt0);
	
			um_ilnconic(newpt0, uvc0, hostp,  nintp, ipt, &mult);
	
			for ( i = 0, k=0; i < *nintp; ++i)
			{
				um_vctovc(ipt[i], ibuf[k].pt);
				ibuf[i].order = (mult == 1)? 0: 1;
	
				if (switched)	/* line is second entity	*/
				{
					um_cctou(vp, ttinv, ipt[i], &(ibuf[k].u1), dist + 1);
					if (um_cctou(hostp, UM_DEFAULT_TF, ipt[i],
                      &(ibuf[k].u0), dist) == 0) k++;
				}
				else
				{
					um_cctou(vp, ttinv, ipt[i], &(ibuf[k].u0), dist);
					if (um_cctou(hostp, UM_DEFAULT_TF, ipt[i], 
                      &(ibuf[k].u1), dist + 1) == 0) k++;
				}
			}
         *nintp = k;
		}
		break;
	
		case	UM_CIRC_CIRC:		/* assuming switched == UU_FALSE	*/
		{
			UM_coord	vcenter;
			UM_coord	vnvec;
			UM_coord		ipt[2];
	
			um_cctmtf(((struct UM_circle_rec *) eptr0)->center, ttinv, vcenter);
			um_vctmtf(((struct UM_circle_rec *)eptr0)->nvec, ttinv, vnvec);
	
			um_icircir(
				vcenter, vnvec,
				((struct UM_circle_rec *)eptr0)->radius,
				((struct UM_circle_rec *)eptr1)->center,
				((struct UM_circle_rec *)eptr1)->nvec,
				((struct UM_circle_rec *)eptr1)->radius,
				nintp, ipt
				);
	
			/* set intersection multiplicity	*/
			if (*nintp == 1)
			{
				ibuf[0].order = 1;
			}
			else
			{
				ibuf[0].order = 0;
				ibuf[1].order = 0;
			}
	
			/* load rest of ibuf[]	*/
			for ( i = 0; i < *nintp; ++i)
			{
				um_vctovc(ipt[i], ibuf[i].pt);
				um_cctou(eptr0, ttinv, ipt[i], &(ibuf[i].u0), dist);
				um_cctou(eptr1, UM_DEFAULT_TF, ipt[i], &(ibuf[i].u1), dist + 1);
			}
		}
		break;
	
		default:
		{ 
/* will see if can be done through rbsplines	*/
/* 
...vp 11-apr-95 removed AG support (next call)

            ret_val = um_unicrv_isect_sp(eptr0, tfmat0, eptr1, tfmat1,
				common_plane, nintp, no_ibuf, ibuf);   

...replaced by following calls
*/ 
			elist[0] = (struct UM_rbsplcrv_rec *) eptr0;
			elist[1] = (struct UM_rbsplcrv_rec *) eptr1;
         tlist[0] = (UU_REAL *) tfmat0;
         tlist[1] = (UU_REAL *) tfmat1;
			ret_val = um_crv_isect_ext (elist, tlist, common_plane,nintp, no_ibuf, ibuf);
/*
...If any original CV is RBspl check if its extension intersects
...with second curve or extension
*/
			if (eptr0->rel_num == UM_RBSPLCRV_REL || eptr1->rel_num == UM_RBSPLCRV_REL)
				ret_val = um_isect_rbspl_ext (elist,tlist,common_plane, nintp,no_ibuf,ibuf);
		}
		break;
	}
	
Done:
#ifdef	UM_ISECTTRC
		sprintf(UM_sbuf, "um_isect_sp: returning %d, numint=%d", ret_val, *nintp);
		um_pscroll(UM_sbuf);
#endif
	uu_dexitstatus("um_isect_sp", ret_val);
	return (ret_val);
}

/*********************************************************************
** E_FUNCTION :  um_isect_line(pt, vec, eptr, tfmat, nintp, no_ibuf, ibuf)
**       intersects line with curve.   This is just a front end to 
**			um_isec
**    PARAMETERS   
**       INPUT  : 
**				pt						--	start point on line
**				vec					--	vector to endpoint of line
**					NOTE: vec need not be unit length, but its length will affect
**							the line parameter value of intersection points
**          eptr					--	pointer to curves
**				tfmat					--	transformations for curve to space of line
**										UM_DEFAUTL_TF is OK
**				nintp					--	pointer to integer output
**				no_ibuf				--	number of intersections allocated in ibuf
**				ibuf					--	pointer to UM_isect buffers to hold output
**       OUTPUT :  
**          *nintp				--	number of intersections found
**				ibuf					--	list of UM_isect entries filled in
**    RETURNS      :
**					0	 if OK
**					UM_BADCLASS, UM_NOTPLANAR, UM_BUFFULL, UM_BADENTITY,
**					UM_BADTFMAT, UM_UNIMPLEMENTED, UM_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : THIS DOES NOT WORK YET: CAN'T COPY ENTITY 'l'
*********************************************************************/
int um_isect_line(pt, vec, eptr, tfmat, nintp, no_ibuf, ibuf)
	UM_coord	pt;
	UM_coord	vec;
	struct UM_crvdatabag	*eptr;
	UM_transf	tfmat;
	int	*nintp;
	int	no_ibuf;
	UM_isect	ibuf[];

	{
	struct	UM_line_rec	l;
	int		ret_val;

	uu_denter(UU_MTRC,(us,"um_isect_line(eptr(r,k,t)=(%d,%x,%x), no_ubuf=%d)",
				eptr->rel_num, eptr->key, tfmat, no_ibuf));
	
	l.rel_num = UM_LINE_REL;
	um_vctovc(pt, l.spt);
	um_vcplvc(pt, vec, l.ept);

	/* pass along to um_isect()	*/
	ret_val =  um_isect(&l, UM_DEFAULT_TF, eptr, tfmat, nintp, no_ibuf, ibuf);
	
	uu_dexitstatus("um_isect_line", ret_val);
	return (ret_val);
	}

/*********************************************************************
**E_FUNCTION:um_isect_comp_sp(eptr0, tfmat0, eptr1, tfmat1, nintp, no_ibuf, ibuf)
**       Intersect a compsite curve with another curve.
**    PARAMETERS   :	\
**    RETURNS      :	(	SAME AS um_isect()
**    SIDE EFFECTS :	(
**    WARNINGS     :	/
*********************************************************************/
int
um_isect_comp_sp(eptr0, tfmat0, eptr1, tfmat1, common_plane, nintp, no_ibuf, ibuf)
	struct UM_crvdatabag	*eptr0;
	UM_transf	tfmat0;
	struct UM_crvdatabag	*eptr1;
	UM_transf	tfmat1;
	UU_REAL		common_plane[2][3];
	int	*nintp;
	int	no_ibuf;
	UM_isect	ibuf[];

	{
	int	ret_val;

	struct UM_compcrv_rec *ccrv;		/* pointer to the composite curve */
	struct UC_entitydatabag	subcrv;	/* databag for subcurve geometry */
	UU_REAL *ctfmat;						/* pointer to tfmat of composite curve	*/
	UM_transf subcrv_tfmat;				/* transformation for subcurve */
	int subcrv_no_int;					/* number of intersection with subcurve */
	UM_isect *subcrv_ibuf;				/* intersection points on subcurve */
	UM_transf comp_tfmat;				/* composition of transformations */
	int the_comp_crv;						/* (eptr0 composite)? 0: 1	*/
	UU_REAL last_endparam;				/* cid[].endparam for previous subcurve*/
	int i;									/* index of subcurves */
	int j;									/* ... on ith_curve */
	UU_REAL delta_u;						/* param. range allocated to subcurve */
	UU_REAL *subcrv_uptr;				/* pointer to u parameter on subcurve */
	UU_REAL *ccrv_uptr;					/* pointer to u parameter on composite */
	UU_LOGICAL on_curve;
	UU_LOGICAL on_extended_curve;

	uu_denter(UU_MTRC,(us,"um_isect_comp_sp(e0(r,k,t)=(%d,%x,%x), e1(r,k,t)=(%d,%x,%x),no_ibuf=%d)",
				eptr0->rel_num, eptr0->key, tfmat0,
				eptr1->rel_num, eptr1->key, tfmat1, no_ibuf));

	/* malloc space for intersections on sub curve */
	subcrv_ibuf = (UM_isect *) uu_malloc(no_ibuf*sizeof(UM_isect));

	/* determine which curve is the composite curve */
	if (eptr0->rel_num == UM_COMPCRV_REL)
		{
		ccrv = (struct UM_compcrv_rec *) eptr0;
		ctfmat = (UU_REAL *) tfmat0;
		the_comp_crv = 0;
		}
	else
		{
		ccrv = (struct UM_compcrv_rec *) eptr1;
		ctfmat = (UU_REAL *) tfmat1;
		the_comp_crv = 1;
		}

	last_endparam = 0.0;

	/* intersect each subcurve of the composite with the other curve */
	for (i=0; i<ccrv->no_cid; i++)
		{
		/* retrieve the subcurve data and transformation */
		subcrv.key = ccrv->cid[i].crvid;
		uc_retrieve_data(&subcrv, sizeof(subcrv));
		um_get_transformation(subcrv.key, subcrv_tfmat);
		um_tftmtf(subcrv_tfmat, ctfmat, subcrv_tfmat);

#ifdef	UM_ISECTTRC
		sprintf(UM_sbuf, "um_isect_comp_sp: subcurve: key=%d, rel_num=%d",
			subcrv.key, subcrv.rel_num);
		um_pscroll(UM_sbuf);
#endif

		/* intersect the subcurve and the other curve */
		subcrv_no_int = 0;
		if (the_comp_crv == 0)
			ret_val = um_isect_sp(&subcrv, subcrv_tfmat, eptr1, tfmat1,
				common_plane, &subcrv_no_int, no_ibuf, subcrv_ibuf);
		else
			ret_val = um_isect_sp(eptr0, tfmat0, &subcrv, subcrv_tfmat, 
				common_plane, &subcrv_no_int, no_ibuf, subcrv_ibuf);
		if (ret_val != UM_OK) goto Done;

		/* get parameter interval width for subcurve	*/
		delta_u =	ccrv->cid[i].endparam - last_endparam;

		/* for each intersection point fix up intersection */
		for (j=0; j<subcrv_no_int; j++)
			{
			subcrv_uptr = (the_comp_crv == 0)?
						&subcrv_ibuf[j].u0:
						&subcrv_ibuf[j].u1;
			ccrv_uptr = (the_comp_crv == 0)?
						&ibuf[*nintp].u0:
						&ibuf[*nintp].u1;

#ifdef	UM_ISECTTRC
			sprintf(UM_sbuf, "comp: open=%d, subcrv: rev=%d, u=%g",
				ccrv->open, ccrv->cid[i].reverse, *subcrv_uptr);
			um_pscroll(UM_sbuf);
#endif

			/* determine if intersection point on subcurve is really on 
				composite curve */
			on_curve = on_extended_curve = UU_FALSE;
			if (*subcrv_uptr < -UM_FUZZ || *subcrv_uptr > 1.0 + UM_FUZZ)
				{
				on_extended_curve =
					(ccrv->open) &&
				 	((i==0) || (i==ccrv->no_cid)) &&
				 	((*subcrv_uptr < 0.0 && !ccrv->cid[i].reverse) ||
					 (*subcrv_uptr > 1.0 && ccrv->cid[i].reverse));
				}
			else
				{
				on_curve = UU_TRUE;
				}

			/* remap intersection points which lie on the composite */
			if ((on_curve) || (on_extended_curve))
				{
				zbytecp(ibuf[*nintp], subcrv_ibuf[j]);
				if (ccrv->cid[i].reverse)
					*ccrv_uptr = last_endparam + (delta_u * (1.0 - *subcrv_uptr));
				else
					*ccrv_uptr = last_endparam + (delta_u * (*subcrv_uptr));

#ifdef	UM_ISECTTRC
				sprintf(UM_sbuf,"subcrv_ibuf[%d].u=%g is on composite curve",
					i, *subcrv_uptr);
				um_pscroll(UM_sbuf);
				sprintf(UM_sbuf,"ibuf[%d].pt = (%g,%g,%g)", *nintp,
					ibuf[*nintp].pt[0], ibuf[*nintp].pt[1], ibuf[*nintp].pt[2]);
				um_pscroll(UM_sbuf);
				sprintf(UM_sbuf,"        .u0 = %g", ibuf[*nintp].u0);
				um_pscroll(UM_sbuf);
				sprintf(UM_sbuf,"        .u1 = %g", ibuf[*nintp].u1);
				um_pscroll(UM_sbuf);
				sprintf(UM_sbuf,"        .t0 = %g", ibuf[*nintp].t0);
				um_pscroll(UM_sbuf);
				sprintf(UM_sbuf,"        .t1 = %g", ibuf[*nintp].t1);
				um_pscroll(UM_sbuf);
#endif

				(*nintp)++;
				}
			else
				{

#ifdef	UM_ISECTTRC
				sprintf(UM_sbuf,"subcrv_ibuf[%d].u=%g not on composite curve",
					i, *subcrv_uptr);
				um_pscroll(UM_sbuf);
#endif

				}
			}

		/* save endparam	*/
		last_endparam = ccrv->cid[i].endparam;
		}

Done:
	uu_free(subcrv_ibuf);
	uu_dexitstatus("um_isect_comp_sp", ret_val);
	return (ret_val);
	}

/*********************************************************************
** E_FUNCTION:um_icoplnrbsplcrv(r0, r1, plane, no_isect, max_isect, isect_buff)
**       sets-up and call recursive routine for intersection of planar
**			rat. bsplines
**    PARAMETERS   
**       INPUT  : 
**				r0, r1	--	pointers to rbspline curve entities
**				plane		--	plane shared by curves (point, normal)
**				no_isect	--	pointer to integer
**				max_isect	--	number of slots in user supplied output buffer
**				isect_buff-	user-supplied output buffer
**       OUTPUT :  
**          no_isect	--	number of intersections found
**    RETURNS      : 0 if OK, other errors (TBD) for system failure (e.g., out
**			of memory), reaching max recursive depth before curve is well-behaved,
**			and so on.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static	UU_REAL	(*curve_plane)[3];
static	UM_isect	*output_buffer;		/* user supplied buffer	*/
static	int		output_size;			/* size of user buffer	*/
static	int		output_count;			/* index of next free UM_isect	*/

static	struct	UM_rbsplcrv_rec	*rbspl0;
static	struct	UM_rbsplcrv_rec	*rbspl1;

um_icoplnrbsplcrv(r0, r1, plane, no_isect, max_isect, isect_buff)
	struct UM_rbsplcrv_rec *r0;
	struct UM_rbsplcrv_rec *r1;
	UU_REAL	plane[2][3];
	int		*no_isect;
	int		max_isect;
	UM_isect	isect_buff[];
  {
	struct UM_rbsplcrv_rec *ptr0, *rb0;
	struct UM_rbsplcrv_rec *ptr1, *rb1;
	C2vh	cvh0;
	C2vh	cvh1;
   UM_param t0[2], t1[2];
	int	nbytes, trimfl[2], status, ret_val;

	uu_denter(UU_MTRC,(us,
             "um_icoplnrbsplcrv(e0(r,k)=(%d,%x),e1(rel,key)=(%d,%x),no_ubuf=%d)",
             r0->rel_num, r0->key, r1->rel_num, r1->key, max_isect));

/*
...make sure that curves are RBsplines
*/
   rb0 = rb1 = UU_NULL;
   if (r0->rel_num != UM_RBSPLCRV_REL)
     {
      ptr0 = rb0 = (struct UM_rbsplcrv_rec *) uu_toolmalloc (um_curve_size(r0));
      status = um_rbcrv_frmnclcrv (r0,ptr0);
     }
   else
     ptr0 = (struct UM_rbsplcrv_rec *) r0;

   if (r1->rel_num != UM_RBSPLCRV_REL)
     {
      ptr1 = rb1 = (struct UM_rbsplcrv_rec *) uu_toolmalloc (um_curve_size(r1));
      status = um_rbcrv_frmnclcrv (r1,ptr1);
     }
   else
     ptr1 = (struct UM_rbsplcrv_rec *) r1;
/*
...redifine curves if they are trimmed since umi_isect1 does not
...work with modified t0, t1 and not adjusted ptr*.pt list to
...corresponding range of t0-t1 (see hulls list below). The simple
...solution for trimmed cv is intersect original curve and disregard
...IO points outside trimming range. vp 11-May-95.
*/
   trimfl[0] = um_trim_reset (ptr0,&t0[0],&t1[0]);
   trimfl[1] = um_trim_reset (ptr1,&t0[1],&t1[1]);

	/* get convex hulls for splines	*/
	cvh0.no_pt	=		ptr0->no_pt;
	cvh0.pt		=	(UU_REAL (*)[3]) ptr0->pt;
   cvh0.cv  = (int *) uu_toolmalloc ((cvh0.no_pt+1)*sizeof(int));
	um_plncvh(&cvh0, plane);

	cvh1.no_pt	=	ptr1->no_pt;
	cvh1.pt		=	(UU_REAL (*)[3])	ptr1->pt;
   cvh1.cv  = (int *) uu_toolmalloc ((cvh1.no_pt+1)*sizeof(int));
	um_plncvh(&cvh1, plane);

	/** initialize list for output	**/
	output_buffer = isect_buff;	/* sorry, just arrays	*/
	output_size = max_isect;
	output_count = 0;

	/** set external pointer to plane	**/
	curve_plane = plane;

	/** save pointers to whole curves **/
	rbspl0 = ptr0;
	rbspl1 = ptr1;

	/** first call to recursive subroutine to build tree	**/
	/** FIX: fix up this call	**/
	ret_val = umi_isect1(0, (UU_REAL) 0.0, (UU_REAL) 1.0, ptr0, &cvh0,
                           (UU_REAL) 0.0, (UU_REAL) 1.0, ptr1, &cvh1);
/*
...check intersections for trimmed curves if
...they are out of limits,
...set trimmed curves back to be trimmed.
*/
   if (trimfl[0] > 0 || trimfl[1] > 0)
     {
      um_trim_isect_verify (ptr0,ptr1,t0,t1);
      if (trimfl[0] != 0) um_trim_set (ptr0,t0[0],t1[0]); 
      if (trimfl[1] != 0) um_trim_set (ptr1,t0[1],t1[1]); 
     }

#ifdef	UM_ISECTTRC
	sprintf(UM_sbuf,
	"after intersection recursion: ret_val: %d, number of isects: %d",
	ret_val, output_count);
	um_pscroll(UM_sbuf);
#endif

	*no_isect = output_count;
   if (rb0 != UU_NULL) uu_toolfree (rb0);
   if (rb1 != UU_NULL) uu_toolfree (rb1);
   uu_toolfree (cvh0.cv);
   uu_toolfree (cvh1.cv);
	uu_dexitstatus("um_icoplnrbsplcrv", ret_val);
   if (ret_val != UU_SUCCESS && *no_isect > 0) ret_val = UU_SUCCESS;
	return (ret_val);
		}
/*********************************************************************
**		I_FUNCTION: umi_isect1(depth, t0, intrv0, r0, cvh0, t1, intrv1, r1, cvh1)
**       (recursive) uses convex hulls to find intersections of Bsplines.
**			calls iterator as soon as possible to home in on precise point
**    PARAMETERS   
**       INPUT  : 
**				depth		--		current recursion depth (first call is depth 0)
**				t0, t1	--		lower parameter value of current interval
**				intrv0, 1--		width of parameter interval
**				r0, r1	--		pointers to rational bspline records
**				cvh0, cvh1-		pointers to corresponding convex hulls
**       OUTPUT :  
**          none
**    RETURNS      : error from lower calls (FIX: specify)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

umi_isect1(depth, t0, intrv0, r0, cvh0, t1, intrv1, r1, cvh1)
	int		depth;
	UU_REAL	t0;
	UU_REAL	intrv0;
	struct	UM_rbsplcrv_rec	*r0;
	C2vh		*cvh0;
	UU_REAL	t1;
	UU_REAL	intrv1;
	struct	UM_rbsplcrv_rec	*r1;
	C2vh		*cvh1;
  {
   struct	UM_rbsplcrv_rec *left0, *right0, *left1, *right1;
	int	intersect;
	int	colinear;
	int	nint, *lh0cv, *lh1cv, *rh0cv, *rh1cv;
	UU_REAL	intpt[3];
	int	ret_val = 0;
	int	split_bitmap;	/* used as (2-wide) bitmap to note which
								 * curve(s) need further splitting
								 */
	UU_REAL	i0;	/* half of current intervals	*/
	UU_REAL	i1, i5 = .5;

	left0 = left1 = right0 = right1 = UU_NULL;
   lh0cv = lh1cv = rh0cv = rh1cv = UU_NULL;

	uu_denter(UU_MTRC,(us,"umi_isect1()"));

#ifdef	UM_ISECTTRC
		sprintf(UM_sbuf, "---\num_isec1: enter. depth = %d", depth);
		NclxDbgPstr(UM_sbuf);
		sprintf(UM_sbuf, "t/int: %f/%f\t%f/%f", t0, intrv0, t1, intrv1);
		NclxDbgPstr(UM_sbuf);
#endif

	/* only proceed if convex hulls intersect	*/
	um_iplncvh( 0, cvh0, cvh1, &intersect, &colinear, &nint, intpt);

	if (!intersect)
		{
#ifdef	UM_ISECTTRC
		sprintf(UM_sbuf,"\n\tConvex hulls don't intersect:\n\t\tdepth = %d",
			depth);
		NclxDbgPstr(UM_sbuf);
#endif
		ret_val = 0;
		goto Done;
		}

	/*	set split_bitmap: 
	 *		bit on means split curve again
	 *		l.s. bit is curve 0
	 */

	/*
	 * ENHANCEMENT NOTE: to say that a curve does not need splitting,
	 * or often more precisely that neither curve need further splitting
	 * is a claim that it is wise to try the iterative procedures in
	 * um_isect_refine.  Since that routine has channels to  protest if
	 * things diverge, our most important job is to make sure that two
	 * convex hulls (alt. b-spline segments) contain no more than one
	 * intersection.  Techniques to assert this may involve curvature
	 * bounds, or, I claim, extreme values of the slope of the control
	 * polyline.
	 */

	split_bitmap = (1<<1)|1;
	if (cvh0->shape & UM_CVH_LINEAR)
		{
		split_bitmap &= ~1;
		}
	if (cvh1->shape & UM_CVH_LINEAR)
		{
		split_bitmap &= ~(1<<1);
		}
#ifdef	UM_ISECTTRC
	sprintf(UM_sbuf,"misect1: split_bitmap: %x, depth: %d", split_bitmap, depth);
	NclxDbgPstr(UM_sbuf);
#endif

	/* test if neither curve segment is to be split further	*/
	if (split_bitmap == 0)
		{
		/** NEED: Leaf handling	**/
		if (ret_val = um_isect_refine(t0, intrv0, r0, cvh0, t1, intrv1, r1,cvh1))
			{
			if (depth < UM_ISECTMAXDEPTH)
				{
#ifdef	UM_ISECTTRC
				sprintf(UM_sbuf, 
					"um_isect_refine insists that we split some more: %d", ret_val);
				NclxDbgPstr(UM_sbuf);
#endif
				split_bitmap = (1<<1)|1;		/* split both curves some more	*/
				}
			else
				{
#ifdef	UM_ISECTTRC
				sprintf(UM_sbuf,
					"isect_refine wants more, we cannot oblige (depth = %d).",
					depth);
				NclxDbgPstr(UM_sbuf);
#endif
				goto Done;
				}
			}
		}

	/* at this point, split_bitmap may have been non-zero since
	 * inception, set to some value because of trouble in isect_refine,
	 * or left zero by isect refine, in which case we are done.
	 */

	depth++ ;	/* for next generation of calls	*/

	/* half of current intervals	*/
	i0 = intrv0/2.0;
	i1 = intrv1/2.0;
/*
.....Added logic to exit if the split curve has zero points
.....to prevent memory problems - Andrew 1/31/13
*/
	switch (split_bitmap)
		{

		 /*************************************/
		 /** Intersection found, just return **/
		 /*************************************/
	 case	0:
		break;
	
		 /************************/
		 /** split both curves	**/
		 /************************/
	 case	(1<<1)|1:
			{
		C2vh		lcvh0, rcvh0, lcvh1, rcvh1;
/*
...allocate space for split curves,
...removed ur_setup_data call since it is in um_c7_splitrbsplcrv
*/
      um_alloc_eq_curve (r0,&left0);
      um_alloc_eq_curve (r0,&right0);
      um_alloc_eq_curve (r1,&left1);
      um_alloc_eq_curve (r1,&right1);
	  ret_val = um_c7_splitrbsplcrv(r0, &i5, &i5, left0, right0);
	  if (ret_val == UU_FAILURE || left0->no_pt == 0 ||
		  right0->no_pt == 0)
	  {
		  if (ret_val != UU_FAILURE) ret_val = UU_FAILURE;
		  goto Done;
	  }
	  um_c7_splitrbsplcrv(r1, &i5, &i5, left1, right1);
	  if (ret_val == UU_FAILURE || left1->no_pt == 0 ||
		  right1->no_pt == 0)
	  {
		  if (ret_val != UU_FAILURE) ret_val = UU_FAILURE;
		  goto Done;
	  }
/*
...fake keys to have access to id matrix when um_isect_refine is called
*/ 
      left0->key = r0->key;
      left1->key = r1->key;
      right0->key = r0->key;
      right1->key = r1->key;

		/* get convex hulls for new splines	*/
		lcvh0.no_pt	=	left0->no_pt;
      lcvh0.cv = lh0cv = (int *) uu_toolmalloc ((lcvh0.no_pt+1)*sizeof(int));
		lcvh0.pt		=	(UU_REAL (*)[3])	left0->pt;
		um_plncvh(&lcvh0, curve_plane);

		rcvh0.no_pt	=	right0->no_pt;
      rcvh0.cv = rh0cv = (int *) uu_toolmalloc ((rcvh0.no_pt+1)*sizeof(int));
		rcvh0.pt		=	(UU_REAL (*)[3])	right0->pt;
		um_plncvh(&rcvh0, curve_plane);

		lcvh1.no_pt	=	left1->no_pt;
      lcvh1.cv = lh1cv = (int *) uu_toolmalloc ((lcvh1.no_pt+1)*sizeof(int));
		lcvh1.pt		=	(UU_REAL (*)[3])	left1->pt;
		um_plncvh(&lcvh1, curve_plane);

		rcvh1.no_pt	=	right1->no_pt;
      rcvh1.cv = rh1cv = (int *) uu_toolmalloc ((rcvh1.no_pt+1)*sizeof(int));
		rcvh1.pt		=	(UU_REAL (*)[3])	right1->pt;
		um_plncvh(&rcvh1, curve_plane);
 
		/** recursive calls **/
		/* left-left	*/
		if (ret_val= 
			umi_isect1(depth, t0, i0, left0, &lcvh0, t1, i1, left1, &lcvh1))
			goto Done;

		/* right-left */
		if (ret_val=
			umi_isect1(depth, t0+i0, i0, right0, &rcvh0, t1, i1, left1, &lcvh1))
			goto Done;

		/* left-right */
		if (ret_val=
			umi_isect1(depth, t0, i0, left0, &lcvh0, t1+i1, i1, right1, &rcvh1))
			goto Done;

		/* right-right	*/
		if (ret_val=
			umi_isect1(depth, t0+i0,i0, right0,&rcvh0, t1+i1,i1, right1,&rcvh1))
			goto Done;

			}
		break;

		 /************************/
		 /** split only first	**/
		 /************************/
	 case	(0<<1)|1:
			{
		C2vh		lcvh0, rcvh0;

      um_alloc_eq_curve (r0,&left0);
      um_alloc_eq_curve (r0,&right0);
	  ret_val = um_c7_splitrbsplcrv(r0, &i5, &i5, left0, right0);
	  if (ret_val == UU_FAILURE || left0->no_pt == 0 ||
		  right0->no_pt == 0)
	  {
		  if (ret_val != UU_FAILURE) ret_val = UU_FAILURE;
		  goto Done;
	  }
      left0->key = r0->key;
      right0->key = r0->key;

		/* get convex hulls for new splines	*/
		lcvh0.no_pt	=	left0->no_pt;
      lcvh0.cv = lh0cv = (int *) uu_toolmalloc ((lcvh0.no_pt+1)*sizeof(int));
		lcvh0.pt		=	(UU_REAL (*)[3])	left0->pt;
		um_plncvh(&lcvh0, curve_plane);

		rcvh0.no_pt	=	right0->no_pt;
      rcvh0.cv = rh0cv = (int *) uu_toolmalloc ((rcvh0.no_pt+1)*sizeof(int));
		rcvh0.pt		=	(UU_REAL (*)[3])	right0->pt;
		um_plncvh(&rcvh0, curve_plane);


		/* left-whole	*/
		if (ret_val=
			umi_isect1(depth, t0, i0, left0, &lcvh0, t1, intrv1, r1, cvh1))
			goto Done;

		/* right-whole	*/
		if (ret_val=
			umi_isect1(depth, t0+i0, i0, right0, &rcvh0, t1, intrv1, r1, cvh1))
			goto Done;

			}
		break;

		 /************************/
		 /** split only second	**/
		 /************************/
	 case	(1<<1)|0:
			{
		C2vh		lcvh1, rcvh1;

      um_alloc_eq_curve (r1,&left1);
      um_alloc_eq_curve (r1,&right1);
	  ret_val = um_c7_splitrbsplcrv(r1, &i5, &i5, left1, right1);
	  if (ret_val == UU_FAILURE || left1->no_pt == 0 ||
		  right1->no_pt == 0)
	  {
		  if (ret_val != UU_FAILURE) ret_val = UU_FAILURE;
		  goto Done;
	  }
      left1->key = r1->key;
      right1->key = r1->key;

		/* get convex hulls for new splines	*/
		lcvh1.no_pt	=	left1->no_pt;
      lcvh1.cv = lh1cv = (int *) uu_toolmalloc ((lcvh1.no_pt+1)*sizeof(int));
		lcvh1.pt		=	(UU_REAL (*)[3])	left1->pt;
		um_plncvh(&lcvh1, curve_plane);

		rcvh1.no_pt	=	right1->no_pt;
      rcvh1.cv = rh1cv = (int *) uu_toolmalloc ((rcvh1.no_pt+1)*sizeof(int));
		rcvh1.pt		=	(UU_REAL (*)[3])	right1->pt;
		um_plncvh(&rcvh1, curve_plane);
 
		/* whole-left	*/
		if (ret_val=
			umi_isect1(depth, t0, intrv0, r0, cvh0, t1, i1, left1, &lcvh1))
			goto Done;

		/* whole-right	*/
		if (ret_val=
			umi_isect1(depth, t0, intrv0, r0, cvh0, t1+i1, i1, right1, &rcvh1))
			goto Done;

			}
		break;

	 default:
		 sprintf(UM_sbuf, "bad bitmap in isect1: %x", split_bitmap);
		 NclxDbgPstr(UM_sbuf);
		}

Done:
   if (left0 != UU_NULL) uu_toolfree (left0);
   if (left1 != UU_NULL) uu_toolfree (left1);
   if (right0 != UU_NULL) uu_toolfree (right0);
   if (right1 != UU_NULL) uu_toolfree (right1);
   if (lh0cv != UU_NULL) uu_toolfree (lh0cv);
   if (lh1cv != UU_NULL) uu_toolfree (lh1cv);
   if (rh0cv != UU_NULL) uu_toolfree (rh0cv);
   if (rh1cv != UU_NULL) uu_toolfree (rh1cv);

	uu_dexitstatus("um_isect1", ret_val);
	return (ret_val);
  }
/*********************************************************************
**    E_FUNCTION :  um_isect_refine(t0, i0, r0, cvh0,t1, i1, r1,cvh1)
**       applies tolerance test and then further iteration if necessary
**    PARAMETERS   
**       INPUT  : 
**          t0, t1	--	left endpoints of original rational bspline
**				i0, i1	--	intervals of original splines repr. by segments
**				r0, r1	--	rational bspline segment entity pointers
**				cvh0, cvh1-	pointers to corresponding convex hulls
**       OUTPUT :  
**          none
**    RETURNS      : 0 if no problem, -1 if iterations diverge,
**							-2 if tangent vectors are zero, -3 if static
**							output buffer is too small
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_isect_refine(initt0, i0, r0, cvh0, initt1, i1, r1,cvh1)
	UU_REAL initt0; 
	UU_REAL i0;
	struct UM_rbsplcrv_rec *r0; 
	C2vh	*cvh0;
	UU_REAL initt1;
	UU_REAL i1;
	struct UM_rbsplcrv_rec *r1;
	C2vh	*cvh1;
{
	int nint;                          /* number of intersections */
	UU_REAL	ipt[3], pt0[3], pt1[3];   /* intersection of tangents */
	UU_REAL	t0 = initt0, t1 = initt1;
	UU_REAL	dsdu0, dsdu1, a, det;     /* length of tgt vectors */
	UU_REAL	utgt0[3], utgt1[3];       /* unit tangent vectors	*/
	UU_REAL	dispvec0[3], dispvec1[3]; /* vector from curve to intersection point	*/
	struct UM_evcrvout ev0,ev1;        /* evaluator records for rbsplines	*/
	int ret_val, i, j;
	UU_REAL	um_dot(), um_mag(), um_dcccc();
	UU_REAL  min0,max0,min1,max1, difference, diff0, diff1;
	UU_LOGICAL points_are_close, load, lextend;
	UU_REAL dist,vc[3],vc1[3];
   
	uu_denter(UU_MTRC,(us,"um_isect_refine(%f, %f, %f, %f)",
								t0, i0, t1, i1));

	lextend = UU_FALSE;
	uc_init_evcrvout(rbspl0, &ev0);
	uc_init_evcrvout(rbspl1, &ev1);
 
	min0 = initt0 - 50.*i0; 
	min1 = initt1 - 50.*i1; 
	max0 = initt0 + 50.*i0; 
	max1 = initt1 + 50.*i1; 
	if (min0 < 0.) min0 = .0;
	if (min1 < 0.) min1 = .0;
	if (max0 > 1.) max0 = 1.0;
	if (max1 > 1.) max1 = 1.0;
   
	diff0 = diff1 = 1.;
	for ( i = 0; i < UM_MAXITERATE; ++i )
	{
/* 
..... evaluate both curves at t
*/
		um_ev7_rbsplcrv(UM_SECDERIV, t0, rbspl0, UM_DEFAULT_TF, &ev0);
		um_ev7_rbsplcrv(UM_SECDERIV, t1, rbspl1, UM_DEFAULT_TF, &ev1);
/*
..... preset t parameter for trivial cases (line, circle etc)
..... so it will not iterate crazy when t range is big. !!! Remember to 
..... pass original curve key to have access to transf MX (conics!)
*/
      if (i == 0)
      {
         if (i0 > .49 || i1 > .49)
         {
             UU_REAL v,dlt,mfuz;
             UM_transf tfmat;
    
             if (i0 > .49)
             {
                 uc_retrieve_transf (rbspl0->key,tfmat);
                 um_cctou (rbspl0,tfmat,ev1.cp,&v,&dlt);
                 if (v < t0+i0) t0 = v;
                 um_ev7_rbsplcrv(UM_FRSTDERIV, t0, rbspl0, UM_DEFAULT_TF, &ev0);
             }
             else
             {
                 uc_retrieve_transf (rbspl1->key,tfmat);
                 um_cctou (rbspl1,tfmat,ev0.cp,&v,&dlt);
                 if (t1 < t1+i1) t1 = v;
		           um_ev7_rbsplcrv(UM_FRSTDERIV, t1, rbspl1, UM_DEFAULT_TF, &ev1);
             }
         }
      }

#if	0
		sprintf(UM_sbuf, "isect_refine: t values: %f,  %f", t0, t1 );
		NclxDbgPstr(UM_sbuf);
		sprintf(UM_sbuf, "evaluator status: %d,  %d", ev0.status, ev1.status);
		NclxDbgPstr(UM_sbuf);
		sprintf(UM_sbuf, "points: <%f, %f, %f>, <%f, %f, %f>", 
			ev0.cp[0], ev0.cp[1], ev0.cp[2], ev1.cp[0], ev1.cp[1], ev1.cp[2]);
		NclxDbgPstr(UM_sbuf);
		sprintf(UM_sbuf, "tangents: <%f, %f, %f>, <%f, %f, %f>", 
			ev0.dcdu[0], ev0.dcdu[1], ev0.dcdu[2], ev1.dcdu[0],
			ev1.dcdu[1], ev1.dcdu[2]);
		NclxDbgPstr(UM_sbuf);
#endif

#ifdef	UM_ISECTTRC
		sprintf(UM_sbuf, "distance between curve points: %f", difference);
		NclxDbgPstr(UM_sbuf);
#endif

/* 
..... aak, 09-05-1997: made changes to improve trimming of almost || curves:
.....     1. criterium for loading intersec. point made stricter: load when not only points on both curves
.....        are close, but also when they stopped moving along the curves (for almost || curves  points on
.....        different curves can be very close but move in parallel towards a true intersection);
.....     2. in the case of almost || curves, linear approximation is replaced by quadratic
.....        ( s(t) = s(t0) + ds/dt*dt + 0.5* d^2 s/dt^2 * dt^2
*/
		difference = um_dcccc(ev0.cp, ev1.cp);
      if(i>0)
      {
		   diff0 = um_dcccc(ev0.cp,pt0);
		   diff1 = um_dcccc(ev1.cp,pt1);
      }
		points_are_close = (difference < .5*UM_ISECTTOLERANCE); 
		load = ( points_are_close && (diff0 < 0.5*UM_ISECTTOLERANCE) && (diff1 < 0.5*UM_ISECTTOLERANCE) );
Load:;
#ifdef	UM_ISECTTRC
	sprintf(UM_sbuf, "\ndifference = %g   diff0 = %g   diff1 = %g   load = %d\n",
		difference,diff0,diff1,load);
	NclxDbgPstr(UM_sbuf);
#endif
		if (load) 
		{
/* 
..... load average point into intersect. point list 
*/
			if (t0 < UM_DFUZZ)		
				um_vctovc(ev0.cp, ipt);
			else if (t1 < UM_DFUZZ)
				um_vctovc(ev1.cp, ipt);
			else		
			{						
				um_vcplvc(ev0.cp, ev1.cp, ipt);
				um_vctmsc(ipt, (UU_REAL) 0.5, ipt);
			}

			if (output_count < output_size)
			{
				for (j = 0; j < output_count; ++j)
				{
					if (um_dcccc(output_buffer[j].pt, ipt) < 5 * UM_FUZZ)
					{
						
#ifdef	UM_ISECTTRC
						sprintf(UM_sbuf,
			"duplicate intersection point: <%f, %f, %f>", ipt[0], ipt[1], ipt[2]);
						NclxDbgPstr(UM_sbuf);
#endif
						ret_val = 0;	/* don't have caller panic	*/
						goto Done;

					}
				}
/*
....Romove extension tangent points with no intersect but close for rbsplines
*/
				if (lextend)
				{
					if (um_dcccc(cvh0->pt, ipt) < 50 * UM_FUZZ ||
						um_dcccc(cvh1->pt, ipt) < 50 * UM_FUZZ)
					{
						ret_val = 0;
						goto Done;
					}
				}

				um_vctovc(ipt, output_buffer[output_count].pt);
				output_buffer[output_count].u0 = t0;
				output_buffer[output_count].u1 = t1;
				++output_count;
				ret_val = 0;
#ifdef	UM_ISECTTRC
						sprintf(UM_sbuf,
			"INTERSECTION POINT FOUND: <%f, %f, %f>", ipt[0], ipt[1], ipt[2]);
						NclxDbgPstr(UM_sbuf);
#endif
			}
			else
			{
				ret_val = -3;
			}
			goto Done;
		}

/* 
..... points are not close: find intersection of tangents to the curves and corresponding t-param.
..... for each curve for next iteration	
*/
		if ( (dsdu0 = um_mag(ev0.dcdu) ) < UM_FUZZ)
		{
			ret_val = -2;
			goto Done;
		}
		if ( (dsdu1 = um_mag(ev1.dcdu) ) < UM_FUZZ)
		{
			ret_val = -2;
			goto Done;
		}
		um_unitvc(ev0.dcdu, utgt0);
		um_unitvc(ev1.dcdu, utgt1);

		um_ilnln1(ev0.cp ,utgt0, ev1.cp, utgt1, &nint, ipt);
#ifdef	UM_ISECTTRC
	sprintf(UM_sbuf,"ev0 = %g,%g,%g   v0 = %g,%g,%g\n",ev0.cp[0],ev0.cp[1],
		ev0.cp[2],utgt0[0],utgt0[1],utgt0[2]);
	NclxDbgPstr(UM_sbuf);
	sprintf(UM_sbuf,"ev1 = %g,%g,%g   v1 = %g,%g,%g\n",ev1.cp[0],ev1.cp[1],
		ev1.cp[2],utgt1[0],utgt1[1],utgt1[2]);
	NclxDbgPstr(UM_sbuf);
	sprintf(UM_sbuf,"nint = %d   points_are_close = %d\n",nint,points_are_close);
	NclxDbgPstr(UM_sbuf);
#endif
		if (nint == 0)
		{

#ifdef	UM_ISECTTRC
		sprintf(UM_sbuf, "tangents don't intersect");
		NclxDbgPstr(UM_sbuf);
#endif
/* 
..... aak: 09-05-1997 
..... if tangents don't intersect and pts. are not close -> curves do not intersect, get out.
..... if tangents don't intersect but pts. are close -> return average
*/
/*
.....Just because the normals are the same
.....and the points are outside a tight tolerance
.....doesn't mean we should give up just yet
.....Halve the distance between the points and try again
.....Bobby  -  3/25/99
*/
			if(!points_are_close)
			{
				um_vcmnvc(ev0.cp,ev1.cp,vc);
				um_unitvc(vc,vc1);
				dist = um_dcccc(ev0.cp,ev1.cp) / 2.;
				um_vctmsc(vc1,dist,vc);
				um_vcplvc(ev1.cp,vc,ipt);

/*				ret_val = -1;
				goto Done;*/
			}
			else
			{
				load = UU_TRUE;
/*
....Check if extension tangent points or not
*/
				if (((cvh0->no_pt == 2 && cvh1->no_pt > 2) ||
					 (cvh0->no_pt > 2 && cvh1->no_pt == 2)) && i > 0)
					lextend = UU_TRUE;
				goto Load;
			}
		}

      um_vctovc(ev0.cp,pt0);
      um_vctovc(ev1.cp,pt1);
/*
..... project intersection points to curves: linear/quadratic approx. 	
*/
		um_vcmnvc(ipt, ev0.cp, dispvec0);
		a = um_dot(utgt0,ev0.d2cdu2); 
		if ( points_are_close && (fabs(a) > UM_DFUZZ) && 
		   ((det = dsdu0*dsdu0 + 2.*a*um_dot(dispvec0, utgt0)) >= 0.0) )
		{
			t0 += (-dsdu0 + sqrt(det) )/a;
		}
		else
		{
			t0 += um_dot(dispvec0, utgt0)/dsdu0;
		}

		um_vcmnvc(ipt, ev1.cp, dispvec1);
		a = um_dot(utgt1,ev0.d2cdu2); 
		if ( points_are_close && (fabs(a) > UM_DFUZZ) && 
		     ((det = dsdu1*dsdu1 + 2.*a*um_dot(dispvec1, utgt1)) >= 0.0) )
		{
			t1 += (-dsdu1 + sqrt(det) )/a;
		}
		else
		{
			t1 += um_dot(dispvec1, utgt1)/dsdu1;
		}

		t0 = (t0 > 1.)? 1.0: t0;
		t0 = (t0 < 0.)? 0.0: t0;
		t1 = (t1 > 1.)? 1.0: t1;
		t1 = (t1 < 0.)? 0.0: t1;

/**	OUTOUT ***
 * This attempt to insure unique intersection points found causes some to
 * be missed as of 8/10/85.  Replaced with less aggressive rejection 
 * test, with uniqueness (cheaply) obtained when points found are added to
 * the output list.
		if ( t0 < initt0 || t0 > initt0 + i0 || t1 < initt1 || t1 > initt1 + i1)
		if ( t0 < 0.0 || t0 > 1.0  || t1 < 0.0 || t1 > 1.0 )
 *** OUT OUT  ***/

		if ( t0 < min0 || t0 > max0 || t1 < min1 || t1 > max1)
		{	
/* 
..... projection is outside of segment	
*/

#ifdef	UM_ISECTTRC
		sprintf(UM_sbuf, "projection is outside space: %f, %f", t0, t1 );
		NclxDbgPstr(UM_sbuf);
#endif

			ret_val = -1;
			goto Done;
		}

	}  /*  for ( i = 0; i < UM_MAXITERATE; ++i ) */
/* 
..... have performed maximum iterations, still no luck	
*/

#ifdef	UM_ISECTTRC
	sprintf(UM_sbuf, "have performed maximum iterations.");
	NclxDbgPstr(UM_sbuf);
#endif

	ret_val = -1;

Done:
	uu_dexitstatus("um_isect_refine", ret_val);
	return (ret_val);
}

/*********************************************************************
**    E_FUNCTION :  um_cctou(eptr, tfmat, pt, u, distp)
**			Determine the logical parameter U such that the curve (EPTR)
**			evaluated at U will result in the specified point (PT).
**    PARAMETERS   
**       INPUT  : 
**          eptr			--	pointer to curve entity
**				tfmat 		--	trmasformation from curve space to space M
**				pt				--	point in space M
**				u, distp		--	pointer to real
**       OUTPUT :  
**          *u				--	INTERNAL parameter for intersection point
**								(may lie outside [0.0, 1.0] for some entities
**				*distp		--	distance from pt to C(u)
**    RETURNS      : 0 if OK, UM_FAILURE if inversion screws up,
**							UM_UNIMPLEMENTED
**    SIDE EFFECTS : none
**    WARNINGS     : although this routine suffices to find the "nearest
**				point" on a curve in some cases, no assurance is made unless
**				the point is very close to the curve.  (esp. conics)
*********************************************************************/
um_cctou(eptr, tfmat, pt, u, distp)
	struct	UM_crvdatabag	*eptr;
	UM_transf	tfmat;
	UM_coord		pt;
	UU_REAL		*u;
	UU_REAL		*distp;

	{
	int	ret_val = 0;
	UM_transf	tinv;	/* inverse of tfmat	*/
	UM_coord	dpt;		/* pt transformed to curve defn. space		*/
	struct UM_rbsplcrv_rec rbcv, *rbptr;

	uu_denter(UU_MTRC,(us,"um_cctou(eptr(r,k,t)=(%d,%x,%x), pt=<%f,%f,%f>)", 
				eptr->rel_num, eptr->key, tfmat, pt[0],pt[1],pt[2]));
	
	/* invert point to curve definition space	*/
	if (tfmat != UM_DEFAULT_TF)
		{
		um_inverttf(tfmat, tinv);
		um_cctmtf(pt, tinv, dpt);
		}
	else
		{
		um_vctovc(pt, dpt);
		}

	switch (eptr->rel_num)
		{
		case	UM_LINE_REL:
			{
			UM_coord		nearpt;
			UM_vector	linevec;	/* vector from head to tail of line	*/
			UM_vector	uvc;
			UM_vector	dispvec;	/* vector from start of line to nearpt	*/
			struct	UM_line_rec	*l;

			l = (struct UM_line_rec *) eptr;
			/* get nearest point on line	*/
			um_vcmnvc(l->ept, l->spt, linevec);
			um_unitvc(linevec, uvc);
			um_nptln( dpt, l->spt, uvc, nearpt);

			/* get parameter value of nearpt	*/
			um_vcmnvc( nearpt, l->spt, dispvec);
			*u = um_dot(dispvec, uvc)/um_mag(linevec);
			*distp = um_dcccc(dpt, nearpt);
			}
			break;

		case	UM_CIRCLE_REL:
			{
			UM_coord		plnpt;	/* nearest point in plane of circle	*/
			UM_coord		cpt;		/* nearest point on circle itself	*/
			UM_vector	uvc;		/* unit vc from center to cpt			*/
			UM_vector	cross;	/* cross of uvc and svec				*/
			UU_REAL		angle;	/* working space for t					*/
			UU_REAL		cosangle;	/* cosine of answer					*/
			struct	UM_circle_rec	*c;

			c = (struct UM_circle_rec *) eptr;

			/* project dpt to plane of circle	*/
			um_nptpln(dpt, c->center, c->nvec, plnpt);

			/* get unit vector from center to plnpt	*/
			um_vcmnvc(plnpt, c->center, uvc);
			um_unitvc(uvc, uvc);

			/* find angle from svec to uvc around nvec*/
			angle = um_angle2p(c->svec, uvc, c->nvec);


			uu_dprint(UU_MTRC, (us, "cctou: first angle %g", angle));

			if (c->dang < 0)
				{
				angle = angle - UM_TWOPI;
				uu_dprint(UU_MTRC, (us, "cctou: adjusted angle: %g",
					angle));
				}

			*u = angle/c->dang;
/* 
...can consider this condition ?
*/
         if (angle > c->dang + .5*(UM_TWOPI - angle))
            *u = (angle-UM_TWOPI) / c->dang;
			/* find distance between our point and dpt	*/
			um_vctmsc(uvc, c->radius, cpt);
			um_vcplvc(c->center, cpt, cpt);
			*distp = um_dcccc(cpt, dpt);
			}
			break;

		case	UM_CONIC_REL:
		{
			UU_REAL t;
			/* note: will need to fix up error handling if
			 * we wish to use this routine as a "nearest point" 
			 * function.
			 */
			/* get internal conic parameter, then convert it to u	*/
			if (umi_cc4_cctot(dpt, eptr,
				((struct UM_conic_rec *) eptr)->tfmat, &t, distp) < 0)
			{
				ret_val = UM_FAILURE;
            break;
			}
			umi_cc4_ttou(t, eptr, u);	/* remember: u is a pointer	*/
		}
			break;

/*
... aak 06-nov-1997: added the next case for uv curves;
.....Calling rbcrv routine directly does not work since structures do not
.....match - Andrew 4/9/13
*/
		case UM_UVCVONSF_REL:
			rbptr = &rbcv;
			ncl_cp_struct_uvcv_rbcv(eptr,&rbptr);
         um_cctou_rbcrv (rbptr, tfmat, dpt, u, distp);
			break;
		case UM_RBSPLCRV_REL:
         um_cctou_rbcrv (eptr, tfmat, dpt, u, distp);
			break;

		case NCL_CURVE_REL:
		{
	      struct UM_rbsplcrv_rec *ptr0;
         
			ptr0 = (struct UM_rbsplcrv_rec *) uu_toolmalloc (um_curve_size(eptr));
         um_rbcrv_frmnclcrv (eptr,ptr0);
         um_cctou_rbcrv (ptr0, tfmat, dpt, u, distp);
			uu_toolfree(ptr0);
		}
			break;

		default:
			ret_val = UM_UNIMPLEMENTED;
			break;
		}


Done:
	uu_dprint(UU_MTRC, (us, "cctou: returns: u = %g, distance = %g",
		*u,  *distp));

	uu_dexitstatus("um_cctou", ret_val);
	return (ret_val);
	}

/*********************************************************************
**    E_FUNCTION :  um_utot(eptr, u, t)
**			Determine the physical parameter T such that the curve (EPTR)
**			evaluated at T will result in the same point as the curve
**			evaluated at U.
**    PARAMETERS   
**       INPUT  : 
**          eptr			--	pointer to curve entity
**				u				--	logical parameter
**       OUTPUT :  
**          *t				--	physical paraameter
**								(may lie outside [0.0, 1.0] for some entities
**    RETURNS      : 0 if OK, UM_FAILURE if inversion screws up,
**							UM_UNIMPLEMENTED
**    SIDE EFFECTS : none
**    WARNINGS     : although this routine suffices to find the "nearest
**				point" on a curve in some cases, no assurance is made unless
**				the point is very close to the curve.  (esp. conics)
*********************************************************************/
int
um_utot(eptr,u, t)
	struct	UM_crvdatabag	*eptr;
	UU_REAL		u;
	UU_REAL		*t;

	{
	int status = UU_SUCCESS;
	UU_REAL distp;

	uu_denter(UU_MTRC,(us,"um_utot(eptr(r,k)=(%d,%x), u=%g)", 
				eptr->rel_num, eptr->key, u));
	
	switch (eptr->rel_num)
		{
		case	UM_LINE_REL:
		case	UM_CIRCLE_REL:
			*t = u;
			break;

		case	UM_CONIC_REL:
			{
			struct UM_evcrvout evcrv;
			uc_evcrv(UM_POINT, u, eptr, UM_idmat, &evcrv);
			if (umi_cc4_cctot(evcrv.cp, eptr,
				((struct UM_conic_rec *) eptr)->tfmat, t, &distp) < 0)
				{
				status = UU_FAILURE;
				}
			}
			break;
      case  NCL_CURVE_REL:
         {
          struct NCL_curve_rec *cptr;
          cptr = (struct NCL_curve_rec *) eptr;
          *t = cptr->t0 + u * (cptr->t1 - cptr->t0);
         }
         break;
      case  UM_RBSPLCRV_REL:
         {
          struct UM_rbsplcrv_rec *rbptr;
          rbptr = (struct UM_rbsplcrv_rec *) eptr;
          *t = rbptr->t0 + u * (rbptr->t1 - rbptr->t0);
         }
         break;

		default:
			status = UM_UNIMPLEMENTED;
			break;
		}


Done:
	uu_dprint(UU_MTRC, (us, "utot: returns: t = %g", *t));

	uu_dexitstatus("um_utot", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION :  um_u_equals_pal(eptr, pal, u)
**			The logical parameter U of the curve is the percentage arc length
**			parameterization.
**    PARAMETERS   
**       INPUT  : 
**          eptr			--	pointer to curve entity
**				pal			--	percentage arc length parameter
**									(0.0 < PAL < 1.0)
**       OUTPUT :  
**          *u				--	logical paraameter
**									(0.0 < U < 1.0)
**    RETURNS      : 0 if OK, UM_FAILURE if inversion screws up,
**							UM_UNIMPLEMENTED
**    SIDE EFFECTS : none
**    WARNINGS     :
*********************************************************************/
int
um_u_equals_pal(eptr, pal, u)
	struct	UM_crvdatabag	*eptr;
	UU_REAL		pal;
	UU_REAL		*u;

	{
	int status = UU_SUCCESS;
	UU_REAL distp;

	uu_denter(UU_MTRC,(us,"um_u_equals_pal(eptr(r,k)=(%d,%x), pal=%g)", 
				eptr->rel_num, eptr->key, pal));
	
	*u = pal;

	uu_dexitstatus("um_u_equals_pal", status);
	return (status);
	}

/************************************************************************
**    E_FUNCTION :  um_trim_isect_verify (ptr1, ptr2, t0, t1)
**			Checks the contents of the global buffer (output_buffer)
**       of the intersection points if the points satisfy an existing
**       curves parameter's limit (when they are trimmed).  Poonts 
**       outside the limits are removed and contents of the output_buffer
**       is altered. 
**    PARAMETERS   
**       INPUT  : 
**          ptr1			- pointer to the first curve entity
**          ptr2			- pointer to the second curve entity
**       OUTPUT :  
**          none
**    RETURNS      : 0 
**    SIDE EFFECTS : output_buffer altered 
**    WARNINGS     :
*************************************************************************/
int
   um_trim_isect_verify (ptr0,ptr1,t0,t1)
   struct UM_rbsplcrv_rec *ptr0, *ptr1;
   UM_param *t0, *t1;
  {
   UM_isect buff[50];
   int i, k;
   UM_param umin0, umin1, umax0, umax1, r0, r1;

   umin0 = t0[0] / (ptr0->t[ptr0->no_t-1] - ptr0->t[0]);  
   umax0 = t1[0] / (ptr0->t[ptr0->no_t-1] - ptr0->t[0]);  
   umin1 = t0[1] / (ptr1->t[ptr1->no_t-1] - ptr1->t[0]);  
   umax1 = t1[1] / (ptr1->t[ptr1->no_t-1] - ptr1->t[0]);  

/*
...Check buffer if t is in range (in original cv units)
...when OK copy to temporary buffer, & and convert t's to
...trimmed cv units.
*/
   k    = 0;
   for (i=0; i < output_count; i++)
     {
/*
.....Changed 't0' and 't1' to 'u0' and 'u1'
.....Bobby  -  7/29/97
*/
      r0 = output_buffer[i].u0;
      r1 = output_buffer[i].u1;

/*      if (r0 < umin0 || r0 > umax0 || r1 < umin1 || r1 > umax1);
      else */
        {
         buff[k] = output_buffer[i];
         buff[k].t0 = (r0 - umin0) / (umax0 - umin0) ;
         buff[k].t1 = (r1 - umin1) / (umax1 - umin1);
         k++;       
        }
     }
   for (i=0; i<k; i++) output_buffer[i] = buff[i];

   output_count = k;

   return (UU_SUCCESS);
  }
/************************************************************************
**    E_FUNCTION :  um_crv_isect_ext (elst, tlst, common_plane, nintp,
**                                   no_ibuf, ibuf)
**			Finds all intersection points of RBspline and any lower order
**       curve extended to get points also on its extension.
**    PARAMETERS   
**       INPUT  : 
**          elst    - pointer to curves data
**          tlst    - pointer to translation matrises
**       OUTPUT :  
**    RETURNS      : 0 if OK, UM_FAILURE if inversion screws up,
**							UM_UNIMPLEMENTED
**    SIDE EFFECTS : none
**    WARNINGS     :
*************************************************************************/
int
  um_crv_isect_ext (elst, tlst, common_plane, nintp, no_ibuf, ibuf)
  struct UM_rbsplcrv_rec	*elst[2];
  UU_REAL *tlst[2];
  UU_REAL common_plane[2][3];	/* plane of curves	*/
  int *nintp, no_ibuf;
  UM_isect	ibuf[];
  {
   int i, swap, relnum, pair,is0,is1,ret_val;
   struct UM_rbsplcrv_rec *eptr0, *eptr1;
   struct UM_rbsplcrv_rec *r0, *r1;
   UM_transf *tfmat0, *tfmat1;
   UU_REAL dumm, rconv[2], offs[2], ui[2], *ub;

   ret_val = UU_SUCCESS;

   swap  = (elst[0]->rel_num <= elst[1]->rel_num)? 0: 1;
   is0   = (swap == 0)? 0: 1;
   is1   = 1 - is0;
   eptr0 = elst[is0];
   eptr1 = elst[is1];
   r0    = r1 = UU_NULL;
   tfmat0 = (UM_transf *) tlst[is0];
   tfmat1 = (UM_transf *) tlst[is1];
   relnum = eptr0->rel_num;
   rconv[0] = rconv[1] = 1.0;
   offs[0] = offs[1] = 0.;
   for (i=0; i<2; i++)
     {
      if (elst[i]->rel_num == UM_RBSPLCRV_REL)
        {
         UU_REAL t0,t1,t01,t00;

         um_utot (elst[i],(UU_REAL) 0., &t00);
         um_utot (elst[i],(UU_REAL) 1., &t01);
         um_get_orignlt (elst[i],&t0,&t1);
         rconv[i] = (t01 - t00) / (t1 - t0);
         offs[i] = -t00 / (t1 - t0); 
        } 
     }
/*
...Both curves conics, if any is ellipses convert it to RBspl
*/
   if (eptr1->rel_num == UM_CONIC_REL)
     {
      struct UM_conic_rec *cptr1;

      cptr1 = (struct UM_conic_rec *) eptr1;
      if (cptr1->type == UM_ELLIPSE)
/*
...cptr1 = ELLIPSE, eptr1 is ignored call um_c7_frm...
*/
        {
         um_alloc_eq_curve (cptr1,&r1);
         um_c7_frmconic_extension (cptr1,UU_NULL,common_plane,r1);
         eptr1 = r1;
        }
/*
...cptr1 open conic
*/
      else 
        {
         struct UM_conic_rec *cptr0;
         cptr0 = (struct UM_conic_rec *) eptr0;
         if (cptr0->rel_num == UM_CONIC_REL)
           {
/*
......cptr0 open conic, solve immediately
*/
            if (cptr0->type != UM_ELLIPSE)
               {
                um_isect_conic_opn(cptr0,tfmat0,cptr1,tfmat1,
                                   nintp,no_ibuf,ibuf);
                goto Done;
               }
           }
/*
......cptr0 ELLIPSE or CI
*/
         um_alloc_eq_curve (cptr0,&r0);
         if (cptr0->rel_num == UM_CIRCLE_REL)
            um_c7_frmcirc_extension (cptr0,UU_NULL,common_plane,r0);
         else
            um_c7_frmconic_extension (cptr0,UU_NULL,common_plane,r0);
         eptr0 = r0;
         um_alloc_eq_curve (cptr1,&r1);
         um_c7_frmconic_extension (cptr1,eptr0,common_plane,r1);
         eptr1 = r1;
         goto Solve;        
        }
     }   
/*
...Prepare non RB_curve for intersection with RBspl curve 
...specific to its type.
*/ 
   switch (relnum)
     {
/*
...Line is redefined so its definition space contains
...the entire RBspl curve space.
*/
      case UM_LINE_REL:
        {
         struct UM_line_rec *lptr;
        
         lptr = (struct UM_line_rec *) eptr0;
         um_alloc_eq_curve (lptr,&r0);
         ret_val = um_c7_frmline_extension (lptr,eptr1,common_plane,r0);
         eptr0 = r0;
        }
        break;
/*
...Circle & Ellipse are redefined to its full size (closed)
*/
      case UM_CIRCLE_REL:
        {
         struct UM_circle_rec *cptr;
        
         cptr = (struct UM_circle_rec *) eptr0;
         um_alloc_eq_curve (cptr,&r0);
         ret_val = um_c7_frmcirc_extension (cptr,eptr1,common_plane,r0);
         eptr0 = r0;
        }
        break;
      case UM_CONIC_REL:
        {
         struct UM_conic_rec *cptr;

         cptr = (struct UM_conic_rec *) eptr0;
         um_alloc_eq_curve (cptr,&r0);
         ret_val = um_c7_frmconic_extension (cptr,eptr1,common_plane,r0);
         eptr0 = r0;
        }
        break;
      default:
        break;
     }
/*
...Solve intersections
*/
Solve:
   if (ret_val == UU_SUCCESS) 
       ret_val = um_icoplnrbsplcrv (eptr0,eptr1,common_plane,nintp,
                    no_ibuf,ibuf);
/*
...Convert u of extended curve to original space
*/
Done:
   if (ret_val == UU_SUCCESS)
      {
       for (i=0; i<*nintp; i++)
         {
          ub  = (UU_REAL *) &ibuf[i].u0;
          ui[0] = ub[0];  ui[1] = ub[1];
          if (elst[is0]->rel_num == UM_RBSPLCRV_REL)
             ub[is0] = (ui[0] + offs[is0]) / rconv[is0];
          else 
             um_cctou (elst[is0],UM_DEFAULT_TF,ibuf[i].pt,&ub[is0],&dumm);
          if (elst[is1]->rel_num == UM_RBSPLCRV_REL)
             ub[is1] = (ui[1] + offs[is1]) / rconv[is1];
          else 
             um_cctou (elst[is1],UM_DEFAULT_TF,ibuf[i].pt,&ub[is1],&dumm);
         }
      } 

   if (r0 != UU_NULL) uu_toolfree (r0);
   if (r1 != UU_NULL) uu_toolfree (r1);
   uu_dexit;
   return(ret_val);
  }
/************************************************************************
**    E_FUNCTION :  um_isect_conic_opn(cptr,tfmat0,cptr1,tfmat1,
**                                     nintp,no_ibuf, ibuf)
**			Finds all intersection points of two open (not ellipses) conics 
**    PARAMETERS   
**       INPUT  : 
**          cptr0    - pointer to first conic curve 
**          tfmat0   - id matrix of the first curve
**          cptr1    - pointer to second conic 
**          tfmat1   - id matrix of the second curve
**          no_ibuf  - size of output buffer
**       OUTPUT :  
**          nintp    - number of intersections found (in ibuf)
**          ibuf     - output buffer with intersections data
**    RETURNS      : 0 if OK, UM_FAILURE if no i/o found. 
**    SIDE EFFECTS : none
**    WARNINGS     :
*************************************************************************/
int
  um_isect_conic_opn(cptr0,tfmat0,cptr1,tfmat1,nintp,no_ibuf,ibuf)
  struct UM_conic_rec *cptr0, *cptr1;
  UM_transf tfmat0,tfmat1;
  int *nintp, no_ibuf;
  UM_isect	ibuf[];
  {
   int status, left, i, is, nin, n, n1, mult, j, k;
   UU_REAL svt00,svt01,svt10,svt11,dd;
   UM_coord pt[2], ptp[2], dlt, pln1, pln, ipt[2];
   UM_vector vec, vec1;
   UM_transf ttinv_sp;
   UU_REAL *ttinv, u0, u1, dist, intrv0,intrv1;
   struct UM_evcrvout ev0, ev1;

/*
...use global buffer for recursive calls
*/
   output_buffer = ibuf;  
   output_size = no_ibuf;
   output_count = 0;

   uc_init_evcrvout (cptr0,&ev0); 
   uc_init_evcrvout (cptr1,&ev1); 
/*
...save conic parameter limits and 
*/
   *nintp = 0;
   svt00 = cptr0->t0; 
   svt01 = cptr0->t1; 
   svt10 = cptr1->t0; 
   svt11 = cptr1->t1; 
/*
...get conversion mx for gest cv in the host space
*/
   if (tfmat0 == UM_DEFAULT_TF && tfmat1 == UM_DEFAULT_TF)
      ttinv = UM_DEFAULT_TF;
   else
      {
       ttinv = (UU_REAL *) ttinv_sp;
       um_inverttf(tfmat0, ttinv_sp);
       um_tftmtf(tfmat1, ttinv_sp, ttinv_sp);
      }
/*
...relax curves boundry
*/
   if (cptr0->type == UM_PARABOLA)
       cptr0->t0 = -10 / cptr0->invariants[0];   
   else
       cptr0->t0 = -1.0 + UM_DFUZZ;  
   cptr0->t1 = -cptr0->t0;

   if (cptr1->type == UM_PARABOLA)
       cptr1->t0 = -10 / cptr1->invariants[0];   
   else
       cptr1->t0 = -1.0 + UM_DFUZZ;   
   cptr1->t1 = -cptr1->t0;  
/*
...Find initial intersection of any tangent line at CV0 to the CV1.
...Start at u=0 incresing, if no luck switch to u=1 with decresing step
*/
   left = 0; 
   dd = .1;
   u0 = -0.1;
bg1:
   u0 += dd;
   um_ev4_conic (UM_FRSTDERIV, u0, cptr0, tfmat0,  &ev0);
   um_vctovc (ev0.cp, pln);
   um_unitvc (ev0.dcdu,vec);
   um_vctmtf (pln,ttinv,pln);
   um_vctmtf (vec,ttinv,vec);
   
   um_ilnconic (pln, vec, cptr1, &nin, ipt, &mult); 
   if (nin == 2) goto bg2;
   if (fabs(.5-u0) > .001 && nin == 0) goto bg1;
   if (nin == 0)
      { 
       if (left != 0) goto Done;
       u0 = 1.1;
       dd = -.1;
       left = 1;
       goto bg1;
      }
/*
...recursive call to get point at possible intersection,
...second branch can also have io's 
*/
bg2:

   status = umi_isect_conic (0,cptr0,ttinv,&ev0,cptr1,UM_DEFAULT_TF,&ev1,ipt[0]);
   if (nin > 1) 
      status = umi_isect_conic (0,cptr0,ttinv,&ev0,cptr1,UM_DEFAULT_TF,&ev1,ipt[1]);
/*
...One more try if second branch of CV0 was not explored
*/
   if(output_count < 4 && left == 0)
     { 
      u0 = 1.1;
      dd = -.1;
      left = 1;
      goto bg1;
     }
   if (output_count == 0 && left == 1)
     {
      u0 = .6005;
      left = 2;
      goto bg1;
     }

Done:
   *nintp = output_count;
/*
...restore conic original space
...and get parameters for points in old conic space 
*/
   cptr0->t0 = svt00;
   cptr0->t1 = svt01;
   cptr1->t0 = svt10;
   cptr1->t1 = svt11;
/*
...get parameter values for all points in buffer
*/
   for (i=0; i<output_count; i++)
     {
      um_cctou (cptr0,UM_DEFAULT_TF,ibuf[i].pt,&u0,&dist);
      ibuf[i].u0 = u0;
      um_cctou (cptr1,UM_DEFAULT_TF,ibuf[i].pt,&u1,&dist);
      ibuf[i].u1 = u1;
     }

   uu_dexit;
   return (status);
  }
/********************************************************************
**    E_FUNCTION :  umi_isect_conic (iter,cptr0,tfm0,ev0,cptr1,tfm1,
**                                   ev1,ptin)
**			Recursive iterator intersects line tangent to conic curve
**       with the second conic curve (open conics only).
**    PARAMETERS   
**       INPUT  : 
**          iter         - current recursion depth
**          cptr0,cptr1  - pointer to curves entity
**          tfm0,tfm1    - conversion matrices for curves to get both
**                         in the same space
**          ev0,ev1      - pointer to evaluator buffers
**          ptin         - initial point on the first (cptr0) curve. 
**       OUTPUT :  
**							none
**    RETURNS      : 0 if OK, -1 if process screws up,
**    SIDE EFFECTS : none
**    WARNINGS     :
********************************************************************/
int
  umi_isect_conic (iter,cptr0,tfm0,ev0,cptr1,tfm1,ev1,ptin)
  int iter;
  struct UM_conic_rec *cptr0, *cptr1;
  UM_transf tfm0, tfm1;
  struct UM_evcrvout *ev0, *ev1;
  UM_coord ptin;
  {
   UM_coord ptp, pt[2], ptln;
   UM_vector vec;
   int ret_val, status, n1, n0;
   UU_REAL u0,u1,dist;
   int i,n,mult,k;

   ret_val = 0;
/*
...make it is not the same point from other leaf
*/
   for (k=0; k<output_count; k++)
      {
       if (um_dcccc(output_buffer[k].pt, ptin) < UM_FUZZ) goto Done;
      }

   if (iter > 6) goto Nosol;
   iter++;
/*
...get line tangent to the curve at ptin point
*/
   status = um_cctou (cptr1,UM_DEFAULT_TF,ptin,&u1,&dist); 
   if (status == UU_FAILURE || dist > 2*UM_FUZZ) goto Nosol;
   um_ev4_conic (UM_FRSTDERIV, u1, cptr1, UM_DEFAULT_TF,  ev1);
   um_vctovc (ev1->cp, ptln);
   um_unitvc (ev1->dcdu, vec);
/*
...convert line to host space
...and intersect with second curve
*/
   if (tfm1 != UM_DEFAULT_TF)
      {
       um_cctmtf (ptln,tfm1,ptln);
       um_vctmtf (vec,tfm1,vec);
      }
   um_ilnconic (ptln,vec,cptr0,&n,pt,&mult);
   if (n == 0) goto Nosol;
/*
...check intersection(s) for tolerance
*/
   for (i=0; i<n; i++)
     { 
      if (um_dcccc(pt[i],ptin) < .1*UM_FUZZ)
         {
          for (k=0; k<output_count; k++)
             {
              if (um_dcccc(output_buffer[k].pt, pt[i]) < UM_FUZZ) goto Nexti;
             }
          um_vctovc (pt[i],output_buffer[output_count].pt);
          output_count += 1;
          ret_val = 0;
         }
      else
/*
...if not in tolerance swap the curves and repeat 
*/
         {
          ret_val = umi_isect_conic (iter,cptr1,tfm1,ev1,
                                        cptr0,tfm0,ev0,pt[i]); 
         }
Nexti:;
      }
   goto Done;

Nosol: 
   ret_val = -1;
   
Done:
   uu_dexit;
   return(ret_val);
  }
