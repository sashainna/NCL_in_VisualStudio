/*********************************************************************
**       NAME         :  tigtrimsf.c
**       CONTAINS: Routines to convert xyz curve to uv curve.
**			uig_evcrv_to_uvcv(cvkey,sfkey,uvkey)
**			uig_evcomp_to_uvcv(cvkey,sfkey,uvkey)
**       uig_adjust_boundary(uvpts, closed_u, closed_v)
**       uig_adjust_boundary(uvpts, closed_u, closed_v, sfkey, uv)
**			UU_REAL uig_lnlndis(spt1,ept1,spt2,ept2,npt1,npt2)
**			uig_ncl_evcrv_to_pts (cvkey, npt, cvpts)
**			uig_evcrv_to_uvcv1(cvkey,sfkey,uvkeyi,tol)
**			uig_gtmmuv(sfkey,umin,vmin,umax,vmax)
**			uig_surfpn
**			uig_usrfpn
**			uig_usrfpn1
**			uig_ev_trimsf (sfkey, u, v, sv)
**			uig_ev_rbsplsrf
**			uig_ev_revsf
**			uig_ncl_interp_rbsp (sfkey, ifit, nents, nclkey)
**			uig_create_rbsp (npts, s, pts, crv)
**			uig_ncl_interp_rbsp1 (n, ptve, itsk, nump, sp, ptsp)
**			uig_ncl_slpset (n, ptve, imode)
**			uig_ncl_slpset_step (n, ptve, imode)
**			uig_ncl_put_uv (pts,npt)
**			uig_ncl_free_uv ()
**			uig_ncl_list_push (list,gent,ptr)
**			uig_mergecrv(cvkey, keylst,key)
**			uig_create_polyline
**			uig_create_line
**			uig_ncl_crvfit
**			uig_ncl_crvgen
**			uig_ncl_crvctr
**			uig_ncl_extended_rbsp (e,rsplrec)
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**       tigtrimsf.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/05/18 , 15:14:45
*****************************************************************/
#include "uhep.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdebug.h"
#include "mdattr.h"
#include "modef.h"
#include "ulist.h"
#include "uminmax.h"

#include "tiges.h"
#include "tigdefs.h"
#include "tigglobal.h"
#include "umath.h"
#include "mcrv.h"
#include "mdattr.h"
#include "tigsupp.h"
#include "mdrel.h"
#include "udebug.h"
#include "usysdef.h"
#include "mxxx.h"
#include "mdeval.h"
#include "rbase.h"
#include "nccs.h" /*jkd31a*/
#include "nclfc.h"

#include "ncl.h"
#include "nclxmdl.h"
#include "msrfddl.h"
#include "tioconv.h"

//extern int process_normal;

static void S_adjust_sphere_boundary();
static void S_adjust_cone_boundary();

UU_LIST NCL_uvintof;
static int NCL_uvintof_init = 0;
static UU_REAL UIG_savu, UIG_savv;
static UU_REAL UIG_firstu, UIG_firstv;
static int UIG_where_in_comp;
static UU_LOGICAL UIG_reverse=UU_FALSE;
static int closdinu = 0, closdinv = 0;
static UU_REAL srf[7];

UU_REAL uig_lnlndis();

UU_LOGICAL T_DEBUV=UU_FALSE;

/*********************************************************************
**    E_FUNCTION     : int uig_evcrv_to_uvcv(cvkey,sfkey,uvkey,tol)
**       Evolve xyz curve on surface to uv curve on surface.
**    PARAMETERS
**       INPUT  :
**          cvkey      - key of the xyz curve on surface
**          sfkey      - surface key
**          uv[6]      - min, max, and periods of u and v
**                       uv[0] = u min
**                       uv[1] = u max
**                       uv[2] = v min
**                       uv[3] = v max
**                       uv[4] = u period ( = 0.0 if not periodic)
**                       uv[5] = u period ( = 0.0 if not periodic)
**          tol        - tolerance
**       OUTPUT :
**          uvkey      - key of the uv curve on surface generated
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_evcrv_to_uvcv(cvkey,sfkey,uvkey,uv,tol)
UU_REAL tol,uv[6];
UU_KEY_ID *cvkey, *sfkey, *uvkey;
{
   struct UM_crvdatabag cv;
   int status;

	UIG_where_in_comp =0;
	cv.key = *cvkey;
	status = ncl_retrieve_data_fixed(&cv);
	if (status == UU_SUCCESS)
	{
		if (cv.rel_num == UM_COMPCRV_REL)
			status = uig_evcomp_to_uvcv(cvkey,sfkey,uvkey,uv,tol);
		else
			status = uig_evcrv_to_uvcv1(cvkey,sfkey,uvkey,uv,tol);
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int uig_evcomp_to_uvcv(cvkey,sfkey,uvkey,uv,tol_ignore)
**       Evolve xyz composite curve on surface to uv curve on surface.
**    PARAMETERS
**       INPUT  :
**          cvkey      - key of the xyz composite curve on surface
**          sfkey      - surface key
**          uv[6]      - min, max, and periods of u and v
**                       uv[0] = u min
**                       uv[1] = u max
**                       uv[2] = v min
**                       uv[3] = v max
**                       uv[4] = u period ( = 0.0 if not periodic)
**                       uv[5] = u period ( = 0.0 if not periodic)
**          tol_ignore - tolerance
**       OUTPUT :
**          uvkey      - key of the uv curve on surface generated
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_evcomp_to_uvcv(cvkey,sfkey,uvkey,uv,tol_ignore)
UU_KEY_ID *cvkey,*sfkey, *uvkey;
UU_REAL tol_ignore,uv[6];
{
   char p_buff[80];
   struct NCL_fixed_databag cvrec,sf;
   struct UM_compcrv_rec *cvptr;
   UU_REAL  umin,umax,vmin,vmax,du,dv;
   UU_REAL tol = 0.008;
   int k, i, count, j, ncvs, npts, status, rel_num,*numptr=UU_NULL;
   int nuv,sftype,ifit,rev, rev0, result, test_flags[4], apex_rev =0;
	int trfl;
   UU_KEY_ID key, *keylst=UU_NULL;
   UU_LIST cvpts, uvpts,revpts,*lspt;
   UM_coord *ptuv,temp,dummy,*pext,fpt,lpt,uvpt,pt1;
	UM_vector vc1;
		
   *uvkey = 0;
   UIG_savu = 0.0; 
   UIG_savv = 0.0;
   cvptr = (struct UM_compcrv_rec *)&cvrec;
   cvptr->key = *cvkey;
   status = ncl_retrieve_data_fixed(cvptr);
   if (status != UU_SUCCESS) return (status);

   ncvs = cvptr->no_cid;
   keylst = (UU_KEY_ID *) uu_malloc(ncvs*sizeof(UU_KEY_ID));
   numptr = (int *) uu_malloc(ncvs*sizeof(int));
   if (keylst == UU_NULL || numptr == UU_NULL) 
   {
      status = UU_FAILURE; goto Done;
   }
/*
.....Break the curve into its components and translate each into 
.....uv curve. First evolve each curve into points then project
.....those points onto the surface to get uv points.
*/
   uu_list_init (&cvpts, sizeof(UM_coord), 100, 100);
   uu_list_init (&uvpts, sizeof(UM_coord), 100, 100);
	uu_list_init (&revpts, sizeof(UM_coord), 100, 100);
   uvpt[0] = .5;
   uvpt[1] = .5;
   uvpt[2] = 0.;
if (*cvkey == 375)
{
i = 0;
}

   ur_retrieve_data_relnum(*sfkey, &sftype);
   if ((sftype == NCL_TRIMSF_REL) || (sftype == UM_RBSPLSRF_REL) \
                                  || (sftype == NCL_REVSURF_REL))
   {
        status = ncl_get_closdinuv (*sfkey,&closdinu,&closdinv);
        if (status != UU_SUCCESS) goto Done;
   }
/*
.....Determine if surface has an apex point (triangular)
*/
	sf.key = *sfkey;
	status = ncl_retrieve_data_fixed(&sf);
	if (status == 0) ncl_get_sf_treflag(&sf,&trfl,UIG_comp_tol);
	else trfl = 0;
/*
..... For each curve in the composite curve, tesselate the curve
..... into a series of points.  Make sure that the endpoints of the
..... curves are joined correctly.  Project each point onto the
..... surface.  Save the uv coordinates.  Eventually, build a uv-curve
..... out of these points.
*/
surface:;
   count = 0;
   for (i = 0; i < ncvs && status == UU_SUCCESS; i++)
   {
      cvpts.cur_cnt = 0;
      key = cvptr->cid[i].crvid;
      UIG_reverse = cvptr->cid[i].reverse;
      status = uig_ncl_evcrv_to_pts(&key, &npts, &cvpts);
      if (status == UU_SUCCESS)
      {
         pext = (UM_coord *) UU_LIST_ARRAY (&cvpts);

         rev = 0;
/*
..... If first curve in composite, save starting point
..... as first point, fpt.  Save last point as lpt.
*/
         if (i == 0) 	
         {
            um_vctovc (pext[0],fpt);
            um_vctovc (pext[npts-1],lpt);
         }
         else
			{
				if (UIG_from_trimsrf)
					tol = UIG_comp_tol;
				else
					tol = 0.008;
            result = uig_compcv_contin_check(fpt, lpt, pext[0], \
                               pext[npts-1], test_flags, tol);

            if (result == -1)
            {
/*
..... Both endpoints of both 1st and 2nd curves are too far away.
..... The curves are separated (not continuous).  Can't make a
..... smooth composite curve using them.
*/
               sprintf (p_buff, "In composite curve, endpoints of subcurves not joined.\n");
               uig_error (p_buff);
               status = UU_FAILURE; goto Done;
            }
            if (test_flags[0] == 1)
            {
               um_vctovc(pext[npts-1], lpt);
               rev = 0; rev0 = 0;
            }
            else if (test_flags[1] == 1)
            {
               um_vctovc(pext[0], lpt);
               rev = 1; rev0 = 0;
            }
            else if (test_flags[2] == 1)
            {
               um_vctovc(lpt, fpt);
               um_vctovc(pext[npts-1], lpt);
               rev = 0; rev0 = 1;
            }
            else if (test_flags[3] == 1)
            {
               um_vctovc(lpt, fpt);
               um_vctovc(pext[0], lpt);
               rev = 1; rev0 = 1;
            }
            if (rev0 == 1)
            {
               int the_size;
               UM_coord *temp_uv_array;

               rev0 = 0;

               the_size = 3 * numptr[0] * sizeof(UU_REAL);
               temp_uv_array = (UM_coord *) uu_malloc(the_size);
               if (!temp_uv_array)
               {
                  sprintf (p_buff, "Can't reverse 1st subcurve.\n");
                  uig_error (p_buff);
                  status = UU_FAILURE; goto Done;
               }

               for (j = 0; j < numptr[0]; j++)
                  uu_list_pop(&uvpts, temp_uv_array[j]);
               for (j = 0; j < numptr[0]; j++)
                  uu_list_push(&uvpts, temp_uv_array[j]);

               if (temp_uv_array) uu_free(temp_uv_array);
            }
			}
/*
..... rev0 = 1  means first curve needs reversing.
..... rev  = 1  means current curve needs reversing.
*/
		 
         numptr[i] = npts;
curve:;
			for (j = 0; j < npts; j++)
			{
            if ((apex_rev==0 && rev == 0)||(apex_rev==1 && rev == 1)) 
               status = uig_surfpn(pext[j],sfkey,&uvpt[0],&uvpt[1],count);
            else 
               status=uig_surfpn(pext[npts-1-j],sfkey,&uvpt[0],&uvpt[1],count);
            if (status == UU_FAILURE)
				{
/*
.....if start of crv: reverse the crv, obtain uv points and then reverse the
.....uvpoints. 
*/
					if (trfl && j == 0 && apex_rev == 0)
					{
						apex_rev = 1;
/*						UU_LIST_EMPTY(&uvpts);
.....Changed the logic so the uvpts list is not emptied when the failure
.....happens on a later curve.  If uvpts is emptied after previous curves
.....worked, numpts will not be accurate and so there will be memory
.....problems when the uvpts list is accessed below - Andrew 4/24/13
*/
						if (j > 0) uu_list_delete(&uvpts,uvpts.cur_cnt-j,j);
						goto curve;
					}
/*
.....Only 2 points in curve
.....We need to add another point
.....in order to calculate the apex point
*/
					else if (npts == 2 && j == 1 && trfl)
					{
						um_middlept(pext[j],pext[j-1],pt1);
						status=uig_surfpn(pt1,sfkey,&uvpt[0],&uvpt[1],count);
						if (status == UU_SUCCESS) j = 2;
					}
/*
.....Special case : if the surface primitive type for surface of revolution 
.....is a sphere or cone, and point is on apex ie. u/v = 0/1.
*/
					if (j > 1 && trfl)
					{
/*
.....if end of crv:get uv point by extending the vector formed by the previous
.....points.
*/
						if (apex_rev) lspt = &revpts;
						else lspt = &uvpts;
						if (npts == 2) uu_list_push(lspt,uvpt);
						ptuv = (UM_coord *)UU_LIST_ARRAY(lspt);
						nuv= UU_LIST_LENGTH(lspt);
						status = ncl_get_apex(*sfkey,uvpt,ptuv,nuv);
						if (npts == 2)
						{
							j = 1;
							uu_list_pop(lspt,temp);
						}
						goto push;
					}
					sprintf (p_buff,
						"Point on curve failed to project onto surface.\n");
					uig_error (p_buff);
					goto Done;
				}
push:;
				count = 1;
				if (apex_rev)
				{
					uu_list_push(&revpts, uvpt);
					if(j == npts -1)
					{
/*
.....reverse the uv projection points 
*/
						for (k=0; k<npts; k++)
						{
							uu_list_pop(&revpts, temp);
							uu_list_push(&uvpts, temp);
						}
						UU_LIST_EMPTY(&revpts);
						apex_rev = 0;
					}
				}
				else uu_list_push(&uvpts, uvpt);
			}
		}
	}
	if (status == UU_SUCCESS)
	{
/* 
.....This flag determines if the generatrix needs to be shifted in the UV space
..... ie. the start and end angle of revolution needs to be modified,
.....so the curves do not lie outside the uv boundary.
.....Himani
*/
		if(UIG_change_uv == 0)
			status = uig_adjust_boundary(&uvpts, closdinu, closdinv);
  		else if(closdinu || closdinv)
		{
			status = uig_adjust_boundary_new(uvpts,closdinu, closdinv, sfkey ,uv);
/*
.....Reset the list counters to call this routine with a new generatix position
*/
			UIG_change_uv = 0;
			UU_LIST_EMPTY(&uvpts);
			UU_LIST_EMPTY(&cvpts);
			UU_LIST_EMPTY(&revpts);
			goto surface;
		}
/*
.....Surface is a sphere
.....Modify boundary so that V-parameters
.....around the apex are consistent
.....Bobby  -  11/06/12
*/
		if (closdinu && closdinv)
			S_adjust_sphere_boundary(&uvpts);
/*
.....Surfaces is a cone
.....Modify boundary so that V-parameters
.....around the apex are consistent
.....Bobby  -  11/06/12
*/
	
		if (trfl)
			S_adjust_cone_boundary(&uvpts);
   }
   
   if (status == UU_SUCCESS)
   {
/*
.....Convert u,v values from 0 to 1 back to surface range.
*/
      ur_retrieve_data_relnum(*sfkey,&rel_num);
      if (rel_num==NCL_TRIMSF_REL)
         uig_gtmmuv(sfkey, &umin, &vmin, &umax, &vmax);
      else if (rel_num==UM_RBSPLSRF_REL)
         uig_get_rbuv(sfkey,&umin,&vmin,&umax,&vmax);
      else
      {
         umin = uv[0]; umax = uv[1];
         vmin = uv[2]; vmax = uv[3];
      }

      if (status == UU_SUCCESS)
      {
         du = umax - umin;
         dv = vmax - vmin;

         pext = (UM_coord *) UU_LIST_ARRAY (&uvpts);
         npts = UU_LIST_LENGTH(&uvpts); 
         for (i=0; i<npts; i++)
         {
            (*pext)[0] = (*pext)[0]*du + umin;
            (*pext)[1] = (*pext)[1]*dv + vmin;
            pext++;
         }
      }
   }
/*
.....Create uv component curves.
*/
   pext = (UM_coord *) UU_LIST_ARRAY (&uvpts);
	if (T_DEBUV)
	{
		char sbuf[80];
		uig_list_out(" \n",UU_FALSE);
		for (i=0;i<UU_LIST_LENGTH(&uvpts);i++)
		{
			sprintf(sbuf,"uv[%d] = %lf,%lf\n",i,pext[i][0],pext[i][1]);
			uig_list_out(sbuf,UU_FALSE);
		}
	}

   for (i=0; i<ncvs && status==UU_SUCCESS; i++)
   {
      npts = numptr[i];
      key = cvptr->cid[i].crvid;
      ur_retrieve_data_relnum(key,&rel_num);
      if (rel_num ==NCL_PATERN_REL || rel_num == UM_POLYLINE_REL)
         status = uig_create_polyline(&npts, pext, &key);
      else if (rel_num == UM_LINE_REL)
         status = uig_create_line (&npts,pext,&key);
      else if (rel_num == UM_RBSPLCRV_REL|| rel_num ==UM_CIRCLE_REL
            || rel_num == UM_CONIC_REL)
      {
         status = uig_ncl_put_uv(pext,&npts);
         if (status == UU_SUCCESS)
         {
            if (rel_num == UM_RBSPLCRV_REL) 
               ifit = 2;
            else
               ifit = 1;
            status = uig_ncl_interp_rbsp(sfkey, &ifit, &npts, &key);
         }
      }
      else
          status = UU_FAILURE;
			
      if (status == UU_SUCCESS)
      {
         ur_update_displayable(key,UM_NEVERDISPLAYABLE);
         keylst[i] = key;
      }
      else
      {
         sprintf (p_buff, "Failed to create parametric subcurve.\n");
         uig_error (p_buff);
      }
      pext += numptr[i];
   }
/*
.....Create a composite curve from the uv component curves.
*/
   if (status == UU_SUCCESS)
   {
      status = uig_mergecrv(cvkey, keylst, uvkey);
      if (status != UU_SUCCESS)
      {
         sprintf (p_buff, "Failed to merge parametric subcurves into composite curve.\n");
         uig_error (p_buff);
      }
   }
Done:;
   uu_list_free (&cvpts);
   uu_list_free (&uvpts);
   uu_list_free (&revpts);
   if (keylst) uu_free(keylst);
   if (numptr) uu_free(numptr);
   return (status);
}


/*********************************************************************
**    E_FUNCTION     : int uig_adjust_boundary(uvpts, closed_u, closed_v)
**       
**    PARAMETERS
**       INPUT  :
**          uvpts     -  parametric points that make up a trimming curve
**
**          closed_u  -  1 if surface is closed in u (periodic -> sphere,
**                         cylinder, surface of revolution)
**                       0 if not closed in u
**
**          closed_v  -  1 if surface is closed in v (periodic -> sphere,
**                         cylinder, surface of revolution)
**                       0 if not closed in v
**       OUTPUT :
**          uvpts     -  parametric points after they have been adjusted
**                       to account for surface boundaries.
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : parametric points modified
**    WARNINGS     : none
*********************************************************************/
int 
uig_adjust_boundary(uvpts, closed_u, closed_v)
int closed_u, closed_v;
UU_LIST *uvpts;
{
	int j, ii, jj, ix, ihld, npts;
	UU_REAL rmin, rmid, rmax, rval, *plst;

	rmin = 0.0001;
	rmax = 0.9999;
	rmid = 0.5;
/*
.....Correct points that are on the wrong edge of a closed surface
.....
..... If the surface is closed in u/v, then check all the u/v values that
..... were just found.  If the values are near the 0.0 or 1.0 boundary,
..... check to see which side they should be on.  You don't want a 
..... continuous line in the uv-plane to have some points at 1.0;
..... meanwhile, the next points along are at 0.0.
*/
	plst = (UU_REAL *) UU_LIST_ARRAY (uvpts);
	npts = uvpts->cur_cnt;
/*
..... ii is the offset for doing just u or just v.
..... The plst array is like (u_0, v_0, 0.0, u_1, v_1, 0.0, u_2, v_2, ...).
..... When ii is set to 0, then all the u's are done; however,
..... when ii = 1, all the v's are done.
.....
..... ihld is the flag.  If ihld = -1, do nothing.  Otherwise, if
..... ihld = the point in the array where the values STARTED getting
..... close to the boundary.
.....
..... ix is the current position in the array.
*/
	for (ii = 0; ii < 2; ii++)
	{
		ihld = -1;
		ix = ii;
		if (ii == 0 && closed_u != 1) continue;
		if (ii == 1 && closed_v != 1) break;
		for (jj=0; jj<npts; jj++,ix+=3)
		{
/*
..... If near the boundary 0.0 or 1.0, make sure the flag ihld is set.
..... If just arriving near the boundary (first one that is close),
..... set ihld to current position in array.
*/
			if (plst[ix] < rmin || plst[ix] > rmax)
			{
				if (ihld == -1) ihld = ix;
			}
			else if (ihld != -1)
			{
/*
..... Just started to move away from the boundary.  Find out which
..... side you are on (0.0 or 1.0).  Reset all points near the boundary
..... to the correct side.  Start at the first one near the boundary
..... (position in array saved in ihld), reset all points up to current
..... position in array (ix), and finally reset the flag (ihld = -1).
*/
				rval = (plst[ix] < rmid)?0.0:1.0;
				for (j=ihld;j<ix;j+=3) 
				{	
/*
.....Reset only those points that lie on the wrong side
*/
					if(fabs(plst[j]-plst[ix])> rmid)
					plst[j] = rval;
				}
				ihld = -1;
			}
		}
/*
..... If was near the boundary when finish the array, go back
..... to last point far enough away, find out which side to be on,
..... and reset all the points.
*/
		if (ihld != -1)
		{
			j = (ihld > 2)? ihld - 3: ii;
			rval = (plst[j] < rmid)?0.0:1.0;
			for (j=ihld;j<ix;j+=3) plst[j] = rval;
/*
.....Reset only those points that lie on the wrong side
*/
			for (j=ihld;j<ix;j+=3) if(fabs(plst[j]-rval)> rmid)plst[j] = rval;
		}
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int uig_evcrv_to_uvcv1(cvkey,sfkey,uvkey,uv,tol)
**       Evolve xyz curve on surface to uv curve on surface.
**    PARAMETERS
**       INPUT  :
**          cvkey      - key of the xyz curve on surface
**          sfkey      - surface key
**          uv[6]      - min, max, and periods of u and v
**                       uv[0] = u min
**                       uv[1] = u max
**                       uv[2] = v min
**                       uv[3] = v max
**                       uv[4] = u period ( = 0.0 if not periodic)
**                       uv[5] = u period ( = 0.0 if not periodic)
**          tol        - tolerance
**       OUTPUT :
**				uvkey		  - key of the uv curve on surface generated
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
uig_evcrv_to_uvcv1(cvkey,sfkey,uvkey,uv,tol)
UU_REAL tol,uv[6];
UU_KEY_ID *cvkey, *sfkey, *uvkey;
{
	int status,ifit,i,npts,rel_num,npts2;
	int vflag,uflag;
	UM_coord *pext,psrf;
	UM_coord *pext2,psrf2;
	UU_REAL u,v,uu,vv,umin,vmin,umax,vmax,dup[2];
	UU_REAL u2,v2;
	UU_LIST cvpts,cvpts2,uvpts;
	struct UM_crvdatabag cv;
/*
... evolve curve to xyz points
*/
	uu_list_init (&cvpts, sizeof(UM_coord), 100, 100);
	uu_list_init (&cvpts2, sizeof(UM_coord), 100, 100);
	uu_list_init (&uvpts, sizeof(UM_coord), 100, 100);
	vflag = 0;
	uflag = 0;
	
	status = uig_ncl_evcrv_to_pts(cvkey, &npts, &cvpts);

	if (status == UU_SUCCESS)
	{
/*
.....Load the coordinates of the points into pext.
*/
		pext = (UM_coord *) UU_LIST_ARRAY (&cvpts);
/*
.....If the surface is a rbsplsrf, we need to set
.....u max and mins and v max and mins. JLS 12-11-98
*/
		ur_retrieve_data_relnum(*sfkey,&rel_num);
		if (rel_num==NCL_TRIMSF_REL)
			uig_gtmmuv(sfkey, &umin, &vmin, &umax, &vmax);
		else if (rel_num==UM_RBSPLSRF_REL)
			uig_get_rbuv(sfkey,&umin,&vmin,&umax,&vmax);
		else
		{
			umin = uv[0]; umax = uv[1];
			vmin = uv[2]; vmax = uv[3];
		}

		dup[0] = umax - umin;
		dup[1] = vmax - vmin;

		u = 0.0;
		v = 0.0;
		u2 = 0.0;
		v2 = 0.0;
/*
.....If the base surface is a surface of revolution that has been 
.....revolved 360 degrees, then we need to make sure that the
.....segment of the xyz curve is getting translated into the 
.....correct uv points. It doesn't have to be a surface
.....of revolution, it can be in cylindrical surface. 
*/
		status = ncl_get_closdinuv (*sfkey,&closdinu,&closdinv);
		if (UIG_where_in_comp == 0 && (dup[1] >6.28 || dup[0] > 6.28))
		{
/*
.....Get the points from the second curve.
*/
			status = uig_usrfpn1(pext[0], sfkey,&u,&v,psrf,0);
			if (status == UU_SUCCESS)
				status = uig_usrfpn1(pext[npts-1], sfkey,&u2,&v2,psrf,0);
			if (status == UU_SUCCESS)
				status = uig_ncl_evcrv_to_pts(cvkey, &npts2, &cvpts2);
			if (status == UU_FAILURE) goto Done;
			pext2 = (UM_coord *) UU_LIST_ARRAY (&cvpts2);
/*
.....It is circular in v, ie v=0 and v=1 result in the same point on the
.....surface.
*/
			if (dup[1]>6.28)
			{
				if ((v <= 0.0001 || v>=.9999) && (v2 <=.0001|| v2>=.9999))
				{
					vflag =1;
					v2=0.0;
					u2=0.0;
/*
.....Make sure that the cv stays on the seam before 
.....checking next entity to see which side of the surface
.....it is on.
*/
					i = 1;
					while (i<npts && (v2>.9999 || v2 < .0001))
					{
						status = uig_usrfpn1(pext[i], sfkey,&u2,&v2,psrf2,0);
						if (status == UU_FAILURE) goto Done;
						i++;
					}
/*
.....The curve moved from the seam and we are able to determine
.....which side of the surface the point is on.
*/
					if (v2>.0001 && v2 < .9999)
					{
						vflag = 2;
						if (v2>.50) 
						{
							v = 1.0;
							v2 = 1.0;
						}
						else
						{
							v = 0.0;
							v2 = 0.0;
						}
					}
					else
/*
.....The curve ran along the seam of the surface, so check the
.....next curve to see which side of the surface it is on.
*/
					{
						while (v2>=1.0 ||v2<=0.0)
						{
							status = uig_usrfpn1(pext2, sfkey,&u2,&v2,psrf2,0);
							if (status == UU_FAILURE) goto Done;
							pext2++;
						}
						if (v2>.50) 
						{
							v = 1.0;
							v2 = 1.0;
						}
						else
						{
							v = 0.0;
							v2 = 0.0;
						}
					}
				}
				else if (v<.0001 || v>.9999)
/*
.....Already know that the curve is not running along the seam of the
.....surface, but we need to make sure that the starting point on the
.....correct side of the surface.
*/
				{
					vflag = 2;
					u2 = 0.0;
					v2 = 0.0;
					i = 1;
					while (i<npts && (v2>.9999 || v2 < .0001))
					{
						status = uig_usrfpn1(pext[i], sfkey,&u2,&v2,psrf2,0);
						if (status == UU_FAILURE) goto Done;
						i ++;
					}
					if (v2>.50) 
					{
						v = 1.0;
						v2 = 1.0;
					}
					else
					{
						v = 0.0;
						v2 = 0.0;
					}
				}
			}
/*
.....It is circular in u, ie, u= 0 and u =1 result in the same point
.....on the surface.
*/
			else if(dup[0]>6.28)
			{
				if ((u <= 0.0001 || u>=.9999) && (u2 <=.0001|| u2>=.9999))
				{
					uflag =1;
					v2=0.0;
					u2=0.0;
/*
.....Make sure that the cv stays on the seam before 
.....checking next entity to see which side of the surface
.....it is on.
*/
					i = 1;
					while (i<npts && (u2>.9999 || u2 < .0001))
					{
						status = uig_usrfpn1(pext[i], sfkey,&u2,&v2,psrf2,0);
						if (status == UU_FAILURE) goto Done;
						i ++;
					}
/*
.....The curve moved from the seam and we are able to determine
.....which side of the surface the point is on.
*/
					if (u2>.0001 && u2 < .9999)
					{
						uflag = 2;
						if (u2>.50) 
						{
							u = 1.0;
							u2 = 1.0;
						}
						else
						{
							u = 0.0;
							u2 = 0.0;
						}
					}
					else
/*
.....The curve ran along the seam of the surface, so check the
.....next curve to see which side of the surface it is on.
*/
					{
						while (u2>=1.0 ||u2<=0.0)
						{
							status = uig_usrfpn1(pext2, sfkey,&u2,&v2,psrf2,0);
							if (status == UU_FAILURE) goto Done;
							pext2++;
						}
						if (u2>.50) 
						{
							u = 1.0;
							u2 = 1.0;
						}
						else
						{
							u = 0.0;
							u2 = 0.0;
						}
					}
				}
				else if (u<.0001 || u>.9999)
/*
.....Already know that the curve is not running along the seam of the
.....surface, but we need to make sure that the starting point on the
.....correct side of the surface.
*/
				{
					uflag = 2;
					u2 = 0.0;
					v2 = 0.0;
					i = 1;
					while (i<npts && (u2>.9999 || u2 < .0001))
					{
						status = uig_usrfpn1(pext[i], sfkey,&u2,&v2,psrf2,0);
						if (status == UU_FAILURE) goto Done;
						i ++;
					}
					if (u2>.50) 
					{
						u = 1.0;
						u2 = 1.0;
					}
					else
					{
						u = 0.0;
						u2 = 0.0;
					}
				}
			}
		}

/*
.....Translate each point into its uv equivalent.
*/
		for( i=0; i<npts; i++)
		{
			status = uig_usrfpn1(pext, sfkey,&u,&v,psrf,i);
			if (status == UU_FAILURE) goto Done;
			if (uflag ==1)
				u = u2;
			else if (uflag ==2)
			{
				u = u2;
				uflag =0;
			}
			else if (vflag ==1)
				v = v2;
			else if (vflag ==2)
			{
				v = v2;
				vflag =0;
			}

/*
.....The following is trying to take care of problems that occur
.....when the base surface is a surface of revolution and it is a 
.....full revolution (there are two different ways to represent some
.....points in terms of u and v on the surface.
.....Have encountered surfaces which are cylindrcal that also cause
.....Problems. 
*/
			if (i==0 && UIG_where_in_comp >=1) 
/*
.....We are in the middle of the composit curve, make sure the start
.....point of this segment is the end point of the previous segment.
*/
			{
				if ((fabs((u-UIG_savu)) >.01) || (fabs((v-UIG_savv)) >.01))
				{
					u = UIG_savu;
					v = UIG_savv;
				}
			}
/*
.....First point of the whole composite curve.  Save it to make sure
.....that the last point of the composit cure is the same.
*/
			else if (i == 0)
			{
				UIG_firstu = u;
				UIG_firstv = v;
			}
			else if ((UIG_where_in_comp ==2) && (i==(npts-1)))
			{
/*
.....At the end of the composite curve, check to make sure it is close
.....to the beginning of the composit curve.
*/
				if ((fabs(u-UIG_firstu) >.01) || (fabs(v-UIG_firstv) > .01))
				{
					u = UIG_firstu;
					v = UIG_firstv;
				}
			}
      	uu = u*dup[0] + umin;
      	vv = v*dup[1] + vmin;
/*
.....Put the uv point into psrf
*/
	  		psrf[0] = uu;		
	  		psrf[1] = vv;
	  		psrf[2] = 0.0;
/*
....Put the points into the list.
*/
	  		uu_list_push(&uvpts, psrf);
	  		pext ++;
		}
		UIG_savu = u;
		UIG_savv = v;
	}
/*		
... generate uvonsf
*/
   pext = (UM_coord *) UU_LIST_ARRAY (&uvpts);

	cv.key = *cvkey;
	status = ncl_retrieve_data_fixed(&cv);
	if (cv.rel_num == NCL_PATERN_REL ||cv.rel_num == UM_POLYLINE_REL)
		status = uig_create_polyline(&npts, pext, uvkey);
	else if (cv.rel_num == UM_LINE_REL)
		status = uig_create_line (&npts,pext,uvkey);
/*
.....Added UM_CIRCLE_REL. JLS 2/8/99
*/
	else if (cv.rel_num == UM_RBSPLCRV_REL|| cv.rel_num ==UM_CIRCLE_REL
            || cv.rel_num == UM_CONIC_REL)
	{
		status = uig_ncl_put_uv(pext,&npts);
		if (status == UU_SUCCESS)
		{
/*
.....Using ifit equal to 2 will indicate to call uig_ncl_crvctr  to
.....to create the curves. JLS 12-28-98
.....ifit equal to 1 will indicate to use uig_ncl_crvfit to create the
.....curves.  JLS 2/8/99
*/
			if (cv.rel_num == UM_RBSPLCRV_REL) 
				ifit=2;
			else
				ifit = 1;
			status = uig_ncl_interp_rbsp(sfkey, &ifit, &npts, uvkey);
		}
	}
	else
		 status = UU_FAILURE;
	ur_update_displayable(*uvkey,UM_NEVERDISPLAYABLE);

Done:;
  	uu_list_free (&cvpts);
  	uu_list_free (&cvpts2);
  	uu_list_free (&uvpts);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int uig_gtmmuv(sfkey,umin,vmin,umax,vmax)
**       Return min & max u & v on a trimmed sf boundary.  Outside
**       boundary only are returned here, but can be used for all
**       inner boudaries as well.  
**    PARAMETERS
**       INPUT  :
**          sfkey      - surface key
**       OUTPUT :
**          umin       - minimum u value
**          umax       - maximum u value
**          vmin       - minimum u value
**          vmax       - maximum u value
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
uig_gtmmuv(sfkey,umin,vmin,umax,vmax)
UU_KEY_ID *sfkey;
UU_REAL *umin,*vmin,*umax,*vmax;
{
   struct NCL_trimsf_rec sf;
   int status;

   sf.key = *sfkey;
   status = ncl_retrieve_data_fixed (&sf);

   if (status == 0)
   {
      if (sf.rel_num == NCL_TRIMSF_REL)
      {
         *umin = sf.u_min;
         *umax = sf.u_max;
         *vmin = sf.v_min;
         *vmax = sf.v_max;
      }
   }

   return (status);
}

/*********************************************************************
**
**     FUNCTION:   uig_get_rbuv
**
**     PURPOSE:    Gets the u and v parameters for a 
**                 rational b-spline surface.
**
*******************************************************************/
int
uig_get_rbuv(sfkey,umin,vmin,umax,vmax)
UU_KEY_ID *sfkey;
UU_REAL *umin, *vmin,*umax,*vmax;
{
	struct UM_rbsplsrf_rec sf;
	sf.key = *sfkey;
	ncl_retrieve_data_fixed(&sf);
/*
.....The reason for using the last two elements in sf.tu and sf.tv
.....is to get the true u max and min and v max and min.  The values
.....that start at sf.tu[0] and go up to sf.tu[no_tu-2] are adjusted 
.....values. Same with sf.tv.   If the range is from 0 to 1, it won't
.....make a difference because then sf.tu[0] = sf.tu[sf.no_tu-2], and so on.
.....JLS 7/13/99
*/
	*umin = sf.tu[sf.no_tu-2];
	*umax = sf.tu[sf.no_tu-1];
	*vmin = sf.tv[sf.no_tv-2];
	*vmax = sf.tv[sf.no_tv-1];
	
	return (0);
}
/*********************************************************************
**    E_FUNCTION     : uig_ncl_interp_rbsp (sfkey, ifit, nents, nclkey)
**       Create uv spline curve using set of uv coordinates. The points
**       are in the global list NCL_uvintof.
**    PARAMETERS
**       INPUT  :
**          sfkey   - key of base surface where curve is created
**          nents   - number of points to interpolate
**          ifit    - = 0 interpolate
**                    = 1 fit
**       OUTPUT :
**          nclkey  - key of created uv curve on surface.
**    RETURNS      :
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int uig_ncl_interp_rbsp (sfkey, ifit, nents, nclkey)
int *nents, *sfkey, *nclkey;
int *ifit;
{
   int i, ns, status;
   int npts, itsk;
   struct NCL_crvgen_rec seg, *segp;
   UU_LIST seglist;
   UM_coord *ptrec;
   UM_coord opt;
   UU_REAL *pt, *s, *pts;
   struct UM_rbsplcrv_rec cv1;

   uu_denter(UU_MTRC,(us,"uig_ncl_interp_rbsp ()"));

   status = UU_SUCCESS;
   *nclkey = 0;
   s = pts = UU_NULL;
   npts = 0;

   ptrec = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
   uu_list_init (&seglist, sizeof(struct NCL_crvgen_rec), 20, 20);
/*
.....set segments for rb-spline interpolator
*/
   for (i=0; i<*nents; i++)
   {
      pt = (UU_REAL *) ptrec;
      seg.x = pt[0];
      seg.y = pt[1];
      seg.z = pt[2];
      seg.inv = 0;
      if (i>0 && i < *nents-1)
      {
         if (*ifit != 2 && um_dcccc(opt, pt) < UM_EQPARM)
         {
            ptrec++; status = UU_FAILURE;
         }
      }
      if (status == UU_SUCCESS)
      {
         uu_list_push (&seglist, &seg);
         um_vctovc (pt, opt);
         npts++;
         ptrec++;
      }
      status = UU_SUCCESS;
   }
   itsk = *ifit;
/*
.....interpolate rbspline
*/
   segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);
   if (status == UU_SUCCESS)
   {
      status = uig_ncl_interp_rbsp1 (npts, segp, itsk, &ns, &s, &pts);
/*
.....store curve in unibase
*/
      if (status == UU_SUCCESS)
      {
        status = uig_create_rbsp (ns, s, pts, &cv1);
        *nclkey = cv1.key;
        ncl_retrieve_data_fixed(&cv1);
      }
      else status = 51;
   }
/*
.....free memory used for interpolator
*/
   uu_list_free (&seglist);
   uig_ncl_free_uv();
   if (s != UU_NULL) uu_free(s);
   if (pts != 0) uu_free(pts);

   uu_dexitstatus("uig_ncl_interp_rbsp ()", status);
   return (status);
}
/*********************************************************************
**    E_FUNCTION     : uig_ncl_extended_rbsp (eptr,rsplrec)
**       Store spline data from an extended curve to an iges spline
**    PARAMETERS
**       INPUT  :
**          eptr   - NCL extended curve
**       OUTPUT :
**          rsplrec- IGES rational bspline parameter record
**    RETURNS      :
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int uig_ncl_extended_rbsp (e,rsplrec)
struct UM_rbsplcrv_rec	*e;
struct IG_igesrspl_rec *rsplrec;	/* IGES rational bspline parameter record */
{
	int i, j,ns, status;
	int nents,npts, itsk;
	struct NCL_crvgen_rec seg, *segp;
	UU_LIST seglist;
	UM_coord *ptrec;
	UM_coord opt;
	UU_REAL *spt, *s, *pts;
	UU_LIST cvpts;
	UU_REAL pt_cc[3], pt[3];

	status = UU_SUCCESS;
	s = pts = UU_NULL;
	npts = 0;
/*
..... Evolve rbspline to get a list of points
*/
	uu_list_init (&cvpts, sizeof(UM_coord), 100, 100);
	status = uig_ncl_evcrv_to_pts (&e->key, &nents, &cvpts);
	
	ptrec = (UM_coord *) UU_LIST_ARRAY (&cvpts);
	
	uu_list_init (&seglist, sizeof(struct NCL_crvgen_rec), 20, 20);
/*
.....set segments for rb-spline interpolator
*/
	for (i=0; i<nents; i++)
	{
		spt = (UU_REAL *) ptrec;
		seg.x = spt[0];
		seg.y = spt[1];
		seg.z = spt[2];
		seg.inv = 0;
		if (i>0)
		{
			if (um_dcccc(opt, spt) < UM_EQPARM)
			{
				ptrec++; status = UU_FAILURE;
			}
		}
		if (status == UU_SUCCESS)
		{
			uu_list_push (&seglist, &seg);
			um_vctovc (spt, opt);
			npts++;
			ptrec++;
		}
		status = UU_SUCCESS;
	}
	itsk = 1;
/*
.....interpolate rbspline
*/
	segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);
	if (status == UU_SUCCESS)
	{
		status = uig_ncl_interp_rbsp1 (nents, segp, itsk, &ns, &s, &pts);
/*
.....store curve data in iges rec
*/
		if (status == UU_SUCCESS)
		{
			rsplrec->key = GRSPLINE;
			rsplrec->planar = e->planar;	
			rsplrec->open = (e->open)? 0 : 1;
			rsplrec->degree = 3;				/* degree of the bspline curve  */
			rsplrec->indx = ns-1;		/* upper index of sum 			  */

			npts = ns;
			ns = ns + 4;
			rsplrec->type = 0;				/* rational bspline  */
			rsplrec->period = 0;			/* non- periodic */
			rsplrec->no_rpara = 1;
			rsplrec->rpara = (struct IG_rpara_rec *)uu_toolmalloc(sizeof(struct IG_rpara_rec));
			rsplrec->rpara->t0 = 0.0;		/* starting value    */
			rsplrec->rpara->t1 = s[ns-1];		/* ending value      */
			rsplrec->rpara->norm[0] = 0.0;
			rsplrec->rpara->norm[1] = 0.0;
			rsplrec->rpara->norm[2] = 1.0;

			rsplrec->no_t = ns;
			rsplrec->t = (UU_REAL *)uu_toolmalloc(ns*sizeof(UU_REAL));
			for (i=0; i<ns; i++)
			{
				rsplrec->t[i] = s[i];		/* knot values 		*/
			}
	
			rsplrec->no_pt3 = npts;
			rsplrec->pt3 = (UU_REAL *)uu_toolmalloc(3*npts*sizeof(UU_REAL));
			for (i = 0, j = 0; i < npts; i++, j = j+3)
			{
				pt_cc[0] = pts[j];
				pt_cc[1] = pts[j + 1];
				pt_cc[2] = pts[j + 2];
				UIO_CC_INTTOEXT(pt_cc,pt);
				um_vctovc(pt, &rsplrec->pt3[j]);		/* control points	 */
			}

			rsplrec->no_w = npts;
			rsplrec->w = (UU_REAL *)uu_toolmalloc(npts*sizeof(UU_REAL));
			for (i=0; i<npts; i++)
			{
				rsplrec->w[i] = 1.0;		/* weights values 		*/
			}
		}
	}
/*
.....free memory used for interpolator
*/

   uu_list_free (&seglist);
   uu_list_free (&cvpts);
   uig_ncl_free_uv();
   if (s != UU_NULL) uu_free(s);
   if (pts != 0) uu_free(pts);

   return (status);
}
/*********************************************************************
**    E_FUNCTION     : ncl_create_rbsp (npts, s, pts, crv)
**       Create a rational B-spline from the list of control points &
**       knots. The created spline is uniform (non-rational) - all the
**       weight are =1.
**    PARAMETERS
**       INPUT  :
**          npts         number of control points.
**          s            array of parameters.
**          pts          array of control points.
**       OUTPUT :
**          crv          created B-spline
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_create_rbsp (npts, s, pts, crv)
int npts;
UU_REAL *s, *pts;
struct UM_rbsplcrv_rec *crv;
{
   int i, ns, status;

   ns = npts + 4;
   ur_setup_data (UM_RBSPLCRV_REL, crv, sizeof(*crv));
	strcpy(crv->label, "@UN");
	crv->subscr = 0;
   crv->planar = UU_FALSE;
   crv->open = UU_TRUE;
   crv->closdinu = 0;
   crv->k = 4; 
   crv->n = npts-3;
   crv->t0 = 0.0;
   crv->t1 = s[ns-1];
   status = uc_create_data (crv, UM_DEFAULT_TF, UM_CURRENT_ATTR);
/*
.....This is the knot vector list.
*/
   if (status == UU_SUCCESS)
      status = ur_update_data_varlist (crv->key, 1, s, 1, ns);
/*
.....This is the control polygon coefficients list.
*/

   if (status == UU_SUCCESS)
      status = ur_update_data_varlist (crv->key, 2, pts, 1, npts);
/*
.....This is the control polygon weight values list.
*/

   if (status == UU_SUCCESS)
   {
      for (i=0;i<npts;i++) s[i] = 1.0;
      status = ur_update_data_varlist (crv->key, 3, s, 1, npts);
   }

   if (status == UU_SUCCESS)
      status = ncl_retrieve_data_fixed (crv);

   return (status);
}
/*********************************************************************
**    E_FUNCTION     : uig_ncl_interp_rbsp1 (n, ptve, itsk, nump, sp, ptsp)
**       Interpolate a rational B-spline curve thru a list of points &
**       optional slope vectors.
**    PARAMETERS
**       INPUT  :
**          n            number of points.
**          ptve         list of entities.
**          itsk         = 0 - interpolate all points
**                       = 1 - fit thru points.
**                       = 2 - use control points from ptve.
**       OUTPUT :
**          nump         number of control points in B-spline curve
**          sp           array of knots
**          ptsp         array of control points
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_ncl_interp_rbsp1 (n, ptve, itsk, nump, sp, ptsp)
int n, *nump;
struct NCL_crvgen_rec *ptve;
int itsk;
UU_REAL **sp, **ptsp;
{
   int status;
   int npts, ns;
   UU_REAL *s, *pts;
   struct NCL_crvgen_rec *pi;

   uu_denter(UU_MTRC,(us,"ncl_interp_rbsp1 ()"));

   s = 0;
   pts = 0;
   if (itsk == 0 || n < 3)
   {
      pi = ptve+1;
      if (n<3 && ptve->inv == 0 && pi->inv == 0)
      {
         ptve->a = pi->x-ptve->x;
         ptve->b = pi->y-ptve->y;
         ptve->c = pi->z-ptve->z;
         ptve->inv = 1;
      }
	  if (iges_in==1)
		status = uig_ncl_slpset (n,ptve,0);
	  else
		  status = uig_ncl_slpset_step (n,ptve,0);
   }
   else if (itsk != 2)
	{
      status = uig_ncl_crvfit(&n, ptve);
	}

   if (itsk == 2)
   {
      npts = n;
      ns = n+4;
      s = (UU_REAL *)uu_malloc(ns*sizeof(*s));
      pts = (UU_REAL *)uu_malloc(3*n*sizeof(*pts));
      status = uig_ncl_crvctr(n, ptve, s, pts);
   }
   else
   {
      if (status == UU_SUCCESS)
      {
         npts = 3*n-2;
         ns = npts+4;
         s = (UU_REAL *)uu_malloc(ns*sizeof(*s));
         pts = (UU_REAL *)uu_malloc(3*npts*sizeof(*pts));
         if (s == 0 || pts == 0) status = UU_FAILURE;
      }

      if (status == UU_SUCCESS)
         status = uig_ncl_crvgen (n, ptve, s, pts);
   }

   if (status == UU_SUCCESS)
   {
      *sp = s;
      *ptsp = pts;
      *nump = npts;
   }

   uu_dexitstatus("uig_ncl_interp_rbsp1 ()", status);
   return(status);
}
/*********************************************************************
**    E_FUNCTION     : uig_ncl_slpset (n, ptve, imode)
**       Set slope vectors for array of curve generation points.
**    PARAMETERS
**       INPUT  :
**          n            number of points.
**          ptve         list of entities.
**          imode        mode - 1 = first pass for curve fit
**                              2 = 2nd    "    "    "    "
**                              0 = all others
**       OUTPUT :
**          ptve         slope vectors calculated
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_ncl_slpset (n, ptve, imode)
   int n, imode;
   struct NCL_crvgen_rec *ptve;
   {
   int i,m,ismall, iknt, jknt, status;
   UU_REAL stol,ro,co,tl,hvchg,vchg,vdel,cal,adis,al,sa,dax,day,daz;
   UU_REAL cbe,bdis,bl,sb,dbx,dby,dbz;
   struct NCL_crvgen_rec *pi, *pj, *pk;
   UM_real8 ver;

   uu_denter(UU_MTRC,(us,"uig_ncl_slpset ()"));

   if (n<2) goto err;
/*   idx = 169;
   getsc (&idx, &ver); */
	ver = 9.0;
   if (imode == 1) stol = 1.0e-4; else stol = 1.0e-5;
   ismall = n<3;
   m=n-1;
   pi=ptve;
   pj = &ptve[1];
   if (m==1 && pi->inv==0 && pj->inv==0)
     {
     pi->a = pj->a = pj->x - pi->x;
     pi->b = pj->b = pj->y - pi->y;
     pi->c = pj->c = pj->z - pi->z;
     pi->inv = pj->inv = 1;
     }
   for (i=0; i<m; i++,pi++,pj++)
     {
     pi->dx = pj->x - pi->x;
     pi->dy = pj->y - pi->y;
     pi->dz = pj->z - pi->z;
     pi->ch = sqrt(pi->dx*pi->dx+pi->dy*pi->dy+pi->dz*pi->dz);
     if (pi->ch == 0) pi->ch = 1.0;
     }
   pi = &ptve[m-1];
   pj = &ptve[m];
   pj->dx = pi->dx;
   pj->dy = pi->dy;
   pj->dz = pi->dz;
   pj->ch = pi->ch;
   if (imode < 2)
     {
     for (i=0,pi=ptve-1,pj=ptve;i<n;i++,pi++,pj++)
       {
       if (pj->inv == 0 && i>0 && i<n-1)
         {
         ro = pi->ch/pj->ch;
         ro = ro*ro;
         pj->a = pi->dx + ro*pj->dx;
         pj->b = pi->dy + ro*pj->dy;
         pj->c = pi->dz + ro*pj->dz;
         }
       if (pj->inv > 0 || i>0 && i<n-1)
         {
         tl = sqrt(pj->a*pj->a+pj->b*pj->b+pj->c*pj->c);
         if (ver < 8.25)
         if (!ismall && pj->a*pj->dx+pj->b*pj->dy+pj->c*pj->dz<0.)tl = -tl;
         if (tl == 0.0) goto err;
/*          if (i > 0 && pj->inv > 0) */
         pj->a = pj->a/tl;
         pj->b = pj->b/tl;
         pj->c = pj->c/tl;
         }
       }
     }

   vchg = 1.0;
   hvchg = 1.0e8;
   hvchg = 1.0e-8;
   for (jknt=0;jknt<3 && vchg > stol;jknt++,stol=hvchg*1.1)
   for (iknt=0;iknt<100 && vchg > stol;iknt++)
     {
     if (iknt>0)
       {
       vchg = 0.0;
       pj = ptve+1;
       for (i=1;i<m;i++,pj++)
         {
         if (pj->inv == 0)
           {
           pi = pj-1;
           pk = pj+1;
           cal = (pi->a*pi->dx+pi->b*pi->dy+pi->c*pi->dz)/pi->ch;
           adis = .531*pi->ch/(.593+cal);
           al = pi->ch*(1.104+.13*cal)/(.851+cal);
           sa = pi->ch*(3.565+.24*cal)/(2.805+cal);
           dax = pi->dx-pi->a*adis;
           day = pi->dy-pi->b*adis;
           daz = pi->dz-pi->c*adis;
           cbe = (pj->dx*pk->a+pj->dy*pk->b+pj->dz*pk->c)/pj->ch;
           bdis = .531*pj->ch/(.593+cbe);
           bl = pj->ch*(1.104+.13*cbe)/(.851+cbe);
           sb = pj->ch*(3.565+.24*cbe)/(2.805+cbe);
           ro = al*sa/(bl*sb);
           dbx = (pj->dx-bdis*pk->a)*ro+dax;
           dby = (pj->dy-bdis*pk->b)*ro+day;
           dbz = (pj->dz-bdis*pk->c)*ro+daz;
           tl = sqrt(dbx*dbx+dby*dby+dbz*dbz);
           if (tl == 0) tl = 1.0;
           dbx /= tl;
           dby /= tl;
           dbz /= tl;
           vdel = fabs(dbx-pj->a)+fabs(dby-pj->b)+fabs(dbz-pj->c);
           if (vdel > vchg) vchg = vdel;
           pj->a = dbx;
           pj->b = dby;
           pj->c = dbz;
           }
         }
       }
     if (ptve->inv == 0)
       {
       pi = ptve;
       pj = ptve+1;
       co = (pi->dx*pj->a+pi->dy*pj->b+pi->dz*pj->c)/pi->ch;
       pi->a = 2.0*co*pi->dx/pi->ch-pj->a;
       pi->b = 2.0*co*pi->dy/pi->ch-pj->b;
       pi->c = 2.0*co*pi->dz/pi->ch-pj->c;
       }
     pj = &ptve[m];
     if (pj->inv == 0)
       {
       pi = pj-1;
       co = (pi->dx*pi->a+pi->dy*pi->b+pi->dz*pi->c)/pi->ch;
       pj->a = 2.0*co*pi->dx/pi->ch-pi->a;
       pj->b = 2.0*co*pi->dy/pi->ch-pi->b;
       pj->c = 2.0*co*pi->dz/pi->ch-pi->c;
       }
     if (ismall) vchg = 0.0;
     if (hvchg>vchg) hvchg = vchg;
     }
   if (ver > 8.25)
     for (i=1,pj=ptve+1;i<n;i++,pj++)
       {
       pi=pj-1;
       co = (pj->a*pi->dx+pj->b*pi->dy+pj->c*pi->dz)/pi->ch;
       if (co < -0.9999)
         {
         co = pj->a*pi->a+pj->b*pi->b+pj->c*pi->c;
         if (co < -0.9999)
           {
           pj->a = -pj->a;
           pj->b = -pj->b;
           pj->c = -pj->c;
           }
         }
       }
   status = UU_SUCCESS;
   goto done;
err:
   status = UU_FAILURE;
done:

   uu_dexitstatus("uig_ncl_slpset ()", status);
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : uig_ncl_slpset_step (n, ptve, imode)
**       Set slope vectors for array of curve generation points, 
**		 code specific for STEP translator
**       Sasha, Dec.05, 2017
**    PARAMETERS
**       INPUT  :
**          n            number of points.
**          ptve         list of entities.
**          imode        mode - 1 = first pass for curve fit
**                              2 = 2nd    "    "    "    "
**                              0 = all others
**       OUTPUT :
**          ptve         slope vectors calculated
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_ncl_slpset_step (n, ptve, imode)
   int n, imode;
   struct NCL_crvgen_rec *ptve;
   {
   int i,m,ismall, iknt, jknt, status,k;
   UU_REAL stol,ro,co,tl,hvchg,vchg,vdel,cal,adis,al,sa,dax,day,daz;
   UU_REAL cbe,bdis,bl,sb,dbx,dby,dbz;
   struct NCL_crvgen_rec *pi, *pj, *pk;
   UM_real8 ver;

   UU_REAL ta, tb, tc, tak, tbk, tck, vdel_mod, ang;
   char output[50];
   UM_vector v1, v2;
   UM_coord p1, p2, p3;

   UM_vector vcn;
	UU_REAL d;
	UM_transf rotmat;


   if (n<2) 
	   goto err;
	ver = 9.0;
   if (imode == 1) stol = 1.0e-4; else stol = 1.0e-5;
   ismall = n<3;
   m=n-1;
   pi=ptve;
   pj = &ptve[1];
   if (m==1 && pi->inv==0 && pj->inv==0)
     {
     pi->a = pj->a = pj->x - pi->x;
     pi->b = pj->b = pj->y - pi->y;
     pi->c = pj->c = pj->z - pi->z;
     pi->inv = pj->inv = 1;
     }
   for (i=0; i<m; i++,pi++,pj++)
     {
     pi->dx = pj->x - pi->x;
     pi->dy = pj->y - pi->y;
     pi->dz = pj->z - pi->z;
     pi->ch = sqrt(pi->dx*pi->dx+pi->dy*pi->dy+pi->dz*pi->dz);
     if (pi->ch == 0) pi->ch = 1.0;
     }
   pi = &ptve[m-1];
   pj = &ptve[m];
   pj->dx = pi->dx;
   pj->dy = pi->dy;
   pj->dz = pi->dz;
   pj->ch = pi->ch;
   if (imode < 2)
     {
     /*for (i=0,pi=ptve-1,pj=ptve;i<n;i++,pi++,pj++)*/
		 for (i=0,pi=ptve-1,pj=ptve, pk=ptve+1;i<n;i++,pi++,pj++,pk++)	// Sasha, June 07, 2017
       {
       if (pj->inv == 0 && i>0 && i<n-1)
         {
			
         ro = pi->ch/pj->ch;
         ro = ro*ro;
         pj->a = pi->dx + ro*pj->dx;
         pj->b = pi->dy + ro*pj->dy;
         pj->c = pi->dz + ro*pj->dz;

         }
       if (pj->inv > 0 || i>0 && i<n-1)
         {
         tl = sqrt(pj->a*pj->a+pj->b*pj->b+pj->c*pj->c);
         if (ver < 8.25)
         if (!ismall && pj->a*pj->dx+pj->b*pj->dy+pj->c*pj->dz<0.)
			 tl = -tl;
         if (tl == 0.0) 
			 goto err;
         pj->a = pj->a/tl;
         pj->b = pj->b/tl;
         pj->c = pj->c/tl;
         }
       }
     }

   vchg = 1.0;
   hvchg = 1.0e8;
   //hvchg = 1.0e-8;
   for (jknt=0;jknt<3 && vchg > stol;jknt++,stol=hvchg*1.1)
   for (iknt=0;iknt<100 && vchg > stol;iknt++)
     {

		  
     if (iknt>0)
       {
		  
       vchg = 0.0;
       pj = ptve+1;
       for (i=1;i<m;i++,pj++)
         {
         if (pj->inv == 0)
           {
			   pi = pj-1;
			   pk = pj+1;

			   cal = (pi->a*pi->dx+pi->b*pi->dy+pi->c*pi->dz)/pi->ch;
			   adis = .531*pi->ch/(.593+cal);
			   al = pi->ch*(1.104+.13*cal)/(.851+cal);
			   sa = pi->ch*(3.565+.24*cal)/(2.805+cal);
			   dax = pi->dx-pi->a*adis;
			   day = pi->dy-pi->b*adis;
			   daz = pi->dz-pi->c*adis;
			   cbe = (pj->dx*pk->a+pj->dy*pk->b+pj->dz*pk->c)/pj->ch;
			   bdis = .531*pj->ch/(.593+cbe);
			   bl = pj->ch*(1.104+.13*cbe)/(.851+cbe);
			   sb = pj->ch*(3.565+.24*cbe)/(2.805+cbe);
			   ro = al*sa/(bl*sb);
			   dbx = (pj->dx-bdis*pk->a)*ro+dax;
			   dby = (pj->dy-bdis*pk->b)*ro+day;
			   dbz = (pj->dz-bdis*pk->c)*ro+daz;
			   tl = sqrt(dbx*dbx+dby*dby+dbz*dbz);
			   if (tl == 0) tl = 1.0;
			   dbx /= tl;
			   dby /= tl;
			   dbz /= tl;

			  			   
			    vdel = sqrt((dbx-pj->a)*(dbx-pj->a)+(dby-pj->b)*(dby-pj->b)+(dbz-pj->c)*(dbz-pj->c));
           
			   pj->a = dbx;
			   pj->b = dby;
			   pj->c = dbz;


			   if (vdel > vchg) 
			   {
				   vchg = vdel;

				   ta = pj->a;
				   tb = pj->b;
				   tc = pj->c;
					
				   cal = (pi->a*pi->dx+pi->b*pi->dy+pi->c*pi->dz)/pi->ch;
				   cbe = (pj->a*pi->dx+pj->b*pi->dy+pj->c*pi->dz)/pi->ch;
				   adis = .666667*pi->ch/(1.0+cal);
				   bdis = .666667*pi->ch/(1.0+cbe);
				   if (adis > bdis) 
					   adis = bdis*(2.0-bdis/adis);
				   if (bdis > adis) 
					   bdis = adis*(2.0-adis/bdis);
				
				   dbx*= adis;
				   dby*= adis;
				   dbz*= adis;

				   tl = sqrt(dbx*dbx+dby*dby+dbz*dbz);
				   if (tl== 0)
				       tl = 1.0;
				   dbx /= tl;
				   dby /= tl;
				   dbz /= tl;
			   
				   pj->a = dbx;
				   pj->b = dby;
				   pj->c = dbz;

				   vdel_mod = sqrt((ta-pj->a)*(ta-pj->a)+(tb-pj->b)*(tb-pj->b)+(tc-pj->c)*(tc-pj->c));
				   if (vdel_mod < vchg) //??	// Not necessary? Difference not seen. Sasha, June 07, 2017
				       vchg = vdel_mod;	

			   }
			   
           }
         }
       }
     if (ptve->inv == 0)
     {
	   pi = ptve;
	   pj = ptve+1;
       co = (pi->dx*pj->a+pi->dy*pj->b+pi->dz*pj->c)/pi->ch;
       pi->a = 2.0*co*pi->dx/pi->ch-pj->a;
       pi->b = 2.0*co*pi->dy/pi->ch-pj->b;
       pi->c = 2.0*co*pi->dz/pi->ch-pj->c;

	   dbx = pj->a ;
	   dby = pj->b ;
	   dbz = pj->c ;

	   tl = sqrt(dbx*dbx+dby*dby+dbz*dbz);
	   if (tl== 0)
		   tl = 1.0;
	   pj->a /= tl;
	   pj->b /= tl;
	   pj->c /= tl;
		
     }
     pj = &ptve[m];
     if (pj->inv == 0)
     {
	   pi = pj-1;
	   co = (pi->dx*pi->a+pi->dy*pi->b+pi->dz*pi->c)/pi->ch;
	   pj->a = 2.0*co*pi->dx/pi->ch-pi->a;
	   pj->b = 2.0*co*pi->dy/pi->ch-pi->b;
	   pj->c = 2.0*co*pi->dz/pi->ch-pi->c;

	   dbx = pj->a ;
	   dby = pj->b ;
	   dbz = pj->c ;

	   tl = sqrt(dbx*dbx+dby*dby+dbz*dbz);
	   if (tl== 0)
		   tl = 1.0;
	   pj->a /= tl;
	   pj->b /= tl;
	   pj->c /= tl;
		
     }
     if (ismall) 
		 vchg = 0.0;
     if (hvchg>vchg) 
		 hvchg = vchg;
     }
	 if (ver > 8.25)
		for (i=1,pj=ptve+1;i<n;i++,pj++)
		{
			pi=pj-1;
			co = (pj->a*pi->dx+pj->b*pi->dy+pj->c*pi->dz)/pi->ch;
			if (co < -0.9999)
			{
				co = pj->a*pi->a+pj->b*pi->b+pj->c*pi->c;
				if (co < -0.9999)
				{
					pj->a = -pj->a;
					pj->b = -pj->b;
					pj->c = -pj->c;
				}
			}
		}
   status = UU_SUCCESS;
   goto done;
err:
   status = UU_FAILURE;
done:

   return(status);
   }

/**********************************************************************
**    E_FUNCTION     : uig_ncl_put_uv (pts, npt)
**       Push points on common list to use by C functions to create
**       rbspline from set of points. Note: if list is not initialized
**       this will initialize list first. F77 callable function.
**    PARAMETERS
**       INPUT  :
**          npt     - number of points in buffer to store
**          pts     - buffer with points
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int uig_ncl_put_uv (pts,npt)
UU_REAL *pts;
int *npt;
{
   int num;
   if (NCL_uvintof_init == 0)
   {
      uu_list_init (&NCL_uvintof,sizeof(UU_REAL),1200,1200);
      NCL_uvintof_init = 1;
   }
   num = *npt * 3;
   uu_list_push_multiple(&NCL_uvintof,num,pts);

   return(UU_SUCCESS);
}

/**********************************************************************
**    E_FUNCTION     : uig_ncl_free_uv()
**       Free list of points used for creation of vu curve on surface.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int
uig_ncl_free_uv ()
{
   if (NCL_uvintof_init == 1)
   {
      uu_list_free (&NCL_uvintof);
      NCL_uvintof_init = 0;
   }
   return(0);
}
/*********************************************************************
**    E_FUNCTION     :  int uig_ncl_evcrv_to_pts(cvkey,npt,cvpts,tol)
**       Evolve RB-spline curve on surface to xyz points and
**       store them in a list.
**    PARAMETERS
**       INPUT  :
**          cvkey      - key of the RB-spline curve
**       OUTPUT :
**          npt        - number of xyz points evolved
**          cvpts      - list of points evolved
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_ncl_evcrv_to_pts (cvkey, npt, cvpts)
int *cvkey;
int *npt;
UU_LIST *cvpts;
{
   struct UM_crvdatabag cv;
   int status, nu, i,i2;
	struct UM_evcrvout evout;
	UU_REAL j,uparm,dx,dy,dz,du,mid[3];

   status = UU_SUCCESS;
   cv.key = *cvkey;
	status = ncl_retrieve_data_fixed (&cv);

   if (status == UU_SUCCESS)
   {
   	if (cv.rel_num == NCL_PATERN_REL)
   	{
      	struct NCL_patern_rec *rcrv;
      	rcrv = (struct NCL_patern_rec *) &cv;
      	i = rcrv->no_patpnt;
     		uu_list_push_multiple(cvpts,i,rcrv->patpnt);
			nu = i/3;
   	}
   	else if (cv.rel_num == UM_RBSPLCRV_REL)
		{
 			struct UM_rbsplcrv_rec *rcrv;
     		rcrv = (struct UM_rbsplcrv_rec *) &cv;
     		i = rcrv->no_pt;
/*
.....The wrong points were being retrieved, what is needed are
.....the actual points on the curve and not the control points.
.....Getting 6*i number of points on the curve is an arbitrary
.....number, 6 was the lowest number to mulitply and have a really
.....good fit.  There maybe files where this doesn't end up working 
.....well. We will just have to wait and see.
.....JLS 12-6-98
*/
/*
     		uu_list_push_multiple(cvpts,i*3,rcrv->pt);
			nu = i;
*/

			j=6.0*i;
/*
.....If the curve should be reversed, we want to do this in the
.....opposite order. JLS 7/14/99
*/
			if(UIG_reverse == UU_FALSE)
			{
				for(i2=0;i2<=(6*i);i2++)
				{
					uparm=i2/j;
					um_ev7_rbsplcrv(UM_POINT,uparm,rcrv,UM_idmat,&evout);
					uu_list_push(cvpts,evout.cp);
				}
			}
			else
			{		
				for(i2=(6*i);i2>=0;i2--)
				{
					uparm=i2/j;
					um_ev7_rbsplcrv(UM_POINT,uparm,rcrv,UM_idmat,&evout);
					uu_list_push(cvpts,evout.cp);
				}
			}
			nu=(6*i)+1;

  		}
/* 
.....Adding UM_CIRCLE_REL. JLS 2/8/99
.......Modified to use tolerance and radius to determine the necessary
.......number of points to accurately represent the circle - ASF 11/12/13.
*/
		else if(cv.rel_num == UM_CIRCLE_REL)
		{
			struct UM_circle_rec *rcrv;
			rcrv = (struct UM_circle_rec *) &cv;
			um_circle_nsides(rcrv->radius,UM_TWOPI,0.001,&nu);
			du = 1./(UU_REAL)nu;
			if (UIG_reverse == UU_FALSE)
				uparm = 0.;
			else
			{
				uparm = 1.;
				du *= -1.;
			}
			for (i=0;i<nu;i++)
			{
				uig_um_ev3_circle(UM_POINT,uparm,rcrv,UM_idmat,&evout);
				uu_list_push(cvpts,evout.cp);
				uparm += du;
			}
			if (UIG_reverse == UU_FALSE) uparm = 1.;
			else uparm = 0.;
			uig_um_ev3_circle(UM_POINT,uparm,rcrv,UM_idmat,&evout);
			uu_list_push(cvpts,evout.cp);
			nu++;
		}
/*
......Adding UM_CONIC_REL. JLS 4/21/99
.......Increased the number of points for better accuracy.  Use
.......the larger of the two invariants to get a better estimate
.......of the number of points needed.   - ASF 11/12/13.
*/
		else if(cv.rel_num == UM_CONIC_REL)
		{
			struct UM_conic_rec *rcrv;
			rcrv= (struct UM_conic_rec *) &cv;
			if (rcrv->invariants[0]>rcrv->invariants[1]) du = rcrv->invariants[0];
			else du = rcrv->invariants[1];
			um_circle_nsides(du,UM_TWOPI,0.001,&nu); //nu *= 4;
			du = 1./(UU_REAL)nu;
			if (UIG_reverse == UU_FALSE)
				uparm = 0.;
			else
			{
				uparm = 1.;
				du *= -1.;
			}
			for (i=0;i<nu;i++)
			{
				um_ev4_conic(UM_POINT,uparm,rcrv,UM_idmat,&evout);
				uu_list_push(cvpts,evout.cp);
				uparm += du;
			}
			if (UIG_reverse == UU_FALSE) uparm = 1.;
			else uparm = 0.;
			um_ev4_conic(UM_POINT,uparm,rcrv,UM_idmat,&evout);
			uu_list_push(cvpts,evout.cp);
			nu++;
		}
		else if (cv.rel_num == UM_LINE_REL)
      {
			struct UM_line_rec *rcrv;
			UM_coord spt,ept;
         rcrv = (struct UM_line_rec *) &cv;
			if (UIG_reverse)
			{
				um_vctovc(rcrv->ept,spt);
				um_vctovc(rcrv->spt,ept);
			}
			else
			{
				um_vctovc(rcrv->spt,spt);
				um_vctovc(rcrv->ept,ept);
			}
			nu = 2;
			uu_list_push(cvpts,spt);
/*
.....Interpolating a line to add  9 more additional points to it
.....So that in UV space the direction of the line is known
.....Himani.
*/ 
			if (UIG_change_uv)
			{
				dx = (ept[0] - spt[0])/10;
				dy = (ept[1] - spt[1])/10;
				dz = (ept[2] - spt[2])/10;
				mid[0] = spt[0] + dx;
				mid[1] = spt[1] + dy;
				mid[2] = spt[2] + dz;
				uu_list_push(cvpts,mid);
				for(i = 1 ;i < 9 ;i++)
				{
					mid[0] = mid[0] + dx;
					mid[1] = mid[1] + dy;
					mid[2] = mid[2] + dz;
					uu_list_push(cvpts,mid);	
				}
         	nu = 11;
			}
        	uu_list_push(cvpts,ept);
       }
      else if (cv.rel_num == UM_POLYLINE_REL)
		{
     		struct UM_polyline_rec *rcrv;
     		rcrv = (struct UM_polyline_rec *) &cv;
     		i = rcrv->no_pt;
     		uu_list_push_multiple(cvpts,i*3,rcrv->pt);
     		nu = i;
   	}
		else
			status = UU_FAILURE;
   }
   *npt = nu;
   return(status);
}

/********************************************************************
**    E_FUNCTION: uig_ncl_list_push (list,gent,ptr)
**       Push (UM_coord) type on data list, update pointer to array.
**    PARAMETERS
**       INPUT  :
**          list   - pointer to list
**          gent   - pointer to entity to push on list
**       OUTPUT :
**          ptr    - pointer to the array
**    RETURNS      : number of items in list
**    SIDE EFFECTS : none
*********************************************************************/
int uig_ncl_list_push (list,gent,ptr)
UU_LIST *list;
UM_coord gent;
char **ptr;
{
   uu_list_push (list,gent);
   *ptr  = (char *) UU_LIST_ARRAY (list);
   return(UU_LIST_LENGTH(list));
}

/*********************************************************************
**    W_FUNCTION  : uig_mergecrv(cvkey, keylst, key)
**       The entities to use in making the composite curve are
**       specified by UNIBASE keys. The curves are assumed to be
**       in order in the array KESY. A composite curve2 is
**       created in UNIBASE which has the same sturcture as composit
**       curve1.
**    PARAMETERS
**       INPUT  :
**          cvkey           key of composite curve 1
**          keylst          array of keys
**       OUTPUT :
**          key             key of the new composite curve 2
**    RETURNS      : stat
**          stat = -1, iff procedure cannot complete
**                  0, otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************/
int
uig_mergecrv(cvkey, keylst,key)
UU_KEY_ID *cvkey,*keylst,*key;
   {
   struct UM_attrdata_rec attrbag;
   struct UM_compcrv_rec *eptr1;
   struct UM_compcrv_rec *eptr2;
	struct UM_cid_rec cidi;
   int i,status = UU_SUCCESS;

   eptr1 = (struct UM_compcrv_rec *)
                  uu_malloc(sizeof(struct UM_compcrv_rec));
   eptr2 = (struct UM_compcrv_rec *)
                  uu_malloc(sizeof(struct UM_compcrv_rec));

   eptr1->key = *cvkey;
   ncl_retrieve_data_fixed(eptr1);
   ur_setup_data(eptr1->rel_num, eptr2, sizeof(struct UM_compcrv_rec));
   /* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
/*
.....Make sure to mark the label with a @UN, otherwise
.....the curves make be accidentally removed from the unibase
.....by the user, causing a fatal error.  JLS 10/14/99
*/
   strcpy (eptr2->label, "@UN");
   eptr2->subscr = 0;
   eptr2->arclen = (eptr1->arclen);
   eptr2->planar = eptr1->planar;
   eptr2->open = eptr1->open;
   eptr2->continuity = eptr1->continuity;
   eptr2->fcolor = eptr1->fcolor;
	eptr2->t0 = 0.;
	eptr2->t1 = 1.;

/*   eptr2->no_cid = eptr1->no_cid;*/

   eptr2->closdinu = eptr1->closdinu;

   uc_retrieve_attr(eptr1->key, &attrbag);
   uc_create_data(eptr2, UM_DEFAULT_TF, &attrbag);
   for (i=0; i<eptr1->no_cid; i++)
   {
/*
.....Instead of updating the structure, which has a fixed
.....size, we update the varlist stored in unibase.
      eptr2->cid[i].crvid = keylst[i];

.....Want to set reverse to UU_FALSE, if the curve had needed to be 
.....reversed, it was reversed in ncl_evcrv_to_pts.  JLS 7/22/99
      eptr2->cid[i].reverse = eptr1->cid[i].reverse;

      eptr2->cid[i].reverse = UU_FALSE;
      eptr2->cid[i].endparam = eptr1->cid[i].endparam;
*/  
		cidi.crvid = keylst[i];
		cidi.reverse = UU_FALSE;
		cidi.endparam = eptr1->cid[i].endparam;
		status = ur_update_data_varlist(eptr2->key,1,&cidi,i+1,1);
		if (status != UU_SUCCESS) return (UU_FAILURE);
   }
/*
.....The new uv curves were being displayed and we don't 
.....want that.  So update the attributes as not displayable.
.....JLS 2/4/99
*/
	attrbag.key=eptr2->key;
	ur_retrieve_attr(&attrbag);
	attrbag.displayable=2;
	ur_update_attr(&attrbag);
   *key = eptr2->key;

   uu_free(eptr1);
   uu_free(eptr2);

   return (status);
   }

/*********************************************************************
**    E_FUNCTION: uig_surfpn(pext,sfkey,sfu,sfv,count)
**       Find the projection of a external point on given surface.
**       NOTE: routine has same logic as surfpn.
**    PARAMETERS
**       INPUT  :
**          pext   -  external xyz point
**          sfkey  -  key of the project surface
**          sfu,sfv - initial value of the projection uv point
**          count   - 0 when first called, do more thorough search
**                    to find initial u,v values.
**       OUTPUT :
**          sfu,sfv - project point u,v
**          psrf    - point on surface x,y,z
**    RETURNS      :
**				NONE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_surfpn(pext,sfkey,sfu,sfv,count)
UM_coord pext;
UU_KEY_ID *sfkey;
UU_REAL *sfu,*sfv;
int count;
{
	int iret = 0;
	int status = UU_SUCCESS;
	int i,rel_num;
	struct NCL_fixed_databag rsf;
	UM_transf tfmat;
	struct UM_evsrfout evsrf;
	UM_param uu, vv;

	uu = *sfu; vv = *sfv;

	ur_retrieve_data_relnum(*sfkey,&rel_num);
	if (rel_num == NCL_TRIMSF_REL)
	{
		struct NCL_trimsf_rec *trsfp;
		trsfp = (struct NCL_trimsf_rec *)&rsf;
		trsfp->key = *sfkey;
		status = ncl_retrieve_data_fixed (trsfp);
		if (status == UU_SUCCESS)
			rsf.key = trsfp->bs_key;
	}
	else
		rsf.key = *sfkey;
	if (status == UU_SUCCESS)
		status = ncl_retrieve_data_fixed (&rsf);
	if (status == UU_SUCCESS)
		status = uc_retrieve_transf(rsf.key, tfmat);

	if (count == 0 && status == UU_SUCCESS)
	{
		int j,nuv = 25;
		UU_REAL duv,u1,v1,dmin,dis;

		duv = 1.0/(nuv-1);
		if (uu < 0.0) uu = 0.0; if (uu > 1.0) uu = 1.0;
		if (vv < 0.0) vv = 0.0; if (vv > 1.0) vv = 1.0;
		status = uc_evsrf(UM_POINT, uu, vv, &rsf, tfmat, &evsrf);
		dmin = um_dcccc(pext,evsrf.sp);
/*
.......Added UM_DFUZZ to distance comparison so the first point found is kept to
.......help prevent moving too far - ASF 8/20/13.
*/
		for (i=0; i<nuv; i++)
		{
			u1 = i * duv;
			for (j = 0; j<nuv; j++)
			{
				v1 = j * duv;
				status = uc_evsrf(UM_POINT, u1, v1, &rsf, tfmat, &evsrf);
				dis = um_dcccc(pext,evsrf.sp);
				if (dis + UM_DFUZZ < dmin)
				{
					dmin = dis;
					uu = u1;
					vv = v1;
				}
			}
		}
	}

	for (i = 0; i < 2 && status == UU_SUCCESS; i++)
	{
		status = uig_usrfpn(pext,&rsf,tfmat,&uu,&vv,&iret,count);
		if (i > 0) break;
		if (status != UU_SUCCESS || iret == 0 || iret == 3) break;

		if (iret == 1 || iret == -1)
		{
			if (closdinu == 0) break;
			else uu = 1. - uu;
		}
		else if (iret == 2 || iret == -2)
		{
			if (closdinv == 0) break;
			else vv = 1. - vv;
		}
	}

	*sfu = uu; *sfv = vv;
	return (status);
}

/*********************************************************************
**    E_FUNCTION: uig_usrfpn (pext,rsf,tfmat,uu,vv,iret,count)
**       Find the projection of a external point on given surface.
**       NOTE: routine has same logic as usrfpn. (this routine follows
**       the 8.1 usrfpn almost exactly)
**    PARAMETERS
**       INPUT  :
**          pext   -  external xyz point
**          rsf    -  the project surface
**          tfmat  -  the surface's transformation
**          uu,vv  - initial value of the projection uv point
**          count   - 0 when first called, do more thorough search
**                    to find initial u,v values.
**       OUTPUT :
**          uu,vv   - project point u,v
**          iret    - return value: 1,-1 means stalled on the u=0,u=1 edge
**                                  2,-2 means stalled on the v=0,v=1 edge
**          srf     - surface plane, point on surface x,y,z
**    RETURNS      :
**				NONE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_usrfpn(pext,rsf,tfmat,uu,vv,iret,count)
UM_coord pext;
struct NCL_fixed_databag *rsf;
UM_transf tfmat;
UM_param *uu,*vv;
int *iret;
int count;
{
	UU_REAL u,v,x[4],y[4],z[4],xn,yn,zn,tx,ty,tz,xu,yu,zu,xv,yv,zv;
	UU_REAL uden,vden,du,dv,uerr,verr,ouerr,overr,hu,hv,utan,vtan,a,b,c;
	UU_REAL tol,uv_tol,duv_tol,sec,true_dist;
	int status,imid,itim;
	int disflag;
	UM_coord orig_pt,proj_pt;
	struct UM_evsrfout evsrf;
	UM_int2 primitive;
	
/*
..... eduard. tol does not depend on UNITS since pext comes here via uc_evsrf, and 
..... the latter (tigmodsp.c) does not scale or transform. 2/28/2001
*/
	tol = .0005;

   u = *uu;
   v = *vv;
   *iret = 0;
   imid = 0;
   itim = 0;
   utan = 1.0;
   vtan = 1.0;
   uv_tol = 0.0001;
   duv_tol = 0.00001;
/*
.. Do not allow O/B u,v
*/
   if (u < 0.0) u = 0.0;
   if (u > 1.0) u = 1.0;
   if (v < 0.0) v = 0.0;
   if (v > 1.0) v = 1.0;
/*
.....Assign tx, ty, tz
*/
   orig_pt[0] = tx = pext[0];
   orig_pt[1] = ty = pext[1];
   orig_pt[2] = tz = pext[2];
/*
...Solve this u,v position and uerr,verr for ini pts
*/
F10:
	status = uc_evsrf(UM_FRSTDERIV, u, v, rsf, tfmat, &evsrf);

   if (status == UU_SUCCESS)
   {
      x[0] = evsrf.sp[0] - u*evsrf.dsdu[0];
      y[0] = evsrf.sp[1] - u*evsrf.dsdu[1];
      z[0] = evsrf.sp[2] - u*evsrf.dsdu[2];
      x[2] = evsrf.sp[0] - v*evsrf.dsdv[0];
      y[2] = evsrf.sp[1] - v*evsrf.dsdv[1];
      z[2] = evsrf.sp[2] - v*evsrf.dsdv[2];
      x[1] = evsrf.dsdu[0];
      y[1] = evsrf.dsdu[1];
      z[1] = evsrf.dsdu[2];
      x[3] = evsrf.dsdv[0];
      y[3] = evsrf.dsdv[1];
      z[3] = evsrf.dsdv[2];

      xn = y[1]*z[3] - z[1]*y[3];
      yn = z[1]*x[3] - x[1]*z[3];
      zn = x[1]*y[3] - y[1]*x[3];

      xu = y[3]*zn - z[3]*yn;
      yu = z[3]*xn - x[3]*zn;
      zu = x[3]*yn - y[3]*xn;

      xv = z[1]*yn - y[1]*zn;
      yv = x[1]*zn - z[1]*xn;
      zv = y[1]*xn - x[1]*yn;
/*
.....We want to project the original point on to the
.....plane that the surface point is in, and then compare
.....the distances between the points.  This is an added
.....check to make sure that even if uerr, and verr are 
.....within their given tolerances, that the actual point
.....is also with in tolerace.  JLS 12/3/99
*/
		disflag = 0;
      srf[0] = xn;
      srf[1] = yn;
      srf[2] = zn;
      sec = sqrt(xn*xn + yn*yn + zn*zn);
      if (fabs(sec) > 0.000001)
		{
			srf[0] /= sec;
			srf[1] /= sec;
			srf[2] /= sec;

			uig_nptpln(orig_pt,evsrf.sp,srf,proj_pt);

			a = proj_pt[0]-evsrf.sp[0];
			b = proj_pt[1]-evsrf.sp[1];
			c = proj_pt[2]-evsrf.sp[2];
			true_dist = sqrt(a*a + b*b +c*c);

			if (true_dist < tol) disflag = 1;
		}
/*
..... Guard against 0 divide
*/
      uden = xu*x[1] + yu*y[1] + zu*z[1];
      vden = xv*x[3] + yv*y[3] + zv*z[3];
      if ((uden == 0) || (vden == 0 )) goto F91;
      uerr = (xu*(tx-x[0])+ yu*(ty-y[0]) +zu*(tz-z[0]))/uden - u;
      verr = (xv*(tx-x[2])+ yv*(ty-y[2]) +zv*(tz-z[2]))/vden - v;
      if (itim > 0) goto F40;
      du = 0.01;
      dv = 0.01;
      if (uerr < 0.0) du = -0.01;
      if (verr < 0.0) dv = -0.01;
      goto  F50;
F40:
      uden = ouerr-uerr;
      if (fabs(uden) < 0.00001) goto F42;
      utan = du/uden;
      if (utan > 0.0) goto F42;
      utan = -utan;
F42:
    if ((utan<=0)&&(u>(1-uv_tol))&&(uerr<0) || (u<=uv_tol)&&(uerr>0))
         utan = 1.0;

      if (utan > 1.0) utan = 1.0;
      du = uerr*utan;

      vden = overr- verr;
      if (fabs(vden) < 0.00001) goto F44;
      vtan = dv/vden;
      if (vtan > 0.0) goto F44;
      vtan = -vtan;
F44:
      if ((vtan<=0)&&(v>(1-uv_tol))&&(verr<0) || (v<=uv_tol)&&(verr>0))
         vtan = 1.0;

      if (vtan >1.0) vtan = 1.0;
      dv = verr*vtan;
F50:
      hu = u + uerr;
      hv = v + verr;

      if ((v+dv) > 1.0) dv = 1.0 - v;
      if ((u+du) < 0.0) du = -u;
      if ((u+du) > 1.0) du = 1.0 - u;
      if ((v+dv) < 0.0) dv = -v;

      u = u+du;
      v = v+dv;
/*
.....If uerr and verr are within uv_tol and disflag is equal
.....to 1 (xyz point is also within tolerance) go down and 
.....exit
*/
      if ((itim >1)&&(fabs(uerr)<uv_tol)&&(fabs(verr)<uv_tol)
				&&(disflag==1)) goto F90;
      itim = itim +1;
      ouerr = uerr;
      overr = verr;
      if (itim > 320) goto F90;
    	if (fabs(du)>duv_tol || fabs(dv)>duv_tol || 
        (disflag == 0 && itim < 50)) goto F10;
/*
..stall out (no real change in u,v)
*/
      if (fabs(hu - 0.5)< fabs(hv-0.5)) goto F65;
      if (hu < -0.002) *iret = -1;
      if (hu > 1.002)  *iret =  1;
      goto F90;
F65:
      if (hv < -0.002) *iret = -2;
      if (hv > 1.002)  *iret =  2;
/*
.. Calculate tension plane
*/
F90:
      if (fabs(sec) > 0.000001 && (fabs(sec) > .0001 || true_dist <= tol*2.))
			goto F92;
/*
...Solution failed. Retry from midpt if feasible
*/
F91:
      if (imid == 1 || itim == 0 ) goto F912;
      imid = 1;
      itim = 0;
      u = 0.5;
      v = 0.5;
      goto F10;
/*
..Specail case : if the surface primitive type for surface of revolution is a 
..sphere or cone, check if point is on apex ie. u/v = 0/1 
*/
F912:;
	  status = ncl_retrieve_data_fixed (rsf);
	  if (status != UU_SUCCESS) goto F915;
	  if (rsf->rel_num == NCL_PLN_REL)goto F915;
	  status = ncl_get_sf_primtyp(&rsf->key, &primitive);
/*
...Could not retry. Error
*/
F915:;
      *iret = 3;
      return (UU_FAILURE);
/*
...Normal exit. Load vec
*/
F92:
		*uu = u;
		*vv = v;

		srf[4] = x[0]+x[1]*u;
		srf[5] = y[0]+y[1]*u;
		srf[6] = z[0]+z[1]*u;
		srf[3] = srf[0]*srf[4]+srf[1]*srf[5]+srf[2]*srf[6];
   }
F99:
   return (status);
}

/*********************************************************************
**    E_FUNCTION: uig_usrfpn1 (pext, sfkey, sfu,sfv,psrf,count)
**       Find the projection of a external point on given surface.
**       NOTE: routine has same logic as usrfpn. (unlike usrfpn, this
**       routine does not return the iret parameter)
**    PARAMETERS
**       INPUT  :
**          pext   -  external xyz point
**          sfkey  -  key of the project surface
**          sfu,sfv - initial value of the projection uv point
**          count   - 0 when first called, do more thorough search
**                    to find initial u,v values.
**       OUTPUT :
**          sfu,sfv - project point u,v
**          psrf    - point on surface x,y,z
**    RETURNS      :
**				NONE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_usrfpn1(pext, sfkey, sfu,sfv,psrf,count)
UM_coord pext,psrf;
UU_KEY_ID *sfkey;
UU_REAL *sfu,*sfv;
int count;
{
	UU_REAL u,v,x[4],y[4],z[4],xn,yn,zn,tx,ty,tz,xu,yu,zu,xv,yv,zv;
	UU_REAL uden,vden,du,dv,uerr,verr,ouerr,overr,hu,hv,utan,vtan,a,b,c;
	UU_REAL sv[9],u1,v1,dis,dmin,tol,uv_tol,duv_tol,sec,true_dist;
	UU_REAL save_v,save_u;
	int i,j,nu,nv,status,imid,itim;
	int rel_num;
	int (*func)();
	int uig_ev_trimsf(), uig_ev_rbsplsrf(), uig_ev_revsf();
	int disflag;
	UM_vector plane;
	UM_coord orig_pt,proj_pt;
/*
..... if INCHES unit_scale=1., if MM it is 1/25.4
	if (unit_scale < 1.) tol = .0125;
	else tol = .0005;
..... eduard. commented since pext comes here via uc_evsrf, and 
..... the latter (tigmodsp.c) does not scale or transform. 2/28/2001
*/
	tol = .0005;

   u = *sfu;
   v = *sfv;
   save_v = *sfv;
   save_u = *sfu;
   imid = 0;
   itim = 0;
   utan = 1.0;
   vtan = 1.0;
   uv_tol = 0.0001;
   duv_tol = 0.00001;
   disflag=0;
/*
.. Do not allow O/B u,v
*/
   if (u < 0.0) u = 0.0;
   if (u > 1.0) u = 1.0;
   if (v < 0.0) v = 0.0;
   if (v > 1.0) v = 1.0;

/*
.....Assign tx, ty, tz
*/
   orig_pt[0] =tx = pext[0];
   orig_pt[1] =ty = pext[1];
   orig_pt[2] =tz = pext[2];
/*
.....Determine if this is a trimsrf, or a rbsplsrf
.....Use the appropriate function to evalute the surface.
.....JLS 7/22/99
*/
	ur_retrieve_data_relnum(*sfkey,&rel_num);
	if (rel_num == UM_RBSPLSRF_REL)
		func = uig_ev_rbsplsrf;
	else if (rel_num == NCL_REVSURF_REL)
		func = uig_ev_revsf;
	else
		func = uig_ev_trimsf;
/*
.....Get the best initial u,v value.  Start with the passed in values for u and v
.....then get the distance between the point on the surface at that particular uv
.....and the point that we want to find its uv value.   
*/
   status = (*(func))(sfkey,&u,&v,sv);
   dmin = um_dcccc(pext,sv); 
/*
.....Occasionally this routine will stall out and accept a point
.....that it really shouldn't.  To avoid this, when working with
.....the first point in the segment, do a more thorough job
.....initially looking for the closest point. so increase nu and
.....nv to 20, but only do it will the first point. JLS 10/14/99
*/
	if (count == 0)
	{
   	nu = 20;
   	nv = 20;
	}
	else
	{
		nu =4;
		nv = 4;
	}

   du = 1.0/(nu-1);
   dv = 1.0/(nv-1);
/*
.....I am not sure why the current values for u1 are set the way they are.
.....u's are only being checked at 1/3 and 2/3.  Lets try checking at 0,1/3,2/3, and
.....1 like v. JLS 5/13/99
.....If this is the first point, check at nineteenths.
*/
   for (i=0; i<nu; i++)
   {
      u1 = i *du;
      for (j = 0; j<nv; j++)
      {
         v1 = j*dv;
         status =(*(func))(sfkey, &u1, &v1, sv);
         dis = um_dcccc(pext,sv);
         if (dis < dmin)
         {
            dmin = dis;
            u = u1;
            v = v1;
         }
      }
   }
/*
.....If this isn't the first point in the curve, make sure that it isn't
.....the point isn't making too large of a jump. JLS 5/13/99
*/
	if (count !=0)
	{
		if ((save_v>=0.0) && (save_v <.25) && (v==1.0)) v =0.0;
		if ((save_v<=1.0) && (save_v >.75) && (v==0.0)) v =1.0;
		if ((save_u>=0.0) && (save_u <.25) && (u==1.0)) u =0.0;
		if ((save_u<=1.0) && (save_u >.75) && (u==0.0)) u =1.0;
	}
/*
...Solve this u,v position and uerr,verr for ini pts
*/
F10:
   status = (*(func))(sfkey,&u,&v,sv);

   if (status == UU_SUCCESS)
   {
      x[0] = sv[0] - u*sv[3];
      y[0] = sv[1] - u*sv[4];
      z[0] = sv[2] - u*sv[5];
      x[2] = sv[0] - v*sv[6];
      y[2] = sv[1] - v*sv[7];
      z[2] = sv[2] - v*sv[8];
      x[1] = sv[3];
      y[1] = sv[4];
      z[1] = sv[5];
      x[3] = sv[6];
      y[3] = sv[7];
      z[3] = sv[8];

      xn = y[1]*z[3] - z[1]*y[3];
      yn = z[1]*x[3] - x[1]*z[3];
      zn = x[1]*y[3] - y[1]*x[3];

      xu = y[3]*zn - z[3]*yn;
      yu = z[3]*xn - x[3]*zn;
      zu = x[3]*yn - y[3]*xn;

      xv = z[1]*yn - y[1]*zn;
      yv = x[1]*zn - z[1]*xn;
      zv = y[1]*xn - x[1]*yn;
/*
.....We want to project the original point on to the
.....plane that the surface point is in, and then compare
.....the distances between the points.  This is an added
.....check to make sure that even if uerr, and verr are 
.....within their given tolerances, that the actual point
.....is also with in tolerace.  JLS 12/3/99
*/
      plane[0] = xn;
      plane[1] = yn;
      plane[2] = zn;
		um_unitvc(plane,plane);
		uig_nptpln(orig_pt,sv,plane,proj_pt);
		a = proj_pt[0]-sv[0];
		b = proj_pt[1]-sv[1];
		c = proj_pt[2]-sv[2];
		true_dist = sqrt(a*a + b*b +c*c);

		if(true_dist<tol)
			disflag = 1;
		else
			disflag = 0;
/*
..Guard against 0 divide
*/
      uden = xu*x[1] + yu*y[1] + zu*z[1];
      vden = xv*x[3] + yv*y[3] + zv*z[3];
      if ((uden == 0) || (vden == 0 )) goto F91;
      uerr = (xu*(tx-x[0])+ yu*(ty-y[0]) +zu*(tz-z[0]))/uden - u;
      verr = (xv*(tx-x[2])+ yv*(ty-y[2]) +zv*(tz-z[2]))/vden - v;
      if (itim > 0) goto F40;
      du = 0.01;
      dv = 0.01;
      if (uerr < 0.0) du = -0.01;
      if (verr < 0.0) dv = -0.01;
      goto  F50;
F40:
      uden = ouerr-uerr;
      if (fabs(uden) < 0.00001) goto F42;
      utan = du/uden;
      if (utan > 0.0) goto F42;
      utan = -utan;
F42:
    if ((utan<=0)&&(u>(1-uv_tol))&&(uerr<0) || (u<=uv_tol)&&(uerr>0))
         utan = 1.0;

      if(utan > 1.0) utan = 1.0;
      du = uerr*utan;

      vden = overr- verr;
      if (fabs(vden) < 0.00001) goto F44;
      vtan = dv/vden;
      if (vtan > 0.0) goto F44;
      vtan = -vtan;
F44:
      if ((vtan<=0)&&(v>(1-uv_tol))&&(verr<0) || (v<=uv_tol)&&(verr>0))
         vtan = 1.0;

      if (vtan >1.0) vtan = 1.0;
      dv = verr*vtan;
F50:
      hu = u+ uerr;
      hv = v+ verr;

      if ((v+dv) >1.0) dv = 1.0- v;
      if ((u+du) < 0.0) du = -u;
      if ((u+du) >1.0) du = 1.0- u;
      if ((v+dv) < 0.0) dv = -v;
      if ((v+dv) >1.0) dv = 1.0- v;


      u = u+du;
      v = v+dv;

/*
.....If uerr and verr are within uv_tol and disflag is equal
.....to 1 (xyz point is also within tolerance) go down and 
.....exit
*/
      if ((itim >1)&&(fabs(uerr)<uv_tol)&&(fabs(verr)<uv_tol)
				&&(disflag==1)) goto F90;
      itim = itim +1;
      ouerr =uerr;
      overr = verr;
      if (itim > 320) goto F91;
    	if (fabs(du)>duv_tol || fabs(dv)>duv_tol || 
        (disflag == 0 && itim < 50)) goto F10;
/*
..stall out (no real change in u,v)
*/
      if (fabs(hu - 0.5)< fabs(hv-0.5)) goto F65;
      goto F90;
F65:
/*
.. Calculate tension plane
*/
F90:
      a = xn;
      b = yn;
      c = zn;
      sec = sqrt( a*a + b*b +c*c);
      if (fabs(sec) > 0.000001) goto F92;
/*
...Solution failed. Retry from midpt if feasible
*/
F91:
      if (imid == 1 || itim == 0 ) goto F915;
      imid =1;
      itim =0;
      u =0.5;
      v = 0.5;
      goto F10;
/*
...Could not retry. Error
*/
F915:
      return (UU_FAILURE);
/*
...Normal exit. Load vec
*/
F92:
	
   *sfu = u;
   *sfv = v;
	psrf[0] = x[0]+x[1]*u;
	psrf[1] = y[0]+y[1]*u;
	psrf[2] = z[0]+z[1]*u;
   }
F99:
   return(status);
}

/*********************************************************************
**    E_FUNCTION     : int uig_ev_trimsf (sfkey, u, v, sv)
**       Evaluate an NCL trimmed surface.
**    PARAMETERS
**       INPUT  :
**          sfkey      - surface key
**          u          - evluation parameter
**          v          - evluation parameter
**       OUTPUT :
**          sv      -  surface evaluation point
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_ev_trimsf (sfkey, u, v, sv)
   UU_KEY_ID *sfkey;
   UU_REAL *u,*v;
   UU_REAL sv[9];
   {

   int status, isze;
   UM_param uu, vv;
   struct NCL_fixed_databag bs;
   struct NCL_trimsf_rec trsf;
   UM_transf tfmat;
   struct UM_evsrfout evsrf;

   uu = *u;
   vv = *v;
   if (uu < 0.0) uu = 0.0;
   if (uu > 1.0) uu = 1.0;
   if (vv < 0.0) vv = 0.0;
   if (vv > 1.0) vv = 1.0;

   status = UU_FAILURE;
   trsf.key = *sfkey;
   status = ncl_retrieve_data_fixed (&trsf);
   if (status == UU_SUCCESS)
   {
      isze = sizeof (struct NCL_fixed_databag);
      bs.key = trsf.bs_key;
      status = uc_retrieve_data (&bs, isze);

      status = uc_retrieve_transf(bs.key, tfmat);

      if (status == UU_SUCCESS)
      status = uc_evsrf(UM_FRSTDERIV, uu, vv, &bs, tfmat, &evsrf);

      um_vctovc(&evsrf.sp[0], &sv[0]);
      um_vctovc(&evsrf.dsdu[0], &sv[3]);
      um_vctovc(&evsrf.dsdv[0], &sv[6]);
   }
   return (status);
}

/*********************************************************************
**    E_FUNCTION     : int uig_ev_revsf (sfkey, u, v, sv)
**       Evaluate an NCL surface of revolution.
**    PARAMETERS
**       INPUT  :
**          sfkey      - surface key
**          u          - evluation parameter
**          v          - evluation parameter
**       OUTPUT :
**          sv      -  surface evaluation point
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_ev_revsf (sfkey, u, v, sv)
   UU_KEY_ID *sfkey;
   UU_REAL *u,*v;
   UU_REAL sv[9];
   {

   int status;
   UM_param uu, vv;
   struct NCL_fixed_databag rsf;
   UM_transf tfmat;
   struct UM_evsrfout evsrf;

   uu = *u;
   vv = *v;
   if (uu < 0.0) uu = 0.0;
   if (uu > 1.0) uu = 1.0;
   if (vv < 0.0) vv = 0.0;
   if (vv > 1.0) vv = 1.0;

   status = UU_FAILURE;
   rsf.key = *sfkey;
   status = ncl_retrieve_data_fixed (&rsf);
	if (status == UU_SUCCESS)
      status = uc_retrieve_transf(rsf.key, tfmat);
   if (status == UU_SUCCESS)
   {
      status = uc_evsrf(UM_FRSTDERIV, uu, vv, &rsf, tfmat, &evsrf);

      um_vctovc(&evsrf.sp[0], &sv[0]);
      um_vctovc(&evsrf.dsdu[0], &sv[3]);
      um_vctovc(&evsrf.dsdv[0], &sv[6]);
   }
   return (status);
}

/*********************************************************************
**    E_FUNCTION     : uig_ncl_crvctr(&n, ptve, s, pts)
**       Generate a rational B-spline curve.
**    PARAMETERS
**       INPUT  :
**          n            number of points.
**          ptve         list of entities.
**       OUTPUT :
**          s            curve s values.
**          pts          curve control points.
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_ncl_crvctr(n, ptve, s, pts)
   int n;
   struct NCL_crvgen_rec *ptve;
   UU_REAL *s, *pts;
   {
   int i, ix, m, status;
   struct NCL_crvgen_rec *pi;

   uu_denter(UU_MTRC,(us,"uig_ncl_crvctr()"));

   status = UU_SUCCESS;
   m = n-3;
   ix = 0;
   for (i=0, pi=ptve; i<n; i++,pi++)
     {
     pts[ix++] = pi->x;
     pts[ix++] = pi->y;
     pts[ix++] = pi->z;
     }
   ix = 4;
   m  = (n - 1)/3;
   for (i=1; i<=m; i++) { s[ix++] = i; s[ix++] = i; s[ix++] = i; }

/*   for (i=1; i<=m; i++) s[i+3] = i; */
   s[0] = s[1] = s[2] = s[3] = 0.0;
   s[n+3] = s[n+2] = s[n+1] = s[n];

   uu_dexitstatus("ncl_crvgen ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : uig_create_polyline()
**       Create polyline by prompting the user for a sequence of points.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_create_polyline(npt,pts,uvkey)
int *npt;
UM_coord pts;
UU_KEY_ID *uvkey;
   {
   struct UM_polyline_rec e;     /* polyline curve */
	int status;

   /* setup unibase storage area */
   ur_setup_data(UM_POLYLINE_REL, &e, sizeof(e));

   /*MILLS: initialize LABEL and SUBSCRIPT */
	strcpy(e.label, "@UN");
	e.subscr = 0;

	e.no_pt = *npt;

	status = uc_create_data (&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
   if (status == UU_SUCCESS)
   	status = ur_update_data_varlist(e.key, 1, pts, 1, e.no_pt);

	if (status == UU_SUCCESS)
		*uvkey = e.key;
	status = ncl_retrieve_data_fixed(&e);
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : uig_create_line()
**       Create line by prompting the user for a sequence of points.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_create_line(npt,pts,uvkey)
int *npt;
UM_coord pts;
UU_KEY_ID *uvkey;
   {
   struct UM_line_rec e;   
   int status;

	if (*npt != 2) return(-1);

   /* setup unibase storage area */
   ur_setup_data(UM_LINE_REL, &e, sizeof(e));

   /*MILLS: initialize LABEL and SUBSCRIPT */
   strcpy(e.label,"@UN");
	e.subscr =0;
	um_vctovc(&pts[0], e.spt);
	um_vctovc(&pts[3], e.ept);

   status = uc_create_data (&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);

   if (status == UU_SUCCESS)
      *uvkey = e.key;
   status = ncl_retrieve_data_fixed(&e);
   return(status);
   }

/*********************************************************************
**      FUNCTION:  uig_ev_rbsplsrf()
**
**                 This is just like uig_ev_trimsf except that 
**                 it is for rbspline surfaces instead of trim 
**                 surfaces
**
**********************************************************************/
int
uig_ev_rbsplsrf(sfkey, u, v, sv)
   UU_KEY_ID *sfkey;
   UU_REAL *u,*v;
   UU_REAL sv[9];
   {

   int status, isze;
   UM_param uu, vv;
   struct NCL_fixed_databag bs;
   struct  UM_rbsplsrf_rec trsf;
   UM_transf tfmat;
   struct UM_evsrfout evsrf;

   uu = *u;
   vv = *v;
   if (uu < 0.0) uu = 0.0;
   if (uu > 1.0) uu = 1.0;
   if (vv < 0.0) vv = 0.0;
   if (vv > 1.0) vv = 1.0;

   status = UU_FAILURE;
   trsf.key = *sfkey;
   status = ncl_retrieve_data_fixed (&trsf);
   if (status == UU_SUCCESS)
   {
      isze = sizeof (struct NCL_fixed_databag);
      bs.key = trsf.key;
      status = uc_retrieve_data (&bs, isze);

      status = uc_retrieve_transf(bs.key, tfmat);

      if (status == UU_SUCCESS)
      status = uc_evsrf(UM_FRSTDERIV, uu, vv, &bs, tfmat, &evsrf);

      um_vctovc(&evsrf.sp[0], &sv[0]);
      um_vctovc(&evsrf.dsdu[0], &sv[3]);
      um_vctovc(&evsrf.dsdv[0], &sv[6]);
   }
   return (status);
}

/*********************************************************************
**
**           FUNCTION:  uig_ncl_crvfit
**
**           This function is the same as ncl_crvfit in
**           nclc/negeogn.c
**
**********************************************************************/
int uig_ncl_crvfit (npts, ptve)
int *npts;
struct NCL_crvgen_rec *ptve;
	{
	int i, k, ih, m, n, imode, otol, status;
	UU_REAL xc, yc, zc, hmov, htol, ftol;
	UU_REAL ro;
	UM_real8 tol8;
	struct NCL_crvgen_rec *pi, *pj, *hldseg, *ph;

	uu_denter(UU_MTRC,(us,"uig_ncl_crvfit ()"));

	hldseg = 0;
	gettol (&tol8);
	ftol = tol8/2.0;
	htol = 1.e-6;
	n = *npts;
	m = n-1;
	imode = 1;
	if (iges_in==1)
		status = uig_ncl_slpset (n, ptve, imode);
	else
		status = uig_ncl_slpset_step (n, ptve, imode);


	if (status == UU_SUCCESS)
	{
		for (i=1,pi=ptve; i<m; i++,pi++)
		{
			status = uig_ncl_segchk(&xc,&yc,&zc,pi,pi+2,imode);
			pj = pi+1;
			pj->dx = xc-pj->x;
			pj->dy = yc-pj->y;
			pj->dz = zc-pj->z;
		}
	}
	if (status == UU_SUCCESS)
	{
		for (i=1,pi=ptve+1; i<m; i++,pi++)
		{
			if (pi->inv == 0)
			{
				hmov = sqrt (pi->dx*pi->dx+pi->dy*pi->dy+pi->dz*pi->dz);
				if (hmov > htol)
				{
					ro = ftol/hmov;
					if (ro > 1.0) ro = 1.0;
					pi->x = pi->x+pi->dx*ro;
					pi->y = pi->y+pi->dy*ro;
					pi->z = pi->z+pi->dz*ro;
				}
			}
		}
		imode = 2;
		//status = uig_ncl_slpset (n, ptve, imode);
		if (iges_in==1)
			status = uig_ncl_slpset (n, ptve, imode);
		else
			status = uig_ncl_slpset_step (n, ptve, imode);
	}

	if (status == UU_SUCCESS)
	{
		hldseg = (struct NCL_crvgen_rec *) uu_malloc(n*sizeof(*hldseg));
		ph = hldseg;
		pi = ptve;
		*ph = *pi;
		ph++;
		ih = 1;
		pj = pi+2;
		k = 2;
		if (hldseg == 0) status = UU_FAILURE;
	}
	while (status == UU_SUCCESS && k<n)
	{
		status = uig_ncl_segchk (&xc,&yc,&zc,pi,pj,imode);
		if (status == UU_SUCCESS)
		{
			otol = fabs(xc)+fabs(yc)+fabs(zc) > ftol;
			if (otol || pj->inv == 1)
			{
				if (otol)
				{
					pj--;
					k--;
				}
				*ph = *pj;
				ph++;
				ih++;
				pi = pj;
				pj += 2;
				k += 2;
			}
			else
			{
				pj++;
				k++;
			}
		}
	}
	if (status == UU_SUCCESS)
	{
		if (k < n+1)
		{
			*ph = *(pj-1);
			ih++;
		}
		for (i=0,pi=ptve,ph=hldseg; i<ih; i++,pi++,ph++)
		{
			pi->x = ph->x;
			pi->y = ph->y;
			pi->z = ph->z;
			pi->a = ph->a;
			pi->b = ph->b;
			pi->c = ph->c;
			if (i<ih-1)
			{
				pj = ph+1;
				pi->dx = pj->x-ph->x;
				pi->dy = pj->y-ph->y;
				pi->dz = pj->z-ph->z;
				pi->ch = sqrt(pi->dx*pi->dx+pi->dy*pi->dy+pi->dz*pi->dz);
			}
		}
	}

	*npts = ih;
	if (hldseg != 0) uu_free (hldseg);

	uu_dexitstatus("uig_ncl_crvfit ()", status);
	return(status);
}

/*********************************************************************
**   
**      FUNCTION:  uig_ncl_crvgen()
** 
**                 This is the same routine as ncl_crvgen
**
*********************************************************************/
int uig_ncl_crvgen (n, ptve, s, pts)
   int n;
   struct NCL_crvgen_rec *ptve;
   UU_REAL *s, *pts;
   {
   int i, ix, m, status;
   UU_REAL arcsum, arcl, cal, cbe, adis, bdis, cdis, ro;
   UU_REAL dxr, dyr, dzr, dxc, dyc, dzc;
   struct NCL_crvgen_rec *pi, *pj;

   status = UU_SUCCESS;
   m = n-1;
   arcsum = 0.0;
   ix = 0;
   for (i=0,pi=ptve,pj = &ptve[1]; i<m; i++,pi++,pj++)
     {
     cal = (pi->a*pi->dx+pi->b*pi->dy+pi->c*pi->dz)/pi->ch;
     cbe = (pj->a*pi->dx+pj->b*pi->dy+pj->c*pi->dz)/pi->ch;
     adis = .666667*pi->ch/(1.0+cal);
     bdis = .666667*pi->ch/(1.0+cbe);
     if (adis > bdis) adis = bdis*(2.0-bdis/adis);
     if (bdis > adis) bdis = adis*(2.0-adis/bdis);

     pi->a *= adis;
     pi->b *= adis;
     pi->c *= adis;
     dxr = pi->dx-pj->a*bdis;
     dyr = pi->dy-pj->b*bdis;
     dzr = pi->dz-pj->c*bdis;
     dxc = dxr+pi->dx-pi->a;
     dyc = dyr+pi->dy-pi->b;
     dzc = dzr+pi->dz-pi->c;
     cdis = sqrt(dxc*dxc+dyc*dyc+dzc*dzc);
     ro = 1.62*(adis+bdis)/cdis-.81;
     arcl = (.5-ro)*(adis+bdis)+(.5+.5*ro)*cdis;
     arcsum += arcl;
     s[i] = arcsum;
     pts[ix++] = pi->x;
     pts[ix++] = pi->y;
     pts[ix++] = pi->z;
     pts[ix++] = pi->x+pi->a;
     pts[ix++] = pi->y+pi->b;
     pts[ix++] = pi->z+pi->c;
     pts[ix++] = pi->x+dxr;
     pts[ix++] = pi->y+dyr;
     pts[ix++] = pi->z+dzr;
     }
   pts[ix++] = pi->x;
   pts[ix++] = pi->y;
   pts[ix++] = pi->z;
/*
.....s is the array for the knot vectors.
*/
   for (i=m-1; i>=0; i--)
     {
     ix = 3*i+4;
     s[ix+2] = s[ix+1] = s[ix] = s[i];
     }
   s[0] = s[1] = s[2] = s[3] = 0.0;
   s[3*n+1] = s[3*n];

   return(status);
   }
/***************************************************************
**
**      FUNCTION:  uig_ncl_segchk()
**
**      Same routine as NCL's ncl_segchk
**
****************************************************************/
int uig_ncl_segchk (xc,yc,zc,pi,pk,imode)
   UU_REAL *xc, *yc, *zc;
   struct NCL_crvgen_rec *pi, *pk;
   int imode;
   {
   int itim, status;
   UU_REAL ctan, hdx, hdy, hdz, dlx,dly,dlz, cc,cal,cbe,adis,bdis;
   UU_REAL xq,yq,zq,xr,yr,zr,c1,c2,c3,xa,ya,za,xb,yb,zb;
   UU_REAL u, du, uerr,oerr, den;
   struct NCL_crvgen_rec *pj;

   status = UU_SUCCESS;
   ctan = 1.0;
   hdx = 0.0;
   hdy = 0.0;
   hdz = 0.0;
   pj = pi+1;
   dlx = pk->x - pi->x;
   dly = pk->y - pi->y;
   dlz = pk->z - pi->z;
   cc = sqrt(dlx*dlx+dly*dly+dlz*dlz);

   cal = (dlx*pi->a+dly*pi->b+dlz*pi->c)/cc;
   cbe = (dlx*pk->a+dly*pk->b+dlz*pk->c)/cc;
   adis = .666667*cc/(1.0+cal);
   bdis = .666667*cc/(1.0+cbe);
   if (adis>bdis) adis = bdis*(2.0-bdis/adis);
   if (bdis>adis) bdis = adis*(2.0-adis/bdis);
   xq = pi->x+pi->a*adis;
   yq = pi->y+pi->b*adis;
   zq = pi->z+pi->c*adis;
   xr = pk->x-pk->a*bdis;
   yr = pk->y-pk->b*bdis;
   zr = pk->z-pk->c*bdis;

   while (pj < pk)
     {
     for (itim=0, u=0.5; itim<10; itim++)
       {
       c1 = 1.0-u;
       c1 = c1*c1;
       c2 = 2.0*(1.0-u)*u;
       c3 = u*u;
       xa = c1*pi->x+c2*xq+c3*xr;
       ya = c1*pi->y+c2*yq+c3*yr;
       za = c1*pi->z+c2*zq+c3*zr;

       xb = c1*xq+c2*xr+c3*pk->x-xa;
       yb = c1*yq+c2*yr+c3*pk->y-ya;
       zb = c1*zq+c2*zr+c3*pk->z-za;
       uerr = (xb*(pj->x-xa)+yb*(pj->y-ya)+zb*(pj->z-za))/(xb*xb+yb*yb+zb*zb)-u;
       uerr = uerr/3.0;
       if (fabs(uerr)<1.0e-5)
         itim = 10;
       else
         {
         if (itim > 0)
           {
           den = oerr-uerr;
           if (fabs(den)>1.0e-5)
             {
             ctan = du/den;
             if (ctan<.1) ctan = 1.0;
             }
           }
         du = uerr*ctan;
         if (du+u > 1.0) du = 1.0-u;
         if (du+u < 0.0) du = -u;
         u = u+du;
         oerr = uerr;
         }
       }
     *xc = xa+u*xb;
     *yc = ya+u*yb;
     *zc = za+u*zb;
     if (imode == 2)
       {
       dlx = *xc-pj->x;
       dly = *yc-pj->y;
       dlz = *zc-pj->z;
       if (fabs(dlx) > fabs(hdx)) hdx = dlx;
       if (fabs(dly) > fabs(hdy)) hdy = dly;
       if (fabs(dlz) > fabs(hdz)) hdz = dlz;
       }
     pj++;
     }
   if (imode == 2)
     {
     *xc = hdx;
     *yc = hdy;
     *zc = hdz;
     }

   return(status);
	}
/*******************************************************************************
**  E_FUNCTION:  int um_ev3_circle(evflag,u,eptr,tfmat,evout)
**       Evaluate a circle at a parameter.
**    PARAMETERS:
**       INPUT:
**          evflag         UM_POINT= calculate point on circle only;
**                         UM_FRSTDERIV= calculate point and 1st
**                                   derivative;
**                         UM_SECDERIV= calculate point, 1st and 2nd
**                                   derivative;
**                         UM_CURVATURE= calc point, 1st, 2nd deriv,
**                                   and curvature;
**          u              the parameter value in range [0,1]
**
**          eptr           pointer to the entity record
**          tfmat          transformation matrix.
**       OUTPUT:
**          evout          pointer to a curve evaluator
**                         record containing both the requested
**                         information, and (ultimately) a
**                         status value indicating the validity
**                         of the requested information.
**    RETURNS : none currently, ultimately will return one of the
**          following:
**            UM_VALID: all requested fields are valid;
**            UM_BADFIELDS: at least one requested fields is invalid;
**            UM_BADRECORDS: at least one entire record is invalid;
**            UM_INVALID: all output is suspect.
**    SIDE EFFECTS :  none
**    WARNINGS     :  none
**************************************************************************/
int
uig_um_ev3_circle(evflag,u,eptr,tfmat,evout)
   int  evflag;
   UM_param u;
   struct UM_circle_rec *eptr;
   UM_transf tfmat;
   struct UM_evcrvout *evout;
   {
   UM_length rad;          /* radius of circle */
   UM_angle dang;          /* total delta angle of arc entity */
   UU_REAL *center;        /* center of circle */
   UU_REAL *svec;          /* direction unit vector from center
                              to start point on circle */
   UU_REAL *nvec;          /* direction unit vector normal to plane
                              containing the circle*/
   UU_REAL *circpt;        /* pointer to output location for new point */
   UU_REAL *tangent;       /* pointer to output location for tangent vector */
   UU_REAL *accel;        /* pointer to output location for second deriv */
   UM_vector crosvec;      /* vector in plane of circle and at rt angle
                              to svec    */
   UM_vector radvec;       /* storage for circpt - center */
   UM_angle ang;           /* angle of rotation from starting pt */
   UU_REAL cosine;
   UU_REAL sine;
   int i;

   uu_denter(UU_MTRC,
      (us,"uig_um_ev3_circle(evflag:%d,u:%g,key:%d,tfmat:%x,evout:%x)",
      evflag,u,eptr->key,tfmat,evout));

   rad = eptr->radius;
   dang = eptr->dang;
   center = eptr->center;
   svec = eptr->svec;
   nvec = eptr->nvec;

   circpt = evout->cp; /*circpt now pts to the storage location to put new pt*/

   /*"radvec" will have the correct direction and magnitude*/
   um_vctmsc(svec,rad,radvec);

   /*get angle from starting pt to the pt corresponding to u*/
   ang = dang * u;
   um_cross(nvec, radvec, crosvec);
   cosine = cos(ang);
   sine = sin(ang);
   circpt[0] = sine * crosvec[0] + cosine * radvec[0] + center[0];
   circpt[1] = sine * crosvec[1] + cosine * radvec[1] + center[1];
   circpt[2] = sine * crosvec[2] + cosine * radvec[2] + center[2];

   if (evflag != UM_POINT) /* evout->cp has new pt*/
      {
      for (i=0; i<3; i++)/* get direction vector of circpt - center */
         radvec[i] = circpt[i] - center[i];

      tangent = evout->dcdu;
      /* now do:  tangent = dang * (nvec X (circpt - center))  */
      tangent[0] =  dang * (nvec[1] * radvec[2] - nvec[2] * radvec[1]);
      tangent[1] =  dang * (nvec[2] * radvec[0] - nvec[0] * radvec[2]);
      tangent[2] =  dang * (nvec[0] * radvec[1] - nvec[1] * radvec[0]);

      if (evflag != UM_FRSTDERIV) /*evout->dcdu has tan vector*/
         {
         accel = evout->d2cdu2;
         dang = (-1.0) * dang * dang;
         for (i=0; i<3; i++)  accel[i] = dang * radvec[i];
         if (evflag != UM_SECDERIV)  /* evout->d2cdu2 has 2nd deriv */
            evout->curv = 1.0 / rad;
         }/* end not UM_FRSTDERIV */
      }/* end not UM_POINT */

   /* position results in evaluator record according to the transform, tfmat */
   um_transform_evcrvout(evflag, eptr, tfmat, evout);
   uu_dexit;

   return (UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : int uig_nptpln(pt,ppt,unvc,npt)
**       Project a point onto a plane..
**    PARAMETERS
**       INPUT  :
**          pt         - point to be projected
**          ppt        - point defining the plane
**          unvc       - unit normal vector defining the plane
**       OUTPUT :
**          npt        - projected point on plane
**    RETURNS      :
**         
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_nptpln(pt,ppt,unvc,npt)
UM_coord pt;
UM_coord ppt;
UM_vector unvc;
UM_coord npt;
{

	UU_REAL proj;           /* projection along normal */
	UM_vector vc;           /* temporary vector */

	um_vcmnvc(pt, ppt, vc);
	proj = um_dot(vc, unvc);
	um_vctmsc(unvc, -proj, vc);
	um_vcplvc(pt, vc, npt);

	return(0);
}

/*********************************************************************
**    E_FUNCTION  : int uig_adjust_boundary_new(uvpts, closed_u, closed_v, 
																sfkey, uv)
**      If the uv curve lies outside the uv boundary of a surface of
**		  revolution, then the start and end angle of the generatrix is modified. 
**    PARAMETERS
**       INPUT  :
**          uvpts     -  parametric points that make up a trimming curve
**
**          closed_u  -  1 if surface is closed in u (periodic -> sphere,
**                         cylinder, surface of revolution)
**                       0 if not closed in u
**
**          closed_v  -  1 if surface is closed in v (periodic -> sphere,
**                         cylinder, surface of revolution)
**                       0 if not closed in v
**				sfkey		 -  Base surface key id.
**          uv[6]      - min, max, and periods of u and v
**                       uv[0] = u min
**                       uv[1] = u max
**                       uv[2] = v min
**                       uv[3] = v max
**                       uv[4] = u period ( = 0.0 if not periodic)
**                       uv[5] = u period ( = 0.0 if not periodic)
**       OUTPUT :
**          uvpts     -  parametric points after they have been adjusted
**                       to account for surface boundaries.
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : parametric points modified
**    WARNINGS     : none
*********************************************************************/
int 
uig_adjust_boundary_new(uvpts, closed_u, closed_v, sfkey, uv)
int closed_u, closed_v;
UU_LIST uvpts;
UU_KEY_ID *sfkey;
UU_REAL uv[6];
{
	int status, i, j, npts, ix, ic;
	UU_REAL tmin, tmax, degmax, lnmax, dist, dx, rmin, rmax, dmin, dmax;
	UM_coord npt1, npt2, *pext, spt, ept, segspt, segept;
	struct NCL_fixed_databag sf;
	struct NCL_revsurf_rec *rsf;

	rmin = 0.0001;
	rmax = 0.9999;
	tmin = 0.2;
	tmax = 0.8;
	dx = 0.04999;

	pext = (UM_coord *) UU_LIST_ARRAY (&uvpts);
	npts = uvpts.cur_cnt;

	dmax = 0.0;
	dmin = 1.0;
	lnmax = 0.0;

/*
..... ix and ic define the changing and the constant coordinates respectively.
..... When closed in u , the changing coordinate is u hence ix is 0,
..... and the constant coordinate is v hence ic is 1.
*/
	if (closed_u)
	{
		ix = 0;
		ic = 1;
	}
	else if (closed_v)
	{
		ix = 1;
		ic = 0;
	}
/* 
..... we find the furthest non intersecting generatrix position from all the
..... segments formed by the the points of the curve. We consider 20 such
..... positions in the uv space.
*/
	spt[ix] = ept[ix] = rmin - dx;
	spt[ic] = 0.0;
	ept[ic] = 1.0;

	for (i = 0; i < 21; i++)
	{
		dmin = 1.0;
		spt[ix] = spt[ix] + dx;
		ept[ix] = ept[ix] + dx;	
		for (j = 0; j < npts; j++)
		{
/*
.....If this is the last point in the list then also consider the segment
..... formed by the last and first point.
*/
			if (j == npts-1)
			{
				segspt[0] = pext[j][0];
				segspt[1] = pext[j][1];
				segept[0] = pext[0][0];
				segept[1] = pext[0][1];
			}
			else
			{
				segspt[0] = pext[j][0];
				segspt[1] = pext[j][1];
				segept[0] = pext[j+1][0];
				segept[1] = pext[j+1][1];
			}
/* 
.....If one point of the segment lies close to u/v = 0.0 and the other lies
.....close to u/v = 1.0 , then we ignore such a segment, since it crosses the
.....original generatrix.
*/
			if ((segspt[ix] < tmin && segept[ix] > tmax) || 
				(segspt[ix] > tmax && segept[ix] < tmin))
				dist =1.0;
			else
			{
				dist = uig_lnlndis(segspt, segept, spt, ept, npt1, npt2);
				if (dist == 0.0) 
				{
					dmin = 0.0;
					break;
				}
			}
			if (dmin > dist)
				dmin = dist;
		}
		if (dmax < dmin)
		{
			dmax = dmin;
			lnmax = spt[ix];
		}
	}
/* 
.....For this new positon find the corresponding position in degrees
.....for the start and the end agnle of revolution.
*/
	degmax = lnmax * 360.0;
	if (degmax > 180.0) degmax = degmax - 360.0;
	sf.key = sfkey[0];
	status = ncl_retrieve_data_fixed(&sf);
	rsf = (struct NCL_revsurf_rec *)&sf;
	rsf->sa = rsf->sa + degmax;
	rsf->ta = rsf->ta + degmax;
/*
..... angles greater than 360
*/
	if(rsf->sa > 360) rsf->sa = rsf->sa -360;
	if(rsf->ta > 360) rsf->ta = rsf->ta -360;
	uv[2] = (rsf->sa) * UM_PI / 180;
	uv[3] = (rsf->ta) * UM_PI / 180;
	if (status == UU_SUCCESS) status = ur_update_data_fixed(rsf);		
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  UU_REAL uig_lnlndis(spt1,ept1,spt2,ept2,npt1,npt2)
**       Calculate the squared distance and near points for 2 line segments.
**    PARAMETERS
**       INPUT  :
**          spt1    - Start point of line 1.
**          ept1    - End point of line 1.
**          spt2    - Start point of line 2.
**          ept2    - End point of line 2.
**       OUTPUT :
**          npt1    - Point on line 1 nearest to line 2.
**          npt2    - Point on line 2 nearest to line 1.
**    RETURNS      : Distance**2
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL uig_lnlndis(spt1,ept1,spt2,ept2,npt1,npt2)
UM_coord spt1,ept1,spt2,ept2,npt1,npt2;
{
	UM_vector u,v,w;
	UU_REAL a,b,c,d,e,D;
	UU_REAL sc, sN, sD;
	UU_REAL tc, tN, tD;

	um_vcmnvc(ept1,spt1,u);
	um_vcmnvc(ept2,spt2,v);
	um_vcmnvc(spt1,spt2,w);
	a = UM_DOT(u,u);        /* always >= 0 */
	b = UM_DOT(u,v);
	c = UM_DOT(v,v);        /* always >= 0 */
	d = UM_DOT(u,w);
	e = UM_DOT(v,w);
	D = a*c - b*b;          /* always >= 0 */
	sD = D;
	tD = D;
/*
..... compute the line parameters of the two closest points
*/
	if (D < UM_DFUZZ) {
/*
..... the lines are almost parallel
*/
		sN = 0.0;
		tN = e;
		tD = c;
	}
	else {
/*
..... get the closest points on the infinite lines
*/
		sN = (b*e - c*d);
		tN = (a*e - b*d);
		if (sN < 0) {
/*
..... sc < 0 => the s=0 edge is visible
*/
			sN = 0.0;
			tN = e;
			tD = c;
		}
		else if (sN > sD) {
/*
..... sc > 1 => the s=1 edge is visible
*/
			sN = sD;
			tN = e + b;
			tD = c;
		}
	}
/*
..... tc < 0 => the t=0 edge is visible
*/
	if (tN < 0) {
		tN = 0.0;
		if (-d < 0)
			sN = 0.0;
		else if (-d > a)
			sN = sD;
		else {
			sN = -d;
			sD = a;
		}
	}
	else if (tN > tD) {
/*
..... tc > 1 => the t=1 edge is visible
*/
		tN = tD;
		if ((-d + b) < 0)
			sN = 0;
		else if ((-d + b) > a)
			sN = sD;
		else {
			sN = (-d + b);
			sD = a;
		}
	}
/*
..... finally do the division to get sc and tc
*/
	sc = (sD == 0.0 || sN == 0.0) ? sN : sN / sD;
	tc = (tD == 0.0 || tN == 0.0) ? tN : tN / tD;
/*
..... get the difference of the two closest points
*/
	um_vctmsc(u,sc,u);
	um_vctmsc(v,tc,v);
	um_vcplvc(spt1,u,npt1);
	um_vcplvc(spt2,v,npt2);
	D = UM_SQDIS(npt1,npt2);
	return (D);
}

/*********************************************************************
**    I_FUNCTION     :  S_adjust_sphere_boundary(uvpts)
**       Repairs a UV boundary curve on a sphere.  NCL has problems
**       when the boundary curve starts at V=0 and goes to V=1 for
**       some reason, for example when trimming a sphere in half.
**    PARAMETERS
**       INPUT  :
**          uvpts   - UV point list of boundary curve.
**       OUTPUT :
**          uvpts   - Updated UV point list of boundary curve.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_adjust_sphere_boundary(uvpts)
UU_LIST *uvpts;
{
	int i,npts;
	UU_REAL rnum,rmax;
	UM_coord *pts;
/*
.....Initialize routine
*/
	npts = UU_LIST_LENGTH(uvpts);
	pts = (UM_coord *)UU_LIST_ARRAY(uvpts);
/*
.....Determine if U-direction goes 360 degrees
*/
	if ((fabs(pts[0][0]) < UM_FUZZ && fabs(pts[npts-1][0]) < UM_FUZZ) ||
	 (fabs(1.-pts[0][0]) < UM_FUZZ && fabs(1.-pts[npts-1][0]) < UM_FUZZ))
	{
/*
.....Now make sure the ending V-values
.....Match the starting V-values
.........Set boundary value
*/
		if (fabs(pts[0][1]) <= .02)
			rnum = 0.;
		else if (fabs(pts[0][1]) >= .98)
			rnum = 1.;
		else
			npts = 0;
/*
........Loop through and modify points
*/
		rmax = 1. - rnum;
		for (i=0;i<npts;i++)
		{
			if (fabs(pts[i][1]-rmax) < UM_FUZZ)
				pts[i][1] = rnum;
		}
	}
}

/*********************************************************************
**    I_FUNCTION     :  S_adjust_cone_boundary(uvpts)
**       Repairs a UV boundary curve on a cone by providing a smooth
**       transition from 0,0 (apex) to 1,1.
**    PARAMETERS
**       INPUT  :
**          uvpts   - UV point list of boundary points.
**       OUTPUT :
**          uvpts   - Updated UV point list of boundary points.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_adjust_cone_boundary(uvpts)
UU_LIST *uvpts;
{
	int i,npts,inc;
	UM_coord *pts,uv;
/*
.....Initialize routine
*/
	npts = UU_LIST_LENGTH(uvpts);
	pts = (UM_coord *)UU_LIST_ARRAY(uvpts);
/*
.....Find if transition is required
*/
	inc = npts - 1;
	for (i=0;i<npts;i++)
	{
		if (pts[i][0] < UM_FUZZ && pts[inc][0]+UM_FUZZ >= 1. &&
			pts[inc][1]+UM_FUZZ >= 1.)
		{
			uv[0] = 0.; uv[1] = 1.; uv[2] = 0.;
			uu_list_insert(uvpts,i,uv);
			break;
		}
		if (pts[inc][0] < UM_FUZZ && pts[i][0]+UM_FUZZ >= 1. &&
			pts[i][1]+UM_FUZZ >= 1.)
		{
			uv[0] = 0.; uv[1] = 1.; uv[2] = 0.;
			uu_list_insert(uvpts,i,uv);
			break;
		}
		inc = i;
	}
}


