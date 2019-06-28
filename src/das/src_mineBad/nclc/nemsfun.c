/*********************************************************************
**	 NAME:  nemsfun.c
**		 CONTAINS:
**		contain functions have same function name and most functions
**		are the same but some are modified for MSLITE
**
**		ncl_cutr_arcc2p
**		ncl_evolve_circ
**		ncl_evolve_curve_own
**		ncl_evolve_curve
**		ncl_evolve_all_curves
**		ncl_revsf_planes
**		ncl_mcstowcs
**		ncl_cutter_same
**		ncl_43mx_to_34mx
**		ncl_34mx_to_43mx
**		ncl_43mx_to_44mx
**		ncl_init_box
**		ncl_update_box
**		ncl_get_uvcvonsf_sfkey()
**		ncl_get_uvcvonsf_transf()
**		ncl_itsa_uvcv_onsf
**		ncl_sprintf
**		ncl_sprintf2
**		ncl_invert_matrix
**		ncl_h_cnpts
**
**	 COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**	   MODULE NAME AND RELEASE LEVEL 
**       nemsfun.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       04/05/18 , 14:57:18
*********************************************************************/

#include "usysdef.h"
#include "lipv.h"
#include "zsysdep.h"
#include "class.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdcpln.h"
#include "mdcoord.h"
#include "gomisc.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gviw.h"
#include "gsegop.h"
#include "gmat4.h"
#include "gconvert.h"
#include "mdcoord.h"
#include "mfort.h"
#include "modef.h"
#include "mplot.h"
#include "mgeom.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nccs.h"
#include "nclxmdl.h"
#include "mdeval.h"
#include "modef.h"
#include "ulist.h"
#include "mdunits.h"

#define DTOR (UM_PI/180.0)

extern int DRAW_DISPLAY;
static UM_vector Svr = {0.0, 0.0, 0.0};

extern int MSLite;
extern int NCLX_internal_geom;
extern int NCL_accuracy;
extern char uw_color_name[64][96];
extern int uw_color_table[64][3];
extern int UW_Store_color;
/*
.....make the NCL color changed, but may not saved, NCL_clrs_changed = 1, changed but not save
.....										NCL_clrs_changed = 0, not changed, or changed and saved
*/
int NCL_clrs_changed = 0;
/*********************************************************************
**    E_FUNCTION     : int ncl_cutr_arcc2p(center, pt1, pt2, cptr)
**         Create an arc  which has
**            1. the given CENTER
**            2. lies in the plane defined by the CENTER and
**               normal where the normal is the cross product of
**               the vector from the CENTER to PT1 and the vector
**               from the CENTER to PT2;
**            3. has a radius defined by the CENTER and PT1
**            4. starts at point PT1 
**				Copied from necdraw.c: Same as NCL function
**    PARAMETERS   
**       INPUT  : 
**            center               center of circle
**            pt1                  first point of arc
**            pt2                  second point of arc
**       OUTPUT :  
**          cptr                  circular arc
**    RETURNS      : 
**            UU_SUCCESS iff arc data defined; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cutr_arcc2p(center, pt1, pt2, cptr)
UM_coord center;
UM_coord pt1;
UM_coord pt2;
struct UM_circle_rec *cptr;

{
	int status;
	UM_vector v0, v1;  /* vecs from the center of circle to pts 0 and 1 */

	status = UU_SUCCESS;
	if (um_cceqcc(center,pt1))
		status = UU_FAILURE;
	else if (um_cceqcc(center,pt2))
		status = UU_FAILURE;
	else if (um_cceqcc(pt1, pt2))
	{
		status = um_c3_cp(center, pt1, cptr);
	}
	else
	{
		um_vctovc(center, cptr->center);
		cptr->radius = um_dcccc(pt1, cptr->center);
		um_vcmnvc(pt1, cptr->center, v0);
		um_unitvc(v0, cptr->svec);
		um_vcmnvc(pt2, cptr->center, v1);
		um_unitvc(v1,v1);
		if (um_vcparall(cptr->svec, v1))
			um_vctovc(UM_cpln.zaxis, cptr->nvec);
		else
			um_cross(cptr->svec, v1, cptr->nvec);
   
		um_unitvc(cptr->nvec, cptr->nvec);
		cptr->dang = um_angle2p(cptr->svec, v1, cptr->nvec);
		if (cptr->radius * fabs(cptr->dang) < UM_FUZZ)
			status = UU_FAILURE;
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION: ncl_evolve_circ (eptr,tfmat,told,pptr,vptr,uptr)
**       Evolve a circle into set of points with given chordal tolerance.
**    PARAMETERS   
**       INPUT  : 
**          eptr   - pointer to curve record
**          tfmat  - ID matrix
**          told   - tolerance (chord higth)
**       OUTPUT :  
**          pptr  - pointer to points list 
**          vptr  - pointer to slope vectors list 
**          uptr  - pointer to u-parameters list
**    RETURNS      : 
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_circ (cptr,tfmat,told,pptr,vptr,uptr)
struct  UM_circle_rec *cptr;
UM_transf tfmat;
UU_REAL told;
UU_LIST *pptr, *vptr, *uptr;
{
	struct UM_evcrvout evout;
	int evflag;
	int i, j;
	UU_REAL u, r, du, delt;
	UU_REAL dang,alpha;
	UM_coord uvp;

	uvp[1] = uvp[2] = 0.;

	r = cptr->radius;
	dang = cptr->dang / 2.;
	if (dang < 0.0)
	{
		if (fabs(dang + UM_PI) > UM_FUZZ)
			dang = UM_PI + dang;
		else
			dang = fabs(dang);
	}

	if (r < 0.0) return (-1);
	if (dang < UM_HALFPI && 2.*r*sin(dang) <= told)
		i = 1;
	else
	{
		/*delt = 0.95*told/r;*/
		delt = told/r;		// Sasha, July 03, 2017
		if (delt >= 1.)
		{
			if (dang <= UM_HALFPI) i = 2;
			else i = 3;
		}
		else
		{
			/*alpha = acos (1 - delt);*/ 
			alpha = 2*acos (1 - delt);	// Sasha, July 03, 2017
			/*i = (int) ceil (dang/alpha);*/
			i = (int) floor (dang/alpha);
			if (i < 2) i = 2;
		}
	}

	evflag = (vptr == UU_NULL)? UM_POINT: UM_FRSTDERIV;
	uc_init_evcrvout (cptr,&evout);

	du = (i <= 1)? 2.: 1./(i-1); 

	for (j = 0,u = 0.; j < i && u < 1.0001; j++,u+=du)
	{
		um_ev3_circle(evflag,u,cptr,tfmat,&evout);
		uu_list_push (pptr,evout.cp);
		if (vptr != UU_NULL) uu_list_push (vptr,evout.dcdu);
		if (uptr != UU_NULL) 
		{
			uvp[0] = u;
			uu_list_push (uptr,uvp);
		}
	}

	return (i);
}
/*********************************************************************
**    E_FUNCTION: ncl_evolve_curve_own (eptr,tfmat, told, pptr, vptr,uptr)
**       Evolve curve (RB or NCL) into set of points with given
**       chordal tolerance.
**       NOTE: routine has same logic as ncl_evolve_crv_on_srf. 
**    PARAMETERS   
**       INPUT  : 
**          eptr   - pointer to curve record
**          tfmat  - ID matrix
**          told   - tolerance (chord higth)
**       OUTPUT :  
**          pptr  - pointer to points list 
**          vptr  - pointer to slope vectors list 
**          uptr  - pointer to u-parameters list
**    RETURNS      : 
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_curve_own (eptr,tfmat,told,pptr,vptr,uptr)
struct UM_crvdatabag *eptr;
UM_transf tfmat;
UU_REAL told;
UU_LIST *pptr, *vptr, *uptr;
{
	struct UM_rbsplcrv_rec *rcrv;
	int relnum;
	int i, size;

	size = 0;
	relnum = eptr->rel_num;
/*
	i = ncl_evolve_circ ((struct  UM_circle_rec *)eptr,tfmat,told,
			pptr,vptr,uptr);
*/
	if (relnum == UM_UVCVONSF_REL)
	{
		i = ncl_evolve_uvcv ((struct UM_uvcvonsf_rec *)eptr,tfmat,told,
			pptr,vptr,uptr);
	}
	else if (relnum == UM_LINE_REL)
	{
		ncl_evolve_line ((struct  UM_line_rec *)eptr,tfmat,pptr,vptr,uptr);
		i = 2;
	}
	else if (relnum == UM_CIRCLE_REL)
	{
		i = ncl_evolve_circ ((struct  UM_circle_rec *)eptr,tfmat,told,
			pptr,vptr,uptr);
	}
	else
	{
		if (relnum != UM_RBSPLCRV_REL)
		{
/*
...Map the curve to RB spline
*/
			size = um_curve_size (eptr);
			um_allocate_curve (&rcrv,size);
			if (um_rbcrv_frmnclcrv (eptr,rcrv) != UU_SUCCESS) return (0);
		}
		else
			rcrv = (struct UM_rbsplcrv_rec *) eptr;

		i = ncl_evolve_rbsp (rcrv,relnum,tfmat,told,pptr,vptr,uptr);
		if (size > 0) uu_toolfree (rcrv);
	}
	return (i);
}

/*********************************************************************
**    E_FUNCTION: ncl_evolve_curve (eptr,tfmat,told,pptr,vptr,uptr,itsk)
**       Evolve any curve into set of points with given
**       chordal tolerance.
**    PARAMETERS   
**       INPUT  : 
**          itsk   - flag for lines: 
**                   0 - evolve as 2 endpoints,
**                   1 - add two points with vectors near endpoints
**                   2 - add as in 1, plus some points inside (uniformly)
**          eptr   - pointer to curve record
**          tfmat  - ID matrix
**          told   - tolerance (chord height)
**       OUTPUT :  
**          pptr  - pointer to points list 
**          vptr  - pointer to slope vectors list 
**          uptr  - pointer to u-parameters list. Note: we assume this list
**                  is either null, or initialized (as list of UM_coord)
**    RETURNS      : 
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_curve (eptr,tfmat,told,pptr,vptr,uptr,itsk)
struct UM_crvdatabag *eptr;
UM_transf tfmat;
UU_REAL told;
UU_LIST *pptr, *vptr, *uptr;
int itsk;
{
	int npts;

	if ((MSLite==0)&&(ncl_itsa_compcrv(eptr) && ncl_own_geometry()))
		npts = ncl_evolve_composite_curve (eptr, told, pptr, vptr,uptr,itsk);
	else
	{
		if ((MSLite==1)||(ncl_own_geometry() &&
			((eptr->rel_num != UM_CONIC_REL && !ncl_itsa_uvcv_onsf(eptr)) ||
			  uptr == UU_NULL)))
			npts = ncl_evolve_curve_own (eptr,tfmat, told, pptr, vptr,uptr);
		else
			npts = ncl_evolve_curve_gen (eptr,tfmat, told, pptr, vptr,uptr);
		if (itsk > 0 && npts == 2)
		{
			UM_coord *pp,ppi,uv;
			UM_vector vvi,vvd;
			UU_REAL d,du;
			int i,n,np;

			np = pptr->cur_cnt - 2;
			pp = (UM_coord *) UU_LIST_ARRAY(pptr);
			um_vcmnvc (pp[np+1],pp[np],vvi);
			d = um_mag (vvi);
			du = 0.;
			if (itsk == 1 && d > 10.*told)
			{
				npts = 4;
				um_unitvc (vvi,vvi);
				um_vctmsc (vvi,3.*told,vvd);
				for (i = 0; i < 3; i++) ppi[i] = pp[np][i] + vvd[i];
				uu_list_insert (pptr,np+1,ppi);
				pp = (UM_coord *) UU_LIST_ARRAY(pptr);
				for (i = 0; i < 3; i++) ppi[i] = pp[np+2][i] - vvd[i];
				uu_list_insert (pptr,np+2,ppi);
				pp = (UM_coord *) UU_LIST_ARRAY(pptr);
				if  (vptr != UU_NULL)
				{
					uu_list_insert (vptr,np+1,vvi);
					uu_list_insert (vptr,np+2,vvi);
				}
				if (uptr != UU_NULL)
				{
					du = (3.*told)/d;
					uv[1] = uv[2] = 0.;
					uv[0] = du;
					uu_list_insert (uptr,np+1,uv);
					uv[0] = 1. - du;
					uu_list_insert (uptr,np+2,uv);
				}
			}
			if (d > 100.*told)
			{
				if (itsk == 1) 
				{
					np++;
					d -= 6.*told;
				}
				else 
					um_unitvc (vvi,vvi);
		
				n = (int) ceil (0.01*d/told);
				npts += n - 1;
				uv[1] = uv[2] = 0.;

				um_vctovc (pp[np],ppi);
				um_vctmsc (vvi,d/n,vvd);
				for (i = 1; i < n; i++)
				{
					um_vcplvc (ppi,vvd,ppi);
					uu_list_insert (pptr,np+i,ppi);
					if  (vptr != UU_NULL)
						uu_list_insert (vptr,np+i,vvi);
					if (uptr != UU_NULL)
					{
						uv[0] = du + (UU_REAL)i/n;
						uu_list_insert (uptr,np+i,uv);
					}
				}
			}
		}
	}
	return (npts);
}

/*********************************************************************
**    E_FUNCTION: ncl_evolve_all_curves (eptr,tfmat, told, pptr, vptr, itsk)
**       Evolve any curve into set of points with given
**       chordal tolerance.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to curve record
**          tfmat  - ID matrix
**          told   - tolerance (chord higth)
**          itsk   - UU_TRUE = return duplicate points of composite curve.
**       OUTPUT :
**          pptr  - pointer to points list
**          vptr  - pointer to slope vectors list, NULL for no vectors req'd.
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_all_curves (eptr, tfmat, told, pptr, vptr, itsk)
struct NCL_fixed_databag *eptr;
UM_transf tfmat;
UU_REAL told;
UU_LIST *pptr, *vptr;
UU_LOGICAL itsk;
{
	int i, status, n1, n2, ncv;
	UU_LOGICAL rev;
	struct NCL_fixed_databag e2;
	if ((MSLite==0)&&(ncl_itsa_compcrv(eptr)))
	{
		status = ncl_compcrv_getnents (eptr, &ncv);
		n2 = 0;
		for (i=0; i<ncv && status == UU_SUCCESS; i++)
		{
			status = ncl_compcrv_getelm (eptr, i, &e2, &rev);
			if (status == UU_SUCCESS)
			{
				n1 = ncl_evolve_curve (&e2,tfmat,told,pptr,vptr,UU_NULL,0);
				if (rev)
				{
					ncl_revers1_list (n1,n2,(UU_LIST_ARRAY(pptr)),1);
					if (vptr) ncl_revers1_list (n1,n2,(UU_LIST_ARRAY(vptr)),2);
				}
				n2 += n1;
				if (i < ncv - 1 && !itsk)
				{
					pptr->cur_cnt--;
					if (vptr) vptr->cur_cnt--;
					n2--;
				}
			}
		}
	}
	else
	{
		n2 = ncl_evolve_curve (eptr, tfmat, told, pptr, vptr,UU_NULL,0);
	}
	return(n2);
}
/*********************************************************************
**    E_FUNCTION   : int ncl_revsf_planes (pta, vca, spt, sa, ea, pl1, pl2)
**       Return the start and end planes of a surf of revolution.
**    PARAMETERS
**       INPUT  :
**          pta        - Rev surf rotation point.
**          vca        - Rev surf axis unit vector.
**          spt        - Start point of rotated entity.
**          sa         - Rev surf start angle.
**          ea         - Rev surf end angle.
**       OUTPUT :
**          pl1        - Plane thru axis point and start point.
**          pl2        - Plane thru axis point and end point.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_revsf_planes (pta, vca, spt, sa, ea, pl1, pl2)
UM_coord pta, spt;
UM_vector vca;
UM_angle sa, ea;
UU_REAL pl1[6], pl2[6];

{
	UM_transf rottf;
	UM_vector vtt,vpt;
	UM_angle  ang;

	um_vcmnvc (spt,pta,vpt);

	ang = sa / UM_RADIAN;
/*	um_rottf (vca, ang, rottf);*/
	um_rotlntf (pta,vca, ang, rottf);
	um_vctmtf (vpt, rottf, vtt);

	um_cross  (vca,vtt,&pl1[3]);
	um_unitvc (&pl1[3], &pl1[3]);
	um_vctovc (pta, pl1);

	ang = ea / UM_RADIAN;
/*	um_rottf (vca, ang, rottf);*/
	um_rotlntf (pta, vca, ang, rottf);
	um_vctmtf (vpt, rottf, vtt);

	um_cross  (vtt,vca,&pl2[3]);
	um_unitvc (&pl2[3], &pl2[3]);
	um_vctovc (pta, pl2);

	return (UU_SUCCESS);
}


/*********************************************************************
**    E_FUNCTION     : ncl_mcstowcs(option, mcs, wcs)
**         Convert a vector or cartesian  coordinate specified in the
**         modeling  coordinate system to a vector or cartesian  coordinate
**         specified relative to the current construction  coordinate
**         system.
**    PARAMETERS   
**       INPUT  : 
**            option         0 => cartesian  coordinate;
**                           1 => vector;
**            mcs            input (modeling coordinate system)
**       OUTPUT :  
**            ccs            output (construction  coordinate system)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mcstowcs(option, mcs, wcs)
   int option;
   UM_coord mcs;
   UM_coord wcs;

   {
   UM_real8 buf[3];
   UM_int2 i2opt;

   buf[0] = mcs[0];
   buf[1] = mcs[1];
   buf[2] = mcs[2];
   if (MSLite==0)
   {
		i2opt = option;
		mcswcs (&i2opt, buf);
   }
   wcs[0] = buf[0];
   wcs[1] = buf[1];
   wcs[2] = buf[2];
}


/*********************************************************************
**    E_FUNCTION     : ncl_cutter_same(cutr1,nc1,cutr2,nc2)
**			Determines if two cutter definitions are the same.
**    PARAMETERS   
**       INPUT  : 
**				cutr1   = 1st cutter definition to compare.
**				nc1     = Number of parameters in 'cutr1'.
**				cutr2   = 2nd cutter definition to compare.
**				nc2     = Number of parameters in 'cutr2'.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_TRUE if definitions are the same.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_cutter_same(cutr1,nc1,cutr2,nc2)
UU_REAL cutr1[],cutr2[];
int nc1,nc2;
{
	int i,nc;
	UU_LOGICAL same;
/*
.....Initialize routine
*/
	same = UU_TRUE;
	nc = nc1;
	if (nc2 > nc) nc = nc2;
	if (nc <= 0 || nc > 9) return(UU_FALSE);
/*
.....Compare cutter defintions
*/
	for (i=0;i<nc;i++)
	{
		if (fabs(cutr1[i]-cutr2[i]) > UM_FUZZ)
		{
			same = UU_FALSE;
			break;
		}
	}
	return(same);
}
void ncl_43mx_to_34mx(mxi,mxo)
UU_REAL mxi[4][3],mxo[3][4];
{
	int i,j;
	for (i=0;i<3;i++)
	{
		for (j=0;j<4;j++) mxo[i][j] = mxi[j][i];
	}
}


void ncl_34mx_to_43mx(mxi,mxo)
UU_REAL mxi[3][4],mxo[4][3];
{
	int i,j;
	for (i=0;i<3;i++)
	{
		for (j=0;j<4;j++) mxo[j][i] = mxi[i][j];
	}
}

void ncl_43mx_to_44mx(mxi,mxo)
UU_REAL mxi[4][3],mxo[4][4];
{
	int i,j;
	for (i=0;i<4;i++)
	{
		for (j=0;j<3;j++) mxo[i][j] = mxi[i][j];
	}
	mxo[0][3] = 0.; mxo[1][3] = 0.; mxo[2][3] = 0.; mxo[3][3] = 1.;
}
/*********************************************************************
**    E_FUNCTION     : int ncl_init_box (point, box)
**       Set the limit box to the first point.
**    PARAMETERS   
**       INPUT  : 
**          point    - Point to set limit box from.
**       OUTPUT :  
**          box      - Updated limit box.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_box (point, box)
UU_REAL point[3];
UM_real8 *box;
   {
   
   int i;

   for (i=0;i<3;i++) box[i+3] = box[i] = point[i];

   return 0;
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_update_box (point, box)
**       Update the limit box.
**    PARAMETERS   
**       INPUT  : 
**          point    - Point to set limit box from.
**       OUTPUT :  
**          box      - Updated limit box.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_box (point, box)
UU_REAL point[3];
UM_real8 box[6];
   {
   
   if (point[0] < box[0]) box[0] = point[0];
   if (point[1] < box[1]) box[1] = point[1];
   if (point[2] < box[2]) box[2] = point[2];
   if (point[0] > box[3]) box[3] = point[0];
   if (point[1] > box[4]) box[4] = point[1];
   if (point[2] > box[5]) box[5] = point[2];

   return 0;
   }

   /*********************************************************************
**    E_FUNCTION: ncl_get_uvcvonsf_sfkey (eptr)
**       Evolve uv-curves on surface into set of points with given
**       chordal tolerance.
**    PARAMETERS   
**       INPUT  : 
**          eptr   - pointer to curve record
**       OUTPUT :  
**			none
**    RETURNS      : 
**			the evolved curve surface key
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID ncl_get_uvcvonsf_sfkey (eptr)
struct UM_crvdatabag *eptr;
{
	int i,ncv,status,rev;
	struct NCL_fixed_databag crv;
	struct UM_uvcvonsf_rec *cv;

	if (ncl_itsa_compcrv(eptr))
	{
		status = ncl_compcrv_getnents (eptr, &ncv);
		for (i=0; i<ncv && status == UU_SUCCESS; i++)
		{
			status = ncl_compcrv_getelm (eptr,i,&crv,&rev);
			if (status ==UU_SUCCESS)
			{
				cv = (struct UM_uvcvonsf_rec *) &crv;
				if (cv)			
					return cv->bskey;
			}
		}
	}
	else
	{
		cv = (struct UM_uvcvonsf_rec *) eptr;
		if (cv)	
			return cv->bskey;
	}

	return 0;
}

/*********************************************************************
**    E_FUNCTION: ncl_get_uvcvonsf_transf(eptr,tran)
**       Evolve uv-curves on surface into set of points with given
**       chordal tolerance.
**    PARAMETERS   
**       INPUT  : 
**          eptr   - pointer to curve record
**       OUTPUT :  
**			none
**    RETURNS      : 
**			the evolved curve trimmed surface tranformation matrix
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_uvcvonsf_transf(eptr,tran)
struct UM_crvdatabag *eptr;
struct UM_transf_rec *tran;
{
	int i,ncv,status,rev;
	struct NCL_fixed_databag crv;
	struct UM_uvcvonsf_rec *cv;

	if (ncl_itsa_compcrv(eptr))
	{
		status = ncl_compcrv_getnents (eptr, &ncv);
		for (i=0; i<ncv && status == UU_SUCCESS; i++)
		{
			status = ncl_compcrv_getelm (eptr,i,&crv,&rev);
			if (status ==UU_SUCCESS)
			{
/* 
.....Get its transform
*/
				tran->key = crv.key;
				tran->rel_num = UM_TRANSFORM_REL;
				status = ur_retrieve_transf(tran);
				break;
			}
		}
	}
	else
	{
		tran->key = eptr->key;
		tran->rel_num = UM_TRANSFORM_REL;
		status = ur_retrieve_transf(tran);
	}

	return 0;
}
/*********************************************************************
**    E_FUNCTION     :  UU_LOGICAL ncl_itsa_uvcv_onsf(cv)
**    Tells if a curve is a uv-curve on surface
**    PARAMETERS
**       INPUT  :
**          cv  -  a curve
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_TRUE/UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_itsa_uvcv_onsf(cv)
struct NCL_fixed_databag *cv;
{
	UU_LOGICAL res;

	if (NCLX_internal_geom)
		res = UU_FALSE;
	else
		res = (cv->rel_num == UM_UVCVONSF_REL);

	return (res);
}
/*********************************************************************
**    E_FUNCTION     : ncl_sprintf(strng,elm,num)
**             Formats a string with a real number and eliminates
**             trailing zeroes.
**    PARAMETERS
**       INPUT  :
**          strng      A character string
**          elm        An array of reals to be formatted
**          num        The number of elements in elm
**          
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_sprintf(strng,elm,num)
char strng[];
UU_REAL elm[];
int num;
{

	int len,i,j,status;
	char buf[80],buf2[80];

	status = UU_FAILURE;
	strng[0]='\0';
/*
.....First search for a decimal point, if we don't find one then we
.....don't want to remove any zeroes.
*/
	if (NCL_accuracy ==0)
	{
		for (j=0;j<num;j++)
		{
			sprintf(buf2,"%f",elm[j]);
			len = strlen(buf2);
			i =0;
			while(i<len && status == UU_FAILURE)
			{
				if (buf2[i] == '.')
					status = UU_SUCCESS;
				i++;
			}
			if (status == UU_SUCCESS)
			{
/*
.....Starting at the end of the string, if we find a zero
.....null it out, otherwise we are done.
*/
				i = len - 1;
				while(i>0 && status == UU_SUCCESS)
				{
/*
.....Leave the 0 if it is right next to the decimal point;
....."1.0" looks a lot better than "1." or just "1".
*/
					if (buf2[i] == '0' && buf2[i-1]!='.')
						buf2[i] = '\0';
					else
						status = UU_FAILURE;
					i--;
				}
			}
			strcat(strng,buf2);
			if (j!=(num-1))
				strcat(strng,",");
		}
	}
	else
	{
		sprintf(buf,"%%.%df",NCL_accuracy);
		for (j=0;j<num;j++)
		{
			sprintf(buf2,buf,elm[j]);
			strcat(strng,buf2);
			if(j != (num-1))
				strcat(strng,",");
		}
	}
		

	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sprintf2(strng,elm,num, precise)
**        Formats a string with a real number use different precise
**    PARAMETERS
**       INPUT  :
**          strng      A character string
**          elm        An array of reals to be formatted
**          num        The number of elements in elm
**          precise:	-1: default precise = 12, remove trailing 0s 
**						>=0 use as precise for sprintf
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_sprintf2(strng,elm,num, precise)
char strng[];
UU_REAL elm[];
int num, precise;
{

	int len,i,j,status;
	char buf[80],buf2[80];

	status = UU_FAILURE;
	strng[0]='\0';
/*
.....First search for a decimal point, if we don't find one then we
.....don't want to remove any zeroes.
*/
	if (precise==-1)
	{
		for (j=0;j<num;j++)
		{
			sprintf(buf2,"%.8f",elm[j]);
			len = strlen(buf2);
			i =0;
			while(i<len && status == UU_FAILURE)
			{
				if (buf2[i] == '.')
					status = UU_SUCCESS;
				i++;
			}
			if (status == UU_SUCCESS)
			{
/*
.....Starting at the end of the string, if we find a zero
.....null it out, otherwise we are done.
*/
				i = len - 1;
				while(i>0 && status == UU_SUCCESS)
				{
/*
.....Leave the 0 if it is right next to the decimal point;
....."1.0" looks a lot better than "1." or just "1".
*/
					if (buf2[i] == '0' && buf2[i-1]!='.')
						buf2[i] = '\0';
					else
						status = UU_FAILURE;
					i--;
				}
			}
			strcat(strng,buf2);
			if (j!=(num-1))
				strcat(strng,",");
		}
	}
	else
	{
		sprintf(buf,"%%.%df",precise);
		for (j=0;j<num;j++)
		{
			sprintf(buf2,buf,elm[j]);
			strcat(strng,buf2);
			if(j != (num-1))
				strcat(strng,",");
		}
	}
	return(UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_invert_matrix(mxin,mxout)
**			Inverts an NCLCAM matrix.
**    PARAMETERS   
**       INPUT  : 
**          mxin    = Matrix to invert.
**       OUTPUT :  
**          mxout   = Inverted matrix.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_invert_matrix(mxin,mxout)
UM_real8 mxin[],mxout[];
{
	int i,j;
	UM_real8 sfx,sfy,sfz,sm;
	sfx = mxin[0]*mxin[0] + mxin[4]*mxin[4] + mxin[8]*mxin[8];
	sfy = mxin[1]*mxin[1] + mxin[5]*mxin[5] + mxin[9]*mxin[9];
	sfz = mxin[2]*mxin[2] + mxin[6]*mxin[6] + mxin[10]*mxin[10];
	sm = .0001;
	if (sfx > sm && sfy > sm && sfz > sm)
	{
		for (i=0;i<3;i++)
		{
			j = 4 * i;
			mxout[i] = mxin[j] / sfx;
			mxout[i+4] = mxin[j+1] / sfy;
			mxout[i+8] = mxin[j+2] / sfz;
		}
		for (j=3;j<12;j=j+4)
		{
			i = j - 3;
			mxout[j] = -mxout[i]*mxin[3] - mxout[i+1]*mxin[7] -
				mxout[i+2]*mxin[11];
		}
	}
	else
	{
		mxout[0] = 1.; mxout[1] = 0.; mxout[2] = 0.; mxout[3] = 0.;
		mxout[4] = 0.; mxout[5] = 1.; mxout[6] = 0.; mxout[7] = 0.;
		mxout[8] = 0.; mxout[9] = 0.; mxout[10] = 1.; mxout[11] = 0.;
	}
	return(UU_SUCCESS);
}

/********************************************************************
**    E_FUNCTION: ncl_h_cnpts (ptsv, pt, pts, nmp, indx)
**       Checks list of 'nmp' points 'pts' for maximum distance from
**       line defined by points 'ptsv' & 'pt'.  If checked point is
**       outside segment than distance from closest end point is used.
**    PARAMETERS
**       INPUT  :
**          ptsv   - start point of CV arc
**          pt     - end point of CV arc
**          pts    - array of intermediate points on arc
**          nmp    - number of points in array
**       OUTPUT :
**          indx   - index of the point in 'pts' array with maximum
**							deviation.
**    RETURNS      :
**          chord value
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
UU_REAL ncl_h_cnpts (ptsv,pt,pts,nmp,indx)
UM_coord ptsv, pt, *pts;
int nmp, *indx;
{
	int i;
	UU_REAL d, dm, dis, p;
	UM_coord ppt;
	UM_vector vec, uvc;
	UU_REAL um_vcdir();

	um_vcmnvc (pt,ptsv,vec);
	um_unitvc (vec,uvc);
	dis = um_mag (vec);

	*indx = nmp - 1;
	dm = 0.;
	for (i=0; i<nmp; i++)
	{
		um_nptln (pts[i],ptsv,uvc,ppt);
		p = um_vcdir (ppt,ptsv,uvc);
		if (p < 0.)
			d = um_dcccc (pts[i],ptsv);
		else if (p > dis)
			d = um_dcccc (pt,pts[i]);
		else
			d = um_dcccc (ppt,pts[i]);
		if (d > dm) {  dm = d;  *indx = i;  }
	}

	return (dm);
}
/*********************************************************************
**    E_FUNCTION     : ncl_save_clrmod
**       Save the custom color into modals file.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
int ncl_save_clrmod()
{
	int i,stat;
	char msg[80];
	UX_pathname fname;
	FILE *fptr;
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
	if (UW_Store_color==0)
	{
		NCL_clrs_changed = 1;
		return stat;
	}
	NCL_clrs_changed = 0;
/*
.....Open modals file
*/
	if (LW_nclipv!=LW_STANDALONE)
		strcpy(fname,"ncl_color.mod");
	else
		strcpy(fname,"nclipv_color.mod");

	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,fname,
		3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;
/*
.....Store color modals
*/	
	ux_fputs0("#COLOR#\n", fptr);
	for (i=0; i<48; i++)
	{
		sprintf(msg,"/NAME/ %s\n", uw_color_name[16+i]);
		ux_fputs0(msg, fptr);
		sprintf(msg,"/RGB/ %d, %d, %d\n", uw_color_table[16+i][0], 
					uw_color_table[16+i][1], uw_color_table[16+i][2]);
		ux_fputs0(msg, fptr);
	}
/*
.....Close modals file
*/
	ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
	return(stat);
}

