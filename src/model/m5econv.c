/*********************************************************************
**    NAME         :  m5econv.c
**       CONTAINS: routines to convert NCL curves to UNICAD curves
**         int um_rbcrv_frmnclcrv(lptr,rptr)
**         int um_cctou_rbcrv (eptr, tfmat, dpt, u, distp)
**         void um_pv_to_line(pptr,lptr)
**		   void um_pl_to_line()
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m5econv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:05
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdebug.h"
#include "mdeval.h"
#include "nccs.h"
#include "nclvx.h"

/*********************************************************************
**    E_FUNCTION     : int um_rbcrv_frmnclcrv (lptr,rptr)
**       Convert an NCL based curve into its equivalent rational
**       bspline form.
**    PARAMETERS   
**       INPUT  : 
**            lptr            input entity
**       OUTPUT :  
**            rptr            equivalent rational bspline
**    RETURNS      : 
**            UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_rbcrv_frmnclcrv(lptr,rptr)
   struct UM_crvdatabag *lptr;
   struct UM_rbsplcrv_rec  *rptr;
  {
   int status;
   
   switch (lptr->rel_num)
     {
      case UM_LINE_REL:
         status = um_c7_frmline(lptr,rptr);
         break;
      case UM_CIRCLE_REL:
         status = um_c7_frmcirc(lptr,rptr);
         break;
      case UM_CONIC_REL:
         status = um_c7_frmconic(lptr,rptr);
         break;
      case NCL_CURVE_REL:
         status = um_c7_frmnclcrv(lptr,rptr);
         break;
/*
.....Adding polylines.  JLS 10/19/99
*/
		case UM_POLYLINE_REL:
			status = um_c7_frmpoly(lptr,rptr);
			break;
      default:
         status = UU_FAILURE;
    }
   if (status == UU_SUCCESS) rptr->key = lptr->key;

   return (status);
  }

/*********************************************************************
**    E_FUNCTION     : int um_cctou_rbcrv (eptr, tfmat, dpt, u, distp)
**
**    PARAMETERS   
**       INPUT  : 
**            lptr            input entity
**       OUTPUT :  
**            rptr            equivalent rational bspline
**    RETURNS      : 
**            UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cctou_rbcrv (eptr, tfmat, dpt, u, distp)
  struct UM_rbsplcrv_rec *eptr;
  UM_transf tfmat;
  UM_coord    dpt;
  UU_REAL     *u;
  UU_REAL     *distp; 
  {
   struct UM_evcrvout evcrv;
   UM_coord pt, ptit, *crvpts;
   int status, npts, i, next, closest,ntim;
   UM_vector vec, vfwd;
   UU_REAL dis, delta, us, uu, uo, dd, spn, r, ro;
	UU_REAL duu, save_duu, save_uu, save_dis;
	UU_LOGICAL closed;

   status = UU_SUCCESS;

   uc_init_evcrvout (eptr,&evcrv);
   npts = eptr->no_pt;
 
   crvpts = (UM_coord *) uu_toolmalloc (npts * sizeof(UM_coord));
   delta = 1.0/(npts-1);
   
   for (i=0, uu=0.0; i<npts; i++, uu=uu+delta)
      {
       uc_evcrv(UM_FRSTDERIV, uu, eptr, UM_DEFAULT_TF, &evcrv);
       um_vctovc(evcrv.cp, crvpts[i]);
      }
	closed = (um_dcccc(crvpts[0],crvpts[npts-1]) < UM_FUZZ);
   closest = um_ccnearcc(dpt, npts, crvpts);
   uu      = delta * closest;
   dis     = um_dcccc (dpt,crvpts[closest]);
   if (dis < .1*UM_FUZZ) goto done; 
/*
...check where it is (FWD or BCK)
*/
   next = closest + 1; 
   if (next == npts)
   {
      next = closest;
      closest = next - 1;
      um_vcmnvc (dpt,crvpts[closest],vec);
      um_vcmnvc (crvpts[next],crvpts[closest],vfwd);
/*
.....Adjust closest if curve is closed and wrong end point
.....was used - ASF 7/2/13.
*/
      if (closed && um_dot (vec,vfwd) > 0.0)
		{
			closest = 0;
			next = 1;
			uu = 0.;
		}
   } 
   else if (closest != 0)
   {
      um_vcmnvc (dpt,crvpts[closest],vec);
      um_vcmnvc (crvpts[next],crvpts[closest],vfwd);
      if (um_dot (vec,vfwd) < 0.0)
      {
          next = closest;
          closest = next - 1;
      }
   }  
   um_vcmnvc (crvpts[next],crvpts[closest],vfwd);
/*
.....Different machines handle really small numbers, so if
.....any entry of vfwd is extremely small, set it equal to 0.
.....JLS 5/20/99
*/
	for (i=0;i<3;i++)
	{
		if(fabs(vfwd[i])<UM_DFUZZ)
		{
			vfwd[i] = 0.0;
		}
	}

   uo = us = delta * closest;
   ro = 10.0; 
	ntim = 0;
	save_duu = 10000.;
/*
...Iterate in this u range
*/
iter:;
   spn  = um_mag (vfwd);

   if (spn < .1*UM_DFUZZ) goto done;

   um_unitvc (vfwd,vfwd);
   um_nptln (dpt,crvpts[closest],vfwd,pt);
   um_vcmnvc (pt,crvpts[closest],vec);
   r    = um_mag(vec) / spn;
   dd   = (um_dot(vec,vfwd) > 0.)? r: -r;
   uu   = us + dd * delta;      
   if (uu < 0.0) uu = 0.0;
   if (uu > 1.0) uu = 1.0;
      
   uc_evcrv(UM_FRSTDERIV, uu, eptr, UM_DEFAULT_TF, &evcrv);
   um_vctovc(evcrv.cp, ptit);
   dis  = um_dcccc(ptit,dpt);

   if (dis < .1*UM_FUZZ) goto done;
	duu = fabs(uu-uo);
   if (duu < 1.e-8) goto done;
   if (fabs(ro-1.0/r) < UM_DFUZZ)
     {
      uu = .5*(uu+uo);
      uc_evcrv(UM_FRSTDERIV, uu, eptr, UM_DEFAULT_TF, &evcrv);
      um_vctovc(evcrv.cp, ptit);
      dis  = um_dcccc(ptit,dpt);
      goto done;
     }
/*
.....Break out if close and already looped 25 times
*/
	if ((ntim > 25 && save_duu < 1.e-6) || ntim > 1000)
	{
		dis = save_dis;
		uu = save_uu;
		goto done;
	}

   um_vcmnvc (ptit,crvpts[closest],vfwd);
   delta = fabs(us - uu);
   uo    = uu;
   ro    = r;
/*
.....Save closest point
*/
	if (duu < save_duu)
	{
		save_dis = dis;
		save_duu = duu;
		save_uu = uu;
	}
	ntim++;
   goto iter; 
/* 
...got point 
*/

done:;
   *u     = uu;
   *distp = dis;
   uu_toolfree (crvpts);

   return (status);
  }

/*********************************************************************
**    E_FUNCTION     : um_pv_to_line (pptr,lptr,iflg)
**       Converts a PointVector to a line.
**    PARAMETERS   
**       INPUT  : 
**            pptr            Pointer to PointVector structure
**            iflg            UU_TRUE:  make line "infinitely" long
**                            UU_FALSE: Use vector magnitude for length
**       OUTPUT :  
**            lptr            Pointer to line structure
**    RETURNS      : 
**            UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_pv_to_line(pptr,lptr,iflg)
struct NCL_nclpv_rec *pptr;
struct UM_line_rec *lptr;
struct UU_logical *iflg;
{
	int i;
	UU_REAL addmag = (iflg)? 10000. : 1.;
	struct UM_line_rec lin;
/*
.....Convert to line
*/
	lin.key = pptr->key;
	lin.rel_num = UM_LINE_REL;
	strncpy(lin.label,pptr->label, NCL_MAX_LABEL);
	lin.subscr = pptr->subscr;
	for (i=0;i<3;i++)
	{
		lin.labloc[i] = pptr->labloc[i];
		lin.spt[i] = pptr->pt[i];
		lin.ept[i] = pptr->pt[i] + addmag*pptr->ve[i];
	}
/*
.....Copy local storage to line
.....Just in case 'pptr' and 'lptr' are the same
*/
	uu_move_byte(&lin,lptr,sizeof(lin));
}

/*********************************************************************
**    E_FUNCTION     : um_pl_to_line (pptr,buff,lptr)
**       Converts a pl to a line.
**    PARAMETERS   
**       INPUT  : 
**            pptr            Pointer to Plane structure
**			  buff			  Plane point and normal vector
**       OUTPUT :  
**            lptr            Pointer to line structure
**    RETURNS      : 
**            UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_pl_to_line(pptr,buff,lptr)
struct NCL_nclpl_rec *pptr;
UU_REAL buff[];
struct UM_line_rec *lptr;
{
	int i;
	struct UM_line_rec lin;
	UM_coord pt;
	UM_vector vnorm;

	pt[0] = buff[0];
	pt[1] = buff[1];
	pt[2] = buff[2];
	vnorm[0] = buff[3];
	vnorm[1] = buff[4];
	vnorm[2] = buff[5];

/*
.....Project plane to curve plane
*/
	um_cross(pptr->nvec, vnorm, pptr->nvec);
	um_nptpln(pptr->pt, pt, vnorm, pptr->pt);

/*
.....Convert to line
*/
	lin.key = pptr->key;
	lin.rel_num = UM_LINE_REL;
	strncpy(lin.label,pptr->label, NCL_MAX_LABEL);
	lin.subscr = pptr->subscr;
	for (i=0;i<3;i++)
	{
		lin.labloc[i] = pptr->labloc[i];
		lin.spt[i] = pptr->pt[i] - 100. * pptr->nvec[i];
		lin.ept[i] = pptr->pt[i] + 100. * pptr->nvec[i];
	}
/*
.....Copy local storage to line
.....Just in case 'pptr' and 'lptr' are the same
*/
	uu_move_byte(&lin,lptr,sizeof(lin));
}
