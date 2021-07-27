/*********************************************************************
**    NAME         :  m4isect1.c
**       CONTAINS:  Routines used to intersect extended curves.
**       int um_isect_rbspl_ext (elist,tlist,plane,nintp,no_ibuf,ibuf)
**       int um_get_orignlt (eptr,t0,t1)
**       int um_trim_set (ptr,t0,t1)
**       int um_trim_set (ptr,t0,t1)
**    COPYRIGHT 1995 (C) INTERSOFT Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m4isect1.c , 25.1
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

/*********************************************************************
** E_FUNCTION :  um_isect_rbspl_ext (elist, tlist, plane, nintp, 
**                                   no_ibuf, ibuf)
**       Intersects two RB curves including extension tangent to the 
**       end point for each curve.  Fills in user supplied buffer of 
**       UM_isect records.  Curves must be coplanar for success.
**    PARAMETERS   
**       INPUT  : 
**          elist   - pointers to alleged curves
**				tlist   - transformations for each to some common
**											model space.  UM_DEFAULT_TF is OK
**				nintp   - pointer to integer output 
**				          number of intersections allocated in ibuf
**				no_ibuf - max number of intersections allowed in ibuf
**				ibuf    - pointer to UM_isect buffers to hold output
**       OUTPUT :  
**          *nintp  - number of intersections found
**				ibuf    - list of UM_isect entries filled in
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
int
  um_isect_rbspl_ext (elist, tlist, plane, nintp, no_ibuf, ibuf)
  struct UM_rbsplcrv_rec   *elist[2];
  UU_REAL *tlist[2];
  UU_REAL plane[2][3]; /* plane of curves   */
  int *nintp, no_ibuf;
  UM_isect  ibuf[];
  {
   UM_isect exbuf[50];
   int nxntp, n1, i, j, is1, status, trmfl[2];
   struct UM_rbsplcrv_rec *nelst[2];
   struct UM_line_rec lnrc[4];
   UU_REAL t00,t01,t[2],dumm, Ufuzz = -1.e-6;
   UU_REAL *ntlst[2],trmt0[2],trmt1[2],d[2],loff[2],roff[2];
   struct UM_evcrvout ev0;
   UM_vector vec;

   lnrc[0].key = lnrc[1].key = 0;
   lnrc[0].rel_num = lnrc[1].rel_num = UM_LINE_REL;
   lnrc[2].rel_num = lnrc[3].rel_num = UM_LINE_REL;
/*
...Get parametrization range for curve to be extended
*/
   for (i=0; i<2; i++)
     {
      loff[i] = roff[i] = 0;
      d[i] = t[i] = 1.0;
      trmfl[i] = 0;
      if (elist[i]->rel_num == UM_RBSPLCRV_REL)
        {
         status = um_utot (elist[i],(UU_REAL) 0., &t00) + 
                  um_utot (elist[i],(UU_REAL) 1., &t01);
         if (status != UU_SUCCESS) goto Done;
         d[i]     = t01 - t00;
         loff[i] = t00;
         um_get_orignlt (elist[i],&dumm,&t[i]);
         roff[i] = t01 - t[i]; 
        }
     }
/*
...Intersect RBspl extension with the second curve
*/
   for (i=0; i<2; i++)
     {
      is1 = 1 - i;
      if (elist[i]->rel_num == UM_RBSPLCRV_REL && 
          elist[i]->closdinu != 1)
/*
...build line at the start point of RB spline (left extension)
...end intersect it with second curve. 
...Note:  lnrc[i] contains the following lines: 
...0 - left ext. for CV0, 2 - right ext. for CV0, 
...1 - left ext. for CV1, 3 - rigth ext. for CV1
*/
        {
         trmfl[i] = um_trim_reset (elist[i],&trmt0[i],&trmt1[i]);
         lnrc[i].key = lnrc[i+2].key = elist[i]->key;
         uc_init_evcrvout (elist[i],&ev0);
         um_ev7_rbsplcrv(UM_FRSTDERIV,(UU_REAL).0,elist[i],tlist[i], &ev0); 
         um_vctovc (ev0.cp,lnrc[i].spt); 
         um_unitvc (ev0.dcdu,vec);
         um_vcmnvc (lnrc[i].spt,vec,lnrc[i].ept);
         nelst[0] = (struct UM_rbsplcrv_rec *) &lnrc[i];
         nelst[1] = elist[is1]; 
         ntlst[0] = tlist[i];
         ntlst[1] = tlist[is1];
         if (nelst[1]->rel_num == UM_RBSPLCRV_REL)
            status = um_crv_isect_ext (nelst,ntlst,plane,&nxntp,no_ibuf,
                                    exbuf);
         else
            status = um_isect_sp (nelst[0],ntlst[0],nelst[1],ntlst[1],
                         plane,&nxntp,no_ibuf,exbuf);
/*
...build line at the end point of RB spline (right extension)
...end intersect it with second curve
*/
         um_ev7_rbsplcrv(UM_FRSTDERIV,(UU_REAL)1.0,elist[i],tlist[i], &ev0); 
         um_vctovc (ev0.cp,lnrc[i+2].spt); 
         um_unitvc (ev0.dcdu,vec);
         um_vcplvc (lnrc[i+2].spt,vec,lnrc[i+2].ept);
         nelst[0] = (struct UM_rbsplcrv_rec *) &lnrc[i+2];
         if (nelst[1]->rel_num == UM_RBSPLCRV_REL)
            status = um_crv_isect_ext (nelst,ntlst,plane,&n1,no_ibuf,
                                    &exbuf[nxntp]);
         else
            status = um_isect_sp (nelst[0],ntlst[0],nelst[1],ntlst[1],
                         plane,&n1,no_ibuf,&exbuf[nxntp]);
/*
...add io points to output buffer if they are outside of
...the extended curve, and adjust parameter to reflect
...the original curve parametrization
*/
         n1 += nxntp;
         for (j=0; j<n1; j++)
            {
             UU_REAL uu[2];
             if (exbuf[j].u0 > Ufuzz)
                {
                 ibuf[*nintp] = exbuf[j];
                 uu[is1] = (exbuf[j].u1*t[is1] - loff[is1]) / d[is1];
                 if (j < nxntp)
                    uu[i] = - (exbuf[j].u0 + loff[i]) / d[i];
                 else
                    uu[i] = 1. + (exbuf[j].u0 - roff[i]) / d[i];
                 ibuf[*nintp].u0 = uu[0];
                 ibuf[*nintp].u1 = uu[1];
                 *nintp += 1;
                 if (*nintp >= no_ibuf) goto Nospc;
                } 
            }
        }
     }
/*
...intersect extension lines if both curves are RB splines
*/
   for (i=0; i<2; i++)
     {
      if (lnrc[i].key != 0)
        {
         is1 = 1 - i;
         if (lnrc[is1].key != 0)
          {
/*
...Left (Right) extension of CV0 with Left extension of CV1
*/
           um_isect_sp (&lnrc[2*i],tlist[0],&lnrc[1],tlist[1],plane,&n1,
                             1,exbuf);
           if (n1 > 0 && exbuf[0].u0 > Ufuzz && exbuf[0].u1 > Ufuzz)
              {
               ibuf[*nintp] = exbuf[0];
               if (i == 0)
                  ibuf[*nintp].u0 = - (exbuf[0].u0 + loff[0]) / d[0];
               else
                  ibuf[*nintp].u0 = 1. + (exbuf[0].u0 - roff[0]) / d[0];
               ibuf[*nintp].u1 = - (exbuf[0].u1 + loff[1]) / d[1];
               *nintp += 1;
               if (*nintp >= no_ibuf) goto Nospc;
              }
/*
...Left (Right) extension of CV0 with Right extension of CV1
*/
           um_isect_sp (&lnrc[2*i],tlist[0],&lnrc[3],tlist[1],plane,&n1,
                           1,exbuf);
           if (n1 > 0 && exbuf[0].u0 > Ufuzz && exbuf[0].u1 > Ufuzz)
              {
               ibuf[*nintp] = exbuf[0];
               if (i == 0)
                  ibuf[*nintp].u0 = - (exbuf[0].u0 + loff[0]) / d[0];
               else
                  ibuf[*nintp].u0 = 1. + (exbuf[0].u0 - roff[0]) / d[0];
               ibuf[*nintp].u1 = 1. + (exbuf[0].u1 - roff[1]) / d[1];
               *nintp += 1;
               if (*nintp >= no_ibuf) goto Nospc;
              }
          }  
        }  
     }

Nospc:
   if (trmfl[0] != 0) um_trim_set (elist[0],trmt0[0],trmt1[0]);
   if (trmfl[1] != 0) um_trim_set (elist[1],trmt0[1],trmt1[1]);
Done:
   uu_dexit;
   return(status);
  }
/*********************************************************************
**    E_FUNCTION :  um_get_orignlt (eptr,t0,t1)
**       Get original phisical parameters of the curve.
**    PARAMETERS
**       INPUT  :
**          eptr        - pointer to curve entity
**       OUTPUT :
**          t0,t1       - original parameters of the curve.
**    RETURNS      : 0 if curve was not trimmed, 1 or 2 if trimmed.
**    SIDE EFFECTS : none
**    WARNINGS     :
*************************************************************************/
int
  um_get_orignlt (eptr,t0,t1)
  struct UM_rbsplcrv_rec *eptr;
  UU_REAL *t0,*t1;
  {
   *t0 = eptr->t[0];
   *t1 = eptr->t[eptr->no_t-1];

   uu_dexit;
   return(UU_SUCCESS);
  }
/*********************************************************************
**    E_FUNCTION :  um_trim_reset(eptr, t0, t1)
**			Checks if curve has been trimmed and restores the phisical
**       parameter limits if so.
**    PARAMETERS   
**       INPUT  : 
**          eptr			- pointer to curve entity
**       OUTPUT :  
**          t0,t1       - original parameters of the curve.
**    RETURNS      : 0 if curve was not trimmed, 1 or 2 if trimmed.
**    SIDE EFFECTS : none
**    WARNINGS     :
*************************************************************************/
int
   um_trim_reset (ptr,t0,t1)
   struct UM_rbsplcrv_rec *ptr;
   UM_param *t0, *t1;
  {
   int flag;

   flag  = 2;
   
   switch (ptr->rel_num)
     {
      case UM_RBSPLCRV_REL:
        {
         flag = um_set_trimflg (&ptr->t0, t0, ptr->t[0]) +
                um_set_trimflg (&ptr->t1, t1, ptr->t[ptr->no_t-1]);
        }
        break;
      case NCL_CURVE_REL:
        {
         struct NCL_curve_rec *cptr;

         cptr = (struct NCL_curve_rec *) ptr;
         flag = um_set_trimflg (&cptr->t0, t0, cptr->param[0]) +
                um_set_trimflg (&cptr->t1, t1, cptr->param[cptr->no_param-1]);
        }
        break;
      default:
        flag = 0;
     }

   return(flag);
  }
int um_set_trimflg (tin,tout,tset)
  UU_REAL *tin, *tout, tset;
  {
   int flag = 1;
   *tout = tset;
   if (*tin != *tout)
      {
       *tout = *tin;
       *tin  = tset;
      }
   else
      flag = 0;
   return (flag);
  } 
/************************************************************************
**    E_FUNCTION :  um_trim_set (ptr, t0, t1)
**			Set the phisical parameters U of the curve, so the curve 
**       is trimmed or can be reset back to its original valus.
**    PARAMETERS   
**       INPUT  : 
**          eptr			- pointer to curve entity
**          t0,t1       - new phisical parametrs to apply (trim the curve)
**       OUTPUT :  
**							none
**    RETURNS      : 0 if OK, UM_FAILURE if inversion screws up,
**							UM_UNIMPLEMENTED
**    SIDE EFFECTS : none
**    WARNINGS     :
*************************************************************************/
int
   um_trim_set (ptr,t0,t1) 
   struct UM_rbsplcrv_rec *ptr;
   UM_param t0, t1;
  {
   int status = UU_SUCCESS;
 
   switch (ptr->rel_num)
     {
      case UM_RBSPLCRV_REL:
        {
         ptr->t0 = t0;
         ptr->t1 = t1;
        }
        break;
      case NCL_CURVE_REL:
        {
         struct NCL_curve_rec *cptr;
         cptr = (struct NCL_curve_rec *) ptr;
         cptr->t0 = t0;
         cptr->t1 = t1;
        }
        break;
      default:
        status = UU_FAILURE;
     }
  
   return (UU_SUCCESS);
  }
