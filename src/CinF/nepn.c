/*********************************************************************
**    NAME         :  nepn.c
**       CONTAINS: routines to handle NCL paterns
**			drwpn(nclkey)
**			ncl_patern_to_nclpatern(&e, buf);
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nepn.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:40
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "ginq.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mplot.h"
#include "ncl.h"
#include "nccs.h"
#include "nclfc.h"
#include "mdeval.h"
#include "msrf.h"
#include "modef.h"

#if (UU_COMP == UU_SUN) || (UU_COMP == UU_IRIS4D) || (UU_COMP == UU_DECUNIX)
#ifndef UU_RS6000
#define dstptv dstptv_
#endif
#endif 

/*********************************************************************
**    E_FUNCTION     : drwpn(nclkey, tranf)
**       Display a patern in the current open segment.
**    PARAMETERS   
**       INPUT  : 
**            nclkey            UNIBASE key of patern entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added parameter tranf
.....allowed pass in tranformation
.....which added in default tranformation
.....Yurong 9/30/97
*/
drwpn (nclkey, npts, ntyp, tranf)
   UU_KEY_ID *nclkey;
   UU_KEY_ID *npts, *ntyp;
   UM_transf tranf;

   {
   struct NCL_patern_rec patern;
   UU_REAL pntrec[6];
   UM_transf tfmat;
   struct NCL_nclattr_rec attr;
   UM_coord pt;
   UM_coord ptout;
   Gwpoint3 gpt, gvc;			/* markers to send to GKS */
   int markertype;			/* type of DIGGS marker */
   int status;
   int numpts;
   int i,m;

   uu_denter(UU_MTRC,(us,"drwpn(key:%d)", *nclkey));

   patern.key = *nclkey;
   status = ur_retrieve_data_fixed(&patern);
   if (status == UU_SUCCESS) uc_retrieve_transf(patern.key, tfmat);
   if (status == UU_SUCCESS) uc_retrieve_attr(patern.key, &attr);
   if (status == UU_SUCCESS)
      {
      markertype = gqmarktype();	/* get current marker type */
/*
.....Added check to see if we are plotting
.....In which case we will use the logical pen
.....for the current color
.....Bobby  -  11/19/91
*/
		if (UM_plotting == UU_TRUE)
		{
			gsmarkcolor(attr.pen);
		}
		else
		{
			gsmarkcolor(attr.color);
		}
      gsmarktype(patern.markertype);
      m   = 3 * patern.pntype;
      *npts = numpts = patern.no_patpnt / m;
      *ntyp = patern.pntype;
      for (i=1; i<=numpts; i++)
         {
         if (ur_retrieve_data_varlist(patern.key, 1, pntrec, (i-1)*m+1, m) != 0)
            status = UU_FAILURE;
         else
            {
/*
.....Set secondary pick id
*/
				gspickid(i);
/*
...get point in patern and draw marker 
*/
            um_cctmtf(pntrec, tfmat, pt);
/*
.....added by Yurong
.....9/30/97
*/
            um_cctmtf(pt, tranf, ptout);
            gpt.x = ptout[0];
            gpt.y = ptout[1];
            gpt.z = ptout[2];
            if (patern.pntype != 2)
              {
               gpolymarker3(1,&gpt);
              }
            else
/*
...vp3.25.93 get vector if type 2 patern (PVs) and draw PV mark 
*/
              {
               um_vctmtf(&pntrec[3], tfmat, pt);
/*
.....added by Yurong
.....9/30/97
*/
	            um_cctmtf(pt, tranf, ptout);
               gvc.x = ptout[0];
               gvc.y = ptout[1];
               gvc.z = ptout[2];
               dstptv (&gpt, &gvc, tfmat);
              }      
            }
         }
      gsmarktype(markertype);	/* reset DIGGS marker type */
      }
   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_patern_to_nclpatern(e, ncl)
**       Convert a UNICAD point to an NCL  point.
**    PARAMETERS   
**       INPUT  : 
**          e						UNICAD point entity
**       OUTPUT :  
**          ncl					buffer to place NCL point
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_patern_to_nclpatern(e, ncl)
struct NCL_patern_rec *e;
/* struct NCLI_point_rec *ncl;i */
struct NCLI_pointvec_rec *ncl;  /* vp3.26.93 PV in patern instead of PT */
	{
	int status;
	int pntyp, m, k;
/*	struct NCL_patpnt_rec *p;*/
 UU_REAL p[6];
	static UM_int2 point = 0;

	uu_denter(UU_MTRC,(us,"ncl_patern_to_nclpatern(key=%x, ncl=%x)",
		e->key, ncl));

	 status = UU_SUCCESS;

	if (!point)
  {
	  status = ncl_retrieve_data(e, sizeof(struct NCL_fixed_databag));
	 	pntyp = e->pntype;
      m     = pntyp * 3;
	 	point = e->no_patpnt / m;
  }

	if (status == UU_SUCCESS)
		{
		if (point)
			{
			/** get num points, pick one and get coords of that one **/
          status = gtpnpt (p, &pntyp, &e->key, &point);
			 point--; 

			 ncl_uureal_to_real8(3, p, ncl->pt);
          if (pntyp == 2) ncl_uureal_to_real8(3, &p[3], ncl->ve);

			}
		}

	uu_dexit;
	return (status);
	}

