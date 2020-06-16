/*********************************************************************
**    NAME         : m2dasup1.c
**   NOTE - This file contains routines which are used by both NCL & IGES.
**       CONTAINS:
**       int um_get_endpts(eptr,tfmat,spt,ept)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m2dasup4.c , 24.1
**    DATE AND TIME OF LAST MODIFICATION
**       09/11/13 , 13:02:07
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mdclass.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "nccs.h"

/*********************************************************************
**    E_FUNCTION     : int um_get_endpts(eptr,tfmat,spt,ept)
**      Return the cartesian  coordinates of the endpoints of a curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr                       pointer to the data of a curve
**          tfmat                      transformation matrix.
**       OUTPUT :  
**          spt                        start point
**          ept                        end point
**    RETURNS      : 
**          UU_SUCCESS iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_get_endpts(eptr, tfmat, spt, ept)
   struct UM_crvdatabag *eptr;
   UM_transf tfmat;
   UM_coord spt;
   UM_coord ept;
   {
   struct  UM_evcrvout evout;
   int status;
	struct NCL_nclpv_rec *p;

   uu_denter(UU_MTRC,(us,"um_get_endpts(key:%d,tfmat:%x,%x,%x)",
                  eptr->key, tfmat, spt, ept));
   switch (uc_super_class(eptr->rel_num))
      {
      case  UM_CURVE_CLASS:
         {
         status = uc_init_evcrvout(eptr, &evout);
         if (status != UU_SUCCESS) break;
         status = uc_evcrv(UM_POINT,(UU_REAL) 0.0,eptr,tfmat,&evout);
         if (status != UU_SUCCESS) break;
         um_vctovc(evout.cp,spt);
         status = uc_evcrv(UM_POINT,(UU_REAL) 1.0,eptr,tfmat,&evout);
         if (status != UU_SUCCESS) break;
         um_vctovc(evout.cp,ept);
         }
         status = UU_SUCCESS;
         break;
      default:
			if (eptr->rel_num == NCL_POINTVEC_REL)
			{
				p = (struct NCL_nclpv_rec *)eptr;
				um_vctovc(p->pt,spt);
				ept[0] = spt[0] * p->ve[0];
				ept[1] = spt[1] * p->ve[1];
				ept[2] = spt[2] * p->ve[2];
         	status = UU_SUCCESS;
			}
			else
			{
         	um_vctovc(UM_zerovec, spt);
        		um_vctovc(UM_zerovec, ept);
        		status = UU_FAILURE;
			}
         break;
      }
   uu_dexitstatus("um_get_endpts", status);
   return (status);
   }
