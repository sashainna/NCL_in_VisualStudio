/*********************************************************************
**    NAME         :  feasub
**       CONTAINS:
**			int um_feasub(pick_id,flag,data)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       feasub.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:45
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mfeatcom.h"
#include "mdcpln.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     :  int um_feasub(pick_id,flag,data)
**			Given the PICK_ID for the desired feature, return the
**			type of feature (real, vector, or coordinate) and the 
**			value for the feature.
**    PARAMETERS   
**       INPUT  : 
**       	pick_id				the reference to the desired feature
**       OUTPUT :  
**          flag					what type of feature it is
**											UM_FREAL  => real feature
**											UM_FCOORD => coordinate feature
**											UM_FVECT  => vector feature
**          data					data for the selected feature
**    RETURNS      :  stat - returned error status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_feasub(pick_id,flag,data)
   int pick_id;
	int *flag;
   UU_REAL data[];

	{
	int indx;
   UU_LOGICAL stat;

	uu_denter(UU_MTRC,(us,"um_feasub(%d,,)",pick_id));

   /* the given PICK_ ID returned from input pipeline
		is encoded to determine the type of feature and
		its position in the feature tables as follows:

     	  - UM_VECTMIN -> UM_VECTMAX = vector feature
     	  - UM_COORDMIN -> UM_COORDMAX = coordinate feature
     	  - UM_REALMIN -> UM_REALMAX = real feature 
	*/

   stat = UU_TRUE;
   if ((pick_id >= UM_REALMIN) && (pick_id <= UM_REALMAX))
      { /* real feature */
      indx = pick_id - UM_REALMIN;
		*flag = UM_FREAL;
		data[0] = UM_F_real[indx];
		data[1] = data[2] = 0.0;
      } 
	else if ((pick_id >= UM_COORDMIN) && (pick_id <= UM_COORDMAX))
      { /* coordinate feature */
		indx = pick_id - UM_COORDMIN;
		*flag = UM_FCOORD;
		um_vctovc(UM_F_coord[indx], data);
		um_mcstoccs(0, data, data);
		UM_cc_inttoext(data, data);
		}
	else if ((pick_id >= UM_VECTMIN) && (pick_id <= UM_VECTMAX))
      { /* vector feature */
		indx = pick_id - UM_VECTMIN;;
		*flag = UM_FVECT;
		um_vctovc(UM_F_vect[indx], data);
		um_mcstoccs(1, data, data);
		UM_cc_inttoext(data, data);
      }
	else 
		{
		stat = UU_FALSE;
		uu_uerror0(UU_FEATERROR, 7);
		}
	sprintf(UM_sbuf,"feasub: pick_id=%d index=%d flag=%d data=(%f,%f,%f)",
		pick_id,indx,*flag, data[0],data[1],data[2]);
	um_pscroll(UM_sbuf);
	uu_dexit;
	return(stat);
  
	}
