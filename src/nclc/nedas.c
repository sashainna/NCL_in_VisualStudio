
/*********************************************************************
**    NAME         : nedas.c
**       CONTAINS:
**       int ua_dl_pdas(type,subnum,prnum,data_ary,
**       int ua_dl_pldas(type,subnum,prnum,data_ary,
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nedas.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:30
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "msol.h"
#include "mdpick.h"
#include "view.h"
#include "bsym.h"

/*********************************************************************
**    E_FUNCTION     : int ua_dl_pdas(type,subnum,prnum,data_ary,
**                                     data_size,numint,level)
**      This routine is a temporary implementation of a "limit the das"
**      system for picking. The calling routine will specify what type of
**      geometry entities are legal. This routine will check to see if the
**      picked data is of the appropriate type. Further, if the requested
**      data is a "component" of the actual picked data (e.g. edge/face
**      of body or component of composite curve), this routine will return
**      the  MTID of the component.
**    PARAMETERS   
**       INPUT  : 
**          type           type of DAS input
**          subnum         subsystem number
**          prnum          prompt number
**          data_ary       array to place MTID information
**          data_size      maximum number of MTIDs to return
**          level          1 => pick at segment level;
**                         2 => pick at item level within segment
**       OUTPUT :  
**          numint         number of picked entities
**    RETURNS      :
**       UU_TRUE if no errors; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_dl_pdas(type,subnum,prnum,data_ary,data_size,numint,level)
   int type;
   int subnum;
   int prnum;
   UM_PICKENT *data_ary;
   int data_size;
   int *numint;
   int level;
    
   {
   UD_PPICKREC pick;                /* data for entity picked */
   int numpick;                     /* number of entities picked */
   int status;                      /* status */
   int das_status;            /* status of das calls */

   uu_denter( UU_MTRC,(us,"ua_dl_pdas(?..?)"));
   *numint = 0;
   status = -1;
   if (type != UD_DASPICK)
      {
      uu_uerror0(/*um_d_pdas: illegal type specification*/UM_MODEL,42);
      }
   else
      {
      numpick = 1;
      status = 0;
      while ((numpick != 0) && (*numint < data_size))
         {
         das_status = ud_ldas(type,subnum,prnum,&pick,1,&numpick,UD_NODEFAULT);
         if ((das_status == UU_TRUE) && (numpick != 0))
            {
            if (level > pick.depth)
               {
               uu_uerror0(/*um_d_pdas: illegal entity picked*/UM_MODEL,43);
               status = UU_FALSE;
               }
            else
               {
               status = um_d_pickresolve(&pick,level,&data_ary[*numint]);
               (*numint)++;
               }
            }
         }
      }
   uu_dexit;
   return(das_status);
   }
/*********************************************************************
**    E_FUNCTION : int ua_dl_pldas(type,subnum,prnum,data_ary,
**                                  data_size,numint,level)
**      This routine is a temporary implementation of a "limit the das"
**      system for picking. The calling routine will specify what type of
**      geometry entities are legal. This routine will check to see if the
**      picked data is of the appropriate type. Further, if the requested
**      data is a "component" of the actual picked data (e.g. edge/face
**      of body or component of composite curve), this routine will return
**      the MTID of the component.
**    PARAMETERS   
**       INPUT  : 
**          type           type of DAS input
**          subnum         subsystem number
**          prnum          prompt number
**          data_ary       array to place PLOCREC information
**          data_size      maximum number of MTIDs to return
**          level          1 => pick at segment level;
**                         2 => pick at item level within segment
**       OUTPUT :  
**          numint         number of picked entities
**    RETURNS      :
**       0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_dl_pldas(type,subnum,prnum,data_ary,data_size,numint,level)
   int type;
   int subnum;
   int prnum;
   UM_PLOCREC data_ary[];
   int data_size;
   int *numint;
   int level;

   {
   UD_PLOCREC pick;                       /* data for entity picked */
   int numpick;                           /* number of entities picked */
   int status;                            /* status */
   int das_status;                     /* status of das calls */
   int um_d_pickresolve();

   uu_denter(UU_MTRC,(us,"um_dl_pldas(%d,%d,%d,%x,%d,,%d)",
               type,subnum,prnum,data_ary,data_size,level));
	*numint = 0;
	if (type != UD_DASPCKLOC)
	{
		uu_uerror0(/*um_d_pldas: illegal type specification*/UM_MODEL,44);
		status = -1;
	}
	else
	{
		status = 0;
		numpick = 1;
		while ((numpick != 0) && (*numint < data_size))
		{
			das_status = ud_ldas(type,subnum,prnum,&pick,1,&numpick,UD_NODEFAULT);
			if (pick.ppath.depth==0)
				numpick = 0;
			if (numpick != 0)
			{
				if (level > pick.ppath.depth)
				{
					uu_uerror0(/*um_d_pldas: illegal entity picked*/UM_MODEL,45);
				}
				else
				{
/*
.....
....."If" statement added by Paul to fix the problem with the key number of the 
....."nested point"  which is a pseudo entity and at this point does not have any 
.....real key number in the unibase.
.....06/15/93.
.....
*/
					if(pick.ppath.pickpath[0] != 0 || pick.ppath.pickpath[1] != 0)
						status = um_d_pickresolve(&pick,level,&data_ary[*numint].pent);
					else
					{
						data_ary[*numint].pent.num = pick.ppath.depth;
						data_ary[*numint].pent.key[0]= pick.ppath.pickpath[0];
						data_ary[*numint].pent.key[1]= pick.ppath.pickpath[1]; 
					}

					um_copyploc(&(pick.pndc), &(data_ary[*numint].ploc));
/*
.....
.....Added by Paul to copy the new part of UD_PLOCREC to UM_PLOCREC
.....07/15/92
..... 
*/
					strcpy(data_ary[*numint].ploc.label,pick.pndc.label);
					(*numint)++;
				}
			}
		}
	}
	uu_dexit;
	return(das_status);
}
