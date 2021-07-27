/*********************************************************************
**    NAME         :  m3utrim1.c
**       CONTAINS: user interface routines for trim/extend & midtrim 
**
**        int umu_get_trlist(ifmid,keys,eloc,kentry)
**            umu_trim1_curve (ifmid)
**
**    COPYRIGHT 1995 (C) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**        m3utrim1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:08:00
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h"
#include "modef.h"
#include "mdrel.h"
#include "mdclass.h"
#include "mdgenent.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdpick.h"
#include "misect.h"
#include "mdebug.h"
#include "ulist.h"
/*
.....Set the DAS level for picking composite curves
.....so we do not get the underlying curves (@UN)
.....Vadim  -  6/20/97
*/

static int daslev=1;

/*********************************************************************
**    E_FUNCTION: umu_get_trlist(ifmid,keys,eloc,kentry)
**      Prompt the user for a curve to trim to and then prompt
**      for a curve to trim, If kentry=1 prompt only for CV to trim.
**    PARAMETERS   
**       INPUT  : 
**          ifmid   - 0 - trim, 1 - midtrim, 2 - untrim
**          kentry  - 0 - begin from picking trimmers, 1 - pick CV only.
**       OUTPUT :  
**          keys    - List of picked entity with pick location.
**                      1,2 - trimming entity, 3... entity to trim 
**                            ended with 0 key.
**          eloc    - List of pick locations.
**          kentry  - 0 - done button hit (no more CV to trim),
**                    1 - CV is picked to trim.
**    RETURNS      : UU_SUCCESS - all picks done, process;
**                   UU_FAILURE - reject hit. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umu_get_trlist(ifmid,keys,eloc,kentry)
int ifmid, *kentry;
UU_KEY_ID *keys;
UD_NDCLOCREC *eloc;

{
	UM_PLOCREC pick;          /* pick information */
	struct UC_entitydatabag *e1;  /* data for curve to trim to */
	UU_KEY_ID key1, key2;
	int numint, enter, msgn;
	int status;
	int iprm[3];

	uu_denter(UU_MTRC,(us,"umu_get_trlist()"));
/*
.....Initialize status
*/
	status = UU_FAILURE;

/* 
.....limit entities picked to curves 
.....aak 31-oct-1997: and uv curves on surfaces
*/
	ud_lgeo(UU_TRUE, UD_allcurvess_pts);  
/* 
...get storage 
*/
	e1 = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));

	enter = (*kentry == 0);
	key1  = keys[0];
	key2  = keys[1];
	iprm[0] = 164; iprm[1] = 166;
/*
.....If user would like to untrim, go down to trimco; JLS 3/2/99
*/
	if (ifmid ==2)
	{
		key1 = keys[0] = key2 = keys[1] = 0;
		goto trimco;
	}
	while (enter)
	{
start1:;
/*
.....limit entities picked to curves and points
*/ 
	ud_lgeo(UU_TRUE, UD_allcurvess_pts);  
		ud_leditable(UU_FALSE);
		um_dl_pldas(UD_DASPCKLOC,/*pick element to trim to*/
  				UM_MODEL, 165, &pick, 1, &numint, daslev);
/* 
...get information for curve to trim to 
*/
		if (numint <= 0) 
		{
			key1 = keys[0] = key2 = keys[1] = 0;
			goto done;
		}

		key1 = keys[0] = e1->key = um_get_pickkey(&pick.pent, daslev);
		um_copyploc(&pick.ploc, &eloc[0]);
		um_retrieve_data_relnum(e1->key, &(e1->rel_num));
/*
... aak 31-oct-1997: added uv curves on surfaces
*/
		if (UD_inselmask(e1->rel_num, UD_allcurvess_pts))
		{
			uu_uerror0(/* can not trim to this curve */UM_MODEL,102);
			goto repeat1;
		}

		if (ifmid == 1)
		{
			ud_leditable(UU_FALSE);
			um_dl_pldas(UD_DASPCKLOC,/*pick second element to trim to*/
                   UM_MODEL, 168, &pick, 1, &numint, daslev);
			if (numint <= 0) goto done;

			key2 = keys[1] = e1->key = um_get_pickkey(&pick.pent, daslev);
			um_copyploc(&pick.ploc, &eloc[1]);
			um_retrieve_data_relnum(e1->key, &(e1->rel_num));
/*
... aak 31-oct-1997: added uv curves on surfaces
*/
			if (UD_inselmask(e1->rel_num, UD_allcurvess_pts)) 
         {
				uu_uerror0(/* can not trim to this curve */UM_MODEL,102);
				goto repeat1;
         }
      }
		else 
			key2 = keys[1] = 0;

		enter = UU_FALSE;

repeat1:;
	}
/*
.....Repeatedly pick a curve to trim to the previously picked trim curve 
*/
trimco:;
	enter = UU_FALSE;
	while (!enter)
	{
		msgn = (key1 == 0)? 345: iprm[ifmid];
		ud_lgeo(UU_TRUE, UD_vircurves); 
		ud_leditable(UU_TRUE);
		um_dl_pldas(UD_DASPCKLOC, /*pick part of entity to trim*/UM_MODEL, 
                   msgn, &pick, 1, &numint, daslev);
/* 
.....get information for curve to trim 
*/
		if (numint <=0) 
		{
			keys[2] = keys[0] = 0;
			*kentry = 0;
			if (ifmid !=2)
				goto start1;
			else
				goto done;
		}

      *kentry = 1;
      keys[2] = e1->key = um_get_pickkey(&pick.pent, daslev);
      um_copyploc(&pick.ploc, &eloc[2]);
      um_retrieve_data_relnum(e1->key, &(e1->rel_num));

		if (UD_inselmask(e1->rel_num, UD_trimcurves))
		{
			uu_uerror0(/* can not trim to this curve */UM_MODEL,102);
			goto repeat0;
		}
		else if (e1->key == key1 || e1->key == key2)
		{
			uu_uerror0(/*trim element and trim entity are the same*/
                     UM_MODEL,103);
         goto repeat0;
      }
      status = UU_SUCCESS;
      goto done;

repeat0:;
	}
done:;
/*
...End of input 
*/  
/*  ud_lgeo (UU_FALSE, UD_circle); */
  uu_free (e1);
  uu_dexit;
  return (status);
}

/*********************************************************************
**    E_FUNCTION: umu_trim1_curve (ifmid)
**      Main triming routine: 
**      Prompt the user for a curve to trim to and then repeatedly
**      prompt for a curve to trim and process the picked stuff.
**    PARAMETERS   
**       INPUT  : 
**          ifmid   - mode: 1 - trim/extend, 2 - midtrim.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
umu_trim1_curve (ifmid)
int ifmid;

{
  UU_KEY_ID keys[3];
  UD_NDCLOCREC eloc[3];
  int status, mentry;

  uu_denter(UU_MTRC,(us,"umu_trim1_curve()"));

  mentry = 0;
  while ((status = umu_get_trlist(ifmid,keys,eloc,&mentry)) == UU_SUCCESS)
  {
		if (keys[2] > 0) umu_trim_curve (keys,eloc);
		else goto done;
  }
done:;
  uu_dexit;
 } 
