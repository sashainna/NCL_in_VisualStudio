/*********************************************************************
**    NAME         :  m4uxent.c
**       CONTAINS: user interface routines to handle intersections
**			int umu_isect_entities()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4uxent.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:05
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mdebug.h"
#include "mdpick.h"
#include "ncl.h"

/*********************************************************************
**    E_FUNCTION     : int umu_isect_entities()
**			Prompt the user to pick two entities (curves/surfaces/shells)
**			and perform the intersection.
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_isect_entities()

	{
	int numint;
	int status;
	int bagsize;
	struct UC_entitydatabag *e1;
	struct UC_entitydatabag *e2;
	struct UC_entitydatabag *e3;
 struct NCL_crvgen_rec *cpt;
 char *cptr;
	UM_PLOCREC pick;
	UU_KEY_ID *newkeys, key1, key2;
	UU_LOGICAL initialize;
	int i,npt;
	
	uu_denter(UU_MTRC,(us,"umu_isect_entities()"));

	/* initialize databags */
	bagsize = sizeof(struct UC_entitydatabag);
	e1 = (struct UC_entitydatabag *) uu_malloc(bagsize);
	e2 = (struct UC_entitydatabag *) uu_malloc(bagsize);
	e3 = (struct UC_entitydatabag *) uu_malloc(bagsize);

	while (UU_TRUE)
		{
  ud_lgeo(UU_TRUE, UD_ncl_allsfpl);
		um_dl_pdas(UD_DASPICK, /* pick first entity to intersect */
			UM_MODEL, 138, &pick, 1, &numint, 1);
		if (numint < 1) goto done;

  ud_lgeo(UU_TRUE, UD_ncl_allsfpl);
		ud_ldas(UD_DASSELECT, /* pick second entity to intersect */
			UM_MODEL, 139, UU_NULL, 0, &numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		/* retrieve first entity to intersect with selected list */
		key1 = um_get_pickkey(&pick.pent, 1);

		/* process list of entities to intersect with first */
		initialize = UU_TRUE;
		while (ud_gnxt(initialize, UU_NULL, &e2->key, 1) == UU_TRUE)
			{
			initialize = UU_FALSE;

			/* check if same entity picked */
			if (key1 == e2->key)
				{
				uu_uerror0(/* same entity picked */ UM_MODEL, 179);
				goto repeat2;
				}

			/* retrieve second entity to intersect with first */

    key2 = e2->key;			

			/* surface and surface */

   status = um_isect_sf_pts (key1,key2,&npt,&cptr);

			/* create intersection */
   if (status == UU_SUCCESS)
     {
      cpt = (struct NCL_crvgen_rec *) cptr;
      status = ncl_interp_rbsp (npt, cpt, 1, e3);
      uu_free(cptr);
     }
			if (status == UU_FAILURE)
				uu_uerror0(/* no intersection found */ UM_MODEL, 80);
			else
				{
					uc_retrieve_data(e3, bagsize);
					uc_display(e3);
				}
repeat2:;
			}
repeat1:;
		}
		  

done:;
	uu_free(e1);
	uu_free(e2);
	uu_free(e3);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int um_isect_sf_pts (key1,key2,npt,cpt)
**			Prompt the user to pick two entities (curves/surfaces/shells)
**			and perform the intersection.
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
 um_isect_sf_pts (key1,key2,npt,cpt)
  UU_KEY_ID key1,key2;
  int *npt;
  char *cpt;
 {
  int n,rel1,rel2,status;
  UM_int2 ncltyp1,ncltyp2;

  status = ur_retrieve_data_relnum(key1,&rel1);
  if (status == UU_SUCCESS)
    {
     ncl_get_type (rel1,&ncltyp1); 
     status = ur_retrieve_data_relnum(key2,&rel2);
    } 

  if (status == UU_SUCCESS) 
    {
     ncl_get_type (rel2,&ncltyp2);
     status = ncl_isect_sf (key1,ncltyp1,key2,ncltyp2,&n,cpt);
     *npt  = n;
    }

  uu_dexit;
  return(status);
 }
