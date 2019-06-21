/*********************************************************************
**    NAME         :  unidone.c
**       CONTAINS:
**			uu_unidone
**			uu_unireset
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       unidone.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:54
*********************************************************************/

#include "usysdef.h"
#include	"gtbl.h"
#include "g.h"
#include "usysg.h"
#include "dasnog.h"
#include "uims.h"

extern UD_UIMS UD_duimsdeflt;
extern UD_CURLAYOUT UD_curlayout;
/*********************************************************************
**    E_FUNCTION     :  int uu_unidone()
**       Cleanup routine for UNICAD
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_unidone()
	{
	int i;

/*	-- be kind to gks and close all the workstations -- */

	for(i=0; i<UD_wsptr; i++)
	{
		gdeactivatews(UD_gksws[i]);
		gclosews(UD_gksws[i]);
	}

/*	-- shut down gks -- */

	gclosegks();

	}

/*********************************************************************
**    E_FUNCTION     :  int uu_unireset() 
**       Reset UNICAD system
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_unireset()

	{

	unsigned long key_id;				/* current entity key */
	int nxt_ent;							/* index of next master key */
	int get_nxt_status;					/* status of next get opertation */
	int dsegid;								/* display segment id of current entity */


	/*------------------------------------------------------------------------
	**  Start of Executable Code
	**-----------------------------------------------------------------------*/


	um_feareset();										/* remove any features */

	/* loop over all entities in the data base
		and delete from the gks segment file */

	nxt_ent = 1;
	get_nxt_status = 0;
	while(get_nxt_status == 0)
		{
		get_nxt_status = ur_get_next_key(&nxt_ent,&key_id);  /* get next entity */
		if(get_nxt_status == 0)
			{
			ur_retrieve_disp_segid(key_id,&dsegid);   /* get entities gks seg-id */
			if(dsegid>=0) uv_delsegs(dsegid);  /* delete from gks */
			ur_update_disp_segid(key_id,-1); 
			}
		nxt_ent++;
		}
	uv_deactivsc(0);								/* deactivate the curent screen */
/*	um_reset_appgeo();						 reset applied geometry */
	ur_reset_unibase();							/* reset unibase */
	um_rm31_romini();								/* reset romulus */
	}
