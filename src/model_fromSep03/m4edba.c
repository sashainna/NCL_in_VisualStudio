/*********************************************************************
**    NAME         :  m4edba.c
**       CONTAINS: APPLIED GEOMETRY substem UNIBASE interface routines
**			int um_agcrv_setup_data(rel_num, eptr, size)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4edba.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:02
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "umoveb.h"
#include "uhep.h"
#include "class.h"
#include "mcrv.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_setup_data(rel_num, eptr, size)
**       Initialize the data bag (EPTR) of the specified SIZE with
**			the format required by the given relation (REL_NUM). 
**			UNIBASE is called to setup the data bag, and appropriate
**			fields within the record are initialized to default values.
**    PARAMETERS   
**       INPUT  : 
**          rel_num						relation number 
**				size							size of data bag (bytes)
**       OUTPUT :  
**				eptr							pointer to data bag to init
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_setup_data(rel_num, eptr, size)
	int rel_num;
	struct UM_agcrv_rec *eptr;
	int size;

	{
	int status;

	uu_denter(UU_MTRC, (us,"um_agcrv_setup_data(rel_num=%d, size=%d)",
		rel_num, size));

	status = ur_setup_data(rel_num, eptr, size);
	if (status != UU_SUCCESS) goto done;

	eptr->key = -1;

	strcpy(eptr->label, "");
	eptr->subscr = 0;
	eptr->crvaddr = NULL;
/*
.....Init closed curve flag - RAZ
*/
	eptr->closdinu = 0;

done:;
	uu_dexitstatus("um_agcrv_setup_data", status);
	return (status);
	}

