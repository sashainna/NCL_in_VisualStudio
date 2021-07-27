/*********************************************************************
**    NAME         :  m5edba.c
**       CONTAINS: routines to interface AG surface relation to UNIBASE
**			int um_agsrf_setup_data(rel_num, srf, size)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m5edba.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:06
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mdrel.h"
#include "msrf.h"
#include "mdcoord.h"
#include "msrf.h"
#include "mattr.h"

/*********************************************************************
**    E_FUNCTION     : int um_agsrf_setup_data(rel_num, srf, size)
**       Initialize the data bag (SRF) of the specified SIZE with
**			the format required by the given relation (REL_NUM). 
**			UNIBASE is called to setup the data bag, and appropriate
**			fields within the record are initialized to default values.
**    PARAMETERS   
**       INPUT  : 
**          rel_num						relation number 
**				size							size of data bag (bytes)
**       OUTPUT :  
**				srf							pointer to data bag to init
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agsrf_setup_data(rel_num, srf, size)
	int rel_num;
	struct UM_agsrf_rec *srf;
	int size;

	{
	int status;

	uu_denter(UU_MTRC, (us,"um_agsrf_setup_data(rel_num=%d, size=%d)",
		rel_num, size));

	status = ur_setup_data(rel_num, srf, size);
	if (status != UU_SUCCESS) goto done;

	srf->key = -1;

	strcpy(srf->label, "");
	/*MILLS- initialize the subscript and rldnu, rldnv fields. */
	srf->subscr = 0;
	srf->numupaths = UM_srfattr.numupaths; 
	srf->numvpaths = UM_srfattr.numvpaths; 
	srf->ptsperucrv = UM_srfattr.ptsperucrv;
	srf->ptspervcrv = UM_srfattr.ptspervcrv;
	srf->rldnu = 0;
	srf->rldnv = 0;
	srf->closdinu = 0;
	srf->closdinv = 0;
	srf->material = UM_mtrlmdl.index;
	srf->rev_normal = UU_FALSE;
	srf->srfaddr = NULL;

done:;
	uu_dexitstatus("um_agsrf_setup_data", status);
	return (status);
	}

