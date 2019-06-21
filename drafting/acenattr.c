/*********************************************************************
**    NAME         :  aaroattr.c
**       CONTAINS:
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       acenattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:31
*********************************************************************/

#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "adrfcom.h"

UU_REAL UA_dot = 0.25 ;
UU_REAL UA_gap = 0.125;
UU_REAL UA_dash_min = 0.5;
UU_REAL UA_dash_max = 1.0;
#define NO_DEFAULT 0
#define DEFAULT 1

/*********************************************************************
**    E_FUNCTION     : ua_set_centerline_attr()
**       Set dimension attributes using forms package.
**    PARAMETERS   
**       INPUT  : 
**          none				
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_centerline_attr()
	
	{
	int status;
	static UU_REAL  farr[4];
	static int *ans[] = {(int *)&farr[0], (int *)&farr[1], (int *)&farr[2],
									(int *)&farr[3]};

	uu_denter(UU_MTRC,(us,"ua_centerline_attr()"));

	farr[0] = UA_dot;
	farr[1] = UA_gap;
	farr[2] = UA_dash_min;
	farr[3] = UA_dash_max;

	status = ud_form("acenattr.frm", ans, ans);
	if (status==-1)
		return -1;

	UA_dot = farr[0];
	UA_gap = farr[1];
	UA_dash_min = farr[2];
	UA_dash_max = farr[3];

	uu_dexit;
	}
