#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "adrfcom.h"

/*********************************************************************
**    NAME         :  allattr.c
**       CONTAINS:
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       allattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:36
*********************************************************************/
#define NO_DEFAULT 0
#define DEFAULT 1
/*********************************************************************
**    E_FUNCTION     : ua_set_ll_attr(formfile)
**       Set dimension attributes using forms package.
**    PARAMETERS   
**       INPUT  : 
**          formfile				form file name
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_ll_attr(formfile)
	char	*formfile;
	
	{
	int status;
	static UU_REAL  stub_length;
	static int stub_orient, stub_location;
	static int  *ans[] = {(int *)&stub_length, &stub_orient, &stub_location};

	uu_denter(UU_MTRC,(us,"ua_set_ll_attr()"));


	stub_length = UA_ldr_stub_len;
	stub_orient = UA_ldr_orient; 
	stub_location = UA_ldr_location; 
	
	status = ud_form(formfile, ans, ans);
	if (status==-1)
	   return -1;

	UA_ldr_orient		=		stub_orient;
	UA_ldr_location		=	stub_location;
	UA_ldr_stub_len = stub_length;

	uu_dexit;
	}
