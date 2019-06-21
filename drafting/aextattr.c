#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "adrfcom.h"

/*********************************************************************
**    NAME         :  aextattr.c
**       CONTAINS:
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       aextattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:34
*********************************************************************/
static UU_REAL line_wt[4] = {1.0, 2.0, 3.0, 4.0};
#define NO_DEFAULT 0
#define DEFAULT 1
/*********************************************************************
**    E_FUNCTION     : ua_set_ext_attr(formfile)
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
ua_set_ext_attr(formfile)
	char	*formfile;
	
	{
	int status;
	static int ext_color, ext_dens, ext_suppress;
	static UU_REAL   gap, past;
	static int *ans[] = {&ext_color, &ext_dens, (int *)&gap, (int *)&past,
									&ext_suppress};

	uu_denter(UU_MTRC,(us,"ua_set_ext_attr()"));

	ext_color = UA_ext_line_color;
	ext_dens = UA_ext_line_dens - 1.0;
	gap = UA_gap_geom_line;
	past = UA_ext_past_line;
	ext_suppress = UA_ext_line_sup; 

	status = ud_form(formfile, ans, ans);
   if (status==-1)
	   return -1;

	UA_ext_line_color =  ext_color;
	UA_ext_line_sup		=		ext_suppress;
	UA_ext_line_dens     =		line_wt[ext_dens];
	UA_gap_geom_line		= 		gap;
	UA_ext_past_line		=		past;

	uu_dexit;
	}
