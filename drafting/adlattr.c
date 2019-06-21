#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "adrfcom.h"
#include "mattrddl.h"

/*********************************************************************
**    NAME         :  adlattr.c
**       CONTAINS:
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       adlattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:33
*********************************************************************/
static UU_REAL line_wt[4] = {1.0, 2.0, 3.0, 4.0};
#define NO_DEFAULT 0
#define DEFAULT 1
/*********************************************************************
**    E_FUNCTION     : ua_set_dl_attr(formfile)
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
ua_set_dl_attr(formfile)
	char	*formfile;
	
	{
	int status;
	static int dim_line_color, dens;
	static UU_REAL gap;
	static int *ans[] = {&dim_line_color, &dens, (int *)&gap};

	uu_denter(UU_MTRC,(us,"ua_set_dl_attr()"));
	dim_line_color = UA_dim_line_color;
	dens = UA_dim_line_dens - 1.0;
	gap = UA_gap_dim_text;

	status = ud_form(formfile, ans, ans);
   if (status==-1)
	   return -1;
/* NCL */
	UA_dim_line_color =  dim_line_color;
	ncl_set_color_attr(UA_dim_line_color);
/* end NCL */
	UA_dim_line_dens     =		line_wt[dens];
	UA_gap_dim_text		=		gap;

	uu_dexit;
	}
