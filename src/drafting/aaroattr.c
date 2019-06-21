#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "adrfcom.h"

/*********************************************************************
**    NAME         :  aaroattr.c
**       CONTAINS:
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       aaroattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:30
*********************************************************************/
static UU_REAL line_wt[4] = {1.0, 2.0, 3.0, 4.0};
#define NO_DEFAULT 0
#define DEFAULT 1
/*********************************************************************
**    E_FUNCTION     : ua_set_aro_attr(formfile)
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
ua_set_aro_attr(formfile)
	char	*formfile;
	
	{
	int status;
	/* local storage */
	static UU_REAL  arrow_size;
	static int arrow_sym, arrow_color, arrow_place, arrow_dens;

	/* answers */
	static int *ans[] = {&arrow_sym, (int *)&arrow_size, &arrow_color,
								&arrow_dens, &arrow_place};
								

	uu_denter(UU_MTRC,(us,"ua_set_aro_attr()"));

/* set-up default values */

	arrow_sym = UA_arrow_symbol;
	arrow_size = UA_arrow_size;
	arrow_color = UA_arrow_color;
	arrow_dens = UA_arrow_dens - 1.0;
	arrow_place = UA_arrow_place;

	status = ud_form( formfile, ans, ans);
	if (status==-1)
		return  -1;
	UA_arrow_symbol = arrow_sym;
	UA_arrow_color =  arrow_color;
	UA_arrow_dens = line_wt[arrow_dens];
	UA_arrow_place =	arrow_place;
	UA_arrow_size = arrow_size;

	uu_dexit;
	}
