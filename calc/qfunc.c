
/*********************************************************************
**    NAME         :  c1calcf.c
**       CONTAINS:
**				uc_calcfunc
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       qfunc.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:54
*********************************************************************/

#include		"usysdef.h"
#include		"udebug.h"
#include		"uhep.h"
#include		"uims.h"
#include		"dwindow.h"
#include		"dasnog.h"
#include		"calcom.h"
#include		"qlang.h"
extern UD_WINDOW_REC UQ_wcb;	/* window control block */

int	um_mindist(),
		um_perpdist(),
		um_crvlen();

/** 	The legal return types are:							   	**/
/**	UM_CARTESIAN		UM_CYLINDRICAL		UM_SPHERICAL		**/
/** 	UM_VCARTESIAN 		UM_VCYLINDRICAL   UM_VSPHERICAL		**/
/**	UQ_SCALAR															**/

UQ_query		UQ_querytb[] = {
 /* return type, calc key word, calling function */
	{UQ_SCALAR,   "mindist",     um_mindist},
	{UQ_SCALAR,   "perpdist",    um_perpdist},
	{UQ_SCALAR,   "crvlen",      um_crvlen},
	{UQ_CNONE,    "",            UU_NULL}};




/*********************************************************************
**    I_FUNCTION :  uq_calcfunc(typ1,val)
**       Pull down the calculator window then call the appropriate
**			function to handle the calculator queries.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uq_calcfunc(typ1,val)
int	*typ1;
UU_REAL 	val[];

{
	register	ind, status;
	int args[2];

	uu_denter(UU_DTRC,(us,"enter queryfunc,typ1=%d,val1=%g",*typ1,val[0]));
	ind = val[0];
	*typ1 = UQ_querytb[ind].type;
	if (uq_calc2flag == UU_FALSE)
		ul_close_window();
/*		WINDOW_OFF(&UQ_wcb);*/
							/* take care all the interrogations*/
	status = (*UQ_querytb[ind].funcnm)(*typ1,val);		
	if (uq_calc2flag == UU_FALSE)
	{
		args[1] = 1;
		UD_winrow = 20; UD_wincol = 80;
		ul_open_window(UD_winrow,UD_wincol,args);
/*		WINDOW_ON(&UQ_wcb, &UD_winrow, &UD_wincol);*/
	}
	uu_dexit;
	return(status);
}	/* uqi_queryfunc */

