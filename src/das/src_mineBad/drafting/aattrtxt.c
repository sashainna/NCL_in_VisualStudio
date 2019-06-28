/*********************************************************************
**    NAME         :aattrtxt.c
**       CONTAINS:
**					ua_set_txt_precision
**					
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aattrtxt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:31
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aattrtxt.c 3.1 2/2/88 14:36:36 single"};
#else
static char uu_sccsident[]={"@(#) aattrtxt.c 3.1 2/2/88 14:36:36 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"

/*********************************************************************
**    E_FUNCTION     : void		ua_set_txt_precision(prec)
**       Set text precision.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          prec						text precision	
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_txt_precision(prec)
Gtxprec	prec;
	{
	uu_denter(UU_STRC,(us,"ua_set_txt_precision(prec=%d)", prec));
	UA_txt_precision = prec;
	uu_dexit;
	}
