/*********************************************************************
**    NAME      :  ginqst.h -- simple GKS inquiry functions, macros.
**       CONTAINS:
**		gqopstate() -- Inquire operating state value
**		gqchoicest(devnum) -- inquire choice device state.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ginqst.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:19
*********************************************************************/

#ifndef GKSINQSTH
#include "gtblvar6.h"
/*********************************************************************
	E_FUNCTION: Gos gqopstate() -- Inquire operating state value
*********************************************************************/
#define gqopstate() ug_gksos.sysstate

/*********************************************************************
	E_FUNCTION: Gchoicest gqchoicest(wkid,devnum) -- Inquire choice device state.
*********************************************************************/
#define	gqchoicest(wkid,devnum) (&(*ug_gksstli.wsopen[*wkid].inptr).choicedata[devnum-1])

/*********************************************************************
	E_FUNCTION: Gint gqsegerasecolor() -- Inquire segment erase color.
*********************************************************************/
#define	gqsegerasecolor() ug_segerasecolor

#define GKSINQSTH
#endif
