/*********************************************************************
**    NAME      :  ginqdsiz.h -- simple GKS inquiry functions, macros.
**       CONTAINS:
**		gqdisplaysize(ws) -- Inquire display size
**		gqacharrow(ws) -- inquire number of hardware char rows
**		gqacharcol(ws) -- inquire number of hardware char cols
**		gqnchdev(ws) -- inquire number of choice devices
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ginqdsiz.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:19
*********************************************************************/

#ifndef GINQDSIZH

#include "gobas.h"
#include "gtblvar4.h"
/*********************************************************************
	E_FUNCTION: Gdspsize *gqdisplaysize(ws) -- Inquire display size
*********************************************************************/
#define gqdisplaysize(ws)  (&((*(ug_gksstli.wsopen[*ws].wdtptr)).dspsize))

/*********************************************************************
	E_FUNCTION: Gint gqacharrow(ws) -- inquire number of hardware char rows
*********************************************************************/
#define gqacharrow(ws) ((*(ug_gksstli.wsopen[*ws].wdtptr)).rowmax)

/*********************************************************************
	E_FUNCTION: Gint gqacharcol(ws) -- inquire number of hardware char cols
*********************************************************************/
#define gqacharcol(ws) ((*(ug_gksstli.wsopen[*ws].wdtptr)).colmax)

/*********************************************************************
	E_FUNCTION: Gint gqnchdev(ws) -- inquire number of choice devices on ws.
*********************************************************************/
#define gqnchdev(ws) ((*((*(ug_gksstli.wsopen[*ws].wdtptr)).inwdtpt)).nchoice)

Gwpoint *gqcharup();
Gwpoint3 *gqtxplane();
Gtxalign *gqtextalign();
Gwpoint *gqpatsize();
Gwpoint *gqpatrefpoint();
Gasfs *gqasfs();
#define GINQDSIZH
#endif
