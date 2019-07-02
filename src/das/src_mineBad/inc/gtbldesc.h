/*********************************************************************
**    NAME         :  gtbldesc.h
**       CONTAINS:
**       All GKS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtbldesc.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:23
*********************************************************************/

#ifndef GTBLDESCH

#include "gobas.h"
#include "gtblopst.h"
#include "goseg.h"

/****** GKS DESCRIPTION TABLE *******/
typedef  struct {
	Gint nwsty;					/* number of available workstation types (1..n)=1 */
	Gchar **wktypes;			/* pointer to list of available workstation types */
	Gint maxopws;				/* max number of simultaneously open workstations=UG_MAXOPWS */
	UG_prat3 defprats;			/* default primitive attributes */
	Gtran defgmtran;			/* default global modeling transformation = identity */
	Gtran deflmtran;			/* default local modeling transformation = identity */
	Gindex defvindex;			/* default view index (0..n) = 1 */
	Gint nrqwsldc;				/* number of requested workstation LDC space=1 */
	Gnrect3 *reqwsldc;		/* list of requested ws LDC space (0,1,0,1,0,1)*/
} UG_gksdesctbl;					/* end of GKS Description Table */
#define GTBLDESCH
#endif
