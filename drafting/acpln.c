/*********************************************************************
**    NAME         : acpln.c
**       CONTAINS:
**			ua_setmcpln
**			ua_resetmcpln
**			
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       acpln.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:32
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) acpln.c 3.1 2/2/88 14:38:38 single"};
#else
static char uu_sccsident[]={"@(#) acpln.c 3.1 2/2/88 14:38:38 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"

static UU_REAL sys_origin[3];
static UU_REAL sys_xaxis[3];
static UU_REAL sys_yaxis[3];
static UU_REAL sys_zaxis[3];

/*********************************************************************
**    E_FUNCTION     : void		ua_resetmcpln()
**       Reset modeling construction plane to before the call to
**			ua_setmcpln.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_resetmcpln()
	{

	uu_denter(UU_STRC,(us,"ua_resetmcpln()"));

	um_setcpln_origin(sys_origin);
	um_setcpln_zaxis(sys_zaxis);
	um_setcpln_yaxis(sys_yaxis);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_setmcpln(entity)
**       Set modeling construction plane to the drafting plane for
**			this drafting entity. Use ua_resetmcpln to restore.
**    PARAMETERS   
**       INPUT  : 
**          entity				drafting entity record
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_setmcpln(entity)
struct UA_generic_draft	(*entity);
	{
	UU_REAL	origin[3];
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];

	uu_denter(UU_STRC,(us,"ua_setmcpln(entity=%s)", "..."));
	um_getcpln(sys_origin,sys_xaxis,sys_yaxis,sys_zaxis);
	ua_getcpln(&((*entity)),origin,xaxis,yaxis,zaxis);
	um_setcpln_origin(origin);
	um_setcpln_zaxis(zaxis);
	um_setcpln_yaxis(yaxis);
	uu_dexit;
	}
