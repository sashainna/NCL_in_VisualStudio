
/*********************************************************************
**    NAME         : tigdrf2.c
**       CONTAINS:
**      	  ua_setcpln
**      	  ua_getcpln
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigdrf2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:45
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) autility.c 3.1 2/2/88 14:45:21 single"};
#else
static char uu_sccsident[]={"@(#) autility.c 3.1 2/2/88 14:45:21 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "modef.h"

static UU_LOGICAL us_i110 = UU_TRUE;
static UU_LOGICAL us_i120 = UU_TRUE;
static UU_LOGICAL us_i93  = UU_TRUE;

/*********************************************************************
**    E_FUNCTION     : us_init_autility()
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
us_init_autility()
	{
	}
/*********************************************************************
**    E_FUNCTION     : um_get_drwscale()
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_get_drwscale(scale)
	UU_REAL *scale;
	{
	*scale = 1.0;
	}

/*********************************************************************
**    E_FUNCTION     : ua_getcpln(entity, cpln_origin, xaxis, yaxis, zaxis)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_getcpln(entity, cpln_origin, xaxis, yaxis, zaxis)
struct UA_generic_draft	(*entity);
UU_REAL	cpln_origin[3];
UU_REAL	xaxis[3];
UU_REAL	yaxis[3];
UU_REAL	zaxis[3];
	{

	uu_denter(UU_STRC,(us,"SAL ua_getcpln(entity=%s, cpln_origin=%s, xaxis=%s,\
		yaxis=%s, zaxis=%s)", "...", "...", "...", "...", "..."));

	um_vctovc((*entity).cpln.cpln_origin,cpln_origin);
	um_vctovc((*entity).cpln.xaxis,xaxis);
	um_vctovc((*entity).cpln.yaxis,yaxis);
	um_vctovc((*entity).cpln.zaxis,zaxis);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_setcpln(entity)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_setcpln(entity)
struct UA_generic_draft	(*entity);
	{

	uu_denter(UU_STRC,(us,"SAL ua_setcpln(entity=%s)", "..."));

(*entity).cpln.cpln_origin[0] = 0.000000e+000;
(*entity).cpln.cpln_origin[1] = 0.000000e+000;
(*entity).cpln.cpln_origin[2] = 0.000000e+000;
(*entity).cpln.xaxis[0] 		= 1.000000e+000;
(*entity).cpln.xaxis[1] 		= 0.000000e+000;
(*entity).cpln.xaxis[2] 		= 0.000000e+000;
(*entity).cpln.yaxis[0] 		= 1.000000e+000;
(*entity).cpln.yaxis[1] 		= 0.000000e+000;
(*entity).cpln.yaxis[2] 		= 0.000000e+000;
(*entity).cpln.zaxis[0] 		= 1.000000e+000;
(*entity).cpln.zaxis[1] 		= 0.000000e+000;
(*entity).cpln.zaxis[2] 		= 0.000000e+000;
	uu_dexit;
	}
