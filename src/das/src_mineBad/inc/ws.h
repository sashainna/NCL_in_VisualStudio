/*********************************************************************
**    NAME         :  ws.h
**       CONTAINS:
**       UG_DEFCHO -- macro defining default choice data record.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ws.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:07
*********************************************************************/
#ifndef WSH
#define WSH
#include "nclcmd.h"

#define UW_DEFCHO MAXCHOICES,NUMPETS,choicepets,0.,.5,.5,1., 1,NULL,\
	defchoptr,0,NULL,NULL,0,NULL,0,0,0,1,1

#define UW_DEFSTR NCL_MAX_COMLINE,1,stringpets,0.,0.,1.,1.,NCL_MAX_COMLINE,0," ",0,0,0,1,2
#endif
