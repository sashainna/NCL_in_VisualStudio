
/*********************************************************************
**    NAME         :  uadok.c  -- part of ulib.a
**       CONTAINS:
**       uu_adok(addr) -- check for legal address.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uadok.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:51
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#ifdef UU_CHECK
/*********************************************************************
**    I_FUNCTION     :  int uu_adok(addr) -- check for legal addr.
**    PARAMETERS   
**       INPUT  :  char *addr -- address to be checked.
**       OUTPUT :  
**    RETURNS      : 1 if address is OK, else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : This is the UNIX and APOLO dependent implementation.
*********************************************************************/

/* This code used for VAXVMS and APOLLO */
#if UU_COMP!=UU_SUN
#if UU_COMP!=UU_IRIS
#if UU_COMP!=UU_IRIS4D
#if UU_COMP!=UU_HPUX
#if UU_COMP!=UU_PYRAMID
#if UU_COMP!=UU_VAXULTRIX
#if UU_COMP!=UU_MASSCOMP
#if UU_COMP!=UU_TEK
#if UU_COMP!=UU_DECUNIX
#if UU_COMP!=UU_WINNT
#if UU_COMP!=UU_WIN2K
int uu_adok(addr)
char *addr;
{
	return(1);
}
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif

/* This code used for all machines other than VAXVMS or APOLLO */
#if UU_COMP!=UU_VAXVMS
#if UU_COMP!=UU_APOLLO
#if UU_COMP!=UU_RIDGE

extern char etext,end;
int uu_adok(addr)
char *addr;
{
	char us[100];
	if (addr> &etext) return(1);
	uu_denter2(-1,(us,"uu_adok(%x) bad address. etext=%x, end=%x",
			addr,&etext,&end));
	uu_dexit;
	return(0);
}

#endif
#endif
#endif

#endif
