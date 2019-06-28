/*********************************************************************
**    NAME         :  d.c
**    	ud_set_mask() -- set debug mask.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
** 
**    MODULE NAME AND RELEASE LEVEL
**       d6sdeb.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:13
*********************************************************************/
#include "ustdio.h"
#include <signal.h>
#include "ustrings.h"
#include "usysdef.h"
#include "dasnog.h"
#include "udebug.h"

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) d6sdeb.c 2.4 11/26/85 09:04:48 single"};
#else
static char uu_sccsident[]={"@(#) d6sdeb.c 2.4 11/26/85 09:04:48 double"};
#endif

/*********************************************************************
**    E_FUNCTION     :  ud_set_mask(flag) -- set debug mask.
**    PARAMETERS   
**       INPUT  :  flag				Type of input from user.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : Sets UU_debmask
**    WARNINGS     : none
*********************************************************************/
ud_set_mask(flag)
int flag;
{
	char buf[60];
	char *tok;
	int numint, val;
	char *uu_strtok();

	switch(flag) {

		case UU_DEBUG_STRING: 
			ud_ddas(UD_DASSTRING,"Enter string (?,?,?,...)",buf,60,
				&numint,UD_NODEFAULT);
			if(numint == 0) return;
			tok = uu_strtok(buf,",");
			if( tok != NULL ) UU_debmask = 0;
			while( tok != NULL ) {
				if(strcmp("A",tok) == 0)  UU_debmask= -1;
				if(strcmp("G",tok) == 0)  UU_debmask=UU_debmask|UU_GTRC; 
				if(strcmp("D",tok) == 0)  UU_debmask=UU_debmask|UU_DTRC;
				if(strcmp("M",tok) == 0)  UU_debmask=UU_debmask|UU_MTRC;
				if(strcmp("X",tok) == 0)  UU_debmask=UU_debmask|UU_XTRC;
				if(strcmp("R",tok) == 0)  UU_debmask=UU_debmask|UU_RTRC;
				if(strcmp("S",tok) == 0)  UU_debmask=UU_debmask|UU_STRC;
				if(strcmp("K",tok) == 0)  UU_debmask=UU_debmask|UU_KTRC;
				if(strcmp("I",tok) == 0)  UU_debmask=UU_debmask|UU_ITRC;
				if(strcmp("N",tok) == 0)  UU_debmask=UU_debmask|UU_NTRC;
				if(strcmp("B",tok) == 0)  UU_debmask=UU_debmask|UU_BTRC;
				if(strcmp("E",tok) == 0)  UU_debmask=UU_debmask|UU_ETRC;
				if(strcmp("GI",tok) == 0)  UU_debmask=UU_debmask|UU_GITRC; 
				if(strcmp("DI",tok) == 0)  UU_debmask=UU_debmask|UU_DITRC;
				if(strcmp("MI",tok) == 0)  UU_debmask=UU_debmask|UU_MITRC;
				if(strcmp("XI",tok) == 0)  UU_debmask=UU_debmask|UU_XITRC;
				if(strcmp("RI",tok) == 0)  UU_debmask=UU_debmask|UU_RITRC;
				if(strcmp("SI",tok) == 0)  UU_debmask=UU_debmask|UU_SITRC;
				if(strcmp("KI",tok) == 0)  UU_debmask=UU_debmask|UU_KITRC;
				if(strcmp("II",tok) == 0)  UU_debmask=UU_debmask|UU_IITRC;
				if(strcmp("NI",tok) == 0)  UU_debmask=UU_debmask|UU_NITRC;
				if(strcmp("BI",tok) == 0)  UU_debmask=UU_debmask|UU_BITRC;
				if(strcmp("UI",tok) == 0)  UU_debmask=UU_debmask|UU_UITRC;
				tok = uu_strtok(NULL,",");
			}
			break;

			
		case UU_DEBUG_INT:
			ud_ddas(UD_DASINT,"Enter integer mask",&val, 1,&numint, UD_NODEFAULT);
			if( numint == 0 ) break;
			UU_debmask = val;
			break;

	}

	sprintf(buf,"Debug mask set to %d",UU_debmask);
	ud_wrerr(buf);

}

