/*********************************************************************
**    NAME         :  aswrite.c
**       CONTAINS:
**       us_write
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       aswrite.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:40
*********************************************************************/

#include "ustdio.h"
#include "udebug.h"
#include "usysdef.h"
#include "xenv1.h"
#define DRFOUTSYM "DRAFTOUT"
#define DRFWRTSYM "DRFWRITE4"
FILE *drfout = NULL;
extern int	UA_write4;					/* write(4) redirection flag */
												/* 0=NO, 1=stdout, 2=stderr */

/*********************************************************************
**    E_FUNCTION :  void us_write(unit_number, string)
**       Write 'string' to 'unit_number'.
**			1= stdout
**			2= stderr
**			3= DRAFTOUT symbol (default= draft.debug)
**			4= UNICAD debug file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void us_write(unit_number, string)
	int 	unit_number;
	char	*string;
{
	int		slen;
	UX_pathname pathname;		/* replaces char	pathname[UU_MAXPATHLEN] */
	char		*cptr;
	char		*ux_getenv();
	char     us[1000];

	if (unit_number != 4) {
		uu_denter2(UU_STRC,(us,"us_write(%d,%s)",unit_number,string));
	}
	
	switch (unit_number) {
		
		case 1:
			fputs(string, stdout);
			break;
		case 2:
			fputs(string, stderr);
			break;
		case 3:
			if (drfout==NULL) {					/* file not open yet */
				cptr = ux_getenv(DRFOUTSYM, UX_PRTERRS);
				if (cptr!=NULL)
					strcpy(pathname,cptr);
				else
					strcpy(pathname,"draft.debug");
				drfout = fopen(pathname,"w");
			}
			if (drfout!=NULL)						/* only write in open OK */
				fputs(string, drfout);
			break;
		/*--- write string to UNICAD trace file (if trace on )-------*/
		case 4:
			switch (UA_write4) {
			 case 0:									/* normal to trace file */
				strcpy(us,string);
				slen = strlen(us) - 1;			/* last char of string */
				if (us[slen]=='\n')				/* newline at end of string */
					us[slen] = '\0';				/* do not use it */
				uu_denter2(UU_STRC,(us,"%s",us));
				uu_dexit;
				break;
			 case 1:									/* redirect to stdout */
				fputs(string, stdout);
				break;
			 case 2:									/* redirect to stderr */
				fputs(string, stderr);
				break;
			}
			break;
		default:
			fputs(string, stdout);

	}
	if (unit_number != 4) {
		uu_dexit;
	}
}
