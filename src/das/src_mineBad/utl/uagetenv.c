#include "usysdef.h"
#if UU_COMP==UU_APOLLO
#include "ustdio.h"
#include "/sys/ins/base.ins.c"
#include "/sys/ins/pgm.ins.c"

/**************************************************************************
**
**  NAME:  uagetenv
**
**      contains:		uu_apollo_getenv
**
**  COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**   MODULE NAME AND RELEASE LEVEL 
**       uagetenv.c , 25.1
**       uagetenv.c , 2.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:51
**
**************************************************************************/
/**************************************************************************
**
**  E_FUNCTION:  char *uu_apollo_getenv(variable)
**
**      Find an APOLLO DOMAIN symbolic name (must be passed as
**		a command line argument in the form name=value).
**
**  PARAMETERS   
**
**      INPUT  :  variable	:	name of variable to be checked
**
**      OUTPUT :  none
**
**  RETURNS      :  address of contents of variable if found
**						  or NULL if not found
**
**  SIDE EFFECTS :  none
**
**  WARNINGS     :  none
**
**************************************************************************/

#define BUFLEN 80

char *uu_apollo_getenv(variable)
char *variable;						/* name of variable to be checked	*/
{
	int len;
	char *str1;
	char *str2;
	short arg_count;
	status_$t status;
	static char buf[BUFLEN];
	char *strchr();

	arg_count = 1;
	for(;;) {

		/* Get command line argument number arg_count */
		len = pgm_$get_arg(arg_count, buf, status, (short)BUFLEN);
		if( status.all != status_$ok ) break;
     buf[len] = '\0';
		
		/* Find location of = in the string */
		str1 = strchr(buf, '=');
		if( str1 == 0 ) {
			printf("argument error...no '='\n");
			break;
		}
     
		/* Replace equal with string terminator */
		*str1 = '\0';

		/* Set str1 to point to string past the equal */
		++str1;

		if( strcmp(variable, buf) == 0 ) return(str1);
		++arg_count;
	}

	/* No variable of this name found */
	return(NULL);


}
#endif
