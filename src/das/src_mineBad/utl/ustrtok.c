#include "ustdio.h"
#include "ustrings.h"

/********************************************************************* 
**  NAME:  ustrtok.c
**
**      System independent version of Unix V strtok()
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       ustrtok.c , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:55
**
*********************************************************************/

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) ustrtok.c 2.3 11/20/85 14:28:21 single"};
#else
static char uu_sccsident[]={"@(#) ustrtok.c 2.3 11/20/85 14:28:21 double"};
#endif

/*********************************************************************
**    E_FUNCTION     :  char *uu_strtok(string, sep)
**       
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_strtok(string, sep)
char *string;
char *sep;
{
	static char *tok;
	char *end, *rtn;
	int len;


	/* On first call, set tok = string */
	if( string != NULL ) tok = string; 

	/* If nothing left in string, return NULL */
	if( *tok == '\0'  )  return(NULL);

	/* Find sep in string */
	end = tok;
	len = strlen(sep);
	while( *end ) {
		if(strncmp(sep,end,len) == 0) break;
		++end;
	}

	/* We've now found token to return */
	rtn = tok;

	/* Set end of token, and tok for next call to this function */
	if( *end == '\0' ) {			/* We're at end of string */
		tok  = end;
	}
	else {							/* Still stuff left in string */
		*end = '\0';
		tok  = end+len;
	}

	/* Don't return zero length strings (found by two separators together) */
	if(strlen(rtn) == 0) return(uu_strtok(NULL,sep));

	/* Normal return */
	return(rtn);

}
