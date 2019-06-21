
/*********************************************************************
**    NAME         :  csymtb.c
**       CONTAINS:
**				uqi_findsym 
**				uqi_gsym
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       qsymtb2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:55
*********************************************************************/

#include		"uctype.h"
#include		"ustdio.h"
#include 	"usysdef.h"
#include		"mdcoord.h"
#include 	"calcom.h"
#include  	"cdef.h"
#include  	"uhep.h"
#include		"tlangtool.h"
#include		"mdrel.h"
#include		"udebug.h"
#include		"qsymtb.h"

extern UQ_qstb	UQ_symtab[UQ_TBSIZE];
extern int		UQ_tbindex;
char	*uu_toolmalloc();


/*********************************************************************
**    I_FUNCTION     :  uqi_findsym (symtext, index)
**       search through the symbol table to find a asked sumbol
**    PARAMETERS   
**       INPUT  : 
**          symtext - symbol to be found
**       OUTPUT :  
**          index - address of the symbol in the symbol table
**    RETURNS      : logical flag
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_findsym (symtext, index)
char symtext[];
int *index;
{
    int   i , cmp;

	uu_denter(UU_DTRC,(us,"uqi_findsym,symtext=%s,UQ_tbindex=%d",symtext,UQ_tbindex));
	uu_dexit;
    cmp = -1;
	 i = 0;
 	while (i < UQ_tbindex  && (cmp=strcmp(symtext, UQ_symtab[i++].symbol)) != 0);
    if  (cmp == 0)
        { *index = i - 1;
			 return (UU_TRUE);
	     }
    else  return (UU_FALSE);
}	/* uqi_findsym  */
 
/*********************************************************************
**    I_FUNCTION     :  uqi_gsym (symb, aptr)
**       get the information of a certain symbol which is in ascii code
**    PARAMETERS   
**       INPUT  : 
**          symb - interested symbol
**       OUTPUT :  
**          aptr - address of the symbol in the symbol table
**    RETURNS      : none
**          UU_TRUE if the symbol is found
**          FALSE if the symbol is unfound
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_gsym (symb, aptr)
char  symb[];
int  *aptr;
{
	int	len, i, index;
	int   xsym[80];

	len = strlen(symb);
	for (i=0; i<len; i++)
	{
	 if ( isupper(symb[i]) )
		 xsym[i] = tolower(symb[i]);
	 else
	    xsym[i] = symb[i];
/*	 j = tsym[i] & 127;
	 xsym[i] = UT_dctoic[j];
*/
	}

	if (len < UQ_SYMSIZE)
		xsym[len] = '\0';
	else
		xsym[UQ_SYMSIZE-1] = '\0';

	if (uqi_findsym(xsym, &index))
		{
		 *aptr = (int) &UQ_symtab[index];
		 return (UU_TRUE);
		}
	else
		return (UU_FALSE);
}		/* uqi_gsym */
