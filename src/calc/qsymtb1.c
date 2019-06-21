
/*********************************************************************
**    NAME         :  csymtb.c
**       CONTAINS:
**				uq_gsymval	 
**				uq_addstbval
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       qsymtb1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:55
*********************************************************************/

#include		"uctype.h"
#include		"ustdio.h"
#include 	"usysdef.h"
#include		"mdcoord.h"
#include  	"cdef.h"
#include  	"uhep.h"
#include		"tlangtool.h"
#include		"mdrel.h"
#include		"udebug.h"
#include		"qsymtb.h"

extern UQ_qstb		UQ_symtab[];
extern int			UQ_tbindex;
char	*uu_toolmalloc();

/*********************************************************************
**    I_FUNCTION     :  uq_gsymval (symb, aptr)
**       get the information of a certain symbol which is in ascii code
**			with type of SCALAR or COORDINATE.
**    PARAMETERS   
**       INPUT  : 
**          symb - interested symbol
**       OUTPUT :  
**          cdrslt - structure contains the information of the symbol
**    RETURNS      : none
**          UU_TRUE if the symbol is found
**          FALSE if the symbol is unfound or type is not SCALAR OR
**						COORDINATE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uq_gsymval (symb, cdrslt)
char  symb[];
CRSLT *cdrslt;
{
	int	len, i, j, index;
	char   xsym[80];
	int  COTYPE;
	UU_REAL   val[3];

	len = strlen(symb);
	for (i=0; i<len; i++)
	{
	 if ( isupper(symb[i]) )
		 xsym[i] = tolower(symb[i]);
	 else
	    xsym[i] = symb[i];
	}

	if (len < UQ_SYMSIZE)
		xsym[len] = '\0';
	else
		xsym[UQ_SYMSIZE-1] = '\0';

	if (uqi_findsym(xsym, &index))
		{
		 cdrslt->ctype = UQ_symtab[index].ttype;
		 if (cdrslt->ctype < UQ_SCALAR)
			{
			 COTYPE = (cdrslt->ctype > UM_SPHERICAL);
			 /* um_ccstomcs (COTYPE, UQ_symtab[index].val, val);	*/
			 for (i=0; i<3; i++)
				 cdrslt->val.cval[i] = UQ_symtab[index].val[i];
			}
		 else if (cdrslt->ctype == UQ_SCALAR)
					cdrslt->val.sval = UQ_symtab[index].val[0];
				else
					return(UU_FALSE);
		 return (UU_TRUE);
		}
	else
		return (UU_FALSE);

}		/* uq_gsymval */

/*********************************************************************
**    I_FUNCTION     :  uq_addstbval (symb, lasttype, lastval)
**       add a new symbol and its value to the symbol table or change its
**       value if the symbol is already existed.
**    PARAMETERS   
**       INPUT  : 
**          symb - the input symbol
**          lasttype - the value type
**          lastval - the input value
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uq_addstbval (symb, lasttype, lastval)
char  symb[];
int	lasttype;
UU_REAL	lastval[3];
{
	int	len, i, j, index, isnew;
	char   xsym[20];

	isnew = 0;				/* to be updated */
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

	if (uqi_findsym(xsym, &index) == UU_FALSE)
		{
		 index = UQ_tbindex++;
		 /* uqi_cstrcpy (UQ_symtab[index].symbol, xsym); */
		 strcpy (UQ_symtab[index].symbol, xsym);
		 isnew = 1;			/* to be created */
		}
   uqi_putsymval (index, lasttype, lastval, isnew);
}		/* uq_addstbval */
