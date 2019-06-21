
/*********************************************************************
**    NAME         :  dvmatch.h
**       CONTAINS:
**				vmatch routine for the das input
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       dvmatch.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:15
*********************************************************************/

#ifndef DVMATCHH

#include "usysdef.h"
#include "calcom.h"
#include "tlangtool.h"


extern	int	UDI_dfsflag;
extern   char	UDI_dftext[];
static	int	sdebug = UU_FALSE;


/*********************************************************************
**    I_FUNCTION     :  
**       get an specified string from the input buffer and check see
**       whether it is a function name and already in the symbol table
**    PARAMETERS   
**       INPUT  : 
**          in - input buffer
**          kf - index to the input buffer
**          kl - index to the last character in the input buffer
**       OUTPUT :  
**          its - an array contains the translation code
**          ltr - index to the "its"
**          irc - a flag for symbol found or unfound
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/


static  vmatch (in, kf, kl, its, ltr, irc)
int  in[];
int  *kf, *kl, *ltr, *irc;
ITSTYPE	its[];

{
    int  i, j;
	 UQ_qstb	*index;
	 int	textbuf[9];

	 if (sdebug)
	    printf ("** in vmatch - in[] = ");
    i = 0;
    while ((i<8)&&(*kf<=*kl)&&(in[*kf]!=' '))
	     {
			 if (sdebug)
			    printf ("%d, ", in[*kf]);
          textbuf[i++] = in[(*kf)++];
        }
    if (sdebug)
       printf ("\n");

	 while ((*kf<*kl)&&(in[*kf]==' '))    /* skip blank */
	 (*kf)++;

	if (sdebug)
	   printf ("&& next in[kf] = %d \n", in[*kf]);
	for (j=0; j<i; j++)
		UDI_dftext[j] = textbuf[j];
	UDI_dftext[j] = '\0';
   if (sdebug)
		printf ("UDI_dftext = %s\n", UDI_dftext);

   if (uqi_gsym (UDI_dftext, &index))
      {
		 if (sdebug)
		  {
		   printf ("** after vmatch-uqi_gsym - sym = %d,%d \n", index->symbol[0],index->symbol[1]);
			printf("**type = %d,\n", index->ttype);
		  }

		 if (index->ttype == UQ_FUNCTION)
			 UDI_dfsflag = UU_TRUE;
		}
	if (UDI_dfsflag)
       *irc = 1;
   else
       *irc = 0;

}	/* vmatch */





#define DVMATCHH

#endif
