/*********************************************************************
**    NAME         :  cvmatch.h
**       CONTAINS:
**       cvmatch
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       cvmatch.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:12
*********************************************************************/

#ifndef CVMATCHH

#include "usysdef.h"
#include "calcom.h"
#include "tlangtool.h"


/*********************************************************************
**    I_FUNCTION :  static  vmatch (in, kf, kl, its, ltr, irc)
**			get an specified string from the input buffer and check see
**			whether it is already in the symbol tabl
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
int  in[] ;
int  *kf, *kl, *ltr, *irc;
ITSTYPE	its[];

{
    char symtext[UQ_SYMSIZE];
    int  i, index;
	 int nextpt;
	 UU_REAL  vval[3];

	 if (UQI_sdebug)
	    printf ("** in vmatch - in[] = ");
#define nextpt  (in[*kf]!='.')&&(in[*kf]!='+')&&(in[*kf]!='*')&&\
			 		 (in[*kf]!=')')&&(in[*kf]!='-')&&(in[*kf]!='/')&&\
			 		 (in[*kf]!='^')&&(in[*kf]!='(')&&(in[*kf]!=',')&&\
					 (in[*kf]!='>')
    i = 0;
    while ((i<UQ_SYMSIZE-1)&&(*kf<=*kl)&&(in[*kf]!=' ')&&(nextpt))
	     {
			 if (UQI_sdebug)
			    printf ("%c, ", in[*kf]);
          symtext[i++] = in[(*kf)++];
        }
    if (UQI_sdebug)
       printf ("\n");

	 while ((*kf<*kl)&&(in[*kf]==' '))    /* skip blank */
	 (*kf)++;

	if (UQI_sdebug)
	   printf ("&& next in[kf] = %c \n", in[*kf]);
   symtext[i] = '\0';
   
   if (uqi_findsym (symtext, &index))
      {
		 if (UQI_sdebug)
		    printf ("** after vmatch-uqi_findsym - index = %d \n", index);
		 its[++(*ltr)].lint = 4;
		 its[++(*ltr)].lint = 1;
		 its[++(*ltr)].lint = index;
       *irc = 1;
		 UQI_cer1 = UU_FALSE;
      }
  else
		{
       *irc = 0;
		 UQI_cer1 = UU_TRUE;
      }

}	/* vmatch */



#define CVMATCHH
#endif
