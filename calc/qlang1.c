/*********************************************************************
**    NAME         :  qlang1.c
**       CONTAINS:
**       	uq_calc2
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       qlang1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:54
*********************************************************************/

#include 	"usysdef.h"
#include		"uhep.h"
#include		"mdcoord.h"
#include		"qlang.h"
#include  	"icalcsem.h"

int	uqi_vmatch();

/*********************************************************************
**
**    E_FUNCTION     :  uq_calc2(ibuf, cdrslt)
**       called by DAS to parse just one input line
**
**    PARAMETERS   
**       INPUT  : 
**          ibuf : input line
**       OUTPUT :  
**          cdrslt: an array contains the result
**
**    RETURNS      : logical flag
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uq_calc2(ibuf, cdrslt)
char  ibuf[];
CRSLT *cdrslt;
{
	static int one=1;		/* static ok since never change value */
	int stack[1000];		/* stack area for uti_parser to use */
	ITSTYPE its[1000];	/* uti_parser returns translation sequence here*/
	int ltr;					/* length of returned translation sequence */
	int irc;					/* return code from ut_lxread or uti_parser */
	int in[200];			/* user input buffer */
	int len;					/* length of fname or input buffer */
	int irkf;				/* in[irkf-1] contains index of last
									matched character position of parse */
	int i, COTYPE;
	UU_REAL   val[3];

	if (UQI_clxconflag == UU_FALSE)
		if	(!(uqi_lxconread()))
			return(UU_FALSE);
	
	irc=0;
	len = strlen (ibuf);
	ut_cin_intern (in, len, ibuf, 1);
	uq_calc2flag = UU_TRUE;
	uq_calc2rslt = UU_FALSE;
		UQI_cer1 = UU_FALSE;   
		UQI_cer2 = UU_FALSE;
		UQI_cfuncflag = UU_FALSE;
		UQ_cfnindex = 0;
		UQI_cstkpt = -1;
		UQI_cinptr = 1;			/* pointer to next symbol in the "in"  */
		UQI_cfpar = -1;

				 /* call the langpak uti_parser routine to parse input */
	uti_parser(UQ_symbol,uq_symlength,in,one,len,UQ_lexicon,stack,&UQ_trace,
				&irkf,its,&ltr,&irc,semmch,uqi_vmatch,NULL);

	uqi_resetstack();
	uq_calc2flag = UU_FALSE;
	/* irc=0 if parse successful, else see page 128 of langpak book*/
	if (UQI_cdebug)
		{
	    printf("uti_parser returned irc=%d\n",irc);
		 printf("UQI_cer1=%d, UQI_cer2=%d \n", UQI_cer1, UQI_cer2);
		}

	if ((uq_calc2rslt==UU_FALSE)&&(irc==0)&&(UQI_cer2==UU_FALSE))
		/* gmessage (D_ksws, " error - illegal expression, action aborted"); */
		uu_uerror0 (UQ_CALCERROR, 4);
	if (irc == 1)
	  {
		if (UQI_cer1)
			/* gmessage (D_ksws, " error - undefined name "); */
			uu_uerror0 (UQ_CALCERROR, 1);
		else
		   /* gmessage (D_ksws, " error - syntax error!! "); */
		   uu_uerror0 (UQ_CALCERROR, 2);
     }	
	if ((irc == 1)||(UQI_cer2==UU_TRUE)||(uq_calc2rslt==UU_FALSE))
		return (UU_FALSE);
	else
		{
		 cdrslt->ctype = UQI_cdval[0];
		 if (cdrslt->ctype < UQ_SCALAR)
			{
/*----------	DAS needs conctruction coordinate
			 COTYPE = (cdrslt->ctype > UM_SPHERICAL);
			 um_ccstomcs (COTYPE, &UQI_cdval[1], val);
------------*/
			 for (i=0; i<3; i++)
				 cdrslt->val.cval[i] = UQI_cdval[i+1];
			}
		 else if (cdrslt->ctype == UQ_SCALAR)
					cdrslt->val.sval = UQI_cdval[1];
		 return (UU_TRUE);
		}
}		/* uq_calc2 */
