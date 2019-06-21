
/*********************************************************************
**    NAME         :  cstblist.h
**       CONTAINS:
**       uqi_stblist
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       cstblist.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:12
*********************************************************************/

#ifndef CSTBLISTH

#include "usysdef.h"
#include "calcom.h"
#include "mdcoord.h"
#include "uhep.h"

extern  int   uq_calc2flag;


/*********************************************************************
**    I_FUNCTION     :  uqi_stblist ()
**       print out the contents of the symbol table 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/


uqi_stblist ()

{
	int  i, j, k, len, COTYPE;
	char cbuf[80];
	UU_REAL	temp1, temp2, val[3], val1[3];

	if (uq_calc2flag == UU_FALSE)
	if (tbindex == 0)
	  {
		/* gmessage (D_ksws, " empty table "); */
		uu_uerror0 (UQ_CALCERROR, 22);
		ud_wrwin("\n");
	  }
	else
	{
	ud_wrwin("\n");
	for (i=0; i<tbindex; i++)
	{
	 if (symtab[i].ttype != UQ_FUNCARG)
	 {
	 k = 0;
	 uqi_prsymm (&len, symtab[i].symbol, cbuf, &k);
	 for (j=len; j<=UQ_SYMSIZE; j++)
		  sprintf (&cbuf[k++], " ");
	 sprintf (&cbuf[k], "   ");
	 COTYPE = (symtab[i].ttype > UM_SPHERICAL);
	 k = strlen(cbuf);
#define  stbv    symtab[i].val
	 switch (symtab[i].ttype)
	 {
	  case	 UQ_SCALAR  :
						sprintf (&cbuf[k], "SCALAR       ");
						k = strlen(cbuf);
						if (UQI_sciflag == UQ_FLT)
						   sprintf (&cbuf[k], UQ_dpt, stbv[0]);
						else  sprintf (&cbuf[k], "%e ", stbv[0]);
						k = strlen(cbuf);
						sprintf (&cbuf[k], "\n");
						ud_wrwin(cbuf);
						break;

	 case    UM_CARTESIAN :
	 case    UM_VCARTESIAN :
						if (symtab[i].ttype == UM_CARTESIAN)
							sprintf (&cbuf[k], "CARTESIAN    < ");
						else
							sprintf (&cbuf[k], "VCARTESIAN    < ");
						/* um_mcstoccs (COTYPE, stbv, val);	*/
						k = strlen(cbuf);
						for (j=0; j<3; j++)
						{
						if (j > 0)
						 {
						  sprintf (&cbuf[k], ", ");
						  k = k + 2;
						 }
						if (UQI_sciflag == UQ_FLT)
						   sprintf (&cbuf[k], UQ_dpt, stbv[j]);
						else  sprintf (&cbuf[k], "%e", stbv[j]);
						k = strlen(cbuf);
						}	
						sprintf (&cbuf[k], " >\n");
						ud_wrwin( cbuf);
						break;

	 case    UM_CYLINDRICAL :
	 case    UM_VCYLINDRICAL :
						if (symtab[i].ttype == UM_CYLINDRICAL)
							sprintf (&cbuf[k], "CYLINDRICAL  < ");
						else
							sprintf (&cbuf[k], "VCYLINDRICAL  < ");
					/*----
						um_cotovc (stbv, symtab[i].ttype, val);
						um_mcstoccs(COTYPE, val, val1);
						um_vctoco (val1, symtab[i].ttype, val);
					--- */
						k = strlen(cbuf);
						if (UQI_angflag == UQ_DEG)
							val[1] = val[1] * UQ_RADIAN;
						for (j=0; j<3; j++)
						{
						if (j > 0)
						 {
						  sprintf (&cbuf[k], ", ");
						  k = k + 2;
						 }
						if (UQI_sciflag == UQ_FLT)
						   sprintf (&cbuf[k],UQ_dpt, stbv[j]);
						else  sprintf (&cbuf[k], "%e", stbv[j]);
						k = strlen(cbuf);
						}
						sprintf (&cbuf[k], " >\n");
						ud_wrwin( cbuf);
						break;
					
	 case    UM_SPHERICAL :
	 case    UM_VSPHERICAL :
						if (symtab[i].ttype == UM_SPHERICAL)
							sprintf (&cbuf[k], "SPHERICAL    < ");
						else
							sprintf (&cbuf[k], "VSPHERICAL    < ");
					/*---
						um_cotovc (stbv, symtab[i].ttype, val);
						um_mcstoccs(COTYPE, val, val1);
						um_vctoco (val1, symtab[i].ttype, val);
					--- */
						k = strlen(cbuf);
						if (UQI_angflag == UQ_DEG)
							{
							 val[1] = val[1] * UQ_RADIAN;
							 val[2] = val[2] * UQ_RADIAN;
							}
						for (j=0; j<3; j++)
						{
						if (j > 0)
						 {
						  sprintf (&cbuf[k], ", ");
						  k = k + 2;
						 }
						if (UQI_sciflag == UQ_FLT)
						   sprintf (&cbuf[k], UQ_dpt, stbv[j]);
						else  sprintf (&cbuf[k], "%e", stbv[j]);
						k = strlen(cbuf);
						}
						sprintf (&cbuf[k], " >\n");
						ud_wrwin( cbuf);
						break;
#undef  stbv

	 case   UQ_FUNCTION  :
#define	  tfptr	symtab[i].func

						sprintf (&cbuf[k], "FUNCTION     args: ");
						k = strlen(cbuf);
						for (j=1; j<=tfptr->arg[0]; j++)
							{ 
							 uqi_prsymm(&len, symtab[tfptr->arg[j]].symbol,cbuf,&k);
							 sprintf(&cbuf[k++], " ");
							}
						sprintf (&cbuf[k], "  --> ");
						k = strlen (cbuf);
						uqi_prsymm(&len, tfptr->funcbuf, cbuf, &k);
#undef	 tfptr
						k = strlen(cbuf);
						sprintf(&cbuf[k], "\n");
						ud_wrwin( cbuf);
						break;

	 case   UQ_FUNCARG  :
						sprintf (&cbuf[k], "PARAMETER  - VALUE UNKNOWN \n");
						ud_wrwin( cbuf);
						break;;

	 default		:
				sprintf (&cbuf[k], " UNDEFINED SYMBOL\n");
				/*  ud_wrwin( cbuf);		*/
				break;
	}	/* switch  */
	}	/* if    */
	}	/* for */
	}  /* else */
}		/* uqi_stblist */



/****************************************************************/
/**
	NAME :		uqi_prsymm
			decode the string to ascii code and print it out

	PARAMETERS :
			input
					: sym - string to be decoded
			output:
					: len - length of the string

**/


/****************************************************************/

uqi_prsymm (len, sym, cbuf, k)
char	sym[];
int  *len, *k;
char cbuf[];

{
	 int  j, ch;

	 /* *len = uqi_symlen( sym ); */
	 *len = strlen( sym );
	 for (j=0; j<*len; j++)
	 {
	  ch = sym[j];
	  sprintf (&cbuf[(*k)++], "%c", ch);
	 }
}		/* uqi_prsymm */





#define CSTBLISTH
#endif
