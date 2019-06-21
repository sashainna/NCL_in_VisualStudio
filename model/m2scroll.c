
/*********************************************************************
**    NAME         :  mscroll
**       CONTAINS:
**			um_pscroll
**			um_lenstr
**			um_concat
**			um_p_ary
**			um_getline
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2scroll.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:48
*********************************************************************/
#include "usysdef.h"
#include "ustdio.h"
#include "udebug.h"
#include "usysg.h"
#include "mromcom.h"
#include "mfcifdef.h"
#include "mdebug.h"


#define  MAXSTR 120

static int first = -1;

/*********************************************************************
**    I_FUNCTION: um_open_psfile()
**			open debugging file "psfile"
**    PARAMETERS   
**       INPUT: 
**          none
**       OUTPUT:  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_open_psfile()

	{
	if (first < 0)
		{
		UM_psfile = fopen("psfile","w");
		first = 1;
		}
	}

/*********************************************************************
**    I_FUNCTION: um_pscroll1(mask, str)
**       Prints the string (STR) on the auxiliary terminal
**			and to the file "psfile" if the debug mask includes MASK.
**    PARAMETERS   
**       INPUT: 
**          str            string to be printed.
**       OUTPUT:  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : prints to a terminal and to a file.
**    WARNINGS     : none
*********************************************************************/
um_pscroll1(mask, str)
	int mask;
	char *str;

	{
	if (mask & UU_debmask)
		{
		if (first < 0)
			{
			UM_psfile = fopen("psfile","w");
			first = 1;
			}
		fprintf(UM_psfile,"%s\n",str);
		printf("%s\n",str); 
		fflush(UM_psfile);
		}
	}

/*********************************************************************
**    I_FUNCTION: um_pscroll(str)
**       Prints the string, "str", on the auxiliary terminal and to the file
**			"psfile".
**    PARAMETERS   
**       INPUT: 
**          str            string to be printed.
**       OUTPUT:  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : prints to a terminal and to a file.
**    WARNINGS     : none
*********************************************************************/
um_pscroll(str)
	char *str;

	{
	if (UU_MTRC & UU_debmask)
		{
		if (first < 0)
			{
			UM_psfile = fopen("psfile","w");
			first = 1;
			}
		fprintf(UM_psfile,"%s\n",str);
		printf("%s\n",str); 
		fflush(UM_psfile);
		}
	}

int um_lenstr(s1)
	char *s1;

	{
	int len;
	int i;

	len = 0;
	for (i=0; (s1[i] != '\0'); i++) len++;
	return (len);
	}


/*********************************************************************
**    I_FUNCTION: um_concat(s3,s1,s2)
**       Concatenates strings s1 and s2 and puts the resulting string in s3.
**    PARAMETERS   
**       INPUT  : 
**				s1           first string to be concatenated.
**
**				s2           second string to be concatenated.
**          
**       OUTPUT :  
**          s3           resulting concatenated string.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_concat(s3,s1,s2)
	char *s1;
	char *s2;
	char *s3;

	{
	int i,len;

	len = 0;
	for (i=0; (s1[i] != '\0') && (len < ( MAXSTR-1)); i++, len++) s3[len] = s1[i];
	for (i=0; (s2[i] != '\0') && (len < ( MAXSTR-1)); i++,len++) s3[len] = s2[i];
	s3[len] = '\0';
	}

/*********************************************************************
**    I_FUNCTION: um_p_ary(fmt,str,nvar,var)
**
**       Debugging facility that allows messages and the value of variables
**			to be printed out.
**    PARAMETERS   
**       INPUT: 
**          fmt            flag indicating the format in which the values
**                         pointed to by "var" are to be printed; the 
**                         following values are allowed:
**                            UM_PINT    causes integer format to be used;
**                            UM_PFLOAT  causes real (double) format to be used;
**                            UM_PHEX    causes hex format to be used.
**										UM_PLOGICAL causes UU_LOGICAL (i.e. char) to be 
**														printed as an int (i.e. 0 or 1).
**				str            pointer to the character string to be printed in
**                         front of the values.
**
**				nvar           the number of values to be printed.
**
**				var            pointer to the values to be printed.
**                            
**       OUTPUT:  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : prints to both the auxiliary terminal and to a file;
**                   the file's name is "psfile".
**    WARNINGS     : none
*********************************************************************/
um_p_ary(fmt,str,nvar,var)
	int fmt;
	char *str;
	int nvar;
	UU_REAL *var;
	{
	int *ivar;
	UU_LOGICAL *lvar;
	int i;
	char s1[MAXSTR];
	char s2[MAXSTR];

	sprintf(s1,"%-15s",str);
	switch (fmt)
		{
		case UM_PINT: 
			ivar = (int*) var;
			for (i=0; i<nvar; i++)
				{
				sprintf(s2," %d ",ivar[i]);
				um_concat(s1,s1,s2);
				if ((i % 6) == 5) 
					{
					um_pscroll(s1);
					sprintf(s1,"               ");
					}
				}
			break;
		case UM_PFLOAT: 
			for (i=0; i<nvar; i++)
				{
				sprintf(s2," %g ",var[i]);
				um_concat(s1,s1,s2);
				if ((i % 6) == 5) 
					{
					um_pscroll(s1);
					sprintf(s1,"               ");
					}
				}
			break;
		case UM_PHEX: 
			for (i=0; i<nvar; i++)
				{
				sprintf(s2," %8x ",var[i]);
				um_concat(s1,s1,s2);
				if ((i % 6) == 5) 
					{
					um_pscroll(s1);
					sprintf(s1,"               ");
					}
				}
			break;
		case UM_PLOGICAL:
			lvar = (UU_LOGICAL*) var;
			for (i=0; i<nvar; i++)
				{
				sprintf(s2," %d ",(int)(lvar[i]));
				um_concat(s1,s1,s2);
				if ((i % 6) == 5) 
					{
					um_pscroll(s1);
					sprintf(s1,"               ");
					}
				}
			break;
		default: 
			for (i=0; i<nvar; i++)
				{
				sprintf(s2," %8x ",var[i]);
				um_concat(s1,s1,s2);
				if ((i % 6) == 5) 
					{
					um_pscroll(s1);
					sprintf(s1,"               ");
					}
				}
			break;
		}
		um_pscroll(s1);
	}

um_getline(s)
	char s[];

	{

	if (UM_rombuf.cur_cmd > UM_rombuf.num_cmd)
		{
		strcpy(s,"@Q");
		 UM_rombuf.cur_cmd = UM_rombuf.num_cmd;
		}
	else if (UM_rombuf.cur_cmd == UM_rombuf.num_cmd)
		{
		strcpy(s,"UNICAD");
		UM_rombuf.cur_cmd++;
		}
	else
		{
		strcpy(s,UM_rombuf.cmd[UM_rombuf.cur_cmd]);
		UM_rombuf.cur_cmd++;
		}
	return(um_lenstr(s));
	}

utxrwp(prompt,plen,buffer,bufmax,buflen)
	char *prompt;
	int *plen;
	char *buffer;
	int *bufmax;
	int *buflen;

	{
	char s1[MAXSTR],s2[MAXSTR],s3[MAXSTR];
	int i,j;
	int len;

	len = um_getline(s2);
	if (UU_debmask & UU_MTRC)
		{
		for (i=0; (i<*plen) && (i<( MAXSTR-1)); i++) s1[i] = prompt[i];
		s1[i] = '\0';
		um_concat(s3,s1,s2);
		um_pscroll(s3);
		}
	if (len < *bufmax) *buflen = len; else *buflen = *bufmax;
	for (i=0, j=0; i<*buflen; i++, j=j+4)
		{
		buffer[j] = s2[i];
		buffer[j+1] = ' ';
		buffer[j+2] = ' ';
		buffer[j+3] = ' ';
		}
	}

uscrol(buffer,buflen)
	char *buffer;
	int *buflen;

	{
	char s1[ MAXSTR];
	int i;
	if (UU_debmask &  UU_MTRC)
		{
		for (i=0; (i<*buflen) && (i<( MAXSTR-1)); i++) s1[i] = buffer[i];
		s1[i] = '\0';
		um_pscroll(s1);
		}
	}

ufiddl(n,x)
	int *n;
	char *x;

	{

/*
	sprintf(s1,"UFIDDL: %d",*n);
	um_pscroll(s1);
*/
	}

#undef MAXSTR
