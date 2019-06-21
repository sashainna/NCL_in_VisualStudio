
/*********************************************************************
**    NAME         :  csymtb.c
**       CONTAINS:
**       	uqi_caddsym
**				uqi_cevalfunc
**				uqi_cfaddsym
**				uqi_cfread
**				uqi_cfuncsym   
**				uqi_chkey
**				uqi_coeval
**				uqi_cstrcmp
**				uqi_cstrcpy
**				uqi_instrcopy
**				uqi_prsymm
**				uqi_putsymval
**				uqi_resumesymv
**				uqi_stblist
**				uqi_svsymval
**				uqi_symlen 	 
**				uqi_tbreset
**				uqi_varva
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       qsymtb.c , 25.1
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

UQ_qstb	UQ_symtab[UQ_TBSIZE];
int		UQ_tbindex;
char	*uu_toolmalloc();
void uqi_prsymm ();
void uqi_svsymval ();
void uqi_resumesymv ();
/*********************************************************************
**    I_FUNCTION     :  uqi_caddsym (in, kf )
**       add a new symbol to the symbol table 
**    PARAMETERS   
**       INPUT  : 
**          in - input buffer
**          kf - index to the input buffer
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uqi_caddsym (in, kf )
int in[];
int  kf;
{
    int   i = 0;
    int   index;
    char   symtext[UQ_SYMSIZE];
	 UU_REAL  cfindex;
	 UU_LOGICAL	isinvalid;
	 char	us[132];

	 isinvalid = UU_FALSE;
    if (uqi_instrcopy ( symtext, in, kf, &isinvalid)==UU_FALSE)
		{
		 if (isinvalid)
		 	/* gmessage (D_ksws, " error - invalid variable name"); */
		 	uu_uerror0 (UQ_CALCERROR, 50); 
		 UQI_cer2 = UU_TRUE;
		 uqi_sc_push(UQ_SCALAR, (UU_REAL) 0.0);
		 return;
		}
	 /* if ((symtext[0]>=240)&&(symtext[0]<=249)) */
	 if ((symtext[0]>='0')&&(symtext[0]<='9')) 
		{
	 	 UQI_cer2 = UU_TRUE;
	 	 uqi_sc_push(UQ_SCALAR, (UU_REAL) 0.0);
/* gmessage (D_ksws, " error - Names must start with non-digit characters"); */
		 uu_uerror0 (UQ_CALCERROR, 47);
		 return;
		}
/*	 if (symtext[0]==198)   */
	 if (symtext[0]=='f')
		{
	 	 UQI_cer2 = UU_TRUE;
	 	 uqi_sc_push(UQ_SCALAR, (UU_REAL) 0.0);
/* gmessage (D_ksws, " error - Names starting with 'f' are reserved for functions"); */
		 uu_uerror0 (UQ_CALCERROR, 9);
		 return;
		}
    if (uqi_findsym ( symtext, &index) == UU_FALSE)
	 	 if (uqi_chkey(symtext))   /* check key word  */
		 	{
		 	 UQI_cer2 = UU_TRUE;
		 	 uqi_sc_push(UQ_SCALAR, (UU_REAL) 0.0);
		 	 /* Reserved word cannot be used as a symbol variable */
		 	 uu_uerror0 (UQ_CALCERROR, 56);
			}
		 else
			{
       	 index = UQ_tbindex++;
        	 /* uqi_cstrcpy (UQ_symtab[index].symbol, symtext); */
        	 strcpy (UQ_symtab[index].symbol, symtext);
			 UQ_symtab[index].rel_num = UQ_CALC_REL;
			 UQ_symtab[index].no_func = 0;
			 UQ_symtab[index].func = NULL;
			 ur_create_data(&UQ_symtab[index]);
	uu_denter2(UU_DTRC,(us,"addsym,after create key=%d",UQ_symtab[index].key));
	uu_dexit;
			 if (UQI_sdebug)
			 for (i=0; i<UQ_SYMSIZE; i++)
			 printf ("** UQ_symtab - %c \n", UQ_symtab[index].symbol[i]);
	      }
				/* can't use a query name as a variable name */
	else if (UQ_symtab[index].ttype==UQ_QUERYFUNC)	
		 	{
		 	 UQI_cer2 = UU_TRUE;
		 	 uqi_sc_push(UQ_SCALAR, (UU_REAL) 0.0);
		 	 /* Reserved word cannot be used as a symbol variable */
		 	 uu_uerror0 (UQ_CALCERROR, 56);
			}
	 cfindex = index;
    uqi_sc_push (UQ_SCALAR, cfindex);
}      /* addsym  */

/*********************************************************************
**    I_FUNCTION     :  uqi_chkey (sym)
**       before adding a new symbol to symbol table check whether that
**       is a key word
**    PARAMETERS   
**       INPUT  : 
**          sym - symbol to be checked
**       OUTPUT :  
**          output
**    RETURNS      : logical flag
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_chkey (sym)
char *sym;
{
	int	i, found;

	found = UU_FALSE;
	for (i=0; ((i<18)&&(found==UU_FALSE)); i++)
		/*if (uqi_cstrcmp(sym,keywd[i]) == 0) */
		if (strcmp(sym,keywd[i]) == 0)
			found = UU_TRUE;
	return (found);
}	/* uqi_chkey */

/*********************************************************************
**    I_FUNCTION     :  uqi_putsymval ( index, type, val, isnew)
**       put the symbol's information to the symbol table
**    PARAMETERS   
**       INPUT  : 
**          index - index of the symbol in the symbol table
**          type  - type of the symbol 
**          val   - value of the symbol
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_putsymval ( index, type, val, isnew)
int  index, type;
UU_REAL  val[];
int	isnew;		/* new or old variable */
{
	int  i, COTYPE;

	uu_denter(UU_DTRC,(us,"uqi_putsymval,isnew=%d,type=%d",isnew,type));
	uu_dexit;
	switch (type)
	{
	 case  UQ_SCALAR  :
					UQ_symtab[index].ttype = type;
					UQ_symtab[index].val[0] = val[0];
					break;

	 case  UM_CARTESIAN :
	 case  UM_CYLINDRICAL :
	 case  UM_SPHERICAL :
	 case  UM_VCARTESIAN :
	 case  UM_VCYLINDRICAL :
	 case  UM_VSPHERICAL :
					COTYPE = (type<=UM_SPHERICAL)? UU_FALSE : UU_TRUE;
					/*---
					um_cotovc (val, type, val1);
					um_ccstomcs (COTYPE, val1, val2);
					um_vctoco (val2, type, val1);
					--- */
					UQ_symtab[index].ttype = type;
					for (i=0; i<3; i++)
						{
						 UQ_symtab[index].val[i] = val[i];
						 if (UQI_sdebug)
							printf ("putsym - %f,\n", UQ_symtab[index].val[i]);
					   }
					break;

	 default 	  :
					/* gmessage (D_ksws, " error - undefined type "); */
					uu_uerror0 (UQ_CALCERROR, 7);
					break;
	}	/* switch */

	if (isnew == 1)
	  {
		UQ_symtab[index].rel_num = UQ_CALC_REL;
		UQ_symtab[index].no_func = 0;
		UQ_symtab[index].func = NULL;
	uu_denter2(UU_DTRC,(us,"uqi_putsymval,before create isnew =%d", isnew));
	uu_dexit;
		ur_create_data(&UQ_symtab[index]);
	uu_denter2(UU_DTRC,(us,"putsymval,after create key=%d",UQ_symtab[index].key));
	uu_dexit;
	  }
	else if (isnew == 0)
	  {
	uu_denter2(UU_DTRC,(us,"uqi_putsymval,before update isnew=%d,type=%d,val=%g",
				  isnew,UQ_symtab[index].ttype,UQ_symtab[index].val[0]));
	uu_dexit;
		ur_update_data(&UQ_symtab[index]);
   {
	 UQ_qstb	 tmp;
	
		tmp.rel_num = UQ_CALC_REL;
		tmp.no_func = 0;
		tmp.func = NULL;
		tmp.key = UQ_symtab[index].key;
	uu_denter2(UU_DTRC,(us,"before retrieve key=%x",UQ_symtab[index].key));
	uu_dexit;
		ur_retrieve_data_fixed(&tmp);
	uu_denter2(UU_DTRC,(us,"after update,sym=%s,type=%d,val=%g,key=%x",
		 tmp.symbol,tmp.ttype,tmp.val[0],tmp.key));
	uu_dexit;
	}
	  }
}	/* uqi_putsymval */

/*********************************************************************
**    I_FUNCTION     :  uqi_instrcopy (s, in, kf, isinvalid)
**       get a string from the input buffer and copy it to another
**       array
**    PARAMETERS   
**       INPUT  : 
**          in - input buffer
**          kf - index to the input buffer
**       OUTPUT :  
**          s - an array to contain the string
**    RETURNS      : logical flag
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_instrcopy (s, in, kf, isinvalid)
char	s[];
int in[];
int kf;
UU_LOGICAL	*isinvalid;

{
    int  i, dot, legalch;
	 register	nondigit = UU_FALSE;
    
	 dot = UU_FALSE;
	 for (i=UQI_cinptr; i<kf; i++)
     {
		if (in[i] == '.')		
         dot = UU_TRUE;
		else
		   if ((in[i]<'0')||(in[i]>'9'))			/* a non digit? */
				nondigit = UU_TRUE;
	  }

	 if ((dot == UU_TRUE)&&(nondigit))
		{
		 /* gmessage (D_ksws, " error - variable can't contain dot"); */
		 uu_uerror0 (UQ_CALCERROR, 11);
		 cfstop = UU_TRUE;
		 UQI_cinptr = kf + 1;
		 return (UU_FALSE);
		}
	 else
	 {
	 while (UQI_cinptr<kf && in[UQI_cinptr]==' ')   UQI_cinptr++;   /* skip leading blank */
	 if (UQI_cinptr==kf)
		 /* gmessage (D_ksws, " error - no input string!! "); */
		 uu_uerror0 (UQ_CALCERROR, 12);
	 if (in[kf-1] == '(')     
		 kf = kf - 1;			/*   put "(" as an end character  */
	 i = 0;
	   legalch = UU_TRUE;
    while ((UQI_cinptr < kf) && (i < UQ_SYMSIZE-1) && (in[UQI_cinptr] != ' '))
		 {
		  if (UQI_sdebug)
		     printf ("** in[UQI_cinptr] = %c,  ", in[UQI_cinptr]);
		  s[i] = in[UQI_cinptr++];
		  if (!(((s[i]>='a')&&(s[i]<='z')) || ((s[i]>='A')&&(s[i]<='Z')) ||
				  ((s[i]>='0')&&(s[i]<='9')) || (s[i]=='_')))
			  legalch = UU_FALSE;
		  i++;
       }
    if (UQI_sdebug)  printf ("\n");
    s[i] = '\0';
	 UQI_cinptr = kf + 1;
	 if (legalch)
	 	return (UU_TRUE);
	 else
		{
		 /* gmessage (D_ksws, " error - invalid variable name"); */
		 /*uu_uerror0 (UQ_CALCERROR, 50); 	*/
		 /* wait until next legal token is found then put the error message */
		 *isinvalid = UU_TRUE; 
		 /* printf ("Invalid variable name\n");	*/
		 cfstop = UU_TRUE;
		 return (UU_FALSE);
		}
	 }
}	/* strcopy */

/*********************************************************************
**    I_FUNCTION     :  uqi_cstrcpy (s, t)         
**       copy a integer string to another string
**    PARAMETERS   
**       INPUT  : 
**          t - string to be copied
**       OUTPUT :  
**          s - string copied from t
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_cstrcpy (s, t)         /* copy t to s */
int *s, *t;
{
    while ((*s++ = *t++) != '\0');
}		/* uqi_cstrcpy */

/*********************************************************************
**    I_FUNCTION     :  uqi_cstrcmp (s, t)      
**       compare a integer string with another string
**    PARAMETERS   
**       INPUT  : 
**          s, t - two integer strings to be compared
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_cstrcmp (s, t)      /* return <0 if s<t, 0 if s=t, >0 if s>t */
int s[], t[];
{
	 int i;

	 i = 0;
	 while  ( s[i] == t[i])
		 if (s[i++] == '\0')
			 return (0);
    return (s[i] - t[i]);
}		/* uqi_cstrcmp  */

/*********************************************************************
**    I_FUNCTION     :  
**       uq_calculate the length of an integer string
**    PARAMETERS   
**       INPUT  : 
**          sym - an integer string
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_symlen (sym)
int  sym[];
{
	int  i;

	i = 0;
	while (sym[i] != '\0')
		i++;
   return (i);
}		/* uqi_symlen */

/*********************************************************************
**    I_FUNCTION     :  uqi_varval (op, its, ltr)
**       get a symbol's address and its value from the array contains
**       the translation code and then put the value to the symbol
**       table
**    PARAMETERS   
**       INPUT  : 
**          op - specify what kind of element the value is
**          its - an array contains the traslation code
**          ltr - index to "its"
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_varval (op, its, ltr)
int  op, ltr;
ITSTYPE	its[];
{
	int  index, i;

	uu_denter(UU_DTRC,(us,"enter uqi_varval()"));
	if (UQI_sdebug)
	{
	printf ("&& in uqi_varval, ltr= %d\n", ltr);
	for (i=0; i<=ltr; i++)
	printf ("its= %d,",its[i].lint);
	printf ("\n");
	}

	if (op == 1)					/* normal variable             */
		index = its[ltr].lint;
	else if (op==2)				/* component of the coordinate */
			  index = its[ltr-3].lint;

   switch (UQ_symtab[index].ttype)
	 {
	  case  UQ_SCALAR  :
	  case  UQ_QUERYFUNC :
					uu_dprint(UU_DTRC,(us,"val[0]=%g",UQ_symtab[index].val[0]));
     				uqi_sc_push (UQ_symtab[index].ttype, UQ_symtab[index].val[0]);
					uu_dexit;
					return (UU_TRUE);
	 				break;

	  case  UM_CARTESIAN  :
	  case  UM_CYLINDRICAL :
	  case  UM_SPHERICAL  :
	  case  UM_VCARTESIAN  :
	  case  UM_VCYLINDRICAL :
	  case  UM_VSPHERICAL  :
			/*-----
			COTYPE = (UQ_symtab[index].ttype<=UM_SPHERICAL)? UU_FALSE : UU_TRUE;
			um_cotovc (UQ_symtab[index].val, UQ_symtab[index].ttype, val1);
			um_mcstoccs (COTYPE, val1, val2);
			um_vctoco (val2, UQ_symtab[index].ttype, val1);
			uqi_cor_push (UQ_symtab[index].ttype, val1);
			-----*/
			uqi_cor_push (UQ_symtab[index].ttype, UQ_symtab[index].val);
			uu_dexit;
			return (UU_TRUE);

	  case  UQ_FUNCARG  :
			/* gmessage (D_ksws, " error - undefined variable"); */
		 	uu_uerror0 (UQ_CALCERROR, 13);
			uu_dexit;
			return (UU_FALSE);
			break;

	  default  :
			/* gmessage (D_ksws, " error - undefined variable"); */
		 	uu_uerror0 (UQ_CALCERROR, 13);
			uu_dexit;
			return (UU_FALSE);
			break;
	 }
}		/* uqi_varval */

/*********************************************************************
**    I_FUNCTION     :  uqi_coeval ()
**       put the value of the specified coordinate element into
**       symbol table
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uqi_coeval()
{
	UU_REAL	val1[3], val2[3];
	int   typ1, typ2, index, vtype, k, COTYPE;

	uqi_cpop (&typ1, val1);		/* value to be added  */
	uqi_cpop (&typ2, val2);		/* information of the variable */
	if (typ1 == UQ_SCALAR)
	{
	index = (int)val2[0];
	if ((UQ_symtab[index].ttype>=UM_CARTESIAN)&&(UQ_symtab[index].ttype<=UM_VSPHERICAL))
	{
	vtype = (int)val2[1];
	k = (int)val2[2];
	COTYPE = (UQ_symtab[index].ttype > UM_SPHERICAL);

#define	coptr		UQ_symtab[index].val

/*
	um_cotovc (coptr, UQ_symtab[index].ttype, val3);
	um_mcstoccs (COTYPE, val3, val4);
	um_vctoco (val4, vtype, coptr);
*/
	if ((((vtype==UM_CYLINDRICAL)||(vtype==UM_VCYLINDRICAL)) && (k==1)) ||
		 (((vtype==UM_SPHERICAL)||(vtype==UM_VSPHERICAL)) && (k==1)) ||
		 (((vtype==UM_SPHERICAL)||(vtype==UM_VSPHERICAL)) && (k==2)))
		if (UQI_angflag == UQ_DEG)
			val1[0] = val1[0] / UQ_RADIAN;
	/*  *(coptr+k) = val1[0];	*/
	UQ_symtab[index].val[k] = val1[0];
	/*---
	um_cotovc (coptr, vtype, val3);			
	um_vctoco (val3, UQ_symtab[index].ttype, val5);		
	um_ccstomcs (COTYPE, val3, val4);			
	um_vctoco (val4, UQ_symtab[index].ttype, coptr);	
	uqi_panswer (UQ_symtab[index].ttype, val5);
	---*/
	uqi_panswer (UQ_symtab[index].ttype, coptr);
#undef	coptr
	return;
	}
	else
	        /* gmessage (D_ksws, " error - not a coordinate type variable"); */
		uu_uerror0 (UQ_CALCERROR, 14);
	}
	else
		/* gmessage (D_ksws, " error - input value should be type of scalar"); */
		uu_uerror0 (UQ_CALCERROR, 15);
/*	ud_wrwin("\n");*/
	ul_win_out("\n",0);
}		/* uqi_coeval */

/*********************************************************************
**    I_FUNCTION     :  uqi_cfuncsym (op, in, kf)
**       store the defined function name and parameter in a
**       temporary array.
**    PARAMETERS   
**       INPUT  : 
**          op - specify function name or parameters
**          in - input string array
**          kf - index to the next to be examed string
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_cfuncsym (op, in, kf)
int  op, in[], kf;
{
	int  tempkf;
	UU_LOGICAL	isinvalid;

	isinvalid = UU_FALSE;
	cfstop = UU_FALSE;	
	if (op == CFUNCNAME)
	  {
		/*cfstop = UU_FALSE;	*/
		cfunc.argcount = 0;
		uqi_instrcopy(cfunc.arglist[cfunc.argcount],in,kf,&isinvalid);
	  }
	else	if (op == CFUNCARG)
		{
		 tempkf = UQI_cinptr;
		 while (tempkf < kf)
			 {
									/* find ending point of "," or ")"  */
			  while ((in[tempkf] != ',') && (in[tempkf] != ')')) tempkf++;
			  cfunc.argcount++;
			  if (cfunc.argcount < UQ_ARGNUM)
				  {
					uqi_instrcopy(cfunc.arglist[cfunc.argcount], in, tempkf, &isinvalid);
			  		tempkf = UQI_cinptr;
				  }
			  else    tempkf = kf;

			  if (UQI_sdebug)
			    {
		 		  int  i;
		 		  printf ("******  in uqi_cfuncsym  ******\n");
		 		  printf ("argcount = %d \n char = ", cfunc.argcount);
		 		  for (i=0; i<UQ_SYMSIZE; i++)
				  printf (" %d, ", cfunc.arglist[cfunc.argcount][i]);
				  printf ("\n");
	  			 }
			}
	  }
	else
		/* gmessage (D_ksws, " error - undefined function name"); */
		uu_uerror0 (UQ_CALCERROR, 16);
}		/* uqi_cfuncsym */

/*********************************************************************
**    I_FUNCTION     :  uqi_cfaddsym (in, kf, kl)
**       transfer the function information from the temporary array
**       to the symbol table
**    PARAMETERS   
**       INPUT  : 
**          in - input string array
**          kf - index to the next to be examed string
**          kl - index of the last char in the string
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_cfaddsym (in, kf, kl)
int 	in[], kf, kl;
{
	int 	index, aindex;
	register	i, j;
	int	argname = UU_TRUE;
	int	isnew, flgstk[UQ_ARGNUM];

	if (cfstop)
		/* gmessage (D_ksws, " error - invalid variable name"); */
		uu_uerror0 (UQ_CALCERROR, 50); 
	if (cfunc.argcount >= UQ_ARGNUM)
		/* gmessage(D_ksws, " error - too many parameters, action aborted"); */
		uu_uerror0 (UQ_CALCERROR, 17);
	else if ((cfstop == UU_FALSE) && ( uq_calc2flag == UU_FALSE))
	{
#define	symtext	cfunc.arglist[i]

	 for (i=1; i<cfunc.argcount; i++)
/*	if ((symtext[0]>=240)&&(symtext[0]<=249))   */
		if ((symtext[0]>='0')&&(symtext[0]<='9'))
			argname = UU_FALSE;
	 if (!argname)
    	{
		/* gmessage(D_ksws,"error - Argument name must start with non-digit"); */
		 uu_uerror0 (UQ_CALCERROR, 47);
		 cfstop = UU_TRUE;
		}
	 else
	 {
	  i = 0;
	  isnew = UU_FALSE;
	  if ( uqi_findsym(symtext, &index) == UU_FALSE )
		{
		 index = UQ_tbindex++;
		 isnew = UU_TRUE;
		 UQ_symtab[index].rel_num = UQ_CALC_REL;
	/*	 uqi_cstrcpy (UQ_symtab[index].symbol, symtext);	*/
		 strcpy (UQ_symtab[index].symbol, symtext);
		}
 	  else  if ( UQ_symtab[index].ttype == UQ_FUNCTION )
				  uu_toolfree (UQ_symtab[index].func); 
	  UQ_symtab[index].ttype = UQ_FUNCTION;

#define 		tbfptr		UQ_symtab[index].func

	  tbfptr = (UQ_func *) uu_toolmalloc (sizeof(UQ_func));
	  tbfptr->arg[0] = cfunc.argcount;
	  for (i=1; i<=cfunc.argcount; i++)
		{
		 if ( uqi_findsym(symtext, &aindex) == UU_FALSE)
			{
			 aindex = UQ_tbindex++;
	/*		 uqi_cstrcpy( UQ_symtab[aindex].symbol, symtext );	*/
			 strcpy( UQ_symtab[aindex].symbol, symtext );
			 UQ_symtab[aindex].ttype = UQ_FUNCARG;
			 UQ_symtab[aindex].rel_num = UQ_CALC_REL;
			 UQ_symtab[aindex].no_func = 0;
			 UQ_symtab[aindex].func = NULL;
			 flgstk[i] = UU_TRUE;
			}
		 else
			flgstk[i] = UU_FALSE;
		 tbfptr->arg[i] = aindex;
	   }


/*	  tbfptr->funcbuf = (char *) uu_malloc ((kl-kf+2)*sizeof(char));	*/
	  for (j=0; j<=kl-kf; j++)
		 /*  *(tbfptr->funcbuf+j) = in[kf+j];  */
		 tbfptr->funcbuf[j] = in[kf+j];  
	  tbfptr->funcbuf[j] = '\0';

	  UQ_symtab[index].no_func = 1;
	  if (isnew)
		  ur_create_data(&UQ_symtab[index]);
	  else
		  ur_update_data(&UQ_symtab[index]);
/*----------------------
   {
	 UQ_qstb	 tmp;
	
		tmp.rel_num = UQ_CALC_REL;
		tmp.no_func = 0;
		tmp.func = NULL;
		tmp.key = UQ_symtab[index].key;
	uu_denter2(UU_DTRC,(us,"before retrieve key=%x",UQ_symtab[index].key));
	uu_dexit;
		ur_retrieve_data_fixed(&tmp);
	uu_denter2(UU_DTRC,(us,"after update,sym=%s,type=%d,val=%d,key=%x",
		 tmp.symbol,tmp.ttype,tmp.val[0],tmp.key));
	uu_dexit;
	}
----------------------- */
	  for (i=1; i<=cfunc.argcount; i++)
		 if (flgstk[i])
			 ur_create_data(&UQ_symtab[tbfptr->arg[i]]);

#undef	tbfptr
#undef	symtext
	  }	/*  else  */
	} /* else */
/*	ud_wrwin("\n");*/
	ul_win_out("\n",0);

}		/*  uqi_cfaddsym */

/*********************************************************************
**    I_FUNCTION     :  uqi_cevalfunc (op, its, ltr)
**       store function parameters' value in the symbol table
**    PARAMETERS   
**       INPUT  : 
**          its - parser retuned translation sequence
**          ltr - index to the next translation sequence
**          op  - specify function name or parameter
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_cevalfunc (op, its, ltr)
int  op, ltr;
ITSTYPE	its[];
{		
	static   int   argcnt[12];		/* parameter count            */
	static   int   feror;		/* error flag for parameters  */
	static   TPSTK  tmpstk[20][4];  /* temp stack to store parameters */
	static   int   parcnt[20];		/* index to the the temp stack  */

	int	aindex, vtype;
	UU_REAL	val[3];

	if (UQI_sdebug)
	   printf ("~~~ op = %d\n", op);
	if (!UQI_cer2)
	switch (op)
	{
	 case  CFUNCNAME  :
			 if (UQ_symtab[its[ltr].lint].ttype!=UQ_FUNCTION)
				{
					/* Syntax error - illegal function type */
				 uu_uerror0 (UQ_CALCERROR, 57);
				 UQI_cer2 = UU_TRUE;
				}
			 else
				{
				 ++UQI_cfpar;
				 if (UQI_cfpar < 20)
				 {
				 findex[UQI_cfpar] = its[ltr].lint;	 /*index to the func var */	
		 		 argcnt[UQI_cfpar] = 0;
				 parcnt[UQI_cfpar] = -1;
				 feror = UU_FALSE;
				 }
				 else
					/* gmessage (D_ksws, " error - too many nested function call");*/
					uu_uerror0 (UQ_CALCERROR, 18);
					}
			break;


	 case  CFUNCARG  :
		 	uqi_cpop (&vtype, val);
			if (UQI_sdebug)
				{
				 printf ("findex[UQI_cfpar] = %d \n", findex[UQI_cfpar]);
				 printf ("arg = %d \n", UQ_symtab[findex[UQI_cfpar]].func->arg[0]);  
				}
			if (++argcnt[UQI_cfpar] <= UQ_symtab[findex[UQI_cfpar]].func->arg[0])
				{
			 	 aindex = UQ_symtab[findex[UQI_cfpar]].func->arg[argcnt[UQI_cfpar]];
			 	 if (vtype <= UQ_SCALAR)
					{
					 uqi_svsymval (aindex, tmpstk[UQI_cfpar], ++parcnt[UQI_cfpar]);
					 uqi_putsymval (aindex, vtype, val, 3);
					}
			 	 else
					{
					 feror = UU_TRUE;
					 /* gmessage (D_ksws, " error - illegal parameter type "); */
					 uu_uerror0 (UQ_CALCERROR, 19);
				   }
				}
			else
			  {
				feror = UU_TRUE;
				/* gmessage (D_ksws,  " error - parameter number exceeds range "); */
				uu_uerror0 (UQ_CALCERROR, 21);
			  }
			break;

	 default :				/*  perform function uqi_evaluation  */
			if (argcnt[UQI_cfpar] != UQ_symtab[findex[UQI_cfpar]].func->arg[0])
				/* gmessage (D_ksws, " error - unequal parameter number "); */
				uu_uerror0 (UQ_CALCERROR, 21);
			else
			   if (feror == UU_FALSE)
					uqi_funcparse();
			uqi_resumesymv (tmpstk[UQI_cfpar], parcnt[UQI_cfpar]);
			UQI_cfpar--;
			break;
	}	 /* switch */
			
}		/* cfuncval */

/*********************************************************************
**    I_FUNCTION     :  uqi_svsymval (aindex, tmpstk, parcnt)
**       put the symbol value correspond to the parameters of the 
**       called function into a temporary storage to be resumed 
**       latter.
**    PARAMETERS   
**       INPUT  : 
**          aindex - address of the symbol in the symbol table.
**          parcnt - index of the temporary stack.
**          tmpstk - temporary stack to store the value.
**       OUTPUT :  
**          tmpstk - temporary stack to store the value.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uqi_svsymval (aindex, tmpstk, parcnt)
int  aindex, parcnt;
TPSTK  tmpstk[];
{
 int	i;

	uu_denter(UU_DTRC,(us,"uqi_svsymval,parcnt=%d",parcnt));
	if (parcnt < 4)
	{
	 tmpstk[parcnt].tpindex = aindex;
	 tmpstk[parcnt].ttype = UQ_symtab[aindex].ttype;
	 /*uu_dprint(UU_DTRC,(us,"ttype=%d",UQ_symtab[aindex].ttype)); */
	 switch (UQ_symtab[aindex].ttype)
	 {
	  case  UQ_FUNCARG  :  break;
	  case  UM_CARTESIAN  :
	  case  UM_CYLINDRICAL:
	  case  UM_SPHERICAL  :
			  for (i=0; i<3; i++)
				 tmpstk[parcnt].tval[i] = UQ_symtab[aindex].val[i];
				break;

	  case  UQ_SCALAR  :
				tmpstk[parcnt].tval[0] = UQ_symtab[aindex].val[0];
				break;

	  case  UQ_FUNCTION :
				tmpstk[parcnt].fptr = UQ_symtab[aindex].func;
				break;

	  default :
				/* gmessage (D_ksws, " error - invalid type ");  */
				uu_uerror0 (UQ_CALCERROR, 8);
				break;
	 }
 	}
	else
		printf (" error - temporary stack overflow\n");
	uu_dexit;
}		/* uqi_svsymval */

/*********************************************************************
**    I_FUNCTION     :  uqi_resumesymv (tmpstk, parcnt)
**       put the temporarily stored symbol value back to symbol table.
**    PARAMETERS   
**       INPUT  : 
**          tmpstk : temporary stack
**          parcnt : index of the temporary stack
**       OUTPUT :  
**          parcnt : index of the temporary stack
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uqi_resumesymv (tmpstk, parcnt)
TPSTK  tmpstk[];
int  parcnt;
{
	int  i, j;

#define   tindex	  tmpstk[i].tpindex

	uu_denter(UU_DTRC,(us,"uqi_resumesymv,parcnt=%d",parcnt));
	for (i=0; i<=parcnt; i++)
		{
		 /*uu_dprint(UU_DTRC,(us,"symtype=%d",UQ_symtab[tindex].ttype));*/
		 switch (UQ_symtab[tindex].ttype)
		 {
		  case  UQ_SCALAR :
		  case  UQ_FUNCARG:
		  case  UM_CARTESIAN  :
		  case  UM_CYLINDRICAL:
		  case  UM_SPHERICAL  :
						break;

		  case  UQ_FUNCTION :
						uu_toolfree (UQ_symtab[tindex].func);
						break;

		  default  :
						/* gmessage (D_ksws, " error - invalid type "); */
						uu_uerror0 (UQ_CALCERROR, 8);
						break;
		 }

		 UQ_symtab[tindex].ttype = tmpstk[i].ttype;
		 uu_dprint(UU_DTRC,(us,"ttype=%d",tmpstk[i].ttype));
		 switch (tmpstk[i].ttype)
		 {
		  case  UQ_FUNCARG :		break;

		  case  UM_CARTESIAN  :
		  case  UM_CYLINDRICAL:
		  case  UM_SPHERICAL  :
					for (j=0; j<3; j++)
						UQ_symtab[tindex].val[j] = tmpstk[i].tval[j];
						break;

		  case  UQ_SCALAR  :
						UQ_symtab[tindex].val[0] = tmpstk[i].tval[0];
						break;

		  case  UQ_FUNCTION  :
						UQ_symtab[tindex].func = tmpstk[i].fptr;
						break;

		  default :
						/* gmessage (D_ksws, " error - invalid type "); */
						uu_uerror0 (UQ_CALCERROR, 8);
						break;
		 }
		}

#undef  tindex
uu_dexit;

}		/* uqi_resumesymv */

/*********************************************************************
**    I_FUNCTION     :  uqi_cfread (in, kf, len)
**       read the function string into input buff
**    PARAMETERS   
**       INPUT  : 
**          kf - index of where to start placing data
**       OUTPUT :  
**          in - array to receive data
**          len - length of function string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_cfread (in, kf, len)
int  in[], kf, *len;
{
	int  i;

	/* *len = uqi_symlen(UQ_symtab[findex[UQI_cfpar]].tbval.fptr->funcbuf); */
	*len = strlen(UQ_symtab[findex[UQI_cfpar]].func->funcbuf);
	for (i=0; i< *len; i++)
		in[kf+i] = UQ_symtab[findex[UQI_cfpar]].func->funcbuf[i];
}		/*  uqi_cfread  */

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
	UU_REAL	val[3];

	if (uq_calc2flag == UU_FALSE)
	if (UQ_tbindex == 0)
	  {
		/* gmessage (D_ksws, " empty table "); */
		uu_uerror0 (UQ_CALCERROR, 22);
/*		ud_wrwin("\n");*/
		ul_win_out("\n",0);
	  }
	else
	{
/*	ud_wrwin("\n");*/
	ul_win_out("\n",0);
	for (i=0; i<UQ_tbindex; i++)
	{
	 if (UQ_symtab[i].ttype != UQ_FUNCARG)
	 {
	 k = 0;
	 uqi_prsymm (&len, UQ_symtab[i].symbol, cbuf, &k);
	 for (j=len; j<=UQ_SYMSIZE; j++)
		  sprintf (&cbuf[k++], " ");
	 sprintf (&cbuf[k], "   ");
	 COTYPE = (UQ_symtab[i].ttype > UM_SPHERICAL);
	 k = strlen(cbuf);
#define  stbv    UQ_symtab[i].val
	 switch (UQ_symtab[i].ttype)
	 {
	  case	 UQ_SCALAR  :
						sprintf (&cbuf[k], "SCALAR       ");
						k = strlen(cbuf);
						if (UQI_sciflag == UQ_FLT)
						   sprintf (&cbuf[k], UQ_dpt, stbv[0]);
						else  sprintf (&cbuf[k], "%e ", stbv[0]);
						k = strlen(cbuf);
						sprintf (&cbuf[k], "\n");
/*						ud_wrwin(cbuf);*/
						ul_win_out(cbuf,0);
						break;

	 case    UM_CARTESIAN :
	 case    UM_VCARTESIAN :
						if (UQ_symtab[i].ttype == UM_CARTESIAN)
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
/*						ud_wrwin(cbuf);*/
						ul_win_out(cbuf,0);
						break;

	 case    UM_CYLINDRICAL :
	 case    UM_VCYLINDRICAL :
						if (UQ_symtab[i].ttype == UM_CYLINDRICAL)
							sprintf (&cbuf[k], "CYLINDRICAL  < ");
						else
							sprintf (&cbuf[k], "VCYLINDRICAL  < ");
					/*----
						um_cotovc (stbv, UQ_symtab[i].ttype, val);
						um_mcstoccs(COTYPE, val, val1);
						um_vctoco (val1, UQ_symtab[i].ttype, val);
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
/*						ud_wrwin(cbuf);*/
						ul_win_out(cbuf,0);
						break;
					
	 case    UM_SPHERICAL :
	 case    UM_VSPHERICAL :
						if (UQ_symtab[i].ttype == UM_SPHERICAL)
							sprintf (&cbuf[k], "SPHERICAL    < ");
						else
							sprintf (&cbuf[k], "VSPHERICAL    < ");
					/*---
						um_cotovc (stbv, UQ_symtab[i].ttype, val);
						um_mcstoccs(COTYPE, val, val1);
						um_vctoco (val1, UQ_symtab[i].ttype, val);
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
/*						ud_wrwin(cbuf);*/
						ul_win_out(cbuf,0);
						break;
#undef  stbv

	 case   UQ_FUNCTION  :
#define	  tfptr	UQ_symtab[i].func

						sprintf (&cbuf[k], "FUNCTION     args: ");
						k = strlen(cbuf);
						for (j=1; j<=tfptr->arg[0]; j++)
							{ 
							 uqi_prsymm(&len, UQ_symtab[tfptr->arg[j]].symbol,cbuf,&k);
							 sprintf(&cbuf[k++], " ");
							}
						sprintf (&cbuf[k], "  --> ");
						k = strlen (cbuf);
						uqi_prsymm(&len, tfptr->funcbuf, cbuf, &k);
#undef	 tfptr
						k = strlen(cbuf);
						sprintf(&cbuf[k], "\n");
/*						ud_wrwin(cbuf);*/
						ul_win_out(cbuf,0);
						break;

	 case   UQ_FUNCARG  :
						sprintf (&cbuf[k], "PARAMETER  - VALUE UNKNOWN \n");
/*						ud_wrwin(cbuf);*/
						ul_win_out(cbuf,0);
						break;;

	 case	  UQ_QUERYFUNC :
						break;

	 default		:
				sprintf (&cbuf[k], " UNDEFINED SYMBOL\n");
				/*  ud_wrwin( cbuf);		*/
				break;
	}	/* switch  */
	}	/* if    */
	}	/* for */
	}  /* else */
}		/* uqi_stblist */

/*********************************************************************
**    I_FUNCTION     :  uqi_prsymm
**			decode the string to ascii code and print it out
**    PARAMETERS   
**       INPUT  : 
**					sym - string to be decoded
**       OUTPUT :  
**					len - length of the string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uqi_prsymm (len, sym, cbuf, k)
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

/*********************************************************************
**    I_FUNCTION :  uq_load
**       load the symbol table from the unibase
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uq_load(ldflag)
int	ldflag;
{
	int	next_tupleid, key_id;
	int	status;

	uu_denter(UU_RTRC,(us,"enter uq_load,ldflag=%d",ldflag));
	next_tupleid = 1;
	if (!ldflag)			/* load, load the new symbols */
	  {
		UQ_tbindex = 0;		
		while ((status=ur_get_next_new_data_key(UQ_CALC_REL,&next_tupleid,&key_id))==0)
	  	  {
			UQ_symtab[UQ_tbindex].rel_num = UQ_CALC_REL;
			UQ_symtab[UQ_tbindex].key = key_id;
			status = ur_retrieve_data_fixed(&UQ_symtab[UQ_tbindex]);
			if (UQ_symtab[UQ_tbindex].ttype == UQ_FUNCTION)
		  	  {
				UQ_symtab[UQ_tbindex].func = (UQ_func *) uu_toolmalloc(sizeof(UQ_func));
				ur_retrieve_data_varlist(key_id,1,UQ_symtab[UQ_tbindex].func,1,1);
		  	  }
			UQ_tbindex++;		next_tupleid++;
	  	  }
	  }
	else			/* merge, delete symbols in unibase */
		while ((status=ur_get_next_new_data_key(UQ_CALC_REL,&next_tupleid,&key_id))==0)
		  {
	  		ur_delete_all(key_id);
			next_tupleid++;
		  }

	uu_denter2(UU_RTRC,(us,"after uq_load,UQ_tbindex=%d,%s,%s,%s,%s,%s,%s,%s", 
		UQ_tbindex,UQ_symtab[0].symbol,UQ_symtab[1].symbol,UQ_symtab[2].symbol,
		UQ_symtab[3].symbol,UQ_symtab[4].symbol,UQ_symtab[5].symbol,
		UQ_symtab[6].symbol));
	uu_dexit;
	uu_denter2(UU_RTRC,(us,"type=%d,%d,%d,%d,%d,%d,%d", 
		UQ_symtab[0].ttype,UQ_symtab[1].ttype,UQ_symtab[2].ttype,
		UQ_symtab[3].ttype,UQ_symtab[4].ttype,UQ_symtab[5].ttype,
		UQ_symtab[6].ttype));
	uu_dexit;
	uu_dexit;
}	/* uq_load	*/
