
/*********************************************************************
**    NAME         :  cstack.c
**       CONTAINS:
**       	uqi_cor_push		uqi_cordpush		uqi_its1push		uqi_its2push
**				uqi_cpop			uqi_resetstack	uqi_sc_push
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       qstack.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:55
*********************************************************************/


#include  "usysdef.h"
#include  "mdcoord.h"
#include  "calcom.h"
#include  "cdef.h"
#include  "uhep.h"
#include	 "tlangtool.h"

extern int   UQI_cdebug;
extern int   UQI_cer2;
extern int   UQI_angflag;

static CSTK  stack[UQ_MAXSTK];
static int  UQI_cstkpt = -1;
char	*uu_toolmalloc();



/*********************************************************************
**    I_FUNCTION     :  uqi_resetstack ()
**			reset the uq_calcultor stack to empty
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_resetstack ()

{
	UQI_cstkpt = -1;
}		/* uqi_resetstack */




/*********************************************************************
**    I_FUNCTION     :  uqi_sc_push (ptype, pval)
**       push a scalar value and its type to the stack
**    PARAMETERS   
**       INPUT  : 
**					 ptype - type of the value
**					 pval	 - number to be put to the stack
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_sc_push (ptype, pval)
int  ptype;	
UU_REAL  pval;

{
    if (UQI_cstkpt == UQ_MAXSTK)
       /* gmessage (D_ksws, " error - expression is too complex !"); */
	uu_uerror0 (UQ_CALCERROR, 6);
    else
	 {
	  if (UQI_cdebug)
    	   printf (" uqi_sc_push - %d, %f  \n",ptype, pval);
	   stack[++UQI_cstkpt].ftype = ptype;
		stack[UQI_cstkpt].sval.tval = pval;
	 }
}	/* uqi_sc_push */




/*********************************************************************
**    I_FUNCTION     :  uqi_cor_push (ptype, pval)
**       push a coordinate type value onto the stack
**    PARAMETERS   
**       INPUT  : 
**          ptype - type of the coordinate
**          pval	- value of the coordinate
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_cor_push (ptype, pval)
int  ptype;
UU_REAL  pval[3];

{
	 int  i;
    if (UQI_cstkpt == UQ_MAXSTK)
       /* gmessage (D_ksws, " error - expression is too complex!"); */
	uu_uerror0 (UQ_CALCERROR, 6);
    else
	 {
	  if (UQI_cdebug)
    	   printf (" uqi_cor_push-type - %d \n", ptype);
	   stack[++UQI_cstkpt].ftype = ptype;
		stack[UQI_cstkpt].sval.tptr = (UU_REAL *) uu_toolmalloc (3*sizeof(UU_REAL));
		for (i=0; i<3; i++)
		 {
		  *(stack[UQI_cstkpt].sval.tptr+i) = pval[i];
	     if (UQI_cdebug)
    	   printf (" uqi_cor_push - %f, ", *(stack[UQI_cstkpt].sval.tptr+i));
		 }
		if (UQI_cdebug)  printf ("\n");
	 }
}	/* uqi_cor_push */





/*********************************************************************
**    I_FUNCTION     :  uqi_cpop ( pptype, ppval )
**       pop out an element of the stack
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          pptype - type of the element
**          ppval  - value of the element
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_cpop ( pptype, ppval )
int *pptype;
UU_REAL ppval[3];

{  int  i;
   if (UQI_cstkpt < 0)
	  {
		UQI_cer2 = UU_TRUE;
		*pptype = 10;
		ppval[0] = 0;
		if (UQI_cdebug)
	   printf (" error - stack is underflow!\n");
	  }
   else
       {
        if (UQI_cdebug)
			 { if (stack[UQI_cstkpt].ftype == UQ_SCALAR)
 	            printf (" uqi_cpop - %f", stack[UQI_cstkpt].sval.tval);
				else if ((stack[UQI_cstkpt].ftype & ~07) == 0)
						  for (i=0; i<3; i++)
							 printf ("uqi_cpop - %f,", *(stack[UQI_cstkpt].sval.tptr+i));
           printf ("\n");
          }

		 *pptype = stack[UQI_cstkpt].ftype;
		 switch (*pptype)
		 {
		  case  UQ_SCALAR  :
		  case  UQ_QUERYFUNC :
							ppval[0] = stack[UQI_cstkpt--].sval.tval;
							break;

		  case  UM_CARTESIAN	:
		  case  UM_SPHERICAL  :
		  case  UM_CYLINDRICAL :
		  case  UM_VCARTESIAN	:
		  case  UM_VSPHERICAL  :
		  case  UM_VCYLINDRICAL :
						for (i=0; i<3; i++)
							ppval[i] = *(stack[UQI_cstkpt].sval.tptr+i);
				      uu_toolfree(stack[UQI_cstkpt].sval.tptr);   
						UQI_cstkpt--;
						break;
		  default      :
						/* gmessage (D_ksws, " error - undefined type "); */
						uu_uerror0 (UQ_CALCERROR, 7);
						break;
		 }
     }
}	/* uqi_cpop */





/*********************************************************************
**    I_FUNCTION     :  uqi_its1push (its, ltr)
**       get an input number from the "its" which contains the 
**       translation code
**    PARAMETERS   
**       INPUT  : 
**          its - an array contains the translation code
**          ltr - index to the its
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_its1push (its, ltr)
ITSTYPE  its[];
int  ltr;

{
	  UU_REAL num;

     if (its[ltr-1].lint == 1)    /* an integer */
		 {
			/*num = *(UU_REAL *)(&its[ltr].lint); */
			num = its[ltr].lint;
			uqi_sc_push (UQ_SCALAR, num);
		  }
     else if (its[ltr-1].lint == 2) 	/* a real number */
		 {
			num = its[ltr].lreal;
			uqi_sc_push (UQ_SCALAR, num);
		  }
      else
			if (UQI_cdebug)
	      printf ( " error - wrong translation code\n");
}	/* uqi_its1push */

  



/*********************************************************************
**    I_FUNCTION     :  uqi_its2push (its, ltr)
**       get the translation code from different operators
**    PARAMETERS   
**       INPUT  : 
**          its - an array contains translation code
**          ltr - index to the "its"
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_its2push (its, ltr)
ITSTYPE  its[];
int  ltr;

{
	  UU_REAL num;
     
	  if (its[ltr-2].lint == 5)
        {
	    	num = its[ltr].lint;
			uqi_sc_push (UQ_SCALAR, num);
        }
      else
			if (UQI_cdebug)
	      printf ( " error - wrong translation code\n");
}	/* uqi_its2push */





/*********************************************************************
**    I_FUNCTION     :  uqi_cordpush (ctype)
**       pop out the single element of an coordinate and then store
**       the whole coordinate to the stack
**    PARAMETERS   
**       INPUT  : 
**          ctype - type of the coordinate
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_cordpush (ctype)
int  ctype;

{
	int  i, ptype;
	UU_REAL  ary1[3], ary2[3];

	for (i=0; i<3; i++)
	  {
		uqi_cpop (&ptype, ary1);
		ary2[2-i] = ary1[0];
	  }

	switch(ctype)
	{
	 case  UM_CARTESIAN : case  UM_VCARTESIAN :  break;

	 case  UM_CYLINDRICAL : case  UM_VCYLINDRICAL :
					if (UQI_angflag == UQ_DEG)
						ary2[1] = ary2[1] / UQ_RADIAN;
					break;

	 case  UM_SPHERICAL :  case  UM_VSPHERICAL :
					if (UQI_angflag == UQ_DEG)
					  {
						ary2[1] = ary2[1] / UQ_RADIAN;
						ary2[2] = ary2[2] / UQ_RADIAN;
					  }
					break;

	 default :  break;
	}
   uqi_cor_push (ctype, ary2);
}		/* uqi_cordpush */




