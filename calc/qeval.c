/*********************************************************************
**    NAME         :qeval.c
** 
**    CONTAINS     : uqi_angtype uqi_bi_op
**                   uqi_calpow  uqi_chgnotation
**                   uqi_cnegate
**                   uqi_coordop uqi_cordmult 
**                   uqi_dang 
**                   uqi_eval
**                   uqi_evala1
**                   uqi_evala2  
**                   uqi_evala3 
**                   uqi_panswer 
**                   uqi_veval
**  
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
** 
**     MODULE NAME AND RELEASE LEVEL
**       qeval.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:53
*********************************************************************/
/*****  ceval.c - perform all the uqi_evaluation                *****/

#include  "usysdef.h"
#include  "umath.h"
#include	 "udebug.h"
#include  "mdcoord.h"
#include  "modef.h"
#include  "calcom.h"
#include  "cdef.h"
#include  "uhep.h"
#include	 "tlangtool.h"

extern   int  UQI_cdebug;
extern   int  UQI_cer2;
extern   int  UQI_cfuncflag;
extern   int  UQI_angflag;
/* extern   int  *D_ksws; */
extern   int  uq_calc2flag;
extern   int  uq_calc2rslt;
extern   UU_REAL UQI_cdval[];

int  UQI_sciflag = UQ_FLT;
char  UQ_dpt[] = {"%.6f"};


/*********************************************************************
**    I_FUNCTION     :  uqi_chgnotation (op, its, ltr)
**       change number print out format and notation
**    PARAMETERS   
**       INPUT  : 
**          op - flag of the different notation
**          its - an array contains the translation code
**          ltr - index to the "its"
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_chgnotation (op, its, ltr)
int  op, ltr;
ITSTYPE	its[];

{
	 int  k, i1, i2, i3;
	 UU_REAL i0, ii0;

	 if (uq_calc2flag == UU_FALSE)
	 if (op == 1)
		 UQI_sciflag = UQ_SCI;
	 else  if (op == 2)
				 {
				  UQI_sciflag = UQ_FLT;
				  if (its[ltr-1].lint == 1)
					  if (its[ltr].lint < 10)
						 if (its[ltr].lint >= 0)
					  	   UQ_dpt[2] = its[ltr].lint + '0';
					    else
						   /* gmessage (D_ksws, " error - decimal digit can't be negative"); */
						   uu_uerror0 (UQ_CALCERROR, 23);
					  else
						 /* gmessage (D_ksws, " error - maximum decimal digit is nine"); */
						   uu_uerror0 (UQ_CALCERROR, 24);
				 }
          else
				 /* gmessage (D_ksws, "error - no notation changed "); */
				   uu_uerror0 (UQ_CALCERROR, 25);
}		/* uqi_chgnotation */



/*********************************************************************
**    I_FUNCTION     :  uqi_angtype (op)
**       change the angle type to DEGREE or RADIAN
**    PARAMETERS   
**       INPUT  : 
**          op - indicator of the angle type
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_angtype (op)
int op;

{
	if (uq_calc2flag == UU_FALSE)
	if (op == 1)
		UQI_angflag = UQ_DEG;
	else  if (op == 2)
				UQI_angflag = UQ_RAD;
 
}		/* uqi_angtype */



/*********************************************************************
**    I_FUNCTION     :  uqi_eval ( op )
**       perform the uqi_evaluation for different operator and actions
**    PARAMETERS   
**       INPUT  : 
**          op - indicator of the action
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_eval ( op )
int  op;

{
	 int  i, index, ind;
	 int  typ1, typ2;
	 UU_REAL  val1[3], val2[3];

	 uu_denter(UU_DTRC,(us,"enter uqi_eval,op=%d", op));
    switch (op)
    {
     case  1 :			/*  unary minus operator  */
				uqi_cnegate ();
	         break;

     case  2 :      /* operators of +, -, *, /, ^, cross,dot */
				uqi_bi_op ();
            break;
	
     case 3  :                  /*  variables */
				uqi_cpop (&typ1, val1);
				uqi_cpop (&typ2, val2);
	 			uu_dprint(UU_DTRC,(us,"typ1=%d,typ2=%d",typ1,typ2));
				if ((typ1 == UQ_QUERYFUNC)&&(!UQI_cer2))
					if (!uq_calcfunc(&typ1,val1))
						UQI_cer2 = UU_TRUE;
				if (UQI_cer2 != UU_TRUE)
				{
		       index = val2[0];
				 if (UQI_cdebug)
		         printf ("** index = %d\n", index);
	          uqi_putsymval ( index, typ1, val1, 0 );
				 if (uq_calc2flag == UU_FALSE)
				 	uqi_panswer (typ1, val1);
				 else
					{
					 uq_calc2rslt = UU_TRUE;
					 UQI_cdval[0] = typ1;
					 for (i=0; i<3; i++)
						UQI_cdval[i+1] = val1[i];
					}
				}
				if ((UQI_cer2) && (!uq_calc2flag))
/*					ud_wrwin("\n");		/* put line feed if no answer show up */
					ul_win_out("\n",0);		/* put line feed if no answer show up */
				break;

     case  4 :						/*  direct answer  */
				 if (UQI_cfuncflag == UU_FALSE)
					{
				 	 uqi_cpop (&typ1, val1);
					 if (typ1 == UQ_QUERYFUNC)
						{
								/* reserve word can't be used as a variable name */
	   				 uu_uerror0 (UQ_CALCERROR, 56);
/*						 ud_wrwin("\n");		/* put line feed if no answer show up */
						ul_win_out("\n",0);		/* put line feed if no answer show up */
						}
					 else
						{
					 	 if (UQI_cer2 == UU_FALSE) 
				 	 	 if (uq_calc2flag == UU_FALSE)
				 		 	 uqi_panswer (typ1, val1);
				 	 	 else
						 	 {
					 	  	 uq_calc2rslt = UU_TRUE;
					 	  	 UQI_cdval[0] = typ1;
					 	  	 for (i=0; i<3; i++)
						  	 UQI_cdval[i+1] = val1[i];
						 	 }
						 if ((UQI_cer2) && (!uq_calc2flag))
/*							 ud_wrwin("\n");		/* put line feed if no answer show up */
							ul_win_out("\n",0);		/* put line feed if no answer show up */
						}
					}
				 else
/*						ud_wrwin("\n");		/* put line feed if no answer show up */
						ul_win_out("\n",0);		/* put line feed if no answer show up */
            break;

     default :
	     /* gmessage ( D_ksws, " error - improper operation  "); */
	   uu_uerror0 (UQ_CALCERROR, 26);
    }    /* switch */
	 uu_dexit;
}	/*  uqi_eval  */



/*********************************************************************
**    I_FUNCTION     :  uqi_cnegate ()
**       negate a value
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_cnegate ()

{
	int  typ1;
	UU_REAL  val1[3], val2[3];

	uu_denter(UU_DTRC,(us,"enter uqi_tbreset"));
	uqi_cpop (&typ1, val1);
	switch (typ1)
	{
	 case  UQ_SCALAR  :
						uqi_sc_push (typ1, -val1[0]);
						break;

	 case  UM_CARTESIAN   :
	 case  UM_VCARTESIAN   :
						um_vctmsc (val1, (UU_REAL) -1.0, val1);
						uqi_cor_push (typ1, val1);
						break;

	 case  UM_CYLINDRICAL:
	 case  UM_SPHERICAL  :
	 case  UM_VCYLINDRICAL:
	 case  UM_VSPHERICAL  :
						um_cotovc (val1, typ1, val2);
						um_vctmsc (val2, (UU_REAL) -1.0, val2);
						um_vctoco (val2, typ1, val1);
						uqi_cor_push (typ1, val1);
						break;

	 default			:
						/* gmessage (D_ksws, " error - undefined type"); */
	   					uu_uerror0 (UQ_CALCERROR, 7);
						break;
	}
	uu_dexit;
}		/* uqi_cnegate */



/*********************************************************************
**    I_FUNCTION     :  uqi_bi_op ()
**       uq_calculate the result for those operator with two operands
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_bi_op ()

{
	int  op, lscalar, lcoord, type, typ1, typ2, typ3;
	int  mix, i, iv1, iv3;
	UU_REAL  val1[3], val2[3], val3[3], mult, power;

   uqi_cpop (&typ1,val1);   uqi_cpop (&typ2,val2);  uqi_cpop (&typ3,val3);
   op = val2[0];
	lscalar = (typ1==UQ_SCALAR) && (typ3==UQ_SCALAR);
	lcoord  = (typ1 & ~07) || (typ3 & ~07);
	mix = (typ1 <= UQ_SCALAR) && (typ3 <= UQ_SCALAR);
   switch (op)
   {
	 case  1 :								/*  addition  */
			if (lcoord==0)				   /* both operands are coord */
				uqi_coordop (typ1, typ3, val1, val3, op);
		   else if (lscalar)
					  uqi_sc_push (UQ_SCALAR, val1[0]+val3[0]);
				  else
				  {
					UQI_cer2 = UU_TRUE;
			/* gmessage (D_ksws, " error - illegal operation, wrong operand type"); */
			uu_uerror0 (UQ_CALCERROR, 27);
				  }
			break;

    case  2 :							 /*  subtraction */
			if (lcoord == 0)
				uqi_coordop (typ1, typ3, val1, val3, op);
		   else if (lscalar)
					  uqi_sc_push (UQ_SCALAR, val3[0]-val1[0]);
				  else 
				  {
               UQI_cer2 = UU_TRUE;
			/* gmessage (D_ksws, " error - illegal operation, wrong operand type"); */
			uu_uerror0 (UQ_CALCERROR, 27);
				  }

			break;

    case  3 : 							/* multiplication  */
			if (lcoord == 0)
			  {
				UQI_cer2 = UU_TRUE;
				/* gmessage (D_ksws, " error - coord can't multiply with each other"); */
				uu_uerror0 (UQ_CALCERROR, 28);
			  }
		   else if (lscalar)
					  uqi_sc_push (UQ_SCALAR, val3[0]*val1[0]);
				  else if (mix)
					 {
					  if (typ1==UQ_SCALAR) 
						 {
					  if (val1[0] != 0)
					  uqi_cordmult (1.0/val1[0]*val1[0]*val1[0], typ3, val3);
					  else
					  		uqi_cordmult (val1[0], typ3, val3);
						  }
					  else if (typ3==UQ_SCALAR)
						     uqi_cordmult (val3[0], typ1, val1);
					 }	 
			break;

    case  4 :						 /*  division */
			if (lcoord == 0)
			  {
				UQI_cer2 = UU_TRUE;
				/* gmessage (D_ksws, " error - coord can't divide with each other"); */
				uu_uerror0 (UQ_CALCERROR, 29);
			  }
		   else if (lscalar)
					  if (fabs(val1[0]) > UQ_FUZZ)
					  	  uqi_sc_push (UQ_SCALAR, val3[0]/val1[0]);
					  else
						 {
						  /* gmessage (D_ksws, " error - denominator can't be zero"); */
						  uu_uerror0 (UQ_CALCERROR, 30);
						  UQI_cer2 = UU_TRUE;
						 }
				  else  if (mix)
					 {
					  if (typ1==UQ_SCALAR) 
						  if (val1[0] != 0.0)
						  	  uqi_cordmult (1.0/val1[0], typ3, val3);
						  else
							 {
							  /* gmessage (D_ksws," error - denominator can't be zero"); */
						  	  uu_uerror0 (UQ_CALCERROR, 30);
							  UQI_cer2 = UU_TRUE;
							 }
  					  else
					  {
						UQI_cer2 = UU_TRUE;
					  /* gmessage (D_ksws, " error - scalar can't be divided by a coord"); */
				  	  uu_uerror0 (UQ_CALCERROR, 31);
					  }
					 }
			break;

	 case  5 :						/*  power  */
			if (lscalar)		
			  {
				if (uqi_calpow(val3[0],val1[0],&power))
					uqi_sc_push (UQ_SCALAR, power);
				else
					{
					 UQI_cer2 = UU_TRUE;
					 /* gmessage (D_ksws, " error - illegal value range "); */
				  	  uu_uerror0 (UQ_CALCERROR, 32);
					 uqi_sc_push (UQ_SCALAR, (UU_REAL) 0.0);
					}
			  }
			else
			  {
				UQI_cer2 = UU_TRUE;
				/* gmessage (D_ksws, " error - only scalar operands are allowed"); */
				 uu_uerror0 (UQ_CALCERROR, 33);
			  }
			break;

	 case  6 :						/*  cross operation  */
	 case  7 :						/*  dot operation    */
	 case  8 :						/*  distance operation  */
	 case  10:						/*  angle between two vectors */
			if (lcoord == 0)
				uqi_coordop (typ1, typ3, val1, val3, op);
			else
			 {
			  UQI_cer2 = UU_TRUE;
			  /* gmessage (D_ksws, " error - only coord operands are allowed"); */
			  uu_uerror0 (UQ_CALCERROR, 34);
			 }
	      break;

	 case  9 :					  /*  %  */
			iv1 = val1[0];     iv3 = val3[0];
			if (lscalar)
			  {
				if (iv1 == 0)
				  {
				  /* gmessage (D_ksws," error - denominator can't be zero"); */
					  uu_uerror0 (UQ_CALCERROR, 30);
					  UQI_cer2 = UU_TRUE;
				  }
				else
				  {
					val1[0] = iv3%iv1;
					uqi_sc_push (UQ_SCALAR, val1[0]);
				  }
			  }
			else
			  {
				UQI_cer2 = UU_TRUE;
				/* gmessage (D_ksws, " error - only scalar operands are allowed"); */
			        uu_uerror0 (UQ_CALCERROR, 33);
			  }
			break;

    default :
		   /* gmessage (D_ksws,  " error - no such operation !! "); */
	        uu_uerror0 (UQ_CALCERROR, 26);
   }    /* switch */
}		/* uqi_bi_op */





/*********************************************************************
**    I_FUNCTION     :  uqi_calpow (val1, val2, power)
**       check the valid value to perform the power uqi_evaluation,
**       otherwise return UU_FALSE
**    PARAMETERS   
**       INPUT  : 
**          val1 - base of the power expression
**          val2 - power
**       OUTPUT :  
**          power - result value
**    RETURNS      : logical flag
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_calpow (val1, val2, power)
UU_REAL val1, val2, *power;

{
	int	k;
	UU_REAL  x;

	if ((val1==0) && (val2==0))
		return (UU_FALSE);
	else if (val1 < 0)
			 {
			  k = val2;   x = k;
			  if (x == val2)		/* power is an integer */
				  *power = ((k%2)==0) ? pow(-val1,val2) : -pow(-val1,val2);
			  else
				  return (UU_FALSE);
			 }
		  else
			 *power = pow(val1, val2);
	return (UU_TRUE);
}	/* uqi_calpow */



/*********************************************************************
**    I_FUNCTION     :  uqi_coordop (typ1, typ3, val1, val3, op)
**       uqi_evaluate coordinate uq_calculation
**    PARAMETERS   
**       INPUT  : 
**          typ1 - type of the second coordinate
**          typ3 - type of the first coodinate
**          val1 - value of the second coordinate
**          val3 - value of the first coordinate
**          op   - indicator of the operation
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_coordop (typ1, typ3, val1, val3, op)
UU_REAL  val1[], val3[];
int    typ1, typ3, op;

{
	UU_REAL  val2[3], val4[3];
	UU_REAL um_dcccc();
	UU_REAL um_dot();
	UU_REAL um_angle();
	int    type;

	type = (typ1 <= typ3) ? typ1 : typ3;
	if ((typ1 != UM_CARTESIAN) && (typ1 != UM_VCARTESIAN))   
		{
		 um_cotovc(val1, typ1, val2);
		 um_vctovc(val2, val1);
		}
	if ((typ3 != UM_CARTESIAN) && (typ3 != UM_VCARTESIAN))   
		{
		 um_cotovc(val3, typ3, val2);
		 um_vctovc(val2, val3);
		}
	switch (op)
	{
	 case  1 :    		/*  addition  */
			 um_vcplvc (val1, val3, val2);
			 break;

	 case  2 :			/* subtraction */
			 if ((typ1 < UM_VCARTESIAN) && (typ3 < UM_VCARTESIAN))
				type = typ1 + 3;
			 um_vcmnvc (val3, val1, val2);
			 break;

	 case  6 :			/* cross operation */
			 um_cross (val3, val1, val2);
			 break;

	 case  7 :			/* dot operation */
			 val2[0] = um_dot (val3, val1);
			 uqi_sc_push (UQ_SCALAR, val2[0]);
			 break;

	 case  8 :			/* distance  */
			 val2[0] = um_dcccc (val3, val1);
			 uqi_sc_push (UQ_SCALAR, val2[0]);
			 break;

	 case  10:			/* angle */
			 val2[0] = um_angle (val3, val1);
			 if (UQI_angflag == UQ_RAD)
				uqi_sc_push (UQ_SCALAR, val2[0]);
			 else
				uqi_sc_push (UQ_SCALAR, val2[0]*UQ_RADIAN);
			 break;

	 default :
			 /* gmessage (D_ksws, " error - no such operation "); */
	        	 uu_uerror0 (UQ_CALCERROR, 35);
			 break;
	}
	if ((op == 1) || (op == 2) || (op==6))
	{
	 if ((type != UM_CARTESIAN) && (type != UM_VCARTESIAN))   
		{
	 	 um_vctoco(val2, type, val4);
	 	 uqi_cor_push (type, val4);
		}
	 else
	 	uqi_cor_push (type, val2);
	}
}		/* uqi_coordop */



/*********************************************************************
**    I_FUNCTION     :  uqi_cordmult (factor, type, val)
**       perform the multiplication of the coordinate
**    PARAMETERS   
**       INPUT  : 
**          factor - multiplicant
**          type   - coordinate type
**          val    - an array contains the coordinate
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_cordmult (factor, type, val)
UU_REAL  factor, val[3];
int    type;

{
	 UU_REAL  val2[3];

	 if ((factor >= 0.0) && ((type==UM_CARTESIAN)||(type==UM_VCARTESIAN)))
	    um_vctmsc (val, factor, val);
    else
	   {
	    um_cotovc (val, type, val2);
	    um_vctmsc (val2, factor, val2);
	    um_vctoco (val2, type, val);
	   }
	 uqi_cor_push (type, val);
}		/* uqi_cordmult */



/*********************************************************************
**    I_FUNCTION     :  uqi_panswer (type, val)
**       print out the result
**    PARAMETERS   
**       INPUT  : 
**          type - type the value
**          val  - an array contains the result
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_panswer (type, val)
int  type;
UU_REAL  val[3];

{
	int  i, j;
	char cbuf[121];

	uu_denter(UU_DTRC,(us,"enter uqi_tbreset"));
	j = 0;
	switch (type)
	{
	 case  UQ_SCALAR  :
					sprintf (&cbuf[j], "\n   Ans : ");
					j = strlen (cbuf);
		   		if (UQI_sciflag == UQ_FLT)
           			sprintf (&cbuf[j], UQ_dpt , val[0]);
        			else  sprintf (&cbuf[j], "%e ", val[0]);
					j = strlen (cbuf);
					sprintf (&cbuf[j], "\n");
/*					ud_wrwin( cbuf);*/
					ul_win_out(cbuf,0);
					break;

	 case  UM_CARTESIAN	:
	 case  UM_CYLINDRICAL:
	 case  UM_SPHERICAL  :
	 case  UM_VCARTESIAN	:
	 case  UM_VCYLINDRICAL:
	 case  UM_VSPHERICAL  :
	 			  if (((type==UM_CYLINDRICAL)||(type==UM_VCYLINDRICAL)||
						 (type==UM_SPHERICAL)||(type==UM_VSPHERICAL))
				  		&& (UQI_angflag == UQ_DEG))
						val[1] = val[1] * UQ_RADIAN;
	 			  if (((type==UM_SPHERICAL)||(type==UM_VSPHERICAL)) 
						&& (UQI_angflag == UQ_DEG))
						val[2] = val[2] * UQ_RADIAN;
				  if (type <= UM_SPHERICAL)
				  		sprintf (&cbuf[j], "\n   Ans : < ");
				  else
				  		sprintf (&cbuf[j], "\n   Ans : V< ");
				  j = strlen (cbuf);
				  for (i=0; i<3; i++)
					 {
					  if (i > 0)
						 {
						  sprintf (&cbuf[j], ", ");
						  j = j + 2;
						 }
				 	  if (UQI_sciflag == UQ_FLT)
               		sprintf (&cbuf[j],  UQ_dpt , val[i]);
          	  	  else  sprintf (&cbuf[j], "%e", val[i]);
					  j = strlen (cbuf);
					 }
				  sprintf (&cbuf[j], " >\n");
/*				  ud_wrwin( cbuf);*/
					ul_win_out(cbuf,0);
				  break;
						  
	 default      :
				 /* gmessage (D_ksws, " error - undefined type"); */
	        	 uu_uerror0 (UQ_CALCERROR, 7);
   }
	uu_dexit;
}		/* uqi_panswer */




/*********************************************************************
**    I_FUNCTION     :  uqi_veval (op, its, ltr)
**       put the information of the component of the coordinate
**       to the stack
**    PARAMETERS   
**       INPUT  : 
**          op - coordinate type 
**          its - translation code
**          ltr - index of the its array
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_veval (op, its, ltr)
int  op, ltr;
ITSTYPE	its[];

{
	UU_REAL  val[3];

	val[0] = its[ltr-3].lint;    /* index of the symbol table  */
	val[1] = op;				/* type of the coordinate     */
	val[2] = its[ltr].lint;			/* which element of the coordinate */
	uqi_cor_push (UM_CARTESIAN, val);
}		/* uqi_veval */




/*********************************************************************
**    I_FUNCTION     :  uqi_evala1 ()
**       uqi_evaluate the trigonometric and some math functions
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_evala1 ()

{
	int i, typ1, typ2;
	UU_REAL  v1, val1[3], val2[3];
	UU_REAL  uqi_dang();

	uqi_cpop (&typ1, val1);     uqi_cpop (&typ2, val2);
	i = val2[0];
	v1 = val1[0];
	if (typ1 == UQ_SCALAR)
	{
	if  (((i >= 4) && (i <= 8))&&(UQI_angflag == UQ_DEG)) 
	 	v1 = v1 / UQ_RADIAN;

	switch (i)
	{
	 case  1  :							/* sin inverse  */
				if (UQI_angflag == UQ_RAD)
					uqi_sc_push (UQ_SCALAR, asin(v1) );
				else
					uqi_sc_push (UQ_SCALAR, asin(v1)*UQ_RADIAN );
				break;

	 case  2  :						 	/* cosine inverse  */
				if (UQI_angflag == UQ_RAD)
					uqi_sc_push (UQ_SCALAR, acos(v1) );
				else
					uqi_sc_push (UQ_SCALAR, acos(v1)*UQ_RADIAN );
				break;

	 case  3  :							/* tan inverse     */
				if (UQI_angflag == UQ_RAD)
					uqi_sc_push (UQ_SCALAR, atan(v1) );
				else
					uqi_sc_push (UQ_SCALAR, atan(v1)*UQ_RADIAN );
				break;

	 case  4  :							/*  hyperbolic sin	*/
				uqi_sc_push (UQ_SCALAR, sinh(v1) );
				break;

	 case  5  :							/* sine					*/
				uqi_sc_push (UQ_SCALAR, sin(v1) );
				break;

	 case  6  :							/* hyperbolic cosine */
				uqi_sc_push (UQ_SCALAR, cosh(v1) );
				break;

	 case  7  :							/* cosine 				*/
				uqi_sc_push (UQ_SCALAR, cos(v1) );
				break;

	 case  8  :							/* hyperbolic tangient */
				uqi_sc_push (UQ_SCALAR, tanh(v1) );
				break;

	 case  9  :							/* tangient 				*/
 	 			if (UQI_angflag == UQ_RAD)
		 			v1 = v1 * UQ_RADIAN;
	 			v1 = uqi_dang (v1);			/* change the angle to be less than 360 */
				if ((fabs(v1-90.0) <= UM_FUZZ) || (fabs(v1-270.0) <= UM_FUZZ))
					/*  printf ("error - undefined value\n");   */
					uu_uerror0 (UQ_CALCERROR, 46);
				else
				  {	
	 				v1 = v1 / UQ_RADIAN;
					uqi_sc_push (UQ_SCALAR, tan(v1) );
				  }
				break;

	 case  21 :							/* logrithm	 with base of ten	*/
				if (v1 <= 0)
				  {
					UQI_cer2 = UU_TRUE;
					/* gmessage (D_ksws, " Invalid input, logarithm of zero or negative value not allowed"); */
	        	 		 uu_uerror0 (UQ_CALCERROR, 54);
				  }
				else
					uqi_sc_push (UQ_SCALAR, log10(v1) );
				break;

	 case  22 :							/* natural log				*/
				if (v1 <= 0)
				  {
					UQI_cer2 = UU_TRUE;
					/* gmessage (D_ksws, " Invalid input, logarithm of zero or negative value not allowed"); */
	        	 		 uu_uerror0 (UQ_CALCERROR, 54);
				  }
				else
					uqi_sc_push (UQ_SCALAR, log(v1) );
				break;

	 case  23 :							/* power of ten			*/
				if (v1 <= 15)
					uqi_sc_push (UQ_SCALAR, pow(10.0, v1) );
				else
					{
					 UQI_cer2 = UU_TRUE;
					 /* gmessage (D_ksws, " warning - exceed machine value range"); */
	        	 		 uu_uerror0 (UQ_CALCERROR, 36);
					}
				break;

	 case  24 :							/* power with natural base  */
				if (v1 <= 34.5)
					uqi_sc_push (UQ_SCALAR, exp(v1) );
				else
					{
					 UQI_cer2 = UU_TRUE;
					 /* gmessage (D_ksws, " warning - exceed machine value range"); */
	        	 		 uu_uerror0 (UQ_CALCERROR, 36);
					}
				break;

	 case 25  :							/* squre root  				*/
				if (v1 < 0)
				  {
					UQI_cer2 = UU_TRUE;
					/* gmessage (D_ksws, " error - sqrt can't have negative operand "); */
	        	 		 uu_uerror0 (UQ_CALCERROR, 37);
				  }
				else
					uqi_sc_push (UQ_SCALAR, sqrt(v1) );
				break;

	 case 26  :							/* absolute value				*/
				v1 = (v1 > 0) ? v1 : -v1;
				uqi_sc_push (UQ_SCALAR, v1);
				break;

	 default  :
				/* gmessage (D_ksws, "error - invalid operator!!"); */
	        	 	 uu_uerror0 (UQ_CALCERROR, 38);
				break;
	}	/* switch */
	}	/* if */
	else
	  {
		UQI_cer2 = UU_TRUE;
		/* gmessage (D_ksws, " error - illegal operand type "); */
  	 	 uu_uerror0 (UQ_CALCERROR, 27);
	  }
}		/* uqi_evala1 */





/*********************************************************************
**    I_FUNCTION     :  uqi_evala2 ()
**       uqi_evaluate unit vector, magnitude, change degree to radian,
**       change radian to degree, a given angle's complement and
**       supplement.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_evala2 ()

{
	int  op, typ1, typ2, i;
	UU_REAL  val1[3], val2[3], vmag;
	UU_REAL um_mag();
	char  temp[20];

	uqi_cpop (&typ1, val1);
	uqi_cpop (&typ2, val2);
	op = val2[0];

	switch (op)
	{
	 case  1 :					/* unit vector  */
			 if ((typ1 >= UM_CARTESIAN) && (typ1 <= UM_VSPHERICAL))
				{
				 um_cotovc(val1, typ1, val2);
				 um_unitvc (val2, val1);
				 um_vctoco (val1, typ1, val2);
				 uqi_cor_push (typ1, val2);
			   }
			 else
				{
				 UQI_cer2 = UU_TRUE;
				 /* gmessage (D_ksws, " error - operand should be a coordinate"); */
  	 	 		 uu_uerror0 (UQ_CALCERROR, 34);
				}
			 break;

	 case  2 :					/* magnitude    */
			 if ((typ1 >= UM_CARTESIAN) && (typ1 <= UM_VSPHERICAL))
				{
				 vmag = 0.0;
				 um_cotovc (val1, typ1, val2);
				 vmag = um_mag (val2);
				 if (vmag != 0)
				 	uqi_sc_push (UQ_SCALAR, 1.0/vmag*vmag*vmag);
				 else
				 	uqi_sc_push (UQ_SCALAR, (UU_REAL) 0.0);
				}
			 else
				{
				 UQI_cer2 = UU_TRUE;
				 /* gmessage (D_ksws, " error - operand should be a coordinate"); */
  	 	 		 uu_uerror0 (UQ_CALCERROR, 34);
				}
			 break;

	 case  3 :					/*  change radian to degree  */
			 if (typ1 == UQ_SCALAR)
				 uqi_sc_push (UQ_SCALAR, val1[0]*UQ_RADIAN);
			 else
				{
				 UQI_cer2 = UU_TRUE;
				 /* gmessage (D_ksws, " error - operand should be scalar"); */
  	 	 		 uu_uerror0 (UQ_CALCERROR, 33);
				}
			 break;
	 case  4 :					/* change degree to radian   */
			 if (typ1 == UQ_SCALAR)
				 uqi_sc_push (UQ_SCALAR, val1[0]/UQ_RADIAN);
			 else
				{
				 UQI_cer2 = UU_TRUE;
				 /* gmessage (D_ksws, " error - operand should be scalar"); */
  	 	 		 uu_uerror0 (UQ_CALCERROR, 33);
				}
			 break;

	 case  5 :					/* complement of an angle */
			 if (typ1 ==  UQ_SCALAR)
				 if (UQI_angflag == UQ_DEG)
					 uqi_sc_push(UQ_SCALAR, 90-val1[0]);
				 else
					 uqi_sc_push(UQ_SCALAR, UM_HALFPI-val1[0]);
			 else
				{
				 UQI_cer2 = UU_TRUE;
				 /* gmessage (D_ksws, " error - operand should be scalar"); */
  	 	 		 uu_uerror0 (UQ_CALCERROR, 33);
				}
			 break;

	 case  6 :
			 if (typ1 == UQ_SCALAR)
				if (UQI_angflag == UQ_DEG)
					uqi_sc_push(UQ_SCALAR, 180-val1[0]);
				else
					uqi_sc_push(UQ_SCALAR, UM_PI-val1[0]);
			 else
				{
				 UQI_cer2 = UU_TRUE;
  	 	 		 uu_uerror0 (UQ_CALCERROR, 33);
				}
			 break;

	 case  7:			/* change coordinate type to COORD  */
			 if (typ1 <= UM_VSPHERICAL)
				{
				 if (typ1 > UM_SPHERICAL)
					 typ1 = typ1 - 3;
				 uqi_cor_push (typ1, val1);
				}
			 else
  	 	 		 uu_uerror0 (UQ_CALCERROR, 34);
			 break;

	 case  8:			/* change coordinate type to VECTOR */
			 if (typ1 <= UM_VSPHERICAL)
				{
				 if (typ1 <= UM_SPHERICAL)
					 typ1 = typ1 + 3;
				 uqi_cor_push (typ1, val1);
				}
			 else
  	 	 		 uu_uerror0 (UQ_CALCERROR, 34);
			 break;

	 default :
			 /* gmessage (D_ksws, " error - invalid vector and angle operation"); */
  	 	 	 uu_uerror0 (UQ_CALCERROR, 39);
	}
}		/* uqi_evala2 */




/*********************************************************************
**    I_FUNCTION     :  uqi_evala3 (op, its, ltr)
**       get a component value from a coordinate
**    PARAMETERS   
**       INPUT  : 
**          op - type of the component to be fetched
**          its- an array contains translation code
**          ltr - index to the "its"
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_evala3 (op, its, ltr)
int op, ltr;
ITSTYPE	its[];

{
	int  vtype, stype, erorflag;
	UU_REAL  val[3], tval[3];
	int i;

   if (uqi_varval (2, its, ltr))
	{
	vtype = op;
	uqi_cpop (&stype, val);
	erorflag = UU_FALSE;

	if ((stype>0) && (stype<UQ_SCALAR))
	{
	switch (op)
	{
	 case  1	:				/*  cartesian */
			if ((stype != UM_CARTESIAN) && (stype != UM_VCARTESIAN))   
			  {
				um_cotovc(val, stype, tval);
				um_vctovc(tval, val);
			  }
			break;

	 case  2	:				/*  cylindrical */
			if ((stype != UM_CYLINDRICAL) && (stype != UM_VCYLINDRICAL))   
			  {
				um_cotovc(val, stype, tval);
			   um_vctoco(tval, vtype, val);
			  }
			if (UQI_angflag == UQ_DEG)
				val[1] = val[1] * UQ_RADIAN;
			break;

	 case  3	:				/*  spherical */
			if ((stype != UM_SPHERICAL) && (stype != UM_VSPHERICAL))   
			  {
				um_cotovc(val, stype, tval);
			   um_vctoco(tval, vtype, val);
			  }
			if (UQI_angflag == UQ_DEG)
			  {
				val[1] = val[1] * UQ_RADIAN;
				val[2] = val[2] * UQ_RADIAN;
			  }
			break;
	 default  :
			erorflag = UU_TRUE;
			/* gmessage (D_ksws, " error - invalid element "); */
  	 	 	uu_uerror0 (UQ_CALCERROR, 41);
			break;
	}
	if (erorflag == UU_FALSE)
	switch (its[ltr].lint)       /* which component  */
	{
	 case  1  :  uqi_sc_push (UQ_SCALAR, val[0]);
					 break;

	 case  2  :  uqi_sc_push (UQ_SCALAR, val[1]);
					 break;

	 case  3  :  uqi_sc_push (UQ_SCALAR, val[2]);
					 break;

	 default  :  /* gmessage (D_ksws, " error - no such axis "); */
  	 	     uu_uerror0 (UQ_CALCERROR, 40);
					 break;
   }	/* switch */
	}	/* if     */
	else
      /* gmessage (D_ksws, " error - variable is not a coordinate type"); */
  	  uu_uerror0 (UQ_CALCERROR, 42);
	}	/* if  */

}		/* uqi_evala3 */





/*********************************************************************
**    I_FUNCTION     :  UU_REAL uqi_dang (ang)
**       given an angle of any degree, this routine cut it to an  
**       degree within 360 
**    PARAMETERS   
**       INPUT  : 
**          ang - an angle of any degree
**       OUTPUT :  
**          output
**    RETURNS      : a real number of angle
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_REAL uqi_dang (ang)
UU_REAL	ang;

{
	UU_REAL	j;
	
	if (ang > UM_FUZZ)
		for (j=ang-360.0; j>UM_FUZZ; ang=ang-360.0,j=ang-360.0);
	else
		{
		 for (j=ang+360.0; j<UM_FUZZ; ang=ang+360.0,j=ang+360.0);
		 ang = ang + 360.0;
		}
	return (ang);
}
