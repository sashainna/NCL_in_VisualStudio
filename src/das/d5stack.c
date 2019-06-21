
/*********************************************************************
**    NAME         :  	d5stack.c
**       CONTAINS:
**       	udi_chkpostf
**				udi_ckcart
**				udi_ckcylindr
**				udi_ckincfm
**				udi_ckspherical
**				udi_dgcord
**				udi_dgincremnt
**				udi_dgnum
**				udi_dgresult
**				udi_dinitflag			
**				udi_dits1push
**				udi_dits2push
**				dit3push
**				udi_dpop
**				udi_dpush
**				udi_dresetstack
**				udi_dsetdebug
**				udi_dstnum
**				udi_dunitrack
**				udi_interval
**				udi_prnresult
**				udi_setpflag
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       d5stack.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:13
*********************************************************************/

#include  "usysdef.h"
#include  "ustdio.h"
#include  "calcom.h"
#include  "cdef.h"
#include  "ddef.h"
#include  "mdcpln.h"
#include  "mdunits.h"
#include	 "uhep.h"
#include	 "tlangtool.h"
#include	 "udebug.h"

static UD_DSTK dstack[UD_dmaxstk];
static UD_DASDATA  ud_dasin;
static int  dstkpt = -1;
static int  dcordtype;
extern int  UDI_der1;				/* error flag */
static int 	dtime;			/* number of unit specified by the operator */
static int	dunitf;			/* flag to indicate an occurennce of unit */
static int  dwunitf;			/* flag to indicate the correctness of unit */
static int  postflag;		/* flag for the pre|post increment */
static int  ddebug;
static int	iszaxis = UU_FALSE;	/* no z axis						*/

int	UDI_dfsflag;					/* flag for the function name  */
extern int   UDI_dtypeflag;				/* das input value type        */
char	UDI_dftext[UD_SYMSIZE];		/* array to contain the functin name */




/*********************************************************************
**    I_FUNCTION     :  udi_dinitflag ()
**       init all the flag
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_dinitflag ()

{
	UDI_der1 = UU_FALSE;
	dtime = 0;
	dunitf = UU_FALSE;
	dwunitf = UU_TRUE;
	postflag = UU_FALSE;
	UDI_dfsflag = UU_FALSE;
}	/* udi_dinitflag */





/*********************************************************************
**    I_FUNCTION     :  udi_dresetstack ()
**       reset the das's input stack to empty
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_dresetstack ()

{
	dstkpt = -1;
}		/* udi_dresetstack */




/*********************************************************************
**    I_FUNCTION     :  udi_dsetdebug (op)
**       set or reset the debug flag
**    PARAMETERS   
**       INPUT  : 
**          op - debug flag
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_dsetdebug (op)
int  op;

{
	ddebug = op;
}	/* udi_dsetdebug */




/*********************************************************************
**    I_FUNCTION     :  udi_dpush (ptype, pval)
**       push a scalar value and its type to the stack
**    PARAMETERS   
**       INPUT  : 
**          ptype - type of the value(w|w/o unit)
**          pval	 - number to be put to the stack
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_dpush (ptype, pval)
int  ptype;	
UU_REAL  pval;

{
	uu_denter(UU_DTRC,(us,"entering udi_dpush,type=%d,val=%g",ptype,pval))
    if (dstkpt == UD_dmaxstk)
       /* derror (" error - stack overflow"); */
		uu_uerror0 (DASINERROR, 2);
    else
	 {
	  if (ddebug)
    	   printf (" udi_dpush - %d, %g  \n",ptype, pval);
	   dstack[++dstkpt].dtype = ptype;
		dstack[dstkpt].dval = pval;
	 }
	uu_dexit;
}	/* udi_dpush */





/*********************************************************************
**    I_FUNCTION     :  udi_dpop ( pptype, ppval )
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

udi_dpop ( pptype, ppval )
int *pptype;
UU_REAL *ppval;

{  int  i;
	uu_denter(UU_DTRC,(us,"entering udi_dpop"))
   if (dstkpt < 0)
	  {
		UDI_der1 = UU_TRUE;
		*pptype = 10;
		*ppval = 0;
	   /* derror (" error - stack is underflow"); */
		uu_uerror0 (DASINERROR, 3);
	  }
   else
       {
		 *pptype = dstack[dstkpt].dtype;
		 *ppval  = dstack[dstkpt--].dval;
        if (ddebug)
	        printf (" udi_dpop - %d,  %g\n", *pptype, *ppval);
		 uu_dprint(UU_DTRC,(us,"type=%d,val=%g",*pptype,*ppval));
		 }
	uu_dexit;
}	/* udi_dpop */





/*********************************************************************
**    I_FUNCTION     :  udi_dits1push (its, ltr, op)
**       get an input number from the "its" which contains the 
**       translation code
**    PARAMETERS   
**       its - an array contains the translation code
**       ltr - index to the its
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_dits1push (its, ltr, op)
ITSTYPE  its[];
int  ltr, op;

{
	  UU_REAL num;

	uu_denter(UU_DTRC,(us,"entering ud_dits1push, ltr=%d, its[]=%d",
		ltr,its[ltr-1].lint));
     if ((its[ltr-1].lint == 1) || (its[ltr-1].lint == 2))
		 {
		  if (op==2)		/* real number */
				num = its[ltr].lreal;
		  else if (op==1)
					{
					 if (its[ltr].lint <= 0)
						{
						 UDI_der1 = UU_TRUE;
						 /* derror (" error - incremental number can't be zero or negative"); */
						 uu_uerror0 (DASINERROR, 4);
						}
					 num = its[ltr].lint;
					}
			udi_dpush (UD_WOUNIT, num);
		  }
      else
	      /* derror (" error - wrong translation code"); */
		   uu_uerror0 (DASINERROR, 5);
uu_dexit;
}	/* udi_dits1push */

  



/*********************************************************************
**    I_FUNCTION     :  udi_dits2push (its, ltr)
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

udi_dits2push (its, ltr)
ITSTYPE  its[];
int  ltr;

{
	  UU_REAL num;
     
	  uu_denter(UU_DTRC,(us,"entering udi_dits2push,lint=%d",its[ltr-2].lint))
	  if (its[ltr-2].lint == 5)
        {
	    	num = its[ltr].lint;
			udi_dpush (UD_WUNIT, num);
			dunitf = UU_TRUE;
        }
      else
	      /* derror (" error - wrong translation code"); */
			uu_uerror0 (DASINERROR, 5);
	  uu_dexit;
}	/* udi_dits2push */






/*********************************************************************
**    I_FUNCTION     :  udi_dits3push (its, ltr)
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

udi_dits3push (its, ltr)
ITSTYPE  its[];
int  ltr;

{
	  UU_REAL num;

	  uu_denter(UU_DTRC,(us,"entering udi_dits2push,lint=%d",its[ltr-1].lint))
     if ((its[ltr-1].lint == 1) || (its[ltr-1].lint == 2)) 
		 {
			/* num = *(float *)(&its[ltr]); */
			num = its[ltr].lreal;
			if (UM_cpln.angle_unit == UM_DEGR)
				num = num / UQ_RADIAN;
			udi_dpush (UD_WOUNIT, num);
		  }
      else
	      /* derror (" error - wrong translation code"); */
			uu_uerror0 (DASINERROR, 5);
		uu_dexit;
}	/* udi_dits1push */

  

/*********************************************************************
**    I_FUNCTION     :  udi_dunitrack ()
**       keep track whether a number is followed by an unit
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_dunitrack ()

{
	uu_denter(UU_DTRC,(us,"entering udi_dunitrack"));
	dtime++;
	if (dunitf == UU_FALSE)
		dwunitf = UU_FALSE;
	dunitf = UU_FALSE;
	uu_dexit;
}	/* udi_dunitrack */




/*********************************************************************
**    I_FUNCTION     :  udi_dstnum ()
**       calculate the input number and convert it to internal 
**       unit - 'cm'
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_dstnum ()

{
	int	i, typ1, typ2, tmptype;
	UU_REAL  sum, val1, val2;

	uu_denter(UU_DTRC,(us,"entering udi_dstnum"));
	sum = 0.0;
	if ((dtime > 1) && (dwunitf == UU_FALSE))
		{
		 /* derror (" error - illegal input formate "); */
		 uu_uerror0 (DASINERROR, 6);
		 UDI_der1 = UU_TRUE;
		}
	else if (dwunitf == UU_TRUE)		/* units are associated   */
			  {
				switch (UDI_dtypeflag)
				{
				 case UD_DISTANCE:
					for (i=0; i<dtime; i++)
					{
					 udi_dpop (&typ1, &val1);
					 udi_dpop (&typ2, &val2);
					 tmptype = val1;
					 udi_interval (tmptype, val2, &sum);
					}	/* for */
			  		if (UDI_der1 == UU_FALSE)
				  		udi_dpush (UD_WUNIT, sum);
					break;

				case UD_DANGLE:
					if (dtime==1)
					 {
					  udi_dpop (&typ1, &val1);
					  udi_dpop (&typ2, &val2);
					  if (val1 > 7)				/* eith degree or radian */
						{
					    if (val1 == 8)					/* an degree	*/
					  	   val2 = val2 / UQ_RADIAN;
			  		    if (UDI_der1 == UU_FALSE)
							udi_dpush (UD_WOUNIT, val2);
						}
					  else
					   {
		 				  uu_uerror0 (DASINERROR, 7);	/* expect angle or unitless */	
			  		     UDI_der1 = UU_TRUE;
					   }
					 }
					else
					 {
		 				uu_uerror0 (DASINERROR, 6);		/* illegal input */
			  		   UDI_der1 = UU_TRUE;
					 }
					break;

			   default:
 				   /* derror ("error - Angle or unitless number is expected"); */
				   uu_uerror0 (DASINERROR, 7);
				   UDI_der1 = UU_TRUE;
					break;
			  }
			}
		 else				/*   no unit associated   */
			 if (dtime > 1)		/* more than one number  */
				 /* derror (" error - invalid format"); */
				uu_uerror0 (DASINERROR, 8);
			 else
				 {
				  udi_dpop (&typ1, &val1);
				  /* printf("dtypeflag = %d\n", UDI_dtypeflag); */
				  switch (UDI_dtypeflag)
					{
					 case  UD_DISTANCE :
				  			udi_dpush (UD_WOUNIT, val1*UM_cpln.length_to_cm);
							break;
					 case  UD_DANGLE :
							if (UM_cpln.angle_unit == UM_DEGR)
								val1 = val1 / UQ_RADIAN;
							udi_dpush (UD_WOUNIT, val1);
							break;
					 case  UD_UNITLESS :
							udi_dpush (UD_WOUNIT, val1);
							break;
					 default  :
							/* derror (" error - unspecified input value type "); */
							uu_uerror0 (DASINERROR, 11);
							UDI_der1 = UU_TRUE;
							break;
					}
				 }
	dtime = 0;
	dwunitf = UU_TRUE;
	uu_dexit;
}	/* udi_dstnum */



/*********************************************************************
**    I_FUNCTION :  udi_scinum()
**       Get each individual number of a scientific notation, then put
**			them back into a real number.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
udi_scinum()
{
	int	typ1, typ2;
	UU_REAL	val1, val2;
	UU_TRUEDOUBLE	val3;
	char	buf[80];

	uu_denter(UU_DTRC,(us,"entering udi_scinum"));
   udi_dpop (&typ1, &val2);
   udi_dpop (&typ2, &val1);
	sprintf(buf, "%ge%g", val1, val2);
#if (UU_COMP == UU_IRIS) || (UU_COMP==UU_IRIS4D)
	sscanf(buf,"%f", &val3);		/* IRIS can't take %lf  */
#else
	sscanf(buf,"%lf", &val3);	/* make sure the scientific noation converted into double */
#endif
	sscanf(buf,"%lf", &val3);		/* make sure the scientific noation converted into double */
	uu_dprint(UU_DTRC,(us,"val1=%g,val2=%g,buf=%s,val3=%g",val1,val2,buf,val3));
	val1 = val3;
	udi_dpush (UD_WOUNIT, val1);
	uu_dexit;
}	/* udi_scinum */


/*********************************************************************
**    I_FUNCTION     :  udi_interval (unitype, val, sum)
**       convert different scale number to a scale of 'cm'
**    PARAMETERS   
**       INPUT  : 
**          unitype - number's unit
**          val		- number to be converted
**       OUTPUT :  
**          sum		- value after converted 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_interval (unitype, val, sum)
int  unitype;
UU_REAL val, *sum;

{
	uu_denter(UU_DTRC,(us,"entering udi_interval"));
	switch(unitype)
	{
	 case 0	:	*sum = *sum + val*UM_cpln.conv_factors[UM_INCH];
					break;

	 case 1	:	*sum = *sum + val*UM_cpln.conv_factors[UM_FEET];
					break;

	 case 2 :	*sum = *sum + val*UM_cpln.conv_factors[UM_MILE];
					break;

	 case 3 :	*sum = *sum + val*UM_cpln.conv_factors[UM_MM];
					break;

	 case 4 :	*sum = *sum + val*UM_cpln.conv_factors[UM_CM];
					break;

	 case 5 :	*sum = *sum + val*UM_cpln.conv_factors[UM_M];
					break;

	 case 6 :	*sum = *sum + val*UM_cpln.conv_factors[UM_KM];
					break;

	 case 7 :	*sum = *sum + val*UM_cpln.conv_factors[UM_MIL];
					break;

	 case 8 :   *sum = *sum + val*UM_cpln.conv_factors[UM_INCH];
					break;

	 default:  /* derror (" error - illegal unit"); */
				  uu_uerror0 (DASINERROR, 9);
				  UDI_der1 = UU_TRUE;
				  break;
	}	/* switch */
	uu_dexit;
}	/* udi_interval */




/*********************************************************************
**    I_FUNCTION     :  udi_chkz ()
**			set the flag to tell the Z axis exist
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_chkz ()

{
	uu_denter(UU_DTRC,(us,"entering udi_chkz"));
	if (UDI_der1 == UU_FALSE)
		iszaxis = UU_TRUE;
	uu_dexit;
}	/*  udi_chkz  */




/*********************************************************************
**    I_FUNCTION     :  udi_ckcart ()
**       set the the cartesian  flag
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_ckcart ()

{

	uu_denter(UU_DTRC,(us,"entering udi_ckcart"));
	if (UDI_der1 == UU_FALSE)
		{
		 dcordtype = UM_CARTESIAN;
		 if (!iszaxis)
			udi_dpush (UD_WOUNIT, (UU_REAL) 0.0);
		 else
			iszaxis = UU_FALSE;
		}
	uu_dexit;
}	/*  udi_ckcart  */




/*********************************************************************
**    I_FUNCTION     :  udi_ckcylindr ()
**       set the the cylindrical flag 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_ckcylindr ()

{
	UU_REAL  val1, val2, val3;
	int   typ1, typ2, typ3;

	uu_denter(UU_DTRC,(us,"entering udi_ckcylindr"));
	if (UDI_der1 == UU_FALSE)
		 dcordtype = UM_CYLINDRICAL;
	uu_dexit;
}	/* udi_ckcylindr */





/*********************************************************************
**    I_FUNCTION     :  udi_ckspherical ()
**       set the the spherical flag 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_ckspherical ()

{
	UU_REAL  val1, val2, val3;
	int   typ1, typ2, typ3;

	uu_denter(UU_DTRC,(us,"entering udi_ckspherical"));
	if (UDI_der1 == UU_FALSE)
		 dcordtype = UM_SPHERICAL;
	uu_dexit;
}	/* spherical */






/*********************************************************************
**    I_FUNCTION     :  udi_setpflag ()
**       set the post increment flag 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_setpflag ()

{
	uu_denter(UU_DTRC,(us,"entering udi_setpflag"));
	postflag = UU_TRUE;
	uu_dexit;
}	/* setflag */





/*********************************************************************
**    I_FUNCTION     :  udi_chkpostf ()
**       check the incremental method and put the corresponding infor-
**       mation on the stack
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_chkpostf ()

{
	uu_denter(UU_DTRC,(us,"entering udi_chkpostf"));
	if (postflag)
		udi_dpush (0,(UU_REAL) 1.0);
	else udi_dpush (0,(UU_REAL) 0.0);
	postflag = UU_FALSE;
	uu_dexit;
}	/* udi_chkpostf */





/*********************************************************************
**    I_FUNCTION     :  udi_dgnum ()
**       get a number from the stack
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_dgnum ()

{
	int type;
	UU_REAL val;

	uu_denter(UU_DTRC,(us,"entering udi_dgnum"));
	if (UDI_der1 == UU_FALSE)
		{
		 udi_dpop (&type, &val);
		 ud_dasin.dtype = UD_DSCALAR;
		 ud_dasin.stval.dval = val;
		}
	uu_dexit;
}	/* udi_dgnum */




/*********************************************************************
**    I_FUNCTION     :  udi_dgcord (op)
**       get the reference coordinate
**    PARAMETERS   
**       INPUT  : 
**          op - indicator for reference or normal coordinate
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_dgcord (op)
int	op;

{
	int  i, type;
	UU_REAL	v1[3];

	uu_denter(UU_DTRC,(us,"entering udi_dgcord,op=%d",op));
	if (UDI_der1 == UU_FALSE)
		{
		 if (op == 1)
		 	ud_dasin.dtype = UD_DREFERENCE;
		 else if (op == 2)
			ud_dasin.dtype = UD_DCOORDINATE;
		 else if (op == 3)
			ud_dasin.dtype = UD_DELTAPOINT;
		 for (i=2; i>=0; i--)
			 udi_dpop (&type, &ud_dasin.stval.stcord.coord[i]);
/*-----------
		 if (dcordtype != UM_CARTESIAN)
			{	
		 	 um_cotocc(dcordtype,ud_dasin.stval.stcord.coord,v1);
		 	 for (i=0; i<3; i++)
				ud_dasin.stval.stcord.coord[i] = v1[i];
		   }
		 ud_dasin.stval.stcord.cordtype = UM_CARTESIAN;
------------*/
		 ud_dasin.stval.stcord.cordtype = dcordtype;
		}
		uu_dexit;
}	/* dgref */




/*********************************************************************
**    I_FUNCTION     :  udi_ckincfm (its, ltr)
**       check the incremental format
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

udi_ckincfm (its, ltr)
int	 ltr;
ITSTYPE	its[];

{
	uu_denter(UU_DTRC,(us,"entering udi_ckincfm"));
	ud_dasin.dtype = its[ltr].lint;
	uu_dexit;
}	/* udi_ckincfm  */




	
/*********************************************************************
**    I_FUNCTION     :  udi_dgincremnt ()
**       get the incremental information
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_dgincremnt ()

{
	int  type ,i;
	UU_REAL val;

	uu_denter(UU_DTRC,(us,"entering udi_dgincremnt"));
	if (UDI_der1 == UU_FALSE)
		{
		 ud_dasin.stval.stcord.cordtype = dcordtype;
		 for (i=2; i>=0; i--)
			{
		 	 udi_dpop(&type,&val);
		 	 ud_dasin.stval.stcord.prepostflag[i] = val;
		 	 udi_dpop(&type,&val);
		 	 ud_dasin.stval.stcord.incnum[i] = val;
			}
		 for (i=2; i>=0; i--)
		 	 udi_dpop(&type,&ud_dasin.stval.stcord.coord[i]);
		}
		uu_dexit;
}	 /* udi_dgincremnt */





/*********************************************************************
**    I_FUNCTION     :  udi_prnresult ( xdasin )
**       A test routine to print out the result befor sending back to
**			the request routine
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_prnresult ( xdasin )
UD_DASDATA  *xdasin;

{
	int  i;

	printf ("*** TYPE = %d\n", xdasin->dtype);
	switch (xdasin->dtype)
	{
	 case  UD_DSCALAR:
			 printf ("*** dval =%g\n", xdasin->stval.dval);
			 break;

	 case  UD_DCOORDINATE :
	 case  UD_DREFERENCE  :
	 case  UD_DELTAPOINT :
			 printf ("*** CORDTYPE = %d\n", xdasin->stval.stcord.cordtype);
			 printf ("*** <x,y,z>=<%g,%g,%g>\n",xdasin->stval.stcord.coord[0],xdasin->stval.stcord.coord[1],xdasin->stval.stcord.coord[2]);
			 break;

	 case  UD_DNINCREMENT  :
	 case  UD_DQINCREMENT  :
			 printf ("*** FUNCFLAG = %d\n", xdasin->funcflag);
			 if (xdasin->funcflag)
				printf ("*** FUNCNAME = %s\n", xdasin->funcname);
			 printf ("*** CORDTYPE = %d\n", xdasin->stval.stcord.cordtype);
			 printf ("*** coord = ");
			 for (i=0; i<3; i++)
				printf ("%g ", xdasin->stval.stcord.coord[i]);
			 printf ("\n*** incnum = " );
			 for (i=0; i<3; i++)
				printf ("%d ", xdasin->stval.stcord.incnum[i]);
			 printf ("\n*** ppflag = ");
			 for (i=0; i<3; i++)
				printf ("%d ", xdasin->stval.stcord.prepostflag[i]);
			 printf ("\n");
			 break;

	 default :
			 printf ("   invalid type\n");
			 break;
	}
}	/* udi_prnresult */



/*********************************************************************
**    I_FUNCTION     :  udi_dgresult ( xdasin )
**       Pack the result into a structure to be used by the requsting
**			routine
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

udi_dgresult ( xdasin )
UD_DASDATA  *xdasin;

{
	int  i;

	uu_denter(UU_DTRC,(us,"entering udi_dgresult"));
	xdasin->dtype = ud_dasin.dtype;
	switch (xdasin->dtype)
	{
	 case  UD_DSCALAR:
			 xdasin->stval.dval = ud_dasin.stval.dval;
			 break;

	 case  UD_DCOORDINATE :
	 case  UD_DREFERENCE  :
	 case  UD_DELTAPOINT  :
			 for (i=0; i<3; i++)
				xdasin->stval.stcord.coord[i] = ud_dasin.stval.stcord.coord[i];
			 xdasin->stval.stcord.cordtype = ud_dasin.stval.stcord.cordtype;
			 break;

	 case  UD_DNINCREMENT  :
	 case  UD_DQINCREMENT  :
			 xdasin->funcflag = UDI_dfsflag;
			 if (UDI_dfsflag)
				strcpy (xdasin->funcname, UDI_dftext);
			 xdasin->stval.stcord.cordtype = ud_dasin.stval.stcord.cordtype;
			 for (i=0; i<3; i++)
				xdasin->stval.stcord.coord[i] = ud_dasin.stval.stcord.coord[i];
			 for (i=0; i<3; i++)
				xdasin->stval.stcord.incnum[i] = ud_dasin.stval.stcord.incnum[i];
			 for (i=0; i<3; i++)
				xdasin->stval.stcord.prepostflag[i] = ud_dasin.stval.stcord.prepostflag[i];
			 break;

	 default :
			 /* derror (" error - invalid type"); */
			 uu_uerror0 (DASINERROR, 10);
			 break;
	}
	uu_dexit;
}	/* udi_dgresult */

