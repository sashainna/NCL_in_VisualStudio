/*********************************************************************
**    NAME         :  ascutil.c
**       CONTAINS:
**			int ua_trunc(value)
**			UU_REAL us_calpow (type, iv1, iv2, val1, val2)
**			us_fint (rval)
**			UU_REAL us_ifloat (ival)
**			UU_REAL us_bfloat (bval)
**			us_fbyte (rval)
**			us_ibyte (ival)
**			us_fround (rval)
**			us_ftrunc (rval)
**			unsigned char us_rlogic (rval)
**			unsigned char us_ilogic (ival)
**			unsigned char us_blogic (bval)
**			unsigned char us_chlogic (chstr)
**			char *us_substr (str, start, len, temp)
**			char *us_intstr (s1, s2, start)
**			us_ctoscart(cp3,cp2,cp1,type,sc)
**			UU_REAL us_compsctosc(sc,stype,comp)
**			UU_REAL us_scdistsc(cc1,cc2)
**			UU_REAL us_magsc(v1)
**			UU_REAL us_scdotsc(v1,v2)
**			UU_REAL us_scangsc(vc1,vc2)
**			us_chscomp(vr, op, value)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       ascutil.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:39
*********************************************************************/

#include  "ustdio.h"
#include  "umath.h"
#include  "usysdef.h"
#include  "mdcoord.h"
#include  "mdmatrix.h"
#include  "asalc.h"
#include  "modef.h"
#include  "calcom.h"
#include  "cdef.h"

/*********************************************************************
**    E_FUNCTION :  integer ua_trunc(value)
**       Truncate a real number to an integer
**    PARAMETERS   
**       INPUT  : 
**          value - real to truncate
**       OUTPUT :  
**          output
**    RETURNS      : interger
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_trunc(value)
UU_REAL value;
{
	int temp;

	temp = value;

	return(temp);
}



/******************************************************************/
/**
	NAME :		scalpow
			check the valid value to perform the power evaluation,

	PARAMETERS:
		input :
			type - type of the input
			iv1 - base of the power expression (integer)
			val1 - base of the power expression (real)
			iv2 - power (integer)
			val2 - power (real)

	RETURN : the result
**/
/******************************************************************/

UU_REAL us_calpow (type, iv1, iv2, val1, val2)
int	type, iv1, iv2;
UU_REAL  val1, val2;

{
	int	k;
	UU_REAL	power;
	UU_REAL  x, v1, v2;

	if (type == 0)
		{
		 val1 = iv1;
		 val2 = iv2;
		}
	if ((val1==0) && (val2==0))
		ud_printmsg ("-E- Illegal operand value for power operation\n");
	else if (val1 < 0)
			 {
			  k = val2;   x = k;
			  if (x == val2)		/* power is an integer */
				  power = ((k%2)==0) ? pow(-val1,val2) : -pow(-val1,val2);
			  else
				ud_printmsg ("-E- Illegal operand value for power operation\n");
			 }
		  else
			  	power = pow(val1, val2);
	return (power);
}	/* scalpow */




/********************************************************************/
/**
	NAME :			sfint
			return an integer number for a real 
**/
/********************************************************************/

us_fint (rval)
UU_REAL	rval;

{
	return (rval);
}	/*  sfint  */



/********************************************************************/
/**
	NAME:			sifloat
		  return a real number for an integer
**/
/********************************************************************/

UU_REAL us_ifloat (ival)
int	ival;

{
	UU_REAL  rval;
	rval=ival;
	return (rval);
}	/* sifloat */



/********************************************************************/
/**
	NAME:			sbfloat
		  return a real number for an byte
**/
/********************************************************************/

UU_REAL us_bfloat (bval)
unsigned char	bval;

{
	UU_REAL  rval;
	rval=bval;
	return (rval);
}	/* sbfloat */



/********************************************************************/
/**
	NAME:			sfbyte
			return a byte for a real number
**/
/********************************************************************/

us_fbyte (rval)
UU_REAL	rval;

{
	unsigned	char	rtn;
	int	i;

	i = rval;
	if ((i<0) || (i>255))
		{
		 ud_printmsg("The input number for the BYTE function is out of range\n");
		 rtn = 0;
		}
	else
		rtn = i;
	return (rtn);
}	/* sfbyte */



/********************************************************************/
/**
	NAME:			sibyte
			return a byte for a real number
**/
/********************************************************************/

us_ibyte (ival)
int	ival;

{
	unsigned	char	rtn;

	if ((ival<0) || (ival>255))
		{
		 ud_printmsg("The input number for the BYTE function is out of range\n");
		 rtn = 0;
		}
	else
		rtn = ival;
	return (rtn);
}	/* sibyte */



/********************************************************************/
/**
	NAME:			sfround
		  return a rounded real number
**/
/********************************************************************/

us_fround (rval)
UU_REAL	rval;

{
	char	temp[30];
	register		i, k;
	int   j, len, rtn;

	j = 5;
	sprintf (temp, "%f", rval);
	for (i=0; temp[i]!='.'; i++);
	len = strlen(&temp[i+1]);		/* length of the decimal points  */
	for (k=2; (k<=len)&&(k<=3); j=10*j,k++);
	if (len > 3)
		temp[i+4] = '\0';
	sscanf (&temp[i+1], "%d", k);	/* change decimal points to integer */
	temp[i] = '\0';
	sscanf (temp, "%d", &rtn);		/* put integer part to a integer  */

	if ( k < j )		/* truncated */
		return (rtn);
	else					/* round up  */
		return (rtn++);
}		/* sfround */




/********************************************************************/
/**
	NAME :		sftrunc
			return a truncated real number's value
**/
/********************************************************************/

us_ftrunc (rval)
UU_REAL	rval;

{
	char	temp[30];
	register		i;
	int   rtn;

	sprintf (temp, "%f", rval);
	for (i=0; temp[i]!='.'; i++);
	temp[i] = '\0';
	sscanf (temp, "%d", &rtn);		/* put integer part to a integer  */
	return (rtn);
}	/* sftrunc */



/********************************************************************/
/**
	NAME :		srlogic
			return TRUE for any nonzero real number, otherwise return
			FALSE
**/
/********************************************************************/

unsigned char us_rlogic (rval)
UU_REAL	rval;

{
	unsigned char  rtn;

	if (fabs(rval) < UM_FUZZ)
		return (rtn=0);
	else
		return (rtn=1);
}	/* srlogic */


	

/********************************************************************/
/**
	NAME :		silogic
			return TRUE for any nonzero integer number, otherwise return
			FALSE
**/
/********************************************************************/

unsigned char us_ilogic (ival)
int	ival;

{
	unsigned char  rtn;

	if (abs(ival) == 0)
		return (rtn=0);
	else
		return (rtn=1);
}	/* silogic */




/********************************************************************/
/**
	NAME :		sblogic
			return TRUE for any nonzero byte number, otherwise return
			FALSE
**/
/********************************************************************/

unsigned char us_blogic (bval)
unsigned char	bval;

{
	unsigned char  rtn;

	if (abs(bval) == 0)
		return (rtn=0);
	else
		return (rtn=1);
} /* sblogic */




/********************************************************************/
/**
	NAME :		schlogic
			return TRUE for any nonzero length charstr, otherwise return
			FALSE
**/
/********************************************************************/

unsigned char us_chlogic (chstr)
char	chstr[];

{
	unsigned char  rtn;
	register	len;

	if ((len=strlen(chstr)) == 0)
		return (rtn=0);
	else
 		return (rtn=1);
}	/* schlogic */



/********************************************************************/
/**
	NAME :		substr
			reutrn a substring of the specified string

	PARAMETER:
		input :
			str - the specified input string
			start - the start point
			len - length of the substring
			temp - space to hold the result
**/
/********************************************************************/

char *us_substr (str, start, len, temp)
char *str, *temp;
int	start, len;

{
	register		length, i;

	temp[0] = '\0';
  start = start - 1;
	length = strlen(str);
	if (start < length)
		{
		 for (i=0; (start<length)&&(i<len); start++,i++)
			temp[i] = str[start];
		 temp[i] = '\0';
		}
	return (temp);
}	/* substr */



/********************************************************************/
/**
	NAME :		intstr
			Insert the second string into the first string

	PARAMETER:
		input :
			s1 - the first string
			s2 - the second string
			start - the start point of the first string to be added at
**/
/********************************************************************/

char *us_intstr (s1, s2, start)
char	*s1, *s2;
int	start;

{
	char	temp1[256], temp2[256];

	strcpy (temp1,&s1[start]);
	s1[start] = '\0';
	strcpy (temp2,s1);
	sprintf (s1, "%s%s%s", temp2, s2, temp1);
	return (s1);
}	/* intstr */




/*********************************************************************
**    E_FUNCTION     : us_ctoscart(sc,cc)
**      Convert a SAL  coordinate represented in either  CARTESIAN,
**	  	  CYLINDRICAL, or SPHERICAL  coordinates into an internal 
**      CARTESIAN  coordinate system.
**    PARAMETERS   
**       INPUT  : 
**				cp1,cp2,cp3 - component of the input coordinate
**        type - type of the input coordinate
**       OUTPUT :  
**				sc - internal SAL coordinate (cartesian)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
us_ctoscart(cp3,cp2,cp1,type,sc)
  UU_REAL   cp3, cp2, cp1;
  int  type;
  US_COORD  *sc;

	{
	switch (type)
		{
		case  UM_CARTESIAN:      
      sc->x = cp1;
      sc->y = cp2;
      sc->z = cp3;
			break;
		case  UM_CYLINDRICAL:
      cp2 = cp2 / UQ_RADIAN;
			sc->x = cp1 * cos(cp2);
			sc->y = cp1 * sin(cp2);
			sc->z = cp3;
			break;
		case  UM_SPHERICAL:
      cp2 = cp2 / UQ_RADIAN;
      cp3 = cp3 / UQ_RADIAN;
			sc->x = cp1 * cos(cp2) * sin(cp3);
			sc->y = cp1 * sin(cp2) * sin(cp3);
			sc->z = cp1 * cos(cp3);
			break;
		default:
			break;
		}
	}  /* sctoscart */



/*********************************************************************
**    E_FUNCTION     : compsctosc(sc,stype,comp)
**      Convert an internally represented SAL CARTESIAN 
**			coordinate to the specified SAL component.
**    PARAMETERS   
**       INPUT  : 
**				sc        internal SAL CARTESIAN  coordinate
**          stype     type to convert to
**        comp - component
**       OUTPUT :  
**				
**    RETURNS      : the component value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL us_compsctosc(sc,stype,comp)
  US_COORD *sc;
	int stype, comp;

	{
  UU_REAL   rad;

	switch (stype)
		{
		case  UM_CYLINDRICAL:
      if (comp==0)
		  	return(sc->x*sc->x + sc->y*sc->y);
      else
			  return(atan2(sc->y, sc->x));

		case  UM_SPHERICAL:
			rad = sqrt(sc->x*sc->x + sc->y*sc->y + sc->z*sc->z);
			if (rad < UM_FUZZ)
        return ((UU_REAL)  0.0);
			else
				{
         if (comp==0)
           return (rad);
         else if (comp==1)
				        return(atan2(sc->y,sc->x));
         else if (comp==2)
				        return(acos(sc->z/rad));
				}
		default:
			break;
		}
	}  /* compsctosc */





/*********************************************************************
**    E_FUNCTION     : scdistsc(cc1,cc2)
**      Calculate the distance between two points represented in
**			the internal CARTESIAN coordinate format.
**    PARAMETERS   
**       INPUT  : 
**				cc1        first internal  coordinate
**          cc2        second internal  coordinate
**       OUTPUT :  
**				dist       distance between points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL us_scdistsc(cc1,cc2)
US_COORD  *cc1, *cc2;

{
  UU_REAL  dist;

  um_dcccc(cc1,cc2,&dist);
  return(dist);
} /* scdistsc */



/*********************************************************************
**    E_FUNCTION     : magsc(v1)
**      Calculate magnitude of a vector.
**    PARAMETERS   
**       INPUT  : 
**				v1         vector
**       OUTPUT :  
**				mag        magnitude
**    RETURNS      : magnitude
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL us_magsc(v1)
US_COORD  *v1;

{
  UU_REAL  rmag;

  um_mag(v1, &rmag);
  return(rmag);
} /* magsc */




/*********************************************************************
**    E_FUNCTION     : scdotsc(v1,v2)
**      Calculate the dot product of two vectors.
**    PARAMETERS   
**       INPUT  : 
**				v1          first vector
**          v2          second vector
**       OUTPUT :  
**				rdot        dot product
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL us_scdotsc(v1,v2)
US_COORD *v1, *v2;

{
  UU_REAL rdot;

  um_dot(v1,v2,&rdot);
  return(rdot);
	} /* scdotsc */



/*********************************************************************
**    E_FUNCTION     : scangsc(vc1,vc2)
**      Calculate the angle between two vectors.
**    PARAMETERS   
**       INPUT  : 
**				vc1        first vector
**          vc2        second vector
**       OUTPUT :  
**				ang        angle between vectors
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL us_scangsc(vc1,vc2)
US_COORD  *vc1, *vc2;

{
  UU_REAL ang;

  um_angle(vc1,vc2,&ang);
  return(ang);
	}  /* scangsc */



/*********************************************************************
**    I_FUNCTION     :  chscomp(vr,op,value)
**       Change a coordinate's component's value
**    PARAMETERS   
**       INPUT  : 
**          vr - a coordinate
**          op - component of the coordinate
**          value - value to be added
**       OUTPUT :  
**          vr - a coordinate
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

us_chscomp(vr, op, value)
US_COORD  *vr;
int  op;
UU_REAL  value;

{
  register  type;
  UU_REAL  v1[3], vtemp[3];

  type = ((op=='r')||(op=='t'))? UM_CYLINDRICAL : UM_SPHERICAL;
  v1[0] = vr->x;
  v1[1] = vr->y;
  v1[2] = vr->z;
  um_vctoco(v1,type,vtemp);
  switch (op)
   {
    case 'r':
      vtemp[0] = value;
      break;
    case 't':
      vtemp[1] = value;
      break;
    case 'a':
      vtemp[0] = value;
      break;
    case 'b':
      vtemp[1] = value;
      break;
    case 'c':
      vtemp[2] = value;
      break;
    }
  um_cotovc(vtemp,type,v1);
  vr->x = v1[0];
  vr->y = v1[1];
  vr->z = v1[2];
}   /* chscomp */







