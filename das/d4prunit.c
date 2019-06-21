/*********************************************************************
**
**    NAME         :  d2prunit.c
**
**       CONTAINS:
**          ud_uncord
**          ud_unvec
**          ud_undist
**          ud_unang
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d4prunit.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:10
**
*********************************************************************/

#include "usysdef.h"
#include <stdio.h>
#include "mdunits.h"
#include "mdcpln.h"

   static char pbuf[100];  /* string buffer */

/*********************************************************************
**
**    E_FUNCTION :  char *ud_uncord(prec, len, cord)
**       convert model coodinates from internal to user units
**
**    PARAMETERS   
**       INPUT  : 
**          prec = print precision
**          len  = print length
**          cord = model cooordinate
**       OUTPUT :  
**          none
**
**    RETURNS      : pointer to converted string
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

char *ud_uncord(prec, len, wcord)
int prec;                  /* precision */
int len;                   /* length */
UU_REAL wcord[3];          /* coordinate to convert */
{

   UU_REAL x, y, z;        /* temporary reals to hold coordinates */
   UU_REAL ccord[3];       /* local coordinate buffer */
   char frmbuf[100];       /* format buffer */

   um_mcstoccs(0, wcord, ccord);
   x = ccord[0] / UM_cpln.length_to_cm;
   y = ccord[1] / UM_cpln.length_to_cm;
   z = ccord[2] / UM_cpln.length_to_cm;

   len = 1;
   sprintf(frmbuf,"<%%%d.%df%%s,%%%d.%df%%s,%%%d.%df%%s>\0",
               len, prec, len, prec, len, prec);

   sprintf(pbuf, frmbuf,
      x, UM_linear_units_name[UM_cpln.length_unit],
      y, UM_linear_units_name[UM_cpln.length_unit],
      z, UM_linear_units_name[UM_cpln.length_unit]);

   return(pbuf);
}

/*********************************************************************
**
**    E_FUNCTION :  char *ud_unvec(prec, len, cord)
**       convert model vectors from internal to user units
**
**    PARAMETERS   
**       INPUT  : 
**          prec = print precision
**          len  = print length
**          cord = model cooordinate
**       OUTPUT :  
**          none
**
**    RETURNS      : pointer to converted string
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

char *ud_unvec(prec, len, wvec)
int prec;                  /* precision */
int len;                   /* length */
UU_REAL wvec[3];           /* coordinate to convert */
{

   UU_REAL cvec[3];        /* local coordinate buffer */
   char frmbuf[100];       /* format buffer */
   UU_REAL x, y, z;        /* temporary reals to hold coordinates */

   um_mcstoccs(1, wvec, cvec);
   x = cvec[0] / UM_cpln.length_to_cm;
   y = cvec[1] / UM_cpln.length_to_cm;
   z = cvec[2] / UM_cpln.length_to_cm;

   len = 1;
   sprintf(frmbuf,"<%%%d.%df,%%%d.%df,%%%d.%df>\0",
               len, prec, len, prec, len, prec);

   sprintf(pbuf, frmbuf, x, y, z);
   return(pbuf);
}

/*********************************************************************
**
**    E_FUNCTION :  char *ud_undist(prec, len, distance)
**       convert distance value form internal to user units
**
**    PARAMETERS   
**       INPUT  : 
**          distance = distance value in internal units
**          prec = print precision
**          len  = print length
**       OUTPUT :  
**          none
**
**    RETURNS      : pointer to converted string
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

char *ud_undist(prec, len, distance)
int prec;                     /* precision */
int len;                      /* length */
UU_REAL distance;             /* distance value to convert */
{
   UU_REAL locdist;
   char frmbuf[100];          /* format buffer */

   locdist = distance / UM_cpln.length_to_cm;
   len = 1;

   sprintf(frmbuf, "%%%d.%df%%s\0", len, prec);
   sprintf(pbuf, frmbuf, locdist, UM_linear_units_name[UM_cpln.length_unit]);
   return(pbuf);
}

/*********************************************************************
**
**    E_FUNCTION :  char *ud_unang(prec, len, angle)
**       convert angle value form internal to user units
**
**    PARAMETERS   
**       INPUT  : 
**          prec = print precision
**          len  = print length
**          angle = angle value in internal units
**       OUTPUT :  
**          none
**
**    RETURNS      : pointer to converted string
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

char *ud_unang(prec, len, angle)
int prec;                        /* precision */
int len;                         /* length */
UU_REAL angle;                   /* angle value to convert */
{
   UU_REAL locang;
   char frmbuf[100];             /* format buffer */

   locang = angle / UM_cpln.ang_to_radians;

   len = 1;
   sprintf(frmbuf, "%%%d.%df%%s\0", len, prec);
   sprintf(pbuf, frmbuf, locang, UM_angular_units_name[UM_cpln.angle_unit]);

   return(pbuf);
}
