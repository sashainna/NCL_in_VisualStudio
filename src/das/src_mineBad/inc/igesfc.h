/*********************************************************************
**    NAME         :  igesfc.h
**       CONTAINS: redefinition of all FORTRAN/C interface routines
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       igesfc.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:25
*********************************************************************/

#ifndef IGESFC

#include "usysdef.h"

#if UU_COMP != UU_WIN2K
#if UU_COMP != UU_VAXVMS

/***************************************************************************
       C ROUTINES CALLED BY FORTRAN 
***************************************************************************/
/*
...IBM
*/
#if UU_COMP != UU_HPUX && UU_COMP != UU_WINNT && !defined UU_RS6000
#define igtseg igtseg_
#define igett igett_ 
#define igtxyz igtxyz_
#define igeror igeror_
#define igvctmsc igvctmsc_

/***************************************************************************
      IGES FORTRAN ROUTINES CALLED BY C 
***************************************************************************/
#define igspln igspln_
#define igmat igmat_
#endif
#endif
/* #else */
/*
.....C routine called by fortran must declared inside fortran code
*/
/***************************************************************************
      IGES FORTRAN ROUTINES CALLED BY C 
***************************************************************************/
/* #define igspln IGSPLN */
/* extern void __stdcall IGSPLN(double RDR[600]); */
/* #define igmat IGMAT */
/* extern void __stdcall IGMAT (double RDR[12],double*CTOL); */
#endif

#define IGESFC
#endif
