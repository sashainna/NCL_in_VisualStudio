/*********************************************************************
**    NAME         :  nutil.c
**       CONTAINS: utility routines 
**       ncl_uureal_to_real8(n, inbuf, outbuf)
**       ncl_real8_to_uureal(n, inbuf, outbuf)
**       convrt (from, to, len)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nutil.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:16
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "class.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdebug.h"

#include "ncl.h"
#include "nclfc.h"

#include <ctype.h>

/*********************************************************************
**    E_FUNCTION     : ncl_uureal_to_real8(n, inbuf, outbuf)
**       Move N UU_REALs to UM_real8
**    PARAMETERS   
**       INPUT  : 
**          n                 number of values
**          inbuf             array of UU_REAL values
**       OUTPUT :  
**          outbuf            array of UM_real8 values
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_uureal_to_real8(n, inbuf, outbuf)
   int n;
   UU_REAL inbuf[];
   UM_real8 outbuf[];

   {
   int i;
   for (i=0; i<n; i++) outbuf[i] = inbuf[i];
   }

/*********************************************************************
**    E_FUNCTION     : ncl_real8_to_uureal(n, inbuf, outbuf)
**       Move N UM_real8 to UU_REAL
**    PARAMETERS   
**       INPUT  : 
**          n                 number of values
**          inbuf             array of UM_real8 values
**       OUTPUT :  
**          outbuf            array of UU_REAL values
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_real8_to_uureal(n, inbuf, outbuf)
   int n;
   UM_real8 inbuf[];
   UU_REAL outbuf[];

   {
   int i;
   for (i=0; i<n; i++) outbuf[i] = inbuf[i];
   }

/*********************************************************************
**    E_FUNCTION     : convrt (from, to, len)
**       Convert string to upper case.
**    PARAMETERS   
**       INPUT  : 
**          from                 string to convert
**          len                  number of character to convert
**       OUTPUT :  
**          to                   converted string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void convrt (from, to, len)
UM_f77_str_ptr from;
UM_f77_str_ptr to;
UM_int2 *len;

{
int i;
char *cfrom;
char *cto;

   cfrom = UM_cstr_of_f77_str(from);
   cto = UM_cstr_of_f77_str(to);
/*
.....it will hit a break if cfrom[i]<0 (which is junk char), it should stop when cfrom[i]=='\0')
*/
   for (i=0; i<*len; i++)
	{
		if (cfrom[i] <= 0) cto[i] = cfrom[i];
		else cto[i] = islower(cfrom[i]) ? toupper(cfrom[i]) : cfrom[i];
	}
}
