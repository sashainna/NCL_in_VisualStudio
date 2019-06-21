/*********************************************************************
**    NAME         :  uio.c
**       CONTAINS:
**  			  ud_write -- write to a logical device.
**  			  ud_idev -- initialize device tables.
**  			  ud_dslogd -- associate logical device with physical unit.
**  			  ud_dsphun -- associate physical device with physical unit.
**
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uio.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:53
*********************************************************************/
#include "ustdio.h"
#include "usysdef.h"
#define UPGM
#include "uio.h"
#undef UPGM
#include "udebug.h"

/**************************************************************************
**  E_FUNCTION         :  ud_write(logdev, len, data)		
**       write data of length len to logical device logdev
**  PARAMETERS   
**      INPUT  : 
**          logdev = logical device name
**				len    = buffer length
**          buffer = output buffer
**      OUTPUT :   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

ud_write(logdev, len, data)		
int logdev;								/* logical device name */
int len;									/* length of buffer to output */
char *data;								/* buffer to output */
{
	int i, phunit;
	char us[150];

	if(UD_first == UU_TRUE)
		ud_idev();
	phunit=UD_Physunit[logdev];		/* physical unit */

	if (UD_Physdev[phunit].type == 0)
	{
		fprintf(UD_Physdev[phunit].fdo,"%s",data);
		fflush(UD_Physdev[phunit].fdo);		/* in case of crashes */
		uu_denter2(-1,(us,"%s",data));
		uu_dexit;
	}
	else
	{				/* call gks */

	}
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_idev
**
**      um_initialize device tables
**
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

ud_idev()
{
	int i;

/*		-- set physical unit table to UD_CONS -- */

		for(i=0; i<UD_PUNLEN; UD_Physunit[i++] = UD_CONS)
		;
		UD_Physdev[UD_CONS].type = 0;
		UD_Physdev[UD_CONS].fdi = stdin;
		UD_Physdev[UD_CONS].fdo = stdout;
		UD_first=0;				/* remember we um_initialized */
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_dslogd
**      associate logical device with physical unit
**  PARAMETERS   
**      INPUT  : 
**          logdev = logical device
**				phunit = physical unit
**      OUTPUT :   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

ud_dslogd(logdev, phunit)
int logdev;									/* logical device number */
int phunit;									/* physical unit number */
{
	if (UD_first==1) ud_idev();
	UD_Physunit[logdev] = phunit;

}

/**************************************************************************
**
**  I_FUNCTION         :  ud_dsphun(phunit,phdev)
**      associate physical device with physical unit
**  PARAMETERS   
**      INPUT  : 
**          phunit = physical unit
**				phdev = physical device pointer
**      OUTPUT :   none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

ud_dsphun(phunit, phdev)
int phunit;							/* physical unit number */
Dphdev phdev;						/* pointer to physical device */
{
	if (UD_first==1) ud_idev();
	UD_Physdev[phunit] = phdev;
}
