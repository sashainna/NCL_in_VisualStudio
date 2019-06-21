/*********************************************************************
**
**    NAME         :  d2timsp.c
**
**    CONTAINS:
**					ud_dtval
**					ud_dtsel
**					ud_dtsel_str
**					ud_dtcord
**					ud_dtact
**					ud_dtdeact
**					ud_qcord
**					ud_qstr
**					ud_dtstr
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d2timsp.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:05
**
*********************************************************************/

#include "usysdef.h"
#include "usysg.h"
#include "uims.h"
#include "g.h"
#include "gi1.h"
#include "dinput.h"
#include "dasnog.h"
#include "dasg.h"
#include "udebug.h"

/*********************************************************************
**
**    E_FUNCTION     :  ud_dtval(type, device, pet)
**
**       change the default value event type
**
**    PARAMETERS   
**
**       INPUT  : 
**
**				type = event type
**				device = device number
**				pet = pet
**
**       OUTPUT :  
**
**          output
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ud_dtval(type, device, pet)
int type;							/* event type */
int device;							/* device number */
int pet;								/* pet */
{
	UD_valint = type;
	UD_valdev = device;
	UD_valech = pet;
	return;
}

/*********************************************************************
**
**    E_FUNCTION     :  ud_dtsel(type, device, pet)
**
**       change the default select event type
**
**    PARAMETERS   
**
**       INPUT  : 
**
**				type = event type
**				device = device number
**				pet = pet
**
**       OUTPUT :  
**
**          output
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ud_dtsel(type, device, pet)
int type;							/* event type */
int device;							/* device number */
int pet;								/* pet */
{
	UD_pckint = type;
	UD_pckdev = device;
	UD_pckech = pet;
	return;
}

/*********************************************************************
**
**    E_FUNCTION     :  ud_dtsel_str(setting)
**
**       Enables / Disables the text input of non-existent labels during
**       pick mode.
**
**    PARAMETERS   
**
**       INPUT  : 
**
**				flag = UU_TRUE allows non-existent text to be input.
**                 UU_FALSE does not (this is the default).
**
**       OUTPUT :  
**
**          output
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

void ud_dtsel_str(flag)
UU_LOGICAL flag;
{
	UD_pckstr = flag;
	return;
}

/*********************************************************************
**
**    E_FUNCTION     :  ud_dtcord(type, device, pet)
**
**       change the default coordinate event type
**
**    PARAMETERS   
**
**       INPUT  : 
**
**				type = event type
**				device = device number
**				pet = pet
**
**       OUTPUT :  
**
**          output
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ud_dtcord(type, device, pet)
int type;							/* event type */
int device;							/* device number */
int pet;								/* pet */
{
	UD_locint = type;
	UD_locdev = device;
	UD_locech = pet;
	return;
}

/*********************************************************************
**
**    I_FUNCTION     :  ud_dtact()
**
**       activate a GKS workstation
**
**    PARAMETERS   
**
**       INPUT  : 
**
**          iconfile = icon file name
**
**       OUTPUT :  
**
**          output
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ud_dtact()
{
	if(UD_wsptr == 1)
	{
		gactivatews(UD_gksws[1]);
		UD_wsptr = 2;
	}
}

/*********************************************************************
**
**    I_FUNCTION     :  ud_dtdeact()
**
**       deactivate a GKS workstation
**
**    PARAMETERS   
**
**       INPUT  : 
**
**          iconfile = icon file name
**
**       OUTPUT :  
**
**          output
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ud_dtdeact()
{

/*			-- Deactivate WS -- */

	if(UD_wsptr == 2)
	{
		gdeactivatews(UD_gksws[1]);
		UD_wsptr = 1;
	}
}

/*********************************************************************
**
**    E_FUNCTION     :  ud_qcord(type, device, pet)
**
**       inquire the default coordinate event type
**
**    PARAMETERS   
**
**       INPUT  : 
**
**          none
**
**       OUTPUT :  
**
**				type = event type
**				device = device number
**				pet = pet
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ud_qcord(type, device, pet)
int *type;							/* event type */
int *device;						/* device number */
int *pet;							/* pet */
{
	*type = UD_locint;
	*device = UD_locdev;
	*pet = UD_locech;
}

/*********************************************************************
**
**    E_FUNCTION     :  ud_qstr(type, device, pet)
**
**       inquire the default string event type
**
**    PARAMETERS   
**
**       INPUT  : 
**
**          none
**
**       OUTPUT :  
**
**				type = event type
**				device = device number
**				pet = pet
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ud_qstr(type, device, pet)
int *type;							/* event type */
int *device;						/* device number */
int *pet;							/* pet */
{
	*type = UD_strint;
	*device = UD_strdev;
	*pet = UD_strech;
}

/*********************************************************************
**
**    E_FUNCTION     :  ud_dtstr(type, device, pet)
**
**       change the default string event type
**
**    PARAMETERS   
**
**       INPUT  : 
**
**				type = event type
**				device = device number
**				pet = pet
**
**       OUTPUT :  
**
**          output
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ud_dtstr(type, device, pet)
int type;							/* event type */
int device;							/* device number */
int pet;								/* pet */
{
	UD_strint = type;
	UD_strdev = device;
	UD_strech = pet;
	return;
}
