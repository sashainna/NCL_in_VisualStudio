/*********************************************************************
**
**    NAME         : udactfld.c
**
**       CONTAINS:
**				UD_FSTAT ud_actfld(fno, de)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
** 
**    MODULE NAME AND RELEASE LEVEL
**       d4actfl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:08
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "udfrmdec.h"
#include "udfmracs.h"
#include "dinput.h"
#include "driver.h"

#define STATNO 29

extern char **ud_curdflt;			/* current default for each field */
extern int UD_ksws;
extern int UD_formdev;				/* form device number */
extern char sfstat[STATNO][12];	/* UD_FSTAT strings: must match typedef enum */
extern int ud_togglefield;			/* set to 1 iftoggle field */
extern UD_EVENTDEF ud_inparm;		/* ud_gevt's default string and max len */

/*******************************************************************
**
**		I_FUNCTION	:		UD_FSTAT ud_actfld(fno, de)
**
**						actfld inputs data for field fno until a field fwd
**						or field bkwd, or valid data is entered by the opr. If
**						data is entered, it is checked as to type and range
**						(later for mask/format, too). The algorythm structure
**						diagrammed would look like a ring with three external
**						spokes.
**
**			PARAMETERS	:
**				INPUT		:
**					fno	=	field number entering data for
**					de		=	pointer to data element to fill
**				OUTPUT	: none
**
**			RETURNS		: none
**				The value of the function is of type UD_FSTAT:
**					UD_FLDOK - data entered and checked ok
**					UD_FWD - requested to go to next field
**					UD_BAK - requested to go to previous field
**					UD_BADREQ - data type requested was invalid or undefined
**					UD_DONE	- the "done" key was hit
**					UD_ALT - the "user alternate action" key was hit
**			SIDE EFFECTS:
**				Will possibly change value of data-element pointed to
**			WARNINGS		: none
********************************************************************/

UD_FSTAT ud_actfld(fno,  de)						/* activate field for input */
int fno;													/* field number */
UD_DPIECE *de;											/* data element pointer */
{
/*
.....Not used with windowing systems
*/
	return(UD_FLDOK);
}
