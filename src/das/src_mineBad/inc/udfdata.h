/*************************************************************************
**		NAME	:		udfdata.h
**			Contains:	definition of forms data structure and associated
**							constants
**
**		COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**		MODULE NAME AND RELEASE LEVEL:
**       udfdata.h , 25.1
**		DATE AND TIME OF LAST MODIFICATION:
**       04/29/15 , 15:07:03
**************************************************************************/

#ifndef UDFDATAH

#include "usysdef.h"
#include "udfconst.h"

/* -- Definition of Structure to hold data for forms package.
		On input, holds initial values to display.
		On return, holds data input by user. -- */

typedef union					/* data element definition */
{
	UU_REAL	*frmflt;				/* single element float */
	UU_REAL	*frmvec;				/* float vector */
	int	*frmint;					/* int data */
	char	*frmstr;					/* character string data */
} UD_DDATA;

typedef struct					/* data element and default def */
{
	char dflg;						/* default flag: 0=none, 1=present */
	short dtyp;						/* one of the eleven das data types */
	UD_DDATA ud_delem;			/* data element */
} UD_DPIECE;

typedef struct					/* forms-data structure definition */
{
	int ud_ndf;						/* no. of active data fields */
	UD_DPIECE *ud_data;			/* vector of data elements */
} UD_FDATA;

#define UDFDATAH
#endif
