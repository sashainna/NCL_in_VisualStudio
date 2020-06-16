/*********************************************************************
**
**    NAME         :  tigmsgs.c
**
**       CONTAINS:
**         	routines to retrieve messages for iunihep.msg
**          without making calls to display routines.
**
**	This file is intended for use in TOOLS ONLY (for now) and is built
**	by doing a cat iunihep.c nclerrmsg.c > tmp.c; mv tmp.c iunihep.c; etc.
**	SEE $NCLPATH/sbin/nclmsgtool!!!!
**
**    COPYRIGHT 1990 (c) MILLS DATA SYSTEMS Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       tigmsgs.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:12:50
**
*********************************************************************/
/*********************************************************************
**
**    NAME         :  tig_retrieve_errmsg0()
**
**	Retrieve an error message, no parameters.
** Same as uu_uerror0 except that the error display routine is not
** called and the error mesage string is returned to the calling routine.
**
** The routines uu_uerror0(), etc. are located in iunihep.c -> generated
** from running msgtool against interface/iunihep.msg.
**
*********************************************************************/
tig_retrieve_errmsg0 (subid, erid, ebuf)
int  subid, erid;
char *ebuf;
	{
	int	index;

/*	-- remember the error numbers for help -- */

	if ((subid == 14) && (erid == 11))
		{
		sprintf(ebuf, "The system has run out of dynamic memory.");
		}
	else
		sprintf (ebuf,"ERROR INFORMATION - Subsystem %d, error number %d",subid,erid);
	return;
	}
