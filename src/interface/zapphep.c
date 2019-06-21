/*********************************************************************
**
**    NAME         :  iapphep.c
**
**	STUB ROUTINES FOR MPE 2.6 INTERFACE/IAPPHEP.MSG
**
**    CONTAINS:
**			uu_aprompt0
**			uu_achoicemsg
**			uu_aerhep
**			uu_apmthep
**			uu_asysinfor
**
**    COPYRIGHT 1987 (c) Mills Data Systems Co. Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zapphep.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:36
**
*********************************************************************/

char *
uu_aprompt0 (subid, pmtid)
int  subid, pmtid;
	{
	}	/* uu_aprompt0 */


char *
uu_aerhep (subid, erid, first, status, length )
int  subid, erid, length ;
int  first, *status;
	{
	}	/* uu_aerhep */

char *
uu_apmthep (subid, pmtid, first, status, length )
int  subid, pmtid, length ;
int  first, *status;
	{
	}	/* uu_apmthep */


char *
uu_asysinfor (subid,first,status,length)
int	subid, first, *status, length;
	{
	}		/* uu_asysinfor */

char **
uu_achoicemsg(subid,choid,choicenum)
int	subid, choid, *choicenum;
	{
	}	/* uu_achoicemsg */
 

