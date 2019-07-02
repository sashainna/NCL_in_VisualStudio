/*********************************************************************
**    NAME         :  rienvtab.h
**       CONTAINS:
**       structure definition for the environment table
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tzenvtab.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:00
*********************************************************************/

#ifndef TZENVTABH


struct TZ_env_table_rec
{
	char	name[12];			/* name of the environmental variable */
	char	*adrs;			/* address of env. var. */
	char	*new_adrs;		/* address of new copy of env. var. */
	int	length;			/* length of env. var. */
};

#define TZENVTABH
#endif
