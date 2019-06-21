/*********************************************************************
**    NAME         :  rienvtab.h
**       CONTAINS:
**       structure definition for the environment table
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rienvtab.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:47
*********************************************************************/

#ifndef RIENVTABH


struct UR_env_table_rec
{
	char	name[12];			/* name of the environmental variable */
	char	*adrs;			/* address of env. var. */
	char	*new_adrs;		/* address of new copy of env. var. */
	int	length;			/* length of env. var. */
};

#define RIENVTABH
#endif
