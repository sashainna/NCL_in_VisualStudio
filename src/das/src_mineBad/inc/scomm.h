/*********************************************************************
**    NAME         : scomm.h 
**       Header for SALEX command input 
**		CONTAINS:
**			int = us_get_command()
**			us_command_list()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       scomm.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:50
*********************************************************************/

#ifndef SCOMMH


/* SALEX Commands */
#define ADD			1
#define DELETE		2
#define LIST		3
#define EDIT		4
#define RUN			5
#define TRACE		6
#define INPUT		7
#define QUIT		8
#define HELP		9
#define CGEN		10
#define SEARCH		11
#define NOCHECK	12

extern	int	us_get_command();
extern	void	us_command_list();


#define SCOMMH
#endif
