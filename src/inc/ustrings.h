/*********************************************************************
**    NAME         :  	ustrings.h
**		CONTAINS:
**
**			External function definitions for routines described in string(3).
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ustrings.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:06
*********************************************************************/
#ifndef USTRINGSH

/*
 * External function definitions
 * for routines described in string(3).
 */
char	*strcat();
char	*strncat();
int	strcmp();
int	strncmp();
char	*strcpy();
char	*strncpy();
#ifndef UU_IRIX64
int	strlen();
#endif
char	*index();
char	*rindex();

#define USTRINGSH
#endif
