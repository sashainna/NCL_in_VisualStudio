/*********************************************************************
**
**    NAME         :  gloc3.h
**
**       CONTAINS:
**       	definition for missing Gloc3 struct
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       gloc3.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:20
**
*********************************************************************/

#ifndef GLOC3H
#include "gobas.h"

/* -- Gloc3 definition -- */

	typedef struct	
	{
		Gint		transform;	/* normalization transformation number */
		Gwpoint3	position;	/* locator position */
	} Gloc3;

#define GLOC3H
#endif
