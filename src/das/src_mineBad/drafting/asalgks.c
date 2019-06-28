
/*********************************************************************
**    NAME         :  asalgks.c
**       CONTAINS:
**       us_textextent
**    	us_set_disattr
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       asalgks.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:39
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "usysg.h"
#include "go.h"
#include "mattr.h"
/*********************************************************************
**    E_FUNCTION :  void us_textextent (position, string, concat, extent)
**       Sal interface routine to gqtextextent3.
**    PARAMETERS   
**       INPUT  : 
**          position - start postion of string
**				string	- string to get extent of 
**       OUTPUT :  
**          concat	- concatentation point
**				extent	- text extent rectangle
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void	us_textextent(position, string, concat, extent)
UU_REAL	position[3];
char		*string;
UU_REAL	concat[3];
UU_REAL	extent[2][3];

{

	Gws		*ws;
	Gchar		c_string[1025];
	Gwrect3	c_extent;
	Gwpoint3	c_concat;
	Gwpoint3 c_position;

	uu_denter(UU_STRC,(us,"us_textextent(<%g,%g,%g>,%s)",
		position[0],position[1],position[2],string));
	/*	translate input parameters to gks data types	*/
	c_position.x = position[0];
	c_position.y = position[1];
	c_position.z = position[2];

	strcpy(c_string, string);

	/*	get the active workstation pointer	*/
	ws = UD_ksws;

	/*	call gks to get the text extent	*/
	gqtextextent3(ws, &c_position, c_string, &c_concat, &c_extent);

	/*	move c_concat and c_extent to their output vars	*/
	concat[0] = c_concat.x;
	concat[1] = c_concat.y;
	concat[2] = c_concat.z;

	extent[0][0] = c_extent.llf.x;
	extent[0][1] = c_extent.llf.y;
	extent[0][2] = c_extent.llf.z;

	extent[1][0] = c_extent.urb.x;
	extent[1][1] = c_extent.urb.y;
	extent[1][2] = c_extent.urb.z;

	uu_denter2(UU_STRC,
		(us,"us_textextent return concat(<%g,%g,%g>)",
		concat[0],concat[1],concat[2]));
	uu_dexit;
	uu_denter2(UU_STRC,
		(us,"us_textextent return extent(<%g,%g,%g>,<%g,%g,%g>)",
		extent[0][0],extent[0][1],extent[0][2],
		extent[1][0],extent[1][1],extent[1][2]));
	uu_dexit;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : us_set_disattr(key, color, style, width, what)
**    PARAMETERS   
**       INPUT  : 
**          color					line color number
**          style					line font number
**          width					line width number
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
us_set_dispattr(key, color, style, width, what)
	int   key, color, style, what;
	UU_REAL	width;

	{
	int status;
	struct UM_attrdata_rec attrptr;
	Glntype line_style;
	UU_LOGICAL uj_miplotting();

	uu_denter(UU_STRC,(us,"us_set_dispattr(key=%d,color=%d,style=%d,what=%d)",
		key, color, style, what));

	if(!uj_miplotting())
		{
		if(what == 1)
			gstextcolor(color);
		else
			gslinecolor(color);
		}
	else
		{
/*
.....Change pen number even if it is not text
.....Added because crosshatching was always
.....plotted with logical Pen #1
.....Bobby  -  7/19/91
*/
		if (key != 0)
			{
			uc_retrieve_attr(key, &attrptr);
			if(what == 1)
				{
				gstextcolor(uj_setpen(attrptr.pen));
				}
			else
				{
				gslinecolor(uj_setpen(attrptr.pen));
				}
			}
		}
	line_style.typeno = style;
	line_style.npatn = 0;
	gslinetype(&line_style);
	gslinewidth(width);

	uu_dexit;
	}
