/*********************************************************************
**    NAME         :  tigconst.h
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       tigconst.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:53
*********************************************************************/

#ifndef TIGCONSTH

#include "tiges.h"


 /*************************************************************
 **                                                          **
 **   IGES entity relation numbers and names                 **
 **                                                          **
 *************************************************************/

 /* relation numbers */
 int iges_st_type[IG_NUM+1] = {100, 102, 104, 106, 606, 906, 108, 110, 112,
		114, 116, 118, 120, 122, 124, 126, 128, 140, 141, 142, 143, 144, 202,  
		206, 210, 212, 214, 216, 222, 228, 402, 605, 602, 308, 408, 410, 404,
		406, 0, 186, 502, 504, 508, 510, 514, 314};	/*jkd29: fix type mapping */

 /* relation names */

 char *iges_st_name[] = {
		"circ arc  ",
		"compcrv   ",
		"conic     ",
		"poly2d    ",
		"poly3d    ",
		"poly6d    ",
		"plane     ",
		"line      ",
		"p-spline  ",
		"p-spl srf ",
		"point     ",
		"ruled srf ",
		"rev srf   ",
		"tab cyl   ",
		"transform ",
		"rb-spline ",
		"rb-spl srf",
		"of_set srf",		/* cpp: OFFSET SURFACE */
		"boundary  ",     /* Boundary entity.  JLS 10-30-98 */
		"crv_on_srf",
		"bdsrf     ",     /* Bounded surface entity  JLS 10-30-98 */
		"trimmed_sf",
		"ang dim   ",
		"dia dim   ",
		"gen label ",
		"gen note  ",
		"leader    ",
		"lin dim   ",
		"rad dim   ",
		"gen symbol",
		"group     ",
		"viewvis   ",
		"plnassoc  ",
		"subfig    ",
		"inst      ",
		"view      ",
		"drawing   ",
		"property  ",
		"(unknown) ",		/*jkd29: fix type mapping */
		"mf solid  ",		/* Mainfold Solid B-Rep Object Entity 06-20-05 */
		"ver list  ",     /*Vertex List Entity */
		"edge list ",		/*Edge List entity */
		"loop      ",		/*Loop Entity */
		"face      ",		/*Face Entity */
		"shell     ",      /*Shell Entity */
		"color	  "		/*Color Entity*/	
		};

 /* parameter/record delimitators */

char term_st = ';';
char delim_st = ',';

/* Uni-ddl table names for the IGES entities */

char *ddl_st_nam[] = {
	"igesarc",
	"igescomp",
	"igescon",
	"poly2d",
	"poly3d",
	"poly6d",
	"igespln",
	"igesline",
	"igesplin",
	"igesplsf",
	"igespt",
	"igesrlsf",
	"igesrvsf",
	"igestbcy",
	"igestran",
	"igesrspl",
	"igesrssf",
	"igesofsf",		/* cpp: OFFSET SURFACE */
   "igesbndy",    /* Boundary entity JLS 10/30/98 */
	"igescvsf",
	"igesbdsf",    /* Bounded surface entity. JLS 10/30/98 */
	"igestrsf",
	"igesangd",
	"igesdiad",
	"igeslabl",
	"igesnote",
	"igeslead",
	"igeslind",
	"igesrad",
	"igesgsym",
	"igesgrp",
	"viewvs",
	"plnassoc",
	"igessfd",
	"igessfi",
	"igesvie",
	"igesdrw",
	"igesprop",
	"unknown",
	"igesolid",		/* Mainfold Solid B-Rep Object Entity 06-20-05 */
   "igesvlst",    /* Vertex List */
	"igeselst",		/* Edge List */
	"igesloop",    /* Loop */
	"igesface",    /* Face */
	"igeshell",		/* shell */
	"igesclr"		/* color */
	};

#define TIGCONSTH
#endif
