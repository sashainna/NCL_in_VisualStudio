/*********************************************************************
**    NAME:  jquery.h
**       CONTAINS:
**       	contains macros and data types used in query functions
**    COPYRIGHT 1984 (c) UNICAD Inc.  ArowsUsed Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       jquery.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:27
*********************************************************************/
#ifndef UJ_QUERY

#define UJ_typeRows  15
#define UJ_typeCols  18
#define UJ_unitRows  8
#define UJ_unitCols  20
#define UJ_angleRows 2
#define UJ_angleCols 20
#define UJ_ctypeRows  4
#define UJ_ctypeCols 11
#define UJ_lstylesRows 9
#define UJ_lstylesCols 13

#ifdef UJ_QPGM  /* defined in jquery.c */

	char UJ_sbuf[300]; /* buffer for line of output */

	char UJ_type[UJ_typeRows][UJ_typeCols] = {
		" ",              /* rel_num = 0  */
		"POINT",          /*         = 1  */
		"LINE",           /*         = 2  */ 
		"ARC/CIRCLE",     /*         = 3  */
		"CONIC",          /*         = 4  */
		"COMPOSITE",      /*         = 5  */
		"CURVE",          /*         = 6  */
		"BSPLINE",        /*         = 7  */
		"BSPLINE",        /*         = 8  */
		" ",              /*         = 9  */
		" ",              /*         = 10 */
		" ",              /*         = 11 */
		" ",              /*         = 12 */
		"SSPLINE   ",     /*         = 13 */
		" "};

   char UJ_units[UJ_unitRows][UJ_unitCols] = 
	   {"(inches) ..........",
		 "(feet) ............",
		 "(miles) ...........", 
		 "(millimeters) .....", 
		 "(centimeters) .....",
		 "(meters) ..........",
		 "(kilometers) ......",
		 "(mils) ............"};

	char UJ_angle[UJ_angleRows][UJ_angleCols] =
	   {"(radians) .........",
	    "(degrees) ........."};

	char UJ_ctype[UJ_ctypeRows][UJ_ctypeCols] =
		{"CN_UNKNOWN",
		 "ELLIPSE",
		 "HYPERBOLA",
		 "PARABOLA"};

   char UJ_lstyles[UJ_lstylesRows][UJ_lstylesCols] =
		{"unknown",
		 "SOLID",
		 "SMALL DASHED",
		 "DOTS",
		 "CENTERLINE",
		 "PHANTOM",
		 "LARGE DASHED",
		 "DASH DOT",
		 "DASH SPACE"};
#else
	extern char UJ_sbuf[300];
	extern char UJ_type[UJ_typeRows][UJ_typeCols];
	extern char UJ_units[UJ_unitRows][UJ_unitCols];
	extern char UJ_angle[UJ_angleRows][UJ_angleCols];
	extern char UJ_ctype[UJ_ctypeRows][UJ_ctypeCols];
	extern char UJ_lstyles[UJ_lstylesRows][UJ_lstylesCols];
#endif

#define UJ_ROWCHK \
	{	\
	if (rowsUsed+4 > maxRows)\
	{\
		sprintf(data+maxCols*rowsUsed++, "\n                .\n");\
		sprintf(data+maxCols*rowsUsed++, "\n                .\n");\
		sprintf(data+maxCols*rowsUsed++, "\n    - MORE DATA BUT NO ROOM -\n");\
		sprintf(data+maxCols*rowsUsed++, "\n                .\n");\
		goto done;\
	}}

#define UJ_COLCHK	\
		{	\
		uu_dprint(UU_MTRC,(us,"strlen(UJ_sbuf)=%d,maxCols=%d",strlen(UJ_sbuf),maxCols)); \
		if (strlen(UJ_sbuf) >= maxCols) \
		{ uu_uerror0(UJ_SUPPORT,27); goto failed; \
			/* Error: query line too long */ }}



/******
#define UJ_PUTSTR(list, str) \
	{ \
	 UJ_query_ptr = (char *)uu_malloc(strlen(str)*sizeof(char)); \
	 strcpy(UJ_query_ptr, str); \
    uu_dprint(UU_MTRC,(us,"before push, list=%x, ptr=%x, ch=%c", \
					list, UJ_query_ptr, *UJ_query_ptr)); \
	 uu_list_push(list, &UJ_query_ptr); \
	}
#define UJ_PUTSTR(list, str) 		uj_putstr(list, str)
*****/



#define UJ_PUT0(list, string) \
	{	\
	 sprintf(UJ_sbuf,string); \
	 uj_putstr(list, UJ_sbuf);}

#define UJ_PUT1(list, string, arg1) \
	{sprintf(UJ_sbuf,string, arg1); \
	 uj_putstr(list, UJ_sbuf);}

#define UJ_PUT2(list, string, arg1, arg2) \
	{sprintf(UJ_sbuf,string, arg1, arg2); \
	 uj_putstr(list, UJ_sbuf);}

#define UJ_PUT3(list, string, arg1, arg2, arg3) \
	{sprintf(UJ_sbuf,string, arg1, arg2, arg3); \
	 uj_putstr(list, UJ_sbuf);}

#define UJ_PUT4(list, string, arg1, arg2, arg3, arg4) \
	{sprintf(UJ_sbuf, string, arg1, arg2, arg3, arg4); \
	 uj_putstr(list, UJ_sbuf);}


#define UJ_QUERY
#endif
