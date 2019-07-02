/*********************************************************************
**    NAME         :  dasnog.h
**       CONTAINS:
**       	DAS input type defines
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       dtypes.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:15
*********************************************************************/

#ifndef DTYPESH

/*	-- DAS input request types -- */

#define UD_DASCART   	1					/* cartesian coordinate input request */
#define UD_DASSELECT 	2					/* select subsystem input request */
#define UD_POPUP			3					/* popup choice input */
#define UD_DASVAL    	4					/* distance value input request */
#define UD_DASDISTANCE  5					/* distance value input request */
#define UD_DASINT    	6 					/* integer input request */
#define UD_DASVEC    	7					/* vector input request */
#define UD_DASPICK   	8					/* pick input request */
#define UD_DASPCKLOC 	9					/* pick location input request */
#define UD_DASSTRING 	10					/* string input request */
#define UD_DASCHOICE 	11					/* choice input request */
#define UD_DASNDC    	12					/* cartesian ndc coordinate request */
#define UD_DASANGLE		13					/* angle input */
#define UD_DASUNITLESS	14 				/* unitless real number input */
#define UD_DASSTRINGDEF 15					/* string with default input request */
#define UD_DASPLANE 		16					/* plane input request */
#define UD_DASSCALAR 		17					/* scalar input request */
#define UD_SCACART 		18					/* scalar input request */
#define UD_SCAVAL 		19					/* scalar input request */
#define UD_SCADISTANCE  20					/* distance value input request */
#define UD_SCAINT    	21 					/* integer input request */
#define UD_SCAVEC    	22					/* vector input request */
#define UD_SCANDC    	23					/* cartesian ndc coordinate request */
#define UD_SCAANGLE		24					/* angle input */
#define UD_SCAUNITLESS	25 				/* unitless real number input */
#define UD_SCACART2 		26			/* cartesian coordinate input request, but it can 
										include scalar in the cord or return PT label as the
										input value*/
#define UD_SCAVEC2 		27			/* cartesian coordinate input request, but it can 
										include scalar in the cord or return PV or VE label as the
										input value*/

/*	-- The types of input that can be passed up from ud_gevt -- */

#define UD_NONE		0
#define UD_LOCATOR	1
#define UD_STROKE		2
#define UD_VALUATOR	3
#define UD_CHOICE		4
#define UD_PICK		5
#define UD_STRING		6
#define UD_VECTOR		7
#define UD_PROMPT		8
#define UD_ANY			9
#define UD_PICKMENU		10

#define DTYPESH

#endif
