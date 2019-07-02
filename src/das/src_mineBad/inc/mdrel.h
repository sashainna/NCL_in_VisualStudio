/*********************************************************************
**    NAME         :  mdrel.h
**       CONTAINS: define all relation numbers
**
**    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**    NOTE: The module view/vevport.c must be recompiled
**    to make changes here effective.  (why I don't know -rah)
**    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       mdrel.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:30
*********************************************************************/
#ifndef UM_MDREL

											
/***************************************************************************
*
*								RELATION NUMBERS
*
***************************************************************************/
#define	UM_MAX_RELATION		128			/* maximum number of relations */

#define  UM_UNKNOWN				-1			/* unknown relation */

#define  UM_MTID_REL				0			/* master tuple id  relation */

/**		Modeling Subsystem	**/

#define  UM_POINT_REL			1			/* point */
#define  UM_LINE_REL				2			/* line */
#define  UM_CIRCLE_REL			3			/* circle */
#define  UM_CONIC_REL			4			/* conic curve*/
#define  UM_COMPCRV_REL			5			/* composite curve */
#define  UM_RBSPLCRV_REL		7			/* rational bspline curve */

#define	UM_AGCRV_REL			8			/* AG rational bspline curve */
#define	UM_AGSRF_REL			10			/* AG surface */
#define	UM_RBSPLSRF_REL		11			/* Rational B-Spline surface */
#define	UM_UVCVONSF_REL		13			/* UV curve on surface */
#define	UM_SURFATTR_REL		14			/* Surface/Solid Attributes */

#define	UM_AGSHELL_REL			20			/* AG shell */
#define	UM_SOLID_REL			21			/* Visual solid */

#define  UM_BODY_REL				31			/* ROMULUS solid body */
#define  UM_FEAT_REL				32			/* features "relation" (not UNIBASE) */
#define	UM_TRANSFORM_REL		36			/* transformation */
#define 	UM_ATTR_REL				37			/* point attribute */

#define 	UM_LIGHT_REL			38			/* rendering lights */
#define	UM_POLY_REL				40			/* polyfill region	*/
#define	UM_POLYLINE_REL		42			/* polyline */
#define	UM_COORDSYS_REL		43			/* coordinate system */
#define	UM_GROUP_REL			44			/* group */
#define	UM_DRAWING_REL			46			/* drawing */
#define	UM_LAYER_REL			47			/* layer */

/**		Drafting Subsystem	**/

#define	UA_LINEAR_DIMS_REL	48			/* linear dimension */
#define	UA_DIA_RAD_REL			49			/* diameter/radius dimension */
#define	UA_ANGULAR_REL			50			/* angular dimension */
#define	UA_COORDINATE_REL		51			/* coordinate dimension */
#define	UA_SYMBOLS_REL			52			/* symbol dimension */
#define	UA_NOTES_REL			53			/* note */
#define	UA_CENT_LINE_REL		54			/* center line dimension */
#define	UA_HATCHING_REL		55			/* hatching */
#define	UA_FANDPTOL_REL		56			/* */
#define	UA_SECT_ARROW_REL		57			/* sections */
#define	UA_SRF_FINISH_REL		58			/* surface finish */
#define	UA_TITLE_BLK_REL		59			/* title block */
#define	UA_DRWBORDR_REL		60			/* drawing border */

/**		Symbol Subsystem		**/

#define	UB_SYMBOL_REL			61			/* master symbol */
#define  UB_INSTANCE_REL   	62			/* symbol instance */
#define 	UB_SYMATTR_REL			63			/* symbol attribute */
#define 	UB_CONECTOR_REL		64			/* connector */

/**		Viewing attribute relations	**/

#define  UV_VIEW_REL				73
#define  UV_VPORT_REL			74
#define  UV_SCREEN_REL			75
 
/**		Calc Subsystem					  **/

#define	UQ_CALC_REL				76

/**		Text								 **/

#define  UA_TEXT_REL				77			/* text primitive */
#define  UA_TEXTATTR_REL		78			/* text attribute */

/**		NCL relations **/

#define 	NCL_ATTR_REL			79	
#define	NCL_VECTOR_REL			80
#define	NCL_MATRIX_REL			81
#define	NCL_CURVE_REL			82
#define	NCL_SURF_REL			83
#define	NCL_PANEL_REL			84
#define	NCL_MESHSURF_REL		85
#define	NCL_QUILTSURF_REL		86
#define	NCL_SHAPE_REL			87
#define	NCL_POINT_REL			88
#define	NCL_LINE_REL			89
#define	NCL_CIRCLE_REL			90
#define	NCL_PLN_REL				91
#define	NCL_PATERN_REL			92
#define	NCL_NETSF_REL			93
#define	NCL_SCALAR_REL			94
#define	NCL_LABTBL_REL			95
#define	NCL_EVALCV_REL			96
#define	NCL_EVALSF_REL			97
#define	NCL_POINTVEC_REL		98
#define	NCL_TRIMSF_REL			99
#define	NCL_REVSURF_REL		100
#define	NCL_DATAST_REL			105
#define	NCL_TEXTVAR_REL		106
#define	NCL_COLOR_REL			107

/**	Space Planning Relations	**/

#define	UR_UNISTAT_REL			111

/**	Space Planning Relations	**/

#define UY_SPLANATT_REL			123
#define UY_ROOM_REL				124
#define UY_PARTN_REL				125
#define UY_DESK_REL				126
#define UY_CHAIR_REL				127


#define UM_MDREL
#endif
