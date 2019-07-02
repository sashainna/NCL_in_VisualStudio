
/*********************************************************************
**    NAME         :  mdrel.h
**       CONTAINS: define all relation numbers
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tzmdrel.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:00
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
#define  UM_BSPLCRV_REL			6			/* bspline curve */
#define  UM_RBSPLCRV_REL		7			/* rational bspline curve */
#define  UM_WFSPLCRV_REL		8			/* wilson-fowler spline curve */
#define  UM_CSPLCRV_REL			9			/* cardinal spline */
#define  UM_RESTCRV_REL			10			/* restricted curve */
#define  UM_MACHCRV_REL			11			/* machining curve */
#define  UM_INTCRV_REL			12			/* intersection curve */
#define  UM_PLN_REL				13			/* plane */
#define  UM_CYL_REL				14			/* cylinder */
#define  UM_CONE_REL				15			/* cone */
#define  UM_SPH_REL				16			/* sphere */
#define  UM_REVSRF_REL			17			/* surface of revolution */
#define  UM_RULSRF_REL			18			/* ruled surface */
#define  UM_TABCYL_REL			19			/* tabulated cylinder realtion */
#define  UM_BSPLSRF_REL			20			/* bspline surface */
#define  UM_RBSPLSRF_REL		21			/* rational bspline surface */
#define  UM_COONSRF_REL			22			/* coons interpolated surface */
#define  UM_CDRVSRF_REL			23			/* curve driven surface */
#define  UM_COMPSRF_REL			24			/* composite surface */
#define  UM_RESTSRF_REL			25			/* restricted surface */
#define  UM_EXTSRF_REL			26			/* extrapolated surface */
#define  UM_PLANARFACE_REL		27			/* planar facet */
#define  UM_COMPFACE_REL		28			/* planar faceted surface */

#define  UM_BODY_REL				31			/* solid body */
#define  UM_FEAT_REL				32			/* features "relation" (not UNIBASE) */
#define  UM_TEXT_REL				33			/* text primitive */
#define  UM_TEXTATTR_REL		34			/* text attribute */
#define  UM_SURFREND_REL		35       /* surface rendering */
#define	UM_TRANSFORM_REL		36			/* transformation */
#define 	UM_ATTR_REL				37			/* point attribute */

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

#define	TZ_SYMBOL_REL			61			/* master symbol */
#define  TZ_INSTANCE_REL   	62			/* symbol instance */
#define 	TZ_SYMATTR_REL			63			/* symbol attribute */
#define 	TZ_CONECTOR_REL		64			/* connector */

/*		Training  Subsystem		**/

#define	UE_BOX_REL				70			/* box */
#define	UE_POLY_REL				71			/* n sided regular polygon */
#define	UE_TRATTR_REL			72			/* attribute bundle */

/**		Viewing attribute relations	**/

#define  UV_VIEW_REL				73
#define  UV_VPORT_REL			74
#define  UV_SCREEN_REL			75
 
/**		Calc Subsystem					  **/

#define	UQ_CALC_REL				76

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
#define	NCL_PLN_REL             91
#define	NCL_PATERN_REL			92
#define	NCL_NETSF_REL			93
#define	NCL_SCALAR_REL			94
#define	NCL_LABTBL_REL			95

/**     Space Planning relations       **/

#define UY_SPLANATT_REL			123
#define UY_ROOM_REL				124
#define UY_PARTN_REL				125
#define UY_DESK_REL				126
#define UY_CHAIR_REL				127


#define UM_MDREL
#endif
