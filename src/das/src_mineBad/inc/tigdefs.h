/*********************************************************************
**    NAME         :  tigdefs.h
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       tigdefs.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:54
*********************************************************************/

#ifndef TIGDEFSH


#define IG_IGESPT_BUFSZ  		80
#define IG_IGESLINE_BUFSZ  	80
#define IG_IGESRSPL_BUFSZ   	80
#define IG_IGESRSSF_BUFSZ   	80
#define IG_IGESRLSF_BUFSZ   	80
#define IG_IGESRVSF_BUFSZ   	80
#define IG_IGESTBCY_BUFSZ   	80
#define IG_IGESOFSF_BUFSZ   	80	/* cpp: OFFSET SURFACE */
#define IG_IGESTRSF_BUFSZ		80	/* ijd: trimmed surface */
#define IG_IGESCVSF_BUFSZ   	80	/* ijd: curve on surface */
#define IG_IGESARC_BUFSZ  		80
#define IG_IGESCOMP_BUFSZ		80
#define IG_IGESGRP_BUFSZ		80
#define IG_VIEWVS_BUFSZ			80
#define IG_PLNASSOC_BUFSZ		80
#define IG_POLY2D_BUFSZ  		80
#define IG_POLY3D_BUFSZ  		80
#define IG_POLY6D_BUFSZ  		80
#define IG_IGESCON_BUFSZ  		80
#define IG_IGESTRAN_BUFSZ 		80
#define IG_IGESPLIN_BUFSZ  	80
#define IG_IGESNOTE_BUFSZ  	80
#define IG_IGESLEAD_BUFSZ  	80
#define IG_IGESANGD_BUFSZ  	80
#define IG_IGESDIAD_BUFSZ  	80
#define IG_IGESLIND_BUFSZ  	80
#define IG_IGESRAD_BUFSZ	  	80
#define IG_IGESGSYM_BUFSZ	  	80
#define IG_IGESSFD_BUFSZ	  	80
#define IG_IGESSFI_BUFSZ	  	80
#define IG_IGESVIE_BUFSZ	  	80
#define IG_IGESDRW_BUFSZ	  	80
#define IG_IGESLABL_BUFSZ	  	80
#define IG_IGESPLSF_BUFSZ	  	80		/*jkd14: p-spline surface */
#define IG_IGESPLN_BUFSZ	  	80		/*jkd31: planes */
#define IG_IGESPROP_BUFSZ		80
#define IG_IGESBNDY_BUFSZ     80    /*Boundary entity JLS 10-30-98*/
#define IG_IGESBDSF_BUFSZ     80    /*Bounded surface entity. JLS 10-30-98*/
#define IG_CRVPTR_BUFSZ       80    /*Bounded surface entity. JLS 7/7/99*/
#define IG_BNDYPTR_BUFSZ      80
#define IG_IGESOLID_BUFSZ     80    /*Solid entity 06-23-05*/
#define IG_IGESHELL_BUFSZ     80    /*Shell entity 06-23-05*/
#define IG_IGESFACE_BUFSZ     80    /*Face entity 06-23-05*/
#define IG_IGESLOOP_BUFSZ     80    /*Loop entity 06-23-05*/
#define IG_EDGE_BUFSZ         80    /*Join to loop 06-23-05*/
#define IG_IGESVLST_BUFSZ     80    /*Vertex List  entity 06-23-05*/
#define IG_IGESELST_BUFSZ     80    /*Edge List entity 06-23-05*/
#ifndef NCCS_H
#define NCL_CURVE_BUFSZ			4000	/*jkd14: "                */
#define NCL_PANEL_BUFSZ			4000
#define NCL_SURFACE_BUFSZ		4000
#define NCL_REVSURF_BUFSZ		8
#define NCL_MESHSF_BUFSZ		4000
#define NCL_QUILTSF_BUFSZ		4000
#define NCL_PATERN_BUFSZ		4000
#define NCL_NETSF_BUFSZ			4000    /*jkd14: "                */
#endif

#include "tigesddl.h"

#define TIGDEFSH
#endif
