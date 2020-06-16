/*********************************************************************
**
**    NAME         :  tigclas.c
**
**       CONTAINS:
**       	IGES default class table definitions
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**     MODULE NAME AND RELEASE LEVEL
**       tigclas.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:44
**
*********************************************************************/

#include "class.h"
#include "mdrel.h"


/*	-- Method Description Table for this class system -- */

	char *UC_meth_name[] = {
		"method UC_DISPLAY          (0)",
		"method UC_DRAW             (1)",
		"method UC_PRINT            (2)",
		"method UC_DELETE           (3)",
		"method UC_COPY             (4)",
		"method UC_TRANSLATE        (5)",
		"method UC_ROTATE           (6)",
		"method UC_SCALE            (7)",
		"method UC_MIRROR           (8)",
		"method UC_TRANSFORM        (9)",
		"method UC_RETRIEVE_DATA   (10)",
		"method UC_RETRIEVE_TRANSF (11)",
		"method UC_RETRIEVE_ATTR   (12)",
		"method UC_CREATE_DATA     (13)",
		"method UC_PLOC_TO_COORD   (14)",
		"method UC_PLOC_TO_VECTOR  (15)",
		"method UC_NEAR_ON_ENTITY  (16)",
		"method UC_FEATURE         (17)",
		"method UC_PROJ_TO_DRAWING (18)",
		"method UC_DRAFT_TYPE      (19)",
		"method UC_DRAFT_LINE      (20)",
		"method UC_DRAFT_ARC       (21)",
		"method UC_DRAFT_CONIC     (22)",
		"method UC_DRAFT_ENDPTS    (23)",
		"method UC_QUERY           (24)",
		"method UC_CANBE           (25)",
		"method UC_INIT_EVCRVOUT   (26)",
		"method UC_EVCRV           (27)",
		"method UC_CCTOU           (28)",
		"method UC_CRV_INTERSECT   (29)",
		"method UC_CRV_TO_UNIRBSC  (30)",
		"method UC_CRV_TO_AGRBSC   (31)",
		"method UC_INIT_EVSRFOUT   (32)",
		"method UC_EVSRF           (33)",
		"method UC_SPAN_ENTITY     (34)",
		"method UC_TAN_LINE_THRU_PT(35)",
		"method UC_TAN_TAN_LINE    (36)",
		"method UC_FILLET          (37)",
		"method UC_TRIM_EXTEND     (38)",
		"method UC_SPLIT_CURVE     (39)",
		"method UC_EXTEND_CURVE    (40)",
		"method UC_MIDTRIM_CURVE   (41)",
  		"method UC_SRF_TESSALATE   (42)",
  		"method UC_CRV_INTERSECT_SP (43)",
  		"method UC_ASSOC_UPDATE		(44)"
	};

static UC_METHOD_TABLE UC_method_descriptor =
	{ 512, 0, UU_NULL, UC_meth_name };

UC_TABLE_DESCRIPTOR UC_cot_descriptor = 
				{ "UC_cot", UC_NUM_CLASS, UC_NUM_METHODS, UU_NULL, UU_NULL,
						&UC_method_descriptor };

UC_TABLE_DESCRIPTOR UC_root_descriptor = 
				{ "UC_root", 1, 0, UU_NULL, UU_NULL,
						&UC_method_descriptor };
