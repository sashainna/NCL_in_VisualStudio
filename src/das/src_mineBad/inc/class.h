/*********************************************************************
**
**    NAME         :  class.h
**       CONTAINS: definitions for class structures
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       class.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:12
**
*********************************************************************/

#ifndef UC_CLASS

#include "usysdef.h"

/*******************************************************************
*
*		Define error return codes.
*
*******************************************************************/

#define UC_UNDEFINED_CLASS_STAT		-1
#define UC_UNDEFINED_METHOD_STAT		-2

/********************************************************************
*
*		Defined super class numbers. These numbers define the order of the
*		the entries in the master table SCOT.
*
********************************************************************/

#define UC_UNKNOWN_CLASS		-1
#define UC_ROOT_CLASS			-1
#define UC_UNIBASE_CLASS		0

#define UC_GEOMETRY_CLASS		1
#define UC_CURVE_CLASS			2
#define UC_SURFACE_CLASS		3
#define UC_SOLID_CLASS			4

#define UC_ANNOTATION_CLASS	5

#define UC_SUPPORT_CLASS		6
#define UC_VIEWING_CLASS		7

#define UC_RELATED_CLASS		8
#define UC_SYMBOL_CLASS			9

#define UC_ATTRIBUTE_CLASS		10

#define UC_APPLICATION_CLASS	11
#define UC_NUM_SUPERCLASS		12 		/* maximum SCOT size */

/********************************************************************
*
*		Defined class numbers are the relation number as defined in mdrel.h.
*		These numbers define the order of the entries in the master table COT.
*
********************************************************************/

#include "mdrel.h"
#include "ribase.h"
#define UC_NUM_CLASS UR_MAX_REL+UC_NUM_SUPERCLASS	/* max table size */
#define UC_NUM_DOMAINS UM_MAX_RELATION

/********************************************************************
*
*		Defined message numbers. These numbers define the order
*		and meaning of each of the entries in the dispatch tables
*		for each class.
*
********************************************************************/

#define UC_DISPLAY					0
#define UC_DRAW						1
#define UC_PRINT						2
#define UC_DELETE						3
#define UC_COPY						4
#define UC_TRANSLATE					5
#define UC_ROTATE						6
#define UC_SCALE						7
#define UC_MIRROR						8
#define UC_TRANSFORM					9
#define UC_RETRIEVE_DATA			10
#define UC_RETRIEVE_TRANSF			11
#define UC_RETRIEVE_ATTR			12
#define UC_CREATE_DATA				13
#define UC_PLOC_TO_COORD			14
#define UC_PLOC_TO_VECTOR			15
#define UC_NEAR_ON_ENTITY			16
#define UC_FEATURE					17
#define UC_PROJ_TO_DRAWING 		18
#define UC_DRAFT_TYPE 				19
#define UC_DRAFT_LINE 				20
#define UC_DRAFT_ARC 				21
#define UC_DRAFT_CONIC 				22
#define UC_DRAFT_ENDPTS 			23
#define UC_QUERY						24
#define UC_CANBE						25
#define UC_INIT_EVCRVOUT			26
#define UC_EVCRV						27
#define UC_CCTOU						28
#define UC_CRV_INTERSECT			29
#define UC_CRV_TO_UNIRBSC			30
#define UC_CRV_TO_AGRBSC			31
#define UC_INIT_EVSRFOUT			32
#define UC_EVSRF						33
#define UC_SPAN_ENTITY				34
#define UC_TAN_LINE_THRU_PT		35
#define UC_TAN_TAN_LINE				36
#define UC_FILLET_CURVE				37
#define UC_TRIM_EXTEND_CURVE		38
#define UC_SPLIT_CURVE				39
#define UC_EXTEND_CURVE				40
#define UC_MIDTRIM_CURVE			41
#define UC_SRF_TESSELLATE			42
#define UC_CRV_INTERSECT_SP		43
#define UC_ASSOC_UPDATE				44
#define UC_DISSOLVE					45
#define UC_UTOT						46
#define UC_PROJECT_TO_PLANE		47
#define UC_MODIFY_ENTITY			48
#define UC_ALTOU						49
#define UC_ALTOUV						50
#define UC_PT_ON_CRV_AT_PAL 		51
#define UC_PT_ON_SRF_AT_PAL 		52
#define UC_SETUP_DATA 				53
#define UC_CANBE_UNDELETED			54
#define UC_REVERSE_CRV				55

#define UC_NUM_METHODS				56

#define UC_UNDEFINED_METHOD	(UC_METHOD) UU_NULL
#define CONVERT_REL_TO_CLASS(relation)  relation+UC_NUM_SUPERCLASS-1

/*	-- Type of class call -- */

	typedef enum
	{
		UC_PERFORM,					/* perform class call */
		UC_PERMISSION,				/* asking permission class call */
		UC_TOUCH						/* invoke associativity back-end only */
	} UC_CLASS_ACTION;

/*	-- Class Table Structure Definition Section -- */

typedef int (*UC_METHOD)();

typedef struct
{
	int name;										/* class id */
	char *label;									/* label for debugging */
	int super_class;								/* superclass id */
	UC_METHOD initialize;						/* initialize method */
	UU_LOGICAL initted;							/* initialized flag */
	UC_METHOD methods[UC_NUM_METHODS];		/* array of methods */
} UC_CLASS_REC;

#define UC_LABEL_SIZE 31						/* size of ASCII lable */
typedef struct
{
	int (*intry)();					/* entry point */
	char name[UC_LABEL_SIZE+1];	/* character name buffer*/
} UC_METHOD_TABLE_DESCRIPTOR;

typedef struct
{
	int meth_ptr_num;							/* number of entries in method table */
	int meth_ptr_left;						/* number entries in method table left */
	UC_METHOD_TABLE_DESCRIPTOR *method_desc;	/* pointer to method table */
	char **meth_name;							/* method name table */
} UC_METHOD_TABLE;

typedef struct
{
	char *table_name;								/* ASCII name of the table */
	int num_classes;								/* number of classes in table */
	int num_methods;								/* number of methods per class */
	UC_CLASS_REC **table_ptr;					/* pointer to class table */
	UC_CLASS_REC **super_ptr;					/* pointer to superclass table */
	UC_METHOD_TABLE *method_ptr;				/* pointer to method table */
} UC_TABLE_DESCRIPTOR;

/********************************************************************
*
*		THIS STRUCTURE DEFINITION MUST BE LARGE ENOUGH TO HOLD ANY
*		RELATION TUPLE.
*
********************************************************************/

struct UC_entitydatabag 
{
	UU_KEY_ID key;
	int rel_num;
	char data[25000];
};

/********************************************************************
*
*		THIS STRUCTURE DEFINITION MUST BE LARGE ENOUGH TO HOLD ANY
*		ATTRIBUTE RELATION TUPLE.
*
********************************************************************/

struct UC_attributedatabag {
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	color;
	int	layer;
	int	pen;
	int	line_style;
	UU_REAL line_weight;
	UU_REAL line_width;
	int displayable;
	UU_LOGICAL selectable;
	int label_on;
	char data[1000];
}; 

/*	-- Macro to Initialize Class Table -- */

#define UC_CLASS_INIT(name, label, superclass, init_method) \
{ \
	name, label, superclass, init_method, UU_FALSE, { \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD, \
	UC_UNDEFINED_METHOD, UC_UNDEFINED_METHOD } \
}

/*	-- Macro to initialize a superclass -- */

#define UC_SUPER_IN UC_CLASS_INIT

/*	-- Macro to define a relation-based class -- */

#define UC_CREL_IN(relnum, label, superclass, init_method) \
	UC_CLASS_INIT(UC_NUM_SUPERCLASS+relnum-1, label, superclass, init_method)

/*	-- Macros to help initialize the Class Tables -- */

#ifdef UU_DEBUGON

/*	-- This macro is used to assign a method into a class. In addition,
		the method name is added to the method name table for debugging
		purposes. The parameters are:

			table_desc = pointer to the table descriptor
			method = method number
			procedure = the procedure name 
			
	  The macro assumes that a class record classed "class_rec" has
	  been defined in the procedure using the macro. -- */

#define UC_ASSIGN_METHOD(table_desc, method, procedure) \
{ \
	extern int procedure(); \
	(*class_rec).methods[method] = procedure; \
	uc_init_method_trace(table_desc, procedure, "procedure()"); \
}

/*	-- This macro adds the procedure name to the method name table for
		debugging purposes.  The parameters are:

			table_desc = pointer to the table descriptor
			procedure = procedure name -- */

#define UC_ADD_METHOD_NAME(table_desc, procedure) \
{ \
	extern int procedure(); \
	uc_init_method_trace(table_desc, procedure, "procedure()"); \
}

#else

/*	-- These are the nodebug version of the above two macros.  In these
		versions the method name table is not used -- */

#define UC_ASSIGN_METHOD(table_desc, method, procedure) \
{ \
	extern int procedure(); \
	(*class_rec).methods[method] = procedure; \
}

#define UC_ADD_METHOD_NAME(table_desc, procedure)

#endif

#define UC_CLASS
#endif
