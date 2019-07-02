/********************************************************************
**    NAME:  bsym.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tzbsym.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:59
*********************************************************************/

#ifndef TZ_BSYM

#ifdef TZ_BPGM
#define EXT
#else
#define EXT extern
#endif

#define TZ_MSYMPATH(msymptr) (msymptr)->path
#define TZ_MSYMNAME(msymptr) (msymptr)->name

/* The following is the name put into master symbol files to indicate the root
 * master symbol in the file. */
#define TZ_ROOTMASTER "ROOTMASTR"
#define TZ_NOLIB 0		/* special status: master sym has no file or library */

/* The following identify the lists in the records for master symbols and 
 * symbol instances. */
			/* for master symbols */
#define TZ_MSYM_LIST	1
#define TZ_INST_LIST 2
#define TZ_MGEOM_LIST 3
#define TZ_MTEXT_LIST 4
#define TZ_MSNAP_LIST 5
			/* for instances */
#define TZ_IGEOM_LIST 1
#define TZ_ITEXT_LIST 2
#define TZ_ISNAP_LIST 3
#define TZ_ACON_LIST  4

/* The following specifies the maximum number of variable 
 * lists in a symbol entity.
 */
#define TZ_MAX_NBR_VAR_LISTS 5

/* The following 3 defines specify the maximum number of geometric and master
 * symbol entities that can be put on the corresponding symbol without more
 * dynamic storage allocation */
#define TZ_MAX_SYM_GEOM	10
#define TZ_MAX_MASTERS  2
#define TZ_MAX_INSTANCES 20

/* The following specifies the maximum number of text nodes
 * a symbol can have without more dynamic storage allocation. */
#define TZ_MAX_TEXT_NODES	8 
/* The following specifies the maximum number of snap nodes 
 * a symbol can have without more dynamic storage allocation. */
#define TZ_MAX_SNAP_NODES	8
#define TZ_MAX_ACON_NODES	10

/* The following specifies the maximum number of characters
 * that can be in a text node; note, this must be less 
 * the number of characters specified by UM_TEXT_BUFSZ. */
#define TZ_MAX_TEXT_LEN 100

/* The following specifies the maximum length of a path name to a master symbol
 * library. */
#define TZ_MAX_PATH_LEN 200

#define TZ_SYMBOL_BUFSZ 4

#define TZ_INSTANCE_BUFSZ  4

#define TZ_CONECTOR_BUFSZ  4

/* The following definitions are used as values of the global visibility
 * variable for text nodes in instances; see below for the definition of 
 * the global that gets these values assigned to it 
 * (int TZ_text_node_visibility; below). */
#define TZ_ALL_TEXT_NODES_VISIBLE 0
#define TZ_ONLY_GRAPHIC_TEXT_NODES -1
#define TZ_NO_TEXT_NODES_VISIBLE -2

/* The following definitions are used to indicate whether a text node
 * is a graphic text node or not */
#define TZ_GRAPHIC_TEXT_NODE 0
#define TZ_NONGRAPHIC_TEXT_NODE 1
 
/* The following values are used in symbol post load operations */
#define TZ_CHKLOADEDMSYM 0
#define TZ_USEMSYMFROMPART 1
#define TZ_DONTUSE 2

/* The following definition is used to indicate an unknown key */
#define TZ_UNKNOWN -1

/* The following string is used to for debugging */
EXT char TZ_sbuf[120];

#include "tzsymddl.h"

#define TZ_FAILURE -1

/* The following specifies the length of the name of a 
 * symbol; note this must be the same as what is specified
 * in the symbol ddl specification. */
#define TZ_SYMBOL_NAME_LEN 11 

/* The following is used to hold the name of a symbol plus some
 * extra characters that "ux_ " functions may add to the name
 * in some calls such as "ux_decompose_path". */
#define TZ_SYMBOL_NAME_LEN_PLUS	TZ_SYMBOL_NAME_LEN+10
 

/* The following struct is used in returning data from the master symbol 
 * creation form. */
struct TZ_master_sym_frm_rec {
	char *name;		/* pointer to the buffer for the name of a master symbol */
	char *lib;		/* pointer to the library in which to put the symbol */
	UU_REAL *origin;	/* origin of the master symbol */
	};

/* The following struct is used in returning data from
 * the symbol instance form. */
struct TZ_sym_instance_frm_rec {
	char *name;			/* pointer to the buffer for the name of a symbol */
	char *lib;			/* which library */
	char *area;			/* which file area: "local", or "master" */
	UU_REAL scale;		/* scale factor of the symbol instance */
	UU_REAL angle;		/* angle to orient the instance */
	};

/* The following struct is used in returning data from
 * the text node form. */
struct TZ_text_node_frm_rec {
	int type;			/* text function; note, these fields are "int"s
							 * because they are toggle fields */
	int visibility;	/* visibility */
	int color;			/* color of text */
	UU_REAL angle;		/* angle of text with respect to the 
						 	 * construction plane x-axis */
	};                           

#ifndef both
/* The following enumeration types are used to specify form
 * return values of toggle fields */
enum TZ_promptval { promptopt, promptreq, label };
enum TZ_visibility { graphic, nongraphic };
#endif

/* The following structure is used in storing the library name of the most 
 * recently referenced master symbol library */
struct TZ_lib_name {
	char name[TZ_MAX_PATH_LEN];
	char area[8];			/* specifies the area: either "local" or "system" */
	UU_LOGICAL firstim;
 }; 

#ifdef TZ_BPGM
#include "mdattr.h" /* for modelling colors */
	/* This variable is used to store the name of the most recently accessed
	 * symbol. */
	char TZ_default_sym_name[TZ_SYMBOL_NAME_LEN] = {"******"}; 
	/* This variable is used to store the name of the most recently accessed
	 * symbol library and file area. */
	struct TZ_lib_name TZ_default_lib = { "******","local", UU_TRUE };
	/* The following variable is used to store the most recently used instance
	 * scale. */
	UU_REAL TZ_default_scale = 1.0;
	/* The following variable is used to store the most recently used instance
	 * orientation angle with respect to the construction plane axises */
	UU_REAL TZ_default_angle = 0.0;

	/* The following variables specify attributes for snap nodes */ 
	int TZ_snap_node_color =  UM_DARKRED; 
	int TZ_snap_node_marker_type	= 4; /* This is a circle; read next comment */
	/* Note, marker types are: ".", "+", "*", "O", "x"; forms have these indexed
	 * from 0-4, gks indexes them 1-5 */

	UU_LOGICAL TZ_snap_nodes_visible = UU_TRUE; /* for instances */

	/* The following variable stores the text node visibility for instances */
	int TZ_text_node_visibility = TZ_ALL_TEXT_NODES_VISIBLE;

	/* The following variable stores the text attribute flag for instances */
	char TZ_text_node_attr[] = "inherited"; /* can be either "inherited" or 
														  * "current" */
	/* declare default symbol attribute bundle */
	struct TZ_symattr_rec TZ_symattr_default_bundle;
#else
	extern char TZ_default_sym_name[TZ_SYMBOL_NAME_LEN]; 
	extern struct TZ_lib_name TZ_default_lib;
	extern UU_REAL TZ_default_scale;
	extern UU_REAL TZ_default_angle;
	extern struct TZ_lib_name TZ_default_lib; 
	extern int TZ_snap_node_color;
	extern int TZ_snap_node_marker_type;
	extern UU_LOGICAL TZ_snap_nodes_visible;
	extern int TZ_text_node_visibility;
	extern char TZ_text_node_attr[];
	extern struct TZ_symattr_rec TZ_symattr_default_bundle;
#endif

/* error message below is: Error in UNIBASE symbol setup 
 * from symbol subsystem */ 
#define TZ_SETUP_DATA(rel, ptr, size, status)		\
		uu_dprint(UU_BTRC,(us,"TZ_SETUP_DATA(rel:%d,ptr:%x,size:%d,status:%d)", \
						rel, ptr, size, status));			\
		(ptr)->key = -1;											\
		if (ur_setup_app_data(rel, ptr, size) != 0)		\
		{															\
			uu_uerror0(TZ_SYMBOL, 12);						\
			status = TZ_FAILURE;								\
		}															\
		else														\
			status = UU_SUCCESS;	

#define TZ_DEFAULT_TF UU_NULL
#define TZ_CURRENT_ATTR UU_NULL

/* The following definitions are used to indicate which kind of change basis
 * transformation is desired.
 */
#define TZ_CPLANE_TO_MODEL				0
#define TZ_MODEL_TO_CPLANE				1

/* The following definitions are the return statuses from "ub_check_fullpath".
 * These are the file statuses returned.
 */
#define TZ_E_X 	0	/* file exists and is xio compatible */
#define TZ_NE		-1 /* file does not exist */
#define TZ_E_NX	-2 /* file exists but is not xio compatible */
#define TZ_D		-3 /* file is a directory */
#define TZ_BP		-4 /* bad path name */

/* The following definitions are used to pair up symbol names and keys during
 * symbol post load operations */

typedef struct {
	char name[TZ_SYMBOL_NAME_LEN];
	UU_KEY_ID key;
	} TZ_list;

#define TZ_LIST_LEN 200	/* maximum number of master symbol names in list */

/* The following definition gives the box size for a master symbol.
 */
#define TZ_MASTER_SYMBOL_SIZE 	10

#if UU_COMP == UU_VAXVMS
#include ctype /* for "_toupper" definition */
#define toupper(c) _toupper(c)
#endif

#define TZ_TO_UPPER(name)									\
		{ char *ptr; ptr = name; while (*ptr != '\0'){*ptr=toupper(*ptr);ptr++;}}


#ifdef UU_DEBUGOFF 
#define TZ_IF_FAILURE_PRINT_IT
#else
#define TZ_IF_FAILURE_PRINT_IT                       \
	if (status != UU_SUCCESS)                         \
		uu_dprint(UU_BTRC,(us, "returned FAILURE"));   
#endif

#define TZ_GET_FAREA_IDENT(area, ident)		\
		if (strcmp(area, "local") == 0)			\
			strcpy(ident, "TZ_LOC_M_SYMDIR");	\
		else												\
			strcpy(ident, "TZ_SYS_M_SYMDIR")

#undef EXT

#define TZ_BSYM
#endif
