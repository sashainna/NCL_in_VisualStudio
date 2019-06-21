/********************************************************************
**    NAME:  bsym.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bsym.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:11
*********************************************************************/

#ifndef UB_BSYM

#include "nconst.h"
#include "xfsys1.h"

#ifdef UB_BPGM
#define EXT
#else
#define EXT extern
#endif

#define UB_MSYMPATH(msymptr) (msymptr)->path
#define UB_MSYMNAME(msymptr) (msymptr)->label

/* The following is the name put into master symbol files to indicate the root
 * master symbol in the file. */
#define UB_ROOTMASTER "ROOTMASTR\"
#define UB_NOLIB 0		/* special status: master sym has no file or library */

/* The following identify the lists in the records for master symbols and 
 * symbol instances. */
			/* for master symbols */
#define UB_MSYM_LIST	1
#define UB_INST_LIST 2
#define UB_MGEOM_LIST 3
#define UB_MTEXT_LIST 4
#define UB_MSNAP_LIST 5
			/* for instances */
#define UB_IGEOM_LIST 1
#define UB_ITEXT_LIST 2
#define UB_ISNAP_LIST 3
			/* for connectors */
#define UB_CON_INST_LIST 1

/* The following specifies the field number of instances within the connector
 * record. */
#define UB_CON_INST_FLD  3

/* The following specifies the maximum number of variable 
 * lists in a symbol entity.
 */
#define UB_MAX_NBR_VAR_LISTS 5

/* The following 3 defines specify the maximum number of geometric and master
 * symbol entities that can be put on the corresponding symbol without more
 * dynamic storage allocation */
#define UB_MAX_SYM_GEOM	10
#define UB_MAX_MASTERS  2
#define UB_MAX_INSTANCES 20

/* The following specifies the maximum number of text nodes
 * a symbol can have without more dynamic storage allocation. */
#define UB_MAX_TEXT_NODES	8 
/* The following specifies the maximum number of snap nodes 
 * a symbol can have without more dynamic storage allocation. */
#define UB_MAX_SNAP_NODES	8

/* The following specifies the maximum number of characters
 * that can be in a text node; note, this must be less 
 * the number of characters specified by UM_TEXT_BUFSZ. */
#define UB_MAX_TEXT_LEN 100

/* The following specifies the maximum length of a path name to a master symbol
 * library. */
#define UB_MAX_PATH_LEN UX_MAX_PATH_LEN

#define UB_SYMBOL_BUFSZ 4

#define UB_INSTANCE_BUFSZ  4

#define UB_CONECTOR_BUFSZ  4

/* The following definitions are used as values of the global visibility
 * variable for text nodes in instances; see below for the definition of 
 * the global that gets these values assigned to it 
 * (int UB_text_node_visibility; below). */
#define UB_ALL_TEXT_NODES_VISIBLE 0
#define UB_ONLY_GRAPHIC_TEXT_NODES -1
#define UB_NO_TEXT_NODES_VISIBLE -2

/* The following definitions are used to indicate whether a text node
 * is a graphic text node or not */
#define UB_GRAPHIC_TEXT_NODE 0
#define UB_NONGRAPHIC_TEXT_NODE 1
 
/* The following values are used in symbol post load operations */
#define UB_CHKLOADEDMSYM 0
#define UB_USEMSYMFROMPART 1
#define UB_DONTUSE 2

/* The following definition is used to indicate an unknown key */
#define UB_UNKNOWN -1

/* The following string is used to for debugging */
EXT char UB_sbuf[120];

#include "bsymddl.h"
#include "xfsys1.h"

#define UB_FAILURE -1

/* The following specifies the length of the name of a 
 * symbol; note this must be the same as what is specified
 * in the symbol ddl specification. */
#define UB_SYMBOL_NAME_LEN NCL_MAX_LABEL 

/* The following is used to hold the name of a symbol plus some
 * extra characters that "ux_ " functions may add to the name
 * in some calls such as "ux_decompose_path". */
#define UB_SYMBOL_NAME_LEN_PLUS	NCL_MAX_LABEL_AND_SUBSCRIPT
 
#define UB_SPART_BUFSZ 4000	 /** Buffer sizes for curve records **/

struct UB_spart_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[UB_SYMBOL_NAME_LEN];
	int	version;
	char	path[UB_MAX_PATH_LEN];
	int	no_geom;
	UU_KEY_ID	geom[UB_SPART_BUFSZ];
};

/*
......added Yurong 9/21/98
*/
/* The following struct is used in returning data from the symbol name
  form. */
struct UB_symnam_frm_rec {
	char *name;		/* pointer to the buffer for the name of a master symbol */
	char *lib;		/* pointer to the library in which to put the symbol */
	int libdir;    /* local = 0, system = 1 */
	};

/* The following struct is used in returning data from the master symbol 
 * creation form. */
struct UB_master_sym_frm_rec {
	char *name;		/* pointer to the buffer for the name of a master symbol */
	char *lib;		/* pointer to the library in which to put the symbol */
	UU_REAL *origin;	/* origin of the master symbol */
	int cmd;
	};

/* The following struct is used in returning data from
 * the symbol instance form. */
struct UB_sym_instance_frm_rec {
	char *name;			/* pointer to the buffer for the name of a symbol */
	char *lib;			/* which library */
	char *area;			/* which file area: "local", or "master" */
	UU_REAL scale;		/* scale factor of the symbol instance */
	UU_REAL angle;		/* angle to orient the instance */
	int cmd;
	};

/* The following struct is used in returning data from
 * the text node form. */
struct UB_text_node_frm_rec {
	int type;			/* text function; note, these fields are "int"s
							 * because they are toggle fields */
	int visibility;	/* visibility */
	int color;			/* color of text */
	UU_REAL angle;		/* angle of text with respect to the 
						 	 * construction plane x-axis */
	};                           

/* The following enumeration types are used to specify form
 * return values of toggle fields */
enum UB_promptval { promptopt, promptreq, label };
enum UB_visibility { graphic, nongraphic };

#ifdef UB_BPGM
#include "mdattr.h" /* for modelling colors */
	/* This variable is used to store the name of the most recently accessed
	 * symbol. */
	char UB_default_sym_name[UB_SYMBOL_NAME_LEN_PLUS] = {"******"}; 
	char UB_default_spart_name[UB_SYMBOL_NAME_LEN_PLUS] = {"******"}; 

	/* This variable is used to store the name of the most recently accessed
	 * symbol library name and area, the env. variables used for the local
	 * and system symbol library file areas, and a prompt string which is
	 * the application type of library, and the file type and file are
	 * extensions (env. var. 's of them, that is) */
	UX_libdata_bag UB_libdata_rec = { UU_TRUE, "local", "******",
		"UB_LOC_M_SYMDIR", "UB_SYS_M_SYMDIR", "symbol", "UB_SYM_EXTEN",
		"UB_SYM_AREA_SUFFIX", "UB_SYM_SUFFIX" };
 	UX_libdata_bag UB_spl_libdata_rec = { UU_TRUE, "local", "******",
 		"UB_LOC_SPLDIR", "UB_SYS_SPLDIR", "standard part", "UB_SPL_EXTEN",
 		"UB_SPL_AREA_SUFFIX", "UB_SP_SUFFIX" };

	/* The following variable is used to store the most recently used instance
	 * scale. */
	UU_REAL UB_default_scale = 1.0;
	/* The following variable is used to store the most recently used instance
	 * orientation angle with respect to the construction plane axises */
	UU_REAL UB_default_angle = 0.0;
 	UU_REAL UB_spl_default_scale = 1.0;
 	UU_REAL UB_spl_default_angle = 0.0;
	int UB_default_cmd = 0;

	/* The following variables specify attributes for snap nodes */ 
	int UB_snap_node_color =  UM_WHITE; 
	int UB_snap_node_marker_type	= 4; /* This is a circle; read next comment */
	/* Note, marker types are: ".", "+", "*", "O", "x"; forms have these indexed
	 * from 0-4, gks indexes them 1-5 */

	UU_LOGICAL UB_snap_nodes_visible = UU_TRUE; /* for instances */

	/* The following variable stores the text node visibility for instances */
	int UB_text_node_visibility = UB_ALL_TEXT_NODES_VISIBLE;

	/* The following variable stores the text attribute flag for instances */
	char UB_text_node_attr[] = "inherited"; /* can be either "inherited" or 
														  * "current" */
	/* declare default symbol attribute bundle */
	struct UB_symattr_rec UB_symattr_default_bundle;
	UU_LOGICAL UB_text_default = UU_TRUE; /* for default text */
#else
	extern char UB_default_sym_name[UB_SYMBOL_NAME_LEN_PLUS]; 
	extern char UB_default_spart_name[UB_SYMBOL_NAME_LEN_PLUS]; 
 	extern UX_libdata_bag UB_spl_libdata_rec;
	extern UX_libdata_bag UB_libdata_rec;
	extern UU_REAL UB_default_scale;
	extern UU_REAL UB_default_angle;
	extern UU_REAL UB_spl_default_scale;
	extern UU_REAL UB_spl_default_angle;
	extern int UB_default_cmd;
	extern int UB_snap_node_color;
	extern int UB_snap_node_marker_type;
	extern UU_LOGICAL UB_snap_nodes_visible;
	extern int UB_text_node_visibility;
	extern char UB_text_node_attr[];
	extern struct UB_symattr_rec UB_symattr_default_bundle;
	extern UU_LOGICAL UB_text_default; /* for default text */
#endif

/* error message below is: Error in UNIBASE symbol setup 
 * from symbol subsystem */ 
#define UB_SETUP_DATA(rel, ptr, size, status)		\
		uu_dprint(UU_BTRC,(us,"UB_SETUP_DATA(REL:%d,PTR:%x,SZ:%d,?)", \
						rel, ptr, size));			\
		(ptr)->key = -1;											\
		if (ur_setup_app_data(rel, ptr, size) != 0)		\
		{															\
			uu_uerror0(UB_SYMBOL, 12);						\
			status = UB_FAILURE;								\
		}															\
		else														\
			status = UU_SUCCESS;	

#define UB_DEFAULT_TF UU_NULL
#define UB_CURRENT_ATTR UU_NULL

/* The following definitions are used to indicate which kind of change basis
 * transformation is desired.
 */
#define UB_CPLANE_TO_MODEL				0
#define UB_MODEL_TO_CPLANE				1

/* The following definitions are the return statuses from "ub_check_fullpath".
 * These are the file statuses returned.
 */
#define UB_E_X 	0	/* file exists and is xio compatible */
#define UB_NE		-1 /* file does not exist */
#define UB_E_NX	-2 /* file exists but is not xio compatible */
#define UB_D		-3 /* file is a directory */
#define UB_BP		-4 /* bad path name */

/* The following definitions are used to pair up symbol names and keys during
 * symbol post load operations */

typedef struct {
	char name[UB_SYMBOL_NAME_LEN];
	int subscr;
	UU_KEY_ID key;
	} UB_list;

#define UB_LIST_LEN 300	/* maximum number of master symbol names in list */

/* The following definition gives the box size for a master symbol.
 */
#define UB_MASTER_SYMBOL_SIZE 	10

#if UU_COMP == UU_VAXVMS
#include ctype /* for "_toupper" definition */
#define toupper(c) _toupper(c)
#endif

#define UB_TO_UPPER(name)									\
		{ char *ptr; ptr = name; while (*ptr != '\0'){*ptr=toupper(*ptr);ptr++;}}


#ifdef UU_DEBUGOFF 
#define UB_IF_FAILURE_PRINT_IT
#else
#define UB_IF_FAILURE_PRINT_IT                       \
	if (status != UU_SUCCESS)                         \
		uu_dprint(UU_BTRC,(us, "returned FAILURE"));   
#endif

#define UB_GET_FAREA_IDENT(area, ident)		\
		if (strcmp(area, "local") == 0)			\
			strcpy(ident, "UB_LOC_M_SYMDIR");	\
		else												\
			strcpy(ident, "UB_SYS_M_SYMDIR")

/*********************************************************************
**    I_MACRO: ubi_get_app_spec_assoc_list(key,assocRel,field,
**											nbrKeys,keyListptr) 
**			
**    PARAMETERS   
**       INPUT: 
**				key		Key of entity whose associated entities are desired.
**				assocRel	Relation number of associated entities desired.
**				field		Only the associated entities that have "key" in this ddl
**							field number will be retrieved.  If this is 0, all fields
**							are considered.
**       OUTPUT:  
**				nbrKeysptr	Pointer to the number of keys returned.
**				keyListptr	Pointer to the pointer to the buffer containing the 
**								returned keys.
**    RETURNS: none.
**    SIDE EFFECTS: calling function MUST have a "failed" label.
**    WARNINGS: none
*********************************************************************/
#define ubi_get_app_spec_assoc_list(key,assocRel,field,nbrKeysptr,keyListptrptr) \
	uu_dprint(UU_BTRC,(us,"ubi_get_app_spec_assoc_list(ky:%d,rel:%d,fld:%d,?,?)(macro)", key, assocRel, field));	\
	if (ur_get_app_spec_assoc_list(key,assocRel,field,nbrKeysptr,keyListptrptr)\
			!= UU_SUCCESS) goto failed

#ifdef EXT
#undef EXT
#endif

#define UB_BSYM
#endif
