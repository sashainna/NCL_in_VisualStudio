#ifndef RMTUPLEH
/*********************************************************************
**    NAME         :	rmtuple.h
**       CONTAINS:
**       Master entry
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rmtuple.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:48
*********************************************************************/

#include "riddle.h"

/* define the master identifier relation */
#define UR_MTUPLE_REL	0

/* defines for bit fields in the master tuple: */
#define UR_EDITABLE		0	/* editable flag for this entity, 1=editable */
#define UR_BLANKED		1	/* blanked flag for this entity, 1=blanked */

/* predicate to question if an entity is editable */
#define ur_editablep(master) \
(uu_tst_bit(&(master->bit_tbl), UR_EDITABLE)!=0)

/* predicate to question if an entity is blanked and blank/unblank functions */
#define ur_blankedp(master) \
(uu_tst_bit(&(master->bit_tbl), UR_BLANKED)!=0)
#define ur_blank(master) \
uu_set_bit(&(master->bit_tbl), UR_BLANKED)
#define ur_unblank(master) \
uu_clr_bit(&(master->bit_tbl), UR_BLANKED)

/* define hard wired indexes into association list within the mtid, */
#define UR_DATA_INDX		0	/* data index into attribute table (must be first) */
#define UR_TRANSF_INDX	1	/* transformation indx into attribute table*/
#define UR_ATTR_INDX		2	/* attribute bundle index into attribute table	*/

/* and max hardwired index */
#define UR_MAX_INDX		2	/* max index which is always present */

#define RMTUPLEH
#endif
