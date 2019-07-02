
/*********************************************************************
**    NAME         :  ysplan.h
**       CONTAINS:
**      		Include file for Space Planning. 
**    COPYRIGHT 1984, 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tzysplan.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:02
*********************************************************************/


#include "usysdef.h"
#include "mdrel.h"

 
#ifdef UY_YPGM
#define EXT
#else
#define EXT extern
#endif
/*           NUMBER OF INITIAL USER RELATIONAL ENTITIES
 *  This section gives the initial nbr of user entities of each type to be
 *  allocated by unibase; these also designate the nbr of entity records
 *  of each type to be allocated when more are needed.
 *  Note, if the modeling subsystem were to be changed, these defines
 *  would go in "minitrel.h".
 */

#define UY_NE_ROOM  1 		/*	nbr of initial room relational entities		*/
#define UY_NE_PARTN 50 	/*	nbr of initial partition relational entities	*/
#define UY_NE_DESK  50  		/*	nbr of initial desk relational entities		*/
#define UY_NE_CHAIR  50 		/*	nbr of initial chair relational entities		*/
#define UY_NE_SPLANATT 50	/* nbr of initial space planning attribute bundles */



/*      NUMBER OF VARIABLE LISTS ASSOCIATED WITH EACH USER ENTITY
 *  Note, if the modeling subsystem were to be changed, these defines
 *  would go in "initrel.h".
 */

#define UY_MAX_NBR_LISTS 1	/* max number of variable lists in any entity in
									 * space planning */

#define UY_NV_ROOM  1  /* the variable list here contains the entity mtid's
                       * for the geometric entities corresponding to the
                       * walls of the room.
							  */
#define UY_NV_PARTN 0 	/* there are no variable lists associated with
                         	/* partitions  
									 */
#define UY_NV_DESK 0     	/* there are no variable lists associated with
                        	 * desks
									 */
#define UY_NV_CHAIR 1     	/* the variable list associated with chairs
									 * will contain exactly one circle entity record.
									 */



/*            USER RELATION TYPES (REL_NUM)						 */
/* now in mdrel.h                                            */



/*         expansion size for variable length list entity      */
#define UY_ROOM_EXPANDSIZE   20
#define UY_CHAIR_EXPANDSIZE  5


#define UY_USER_ENTITY_CLASS  0 
#define UY_UNKNOWN_USER_CLASS 1


/**************************************************************************
 *            USER ENTITY DATA TYPES                                      *
 **************************************************************************/

#define UY_MAXWALLS 50		/* max number of walls for a room				   */
#define UY_ROOM_BUFSZ 4000 /* size of the variable list buffer for rooms    */

#define UY_CHAIR_BUFSZ 3000		/* variable list of length one */

/* structure large enough for any user defined entity */
struct UY_userdatabag { 
	UU_KEY_ID key;
	int rel_num;
	char data[1000];
	};

/* structure large enough for any users defined atribute  */

struct UY_userattrbag {
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	color;
	int	layer;
	int	pen;
	int	line_style;
	UU_REAL line_width;
	UU_LOGICAL displayable;
	UU_LOGICAL selectable;
	UU_LOGICAL blanked;
	char data[1000];
}; 

#include "tzspland.h"	/* output from ddl to define space planning entities */

/**************************************************************
 *                 USER PROMPT AND ERROR SUBSYSTEM DEFINITION *
 *  Note, this could be put in the include file "sysdef.h"    *
 *  where the other prompt and error subsystem definitions are*
 *   defined.                                                 *
 **************************************************************/

#define USER_APPLIC  49
              
/***************************************************************
 *                  USER PROMPT OPTIONS                        *
 ***************************************************************/

#define UY_ALONG_A_VECTOR 0
#define UY_BTWN_2_PTS     1

/*************************************************************
 *                    STATUS VARIABLES                       *
 *************************************************************/

#define UY_SUCCESS  0
#define UY_FAILURE  -1 

/*************************************************************
 *                   PARTITION COLORS                        *
 *************************************************************/

#define UY_BROWN 0
#define UY_GREEN 1
#define UY_ORANGE 2
#define UY_BEIGE  3
#define UY_GREY   4
#define UY_MAUVE  5

/***********************************************************************
*						SYSTEM MODALS													*
***********************************************************************/

EXT UU_REAL uy_partition_ang;
EXT UU_REAL uy_partition_len;
EXT UU_REAL uy_support_len;
EXT UU_REAL uy_desk_ang;
EXT UU_REAL uy_desk_len;
EXT UU_REAL uy_desk_wid;
EXT UU_REAL uy_chair_radius;
/************************************************************************
 *						MISCELLANEOUS													 *
 ***********************************************************************/

#define UY_NBRDESK_SIDES 4
EXT char uy_buf[120];									/*	global buffer for printing	*/

/* for view key */
#define UY_ALLVIEWS 0


#undef EXT
