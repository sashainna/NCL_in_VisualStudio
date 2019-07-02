/*********************************************************************
**    NAME:  r1isched.h
**       CONTAINS:
**       	Associativity scheduler data types.
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       r1isched.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:42
*********************************************************************/
#include "usysdef.h"	/* basic data types */
#include "uhep.h"		/* for error system */
#include "udebug.h"	/* for debugging trace facility */
#include "uhash.h"	/* for UU_HASH_TBL */
/* #include "umessages.h" */
#include "r1esched.h"

#ifndef UR_R1ISCHED

#define UR_schedHashTblSz  100 /* size of hash table for retaining information
										  * about each association tuple scheduled; 
										  * this should be at least ~50% larger than 
										  * the number of (average) distinct association
										  * tuples that are being updated at one time. */

#ifdef UR_R1PGM
#define EXT
UU_LOGICAL  UR_initSched = UU_TRUE; /* UU_TRUE iff schedule is must be init'd */
UR_schedEntry  *UR_topOfSched = UU_NULL;/* pointer to top of schedule */
UR_schedEntry  *UR_endOfSched = UU_NULL;/* pointer to end of schedule */
UU_LOGICAL  UR_schedOn = UU_FALSE;/* UU_TRUE iff the schedule is processing */ 
UU_HASH_TBL UR_schedHashTbl;/* used to find out if an assoc. tuple
									  * is scheduled. */
char  *UR_updateListHead = UU_NULL;
					/* head of the list of updates to be committed to the schedule */
UR_schedEntry	**UR_updateListEnd  = UU_NULL;
					/* end of the list of updates to be committed to the schedule */
int 			UR_nbrUpdates = 0; /* the number of elements on the list pointed
										  * to by "UR_updateListHead" */
char *UR_message[]={ 
							"UR_HIGHEST_PRIORITY",
							"UR_NO_MSG",  "UR_TOTOP",     "UR_NXT_HIGHER_POSITION",
							"UR_FOUND",   "UR_NOTFOUND",  "UR_TOPSCHED", 
							"UR_TOEND",   "UR_NXT_LOWER_POSITION"};
#else
#define EXT extern
extern UU_LOGICAL  UR_initSched; 
extern UR_schedEntry		*UR_topOfSched;
extern UR_schedEntry  	*UR_endOfSched;
extern UU_LOGICAL  UR_schedOn;	
extern UU_HASH_TBL UR_schedHashTbl;
extern char 		*UR_updateListHead;
extern UR_schedEntry		**UR_updateListEnd;
extern int 			 UR_nbrUpdates;
extern char 		*UR_message[];
#endif

EXT char UR_sbuf[100];


	/* Schedule position data type */
typedef struct
{
		UR_schedEntry *where;		/* pointer to a schedule entry */
		int message;	/* message indicating where to look for sched entry 
							 * relative to "where" */
} UR_schedPosition;

#define UR_HIGHEST_PRIORITY				0

#define UR_NO_MSG 							1 
#define UR_TOTOP 								2 
#define UR_NXT_HIGHER_POSITION 			3 
#define UR_FOUND 								4 
#define UR_NOTFOUND 							5
#define UR_TOPSCHED 							6
#define UR_TOEND 								7
#define UR_NXT_LOWER_POSITION 			8

#undef EXT

#define UR_R1ISCHED

#endif /* pairs with #ifndef UR_R1SCHED at top of file */
