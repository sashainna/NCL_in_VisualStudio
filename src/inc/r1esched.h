/*********************************************************************
**    NAME:  r1esched.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       r1esched.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:42
*********************************************************************/
#ifndef UR_R1ESCHED
#include "r1emsgpk.h"	/* for UR_messagePacket */

#ifdef UR_R1PGM
int UR_batchNumber = 0;	/* nbr of unmatched ur1_beginBatch calls */
#else
extern int UR_batchNumber;
#endif

	/* Association tuple update descriptor */
typedef struct
{
	UU_KEY_ID	assocKey;
	int			field;		/* assoc. tuple column containing entity causing 
									 * update */
	UU_KEY_ID	causeKey;	/* key of entity being causing update */
	UR_messagePacket msgPacket[UR_MAX_MSGS]; /* message packets */
	int currentMsgPacket;	/* message packet currently being examined by user;
									 * starts with 0. */
	int nbrPackets;			/* total number of message packets; starts with 1. */
} UR_assocUpdateDescriptor;

	/* Schedule entry data type */
typedef struct UR_schedRec
{	
	UR_assocUpdateDescriptor descrpt;
									/* pointer to the update descriptor */
	int priority;				/* priority of the schedule entry */
	UU_LOGICAL valid;			/* UU_TRUE iff the schdule entry is valid */
	struct UR_schedRec *higher;/* pointer to the schedule entry immediately
										 * higher in priority */
	struct UR_schedRec *lower;	/* pointer to the schedule entry immediately
										 * lower in priority */
	struct UR_schedRec *sameTupleHigher; 
										/* links to any other update descriptors for 
							 			/* the same tuple of higher priority */
	struct UR_schedRec *sameTupleLower; 
										/* links to any other update descriptors for 
							 			/* the same tuple of lower priority */
} UR_schedEntry;

#define UR_R1ESCHED
#endif
