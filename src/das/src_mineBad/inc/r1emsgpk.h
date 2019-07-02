/*********************************************************************
**    NAME:  r1emsgpk.h
**       CONTAINS: Message data type for associativity messages.
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       r1emsgpk.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:41
*********************************************************************/
#ifndef UR_R1EMSGPK

#define UR_MAX_MSGS 5 /*max nbr of optional messages in update descriptors*/

extern char *uu_msg();

typedef struct
{
	char *dataptr;
	int datasz;
} UR_dataPacket;

	/* Message packet definition */
typedef struct
{
	int message;
	int nbrDataPks;	/* actual number of data packets */
	UR_dataPacket *dataPacket; 
} UR_messagePacket;

/* The following gets the message from the message packet. */
#define UR1_GET_MSG(msgPacketptr) \
	((msgPacketptr == UU_NULL) ?  UU_NO_MSG_PACKET : ((msgPacketptr)->message))

/* The following macro gets the current message from the association update
 * descriptor.  Note, this does NOT increment "currentMsgPacket" field. */
#define UR1_CURNT_MSG(descrptr) \
	((descrptr)->msgPacket[(descrptr)->currentMsgPacket].message)

#define UR1_BAD_MSGPK_NBR	-1 /* This is used to indicate tha the current 
										 * message packet indicator is illegitimate. */

#define UR_R1EMSGPK
#endif

