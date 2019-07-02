/*********************************************************************
**    NAME:  umessage.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       umessages.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:05
*********************************************************************/
#ifndef UMESSAGE
#define UU_STOP_SCHED_MSG 		0 
#define UU_NO_OTHER_MSGS		1

typedef struct {
		int msgnbr;
		char message[100];
	} UU_message;

typedef char *UU_messageDescriptor;

#define UU_NOKEY		-1
#define UU_NOFIELD	-2

/* The following 2 messages are to be used with UR1_UPDATE_ME and 
 * UR1_AUTO_UPDATE macros. */
#define UU_REC_CHANGE			0
#define UU_NO_REC_CHANGE		1
#define UU_NOMESSAGE 			2
/* The following are miscellaneous messages. */
#define UU_ENDOFMSGS				3
#define UU_NO_MSG_PACKET		4
/* The following are the default messages Unibase supplies if no messages are
 * given by the calling module. */
#define UU_DEFAULT_UPDATE		5
#define UU_DEFAULT_UPDT_FIXED	6
#define UU_DEFAULT_UPDT_DVARL	7
#define UU_DEFAULT_UPDT_TRANS	8
#define UU_DEFAULT_UPDT_ATTR	9
#define UU_DEFAULT_UPDT_DSEG	10
#define UU_DEFAULT_UPDT_COLOR	11
#define UU_DEFAULT_UPDT_LAYER	12
#define UU_DEFAULT_UPDT_PEN	13
#define UU_DEFAULT_UPDT_LNSTY	14
#define UU_DEFAULT_UPDT_LNWGT	15
#define UU_DEFAULT_UPDT_LNWID	16
#define UU_DEFAULT_UPDT_DISP	17
#define UU_DEFAULT_UPDT_SEL	18
#define UU_DEFAULT_UPDT_BLNKD	19
#define UU_DEFAULT_UPDT_VKEY	20
#define UU_DEFAULT_DELETE		21
#define UU_DEFAULT_DEL_ATTR	22
#define UU_DEFAULT_DEL_DVARL	23
#define UU_DEFAULT_DEL_TRANS	24
#define UU_DEFAULT_DEL_VARLA	25
/* The following messages are passed to permission and update modules. */
#define UU_TRANSLATE 			26
#define UU_ROTATE					27
#define UU_MIRROR					28
#define UU_SCALE					29
#define UU_DELETE					30
#define UU_VIS						31
#define UU_INVIS					32
#define UU_CHANGE_ATTR 			33
#define UU_REGENERATE  			34
#define UU_ADD2CONNECTOR 		35
#define UU_NEVERDISPLAYABLE	36
#define UU_DISPLAYABLE			37
#define UU_UPDATE_TRANSF		38
#define UU_TRANSFORM				39


#ifdef UU_MSGPGM
char *uu_message[] = {
	"UU_REC_CHANGE",
	"UU_NO_REC_CHANGE",
	"UU_NOMESSAGE",
	"UU_ENDOFMSGS",
	"UU_NO_MSG_PACKET",
	"UU_DEFAULT_UPDATE",
	"UU_DEFAULT_UPDT_FIXED",
	"UU_DEFAULT_UPDT_DVARL",
	"UU_DEFAULT_UPDT_TRANS",
	"UU_DEFAULT_UPDT_ATTR",
	"UU_DEFAULT_UPDT_DSEG",
	"UU_DEFAULT_UPDT_COLOR",
	"UU_DEFAULT_UPDT_LAYER",
	"UU_DEFAULT_UPDT_PEN",
	"UU_DEFAULT_UPDT_LNSTY",
	"UU_DEFAULT_UPDT_LNWGT",
	"UU_DEFAULT_UPDT_LNWID",
	"UU_DEFAULT_UPDT_DISP",
	"UU_DEFAULT_UPDT_SEL",
	"UU_DEFAULT_UPDT_BLNKD",
	"UU_DEFAULT_UPDT_VKEY",
	"UU_DEFAULT_DELETE",
	"UU_DEFAULT_DEL_ATTR",
	"UU_DEFAULT_DEL_DVARL",
	"UU_DEFAULT_DEL_TRANS",
	"UU_DEFAULT_DEL_VARLA",
	"UU_TRANSLATE",
	"UU_ROTATE",
	"UU_MIRROR",
	"UU_SCALE",
	"UU_DELETE",
	"UU_VIS",
	"UU_INVIS",
	"UU_CHANGE_ATTR",
	"UU_REGENERATE",
	"UU_ADD2CONNECTOR",
	"UU_NEVERDISPLAYABLE",
	"UU_DISPLAYABLE",
	"UU_UPDATE_TRANSF",
	"UU_TRANSFORM"
};
#else
extern char *uu_message[];
#endif

#define UMESSAGE
#endif
