/*********************************************************************
**    NAME         :  rerrdef.h
**       CONTAINS:
**       symbolic names for Unibase error codes
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rerrdef.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:42
*********************************************************************/

#ifndef RERRDEFH


/* this first couple are not errors and should be elsewhere */
#define UR_PRTNMLN 16
#define UR_MAX_ATTR1 64

/* error codes */
#define UR_UU -5
#define UR_PRTXISTS -1

/* errors */
#define URM_MT_PART 1
#define URM_CANT_DEL 2
#define URM_CANTOPEN_SV 3
#define URM_NO_MEM_SV 4
#define URM_SVVARLSTERR 5
#define URM_WRT_ERR 6
#define URM_CANTOPEN_LD 7
#define URM_INCOMPAT 8
#define URM_ILL_REL 9
#define URM_MTID_RDERR 10
#define URM_NO_MEM_LD 11
#define URM_ILL_RCB 12
#define URM_VARL_RDERR 13
#define URM_NEXIST_VARL 14
#define URM_ILL_MTID 15
#define URM_MTID_ERR 16
#define URM_NO_RDBMS 17
#define URM_RELNTFND 18
#define URM_NO_PART 19
#define URM_RDBMS_ERR 20
#define URM_AUTOSAVE 21
#define URM_ANS_NOT_TORN 22
#define UR_BAD_ENV_FILE 23
#define UR_BAD_DD -25

#define RERRDEFH
#endif
