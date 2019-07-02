/*********************************************************************
**    NAME         :  uhash.h
**       CONTAINS:
**    		include file for hash table routines.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uhash.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:04
*********************************************************************/

#ifndef UHASHH

typedef struct 
{
	int nbuckets;				/* number of buckets in this table, power of 2 */
	char **buckets;			/* pointer to bucket array */
	int scanbucket;			/* current bucket number, for sequential scan */
	char *scanelt;				/* pointer to next entry, for scan */
} UU_HASH_TBL[1];

char *uu_hash_add();
char *uu_hash_get();
char *uu_hash_scan();


#define UHASHH
#endif
