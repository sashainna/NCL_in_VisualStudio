/********************************************************************* 
**  NAME:  gsegac.h  -- Segment table access routines.
**		These macros use the hash table routines (found in uhash.c) to
**		access graphics segment headers. Each segment header is an entry
**		in a single hash table.
**       CONTAINS:
**		ug_segacinit() -- init segment storage.
**		ug_segacterm() -- terminate segment storage.
**	  	UG_segstli *ug_segac(segid) -- returns a pointer to the seg's state info. 
**		ug_seginitscan() -- initialize segment state info scan.
**		UG_segstli *ug_segscan() -- return ptr to next segment's state info.
**		int *ug_segtop(segptr) -- set current cmd at beginning of seg (before the 
**									first command). return NULL.
**		ug_segcomp(n) -- compact a seg's storage.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gsegac.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:22
*********************************************************************/
#ifndef GSEGACH
#include "gtblvar1.h"
#include "gtblseg.h"
#include "uhash.h"
char *uu_hash_get(), *uu_hash_add(), *uu_hash_scan();
UG_segstli *ug_segadd(), *ug_segscan();

/*********************************************************************
**    I_FUNCTION     :  UG_segstli *ug_segac(segid) -- access a segment.
*********************************************************************/
#define ug_segac(segid) ((UG_segstli *)uu_hash_get(ug_seghash,segid))

/*********************************************************************
**    I_FUNCTION     :  ug_seginit() -- initialize segment tables.
**								All existing segments are deleted .
*********************************************************************/
#define ug_seginit() uu_hash_empty(ug_seghash)

/*********************************************************************
**    I_FUNCTION     :  ug_segcomp(n)  -- compact seg n's storage.
*********************************************************************/
#define ug_segcomp(n) {uu_alloc_push(uu_segstore); \
	ug_lsicomp(ug_segac(n)->seglist); uu_alloc_pop();}
	
#define GSEGACH
#endif
