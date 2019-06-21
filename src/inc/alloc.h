/*********************************************************************
**    NAME         :  alloc.h
**       CONTAINS:	defn's for prt's full umalloc.c
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       alloc.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:11
*********************************************************************/

#ifndef ALLOCH

#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif

#define STBLKSIZE 500000
#define USEDB 2
#define USED 1
#define FREE 0
#define SECURITY_CODE 0xF5F5

#define ERR_FREE_ROOT			0
#define ERR_FREE_CURRENT		1
#define ERR_INVALID_STORE		2
#define ERR_STACK_OVERFLOW		3
#define ERR_STACK_UNDERFLOW	4

#define STACK_MAX 20

typedef struct
{
	unsigned					used:2;			/* FALSE if free, USEDB->block BOUND_T */
	unsigned					size:29;			/* size of data storage in bytes */
} BOUND_T;

typedef struct block_t
{
	struct block_t			*next;		/* pointer to next block */
	struct block_t			*prev;		/* pointer to prev block */
		/* the following fields are included so that 
			sizeof( BLOCK_T ) + STBLKSIZE gives the 
			size of a standard block */
	BOUND_T					first;		
	BOUND_T					data[2];
	BOUND_T					last;
} BLOCK_T;

typedef struct chunk_t
{
	BOUND_T					bound;		/* size & used flag, dup'ed at chunk end */
		/* the following fields are used when the chunk is on the free list */
		/* &chunk->forw is used as the data address */
		/* a chunk must always be large enough to hold these pointers */
	struct chunk_t			*forw;		/* forward pointer to chunk on free list */
	struct chunk_t			*back;		/* backward pointer to chunk on free list */
		/* the following fields are not accurately positioned, */
		/* they are included for size only	*/
	char						data[4];
	BOUND_T					end_bound;	
} CHUNK_T;

#define MIN_CHUNK (sizeof( CHUNK_T )+72)

typedef struct store_t
{
	unsigned					security;	/* used in valid_store() for error check */
	struct store_t			*parent, *son, *sibling;
	BLOCK_T					*block;		/* pointer to next block, or NULL */
	CHUNK_T					*free;		/* pointer into free chunk list */
} STORE_T;

typedef struct {			/* store scan block */
	BLOCK_T *block;		/* pointer to next scan block, or NULL if no more */
	CHUNK_T *chunk;		/* pointer to next chunk (undefined if blk==NULL) */
} STORE_SCAN;

EXT STORE_T *uu_current_store;
/*********************************************************************
**    E_FUNCTION     : uu_set_current_store( store ) - make store current.
*********************************************************************/
#define uu_set_current_store(store) \
	if(((STORE_T *)(store))->security==SECURITY_CODE) \
	uu_current_store = (STORE_T *)store; \
	else valid_store(store)

/*********************************************************************
**    E_FUNCTION     : STORE_T * uu_get_current_store() 
*********************************************************************/
#define uu_get_current_store() uu_current_store
#define ALLOCH

#endif
