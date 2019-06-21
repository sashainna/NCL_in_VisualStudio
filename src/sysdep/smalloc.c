/*********************************************************************
**    NAME         : smalloc.c -- UNICAD dynamic storage management
**		CONTAINS:
**          int uu_alloc_init() -- initialize storage management
**          int uu_alloc_term() -- terminate storage management
**				store = uu_create_store() -- Create a new store and make it a 
**										son of the current store
**				uu_free_store( store ) -- De-allocate a store
**				ptr = uu_malloc( size ) -- Storage allocator
**				ptr=uu_realloc(ptr,siz) -- reallocate a storage chunk.
**				uu_free( ptr ) -- Free a storage chunk
**				uu_store_scan_init(store,scanblk) -- initialize a scan of store.
**				ptr=uu_store_scan(scanblk) -- visit next chunk of store.
**				uu_alloc_begin() -- Create a son store to the current store 
**									and then make it current
**				uu_alloc_end() -- Free the current store and then make 
**									its parent current
**				uu_alloc_push( store ) -- Push the current store on the stack 
**									and make the given store current
**				uu_alloc_pop() -- Pop the stack, restoring the previous 
**									current store
**	 The following are macros, defined in alloc.h:
**				uu_set_current_store( store ) -- Make a store current
**				store = uu_get_current_store()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       smalloc.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:08
*********************************************************************/

#include "usysdef.h"
#include "ustdio.h"
#include "udebug.h"
#include "uhep.h"
#include "alloc.h"

#define void int

#define NCL_STANDARDMALLOC

/* Definitions:
		"block" is used to describe the storage units allocated by malloc();
		"chunk" is used to describe the storage units requested by 
			the user of this package 
*/

/* macro returning a pointer to the end of a chunk */
#define end_of_chunk(chunk) \
	( (BOUND_T *) ( ((char *) &((chunk)->forw)) + (chunk)->bound.size ) )

/* macro returning a pointer to the end of a block */
#define end_of_block(block) \
	( (BOUND_T *) ( ((char *) (block) + block->first.size +  \
						sizeof(BLOCK_T) - sizeof(BOUND_T) ) ) )

/* macro returning a pointer to the next sequential chunk within a block */
/*
.....Changed to compile correctly on RS6000. Paul. 06/02/92
*/
#define next_chunk(chunk) \
	 ((CHUNK_T *) ( (chunk)->bound.used == USEDB ? (char *)NULL : \
		( (char *)&( (chunk)->forw) + (chunk)->bound.size +sizeof(BOUND_T))))
/*	( (CHUNK_T *) ( (chunk)->bound.used == USEDB ? NULL : \
			( (char *)&( (chunk)->forw) + (chunk)->bound.size +sizeof(BOUND_T))))*/

STORE_T root_store = 	{0	};			/* root of store tree */
STORE_T *uu_current_store = 0;		/* pointer to current store */

static STORE_T *stack[ STACK_MAX ];
static int	 	sp;
#ifndef UU_DEBUGOFF
static int mloopcnt=0;					/* number times thru free list loop */
static int malloccnt=0;					/* number times uu_malloc called */
static int initblkcnt=0;				/* number times init_block called */
static int fblockcnt=0;					/* number times free_block  called */
#endif

char	*uu_malloc();
char	*malloc();
static int uu_alloc_usecnt=0;       /* number of other sub-systems using this*/

int		valid_store();
static void		init_store();
static int		init_block();
static void		free_blocks();
static void		remove_from_parent();
static void		alloc_error();
static void		init_chunk();
static void		add_chunk();
static void		remove_chunk();
static void		used_chunk();


/*********************************************************************
**    E_FUNCTION     : int uu_alloc_init()
**       initialize storage management
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 0 if all went OK.
**    SIDE EFFECTS : the root store is created and made current, 
**								the stack is cleared
**    WARNINGS     : none
*********************************************************************/

int uu_alloc_init()
{
	uu_denter(UU_UITRC,(us,"uu_alloc_init() usecnt=%d",uu_alloc_usecnt));
	if (uu_alloc_usecnt==0)
	{        /* if this is 1st init */
		/* create the root store and make it current */
		init_store( &root_store, UU_NULL );
		uu_current_store = &root_store;

		/* clear the stack */
		sp = 0;
	}
	uu_alloc_usecnt++;				/* bump use counter */
	uu_dexit;
   return(0);
}
 
/*********************************************************************
**    E_FUNCTION :  int uu_alloc_term() -- terminate storage manager.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : 0 if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_alloc_term()
{
   uu_denter(UU_UITRC,(us,"uu_alloc_term() usecnt=%d",uu_alloc_usecnt));
   /* if (uu_alloc_usecnt==1) * here should free the root_store */
   if (uu_alloc_usecnt>0) uu_alloc_usecnt--;
   uu_dexit;
   return(0);
}


/*********************************************************************
**    E_FUNCTION     : STORE_T * uu_create_store()
**       Create a new store and make it a son of the current store
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : pointer to new store
**    SIDE EFFECTS : new store is created
**    WARNINGS     : none
*********************************************************************/

STORE_T *
uu_create_store()
{
	STORE_T				*store;

	uu_denter(UU_UITRC,(us,"uu_create_store()"));

	/* allocate a store */
	store = (STORE_T *) uu_malloc( sizeof( STORE_T ) );
	uu_dprint(UU_UITRC,(us,"uu_current_store=%x; created_store=%x",
		uu_current_store, store));

	/* initialize the new store */
	init_store( store, uu_current_store );

	uu_dexit;
	return( store );
}


/*********************************************************************
**    E_FUNCTION     : void uu_free_store( store )
**       De-allocate a store
**    PARAMETERS   
**       INPUT  : 
**          store			pointer to store (from uu_alloc_create())
**       OUTPUT :  
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : store is de-allocated
**    WARNINGS     : the store cannot be reused
*********************************************************************/

void
uu_free_store( store )
	STORE_T			*store;

{
	uu_denter(UU_UITRC,(us,"uu_free_store(store=%x)", store));

	/* if the store is a valid store ... */
	if( valid_store( store ) )
		{
		STORE_T			*son = store->son;

		/* if this store is the root store ... */
		if( store == &root_store )
			{
			alloc_error( ERR_FREE_ROOT );
			goto done;
			}

		/* if this store is the current store ... */
		if( store == uu_current_store )
			{
			alloc_error( ERR_FREE_CURRENT );
			goto done;
			}

		/* free all the son stores */
		while( son )
			{
			uu_free_store( son );
			son = son->sibling;
			}

		/* remove the given store from its parent's sons */
		remove_from_parent( store );

		/* free the given store's blocks */
		free_blocks( store );
		uu_free( store );
		}
done:;
	uu_dexit;
	return (0);
}


/*********************************************************************
**    E_FUNCTION     : char * uu_malloc( size )
**       Storage allocator
**    PARAMETERS   
**       INPUT  : 
**          size		number of bytes of storage needed
**       OUTPUT :  
**          none
**    RETURNS      : pointer to storage
**    SIDE EFFECTS : storage is removed from block
**    WARNINGS     : none
*********************************************************************/
char * uu_malloc( size )
int				size;
{
	char				*start_addr;
#ifdef NCL_STANDARDMALLOC
	if (size <1) start_addr = NULL;
   else start_addr = malloc(size);
#else
	CHUNK_T			*free;
	CHUNK_T			*new;
	int				status;			/* return code from init_block */
	int				left;				/* space left after allocation */

/* NCL - increase MAXMALLOC */
/* Incresed so MAX_PARA_REC can be increased to such a value*/
#define MAXMALLOC 20000000
	uu_denter(UU_UITRC,(us,"uu_malloc(size=%d)",size));
	if ((size<1)||(size>MAXMALLOC)) 
	{ 
		uu_dexit;
		return(NULL);
	}
	if (size < MIN_CHUNK)
		size = MIN_CHUNK;
#ifndef UU_DEBUGOFF
	malloccnt++;				/* for debugging */
#endif

	/* adjust size to be word multiple */
	size = (size + sizeof(UU_REAL) - 1) & ~(sizeof(UU_REAL) - 1);


	/* if the free list is not empty ... */
	/*uu_dprint(UU_UITRC,(us,"uu_malloc:free:%x",uu_current_store->free));*/
	if( free = uu_current_store->free )
	{
		do
		{
			left = free->bound.size - size;

			/*uu_dprint(UU_UITRC,(us,"uu_malloc:left=%d",left));*/
#ifndef UU_DEBUGOFF
			if( left > MAXMALLOC )
			{
				/* shouldn't be anything this big - store probably corrupt */
				uu_dprint(-1,(us,"ERROR: store corrupt"));
			}
			else
#endif
			if( left >= 0 )
			{
				/* if the chunk is big enough to be split ... */
				if (left >= MIN_CHUNK)
				{
					/* get pointer to the chunk to give to the caller */
					new = (CHUNK_T *) 
						( (char *)free + left );
					
					/* setup new chunk and used chunk */
					init_chunk( new, size, USED );
					init_chunk( 
						free, 
						(char *)new - (char *)free - 2*sizeof(BOUND_T), 
						FREE 
					);

					/* return a pointer to the data */
					/*uu_dprint(UU_UITRC,(us,
						"uu_current_store=%x; alloc split %d byte chunk=%x",
						uu_current_store, size, &new->forw));*/
					start_addr = (char *) &new->forw;
					goto theExit;
				}
				else
				{
					/* remove the chunk from the free list */
					remove_chunk( free );
			
					/* mark the chunk as used */
					used_chunk( free );

					/* return a pointer to the data */
					/*uu_dprint(UU_UITRC,(us,
						"uu_current_store=%x; alloc entire %d byte chunk=%x",
						uu_current_store, free->bound.size, &free->forw));*/
					start_addr = (char *) &free->forw;
					goto theExit;
				}
				}

			/* advance to next chunk on the list */
			free = free->forw;
#ifndef UU_DEBUGOFF
			mloopcnt++;				/* bump loop counter */
#endif
			/*uu_dprint(UU_UITRC,(us,"uu_malloc:free:%x", free));*/
			}
		while( free != uu_current_store->free );
		}

	/* no space on free list - allocate a block */
	status = init_block( size );
	if (status == 0)
	{
		/* try again */
		start_addr = uu_malloc(size);
	}
	else
	{
		start_addr = NULL;	/* fail */
	}

theExit:;
	uu_dprint(UU_UITRC,(us,"uu_malloc returning start addr=%x",start_addr));

	uu_dexit;
#endif /* else NCL_STANDARDMALLOC */
	return(start_addr);
}


/*********************************************************************
**    I_FUNCTION :  char *uu_realloc(chunk_data,siz)
**        re-size chunk_data to siz. Siz must be smaller than original size.
**			 This function was never used or tested.
**    PARAMETERS   
**       INPUT  : 	char *chunk_data -- pointer to a malloc'd chunk.
**							int siz -- new size.
**       OUTPUT :  
**    RETURNS      : chunk_data if all went OK, else NULL. NULL usually means
**							siz was bigger than original size.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*char *uu_realloc(chunk_data,siz)
char *chunk_data;
int siz;					 new size. Must be smaller than original size *
{	
	CHUNK_T *chunk=(CHUNK_T *)(chunk_data - sizeof(BOUND_T));
	BOUND_T *chunk_start = (BOUND_T *)chunk;
	BOUND_T *chunk_end;		* end of chunk being realloc'd *
	BOUND_T *new_chunk;		* newly split off free chunk *
	BOUND_T *new_chunk_end;	* end of newly split off free chunk *
	int oldsiz;					* original size of chunk_data *
	int freesiz;				* size of new_chunk's data *
	char *newptr;				* pointer to data area in new_chunk *

	uu_denter(UU_UITRC,("uu_realloc(%x,%d)",chunk_data,siz));
	oldsiz=chunk_start->size;
	if (oldsiz<=MAXMALLOC) {	* do nothing if we don't believe it *
		freesiz=oldsiz-siz-2*sizeof(BOUND_T);
		if ((freesiz)>=MIN_CHUNK) { 	* if leftover is big enough to split *
			chunk_end = (BOUND_T *)(chunk_data + chunk_start->size);
			new_chunk = chunk_start + 1;
			* adjust size of existing malloc'd piece *
			chunk_start->size=siz;
			chunk_end->size=siz;
			* initialize 2nd piece as USED and then free it *
			* cant use init_chunk(new_chunk,freesiz,USED) since forw not set*
			new_chunk->used=USED;
			new_chunk->size=freesiz;
			newptr= ((char *)(new_chunk)) + sizeof(BOUND_T);
			new_chunk_end=(BOUND_T *)(newptr+freesiz);
			new_chunk_end->used=USED;
			new_chunk_end->size=freesiz;
			uu_free(newptr);				* free the newly split chunk *
		}
	}
	uu_dexit;
}*/

/*********************************************************************
**    E_FUNCTION     : void uu_free( chunk_data )
**       Free a storage chunk
**    PARAMETERS   
**       INPUT  : 
**          chunk_data		pointer to data section of storage chunk 
**									as allocated by uu_malloc()
**       OUTPUT :  
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : the chunk is returned to the free list
**    WARNINGS     : the current store must be the store from 
**							which the chunk was allocated
*********************************************************************/

void
uu_free( chunk_data )
char			*chunk_data;
{
#ifdef NCL_STANDARDMALLOC
   free(chunk_data);
#else
	CHUNK_T		*chunk = (CHUNK_T *) (chunk_data - sizeof( BOUND_T ));

	/* pointers to the start & end of the before chunk, 
		the given chunk, and the after chunk */
	BOUND_T		*chunk_start = (BOUND_T *)chunk;
	BOUND_T		*chunk_end;
	BOUND_T		*bchunk_start;
	BOUND_T		*bchunk_end = chunk_start - 1;
	BOUND_T		*achunk_start;
	/*BOUND_T		*achunk_end;*/

	uu_denter(UU_UITRC,(us,"uu_free(ptr=%x)",chunk_data));

	/* get pointer to the end of the given chunk */
	chunk_end = (BOUND_T *) (chunk_data + chunk_start->size);
	uu_dprint(UU_UITRC,(us,"uu_current_store=%x; free %d bytes from  %x",
		uu_current_store, chunk_start->size, chunk_start));
	if (chunk_start->size <= MAXMALLOC)	/* dont free if we dont believe it */
	{
		if (chunk_start->size == chunk_end->size)	/* see if lengths differ */
		{
			/* get pointer to the start of the before chunk */
			bchunk_start = (BOUND_T *) 
				( (char *) bchunk_end - bchunk_end->size - sizeof(BOUND_T) );
			/* if chunk before given chunk is free ... */
			if( bchunk_end->used == FREE )
			{
				/* coalesce with before chunk */
				uu_dprint(UU_UITRC,(us,"combine with chunk at 0x%x",bchunk_start));

				/* change size of coalesced chunk */
				bchunk_start->size += chunk_start->size + 2 * sizeof(BOUND_T);
				chunk_end->used = FREE;
				chunk_end->size = bchunk_start->size;
	
				/* given chunk now becomes coalesced chunks */
				chunk_start = bchunk_start;
				bchunk_end = chunk_start - 1;
			}
			else
			{
				add_chunk( chunk_start );	/* add block to free list */
			}
	
			/* get pointer to after-chunk start */
			achunk_start = chunk_end + 1;
	
			/* if after-chunk is free ... */
			if( achunk_start->used == FREE )
			{
				CHUNK_T		*achunk = (CHUNK_T *) achunk_start;
				uu_dprint(UU_UITRC,(us,"combine with chunk at 0x%x",achunk_start));

				/* remove the after-chunk from the free list */
				remove_chunk( achunk );
	
				/* adjust size of chunk */
				init_chunk( 
					chunk_start, 
					chunk_start->size + achunk_start->size + 2 * sizeof( BOUND_T ), 
					FREE 
				);
				chunk_end = (BOUND_T *) ((char *)chunk_start + sizeof(BOUND_T)
						+ chunk_start->size);
				achunk_start = chunk_end + 1;
			}
			if ((bchunk_end->used==USEDB)&&(achunk_start->used==USEDB))
			{
				/* neighbors of this chunk are ends of block. Free the block. */
				free_block(((char *)(chunk_start))-sizeof(BOUND_T)-2*sizeof(int *));
			}
		}	/* end lengths don't differ */
		else
		{
			uu_dprint(-1,(us,"ERROR:uu_free block size mismatch"));
		}
	}	/* end length<=MAXMALLOC */
	else
	{
		uu_dprint(-1,(us,"ERROR:uu_free illegal block size"));
	}
	uu_dexit;
#endif /* else NCL_STANDARDMALLOC */
	return (0);
}

/*********************************************************************
**    E_FUNCTION :  uu_store_scan_init(store,scanblk) -- initialize a scan 
**							of store.
**    PARAMETERS   
**       INPUT  : 	STORE_T *store; -- the store to be scanned.
**							STORE_SCAN *scanblk; -- a pointer to a data area used 
**											by uu_store_scan. Caller should not modify
**											this area.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_store_scan_init(store,scanblk)
STORE_T		*store;					/* store to be scanned */
STORE_SCAN	*scanblk;				/* pointer to scan data area */
{
	uu_denter(UU_UITRC,(us,"uu_store_scan_init(0x%x,0x%x)",store,scanblk));
	scanblk->block=store->block;	/* pointer to 1st block in store */
	scanblk->chunk=(CHUNK_T *)&(scanblk->block->first);
	uu_dexit;
	return(0);
}

/*********************************************************************
**    E_FUNCTION :  char *uu_store_scan(scanblk) -- visit next chunk of store.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char *uu_store_scan(scanblk)
STORE_SCAN *scanblk;
{
	uu_denter(UU_UITRC,(us,"uu_store_scan_init(0x%x)",scanblk));
	if (scanblk->chunk == (CHUNK_T *)&(scanblk->block->first))
	{
		scanblk->chunk = (CHUNK_T *)( (char *)scanblk->chunk + sizeof(BOUND_T));
	}
	else
	{
		scanblk->chunk = next_chunk(scanblk->chunk);
	}
	while (scanblk->chunk && scanblk->chunk->bound.used != USED)
	{
		if (scanblk->chunk->bound.used == USEDB)
		{
			/* next block */
			scanblk->block = scanblk->block->next;
			if (scanblk->block)
			{
				scanblk->chunk = (CHUNK_T *)&(scanblk->block->data[0]);
			}
			else
			{
				scanblk->chunk = (CHUNK_T *)NULL;
			}
		}
		else
		{
			scanblk->chunk = next_chunk(scanblk->chunk);
		}
	}
	uu_dexit;
/*
.....Changed to compile correctly on RS6000. Paul. 06/02/92
....Added (char *) before NULL and before &(scanblk->...
*/
	return( (char *)((scanblk->chunk == 0)? (char *)NULL : (char *) &(scanblk->chunk->forw)));
}

/*********************************************************************
**    E_FUNCTION     : void uu_alloc_begin()
**       Create a son store to the current store and then make it current
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : the current store is set to the new store
**    WARNINGS     : none
*********************************************************************/

void
uu_alloc_begin()
	{
	uu_denter(UU_UITRC,(us,"uu_alloc_begin()"));
	uu_set_current_store( uu_create_store() );
	uu_dexit;
	return (0);
	}


/*********************************************************************
**    E_FUNCTION     : void uu_alloc_end()
**       Free the current store and then make its parent current
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : the parent of the current store is made the 
**							new current store
**    WARNINGS     : cannot be done if the current store is the root
*********************************************************************/

void
uu_alloc_end()
	{
	STORE_T			*store = uu_current_store;

	uu_denter(UU_UITRC,(us,"uu_alloc_end()"));
	uu_current_store = store->parent;
	uu_free_store( store );
	uu_dexit;
	return(0);
	}

/*********************************************************************
**    E_FUNCTION     : void uu_alloc_push( store )
**       Push the current store on the stack and make the given store current
**    PARAMETERS   
**       INPUT  : 
**          store			pointer to the store to be made current
**       OUTPUT :  
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : the current store is pushed on the stack
**    WARNINGS     : none
*********************************************************************/

void
uu_alloc_push( store )
	STORE_T			*store;
	{
	uu_denter(UU_UITRC,(us,"uu_alloc_push(new store=%x). sp=%d",store,sp));
	/* if store is a valid store ... */
	if( valid_store( store ) )
		{
		/* if stack is not full ... */
		if( sp < STACK_MAX )
			{
			stack[ sp++ ] = uu_current_store;
			uu_current_store = store;
			}
		else
			alloc_error( ERR_STACK_OVERFLOW );
		}
	uu_dprint(UU_UITRC,(us,"uu_alloc_push returns. sp=%d",sp));

	uu_dexit;
return (0);
	}


/*********************************************************************
**    E_FUNCTION     : void uu_alloc_pop()
**       Pop the stack, restoring the previous current store
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : the current store is restored
**    WARNINGS     : none
*********************************************************************/

void
uu_alloc_pop()
	{
	uu_denter(UU_UITRC,(us,"uu_alloc_pop(). sp=%d",sp));
	/* if stack not empty ... */
	if( sp > 0 )
		uu_current_store = stack[ --sp ];
	else
		alloc_error( ERR_STACK_UNDERFLOW );
	uu_dexit;
	return(0);
	}


/* Local Functions */
/* --------------- */

/*********************** initialize a store **************************/
static void
init_store( store, parent )
STORE_T		*store;			/* pointer to the store to be initialized */
STORE_T		*parent;			/* pointer to the parent store */
{
	uu_denter(UU_UITRC,(us,"init_store(store=0x%x, parent=0x%x)",store,parent));
	/* security code is an unusual bit-pattern checked by valid_store() */
	store->security =  SECURITY_CODE;

	/* install the new store as a son of the parent */
	store->parent = parent;
	store->son = 0;

	/* if the parent is given ... */
	if( parent )
	{
		store->sibling = parent->son;
		parent->son = store;
	}

	/* clear pointers to block chain and chunk free list */
	store->block = 0;
	store->free = 0;
	uu_dexit;
	return (0);
}

/* ------------------------ */
/****************** handle allocation errors ********************/
static void
alloc_error( err_num )
int			err_num;
{
	/*char us[80];*/
	switch( err_num )
	{
		case ERR_FREE_CURRENT:
			uu_dprint(UU_UITRC,(us, "ERROR:Attempt to free Current Store." ));
			puts( "ERROR:Attempt to free Current Store." );
			break;

		case ERR_FREE_ROOT:
			uu_dprint(UU_UITRC,(us, "ERROR:Attempt to free Root Store." ));
			puts( "ERROR:Attempt to free Root Store." );
			break;

		case ERR_STACK_OVERFLOW:
			uu_dprint(UU_UITRC,(us, "ERROR:Store stack overflow." ));
			puts( "ERROR:Store stack overflow." );
			break;

		case ERR_STACK_UNDERFLOW:
			uu_dprint(UU_UITRC,(us, "ERROR:Store stack underflow." ));
			puts( "ERROR:Store stack underflow." );
			break;

		case ERR_INVALID_STORE:
			uu_dprint(UU_UITRC,(us, "ERROR:Store argument is invalid." ));
			puts( "ERROR:Store argument is invalid." );
			break;
	}
	/* dump the current store */
	uu_print_store(uu_current_store, 1);
	uu_print_store(uu_current_store, 2);
	return(0);
}

/* ----------------------------- */
/**************** validate a pointer to a store *********************/
int						/* returns TRUE if store is valid */
valid_store( store )
STORE_T		*store;
{
	if( store->security != SECURITY_CODE )
	{
		alloc_error( ERR_INVALID_STORE );
		return( UU_FALSE );
	}
	else
		return( UU_TRUE );
}

/************** remove a chunk from the current store's free list *************/
static void
remove_chunk( chunk )
CHUNK_T		*chunk;
{
	uu_denter(UU_UITRC,(us,"remove_chunk(0x%x)", chunk));
	/* if the free list starts at the chunk to be free'd ... */
	if( uu_current_store->free == chunk )
	{
		/* move the free pointer back one chunk */
		uu_current_store->free = uu_current_store->free->back;

		/* if the free list still starts at the chunk to be free'd ... */
		if( uu_current_store->free == chunk )
		{
			/* chunk to be free'd must be the only chunk; empty the free list */
			uu_current_store->free = 0;
			uu_dexit;
			return(0);
		}
	}

	/* adjust links to release the chunk */
	chunk->back->forw = chunk->forw;
	chunk->forw->back = chunk->back;
	uu_dexit;
return (0);
}

/*************** initialize a chunk given its size and status **************/
static void
init_chunk( chunk, size, status )
	CHUNK_T		*chunk;
	int			size;			/* number of bytes of data storage */
	int			status;		/* USED or FREE or USEDB */
{
	uu_denter(UU_UITRC,(us,"init_chunk(0x%x, size=0x%x, status=%d)",
						chunk, size, status));
	/* insert the size and status at the start of the chunk */
	chunk->bound.size = size;
	chunk->bound.used = status;
	{
		BOUND_T		*chunk_end = end_of_chunk(chunk);

		/* insert the size and status at the end of the chunk */
		chunk_end->size = size;
		chunk_end->used = status;
	}
	uu_dexit;
	return (0);
}

/************* add a chunk to the current store's free list *************/
static void
add_chunk( chunk )
	CHUNK_T			*chunk;
{
	CHUNK_T			*free_chunk;
	BOUND_T			*chunk_end = end_of_chunk(chunk);

	uu_denter(UU_UITRC,(us,"add_chunk(0x%x)",chunk));

	/* set "USED" flags */
	chunk->bound.used = FREE;
	chunk_end->used = FREE;

	/* if the free list is not empty ... */
	if( free_chunk = uu_current_store->free )
	{
		free_chunk->back->forw = chunk;
		chunk->back = free_chunk->back;
		chunk->forw = free_chunk;
		free_chunk->back = chunk;
		/*uu_current_store->free=chunk;	 put new at beginning of free list*/
	}
	else
	{
		chunk->forw = chunk->back = uu_current_store->free = chunk;
	}
	uu_dexit;
	return (0);
}

/******************** mark a chunk as used ********************/
static void
used_chunk( chunk )
CHUNK_T			*chunk;
{
	BOUND_T			*chunk_end = end_of_chunk(chunk);

	/* set "USED" flags */
	chunk->bound.used = USED;
	chunk_end->used = USED;
	return (0);
}

/************* get & initialize a block *********************/
static int
init_block( size )
int			size;		/* number of bytes required */
{
	/*BLOCK_T		*prev_block;*/
	BLOCK_T		*block;
	/*BLOCK_T		*next_block;*/
	BLOCK_T		*old_first_block;
	BOUND_T		*block_end;
	/*CHUNK_T		*fst_chunk;*/
	char us[120];

	uu_denter2(UU_UITRC,(us,"init_block(%d)", size));
#ifndef UU_DEBUGOFF
	initblkcnt++;				/* DEBUG bump number times init_block called */
#endif
	/* allocate at least the standard block size */
	if( size < STBLKSIZE ) size = STBLKSIZE;

	/* allocate the new block */
	block = (BLOCK_T *) malloc( sizeof( BLOCK_T ) + size );
	if (block==NULL)
	{
		uu_dprint(-1,(us,"ERROR:init_block. malloc(%d) out of memory",
			sizeof(BLOCK_T)+size));
		/* call user out of memory space recovery */
		uu_sys_err_recovery(-1, UU_SIGNON, 11);
		/* must exit ptr expected below is bad */
		uu_dexit;
		return(-1);		/* return failure */
	}
	uu_dprint(UU_UITRC,(us,"uu_current_store=%x; sysmalloc block=%x of %d bytes",
		uu_current_store, block, size));

	/* add the block to the head of the current store's block list */
	old_first_block = uu_current_store->block;
	if (old_first_block!=0) old_first_block->prev=block;
	block->next = old_first_block;
	block->prev = 0;
	uu_current_store->block = block;

	/* fill in the beginning and end bounds with a USEDB status so
		that the free algorithm never attempts to coalesce chunks
		past the ends of the block, & so the free algorithm can tell if the
		whole block is free */
	block->first.size = size;
	block->first.used = USEDB;
	block_end = end_of_block(block);
	block_end->size = size;
	block_end->used = USEDB;

	/************** KLUDGE. The following sprintf needed to overcome **********/
	/************** a Masscomp compiler bug ************/
	sprintf(us,"size=%d",size);
	/* initialize one big chunk as the block's data */
	init_chunk((CHUNK_T *)block->data, size, FREE );

	/* add the chunk to the current store's free list */
	add_chunk((CHUNK_T *)block->data );
	uu_dexit;
	return(0);		/* return success */
}

/***************** free a specified block *********************/
free_block(block)
BLOCK_T *block;
{
	BLOCK_T *next_block, *prev_block;

	uu_denter(UU_UITRC,(us,"free_block(%x) uu_current_store=%x",
		block,uu_current_store));
	next_block = block->next;
	prev_block = block->prev;
	if (prev_block!=0) prev_block->next = next_block;
	else uu_current_store->block=next_block; 	/* block is 1st block in store */
	if (next_block!=0) next_block->prev = prev_block;
 	/* remove the chunk from the free list */
	remove_chunk((CHUNK_T *)block->data);
	free(block);
#ifndef UU_DEBUGOFF
	fblockcnt++;				/* times thru free block loop */
#endif
	uu_dexit;
	return(0);
}

/**************** free all blocks in the store ******************/
static void
free_blocks( store )
STORE_T		*store;
{
	/*char 			us[60];*/
	BLOCK_T		*block = (BLOCK_T *) store->block;
/*
.....WinNT
*/
	BLOCK_T		*nxt;
	uu_dprint(UU_UITRC,(us,"free_blocks(store=0x%x)", store));
	while( block )
	{
		uu_dprint(UU_UITRC,(us,"uu_current_store=%x; sysfree block %x",
			uu_current_store,block));
		nxt = block->next;
		free( block );
		block = nxt;
	}
	return(0);
}

/************** remove a store from its parent's son chain ****************/
static void
remove_from_parent( store )
STORE_T			*store;
{
	STORE_T			*parent = store->parent;

	/* if the store has a parent ... */
	if( parent )
	{
		STORE_T			*sibling = parent->son;

		/* if the store is the parent's first son ... */
		if( sibling == store )
		{
			parent->son = store->sibling;
		}
		else
		{
			/* while there is another sibling ... */
			while( sibling )
			{
				/* if the sibling's sibling is the given store ... */
				if( sibling->sibling == store )
				{
					/* link past the given store */
					sibling->sibling = store->sibling;
					break;
				}
				else
				{
					/* try next sibling */
					sibling = sibling->sibling;
				}
			}
		}
	}
	return (0);
}

/******************** useful routine for debug purposes *******************/
/********************* print a block **********************/
static void
uu_print_block( block,flag)
	BLOCK_T		*block;
	int flag;				/* 0=don't print chunks in block, else print them */
	{
	BOUND_T		*block_end;
	BOUND_T	  *chunk;
	BOUND_T		*blocklast;
	/*char us[128];*/
	char s[80];
	int slen,siz;

	block_end = end_of_block(block);

	uu_dprint(UU_UITRC,(us,"block (start=%x, used=%d, size=%d",
		block, block->first.used, block->first.size));
	uu_dprint(UU_UITRC,(us,"         end=%x, used=%d, size=%d",
		block_end, block_end->used, block_end->size));
	uu_dprint(UU_UITRC,(us,"        next=%x)",
		block->next));
	if (flag)
	{
		/* print out the chunks in the block */
		chunk= (BOUND_T *)block->data;
		strcpy(s,"chunk sizes in block: ");
		slen=strlen(s);
		blocklast=(BOUND_T *)((char *)(chunk)+block->first.size);
		siz=block->first.size;
		while (chunk<blocklast)
		{
			siz=chunk->size;
			if (chunk->used==FREE) sprintf(&s[slen],"(%d) ",siz);
			else sprintf(&s[slen],"%d ",chunk->size);
			slen=strlen(s);
			if (slen>64)
			{
				uu_dprint(UU_UITRC,(us,"%s",s));
				slen=0; s[0]=0;
			}
			chunk=(BOUND_T *)((char *)(chunk+2)+chunk->size); /* next chunk */
		}
		if (strlen(s)>0)
		{ uu_dprint(UU_UITRC,(us,"%s",s));};
	}								/* end if (flag) */
	return (0);
}

/********************** print a chunk **************************/
static void
uu_print_chunk( chunk )
	CHUNK_T		*chunk;
	{
	BOUND_T		*chunk_end;
	/*char us[128];*/

	chunk_end = end_of_chunk(chunk);

	uu_dprint(UU_UITRC,(us,"chunk (start=%x, used=%d, size=%d",
		chunk, chunk->bound.used, chunk->bound.size));
	uu_dprint(UU_UITRC,(us,"         end=%x, used=%d, size=%d",
		chunk_end, chunk_end->used, chunk_end->size));
	uu_dprint(UU_UITRC,(us,"         forw=%x, back=%x)",
		chunk->forw, chunk->back));
	return (0);

	}

/*********************** print a store **************************/
void uu_print_store( store , option)
	STORE_T		*store;
	int			option;

	{
	BLOCK_T *block;
	CHUNK_T *chunk;
	/*char us[128];*/
	int size;

	uu_dprint(UU_UITRC,(us,"store (start=%x)",store));
#ifndef UU_DEBUGOFF
	uu_dprint(UU_UITRC,(us,"free list loop count=%d uu_malloc count=%d",
		mloopcnt,malloccnt));
	uu_dprint(UU_UITRC,(us," init_block calls=%d free block calls=%d",
		initblkcnt,fblockcnt));
	mloopcnt=0; malloccnt=0;
	initblkcnt=0; fblockcnt=0;
#endif
	if (option == 1)
		{
		size = 0;
		uu_dprint(UU_UITRC,(us,"block list"));
		block = store->block;
		while (block != 0)
			{
			size = size + block->first.size;
			uu_print_block(block,0);
			block = block->next;
			}
		uu_dprint(UU_UITRC,(us,"total size of blocks on store = %d",size));
		}

	if (option == 2) 
		{
		uu_dprint(UU_UITRC,(us,"free list"));
		chunk = store->free;
		size = 0;
		while (chunk != 0)
			{
			size = size + chunk->bound.size;
			uu_print_chunk(chunk);
			chunk = chunk->forw;
			if (chunk == store->free) break;
			}
		uu_dprint(UU_UITRC,(us,"total size of free space on store = %d",size));
		}

	uu_dprint(UU_UITRC,(us,"end of store"));
	return(0);

	}

/***************** UU_check_all_stores() ********************/
uu_check_all_stores()
{
	STORE_T	*chk_store;	/* variable to contain the store being examined */

	uu_denter(UU_UITRC,(us,"uu_check_all_stores()"));
	chk_store = uu_get_current_store();
	while (chk_store)	/* check current and all it derived from */
	{
		valid_store(chk_store);
		chk_store = chk_store->parent;
	}
	uu_dexit;
	return 0;
}
/*
.....Dummy routine for OpenNCL
*/
void uu_dump_memalloc() 
{
	return (0);
}

