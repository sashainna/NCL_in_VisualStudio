/*********************************************************************
**    NAME         :  utlaloc.c -- tool storage malloc.
**       CONTAINS:
**		  int uu_toolmalloc_init() -- initialize tool storage manager.
**		  int uu_toolmalloc_term() -- terminate tool storage manager.
**      ptr = uu_toolmalloc(size) -- get unicad tool storage.
**		  uu_toolfree(ptr); -- free unicad tool storage.
**		  uu_appush_store(); -- push a new application store.
**		  uu_appop_store(); -- pop an application store.
**
**		Usage: When unicad starts up, it calls uu_toolmalloc_init to create the 
**			root storage area and stores a pointer to it in uu_toolstore. 
**			Unicad tools should call uu_toolmalloc and uu_toolfree to use this 
**			store.
**				When an application starts, the menu traverser (or some code) 
**			should call uu_appush_store() to push current store onto a stack, 
**			create a new store for the application, and make it current.
**			Calls to uu_malloc and uu_free will use this application store.
**			When an application ends, it should call  uu_appop_store()
**			to pop the prev appl store from the stack, make it current,
**			and free this application's store.
**				The only time uu_toolstore is the current store is when there
**			are no applications running. Usually, the most recently started
**			application's store is current. The storage manager's stack and
**			its tree structure of stores should always match. That is, the
**			store stack should always contain exactly the ancestors of the 
**			current store. That will as long as calls to uu_appush_store() and
**			uu_appop_store() are paired up correctly.
**			
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       utlaloc.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:56
*********************************************************************/
#include "usysdef.h"
#include "ualloc.h"
#include "udebug.h"

UU_STORE *uu_toolstore;		/* unicad tools' store */
UU_STORE *uu_create_store();

static uu_toolmalloc_usecnt=0;		/* use counter */
/*********************************************************************
**    E_FUNCTION :  int uu_toolmalloc_init() -- initialize tool storage mgr.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      :  0 if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_toolmalloc_init()
{
	uu_denter(UU_UITRC,(us,"uu_toolmalloc_init(). usecnt=%d",
		uu_toolmalloc_usecnt));
	if (uu_toolmalloc_usecnt==0) {
		/* really initialize the tool storage manager */
		uu_alloc_init();	/* initialize the Unicad memory manager,
									creating the root store for use by Unicad tools */
		/* save the tools' store */
		uu_toolstore=(UU_STORE *)uu_get_current_store();		
	}
	uu_toolmalloc_usecnt++;
	uu_dexit;
	return(0);
}

/*********************************************************************
**    E_FUNCTION :  int uu_toolmalloc_term() -- terminate tool storage manager.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : 0 if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_toolmalloc_term()
{
	uu_denter(UU_UITRC,(us,"uu_toolmalloc_term(). usecnt=%d",
		uu_toolmalloc_usecnt));
	if (uu_toolmalloc_usecnt==1) uu_alloc_term();
	if (uu_toolmalloc_usecnt>0) uu_toolmalloc_usecnt--;
	uu_dexit;
	return(0);
}

/*********************************************************************
**    E_FUNCTION :  char *uu_toolmalloc(size) -- tool malloc storage.
**    PARAMETERS   
**       INPUT  : 	int size; -- number bytes to allocate.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char *uu_toolmalloc(size)
int size;
{
	char *irtn;
	char *uu_malloc();
	int *p,intsize;
	char us[80];
	uu_alloc_push(uu_toolstore);
	intsize= (size+(sizeof(int) -1))/sizeof(int);
#ifdef UU_DEBUGON
	if((intsize<1) || (intsize > 1000000)) {
		uu_dprint(-1,(us,"uu_toolmalloc error size out of range"));
		goto rtn;
	}
	p=(int *)uu_malloc(intsize*sizeof(int));
	/*p=(int *)uu_malloc((intsize+2)*sizeof(int));
	/* p[0]=intsize;
	/* p[intsize+1]=intsize;
	/* irtn=(char *)(&p[1]); */
	irtn=(char *)p;
#else
	irtn=uu_malloc(intsize*sizeof(int));
#endif
	rtn:uu_alloc_pop();
	uu_denter2(UU_UITRC,(us,"%x=uu_toolmalloc(%d)",irtn,size));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  uu_toolfree(ptr) -- tool memory free.
**    PARAMETERS   
**       INPUT  : 	char *ptr; -- memory to be freed.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_toolfree(ptr)
char *ptr;
{
	int *p,intsize;
	uu_denter(UU_UITRC,(us,"uu_toolfree(%x)",ptr));
	uu_alloc_push(uu_toolstore);
#ifdef UU_DEBUGON
	/*p = (int *)ptr;
	/*p--;
	/*intsize=p[0];
	/*if((intsize<0) || (intsize > 1000000)|| (intsize != p[intsize+1]))
		/*{
		/*uu_dprint(-1,(us,"uu_toolfree:Memory Corrupted, exiting"));
		/*uu_dprint(-1,(us,"      sizes = %d %d",p[0],p[intsize+1]));
		/*exit(4);
		/*}
	/*ptr = (char *)p;*/
#endif
	uu_free(ptr);
	uu_alloc_pop();
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  int uu_appush_store(); -- create and push a new appl 
**														storage area.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_appush_store()
{

	int status;

	uu_denter(UU_UITRC,(us,"uu_appush_store()"));
	/* create a new application store (son of current store), push current
		store onto stack, make new store current */
	status = uu_alloc_push(uu_create_store());
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  uu_appop_store() - free and pop application memory area.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_appop_store()
{
	UU_STORE *store;

	uu_denter(UU_UITRC,(us,"uu_appop_store()"));
	/*This will free the current appl's store(and all its sons stores). */
	/* get ptr to current appl store */
	store=(UU_STORE *)uu_get_current_store();	
	uu_alloc_pop(); 							/* pop appl store from stack and make
														it current. */
	uu_free_store(store);					/* free the old appl store (and all
														is sons) */
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : uu_dump_toolstore()
**       Dump the current contents of the tool store.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_dump_toolstore()
	{
	uu_denter(UU_UITRC,(us,"uu_dump_toolstore()"));
	uu_print_store(uu_toolstore,1);
	uu_print_store(uu_toolstore,2);
	uu_dexit;
	}

