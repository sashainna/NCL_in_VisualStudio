/*********************************************************************
**    NAME         :  uhash.c -- open hash table routines.
**		CONTAINS:
**			UU_HASH_TBL t -- declares t to be a open hash table.
**			uu_hash_init(t, n) -- init table t with n buckets.
**			char *uu_hash_get(t, n) -- get pointer to entry with key n in table t.
**			char *uu_hash_add(t, n, len) -- add an entry of len bytes to table t, 
**								with key n. Return pointer to the len bytes.
**			uu_hash_dele(t, n) -- delete entry n from table,  or noop.
**			int uu_hash_len(t, n) -- return length of entry n,  or 0.
**			uu_hash_empty(t) -- make table t empty.
**			uu_hash_del(t) -- delete hash table t.
**			uu_hash_initscan(t) -- initialize for sequential scan of table t.
**			char *uu_hash_scan(t) -- return ptr to next entry for table t.
**
**		IMPLEMENTATION STRATEGY:
**			A hash table consists of an array of pointers to lists.
**			Each list contains all the table entries which hash into the same 
**			thing.  All the pointers in the array start out as UU_NULL.  
**			The lists are supported by the glists.c routines. When the 
**			first element in a list is created, 
**			the list_head pointer is placed in the buckets array. 
**			The first integer in each list entry is the key value. 
**			The second integer is the length (bytes) of the remaining 
**			words containing user data,  as shown below:
**			The address returned by one of the hash_table routines is
**			always the address of the first byte of userdata.
**	 	_________      ______________________
**    | 0     |----->|key |len | userdata |------>etc.
**    |       |      |____|____|__________|
**    |_______|      _____________________
**    | 1     |----->|key |len | userdata |----->etc.
**    |	     |		|____|____|__________|
**    |_______|
**			.
**			.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uhash.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:53
*********************************************************************/

#include "usysdef.h"
#include "uhash.h"
#include "udebug.h"

char *uu_toolmalloc(), *uu_lsnew(), *uu_lsnext();

#define uu_hash(t, n) n&(t[0].nbuckets-1)

/*********************************************************************
**    E_FUNCTION :  uu_hash_init(t, n) 
**			init hash table t for n buckets.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_hash_init(t, n)					/* init hash table t for n buckets */
UU_HASH_TBL t;
int n;									/* will be rounded up to a power of 2 */
{
	int m, i;
	char *p;
	char us[100];

	m = 1;

/* -- now m is a power of 2 >= n -- */

	while(m<n)
		m= m<<1;
	t[0].nbuckets = m;

	uu_dprint(UU_U1TRC, (us, "uu_hash_init(%x, %d) nbuckets=%d", t, n, m));

/* -- allocate and initialize bucket table -- */

	t[0].buckets = (char **)uu_toolmalloc(m*sizeof(p));
	for(i=0; i<m; i++)
		(t[0].buckets)[i] = UU_NULL;
}

/*********************************************************************
**    E_FUNCTION :  char *uu_hash_get(t, n) -- get entry n from table t.
**								Return pointer to it,  or UU_NULL.
**    PARAMETERS   
**       INPUT  :  UU_HASH_TBL t -- table to look in.
**						 int n -- key of table entry.
**       OUTPUT :   none
**    RETURNS      : pointer to entry n,  or UU_NULL.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_hash_get(t, n)	/* return pointer to entry n from table t, or UU_NULL */
UU_HASH_TBL t;				/* hash table to look in */
int n;						/* key of table entry to look for */
{
	int i;
	char **p;
	char *irtn;
	int *lisptr;

	uu_denter(UU_U1TRC, (us, "uu_hash_get(tbl:%x, key:%d)", t, n));

	irtn = UU_NULL;

/* -- hash the key and get the list pointer for this hashed value -- */

	i = uu_hash(t, n);
   lisptr = UU_NULL;
   if ((unsigned long)t[0].buckets > 0) lisptr = (int *)(t[0].buckets)[i];

/* -- if there exists a list for this hash value -- */

	if(lisptr != UU_NULL)
	{

/*		-- while more list elements -- */

		while((lisptr=(int *)uu_lsnext(lisptr)) != UU_NULL)
		{

/* 		-- found it: return pointer to userdata -- */

			if(lisptr[0] == n) 
			{
				irtn = (char *)&(lisptr[2]);
				break;	
			}
			(t[0].scanbucket)++;
		}
	}
	uu_dprint(UU_U1TRC, (us, "RETURNS %x=uu_hash_get(%x, %d)buckets[%d]=%x=list hd", 
		irtn, t, n, i, (t[0].buckets)[i]));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  char *uu_hash_add(t, n, len) -- add entry.
**		This function adds an entry to the hash table "t". A pointer to
**		the data area is returned where the new data can be stored.
**		If an entry with key,  "n",  already exists,  then it is deleted
**		and a new entry is created.
**    PARAMETERS   
**       INPUT  :  UU_HASH_TBL t -- table to add to.
**						int n -- key of new entry.
**						int len -- length (bytes) of new entry.
**       OUTPUT :   none
**    RETURNS      : pointer to new entry of len bytes,  or UU_NULL if no memory.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_hash_add(t, n, len)
UU_HASH_TBL t;					/* hash table */
int n;							/* key */
int len;							/* length (bytes) of new entry */
{
	char *irtn;
	int i;
	int *eltptr;

	uu_denter(UU_U1TRC, (us, "uu_hash_add(%x, %d, %d)", t, n, len));

	irtn = UU_NULL;

/* -- n hashes into i -- */

	i = uu_hash(t, n);

/* -- list doesn't exist. create one.
		create a new list, get listhead ptr into buckets[i] -- */

	if((int *)(t[0].buckets)[i] == UU_NULL) 
	{
		(t[0].buckets)[i] = uu_lsnew();	

/* 	-- out of memory -- */

		if((t[0].buckets)[i] == UU_NULL)
			goto rtn;
	}

	uu_dprint(UU_U1TRC, (us, "key:%d, bucket:%d, list ptr in bucket:%x", 
										n, i, (t[0].buckets)[i]));

/* -- see if entry exists already, if so delete it -- */

	irtn = uu_hash_get(t, n);
	if(irtn != UU_NULL)
		uu_lsdele(irtn-2*sizeof(int));

/* -- add list elt at beginning of list -- */

	eltptr = (int *)uu_lsinsrt((t[0].buckets)[i], len+2*sizeof(int));
	eltptr[0] = n; 					/* put in key */
	eltptr[1] = len;					/* put in len of userdata */
	irtn = (char *)&eltptr[2];		/* return ptr to user data */

rtn:
	uu_dprint(UU_U1TRC, (us, "%x=uu_hash_add(%x, %d, %d)", irtn, t, n, len));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  uu_hash_dele(t, n) -- delete entry n in table t.
**    PARAMETERS   
**       INPUT  :  UU_HASH_TBL t -- table.
**						 int n -- key of entry to delete.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_hash_dele(t, n)						/* delete entry n in table t */
UU_HASH_TBL t;							/* hash table */
int n;									/* key of entry to delete */
{
	int *p;

	uu_denter(UU_U1TRC, (us, "uu_hash_dele(%x, %d)", t, n));

	p = (int *)uu_hash_get(t, n);

/* -- entry exists,  delete it -- */

	if(p != UU_NULL) 
	{
		p = p - 2;		/* back up from user data to beginning of list element */
		uu_lsdele(p);	/* delete the list element */
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  int uu_hash_len(t, n) -- return length of entry n.
**    PARAMETERS   
**       INPUT  :  UU_HASH_TBL t -- hash table.
**						 int len -- key of entry whose length is wanted.
**       OUTPUT :  none
**    RETURNS      : lengty (bytes)  of entry n,  or zero.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_hash_len(t, n)			/* return length of entry n, or 0 */
UU_HASH_TBL t;						/* hash table */
int n;								/* key of entry whose length is wanted */
{
	int *p;
	char us[80];
	int irtn;

/* -- look up entry n in table -- */

	p = (int *)uu_hash_get(t, n);

/* -- entry exists -- */

	if(p != UU_NULL) 
	{
		p = p-1;
		irtn = p[0];
	}
	else 
		irtn = 0;

	uu_dprint(UU_U1TRC, (us, "%d=uu_hash_len(%x, %d)", irtn, t, n));
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  uu_hash_empty(t) -- make hash table t empty.
**    PARAMETERS   
**       INPUT  : UU_HASH_TBL t -- hash table to be emptied.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_hash_empty(t)							/* make hash table t empty */
UU_HASH_TBL t;
{
	int m, i;
	char *p;

	uu_denter(UU_U1TRC, (us, "uu_hash_empty(%x)", t));

/* -- number of buckets in table -- */

	m = t[0].nbuckets;
	for(i=0; i<m; i++) 
	{
		p = t[0].buckets[i];

/* 	-- delete list for this bucket -- */

		if(p != UU_NULL) 
			uu_lsdel(p);

/* 	-- remember the list is gone -- */

		t[0].buckets[i] = UU_NULL;
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  uu_hash_del(t) -- delete hash table t entirely.
**    PARAMETERS   
**       INPUT  : 	UU_HASH_TBL t -- table to be deleted.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_hash_del(t)							/* delete hash table t */
UU_HASH_TBL t;
{
	uu_denter(UU_U1TRC, (us, "uu_hash_del(%x)", t));

/* -- first empty the table, then free the bucket array -- */

	uu_hash_empty(t);
	uu_toolfree(t[0].buckets);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  uu_hash_initscan(t) -- initialize scan of table t.
**			Subsequent calls to uu_hash_scan(t) return pointers to each
**			entry in table t.
**    PARAMETERS   
**       INPUT  :  UU_HASH_TBL t; -- hash table to scan.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_hash_initscan(t)
UU_HASH_TBL t;
{
	uu_denter(UU_U1TRC, (us, "uu_hash_initscan(%x)", t));

	t[0].scanbucket =  -1;
	t[0].scanelt = UU_NULL;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  char *uu_hash_scan(t) -- scan table t.
**			return pointer to next entry in table t.
**    PARAMETERS   
**       INPUT  :  UU_HASH_TBL t; -- hash table to scan.
**       OUTPUT : 	
**    RETURNS      : pointer to next entry,  or UU_NULL.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_hash_scan(t)
UU_HASH_TBL t;
{
	char *irtn;
	int *lisptr;
	int i;

	uu_denter(UU_U1TRC, (us, "uu_hash_scan(tbl:%x)", t));

/* -- find a non-empty bucket -- */

	while(t[0].scanelt == UU_NULL) 
	{
		t[0].scanbucket++;

/* 	-- no more buckets. -- */

		if(t[0].scanbucket >= t[0].nbuckets)
			break;
		i = t[0].scanbucket;

/* 	-- get listhead pointer -- */

		lisptr = (int *)(t[0].buckets)[i];
		t[0].scanelt = (char *)lisptr;
		if(t[0].scanelt != UU_NULL)
		{
		uu_dprint(UU_U1TRC,(us, "bucket %d has nonemtpy head, addr of bucket:%x", 
					i, &((t[0].buckets)[i])));

/* 		-- get pointer to first list element -- */

			t[0].scanelt = uu_lsnext(t[0].scanelt);
		}
	}

	if(t[0].scanelt != UU_NULL)  
	{
		lisptr = (int *)t[0].scanelt;
		irtn = (char *)&(lisptr[2]);					/* get ptr to this elt's data */
		t[0].scanelt = uu_lsnext(t[0].scanelt);	/* get ptr to next list elt*/
	}
	else
		irtn = UU_NULL;

	uu_dprint(UU_U1TRC, (us, "%x=uu_hash_scan(%x). bucket=%d,  nbuckets=%d", 
		irtn, t, t[0].scanbucket, t[0].nbuckets));
	uu_dexit;
	return(irtn);
}
