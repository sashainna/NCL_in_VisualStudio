/*********************************************************************
**    NAME    :  ulists.c -- list processing package.
**			These functions operate on doubly linked (circular) lists.
**		The algorithm is taken from Knuth, Vol 1, P. 278. A list		
**		contains zero or more list elements.  A list element may 
**		contain any kind of data. Each element of a list may be 
**		of a different length. All data is kept in main memory.
**       CONTAINS:
**	uu_lsnew -- creates a new (empty) list and returns a pointer to the list.  
**		This pointer is conceptually before the first element of the list.
**		Returns NULL if out of memory.
** uu_lsdel(p) -- deletes the entire list P may point to any element or may be 
**		the pointer returned by uu_lsnew.
**	uu_lsinsrt(p,len) --  returns a pointer to a new list element, inserted 
**		after the list element pointed to by p. If p is the pointer returned 
**		by uu_lsnew, the new element becomes the first element of the list. 
**		The new element length is len bytes. Storage is allocated for the 
**		new element. The caller may now place any data he wishes in the 
**		new element.
**	uu_lsdele(p) --  deletes the list element pointed to by p and returns a 
**		pointer to next element in the list. If p pointed to the last element 
**		in the list, or if the list was empty, NULL  is returned.
** uu_lsempty(p) -- make list empty. Return pointer, as in uu_lsnew.
**	uu_lsnext(p) -- returns a pointer to the next element in the list after 
**		the element pointed to by p. If p points to the last element in the 
**		list, or the list is empty, NULL  is returned.
**	uu_lsprev(p) -- returns a pointer to the previous element in the list before 
**		the element pointed to by p. If p points to the first element in the 
**		list, or the list is empty, NULL is returned.
** uu_lsend(p) --  returns pointer to last elt in the list, or NULL.
**					P is a pointer to any element of the list.
**					Is efficient if p is the pointer returned by uu_lsnew.
**	uu_lslen(p) -- returns the length, in bytes, of the list element pointed 
**		to by p.
**	uu_lsswap(p,q) -- swaps the order of elements p and q in the list.
**	uu_lsprnt(p) -- is only for debugging this list package. It prints 
**		(dumps) on stdout the elements in the list starting with the
**		element pointed to by p.
** int uu_lstobgn(l) -- moves list element to beginning of list.
** int uu_lsmovel(p, q) -- moves list element in front of another list
**    element.
** int uu_lsmove (pad, qad) -- moves list element in front of another list
**    element using element addresses.
** int uu_lsapndl(p, q) -- concatenates two lists to make one.
** int uu_lsapnd (pad, qad) -- concatenates two lists to make one.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       ulists.c , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:54
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"

/* this typedef defines a list element header. */

typedef struct uu_lselt 
{
	struct uu_lselt *fptr,*bptr;	/* forward, back pointers */
	int len;							/* length of element data, in bytes */
	int dummy;
} uu_list;							/* the element data follows the list elt hdr*/

char *malloc();

/*********************************************************************
**    E_FUNCTION :  char *uu_lsnew() -- create new list.
**			Create a new (empty) list and returns a pointer to the list.  
**			This pointer is conceptually before the first element of the list.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : pointer to the new list, or NULL if out of memory.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_lsnew()
{
	uu_list *p;
	char *irtn;
	uu_denter(UU_U1TRC,(us,"uu_lsnew()"));

	irtn=NULL;
	/* allocate the list head element*/
	p=(uu_list *)malloc(sizeof(uu_list));	
	if (p!=NULL) 
	{
		uu_lsinit(p);						/* init the list head elt */
		irtn=(char *)p;
		irtn=irtn+sizeof(uu_list);		/* irtn points to the (non-existant) data */
	}
	uu_dprint(UU_U1TRC,(us,"RETURNING %x=uu_lsnew()",irtn));
	uu_dexit;
	return(irtn);
}

uu_lsinit(head)								/* initialize a new list head */
uu_list *head;
{
	uu_denter(UU_U1TRC,(us,"uu_lsinit(head:%x)", head));
	/* make list head point to itself */
	(*head).fptr=head;
	(*head).bptr=head;
	(*head).len=0;					/* identifies the list head element */
	uu_dexit;
	return (0);
}

 /************ ELEMENT MANIPULATION PROCEDURES **********/

/*********************************************************************
**    E_FUNCTION :  char *uu_lsnext(l) -- next element.
**			Returns a pointer to the next element in the list after 
**			the element pointed to by l. If l points to the last element in the 
**			list, or the list is empty, NULL  is returned.
**			Note, l points to the data associated with some list element, or
**			it was returned from lsnew(). Returns a pointer to the data associated 
**			with the next element of the list, or NULL if no more elements. 
**    PARAMETERS   
**       INPUT  :  char *l -- pointer to a list element
**       OUTPUT :  
**    RETURNS      : pointer to next list element or NULL.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_lsnext(l)	
char *l;
{ 
	char *irtn;
	struct uu_lselt *p;

	uu_denter(UU_U1TRC,(us,"uu_lsnext(%x, ptr to data area of list elt)",l));
	p=(struct uu_lselt *)(l-sizeof(struct uu_lselt));	/* pointer to current list elt*/
	p=(*p).fptr;		/* follow forward pointer */
	if ((*p).len==0) irtn=NULL;	/* we are at list head, return  NULL */
	else 
	{
		irtn=(char *)p;
		irtn=irtn+sizeof(struct uu_lselt);	/* return pointer to this elt's data*/
	}
	
	uu_dprint(UU_U1TRC,(us,"RETURNS %x=uu_lsnext(%x)",irtn,l));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  char *uu_lsprev(l) -- previous element.
**			Returns a pointer to the previous element in the list before 
**			the element pointed to by p. If p points to the first element in the 
**			list, or the list is empty, NULL is returned.
**
**			l points to the data assoc with some list element, or
**			it was returned from lsnew(). Return a pointer to
**			the data associated with the prev element of the
**			list, or NULL if no more elements
**    PARAMETERS   
**       INPUT  : 	char *l -- points to a list element.
**       OUTPUT :  none
**    RETURNS      : pointer to previous list element, or NULL.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_lsprev(l)
char *l;
{ 
	char *irtn;
	struct uu_lselt *p;

	uu_denter(UU_U1TRC,(us,"uu_lsprev(ptr to the data assoc with elt:%x)",l));
	p=(struct uu_lselt *)(l-sizeof(struct uu_lselt));	/* pointer to current list elt*/
	p=(*p).bptr;		/* follow backward pointer */
	if ((*p).len==0) irtn=NULL;	/* we are at list head, return  NULL */
	else 
	{
		irtn=(char *)p;
		irtn=irtn+sizeof(struct uu_lselt);	/* return pointer to this elt's data*/
	}
	uu_dprint(UU_U1TRC,(us,"RETURN %x=uu_lsprev(%x)",irtn,l));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  char *uu_lsend(l) -- last element.
**			Returns a pointer to the last element in the list.
**			If list empty, return NULL. If l is the pointer returned by
**			ug_lsnew, this is very efficient.
**
**			l points to the data assoc with some list element, or
**			it was returned from lsnew(). Return a pointer to
**			the data associated with the last element of the
**			list, or NULL if no elements
**    PARAMETERS   
**       INPUT  : 	char *l -- points to a list element.
**       OUTPUT :  none
**    RETURNS      : pointer to last list element, or NULL.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_lsend(l)
char *l;
{ 
	char *irtn;
	struct uu_lselt *p;

	uu_denter(UU_U1TRC,(us,"uu_lsend(ptr to the data assoc with elt: %x)",l));
	p=(struct uu_lselt *)(l-sizeof(struct uu_lselt));	/* pointer to current list elt*/
	/* follow backward pointer to list head */
	while ((*p).len!=0) p=(*p).bptr;
	/* we are at list head. go directly to end */
	p=(*p).bptr;
	if ((*p).len==0) {	/* list is empty, return null */
		irtn=NULL;
	}
	else {
		irtn=(char *)p;
		irtn=irtn+sizeof(struct uu_lselt);		/* return ptr to this elt's data*/
	}
	uu_dprint(UU_U1TRC,(us,"RETURN %x=uu_lsend(%x)",irtn,l));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  int uu_lslen(l) -- length of list element.
**    PARAMETERS   
**       INPUT  :  char *l -- pointer to list element.
**       OUTPUT :  
**    RETURNS      : length of list element in bytes, or 0 if l is list head
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_lslen(l)
char *l;
{
	struct uu_lselt *p;

	uu_denter(UU_U1TRC,(us,"uu_lslen(?)"));
	p=(struct uu_lselt *)(l-sizeof(struct uu_lselt));
	uu_dprint(UU_U1TRC,(us,"%d=uu_lslen(%x)",(*p).len,l));
	uu_dexit;
	return((*p).len);
}

/*********************************************************************
**    E_FUNCTION :  char *uu_lsinsrt(l,len) -- insert new element.
**			Returns a pointer to a new list element, inserted 
**			after the list element pointed to by l. If l is the pointer returned 
**			by uu_lsnew, the new element becomes the first element of the list. 
**			The new element length is len bytes. Storage is allocated for the 
**			new element. The caller may now place any data he wishes in the 
**			new element.
**
**			insert new element of length len
**			after current elt. Return ptr to it
**    PARAMETERS   
**       INPUT  :  char *l -- pointer to list element.
**						 int len -- length of new element.
**       OUTPUT :  
**    RETURNS      : pointer to new element, or NULL if out of memory.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_lsinsrt(l,len)
char *l;
int len;
{ 
	char *irtn;
	struct uu_lselt *p,*curad,*nxtad;

	uu_denter(UU_U1TRC,(us,"uu_lsinsrt(point to list elt:%x, length:%d)",
					l, len));
	irtn=NULL;
	if (len<=0) 
	{	/* len must be positive */
		uu_dprint(UU_U1TRC,(us,
			"uu_lsinsrt(list=%x, len=%d) error. len not positive.", l,len));
	}
	else 			/* len is OK */
	{	
		curad=(struct uu_lselt *)(l-sizeof(struct uu_lselt));
		nxtad=(*curad).fptr;			/* next element pointer */
		/* allocate space for new elt */
		p=(struct uu_lselt *)malloc(len+sizeof(struct uu_lselt));
		if (p!=NULL) 
		{
			(*p).fptr=nxtad; 	/* set pointers in new elt */
			(*p).bptr=curad;
			(*p).len=len;		/* remember length of new element */
			(*nxtad).bptr=p; 	/* fix back pointer in old next element */
			(*curad).fptr=p;	/* fix forward pointer in old cur elt */
			irtn=(char *)p;
			irtn=irtn+sizeof(struct uu_lselt);	/* irtn -> new elt data area */
		}
		uu_dprint(UU_U1TRC,(us,
				"RETURN %x=uu_lsinsrt(list elt=%x, len=%d)",irtn,l,len));
	}
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  int uu_lsswap(p,q) -- swap order of elements p & q
**	
**    PARAMETERS   
**       INPUT  :  char *p -- pointer to any list element.
**	  					 char *q -- pointer to any list element.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : Order of p and q in list is exchanged
**    WARNINGS     : none
*********************************************************************/

int uu_lsswap(p, q)
char *p;
char *q;
{ 
	struct uu_lselt *qad, *pad, *qfptr, *qbptr, *tmp;

	uu_denter(UU_U1TRC,(us,"uu_lsswap(%x, %x)",p,q));

	pad=(struct uu_lselt *)(p-sizeof(struct uu_lselt));
	qad=(struct uu_lselt *)(q-sizeof(struct uu_lselt));

	/* If q is immediately after p, swap p and q */
	if( qad->fptr == pad ) {
		uu_dprint(UU_U1TRC,(us,"swapping qp"));
		tmp = pad; pad = qad; qad = tmp;
	}

	/* Save q's forward and backward pointers */
	qbptr = qad->bptr;
	qfptr = qad->fptr;

	if( pad->fptr == qad ) {		/* Handle consecutive p,q as special case */
		uu_dprint(UU_U1TRC,(us,"pq adjacent elements"));
		pad->bptr->fptr = qad;
		qad->fptr->bptr = pad;
		qad->bptr = pad->bptr;
		qad->fptr = pad;
		pad->bptr = qad;
		pad->fptr = qfptr;
	}

	else {							/* P, Q not consecutive in list */
		/* Put q in p's spot */
		pad->bptr->fptr = qad;
		pad->fptr->bptr = qad;
		qad->bptr		 = pad->bptr;
		qad->fptr		 = pad->fptr;
	
		/* Put p in q's spot */
		qbptr->fptr = pad;
		qfptr->bptr = pad;
		pad->bptr	= qbptr;
		pad->fptr	= qfptr;
	}

	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION :  char *uu_lsdele(l) -- delete list element.
**			Deletes the list element pointed to by l and returns a 
**			pointer to next element in the list. If l pointed to the last element 
**			in the list, or if the list was empty, NULL is returned.
**
**			delete list element l. Return pointer
**			to next element, or NULL if l is list head.
**			If l is last elt in list, return ptr to list head.
**    PARAMETERS   
**       INPUT  : 	char *l -- pointer to element.
**       OUTPUT :  
**    RETURNS      : pointer to next element, or NULL.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_lsdele(l)
char *l;
{ 
	struct uu_lselt *prvad,*nxtad,*curad;
	char *irtn;

	uu_denter(UU_U1TRC,(us,"uu_lsdele(%x, ptr to data of elt to delete)",l));
	curad=(struct uu_lselt *)(l-sizeof(struct uu_lselt));
	if ((*curad).len<=0) irtn=NULL;		/* l is list head */
	else {										/* l is not list head */
		prvad=(*curad).bptr;
		nxtad=(*curad).fptr;
		free(curad);						/* free the deleted elt's storage */
		(*prvad).fptr=nxtad;
		(*nxtad).bptr=prvad;
		irtn=(char *)nxtad;
		irtn=irtn+sizeof(struct uu_lselt);	/* pointer to next elt's data */
	}
	uu_dprint(UU_U1TRC,(us,"RETURNS %x=uu_lsdele(%x)",irtn,l));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  char *uu_lsempty(l)
**
** 	Make list empty. Return pointer to head of list, as in uu_lsnew.
**
**    PARAMETERS   
**       INPUT  : 	char *l -- pointer to any element in list.
**       OUTPUT :    none
**    RETURNS      : pointer to list, as in uu_lsnew.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_lsempty(l)				/* make list l empty, return head ptr */
char *l;								/* any list elt pointer */
{ 
	char *ad,*adf;
	uu_denter(UU_U1TRC,(us,"uu_lsempty(%x)",l));
	ad=l;
	while ((adf=uu_lsdele(ad))!=NULL) 
		ad=adf;	/* delete elts until find list head */
	/* found list head. ad points to its (non-existant) data */
	adf=uu_lsnext(ad);				/* ad=1st elt of list, or NULL if empty*/
	while (adf!=NULL) {
		ad=adf;	/* delete elts till find head again*/
		adf=uu_lsdele(ad);
	}
	uu_dexit;
	return(ad);					/* return list head ptr */
}

/*********************************************************************
**    E_FUNCTION :  uu_lsdel(l) -- delete entire list
** 	Deletes the entire list, l may point to any element or may be 
**		the pointer returned by uu_lsnew or uu_lsempty.
**    PARAMETERS   
**       INPUT  :  char *l -- pointer to any list element.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_lsdel(l)					/* entirely delete the list containing elt l */
char *l;
{
	char *p;
	uu_list *head;

	uu_denter(UU_U1TRC,(us,"lsdel(%x)",l));
	p=uu_lsempty(l);
	head=(uu_list *)(p-sizeof(uu_list));
	free(head);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  uu_lsprnt(l) -- print list elements.
**		Only for debugging this list package. It prints 
**		(dumps) on stdout the elements in the list starting with the
**		element pointed to by l.
**    PARAMETERS   
**       INPUT  :  char *l -- pointer to 1st list element to print.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_lsprnt(l)
char *l;
{ 
	struct uu_lselt *p;

	uu_denter(UU_U1TRC,(us,"uu_lsprnt(ptr to 1st list elt to print:%x)",l));
	p=(struct uu_lselt *)(l-sizeof(struct uu_lselt));
	uu_dprint(UU_U1TRC,(us,"uu_lsprnt(real address: %x)",p));
	while ((*p).len>0) {
		uu_dprint(UU_U1TRC,(us,"elt ptr=%x, fptr=%x, bptr=%x, len=%d",
			p,(*p).fptr,(*p).bptr,(*p).len));
	}
	uu_dexit;
}                                 /* uu_lsprnt */

/*********************************************************************
**    E_FUNCTION :  int uu_lstobgn(l)
**        Move list element to beginning list.
**    PARAMETERS   
**       INPUT  :  char *l -- pointer to any list element.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : l is moved to beginning of list.
**    WARNINGS     : none
*********************************************************************/

int uu_lstobgn(l)
char *l;
{ 
   struct uu_lselt *p, *prvad;

   p=(struct uu_lselt *)(l-sizeof(struct uu_lselt));
   prvad = p->bptr;

   if (p->len > 0 && prvad->len > 0)
     {
     while (prvad->len > 0) prvad = prvad->bptr;
     uu_lsmove (p, prvad);
     }

	return (0);
}
/*********************************************************************
**    E_FUNCTION :  int uu_lsmovel(p, q)
**        Move list element to be after another list element.
**    PARAMETERS   
**       INPUT  :  char *p -- pointer to any list element.
**                 char *q -- pointer to list element p is to follow.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : p is moved.
**    WARNINGS     : none
*********************************************************************/

int uu_lsmovel(p, q)
char *p;
char *q;
{ 
   struct uu_lselt *pad, *qad;

   pad=(struct uu_lselt *)(p-sizeof(struct uu_lselt));
   qad=(struct uu_lselt *)(q-sizeof(struct uu_lselt));

   if (pad != qad->fptr && pad->len > 0) uu_lsmove (pad, qad);

	return (0);
}
/*********************************************************************
**    E_FUNCTION :  int uu_lsmove (pad, qad)
**        Move list element to be after another list element.
**    PARAMETERS   
**       INPUT  :  char *pad -- Address to any list element.
**                 char *qad -- Address to list element pad is to follow.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : pad is moved.
**    WARNINGS     : none
*********************************************************************/

int uu_lsmove (pad, qad)
struct uu_lselt *pad, *qad;
{ 
   struct uu_lselt *prvad, *nxtad;

   prvad = pad->bptr;
   nxtad = pad->fptr;
   prvad->fptr = nxtad;
   nxtad->bptr = prvad;
   nxtad = qad->fptr;
   qad->fptr = pad;
   pad->bptr = qad;
   pad->fptr = nxtad;
   nxtad->bptr = pad;

	return (0);
}
/*********************************************************************
**    E_FUNCTION :  int uu_lsapndl(p, q)
**        Appand list pad to the end of qad list.
**    PARAMETERS   
**       INPUT  :  char *p -- pointer to start of appended list
**                 char *q -- pointer to end of current list
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : p is moved.
**    WARNINGS     : none
*********************************************************************/

int uu_lsapndl(p, q, pe, qh)
char *p,*q,*pe,*qh;
{ 
   struct uu_lselt *pad, *qad, *pend, *qfir;

   pad=(struct uu_lselt *)(p-sizeof(struct uu_lselt));
   qad=(struct uu_lselt *)(q-sizeof(struct uu_lselt));
	pend=(struct uu_lselt *)(pe-sizeof(struct uu_lselt));
	qfir=(struct uu_lselt *)(qh-sizeof(struct uu_lselt));

   if (pad != qad->fptr && pad->len > 0) uu_lsapnd (pad,qad,pend,qfir);

	return (0);
}
/*********************************************************************
**    E_FUNCTION :  int uu_lsapnd (pad, qad)
**        Appand list pad to the end of qad list.
**    PARAMETERS   
**       INPUT  :  char *pad -- Address of first element of added list
**                 char *qad -- Address of last element of list to
**                              add to
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : pad is moved.
**    WARNINGS     : none
*********************************************************************/

int uu_lsapnd (pad,qad,pend,qfir)
struct uu_lselt *pad,*qad,*pend,*qfir;
{ 
   struct uu_lselt *pclpt;

	pclpt = pad->bptr;
/*
.....Free header memory from list being added onto the end of the other
*/
	if ((*pclpt).len<=0) free(pclpt);
   qad->fptr = pad;
   pad->bptr = qad;
	pend->fptr = qfir;
	return (0);
}
