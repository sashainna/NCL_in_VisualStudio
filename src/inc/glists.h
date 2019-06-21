/********************************************************************* 
**  NAME:  glists.h
** 	Include file for List manager for lists with variable sized elements.
**
**		These functions operate on doubly linked (circular) lists.
**		The algorithm is taken from Knuth, Vol 1, P. 278. A list		
**		contains zero or more list elements.  A list element may 
**		contain any kind of data. Each element of a list may be 
**		of a different length. All data is kept in main memory.
**       CONTAINS:
**	ug_lsnew -- creates a new (empty) list and returns a pointer to the list.  
**		This pointer is conceptually before the first element of the list.
** ug_lsdel(p) -- deletes the entire list P may point to any element or may be 
**		the pointer returned by ug_lsnew.
**	ug_lsinsrt(p,len) --  returns a pointer to a new list element, inserted 
**		after the list element pointed to by p. If p is the pointer returned 
**		by ug_lsnew, the new element becomes the first element of the list. 
**		The new element length is len bytes. Storage is allocated for the 
**		new element. The caller may now place any data he wishes in the 
**		new element.
**	ug_lsdele(p) --  deletes the list element pointed to by p and returns a 
**		pointer to next element in the list. If p pointed to the last element 
**		in the list, or if the list was empty, NULL  is returned.
**	ug_lsnext(p) -- returns a pointer to the next element in the list after 
**		the element pointed to by p. If p points to the last element in the 
**		list, or the list is empty, NULL  is returned.
**	ug_lsprev(p) -- returns a pointer to the previous element in the list before 
**		the element pointed to by p. If p points to the first element in the 
**		list, or the list is empty, NULL is returned.
**	ug_lslen(p) -- returns the length, in bytes, of the list element pointed 
**		to by p.
**	ug_lsprnt(p) -- is only for debugging this list package. It prints 
**		(dumps) on stdout the list pointed to by p.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       glists.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:20
*********************************************************************/

#ifndef GLISTSH

char *ug_lsnew(),*ug_lsnext(),*ug_lsprev(),*ug_lsinsrt(),
	*ug_lsdele(),*ug_lsempty();


#define GLISTSH
#endif
