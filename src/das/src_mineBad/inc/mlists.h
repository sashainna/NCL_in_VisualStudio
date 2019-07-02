
/********************************************************************* 
**  NAME:  lists.h
**
** 	Include file for List manager for lists with variable sized elements.
** 	All list data and sizes are in terms of arrays of integers.
**  	This file of include info and external documentation is designed to
**  	be independent of the implementation.
**  	STORAD and STORFILE_FUNCTION data types are defined in stor.h.
** 	The user should never read/write directly from/to list elements but
**  	use lsread/lswrit. This allows the list implementation to use disk
**  	as required.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       mlists.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:31
**
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  none
**
**  SIDE EFFECTS :  none
**
**  WARNINGS     :  none
*********************************************************************/

#ifndef MLISTSH

#define UMI_lismaxsiz 299     /* maximum no. integers size of list elt */
#define umi_liselmt(x) int x[UMI_lismaxsiz]
/* The user-callable routines are:
   lsinit(maxwds)--initialize list system for at least maxwds total.
     int maxwds;
   lsdone()--done with lists.
   lssave(fname)--save all lists on named file.
     umi_storfilename fname;
   lsread(fname)--read lists from named file.
     umi_storfilename fname;
   lscre(p)--umi_create new list p. p must be between 0 and some
             implementation defined maximum.
     int p;
   lsdel(p)--delete list p if it exists.
     int p;
   lsmove(p1,p2)--move p2 to the end of p1. delete p2.
     int p1, p2;
   lstop(p)--move the 'current posn' of list p to before 1st element of p.
     int p;
   lsbot(p)--move 'current posn' of list p to last element of p.
     int p;
   int lsnext(p)--move 'current posn' to next element of p. Normally
                  return 1. If already at end, return 0.
     int p;
   lsprev(p)--move 'current posn' to prev element of list p.
     int p;
   lsset(p,ad)--make the element with address ad the 'current posn' of list p.
     int p; storad *ad;
   lscur(p,ad)--return in ad the address of the 'current posn' of list p.
     int p; storad *ad;
   int lsgete(p,elt)--return in elt the element at the 'current posn' of
                      list p. Value is length of current element.
     int p; umi_liselmt elt;
   lsput(p,elt,len)--replace current element of list p with data in elt of
                     length len.
     int p, len; umi_liselmt elt;
   lsins(p,elt,len)--insert after the current element of list p the data in
                     elt of length len.
     int p, len; umi_liselmt elt;
   lsdele(p)--delete current element of list p.
     int p;
int lsexst(p)--return 1 if list p exists. Else 0.
  int p;
   lsprnt(p)--print (dump) for debugging list p on stdout.
     int p;
int lssiz(l)--return the size in integers of list l
  int l;
*/


#define MLISTSH
#endif
