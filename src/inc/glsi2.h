/*********************************************************************
**    NAME         :  glsi2.h -- include file for glsi2.c.
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       glsi2.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:20
*********************************************************************/

#ifndef GLSI2H


typedef struct UG_lsi {		/* define an indexed list*/
int *blkptr;			/* ptr to beginning of list block */
int cureltindx;		/* block word offset to current element, or -1. */
int lsisize;			/* integer size of used part of list block */
int lsiblksize;		/* integer size of list block 
									(starts UG_LSIBLKSIZE) */
int curindx;			/* current element index, -1 if current element is -1 */
int nelts;				/* number of elements in this list */
} UG_LSI[1];

extern int *ug_lsispare;		/* pointer to spare std size block, or NULL */

/* size of a standard list block, in integers. */
#define UG_LSIBLKSIZE 512

#define FREEBLK(l) \
		if ((ug_lsispare==NULL)&&(l[0].lsiblksize==UG_LSIBLKSIZE)) { \
			/* save this memory block for possible future use */ \
			ug_lsispare=l[0].blkptr; } \
		else uu_free(l[0].blkptr)

#define ALLOCSTDBLK(blkptr) \
	if (ug_lsispare!=NULL) { blkptr=ug_lsispare; ug_lsispare=NULL;} \
	else blkptr=(int *)uu_malloc(UG_LSIBLKSIZE*sizeof(int))

/*********************************************************************
**    E_FUNCTION :  ug_lsidel(l) -- delete list l entirely.
**		PARAMETERS :
**       INPUT  :  UG_LSI l -- list to be deleted.
*********************************************************************/
#define ug_lsidel(l) {if (l[0].blkptr!=NULL) { \
	FREEBLK(l); } \
	l[0].blkptr=NULL;}

/*********************************************************************
**    E_FUNCTION :  int ug_lsinelt(l) -- number of elts in list l.
**    PARAMETERS   
**       INPUT  :  UG_LSI l -- list.
**    RETURNS      : number of elts in list l.
*********************************************************************/
#define ug_lsinelt(l) l[0].nelts
	
/*********************************************************************
**    E_FUNCTION :  int ug_lsicur(l) -- return index of current elt.
**    PARAMETERS   
**       INPUT  :  UG_LSI l -- list.
**    RETURNS      : index of most recently accessed index of list l.
*********************************************************************/
#define ug_lsicur(l) l[0].curindx

/*********************************************************************
**    E_FUNCTION     :  ug_lsiempty(l) -- make list i empty
**    PARAMETERS   
**       INPUT  : UG_LSI l -- list to be made empty.
*********************************************************************/
#define ug_lsiempty(l) {l[0].cureltindx= -1; \
	l[0].curindx= -1; l[0].nelts=0; l[0].lsisize=0;}
	
#define GLSI2H
#endif
