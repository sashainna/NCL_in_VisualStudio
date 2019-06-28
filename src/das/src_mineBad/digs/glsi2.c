/********************************************************************
**    NAME         :  glsi2.c -- indexed list processing package.a
**			This is the 2nd implementation. Stores each list as an array
**			of elements. Each element starts on an integer boundary and
**			is preceeded by its byte count.  All storage comes from the
**			current store, by calling uu_malloc/uu_free.
**       CONTAINS:
**		UG_LSI L -- define L as a list.
**		Elements in a list are indexed from 0 to no_elts-1.
**    ug_lsiinit(l) -- init list l to empty. Must be called 1st.
**		ug_lsidel(l) -- delete list l entirely.
**		char *ug_lsielt(l,i) -- return pointer to ith element, or NULL.
**		int ug_lsinelt(l) -- return no. elements in list l.
**		ug_lsilen(l,i) -- return length of ith element, or 0.
**		ug_lsidele(l,i) -- delete ith element of list l.
**		char *ug_lsiins(l,i,len) -- insert elt of length len as ith elt.
**								return pointer to it, or NULL.
**		int lsicur(l) -- return most recently accessed element of list l.
**		ug_lsiempty(l) -- make list l empty.
**		ug_lsicpy(l1,l2) -- copy l2 to end of l1.
**		ug_lsicomp(l) -- compact  list l. (copy it to a block just right size).
**								Don't free the old block.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       glsi2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:20
*********************************************************************/
#include "glsi2.h"
#include "udeboff.h"
#define NULL 0

/* the following typedef is defined in glsi2.h: 
/* typedef struct {		/* define an indexed list
/* int *blkptr;			/* ptr to beginning of list block */
/* int cureltindx;		/* block offset to current element, or -1
/* int lsisize;			/* integer size of used part of list block
/* int lsiblksize;		/* integer size of list block 
									(starts multiple of UG_LSIBLKSIZE)
/* short curindx;			/* current element index, -1 if cureltindx is -1
/* short nelts;			/* number of elements in this list
/* } UG_LSI[1]; */

int *ug_lsispare=NULL;		/* pointer to spare std size block, or NULL */

#include "umoveb.h"
#define INTMEMCOPY(to,from,intlen) uu_move_byte(from,to,intlen*sizeof(int))

/*********************************************************************
**    E_FUNCTION     :  int ug_lsiinit(l) -- init an indexed list.
**    PARAMETERS   
**       INPUT  : UG_LSI l -- list to be initialized.
**    RETURNS      : 0 if all went OK, 1 = out of memory.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_lsiinit(l)
UG_LSI l;
{
	int irtn;
	int *blkptr;
	char us[50];
	/* allocate and init a std size block of memory for this list. */
	ALLOCSTDBLK(blkptr);
	if (blkptr==NULL) irtn=1;
	else {
		l[0].blkptr=blkptr;
		l[0].cureltindx= -1;
		l[0].curindx= -1;
		l[0].lsisize=0;
		l[0].lsiblksize=UG_LSIBLKSIZE;
		l[0].nelts=0;
		irtn=0;
	}
	uu_denter2(UU_GITRC,(us,"%d=ug_lsiinit(%x)",irtn,l));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION  :  int ug_lsiindx(l,i) -- return index in block to 
**							length of ith elt of list.
**    PARAMETERS   
**       INPUT  : 	UG_LSI l -- list.
**							int i -- list element index.
**    RETURNS      : index of ith element's length, or  -1.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_lsiindx(l,i)
UG_LSI l; int i;
{	
	int irtn;
	int indx;
	uu_denter(UU_GITRC,(us,"ug_lsiindx(%x,%d) blkptr=%x, curindx=%d",
		l,i,l[0].blkptr, l[0].curindx));
	if ((i<0)||(i>=l[0].nelts)) irtn= -1;
	else {						/* there is an ith element */
		irtn=l[0].cureltindx;
		indx=l[0].curindx;
		if ((irtn== -1)||(i<indx)) {	/* there was no current elt., or we need 
													to go backward. use 0th elt */
			irtn=0; indx=0;
		}
		while (indx<i) { 		/* go forward if needed */
			/* add integer size of this element (plus 1) to irtn */
			irtn += (l[0].blkptr[irtn]+sizeof(int)-1)/sizeof(int) + 1;
			indx++;
		}
		/* now we are at ith elt */
	}									/* end there is an ith element */
	uu_dprint(UU_GITRC,(us,"%d=ug_lsiindx(%x,%d)",irtn,l,i));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  char *ug_lsielt(l,i) -- pointer to ith elt, or NULL.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : pointer to ith elt, or NULL.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char *ug_lsielt(l,i)
UG_LSI l;
int i;
{
	char *irtn;
	int indx;						/* index in list block of ith elt's length*/
	char us[50];

	indx=ug_lsiindx(l,i);		/* get index in list block of element */
	if (indx== -1) irtn=NULL;
	else {
		irtn= (char *)&l[0].blkptr[indx+1];
		/* make this elt the current elt */
		l[0].cureltindx=indx; l[0].curindx=i;
	}	
	uu_dprint(UU_GITRC,(us,"%x=ug_lsielt(%x,%d) indx=%d",irtn,l,i,indx));
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION:  int ug_lsilen(l,i) -- length of ith elt of list l.
**    PARAMETERS   
**       INPUT  : UG_LSI l -- list
**						int i -- list element index.
**    RETURNS      : length of ith elt of list l, or zero of no ith elt.
*********************************************************************/
int ug_lsilen(l,i) 
UG_LSI l;
int i;
{
	int indx;
	indx=ug_lsiindx(l,i);
	if(indx== -1) return( 0); else return( l[0].blkptr[indx]);
}

/*********************************************************************
**    E_FUNCTION     :  ug_lsidele(l,i) -- delete ith elt from list l.
**    PARAMETERS   
**       INPUT  :  UG_LSI l -- list.
**						 int i; -- list element index
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_lsidele(l,i)
UG_LSI l;
int i;
{
	int indx1;				/* index to elt i */

	uu_denter(UU_GITRC,(us,"ug_lsidele(%x,%d) blkptr=%x",
		l,i,l[0].blkptr));
	/* copy all elts after elt l upwarads in list block. */
	indx1=ug_lsiindx(l,i);
	if (indx1!= -1) {	/* there is an ith element */
		int intlen;			/* integer length of elt being deleted, incl length */
		int indx2;			/* index to elt i+1 */
		intlen = (l[0].blkptr[indx1]+sizeof(int)-1)/sizeof(int) + 1;
		indx2=ug_lsiindx(l,i+1);
		if (indx2!= -1) {		/* there is an (i+1)st elt. copy up */
			/* copy block from blkptr[indx2] to end to blkptr[indx1] */
			INTMEMCOPY(&l[0].blkptr[indx1],&l[0].blkptr[indx2],
				l[0].lsisize-indx2);
		}
		l[0].lsisize -= intlen;
		l[0].nelts--;
	}
	uu_dprint(UU_GITRC,(us,"ug_lsidele. indx1=%d lsizise=%d",
		indx1,l[0].lsisize));
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  char *ug_lsiins(l,i,len) -- insert elt as ith elt.
**								Make it the current element.
**    PARAMETERS   
**       INPUT  : UG_LSI l -- list.
**						int i -- insert new elt as ith elt.
**						int len -- length (bytes) of new elt.
**       OUTPUT :   none
**    RETURNS      : pointer to new element.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char *ug_lsiins(l,i,len)
UG_LSI l;						/* list */
int i;							/* insert new elt as this elt */
int len;							/* length (bytes) of new elt */
{
	/* copy elts after (i-1)st elt down to make room for the new one.*/
	int indx1;				/* index to elt i */
	int indx2;				/* index to elt i+1, or end */
	int intlen;				/* length of new element in words */
	int newblklen;			/* new block length required */
	int ii;
	int j;

	uu_denter(UU_GITRC,(us,"ug_lsiins(%x,i=%d,len=%d) blkptr=%x",
		l,i,len,l[0].blkptr));
	ii=i;
	if (ii>l[0].nelts) ii=l[0].nelts;
	intlen=(len+sizeof(int)-1)/sizeof(int);
	newblklen=l[0].lsisize+intlen+1;

	/* copy entire list into a new (bigger) block if needed */
	if (newblklen>l[0].lsiblksize) {	/* block too small, get a new one */
		int *newblkptr;		/* pointer to new block */

		/* Increase size of block.  Double the size as long as its
		 * less than 100k, else add 100k.  Nothing magic about these
		 * values, they can be changed as needed.
		 */

		if( newblklen < 100000 ) {
			/* make new block at least twice as big as the current block */
			if (newblklen<(l[0].lsiblksize*2))
					newblklen=l[0].lsiblksize*2;
		}
		else {
			/* add at least 100k to the current block */
			if (newblklen<(l[0].lsiblksize+100000))
					newblklen=l[0].lsiblksize+100000;
		}

		newblkptr=(int *)uu_malloc(newblklen*sizeof(int));
		INTMEMCOPY(newblkptr,l[0].blkptr,l[0].lsisize);
		FREEBLK(l);				/* free list l's old block */
		l[0].blkptr=newblkptr;
		l[0].lsiblksize=newblklen;		
		uu_dprint(UU_GITRC,(us,"ug_lsiins. new blksize=%d, lsisize=%d",
				l[0].lsiblksize,l[0].lsisize));
	}

	/* now we have a big enough block to hold new elt */
	/* copy all elts from elt i (if any) downwards in list block. */
	indx1=ug_lsiindx(l,ii);
	if (indx1!= -1) {		/* there is already an ith element. copy down */
		int *p1,*p2;
		p1= &l[0].blkptr[l[0].lsisize-1];
		p2= &l[0].blkptr[l[0].lsisize+intlen];
		uu_dprint(UU_GITRC,(us,"ug_lsiins copying %d words from %x to %x",
			l[0].lsisize-indx1,p1,p2));
		for (j=0; j<l[0].lsisize-indx1; j++) *(p2--) = *(p1--);
	}
	else indx1=l[0].lsisize;		/* not already an ith elt. New one at end */
	/* now indx1 points to place for new elt. put length there */
	l[0].blkptr[indx1]=len;
	l[0].lsisize += (intlen+1);
	l[0].nelts++;
	/* make this elt current */
	l[0].cureltindx=indx1;
	l[0].curindx=ii;
	
	uu_dprint(UU_GITRC,(us,"%x=ug_lsiins. indx1=%d  lsizise=%d",
		&l[0].blkptr[indx1+1],indx1,l[0].lsisize));
	uu_dexit;
	return((char *)&l[0].blkptr[indx1+1]);
}

/*********************************************************************
**    E_FUNCTION :  ug_lsicpy(l1,l2) -- copy l2 to end of l1.
**    PARAMETERS   
**       INPUT  :  UG_LSI l1,l2;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_lsicpy(l1,l2)						/* copy l2 to end of l1 */
UG_LSI l1,l2;
{
	int len1,len2;
	uu_denter(UU_GITRC,(us,"ug_lsicpy(%x,%x)",l1,l2));
	len1=l1[0].lsisize;
	len2=l2[0].lsisize;
	/* if needed, copy l1 into a block big enough to hold l1 and l2 */
	if ((len1+len2)>l1[0].lsiblksize) {
		int *newblkptr;
		newblkptr=(int *)uu_malloc((len1+len2)*sizeof(int));
		INTMEMCOPY(newblkptr,l1[0].blkptr,len1);
		FREEBLK(l1);
		l1[0].blkptr=newblkptr;
		l1[0].lsiblksize=len1+len2;
	}
	/* now l1 is in a block big enough to hold l2 */
	INTMEMCOPY(&l1[0].blkptr[l1[0].lsisize],l2[0].blkptr,len2);
	l1[0].nelts += l2[0].nelts;
	l1[0].lsisize += len2;
	/* update current element */
	l1[0].curindx=ug_lsiindx(l1,l1[0].nelts-1);
	l1[0].cureltindx=l1[0].nelts-1;
	uu_dprint(UU_GITRC,(us,
		"ug_lsicpy returns. nelts=%d. lsisize=%d cureltno=%d, cureltindx=%d",
		l1[0].nelts,l1[0].lsisize,l1[0].curindx,l1[0].cureltindx));
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ug_lsicomp(l) -- compact list l. (copy it into a block
**			just the right size. Don't free the old block.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_lsicomp(l)
UG_LSI l;
{
	int *newblkptr;
	int siz;
	uu_denter(UU_GITRC,(us,"ug_lsicomp(%x)",l));
	if (l[0].lsisize<l[0].lsiblksize) {		/* block is bigger than needed. */
		siz=l[0].lsisize;
		if (siz<=0) siz=1;
		newblkptr=(int *)uu_malloc(siz*sizeof(int));
		if (newblkptr!=NULL) {
			if (l[0].lsisize>0) {
				INTMEMCOPY(newblkptr,l[0].blkptr,l[0].lsisize);
			}
			FREEBLK(l);
			l[0].blkptr=newblkptr;
			l[0].lsiblksize=l[0].lsisize;
		}
	}
	uu_dexit;
}
