/*********************************************************************
**    NAME         :  tiotrf.c
**       CONTAINS:
**				uio_mtrtomtr
**				uio_tran
**				uio_chk_matrix
**				uio_set_trf
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tiotrf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:54
*********************************************************************/

#include		"usysdef.h"
#include		"umath.h"
#include		"ulist.h"
#include		"rbase.h"
#include		"modef.h"
#include		"mdeval.h"
#include		"udebug.h"
#include		"tiges.h"
#include		"tigdefs.h"
#include		"mcrv.h"
#include		"mdcoord.h"
#include		"adraft.h"
#include		"nccs.h"

static UU_LIST trflst;
static int trfinit = 0;
typedef struct
{
	int ind;
	UM_transf trf;
} IG_trfrec;

/*********************************************************************
**    I_FUNCTION :  uio_trflst_init()
**       Initialize the transformation list.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
uio_trflst_init()
{

	if (trfinit)
	{
		trflst.cur_cnt = 0;
	}
	else
	{
		uu_list_init (&trflst, sizeof(IG_trfrec), 50, 50);
		trfinit = 1;
	}
}

/*********************************************************************
**    I_FUNCTION :  uio_mtrtomtr(transf,t)
**       Put unicad transformation matrix into a iges formed matrix
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_mtrtomtr(transf,t)
	UM_transf	transf;
	UU_REAL	t[];
	
	{
	int	i, j, k;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	k = 0;
	for (i=0; i<3; i++)
	  for (j=0; j<4; j++)
		 t[k++] = transf[j][i];
/*---
	um_vctovc(transf[0],&t[0]);
	um_vctovc(transf[1],&t[4]);
	um_vctovc(transf[2],&t[8]);
	t[3] = transf[3][0];
	t[7] = transf[3][1];
	t[11] = transf[3][2];
---*/
	}

/*********************************************************************
**    I_FUNCTION :  uio_tran(fd1,fd2,t,dcount,pcount)
**       Put the IGES generated transformation matrix information 
**       into the directory and parameter section
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_tran(fd1,fd2,t,dcount,pcount)
	int	fd1, fd2;
	UU_REAL	t[];
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	
	{
	struct  dir_rec dblk;		/* directory record */

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_trans"));	

	uio_tran_dir(&dblk);
	dblk.rel_type = GTRAN;
	dblk.par_ptr = *pcount;
	strcpy(dblk.label,"MATRIX");
	dblk.subsno = 0;                /*721vp*/

   uio_tranmat (fd1,fd2,t,dcount,pcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	
	uu_dexit;	
	}

/*********************************************************************
**    I_FUNCTION :  uio_ncltran(fd1,fd2,t,dcount,pcount)
**       Put the NCL transformation matrix information into the
**       directory and parameter section
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_ncltran(fd1,fd2,e,attr,dcount,pcount)
	int	fd1, fd2;
   struct NCL_matrix_rec  *e;
   struct UR_attr  *attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	
	{
	struct  dir_rec dblk;		/* directory record */
	UU_REAL	*t;
	struct  IG_igestran_rec	tranrec;	/* iges matrix record */
	int	i;
	int prop[2];

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_trans"));	

	uio_tran_dir(&dblk);
	dblk.rel_type = GTRAN;
	strcpy(dblk.label,"TRANS");   
	dblk.subsno = e->subscr;      

	uio_label (fd1,fd2,e->label,dcount,pcount);
/*
.....Pointer to parameter record must be stored after label is created because
.....pcount is updated to store label property - FSR 60061  IJD 29APR98
*/
	dblk.par_ptr = *pcount;

	tranrec.key = GTRAN;
	t = (UU_REAL *)e->mat;
	for (i=0; i<12; i++)
		tranrec.trans[i] = *t++;
	tranrec.no_bptr = 0;       /* no back pointers */
	tranrec.no_prop = 1;       /* name property */
	prop[0] = *dcount - 2;
	tranrec.prop = &prop[0];
	uio_put_para(GTRAN,&tranrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	
	uu_dexit;	
	}

/*********************************************************************
/*********************************************************************
**    I_FUNCTION :  uio_chk_matrix(transf,dcount)
**       Compare a matrix to a list of matrix, if they are not the same
**			add the matrix to the list, otherwise, return the index 
**			associated with the matrix
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_chk_matrix(transf,dcount)
	UM_transf	transf;
	int	dcount;

	{
	int	i, j, k, num;
	UU_REAL	*ptr1, *ptr2;
	UU_LOGICAL	unequal;
	IG_trfrec trfrec, *trfptr;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_chk_matrix,TRFnum=%d,dcount=%d",
								TRFnum,dcount));	

	ptr1 = transf[0];
	uu_dprint(UU_MTRC,(us,"transf=%g,%g,%g;%g,%g,%g;%g,%g,%g;%g,%g,%g",
	*ptr1++, *ptr1++, *ptr1++, *ptr1++, *ptr1++, *ptr1++, *ptr1++, *ptr1++,
	*ptr1++, *ptr1++, *ptr1++, *ptr1++));

	num = UU_LIST_LENGTH(&trflst);
	trfptr = (IG_trfrec *) UU_LIST_ARRAY (&trflst);
	for (i=0; i<num; i++)
	  {
		unequal = UU_FALSE;
		ptr1 = transf[0];
		ptr2 = (UU_REAL *)trfptr->trf;
		for (j=0; j<12; j++)
		if (fabs(*ptr1++ - *ptr2++) > UM_FUZZ)
			{
			 ptr1--; ptr2--;
			 uu_dprint(UU_MTRC,(us,"ptr=%g,%g",*ptr1,*ptr2));
			 unequal = UU_TRUE;
			 break;
			}
		if (!unequal)
		  {
			uu_dexit;
			return(trfptr->ind);
		  }
		trfptr++;
	  }

 	uu_dprint(UU_MTRC,(us,"after for loop"));
	ptr1 = transf[0];
	ptr2 = trfrec.trf[0];
	for (k=0; k<12; k++)
		*ptr2++ = *ptr1++;
	trfrec.ind = dcount;
	uu_list_push (&trflst, &trfrec);
	uu_dexit;
	return(-1);

	}

/*********************************************************************
**    I_FUNCTION :  uio_set_trf(e,trf)
**       Set the modeling transfomation matrix from the drafting 
**			construction plan information.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_set_trf(e,trf)
	struct UA_generic_draft		*e;
	UM_transf	trf;

	{

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter uio_set_trf"));

	um_vctovc((*e).cpln.xaxis,trf[0]);
	um_vctovc((*e).cpln.yaxis,trf[1]);
	um_vctovc((*e).cpln.zaxis,trf[2]);
	um_vctovc((*e).cpln.cpln_origin,trf[3]);
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION :  uio_tranmat (fd1,fd2,t,dcount,pcount)
**       Put the transformation matrix information into the directory
**			and parameter section
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_tranmat (fd1,fd2,t,dcount,pcount)
	int	fd1, fd2;
	UU_REAL	t[];
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	
	{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igestran_rec	tranrec;	/* iges point record */
	int	i;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_trans"));	

	tranrec.key = GTRAN;
	for (i=0; i<12; i++)
		tranrec.trans[i] = t[i];
								/* no back pointers */
	tranrec.no_bptr = 0;
								/* no properties */
	tranrec.no_prop = 0;
	uio_put_para(GTRAN,&tranrec,fd2,pcount,*dcount);
	uu_dexit;	
	}

