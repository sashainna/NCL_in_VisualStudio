/*********************************************************************
**    NAME         :  m7mathmx.c
**		CONTAINS:
**			um_mxplmx(m1,m2,mr)
**			um_mxmnmx(m1,m2,mr)
**			um_mxtmmx(m1,m2,mr)
**			um_rtmmx(rnum,m1,mr)
**			um_eval_matrix(indic,eps,a,x,deter)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m7mathmx.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:10
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdcoord.h"
#include "mdmatrix.h"
#include "modef.h"

/*********************************************************************
**    E_FUNCTION     : um_mxplmx(m1,m2,mr)
**      Add two matrices.
**    PARAMETERS   
**       INPUT  : 
**				m1                first  matrix
**          m2                second  matrix
**       OUTPUT :  
**				mr                resultant  matrix
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_mxplmx(m1,m2,mr)
	struct  UM_matrix  *m1;
	struct  UM_matrix  *m2;
	struct  UM_matrix  *mr;

	{
	int ij;					/* index */

	if ( (m1->nrow  ==  m2->nrow) && (m1->ncol  ==  m2->ncol) )
		{
		mr->nrow = m1->nrow;
		mr->ncol = m1->ncol;
		for (ij = 0; ij < (mr->nrow *mr->ncol); ij++) 
			mr->ary[ij] = m1->ary[ij] + m2->ary[ij];
		}
	}
/*********************************************************************
**    E_FUNCTION     : um_mxmnmx(m1,m2,mr)
**      Subtract two matrices.
**    PARAMETERS   
**       INPUT  : 
**				m1                first  matrix
**          m2                second  matrix
**       OUTPUT :  
**				mr                resultant  matrix
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_mxmnmx(m1,m2,mr)
	struct  UM_matrix  *m1;
	struct  UM_matrix  *m2;
	struct  UM_matrix  *mr;

	{
	int ij;					/* index */

	if ( (m1->nrow  ==  m2->nrow) && (m1->ncol  ==  m2->ncol) )
		{
		mr->nrow = m1->nrow;
		mr->ncol = m1->ncol;
		for (ij = 0; ij < (mr->nrow *mr->ncol); ij++) 
			mr->ary[ij] = m1->ary[ij] - m2->ary[ij];
		}
	}
/*********************************************************************
**    E_FUNCTION     : um_mxtmmx(m1,m2,mr)
**       Multiply matrix m1 by matrix m2 and return the result.
**    PARAMETERS   
**       INPUT  : 
**				m1                first matrix
**          m2                second matrix
**       OUTPUT :  
**				mr                resultant matrix
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_mxtmmx(m1,m2,mr)
	struct  UM_matrix  *m1;
	struct  UM_matrix  *m2;
	struct  UM_matrix  *mr;

	{
	int i, ii, ij, ik, j, k, kj;				/* indicies */

	if (m1->ncol  ==  m2->nrow)
		{
		ii = 0;
		ij = 0;
		for (i = 0; i < m1->nrow; i++,  ii =  ii +  m1->ncol)
			{
			for (j = 0; j < m2->ncol; j++,  ij++)
				{
				ik = ii;
				kj = j;
				mr->ary[ij] = 0;
				for (k = 0; k < m1->ncol; k++,  kj =  kj + m2->ncol)
	mr->ary[ij]  = mr->ary[ij] + (m1->ary[ik++] *m2->ary[kj]);
				}
			}
		}
	}
/*********************************************************************
**    E_FUNCTION     : um_rtmmx(rnum,m1,mr)
**       Multiply a  matrix by a scalar.
**    PARAMETERS   
**       INPUT  : 
**				m1               matrix
**          rnum            scalar
**       OUTPUT :  
**				mr              resultant  matrix
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_rtmmx(rnum,m1,mr)
	UU_REAL rnum;
	struct  UM_matrix  *m1;
	struct  UM_matrix  *mr;

	{
	int ij;					/* index */

	for (ij = 0; ij < (m1->nrow *m1->ncol); ij++)
		mr->ary[ij] = rnum *m1->ary[ij];
	}
/*********************************************************************
**    E_FUNCTION     : um_eval_matrix(indic,eps,a,x,deter)
**      This routine will invert an NxN matrix,  solve a system of N simultaneous
**      equations, and calculate the determinant of an NxN matrix.
**    PARAMETERS   
**       INPUT  : 
**				indic switch:
**                    UM_NVERT : compute inverse of a in place;
**                    UM_SOLVANDINVERT:  solve system of linear 
**                        equations ax=b then compute inverse of
**                        a in place;
**                    UM_SOLVEQNS    solve system of linear 
**                        equations ax=b but don't compute inverse.
**				eps    lower limit on magnitude of pivot;
**				a      ptr to an NxN  matrix or an Nx(N+1) augmented 
**                    matrix;
**				x    	 solutions to system of equations;
**				b      right hand side of equation;
**       OUTPUT :  
**				a 		 inverse of input  matrix (indic=INVERT,SOLVANDINVERT)
**				x 		 solutions of linear equations (indic=SOLVANDINVERT,SOLVEQNS)
**				deter  determinant
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_eval_matrix(indic,eps,a,x,deter)
        int indic;
        UU_REAL eps;
        struct  UM_matrix  *a;
        UU_REAL  x[][4];
        UU_REAL  *deter;

{
 int max,n,l,k,ij,i,iscan,irow[100],jcol[100],jord[100],jtemp,intch,ip1;
 int iden;
 int non_singular,ok_to_pivot,j,irowk,jcolk,jscan,ik,true,false;
 UU_REAL pivot,aijck,y[100],one;
          
        /*  start of executable code */
          
			 /*
 uu_denter( UU_MTRC,(us,"um_eval_matrix(%d,%g,%x,%x,%x)",indic,eps,a,x,deter));
 */
 n = a->nrow;
 max = n;
 if((indic ==  UM_SOLVANDINVERT) || (indic ==  UM_SOLVEQNS)) max = a->ncol;
 iden = max - n;
 true = 1;
 false = 0;
 one = 1.0;
 *deter = 1.0;
 non_singular = true;
          
        /*  initilize row/col arrays */
          
 for (k=0;k<n;k++)
	{
	irow[k] = -1;
	jcol[k] = -1;
	}
                  
        /* begin elimination procedure */
          
 for (k = 0; (k < n && non_singular); k++)
  {
        /* search for the pivot element */
		pivot = 0.0;
		ij = 0;
                  
        /* scan rows and columns for largest pivot element */
          
		for (i = 0; i < n; i++)
		 {
			ok_to_pivot = true;
			for (iscan = 0; (iscan < k && ok_to_pivot); iscan++) 
					ok_to_pivot = (i != irow[iscan]);
			if (ok_to_pivot != 1) ij  += max;
			else
			 {
				for (j = 0; j < n; j++, ij++)
				 {
					ok_to_pivot = true;
					for (jscan = 0; (jscan < k && ok_to_pivot); jscan++) 
					 {
						ok_to_pivot = (j != jcol[jscan]);
					 }
					if ( ok_to_pivot == 1)
						if(fabs((a->ary)[ij]) > fabs(pivot))
						 {
							pivot = (a->ary)[ij];
							irow[k] = i;
							jcol[k] = j;
						 }
						else	break;
				 }
				ij = (i+1)*max;
			 }
		 }
                         
        /* check that pivot is larger than eps */
          
		if (fabs(pivot) < eps)
		 {
			non_singular = false;
			*deter = 0.0;
			/*
			uu_dexit;
			*/
			return( UM_ILLCONDITIONED);
		 }
		else
		 {
        /* update determinant */
         
			*deter = *deter*pivot;
			irowk = irow[k];
			jcolk = jcol[k];
                         
        /* normalize pivot row elements */
          
			ij = irowk  * max;
			for (j = 0; j < max; j++, ij++) 
				(a->ary)[ij]  =  (a->ary)[ij] / pivot;
		 }
        /*carry out elimination and develop inverse */
         
		ij = irowk*max + jcolk;
		(a->ary)[ij] = one/pivot;
		for (i=0;i<n ;i++)
		 {
			if(i != irowk)
			 {
				ij = i*max + jcolk;
				aijck = (a->ary)[ij];
				(a->ary)[ij] = -aijck/pivot;
				ij = i*max;
				ik = irowk*max;
				for (j=0;j<max ;j++,ij++,ik++)
					if(j != jcolk)
						(a->ary)[ij] -=  aijck*(a->ary)[ik];
			 }
		 }
	}
        /* determine solution order */
         
 for (k=0;k<n;k++)
  {
		irowk = irow[k];
		jcolk = jcol[k];
		jord[irowk] = jcolk;
		ij = irowk*max + n;
 		if ((indic ==  UM_SOLVANDINVERT) || (indic ==  UM_SOLVEQNS)) 
			{
			for(l=0,ik=ij;l<iden;l++,ik++)
				{
				x[jcolk][l] = (a->ary)[ik];
				}
			}
  }
                         
        /* determine sign of the determinant */
         
 intch = 0;
 for (i=0;i<n-1;i++)
  {
	for (j=i+1;j<n ;j++)
	 {
		if(jord[j] >= jord[1])
		 {
			jtemp = jord[j];
			jord[j] = jord[i];
			jord[i] = jtemp;
			intch = intch + 1;
		 }
	 }
  }
 if ((intch%=2) != intch) *deter = -*deter;
                 
        /* check if inverse required */
         
 if(indic ==  UM_SOLVEQNS) 
 {
 /*
	uu_dexit;
	*/
	return( UM_WELLCONDITIONED);
 }
                 
        /* yes - unscramble the inverse */
         
 for (j=0;j<n;j++)
 {
	for (i=0;i<n;i++)
	 {
		irowk = irow[i];
		jcolk = jcol[i];
		ij = irowk*max + j;
		y[jcolk] = (a->ary)[ij];
	 }
 }
 for (i=0;i<n;i++)
 {
	ij = i*max;
	for (j=0;j<n;j++)
	 {
		irowk = irow[j];
		jcolk = jcol[j];
		y[irowk] = (a->ary)[ij+jcolk];
	 }
	for (j=0;j<n;j++,ij++)
	 {
		(a->ary)[ij] = y[j];
	 }
 }
 /*
 uu_dexit;
 */
 return( UM_WELLCONDITIONED);
}
