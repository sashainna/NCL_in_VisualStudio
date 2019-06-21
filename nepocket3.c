/*********************************************************************
**    NAME         :  nepocket3.c
**       CONTAINS:  Common block RRXY linked list operations:
**                  int rrini ()
**                  int rrput (ind,x,y,ier)
**                  int rrscal (start,end,factor,ier)
**                  int rrfind (k)
**                  int rrget (k,xx,yy,ier)
**                  int rrflip (k,nppr,ier)
**                  void rrplce (k,xx,yy,ier)
**
**    COPYRIGHT 1999 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nepocket3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:41
*********************************************************************/

#include "ncldef.h"

char *uu_lsinsrt(), *uu_lsend(), *uu_lsnext(), *uu_lsnew(), *uu_lsdel();
/*char *malloc();*/

struct rrxy
  {
  int index;
  double xrr;
  double yrr;
  };

static int CHUNK = 3000;
static int LEN = 3000*sizeof(struct rrxy);
static int lastind = 0;

char *rrlist = UU_NULL;
char *top = UU_NULL;

static UM_real8 flarc = 1.0e+25;

/*********************************************************************
**    E_FUNCTION     : int rrini ()
**       Initialize the linked list rrlist, which serves as a 
**       replacement of the common block RRXY arrays XRR, YRR in 
**       off11.f, off12.f, and razpok.f. 
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int rrini ()
   {
   int status;

   status = UU_SUCCESS;

   rrlist = uu_lsnew();
   lastind = 0;
   top = rrlist;

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int rrdel ()
**       Delete the linked list rrlist.     
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int rrdel ()
   {
   int status;

   status = UU_SUCCESS;

   uu_lsdel(rrlist);
   lastind = 0;
   top = UU_NULL;

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int rrput (ind,x,y,ier)
**       Store  into the linked list rrlist the next 2D-point with
**       the index. rrlist is organized as a list of linked big pieces,
**       each containing 2000 (encrypted in the parameter "CHUNK") 
**       units of the struct rrxy. A unit of rrxy is a 2-D point, plus
**       an index. We fill each "CHUNK" with 2000 points, then allocate 
**       the next "CHUNK".
**    PARAMETERS
**       INPUT  :
**          ind     - the element's index  
**          x       - the element's x-coordinate
**          y       - the element's y-coordinate
**       OUTPUT :
**          ier     - the error flag 
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int rrput (ind,x,y,ier)
UM_int4 *ind;
UM_real8 *x;
UM_real8 *y;
UM_int2 *ier;

   {
   int i, status;
   char *p1;
   struct rrxy *lst;

   status = UU_SUCCESS;
   *ier = 0;
   i = (*ind) % CHUNK;
   if (i == 0) i = CHUNK;

   status = rrfind(ind);
   if (status != UU_SUCCESS) 
     {
     p1 = uu_lsend(rrlist);
     if (p1 == UU_NULL) p1 = rrlist;
     if (p1 != UU_NULL) 
       {
       status = UU_SUCCESS;
       rrlist = uu_lsinsrt(rrlist,LEN);
       }
     else
       status = UU_FAILURE;
     }

   if (status == UU_SUCCESS) 
     {
     lst = (struct rrxy*) rrlist;
     lst = lst + i - 1;
     if (lst == UU_NULL)
        {
        status = UU_FAILURE;
        goto done;
        }
     lst->index=*ind;
     if (lst->index > lastind) lastind = lst->index;
     lst->xrr=*x;
     lst->yrr=*y;
     }

done:;

   if (status != UU_SUCCESS) *ier = 1;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int rrscal (start,end,factor,ier)
**       Scale the struct rrxy elements of the linked list rrlist by
**       the given factor. Those elements with indices between
**       "start" and "end" are scaled. 
**       Note: If the x-coordinate is a flag "flarc" (=1.e25, which
**       means "a circular arc"), the rules are: this element is not 
**       scaled; in the next element only the x-coordinates (radius)
**       is scaled, while the y-coordinate (+1/-1, for cclw/clw 
**       direction) is not scaled; in the next element both 
**       coordinates (giving the arc center) are scaled. 
**    PARAMETERS
**       INPUT  :
**          start   - the beginning index  
**          end     - the ending index  
**          factor  - the scaling multiple 
**       OUTPUT :
**          ier     - the error flag 
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int rrscal (start,end,factor,ier)
UM_int4 *start, *end;
UM_real4 *factor;
UM_int2 *ier;

   {
   int i, status;
   UM_int4 k;
   struct rrxy *lst;

   status = UU_SUCCESS;
   *ier = 0;
   if (*start >= *end) 
      {
      *ier = 1;
      goto done;
      }
   if (*end > lastind + 1) 
      {
      *ier = 1; return (UU_FAILURE);
      }

   k = *start;
   i = k % CHUNK;
   if (i == 0) i = CHUNK;
   status = rrfind(&k);
   if (status != UU_SUCCESS) goto done;
   lst = (struct rrxy*) rrlist;
   lst = lst + i - 1;
   if (lst == UU_NULL)
      {
      status = UU_FAILURE;
      goto done;
      }

   while (k<*end && *ier==0)
   {
   if (lst->index != k) 
      {
      *ier = 1;
      goto done;
      }
   if (lst->xrr >= 0.9*flarc)
      {
      k = k + 1;
      if (k%CHUNK == 1)
         {
         status = rrfind(&k);
         if (status != UU_SUCCESS) break; 
         lst = (struct rrxy*) rrlist;
         }
      else
         lst = lst + 1;
         if (lst == UU_NULL)
            {
            status = UU_FAILURE;
            goto done;
            }
      if (lst->index != k)
         {
         *ier = 1;
         goto done;
         }
      lst->xrr *= *factor;
      }
   else
      {
      lst->xrr *= *factor;
      lst->yrr *= *factor;
      }
   k = k + 1;
   if (k%CHUNK == 1)
      {
      status = rrfind(&k);
      if (status != UU_SUCCESS) break; 
      lst = (struct rrxy*) rrlist;
      }
   else
      lst = lst + 1;
      if (lst == UU_NULL)
        {
        status = UU_FAILURE;
        goto done;
        }
   }

done:;
   if (status != UU_SUCCESS) *ier = 1;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int rrfind (k)
**       Find a "CHUNK" piece of the linked list rrlist, which
**       contains a struct rrxy element with the index k. Set the 
**       rrlist pointer to the beginning of the found piece.
**       Note: the first element of a piece has an index of the form
**       1, 2001, 4001,... .
**    PARAMETERS
**       INPUT  :
**          k     - index to find 
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int rrfind (k)
UM_int4 *k;

   {
   int i, status;
   char *p = UU_NULL;
   struct rrxy *lst;

   status = UU_FAILURE;
   i = -1;

   p = top;
   while (p && status == UU_FAILURE)
     {
     p = uu_lsnext(p);
     if (p == UU_NULL) break;
     lst = (struct rrxy*) p;
     i = *k - lst->index;
     if (i>=0 && i<CHUNK) status = UU_SUCCESS;
     }

   if (status == UU_SUCCESS) rrlist = p;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int rrget (k,xx,yy,ier)
**       By the index k, find the corresponding 2-D point in the linked 
**       list rrlist.
**    PARAMETERS
**       INPUT  :
**          k      - the index of a point
**       OUTPUT :
**          xx     - the x-coordinate of a point
**          yy     - the y-coordinate of a point
**          ier    - the error flag 
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int rrget (k,xx,yy,ier)
UM_int4 *k;
UM_real8 *xx;
UM_real8 *yy;
UM_int4 *ier;

   {
   int status;
   struct rrxy *lst;
   int i;

   *ier = 0;
   *xx = 0.0;
   *yy = 0.0;
   if (*k > lastind) 
      {
      *ier = 8; return (UU_FAILURE);
      }

   i = (*k)%CHUNK;
   if (i == 0) i = CHUNK;

   status = rrfind(k);
   if (status != UU_SUCCESS) 
     {
     *ier = 8;
     }
   else 
     {
     lst = (struct rrxy*) rrlist;
     lst = lst + i - 1;
     if (lst == UU_NULL)
        {
        status = UU_FAILURE;
        goto done;
        }
     if (lst->index == *k)
        {
        *xx = lst->xrr;
        *yy = lst->yrr;
        }
     else 
        *ier = 8;
     }

done:;

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : void rrplce (k,xx,yy,ier)
**       Replace (overwrite) the data in the k-th record in the linked 
**       list rrlist.
**    PARAMETERS
**       INPUT  :
**          k      - the index of a point
**          xx     - the x-coordinate of a point
**          yy     - the y-coordinate of a point
**       OUTPUT :
**          ier    - the error flag 
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void rrplce (k,xx,yy,ier)
UM_int4 *k;
UM_real8 *xx;
UM_real8 *yy;
UM_int4 *ier;
{
   int status;
   struct rrxy *lst;
   int i;

	*ier = 0;
	if (*k > lastind) goto Err;
	i = (*k)%CHUNK;
	if (i == 0) i = CHUNK;

	status = rrfind(k);
	if (status != UU_SUCCESS) goto Err;

	lst = (struct rrxy*) rrlist;
	lst = lst + i - 1;
	if (lst == UU_NULL) goto Err;

	if (lst->index != *k) goto Err;

	lst->xrr = *xx;
	lst->yrr = *yy;
	return;
Err:
	*ier = 8;
	return;
}

/*********************************************************************
**    E_FUNCTION     : int rrflip_ (start,nppr,ier)
**       Flip a sequence of elements of the linked list rrlist, namely,
**       the nppr elements starting with the index start.
**    PARAMETERS
**       INPUT  :
**          start  - the first index of the sequence.
**          nppr   - the number of elements in the sequence.
**       OUTPUT :
**          ier    - the error flag 
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int rrflip (start,nppr,ier)
UM_int4 *start, *nppr;
UM_int2 *ier;

   {
   int end, i, j, k, n, status;
   struct rrxy *lst;
   double *x, *y, tx, ty;

   n = *nppr;
   x = y = UU_NULL;
   if (n <= 0 || *start + n > lastind+1) 
      {
      *ier = 1; return (UU_FAILURE);
      }
   x = (double*) malloc(sizeof(double)*(n+1));
   y = (double*) malloc(sizeof(double)*(n+1));
   *ier = 0;

   status = rrfind(start);
   if (status != UU_SUCCESS) 
     {
     *ier = 1;
     goto done;
     }

   k = *start;
   end = k + n;
   i = k % CHUNK;
   if (i == 0) i = CHUNK;
   status = rrfind(&k);
   if (status != UU_SUCCESS) goto done;
   lst = (struct rrxy*) rrlist;
   lst = lst + i - 1;
   if (lst == UU_NULL)
      {
      status = UU_FAILURE;
      goto done;
      }

   i = 0;
   x[0] = y[0] = flarc;

   while (k<end && *ier==0)
   {
   if (lst->index != k)
      {
      *ier = 1;
      goto done;
      }
   i++;
   x[i] = lst->xrr;
   y[i] = lst->yrr;
   k++;
   if (k%CHUNK == 1)
      {
      status = rrfind(&k);
      if (status != UU_SUCCESS) break;
      lst = (struct rrxy*) rrlist;
      }
   else
      lst = lst + 1;
      if (lst == UU_NULL)
        {
        status = UU_FAILURE;
        goto done;
        }
   }

     i = 0;
     while (i < n -3)
       {
       i++;
       if (x[i] >= 0.9*flarc)
         {
         tx = x[i];
         ty = y[i];
         x[i] = x[i+2];
         y[i] = y[i+2];
         x[i+2] = tx;
         y[i+2] = ty;
         y[i+1] = -y[i+1];
         i += 3;
         }
       }

       k = n/2;
       for (i=1; i <= k; i++)
          {
          j = n - i + 1;
          tx = x[i];
          ty = y[i];
          x[i] = x[j];
          y[i] = y[j];
          x[j] = tx;
          y[j] = ty;
          }

   k = *start;
   i = k % CHUNK;
   if (i == 0) i = CHUNK;
   status = rrfind(&k);
   lst = (struct rrxy*) rrlist;
   lst = lst + i - 1;

   i = 1;
   while (k<end && *ier==0)
   {
   lst->xrr = x[i];
   lst->yrr = y[i];
   i++;
   k++;
   if (k%CHUNK == 1)
      {
      status = rrfind(&k);
      if (status != UU_SUCCESS) break;
      lst = (struct rrxy*) rrlist;
      }
   else
      lst = lst + 1;
   }

done:;
   if (x != UU_NULL) free(x);
   if (y != UU_NULL) free(y);
   return (status);
   }
