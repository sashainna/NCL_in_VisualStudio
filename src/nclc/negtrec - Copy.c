/*********************************************************************
**    NAME         :  negtrec.c
**       CONTAINS:
**			ptopen(clfile,clpt)
**			ptclos(clfile)
**			ptpnum (nn)	
**			ptput1(clfile,icpt,iclw,rdata)
**			ptgetn(clfile,icpt,iclw,rdata)
**			ptgetp(clfile,icpt,iclw,rdata)
**			ang2dp(gv1,gv2,gv3)
**			ncl_zroptr(ptr)
**			ncl_tstptr(ptr,iflg)
**			ncl_setptr(ptr1,ptr2)
**			ncl_eqlptr(ptr1,ptr2,iflg)
**
**    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**        negtrec.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:08:35
*********************************************************************/

#include "ncldef.h"

int UN_gtrec, UN_gtlen, ptlen, cptr;
double *pd;
typedef struct 
   {
    UU_REAL gtpt[640];
   } GOTO_dat; 

GOTO_dat *UN_gtfirst, *UN_gtptr;

char *uu_lsnew(), *uu_lsnext(), *uu_lsprev(), *uu_lsinsrt(), *uu_lsend();

/*********************************************************************
**    E_FUNCTION     : ptopen(ptr,ptyp)
**       FORTRAN callable routine to initialize GOTO record.
**    PARAMETERS   
**       INPUT  : 
**          ptyp    = Point occupancy (3 without multax, 6 with).
**       OUTPUT :  
**          ptr     = Pointer to next record in internal clfile.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ptopen(ptr,ptyp)
GOTO_dat **ptr;
UM_int4 *ptyp;
{
/*
.....Create a GOTO list to store
.....all GOTO points
*/
	ptclos();
	UN_gtptr = (GOTO_dat *)uu_lsnew();
/*
.....Could not allocate list
*/
	if (UN_gtptr == 0)
	{
/* 		ud_wrerr("Could not allocate memory for internal GOTO record."); */
		goto done;
	}
/*
.....Initialize number of points in GOTO record
*/
	UN_gtlen = 0;
	UN_gtfirst = UN_gtptr;
	UN_gtrec = 1;
 cptr  = 0;
 ptlen = *ptyp;
	pd = (double *)uu_lsinsrt(UN_gtptr,sizeof(GOTO_dat));
	UN_gtptr = (GOTO_dat *)pd;
done:;
    *ptr = UN_gtptr;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ptpnum(num)
**       FORTRAN callable routine to initialize GOTO record.
**    PARAMETERS   
**       INPUT  : 
**          none 
**       OUTPUT :  
**          num     = Number of points in GO statement. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ptpnum(num)
UM_int4 *num;
{
/*
.....Get number of saved points in GOTO. 
*/
	*num = UN_gtlen;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ptclos()
**       FORTRAN callable routine to close and delete an internal
**       clfile.
**    PARAMETERS   
**       INPUT  : 
**          clfile  = 1 = Close primary clfile.  2 = Secondary.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ptclos()
{
/*
.....Delete the GOTO record list
*/
	if (UN_gtfirst != 0) uu_lsdel(UN_gtfirst);
	UN_gtptr = 0;
	UN_gtfirst = 0;
	UN_gtrec = 0;
	UN_gtlen = 0;
 return(0);
}

/*********************************************************************
**    E_FUNCTION     : ptput1(icpt,numpt,rdata,ptyp,maxpt)
**       FORTRAN callable routine to store data in an internal clfile.
**    PARAMETERS   
**       INPUT  : 
**			icpt    = Pointer inside GOTO record to store first point.
**			numpt   = number of points to store.
**			rdata   = GOTO points data.
**			ptyp    = flag - 3 = point coordinates., 6 = pointvector coordinates.
**			maxpt   = maximum size of point buf
**       OUTPUT :  
**   icpt    = Pointer to next available point in GOTO record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ptput1(icpt,numpt,rdata,ptyp,maxpt)
GOTO_dat **icpt;
UM_int4 *numpt,*ptyp, *maxpt;
UM_real8 rdata[];
{
	int i,j,inc;
/*
.....Clfile is not open
*/
	if (UN_gtptr == 0) ptopen (icpt,ptyp);
/*
.....Increment cl record counter
*/
 inc  = 0;
 for (i=0;i<*numpt;i++)
  {
   if (cptr == *maxpt / *ptyp)
/*
......Allocate next chunk of memory
*/
     {
	    pd = (double *)uu_lsinsrt(UN_gtptr,sizeof(double)*(*maxpt));
	    UN_gtptr = (GOTO_dat *)pd;
     if (UN_gtptr == 0)
       {
/*        ud_wrerr("Could not allocate memory for GOTO data."); */
       goto done;
       }
     UN_gtrec++;
     cptr   = 0;
     }
/*
......Save data array
*/
   for (j=0; j<ptlen; j++) *pd++ = rdata[inc+j];
   inc = inc + ptlen;
   UN_gtlen++;
   cptr++;
		}
 done:;
	return;
}
/*********************************************************************
**    E_FUNCTION     : ptgetn(icpt,numpt,rdata, maxpt)
**       FORTRAN callable routine to get GOTO points from the beginning
**       of internal record. Points are retrieved in groups of
**       40 or 20 if applicable exept the last group which can be
**       smaller.
**    PARAMETERS   
**       INPUT  : 
**			icpt    = Pointer inside GOTO record to get first chunk. Can't
**             be changed outside this routine.  To set icpt call
**             ptrbeg and subsequently ptgetn.
**       OUTPUT :  
**			rdata   = GOTO points data.
**   numpt   = Number of points in rdata.
**   icpt    = Pointer to next available chunk in GOTO record or 0
**             if end of list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ptgetn(icpt,numpt,rdata,maxpt,snu,sm1p)
GOTO_dat **icpt;
UM_int4 *numpt, *maxpt, *snu, *sm1p;
UM_real8 rdata[];
{
	int i,j,nr,inc,kr;
	GOTO_dat *ps;

	int smaxp, ssnu, ssm1p;
	smaxp=*maxpt;
	ssnu = *snu;
	ssm1p = *sm1p;
/*
.....Check point index to define memory chunk
*/
 if (*icpt == 0) 
   {
    ps   = (GOTO_dat *)uu_lsnext (UN_gtfirst);
    cptr = 0;
   }
 else
    ps   = (GOTO_dat *)uu_lsnext (*icpt);
 if (ps == 0) goto failed;
 pd    = (double *)ps;
 nr    = *maxpt / ptlen;
 kr    = cptr + nr;
 if (kr > UN_gtlen) nr = nr - kr + UN_gtlen; 
/*
.....Retrieve GOTO data
*/
 inc   = 0;
 for (i=0; i<nr; i++)
   {
   for (j=0; j<ptlen; j++) rdata[inc+j] = *pd++;
   *maxpt = smaxp;
   *snu = ssnu;
   *sm1p = ssm1p;
   inc = inc + ptlen;
   cptr++;
   }
 *numpt = nr;
/*
.....Move pointer to used chunk
*/
	*icpt = ps;
	goto done;
/*
.....Failure
.....Return EOF
*/
failed:;
	*icpt = 0;
 *numpt = 0;
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ptgetp(icpt,numpt,rdata)
**       FORTRAN callable routine to get GOTO points from the
**       end of internal record. Points are retrieved in groups of
**       40 or 20 if applicable exept the first group which can be
**       smaller.
**    PARAMETERS   
**       INPUT  : 
**			icpt    = Pointer inside GOTO record to get first chunk. Can't
**             be changed outside this routine.  To set icpt call
**             ptrlst and subsequent ptgetp.
**			rdata   = GOTO points data.
**       OUTPUT :  
**   numpt   = Number of points in rdata.
**   icpt    = Pointer to next available chunk in GOTO record or 0
**             if end of list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ptgetp(icpt,numpt,rdata,maxpt,snu,sm1p)
GOTO_dat **icpt;
UM_int4 *numpt,*maxpt, *snu, *sm1p;
UM_real8 rdata[];
{
	int i,j,nr,inc,kr;
	GOTO_dat *ps;
	int smaxp, ssnu, ssm1p;
	smaxp=*maxpt;
	ssnu = *snu;
	ssm1p = *sm1p;
/*
.....Check record index to define memory chunk
*/
 if (*icpt == 0) 
   {
    ps   = (GOTO_dat *)uu_lsend (UN_gtfirst);
    cptr = UN_gtlen;
   }
 else
    ps   = (GOTO_dat *)uu_lsprev (*icpt);
 if (ps == 0) goto failed;
 pd    = (double *)ps;
 nr    = *maxpt / ptlen;
 if (cptr == UN_gtlen) 
   {
    kr = cptr / nr;
    kr = cptr - kr * nr;
    if (kr != 0) nr = kr;
   }
/*
.....Retrieve cl data
*/
 inc   = 0;
 for (i=0; i<nr; i++)
   {
   for (j=0; j<ptlen; j++) rdata[inc+j] = *pd++;
   *maxpt = smaxp;
   *snu = ssnu;
   *sm1p = ssm1p;
   inc = inc + ptlen;
   cptr--;
   }
 *numpt = nr;
/*
.....Move pointer to used chunk 
*/
	*icpt = ps;
	goto done;
/*
.....Failure
.....Return EOF
*/
failed:;
	*icpt = 0;
 *numpt = 0;
done:;
	return;
}
/*********************************************************************
**    E_FUNCTION     : ang2dp(gv1,gv2,gv3)
**       FORTRAN callable routine to get angle between two vectors 
**       using UNICADD call. 
**    PARAMETERS
**       INPUT  :
**   gv1(3)    = The first vector on Z plane from which angle starts. 
**   gv2(3)    = The second vector on Z plane where angle ends. 
**   gv3(3)    = Z plane vector. 
**       OUTPUT :
**             none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL
ang2dp(gv1,gv2,gv3)
 UM_vector gv1;
 UM_vector gv2;
 UM_vector gv3;
{
 UM_angle ang;
 UM_vector v1;
 UM_vector v2;
 um_unitvc(gv1,v1); 
 um_unitvc(gv2,v2); 
 ang = um_angle2p(v1,v2,gv3);
 return (ang);
}
/*********************************************************************
**    E_FUNCTION     : ncl_zroptr(ptr)
**       FORTRAN callable routine to zero a C pointer.
**    PARAMETERS
**       INPUT  :
**           ptr    - Pointer to sero.
**       OUTPUT :
**           ptr    - Zeroed pointer.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_zroptr(ptr)
char **ptr;
{
	*ptr = 0;
	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : ncl_tstptr(ptr,iflg)
**       FORTRAN callable routine to test if a C pointer is NULL.
**    PARAMETERS
**       INPUT  :
**           ptr    - Pointer to test.
**       OUTPUT :
**           iflg   - =0 if pointer is NULL, =1 otherwise.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_tstptr(ptr,iflg)
char **ptr;
UM_int2 *iflg;
{
	*iflg = 0;
	if (*ptr) *iflg = 1;
	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : ncl_setptr(ptr1,ptr2)
**       FORTRAN callable routine to set one C pointer to another.
**    PARAMETERS
**       INPUT  :
**           ptr1   - Input pointer.
**       OUTPUT :
**           ptr2   - Ouput pointer.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_setptr(ptr1, ptr2)
char **ptr1, **ptr2;
{
	*ptr2 = *ptr1;
	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : ncl_eqlptr(ptr1,ptr2,iflg)
**       FORTRAN callable routine to test if two C pointers are equal.
**    PARAMETERS
**       INPUT  :
**           ptr1   - First pointer to test.
**           ptr2   - Second pointer to test.
**       OUTPUT :
**           iflg   - =1 if pointers are equal, =0 otherwise.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_eqlptr(ptr1,ptr2,iflg)
char **ptr1, **ptr2;
UM_int2 *iflg;
{
	*iflg = 0;
	if (*ptr1 == *ptr2) *iflg = 1;
	return (UU_SUCCESS);
}
