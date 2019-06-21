/***********************************************************************
c
c   FILE NAME: memory.c
c   CONTAINS:
c               cmall   cmdea   cldmem  clodi2  clodi4  clodr8  cstmem
c               cstoi2  cstoi4  cstor8
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        memory.c , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:19
c
***********************************************************************/
#ifndef WNT 
#if !defined IBM && !defined HP
#define cmall cmall_
#define cmdea cmdea_
#define cldmem cldmem_
#define clodi2 clodi2_
#define clodi4 clodi4_
#define clodr8 clodr8_
#define cstmem cstmem_
#define cstoi2 cstoi2_
#define cstoi4 cstoi4_
#define cstor8 cstor8_
#endif
#endif
/***********************************************************************
c
c   SUBROUTINE:  cmall (ksiz,kpt)
c
c   FUNCTION:  This routine allocates a chunk of memory.
c
c   INPUT:  ksiz    I*4  D1    -  Amount of memory to allocate.
c
c   OUTPUT: kpt     I*4        -  Returns the pointer to the chunk of
c                                 memory allocated.  Returns 0 if the
c                                 allocation request failed.
c
***********************************************************************/
 
cmall(ksiz,kpt)
int *ksiz,*kpt;
{
/*
...Allocate a chunk of memory
*/
	*kpt = malloc(*ksiz);
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  cmdea (kpt)
c
c   FUNCTION:  This routine deallocates a chunk of memory.
c
c   INPUT:  kpt     I*4  D1    -  Pointer to memory to deallocate.
c
c   OUTPUT: none.
c
***********************************************************************/

cmdea(kpt)
int *kpt;
{
/*
.....one pointer only can free once
.....Yurong changed 5/15/98
*/
/*	free(*kpt); */
	if (*kpt!=0)
	{
		free(*kpt);
		*kpt = 0;
	}
	return;
}
/***********************************************************************
c
c   SUBROUTINE:  cldmem (kdat,kpt,kdest)
c
c   FUNCTION:  This routine loads a page of memory to a data buffer.
c
c   INPUT:  kpt     I*4  D1    -  Section of memory to read from.
c
c           kdest   I*4  Dn    -  Allocated memory pointer.
c
c   OUTPUT: kdat    I*4  D128  -  Data read from memory.
c
***********************************************************************/

cldmem(kdat,kpt,kdest)
int kdat[128],*kpt,**kdest;
{
	int *pt,i;
	pt = *kdest + *kpt - 1;
	for (i=0;i<128;i++)
	{
		kdat[i] = *pt++;
	}
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  clodi2 (kmem,kdat,kst,ken)
c
c   FUNCTION:  This routine loads a section of allocated memory into an
c              I*2 array.
c
c   INPUT:  kmem    I*4  Dn    -  Allocated memory pointer.
c
c           kst     I*4  Dn    -  Start of memory to load.
c
c           ken     I*4  Dn    -  End of memory to load.
c
c   OUTPUT: kdat    I*2  D256  -  Data to receive I*2 data.
c
***********************************************************************/
 
clodi2(kmem,kdat,kst,ken)
int *kst,*ken;
short **kmem,kdat[256];
{
        int i,inc;
        short *pt;
/*
...Load the requested portion of memory
*/
	pt = *kmem + *kst - 1;
	inc = 0;
        for (i=(*kst)-1;i<*ken;i++)
        {
                kdat[inc] = *pt++;
		inc = inc + 1;
        }
        return;
}        
 
/***********************************************************************
c
c   SUBROUTINE:  clodi4 (kmem,kdat,kst,ken)
c 
c   FUNCTION:  This routine loads a section of allocated memory into an
c              I*4 array.
c 
c   INPUT:  kmem    I*4  Dn    -  Allocated memory pointer.
c
c           kst     I*4  Dn    -  Start of memory to load.
c 
c           ken     I*4  Dn    -  End of memory to load. 
c 
c   OUTPUT: kdat    I*4  D128  -  Data to receive I*4 data. 
c 
***********************************************************************/ 
  
clodi4(kmem,kdat,kst,ken)
int **kmem,*kst,*ken,kdat[256];
{ 
        int i,*pt,inc; 
/* 
...Load the requested portion of memory 
*/ 
	pt = *kmem + *kst - 1;
        inc = 0;   
        for (i=(*kst)-1;i<*ken;i++) 
        { 
                kdat[inc] = *pt++;
                inc = inc + 1;
        }
        return;  
}

/***********************************************************************
c
c   SUBROUTINE:  clodr8 (kmem,kdat,kst,ken)
c
c   FUNCTION:  This routine loads a section of allocated memory into an
c              R*8 array.
c
c   INPUT:  kmem    I*4  Dn    -  Allocated memory pointer.
c
c           kst     I*4  Dn    -  Start of memory to load.
c
c           ken     I*4  Dn    -  End of memory to load.
c
c   OUTPUT: gdat    R*8  D64   -  Data to receive R*8 data.
c
***********************************************************************/
 
clodr8(kmem,gdat,kst,ken)
int *kst,*ken;
double **kmem,gdat[64];
{
        int i,inc;
	double *pt;
/*
...Load the requested portion of memory
*/
	pt = *kmem + *kst - 1;
        inc = 0;
        for (i=(*kst)-1;i<*ken;i++)
        {
                gdat[inc] = *pt++;
                inc = inc + 1;
        }
        return; 
}

/***********************************************************************
c
c   SUBROUTINE:  cstmem (kdat,kpt,kdest)
c
c   FUNCTION:  This routine loads a data buffer to a page of memory.
c
c   INPUT:  kpt     I*4  D1    -  Section of memory to write to.
c
c           kdat    I*4  D128  -  Data to store in memory.
c
c           kdest   I*4  Dn    -  Allocated memory pointer.
c
c   OUTPUT: none.
c
***********************************************************************/
 
cstmem(kdat,kpt,kdest)
int kdat[128],*kpt,**kdest;
{
        int *pt,i; 
        pt = *kdest + *kpt - 1;
        for (i=0;i<128;i++)
        {
                *pt++ = kdat[i];
        }
        return;
}

/***********************************************************************
c
c   SUBROUTINE:  cstoi2 (kmem,kdat,kst,ken)
c
c   FUNCTION:  This routine stores an I*2 array in a  section of allocat-
c              ed memory.
c
c   INPUT:  kmem    I*4  Dn    -  Allocated memory pointer.
c
c           kdat    I*2  D256  -  I*2 buffer to store in memory.
c
c           kst     I*4  Dn    -  Start of memory storage.
c
c           ken     I*4  Dn    -  End of memory storage.
c
c   OUTPUT: none.
c
***********************************************************************/
 
cstoi2(kmem,kdat,kst,ken)
int *kst,*ken;
short **kmem,kdat[256];
{
        int i,inc;
        short *pt;
/*
...Store the data array in memory
*/
        pt = *kmem + *kst - 1;
        inc = 0;
        for (i=(*kst)-1;i<*ken;i++)
        {
                *pt++ = kdat[inc];
                inc = inc + 1;
        }
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  cstoi4 (kmem,kdat,kst,ken)
c
c   FUNCTION:  This routine stores an I*4 array in a  section of allocat-
c              ed memory.
c
c   INPUT:  kmem    I*4  Dn    -  Allocated memory pointer.
c
c           kdat    I*4  D128  -  I*4 buffer to store in memory.
c 
c           kst     I*4  Dn    -  Start of memory storage.
c
c           ken     I*4  Dn    -  End of memory storage. 
c
c   OUTPUT: none.
c
***********************************************************************/
 
cstoi4(kmem,kdat,kst,ken)
int **kmem,*kst,*ken,kdat[256];
{
        int i,*pt,inc;
/*
...Store the data array in memory
*/
        pt = *kmem + *kst - 1;
        inc = 0;
        for (i=(*kst)-1;i<*ken;i++)
        {
                *pt++ = kdat[inc];
                inc = inc + 1;
        }
        return;
}

/***********************************************************************
c
c   SUBROUTINE:  cstor8 (kmem,gdat,kst,ken)
c
c   FUNCTION:  This routine stores an R*8 array in a  section of allocat-
c              ed memory.
c
c   INPUT:  kmem    I*4  Dn    -  Allocated memory pointer.
c
c           gdat    R*8  D64   -  R*8 buffer to store in memory.
c 
c           kst     I*4  Dn    -  Start of memory storage.
c
c           ken     I*4  Dn    -  End of memory storage.
c
c   OUTPUT: none.
c
***********************************************************************/
 
cstor8(kmem,gdat,kst,ken)
int *kst,*ken;
double **kmem,gdat[64];
{
        int i,inc;
        double *pt;
/*
...Store the data array in memory
*/
        pt = *kmem + *kst - 1;
        inc = 0;
        for (i=(*kst)-1;i<*ken;i++)
        {
                *pt++ = gdat[inc];
                inc = inc + 1;
        }
        return;
}
