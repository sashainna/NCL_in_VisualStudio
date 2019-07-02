
/********************************************************************* 
**  NAME:  stor.h
**
** 	Storage manager for variable size blocks of storage.
**   	All data and sizes are in terms of arrays of short integers.
**   	These includes designed to be independent of the implementation.
**   	All parameters are passed by address to be Fortran compatible.
**  	A storad is the address of a block of storage. The user should
**    never use it for anything but agruments to these storage routines
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       mstor.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:33
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

#ifndef MSTORH


#define umi_stormaxsiz 600         /* size of largest block of storage */
typedef short int stordata[umi_stormaxsiz];
typedef stordata *storad;
#define umi_storfilename(x) char x[20]
/*  The user-callable procedures are:
   umi_stinit(maxwds)--initialize storage system for at least maxwds.
     int *maxwds;
   umi_stread(ad,blk)--read the storage block pointed to by ad into blk.
     storad *ad; stordata blk;
   umi_stwrit(ad,blk)--write blk into the storage block pointed to by ad.
     storad *ad; stordata blk;
   umi_stnew(ad,len)--return in ad a pointer to new storage of length len.
     storad *ad; int *len;
   umi_stfree(ad)--uu_free the storage block pointed to by ad.
     storad *ad;
   stsave(fname)--save the storage system on file fname.
     umi_storfilename fname;
   stget(fname)--retrieve a storage system from file fname.
     umi_storfilename fname;
       Note that if stsave and stget are implemented, they must preserve
       a storad aumi_cross the stsave/stget.
   umi_stsnil(ad)--set ad to the nil pointer.
   umi_stadeq(ad1,ad2)--return 1 if ad1=ad2, else 0.
   umi_stnil(ad)--return 1 if ad is nil, else 0.
	umi_stadcp(adout,adin) -- copy adin to adout.
	storad *adout,adin;
*/

#define umi_stinit(maxwds) 
#define umi_stsnil(ad) *ad=0;
#define umi_stadeq(ad1,ad2) (*(ad1)==(*(ad2)))
#define umi_stnil(ad) (*(ad)==0)
#define umi_stnew(ad,len) *(ad)=(storad)(uu_malloc(2*(*(len))+2));(*(*(ad)))[0]=(*(len))
#define umi_stfree(ad) uu_free(*ad)
#define umi_stdone(x) 
#define umi_stadcp(adout,adin) *(adout)=(*(adin))
#define umi_stlen(ad) (*(*ad))[0]


#define MSTORH
#endif
