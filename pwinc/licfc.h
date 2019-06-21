/*********************************************************************
**    NAME         :  licfc.h
**       CONTAINS: redefinition of all FORTRAN/C interface routines
**
**    COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
**          All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       licfc.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:39
**
*********************************************************************/

#ifndef LICFC

#if !defined WNT && !defined DOS && !defined VAXVMS

#if !defined IBM && !defined HP
/***************************************************************************
       C ROUTINES CALLED BY FORTRAN 
***************************************************************************/
#define get_licbuf get_licbuf_
#define get_batbuf get_batbuf_
#define mfdisfld mfdisfld_
#define lic_mfprompt lic_mfprompt_
#define lic_mfmessag lic_mfmessag_

/***************************************************************************
      LICENSE FORTRAN ROUTINES CALLED BY C 
***************************************************************************/
#define init init_
#define autini autini_
#define mfopndat mfopndat_
#define mfbatchlic mfbatchlic_
#define clsfil clsfil_
#define clrrec clrrec_
#define addrec addrec_
#define delrec delrec_
#define search search_
#define putbuf putbuf_
#define setmotif setmotif_
#define setbatch setbatch_
#define getsparm getsparm_
#define savsparm savsparm_
#endif
#endif

#define LICFC
#endif
