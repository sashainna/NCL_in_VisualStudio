
/*********************************************************************
**    NAME         :  tioconv.h
**       CONTAINS:
**       units converting macros
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tioconv.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:55
*********************************************************************/

#ifndef UIO_CONV

/*******************MACROS ************************************/

#define	UIO_LEN_INTTOEXT(n1,n2)\
	{ n2 = n1 / uio_units;	}
#define	UIO_CC_INTTOEXT(v1,v2)\
	{ v2[0] = v1[0] / uio_units;\
	  v2[1] = v1[1] / uio_units;\
	  v2[2] = v1[2] / uio_units;  }

#define	UIO_CONV		
#endif
