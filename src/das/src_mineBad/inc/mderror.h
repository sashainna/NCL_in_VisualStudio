/*********************************************************************
**    NAME         :  mderror.h
**       CONTAINS:
**       modeling error codes
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       mderror.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:29
*********************************************************************/

#ifndef MDERRORH


#define	UM_OK					0		/* function returned OK	*/
#define	UM_FAILURE			-1		/* general bad error		*/
#define	UM_BADCLASS			-2		/* entity class not appropriate to operation	*/
#define	UM_NOTPLANAR		-3		/* entities not (co)planar	*/
#define	UM_NOTLINEAR		-4		/* entities not (co)linear	*/
#define	UM_BUFFULL			-5		/* more data produced than space to store	it */
#define	UM_BADENTITY		-6		/* unknown relation number, couldn't get geom*/
#define	UM_BADTFMAT			-7		/* couldn't get transformation 	*/
#define	UM_UNIMPLEMENTED	-8		/* function not implemented (for entity type)*/
#define	UM_OUTOFRANGE		-9		/* (parameter) out of range	*/
#define	UM_TOOSMALL			-10	/* object is too small to manipulate, eg.  um_unitvc()	*/

#define MDERRORH
#endif
