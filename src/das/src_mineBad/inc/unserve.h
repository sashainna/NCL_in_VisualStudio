/*********************************************************************
**
**    NAME         :  unserve.h
**
**				Contains the ID numbers of the Unicad system number servers
**
**			Number servers:
**
**			Generic number server, after being initialized by a call
**			to uu_nserv_init, calls to uu_nserve_req will return an
**			unused number between max and min, inclusive.  Numbers
**			that are no longer needed are returned with uu_nserv_ret.
**			The number server can be reset by a call to uu_nserv_reset
**			The number server can be deleted by a call to uu_nserv_del.
**			A specific number can be reserved by a call to uu_nserv_resv.
**
**			The number server is implemented as a bit table as is therefore
**			unsuitable for huge spans of numbers.
**
**			ROUTINES: (unserve.c)
**				int uu_nserv_init(min, max, id);	initialize a number server
**				int uu_nserv_del(id);				delete a number server
**				int uu_nserv_req(id);				request a number from a server
**				int uu_nserv_ret(id, number);		return a number to a server
**				int uu_nserv_resv(id, number);		reserve a specific number from a
**															server
**				int uu_nserv_reset(id);				reset a number server
**
**    PARAMETERS   
**
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**
**   MODULE NAME AND RELEASE LEVEL 
**       unserve.h , 25.1
**      unserve.h, 3.3
**
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:05
**      6/10/87, 15:32:48
**
**************************************************************************/

#ifndef UNSERVEH


#define UU_CHOICE_NM 	0	/* choice device names (numbers)							*/
#define UU_STROKE_NM		1	/* stroke device names (numbers)							*/
#define UU_STRING_NM		2	/* string device names (numbers)							*/
#define UU_VALUATOR_NM	3	/* valuator device names (numbers)						*/
#define UU_PICK_NM		4	/* pick device names (numbers)							*/
#define UU_XFORM_NM		5	/* norm transformation names (numbers)					*/
#define UU_LDU_NM			6	/* xio logical device names (numbers)					*/
#define UU_APPL_NM1     7  /* For application use										*/
#define UU_APPL_NM2     8  /* For application use										*/
#define UU_APPL_NM3     9  /* For application use										*/
#define UU_OT_NM	      10 /* udos open file table index values					*/

#define UNSERVEH
#endif
