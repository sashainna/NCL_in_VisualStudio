/*********************************************************************
**    NAME         : vsegbf.h 
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       vsegbf.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:07
*********************************************************************/

#ifndef VSEGBFH


#include "usysdef.h"
#include "gtbldef.h"

/*	-------- User Data Macros ---------- */

#define  uv_segbuff(buff)		long buff[UG_UDATA_SIZE]
#define	uv_getsegid(buff)		(buff[0] >> 8)
#define	uv_getrelnum(buff)	(buff[0] & 0x00FF)
#define	uv_getkey(buff)		((UU_KEY_ID) buff[1])
#define  uv_getlayer(buff)  	(buff[2] >> 16)
#define  uv_getcolor(buff)		(buff[2] & 0xFFFF)
#define  uv_getlstyle(buff)	((buff[3] >> 16) & 0x7FFF)
#define  uv_getpen(buff)		(buff[3] & 0xFFFF)
#define  uv_geteditable(buff)	(buff[3] & 0x80000000)
#define	uv_seteditable(buff)	(buff[3] = buff[3] | 0x80000000)
#define	uv_reseteditable(buff)	(buff[3] = buff[3] & 0x7fffffff)
#define  uv_getviewkey(buff)	((UU_KEY_ID) buff[4])

#define 	uv_setsegid(dsegid,buff)	buff[0] = ((dsegid <<  8) | \
																(buff[0] & 0X00FF))

#define	uv_setsegbuf(buff, tempseg, eptr, attr, viewkey) \
	buff[0] = ((tempseg << 8) | eptr->rel_num); \
	buff[1] = eptr->key; \
	buff[2] = ((attr->layer << 16) | attr->color); \
	buff[3] = ((attr->line_style << 16) | attr->pen); \
	{ \
		UU_LOGICAL flag; \
		ur_retrieve_editability(buff[1], &flag); \
		uu_dprint(UU_DTRC,(us,"uv_setsegbuf: key=%x, flag=%d", \
			buff[1], flag)); \
		if(flag) \
			buff[3] = buff[3] | 0x80000000; \
	} \
	buff[4] = viewkey;


#define VSEGBFH
#endif
