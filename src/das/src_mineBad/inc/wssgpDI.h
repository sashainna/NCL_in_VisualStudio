/*********************************************************************
**    NAME         :  wssgpDI.h
**    CONTAINS:
**
**    MODULE NAME AND RELEASE LEVEL 
**       wssgpDI.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:23
**
**		NOTES: THE FOLLOWING ARE THE ONLY CHANGES THAT UNICAD HAS
**			MADE TO THIS FILE:
**			1. CHANGED THE NAME OF THIS FILE FROM "dev_gp1_internal.h"
**				TO "wssgpDI.h".
**			2. CHANGED THE NAME OF THE INCLUDE FILE FROM "dev_gp1.h" TO
**				"wssgpD.h".
**			3. ADDED THIS HEADER AND CONDITIONAL COMPILE.
**
** Copyright (c) 1986 by Sun Microsystems, Inc.
**	@(#)dev_gp1_internal.h 1.1 86/09/12 SMI
**
*********************************************************************/

#ifndef DEV_GP1_INTERNAL_INCLUDED

#include "wssgpD.h"
#include <pixrect/pixrect_hs.h>
#include <pixrect/memreg.h>
#include <pixrect/cg2reg.h>
#include <pixrect/gp1cmds.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include <sun/gpio.h>
#include <sunwindow/window_hs.h>

/* 
 * Internal data structures for a GP1 device.
 */

typedef enum {				/* what type of list is in progress */
    GP1_NO_LIST,
    GP1_VEC_LIST_2DF, 
    GP1_VEC_LIST_3DF, 
}coordlist;

typedef struct _GP1 {
    unsigned char minor_devno;		/* true minor device number */
    int		bytes_left;		/* bytes left in buffer */
    coordlist	list_type;		/* type of vector list being create */
    short	*n_vectors_ptr;		/* ptr to the number of vectors in 
					 * the list
					 */
    short	*bound_ptr;		/* ptr to # verts in a bound  */
    short	static_block_index;	/* static block index */
    short	texture_block_index;	/* static block that holds the
					 * texture */
    int		clip_id;		/* clip list id */
    int		cmd_buff_offset;	/* offset to command buffer */
    unsigned int cmd_buff_bit_vec;	/* command buffer bit vector */
    short	*shmem;			/* GP1 shared memory address */
    short	*cur_ptr;		/* current location in the buffer */
    int		fd;			/* fd for doing IOCTLs*/
    Pixwin	*pw;			/* pixwin into which we are drawing */
    Rect	win_size;		/* size of the window  */
    int		win_org_x, win_org_y;	/* origin of window  */
    int		win_fd;			/* window fd */
    int		plane_mask;		/* plane write mask */
    int		n_buffers;		/* number of command buffers */
    Point3df vp_scale;			/*viewport scale & offset information*/
    Point3df vp_offset;
} GP1;



#ifndef TRUE
#define TRUE	1
#endif

#ifndef FALSE
#define FALSE	0
#endif

#define GP1_DEFAULT_NUMBER_OF_BUFFERS	2
#define GP1_BYTES_PER_BLOCK		1024
#define GP1_MAX_CLIP_RECTS		60

#define GP1_MOVE		0x1
#define GP1_DRAW		0x0
#define GP1_TERMINATE		0x8000

/* 
 * These macros are used to get and put 32 bit integers and floats
 * into or from 2 16 bit shorts.
 */
#if defined(mc68010) || defined(mc68020)

#define SHORTS_GET_FLOAT(sp, fp) 	*((float *)sp)++ = *fp
#define SHORTS_GET_INT(sp, ip)		*((int *)sp)++ = *ip
#define FLOAT_GETS_SHORTS(fp, sp)	*fp = *((float *)sp)++
#define INT_GETS_SHORTS(ip, sp)		*ip = *((int *)sp)++

#else
#define SHORTS_GET_FLOAT(sp, fp)	\
 	*(sp)++ = *((short *)(fp));	\
 	*(sp)++ = *((short *)(fp) + 1);
	
#define SHORTS_GET_INT(sp, ip)	\
 	*(sp)++ = *((short *)(ip));	\
 	*(sp)++ = *((short *)(ip) + 1);

#define INT_GETS_SHORTS(ip, sp)	\
	*((short *)ip) = *(sp)++;	\
	*((short *)ip + 1) = *(sp)++;

#define FLOAT_GETS_SHORTS(fp, sp)	\
	*((short *)fp) = *(sp)++;	\
	*((short *)fp + 1) = *(sp)++;
	
#endif


/* 
 * Useful macros - IS_NO_ROOM  is true if there is NOT enough
 * room in the buffer for n bytes. MAKE_ROOM flushes the buffer
 * if there is not enough room.
 */

#define IS_NO_ROOM(p, n)	(p->bytes_left < (n))
#define MAKE_ROOM(p, n)		\
    if (IS_NO_ROOM(p, n)) {		\
	Dev_gp1_flush(p, TRUE);	\
    }				\
    p->bytes_left -= n


/* Debuging control */

#ifdef DEBUG_GP1

#define DBG_GP1(a)  a

#else

#define DBG_GP1(a)

#endif



/* 
 * Macros for setting attributes.
 */

#define DEV_GP1_SET_COLOR(gp1, color)				\
{   								\
    MAKE_ROOM((gp1), sizeof(short));				\
    (gp1)->list_type = GP1_NO_LIST;				\
    *((gp1)->cur_ptr)++ = GP1_SETCOLOR | (color & 0xff);	\
}

#define DEV_GP1_SET_WRITE_PLANES(gp1, mask)			\
{   								\
    MAKE_ROOM((gp1), sizeof(short));				\
    (gp1)->list_type = GP1_NO_LIST;    				\
    *((gp1)->cur_ptr)++ = GP1_SETPIXPLANES | (mask & 0xff);	\
    (gp1)->plane_mask = (mask & 0xff);				\
}

#define DEV_GP1_SET_CLIP_PLANES(gp1, mask)			\
{   								\
    MAKE_ROOM((gp1), sizeof(short));				\
    (gp1)->list_type = GP1_NO_LIST;    				\
    *((gp1)->cur_ptr)++ = GP1_SETCLIPPLANES | (mask & 0xff);	\
}

#define	DEV_GP1_SET_HIDDEN_SURF(gp1, type)			\
{   								\
    MAKE_ROOM((gp1), sizeof(short));				\
    (gp1)->list_type = GP1_NO_LIST;    				\
    *((gp1)->cur_ptr)++ = GP1_SETHIDDENSURF | (type & 0xff);	\
}



/* 
 * Macros for stuffing polygons into the gp1.
 */

#define	DEV_GP1_POLY_3DF_INIT(gp1, n_bounds, n_verticies, fit, shade)	\
{   									\
    register	int	total_bytes;					\
    total_bytes = n_verticies * 3 * sizeof(float);			\
    total_bytes += 2 * sizeof(short) + n_bounds * sizeof(short);	\
    if (total_bytes > (GP1_BYTES_PER_BLOCK * (gp1)->n_buffers - 8))	\
	fit = 0;	/* cannot fit polygon in cmd buffers */		\
    else {								\
	MAKE_ROOM((gp1), total_bytes);					\
	(gp1)->list_type = GP1_NO_LIST;					\
	*((gp1)->cur_ptr)++ = GP1_XF_PGON_FLT_3D | (shade & 0x7);	\
	*((gp1)->cur_ptr) = n_bounds;					\
	(gp1)->bound_ptr = (gp1)->cur_ptr;				\
	(gp1)->cur_ptr += n_bounds + 1;					\
	fit = 1;							\
    }									\
}

#define DEV_GP1_POLY_3DF_ADD_BOUND(gp1)					\
    *++((gp1)->bound_ptr) = 0;

#define DEV_GP1_POLY_3DF_ADD_VERTEX(gp1, x, y, z)			\
{									\
    register	short	*ptr;						\
    ptr = (gp1)->cur_ptr;						\
    SHORTS_GET_FLOAT(ptr, &(x));					\
    SHORTS_GET_FLOAT(ptr, &(y));					\
    SHORTS_GET_FLOAT(ptr, &(z));					\
    (gp1)->cur_ptr = ptr;						\
    (*((gp1)->bound_ptr))++;						\
}


#define	DEV_GP1_POLY_2DF_INIT(gp1, n_bounds, n_verticies, fit, fill_style)\
{   									\
    register	int	total_bytes;					\
    total_bytes = n_verticies * 2 * sizeof(float);			\
    total_bytes += 2 * sizeof(short) + n_bounds * sizeof(short);	\
    if (total_bytes > (GP1_BYTES_PER_BLOCK * (gp1)->n_buffers - 8))	\
	fit = 0;	/* cannot fit polygon in cmd buffers */		\
    else {								\
	MAKE_ROOM((gp1), total_bytes);					\
	(gp1)->list_type = GP1_NO_LIST;					\
	*((gp1)->cur_ptr)++ = GP1_XF_PGON_FLT_2D | (fill_style & 0x7);	\
	*((gp1)->cur_ptr) = n_bounds;					\
	(gp1)->bound_ptr = (gp1)->cur_ptr;				\
	(gp1)->cur_ptr += n_bounds + 1;					\
	fit = 1;							\
    }									\
}

#define DEV_GP1_POLY_2DF_ADD_BOUND(gp1)					\
    *++((gp1)->bound_ptr) = 0;

#define DEV_GP1_POLY_2DF_ADD_VERTEX(gp1, x, y)				\
{									\
    register	short	*ptr;						\
    ptr = (gp1)->cur_ptr;						\
    SHORTS_GET_FLOAT(ptr, &(x));					\
    SHORTS_GET_FLOAT(ptr, &(y));					\
    (gp1)->cur_ptr = ptr;						\
    (*((gp1)->bound_ptr))++;						\
}




/* 
 * Transform a polygon.
 */
#define	DEV_GP1_XPOLY_3DF_BEGIN(gp1, n_bounds, n_verticies, fit)	\
{   									\
    register	int	GP1_total_bytes;				\
    register	short	*GP1_ptr;					\
    									\
    Dev_gp1_flush((gp1), TRUE);	/* start afresh */		\
    GP1_total_bytes = n_verticies * 3 * sizeof(float);			\
    GP1_total_bytes += 3 * sizeof(short) + n_bounds * sizeof(short);	\
    if (GP1_total_bytes > (GP1_BYTES_PER_BLOCK * (gp1)->n_buffers - 8))	\
	fit = 0;	/* cannot fit polygon in cmd buffers */		\
    else {								\
	MAKE_ROOM((gp1), GP1_total_bytes);				\
	(gp1)->list_type = GP1_NO_LIST;					\
	GP1_ptr = (gp1)->cur_ptr;					\
	*GP1_ptr++ = GP1_PROC_PGON_FLT_3D;				\
	*GP1_ptr = n_bounds;						\
	(gp1)->bound_ptr = GP1_ptr;					\
	GP1_ptr += n_bounds + 1;					\
	fit = 1;							\
    }


#define DEV_GP1_XPOLY_3DF_ADD_BOUND(gp1)				\
    *++((gp1)->bound_ptr) = 0;
    
#define DEV_GP1_XPOLY_3DF_ADD_VERTEX(gp1, x, y, z)			\
    SHORTS_GET_FLOAT(GP1_ptr, &(x));					\
    SHORTS_GET_FLOAT(GP1_ptr, &(y));					\
    SHORTS_GET_FLOAT(GP1_ptr, &(z));					\
    (*((gp1)->bound_ptr))++;


#define DEV_GP1_XPOLY_3DF_XFORM(gp1, n_bounds)				\
    *GP1_ptr = (gp1)->bytes_left / sizeof(short);			\
    (gp1)->cur_ptr = GP1_ptr;						\
    (gp1)->cur_ptr++;							\
    Dev_gp1_flush((gp1), FALSE);					\
    gp1_wait0(GP1_ptr, (gp1)->fd);					\
    GP1_ptr += 2;/* skip to number of bounds in the xformed poly */	\
    n_bounds = *GP1_ptr;						\
    (gp1)->bound_ptr = GP1_ptr;					\
    GP1_ptr += n_bounds + 1;						\


#define DEV_GP1_XPOLY_3DF_GET_BOUND(gp1, bound)				\
    bound = *++(gp1)->bound_ptr;
    
#define DEV_GP1_XPOLY_3DF_GET_VERTEX(x, y, z)				\
    INT_GETS_SHORTS(&(x), GP1_ptr);					\
    INT_GETS_SHORTS(&(y), GP1_ptr);					\
    INT_GETS_SHORTS(&(z), GP1_ptr);

#define DEV_GP1_XPOLY_3DF_END }
		
		


#define	DEV_GP1_XPOLY_2DF_BEGIN(gp1, n_bounds, n_verticies, fit)	\
{   									\
    register	int	GP1_total_bytes;				\
    register	short	*GP1_ptr;					\
    									\
    Dev_gp1_flush((gp1), TRUE);	/* start afresh */		\
    GP1_total_bytes = n_verticies * 2 * sizeof(float);			\
    GP1_total_bytes += 3 * sizeof(short) + n_bounds * sizeof(short);	\
    if (GP1_total_bytes > (GP1_BYTES_PER_BLOCK * (gp1)->n_buffers - 8))	\
	fit = 0;	/* cannot fit polygon in cmd buffers */		\
    else {								\
	MAKE_ROOM((gp1), GP1_total_bytes);				\
	(gp1)->list_type = GP1_NO_LIST;					\
	GP1_ptr = (gp1)->cur_ptr;					\
	*GP1_ptr++ = GP1_PROC_PGON_FLT_2D;				\
	*GP1_ptr = n_bounds;						\
	(gp1)->bound_ptr = GP1_ptr;					\
	GP1_ptr += n_bounds + 1;					\
	fit = 1;							\
    }


#define DEV_GP1_XPOLY_2DF_ADD_BOUND(gp1)				\
    *++((gp1)->bound_ptr) = 0;
    
#define DEV_GP1_XPOLY_2DF_ADD_VERTEX(gp1, x, y)				\
    SHORTS_GET_FLOAT(GP1_ptr, &(x));					\
    SHORTS_GET_FLOAT(GP1_ptr, &(y));					\
    (*((gp1)->bound_ptr))++;


#define DEV_GP1_XPOLY_2DF_XFORM(gp1, n_bounds)				\
    *GP1_ptr = (gp1)->bytes_left / sizeof(short);			\
    (gp1)->cur_ptr = GP1_ptr;						\
    (gp1)->cur_ptr++;							\
    Dev_gp1_flush((gp1), FALSE);					\
    gp1_wait0(GP1_ptr, (gp1)->fd);					\
    GP1_ptr += 2;/* skip to number of bounds in the xformed poly */	\
    n_bounds = *GP1_ptr;						\
    (gp1)->bound_ptr = GP1_ptr;						\
    GP1_ptr += n_bounds + 1;						\


#define DEV_GP1_XPOLY_2DF_GET_BOUND(gp1, bound)				\
    bound = *++(gp1)->bound_ptr;
    
#define DEV_GP1_XPOLY_2DF_GET_VERTEX(x, y)				\
    INT_GETS_SHORTS(&(x), GP1_ptr);					\
    INT_GETS_SHORTS(&(y), GP1_ptr);					\

#define DEV_GP1_XPOLY_2DF_END }
		



#define DEV_GP1_MOVE_2DF(gp1, pt_x, pt_y)				\
    if ((gp1)->list_type != GP1_VEC_LIST_2DF) {				\
	MAKE_ROOM((gp1), (2*sizeof(short) + 2*sizeof(float)));		\
	(gp1)->list_type = GP1_VEC_LIST_2DF;				\
	*((gp1)->cur_ptr)++ = GP1_XF_LINE_FLT_2D;			\
    }									\
    else if (IS_NO_ROOM((gp1), (sizeof(short) + 2*sizeof(float)))) {	\
	Dev_gp1_flush((gp1), TRUE);					\
	(gp1)->list_type = GP1_VEC_LIST_2DF;				\
	*((gp1)->cur_ptr)++ = GP1_XF_LINE_FLT_2D;			\
	(gp1)->bytes_left -= (2*sizeof(short) + 2*sizeof(float));	\
    }									\
    else {								\
	(gp1)->bytes_left -= (sizeof(short) + 2*sizeof(float));		\
	*(gp1->cur_ptr - 5) &= ~GP1_TERMINATE;				\
    }									\
    *((gp1)->cur_ptr)++ = GP1_TERMINATE | GP1_MOVE;			\
    SHORTS_GET_FLOAT((gp1)->cur_ptr, &(pt_x));				\
    SHORTS_GET_FLOAT((gp1)->cur_ptr, &(pt_y));



#define DEV_GP1_DRAW_2DF(gp1, pt_x, pt_y)				\
    if ((gp1)->list_type == GP1_VEC_LIST_2DF) {				\
	if (IS_NO_ROOM((gp1), (sizeof(short) + 2*sizeof(float)))) {	\
	    Point2df	tmp_pt;						\
	    short	*tmp_ptr;					\
	    								\
	    tmp_ptr = (gp1)->cur_ptr - 4;				\
	    FLOAT_GETS_SHORTS(&tmp_pt.x, tmp_ptr);			\
	    FLOAT_GETS_SHORTS(&tmp_pt.y, tmp_ptr);			\
	    Dev_gp1_flush((gp1), TRUE);					\
	    DEV_GP1_MOVE_2DF((gp1), tmp_pt.x, tmp_pt.y);		\
	}								\
	(gp1)->bytes_left -= (sizeof(short) + 2*sizeof(float));		\
	*((gp1)->cur_ptr - 5) &= ~GP1_TERMINATE;			\
	*((gp1)->cur_ptr)++ = GP1_TERMINATE | GP1_DRAW;			\
	SHORTS_GET_FLOAT((gp1)->cur_ptr, &(pt_x));			\
	SHORTS_GET_FLOAT((gp1)->cur_ptr, &(pt_y));			\
    }




#define DEV_GP1_MOVE_3DF(gp1, pt_x, pt_y, pt_z)				\
    if ((gp1)->list_type != GP1_VEC_LIST_3DF) {				\
	MAKE_ROOM((gp1), (2*sizeof(short) + 3*sizeof(float)));		\
	(gp1)->list_type = GP1_VEC_LIST_3DF;				\
	*((gp1)->cur_ptr)++ = GP1_XF_LINE_FLT_3D;			\
    }									\
    else if (IS_NO_ROOM((gp1), (sizeof(short) + 3*sizeof(float)))) {	\
	Dev_gp1_flush((gp1), TRUE);					\
	(gp1)->list_type = GP1_VEC_LIST_3DF;				\
	*((gp1)->cur_ptr)++ = GP1_XF_LINE_FLT_3D;			\
	(gp1)->bytes_left -= (2*sizeof(short) + 3*sizeof(float));	\
    }									\
    else {								\
	(gp1)->bytes_left -= (sizeof(short) + 3*sizeof(float));		\
	*(gp1->cur_ptr - 7) &= ~GP1_TERMINATE;				\
    }									\
    *((gp1)->cur_ptr)++ = GP1_TERMINATE | GP1_MOVE;			\
    SHORTS_GET_FLOAT((gp1)->cur_ptr, &(pt_x));				\
    SHORTS_GET_FLOAT((gp1)->cur_ptr, &(pt_y));				\
    SHORTS_GET_FLOAT((gp1)->cur_ptr, &(pt_z));



#define DEV_GP1_DRAW_3DF(gp1, pt_x, pt_y, pt_z)				\
    if ((gp1)->list_type == GP1_VEC_LIST_3DF) {				\
	if (IS_NO_ROOM((gp1), (sizeof(short) + 3*sizeof(float)))) {	\
	    Point3df	tmp_pt;						\
	    short	*tmp_ptr;					\
	    								\
	    tmp_ptr = (gp1)->cur_ptr - 6;				\
	    FLOAT_GETS_SHORTS(&tmp_pt.x, tmp_ptr);			\
	    FLOAT_GETS_SHORTS(&tmp_pt.y, tmp_ptr);			\
	    FLOAT_GETS_SHORTS(&tmp_pt.z, tmp_ptr);			\
	    Dev_gp1_flush((gp1), TRUE);					\
	    DEV_GP1_MOVE_3DF((gp1), tmp_pt.x, tmp_pt.y, tmp_pt.z);	\
	}								\
	(gp1)->bytes_left -= (sizeof(short) + 3*sizeof(float));		\
	*((gp1)->cur_ptr - 7) &= ~GP1_TERMINATE;			\
	*((gp1)->cur_ptr)++ = GP1_TERMINATE | GP1_DRAW;			\
	SHORTS_GET_FLOAT((gp1)->cur_ptr, &(pt_x));			\
	SHORTS_GET_FLOAT((gp1)->cur_ptr, &(pt_y));			\
	SHORTS_GET_FLOAT((gp1)->cur_ptr, &(pt_z));			\
    }




/* 
 * transform a vector list
 */

#define DEV_GP1_XVECTOR_3DF_BEGIN(gp1, fit)				\
{   									\
    register	short	*GP1_ptr;					\
    		short	*GP1_ready;					\
    		short	*GP1_n_vecs;					\
    									\
    Dev_gp1_flush((gp1), TRUE);	/* start afresh */		\
    /* find # of vectors that will with in the buffer */		\
    fit = ((gp1)->bytes_left - (2 * sizeof(short))) / 			\
	(6*sizeof(float) + (2 * sizeof(short)));			\
    GP1_ptr = (gp1)->cur_ptr;						\
    *GP1_ptr++ = GP1_PROC_LINE_FLT_3D;					\
    GP1_n_vecs = GP1_ptr++;						\
    *GP1_n_vecs = 0;							\
    GP1_ready = GP1_n_vecs + 14;

#define DEV_GP1_XVECTOR_3DF_ADD_VECTOR( x1, y1, z1, x2, y2, z2)		\
    GP1_ptr++;		/* skip over clip flag */			\
    SHORTS_GET_FLOAT(GP1_ptr, &(x1));					\
    SHORTS_GET_FLOAT(GP1_ptr, &(y1));					\
    SHORTS_GET_FLOAT(GP1_ptr, &(z1));					\
    SHORTS_GET_FLOAT(GP1_ptr, &(x2));					\
    SHORTS_GET_FLOAT(GP1_ptr, &(y2));					\
    SHORTS_GET_FLOAT(GP1_ptr, &(z2));					\
    *GP1_ptr++ = 0xff;	/* set the ready flag to non zero */		\
    (*GP1_n_vecs)++;
    
    
#define DEV_GP1_XVECTOR_3DF_XFORM(gp1)    				\
    (gp1)->cur_ptr = GP1_ptr;						\
    Dev_gp1_flush((gp1), FALSE);					\
    GP1_ptr = GP1_n_vecs + 1;		/* reset to the first vector */
    
#define DEV_GP1_XVECTOR_3DF_GET_VECTOR(gp1, clip, x1, y1, z1, x2, y2, z2) \
    gp1_wait0(GP1_ready, (gp1)->fd);					\
    clip = *GP1_ptr++;							\
    INT_GETS_SHORTS(&(x1), GP1_ptr);					\
    INT_GETS_SHORTS(&(y1), GP1_ptr);					\
    INT_GETS_SHORTS(&(z1), GP1_ptr);					\
    INT_GETS_SHORTS(&(x2), GP1_ptr);					\
    INT_GETS_SHORTS(&(y2), GP1_ptr);					\
    INT_GETS_SHORTS(&(z2), GP1_ptr);					\
    GP1_ptr++;			/* skip over ready flag */		\
    GP1_ready += 14;
    
#define DEV_GP1_XVECTOR_3DF_END }



#define DEV_GP1_XVECTOR_2DF_BEGIN(gp1, fit)				\
{   									\
    register	short	*GP1_ptr;					\
    		short	*GP1_ready;					\
    		short	*GP1_n_vecs;					\
    									\
    Dev_gp1_flush((gp1), TRUE);	/* start afresh */		\
    /* find # of vectors that will with in the buffer */		\
    fit = ((gp1)->bytes_left - (2 * sizeof(short))) / 			\
	(4*sizeof(float) + (2 * sizeof(short)));			\
    GP1_ptr = (gp1)->cur_ptr;						\
    *GP1_ptr++ = GP1_PROC_LINE_FLT_2D;					\
    GP1_n_vecs = GP1_ptr++;						\
    *GP1_n_vecs = 0;							\
    GP1_ready = GP1_n_vecs + 10;

#define DEV_GP1_XVECTOR_2DF_ADD_VECTOR( x1, y1, x2, y2)			\
    GP1_ptr++;		/* skip over clip flag */			\
    SHORTS_GET_FLOAT(GP1_ptr, &(x1));					\
    SHORTS_GET_FLOAT(GP1_ptr, &(y1));					\
    SHORTS_GET_FLOAT(GP1_ptr, &(x2));					\
    SHORTS_GET_FLOAT(GP1_ptr, &(y2));					\
    *GP1_ptr++ = 0xff;	/* set the ready flag to non zero */		\
    (*GP1_n_vecs)++;
    
    
#define DEV_GP1_XVECTOR_2DF_XFORM(gp1)    				\
    (gp1)->cur_ptr = GP1_ptr;						\
    Dev_gp1_flush((gp1), FALSE);					\
    GP1_ptr = GP1_n_vecs + 1;		/* reset to the first vector */
    
#define DEV_GP1_XVECTOR_2DF_GET_VECTOR(gp1, clip, x1, y1, x2, y2) \
    gp1_wait0(GP1_ready, (gp1)->fd);					\
    clip = *GP1_ptr++;							\
    INT_GETS_SHORTS(&(x1), GP1_ptr);					\
    INT_GETS_SHORTS(&(y1), GP1_ptr);					\
    INT_GETS_SHORTS(&(x2), GP1_ptr);					\
    INT_GETS_SHORTS(&(y2), GP1_ptr);					\
    GP1_ptr++;			/* skip over ready flag */		\
    GP1_ready += 10;
    
#define DEV_GP1_XVECTOR_2DF_END }

#define DEV_GP1_INTERNAL_INCLUDED 1
#endif
