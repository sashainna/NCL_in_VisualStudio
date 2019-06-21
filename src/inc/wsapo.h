/*********************************************************************
**
**    NAME         :  wsapo.h
**       CONTAINS:
**       Include file for the "wsapo*.c" driver files
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       wsapo.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:09
*********************************************************************/

#ifndef WSAPOH


#include "/sys/ins/base.ins.c"
#include "/sys/ins/pad.ins.c"
#include "/sys/ins/pfm.ins.c"
#include "/sys/ins/kbd.ins.c"
#include "/sys/ins/gpr.ins.c"
#include "/sys/ins/error.ins.c"
#include "gtbl.h"
#include "udebug.h"

/* Often used constants */
#define XOR (short)6
#define ZERO (short)0


typedef struct {
	int wid;            	/* workstation id of this workstation */
	UG_wdt *wdtptr;		/* pointer to workstation description table */
	int devxmax;			/* maximum x device coordinate */
	int devymax;        	/* maximum y device coordinate */
	int maxxrast;			/* maximum x raster coordinate */
	int maxyrast;			/* maximum x raster coordinate */
	gpr_$pixel_value_t line_color;
	gpr_$pixel_value_t text_color;
	gpr_$pixel_value_t mark_color;
	gpr_$pixel_value_t fill_color;
	status_$t gstat;
	gpr_$bitmap_desc_t disp_map;
	gpr_$attribute_desc_t disp_attr;
	gpr_$plane_t plane_id;
	gpr_$bitmap_desc_t ar_crs_bitmap_id;
	gpr_$bitmap_desc_t sq_crs_bitmap_id;
	gpr_$bitmap_desc_t pl_crs_bitmap_id;
	gpr_$bitmap_desc_t tx_crs_bitmap_id;

	/* These bitmaps are used for putting up markers */
	gpr_$attribute_desc_t marker_att;
	gpr_$bitmap_desc_t dot_marker;
	gpr_$bitmap_desc_t plus_marker;
	gpr_$bitmap_desc_t asterisk_marker;
	gpr_$bitmap_desc_t circle_marker;
	gpr_$bitmap_desc_t X_marker;
	gpr_$bitmap_desc_t box_marker;
	gpr_$bitmap_desc_t diamond_marker;

	short rast_ops[7];
	short obs;				/* acquire display return parameter */
	UU_LOGICAL no_color;
	UU_LOGICAL redraw;
	} UW_apodata;

/* Structure to hold bitmap data */
typedef struct { 
	gpr_$attribute_desc_t 	attr_id;
	gpr_$bitmap_desc_t   	bitmap_id;
	gpr_$window_t           window;
} UW_apo_map_data;

/* Some macros */

#define cursor_on()	\
	gpr_$set_cursor_active(-1, UW_apo.gstat); \
	check_stat()

#define cursor_off() \
	gpr_$set_cursor_active(0, UW_apo.gstat); \
	check_stat()

#ifdef UU_DEBUG
#define check_stat() \
	if( UW_apo.gstat.all != status_$ok && \
		 UW_apo.gstat.all != gpr_$not_in_direct_mode ) \
		 uw_apollo_check_stat(UW_apo.gstat);
#else
#define check_stat()
#endif

#define uw_apollocolormap(index)                               \
( index>UW_apo.wdtptr->outwdtpt->cofac.colors-1 ? -1 : index );\
if( index > UW_apo.wdtptr->outwdtpt->cofac.colors-1 ) {        \
		uu_denter2(UU_GITRC,(us,"colormap returns -1"));          \
		uu_dexit;						                               \
	}                             		                         \
	else {                              		                   \
		uu_denter2(UU_GITRC,(us,"colormap returns %d",index))   	 \
		uu_dexit;                 											 \
	}                                                            
	

#define WSAPOH
#endif
