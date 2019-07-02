/*********************************************************************
**
**    NAME         :  axhatch.h
**
**    CONTAINS:  cross hatch typedefs etc.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       axhatch.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:11
**
*********************************************************************/

#ifndef AXHATCHH


#include "usysdef.h"
#include "ulist.h"


typedef struct		/* holds form data */
{
	int		material[5];
	UU_REAL	xh_angle;
	UU_REAL	xh_scale;
	int 		color[5];
	int		linestyle[5];
	int		pen;
} 
UA_xh_attr_rec;


enum UA_line_event
{
	bad_event_type,region_boundary,island_boundary,near_endpoint
	    ,line_seg_start,line_seg_end,line_dot,mask_encounter
} 
;
struct UA_isect
{
	UU_REAL	pt[3];	/* pt */
	UU_REAL	t0;	/* t0 */
	UU_REAL	t1;	/* t1 */
	int		order;	/* order */
} 
;
struct UA_xhpattern
{
	int		xh_id;	/* xh_id */
	UU_LIST	xh_lf;	/* xh_lf */
} 
;
struct UA_lfdata
{
	UU_REAL	lf_angle;	/* lf_angle */
	UU_REAL	lf_xorg;	/* lf_xorg */
	UU_REAL	lf_yorg;	/* lf_yorg */
	UU_REAL	lf_offset;	/* lf_offset */
	UU_REAL	lf_space;	/* lf_space */
	UU_REAL	lf_shift;	/* lf_shift */
	UU_LIST	lf_dashes;	/* lf_dashes */
} 
;
enum UA_xhatch_error
{
	hatch_ok,illegal_boundary_type,illegal_event_type,
	region_not_planar,region_not_legal,invalid_pattern,
	nothing_generated,illegal_asso_type,system_error,
	too_much_geometry,no_legal_geometry
} 
;
struct UA_ilnode
{
	int		line_id;	/* line_id */
	UU_REAL	line_dir[3];	/* line_dir */
	UU_REAL	t0;	/* t0 */
	UU_REAL	t1;	/* t1 */
	int		key;	/* key */
	enum UA_line_event	event;	/* event */
} 
;

int ua_xhatch_compare();

#define AXHATCHH
#endif
