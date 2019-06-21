/* NAME :  ag_bs_segs.h       MODULE : Curve internal 
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS:  b-spline segment data
**  ag_bs_segsh    b-spline segments header      
**  ag_bs_seg      b-spline segment
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

typedef struct ag_bs_segsh {       /* b-spline segments header     */
   struct ag_bs_seg     *seg0;     /* initial segment of loop      */
   } AG_BS_SEGSH, *AG_BS_SEGSHP;

typedef struct ag_bs_seg  {       /* b-spline segment              */
   struct ag_bs_seg     *next;
   struct ag_bs_seg     *prev;
   int                  V_id0;    /* id at start of segment        */
   int                  V_id1;    /* id at end                     */
   struct ag_spline     *bs0;     /* bs at start                   */
   struct ag_spline     *bs1;     /* bs at end                     */
   } AG_BS_SEG, *AG_BS_SEGP;

/*
Notes: Two segments are merged when the start V_id of either
       segment is equal to the end V_id of the other segment.
       The segment is closed if V_id0 = V_id1.
       Each segment has bs0->prev = bs1 and bs1->next = bs0.
*/
