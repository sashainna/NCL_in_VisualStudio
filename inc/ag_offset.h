/* NAME :  ag_offset.h        MODULE : Curve internal
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: Curve offset 
**  ag_offsetd     offset data 
**  ag_offsegs     offset segment data   
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/
                                                                      
typedef struct ag_offsetd {    /* offset data                      */
   double            N[3];     /* normal to curve                  */
   int               lor;      /* 0=left or 1=right  offset        */
   double            d;        /* distance                         */
   double            ftol;     /* fitting tolerance                */
   struct ag_offsegs *offseg0; /* initial offset segment data      */
   } AG_OFFSETD, *AG_OFFSETDP;
  
typedef struct ag_offsegs  {   /* offset segment data              */
   struct ag_offsegs *next;    /* seg data of loop                 */
   struct ag_offsegs *prev;    /* seg data of loop                 */
   int               oseg_type;/* type of segment                  */
   struct ag_spline  *bs0;     /* bsplines from crv and crv-off    */
   struct ag_spline  *bs_off0; /* for start of segment             */
   double            s0, t0;   /* at s0, t0                        */
   struct ag_spline  *bs1;     /* bsplines from crv and crv-off    */
   struct ag_spline  *bs_off1; /* for end of segment               */
   double            s1, t1;   /* at s1, t1                        */
   } AG_OFFSEGS, *AG_OFFSEGSP;

/*
Notes:  The offset segment types (oseg_type) are:
     0    no offset (crv has a circular seg whose offset is a point)
          bs_off0 = bs_off1 = NULL and bs0,s0 to bs1,s1 is the
          circular segment of the curve that has no offset.
    -1,1  offset has the same (1) or reverse (-1) direction as curve.
    -2,2  circular fillet of same (2) or reverse (-2) direction at a 
          tangent discontinuity. bs_off0,t0 to bs_off1,t1 is the
          circular segment and bs0,s0  bs1,s1 is the tangent
          discontinuity ( bs0'(s0-) != bs1'(s1+) ).
      
    The offset distance d must be positive and the normal N is used
    to determine the offset direction.
    If P(t) is the curve:
       When P is 3d the offset direction is:
         (lor=0)  NxP'(t)    or (lor=1) -NxP'(t)
       When P is 2d, N is not used and the offset is:
         (lor=0) to the left or (lor=1) to the right.

    A fitting toterance (ftol) of less than 0.001 may generate a
    large amount of data for the offset curve.
*/
