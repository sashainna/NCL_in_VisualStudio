/*********************************************************************
**    NAME         :  m4evcrv.c
**       CONTAINS:
**       um_rotatept(pt,rotvec,axispt,angle,redomatrix,m)
**       um_rebuildmatrix(dir,angle,axispt,m)
**       umi_adjust_evaluatorec(evflag,crvreversed,compcrvptr,i,evoutptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m4evcrv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:03
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdebug.h"
#include "mfort.h"
#include "nclfc.h"

/***************************************************************************
**  E_FUNCTION: um_rotatept(pt,rotvec,axispt,angle,redomatrix,m)
**
**    DESCRIPTION: rotates a 3-dimensional point around a 3-dimensional axis
**
**    PARAMETERS:
**       INPUT:
**           PARAMETER      TYPE            MEANING   
**           *pt            UU_REAL     a pointer to a point entity to 
**                                      be rotated;
**
**           rotvec[3]      UU_REAL     directional vector for axis of rotation;
**
**           axispt[3]      UU_REAL     a point on the axis of rotation;
**
**           angle          UU_REAL     angle of rotation in radians;
**
**           redomatrix     UU_LOGICAL  a flag indicating whether the matrix 
**                                      computed on a previous call should be 
**                                      used rather than compute a new matrix.
**
**           *m             struct UM_rotmatrix    
**                                      storage (perhaps) containing the 
**                                      rotation matrix
**
**       OUTPUT:  
**           PARAMETER      TYPE               MEANING
**            pt            UU_REAL     coordinates of the new point
**
**            *m            struct UM_rotmatrix             
**                                      if redomatrix is input as TRUE, 
**                                      then new matrix coefficients will be 
**                                      stored and returned here.
**
**    RETURNS: none.
**
**    SIDE EFFECTS: none.
**
**    WARNINGS: none. 
**
**    NOTE: A message will be printed if the magnitude of rotvec is too small.
**          In this case the original point is returned.
**
***************************************************************************/

void um_rotatept(pt,rotvec,axispt,angle,redomatrix,m)

    UU_REAL    *pt;            /* point entity */
    UU_REAL    rotvec[];       /* vector about which to rotate */
    UU_REAL    axispt[];       /* point on the axis of rotation*/
    UU_REAL    angle;          /* angle through which to rotate*/
    UU_LOGICAL redomatrix;     /* TRUE iff matrix should be redone*/
    struct UM_rotmatrix *m;    /*ptr to rotation matrix structure*/
   {
    UU_REAL   copyx;           /* temporary point */
    UU_REAL   copyy;
    UU_REAL   copyz;

    if (redomatrix)
       if (-1 == um_rebuildmatrix(rotvec,angle,axispt,m))  
         {  
            uu_uerror0(/*From um_rotatept: direction vector (rotvec) too small*/
                     UM_MODEL,119);
            return;
         }
    copyx = pt[0];
    copyy = pt[1];
    copyz = pt[2];
    /*multiply point by matrix*/
    pt[0] = copyx * m->mat00 + copyy * m->mat10 + copyz * m->mat20 + m->mat30;
    pt[1] = copyx * m->mat01 + copyy * m->mat11 + copyz * m->mat21 + m->mat31;
    pt[2] = copyx * m->mat02 + copyy * m->mat12 + copyz * m->mat22 + m->mat32;
   }            



/******************************************************************************
** I_FUNCTION: um_rebuildmatrix(dir,angle,axispt,m)
**
**    DESCRIPTION: rebuilds a rotation matrix
**
**    PARAMETERS:
**       INPUT:
**          dir          direction vector of axis of rotation
**          angle        angle of rotation radians
**          axispt       point on the rotation axis
**
**       OUTPUT:
**          m            rotation matrix  
**
**    RETURNS: none.
**
**    SIDE EFFECTS: none.
**
**    WARNINGS: none.
**
******************************************************************************/     
static um_rebuildmatrix(dir,angle,axispt,m)
    UU_REAL    dir[];
    UU_REAL    angle;
    UU_REAL    axispt[3];
    struct UM_rotmatrix *m;
   {
     UU_REAL    temp1;
     UU_REAL    temp2;
     UU_REAL    temp3;
     UU_REAL    a, b, c;        /* Direction cosines of dir */
     UU_REAL    sine;
     UU_REAL    cosine;
     UU_REAL    vsq;
     UU_REAL    x;              /* Axis pt  coordinates*/
     UU_REAL    y;
     UU_REAL    z;
     UU_REAL    len;

    len = sqrt(dir[0] * dir[0] + dir[1] * dir[1] + dir[2] * dir[2]);
    if (len  <=UM_TOLR)
      {
      /*
      uu_dexit;
      /* */
      return(-1);
      }
    else
      { 
       a = dir[0]/len;
       b = dir[1]/len;
       c = dir[2]/len;
       vsq = b * b + c * c;
       sine = sin(angle);
       cosine = cos(angle);
       if (vsq <=UM_TOLR)
         {   
          /*then we are parallel to the x-axis and merely rotate around it*/
          m->mat00 = 1.0;
          m->mat01 = 0.0;
          m->mat02 = 0.0;
          m->mat10 = 0.0;
          m->mat11 = cosine; 
          m->mat20 = 0.0;
          m->mat22 = cosine;
          m->mat12 = sine * a; /*Note, in this case a is approx 1. If a is */
          m->mat21 = -sine * a;/*plus, then use standard rotation matrix.  */
                               /*If a is negative, do negative rotation.   */ 
    
          /*Now compute the translation row of the UM_matrix.*/
          x = axispt[0];
          y = axispt[1];
          z = axispt[2];
          if (x != 0.0 || y != 0.0 || z != 0.0)
            { 
              m->mat30 = 0.0;
              m->mat31 = y * (1.0 - cosine) + z * sine * a;
              m->mat32 = z * (1.0 - cosine) - y * sine * a;
            }
          else 
            { 
              m->mat30 = 0.0;
              m->mat31 = 0.0;
              m->mat32 = 0.0;
            }
         }
       else /*The direction is not parallel to the x-axis.*/
         {  
          temp1 = 1.0 - cosine;
          temp2 = a * b * temp1;
          temp3 = c * sine;
          m->mat01 = temp2 + temp3;
          m->mat10 = temp2 - temp3;
                
          temp2 = a * c * temp1;
          temp3 = b * sine;
          m->mat02 = temp2 - temp3;
          m->mat20 = temp2 + temp3;

          temp1 = cosine * (a * a - 1.0) / vsq;
          temp2 = c * (temp1 + 1.0);
          m->mat22 = c * temp2 + cosine;
          temp1 = b * temp2;
          temp2 = a * sine;
          m->mat12 = temp1 + temp2;
          m->mat21 = temp1 - temp2;

          temp1 = a * a;
          temp2 = b * b;
          m->mat00 = vsq * cosine + temp1;
          m->mat11 = (temp1 * temp2  + c * c) * cosine / vsq + temp2;
          /*End rotation compution*/
    
          /*Now compute the translation row of the UM_matrix.*/
          x = axispt[0];
          y = axispt[1];
          z = axispt[2];
          if (x != 0.0 || y != 0.0 || z != 0.0)
            {   
             temp2 = (a * (b * y + c * z) / vsq - x);
             temp3 = (b * z - c * y) / vsq;
             temp1 = temp2 * cosine - temp3 * sine;
             temp2 = temp2 * sine + temp3 * cosine;
             temp3 = x * a + y * b + z * c;
             m->mat30 = vsq * temp1 - temp3 * a + x;
             temp1 = a * temp1 + temp3;
             m->mat31 = c * temp2 - b * temp1 + y; 
             m->mat32 = z - b * temp2 - c * temp1;
            }
          else 
            { 
             m->mat30 = 0.0;
             m->mat31 = 0.0;
             m->mat32 = 0.0;
            }
         }/*End of direction not parallel to the x-axis*/
       return(1);
      }
   }

/*********************************************************************
**    I_FUNCTION: umi_adjust_evaluatorec
**                      (evflag,crvreversed,compcrvptr,i,evoutptr)
**       DESCRIPTION: If Cn is a subcurve of the composite curve C, this 
**          function adjusts the fields of an evaluation record on Cn so that
**          that the fields of the record are correct for the parameterization
**          of the composite curve, C.  
**    PARAMETERS   
**       INPUT: 
**          evflag             indicates what fields in the record have been
**                             computed and must be adjusted.
**
**          crvreversed        TRUE iff the subcurve is parameterized in the 
**                             opposite direction to the composite cirve.
**
**          compcrvptr         pointer to the composite curve entity containing
**                             the subcurve.
**
**          i                  integer indicating what subcurve we are on.
**
**          evoutptr           pointer to the evaluator record to adjust.
**       OUTPUT:  
**          evoutptr           pointer to the newly adjusted evaluator record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umi_adjust_evaluatorec(evflag, crvreversed, compcrvptr, i, evoutptr)
   int evflag;
   UU_LOGICAL crvreversed;
   struct UM_compcrv_rec *compcrvptr;
   int i;
   struct UM_evcrvout *evoutptr;

   {
   UU_REAL dudw;
   UU_REAL ep0;
   UU_REAL um_getarclen();

   uu_denter(UU_MTRC,(us,"umi_adjust_evaluatorec(%d,%d,%x,%d,%x)",
                  evflag,crvreversed,compcrvptr,i,evoutptr));

	if (evflag <= UM_POINT) return (0);

         /* if "w" is the parameter variable for the composite curve and
          * "u" is the parameter variable for the subcurve, then these two
          * variables are related by an equation of the form: 
          * u = (w-w(i))/(w(i+1)-w(i)), where w(i), w(i+1) are the composite
          * curve parameters of the end points of the subcurve; so dudw is 
          * computed here:
          */
         if (i==0) ep0 = 0.0; else ep0 = compcrvptr->cid[i-1].endparam;
         dudw = 1.0/(compcrvptr->cid[i].endparam-ep0);
         if (crvreversed)
            dudw = -1.0 * dudw;

         /* now adjust first derivative */
         /* if C is the composite curve and Cn is a subcurve of C, then for
          * any point in Cn, dC/dw = dCn/du * du/dw
          */
         um_vctmsc(evoutptr->dcdu, dudw, evoutptr->dcdu);
         /* now adjust second derivative */
         if (evflag >= UM_SECDERIV)
            {
            /* if C and Cn are as above, then 
             * d2C/dw2 = dCn/du * d2u/dw2 + (du/dw)**2 * d2Cn/du2;
             * note, d2u/dw2 = 0.
             */
            um_vctmsc(evoutptr->d2cdu2, dudw * dudw, evoutptr->d2cdu2); 
            }

   uu_dexit;
	return (0);
   }
/*********************************************************************
**    FUNCTION : UU_LOGICAL um_planar_curve (pts,npts,plane,xaxis,yaxis)
**
**      Determines if a set of points lies on a plane and returns the
**      plane, and X-Y axis components of plane.
**
**    PARAMETERS
**       INPUT  :
**                  pts   - Set of points to check for planar.
**                  npts  - Number of points in 'pts'.
**       OUTPUT :
**                  plane - Plane that curve lies on. (i,j,k,d)
**                  xaxis - X-axis of plane.
**                  yaxis - Y-axis of plane.
**
**    RETURNS      : UU_TRUE if points lie on a plane.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_planar_curve (pts,npts,plane,xaxis,yaxis)
UM_coord *pts;
int npts;
UU_REAL *plane;
UM_vector xaxis,yaxis;
{
	UU_LOGICAL stat;
	int col,i ;
	UM_coord  ppt;
	UU_REAL um_dsupt();
	UM_vector nvec,unvc;
	UM_real8 tol8;
	UM_int2 idx;
/*
.....Initialize routine
*/
	idx = 27; getsc (&idx,&tol8);
/*
.....Make sure there are enough points
*/
	if (npts < 3) goto failed;
/*
.....For 3 points um_ptstopln will give the approx fit , since it works
.....iteratively. check if these points are collinear. If they are collinear
.....a plane cannot be constructed but if they are not return the plane.
*/
	if (npts ==3)
	{
		col = um_3pt_colinear(pts[0],pts[1],pts[2],nvec,tol8);
		if (col) goto failed;
		else
		{
			um_vctovc(nvec,unvc);
			um_vctovc(pts[0],ppt);
		}
	}

/*
.....Get plane of curve
*/
/*
..... old logic
	for (i=2;i<npts;i++)
	{
		if (um_plane1(pts[0],pts[1],pts[i],&uplane)) break;
	}
*/
/*
.....new logic
.....construct an average plane and verify if all points lie on this plane
*/
	else
		if(um_ptstopln(npts,pts,ppt,unvc,tol8)==-1) goto failed;

	plane[3] = 0.;
	um_vctovc(unvc,plane);
	plane[3] = um_dot(plane,ppt);
/*
.....Make sure Z-axis is pointing positive
*/
	if (plane[2] < 0.)
	{
		plane[3] = plane[3] * -1.;
		um_vctmsc(plane,-1.,plane);
	}
/*
.....Verify all points lie on plane
*/
	for (i=0;i<npts;i++)
	{
		if (um_dsupt(plane,pts[i]) > tol8) goto failed;
	}
/*
.....Determine X-axis and Y-axis of plane
*/
	um_vcmnvc(pts[1],pts[0],xaxis);
	um_unitvc(xaxis,xaxis);
	um_cross(plane,xaxis,yaxis);
	stat = UU_TRUE;
	goto done;
/*
.....Curve is not coplanar
*/
failed:;
	stat = UU_FALSE;
/*
.....End of routine
*/
done:;
	return(stat);
}

