
/*********************************************************************
**    NAME         :  mmsmathf.c
**			all function are used for NCL and MSLITE
**       CONTAINS:
**			um_phicmp
**			um_replac
**			um_reverse_circle
**			um_dploccc
**			um_nearest_to_ploc
**			um_isect_boxes
**			um_ilncir
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**        mmsmathf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**        04/29/15 , 15:08:14
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "modef.h"
#include "mdcoord.h"
#include "mfort.h"
#include "mcrv.h"

#include "gmat4.h"
#include "dasnog.h"

struct hull{								/* data for convex hull calculation */
	UU_REAL phi;
	UU_REAL rad;
	UU_REAL x;
	UU_REAL y;
	int index;
};
/*********************************************************************
**    E_FUNCTION     : um_phicmp(e1,e2)
**       Sort routine for the convex hull algorithm.
**    PARAMETERS   
**       INPUT  : 
**				e1									first element to be compared
**				e2									second element
**       OUTPUT :  
**				none
**    RETURNS      :  1 if  e1->phi < e2->phi
**							-1 if e1->phi > e2->phi
**							 0 if e1->phi = e2->phi
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_phicmp(e1,e2)

	struct hull *e1,*e2;

	{
	UU_REAL t1,t2;

	/*------------------------------------------------------------------------
	**  Start of Executable Code
	**-----------------------------------------------------------------------*/

	t1 = e1->phi;
	t2 = e2->phi;

	if(t1 > t2) return(1);

	else if(t1 < t2) return(-1);

	else return(0);

	}
/*********************************************************************
**    E_FUNCTION     : um_replac(e1,e2)
**       Replace data in the convex hull structure array position e1 
**			with that in e2.
**    PARAMETERS   
**       INPUT  : 
**				e1									first element
**				e2									second element
**       OUTPUT :  
**				none
**    RETURNS      :  none 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_replac(e1,e2)

	struct hull *e1,*e2;

	{

	/*------------------------------------------------------------------------
	**  Start of Executable Code
	**-----------------------------------------------------------------------*/


	e1->phi = e2->phi;
	e1->rad = e2->rad;
	e1->x = e2->x;
	e1->y = e2->y;
	e1->index = e2->index;
	}
/*********************************************************************
**    E_FUNCTION     : int um_reverse_circle(eptr)
**         Reverse the parameterization of the circle.
**    PARAMETERS   
**       INPUT  : 
**          eptr                  pointer to circle
**       OUTPUT :  
**          eptr                  pointer to circle
**    RETURNS      : 
**         UU_SUCCESS iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_reverse_circle(eptr)
   struct UM_circle_rec *eptr;

   {
   UM_transf rotmat;

   uu_denter(UU_MTRC,(us,"um_reverse_circle(key=%d)", eptr->key));

   um_rotlntf(eptr->center, eptr->nvec, eptr->dang, rotmat);
   um_vctmtf(eptr->svec, rotmat, eptr->svec);
   eptr->dang = -eptr->dang;

   uu_dexit;
   return (UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : UU_REAL um_dploccc(ploc, cc)
**      Calculate a distance metric between a picked location (returned by
**       DIGS) and a cartesian model coordinate by using DIGS to determine
**       the transformed  coordinate and simply calculating the square of the
**       NDC distance.
**    PARAMETERS   
**       INPUT  : 
**          ploc                 picked location
**          cc                   cartesian  coordinate
**       OUTPUT :  
**          none
**    RETURNS      : 
**       distance metric between points (in NDC space)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL
um_dploccc(ploc, cc)
   UD_NDCLOCREC *ploc;
   UM_coord cc;

   {
   Gnpoint3 ndc;
   UU_REAL x,y;                            /* NDC  coordinates */
   UU_REAL dist;

   uu_denter(UU_MTRC,(us,"um_dploccc(tran=%d,ndc=(%f,%f,%f),cc=(%f,%f,%f)",
      ploc->transform,ploc->cord[0],ploc->cord[1],ploc->cord[2],
      cc[0],cc[1],cc[2]));

   UG_XFORM(cc[0], cc[1], cc[2], &ndc, ploc->wndc3_mat); 
   x = ndc.x - ploc->cord[0];
   y = ndc.y - ploc->cord[1];
   dist = (x*x) + (y*y);
   uu_dexit;
   return (dist);
   }
/*********************************************************************
**    E_FUNCTION     : int um_nearest_to_ploc(ploc,npts,ptary)
**       Given an array of points (cartesian modeling  coordinates) and 
**       a pick location (NDC space), return the index of the closest
**       point to the pick location.
**    PARAMETERS   
**       INPUT  : 
**          ploc            pick location
**          npts            number of points in array
**          ptary           array of points (world  coordinate)
**       OUTPUT :  
**          none
**    RETURNS      : 
**       index of closest point
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_nearest_to_ploc(ploc,npts,ptary)
   UD_NDCLOCREC *ploc;
   int npts;
   UM_coord ptary[];

   {
   int closest;                  /* index of closest point */
   UU_REAL dist, mindist;        /* distance (minimum distance) to point */
   UU_REAL um_dploccc();
   int i;                        /* index */

   uu_denter( UU_MTRC,(us,"um_nearest_to_ploc(?,?,?)"));
   if (npts  ==  0) closest = -1;
   else
      {
      for (i = 0; i < npts; i++)
         {
         dist = um_dploccc(ploc, ptary[i]);
         if ( (i == 0) || (dist < mindist) )
            {
            closest = i;
            mindist = dist;
            }
         }
      }
   uu_dexit;
   return (closest);
   }

/*********************************************************************
**    E_FUNCTION: um_isect_boxes (bxx1, bxy1, bxx2, bxy2, tol)
**       Checks if two boxes overlap within given tolerance
**    PARAMETERS
**       INPUT  :
**          bxx1  - min X & max X of the first box. 
**          bxy1  - min Y & max Y of the first box. 
**          bxx2  - min X & max X of the second box. 
**          bxy2  - min Y & max Y of the second box. 
**          tol   - tolerance
**       OUTPUT :
**    RETURNS      :   0 = boxes do not overlap
**                     1 = boxes overlap
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_isect_boxes (bxx1, bxy1, bxx2, bxy2, tol) 
UU_REAL *bxx1, *bxy1, *bxx2, *bxy2, tol;
{
	int n;
	
	n = (bxx1[0] > bxx2[1]+tol || bxx1[1] < bxx2[0]-tol ||
		  bxy1[0] > bxy2[1]+tol || bxy1[1] < bxy2[0]-tol)? 0: 1;

	return(n);
}  


/*********************************************************************
**    E_FUNCTION     : um_ilncir(lpt,ulvc,cpt,unvc,rad,nint,pt)
**      Intersect a line and a circle.
**    PARAMETERS   
**       INPUT  : 
**				lpt            point on line
**          ulvc           unit vector along line
**          cpt            center point of circle
**          unvc           unitnormal to plane of circle
**          rad            radius of circle
**       OUTPUT :  
**				nint            number of intersection points
**          pt              array of intersection points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ilncir(lpt,ulvc,cpt,unvc,rad,nint,pt)
	UM_coord lpt;
	UM_vector ulvc;
	UM_coord cpt;
	UM_vector unvc;
	UM_length rad;
	int  *nint;
	UM_coord pt[];

	{
	UM_coord npt;				/* nearest point on line to center of circle */
	UM_length dist;			/* distance from center to near point */
	UM_length len;				/* length of other side of right triangle */
	UM_vector vc;				/* temporary vector */
	UU_LOGICAL in_plane;		/* does line lie in plane of circle */
	int i;						/* index */

	in_plane = um_vcperp(ulvc, unvc);
	if (in_plane  ==  UU_TRUE)
		{
		um_nptln(cpt, lpt, ulvc, npt);
		}
	else
		{
		um_ilnpln(lpt, ulvc, cpt, unvc, nint, npt);
		}
	dist = um_dcccc(cpt, npt);
	if (dist > (rad + UM_FUZZ) ) 
		{
		*nint = 0;
		}
	else if (dist < (rad - UM_FUZZ) )
		{
		if (in_plane  ==  UU_FALSE)
			{
			*nint = 0;
			}
		else
			{
			len = sqrt(rad *rad - dist *dist);
			um_vctmsc(ulvc, len, vc);
			*nint = 2;
			um_vcplvc(npt, vc, pt[0]);
			um_vcmnvc(npt, vc, pt[1]);
			}
		}
	else
		{
		*nint = 1;
		for (i = 0; i < 3; i++) pt[0][i]  =  npt[i];
		}

	return (0);
	}
