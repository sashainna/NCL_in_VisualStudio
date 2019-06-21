/*******************************************************************
**    NAME         :  m1mcrv.c
**       CONTAINS:
**      int um_allocate_curve (eptr,eptr2,size)
**      int um_alloc_eq_curve (eptr,eptr2)
**      int um_rbsplcrv_size (eptr,size)
**      int um_curve_size (eptr)
**		  int um_ssplin_size (eptr)
**		  int um_cvsplin_size (eptr, relnum)
**      int um_geometry_size (eptr,size)
**      int um_alloc_eq_geom (eptr,eptr2)
**
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m1mcrv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:44
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "class.h"
#include "mcrv.h"
#include "modef.h"
#include "ribase.h"
#include "mdeval.h"
#include "mdpick.h"
#include "mdebug.h"
#include "nccs.h"

/*********************************************************************
**    E_FUNCTION     : int um_allocate_curve (eptr2,size)
**      Allocates space for curve entity of the given size as input.
**    PARAMETERS   
**       INPUT  : 
**          size        number of bytes to allocate.
**       OUTPUT :  
**          eptr2       pointer to allocated structure of the 
**                      size as spcefied.
**    RETURNS      : 
**      index of closest point
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_allocate_curve (eptr2,size)
   char **eptr2;
   int size;
  {
   struct UM_crvdatabag *optr2;

   optr2  = UU_NULL;
   if (size > 0) optr2 = (struct UM_crvdatabag *) uu_toolmalloc(size);
  
   *eptr2 = (char *) optr2;
   return (UU_SUCCESS);
  }

/*********************************************************************
**    E_FUNCTION     : int um_alloc_eq_geom (eptr,eptr2)
**      Allocates space for curve entity of the given size as input.
**    PARAMETERS   
**       INPUT  : 
**          size        number of bytes to allocate.
**       OUTPUT :  
**          eptr2       pointer to allocated structure of the 
**                      size as spcefied.
**    RETURNS      : 
**      index of closest point
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_alloc_eq_geom (eptr,eptr2)
   struct UM_crvdatabag *eptr;
   char **eptr2;
  {
   struct UM_crvdatabag *optr2;
   int size;

   optr2  = UU_NULL;
   size   = um_geometry_size(eptr);
   if (size > 0) optr2 = (struct UM_crvdatabag *) uu_toolmalloc(size);
  
   *eptr2 = (char *) optr2;
   return (UU_SUCCESS);
  }
/*********************************************************************
**    E_FUNCTION     : int um_alloc_eq_curve (eptr,eptr2)
**      Allocates space for curve entity of the given size as input.
**    PARAMETERS   
**       INPUT  : 
**          size        number of bytes to allocate.
**       OUTPUT :  
**          eptr2       pointer to allocated structure of the 
**                      size as spcefied.
**    RETURNS      : 
**      index of closest point
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_alloc_eq_curve (eptr,eptr2)
   struct UM_crvdatabag *eptr;
   char **eptr2;
  {
   struct UM_crvdatabag *optr2;
   int size;

   optr2  = UU_NULL;
   size   = um_curve_size(eptr);
   if (size > 0) optr2 = (struct UM_crvdatabag *) uu_toolmalloc(size);
  
   *eptr2 = (char *) optr2;
   return (UU_SUCCESS);
  }

/*
....jingrong 9/18/98: higher level rountines um_ssplin_size & um_curve_size 
... are created, which call um_cvsplin_size( the original um_curve_size rountine).
*/

/*********************************************************************
**    E_FUNCTION     : int um_curve_size (eptr)
**      Calculate the size of the RB curve necessary to fit the input
**      curve definition when it is converted to RB curve. The
**      calculated size is slitly biger so it can hold RB curve with
**      one extra knot which can be usefull when curve is splited
**      by calling um_c7_splitrbsplcrv.
**    PARAMETERS
**       INPUT  :
**          eptr        pointer to geometry structure
**       OUTPUT :
**          none
**    RETURNS      :
**          size        number of bytes.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_curve_size (eptr)
	struct UM_crvdatabag *eptr;
  {
	int relnum;
	int csize;
	relnum = UM_RBSPLCRV_REL;
	csize = um_cvsplin_size(eptr, relnum);
	return (csize);
	}

/*********************************************************************
**    E_FUNCTION     : int um_ssplin_size (eptr)
**      Calculate the size of the ssplin curve necessary to fit the input
**      curve definition when it is converted to ssplin curve. 
**    PARAMETERS
**       INPUT  :
**          eptr        pointer to geometry structure
**       OUTPUT :
**          none
**    RETURNS      :
**          size        number of bytes.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_ssplin_size (eptr)
   struct UM_crvdatabag *eptr;
  {
   int relnum;
	int ssize;
   relnum = UM_UVCVONSF_REL;
	ssize = um_cvsplin_size(eptr, relnum);
   return (ssize);
   }

/*********************************************************************
**    E_FUNCTION     : int um_cvsplin_size (eptr,relnum)
**      Calculate the size of the RB/SSPLIN curve necessary to fit the input
**      curve definition when it is converted to RB/SSPLIN curve. The 
**      calculated size is slitly biger so it can hold RB curve with
**      one extra knot which can be usefull when curve is splited
**      by calling um_c7_splitrbsplcrv.
**    PARAMETERS   
**       INPUT  : 
**          eptr        pointer to geometry structure
**				RELNUM		relation number of the converted curve
**       OUTPUT :  
**          none
**    RETURNS      : 
**          size        number of bytes.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cvsplin_size (eptr, RELNUM)
   struct UM_crvdatabag *eptr;
	int RELNUM;
  {
   int size, n, k;
	
   n     = UR_rcb[RELNUM].tuple_size;
   switch (eptr->rel_num)
    {
    case UM_LINE_REL:
      k     = n + 4*8 * sizeof (UU_REAL);
      break;
    case UM_CIRCLE_REL:
      k     = n + 14*8 * sizeof (UU_REAL);
      break;
    case UM_CONIC_REL:
      k     = n + 14*8 * sizeof (UU_REAL);
      break;
    case NCL_CURVE_REL:
     {
      struct NCL_curve_rec *cv;
      cv    = (struct NCL_curve_rec *) eptr;
      k     = 3*cv->no_segment + 2 + 3;
      k     = n + 8*k * sizeof (UU_REAL);
     }
      break;
    case UM_RBSPLCRV_REL:
     {
      struct UM_rbsplcrv_rec *cv;
      cv    = (struct UM_rbsplcrv_rec *) eptr;
      k     = n + 8*(cv->no_t+cv->k) * sizeof (UU_REAL); 
     }
      break;
/*
... jingrong 9/18/98
*/
    case UM_UVCVONSF_REL:
     {
      struct UM_uvcvonsf_rec *cv;
      cv    = (struct UM_uvcvonsf_rec *) eptr;
      k     = n + 8*(cv->no_t+cv->k) * sizeof (UU_REAL);
     }
      break;
/*
.....Added polylines. JLS 10/19/99
*/
	case UM_POLYLINE_REL:
	{
		struct UM_polyline_rec *cv;
		cv = (struct UM_polyline_rec *) eptr;
		k  = n + 8*(cv->no_pt + 4)*sizeof(UU_REAL);
	}
		break;
	case UM_COMPCRV_REL:
	{
		struct UM_compcrv_rec *cv;
		cv    = (struct UM_compcrv_rec *) eptr;
		k     = cv->no_cid * (sizeof (UU_REAL) + sizeof(struct UM_cid_rec) + 4);
		k     = n + k + 64;
	}
		break;

	default:
      k      = 0;
      break;
    }
  size = k;

  uu_dexit;
  return (size);
  }

/*********************************************************************
**    E_FUNCTION     : int um_geometry_size (eptr)
**      Calculate the size of the data structure to store input entity
**    PARAMETERS   
**       INPUT  : 
**          eptr        pointer to geometry structure
**       OUTPUT :  
**          none
**    RETURNS      : 
**          size        number of bytes.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_geometry_size (eptr)
struct UM_crvdatabag *eptr;
{
	int size, n, k;

	n     = UR_rcb[eptr->rel_num].tuple_size;
	switch (eptr->rel_num)
	{
		case UM_POINT_REL:
			k     = n + 64;
			break;
		case UM_LINE_REL:
			k     = n + 64;
			break;
		case UM_CIRCLE_REL:
			k     = n + 64;
			break;
		case UM_CONIC_REL:
			k     = n + 64;
			break;
		case UM_POLY_REL:
			k     = n + 64;
			break;
		case UM_POLYLINE_REL:
		{
			struct UM_polyline_rec *cv;
			cv    = (struct UM_polyline_rec *) eptr;
			k     = cv->no_pt * (2*sizeof (UU_REAL) + 4);
			k     = n + k + 64;
		}
			break;
		case UM_COMPCRV_REL:
		{
			struct UM_compcrv_rec *cv;
			cv    = (struct UM_compcrv_rec *) eptr;
			k     = cv->no_cid * (sizeof (UU_REAL) + sizeof(struct UM_cid_rec) + 4);
			k     = n + k + 64;
		}
			break;
		case NCL_CURVE_REL:
		{
			struct NCL_curve_rec *cv;
			cv    = (struct NCL_curve_rec *) eptr;
			k     = 3*cv->no_segment + 2 + 3;
			k     = n + 8*k * sizeof (UU_REAL);
		}
			break;
		case UM_RBSPLCRV_REL:
		{
			struct UM_rbsplcrv_rec *cv;
			cv    = (struct UM_rbsplcrv_rec *) eptr;
			k     = n + 8*(cv->no_t+cv->k) * sizeof (UU_REAL); 
		}
			break;
		default:
			k      = 0;
			break;
	}
	size = k;
		
	uu_dexit;
	return (size);
}

