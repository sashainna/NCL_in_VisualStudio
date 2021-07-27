/*********************************************************************
**    NAME         :  m4e2da.c
**       CONTAINS: 2D analysis routines
**       um_2d_props()
**       um_polygon_props()
**       um_ccrv_props()
**       um_ccrv_to_pts()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4e2da.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:01
*********************************************************************/
#include "umath.h"
#include "udebug.h"
#include "mdcoord.h"
#include "usysdef.h"
#include "mdebug.h"
#include "mcrv.h"
#include "mdrel.h"
#include "modef.h"
#include "ustdio.h"
#include "ulist.h"
#include "class.h"
#include "mdeval.h"
#include "m2dattr.h"
#include "mattr.h"

/*********************************************************************
**    E_FUNCTION     : um_2d_props(p,numpts,area,perimeter,xbar,ybar,
**                   				  Ixo,Iyo,xaxis,plane_normal)
**       Calculate the cross sectional properties of a planar area
**       defined by a buffer of 2-D points
**    PARAMETERS   
**       INPUT  : 
**          p	          buffer of points of a closed planar area 
**	         numpts        number of points in the area buffer 
**       OUTPUT :  
**          area,         area of polygon 
**          perimeter,    perimeter of trapazoid (sum of dp's) 
**          xbar,         centroid of polygon in x direction wrt model origin 
**          ybar,         centroid of polygon in y direction wrt model origin 
**          Ixo,          m.o.i. wrt centroidal x axis of a polygon 
**          Iyo,          m.o.i. wrt centroidal y axis of a polygon 
**          xaxis,        x axis to calculate props 
**          plane_normal  normal to calculation plane
**    RETURNS      : none
**    WARNINGS     : none
*********************************************************************/

um_2d_props(p,numpts,area,perimeter,xbar,ybar,Ixo,Iyo,xaxis,plane_normal) 

int     numpts;       /* number of points in buffer */
UM_coord p[];        /* points buffer */
UM_vector plane_normal,
          xaxis;
UU_REAL *area,        /* area of polygon */
        *perimeter,   /* perimeter of trapazoid (sum of dp's) */
        *xbar,        /* centroid of polygon in x direction wrt model origin */
        *ybar,        /* centroid of polygon in y direction wrt model origin */
        *Ixo,         /* m.o.i. wrt centroidal x axis of a polygon */
        *Iyo;         /* m.o.i. wrt centroidal y axis of a polygon */
  {
  int     i;           /* counter */
  UU_REAL Ix,          /* m.o.i. wrt centroidal y axis of trapazoid */
          Ixsum,       /* sum of Ix's for all trapazoids */
          Ixorigin,    /* m.o.i. wrt origin x axis of a  trapazoid */
          Iy,          /* m.o.i. wrt centroidal y axis of trapazoid */
          Iysum,       /* sum of Iy's for all trapazoids */
          Iyorigin,    /* m.o.i. wrt origin y axis of a  trapazoid */
          a,           /* sum of areas of the trapazoids */
          atrap,       /* area of a trapazoid */
          x,           /* dist. from origin x axis to trap. centroid */
          y,           /* dist. from origin y axis to trap. centroid */
          ax,          /* sum of products of a and x for trapazoids */
          ax2,         /* sum of products of a and x**2 for trapazoids */
          ay,          /* sum of products of a and y for trapazoids */
          ay2,         /* sum of products of a and y**2 for trapazoids */
          origin[2],   /* point which will place polygon in 1st quadrant */
          du,          /* x length of trapazoid */
          dv,          /* difference in y length of trapazoid */
          v,           /* length of shorter leg of trapazoid */
          sumheight,   /* sum of the heights of the trap. legs */
          dp;          /* perimeter contribution of each trapazoid */
 UM_transf tf1,
           tf2,
			  tfmat1,     /* trans matrices */
			  tfmat;
 UM_angle  ang;        /* angle between plane normal and z axis */
 UM_vector axis1,
			  axis2,
			  nvect,      /* normal vector */
			  new_xaxis;  /* new x axis */

  uu_denter(UU_MTRC,(us,"um_2d_props()"));

 /* initialize summing variables */
  a = 0.;
  ax = 0.;
  ax2 = 0.; 
  ay = 0.;
  ay2 = 0.; 
  Ixsum = 0.;
  Iysum = 0.;
  *perimeter = 0;
  origin[0] = UM_MAXREAL;
  origin[1] = UM_MAXREAL;

  um_identtf(tf1);

  if (um_cceqcc(plane_normal, UM_zaxis))
	  um_tftotf(tf1, tfmat);
  else
	  {
     um_cross(plane_normal, UM_zaxis, nvect);
     um_unitvc(plane_normal, plane_normal);
     um_unitvc(nvect, nvect);
     ang = um_angle2p(plane_normal ,UM_zaxis, nvect);
     um_rottf(nvect, ang, tf2);
     um_tftotf(tf2, tfmat);
     }

  um_unitvc(new_xaxis, new_xaxis);
  um_cctmtf(xaxis, tfmat, new_xaxis);

  if (um_cceqcc(new_xaxis, UM_xaxis))
	  um_tftotf(tf1, tfmat1);
  else
	  {
     um_cross(new_xaxis, UM_xaxis, nvect);
     um_unitvc(new_xaxis, new_xaxis);
     um_unitvc(nvect, nvect);
     ang = um_angle2p(new_xaxis ,UM_xaxis, nvect);
     um_rottf(nvect, ang, tf2);
     um_tftotf(tf2, tfmat1);
     }

/* calculate z offset of the xy plane */
  um_cctmtf(p[0], tfmat, p[0]);
  um_cctmtf(p[0], tfmat1, p[0]);

/* transform all points to a plane parellel to the x y plane with tfmat */
/* calculate the origin for computation (min x and y) */
/* make sure all points are planar */
  for (i=1; i < numpts; i++)
    {
	 um_cctmtf(p[i], tfmat, p[i]);
	 um_cctmtf(p[i], tfmat1, p[i]);
   if (p[i][0] < origin[0])
     origin[0] = p[i][0];
   if (p[i][1] < origin[1])
     origin[1] = p[i][1];
    }

/* calculate areas centroids and moments of the trapazoids */
/* transform all points to the first quadrant */
  for (i = 0; i < numpts; i++)
    {
    p[i][0] = p[i][0] - origin[0];
    p[i][1] = p[i][1] - origin[1];
    }


/* calculate areas centroids and moments of the trapazoids */
  for (i=0; i < numpts - 1; i++)
    {
    atrap = (p[i][1] + p[i+1][1])*(p[i+1][0] - p[i][0])/2.;
    a = a + atrap;
    dp = sqrt(pow(fabs(p[i][0] - p[i+1][0]),2.) +
               pow(fabs(p[i][1] - p[i+1][1]),(UU_REAL) 2.));
    *perimeter = *perimeter + dp;
    sumheight = p[i][1] + p[i+1][1];
    if (p[i+1][1] < p[i][1])
      v = p[i+1][1];
    else
      v = p[i][1];
      du = fabs(p[i][0] - p[i+1][0]);
      dv = fabs(p[i][1] - p[i+1][1]);
    if (sumheight != 0.)
      {
      if (p[i][0] <= p[i+1][0]) /* i to i+1 is left to right */
        {
        x = p[i][0] + (((2. * p[i+1][1]) + p[i][1]) * (p[i+1][0] - p[i][0]))/
                                      (3. * sumheight);   
        y = (pow(p[i][1],2.) + (p[i][1] * p[i+1][1]) + pow(p[i+1][1],2.))/
                                      (3. * sumheight);
        Ix = (du * pow(dv,3.)/9.) + (du * pow(v,3.)/3.) +
               (du * dv * pow(v,2.)/2.) +
               (du * v * pow(dv,2.)/3.) - (fabs(atrap) * y * y);
        Iy = (pow(fabs(p[i+1][0] - p[i][0]),3.)) *
               (pow(p[i][1],2.) +
      				(4. * p[i][1] * p[i+1][1]) + pow(p[i+1][1],2.))/
               (36. * sumheight);
        if (atrap < 0.)
          {
          Ix = (0. - Ix);
          Iy = (0. - Iy);
          }
          ax = ax + (atrap * x);
          ay = ay + (atrap * y);
          ax2 = ax2 + (atrap * x * x);
          ay2 = ay2 + (atrap * y * y);
          Ixsum = Ixsum + Ix;
          Iysum = Iysum + Iy;
         }
       else                      /* i to i+1 is right to left */
         {
         x = p[i+1][0] + (((2. * p[i][1]) + p[i+1][1]) * 
										(p[i][0] - p[i+1][0])) /
                                      (3. * sumheight);   
         y = (pow(p[i][1],2.) + (p[i][1] * p[i+1][1]) + pow(p[i+1][1],2.))/
                                      (3. * sumheight);
         Ix = (du * pow(dv,3.)/9.) + (du * pow(v,3.)/3.) +
               (du * dv * pow(v,2.)/2.) +
               (du * v * pow(dv,2.)/3.) - (fabs(atrap) * y * y);
         Iy = (pow(fabs(p[i+1][0] - p[i][0]),3.)) *
               (pow(p[i][1],2.) +
					(4. * p[i][1] * p[i+1][1]) + pow(p[i+1][1],2.))/
               (36. * sumheight);
         if (atrap < 0.)
           {
           Ix = (0. - Ix);
           Iy = (0. - Iy);
           }
         ax = ax + (atrap * x);
         ay = ay + (atrap * y);
         ax2 = ax2 + (atrap * x * x);
         ay2 = ay2 + (atrap * y * y);
         Ixsum = Ixsum + Ix;
         Iysum = Iysum + Iy;
         } 
      }
    }

/* calculate output values */
  *area = fabs(a);
  x = ax/a;
  *xbar = x + origin[0];
  y = ay/a;
  *ybar = y + origin[1];
  if (a < 0.)
    {
    ax2 = (0. - ax2);
    ay2 = (0. - ay2);
    Ixsum = (0. - Ixsum);
    Iysum = (0. - Iysum);
    }
  Ixorigin = Ixsum + ay2;
  *Ixo = Ixorigin - ((*area) * y * y);  
  Iyorigin = Iysum + ax2;
  *Iyo = Iyorigin - ((*area) * x * x); 

  uu_dexit;

  return;
  }

/*********************************************************************
**    E_FUNCTION     : um_polygon_props(key,plane_normal,plane_origin,
**                             xaxis,area,perimeter,Ixo,Iyo,xbar,ybar,error
**
**      Function to calculate the 2-D properties of a planar polygon
**      about the work plane axis
**    PARAMETERS   
**       INPUT  :    key           key id of the polygon 
**                   plane_normal  normal to the work plane 
**                   plane_origin  origin of the work plane 
**                   xaxis         x axis of the work plane 
**       OUTPUT :    area          area of the polygon 
**                   perimeter     perimeter of the polygon
**                   Ixo           moment of inertia about area centroidal
**                                    x axis
**                   Iyo           moment of inertia about area centroidal
**                                    y axis
**                   xbar          x dist. to centroid from work plane axis
**                   ybar          y dist. to centroid from work plane axis
**                   error         0  -  o.k.
**                                 1  -  polygon is not planar
**                                 2  -  polygon not in x-y workplane
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_polygon_props(key,plane_normal,plane_origin,xaxis,area,perimeter,
						Ixo, Iyo,xbar,ybar,error)
	UU_KEY_ID key;            /* key id of polygon */
	UU_REAL *area,            /* area bounded by polygon */
			  *perimeter,       /* perimeter of polygon */
			  *Ixo,             /* moment of int about x centroidal axis */
			  *Iyo,             /* moment of int about y centroidal axis */
			  *xbar,            /* x dist from w.p. origin to centroid */
			  *ybar;            /* y dist from w.p. origin to centroid */
	UM_vector plane_normal,   /* normal to work plane */
				 xaxis;          /* w.p. x axis */
	UM_coord	plane_origin;    /* w.p. origin */
   int *error;               /* error flag */
	{
	int rel_num;              /* relation number */
	int coplanar;             /* coplanar flag */
	int i;                    /* counter */
	int dim;						  /* dimension of space defined by comp. curve */
	UU_REAL space[2][3];      /* space from span */
	UM_vector object_normal,  /* normal to polygon */
	          perp;           /* perpendicular vector */
	UM_coord	plane_x;         /* x vector */
	struct UM_poly_rec pgon;  /* polygon data structure */
	struct UM_point_rec tmp_pt;
	struct UM_attrdata_rec *attrptr;
   UM_transf tfmat;               /* trans matrix */

	uu_denter(UU_MTRC,(us,"um_polygon_props()"));

	pgon.key = key;
	um_get_all_geom(&pgon, sizeof(pgon));
   uc_retrieve_transf(pgon.key, tfmat);

   if (UM_2dattr.disp_flag == UU_TRUE)
		{
		  /* fill-in the temporary point structure with default values */
		  tmp_pt.key 			= 0;
		  tmp_pt.rel_num 		= UM_POINT_REL;
		  tmp_pt.markertype	= UM_ptattr.markertype;
		  tmp_pt.snap_node	= UM_ptattr.snap_node;
		  attrptr				= (struct UM_attrdata_rec *) &UM_attrmdl;
		}
	/* check that polygon is coplanar */
	um_span_entity(&pgon, &dim, space);

   /* check that the polygon lies in the predefined calculation plane */
	um_vcplvc(plane_origin,xaxis,plane_x);
  object_normal[0] = space[1][0];
  object_normal[1] = space[1][1];
  object_normal[2] = space[1][2];
  um_unitvc(object_normal, object_normal);
  um_unitvc(plane_normal, plane_normal);
  if (um_cceqcc(space[0],plane_origin) == UU_FALSE)
    {
    perp[0] = space[0][0] - plane_origin[0];
    perp[1] = space[0][1] - plane_origin[1];
    perp[2] = space[0][2] - plane_origin[2];
    }
	else
    {
    perp[0] = space[0][0] - plane_x[0];
    perp[1] = space[0][1] - plane_x[1];

    perp[2] = space[0][2] - plane_x[2];
		}

  if (um_vcparall(plane_normal, object_normal) == UU_TRUE &&
			um_vcperp(plane_normal, perp) == UU_TRUE)
		coplanar = 0;
  else
		coplanar = 1;

  if (dim == 2 && coplanar == 0) /* planar curve */
	  {
	  /* calculate props */
	  *error = 0;

	  /* transform points to local coordinate origin */
	  for (i=0; i<pgon.numvtx; i++)
		  {
		  if (UM_2dattr.disp_flag == UU_TRUE)
				{
				tmp_pt.pt[0] = pgon.vertex[i][0];
				tmp_pt.pt[1] = pgon.vertex[i][1];
				tmp_pt.pt[2] = pgon.vertex[i][2];
				um_drw1_pt(&tmp_pt, tfmat, attrptr);
				}
		  um_vcmnvc(pgon.vertex[i],plane_origin,pgon.vertex[i]);
		  }
	  um_2d_props(pgon.vertex,pgon.numvtx,area,perimeter,
				  xbar,ybar,Ixo,Iyo,xaxis,plane_normal);
	  }
  else
	  {
	  if (dim != 2)
      *error = 1;
	  if (coplanar == 1)
      *error = 2;
	  }

  uu_dexit;
  }

/*********************************************************************
**    E_FUNCTION     : um_ccrv_props(key,plane_normal,plane_origin,
**                             xaxis,area,perimeter,Ixo,Iyo,xbar,ybar,error
**
**      Function to calculate the 2-D properties of a planar composite
**      curve
**    PARAMETERS   
**       INPUT  :    key           key id of the comp curve 
**                   plane_normal  normal to the work plane 
**                   plane_origin  origin of the work plane 
**                   xaxis         x axis of the work plane 
**       OUTPUT :    area          area bounded by comp curve  
**                   perimeter     perimeter of the  comp curve
**                   Ixo           moment of inertia about area centroidal
**                                    x axis
**                   Iyo           moment of inertia about area centroidal
**                                    y axis
**                   xbar          x dist. to centroid from work plane axis
**                   ybar          y dist. to centroid from work plane axis
**                   error         0  -  o.k.
**                                 1  -  unsupported curve in ccrv
**                                         (lines and arc supported)
**                                 2  -  ccrv is not planar
**                                 3  -  ccrv not in x-y workplane
**                                 4  -  ccrv is not closed
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ccrv_props(key,plane_normal,plane_origin,xaxis,area,perimeter,
						Ixo, Iyo,xbar,ybar,error)
	UU_KEY_ID key;            /* key id of comp curve */
	UU_REAL *area,            /* area bounded by comp curve */
			  *perimeter,       /* perimeter of comp curve */
			  *Ixo,             /* moment of int. about centroidal x axis */
			  *Iyo,             /* moment of inertia about centroidal y axis */
			  *xbar,            /* x dist from wp axis to centroid */
			  *ybar;            /* y axis from wp axis to centroid */
	UM_vector plane_normal,   /* normal to w.p. */
				 xaxis;          /* x axis vector of w.p. */
	UM_coord	plane_origin;    /* w.p. origin */
   int *error;               /* error status */
	{
	struct UM_compcrv_rec ccrv; /* comp curve structure */
	int rel_num;              /* relation number */
	int i;                    /* counter */
	int pt_num;               /* number of points comp curv defined with*/
	int dim;						  /* dimension of space defined by comp. curve */
	UM_coord *pt_buf;         /* point buffer */
  UU_LIST list;             /* linked list for point buffer */
	int cnt = 800;            /* initial and expansion size of linked list */
	UM_coord	plane_x;         /* point on x axis */
	UU_REAL space[2][3];      /* entity span */
	UM_vector object_normal,  /* normal to comp curve */
	          perp;           /* perpendicular vector */
	int coplanar;             /* flag for coplanar check */

	uu_denter(UU_MTRC,(us,"um_ccrv_props()"));

   /* access memory for point buffer */
   uu_list_init(&list,sizeof(UM_coord),cnt,cnt);

   um_vcplvc(plane_origin,xaxis,plane_x);

   ccrv.key = key;
   uc_retrieve_data(&ccrv, sizeof(ccrv));

   uc_span_entity(&ccrv, &dim, space);

   /* check that the ccrv lies in the predefined calculation plane */
   object_normal[0] = space[1][0];
   object_normal[1] = space[1][1];
   object_normal[2] = space[1][2];
   um_unitvc(object_normal, object_normal);
   um_unitvc(plane_normal, plane_normal);
   if (um_cceqcc(space[0],plane_origin) == UU_FALSE)
	   {
	   perp[0] = space[0][0] - plane_origin[0];
	   perp[1] = space[0][1] - plane_origin[1];
	   perp[2] = space[0][2] - plane_origin[2];
	   }
	 else
	   {
	   perp[0] = space[0][0] - plane_x[0];
	   perp[1] = space[0][1] - plane_x[1];
		 perp[2] = space[0][2] - plane_x[2];
		 }
	 if (um_vcparall(plane_normal, object_normal) == UU_TRUE &&
					um_vcperp(plane_normal, perp) == UU_TRUE)
	   coplanar = 0;
   else
		 coplanar = 1;

   /* check to see if comp. curve is planar and closed */
   if (ccrv.planar == UU_TRUE && ccrv.open == UU_FALSE && coplanar == 0)
	   {
	   /*convert composite curve to array of points*/
     um_ccrv_to_pts(key,&list,&pt_num,error);
     pt_buf =  (UM_coord *) UU_LIST_ARRAY(&list);

     if (*error == 0)
		   {
	     /* calculate props */

       /* transform points to local coordinate origin */
	     for (i=0; i<pt_num; i++)
			 um_vcmnvc(pt_buf[i],plane_origin,pt_buf[i]);

       um_2d_props(pt_buf,pt_num,area,perimeter,
                 xbar,ybar,Ixo,Iyo,xaxis,plane_normal);
	     }
		 else
		   {
			 /* unsupported curve type in composite curve */
			 *error = 1;
       }
     }
   else
	   {
	   if (ccrv.planar == UU_FALSE)
	     *error = 2;
	   if (coplanar == 1)
	     *error = 3;
	   if (ccrv.open == UU_TRUE)
	     *error = 4;
	   }

   /* free point buffer memory */
	uu_list_free(&list);

  uu_dexit;
  }
 
/*********************************************************************
**    E_FUNCTION     : um_ccrv_to_pts(key,list,numpts,error)
**      Function to convert a composite curve into a buffer of points.
**      This function presently supports composite curves with lines,
**      arcs, conics, and bsplines.
**
**    PARAMETERS   
**       INPUT  :   key     key to the composite curve
**       OUTPUT :   list    buffer of 3-D points describing the composite curve 
**                  numpts  the number of points calculated
**                  error   error flag
**                            1 - unsupported curve in compsite curve
**                                 (presently supports lines, arcs, conics
**                                  and bsplines)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_ccrv_to_pts(key,list,numpts,err)

UU_KEY_ID	key;			/* key id to the composite curve */
UU_LIST		*list;		/* linked list for point buffer */
int			*numpts;	   /* number of points */
int			*err;			/* error flag */

  {
  int		beg;				/* beginning value for evaluating point on span */
  int		del;				/* delta value for looping through points on span */
  int    i,j;           /* loop counters */
  int    status;
  UU_REAL param;

  UM_coord  list_point;          /* point to put on linked list */
  UM_coord  lpt;                 /* last point of comp curv */
  UM_transf tfmat;               /* trans matrix */
  UM_transf subcrv_tfmat;

  struct UC_entitydatabag *crv;
  struct UM_compcrv_rec ccrv;    /* composite curve data stucture */
  struct UM_evcrvout evcrv;
  struct UM_attrdata_rec *attrptr;
  struct UM_point_rec tmp_pt;

  uu_denter(UU_MTRC,(us,"um_ccrv_to_pts()"));

  crv = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));

  *err = 0;
  *numpts = 0;
  ccrv.key = key;

  if (UM_2dattr.disp_flag == UU_TRUE)
		{
		  /* fill-in the temporary point structure with default values */
		  tmp_pt.key 			= 0;
		  tmp_pt.rel_num 		= UM_POINT_REL;
		  tmp_pt.markertype	= UM_ptattr.markertype;
		  tmp_pt.snap_node	= UM_ptattr.snap_node;
		  attrptr				= (struct UM_attrdata_rec *) &UM_attrmdl;
		}

  uc_retrieve_data(&ccrv, sizeof(ccrv));
  uc_retrieve_transf(ccrv.key, tfmat);

  /* get first point of composite curve --don't bother to
	  to display it, last point is the same and will display */
  um_get_endpts(&ccrv, tfmat, list_point, lpt);
  uu_list_push(list, list_point);
  *numpts = *numpts + 1;

  /* determine the rest of the points in the composite curve */

  for (i=0; i < ccrv.no_cid; i++) 
      {
      crv->key = ccrv.cid[i].crvid;
    	uc_retrieve_data(crv, sizeof(struct UC_entitydatabag));
		uc_retrieve_transf(crv->key, subcrv_tfmat);
		um_tftmtf(subcrv_tfmat, tfmat, subcrv_tfmat);
 	  	switch(crv->rel_num)
			{
			case UM_LINE_REL:
				{
        		um_get_endpts(crv, tfmat, list_point, lpt);

 				/* make sure point evaluation is in the correct direction */
        		if (ccrv.cid[i].reverse == UU_FALSE)
	         	um_vctovc(lpt ,list_point);
				uu_list_push(list, list_point);
				*numpts = *numpts + 1;
				if (UM_2dattr.disp_flag == UU_TRUE)
					{
					tmp_pt.pt[0] = list_point[0];
					tmp_pt.pt[1] = list_point[1];
					tmp_pt.pt[2] = list_point[2];
					um_drw1_pt(&tmp_pt, tfmat, attrptr);
					}
				}
				break;
			case UM_CIRCLE_REL:
			case UM_CONIC_REL:
			case UM_AGCRV_REL:
				{
 				/* set direction of point evaluation based on curve direction */
        		if (ccrv.cid[i].reverse == UU_FALSE)
					{
					beg = 0;
					del = 1;
					}
				else
					{
					beg = UM_2dattr.pts_per_span;
					del = -1;
					}
				uc_init_evcrvout(crv, &evcrv);

				/* loop through curve generating intermediate points

					NOTE: the counter does not start at the 1st point!
							This is because the 1st and last points of
							each sub-curve are the same!							*/
				for (j=1; j<=UM_2dattr.pts_per_span; j++)
					{
					param = (UU_REAL ) (beg + (del*j))/UM_2dattr.pts_per_span;
					uc_evcrv(UM_POINT, param, crv, tfmat, &evcrv);
					uu_list_push(list, evcrv.cp);
					*numpts = *numpts +1;
					if (UM_2dattr.disp_flag == UU_TRUE)
						{
						tmp_pt.pt[0] = evcrv.cp[0];
						tmp_pt.pt[1] = evcrv.cp[1];
						tmp_pt.pt[2] = evcrv.cp[2];
						um_drw1_pt(&tmp_pt, tfmat, attrptr);
						}
					}
  				}
  				break;
			default:
				/* unsupported curve for 2D props in composite curve */
        		*err = 1;
        		goto done;
        		break;
			}
  		}
  done:;
  uu_free(crv);
  uu_dexit;
  return;
}
