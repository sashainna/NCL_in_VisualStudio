/********************************************************************
**
** NAME: m7geom.c
** CONTAINS: geometric functions
** 
**      um_dist_from_line
**      um_sqdis_from_line
**      um_point_is_in_cylinder
**      um_segm_cross_cylinder
**      um_line_cross_cylinder
**      um_line_cross_plane
**      um_points_are_collinear
**      um_plane1
**      um_point_in_2dbox
**      um_line
**      um_2d_box
**      um_3d_box
**      um_cylinder
**      um_3dbox_cross_cylinder
**      um_3d_diameter
**      um_3dbox_around
**      um_2d_strip
**      um_proj_pline_on_circle
**      um_proj_pt_on_circle
**      um_proj_line_circle
**      um_3pt_circle
**      um_evolve_polyline
**      plcvsf
**      um_centroid
**		um_point_in_segment
**		um_points_within_tol
**		um_iSegPlane
**		um_iBoxPlane
**		um_bound_2box
**		um_bound_line_minmaxpt
**		um_boundbox_minmax
**		um_boundbox_between_planes_minmax
**		um_point_bewteen_planes
**		um_get_between_point
**		um_sqdis_from_segment
**
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**     MODULE NAME AND RELEASE LEVEL
**       m7geom.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:08
*********************************************************************/	

#include "mgeom.h"
#include "uminmax.h"
#include "mfort.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"

char *uu_malloc();
#define GO_DONE { status = UU_FAILURE; goto Done; }

/*********************************************************************
** FUNCTION : UU_REAL um_dist_from_line (p, line)
**        returns distance of a point from a line 
** PARAMETERS:
**    INPUT :
**       p - point 
**       line - line
**    OUTPUT : none
** RETURNS:  
**        distance of  p from line 
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_REAL um_dist_from_line (p,line)
UM_coord p;
UM_line *line;
{
	UU_REAL d, um_dot();
	UM_vector v; 
 
	um_vcmnvc(line->p0, p, v);
	d = um_dot(v,line->n);
	d = MAX2 (0., um_dot(v,v) - d*d);
 
	return ( sqrt(d) );
}

/*********************************************************************
** FUNCTION : UU_REAL um_dis_from_line (p,pt,vc)
**      returns 2d distance from a point to a line, which may be negative
**		mean that the point p is in th elft side of line
** PARAMETERS:
**    INPUT :
**       p  - point 
**       pt - line point
**       vc - line unit vector
**    OUTPUT : none
** RETURNS:  
**        squared distance
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_REAL um_dis_from_line (p,pt,vc)
UM_coord p,pt;
UM_vector vc;
{
	UU_REAL f,g,h,d;
	UM_coord pt1;

	um_vcplvc_2d(pt,vc,pt1);
	f = -(pt1[1] - pt[1]);
	g = pt1[0] - pt[0];
	h = pt[0]*pt1[1] - pt1[0]*pt[1];
	d = f*p[0] + g*p[1] + h;

	return (d);
}

/*********************************************************************
** FUNCTION : UU_REAL um_sqdis_from_line (p,pt,vc)
**        returns squared distance from a point to a line 
** PARAMETERS:
**    INPUT :
**       p  - point 
**       pt - line point
**       vc - line unit vector
**    OUTPUT : none
** RETURNS:  
**        squared distance
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_REAL um_sqdis_from_line (p,pt,vc)
UM_coord p,pt;
UM_vector vc;
{
	UU_REAL d,dv;
	UM_vector v; 
 
	um_vcmnvc(p, pt, v);
	d = UM_DOT(v,vc);
	dv = UM_DOT(v,v);
	d = MAX2 (0., dv - d*d);
 
	return (d);
}

/*********************************************************************
** FUNCTION : UU_LOGICAL um_point_is_in_cylinder (p, cyl)
**       Determine whether a point is inside a cylinder within tolerance 
** PARAMETERS:
**    INPUT :
**       p - point 
**       cyl - cylinder
**    OUTPUT : none
** RETURNS:  
**           UU_TRUE if p is inside the cylinder; UU_FALSE otherwise
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_LOGICAL um_point_is_in_cylinder (p,cyl)
UM_coord p;
UM_cylinder *cyl;
{
	UU_REAL d, um_dist_from_line();

	d =  um_dist_from_line(p, &(cyl->axis));
   
	if ((d - cyl->radius) <= UM_DFUZZ)
		return UU_TRUE;
	else
		return UU_FALSE;
}

/*********************************************************************
** FUNCTION : int um_segm_cross_cylinder(cyl,p1,p2,pt,dist)
**      determines whether a segment intersects a cylinder within 1.e-8 
** PARAMETERS:
**    INPUT :
**       p1, p2  - segment endpoints
**       cyl - cylinder
**    OUTPUT : 
**       pt   -  nearest point on the segment to the cyl. axis 	
**       dist - distance from segment to cylinder axis, if return is -1.
** RETURNS:  
**           -1: segment is outside the cylinder
**            0: segment is inside cyl. and || to its axis
**            1: segment intersects cylinder; 
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_segm_cross_cylinder(cyl,p1,p2,pt,dist)
UM_cylinder *cyl;
UM_coord p1,p2,pt;
UU_REAL *dist;
{
	int res;
	UM_vector v, vt;  
	UU_REAL dot, um_dot(), um_dist_from_line(), um_mag(), vmag;
	
/* 
.....find distance between the line and the cyl. axis 
*/
	um_vcmnvc(p2,p1,v);

	vmag = um_mag(v);

	if (vmag <= UM_DFUZZ)
	{
		*dist =  um_dist_from_line(p1, &(cyl->axis));
		um_vctovc (p1,pt);
		res = 0; 
	}
	else
	{
		um_unitvc(v,v);
		res = um_lnlndis(p1, v, cyl->axis.p0, cyl->axis.n, dist, pt); 
	}

/* 
.....line is || to cyl. axis
*/
	if (res == 1)  
	{
		if (*dist > (cyl->radius + UM_DFUZZ)) return (-1);
		else return (0);
	}

/* 
.....line is not parallel to cyl. axis
*/
	um_vcmnvc(pt,p1,vt);
	dot = um_dot(v,vt);
	if (dot < 0.) 
	{
/*
.....pt is outside the segment on the p1 side
*/
		um_vctovc (p1,pt);
		*dist =  um_dist_from_line(p1, &(cyl->axis));
	}
	else if (dot > vmag)
	{
/*
.....pt is outside the segment on the p2 side
*/
		um_vctovc (p2,pt);
		*dist =  um_dist_from_line(p2, &(cyl->axis));
	}

	if ( *dist > (cyl->radius + UM_DFUZZ) ) return (-1);   
	else return (1);
}

/*********************************************************************
** FUNCTION : int um_line_cross_cylinder1 (line,cyl,dis)
**      determines whether a line intersects a cylinder within tolerance 
** PARAMETERS:
**    INPUT :
**       line - line
**       cyl - cylinder
**    OUTPUT : 
**       dis - distance from line to cylinder axis, if return is -1.
** RETURNS:  
**           -1: line is outside the cylinder
**            0: line is inside cyl. and || to its axis
**            1: line intersects cylinder; 
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_line_cross_cylinder1 (line,cyl,dis)
UM_line *line;
UM_cylinder *cyl;
UU_REAL *dis;
{
	int res;
	UM_coord pt;   /* nearest point on line to cyl. axis */	
/* 
.....find distance between the line and the cyl. axis 
*/
	res = um_lnlndis(line->p0, line->n, cyl->axis.p0, cyl->axis.n, dis, pt); 
/*
..... res = 1 means lines are parallel, otherwise res = 0
*/
	if ( *dis > (cyl->radius + UM_DFUZZ) ) return (-1);   
   else return (1 - res);
}

/*********************************************************************
** FUNCTION : int um_line_cross_cylinder (line, cyl, p1, p2)
**      determines whether a line intersects a cylinder within tolerance 
** PARAMETERS:
**    INPUT :
**       line - line
**       cyl - cylinder
**    OUTPUT : 
**       p1, p2  - intersection points
** RETURNS:  
**           -1: line is outside the cylinder
**            0: line is inside cyl. and || to its axis
**            1: line intersects cylinder; 
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_line_cross_cylinder (line,cyl,p1,p2)
UM_line *line;
UM_cylinder *cyl;
UM_coord p1,p2;
{
	UU_REAL dis, a, b, c, t;	
	UU_REAL nlnc,   /* dot prod. of the line & cyl. axis unit vectors:
                      n_line & n_axis */
	        bnc,    /* b dot n_axis */
	        bnl,    /* b dot n_line */				
	        bb;     /* b dot b */		 	
	int res;
	UM_coord pt;   /* nearest point on line to cyl. axis */	
	UM_vector v;   /* intermediate vector */
   UU_REAL um_dot();
	
/* 
.....find distance between the line and the cyl. axis 
*/
	res = um_lnlndis(line->p0, line->n, cyl->axis.p0, cyl->axis.n, &dis, pt); 
/* 
.....line is outside the cylinder 
*/
   if ( dis > (cyl->radius + UM_DFUZZ) ) return -1;
/* 
.....line is inside cyl. and || to its axis
*/
	if (res == 1) return 0;
/* 
.....line intersects cylinder: find points of intersection p1, p2 
*/
	um_vcmnvc(line->p0, cyl->axis.p0, v);					
	bb = um_dot(v, v);
	bnl = um_dot(v, line->n); 
	bnc = um_dot(v, cyl->axis.n); 
	nlnc = um_dot(line->n,cyl->axis.n);
/* 
...coeff. of the quadratic eq. for line parameter t 
*/
	a = 1.0 - nlnc*nlnc;
	b = bnl - bnc*nlnc;
	c = bb - bnc*bnc - (cyl->radius)*(cyl->radius);
	dis = MAX2 (0., b*b - a*c);
	dis = sqrt(dis);
/* 
...get line param. t for 2 points of intersection 
*/
	t = (-b + dis)/a;		
	um_translate_point(line->p0, t, line->n, p1); 	

	t = (-b - dis)/a;		
	um_translate_point(line->p0, t, line->n, p2); 	

	return(1);
}

/*********************************************************************
** FUNCTION : int um_line_cross_plane (line, plane, p)
**      determines whether a line intersects a plane within tolerance 
** PARAMETERS:
**    INPUT :
**       line - line
**       plane - plane 
**    OUTPUT : 
**       p - intersection point
** RETURNS:  +1 if it does ; p is point of intersection
**            0 if line is || to plane and lies on it within tolerance
**           -1 if line is || to plane but away from it	
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_line_cross_plane (line,plane,p)
UM_line *line;
UM_plane *plane;
UM_coord p;
{
	UU_REAL	co, d, t;   /* t is line parameter of intersection point */
	UM_vector a;         /* intermediate vector */
   UU_REAL um_dot();

	co = um_dot(line->n, plane->n); 
	um_vcmnvc(line->p0, plane->p0, a);
/* 
...find if line is || to plane 
*/
	if ( fabs(co)  <= UM_DFUZZ )
	{
/* 
...distance from line to the plane
*/
		d = fabs(um_dot(a,plane->n));	
		if (d <= UM_DFUZZ ) return (0);
		return (-1);
	}
/*  
...find intersection point 
*/
	t = - um_dot(a, plane->n)/co;
 	um_translate_point ( line->p0, t, line->n, p);

	return (1);
}
/*********************************************************************
** FUNCTION : UU_LOGICAL um_points_are_collinear (p1, p2, p3, n)
**            finds if 3 points are on one line within tolerance
** PARAMETERS:
**    INPUT :
**       p1, p2, p3  -  points in question
**    OUTPUT : 
**       n - unit vector normal to plane 
** RETURNS: 
**           UU_TRUE iff the points don't lie on a line within tolerance; 
**           UU_FALSE otherwise	
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_LOGICAL um_points_are_collinear (p1,p2,p3,n)
UM_coord p1,p2,p3;
UM_vector n;
{
	UU_REAL d, mag;
	UM_vector v2, v3, vcross;
	UU_REAL um_mag();

	um_vcmnvc(p2, p1, v2);
	um_vcmnvc(p3, p1, v3);
 	um_cross(v2, v3, vcross);
	mag = um_mag(v2)*um_mag(v3);
	d = um_mag (vcross)/mag;

/* 
...all 3 points lie on a line 
*/
	if (d <= UM_DFUZZ || um_mag(vcross) <= UM_DFUZZ) return UU_TRUE;				

	um_unitvc(vcross, n);
	return UU_FALSE;
}

/*********************************************************************
** FUNCTION : UU_LOGICAL um_plane1 (p1, p2, p3, plane)
**               plane constructor from 3 points 
** PARAMETERS:
**    INPUT :
**       p1, p2, p3  -  points determining plane
**    OUTPUT : 
**       plane  - constructed plane 
** RETURNS: 
**           UU_TRUE iff the points don't lie on a line within tolerance; 
**           UU_FALSE otherwise; plane is not constructed in this case	
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_LOGICAL um_plane1 (p1,p2,p3,plane)
UM_coord p1,p2,p3;
UM_plane *plane;
{
	UM_vector n_plane;
/* 
...all 3 points lie on a line 
*/
	if ( um_points_are_collinear(p1, p2, p3, n_plane) ) return (UU_FALSE);	

	um_vctovc (n_plane, plane->n);
	um_vctovc (p1, plane->p0);
	return (UU_TRUE);
}

/*********************************************************************
** FUNCTION : UU_LOGICAL um_point_in_2dbox (p, box)
**       determines whether a point is inside a 2D-parallelogram 
**       the point is supposed to be on the plane of the parallelogram 
** PARAMETERS
**    INPUT  :
**       p  -  point
**       box -  2D-parallelogram 
**    OUTPUT : none
** RETURNS: 
**           UU_TRUE if point is inside box within tolerance; UU_FALSE otherwise	
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_LOGICAL um_point_in_2dbox (p,box)
UM_coord p;
UM_2D_box *box;
{
	UM_vector v1,     /* 1st vertex of box */
	          v2,     /* 2nd vertex of box */
	          vp;     /* vector of the point relative to the box origin */

	UU_REAL 	v12,   /* (v1,v1)  (v2,v2)  (v1, v2) */
	         vp_1, vp_2,      /*  (vp,v1) (vp,v2)  */
	         d, a1, a2,h,w;	

	um_vcmnvc(box->p1, box->p0, v1);
	um_vcmnvc(box->p2, box->p0, v2);
	um_vcmnvc(p, box->p0, vp);
	h = box->height;
	w = box->width;
		
	a1 = h*w;
	v12 = a1*box->cosa;
	d = a1*a1 - v12*v12;

   if(d == 0.) return UU_FALSE;

	vp_1 = um_dot(vp, v1);
	vp_2 = um_dot(vp, v2);

	a1 = (vp_1*h*h - vp_2*v12)/d;
	a2 = (vp_2*w*w - vp_1*v12)/d;

	if ( (a1 > -UM_DFUZZ) && (a1 <  1. + UM_DFUZZ) )
		if ( (a2 > -UM_DFUZZ) && (a2 < 1. + UM_DFUZZ ) )  
				return UU_TRUE;

	return UU_FALSE;
}

/*********************************************************************
** FUNCTION : int um_line (p1, p2, line) 
**        line constructor from 2 points
** PARAMETERS
**    INPUT  :
**       p1,p2  -  2 points determining the line 
**    OUTPUT :
**       line     - constructed line
** RETURNS      	: none	
** SIDE EFFECTS   : none
** WARNINGS       : none
*********************************************************************/
int um_line (p1,p2,line)
UM_coord p1,p2;
UM_line *line;
{
	UM_vector v;

	um_vcmnvc(p2, p1, v);
	um_unitvc(v,line->n);         /* unit vector from p1 to p2 */

	um_vctovc(p1,line->p0);

	return (0);
}

/*********************************************************************
** FUNCTION : UU_LOGICAL um_2d_box (p0, p1, p2, box)
**     2D-box constructor from 3 points
** PARAMETERS
**     INPUT :
**        p0       -  the lower-left vertex of the box (origin)
**        p1,p2    -  2 vertices adjacent to p0 (along x,y axes, resp.)
**     OUTPUT :
**        box   - constructed box;
**               if input vertices lie on a line, the box is not calculated
** RETURNS :
**       UU_TRUE iff input vertices do not lie on a line; else UU_FALSE
** SIDE EFFECTS : none
** WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_2d_box (p0,p1,p2,box)
UM_coord p0,p1,p2;
UM_2D_box *box;
{
	UM_vector v1,v2;
	UU_REAL co, um_dot();

	if (um_points_are_collinear(p0,p1,p2, v1) )
		return UU_FALSE;
										
	um_vctovc(p0,box->p0);
	um_vctovc(p1,box->p1);
	um_vctovc(p2,box->p2);
	um_vcmnvc(p1,p0,v1);
	um_vcmnvc(p2,p0,v2);
	um_vcplvc(p1,v2,box->p3);

	box->width = um_mag(v1);
	box->height = um_mag(v2);

	co = um_dot(v1,v2)/(box->width * box->height);
	box->cosa = co;
	if (fabs(co) >= 1.0)
		box->sina = 0;
	else
		box->sina = sqrt(1. - co*co);

	return UU_TRUE;
}


/*********************************************************************
** FUNCTION: UU_LOGICAL um_3d_box (p0, p1, p2, p3, box)
**      3D-box constructor from 4 points
** PARAMETERS
**    INPUT  :
**       p0 - the lower-left vertex of the box (origin)
**       p1,p2,p3	- 3 vertices adjacent to p0 (along x,y,z axes, resp.)
**    OUTPUT :
**       box - constructed box;
**            if any 3 input vertices lie on a line, the box is not calculated
**  RETURNS:
**       UU_TRUE iff no 3 input vertices lie on a line; else UU_FALSE
**  SIDE EFFECTS : none
**  WARNINGS     : none 
*********************************************************************/
UU_LOGICAL um_3d_box (p0,p1,p2,p3,box)
UM_coord p0,p1,p2,p3;
UM_3D_box *box;
{
	UM_vector  n;
	UM_vector e2, e3;        
	int i;
/*
...check if any 3 point out of (p0,p1,p2,p3) lie on a line
*/
	if	( (um_points_are_collinear(p0,p1,p2,n)) )      return UU_FALSE;
	else if ( (um_points_are_collinear(p0,p1,p3,n)) ) return UU_FALSE;
	else if ( (um_points_are_collinear(p0,p2,p3,n)) ) return UU_FALSE;
	else if ( (um_points_are_collinear(p1,p2,p3,n)) ) return UU_FALSE;
/*
...enumeration of box edges and faces is defined in "mgeom.h" 
*/
	um_vcmnvc(p2,p0,e2);
	um_vcmnvc(p3,p0,e3);
		
	um_vctovc(p0,   box->ver[0]);
	um_vctovc(p1,   box->ver[1]);
	um_vcplvc(p1,e2,box->ver[2]);
	um_vctovc(p2,   box->ver[3]);

	for(i=0;i<4;i++)
		um_vcplvc(box->ver[i],e3,box->ver[i+4]);

	return UU_TRUE;
}

/*********************************************************************
** FUNCTION: int um_cylinder (line, radius, cyl)
**          cylinder constructor 
** PARAMETERS
**    INPUT:	
**          line   - cylinder axis
**          radius - cylinder radius
**    OUTPUT: 
**          cyl    - constructed cylinder 
** RETURNS:	none
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_cylinder (line,radius,cyl)
UM_line *line;
UU_REAL radius;
UM_cylinder *cyl;
{
	cyl->radius = radius;
	um_vctovc(line->n, cyl->axis.n);
	um_vctovc(line->p0, cyl->axis.p0);

	return (0);
}

/*********************************************************************
** FUNCTION: UU_LOGICAL um_3dbox_cross_cylinder (box, cyl, dist)
**     finds if a cylinder intersects a 3D-box within tolerance 
** PARAMETERS
**    INPUT:	
**          box - 3D-box (parallelepiped)
**          cyl - cylinder
**    OUTPUT: 
**          dist - distance from box to cylinder
** RETURNS:	
**            UU_TRUE iff box and cylinder intersect; else UU_FALSE
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_LOGICAL um_3dbox_cross_cylinder (box,cyl,dist)
UM_3D_box *box;
UM_cylinder *cyl;
UU_REAL *dist;
{
	UM_coord pc1, pc2, pc;    /* intermediate points */
	UM_plane face_plane;     /* plane of a face of the box */
	UM_2D_box this_face;     /* a face of the box */
	UU_REAL dist0, dist1, um_dcccc();	
	int res, i, j1, j2, j3;

   *dist = 0.;
   dist1 = 0.;
   dist0 = 1.e9;
/* 
... check if a box vertex is inside the cylinder
*/
	for (i=0; i<UM_3DBOX_NUM_VERT ;i++)
	{
		if ( um_point_is_in_cylinder(box->ver[i],cyl) ) return UU_TRUE; 
	}
/* 
...check if there is an edge crossing the cylinder 
*/
	for(i=0; i<UM_3DBOX_NUM_EDGES; i+=2)
	{
/* 
...2 vertices of box defining i-th edge 
*/
		j1 = UM_3DBOX_EDGES[i];
		j2 = UM_3DBOX_EDGES[i+1];
/*
..... Eduard 6/2/1999. I would like to make it more effective, so I use
..... a new routine. The reason: we don't need the points pc1, pc2; only
..... a "yes or no" answer, plus maybe a distance.
		um_line(box->ver[j1], box->ver[j2], &edge_line);
		res = um_line_cross_cylinder(&edge_line, cyl, pc1, pc2);
*/
      um_vctovc(box->ver[j1],pc1);
      um_vctovc(box->ver[j2],pc2);
		res = um_segm_cross_cylinder(cyl, pc1, pc2, pc, &dist1);

		switch (res) 
		{
         case -1:
			{	
/* 
...this edge is outside the cylinder: remember the distance and
...go to the next edge 
*/ 
            if (dist1 < dist0) dist0 = dist1;
				break;				
			}		

			case 	0:
				break;				
/*
..... Eduard. There was also case 0: edge is inside cylinder, but
..... this is not possible since both vertices would then have to 
..... be inside the cylinder.
*/
			case 	1:
/* 
...this edge crosses the cylinder 
*/
				return UU_TRUE;	

		}		/* switch (res) */
	}		/* for */		

/*
...if none of the edges crosses cyl., check if cyl. axis crosses a 
...face of the 3D-box
*/
			
	for(i=0; i<UM_3DBOX_NUM_FACES; i+=3)
	{
/* 
...construct face plane 
*/
		j1 = UM_3DBOX_FACES[i];
		j2 = UM_3DBOX_FACES[i+1];
		j3 = UM_3DBOX_FACES[i+2];
		if (um_plane1 (box->ver[j1], box->ver[j2], box->ver[j3], &face_plane))
		{
			res = um_line_cross_plane(&(cyl->axis), &face_plane, pc);
			switch (res)
			{
				case -1:
/* 
...axis is away from this face; go to the next face 
*/
					break;							
				case 0:								
/* 
...axis lies on the face plane; but it does not cross an edge
...of the face: otherwise, the edge would cross cylinder 
*/ 
					break;
				case 1:
				{			
/* 
...axis crosses the face plane; check if crossing point is within face 
...construct face # i 
*/
					if(um_2d_box(box->ver[j1], box->ver[j2], box->ver[j3], 
                                                          &this_face))
					{
						if (um_point_in_2dbox(pc, &this_face))
/* 
...cyl. axis crosses this face 
*/
							return UU_TRUE;		
						else
/* 
.....go to the next face 
*/						
							break;										
					}	
				}	/* case 1: */
			}	/* switch */
		}	/* if */
	}	/* for */

   if (dist0 > 0.) *dist = dist0;
	return UU_FALSE;
} 	

/*********************************************************************
** FUNCTION: UU_REAL um_3d_diameter (np, p, ind1, ind2)
**          finds diameter of a set of points in 3D 
** PARAMETERS
**    INPUT:	
**            np - number of points in the set
**            p   - pointer to array of points
**    OUTPUT: 
**            ind1,ind2 - indices of diametral pair of the set
**
** RETURNS:	 diameter of the set
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_REAL um_3d_diameter (np,p,ind1,ind2)
int np,*ind1,*ind2;
UM_coord *p;
{
	int i,j;
	UU_REAL diam,dij;

	diam = 0.0;
	*ind1 = 0;
	*ind2 = 1;

	if(np == 1)
	{
		*ind2 = 0;
		return diam;
	}

	for(i=0; i< np - 1; i++)
		for(j=i+1; j<np; j++)
		{
			dij = (p[i][0] - p[j][0])*(p[i][0] - p[j][0]) +
			      (p[i][1] - p[j][1])*(p[i][1] - p[j][1]) +
			      (p[i][2] - p[j][2])*(p[i][2] - p[j][2]) ;
			      
			if (dij > diam) 
			{
				diam = dij;
				*ind1 = i;
				*ind2 = j;
			}
		}
	return sqrt(diam);
}

/*********************************************************************
** FUNCTION: int um_3dbox_around (np, p, box)
**                                               
**          constructs a box in 3D containing a set of points 
** PARAMETERS
**    INPUT:	
**            np - number of points in the set
**            p   - pointer to array of points
**    OUTPUT: 
**            box - resulting box
**
** RETURNS:	
**            0 if points don't lie on one plane
**            1 if points lie on a plane
**            2 if points lie on a line
**            3 if diam(set) < UM_DFUZZ
**         -100 if memory alloc. failed
**
** SIDE EFFECTS: none
** WARNINGS    : in cases (1) and (2) the constructed box has size 
**               UM_DFUZZ in missing dimensions; in case (3) the box
**               of size UM_DFUZZ is built around center point
**
*********************************************************************/
int um_3dbox_around (np,p,box)
int np;
UM_coord *p;
UM_3D_box *box;
{
	UU_REAL diam,dproj,a, width, um_3d_diameter();
	int ind1, ind2, id1, id2;
	UM_coord p0,p1,p2,p3, pc, *proj;
	UM_vector vec1, vec2;
	UM_vector static in = { 1.,0.,0. },
	                 jn = { 0.,1.,0. },     
	                 kn = { 0.,0.,1. },
	               ijkn = { 1.,1.,1. };
	UM_plane plane;              
	UM_line line, ln1,ln2;
	UU_REAL D0 = UM_FUZZ;

	diam = um_3d_diameter(np, p, &ind1, &ind2);
/*
...if all points are very close (or just 1 point), 
...do box UM_DFUZZ*UM_DFUZZ*UM_DFUZZ around center point pc
*/
	if(diam <= D0)
	{
		a = 0.5*D0;
		if (np > 1)
		{
			um_vcplvc(p[ind1],p[ind2],pc);
			um_vctmsc(pc,0.5,pc);
		}
		else
			um_vctovc(p[0],pc);

		um_translate_point(pc,-a,ijkn,p0);
		um_translate_point(p0,D0,in,p1);
		um_translate_point(p0,D0,jn,p2);
		um_translate_point(p0,D0,kn,p3);
		um_3d_box(p0,p1,p2,p3,box);
		return (3);
	}
/*
... now case: diam > UM_DFUZZ;
... project points on plane perpto diameter line and get diameter of the projected set
*/
	proj = (UM_coord *) uu_malloc(np*sizeof(UM_coord));
	if(!proj) return (-100);            /* malloc failed */

	um_vcmnvc(p[ind2], p[ind1], plane.n);
	um_unitvc(plane.n, plane.n);
	um_vctovc(p[ind1], plane.p0);
	um_proj_pt_on_plane(np, p, &plane, proj);
	
	dproj = um_3d_diameter(np, proj, &id1, &id2);
/*
...if size of the projected set is small: points lie around a line;
...do box of size D0*D0*diam
*/
	if(dproj <= D0)
	{
		if (np > 1)
		{
			um_vcplvc(proj[id1],proj[id2],pc);
			um_vctmsc(pc,0.5,pc);
		}
		else
			um_vctovc(proj[0],pc);

		um_perpvc(plane.n, vec1);
		um_cross(plane.n,vec1,vec2);
		um_unitvc(vec1,vec1);
		um_unitvc(vec2,vec2);

		a = 0.5*D0;
		um_translate_point(pc,-a,vec1,p0);
		um_translate_point(p0,-a,vec2,p0);
		um_translate_point(p0,D0,vec1,p1);
		um_translate_point(p0,D0,vec2,p2);
		um_translate_point(p0,diam,plane.n,p3);
		
		um_3d_box(p0,p1,p2,p3,box);

		uu_free(proj);
		return 2;
	}
/*
... non-line case; find strip on plane: contains proj[] + || to line id1-id2
*/
	um_line(proj[id1], proj[id2], &line);
	um_2d_strip(np, proj, &plane, &line, &ln1, &ln2, &width);
/*
... plane case; strip has zero width
*/
	if(width <= D0)
	{
		a = 0.5*D0;
		um_cross(plane.n, line.n, vec1);
		um_unitvc(vec1,vec1);
		um_translate_point(proj[id1],-a,vec1,p0);
		um_translate_point(p0,D0,vec1,p1);
		um_translate_point(p0,dproj,line.n, p2);
		um_translate_point(p0,diam,plane.n, p3);

		um_3d_box(p0,p1,p2,p3,box);
		uu_free(proj);
		return (1);
	}
	else
	{
/*
... general 3D case
*/
		um_nptln(proj[id1],ln1.p0,ln1.n,p0);
		um_nptln(proj[id2],ln1.p0,ln1.n,p1);
		um_nptln(proj[id1],ln2.p0,ln2.n,p2);
		um_translate_point(p0,diam,plane.n,p3);

		um_3d_box(p0,p1,p2,p3,box);
		uu_free(proj);
		return (0);
	}
}
/*********************************************************************
** FUNCTION: int um_2d_strip (np, p, plane, line, ln1, ln2)
**                                               
**   for a set of points on a plane, finds min strip containing the set
**   and || to a given line
**                                               
** PARAMETERS
**    INPUT:	
**            np - number of points in the set
**            p   - pointer to array of points
**            plane - plane the points belong to
**            line  - given line on the plane
**    OUTPUT: 
**            ln1, ln2 - lines on the plane = boundaries of min strip
**            (ln1.p0 and ln2.p0 are extreme points w.r.t. line )
**
** RETURNS:	none
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_2d_strip (np,p,plane,line,ln1,ln2,width)
int np;
UM_coord *p;
UM_plane *plane;
UM_line *line,*ln1,*ln2;
UU_REAL *width;
{
	UU_REAL y,ymax,ymin, um_dot();
	UM_vector perp,vec;
	int i,imax,imin;

	um_vctovc(line->n, ln1->n);
	um_vctovc(line->n, ln2->n);

	um_cross(plane->n, line->n, perp);
	um_unitvc(perp,perp);

/* 
... HUGE_VAL is C's macro
*/
	ymax = -HUGE_VAL;
	ymin = HUGE_VAL;
	imax = imin = 0;

	for(i=0;i<np;i++)
	{
		um_vcmnvc(p[i],line->p0,vec);
		y = um_dot(vec,perp);
		if(y > ymax)
		{
			ymax = y;
			imax = i;
		}
		else if (y < ymin)
		{
			ymin = y;
			imin = i;
		}
	}

	um_vctovc(p[imax],ln1->p0);
	um_vctovc(p[imin],ln2->p0);
	*width = ymax - ymin;

   return (0);
}

/*********************************************************************
** FUNCTION: int um_proj_pline_on_circle (with_ext,np, p, 
**                     circle,ptpline,vtan,ptcir)
**                                               
** Projects polyline on circle: finds a point on the polyline and
** a point on the circle closest to each other
**                                               
** PARAMETERS
**    INPUT:	
**            with_ext - logical flag; if true, pline extensions 
**                       will be included
**            np - number of polyline knots
**            p   - pointer to array of polyline knots
**            circle - circle
**    OUTPUT: 
**            ptpline - projection pt on polyline (including extensions)
**            vtan    - vector tangent to pline at ptpline
**            ptcir   - projection pt on circle
**
** RETURNS:	UU_SUCCESS
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_proj_pline_on_circle (with_ext,np, p, circle,ptpline,vtan,ptcir)
UU_LOGICAL with_ext;
int np;
UM_coord *p;
UM_circle *circle;
UM_coord ptpline;
UM_vector vtan;
UM_coord ptcir;
{
	UM_line line;
	UM_coord pc[2],pl[2];
	int i,nsol,il,ir,isin, ext;
	UU_REAL dmin,d, um_vcdir(), um_dcccc(), um_mag(), um_dot();

	if (np < 2) return (UU_FAILURE);
	ext = (with_ext && (um_dcccc (p[0],p[np-1]) > UM_FUZZ) );
	
	um_proj_pt_on_circle (p,circle,ptcir);
	dmin = um_dcccc (p,ptcir);
	um_vctovc (p,ptpline);

	for (il=0,ir=1; il< np-1; il++,ir++)
	{
		
		um_line (p[il],p[ir], &line);

		if (il == 0) um_vctovc (line.n,vtan);

		um_proj_pt_on_circle (p[ir],circle,pc);
		d = um_dcccc (p[ir],pc);
		if (d < dmin)
		{
			dmin = d;
			um_vctovc (p[ir],ptpline);
			um_vctovc (pc,ptcir);
			um_vctovc (line.n,vtan);
		}

		um_proj_line_circle (&line, circle,&nsol,pl,pc);

		for (i=0; i< nsol;i++)
		{
			d = um_dcccc (pl[i],pc[i]);
			if (d < dmin)
			{
				if (il == 0 && ext)
					isin = (um_vcdir (pl[i],p[ir],line.n) <= 0.) || np == 2;
				else if (ir == np-1 && ext)
					isin = um_vcdir (pl[i],p[il],line.n) >= 0.;
				else
					isin = um_vcdir (pl[i],p[il],line.n) *
						um_vcdir (pl[i],p[ir],line.n) <= 0.;

				if (isin)
				{
					dmin = d;
					um_vctovc (pl[i],ptpline);
					um_vctovc (line.n,vtan);
					um_vctovc (pc[i],ptcir);
				}
			}
		}  /* i<nsol */

	} /*  for (il=0 ... */

	return (UU_SUCCESS);
}
/*********************************************************************
** FUNCTION: int um_proj_pt_on_circle (p, circle,pc)
**                                               
** Projects a point on circle
**                                               
** PARAMETERS
**    INPUT:	
**            p   - point
**            circle - circle
**    OUTPUT: 
**            pc   - projection pt on circle
**
** RETURNS:	UU_SUCCESS
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_proj_pt_on_circle (p, circle,pc)
UM_coord p;
UM_circle *circle;
UM_coord pc;
{
	UM_vector v;
/*
... project p on the circle plane
*/
	um_nptpln (p,circle->center,circle->n,v);
	um_vcmnvc (v,circle->center,v);

	if (um_mag (v) < UM_DFUZZ) um_vcperp (circle->n,v);

	um_unitvc (v,v);
	um_translate_point (circle->center,circle->radius,v,pc);

	return (UU_SUCCESS);
}
/*********************************************************************
** FUNCTION: int um_proj_line_circle (line, circle,npt,pl,pc)
**                                               
** Finds 2 closest points on an infinite line & circle
**                                               
** PARAMETERS
**    INPUT:	
**            line - line
**            circle - circle
**    OUTPUT: 
**            npt  -   number of solutions (1 or 2)
**            pl   -   pt on line closest to circle
**            pc   -   pt on circle closest to line
**
** RETURNS:	UU_SUCCESS
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_proj_line_circle (line, circle,npt,pl,pc)
UM_line *line;
UM_circle *circle;
int *npt;
UM_coord *pl,*pc;
{
	UM_vector n1,n2,m;
	UM_coord pc1;
	UU_REAL a,b,co,si,err,
	        um_mag(),um_dot(),um_dcccc();
	int i;

	*npt = 1;

	if (circle->radius < UM_FUZZ)
	{
		um_nptln (circle->center, line->p0, line->n,pl);
		um_vctovc (circle->center,pc);
		return (UU_SUCCESS);
	}

	co = um_dot (line->n,circle->n);
	si = MAX2 (0., 1. - co*co);
	si = sqrt (si);

	um_nptln (circle->center, line->p0, line->n,pl);
	um_proj_pt_on_circle (pl[0],circle,pc[0]);
/*
... if line is perpto the circle plane - done
*/
	if (si < UM_FUZZ) return (UU_SUCCESS);
/*
... unit vec's n1,n2 lie on the cirle plane, are orthogonal,
... and n1 is || to the projection of line on the cirle plane
*/
	um_avcplbvc (1./si,line->n, -co/si,circle->n,n1);

	um_vcmnvc (pl,circle->center,m);
	um_unitvc (m,m);
/*
... special case: line is || to the circle plane; 1 or 2 solutions
*/
	if (fabs(um_dot (m,n1)) < UM_FUZZ) 
	{
		um_cross (circle->n, n1,n2);
		a = um_dcccc(pl,circle->center)*um_dot(m,n2)/(si*si);

		if (fabs(a) < circle->radius) 
		{
			*npt = 2;
			b = MAX2 (0., (circle->radius*circle->radius - a*a));
			b = sqrt (b);
			for (i=0; i<2; i++, b=-b, pc++,pl++)
			{
				um_avcplbvc (a,n2,b,n1,pc);
				um_vcplvc (circle->center,pc,pc);
				um_nptln (pc, line->p0, line->n,pl);
			}
		}

		return (UU_SUCCESS);
	}
/*
... general case: 1 solution; iterative scheme:
...
... pc = circle->center;
... 
... begin:;
...    pl' = (pc  projected on line);
...    pc' = (pl' projected on circle);
...    pc  =  pc';
... goto Begin;
...
*/
	for (i=0, err=1. ;i < 50 && err > UM_FUZZ; i++)
	{
		um_nptln (pc, line->p0, line->n,pl);
		um_proj_pt_on_circle (pl[0],circle,pc1);
		err = um_dcccc (pc,pc1);
		um_vctovc (pc1,pc);
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int um_3pt_circle (pt1,pt2,pt3,cen,rad,tolsq)
**                Create a 2D circle through three points. 
**    PARAMETERS   
**       INPUT  : 
**              pt1                 first point
**              pt2                 second point
**              pt3                 third point
**              rmin,rmax           min and max values for radius
**       OUTPUT :  
**              cen                 circle's center
**              rad                 circle's radius
**              ccw                 1 - if counter-clockwise, else -1
**    RETURNS      : 
**                 0 if circle data defined; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_3pt_circle (pt1,pt2,pt3,cen,rad,ccw,rminsq,rmaxsq)
UM_2Dcoord pt1,pt2,pt3,cen;
UU_REAL *rad,rminsq,rmaxsq;
int *ccw;
{
	UU_REAL A,B,C,D,E,F,G;

	A = pt2[0] - pt1[0];
	B = pt2[1] - pt1[1];
	C = pt3[0] - pt1[0];
	D = pt3[1] - pt1[1];
	E = A*(pt1[0] + pt2[0]) + B*(pt1[1] + pt2[1]);
	F = C*(pt1[0] + pt3[0]) + D*(pt1[1] + pt3[1]);
	G = 2*(A*(pt3[1] - pt2[1]) - B*(pt3[0] - pt2[0]));
	if (fabs(G) < 1.0e-12) return(-1);
	*ccw = (G < 0)? -1: 1;

	cen[0] = (D*E - B*F)/G; cen[1] = (A*F - C*E)/G;

	G = UM_SQDIS_2D(pt2,cen);
	if (G < rminsq || G > rmaxsq) return(-1);
	*rad = sqrt(G);
	return(0);
}

#if 0
/*********************************************************************
** FUNCTION: int um_evolve_polyline (t,np,pts,pt)
** --------- NOT USED CURRENTLY ----------------------------------
** Evaluates polyline point at given paremeter t 
**
** PARAMETERS
**           t - curve parameter ( <1, >0)
**           np - # of polyline points
**           pts - polyline points
**    INPUT:	
**    OUTPUT: 
**            pt  -   evaluated point
** RETURNS:	UU_SUCCESS
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_evolve_polyline (t,np,pts,pt)
UM_real8 t;
int np;
UM_coord *pts,pt;
{
	int i,j;
	UM_real8 a,b,d,t1,t0,len,um_getpolylen(),um_dcccc();

	len = um_getpolylen (np,pts);

	for (i=0, t1 = 0.; i < np-1 && t >= t1; i++)
	{
		t0 = t1;
		t1 += um_dcccc(pts[i],pts[i+1])/len;
	}

	len = t1 - t0;
	if (len == 0.) return  (UU_FAILURE);
	a = (t1-t)/len; b = (t-t0)/len;

	for (j=0;j<3;j++) pt[j] = a*pts[i-1][j] + b*pts[i][j];

	return (UU_SUCCESS);
}
#endif


/*********************************************************************
**    E_FUNCTION: void plcvsf (key,cvflg,buff,plnr,ier)
**       Create an average plane from a curve/surface.
**		 Calculate the point and pntvec at the center of this plane
**    PARAMETERS
**       INPUT  :
**          key   - key id of the curve/sf
**			cvflg - 0 : surface, 1 :  curve
**       OUTPUT :
**			buff  - stores the pl/pt/pntvec
**			plnr  - 0: if not planar, 1: if  planar
**			ier   - error flag
**    RETURNS      :   none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void plcvsf (key,cvflg,buff,plnr,ier)
UM_int4 *key;
UM_real8 buff[6];
UM_int2 *cvflg,*plnr,*ier;
{
	int i, status, npts, istat;
	struct NCL_fixed_databag e;
	UM_real8 tol8;
	UU_REAL d,tol;
	UU_LIST ptlist;
	UM_srf_boundary bound;
	UM_coord *p1;
	UM_coord cpt,plpt;
	UM_transf tfmat1;
	UM_vector plvc,vec;
	UU_LOGICAL lclosd;
	UM_int2 IPT = NCLI_POINT, IVE = NCLI_VECTOR;
	
	UU_LOGICAL lv97;

	gettol (&tol8);
	tol = tol8;

	*plnr = 0;
	*ier = 0;
	npts = 0;

	e.key = *key;
	status = ncl_retrieve_data_fixed(&e,sizeof(struct NCL_fixed_databag));
	if (status != UU_SUCCESS)
	{
		 *ier = 1; return;
	}
/*
.....IF curve then: Evolve points on curve.
*/
	if (*cvflg == 1)
	{
		uu_list_init (&ptlist, sizeof(UM_coord), 200, 200);
		status = uc_retrieve_transf(e.key, tfmat1);
		if (status == UU_SUCCESS)
		{
			npts = ncl_evolve_curve(&e,tfmat1,tol,&ptlist,UU_NULL,UU_NULL,0);
			p1 = (UM_coord *)UU_LIST_ARRAY(&ptlist);
		}
	}
	else
	{
/*
.....IF surface then: we get points from the outer boundary of the surf.
*/
		status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,&e,&bound);
		if (status == UU_SUCCESS)
		{
			npts = bound.np[0];
			p1 = (UM_coord *) UU_LIST_ARRAY (bound.cvpts);
		}
	}
	if (status != UU_SUCCESS) goto Done;
/*
.....If the curve is closed drop the last point as it is the same as the
.....first point.
*/
	lclosd = UU_FALSE;
	if (npts > 1)
	{
		d = UM_SQDIS (p1[0],p1[npts-1]);
		lclosd = (d < tol*tol);
		if (lclosd) npts = npts-1;
	}

	if (npts < 3) GO_DONE
/*
.....For 3 points um_ptstopln will give the approx fit, since it works
.....iteratively. check if these points are collinear. If they are collinear
.....a plane cannot be constructed but if they are not return the plane.
*/    
	if (npts == 3)
	{
		if (um_3pt_colinear(p1[0],p1[1],p1[2],plvc,tol)) GO_DONE
		else
		{
			*plnr = 1;
			goto Center;
		}
	}
/*
.....Determine if the curve is planar.
*/
	if (status == UU_SUCCESS)
	{
		lv97 = ncl_setver(97);
		if (*cvflg == 0 && !lv97)
			status = ncl_sftopln (*key,npts,p1,plpt,plvc,tol);
		else
			status = um_ptstopln (npts,p1,plpt,plvc,tol);
		if (status == UU_SUCCESS)
		{
/*
.....Verify all points lie on plane
*/
			for (i = 0; i < npts; i++)
			{
				um_vcmnvc(p1[i],plpt,vec);
				d = UM_DOT (vec,plvc);
				if (fabs(d) > tol) goto Notplanar;
/*
.....Curve is not coplanar
*/
			}
			*plnr = 1;
			goto Center;
		}
	}
	if (status != UU_SUCCESS) goto Done;

Notplanar:
	*plnr = 0;

Center:
	if (lclosd) npts = npts+1;

	istat = um_centroid (p1,npts,cpt,tol);
	if (istat != 0) um_vctovc (plpt,cpt);

	for (i=0; i<3; i++)
	{
		buff[i] = cpt[i];
		buff[i+3] = plvc[i];
	}
/*
.....Adjust for modsys/refsys
*/
	 from_unibase (buff,buff,&IPT);
	 from_unibase (&buff[3],&buff[3],&IVE);
/*
.....End of routine
*/
Done:;
	if (*cvflg == 1)
		uu_list_free (&ptlist);
	else
		um_free_boundary (&bound);

	if (status != UU_SUCCESS) *ier = 1;
}

/*********************************************************************
**    FUNCTION :  um_centroid (pts,npts,cpt,tol)
**      Calculates the centroid of an evolved curve.
**    PARAMETERS
**       INPUT  :
**                  pts   - Set of points to check for planar.
**                  npts  - Number of points in 'pts'.
**                  tol   - tolerance
**       OUTPUT :
**                  cpt	  - centroid point
**    RETURNS      : 0 iff success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_centroid (pts,npts,cpt,tol)
UM_coord *pts,cpt;
int npts;
UU_REAL tol;
{
	int i,i1,k;
	UM_coord pp;
	UU_REAL di,dlen;

	um_nullvc (pp);

	dlen = 0;
	for (i = 0; i < npts - 1; i++)
	{
		i1 = i+1;
		di = um_dcccc (pts[i], pts[i1]);
		dlen += di;
		di /= 2;

		for (k = 0; k < 3; k++)
		{
			pp[k] = pp[k] + di*(pts[i][k] + pts[i1][k]);
		}
	}

	if (dlen < tol) return (-1);

	for (k = 0; k < 3; k++)
	{
		cpt[k] = pp[k]/dlen;
	}

	return (0);
}

/*********************************************************************
** FUNCTION : UU_LOGICAL um_point_in_segment (p0, p1, p2)
**            finds if point p0 is inside the segment of line(p1,p2)
** PARAMETERS:
**    INPUT :
**       p0		  - The point to check
**		 p1, p2   - the segment start and end point
**    OUTPUT : 
**       none
** RETURNS: 
**           UU_TRUE iff the point is inside the segment; 
**           UU_FALSE otherwise	
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_LOGICAL um_point_in_segment (p0,p1,p2)
UM_coord p0,p1,p2;
{
	UM_vector v1;
	UU_REAL d1,d2,d;

	if (!um_points_are_collinear(p0,p1,p2, v1) )
		return UU_FALSE;

	d = UM_SQDIS(p1,p2);
	d1 = UM_SQDIS(p0,p1);
	d2 = UM_SQDIS(p0,p2);
	if (d < d1 || d < d2)
		return UU_FALSE;

	return UU_TRUE;
}

/*********************************************************************
** FUNCTION : UU_LOGICAL um_points_within_tol (p1, p2, p3, tol)
**            finds if 3 points are on one line within tolerance
** PARAMETERS:
**    INPUT :
**       p1, p2, p3  - points in question
**		 tol	     - tolerance
**    OUTPUT : 
**       none 
** RETURNS: 
**           UU_TRUE iff the points don't lie on a line within tolerance; 
**           UU_FALSE otherwise	
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_LOGICAL um_points_within_tol (p1,p2,p3,tol)
UM_coord p1,p2,p3;
UU_REAL tol;
{
	UU_REAL d, d1,d2,mag;
	UM_vector v2, v3, vcross;
	UM_line ln;
	UM_int2 idx;
	UM_real8 ver;
	UM_coord pt1,pt2,pt3;
	char tbuf[80];
	UU_LOGICAL write = UU_FALSE;

	idx = 169; getsc(&idx,&ver);
	if (ver < 10.003)
	{
		um_vcmnvc(p3, p1, v2);
		um_unitvc(v2, v2);

		um_vcmnvc(p2, p1, v3);
		um_unitvc(v3, v3);
		um_cross(v2, v3, vcross);
		d = UM_DOT(vcross,vcross);
/*
...check if moddle point withn tolerance
*/
		if (d <= UM_FUZZ * UM_FUZZ)
			return UU_TRUE;
/*
.....vertical points
*/
		if (fabs(v2[2]) >= 0.90)
		{
			if (d < 0.1 * tol * tol)
				return UU_TRUE;
		}
		else if (fabs(v2[2]) >= 0.75 && d < 2.0*tol * tol)
			return UU_TRUE;

		if (fabs(v2[2]) < 0.75 && d < 0.5*tol)
			return UU_TRUE;
	}
/*
.....Check distance "midpoint" is from line - ASF 7/15/13
*/
	else
	{
		um_vcmnvc(p3, p1, ln.n);
		um_unitvc(ln.n, ln.n);
		um_vctovc(p1,ln.p0);
		d = um_dist_from_line(p2,&ln);
		if (d < tol/*tol*/) return UU_TRUE;
	}
	return UU_FALSE;
}

/*********************************************************************
**    FUNCTION :  um_SegmentPlane(pt1,pt2,plane,pt)
**      Calculates the intersection of a segment and plane.
**    PARAMETERS
**       INPUT  :
**                pt1   - The first point of the segment
**                pt2   - The second point of the segment
**                plane  - The plane
**       OUTPUT :
**                ipt	  - teh intersection point
**    RETURNS     : 0 = disjoint (no intersection)
**				    1 = intersection in the unique point
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_iSegPlane(pt1,pt2,plane,ipt)
UM_coord pt1,pt2;
UM_plane plane;
UM_coord ipt;
{
	int nint;
	UM_vector v1,v2;

	um_vcmnvc(pt2,pt1,v1);
	um_unitvc(v1,v1);

	um_ilnpln(pt1,v1,plane.p0,plane.n,&nint,ipt);
	if (nint > 0)
	{
		if (!um_point_in_segment(ipt,pt1,pt2))
			nint =0;
	}

	return nint;
}

/*********************************************************************
**    FUNCTION :  um_SegmentPlane(pt1,pt2,plane,pt)
**      Calculates the intersection of a segment and plane.
**    PARAMETERS
**       INPUT  :
**                pt1   - The first point of the segment
**                pt2   - The second point of the segment
**                plane  - The plane
**       OUTPUT :
**                ipt	  - the intersection point
**    RETURNS     : The number of intersection oints
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_iBoxPlane(bbox,plane,ipt)
UM_2box bbox;
UM_plane plane;
UM_coord ipt[];
{
	int nint;
	UM_coord pt;
	UM_coord pt0,pt1,pt2,pt3;

	pt0[0] = bbox.xmin; pt0[1] = bbox.ymin; pt0[2] = 0.0;
	pt1[0] = bbox.xmin; pt1[1] = bbox.ymax; pt1[2] = 0.0;
	pt2[0] = bbox.xmax; pt2[1] = bbox.ymax; pt2[2] = 0.0;
	pt3[0] = bbox.xmax; pt3[1] = bbox.ymin; pt3[2] = 0.0;
	
	nint = 0;
	if (um_iSegPlane(pt3,pt0,plane,pt) > 0)
	{
		um_vctovc(pt,ipt[nint]);			
		++nint;
	}
    if (um_iSegPlane(pt0,pt1,plane,pt) > 0)
	{
		um_vctovc(pt,ipt[nint]);			
		++nint;
	}
	if (um_iSegPlane(pt1,pt2,plane,pt) > 0)
	{
		um_vctovc(pt,ipt[nint]);			
		++nint;
	}
	if (um_iSegPlane(pt2,pt3,plane,pt) > 0)
	{
		um_vctovc(pt,ipt[nint]);			
		++nint;
	}
	return nint;
}

/*********************************************************************
**    FUNCTION :  um_bound_2box (pts,npts,minmax)
**      Calculates the boundary 2d box.
**    PARAMETERS
**       INPUT  :
**                pts   - boundary points
**                npts  - boundary points number
**       OUTPUT :
**                minmax - boundary 2d box
**    RETURNS     : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_bound_2box (pts,npts,minmax)
int npts;
UM_coord *pts;
UU_REAL minmax[];
{
	int i;
	UU_REAL xmin,xmax,ymin,ymax;

	xmin = 1000;
	xmax = -1000;
	ymin = 1000;
	ymax = -1000;

	for (i = 0; i < npts-1; i++)
	{
		if (pts[i][0] < xmin) xmin = pts[i][0];
		if (pts[i][0] > xmax ) xmax = pts[i][0];
		if (pts[i][1] < ymin) ymin = pts[i][1];
		if (pts[i][1] > ymax ) ymax = pts[i][1];
	}

	minmax[0] = xmin;
	minmax[1] = xmax;
	minmax[2] = ymin;
	minmax[3] = ymax;
}

/*********************************************************************
**    FUNCTION :  um_bound_line_minmaxpt(points,pt,vc,ptmin,ptmax)
**      Get the min and max distance on the boudnary to line(pt,vc).
**    PARAMETERS
**       INPUT  :
**                points   - boundary points list
**                pt       - line point
**                vc       - line vector
**       OUTPUT :
**                ptmin	  - the min distance point on the boundary to line
**                ptmax   - the max distance point on the boundary to line
**    RETURNS     : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_bound_line_minmaxpt(points,pt,vc,ptmin,ptmax)
UU_LIST *points;
UM_coord pt,ptmin,ptmax;
UM_vector vc;
{
	int i,npts;
	UM_coord *pts;
	UU_REAL dissq,mindis = 1000.0,maxdis = 0.0;

	npts = points->cur_cnt;
	pts = (UM_coord *) UU_LIST_ARRAY (points);

	for (i=0; i < npts-1;i++)
	{
		dissq = um_sqdis_from_line (pts[i],pt,vc);
		if (dissq < mindis)
		{
			mindis = dissq;
			um_vctovc(pts[i],ptmin);	
		}
		if (dissq > maxdis)
		{					
			maxdis = dissq;
			um_vctovc(pts[i],ptmax);	
		}
	}
}

/*********************************************************************
**    FUNCTION :  um_boundbox_minmaxpt(points,pt,vc,ptmin,ptmax)
**      Get the min and max distance on the boudnary box to line(pt,vc).
**    PARAMETERS
**       INPUT  :
**                points   - boundary points list
**                pt       - line point
**                vc       - line vector
**				  flag	   - 0: Point 1:Line
**       OUTPUT :
**                ptmin	  - the min distance point on the boundary to point/line
**                ptmax   - the max distance point on the boundary to point/line
**    RETURNS     : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_boundbox_minmax(points,pt,vc,flag,ptmin,ptmax)
UU_LIST *points;
UM_coord pt,ptmin,ptmax;
UM_vector vc;
int flag;
{
	int i,npts;
	UM_coord *pts,tmp;
	UU_REAL dissq,mindis = UM_MAXREAL,maxdis = -UM_MAXREAL;
	UU_REAL minmax[4];
	UM_coord pt0[4],pt1;

	npts = points->cur_cnt;
	pts = (UM_coord *) UU_LIST_ARRAY (points);
/*
.....Get bounding box
*/
	um_bound_2box (pts,npts,minmax);
	pt0[0][0] = minmax[0]; pt0[0][1] = minmax[2]; pt0[0][2] = 0.0;
	pt0[1][0] = minmax[0]; pt0[1][1] = minmax[3]; pt0[1][2] = 0.0;
	pt0[2][0] = minmax[1]; pt0[2][1] = minmax[2]; pt0[2][2] = 0.0;
	pt0[3][0] = minmax[1]; pt0[3][1] = minmax[3]; pt0[3][2] = 0.0;
	
	um_vctovc(pt,pt1);
	pt1[2] = 0.0;

	for (i=0; i < 4;i++)
	{
		if (flag == 1)
			dissq = um_dis_from_line (pt0[i],pt1,vc);
		else
			dissq = um_sqdis(pt0[i],pt1);
		if (dissq < mindis)
		{
			mindis = dissq;
			um_vctovc(pt0[i],ptmin);	
		}
		if (dissq > maxdis)
		{					
			maxdis = dissq;
			um_vctovc(pt0[i],ptmax);	
		}
	}
/*
.....Swap the max/min
*/
	if (fabs(mindis) > fabs(maxdis))
	{
		um_vctovc(ptmin,tmp);
		um_vctovc(ptmax,ptmin);	
		um_vctovc(tmp,ptmax);
	}
}

/*********************************************************************
**    FUNCTION : um_boundbox_between_planes_minmax(points,pl1,pl2,pt,ptmin,ptmax)
**      Get the min and max distance on the boudnary box to line(pt,vc).
**    PARAMETERS
**       INPUT  :
**                points   - boundary points list
**				  pl1,pl2  - parallel planes
**                pt       - point
**       OUTPUT :
**                ptmin	  - the min distance point on the boundary to point/line
**                ptmax   - the max distance point on the boundary to point/line
**    RETURNS     : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_boundbox_between_planes_minmax(points,pl1,pl2,pt,tol,ptmin,ptmax)
UU_LIST *points;
struct NCL_nclpl_rec *pl1,*pl2;
UU_REAL tol;
UM_coord pt,ptmin,ptmax;
{
	int i,npts,inpt1,inpt2;
	UM_coord *pts,ipt1[2],ipt2[2],pt0[4];
	UU_REAL dissq,mindis = UM_MAXREAL,maxdis = 0.0;
	UU_REAL minmax[4];
	UM_2box bbox;
	UM_plane pln1,pln2;

	npts = points->cur_cnt;
	pts = (UM_coord *) UU_LIST_ARRAY (points);
/*
.....Get bounding box
*/
	um_bound_2box (pts,npts,minmax);
	bbox.xmin = minmax[0]-tol; 
	bbox.xmax = minmax[1]+tol;
    bbox.ymin = minmax[2]-tol; 
	bbox.ymax = minmax[3]+tol; 
/*
.....Planes
*/
	ncl_nclpl_to_umplane(pl1,&pln1);
	ncl_nclpl_to_umplane(pl2,&pln2);
/*
.....Intersection between planes and bbox
*/
	inpt1 = um_iBoxPlane(bbox,pln1,ipt1);
	inpt2 = um_iBoxPlane(bbox,pln2,ipt2);

	if (inpt1 == 2)
	{
		um_vctovc(ipt1[0],pt0[0]); pt0[0][2] = 0.0;
	    um_vctovc(ipt1[1],pt0[1]); pt0[1][2] = 0.0;
	}
	else
	{
		pt0[0][0]=bbox.xmin; pt0[0][1]=pln1.p0[1]; pt0[0][2] = 0.0;
		pt0[1][0]=bbox.xmax; pt0[1][1]=pln1.p0[1]; pt0[1][2] = 0.0;
	}
	
	if (inpt2 == 2)
	{
		um_vctovc(ipt2[0],pt0[2]); pt0[2][2] = 0.0;
	    um_vctovc(ipt2[1],pt0[3]); pt0[3][2] = 0.0;
	}
	else
	{
		pt0[2][0]=bbox.xmin; pt0[2][1]=pln2.p0[1]; pt0[2][2] = 0.0;
		pt0[3][0]=bbox.xmax; pt0[3][1]=pln2.p0[1]; pt0[3][2] = 0.0;
	}

	for (i=0; i < 4;i++)
	{
		dissq = um_sqdis(pt0[i],pt);
		if (dissq < mindis)
		{
			mindis = dissq;
			um_vctovc(pt0[i],ptmin);	
		}
		if (dissq > maxdis)
		{					
			maxdis = dissq;
			um_vctovc(pt0[i],ptmax);	
		}
	}
}

/*********************************************************************
**    FUNCTION : um_point_bewteen_planes(pt,pl1,pl2)
**      Check if pt is between two parallel planes.
**    PARAMETERS
**       INPUT  :
**                pt       - point
**				  pl1,pl2  - parallel planes
**       OUTPUT :
**				  none
**    RETURNS     : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_point_bewteen_planes(pt,pl1,pl2)
UM_coord pt;
struct NCL_nclpl_rec *pl1,*pl2;
{
	UU_REAL dis1,dis2,dis;
	UU_REAL um_ptplndis();

	dis = um_dcccc(pl1->pt,pl2->pt);
	dis1 = um_ptplndis(pt,pl1->pt,pl1->nvec);
	dis2 = um_ptplndis(pt,pl2->pt,pl2->nvec);
	if (dis1 <= dis + UM_DFUZZ && dis2 <= dis + UM_DFUZZ)
		return UU_TRUE;

	return UU_FALSE;
}

/*********************************************************************
**    FUNCTION : um_get_between_point(pt1,pt2,lbetween,pt)
**      Get the point pt given start pt1 and end pt2.
**    PARAMETERS
**       INPUT  :
**                pt1       - start point
**				  pt2		- end point
**				  lbetween	- pt between pt1 and pt2
**				  pt		- 2d between point
**       OUTPUT :
**				  pt		-3d beteween point
**    RETURNS     : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_get_between_point(pt1,pt2,lbetween,pt)
UM_coord pt1,pt2,pt;
UU_LOGICAL lbetween;
{
	UU_REAL t;				
	if (fabs(pt2[0]-pt1[0]) > fabs(pt2[1]-pt1[1]))	
		t = (pt[0]-pt1[0]) / (pt2[0]-pt1[0]); 
	else 					
		t = (pt[1]-pt1[1]) / (pt2[1]-pt1[1]); 

	if (lbetween)
	{
		if (t > 1.0) t = 1.0;
		if (t < 0.0) t = 0.0;
	}

	pt[2] = pt1[2] + t * (pt2[2]-pt1[2]);         
}

/*********************************************************************
** FUNCTION : UU_REAL um_dist_from_line (pt1, pt2,pt)
**        returns distance of a point from a line 
** PARAMETERS:
**    INPUT :
**       pt		- point 
**       pt1	- start point of segment
**		 pt2	- end point of segment
**    OUTPUT : none
** RETURNS:  
**        sqrt distance of  pt from segment(pt1,pt2) 
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_REAL um_sqdis_from_segment(pt,pt1,pt2)
UM_coord pt,pt1,pt2;
{
	UU_REAL c1,c2,b;
	UM_coord v,w,pb; 
 
	um_vcmnvc(pt2,pt1,v);
	um_vcmnvc(pt, pt1, w);

	c1 = UM_DOT(w,v);
	if (c1 <= 0.0)
		return UM_SQDIS(pt,pt1);

	c2 = UM_DOT(v,v);
	if (c2 <= c1)
		return UM_SQDIS(pt,pt2);

	b = c1/ c2;
	um_translate_point(pt1,b,v,pb);

	return UM_SQDIS(pt,pb);
}

