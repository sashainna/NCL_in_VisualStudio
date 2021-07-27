/*********************************************************************
**    NAME         :  nesmedge.c
**       CONTAINS:  main routines for smill bull nose tool
**
**		ncl_copy_epos()
**		Brent0()
**		ncl_sm_setangle()
**		ncl_sm_normepos()
**		ncl_sm_ellipsepoint()
**		ncl_sm_offsetellipsepoint()
**		ncl_sm_func()
**		ncl_sm_difference()
**		ncl_sm_findellipsepos()
**		ncl_sm_brent()
**		ncl_sm_findellipseecenter()
**		ncl_edges_contact1()
**    COPYRIGHT 2011 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesmedge.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:50
*********************************************************************/
#include "mgeom.h"

#define ZEPS 1e-10
static int ITMAX=100;

extern UU_LIST Sedgelst;			/*traingle edgelist*/

typedef struct
{
	double s;
	double t;
	double angle;
} EPosition ;

typedef struct
{
	UM_coord center;
	EPosition epos1;
	EPosition epos2;
	double a;  
	double b;
	double offset;     
} Ellipse ;

/**********************************************************************
**    E_FUNCTION     : ncl_copy_epos(epos1,epos2)
**       copy EPosition
**    PARAMETERS
**       INPUT  :
**          epos1		: The ellipse position
**       OUTPUT :
**          epos2		: The ellipse position
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_copy_epos(epos1,epos2)
EPosition epos1,*epos2;
{
	epos2->s = epos1.s;
	epos2->t = epos1.t;
	epos2->angle = epos1.angle;
}

/*********************************************************************
**    E_FUNCTION     : Brent0(ax, bx, ell, tol, f)  
** 
**.....find a zero of function f in the interval [a,b]
**.....a and b must bracket the root, i.e. f(a) must have different sign than f(b)
**.....needs a pointer to an T which must provide a function
**       INPUT  :
**			ax		: initial start value
**			bx		: initial end value
**			ell		: The ellipse
**			tol		: tolerance
**			f	 	: pointer to function
**    RETURNS      : root
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
double Brent0(ax, bx, ell, tol, f) 
double ax,bx,tol;
Ellipse *ell;
double (*f)(double,Ellipse*);
{
/*
.....Numerical recipe in C
.....f must have different sign at a and b, i.e. f(a)*f(b) < 0
.....returns the location of a root c where f(c)=0 
*/
	int iter;
    double a,b,c,d,e;
    double fa,fb,fc; 
    double xm,p,q,r,s;
    double tol1;

	a = ax;
	b = bx;
    fa = (double)(*f)(a,ell); 
    fb = (double)(*f)(b,ell); 

    if (fa * fb >= 0.0)
	   return 0;

    c  = a; 
    fc = fa; 
    e  = b-a;
    d  = e;
	for (iter = 0; iter <= ITMAX; iter++)
	{
        if (fabs(fc) < fabs(fb)) 
		{ 
            a = b; 
            b = c; 
            c  = a;
            fa = fb;
            fb = fc; 
            fc = fa;
        }
        tol1 = 2.0*tol*fabs(b) + ZEPS;
        xm = 0.5*(c-b); 
        if ((fabs(xm) <= tol1) || fb == 0.0) 
            break; 
        
        if ((fabs(e) < tol1) || (fabs(fa) <= fabs(fb))) 
		{
            e = xm; 
            d = e; 
        } 
		else
		{
            s = fb/fa;
            if (a == c) 
			{
                p = 2.0 * xm * s;
                q = 1.0 - s;
            }
			else 
			{
                q = fa/fc;
                r = fb/fc;
                p = s*(2.0*xm*a*(q-r)-(b-a)*(r-1.0));
                q = (q-1.0)*(r-1.0)*(s-1.0);
            }
            
            if (p>0.0)
                q = -q;
            else
                p = -p; 
            
            s = e;
            e = d;
            if ((2.0*p < (3.0*xm*q-fabs(tol1*q))) && p<fabs(0.5*s*q))
                d = p/q;
			else
			{
                e = xm;
                d = e;
            }                
        }

        a = b; 
        fa = fb;
        if (fabs(d) > tol1)
            b = b + d; 
        else if (xm > 0.0)
            b = b + tol1; 
        else
            b = b - tol1;
        
        fb = (double)(*f)(b,ell);    
        if ((fb > 0.0 && fc> 0.0) || (fb <= 0.0 && fc <= 0.0))
		{
            c = a;  
            fc = fa;
            e = b-a; 
            d = e;
        }
    }
    return b;
}

/*********************************************************************
**    E_FUNCTION: ncl_sm_setangle(epos,pos1) 
**       set epos(s,t) to ellipse angle  
**    PARAMETERS
**       INPUT  :
**			epos		: ellipse position
**       OUTPUT : 
**			pos			: unit vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_sm_setangle(epos,pos) 
EPosition epos;
UM_vector pos;
{
	UM_vector pos1;
    double d = epos.angle;
    while (d > 4.0)
        d -= 4.0;
    while (d < 0.0)
        d+=4.0;

    if (d < 2.0) 
        pos1[0] = 1-d; 
    else
        pos1[0]= d-3;
        
    if (d < 3.0)
	{
        if (d > 1.0) 
            pos1[1] = 2-d; 
        else
            pos1[1]= d;  
    } else {
        pos1[1] = d - 4; 
    }
/*
.....The normalized vector
*/
	pos1[2] = 0.0;
	um_unitvc(pos1, pos);
}

/*********************************************************************
**    E_FUNCTION: ncl_sm_normepos(ell,epos,pos)
**       Normal(const EPosition& aPosition) const
**    PARAMETERS
**       INPUT  : 
**			ell		: The ellipse
**			epos	: The ellipse position(s,t)
**       OUTPUT : 
**			pos		: unit vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_sm_normepos(ell,epos,pos)
Ellipse *ell;
EPosition epos;
UM_coord pos;
{
	UU_REAL dis;

	pos[0] = ell->b * epos.s;
	pos[1] = ell->a * epos.t;
	pos[2] = 0.0;
	dis = sqrt(pos[0] * pos[0] + pos[1] * pos[1]);

	pos[0] = pos[0]/dis;
	pos[1] = pos[1]/dis;
}
        
/*********************************************************************
**    E_FUNCTION: ncl_sm_ellipsepoint(ell,epos,pos)
**       Get the ellipse point given epos(s,t)
**    PARAMETERS
**       INPUT  : 
**			ell		: The ellipse
**          epos	: The ellipse  position(s,t)
**       OUTPUT : 
**			pos		: ellipse point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_sm_ellipsepoint(ell,epos,pos)
Ellipse *ell;
EPosition epos;
UM_coord pos;
{	
	UM_coord norm;
	norm[0] = 0.0;
	norm[1] = 0.0;
	norm[2] = 0.0;

	um_vctovc(ell->center, pos);
    pos[0] += ell->a * epos.s;  
    pos[1] += ell->b * epos.t; 
}

/*********************************************************************
**    E_FUNCTION: ncl_sm_offsetellipsepoint(ell,epos,pos)
**       get the offset ellipse point given epos(s,t)
**    PARAMETERS
**       INPUT  : 
**			ell		: The ellipse
**          epos	: The ellipse  position(s,t)
**       OUTPUT : 
**			pos		: ellipse point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_sm_offsetellipsepoint(ell,epos,pos)
Ellipse *ell;
EPosition epos;
UM_coord pos;
{	
	UM_coord norm;

	norm[0] = 0.0;
	norm[1] = 0.0;
	norm[2] = 0.0;
	um_vctovc(ell->center, pos);

    pos[0] += ell->a * epos.s;  
    pos[1] += ell->b * epos.t; 

	ncl_sm_normepos(ell,epos,norm);
	pos[0] += ell->offset * norm[0];
	pos[1] += ell->offset * norm[1];
	pos[2] = 0.0;
}

/*********************************************************************
**    E_FUNCTION: ncl_sm_func(dia, ell) 
**       solve offfset ellipse using Brent's method
**    PARAMETERS
**       INPUT  : 
**          angle		- ellipse angle
**			ell		: The ellipse
**       OUTPUT : 
**			The ellipse offset distance
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
double ncl_sm_func(angle, ell) 
double angle;
Ellipse *ell;
{
	UM_coord p1;
	UM_vector pos1;
	EPosition epos1;

    epos1.angle = angle;
    ncl_sm_setangle(epos1,pos1);
	epos1.s = pos1[0];
	epos1.t = pos1[1];

	ncl_sm_offsetellipsepoint(ell,epos1,p1);

	return p1[1];
}

/*********************************************************************
**    E_FUNCTION: ncl_sm_difference(const EPosition& aPosition, const CPosition& aPoint) 
**       difference-function for the offset ellipse 
**    PARAMETERS
**       INPUT  : 
**          ell		: The ellipse
**          epos	: The ellipse position
**       OUTPUT : 
**			The ellipse distance difference
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
double ncl_sm_difference(ell,epos,point)
Ellipse *ell;
EPosition epos;
UM_coord point;
{
	UM_coord pt1;
    ncl_sm_offsetellipsepoint(ell,epos,pt1);
    return (pt1[1] - point[1]);
}

/*********************************************************************
**    E_FUNCTION: ncl_sm_findellipsepos(ell,aPoint)
**       Given one ellipse point, find another ellipse point
**    PARAMETERS
**       INPUT  : 
**          aPoint	: The ellipse point
**          ell		: The ellipse
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_sm_findellipsepos(ell,aPoint)
UM_coord aPoint;
Ellipse *ell;
{
	double dif1 = fabs(ncl_sm_difference(ell,ell->epos1,aPoint));
    ell->epos2.s = ell->epos1.s;
    ell->epos2.t = -ell->epos1.t;
    if (fabs(ncl_sm_difference(ell,ell->epos2,aPoint)) < dif1 + ZEPS)  
	{   
        if ((fabs(ell->epos2.s - ell->epos1.t) > UM_DFUZZ) ||
			(fabs(ell->epos2.t - ell->epos1.t) > UM_DFUZZ))           
			return UU_TRUE;
    }
    
    ell->epos2.s = -ell->epos1.s;  
    ell->epos2.t = ell->epos1.t; 
    if (fabs(ncl_sm_difference(ell,ell->epos2,aPoint)) < dif1 + ZEPS) 
	{ 
        if ((fabs(ell->epos2.s - ell->epos1.s) > UM_DFUZZ) ||
			(fabs(ell->epos2.t - ell->epos1.t) > UM_DFUZZ))
            return UU_TRUE;
    }
    
    ell->epos2.s = -ell->epos1.s;  
    ell->epos2.t = -ell->epos1.t; 
    if (fabs(ncl_sm_difference(ell,ell->epos2,aPoint)) < dif1 + ZEPS)  
	{  
        if ((fabs(ell->epos2.s - ell->epos1.s) > UM_DFUZZ) || 
			(fabs(ell->epos2.t - ell->epos1.t) > UM_DFUZZ)) 
            return UU_TRUE;
    }
    
    ell->epos2.s = ell->epos1.s;  
    ell->epos2.t = ell->epos1.t; 
    if (fabs(ncl_sm_difference(ell,ell->epos2,aPoint)) < dif1 + ZEPS)       
		return UU_TRUE;

	return UU_FALSE;
}

/*********************************************************************
**    E_FUNCTION: ncl_sm_brent(ell,point)
**       solve offfset ellipse using Brent's method
**    PARAMETERS
**       INPUT  : 
**          aPoint		- ellipse point
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sm_brent(ell,point)
UM_coord point;
Ellipse *ell;
{
	UM_vector posa,posb;
	double tol,dia;
	int iters;

    EPosition eposa, eposb;
    eposa.angle = 0.0;
    eposb.angle = 3.0;
    ncl_sm_setangle(eposa,posa);
	eposa.s = posa[0];
	eposa.t = posa[1];
    ncl_sm_setangle(eposb,posb);
	eposb.s = posb[0];
	eposb.t = posb[1];

	iters = 1;
	tol = 1E-12;
/*
.....The root is now bracketed between eposa and eposb
*/
    dia = Brent0(eposa.angle,eposb.angle,ell,tol,ncl_sm_func); 

    eposa.angle = dia;
    ncl_sm_setangle(eposa,posa);
	eposa.s = posa[0];
	eposa.t = posa[1];
	ncl_copy_epos(eposa, &ell->epos1);
/*
.....Get another possible ellipse point
*/
	if (!ncl_sm_findellipsepos(ell,point)) 
		return 0;

	return iters;
}

/*********************************************************************
**    E_FUNCTION: ncl_sm_findellipseecenter(ell,up1,up2,index,ecen)
**
**      Get the ellipse center given the m_epos1 and m_epos2 and
**		the line trough aPoint1 and aPoint2
**    PARAMETERS
**       INPUT  : 
**          aPoint1	    - ellipse point
**          aPoint2		- ellipse point
**			index		- m_epos index
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_sm_findellipseecenter(ell,pt1,pt2,index,ecen)
Ellipse *ell;
UM_coord pt1,pt2,ecen;
int index;
{
	double offset,t;
    EPosition pos;
	UM_coord offsetpoint;
    if (index == 1)
		ncl_copy_epos(ell->epos1, &pos);
    else
        ncl_copy_epos(ell->epos2, &pos);
/*
.....Calculate the ellipse offset point
*/
	ncl_sm_offsetellipsepoint(ell,pos,offsetpoint);
/*
.....Calculate the ellipse center
*/
    offset = - offsetpoint[0];
    t = (ell->center[0] + offset - pt1[0])/(pt2[0] - pt1[0]);
	ecen[0] = pt1[0] + t * (pt2[0] - pt1[0]);
	ecen[1] = pt1[1] + t * (pt2[1] - pt1[1]);
	ecen[2] = pt1[2] + t * (pt2[2] - pt1[2]);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_edge_contact1(crad,frad,ptri,clpt,ccpt)
**       Calculate the clpt max z and ccpt given traingle edges for Bullnose
**    PARAMETERS
**       INPUT  :
**			crad	- tool corner radius
**			frad	- tool flat radius
**          ptri   - triangle 
**          clpt   - the clpt (x,y)
**       OUTPUT :
**          clpt   - the clpt (x,y,maxz)
**          ccpt   - the ccpt
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_edges_contact1(crad,frad,ptri,clpt,clmaxz,ccpt)
UU_REAL crad,frad,*clmaxz;
UM_trian *ptri;
UM_coord clpt,ccpt;
{ 
	UM_coord pt1,pt2,scpt,ccpos,pt1a,pt2a,clpta;
	UM_vector norm,vec,vec1,vec2,vec_horz;
	int i,nedge,nrs;
	UU_REAL ccdis,clz,dispt1,dispt2,du,dz,radius,dis,dh,r,t,a,b,c,d,e;
	UU_REAL dis2,radius2,crad2,frad2,srad,lrad,angle;
	UU_REAL a1,b1,c1,d1,xt[4],yc,yt,f,g,h,maxclz;
	UM_segment seg;

	UM_coord ellcenter,ecen,ucl,ecen1,ecen2,ell_ccp,cc_tmp,poscl;
	UM_vector vec12,pos1,pos2;
	EPosition epos1,epos2,epos;
	Ellipse *ell=UU_NULL;
	int iters;

	UU_REAL um_sqdis_from_segment_2d();
/*
	char tbuf[80];
*/
	radius = crad + frad;
    radius2 = radius * radius;   
	crad2 = crad * crad;   					
    frad2 = frad * frad;  
	maxclz = *clmaxz;
	clpt[2] = *clmaxz;
	um_vctovc(clpt,scpt); 

    for (nedge = 0; nedge < 3; nedge++) 
	{
/*
.....Get the triangle edges
*/ 
		ncl_trian_edge(nedge,ptri,pt1,pt2);         
/*
.....Check the edge is not vertical
*/ 
		if (fabs(pt1[0]-pt2[0]) > UM_DFUZZ || fabs(pt1[1]-pt2[1]) > UM_DFUZZ)
		{    
/*
.....The minmum 2d distance from clpt to the edge
*/		
			dis2 = um_sqdis_from_segment_2d(clpt,pt1,pt2,UU_FALSE);  							
			dis = sqrt(dis2);	
			if (dis2 <= radius2)
			{
/*
				ncl_debug_clpt2(pt1,pt2,6);
*/
				if (fabs(pt1[2]-pt2[2]) < UM_DFUZZ)
				{
/*
					sprintf(tbuf,"$$horiztal Edge"); 						
					NclxDbgPstr(tbuf);
					ncl_debug_clpt2(pt1,pt2,6);
*/
/*
.....The edge is horizontal
*/					
					um_nptln_2d(clpt,pt1,pt2,scpt);						
					um_vctovc(scpt,ccpos); 
/*
.....Get the contact point on the edge
*/					
					um_get_between_point(pt1,pt2,UU_TRUE,ccpos);
					if (dis2 < frad2)
					{
/*
.....The edge contact with flat area of tool
*/									
						clz = pt1[2];
					}
					else if (dis2 <= radius2)
					{
/*
.....The edge contact with corner area of tool
*/								
						dh = crad - sqrt(crad*crad - (dis-frad)*(dis-frad));  		
						clz = ccpos[2] - dh;   
					}

					if (um_point_in_segment(ccpos,pt1,pt2))
					{ 
/*
						ncl_debug_clpt2(pt1,pt2,9);
						ncl_debug_triangle(0,1,1,0,ptri);
						ncl_debug_clpt(ccpos,10);
*/						
						if (clz > maxclz)
						{				
							maxclz = clz;
							um_vctovc(ccpos,ccpt);
						}
					} 
				}
				else
				{
/*
.....The edge is not horizontal
*/		
					um_vctovc(pt1,seg.p1);
					um_vctovc(pt2,seg.p2);
#if 0
					if (Sedgelst.cur_cnt > 0 && 
						ncl_sgbucket_contain_seg(&Sedgelst,&seg))			
						continue;
					uu_list_push(&Sedgelst,&seg);
#endif
/*
					sprintf(tbuf,"$$General Edge"); 
					NclxDbgPstr(tbuf);
					ncl_debug_clpt2(pt1,pt2,4);
*/
					um_nptln_2d(clpt,pt1,pt2,scpt);		

					um_vcmnvc(pt2,pt1,vec);
                    vec[2] =0;   
					um_unitvc(vec,vec);

					um_vcmnvc(pt1,scpt,vec1);
					um_vcmnvc(pt2,scpt,vec2);
					dispt1 = um_dot(vec1,vec);
					dispt2 = um_dot(vec2,vec);
/*
.....The edge end points in the new coordinate:   
*/
					pt1a[0] = dispt2; pt1a[1] = dis; pt1a[2] = pt2[2];				
					pt2a[0] = dispt1; pt2a[1] = dis; pt2a[2] = pt1[2];
/*
.....The clpt point in the new coordinate
*/
					clpta[0]= 0.0; clpta[1] = 0.0;
					clpta[2] = maxclz;

/*
.....The ellipse is the intersection of cutter bottom plane and the cylinder
.....the short radius is the corner radius, the longer radius is corner 
.....radius/sin(alpha)alpha is the angle of the slope of the edge
*/
					r = crad;
					if (pt2a[0] != pt1a[0])
						angle = atan((pt2a[2]-pt1a[2])/(pt2a[0]-pt1a[0]));   
					t = fabs(crad/sin(angle));

					ellcenter[0] = 0.0;
					ellcenter[1] = dis;
					ellcenter[2] = 0.0;
/*
.....The ellipse to solve
*/
					ell = (Ellipse *) uu_malloc(sizeof(Ellipse));
					um_vctovc(ellcenter, ell->center);
					ell->a = t;
					ell->b = r;
					ell->offset = frad;
					
					epos1.angle = 0.0;
					ncl_sm_setangle(epos1,pos1);
					epos1.s = pos1[0];
					epos1.t = pos1[1];
					ncl_copy_epos(epos1,&ell->epos1);

					epos2.angle = 0.0;
					ncl_sm_setangle(epos2,pos2);
					epos2.s = pos2[0];
					epos2.t = pos2[1];				
					ncl_copy_epos(epos2,&ell->epos2);	

					poscl[0] = 0.0;
					poscl[1] = 0.0;
					poscl[2] = 0.0;
					iters = ncl_sm_brent(ell,poscl);
/*
.....Find the ellpse center points
*/
					ncl_sm_findellipseecenter(ell, pt1a, pt2a, 1, ecen1); 
					ncl_sm_findellipseecenter(ell, pt1a, pt2a, 2, ecen2); 
								
					if (ecen1[2] >= ecen2[2])
					{           
						um_vctovc(ecen1,ecen);	
						ncl_copy_epos(ell->epos1, &epos);
					} 
					else
					{         
						um_vctovc(ecen2,ecen);	
						ncl_copy_epos(ell->epos2, &epos);											
					}   
					um_vctovc(ecen,ell->center);	
/*
.....Calculate cutter contact point
*/
					ncl_sm_ellipsepoint(ell,epos,ell_ccp);				
					uu_free(ell);
/*
.....The contact point in XY plane
*/
					um_vcmnvc(pt1a,pt2a,vec12);
					um_unitvc(vec12, vec12);
					um_nptln(ell_ccp,pt1a,vec12,cc_tmp);
					um_translate_point(scpt,cc_tmp[0],vec,ccpos);
/*
.....Get the contact point on the edge
*/
					um_get_between_point(pt1,pt2,UU_TRUE,ccpos);
					clz = ecen[2]- crad; 
					if (um_point_in_segment(ccpos,pt1,pt2))
					{ 
/*
						ncl_debug_clpt2(pt1,pt2,9);
						ncl_debug_triangle(0,1,1,0,ptri);
						ncl_debug_clpt(ccpos,10);
*/
						if (clz > maxclz)
						{				
							maxclz = clz;
							um_vctovc(ccpos,ccpt);
						}
					} 
				}
			}
		}   
     }

	 *clmaxz = maxclz;
} 

