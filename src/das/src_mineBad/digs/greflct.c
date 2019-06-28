/********************************************************************* 
**  NAME:  greflct.c
**
**  Contains:	
**    ug_specular()
**    ug_diffuse()
**    ug_gouraud()
**    ug_initgouraud() 
**  COPYRIGHT  1984  UNICAD, Inc.
**
**    MODULE NAME AND RELEASE LEVEL
**       greflct.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:24
*********************************************************************/

#include <stdio.h>
#include "umath.h"
#include "udebug.h"
#include "usysdef.h"
#include "gtbl.h"
#include "gobas.h"
#include "gows.h"
#include "goren.h"
#include "grender.h"
#include "gtblvar8.h"

#define DOT(a,b)		( ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2]) )
#define MAG(a,b,c)	( sqrt(a*a + b*b + c*c) )

/*********************************************************************
**    I_FUNCTION     :  ug_specular 
**
**		Returns specular component of phong lighting model.   This
**		follows closely the lighting model suggested in Revision 2.0
**		of the PHIGS+ Functional Description.
**
**    PARAMETERS   
**       INPUT  : 
**				ks				Specular coefficient.
**				oe				Object specular exponent.
**				op				3-D point on surface to shade.
**				vn				Unit normal to surface at point.
**				os				Object specular color.
**       OUTPUT :  
**          cs				Specular component of surface intensity.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ug_specular( ws, ks, oe, op, vn, os, cs )
Gws *ws;					/* Workstation containing light definitions */
Gfloat ks, oe;			/* Specular coefficients */
Gfloat op[];			/* Point on surface. */
Gfloat vn[];			/* Unit surface normal */
Gcolr *os;				/* Object specular color */
Gcolr *cs;				/* Specular component of surface intensity */
{
	int i;						/* Loop counter */
	Gfloat dprd, dprd2;		/* Dot products */
	Gfloat ug_pow();			/* Fast integer power function */
	Gfloat vr[3];				/* Unit surface reflection vector */
	Gfloat vl[3];				/* Unit vector from object to light */
	Gfloat la;					/* Light source attenuation */
	Glighttbl *p;				/* Points to light table */
	Gfloat makela();			/* Calculates la */

	/* Direction to viewer */
	static Gfloat ve[3] = { 0.0, 0.0, 1.0 };

	uu_denter(UU_GITRC,(us,"ug_specular(*ws=%d, ks=%f, oe=%f)", *ws, ks, oe));
	uu_dprint(UU_GITRC,(us,"vn %f %f %f", vn[0], vn[1], vn[2]));
	uu_dprint(UU_GITRC,(us,"os %f %f %f", os->red, os->green, os->blue));
	uu_dprint(UU_GITRC,(us,"op %f %f %f", op[0], op[1], op[2]));

	cs->red = cs->green = cs->blue = 0.0;
	p = ug_gksstli.wsopen[*ws].wdtptr->outwdtpt->lights;

	/* Calculate specular intensity contribution from each light source */
	for(i=0; i<GMAXLIGHTS; i++) {		/* For each light */

		/* If light is active, add its contribution */
		if( ug_gksstli.rendats.lightstate[i] == UU_TRUE ) {

			switch(p[i].type) {
			case UG_AMBIENT:			/* No specular contribution */
				break;

			case UG_DIRECTIONAL: 	/* Cs = Ks * Os * Lc * (Ve.Vr)**Oe */

				uu_dprint(UU_GITRC,(us,"UG_DIRECTIONAL dir=%f %f %f",
					p[i].data.dir.x, p[i].data.dir.y, p[i].data.dir.z));
				uu_dprint(UU_GITRC,(us,"light color=%f %f %f",
					p[i].data.color.red, p[i].data.color.green, 
					p[i].data.color.blue));

				makevr( vn, &p[i].data.dir.x, vr );
				dprd = DOT(ve,vr);
		  		if( dprd < 0.0 ) dprd = 0.0;

				dprd = ug_pow(dprd, (int)oe);

				/* Add contribution of this light */
				cs->red   += p[i].data.color.red   * dprd;
				cs->green += p[i].data.color.green * dprd;
				cs->blue  += p[i].data.color.blue  * dprd;

				break;

			case UG_POINT:
				uu_dprint(UU_GITRC,(us,"UG_POINT pos=%f %f %f",
					p[i].data.pos.x, p[i].data.pos.y, p[i].data.pos.z));
				uu_dprint(UU_GITRC,(us,"c1=%f, c2=%f", p[i].data.c1, p[i].data.c2));
				uu_dprint(UU_GITRC,(us,"light color=%f %f %f",
					p[i].data.color.red, p[i].data.color.green, 
					p[i].data.color.blue));

				/* Calculate unit vector from object to light source */
				makevl( op, &p[i].data.pos.x, vl );

				/* Calculate unit relflection vector */
				makevr( vn, vl, vr );

				dprd = DOT(ve, vr);
		  		if( dprd < 0.0 ) dprd = 0.0;
				dprd = ug_pow(dprd, (int)oe);
				
				/* Calculate light source attenuation */
				la = makela( op, &p[i].data.pos.x, p[i].data.c1, p[i].data.c2 );
		  
				/* Multiply all the terms together once, for performance */
				dprd = dprd * la;
		  
				/* Add contribution of this light */
				cs->red   += p[i].data.color.red   * dprd;
				cs->green += p[i].data.color.green * dprd;
				cs->blue  += p[i].data.color.blue  * dprd;

				break;

			case UG_SPOT:
				uu_dprint(UU_GITRC,(us,"UG_SPOT pos=%f %f %f",
					p[i].data.pos.x, p[i].data.pos.y, p[i].data.pos.z));
				uu_dprint(UU_GITRC,(us,"dir=%f %f %f",
					p[i].data.dir.x, p[i].data.dir.y, p[i].data.dir.z));
				uu_dprint(UU_GITRC,(us,"c1=%f, c2=%f, cone %f, exp %f",
					p[i].data.c1, p[i].data.c2, p[i].data.spread, p[i].data.conc));
				uu_dprint(UU_GITRC,(us,"light color=%f %f %f",
					p[i].data.color.red, p[i].data.color.green, 
					p[i].data.color.blue));

				/* Calculate the unit vector from object to light (vl) */
				makevl( op, &p[i].data.pos.x, vl );

				dprd2 = -vl[0]*p[i].data.dir.x
						  -vl[1]*p[i].data.dir.y
						  -vl[2]*p[i].data.dir.z;

				/* Contribution of this light source is zero outside 
				 * cone of influence. Note, light data record contains 
				 * the cosine of the half spread angle!
				 */
				if( dprd2 < p[i].data.spread) {
					uu_dprint(UU_GITRC,(us,"outside cone: dprd2 %f, spread %f", 
						dprd2, p[i].data.spread));
					break;
				}

				/* Add attenuation from concentration exponent */
				dprd2 = ug_pow( dprd2, (int)p[i].data.conc );

				/* Calculate the unit reflection vector (vr) */
				makevr( vn, vl, vr );

				dprd = DOT(ve,vr);
		  		if( dprd < 0.0 ) dprd = 0.0;
				dprd = ug_pow(dprd, (int)oe);

				/* Calculate light source attenuation */
				la = makela( op, &p[i].data.pos.x, p[i].data.c1, p[i].data.c2 );

				/* Multiply all the terms together once, for performance */
				dprd = dprd * dprd2 * la;

				uu_dprint(UU_GITRC,(us,"this light contributes %f %f %f",
					(p[i].data.color.red   * dprd),
					(p[i].data.color.green * dprd),
					(p[i].data.color.blue  * dprd)));
		  
				/* Add contribution of this light */
				cs->red   += (p[i].data.color.red   * dprd);
				cs->green += (p[i].data.color.green * dprd);
				cs->blue  += (p[i].data.color.blue  * dprd);

				break;

			default:
				uu_dprint(-1,(us,"ERROR ug_specular: unknown light source type"));
			}

			uu_dprint(UU_GITRC,(us,"specular after light %d, %f %f %f",
				i, cs->red, cs->green, cs->blue));
		}
	}

	/* Multiply by object surface color and specular coefficient */
	cs->red   *= (ks * os->red);
	cs->green *= (ks * os->green);
	cs->blue  *= (ks * os->blue);

	uu_dprint(UU_GITRC,(us,"ug_specular returns %f %f %f",
		cs->red, cs->green, cs->blue));
	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  ug_diffuse 
**
**		Returns diffuse component of phong lighting model.   This
**		follows closely the lighting model suggested in Revision 2.0
**		of the PHIGS+ Functional Description.
**
**    PARAMETERS   
**       INPUT  : 
**				kd				Diffuse coefficient.
**				op				3-D point on surface to shade.
**				vn				Unit normal to surface at point.
**				od				Object diffuse color.
**       OUTPUT :  
**          cd				Diffuse component of surface intensity.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ug_diffuse( ws, kd, op, vn, od, cd )
Gws *ws;					/* Workstation containing light definitions */
Gfloat kd;				/* Diffuse coefficient */
Gfloat op[];			/* Point on surface. */
Gfloat vn[];			/* Unit surface normal */
Gcolr *od;				/* Object diffuse color */
Gcolr *cd;				/* Diffuse component of surface intensity */
{
	int i;						/* Loop counter */
	Gfloat dprd, dprd2;		/* Dot products */
	Gfloat la;					/* Light source attenuation */
	Gfloat vl[3];				/* Unit vector from object to light */
	Gfloat ug_pow();			/* Fast integer power function */
	Glighttbl *p;				/* Points to light table */
	Gfloat makela();			/* Calculates la */


	uu_denter(UU_GITRC,(us,"ug_diffuse(*ws=%d, kd=%f)", *ws, kd));
	uu_dprint(UU_GITRC,(us,"vn %f %f %f",vn[0], vn[1], vn[2]));
	uu_dprint(UU_GITRC,(us,"os %f %f %f", od->red, od->green, od->blue));
	uu_dprint(UU_GITRC,(us,"op %f %f %f", op[0], op[1], op[2]));

	cd->red = cd->green = cd->blue = 0.0;
	p = ug_gksstli.wsopen[*ws].wdtptr->outwdtpt->lights;

	/* Calculate diffuse intensity contribution from each light source */
	for(i=0; i<GMAXLIGHTS; i++) {		/* For each light */

		/* If light is active, add its contribution */
		if( ug_gksstli.rendats.lightstate[i] == UU_TRUE ) {

			switch(p[i].type) {
			case UG_AMBIENT:			/* No diffuse contribution */
				break;

			case UG_DIRECTIONAL: 	/* Cs = Ks * Os * Lc * (Ve.Vr)**Oe */

				uu_dprint(UU_GITRC,(us,"UG_DIRECTIONAL dir=%f %f %f",
					p[i].data.dir.x, p[i].data.dir.y, p[i].data.dir.z));
				uu_dprint(UU_GITRC,(us,"light color=%f %f %f",
					p[i].data.color.red, p[i].data.color.green, 
					p[i].data.color.blue));

				dprd =	vn[0]*p[i].data.dir.x +
							vn[1]*p[i].data.dir.y +
							vn[2]*p[i].data.dir.z;

		  		if( dprd < 0.0 ) dprd = 0.0;

				/* Add contribution of this light */
				cd->red   += p[i].data.color.red   * dprd;
				cd->green += p[i].data.color.green * dprd;
				cd->blue  += p[i].data.color.blue  * dprd;

				break;

			case UG_POINT:
				uu_dprint(UU_GITRC,(us,"UG_POINT pos=%f %f %f",
					p[i].data.pos.x, p[i].data.pos.y, p[i].data.pos.z));
				uu_dprint(UU_GITRC,(us,"c1=%f, c2=%f", p[i].data.c1, p[i].data.c2));
				uu_dprint(UU_GITRC,(us,"light color=%f %f %f",
					p[i].data.color.red, p[i].data.color.green, 
					p[i].data.color.blue));

				makevl( op, &p[i].data.pos.x, vl );

				dprd = DOT(vn,vl);
		  		if( dprd < 0.0 ) dprd = 0.0;
				
				/* Calculate light source attenuation */
				la = makela( op, &p[i].data.pos.x, p[i].data.c1, p[i].data.c2 );
		  
				/* Multiply all the terms together once, for performance */
				dprd = dprd * la;
		  
				/* Add contribution of this light */
				cd->red   += p[i].data.color.red   * dprd;
				cd->green += p[i].data.color.green * dprd;
				cd->blue  += p[i].data.color.blue  * dprd;

				break;

			case UG_SPOT:
				uu_dprint(UU_GITRC,(us,"UG_SPOT pos=%f %f %f",
					p[i].data.pos.x, p[i].data.pos.y, p[i].data.pos.z));
				uu_dprint(UU_GITRC,(us,"dir=%f %f %f",
					p[i].data.dir.x, p[i].data.dir.y, p[i].data.dir.z));
				uu_dprint(UU_GITRC,(us,"c1=%f, c2=%f, cone %f, exp %f",
					p[i].data.c1, p[i].data.c2, p[i].data.spread, p[i].data.conc));
				uu_dprint(UU_GITRC,(us,"light color=%f %f %f",
					p[i].data.color.red, p[i].data.color.green, 
					p[i].data.color.blue));


				/* Calculate the unit vector from object to light (vl) */
				makevl( op, &p[i].data.pos.x, vl );

				dprd2 = -vl[0]*p[i].data.dir.x
						  -vl[1]*p[i].data.dir.y
						  -vl[2]*p[i].data.dir.z;

				/* Contribution of this light source is zero outside 
				 * cone of influence.
				 */
				if( dprd2 < p[i].data.spread) {
					uu_dprint(UU_GITRC,(us,"outside cone: dprd2 %f, spread %f", 
						dprd2, p[i].data.spread));
					break;
				}

				/* Add attenuation from concentration exponent */
				dprd2 = ug_pow( dprd2, (int)p[i].data.conc );

				dprd = DOT(vn, vl);
		  		if( dprd < 0.0 ) dprd = 0.0;

				/* Calculate light source attenuation */
				la = makela( op, &p[i].data.pos.x, p[i].data.c1, p[i].data.c2 );
		  
				/* Multiply all the terms together once, for performance */
				dprd = dprd * dprd2 * la;
		  
				uu_dprint(UU_GITRC,(us,"this light contributes %f %f %f",
					(p[i].data.color.red   * dprd),
					(p[i].data.color.green * dprd),
					(p[i].data.color.blue  * dprd)));
		  
				/* Add contribution of this light */
				cd->red   += p[i].data.color.red   * dprd;
				cd->green += p[i].data.color.green * dprd;
				cd->blue  += p[i].data.color.blue  * dprd;

				break;

			default:
				uu_dprint(-1,(us,"ERROR ug_diffuse: unknown light source type"));
			}

			uu_dprint(UU_GITRC,(us,"diffuse after light %d, %f %f %f",
				i, cd->red, cd->green, cd->blue));
		}
	}

	/* Multiply by object surface color and diffuse coefficient */
	cd->red   *= (kd * od->red);
	cd->green *= (kd * od->green);
	cd->blue  *= (kd * od->blue);

	uu_dprint(UU_GITRC,(us,"ug_diffuse returns %f %f %f",
		cd->red, cd->green, cd->blue));
	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  ug_ambient 
**
**		Returns ambient component of phong lighting model.   This
**		follows closely the lighting model suggested in Revision 2.0
**		of the PHIGS+ Functional Description.
**
**    PARAMETERS   
**       INPUT  : 
**				ka				Ambient coefficient.
**				od				Object diffuse color.
**       OUTPUT :  
**          ca				Ambient component of surface intensity.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ug_ambient( ws, ka, od, ca )
Gws *ws;					/* Workstation containing light definitions */
Gfloat ka;				/* Ambient coefficient */
Gcolr *od;				/* Object diffuse color */
Gcolr *ca;				/* Ambient component of surface intensity */
{
	int i;						/* Loop counter */
	Glighttbl *p;				/* Points to light table */

	uu_denter(UU_GITRC,(us,"ug_ambient(*ws=%d, ka=%f)", *ws, ka));
	uu_dprint(UU_GITRC,(us,"od %f %f %f", od->red, od->green, od->blue));

	ca->red = ca->green = ca->blue = 0.0;
	p = ug_gksstli.wsopen[*ws].wdtptr->outwdtpt->lights;

	/* Calculate ambient intensity contribution from each light source */
	for(i=0; i<GMAXLIGHTS; i++) {		/* For each light */

		/* If light is active, add its contribution */
		if( ug_gksstli.rendats.lightstate[i] == UU_TRUE ) {

			switch(p[i].type) {
			case UG_AMBIENT:	
				uu_dprint(UU_GITRC,(us,"UG_AMBIENT light color=%f %f %f",
					p[i].data.color.red, p[i].data.color.green, 
					p[i].data.color.blue));

				/* Add contribution of this light */
				ca->red   += p[i].data.color.red;
				ca->green += p[i].data.color.green;
				ca->blue  += p[i].data.color.blue;

				break;

			case UG_DIRECTIONAL:		/* No ambient contribution */
			case UG_POINT:
			case UG_SPOT:
				break;

			default:
				uu_dprint(-1,(us,"ERROR ug_ambient: unknown light source type"));
			}

			uu_dprint(UU_GITRC,(us,"ambient after light %d, %f %f %f",
				i, ca->red, ca->green, ca->blue));
		}
	}

	/* Multiply by object surface color and ambient coefficient */
	ca->red   *= (ka * od->red);
	ca->green *= (ka * od->green);
	ca->blue  *= (ka * od->blue);

	uu_dprint(UU_GITRC,(us,"ug_ambient returns %f %f %f",
		ca->red, ca->green, ca->blue));
	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  ug_gouraud 
**
**		Intensity calculation for Gouraud lighting model.  
**		Lambertian shading is used to compute intensities at 
**		verticies.
**
**		I = ia + kd * Sum(j=1 to nlights) (norm dot light[j]) 
**
**    PARAMETERS   
**       INPUT  : 
**				kd				Diffuse coefficient.
**				ks				Specular coefficient.			IGNORED!!!
**				sex			Phong specular exponent.		IGNORED!!!
**				point			3-D point on surface to shade.
**				norm			Unit normal to surface at point.
**       OUTPUT :  
**          none
**    RETURNS      : Lambert intensity at a point.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

Gfloat ug_gouraud( ws, kd, ks, sex, point, norm )
Gws *ws;							/* Workstation containing light definitions */
Gfloat kd;						/* Diffuse coefficient */
Gfloat ks, sex;				/* Specular coefficients IGNORED!!! */
Gfloat point[];				/* Point on surface. */
Gfloat norm[];					/* Surface normal */
{
	int i;	 					/* Loop counters */
	Gfloat intensity;			/* Returned Lambert intensity */
	Gfloat dprd;				/* Dot product */
	Glighttbl *p;				/* Points to light table */

	uu_denter(UU_GITRC,(us,"ug_gouraud()"));

	uu_dprint(UU_GITRC,(us,"kd %f, ks %f, sex %f", kd, ks, sex));
	uu_dprint(UU_GITRC,(us,"point %f %f %f",point[0], point[1], point[2]));
	uu_dprint(UU_GITRC,(us,"norm %f %f %f",norm[0], norm[1], norm[2]));

	p= ug_gksstli.wsopen[*ws].wdtptr->outwdtpt->lights;

	for(i=0; i<GMAXLIGHTS; i++) {
		if( ug_gksstli.rendats.lightstate[i] == UU_TRUE ) {
			uu_dprint(UU_GITRC,(us,"light %d: %f %f %f",
				i, p[i].data.dir.x,p[i].data.dir.y,p[i].data.dir.z));
		}
	}

	/* Calculate intensity contribution from each light source */
	intensity = 0.0;
	for(i=0; i<GMAXLIGHTS; i++) {		/* For each light */

		/* If light is active, add its contribution */
		if( ug_gksstli.rendats.lightstate[i] == UU_TRUE ) {

			switch(p[i].type) {
			case UG_AMBIENT:
				uu_dprint(UU_GITRC,(us,"UG_AMBIENT color=%f %f %f",
					p[i].data.color.red, p[i].data.color.green, 
					p[i].data.color.blue));
				intensity += ( p[i].data.color.red + 
									p[i].data.color.green + 
									p[i].data.color.blue ) / 3.0; 
				uu_dprint(UU_GITRC,(us,"intensity after light %d, %f",
					i, intensity));
				break;

			case UG_DIRECTIONAL: 
				uu_dprint(UU_GITRC,(us,"UG_DIRECTIONAL dir=%f %f %f",
					p[i].data.dir.x, p[i].data.dir.y, p[i].data.dir.z));
				uu_dprint(UU_GITRC,(us,"color=%f %f %f",
					p[i].data.color.red, p[i].data.color.green, 
					p[i].data.color.blue));

				dprd =	norm[0]*p[i].data.dir.x + 
							norm[1]*p[i].data.dir.y + 
							norm[2]*p[i].data.dir.z;
				if( dprd > 0 )
					intensity += dprd * (p[i].data.color.red + 
												p[i].data.color.green + 
												p[i].data.color.blue) / 3.0; 
				uu_dprint(UU_GITRC,(us,"intensity after light %d, %f",
					i, intensity));
				break;

			case UG_POINT:
				uu_dprint(UU_GITRC,(us,"UG_POINT type light source not supported"));
				break;

			case UG_SPOT:
				uu_dprint(UU_GITRC,(us,"UG_SPOT type light source not supported"));
				break;

			default:
				uu_dprint(-1,(us,"ERROR ug_gouraud: unknown light source type"));

			}
		}
	}

	uu_dprint(UU_GITRC,(us,"after all lights intensity = %f",intensity));

	if( intensity > 1.0 ) intensity = 1.0;

	uu_dprint(UU_GITRC,(us,"ug_gouraud returns %f",intensity));
	uu_dexit;
	return(intensity);
}


/*********************************************************************
**    I_FUNCTION     :  ug_initgouraud 
**
**		Initialize polygon data for gouraud shading model.  This involves
**		computing the rgb triple for each polygon vertex and saving the
**		color in the polygons normal field.
**
**    PARAMETERS   
**       INPUT  : 
**				polylist		List of all polygons to render.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : Rgb triple is store in vertex normal fields.
*********************************************************************/

ug_initgouraud( polylist )
Polygon *polylist;
{
	Polygon *poly;				/* Pointer to current polygon */
	Polygon *nxtpoly;			/* Pointer to next polygon */
	Edge *edge;					/* Pointer to current edge */
	Edge *nxtedge;				/* Pointer to next edge */
	Gfloat r, g, b;			/* Rgb triple */
	Gcolr color;				/* Surface color */
	Gcolr ambient, diffuse;	/* Ambient and diffuse color components */
	Gws ws=0;

	uu_denter(UU_GITRC,(us,"ug_initgouraud"));

	/* For each polygon */
	for( poly = polylist;
		  poly != NULL;
		  poly = poly->nxt ) {

		/* For each edge in this polygon */
		for( edge = poly->edge;
			  edge != NULL;
			  edge = edge->nxt ) {

			/* Compute color for first vertex of this edge */
			uu_dprint(UU_GITRC,(us,"vertex %x",edge->v1));

			color.red = poly->parent->colorrep.red;
			color.green = poly->parent->colorrep.green;
			color.blue = poly->parent->colorrep.blue;

			/* Calculate ambient intensity */
			ug_ambient(&ws, poly->parent->kd, &color, &ambient);

			/* Calculate diffuse intensity */
			ug_diffuse(&ws, poly->parent->kd, edge->v1->p, edge->v1->n, 
				&color, &diffuse);

			uu_dprint(UU_GITRC,(us,"stored rgb %f %f %f",
				ambient.red   + diffuse.red,
				ambient.green + diffuse.green,
				ambient.blue  + diffuse.blue));

			/* Store color information in vertex normal */
			edge->v1->n[0] = ambient.red   + diffuse.red;
			edge->v1->n[1] = ambient.green + diffuse.green;
			edge->v1->n[2] = ambient.blue  + diffuse.blue;

		}
	}

	uu_dexit;
}


/*********************************************************************
**    S_FUNCTION     :  static makevr(norm, lightdir, vr); 
**
**		Calcualtes the unit reflection vector from an object based
**		upon light direction and surface normal.
**
**    PARAMETERS   
**       INPUT  : 
**				norm			Unit surface normal
**				lightdir		Unit light direction
**       OUTPUT :  
**          vr				Unit reflection vector.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static makevr( vn, vl, vr )
Gfloat vn[];		/* Unit surface normal */
Gfloat vl[];		/* Unit light direction */
Gfloat vr[];		/* Unit reflection vector */
{
	Gfloat dprd;			/* 2 times norm*lightdir */

	dprd = 2 * DOT(vn,vl);
	vr[0] = dprd*vn[0] - vl[0];
	vr[1] = dprd*vn[1] - vl[1];
	vr[2] = dprd*vn[2] - vl[2];
}


/*********************************************************************
**    S_FUNCTION     :  static makevl(op, lp, vl)
**
**		Calcualtes the unit vector from object surface to
**		light source.
**
**    PARAMETERS   
**       INPUT  : 
**				op			Surface position.
**				lp			Light position
**       OUTPUT :  
**          vl			Unit vector from object to light source.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static makevl(op, lp, vl)
Gfloat op[];		/* Object position */
Gfloat lp[];		/* Light source position */
Gfloat vl[];		/* Unit vector from object to light source */
{

	vl[0] = lp[0] - op[0];
	vl[1] = lp[1] - op[1];
	vl[2] = lp[2] - op[2];
	ug_unitvec(vl);
 
}


/*********************************************************************
**    S_FUNCTION     :  static Gfloat makela( op, lp, c1, c2 )
**
**		Calculates the light source attenution based upon light postion
**		surface point and attenuation coefficients.
**
**    PARAMETERS   
**       INPUT  : 
**				op			Surface point
**				lp			Light position
**				c1,c2		Attenuation coefficients
**       OUTPUT :  
**          none	
**    RETURNS      : Light source attenuation (0.0->1.0)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static Gfloat makela( op, lp, c1, c2 )
Gfloat op[];		/* Surface point */
Gfloat lp[];		/* Light source position */
Gfloat c1, c2;		/* Attenuation coefficients */
{
	Gfloat mag;		/* Magnitude of distance between light source and surface */
	Gfloat tmp[3];	/* Vector from light to surface */
	Gfloat la;		/* Light source attenuation */

	tmp[0] = lp[0] - op[0];
	tmp[1] = lp[1] - op[1];
	tmp[2] = lp[2] - op[2];
	mag = MAG(tmp[0], tmp[1], tmp[2]);

	la = 1.0 / (c1 + c2*mag);
	return(la);
}
