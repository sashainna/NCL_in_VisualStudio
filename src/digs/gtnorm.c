
/********************************************************************* 
**  NAME:  gtnorm.c
**
**  Contains:	
**		Gfloat ug_taylor_normalize( vec )
**
**  COPYRIGHT  1984  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL
**       gtnorm.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:25
**
*********************************************************************/

#include <stdio.h>
#include <math.h>
#include "udebug.h"
#include "gobas.h"

/*********************************************************************
**    I_FUNCTION     :  ug_taylor_normalize(vec)
**
**		Ron McElhaney's fast approximate vector normalizer.  Uses a
**		Taylor series expansion to approimate unit vector to an
**		accuracy of 1.0e-4
**       
**    PARAMETERS   
**       INPUT  : 
**				vec			Vector to normalize.
**
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : This is an approximate solution!!!
*********************************************************************/

ug_taylor_normalize(vec)
Gfloat vec[];
{
	Gfloat mag;
	Gfloat den;
	Gfloat g, gg;
	Gfloat a, b, c;			/* Absolute values of vector components */

	uu_denter(UU_GTRC,(us,"ug_taylor_normalize(%f %f %f)",
		vec[0], vec[1], vec[2]));

	a = vec[0] > 0.0 ? vec[0] : -vec[0];
	b = vec[1] > 0.0 ? vec[1] : -vec[1];
	c = vec[2] > 0.0 ? vec[2] : -vec[2];

	if( a>=b && a>=c ) {
		g   = (b*b + c*c) / (a*a);
		den = a;
	}

	else if( b>=a && b>=c ) {
		g   = (a*a + c*c) / (b*b);
		den = b;
	}

	else {
		g   = (a*a + b*b) / (c*c);
		den = c;
	}

	if( g < 1.0 ) 
		gg = 1.0 + 0.4856016*g - 0.0713881*g*g;

	else if( g<2.0) 
		gg = 1.032 + 0.4144016*g - 0.0321881*g*g;

	else
		gg = 1.7320508;

	gg *= den;
	vec[0] /= gg;
	vec[1] /= gg;
	vec[2] /= gg;

	uu_dexit;
}
