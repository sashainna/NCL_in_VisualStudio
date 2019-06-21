/*********************************************************************
**    NAME         :  grgb.c
**       CONTAINS: 
**				ug_hsi_to_rgb
**				ug_rgb_to_hsi
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       grgb.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:24
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "gobas.h"

#define ONETHIRD	((UU_REAL) 1./ (UU_REAL) 3.)
#define TWOTHIRDS	((UU_REAL) 2./ (UU_REAL) 3.)
#define ONESIXTH	((UU_REAL) 1./ (UU_REAL) 6.)

#define MIN(A,B)				( (A) < (B) ? (A) : (B) )
#define MAX(A,B)				( (A) > (B) ? (A) : (B) )

/*********************************************************************
**    E_FUNCTION     :  ug_hsi_to_rgb(h, s, i, r, g, b)
**       Converts hue, saturation, intensity triple to its red, green,
**			blue equivalent.
**    PARAMETERS   
**       INPUT  : 
**          h:			Hue range 0..1
**				s:			Saturation range 0..1
**				i:			Intensity range 0..1
**       OUTPUT :  
**          r:			Red component range 0..1
**				g:			Green component range 0..1
**				b:			Blue component range 0..1
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ug_hsi_to_rgb( h, s, i, r, g, b )
Gfloat h, s, i, *r, *g, *b;
{

/* convert hue, saturation, intensity (h,s,v) to red, green,
		blue (r,g,b).  All values in [0,1].	This closely follows
		Foley & VanDam pg. 619													 */

Gfloat m1, m2, value();

	if( i<=0.5 ) 
		m2 = i*(1+s);
	else
		m2 = i+ s - i*s;

	m1 = 2*i - m2;

	if( s==0.0 )
		*r = *g = *b = i;						/* achromatic: there is no hue */

	else{										/* chromatic: there is a hue */
		*r = value(m1,m2,h+ONETHIRD);
		*g = value(m1,m2,h);
		*b = value(m1,m2,h-ONETHIRD);
	}

	*r = MIN(1.0, *r);
	*g = MIN(1.0, *g);
	*b = MIN(1.0, *b);
}


static Gfloat value( n1, n2, hue )
Gfloat n1, n2, hue;
{

	if( hue > 1.0 ) hue -= 1.0;

	if( hue < 0.0 ) hue += 1.0;

	if( hue < ONESIXTH )
		return(n1+(n2-n1)*hue*6.0);

	else if( hue < 0.5 )
		return(n2);

	else if( hue < TWOTHIRDS )
		return(n1 + (n2-n1)*(TWOTHIRDS-hue)*6.0);

	else
		return(n1);
}

/*********************************************************************
**    E_FUNCTION     :  ug_rgb_to_hsi( r,g,b, h,s,i )
**       Converts red, green, blue color triple to its hue, saturation,
**			intensity equivalent.
**    PARAMETERS   
**       INPUT  : 
**          r		red component range 0..1
**				g		green component range 0..1
**				b		blue component range 0..1
**       OUTPUT :  
**          h		hue range 0..1
**				s		saturation range 0..1
**				i		intensity range 0..1
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ug_rgb_to_hsi( r,g,b, h,s,i )
Gfloat r,g,b, *h,*s,*i;
{

/* convert rgb color to hsi equivalent...all values (0->1).  This closely
	follows Folley and Van Dam pg 618.													*/

Gfloat max, min, rc, gc, bc;

	uu_denter(UU_MTRC,(us,"ug_rgb_to_hsi(%f %f %f)", r,g,b));

	max = MAX( MAX(r,g), b);
	min = MIN( MIN(r,g), b);

	*i  = (max + min) / 2.0;				/* set intensity */

	if( max == min ){							/* r==g==b: achromatic */
		*s = 0.0;
		*h = 0.0;
	}
	
	else{
		
		if( *i <=0.5 )
			*s = (max-min)/(max+min);
		else
			*s = (max-min)/(2.0-max-min);

		/* calculate hue */
		rc = (max-r)/(max-min);
		gc = (max-g)/(max-min);
		bc = (max-b)/(max-min);

		if     ( r==max )
			*h = bc - gc;
		else if( g==max )
			*h = 2.0 + rc - bc;
		else if( b==max )
			*h = 4.0 + gc - rc;

		*h *= 1.0/6.0;

		*h = (*h < 0.0 ) ? *h + 1.0 : *h;
	}

	*h = MIN(1.0, *h);
	*s = MIN(1.0, *s);
	*i = MIN(1.0, *i);

	uu_dexit;
}
