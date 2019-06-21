
/********************************************************************* 
**  NAME:  gsqrt.c
**
**  Contains:	
**		Gfloat ug_sqrt( x )
**
**  COPYRIGHT  1984  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL
**       gsqrt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:25
**
*********************************************************************/

#include <stdio.h>
#include "umath.h"
#include "usysdef.h"
#include "udebug.h"
#include "gobas.h"

#define TABLESIZ		100
#define XMAX			(UU_REAL) 1.0
#define EPS				(UU_REAL) 1.0e-6

/* These two arrays are the squart root table */
static Gfloat x[TABLESIZ];
static Gfloat fofx[TABLESIZ];

/*********************************************************************
**    I_FUNCTION     :  ug_sqrt(x0)
**
**		Fast version of sqrt() for values of x0: 0.0 < x < 10.0.
**		Returns sqrt for x0 outside these values.
**       
**    PARAMETERS   
**       INPUT  : 
**				x				Gfloat between 0 and 10 to find sqare root of.
**
**       OUTPUT :  
**          none
**    RETURNS      : ~ sqrt(n)
**    SIDE EFFECTS : none
**    WARNINGS     : X must be in range 0.0 < x < 1.0 for speed increase.
**							Not an exact function.
*********************************************************************/

Gfloat ug_sqrt(x0)
Gfloat x0;
{
	Gfloat answer;
	static int init=1;
	Gfloat t;
	int a, b;
	int i;

	uu_denter(-1,(us,"ug_sqrt(%f)",x0));

	/* If x is out of range, return sqrt(x0) */
	if( x0 < 0.0 || x0 > XMAX ) {
		uu_dprint(-1,(us,"ERROR, x out of range...returning sqrt(%f)", x0));
		uu_dexit;
		return( sqrt(x0) );
	}

	/* Initialize ? */
	if(init) {
		
		for(i=0; i<TABLESIZ; i++) {
			x[i] = XMAX / (TABLESIZ-1) * i;
			fofx[i] = sqrt(x[i]);
		}

		init = 0;

	}

	/* Interpolate answer from values in table */
	a = x0 * (TABLESIZ-1) / XMAX;
	b = a+1;

	t = (x0-x[a]) / (x[b]-x[a]);
	uu_dprint(UU_GTRC,(us,"t = %f",t));

	answer = (1.0-t)*fofx[a] + t*fofx[b];

	uu_dprint(UU_GTRC,(us,"ug_sqrt %f, sqrt %f, err %f",
		answer, sqrt(x0), sqrt(x0)-answer ));

	uu_dexit;
	return(answer);

}
