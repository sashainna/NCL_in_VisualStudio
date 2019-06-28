
/********************************************************************* 
**  NAME:  gpow.c
**
**  Contains:	
**		Gfloat pow( x, n )
**
**  COPYRIGHT  1984  UNICAD, Inc.
**
**    MODULE NAME AND RELEASE LEVEL
**       gpow.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:23
*********************************************************************/

#include <stdio.h>
#include "umath.h"
#include "udeboff.h"
#include "gobas.h"

#define MAXPOWS		25
#define TABLESIZ		100
#define EPS				(UU_REAL) 1.0e-6

/* Table containing power information */
static struct {
	int exponent;
	Gfloat *value;
} table[MAXPOWS];

static Gfloat xs[TABLESIZ];

/* Number of exponents seen so far. */
static int maxpow = 0;

/*********************************************************************
**    I_FUNCTION     :  ug_pow(x, n)
**
**		Fast version of pow() for values of x: 0.0 < x < 1.0, and
**		integer values of n.
**       
**    PARAMETERS   
**       INPUT  : 
**				x				Float between 0 and 1 to be raised to nth power.
**				n				Int to raise x by.
**
**       OUTPUT :  
**          none
**    RETURNS      : ~ x**n
**    SIDE EFFECTS : none
**    WARNINGS     : X must be in range 0.0 < x < 1.0. 
**							Pow(x,n) returned if n > MAXPOWS.
**							Not an exact function.
*********************************************************************/

Gfloat ug_pow(x, n)
Gfloat x;
int n;
{
	Gfloat answer;
	Gfloat denom;
	Gfloat xa, xb;
	Gfloat t;
	int a, b;
	int in;
	int i;

	uu_denter(UU_GTRC,(us,"ug_pow(%f, %d)",x,n));

	/* If x is out of range, return pow(x,n) */
	if( x < 0.0 || x > 1.0 ) {
		uu_dprint(-1,(us,"ERROR, x out of range...returning pow(%f,%d)",
			x, n));
		uu_dexit;
		return( pow(x,(Gfloat)n) );
	}

	/* Find if array of already calculated powers exists */
	for(in=0; in<maxpow; in++) {
		if( table[in].exponent == n ) break;
	}

	/* If we've exceeded MAXPOWS, return pow(x,n) */
	if( in == MAXPOWS ) {
		uu_dprint(-1,(us,"ERROR, MAXPOWS exceeded, returning pow(%f,%d)",
			x, n));
		uu_dexit;
		return( pow(x,(Gfloat)n) );
	}

	if( in == maxpow ) {		/* Need to make table for this exponent */
		uu_dprint(UU_GTRC,(us,"creating table for index %d",in));

		/* If this is first exponent, create the x array */
		if( in == 0 ) {
			answer = 1.0 / (TABLESIZ-1);
			for(i=0; i<TABLESIZ; i++) {
				xs[i] = answer*i;
			}
		}
			

		/* Malloc space for this table */
		table[in].value = (Gfloat *) malloc(TABLESIZ*sizeof(Gfloat));

		/* Fill in this table */
		table[in].exponent = n;
		answer = 1.0 / (TABLESIZ-1);
		for(i=0; i<TABLESIZ; i++) {
			uu_dprint(UU_GTRC,(us,"pow(%f %d) = %f", 
				xs[i], n, pow(answer*i,(Gfloat)n) ));
			table[in].value[i] = pow(xs[i],(Gfloat)n);
		}
		  
		maxpow++;
	}

	/* Interpolate answer from values in table */
	a = x * (TABLESIZ-1);
	b = a+1;

	if( a == TABLESIZ-1 ) {
		uu_dprint(UU_GTRC,(us,"ug_pow returns %f", table[in].value[a]));
		uu_dexit;
		return(table[in].value[a]);
	}

	uu_dprint(UU_GTRC,(us,"a %d, b %d, in %d", a, b, in));
	uu_dprint(UU_GTRC,(us,"v(a) %f, v(b) %f", 
		table[in].value[a], table[in].value[b]));

	xa = xs[a];
	xb = xs[b];

	uu_dprint(UU_GTRC,(us,"xa %f, xb  %f", xa, xb));

	t = (x-xa) / (xb-xa);
		
	uu_dprint(UU_GTRC,(us,"t = %f, in = %d",t, in));

	answer = (1.0-t)*table[in].value[a] + t*table[in].value[b];

/* If uncommented, remember that an answer=0.0 blows up!
/*	uu_dprint(UU_GTRC,(us,"ug_pow %f, pow %f, err %f",
/*		answer, pow(x,(UU_REAL)n), ( pow(x,(UU_REAL)n)-answer )/answer ));
*/

	uu_dexit;
	return(answer);

}
