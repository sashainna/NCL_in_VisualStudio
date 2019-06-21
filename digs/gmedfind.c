
/*********************************************************************
**  NAME:  gmedfind.c
**
**  Contains:	
**
**  COPYRIGHT  1984  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL
**       gmedfind.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:21
**
*********************************************************************/

#include "ustdio.h"
#include "udebug.h"
#include "tmedcut.h"

extern Node *root;			/* Root of the k-d tree */

#define	INF				1024
#define	ABS(a)			(((a) > 0) ? (a) : -(a))

/* Squares of 0-31 */
static short square[] = {	0, 	1,		4,		9,		16,
									25, 	36,	49, 	64,	81,
									100,	121,	144,	169,	196,
									225,	256,	289,	324,	361,
									400,	441,	484,	529,	576,
									625,	676,	729,	784,	841,
									900,	961 };

#define SQRD(a)			square[a]
/* #define	SQRD(a) 			( (a) * (a) ) */

/* Finds square of euclidean distance between two eight bit
 * color rgb triples
 */
#define 	DISSIM(a,b,c)  (SQRD(a) + SQRD(b) + SQRD(c) )
								
/* Global variables needed during the best match tree traversal */
static short X[3];
static int pqd;
static int pqr;
static int bupper[3];
static int blower[3];

#ifdef EXHAUSTIVE
static int exhaustive=0;
#endif

/*********************************************************************
**    I_FUNCTION     :  create_vvlt_entry(color)
**
**    PARAMETERS
**       INPUT  :
**          root			Root node of the median cut k-d tree.
**				color			Color whose best match we wish to find.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int create_vvlt_entry(val)
int val[3];
{
	int i;
	int d1, d2;
	int r1, r2;

	uu_denter(-1,(us,"create_vvlt(%x, color %d %d %d)",
		root, val[0], val[1], val[2]));

	/* Initialize global variables for search */
	pqd = INF;
	for(i=0; i<3; i++ )
	{
		X[i] = val[i];
		bupper[i] = INF;
		blower[i] = -INF;
	}

	/* Search, recursive starting at root */
	search( root );

/*	if( pqd > 8 )
	{
/*		UU_debmask = 513;
/*		uu_denter2(UU_GTRC,(us,"turning on trace, dist %d, index %d",
/*			pqd, pqr ));
/*
/*		/* Initialize global variables for search */
/*		pqd = INF;
/*		for(i=0; i<3; i++ )
		{
/*			X[i] = val[i];
/*			bupper[i] = INF;
/*			blower[i] = -INF;
/*		}
/*
/*		search( root );
/*		uu_dexit;
/*	}
*/

/*	if( pqd > 8 )
	{
/*		printf("dist %d, index %d\n", pqd, pqr );
/*		fflush();
/*	}
*/

	/* Print results */
	uu_dprint(UU_GTRC,(us,"final results, dist %d, index %d",
		pqd, pqr ));

#ifdef  EXHAUSTIVE
	d1 = pqd; r1 = pqr;

	if( d1 > 0 )
	{
		/* Initialize global variables for search */
		pqd = INF;
		for(i=0; i<3; i++ )
		{
			X[i] = val[i];
			bupper[i] = INF;
			blower[i] = -INF;
		}

		/* Exhaustive search, recursive starting at root */
		exhaustive = 1; search( root ); exhaustive = 0;
		d2 = pqd; r2 = pqr;
		
		uu_dprint(UU_GTRC,(us,"final results, dist %d, index %d",
			pqd, pqr ));

		if( d1 != d2 || r1 != r2 )
		{
			printf("search results, dist %d, index %d\n", d1, r1 );
			printf("exhaust result, dist %d, index %d\n", d2, r2 );
		}
	}
#endif

	uu_dexit;
	return(pqr);
}


/*********************************************************************
**    I_FUNCTION     :  static search
**
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static search(node)
Node *node;
{
	int dist;
	int irtn;
	int r, g, b;					/* Rgb indicies of color */
	int p, d, temp;
	
	uu_denter(UU_GTRC,(us,"search(%x)",node));

	/* If node is terminal, examine distance to this nodes color */
	if( node->left == NULL && node->right == NULL )
	{

		uu_dprint(UU_GTRC,(us,"terminal node color %d %d %d",
			node->color->val[0], node->color->val[1], node->color->val[2]));

		r = node->color->val[R] - X[R];	r = ABS(r);
		g = node->color->val[G] - X[G];	g = ABS(g);
		b = node->color->val[B] - X[B];	b = ABS(b);
		dist = DISSIM( r, g, b );

		uu_dprint(UU_GTRC,(us,"dist %d, pqd %d",dist,pqd));
		if( dist < pqd )
		{
			pqd = dist;							/* New color error */
			pqr = node->partition;			/* New vlt index */
			uu_dprint(UU_GTRC,(us,"pqd = %d, pqr = %d",pqd,pqr));
		}

		/* If error distance less than cube size, we're done */
		if( BALL_WITHIN_BOUNDS() )
		{
			irtn = 0;			/* No more searching needed */
		}
		else
		{
			irtn = 1;			/* Still need to search */
		}
		uu_dprint(UU_GTRC,(us,"search returns %d",irtn));
		uu_dexit;
		return( irtn );

	}

	d = node->discriminator;
	p = node->partition;

	uu_dprint(UU_GTRC,(us,"d %d, p %d",d,p));
	uu_dprint(UU_GTRC,(us,"recursive call on closer son, X[d] %d",X[d]));
			
	/* Make recursive call on closer son */
	if( X[d] <= p )
	{
		temp = bupper[d];
		bupper[d] = p;
		irtn = search( node->left );
		bupper[d] = temp;
	}

	else
	{
		temp = blower[d];
		blower[d] = p;
		irtn = search( node->right );
		blower[d] = temp;
	}

	if( irtn == 0 )
	{
		uu_dprint(UU_GTRC,(us,"search returns %d",irtn));
		uu_dexit;
		return(irtn);
	}

	/* Now make recursive call on farther son, if necessary */

	uu_dprint(UU_GTRC,(us,"recursive call on farther son, X[d] %d",X[d]));

	if( X[d] <= p )
	{
		temp = blower[d];
		blower[d] = p;
		if( BOUNDS_OVERLAP_BALL() )
			irtn = search( node->right );
		blower[d] = temp;
	}
	else
	{
		temp = bupper[d];
		bupper[d] = p;
		if( BOUNDS_OVERLAP_BALL() )
			irtn = search( node->left );
		bupper[d] = temp;
	}

	if( irtn == 0 )
	{
		uu_dprint(UU_GTRC,(us,"search returns %d",irtn));
		uu_dexit;
		return(irtn);
	}

	/* See if we should return or terminate,
	 * if error distance less than cube size, we're done */
	if( BALL_WITHIN_BOUNDS() )
	{
		irtn = 0;			/* No more searching needed */
	}
	else
	{
		irtn = 1;			/* Still need to search */
	}
	uu_dprint(UU_GTRC,(us,"search returns %d",irtn));
	uu_dexit;
	return( irtn );

}


/*********************************************************************
**    I_FUNCTION     :  static int BALL_WITHIN_BOUNDS
**
**		Determines if the sphere with radius "minimum error found thus
**		far" intersects the coordinate boundary along each color value.
**		The test fails as soon as one of these coordinate distances is
**		less than the radius. The test succeeds if all of these coordinate
**		distances are greateer than the radius.
**
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static int BALL_WITHIN_BOUNDS()
{
	int i;				/* Loop counter */
	int ld;				/* Linear directed distance */

	uu_denter(UU_GTRC,(us,"BALL_WITHIN_BOUNDS"));

#ifdef EXHAUSTIVE
	if( exhaustive )
		uu_dexit;
		return(0);
#endif

	uu_dprint(UU_GTRC,(us,"lower bounds %d %d %d",
		blower[0], blower[1], blower[2]));
	uu_dprint(UU_GTRC,(us,"upper bounds %d %d %d",
		bupper[0], bupper[1], bupper[2]));

	for( i=0; i<3; i++ )
	{
		ld = X[i] - blower[i];
		if( ld < 0 ) goto outside;
		if( SQRD(ld) <= pqd ) goto outside;

		ld = bupper[i] - X[i];
		if( ld < 0 ) goto outside;
		if( SQRD(ld) <= pqd ) goto outside;
	}

	uu_dprint(UU_GTRC,(us,"returning 1"));
	uu_dexit;
	return(1);

outside:
	uu_dprint(UU_GTRC,(us,"returning 0"));
	uu_dexit;
	return(0);
}
		

/*********************************************************************
**    I_FUNCTION     :  static int BOUNDS_OVERLAP_BALL
**
**		Determine whether the geometric boundaries delimiting a vlt
**		entry overlap a ball centered at the query color with radius
**		r equal to the dissimilarity of the closest color so far
**		encountered.  That is, do we have to search the opposite
**		half-space of a node.  NO, if the current error radius
**		does not intersect the half-space.
**
**
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int BOUNDS_OVERLAP_BALL()
{
	int d;
	int sum[3];

	uu_denter(UU_GTRC,(us,"BOUNDS_OVERLAP_BALL"));

#ifdef EXHAUSTIVE
	if( exhaustive )
		uu_dexit;
		return(1);
#endif

	sum[0] = sum[1] = sum[2] = 0;

	for( d=0; d<3; d++ )
	{

		/* Lower than low boundary ? */
		if( X[d] < blower[d] )
		{
			sum[d] = blower[d] - X[d];
			if( DISSIM( sum[0], sum[1], sum[2]) > pqd )
			{
				uu_dprint(UU_GTRC,(us,"returning no"));
				uu_dexit;
				return(0);
			}
		}

		/* Higher than high boundary ? */
		else if( X[d] > bupper[d] )
		{
			sum[d] = X[d] - bupper[d];
			if( DISSIM( sum[0], sum[1], sum[2]) > pqd )
			{
				uu_dprint(UU_GTRC,(us,"returning no"));
				uu_dexit;
				return(0);
			}
		}

	}

	uu_dprint(UU_GTRC,(us,"returning yes"));
	uu_dexit;
	return(1);
}
