/*********************************************************************
**  NAME:  gmedcut.c
**
**  Contains:	ug_medcut()
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL
**		 gmedcut.c , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:21
**
*********************************************************************/

#include "ustdio.h"
#include "udebug.h"
#include "ualloc.h"
#include "tmedcut.h"
#include "ubuffio.h"

Node *root;
static int hist[32][32][32];
static Qelement *Q=NULL, *qend=NULL;

#define GETENV(var, default) \
{ \
	char *getenv(); \
	char *cptr; \
	cptr = getenv("var"); \
	if( cptr != NULL )  \
		var = atoi(cptr); \
	else \
		var = default; \
}

#define  COPYBOUNDS( a, b ) \
	a->rmin = b->rmin;	a->rmax = b->rmax; \
	a->gmin = b->gmin;	a->gmax = b->gmax; \
	a->bmin = b->bmin;	a->bmax = b->bmax;

#define  ABS(a)	( ((a) > 0) ? (a) : -(a) )

/* File pointers to write and read from */
int fbin, fbout;

ug_medcut(infile, outfile, vlt_size, dither)
char *infile;
char *outfile;
int vlt_size;
int dither;
{
	int i, j, k;
	Node *build_tree();
	UU_STORE *store;

	uu_denter(UU_GITRC,(us,"ug_medcut(%s %s %d %d)",
		infile, outfile, vlt_size, dither));

	uu_dprint(UU_GITRC,(us,"input %s output %s\n",infile, outfile));

	/* Initialize a new store for the medcut algoritm */
	store = (UU_STORE *) uu_create_store();

	/* Make the store the current store */
	uu_alloc_push( store );
			
	/* Open the frame buffer file */
	uu_dprint(UU_GITRC,(us,"opening input file %s",infile));
	if( (fbin = open( infile, 0 )) < 0 )
	{
		/*
		fprintf(stderr, "can't open %s\n", infile);
		exit(1);
		*/
		uu_dexit;
		return (-1);	/* cannot open infile */
	}

	uu_dprint(UU_GITRC,(us,"opening output file %s",outfile));
	if(  (fbout = creat( outfile, 0644 )) < 0 )
	{
		/*
		fprintf(stderr, "can't open %s\n", outfile);
		exit(1);
		*/
		uu_dexit;
		return (-2);	/* cannot open outfile */
	}
	uu_dprint(UU_GITRC,(us,"fbin %d, fbout %d", fbin, fbout));

	/* Create the histogram of colors */
	create_histogram(fbin);

	/* Build the median cut k-d tree */
	root = build_tree( &vlt_size );

	/* Write number of colors in vlt */
	write_vlt_size(vlt_size);

	/* Write the vlt. Create by averaging the color cubes at each leaf
	 * of root.
	 */
	write_vlt( 1, root );

	/* Write a frame buffer file with color indicies to fbout */
	if( dither )
		write_dither_fb(hist);
	else
		write_indexed_fb(hist);

	/* Close the files */
	close(fbin);
	close(fbout);


	/* Pop the store, and free it */
	uu_alloc_pop();
	uu_free_store( store );

	/* Clear all static variables */
	Q=NULL; qend=NULL;
	for( i=0; i<32; i++ )
		for( j=0; j<32; j++ )
			for( k=0; k<32; k++ )
					hist[i][j][k] = 0;

	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  static create_histogram(fb)
**
**    PARAMETERS
**       INPUT  :
**          fb			File pointer
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Fills the color histogram with values found in file.
**    WARNINGS     : none
*********************************************************************/
static create_histogram(fb)
int fb;
{
	int nbytes;
	int n;
	short ir, ig, ib;
	UT_COLOR_REC color;

	uu_denter(-1,(us,"create_histogram(%d)",fb));

	/* Create the histogram of colors */
	for(;;)
	{

/*		nbytes = uu_bread(fb, &n, sizeof(int)); */
		UU_BREAD(fb, &n, sizeof(int), nbytes);

		uu_dprint(UU_GTRC,(us,"uu_bread returns %d, n=%d",nbytes, n));

		if( nbytes != sizeof(int) )
		{
			uu_dprint(UU_GITRC,(us,"end of data read"));
			break;
		}

		if( n > 0 )
		{

/*			uu_bread(fb, &color, sizeof(color)); */
			UU_BREAD(fb, &color, sizeof(color), nbytes);

			uu_dprint(UU_GTRC,(us,"%d of %d %d %d",
				n, color.uname.color.red,
				color.uname.color.green,
				color.uname.color.blue));

			ir = color.uname.color.red >> 3;
			ig = color.uname.color.green >> 3;
			ib = color.uname.color.blue  >> 3;
			hist[ir][ig][ib] += n;

		}
	}
	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  static hist_to_vlt(root)
**
**    PARAMETERS
**       INPUT  :
**				root		Root node of the k-d median cut tree.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Fills the color histogram with vlt entries.
**    WARNINGS     : none
*********************************************************************/
static hist_to_vlt(root)
Node *root;
{
	int val[3];
	short ir, ig, ib;

	uu_denter(-1,(us,"hist_to_vlt(%x)",root));

	/* For each color entry in the histogram, replace color count with
	 * a vlt index.
	 */
	for( ir=0; ir<32; ir++ )
	{
		for( ig=0; ig<32; ig++ )
		{
			for( ib=0; ib<32; ib++ )
			{

				if( hist[ir][ig][ib] > 0 )
				{
					val[0] = ir;
					val[1] = ig;
					val[2] = ib;
					hist[ir][ig][ib] = create_vvlt_entry(val);

					uu_dprint(UU_GITRC,(us,"hist[%d][%d][%d] = %d\n",
						ir, ig, ib, hist[ir][ig][ib]));
				}
			}
		}
	}
	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  build_tree
**
**		Builds the median cut k-d tree.  This is a binary tree, each
**		internal node of the tree represents some fraction of the entire
**		rgb cube.  The root node represents all of rgb space.  The two
**		sons of the root node each represent a half-space of the root,
**		etc.  The average color in each leaf of the tree is represented
**		in the video-lookup table.
**
**		This algorithm is derived from "Color Image Quantization
**		for Frame Buffer Display", by Paul Heckbert, Computer Graphics,
**		Vol 16, No. 3, July 1982.  He implies that k-d trees might be
**		used for efficient mapping of original colors to their nearest
**		neighbors in the vidio-lookup table.  I've implemented just
**		that scheme.  For more about k-d trees... "An Algorithm for
**		Finding Best Matches in Logarithmic Expected Time", by Friedman,
**		et. al., ACM Transactions on Mathematical Software, Vol. 3, No. 3,
**		Sept 1977, Pages 209-226.
**
**    PARAMETERS
**       INPUT  : node
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

Node *build_tree(nleaves)
int *nleaves;
{
	int leaf;							/* Current leaf count */
	Node *root;							/* Root of k-d tree */
	Qelement *qelm;					/* One element in Q */
	Qelement *pushq(), *popq();	/* Returns pointer to pushed Q element */
	Color *color_list();				/* Returns pointer to histogram colors */

	uu_denter(UU_GTRC,(us,"build_tree(%d)", *nleaves));

	/* Create the k-d tree in breath-first order.  Elements are pushed
	 * onto the queue (I use the list package for the queue) until the
	 * number of desired leaves of the tree is reached.
	 */

	/* Create the root node and place it in the queue */
	qelm = pushq();
	root = qelm->node;

	/* Root node spans entire rgb space */
	qelm->rmin = 0;		qelm->rmax = 31;
	qelm->gmin = 0;		qelm->gmax = 31;
	qelm->bmin = 0;		qelm->bmax = 31;

	/* Create list of all colors from histogram */
	qelm->node->color = color_list();

	/* Split rgb boxes along longest dimensions until we get nleaves */
	leaf = 1;							/* One current leaf, the root */
	while( leaf < *nleaves )
	{

		/* Split along longest dimension */
		if( split_box( qelm ) ) leaf++;

		/* Pop this element off queue, get next queue element */
		qelm = popq();

		if( qelm == NULL )
		{
			uu_dprint(UU_GTRC,(us,"queue empty"));
			break;
		}

	}

	uu_dprint(UU_GTRC,(us,"build_tree exits with %d leaves", leaf));
	*nleaves = leaf;

	uu_dexit;
	return(root);
}


/*********************************************************************
**    I_FUNCTION     :  split_box( qelm )
**
**		Splits a queue box, adds new boxes to queue.
**
**    PARAMETERS
**       INPUT  : node
**       OUTPUT :
**    RETURNS      : 1 if box split, 0 if not.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static int split_box( qelm )
Qelement *qelm;
{
	int irtn;												/* Return value */
	Color *color;											/* A vfb color entry */
	int rmin, rmax, gmin, gmax, bmin, bmax;		/* Color extremes */
	int r_range, g_range, b_range;					/* Color ranges */
	int range;												/* Max color range */
	int discriminator;									/* Color component */
	int partition;											/* Partion of rgb space */
	Qelement *qleft, *qright;							/* New Q elements */
	Qelement *pushq();									/* Pushes element on Q */

	uu_denter(UU_GTRC,(us,"split_box(%x)", qelm));

	/* Find longest dimension. Traverse list of colors for this box, find
	 * max and min for each color component.
	 */
	rmin = gmin = bmin = 256;
	rmax = gmax = bmax = -1;
	for( color=qelm->node->color; color!=NULL; color=color->nxt )
	{

/*		uu_dprint(UU_GTRC,(us,"color %d %d %d",
/*			color->val[R], color->val[G], color->val[B]));
*/		
		if( color->val[R] < rmin ) rmin = color->val[R];
		if( color->val[G] < gmin ) gmin = color->val[G];
		if( color->val[B] < bmin ) bmin = color->val[B];
		
		if( color->val[R] > rmax ) rmax = color->val[R];
		if( color->val[G] > gmax ) gmax = color->val[G];
		if( color->val[B] > bmax ) bmax = color->val[B];

	}

	/* Set the discriminator to the axis which has the greatest range. */
	r_range = rmax - rmin;
	g_range = gmax - gmin;
	b_range = bmax - bmin;

	if( r_range >= g_range && r_range >= b_range )
	{
		range = r_range;
		discriminator = R;
	}
	if( g_range >= r_range && g_range >= b_range )
	{
		range = g_range;
		discriminator = G;
	}
	if( b_range >= r_range && b_range >= g_range )
	{
		range = b_range;
		discriminator = B;
	}

	uu_dprint(UU_GTRC,(us,"range %d, discriminator %d",
		range, discriminator));

	/* Create two new nodes, sort the colors along the longest range,
	 * split at median, and place halves in the new nodes. */
	if( range > 0 )
	{

		/* Create two new nodes. */
		qleft = pushq();
		qright = pushq();

		qelm->node->left = qleft->node;
		qelm->node->right = qright->node;

		radix_sort( qelm->node->color, &qleft->node->color,
			&qright->node->color, discriminator, &partition );

		uu_dprint(UU_GTRC,(us,"node %x, discriminator %d, partition %d",
			qelm->node, discriminator, partition));

		qelm->node->discriminator = discriminator;
		qelm->node->partition = partition;

		/* Add these new nodes to the queue */
		switch( discriminator )
		{

			case R:
				COPYBOUNDS( qleft, qelm );
				qleft->rmax = partition;
	
				COPYBOUNDS( qright, qelm );
				qright->rmin = partition;
				break;

			case G:
				COPYBOUNDS( qleft, qelm );
				qleft->gmax = partition;
	
				COPYBOUNDS( qright, qelm );
				qright->gmin = partition;
				break;

			case B:
				COPYBOUNDS( qleft, qelm );
				qleft->bmax = partition;
	
				COPYBOUNDS( qright, qelm );
				qright->bmin = partition;
				break;
		}

		/* Return successful split */
		irtn = 1;

	}
	else
	{

		/* No successful split */
		irtn = 0;
	}

	uu_dprint(UU_GTRC,(us,"returning %d",irtn));
	uu_dexit;
	return( irtn );
}


/*********************************************************************
**    I_FUNCTION     :  radix_sort()
**
**		A radix list sort is performed on the colors pointed to by color.
**		The sorted list is broken at the median into two sub-lists,
**		left and right.  The sort is performed on the color coordinate
**		specified by discriminator.  The partition point (a rgb value)
**		is returned.
**
**    PARAMETERS
**       INPUT  : Color *color 			List of colors to sort.
**						Color **left 			First sub-list returned.
**						Color **right 			Second sub-list returned.
**						int discriminator 	Rgb value to sort on.
**						int *partition			Partition value at median.
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static radix_sort( color, left, right, discriminator, partition )
Color *color;
Color **left, **right;
int discriminator;
int *partition;
{
	int i;							/* Loop counter */
	int index;						/* Index in radix list */
	Color *c;						/* One color entry */
	Color *end;						/* End of color list */
	int total_colors;				/* Total # colors in vfb */
	int median;						/* Half of total colors */
	static struct
	{				/* Radix list stored here */
		Color *start;
		Color *end;
		int count;
	}	radix[256];

	uu_denter(UU_GTRC,(us,"radix_sort( %x, %x, %x, %d )",
		color, left, right, discriminator));

	/* Initialize the radix list */
	total_colors = 0;
	for( i=0; i<256; i++ )
	{
		radix[i].start = radix[i].end = NULL;
		radix[i].count = 0;
	}

	/* Traverse all colors in list, place in radix keeping count */
	for( c=color; c!=NULL; c=c->nxt )
	{
		
		/* Find correct index into radix list */
		index = c->val[discriminator];

		if( radix[index].count == 0 )
		{		/* First color at this index */
			radix[index].start = c;				/* Save starting list pointer */
		}
		else
		{										/* Been here already */
			radix[index].end->nxt = c;			/* Chain this color into list */
		}

		radix[index].end = c;					/* Update end of list */
		radix[index].count += c->count;		/* Update list count */
		total_colors += c->count;				/* Update total color count */

	}


	/* Find median in radix list, left list starts at beginning, right list
	 * starts at median.
	 */
	median = total_colors / 2;				/* Median of colors */
	end = NULL;									/* Initialize list */

	uu_dprint(UU_GTRC,(us,"total_colors %d, median %d",
		total_colors, median));

	total_colors = 0;							/* Initialize running color count */
	for( i=0; i<256; i++ )
	{

		/* If there's a list at this index, add it */
		if( radix[i].count > 0 )
		{

			/* Exit loop if we're at median */
			if( ABS(median-total_colors) <
				 ABS(median-(total_colors+radix[i].count)) )
			{
				
				uu_dprint(UU_GTRC,(us,"partition = %d", *partition));
				break;
			}

			total_colors += radix[i].count;		/* Update running total */

			if( end == NULL )
			{
				*left = radix[i].start;				/* Start of left list */
			}
			else
			{
				end->nxt = radix[i].start;			/* Add this list to current list */
			}

			/* Update new end of list */
			end = radix[i].end;
			end->nxt = NULL;
			*partition = i;
		}
	}

	uu_dprint(UU_GTRC,(us,"left list"));
	print_color_list( *left );

	end = NULL;
	for( i=i; i<256; i++ )
	{
		
		/* If there's a list here, add it */
		if( radix[i].count > 0 )
		{

			if( end == NULL )
			{
				*right = radix[i].start;				/* Start of right list */
			}
			else
			{
				end->nxt = radix[i].start;			/* Add this list to current list */
			}

			/* Update new end of list */
			end = radix[i].end;
			end->nxt = NULL;
		}
	}

	uu_dprint(UU_GTRC,(us,"right list"));
	print_color_list( *right );

	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  pushq()
**
**		Push a queue element onto the list.
**
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static Qelement *pushq()
{
	Node *node;
	Qelement *q;

	node = (Node *) uu_malloc( sizeof(Node) );
	node->left = node->right = NULL;

	/* Create the root node and place it in the queue */
	if( Q == NULL )  Q = (Qelement *) uu_lsnew();

	q = (Qelement *) uu_lsinsrt(Q, sizeof(Qelement));
	q->node = node;
	if( qend == NULL ) qend = q;

	return(q);

}

/*********************************************************************
**    I_FUNCTION     :  popq()
**
**		Pops current queue element off the queue, returns next element in
**		queue or null.
**
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static Qelement *popq()
{
	Qelement *q;

	q = qend;
	qend = (Qelement *)uu_lsprev(qend);
	uu_lsdele(q);

	return(qend);

}

/*********************************************************************
**    I_FUNCTION     :  color_list()
**
**		Create linked list of all colors in histogram.
**
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static Color *color_list()
{
	Color *head, *new, *cur;
	int r, g, b;

	uu_denter(UU_GTRC,(us,"color_list()"));
	head = cur = new = NULL;
	for( r=0; r<32; r++ )
	{
		for( g=0; g<32; g++ )
		{
			for( b=0; b<32; b++ )
			{

				if( hist[r][g][b] )
				{
					new = (Color *)uu_malloc( sizeof(Color) );
					if( head == NULL )
					{
						head = new;
					}
					else
					{
						cur->nxt = new;
					}

					cur = new;
					cur->val[R] = r;	cur->val[G] = g; cur->val[B] = b;
					cur->count = hist[r][g][b];
					cur->nxt = NULL;
				}
			}
		}
	}
	print_color_list(head);
	uu_dexit;
	return(head);
}

/*********************************************************************
**    I_FUNCTION     :  print_color_list( listhead )
**
**		Print linked list of colors.
**
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static print_color_list( listhead )
Color *listhead;
{
	Color *c;
	char us[120];

	for( c=listhead; c!=NULL; c=c->nxt )
	{
		uu_dprint(UU_GTRC,(us,"%d %d %d count %d",
			c->val[R], c->val[G], c->val[B], c->count));
	}
}


/*********************************************************************
**    I_FUNCTION     :  print_tree( node )
**
**		Prints the colors at each leaf of tree starting at node.
**
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static print_tree( node )
Node *node;
{
	char us[120];

	if( node->left != NULL )
		print_tree( node->left );

	if( node->right != NULL )
		print_tree( node->right );

	if( node->right == NULL && node->left == NULL )
	{
		uu_dprint(UU_GTRC,(us,"color list for node %x",node));
		print_color_list( node->color );
	}
}
