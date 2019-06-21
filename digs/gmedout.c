
/*********************************************************************
**  NAME:  gmedout.c
**
**  Contains:
**
**  COPYRIGHT  1984  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL
**       gmedout.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:21
**
*********************************************************************/

#include <stdio.h>
#include "udebug.h"
#include "tmedcut.h"

extern int fbin, fbout;

#define MAXVLT 4096
#define MAXRES 2048
static UT_COLOR_REC vlt[MAXVLT];

#include "ubuffio.h"

static int vlt_index;

/*********************************************************************
**    I_FUNCTION     :  write_vlt( node )
**
**		Averages the colors at each leaf of tree starting at node, and
**		writes averages to fbout.
**
**    PARAMETERS
**       INPUT  :
**				node					Node where traversal starts.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
write_vlt( init, node )
int init;
Node *node;
{
	Color *c;
	float r, g, b;
	int count;
	UT_COLOR_REC color;

	uu_denter(UU_GTRC,(us,"write_vlt(init=%d, node %x)", init, node));

	if( init ) vlt_index = 0;

	if( node->left != NULL )
		write_vlt( 0, node->left );

	if( node->right != NULL )
		write_vlt( 0, node->right );

	if( node->right == NULL && node->left == NULL )
	{
		uu_dprint(UU_GTRC,(us,"averaging color list for node %x",node));
		
		r = g = b = 0.0;
		count = 0;
		for( c=node->color; c!=NULL; c=c->nxt )
		{
			uu_dprint(UU_GTRC,(us,"color %d %d %d count %d",
				c->val[R], c->val[G], c->val[B], c->count));
			r += ( (c->val[R]) * c->count );
			g += ( (c->val[G]) * c->count );
			b += ( (c->val[B]) * c->count );
			count += c->count;
		}

		node->color->val[R] = r/count + 0.5;
		node->color->val[G] = g/count + 0.5;
		node->color->val[B] = b/count + 0.5;
		node->partition = vlt_index;

		/* Write this vlt color */
		color.uname.color.red = node->color->val[R] << 3;
		color.uname.color.green = node->color->val[G] << 3;
		color.uname.color.blue = node->color->val[B] << 3;

		uu_dprint(UU_GTRC,(us,"new vlt index %d, color %d %d %d", vlt_index,
			color.uname.color.red, color.uname.color.green, color.uname.color.blue));

/*		uu_bwrite(fbout, &color, sizeof(color)); */
		UU_BWRITE(fbout, &color, sizeof(color));

		vlt[vlt_index].uname.color.red = color.uname.color.red;
		vlt[vlt_index].uname.color.green = color.uname.color.green;
		vlt[vlt_index].uname.color.blue = color.uname.color.blue;
		++vlt_index;

	}

	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  write_vlt_size( vlt_size )
**
**		Writes number of vlt entries into fbout.
**
**    PARAMETERS
**       INPUT  :
**				node					Node where traversal starts.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
write_vlt_size( vlt_size )
int vlt_size;
{

	uu_denter(UU_GTRC,(us,"write_vlt_size(%d)", vlt_size));

		UU_BWRITE(fbout, &vlt_size, sizeof(vlt_size));

	uu_dexit;


}


/*********************************************************************
**    I_FUNCTION     :  write_indexed_fb(vlt)
**
**		Read the virtual frame buffer, and write out colors using
**		indicies of vlt.	
**	
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/* Contains vlt scanline, used by run_eol to runlength encode. */
static int scan_index[MAXRES];

write_indexed_fb(vlt)
int vlt[32][32][32];
{
	int nbytes;
	int n;
	int x;
	int i, j, k;
	short ir, ig, ib;
	int index;
	int val[3];
	int eol = -1;
	UT_COLOR_REC color;

	uu_denter(-1,(us,"write_indexed_fb(%d %d %x)",fbin, fbout, vlt));

	/* Rewind the virtual frame buffer file */
	lseek( fbin, (long)0, 0 );

	/* Clear the vlt, we'll fill it with indices on demand */
	for(i=0; i<32; i++)
	{
		for(j=0; j<32; j++)
		{
			for(k=0; k<32; k++)
			{
				vlt[i][j][k] = -1;
			}
		}
	}
	
	/* Read virtual frame buffer, find vlt index in vlt, write out index */
	x=0;
	for(;;)
	{

		UU_BREAD(fbin, &n, sizeof(int), nbytes);

		if( nbytes != sizeof(int) )
		{
			uu_dprint(-1,(us,"end of data"));
			break;
		}

		if( n > 0 )
		{

			uu_dprint(-1,(us,"%d of ",n));
			UU_BREAD(fbin, &color, sizeof(color), nbytes);
			uu_dprint(-1,(us,"%d %d %d",color.uname.color.red,
					color.uname.color.green, color.uname.color.blue));

			ir = color.uname.color.red >> 3;
			ig = color.uname.color.green >> 3;
			ib = color.uname.color.blue  >> 3;

			index = vlt[ir][ig][ib];

			if( index < 0 )
			{
				val[0] = ir; val[1] = ig; val[2] = ib;
				index = vlt[ir][ig][ib] = create_vvlt_entry( val );
			}

			uu_dprint(-1,(us,"vlt[%d][%d][%d] = %d",
				ir, ig, ib, index));

			for(i=x; i<x+n; i++)
				scan_index[i] = index;

			x += n;
/*			UU_BWRITE(fbout, &n, sizeof(int));
/*			UU_BWRITE(fbout, &index, sizeof(int));
*/

		}
		else
		{			/* At end of scanline */
			run_eol(x, scan_index);
/*			UU_BWRITE(fbout, &eol, sizeof(int));
*/
			x = 0;
		}
	}

	UU_BFLUSH(fbout);
	uu_dexit;
}



typedef struct {	short r, g, b; } Pixel;

Pixel scan[2][MAXRES];
Pixel *s1 = scan[0];
Pixel *s2 = scan[1];

/*********************************************************************
**    I_FUNCTION     :  write_dither_fb(hist)
**
**		Read the virtual frame buffer, and write out colors using
**		indicies of vlt.	Dithering adds the color error at a pixel to
**		the pixels right, below, and diagonally right and below of
**		the current pixel.
**	
**		The alogrithm is:
**
**		for( i=0; i<NI; i++) {
**			for(j=0; j<NJ; j++)  {
**				x = c(i,j);				Read a color
**				k = p(x);				Find nearest rep.
**				f(i,j) = k;				Draw quantized image
**				e = x-yk;				Quantization error
**											Distribute in 3 directions
**
**				c(i,j+1) = c(i,j+1) + e*3/8		Right
**				c(i+1,j) = c(i+1,j) + e*3/8		Below
**				c(i+1,j+1) = c(i+1,j+1) + e/4		Right and below
**
**			}
**		}
**
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

write_dither_fb(hist)
int hist[32][32][32];
{
	int nbytes;
	int n;
	int i, j, k;
	short ir, ig, ib;
	int index;
	int x;						/* Current column */
	int val[3];
	UT_COLOR_REC color;
	Pixel e;
	Pixel *tmp;

	uu_denter(-1,(us,"write_dither_fb(%d %d %x)",fbin, fbout, hist));

	/* Rewind the virtual frame buffer file */
	lseek( fbin, (long)0, 0 );

	/* Clear the hist, we'll fill it with indices on demand */
	for(i=0; i<32; i++)
	{
		for(j=0; j<32; j++)
		{
			for(k=0; k<32; k++)
			{
				hist[i][j][k] = -1;
			}
		}
	}
	
	/* Read virtual frame buffer, find vlt index in hist, write out index */
	x = 0;
	for(;;)
	{

		UU_BREAD(fbin, &n, sizeof(int), nbytes);

		if( nbytes != sizeof(int) )
		{
			uu_dprint(-1,(us,"end of data"));
			break;
		}

		if( n > 0 )
		{

			uu_dprint(-1,(us,"x=%d, n=%d",x,n));
			UU_BREAD(fbin, &color, sizeof(color), nbytes);
			uu_dprint(-1,(us,"color %d %d %d",
				color.uname.color.red, color.uname.color.green, color.uname.color.blue));

			/* Dithering, add error term to color */
			for(i=x; i<x+n; i++)
			{
				uu_dprint(UU_GTRC,(us,"s1[%d] = %d %d %d",
					i, s1[i].r, s1[i].g, s1[i].b));
				s1[i].r += color.uname.color.red;
				s1[i].g += color.uname.color.green;
				s1[i].b += color.uname.color.blue;
			}
					
			/* For each color in run */
			for(i=x; i<x+n; i++)
			{

				/* Clamp s1 at 255 */
				s1[i].r = s1[i].r > 255 ? 255 : s1[i].r;
				s1[i].g = s1[i].g > 255 ? 255 : s1[i].g;
				s1[i].b = s1[i].b > 255 ? 255 : s1[i].b;

				/* Clamp s1 at 0 */
				s1[i].r = s1[i].r < 0 ? 0 : s1[i].r;
				s1[i].g = s1[i].g < 0 ? 0 : s1[i].g;
				s1[i].b = s1[i].b < 0 ? 0 : s1[i].b;

				uu_dprint(UU_GTRC,(us,"s1[%d] = %d %d %d",
					i, s1[i].r, s1[i].g, s1[i].b));
				ir = s1[i].r >> 3;
				ig = s1[i].g >> 3;
				ib = s1[i].b >> 3;
	
				index = hist[ir][ig][ib];

				if( index < 0 )
				{
					val[0] = ir; val[1] = ig; val[2] = ib;
					index = hist[ir][ig][ib] = create_vvlt_entry( val );
				}

				uu_dprint(-1,(us,"hist[%d][%d][%d] = %d",
					ir, ig, ib, index));

				scan_index[i] = index;

				/* Find error, add to subsequent colors. */
				uu_dprint(UU_GTRC,(us,"vlt[%d] = %d %d %d",
					index, vlt[index].uname.color.red,
					vlt[index].uname.color.green,
					vlt[index].uname.color.blue));

				e.r = s1[i].r - vlt[index].uname.color.red;
				e.g = s1[i].g - vlt[index].uname.color.green;
				e.b = s1[i].b - vlt[index].uname.color.blue;

				uu_dprint(UU_GTRC,(us,"error[%d] = %d %d %d",
					i, e.r, e.g, e.b));

				/* Add error term to neighboring pixels */
				s1[i+1].r += e.r * 3/8;
				s1[i+1].g += e.g * 3/8;
				s1[i+1].b += e.b * 3/8;

				s2[i].r += e.r * 3/8;
				s2[i].g += e.g * 3/8;
				s2[i].b += e.b * 3/8;

				s2[i+1].r = e.r/4;			/* Clears old color  */
				s2[i+1].g = e.g/4;
				s2[i+1].b = e.b/4;
			}

			x += n;
		}
		else
		{			/* At end of scanline */
			run_eol(x, scan_index);
			tmp = s1;
			s1 = s2;
			s2 = tmp;				/* Swap scanlines */
			s2[0].r = s2[0].g = s2[0].b = 0;			/* Clear s2[0] */
			x = 0;											/* Reset column count */
		}
	}

	UU_BFLUSH(fbout);
	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  run_eol()
**
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : This scanlines output is runlength encoded.
**    WARNINGS     : none
*********************************************************************/
static run_eol(xres, scan)
int xres;
int scan[];
{
	int i, n;
	int index;
	int eol = -1;					/* End of scanline mark */

	uu_denter(UU_GTRC,(us,"run_eol(xes=%d)", xres));

	index = scan[0];
	n = 0;

	for(i=1; i<xres; i++)
	{
		
		n++;
		if( scan[i] != index  )
		{

			UU_BWRITE(fbout, &n, sizeof(int));
			UU_BWRITE(fbout, &index, sizeof(int));
			index = scan[i];
			n = 0;
			
		}
	}

	/* Write the last group of pixels */
	n++;
	UU_BWRITE(fbout, &n, sizeof(int));
	UU_BWRITE(fbout, &index, sizeof(int));

	/* Write the end of scanline mark */
	UU_BWRITE(fbout, &eol, sizeof(int));	/* Write end of line */

	uu_dexit;
}
