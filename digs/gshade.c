
/********************************************************************* 
**  NAME:  gshade.c
**
**  Contains:	
**    ug_vfb_initscan( i )
**    ug_vfb_shade( s, e, r,g,b)
**    ug_vfb_pixels( s, e, data)
**    ug_vfb_eol()
**    ug_vfb_run()
**    ug_vfb_init(filename)
**    ug_vfb_close()
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**		 gshade.c , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:24
**
*********************************************************************/

#include <stdio.h>
#include "udebug.h"
#include "go.h"
/* #include "gtblvar8.h" */
#include "grender.h"
/* #include "glists.h" */

static int vfb;					/* Output file pointer */
static int maxxres;				/* Xresolution of image */
static Pixel scan[MAXRES];		/* Data for one scanline */

/*********************************************************************
**    I_FUNCTION     :  ug_vfb_initscan( i )
**
**		Write background over scanline.
**       
**    PARAMETERS   
**       INPUT  : 
**				i				Scanline to initialize to background.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ug_vfb_initscan( i )
int i;
{
	int j;

	uu_denter(UU_GTRC,(us,"ug_vfb_initscan(%d)",i));

	for( j=0; j<maxxres; j++ ) {
		scan[j].r = 0;
		scan[j].g = 0;
		scan[j].b = 0;
	}

	uu_dexit;

}

/*********************************************************************
**    I_FUNCTION     :  ug_vfb_shade( s, e, r,g,b)
**       
**    PARAMETERS   
**       INPUT  : 
**          s				Starting x pixel
**          e				Ending x pixel
**				r,g,b			Color to write
**				c				Char version of r,g,b
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Color is written to output file
**    WARNINGS     : none
*********************************************************************/

ug_vfb_shade(s, e, r,g,b)
int s, e;
unsigned char r,g,b;
{
	int i;
	uu_denter(UU_GTRC,(us,"ug_vfb_shade(%d to %d of %d %d %d)",
		s, e, r,g,b));

	for( i=s; i<=e; i++ ) {
		scan[i].r = r;
		scan[i].g = g;
		scan[i].b = b;
	}

	uu_dexit;

}


/*********************************************************************
**    I_FUNCTION     :  ug_vfb_pixels( s, e, data)
**       
**    PARAMETERS   
**       INPUT  : 
**          s				Starting x pixel
**          e				Ending x pixel
**				scan			Pixel values to write
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Color is written to output file
**    WARNINGS     : none
*********************************************************************/

ug_vfb_pixels(s, e, data)
int s, e;
Pixel data[];
{
	int i,j;

	uu_denter(UU_GTRC,(us,"ug_vfb_pixels(%d to %d)", s, e));

	for( i=s, j=0; i<=e; i++, j++ ) {
		scan[i].r = data[j].r;
		scan[i].g = data[j].g;
		scan[i].b = data[j].b;
	}

	uu_dexit;

}

/*********************************************************************
**    I_FUNCTION     :  ug_vfb_eol()
**       
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : This scanlines output is finished, next is begun
**    WARNINGS     : none
*********************************************************************/

ug_vfb_eol()
{
	int i, n;
	unsigned char r,g,b;
	static int eol = -1;				/* End of scanline mark */

	uu_denter(UU_GTRC,(us,"ug_vfb_eol"));

	r = scan[0].r;
	g = scan[0].g;
	b = scan[0].b;
	n = 0;

	for(i=1; i<maxxres; i++) {
		
		n++;
		if( scan[i].r != r ||
			 scan[i].g != g ||
			 scan[i].b != b ) {

			ug_vfb_run(n, r,g,b);
			r = scan[i].r;
			g = scan[i].g;
			b = scan[i].b;
			n = 0;
			
		}
	}

	/* Write the last group of pixels */
	ug_vfb_run(n+1, r,g,b);

	/* Write the end of scanline mark */
	uu_bwrite(vfb, &eol, sizeof(eol));
	uu_bflush(vfb);

	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_vfb_run()
**       
**    PARAMETERS   
**       INPUT  : 
**          n				Number of pixels to write
**				r, g, b		Color to write to file.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Color is written to output file.
**    WARNINGS     : none
*********************************************************************/

ug_vfb_run(n, r,g,b)
int n;								/* Number of color to write */
unsigned char r, g, b;			/* Rgb color triple */
{
	/* Structure used for writing to frame buffer */
	struct {	
		int n;
		unsigned char r, g, b;
	} color;

	uu_denter(UU_GTRC,(us,"ug_vfb_run(%d of '%d %d %d')",
		n, r, g, b));

	color.n = n;
	color.r = r;
	color.g = g;
	color.b = b;

	uu_dprint(UU_GTRC,(us,"writing %d of %d %d %d",
		n,color.r, color.g, color.b));

	uu_bwrite(vfb, &color, sizeof(color));

	uu_dexit;

}


/*********************************************************************
**    I_FUNCTION     :  ug_vfb_init(filename, xres, yres)
**       
**    PARAMETERS   
**       INPUT  : 
**          filename		Name of file to create.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Virtual frame buffer file is opened and initialized
**    WARNINGS     : none
*********************************************************************/

ug_vfb_init(filename, xres, yres)
char *filename;
int xres, yres;
{

	uu_denter(UU_GTRC,(us,"ug_vfb_init(%s, %d x %d)",filename, xres, yres));

	vfb = creat(filename, 0644);
	maxxres = xres;

	if( vfb < 0 ) {
		uu_dprint(-1,(us,"ERROR. ug_vfb_init unable to create '%s'",filename));
	}

	uu_dexit;
	return(vfb);

}
/* MILLS: new routine to close file.  Corrects problem on VMS where it can't
   find the file since it was still open! */
/*********************************************************************
**    I_FUNCTION     :  ug_vfb_close()
**       
**    PARAMETERS   
**       INPUT  : 
**          none		
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Close Virtual frame buffer file.
**    WARNINGS     : none
*********************************************************************/

int
ug_vfb_close()
	{
	int irtn;

	uu_denter(UU_GTRC,(us,"ug_vfb_close"));

	irtn = close (vfb);

	uu_dexit;
	return(irtn);

	}
