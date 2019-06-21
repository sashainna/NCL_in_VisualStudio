#include "usysdef.h"
#ifdef UU_OPENGL

/*********************************************************************
**    NAME         :  wsglprint.c
**
**    CONTAINS:
**            uw_glprint_screen()
**				uw_glsave_jpgfile()
**				uw_glsave_giffile()
**
**    MODULE NAME AND RELEASE LEVEL
**       wsglprint.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:08
*********************************************************************/

#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:keysym.h>
#include <decw$include:cursorfont.h>
#else
#if UU_COMP != UU_WIN2K 
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>
#endif
#endif
#include <stdio.h>
#if (UU_COMP == UU_WIN2K)
#include <io.h>
#include <fcntl.h>
#include <share.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "wsmf.h"
#include "wsgl.h"
#include "mdrwsize.h"
#include "mpocket.h"
#include "lipv.h"
#include "wsglfun.h"
#include "view1.h"

extern UG_wdt glwdt;
int print_fid, psid;

extern int UD_printipv;
extern int UV_current_sview;
extern int UL_nclipv_print;
	
extern int UW_print_screen, UW_print_back;

static unsigned char *Spixel = NULL;
static unsigned char *Sipvpixel = NULL;
static unsigned char *Srgbpixel = NULL;
#define PRINT_GIF	2
#define PRINT_JPEG	1

static int uw_print_type = 0;

float uw_gl_clrrgb[256][3];
static int color_table[16][3] =
	{
/*
.....white
*/
		255,  255, 255, 
/*
.....black
*/
		0, 0, 0,
/*   
.....DodgerBlue  
*/
		30, 144, 255,
/*
.....Red
*/
		255,  0,   0, 
/*
.....Green
*/
		0,    255, 0,
/*
....Magenta 
*/
		255,  0,   255, 
/*
.....Yellow
*/
		255,  255, 0, 
/*
.....Cyan
*/
		0,    255, 255,
/*
.....DarkGoldenrod
*/
		184,  134, 11, 
/*
.....Tan
*/
		210,  180, 140,
/*
.....LightBlue
*/
		173,  216, 230,
/*
.....SeaGreen1
*/
		84,   255, 159,
/*
.....Orange
*/
		255,  165, 0,
/*
.....Pink
*/
		255,  195, 203,
/*
.....Plum
*/
		221,  160, 221, 
/*
.....Gray
*/
		192,  192, 192,
	};
		

static int paper_wid, paper_hgt, paper_fit;
	
void uw_rdipv_pixels();
void uw_rgb_inx();
void uw_glsave_jpgfile();
void uw_glsave_giffile();

/********************************************************************
**    I_FUNCTION     :  uw_glcnv_rgb(array, len, flag, bcolor)
**              convert current pixel of the screen to NCL required pixel
**       when the background choose as "SAME", we don't do any conversion
**		but when back ground choose to "White", we need convert background
**			color to "White"
**
**    PARAMETERS
**       INPUT  :
**               int flag: 0 save to file
**                         1: print out to printer
**       OUTPUT :
**                      none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glcnv_rgb(array, len, flag, bcolor)
unsigned char* array;
int len, flag, bcolor;
{
	int i,red,blue,green;
	if ((flag==0) || (bcolor))
		return;
/*
.....solid color, need change background pixes to white color pixel
.....if the bankground select White for Printing
.....if not solid back ground, ignore this
*/
	if (UV_background.shader!=0)
		return;
	red = UV_background.bgrgb[0]*255 + 0.5;
	green = UV_background.bgrgb[1]*255 + 0.5;
	blue = UV_background.bgrgb[2]*255 + 0.5;
	for (i=0; i<len; i=i+3)
	{
		if ((((red-1)<array[i])&& (array[i]<(red+1)))
			&&((green-1)<array[i+1])&& (array[i+1]<(green+1))
			&&((blue-1)<array[i+2])&&(array[i+2]<(blue+1)))
		{
			array[i] = 255.0;
			array[i+1] = 255.0;
			array[i+2] = 255.0;
		}
	}
/*
	for (i=0; i<len; i=i+3)
	{
		if ((array[i]<10)&&(array[i+1]<10)&&(array[i+2]<10))
		{
			array[i] = 255.0;
			array[i+1] = 255.0;
			array[i+2] = 255.0;
		}
		else if ((array[i]>250)&&(array[i+1]>250)&&(array[i+2]>250))
		{
			array[i] = 1.0;
			array[i+1] = 1.0;
			array[i+2] = 1.0;
		}
	}
*/
}

void uw_glpsfunc(buf,len)
char  buf[];
int   len;
{
	write (print_fid, buf,len);
}

void uw_setrgb_tbl()
{
	int i, j, ind;
	int dred, dgreen, dblue;

	for ( i=0, ind=0; i<16; i++)
	{
		dred = color_table[i][0]/15;
		dgreen = color_table[i][1]/15; 
		dblue  = color_table[i][2]/15;
		for (j=0; j<16;j++,ind++)
		{

			if (i==1)
				uw_gl_clrrgb[ind][0] = 192 + (15-j)*(255-192)/15.0;
			else if (i==0)
				uw_gl_clrrgb[ind][0] = 0;
			else
				uw_gl_clrrgb[ind][0] = color_table[i][0]-j*dred;

			if (i==1)
				uw_gl_clrrgb[ind][1] = 192 + (15-j)*(255-192)/15.0;
			else if (i==0)
				uw_gl_clrrgb[ind][1] = 0;
			else
				uw_gl_clrrgb[ind][1] = color_table[i][1]-j*dgreen;
			if (i==1)
				uw_gl_clrrgb[ind][2] = 192 + (15-j)*(255-192)/15.0;
			else if (i==0)
				uw_gl_clrrgb[ind][2] = 0;
			else
				uw_gl_clrrgb[ind][2] = color_table[i][2]-j*dblue;
		}
	}
}
/********************************************************************
**    I_FUNCTION     :  uw_glprint_screen(flag, filename, ptcmd, size, ftype, sflag, fit, bcolor, pcenter)
**              Print current screen in the files or printer at
**       paper size=size (use ptcmd if printer)
**
**    PARAMETERS
**       INPUT  :
**               int flag: 0 save to file
**                         1: print out to printer
**       OUTPUT :
**                      none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_glprint_screen(flag, filename, ptcmd, paper_size, ftype, sflag, fit, bcolor, pcenter)
int flag, ftype, paper_size, sflag, fit, bcolor, pcenter;
char *filename, *ptcmd;
{
	int i, j, x0, y0; 
	int x, y;
	int size, scale_xin, scale_yin;
	float rate;
	char msg[80];
	int dred, dgreen, dblue, num;
	char command[200];
	int marg_x, marg_y;
	float size_x, size_y;
	Gbuffer dbuf,uw_glgetbuffer();

	paper_wid = glwdt.dspsize.raster.x;
	paper_hgt = glwdt.dspsize.raster.y;
/*
......use back buffer
*/
	UW_print_screen = 1;
	UW_print_back = bcolor;
	dbuf = uw_glgetbuffer();
	uw_gldrawbuffer(UG_BACK_BUFFER);

	uw_setrgb_tbl();
	if (flag==1)
	{
#if UU_COMP==UU_WIN2K
		if (UD_printipv==0)
			uz_repaint(0);
		else
			ipv_resize_window();
		uw_ntprint_scr(sflag, paper_size, fit, bcolor, pcenter);
		goto done;
#endif
	}
	else if (ftype==1)
	{
		uw_glsave_jpgfile(filename, paper_size, fit, bcolor);
		goto done;
	}
	else if (ftype==0)
	{
/*
......bitmap files
*/

#if UU_COMP==UU_WIN2K
		if (UD_printipv==0)
			uz_repaint(0);
		else
			ipv_resize_window();
		uw_ntsave_screenfile(filename, ftype, paper_size, fit, bcolor);
/*
//		uw_glsave_bmpfile(filename, paper_size, fit);
*/
#else
/* no bitmap file for XBP now
*/;
	;
#endif
		goto done;
	}
	else if (ftype==3)
	{
		uw_glsave_giffile(filename, paper_size, fit, 1);
		goto done;
	}
	if (UD_printipv)
		bcolor = 1;
/*
......following for Postscript file
*/
	if (UD_printipv)
	{
#if UU_COMP == UU_WIN2K
		x0 = 2*GetSystemMetrics(SM_CXFRAME);
		y0 = GetSystemMetrics(SM_CYCAPTION) + 2*GetSystemMetrics(SM_CYFRAME);
#else
		x0 = 0;
		y0 = 0;
#endif
		if (LW_nclipv!=LW_STANDALONE)
		{
			x = LW_ipv_size[0] - x0;
			y = LW_ipv_size[1] - y0;
		}
		else
		{
			x = glwdt.dspsize.raster.x;
			y = glwdt.dspsize.raster.y;
		}
		size = x*y + 1;
		Sipvpixel = (unsigned char *)uu_malloc(3*size*sizeof(unsigned char));
		if (UL_nclipv_print==0)
			uw_rdipv_pixels(x,y, Sipvpixel);
		else
		{
			ul_ipv_flush();
			ul_ipv_view_segs();
			uw_glset_context(UM_IPV_WINDOW,UU_TRUE);
			uw_glset_dirty_flag(UU_FALSE);
			glReadBuffer_d(GL_BACK);
			glReadPixels_d(0,0,x, y, GL_RGB, GL_UNSIGNED_BYTE, Sipvpixel);
			uw_glcnv_rgb(Sipvpixel, 3*size, 1, bcolor);
			uw_glset_dirty_flag(UU_TRUE);
			uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
		}
		uw_rgb_inx(Sipvpixel, &Spixel, x, y);
		um_set_pocket_graphics(UM_NCL_WINDOW);
	}
	else
	{
		x = glwdt.dspsize.raster.x;
		y = glwdt.dspsize.raster.y;
		size = x*y + 1;
		glPixelStorei_d(GL_PACK_ALIGNMENT, 1);
		glReadBuffer_d(GL_BACK);
		Srgbpixel = (unsigned char *)uu_malloc(3*size*sizeof(unsigned char));
		glReadPixels_d(0,0,x, y, GL_RGB, GL_UNSIGNED_BYTE, Srgbpixel);
		uw_glcnv_rgb(Srgbpixel, 3*x*y, 1, bcolor); 
		uw_rgb_inx(Srgbpixel, &Spixel, x, y);
	}	
/*
.....this 'create' function create problem on PC
//	print_fid = creat(filename, 0644);
*/
#if UU_COMP == UU_WIN2K
	_sopen_s(&print_fid, filename, _O_CREAT + _O_TRUNC + _O_RDWR, _SH_DENYNO,
		_S_IREAD|_S_IWRITE);
#else
	print_fid = creat(filename, 0644);
#endif
	uu_ttstartsave(psid, uw_glpsfunc, 0);
	uu_ttput(psid,"%!PS-Adobe-3.0\n",15);
/*
.....set color space as correspond to openGL colormap
*/      
	uu_ttput(psid, "[ /Indexed\n", 11);
	uu_ttput(psid, "       /DeviceRGB 255\n", 22);
	uu_ttput(psid, "<", 1);

	for (i=0; i<16; i++)
	{
		if ((bcolor==1)&&(i==0))
/*
.....use black as background color
*/
			i = 1;
		else if ((bcolor==1)&&(i==1))
			i = 0;
		dred = color_table[i][0]/15;
		dgreen = color_table[i][1]/15; 
		dblue  = color_table[i][2]/15;
		for (j=0; j<16; j++)
		{
			if (i==0)
				num = 192 + (15-j)*(255-192)/15.0;
			else
				num = color_table[i][0]-j*dred;
			sprintf(msg, "%X", num); 
			if (strlen(msg)==1)
			{
				msg[1] = msg[0];
				msg[0] = '0';
				msg[2] = '\0';
			}
			uu_ttput(psid,msg, strlen(msg));

			if (i==0)
				num = 192 + (15-j)*(255-192)/15.0;
			else
				num = color_table[i][1]-j*dgreen;
			sprintf(msg, "%X", num); 
			if (strlen(msg)==1)
			{
				msg[1] = msg[0];
				msg[0] = '0';
				msg[2] = '\0';
			}
			uu_ttput(psid,msg, strlen(msg));

			if (i==0)
				num = 192 + (15-j)*(255-192)/15.0;
			else
				num = color_table[i][2]-j*dblue;
			sprintf(msg, "%X", num); 
			if (strlen(msg)==1)
			{
				msg[1] = msg[0];
				msg[0] = '0';
				msg[2] = '\0';
			}
			uu_ttput(psid,msg, strlen(msg));

			uu_ttput(psid," ", 1);  
		}
/*
......black and write switch back
*/
		if ((bcolor==1)&&(i==0))
			i = 1;
		else if ((bcolor==1)&&(i==1))
			i = 0;
	}
	uu_ttput(psid, ">\n", 2);
	uu_ttput(psid, "] setcolorspace\n", 16);
/*
.....get paper actural horizontal and vertical
.....size and margin
*/
	marg_x = UM_drawing_size[paper_size+32][2]/2;
	marg_y = UM_drawing_size[paper_size+32][3]/2;
	size_x = UM_drawing_size[paper_size+32][0]/2.54;
	size_y = UM_drawing_size[paper_size+32][1]/2.54;
	if (fit==0)
	{
		marg_x = 10;
		marg_y = 10;
		size_x = (x+20)/72.0;
		size_y = (y+20)/72.0;
	}
/*
.....tranlate to leave margin
*/
	sprintf(msg, "%d %d translate\n", marg_x, marg_y);
	uu_ttput(psid, msg, strlen(msg));
/*
.....x, y should have same ratio
*/
	rate = (float)x/(float)y;
	scale_xin = 72 * size_x - marg_x;
	scale_yin = scale_xin / rate;
	if (scale_yin > 72 * size_y - marg_y)
	{
		scale_yin = 72*size_y - marg_y;
		scale_xin = scale_yin * rate;
	}
	if ((paper_size!=1) && (fit))   /* "AV" */
	{
		sprintf(msg, "%d 0 translate\n", scale_yin);
		uu_ttput(psid, msg, strlen(msg));
		uu_ttput(psid, "90 rotate\n", 10);
	}
	sprintf(msg, "%d %d scale\n", scale_xin, scale_yin);
	uu_ttput(psid, msg, strlen(msg));
	uu_ttput(psid, "<<\n", 3);
	uu_ttput(psid, "   /ImageType 1\n", 16);
	sprintf(msg, "   /Width %d\n" , x);
	uu_ttput(psid, msg, strlen(msg));
	sprintf(msg, "   /Height %d\n", y);
	uu_ttput(psid, msg, strlen(msg));
	uu_ttput(psid, "   /BitsPerComponent 8\n", 23);
	uu_ttput(psid, "   /Decode[0 255]\n", 18);
	uu_ttput(psid, "   /Interpolate true\n", 21 );
	sprintf(msg, "   /ImageMatrix[%d 0 0 %d 0 0]\n", x, y);
	uu_ttput(psid, msg, strlen(msg));
	uu_ttput(psid,"   /DataSource currentfile/ASCIIHexDecode filter\n",
							 49);           
	uu_ttput(psid,">>\n", 3);
	uu_ttput(psid,"image\n", 6);
	uu_ttput(psid,"\n", 1);
/*
.....image strings
*/
	for (i=0; i<x*y; i++)
	{
		sprintf(msg, "%X", Spixel[i]);
		if (strlen(msg)==1)
		{
			msg[1] = msg[0];
			msg[0] = '0';
			msg[2] = '\0';
		}
		uu_ttput(psid, msg, strlen(msg));
	}
	uu_ttput(psid,"\n ",1);   
	uu_ttput(psid,">\n ",2);   
	uu_ttput(psid,"showpage\n ",9);   
	uu_ttstopsave(psid);
	uu_ttclos(psid,2);
	if (Spixel)
	{
		uu_free(Spixel);
		Spixel = NULL;
	}
	if (Sipvpixel)
	{
		uu_free(Sipvpixel);
		Sipvpixel = NULL;
	}
	if (Srgbpixel)
	{
		uu_free(Srgbpixel);
		Srgbpixel = NULL;
	}
	if (flag==1)
	{
		sprintf(command, "%s %s", ptcmd, filename);
		ul_spawn(command,2);
	}
done:
/*
......reset buffer
*/
	uw_gldrawbuffer(dbuf);
	UW_print_screen = 0;
	
	if (UW_print_back==0)
	{
		if (UD_printipv==0)
			uz_repaint(0);
		else
			ipv_resize_window();
	}
	UW_print_back = 1;
}

/********************************************************************
**    I_FUNCTION     :  uw_get_prtsize(width, height)
**              Get size of screen need to be print
**       
**
**    PARAMETERS
**       INPUT  :
**               width, height: size of screen need to be print
**                                                                      1: print out to printer
**       OUTPUT :
**                      none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_get_prtsize(width, height)
int *width, *height;
{
	*width = paper_wid;
	*height = paper_hgt;
}

/********************************************************************
**    I_FUNCTION     :  uw_getncl_pixel(x, y)
**              Get pixel in a point
**       
**
**    PARAMETERS
**       INPUT  :
**           x, y: point  dimension
**                                                                      1: print out to printer
**       OUTPUT :
**                      none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_getncl_pixel(x, y)
int x, y;
{
	int wid, hgt;
#if UU_COMP==UU_WIN2K
	wid = paper_wid;
	hgt = paper_hgt;
#else
	wid = glwdt.dspsize.raster.x;
	hgt = glwdt.dspsize.raster.y;
	if (UD_printipv)
	{
		wid = LW_ipv_size[0];
		hgt = LW_ipv_size[1];
	}
#endif
/*
......if it is not shading, the pixed value only update to 15,
......Yurong  
*/
	if (uw_print_type!=2)
		return Spixel[wid*(hgt-y-1) + x];
	else
		return Spixel[wid*(y-1) + x];
}

/********************************************************************
**    I_FUNCTION     :  uw_glsave_jpgfile(filename, paper_size, fit)
**              Save current screen into a JPEG file
**       
**    PARAMETERS
**       INPUT  :
**           filename: JPEG file name
**			paper_size: print paper size
**			fit: if fit scrren into a paper size
**                                                                      1: print out to printer
**       OUTPUT :
**                      none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glsave_jpgfile(filename, paper_size, fit, bcolor)
int paper_size, fit, bcolor;
char *filename;
{
	int x, y, x0, y0;
	int size;

	uw_print_type = PRINT_JPEG;
	paper_fit = 0;
/*
.....scale the back buffer and use back buffer for scan pixel
*/
	if (UD_printipv==0)
		uz_repaint(0);

	x = glwdt.dspsize.raster.x;
	y = glwdt.dspsize.raster.y;
	size = x*y + 1;

	if (UD_printipv)
	{
#if UU_COMP == UU_WIN2K
		x0 = 2*GetSystemMetrics(SM_CXFRAME);
		y0 = GetSystemMetrics(SM_CYCAPTION) + 2*GetSystemMetrics(SM_CYFRAME);
#else
		x0 = 0;
		y0 = 0;
#endif
		if (LW_nclipv!=LW_STANDALONE)
		{
			x = LW_ipv_size[0] - x0;
			y = LW_ipv_size[1] - y0;
		}
		size = x*y + 1;
		Sipvpixel = (unsigned char *)uu_malloc(3*size*sizeof(unsigned char));
		if (UL_nclipv_print==0)
			uw_rdipv_pixels(x,y, Sipvpixel);
		else
		{
			ul_ipv_flush();
			ul_ipv_view_segs();
			uw_glset_context(UM_IPV_WINDOW,UU_TRUE);
			uw_glset_dirty_flag(UU_FALSE);
			glReadBuffer_d(GL_BACK);
			glReadPixels_d(0,0,x, y, GL_RGB, GL_UNSIGNED_BYTE, Sipvpixel);
			uw_glcnv_rgb(Sipvpixel, 3*size, 1, bcolor);
			uw_glset_dirty_flag(UU_TRUE);
			uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
		}
		um_set_pocket_graphics(UM_NCL_WINDOW);
	}
	else
	{
		glPixelStorei_d(GL_PACK_ALIGNMENT, 1);
		glReadBuffer_d(GL_BACK);
		Srgbpixel = (unsigned char *)uu_malloc(3*size*sizeof(unsigned char));
		glReadPixels_d(0,0,x, y, GL_RGB, GL_UNSIGNED_BYTE, Srgbpixel);
	}	
	paper_wid = x;
	paper_hgt = y;
	if (UD_printipv)
	{
		uw_cyscrn_pixelrgb(&Sipvpixel, paper_size, &paper_wid, &paper_hgt, fit);
	}
	else
	{
		uw_cyscrn_pixelrgb(&Srgbpixel, paper_size, &paper_wid, &paper_hgt, fit);
	}
	x = paper_wid;
	y = paper_hgt;
	uw_create_jpg(filename, bcolor);
	if (Spixel)
	{
		uu_free(Spixel);
		Spixel = NULL;
	}
	if (Sipvpixel)
	{
		uu_free(Sipvpixel);
		Sipvpixel = NULL;
	}
	if (Srgbpixel)
	{
		uu_free(Srgbpixel);
		Srgbpixel = NULL;
	}
	uw_print_type = 0;
}

/********************************************************************
**    I_FUNCTION     :  uw_glsave_giffile(filename, paper_size, fit)
**              Save current screen into a GIF file
**       
**    PARAMETERS
**       INPUT  :
**           filename: GIF file name
**			paper_size: print paper size
**			fit: if fit scrren into a paper size
**                                                                      1: print out to printer
**       OUTPUT :
**                      none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glsave_giffile(filename, paper_size, fit, bcolor)
int paper_size, fit, bcolor;
char *filename;
{
	int i, j, rnum, gnum, bnum, ind; 
	int x, y;
	int size,x0,y0;
	int dred, dgreen, dblue;

	paper_fit = fit;
	uw_print_type = PRINT_GIF;
/*
.....scale the back buffer and use back buffer for scan pixel
*/
	if (UD_printipv==0)
		uz_repaint(0);
	else
		ipv_resize_window();

	x = glwdt.dspsize.raster.x;
	y = glwdt.dspsize.raster.y;
	size = x*y + 1;
/*
#if UU_COMP==UU_WIN2K
	if (fit==0)
	{
#endif
*/
		if (UD_printipv)
		{
#if UU_COMP == UU_WIN2K
			x0 = 2*GetSystemMetrics(SM_CXFRAME);
			y0 = GetSystemMetrics(SM_CYCAPTION) + 2*GetSystemMetrics(SM_CYFRAME);
#else
			x0 = 0;
			y0 = 0;
#endif
			if (LW_nclipv!=LW_STANDALONE)
			{
				x = LW_ipv_size[0] - x0;
				y = LW_ipv_size[1] - y0;
			}
			size = x*y + 1;
			Sipvpixel = (unsigned char *)uu_malloc(3*size*sizeof(unsigned char));
			if (UL_nclipv_print==0)
				uw_rdipv_pixels(x,y, Sipvpixel);
			else
			{
				ul_ipv_flush();
				ul_ipv_view_segs();
				uw_glset_context(UM_IPV_WINDOW,UU_TRUE);
				uw_glset_dirty_flag(UU_FALSE);
				glReadBuffer_d(GL_BACK);
				glReadPixels_d(0,0,x, y, GL_RGB, GL_UNSIGNED_BYTE, Sipvpixel);
				uw_glcnv_rgb(Sipvpixel, 3*size, 1, bcolor);
				uw_glset_dirty_flag(UU_TRUE);
				uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
			}
			uw_rgb_inx(Sipvpixel, &Spixel, x, y);
			um_set_pocket_graphics(UM_NCL_WINDOW);
		}
		else
		{
			glPixelStorei_d(GL_PACK_ALIGNMENT, 1);
			glReadBuffer_d(GL_BACK);
			Srgbpixel = (unsigned char *)uu_malloc(3*size*sizeof(unsigned char));
			glReadPixels_d(0,0,x, y, GL_RGB, GL_UNSIGNED_BYTE, Srgbpixel);
			uw_glcnv_rgb(Srgbpixel, 3*size, 1, bcolor);
			uw_rgb_inx(Srgbpixel, &Spixel, x, y);
		}	
		paper_wid = x;
		paper_hgt = y;
/*
#if UU_COMP==UU_WIN2K
	}
	else
	{
*/
		if (UD_printipv)
		{
			uw_cyscrn_pixelrgb(&Sipvpixel, paper_size, &paper_wid, &paper_hgt, fit);
			uw_rgb_inx(Sipvpixel, &Spixel, paper_wid, paper_hgt);
		}
		else
		{
			uw_cyscrn_pixelrgb(&Srgbpixel, paper_size, &paper_wid, &paper_hgt, fit);
			uw_rgb_inx(Srgbpixel, &Spixel, paper_wid, paper_hgt);
		}
		x = paper_wid;
		y = paper_hgt;
/*
	}
#endif
*/
/* 
......create and set up the GIF-file 
*/
/*
.....set color space as correspond to openGL colormap
*/      
/* 
......create and set up the GIF-file 
*/
	uw__gifcreate(filename, x, y, 256, 8);
	for ( i=0, ind=0; i<16; i++)
	{
/*
......black and write switch
*/
		if ((bcolor==1)&&(i==0))
			i = 1;
		else if ((bcolor==1)&&(i==1))
			i = 0;
		dred = color_table[i][0]/15;
		dgreen = color_table[i][1]/15; 
		dblue  = color_table[i][2]/15;
		for (j=0; j<16;j++,ind++)
		{
			rnum = color_table[i][0]-j*dred;
			gnum = color_table[i][1]-j*dgreen;
			bnum = color_table[i][2]-j*dblue;

			if (i==0)
			{
				rnum = 192 + (15-j)*(255-192)/15.0;
				gnum = 192 + (15-j)*(255-192)/15.0; 
				bnum  = 192 + (15-j)*(255-192)/15.0;
			}
			uw_gif_setcolor(ind, rnum, gnum, bnum);
		}
/*
......black and write switch back
*/
		if ((bcolor==1)&&(i==0))
			i = 1;
		else if ((bcolor==1)&&(i==1))
			i = 0;
	}
    uw_gif_cmpimg(1, 1, -1, -1, uw_getncl_pixel);
    uw_gifclose();

	if (Spixel)
	{
		uu_free(Spixel);
		Spixel = NULL;
	}
	if (Sipvpixel)
	{
		uu_free(Sipvpixel);
		Sipvpixel = NULL;
	}
	if (Srgbpixel)
	{
		uu_free(Srgbpixel);
		Srgbpixel = NULL;
	}
	uw_print_type = 0;
}


/********************************************************************
**    I_FUNCTION     :  uw_rdipv_pixels(int x, int y, unsigned char *ipvpixel)
**              Read the current IPV window's Pixels
**       
**    PARAMETERS
**       INPUT  :
**           x, y: width and height of IPV windows to read
**    
**       OUTPUT :
**          ipvpixel: Pixels read from IPV window
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_rdipv_pixels(x, y, ipvpixel)
int x,y;
unsigned char *ipvpixel;
{
	int i,j,inc;
	LtNat32 i1;
	LtImage image;

	image = LiImageCreate();
	LiImageSetWidth(image,x);
	LiImageSetHeight(image,y);
	LiImageSetDepth(image,24);
	LiImageAllocateData(image);
	LiDrawableCopyToImage(LW_drawable,image);
	inc = 0;
	for (j=y-1;j>=0;j--)
	{
		for (i=0;i<x;i++)
		{
			i1 = LiImageGetPixel(image,i,j);
			ipvpixel[inc++] = (unsigned char)(i1 & 0xff);
			ipvpixel[inc++] = (unsigned char)((i1 & 0xff00) >> 8);
			ipvpixel[inc++] = (unsigned char)((i1 & 0xff0000) >> 16);
		}
	}
	LiImageDestroy(image);
	ul_ipv_set_context();
}
	
/********************************************************************
**    I_FUNCTION     :  uw_glget_pixel (hgt, wid, row, col, red, green, blue)
**              Get the current IPV window's Pixel in one particular position
**       
**    PARAMETERS
**       INPUT  :
**           wid, hgt: width and height of IPV windows to read
**			row, col:  position of the pixel to be read
**       OUTPUT :
**          red, green, blue: RGB of the pixel value
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glget_pixel (hgt, wid, row, col, red, green, blue)
unsigned int row, col;
unsigned char *red, *green, *blue;
{
	if (UD_printipv==0)
	{
		*red = Srgbpixel[3*col+ (paper_hgt-row-1)*wid*3];
		*green = Srgbpixel[3*col+ 1 + (paper_hgt-row-1)*wid*3];
		*blue = Srgbpixel[3*col+ 2 + (paper_hgt-row-1)*wid*3];
	}
	else
	{
		*red = Sipvpixel[3*col+ (paper_hgt-row-1)*wid*3];
		*green = Sipvpixel[3*col+ 1 + (paper_hgt-row-1)*wid*3];
		*blue = Sipvpixel[3*col+ 2 + (paper_hgt-row-1)*wid*3];
	}
}

/*************************************************************************
**
**  Function:  uw_getnear_indx(float red, float green, float blue, int *pixel)
**    convert True color pixels to neariest index color
**
**  INPUT: red, green, blue: RGB color
**
**   OUTPUT : pixel: nearest index
**   RETURN:   none
**
**
*************************************************************************/
void uw_getnear_indx(red, green, blue, pixel)
float red, green, blue;
unsigned char *pixel;
{
	float dx, dy, dz, dr, tmp;
	int i;
	dr = 100000000;
	for (i=0; i<uw_gl.maxgcolors; i++)
	{
		dx = red - uw_gl_clrrgb[i][0];
		dy = green - uw_gl_clrrgb[i][1];
		dz = blue - uw_gl_clrrgb[i][2];
		tmp = dx*dx + dy*dy + dz*dz;
		if (tmp<dr)
		{
			*pixel = (unsigned char)i;
			dr = tmp;
		}
	}
}
/*************************************************************************
**
**  Function:  uw_rgb_inx(float *ipixel, unsigned char **opixel, int x, int y)
**    convert True color pixels to neariest index color
**
**  INPUT:  ipixel: pixel array having true color
**       x, y:  graphic size
**
**   OUTPUT : opixel: out put pixels with index color
**   RETURN:   none
**
**
*************************************************************************/
void uw_rgb_inx(ipixel, opixel, x, y)
unsigned char *ipixel;
unsigned char **opixel;
int x, y;
{
	unsigned char pixel;
	unsigned char * pixels;
	BYTE red, green, blue;
	int i, j, inc = 0;
  
	pixels = (unsigned char *)uu_malloc((x*y+1)*sizeof(unsigned char));

	for (i=0; i<y; i++)
	{
		for (j=0;j<x; j=j++)
		{
			if (UD_printipv)
			{
				red = ipixel[3*j+ (y-i-1)*x*3];
				green = ipixel[3*j+ 1 + (y-i-1)*x*3];
				blue = ipixel[3*j+ 2 + (y-i-1)*x*3];
			}
			else
			{
				red = ipixel[3*j+ i*x*3];
				green = ipixel[3*j+ 1 + i*x*3];
				blue = ipixel[3*j+ 2 + i*x*3];
			}
			uw_getnear_indx((float)red, (float)green, (float)blue, &pixel);
			pixels[inc] = pixel;
			inc++;
		}
	}
	*opixel = pixels;
}
uw_get_bkcolor(red, green, blue)
int *red, *green, *blue;
{
	*red = UV_background.bgrgb[0]*255 + 0.5;
	*green = UV_background.bgrgb[1]*255 + 0.5;
	*blue = UV_background.bgrgb[2]*255 + 0.5;
}
/********************************************************************
**    I_FUNCTION     :  uw_glsave_bmpfile(filename, paper_size, fit)
**              Save current drawing back buffer into a bitmap file
**       
**    PARAMETERS
**       INPUT  :
**           filename: bitmap file name
**			paper_size: print paper size
**			fit: if fit scrren into a paper size
**                                                                      1: print out to printer
**       OUTPUT :
**                      none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glsave_bmpfile(filename, paper_size, fit)
int paper_size, fit;
char *filename;
{
	int x, y, x0, y0;
	int size;

	paper_fit = 0;
/*
.....scale the back buffer and use back buffer for scan pixel
*/
	if (UD_printipv==0)
		uz_repaint(0);

	x = glwdt.dspsize.raster.x;
	y = glwdt.dspsize.raster.y;
	size = x*y + 1;

#if UU_COMP==UU_WIN2K
	if (fit==0)
	{
#endif
		if (UD_printipv)
		{
#if UU_COMP == UU_WIN2K
			x0 = 2*GetSystemMetrics(SM_CXFRAME);
			y0 = GetSystemMetrics(SM_CYCAPTION) + 2*GetSystemMetrics(SM_CYFRAME);
#else
			x0 = 0;
			y0 = 0;
#endif
			if (LW_nclipv!=LW_STANDALONE)
			{
				x = LW_ipv_size[0] - x0;
				y = LW_ipv_size[1] - y0;
			}
			size = x*y + 1;
			Sipvpixel = (unsigned char *)uu_malloc(3*size*sizeof(unsigned char));
			if (UL_nclipv_print==0)
				uw_rdipv_pixels(x,y, Sipvpixel);
			else
			{
				ul_ipv_flush();
				ul_ipv_view_segs();
				uw_glset_context(UM_IPV_WINDOW,UU_TRUE);
				uw_glset_dirty_flag(UU_FALSE);
				glReadBuffer_d(GL_BACK);
				glReadPixels_d(0,0,x, y, GL_RGB, GL_UNSIGNED_BYTE, Sipvpixel);
				uw_glcnv_rgb(Sipvpixel, 3*size, 1, 1);
				uw_glset_dirty_flag(UU_TRUE);
				uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
			}
			um_set_pocket_graphics(UM_NCL_WINDOW);
		}
		else
		{
			glPixelStorei_d(GL_PACK_ALIGNMENT, 1);
			glReadBuffer_d(GL_BACK);
			Srgbpixel = (unsigned char *)uu_malloc(3*size*sizeof(unsigned char));
			glReadPixels_d(0,0,x, y, GL_RGB, GL_UNSIGNED_BYTE, Srgbpixel);
		}	
		paper_wid = x;
		paper_hgt = y;
#if UU_COMP==UU_WIN2K
	}
	else
	{
		uw_ntsave_screenfile(filename, 0, paper_size, fit, 1);
		return;
	}
#endif
	if (UD_printipv)
		uw_ntsave_bitmap(Sipvpixel, paper_wid, paper_hgt, filename);
	else
		uw_ntsave_bitmap(Srgbpixel, paper_wid, paper_hgt, filename);
	if (Sipvpixel)
	{
		uu_free(Sipvpixel);
		Sipvpixel = NULL;
	}
	if (Srgbpixel)
	{
		uu_free(Srgbpixel);
		Srgbpixel = NULL;
	}
}

#endif

				
				
