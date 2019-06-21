#include "zsysdep.h"

#ifdef UU_OPENGL

/********************************************************************* 
**  NAME:  wsglinit.c
**
**              CONTAINS:
**       uw_glinit()
**       uw_glglxinit()
**       uw_glinit_visual()
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**          wsglinit.c , 25.6
**    DATE AND TIME OF LAST  MODIFICATION
**          07/05/16 , 13:02:15
**
*********************************************************************/
#if UU_COMP != UU_WIN2K
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#endif

#include "udebug.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "go1.h"
#include "ws.h"
#include "lcom.h"
#include "lipv.h"
#if UU_COMP != UU_WIN2K
#include "wsxw.h"
#include "wsmf.h"
Cursor pick_cursor,menu_cursor,loc_cursor,strok_cursor,blank_cursor,
	pan_cursor,rotate_cursor,zoom_cursor,text_cursor,mouse_cursor;
#endif
#include "driver.h"
#include "xenv1.h"
#include "nclver.h"
#define WSGL_FIRST
#include "wsgl.h"
#undef WSGL_FIRST
#include "wsglfun.h"


typedef struct
{
	Gint op; Gws wsid; 
	Gchar *conn;    /* connection-filename to access device */
} Swid;
/*
.....Global variable definitions
*/
int uw_glhicolor, uw_glvrfcolor;
int UW_project_depth = 2;
extern int DEBUG_SHOW_TESS;
extern int DEBUG_SHOW_TESS_UV;
/*
.....Workstation Description Table
*/
static Gint ltypes[NLINETYPES]={1,5,6,7,21};    /* Line types */
static Gint mktypes[NMARKTYPES]={1,2,3,4,5,7};  /* Marker types */
static Gtxfp fonprecs[2]={1,UG_STRING,1,UG_STROKE};/* avail font/prec pairs */
static Gpet choicepets[NUMPETS]={3,5,21,22,24,25}; /* choice prompt & echo types */
static Gchar defchostr[2][2]={" "," "}; /* default choice strings */
static Gchar *defchoptr[2]={&defchostr[0][0],&defchostr[1][0]};
static Gdefchoice defcho[NCHDEV] = {UW_DEFCHO}; /* default choice data */
static Gpet locpets[NLOCPETS]={1,3,4,5,21,40,41,42,43,44,45,46};        /* locator prompt & echo types */
static Gdefloc defloc[3]=                       /* default locator data */
{
	0.,0.,NLOCPETS,
	locpets,0.,0.,1.,1.,{UG_PF_POLYLINE},
	0.,0.,NLOCPETS,locpets,0.,0.,1.,1.,{UG_PF_POLYLINE},
	0.,0.,NLOCPETS,locpets,0.,0.,1.,1.,{UG_PF_POLYLINE}
};
static Gpet pickpets[NPICKPETS]={1};    /* pick prompt and echo types */

static Gdefpick defpick[1]=             /* default pick data */
{
	NPICKPETS,pickpets,0.,0.,1.,1.," "
};
static Gpet stringpets[6]={1,22,22,22,22,22};   /* string echotypes */

static Gdefstring defstring[6]=         /* default string data */
{
				{UW_DEFSTR},
				{UW_DEFSTR},
				{UW_DEFSTR},
				{UW_DEFSTR},
				{UW_DEFSTR},
				{UW_DEFSTR}
};
static Gpet strokepets[NSTROKEPETS]={1};        /* default stroke echotypes */
static Gdefstroke defstroke[1]=                 /* default stroke data */
{
	1,NSTROKEPETS,
	strokepets,0.,0.,1.,1.
};
static Gpet valpet1[1]={1};             /* valuator 1 echotype */
static Gpet valpet21[1]={21};           /* valuator 21 echotype */
static Gdefval defval[2]=               /* default val data */
{
	0.,1,valpet1,0.,0.,1.,1.,
	100.,0.," ",0.,1,valpet21,0.,0.,1.,1.,100.,0.," "
};

static Glighttbl litetbl[16] =
{
	{ UG_AMBIENT, 0.,0.,0.,0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. },
	{ UG_AMBIENT, 0.,0.,0.,0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. },
	{ UG_AMBIENT, 0.,0.,0.,0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. },
	{ UG_AMBIENT, 0.,0.,0.,0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. },
	{ UG_AMBIENT, 0.,0.,0.,0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. }
};

static Gcobundl colrbundl[256]=     /* color bundle table (This color table
		  is never used.  We use the one in DAS
		  instead */
{
   .5,.5,.6,   /* 0:, bkground, light blue */
   1.,1.,1.,   /* 1:, white */
   0.,0.,0.,   /* 2: black */
   0.,1.,0.,   /* 3: green */
   0.,0.,1.,   /* 4: blue */
   0.,1.,1.,   /* 5: cyan */
   1.,0.,0.,   /* 6: red */
   1.,1.,0.,   /* 7: yellow */
   1.,0.,.83   /* 8: magenta */
};


static UG_outwdt outwdt=        /* output wdt stuff */
{
	UG_RASTER,              /* workstation type */
	UG_IMM,UG_IMM,          /* structure content, view rep mod */
	{                       /* Gmodws - mod of workstation attributes */
		UG_IRG,UG_IRG,UG_IRG,UG_IRG,UG_IRG,UG_IRG
	},
	UG_BNIG,                /* default deferral mode */
	UG_NIVE,                /* default modification mode */
	{                       /* polyline facilities */
		NLINETYPES,ltypes,      /* no and list of line types */
		1,                      /* one line width */
		.01,.01,.01,            /* nom, min, max width */
		0                       /* number predefined polyline bundles */
	},
	NULL,                   /* array of predefined polyline bundles */
	{                       /* polymarker facilities */
		NMARKTYPES,mktypes,     /* num/list of marker types */
		1,                      /* number of marker sizes */
		.01,.01,.01,            /* nom, min, max marker size */
		0                       /* no predefined polymarker bundles */
	},
	NULL,                   /* array of predefined polymarker bundles */
	{                       /* text facilities */
		2,                      /* number of font/precision pairs */
/*              (Gtxfp (*)[])fonprecs,*/  /* list of font/prec pairs */
		fonprecs,       /* list of font/prec pairs */
		1,.01,.01,.01,          /* number, nom, min,max char ht */
		1,1.,1.,1.,             /* number,nom,min,max expansn factr */
		0                       /* no. predefined text bundles */
	},
	NULL,                   /* pointer to predefined text bundles */
	{                       /* fill area facilities */
		0,NULL,         /* no. and list of of interior styles */
		0,NULL,         /* number and list of hatch styles */
		0                /* number predefined fill area bundles */
	},
	NULL,                   /* pointer to predefined fill area bundles */
	{0,0},                  /* pattern facilities */
	NULL,                   /* ptr to predefined pattern bundles */
	{NCOLORS,UG_COLOUR,9},  /* #colors,color available,#bundles  */
	colrbundl,              /* pointer to color predefined bundles */
	{0,NULL},               /* GDP facilities */
	NULL,                   /* pointer to array of GDPs */
	0,0,0,0,0,0,0,          /* maximum numbers */
	UG_IRG,UG_IRG,UG_IRG,    /*dyn mod of seg contnt,seg pri,unpost*/
	UG_IRG,UG_IRG,          /* dyn mod of add, delete prims to seg */
	16, litetbl
};                              /* end of outwdt */

static UG_inwdt inwdt=          /* input WDT */
{
	3,defloc,       /* number and array of LOC input devices */
	1,defstroke,    /* number and array of STROKE devices */
	2,defval,       /* number and array of VALUATOR devices */
	NCHDEV,defcho,  /* number and array of CHOICE devices */
	1,defpick,      /* number and array of PICK devices */
	6,defstring,    /* number and array of STRING devices */
};                              /* end of inwdt */

UG_wdt glwdt=           /* main WDT */
{
	"gl",UG_OUTIN,  /* workstation type and category */
	UG_DC_OTHER,                    /* device units */
	1.,0.74698,                     /* default display size in dc */
	1075,803,       /* display size in raster */
	1,1,            /* number rows,cols of text */
	20,             /* no definable viewing table indices */
	1,              /* 1 = need NDC segments, 0 = don't */
	1,      /* 1 = UG_DSAVESCRN works right on Device */
	1,      /* # of bit planes */
	&outwdt,        /* output wdt */
	&inwdt          /* input wdt */
};                              /* end of main WDT */
extern UG_wdt wdt;
static char *glfont;

#if UU_COMP != UU_WIN2K
XVisualInfo uw_glvinf; 
GLXContext glx_context;
/*
.....added by Yurong
.....8/27/97
*/
extern UWS_MF uw_mf;
extern float uw_form_scalex;
extern float uw_form_scaley;
#else
float uw_form_scalex;
float uw_form_scaley;
#endif

int XBorder[2],YBorder[2];
int Border_stat;
extern int NCL_swap_changed;
extern int UL_nclipv_print;
void uw_glinit_visual();
/*********************************************************************
**    I_FUNCTION :  uw_glinit(prms,reply) ----------  UG_DOPENWS
**       open workstation
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uw_glinit(prms,reply)
Swid *prms;
UG_rwsinit *reply;      /* struct {Gstatus stat;UG_wdt *wdtptr;} *reply */
{
/*
.....Variable definitions
*/
#ifdef VMS
	static char *font14="-DEC-Terminal-Medium-R-Normal--14-140-75-75-C-8-ISO8859-1";
#else
	static char *font14 = "7x13";
#endif
	int i,maxx,maxy;
	static int onoff = 0;  /* turn on/off X Synchronization */
	char *vptr,*p,*ux_getenv();
	double atof();
#if UU_COMP!=UU_WIN2K
	Arg args[20];
#endif
	int n;
	char buf[80];
/*
.....Initialize MOTIF
*/
	i = 0;
	n = 0;
#if UU_COMP!=UU_WIN2K
	XtSetArg(args[n],XmNmappedWhenManaged,False); n++;
	uw_mf.parent = XtAppInitialize (&uw_mf.application,"NCL",
		NULL,0,&i,NULL,NULL,args,n);
	if (uw_mf.parent == NULL)
	{
		printf("Display %s is not available\n",XDisplayName(""));
		exit (1);
	}
	uw_xw.disp = XtDisplay(uw_mf.parent);
	uw_xw.screen_no = DefaultScreen(uw_xw.disp);
	p = ux_getenv("ONOFF",UX_NPRTERRS);
	if (p != NULL && strcmp(p,"ON") == 0) onoff = 1;
	XSynchronize(uw_xw.disp, onoff);
/*
.....Get the maximum size of the display
*/
	maxx = DisplayWidth(uw_xw.disp, uw_xw.screen_no);
	maxy = DisplayHeight(uw_xw.disp, uw_xw.screen_no);
#else
/*
.....initial display list
*/
	uw_gltext_base = 0;
	gl_line_style = 0;
/*
.....Get the maximum size of the display
*/
	maxx = GetSystemMetrics(SM_CXSCREEN);
	maxy = GetSystemMetrics(SM_CYSCREEN);
#endif
/*
.....Let the user override the window size
*/

	p = ux_getenv("UU_WINX", UX_PRTERRS);
	if (p != 0) maxx = atoi(p);
	p = ux_getenv("UU_WINY", UX_PRTERRS);
	if (p != 0) maxy = atoi(p);
	p = ux_getenv("UU_XYRATIO",UX_PRTERRS);
/*
.....Make this the maximum size of screen
*/
	uw_gl.dev_xmax = maxx;
	uw_gl.dev_ymax = maxy;
#if UU_COMP != UU_WIN2K
   uw_xw.dev_xmax = maxx;
   uw_xw.dev_ymax = maxy;
#endif
/*
.....added for form scale factor
.....Yurong 11/1/00
*/
	uw_form_scalex = 1.0;
	uw_form_scaley = 1.0;
	p = ux_getenv("UW_FORM_SCALEX", UX_PRTERRS);
	if (p != 0) uw_form_scalex = (float) atof(p);
	p = ux_getenv("UW_FORM_SCALEY", UX_PRTERRS);
	if (p != 0) uw_form_scaley = (float) atof(p);
#if UU_COMP!=UU_WIN2K
	uw_form_scalex = uw_form_scalex * 2.0;
	uw_form_scaley = uw_form_scaley * 2.0;
#else
	uw_form_scalex = (float) (uw_form_scalex * 1.0);
	uw_form_scaley = (float) (uw_form_scaley * 1.0);
#endif	
/*
......set diff border size for diff machine
......in Motif.
......12/10/97 Yurong
*/
	XBorder[0] = 7; YBorder[0] = 7; 
	XBorder[0] = 7; YBorder[0] = 32; 
	p = ux_getenv("UU_XBORDER", UX_PRTERRS);
	if (p != 0) XBorder[0] = atoi(p);
	p = ux_getenv("UU_YBORDER", UX_PRTERRS);
	if (p != 0) YBorder[0] = atoi(p);
	p = ux_getenv("UU_MXBORDER", UX_PRTERRS);
	if (p != 0) XBorder[1] = atoi(p);
	p = ux_getenv("UU_MYBORDER", UX_PRTERRS);
	if (p != 0) YBorder[1] = atoi(p);
/*
.....add Border Status
.....12/15/97 Yurong
*/
	Border_stat = 1;
	glwdt.dspsize.raster.x = maxx;
	glwdt.dspsize.raster.y = maxy;
	uw_gl.xpixels = maxx;
	uw_gl.ypixels = maxy;
	uw_gl.xndc = 1.0;
	uw_gl.yndc = (float)maxx/(float)maxy;
	p = ux_getenv("UU_BORDER_STAT",UX_PRTERRS);
	if (p != NULL && strcmp(p,"NO") == 0) Border_stat = 0;
/*
.....Let the user override the default font
*/
	p = ux_getenv("UU_FONT",UX_PRTERRS);
	glfont = font14;
	if (p != 0) glfont = p;
/*
... aak 24-sep-1998: debug features: 
... "setenv TESS 1" enables display of a surface tessellation 
...                 claculated during shading
... "setenv TESS_UV 1" enables display of a surface tessellation 
...                    on uv-domain
*/
   if ((vptr = ux_getenv("TESS")))
   {
      strcpy(buf,vptr);
      if (strcmp(buf,"1") == 0) DEBUG_SHOW_TESS = 1;
		else DEBUG_SHOW_TESS = 0;
   }
   if ((vptr = ux_getenv("TESS_UV")))
   {
      strcpy(buf,vptr);
      if (strcmp(buf,"1") == 0) DEBUG_SHOW_TESS_UV = 1;
		else DEBUG_SHOW_TESS_UV = 0;
   }
   UL_nclipv_print = 0;
   if ((vptr = ux_getenv("UL_NCLIPV_PRINT")))
   {
		strcpy(buf,vptr);
		if (strcmp(buf,"1") == 0) UL_nclipv_print = 1;
		else UL_nclipv_print = 0;
   }
/*
...Set up visual info, colormap and create Context
*/
#if UU_COMP==UU_WIN2K
/*
.....don't initial the WinNT yet, we will initial them
.....when initial NCLView Window
//	uw_ntinit_view();
//	UD_cmap_changed = 0;
*/
   ;
#else
	uw_glinit_visual();
#endif
/*
.....Initialize some default settings
*/
	uw_gl.linetype = 1;
/*
.....Initialize current choice devices
*/
	for (i=1; i<NCHDEV; i++) defcho[i]=defcho[0];
/*
.....Set up return parameters
*/
	(*reply).stat=UG_OK;                    /* return status OK */
	(*reply).wdtptr= &glwdt;                /* return addr of ws desc table */
	uw_gl.wid = (*prms).wsid;       /* workstation id */
	for (i=0;i<UM_POCKET_COLORS;i++)
	{
		LW_material[i] = 0;
	}
/*
.....End
*/
	uu_dexit;
	return(UU_SUCCESS);
}

/**********************************************************************
**    I_FUNCTION :  uw_glglxinit()
**              Initialize viewing parameters and openGL graphic objects.
**    PARAMETERS   
**       INPUT  : size
**                     graphic area size
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glglxinit(size)
int size[2];
{
	int i;

	GLint viewport[4];
#if UU_COMP!=UU_WIN2K
	XFontStruct *fontInfo;
	Font id;
	unsigned int first, last;
#endif
	int clip[4];

	glGetIntegerv_d(GL_MAX_PROJECTION_STACK_DEPTH, &UW_project_depth);
/*
.....when NCL_swap_changed (NCL/IPV swap), we need recreate GL list
.....when call wglMakeCurrent to different window
.....(IPV window used as graphic window), 
.....the original list beloing to old context
.....(which is currect when create list).
.....It will not exist in this context now. so we need to recreate it.
.....But we don't need recreate all those lists when we switch back
.....We have have these, all we need to do is reset context to original one
.....Also the size is the wrong size because this called in
.....OnCreate function which the final size haven't set up, so skip it
.....Yurong
*/
	if (NCL_swap_changed) goto line_style;
/*
..... set uw_gl.xform, the normalization xform, to identity 
.....move the definition before we set up everything
.....Yurong 9/15/97
*/
	glwdt.dspsize.raster.x = size[0];
	glwdt.dspsize.raster.y = size[1];
	glwdt.dspsize.device.x = 1;
	glwdt.dspsize.device.y = (float)size[1]/(float)size[0];
	uw_gl.xpixels = size[0];
	uw_gl.ypixels = size[1];

/* set uw_gl.wsxform, the workstation transformation, to */
	if(size[0]>size[1])
		uw_gl.wsxform.sf=(GLdouble)size[0];
	else
		uw_gl.wsxform.sf=(GLdouble)size[1];
	uw_gl.wsxform.dx=0;
	uw_gl.wsxform.dy=0;

/* initialize the clipping cube */
	uw_glclip.llf.x = 0.0;
	uw_glclip.llf.y = 0.0;
	uw_glclip.llf.z = 0.0;
	uw_glclip.urb.x = glwdt.dspsize.device.x;
	uw_glclip.urb.y = glwdt.dspsize.device.y;
	uw_glclip.urb.z = 1.0;
/*
...set up opengl rendering
*/
#if UU_COMP!=UU_WIN2K
	glXMakeCurrent( uw_xw.disp , uw_xw.wd_id, glx_context );
#endif
line_style:;
	glDepthFunc_d(GL_LEQUAL); 
	glEnable_d(GL_DEPTH_TEST);
	uw_gldepth_mask(1);  
	glClearDepth_d((GLclampd)1.0);
	glClear_d(GL_DEPTH_BUFFER_BIT);
	glEnable_d(GL_BLEND);
	glBlendFunc_d(GL_ONE, GL_ZERO);
/*
.....Disable stencil buffer by default
*/
	uw_glset_stencil(UU_FALSE);
/*
.....added for use GL_SCISSOR_TEST
.....Yurong 2/5/99
*/
	glEnable_d(GL_SCISSOR_TEST);
	clip[0] = 0;
	clip[1] = 0;
	clip[2] = glwdt.dspsize.raster.x;
	clip[3] = glwdt.dspsize.raster.y;
	uw_glset_scissor(clip);
/*
.....Clear all buffers
*/
	glClearColor_d((GLclampf)0,(GLclampf)0,(GLclampf)0,(GLclampf)0);
	glClearDepth_d((GLclampd)1.0);
	uw_gldrawbuffer(UG_FRONT_BUFFER);
	glClear_d(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	uw_gldrawbuffer(UG_BACK_BUFFER); 
	glClear_d(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
/*
.....Set up linestyle
*/
	if ((gl_line_style==0) || (NCL_swap_changed))
		gl_line_style = glGenLists_d (9);
	
	glEnable_d( GL_LINE_STIPPLE);

	glNewList_d( gl_line_style , GL_COMPILE );
		glLineStipple_d((GLint)1, (GLushort)0xffff);              /* Solid */
	glEndList_d();

	glNewList_d( gl_line_style + 1 , GL_COMPILE );
		glLineStipple_d((GLint)1, (GLushort)0x0f0f);              /* Dashed */
	glEndList_d();
	glNewList_d( gl_line_style + 2 , GL_COMPILE );
		glLineStipple_d(1, 0xcccc);              /* Dotted */
	glEndList_d();

	glNewList_d( gl_line_style + 3 , GL_COMPILE );
		glLineStipple_d(1, 0xfe38);              /* Center  _ . _ . _ . _ */
	glEndList_d();           

	glNewList_d( gl_line_style + 4 , GL_COMPILE );
		glLineStipple_d(1, 0xf18c);              /* Phantom _ .. _ .. _ .. _ */  
	glEndList_d();

	glNewList_d( gl_line_style + 5 , GL_COMPILE );
		glLineStipple_d(1, 0x00ff);              /* Dashed  _ _ _ _ _  */
	glEndList_d();

	glNewList_d( gl_line_style + 6 , GL_COMPILE );   
		glLineStipple_d(1, 0xc7f8);              /* Dashed - dotted */
	glEndList_d();

	glNewList_d( gl_line_style + 7 , GL_COMPILE );   
		glLineStipple_d(1, 0xfc00);              /* DASH SPACE _   _   _ */
	glEndList_d();

	glNewList_d( gl_line_style + 8 , GL_COMPILE );   
		glLineStipple_d(1, 0x0000);              /* Invisible */
	glEndList_d();
/*
.....default set to SOLID
*/
	glCallList_d(gl_line_style);
	glLineWidth_d(1.0);

	if (NCL_swap_changed)
		return;
/*
...set up Raster font
*/
#if UU_COMP!=UU_WIN2K
	fontInfo = XLoadQueryFont(uw_xw.disp, glfont);
	if(fontInfo==NULL)
	{
		printf("no font today\n");
		exit(0);
	}
/*
.....Save the font attributes
.....for use later in the program
*/
	uw_glfont.chrpasc = fontInfo->max_bounds.ascent;
	uw_glfont.chrpdsc = fontInfo->max_bounds.descent;
	uw_glfont.chrpwid = fontInfo->max_bounds.width;
	uw_glfont.chrphgt = uw_glfont.chrpasc + uw_glfont.chrpdsc;
	uw_glfont.chrwid = (float)(uw_glfont.chrpwid) / uw_gl.xpixels;
	uw_glfont.chrhgt = (float)(uw_glfont.chrphgt) / uw_gl.ypixels;
	uw_glfont.poffset = fontInfo->ascent;
	uw_glfont.offset = (float)(uw_glfont.chrpasc) / uw_gl.xpixels;
	id = fontInfo->fid;
	first = fontInfo->min_char_or_byte2;
	last = fontInfo->max_char_or_byte2;
	uw_gltext_base = glGenLists_d(last+1);
	if(uw_gltext_base==0)
	{
		printf("out of display list\n");
		exit(0);
	}
	glXUseXFont(id, first, last-first+1, uw_gltext_base+first);
#endif	
	glPixelStorei_d(GL_UNPACK_ALIGNMENT,1);
/*
.....Initialize modeling xform
*/
	uw_gl_ident(uw_gl.modxf);

	glGetIntegerv_d( GL_VIEWPORT , viewport );
/* 
.....set window and viewport
*/
	glViewport_d( 0 , 0,   viewport[2] ,  viewport[3] );
	glMatrixMode_d( GL_PROJECTION);
	glLoadIdentity_d();

	glOrtho_d( (GLdouble)0.0 , (GLdouble)1.0 , (GLdouble)0 , 
					(GLdouble)1.0 , (GLdouble)(-1), (GLdouble)0.0); 

	glMatrixMode_d( GL_MODELVIEW );

	glLoadIdentity_d();
	
	uu_dprint(UU_GITRC,(us,"ortho matrix:"));
/*
.....Setup the transformation matrices
.....Each matrix has the following attributes ***
*/
	for(i=0; i<NOTRANS; ++i)
	{
/*
.....Orthographic (2-D clipping)
*/
		uw_gltag_ortho(i,0.0,1.0,0.0,1.0,-1.0,0.0);
/*
.....Viewport
*/
		uw_gltag_vpt(i,0,0,viewport[2],viewport[3]);
/*
.....Window to viewport
*/
		uw_gltag_w2v(i,0.0,0.0,0.0);
/*
.....Scale
*/
		uw_gltag_s1(i,1.0 , 1.0 , 1.0);
/*
.....Translation
*/
		uw_gltag_t2(i,0.0, 0.0, 0.0);
/*
.....View up rotation
*/
		uw_gltag_vup(i,0.0);
/*
.....View plane normal
*/
		uw_gltag_vpn(i,0.0,0.0);
/*
.....Reference point
*/
		uw_gltag_trn(i,0.0, 0.0, 0.0);
	}
/*
.....Initialize lighting and shading
*/
	uw_glinit_shading();
/*
.....Define the lights
*/
	uw_gllight_define();
}

/**********************************************************************
**    I_FUNCTION :  uw_glupd_font()
**              Update the graphic font
**    PARAMETERS   
**       INPUT  : None
**               
**       OUTPUT : None 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if UU_COMP!=UU_WIN2K
void uw_glupd_font()
{
	XFontStruct *fontInfo;
	Font id;
	unsigned int first, last;
/*
.....Get correct font
*/
	uw_mfquery_font(&UW_label_size[1],&UW_label_size[0]);
/*
.....Load font
*/
	sprintf(glfont,"%dx%d",UW_label_size[1],UW_label_size[0]);
	fontInfo = XLoadQueryFont(uw_xw.disp, glfont);
/*
.....Save the font attributes
.....for use later in the program
*/
	uw_glfont.chrpasc = fontInfo->max_bounds.ascent;
	uw_glfont.chrpdsc = fontInfo->max_bounds.descent;
	uw_glfont.chrpwid = fontInfo->max_bounds.width;
	uw_glfont.chrphgt = uw_glfont.chrpasc + uw_glfont.chrpdsc;
	uw_glfont.chrwid = (float)(uw_glfont.chrpwid) / uw_gl.xpixels;
	uw_glfont.chrhgt = (float)(uw_glfont.chrphgt) / uw_gl.ypixels;
	uw_glfont.poffset = fontInfo->ascent;
	uw_glfont.offset = (float)(uw_glfont.chrpasc) / uw_gl.xpixels;
	id = fontInfo->fid;
	first = fontInfo->min_char_or_byte2;
	last = fontInfo->max_char_or_byte2;
	glXUseXFont(id, first, last-first+1, uw_gltext_base+first);
}
#endif

/**********************************************************************
**    I_FUNCTION : uw_glinit_visual() 
**
**    PARAMETERS   
**       INPUT  : 
**                              none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if UU_COMP!=UU_WIN2K
void uw_glinit_visual()
{
	XColor cblack,ctemp;
	XVisualInfo* vi; 
	int buf[20];
	Colormap def_cmap;
/*
.....User requests shading visual
*/
	vi = NULL;
/*
........Set up visual attributes
*/
	buf[0] = GLX_DOUBLEBUFFER;
	buf[1] = True;
	buf[2] = GLX_DEPTH_SIZE; buf[3] = 8;
	buf[4] = GLX_STENCIL_SIZE; buf[5] = 1;
	buf[6] = GLX_RGBA; buf[7] = None;
	vi = glXChooseVisual(uw_xw.disp,DefaultScreen(uw_xw.disp), buf);
/*
.....Set global variables
*/
	uw_glvinf = *vi;
	if (vi!=NULL)
		XFree(vi);
	uw_gl.maxgcolors = 256;
	def_cmap = DefaultColormap(uw_xw.disp,uw_xw.screen_no);
	UD_cmap_changed = 0;
/*
.....Set up our cursor shapes
*/
	pick_cursor = XCreateFontCursor(uw_xw.disp,XC_dotbox);
	menu_cursor = XCreateFontCursor(uw_xw.disp,XC_top_left_arrow);
	loc_cursor  = XCreateFontCursor(uw_xw.disp,XC_plus);
	strok_cursor= XCreateFontCursor(uw_xw.disp,XC_pencil);
	text_cursor = XCreateFontCursor(uw_xw.disp,XC_xterm);
	pan_cursor  = XCreateFontCursor(uw_xw.disp,XC_fleur);
	rotate_cursor= XCreateFontCursor(uw_xw.disp,XC_exchange);
	zoom_cursor = XCreateFontCursor(uw_xw.disp,XC_sizing);
	mouse_cursor = XCreateFontCursor(uw_xw.disp,XC_mouse);
	XParseColor(uw_xw.disp,def_cmap,"Black",&cblack);
	XParseColor(uw_xw.disp,def_cmap,"White",&ctemp);
	blank_cursor = XCreateFontCursor(uw_xw.disp,XC_watch);
	XRecolorCursor(uw_xw.disp,blank_cursor,&ctemp,&cblack);
	XRecolorCursor(uw_xw.disp,menu_cursor,&ctemp,&cblack);
	XRecolorCursor(uw_xw.disp,text_cursor,&ctemp,&cblack);
	XRecolorCursor(uw_xw.disp,pan_cursor,&ctemp,&cblack);
	XRecolorCursor(uw_xw.disp,rotate_cursor,&ctemp,&cblack);
	XRecolorCursor(uw_xw.disp,zoom_cursor,&ctemp,&cblack);
	XRecolorCursor(uw_xw.disp,loc_cursor,&ctemp,&cblack);
	XRecolorCursor(uw_xw.disp,mouse_cursor,&ctemp,&cblack);
	XParseColor(uw_xw.disp,def_cmap,"Cyan",&ctemp);
	XRecolorCursor(uw_xw.disp,pick_cursor,&ctemp,&cblack); 
   glx_context = glXCreateContext(uw_xw.disp, &uw_glvinf, None, False);
   if(glx_context==NULL)
   {
      printf("Could not create rendering context");
      exit(1);
   }
}
#endif
#if UU_COMP==UU_WIN2K
void uw_glinit_visual()
{
	uw_gl.maxgcolors = 256;
/*
.....Setup the colormap, actually, setup the color setting
*/
	uw_ntcolormap(); 
}
#endif
/*********************************************************************
**
**    I_FUNCTION :  X-Window driver jump table
**
*********************************************************************/
/*
.....gddev2.c
*/
int ug_noop();
/*
.....gddev.c
*/
int ug_dawaitchoice(), ug_dawaitpick(), ug_dawaitloc(), ug_dawaitval(), 
    ug_dawaitstr(), ug_dawaitstroke(),  ug_dawaitev(), ug_dreqchoice(),
    ug_dreqpick(), ug_dreqstroke(), ug_dinitstroke(),
    ug_dreqval(), ug_dinitpick(),ug_dreqstr(), ug_dinitloc2(),
    ug_dinitval(),ug_dstrmode(),ug_dinitstr(), ug_dchoicemode(),ug_dpik();

/*
.....gseg.c
*/
int ug_dseghilite();

/*
.....gout.c
*/
int ug_dpolymk(), ug_dpolymk3(), ug_dpolylnras(), ug_dpolymkras(),
    ug_dfla3();

/*
.....gatts.c
*/
int ug_dcharheight();

int    ug_dflareanm3();
/*
.....wsglatt.c
*/
int uw_glsetlinetype(), uw_glset_linewidth(), uw_glmenutxsz(), uw_gltxtext(), uw_glmarker_size();
/*
.....wsxwdyn.c
*/
int uw_glreqloc();

/*
.....wsgltran.c
*/
int uw_glwind3(), uw_glvport3(),uw_glnormtran(), uw_glwswind(),uw_glwsvport(),
	 uw_glvref3(),  uw_glvpn3(), uw_glvup3(), uw_glmodxf(), uw_glndctodev(),
	uw_gldevtondc();
/*
.....wsglout.c
*/
int uw_glline(), uw_glmarker(), uw_glpolyln3(),uw_glpolyln(),
	uw_glpolymk(), uw_glpolymk3(), uw_gltext(), 
	uw_glfillaras(), uw_glatext(),
	uw_glrasline(), uw_glmarkerras(), uw_glrastext(),uw_glfillarea3(),
	uw_glfillarea(),uw_glfillras(),
	uw_glclearws(), uw_glredrawws(), uw_glupd(), uw_glupd1(),
   uw_glshadearea();
/*
.....wsxwras.c
*/
int uw_glrasput();

/*
.....wsglseg.c
*/
int uw_glopnseg(), uw_glcloseg(), uw_glcreseg(), uw_gldelseg(), uw_glvis(),
	uw_glredrawsegrect(), uw_glviewsg();

/*
.....wsgltrk.c
*/
int uw_gltrk(), uw_glinitloc();
int uw_glpik();
#if UU_COMP!=UU_WIN2K
/*
.....wsmfmenu.c
*/
int uw_mfpopup_menu(), uw_mfstr_prompt(), uw_mfdnprompt(), uw_mfchoice(), uw_mfdown_menunum();

/*
.....wsmfevent.c
*/
int uw_mfkbd(), uw_mfbeep();
/*
.....wsmfsignon.c
*/
int uw_mfterm();

int uw_mfform(), uw_mfform_ckdata();
int uw_mfreset_list(), uw_mfget_field(), uw_mfclose_crrform(),  
	uw_mfget_filename(), uw_mfmd_filename(),
	uw_mfget_dirname(), uw_mfdspfrm_invis(), uw_mfdspfrm_vis(),
	uw_open_mf_window(), uw_mf_win_out(), uw_close_mf_window(),
	uw_mfevent(), uw_mfpocket_window(), uw_mfclose_pocket(); 
int uw_mfsignload();     
int uw_mferror(), uw_mfprmerr(), uw_mfwrprm(),uw_mfwrstat();   
int uw_mfdown_menu();
int uw_mfsignoff();   
int uw_glprint_screen();
int uw_mfmenulayout(), uw_mfload_layout(), uw_mfsave_layout(); 

int uw_mfmenu();
int uw_mfreset_prompt(),uw_mfwindow(),uw_mfsignon();
int uw_mfbcomlin(), uw_mfecomlin(),uw_mfyes_or_no(), uw_mfform_vis(), uw_mfform_invis();
int uw_mfform_display(), uw_mfclose_dispfrm(), uw_mfget_frmfield(), uw_mffrm_setlist();
int uw_mfupdate_form(), uw_mfyes_no_cancel(),uw_mfdisfrm_set_label(),
	uw_mfdisfrm_set_butlabel(), uw_mfget_focusfrm(), uw_mffrm_settlist(),
	uw_mffrm_sorttable(), uw_mfsetwin_title(), uw_mfset_sort_func(), 
	uw_mfnew_session(), uw_chkkey_common(),uw_mffrm_set_focus();
int uw_mfupd_input(), uw_check_event();

#ifndef UU_RS6000
int exit();
#endif

#else
/*
.....wsntmenu.c
*/
int uw_ntstr_prompt(), uw_ntdnprompt(), uw_ntmenu_choice(), uw_ntdown_menunum();

/*
.....wsntevent.c
*/
int uw_ntkbd(), uw_ntbeep();

int uw_ntform();
int uw_ntreset_list(), uw_ntget_field(), uw_ntclose_form(),  
	uw_ntget_filename(), uw_ntmd_filename(),
	uw_ntget_dirname(), uw_ntdspfrm_invis(), uw_ntdspfrm_vis(),
	uw_open_nt_window(), uw_nt_win_out(), uw_close_nt_window(),
	uw_ntapp_exit(), 	uw_ntevent(), uw_ntpocket_window(), uw_ntclose_pocket(); 
int uw_ntsignload(); 
int uz_load_accel(); 
int uw_nterror(), uw_ntprmerr(), uw_ntwrprm(),uw_ntwrstat(), uw_ntmenu();   
int uw_ntdown_menu();   
int uw_ntsignoff();   
int uw_glprint_screen();
int uw_ntmenulayout(), uw_ntload_layout(), uw_ntsave_layout(); 
int uw_ntupd_input();

int uw_ntreset_prompt(),uw_ntwindow(), uw_ntsignon();
int uw_ntbcomlin(), uw_ntecomlin(),uw_ntyes_or_no(), uw_ntform_vis(), uw_ntform_invis(),
	uw_ntform_display(), uw_ntclose_dispfrm(), uw_ntget_frmfield(), uw_ntfrm_setlist();
int uw_ntupdate_form(), uw_ntyes_no_cancel(), uw_ntdisfrm_set_label(), 
	uw_ntdisfrm_set_butlabel(), uw_ntget_focusfrm(), uw_ntfrm_settlist(),
	uw_ntfrm_sorttable(), uw_ntsetwin_title(), uw_ntset_sort_func(), 
	uw_ntnew_session(), uw_chkkey_common(), uw_ntfrm_set_focus(),
	uw_ntfrm_setcolor(), uw_check_event(), uw_ntfrm_set_init_datatyp(), 
	uw_ntfrm_get_form_datatyp(), uw_ntfrm_set_form_attribs();
int uw_ntfrm_enable_sec(), uw_ntfrm_sec_color(), uw_ntfrm_active_sec();
int uw_ntfrm_enable_ok(), uw_ntfrm_enable_close();
#endif

int uw_glclearvp(), uw_glflush(), uw_glhilite();
int uw_glget_scissor(), uw_glset_scissor(), uw_gldyndraw(),
	uw_glget_depthmask(), uw_gldepth_mask(),uw_glget_wsshade();
int uw_picking_rpseg();
/*
.....Workstation entry points table
*/
UG_wsenttbl wsgl={ 
/*
.......Workstation manipulation routines
*/
	uw_glinit,      /* UG_DOPENWS   -- open workstation */ 
#if UU_COMP != UU_WIN2K
	uw_mfterm,      /* UG_DCLOSEWS  -- close workstation */
#else
	ug_noop,	/* UG_DCLOSEWS	-- close workstation */
#endif
	ug_noop ,       /* UG_DACTWS    -- activate workstation */
	ug_noop ,       /* UG_DDEACTWS  -- deactivate workstation */
	uw_glclearws,   /* UG_DCLEARWS  -- clear workstation */
	uw_glredrawws,  /* UG_DREDRAWWS -- redraw all segs on ws */
	uw_glupd,       /* UG_DUPDATE   -- update workstation */
	uw_glredrawsegrect ,     /* UG_DCHGXFORM-- xform changed, fix segs in rect */
	ug_noop ,       /* UG_DSETDEFST -- set deferral state */
	ug_noop,        /* UG_DMSG      -- message */
	ug_noop ,       /* UG_DESCAPE   -- escape */
	ug_noop ,       /* UG_DCALL     -- call specific workstation */
	ug_noop,        /* UG_DIMAGE    -- set imaging mode */
	ug_noop,        /* UG_DINTRACTV -- set interactive mode */
	ug_noop,        /* UG_DHLHSRMODE-- set hlhsr mode */
   ug_noop,     /* UG_DSIGNAL   -- set interrupt function handeler */
/*
.......Output primitives
*/
	uw_glpolyln,    /* UG_DPOLYLN   -- polyline */
	uw_glpolyln3,   /* UG_DPOLYLN3  -- polyline (3-d) */
	uw_glpolymk,    /* UG_DPOLYMK   -- polymarker */
	uw_glpolymk3,   /* UG_DPOLYMK3  -- polymarker (3-d) */
	uw_gltext,      /* UG_DTEXT     -- text */
	uw_glfillarea,  /* UG_DFLAREA   -- fill area */
	uw_glfillarea3, /* UG_DFLAREA3  -- fill area 3D */
	ug_dflareanm3,  /* UG_DFLAREANM3-- fill area  with normals 3D */
	ug_noop,       /* UG_DCELL     -- cell array */
	ug_noop,      /* UG_DCELL3    -- cell array (3D) */
	ug_noop,    /* UG_DCELLRUN  -- runlength encoded 2D cell */
	ug_noop ,       /* UG_DCELLRUN3 -- runlength encoded 3D cell */
	ug_noop ,       /* UG_DGDP      -- generalized drawing primitive */
	ug_dpolylnras,  /* UG_DPOLYLNRAS-- raster polyline */
	ug_dpolymkras,  /* UG_DPOLYMKRAS-- raster polymarker */
	uw_glrastext,   /* UG_DRASTEXT  -- raster text */
	uw_glfillras,   /* UG_DFLAREARAS-- raster fillarea */
	ug_noop,   /* UG_DCELLRAS  -- raster cell  array */
	ug_noop,/* UG_DCELLRUNRAS-- raster encoded array */
	ug_noop,        /* UG_DGDPRAS   -- raster GDP */
#if UU_COMP==UU_WIN2K
	uw_ntbeep,      /* UG_DBEEP     -- beep the bell */
#else
	uw_mfbeep,      /* UG_DBEEP     -- beep the bell */
#endif
/*
.......Output attributes
*/
	ug_noop,        /* UG_DLNINDEX  -- set polyline index */
	uw_glsetlinetype,       /* UG_DLINETYPE -- set linetype */
	uw_glset_linewidth, /* UG_DLINEWIDTH-- set linewidth factor */
	ug_noop,        /* UG_DLNCINDEX -- set polyline color index */
	ug_noop,        /* UG_DMKINDEX  -- set polymarker index */
	ug_noop,        /* UG_DMKTYPE   -- set marker type */
	ug_noop,        /* UG_DMKSIZE   -- set marker scale factor */
	ug_noop,        /* UG_DMKCINDEX -- set polymarker color index */
	ug_noop,        /* UG_DTEXTINDEX-- set text index */
	ug_noop,        /* UG_DTEXTFP   -- set text font precision */
	ug_noop,        /* UG_DCHAREXP  -- set char expansion factor */
	ug_noop,        /* UG_DCHARSPACE-- set character spacing */
	ug_noop,        /* UG_DTXCINDEX -- set text color index */
	ug_noop,        /* UG_DCHARHT   -- set character height */
	ug_noop,        /* UG_DTXPLANE  -- set character plane */
	ug_noop,        /* UG_DCHARUP   -- set character up vector */
	ug_noop,        /* UG_DCHARUP3  -- set char up vector (3-d) */
	ug_noop,        /* UG_DTEXTPATH -- set text path */
	ug_noop,        /* UG_DTEXTALIGN-- set text alignment */
	ug_noop,        /* UG_DFAINDEX  -- set fill area index */
	ug_noop,        /* UG_DFAINTSTYLE-- set fill interior style */
	ug_noop,        /* UG_DFASTYLEINDEX-- set fill style index */
	ug_noop,        /* UG_DFACINDEX -- set fill area color index */
	ug_noop,        /* UG_DPATSIZE  -- set pattern size */
	ug_noop,        /* UG_DPATREFPT -- set pattern reference pt */
	ug_noop,        /* UG_DASFS     -- set aspect source flags */
	ug_noop,        /* UG_DPICKID   -- set pick identifier */
	ug_noop,        /* UG_DLNREP    -- set polyline rep. */
	ug_noop,        /* UG_DMKREP    -- set polymarker rep. */
	ug_noop,        /* UG_DTEXTREP  -- set text representation */
	ug_noop,        /* UG_DFAREP    -- set fill area rep. */
	ug_noop,        /* UG_DPATREP   -- set pattern representation */
	ug_noop,        /* UG_DCOLORREP -- set color representation */
	ug_noop,        /* UG_DEDGEFLAG -- set polygon edge flag */
	ug_noop,        /* UG_DINTRCOLOR-- set polygon interior color */
	ug_noop,	  /* UG_DINTRSHADE-- set polygon shading method */
	ug_noop,        /* UG_DINTRLIGHT-- set polygon lighting method */
	ug_noop,        /* UG_DSURFPROP -- set polygon surface props */
	ug_noop,        /* UG_DLIGHTREP -- set light source rep */
	ug_noop,        /* UG_DLIGHTSTATE-- set light source states */
/*
.......Transformations
*/
	uw_glwind3,     /* UG_DWIND     -- set window  (3D) */
	uw_glvport3,    /* UG_DVPORT    -- set viewport (3D) */
	ug_noop,        /* UG_DVPPRI    -- set vp input priority */
	uw_glnormtran,  /* UG_DNORMTRAN -- set normtran */
	ug_noop,        /* UG_DCLIP     -- set clipping indicator */
	uw_glwswind,    /* UG_DWSWIND   -- set ws window (2D) */
	uw_glwsvport,   /* UG_DWSVPORT  -- set ws viewport (2D) */
	uw_glvref3,     /* UG_DVREF3    -- set view reference pt (3D) */
	uw_glvpn3,      /* UG_DVPN3     -- set view plane (3D) */
	uw_glvup3,      /* UG_DVUP3     -- set view up-vector (3D) */
	uw_glmodxf,     /* UG_DMODXF    -- set modelling matrix (3D) */
	ug_noop,        /* UG_DVMAP     -- set view mapping */
/*
.....Segments
*/
	uw_glcreseg,    /* UG_DCRESEG   -- create segment */
	uw_glopnseg,    /* UG_DOPNSEG   -- open segment */
	uw_glcloseg,    /* UG_DCLOSEG   -- close segment */
	ug_noop,        /* UG_DRENSEG   -- rename segment */
	uw_gldelseg,    /* UG_DDELSEG   -- delete segment */
	ug_noop,        /* UG_DDELSEGWS -- delete seg from workstn */
	ug_noop,        /* UG_DASSSEG   -- assoc seg with workstation */
	ug_noop,        /* UG_DCOPYSEG  -- copy seg. to workstation */
	ug_noop ,       /* UG_DINSSEG   -- insert segment */
	ug_noop ,       /* UG_DSEGTRAN  -- set segment transformation */
	uw_glvis,       /* UG_DSEGVIS   -- set visibility */
	ug_dseghilite,  /* UG_DHILITE   -- set hilighting */
	ug_noop,        /* UG_DSEGPRI   -- seg segment priority */
	ug_noop,        /* UG_DSEGDET   -- set detectability */
/*
.....Segment elements
*/
	ug_noop,        /* UG_DINLABEL  -- insert label */
	ug_noop,        /* UG_DSETEPT   -- set element pointer */
	ug_noop,        /* UG_DOFEPT    -- offset element pointer */
	ug_noop,        /* UG_DEPTLBL   -- set element ptr to label */
	ug_noop,        /* UG_DELELT    -- delete element */
	ug_noop,        /* UG_DELELTR   -- delete element range */
	ug_noop,        /* UG_DELELTLBL -- delete between labels */
/*
.....Input functions
*/
	uw_glinitloc,   /* UG_DINITLOC  -- initialize locator */
	ug_dinitstroke, /* UG_DINITSTROKE-- initialize stroke */
	ug_dinitval,    /* UG_DINITVAL  -- initialize valuator */
	ug_noop,/* UG_DINITCHOICE-- initialize choice */
	ug_dinitpick,   /* UG_DINITPICK -- initialize pick */
	ug_dinitstr,    /* UG_DINITSTRING-- initialize string */
	ug_noop ,       /* UG_DLOCMODE  -- set locator mode */
	ug_noop ,       /* UG_DSTROKEMODE-- set stroke mode */
	ug_noop ,       /* UG_DVALMODE  -- set valuator mode */
	ug_dchoicemode, /* UG_DCHOICEMODE-- set choice mode */
	ug_noop ,       /* UG_DPICKMODE -- set pick mode */
	ug_dstrmode,    /* UG_DSTRINGMODE-- set string mode */
	uw_glreqloc,    /* UG_DREQLOC   -- request locator */
	ug_dreqstroke,  /* UG_DREQSTROKE-- request stroke */
	ug_dreqval,     /* UG_DREQVAL   -- request valuator */
	ug_dreqchoice,  /* UG_DREQCHOICE-- request choice */
	ug_dreqpick,    /* UG_DREQPICK  -- request pick */
	ug_dreqstr,     /* UG_DREQSTRING-- request string */
	ug_noop ,       /* UG_DSAMPLOC  -- sample loc */
	ug_noop ,       /* UG_DSAMPSTROKE-- sample stroke */
	ug_noop ,       /* UG_DSAMPVAL  -- sample valuator */
	ug_noop ,       /* UG_DSAMPCHOICE-- sample choice */
	ug_noop ,       /* UG_DSAMPPICK -- sample pick */
	ug_noop ,       /* UG_DSAMPSTRING-- sample string */
	ug_dawaitev,    /* UG_DAWAITDEV -- await event */
	ug_noop,    /* UG_DPUTSTRING-- write to scrolling area*/
	ug_noop,        /* UG_DCHGCHOICEAREA-- change choice area */
	uw_glmenutxsz,  /* UG_DMENUTEXTSIZE-- menu text size, in DC */
	ug_noop,       /* UG_DCHOICEHILITE-- hilite an icon */
/*
.......Metafile
*/
	ug_noop ,       /* UG_DWRITEGKSM-- write item to GKSM */
	ug_noop ,       /* UG_DTYPEGKSM -- get item type from GKSM */
	ug_noop ,       /* UG_DREADGKSM -- read item type from GKSM */
/*
.......Inquiry
*/
	uw_gltxtext,    /* UG_DTXTEXT   -- text extent */
	ug_noop ,       /* UG_DPIXDIM   -- inquire pixel array dimensions */
	ug_noop ,       /* UG_DPIXARRAY -- inquire pixel array */
	ug_noop ,       /* UG_DPIXEL    -- inquire pixel */
/*
.......Secondary entries (lower level, called by sim. routines)
*/
	ug_dawaitchoice,/* UG_DAWAITCHOICE-- await choice */
	ug_dawaitpick,  /* UG_DAWAITPICK-- await pick */
	ug_dawaitloc,   /* UG_DAWAITLOC -- await locator */
	ug_dawaitval,   /* UG_DAWAITVAL -- await valuator */
	ug_dawaitstr,   /* UG_DAWAITSTRING-- await string */
	ug_dawaitstroke,/* UG_DAWAITSTROKE-- await stroke */
	ug_noop,        /* UG_DECHOOP   -- echo */
	uw_glndctodev,  /* UG_DNDCDEV   -- NDC to device conversion */
	uw_gldevtondc,  /* UG_DDEVNDC   -- device to NDC conversion */
	ug_noop,        /* UG_DPAGOP    -- page (clears the screen) */
	ug_noop,        /* UG_DSAVSCROP -- save screen */
	ug_noop,    /* UG_DRESSCROP   -- read a raster rectangle */
	ug_noop,     /* UG_DATEXT    -- alpha text */
	ug_noop,        /* UG_DRASGET   -- read a raster rectangle */
	uw_glrasput,    /* UG_DRASPUT   -- clear a raster rectangle */
	ug_noop,        /* UG_DRASCPY   -- copy a raster rectangle */
	ug_noop,        /* UG_DRASALLOC -- allocate raster memory */
	ug_noop,/* UG_DRASDEALLOC-- de-allocate raster memory */
	uw_glrasline,   /* UG_DRASLINE  -- draw line raster coords */
	uw_glmarkerras, /* UG_DMARKERRAS   -- draw a raster marker */
#if UU_COMP==UU_WIN2K
	uw_ntkbd,	/* UG_DKBD	-- get keyboard */
#else
	uw_mfkbd,       /* UG_DKBD      -- get keyboard */
#endif
	ug_noop,        /* UG_D1CHAR    -- get 1 char from keyboard */
	ug_noop,        /* UG_DKEYPAD   -- get a keypad key */
	uw_gltrk,       /* UG_DTRK      -- track a loc cursor */
	uw_glpik,       /* UG_DPIK   -- track pick cursor and pick*/
#if UU_COMP==UU_WIN2K
  	uw_ntmenu_choice,	/* UG_DCHOICE	-- get phys chc dev. data */
#else
	uw_mfchoice,    /* UG_DCHOICE   -- get phys chc dev. data */
#endif
	ug_noop,        /* UG_DBUTTON   -- get phys button data */
	ug_noop,        /* UG_DVAL      -- get valuator data */
	ug_noop,        /* UG_DSTREAM   -- get stream device data */
	ug_noop,        /* UG_DMOVOP    -- move to x,y */
	uw_glline,      /* UG_DDRWOP    -- line (2D) */
	uw_glmarker,    /* UG_DPNTOP    -- point at x,y */
	ug_noop,        /* UG_DCHHTNDC  -- set character height*/
	ug_noop,        /* UG_D??? */
/*
.....ANSI terminal functions
*/
   ug_noop,   /* UG_DANSION  -- turn on ansi term. mode */
   ug_noop,  /* UG_DANSIOFF -- turn off ansi term. mode */
   ug_noop,  /* UG_DANSIVIS -- make ansi term visible */
   ug_noop, /* UG_DANSINVIS   -- make ansi term invisible */
   ug_noop,   /* UG_DANSIUP  -- move cursor up one row */
   ug_noop,  /* UG_DANSIDWN -- move cursor down one row */
   ug_noop,  /* UG_DANSILFT -- move cursor left 1 col */
   ug_noop,  /* UG_DANSIRGT -- move cursor right 1 col */
   ug_noop, /* UG_DANSIEC2E   -- Erase from Cur to Eol */
   ug_noop, /* UG_DANSIEB2C   -- Erase Beg of line to Cur */
   ug_noop, /* UG_DANSIEL  -- Erase current Line */
   ug_noop, /* UG_DANSIEC2ES-- Erase Cursor to End */
   ug_noop, /* UG_DANSIEBS2C-- Erase Begin to Cursor */
   ug_noop, /* UG_DANSIESCR   -- Erase ansi SCReen */
   ug_noop, /* UG_DANSISPOS   -- Set cursor POS (row,col) */
   ug_noop, /* UG_DANSIQPOS   -- Inquire cursor position */
#if UU_COMP==UU_WIN2K
   uw_ntstr_prompt,   /* UG_DPROMPT  -- put up text prompt*/
/*
.......Prompt and menu functions
*/
	uw_ntdnprompt,	   /* UG_DDNPROMPT	-- take down text prompt */
	ug_noop,	/* UG_DMENU	 put up menu */
#else
	uw_mfstr_prompt,   /* UG_DPROMPT  -- put up text prompt*/
/*
.......Prompt and menu functions
*/
	uw_mfdnprompt,     /* UG_DDNPROMPT      -- take down text prompt */
	uw_mfpopup_menu,        /* UG_DMENU      put up menu */
#endif
	ug_noop,	/* UG_DDNMENU	 put up menu */
	ug_noop,          /* UG_DRASPNTS -- take down menu */
/*
... aak aug-7-1998: new output primitive
*/
	uw_glshadearea,   /* UG_DSHADEAREA: 3-d shaded area */ 
/*
.....added for forms
*/
#if UU_COMP==UU_WIN2K
	uw_ntform,         /* UW_FORM */
	uw_ntreset_list,   /* UW_SET_LIST */
	uw_ntget_field,    /* UW_GET_FIELD */
	uw_ntclose_form,  /* UW_CLOSE_FORM */
	uw_ntget_filename,    /* UW_GET_FILENAME */
	uw_ntmd_filename,      /* UW_GET_FNAME_ADV */
	uw_open_nt_window,      /* UW_OPEN_WINDOW */
	uw_nt_win_out,		/* UW_WIN_OUT	*/
	uw_close_nt_window,	/* UW_CLOSE_WINDOW */
	uw_ntapp_exit,					/* UW_APP_EXIT */	
	uz_load_accel,		/* UZ_LOAD_ACCEL */
	uw_ntevent,			/* UW_EVENT */
	uw_ntpocket_window,		/* UW_OPEN_POCKET */
	uw_ntclose_pocket,		/* UW_CLOSE_POCKET */
	uw_ntsignload,			/* UW_SIGNON_LOAD */
	uw_nterror,				/* UW_ERROR_MSG */
	uw_ntdown_menu,			/* UW_DOWN_MENU */
	uw_ntsignoff,			/* UW_SIGNOFF */
	uw_glprint_screen,		/* UW_PRINT_SCREEN */
	uw_ntload_layout,		/* UW_LOAD_LAYOUT */
	uw_ntsave_layout,		/* UW_SAVE_LAYOUT */ 
	uw_ntmenulayout,		/* UW_MENU_DESIGN */
#else
	uw_mfform,         /* UW_FORM */
	uw_mfreset_list,   /* UW_SET_LIST */
	uw_mfget_field,    /* UW_GET_FIELD */
	uw_mfclose_crrform,  /* UW_CLOSE_FORM */
	uw_mfget_filename,    /* UW_GET_FILENAME */
	uw_mfmd_filename,      /* UW_GET_FNAME_ADV */
	uw_open_mf_window,      /* UW_OPEN_WINDOW */
	uw_mf_win_out,                  /* UW_WIN_OUT   */
	uw_close_mf_window,             /* UW_CLOSE_WINDOW */
	exit,                                   /* UW_APP_EXIT */       
	ug_noop,                                        /* UZ_LOAD_ACCEL */
	uw_mfevent,                     /* UW_EVENT */
	uw_mfpocket_window,             /* UW_OPEN_POCKET */
	uw_mfclose_pocket,              /* UW_CLOSE_POCKET */
	uw_mfsignload,                  /* UW_SIGNON_LOAD */
	uw_mferror,                             /* UW_ERROR_MSG */
	uw_mfdown_menu,                 /* UW_DOWN_MENU */
	uw_mfsignoff,                   /* UW_SIGNOFF */
	uw_glprint_screen,              /* UW_PRINT_SCREEN */
	uw_mfload_layout,               /* UW_LOAD_LAYOUT */
	uw_mfsave_layout,               /* UW_SAVE_LAYOUT */ 
	uw_mfmenulayout,                /* UW_MENU_DESIGN */
#endif
	uw_glviewsg,          /* UW_VIEW_SEG */
	ug_noop,              /* UW_DEL_CUTSEG */
	ug_noop,              /* UW_ERASE_CUTSEG */
	ug_noop,              /* UW_RESET_CUTSEG */
	ug_noop,              /* UW_OPEN_CUTSEG */
	ug_noop,              /* UW_POSTN_CUTSEG */
	ug_noop,              /* UW_CLOSE_CUTSEG */
	ug_noop,              /* UW_GETSURF */
	ug_noop,              /* UW_SETSURF */
	ug_noop,              /* UW_GRAPHSURF */
	ug_noop,              /* UW_POPSURF */
	uw_glclearvp,         /* UW_CLEAR_VP */
	uw_glflush,           /* UW_FLUSH */  
	uw_glhilite,          /* UW_HILITE */
#if UU_COMP==UU_WIN2K
	ug_noop,			/* UW_FORM_CKDATA , not used now */
	uw_ntprmerr,				/* UW_PRMERR */
	uw_ntwrprm,					/* UW_WRPRM	*/
	uw_ntwrstat,				/* UW_WRSTAT */
	uw_ntmenu,					/* UW_MENU */
	uw_ntreset_prompt,			/* UW_RESET_PROMPT */
	uw_ntwindow,				/* UW_WINDOW */
	uw_ntsignon,				/* UW_SIGNON */
#else
	uw_mfform_ckdata,                       /* UW_FORM_CKDATA */
	uw_mfprmerr,                            /* UW_PRMERR */
	uw_mfwrprm,                                     /* UW_WRPRM     */
	uw_mfwrstat,                            /* UW_WRSTAT */
	uw_mfmenu,                                      /* UW_MENU */
	uw_mfreset_prompt,                      /* UW_RESET_PROMPT */
	uw_mfwindow,                            /* UW_WINDOW */
	uw_mfsignon,                            /* UW_SIGNON */
#endif
	uw_glget_scissor,                       /* UW_GET_CLIP */
	uw_glset_scissor,                       /* UW_SET_CLIP */
	uw_gldyndraw,                           /* UW_DYNDRAW */
	uw_glget_depthmask,                     /* UW_GET_DEPMASK */
	uw_gldepth_mask,                        /* UW_SET_DEPMASK */
	uw_glget_wsshade,                       /* UW_GET_WSSHADE */
/*	uw_glgetmask,*/                         /* UW_GET_LUCENCY */
	ug_noop,
#if UU_COMP==UU_WIN2K
	uw_ntyes_or_no,				/* UW_YESNO */
	uw_ntbcomlin,				/* UW_BCOMLINE */
	uw_ntecomlin,				/* UW_ECOMLINE */
	uw_ntform_vis,				/* UW_FORM_VIS */
	uw_ntform_invis,			/* UW_FORM_INVIS */
	uw_ntform_display,		/* UW_FORM_DISPLAY */
	uw_ntclose_dispfrm,			/* UW_CLOSE_DISPFRM */
	uw_ntget_frmfield,			/* UW_GETFRM_FIELD */
	uw_ntfrm_setlist,			/* UW_DISPFRM_SET_LIST */
	uw_ntupdate_form,			/* UW_UPDATE_FRM */
#else
	uw_mfyes_or_no,                 /* UW_YESNO */
	uw_mfbcomlin,                           /* UW_BCOMLINE */
	uw_mfecomlin,                            /* UW_ECOMLINE */
	uw_mfform_vis,				/* UW_FORM_VIS */
	uw_mfform_invis,			/* UW_FORM_INVIS */
	uw_mfform_display,		/* UW_FORM_DISPLAY */
	uw_mfclose_dispfrm,			/* UW_CLOSE_DISPFRM */
	uw_mfget_frmfield,			/* UW_GETFRM_FIELD */
	uw_mffrm_setlist,			/* UW_DISPFRM_SET_LIST */
	uw_mfupdate_form,			/* UW_UPDATE_FRM */
#endif
	uw_picking_rpseg,			/* UG_RPPICK_SEG */
#if UU_COMP==UU_WIN2K
	uw_ntget_dirname,			/* UW_GET_DIRNAME */
	uw_ntdspfrm_invis,			/* UW_DSPFRM_INVIS */
	uw_ntdspfrm_vis	,		/* UW_DSPFRM_VIS */
	uw_ntyes_no_cancel,		/* UW_YESNOCANCEL */
	uw_ntdisfrm_set_label,		/* UW_DISPFRM_SET_LABEL */
	uw_ntdisfrm_set_butlabel,		/* UW_DISPFRM_SET_BUTLABEL */
#else
	uw_mfget_dirname,			/* UW_GET_DIRNAME */
	uw_mfdspfrm_invis,			/* UW_DSPFRM_INVIS */
	uw_mfdspfrm_vis	,		/* UW_DSPFRM_VIS */
	uw_mfyes_no_cancel,		/* UW_YESNOCANCEL */
	uw_mfdisfrm_set_label,		/* UW_DISPFRM_SET_LABEL */
	uw_mfdisfrm_set_butlabel,		/* UW_DISPFRM_SET_BUTLABEL */
#endif
	uw_glmarker_size,		/* UW_MARKER_SIZE */
#if UU_COMP==UU_WIN2K
	uw_ntupd_input,			/* UW_UPDATE_INPUT */
	uw_ntget_focusfrm,		/* UW_GET_FOCUSFRM */
	uw_ntfrm_settlist,			/* UW_DISPFRM_SET_TLIST */
	uw_ntfrm_sorttable,			/* UW_FORM_SORTTABLE */
	uw_ntsetwin_title,			/* UW_SETWIN_TITLE */
	uw_ntset_sort_func,			/* UW_SETFORM_TFUNC */
	uw_ntnew_session,				/* UW_NEW_SESSION */
	uw_chkkey_common,			/*UW_CHKKEY_COM */
	uw_ntdown_menunum,          /* UG_DDNMENUNUM  take down menu */
	uw_ntfrm_set_focus,			/* UW_DISPFRM_SET_FOCUS  set focus of a form field */
	uw_ntfrm_setcolor,                     /* UW_DISPFRM_SET_COLOR */
	uw_check_event,			/*UW_CHKWIN_EVENT */
	uw_ntfrm_enable_sec,                     /* UW_FRMSEC_ENABLE */
	uw_ntfrm_sec_color,                     /* UW_FRMSEC_SET_COLOR */
	uw_ntfrm_active_sec,                     /* UW_FRMSEC_ACTIVE */
	uw_ntfrm_set_init_datatyp,			/*UW_DISPFRM_SET_INIT_DATATYP */
	uw_ntfrm_get_form_datatyp,			/*UW_DISPFRM_GET_FORM_DATATYP */	
	uw_ntfrm_set_form_attribs,			/*UW_DISPFRM_SET_ATTR */
	uw_ntfrm_enable_ok,			/*UW_FRM_ENABLE_OK */
	uw_ntfrm_enable_close,			/*UW_FRM_ENABLE_CLOSE */
#else
	uw_mfupd_input,			/* UW_UPDATE_INPUT */
	uw_mfget_focusfrm,		/* UW_GET_FOCUSFRM */
	uw_mffrm_settlist,			/* UW_DISPFRM_SET_TLIST */
	uw_mffrm_sorttable,			/* UW_FORM_SORTTABLE */
	uw_mfsetwin_title,			/* UW_SETWIN_TITLE */
	uw_mfset_sort_func,			/* UW_SETFORM_TFUNC */
	uw_mfnew_session,				/* UW_NEW_SESSION */
	uw_chkkey_common,			/*UW_CHKKEY_COM */
	uw_mfdown_menunum ,         /* UG_DDNMENUNUM  take down menu */
	uw_mffrm_set_focus,			/* UW_DISPFRM_SET_FOCUS  set focus of a form field */
	ug_noop,                    /* UW_DISPFRM_SET_COLOR */
	uw_check_event,			/*UW_CHKWIN_EVENT */
	ug_noop,                     /* UW_FRMSEC_ENABLE */
	ug_noop,                     /* UW_FRMSEC_SET_COLOR */
	ug_noop,                     /* UW_FRMSEC_ACTIVE */
	ug_noop,			/*UW_DISPFRM_SET_INIT_DATATYP */
	ug_noop,			/*UW_DISPFRM_GET_FORM_DATATYP */
	ug_noop,			/*UW_DISPFRM_SET_ATTR */
	ug_noop,			/*UW_FRM_ENABLE_OK */
	ug_noop,			/*UW_FRM_ENABLE_OK */
#endif
};
#endif
