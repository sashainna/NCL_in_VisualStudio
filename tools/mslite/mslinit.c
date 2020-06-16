/*********************************************************************
**  NAME:  mslinit.c
**
**      MSLITE: initial functions section.
**
**		CONTAINS:
**			msl_glinit
**			msl_setsize
**			define UG_wsenttbl wsgl used for MSLITE
**    COPYRIGHT 2008 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			mslinit.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:59
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#define DPGM 1
#include "dmotif.h"
#include "usysg.h"
#undef DPGM

#include "ws.h"
#define WSGL_FIRST
#include "wsgl.h"
#undef WSGL_FIRST


#include "gdidd.h"
#include "gdidd2.h"

int uw_glhicolor, uw_glvrfcolor;
typedef struct
{
	Gint op; Gws wsid; 
	Gchar *conn;    /* connection-filename to access device */
} Swid;
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

extern Gnrect MSL_area;
float uw_form_scalex;
float uw_form_scaley;
								
/*********************************************************************
**    I_FUNCTION :  msl_glinit(prms,reply) ----------
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

int msl_glinit(prms,reply)
Swid *prms;
UG_rwsinit *reply;      /* struct {Gstatus stat;UG_wdt *wdtptr;} *reply */
{
/*
.....Variable definitions
*/
	int i,maxx,maxy;
	static int onoff = 0;  /* turn on/off X Synchronization */
	char *p,*ux_getenv();
	double atof();
	int n;

	uw_form_scalex = 1.0;
	uw_form_scaley = 1.0;
	p = ux_getenv("UW_FORM_SCALEX", UX_PRTERRS);
	if (p != 0) uw_form_scalex = (float) atof(p);
	p = ux_getenv("UW_FORM_SCALEY", UX_PRTERRS);
	if (p != 0) uw_form_scaley = (float) atof(p);

	uw_form_scalex = (float) (uw_form_scalex * 1.0);
	uw_form_scaley = (float) (uw_form_scaley * 1.0);
	
	i = 0;
	n = 0;
	uw_gltext_base = 0;
	gl_line_style = 0;
/*
.....Let the user override the window size
*/
	maxx = GetSystemMetrics(SM_CXSCREEN);
	maxy = GetSystemMetrics(SM_CYSCREEN);

	p = ux_getenv("UU_WINX", UX_PRTERRS);
	if (p != 0) maxx = atoi(p);
	p = ux_getenv("UU_WINY", UX_PRTERRS);
	if (p != 0) maxy = atoi(p);
	p = ux_getenv("UU_XYRATIO",UX_PRTERRS);

	uw_gl.dev_xmax = maxx;
	uw_gl.dev_ymax = maxy;

	glwdt.dspsize.raster.x = maxx;
	glwdt.dspsize.raster.y = maxy;
	uw_gl.xpixels = maxx;
	uw_gl.ypixels = maxy;
	uw_gl.xndc = 1.0;
	uw_gl.yndc = (float)maxx/(float)maxy;
/*
.....Initialize some default settings
*/
	uw_gl.linetype = 1;
/*
.....Set up return parameters
*/
	(*reply).stat=UG_OK;                    /* return status OK */
	(*reply).wdtptr= &glwdt;                /* return addr of ws desc table */
	uw_gl.wid = (*prms).wsid;       /* workstation id */
	return(UU_SUCCESS);
}
/*********************************************************************
**       I_FUNCTION : msl_setsize(wid,hgt)
**                      This function size the window.
**       PARAMETERS     
**               INPUT  :  
**               wid      = Width of new graphics window.
**               hgt      = Height of new graphics window.
**               OUTPUT :  none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
msl_setsize(wid,hgt)
int wid,hgt;
{
	UU_REAL rx,ry;
/*
........Screen size
*/
	MSL_area.ll.x = 0;
	MSL_area.ll.y = 0;
	rx = wid; ry = hgt;
	if (wid > hgt)
	{
		MSL_area.ur.x = 1;
		MSL_area.ur.y = ry / rx;
	}
	else
	{
		MSL_area.ur.y = 1;
		MSL_area.ur.x = rx / ry;
	}
/*
.....Set the workstation Xform matrix
*/
	uw_gl.xpixels = wid;
	uw_gl.ypixels = hgt;
	glwdt.dspsize.raster.x = wid;
	glwdt.dspsize.raster.y = hgt;
	if(wid>hgt)
		uw_gl.wsxform.sf=(GLdouble)wid;
	else
		uw_gl.wsxform.sf=(GLdouble)hgt;
	uw_gl.wsxform.dx=0;
	uw_gl.wsxform.dy=0;
	glwdt.dspsize.device.x = 1;
	glwdt.dspsize.device.y = (float)hgt/(float)wid;
	if (glwdt.dspsize.device.y>1)
	{
		glwdt.dspsize.device.x = 1/glwdt.dspsize.device.y;
		glwdt.dspsize.device.y = 1.0;
	}
	uw_glclip.urb.x = glwdt.dspsize.device.x;
	uw_glclip.urb.y = glwdt.dspsize.device.y;

	return 0;
}
int uw_ntmenu(), uw_ntdown_menu();
int uw_glwind3(), uw_glvport3(),uw_glnormtran(), uw_glwswind(),uw_glwsvport(),
	 uw_glvref3(),  uw_glvpn3(), uw_glvup3(), uw_glmodxf(), uw_glndctodev(),
	uw_gldevtondc();

int uw_ntform_vis(), uw_ntform_invis(), uw_ntform_display(), uw_ntclose_dispfrm(), 
	uw_ntget_frmfield(), uw_ntfrm_setlist(), uw_ntupdate_form(), uw_ntdspfrm_invis(),
	uw_ntdspfrm_vis(), uw_ntdisfrm_set_label(), uw_ntdisfrm_set_butlabel(),
	uw_ntform(), uw_ntreset_list(), uw_ntget_field(), uw_ntclose_form(), uw_ntupd_input(),
	uw_ntget_focusfrm(), uw_ntfrm_settlist(),
	uw_ntfrm_sorttable(), uw_ntsetwin_title(), uw_ntset_sort_func(), 
	uw_ntfrm_set_focus(),
	uw_ntfrm_setcolor();
int uw_ntget_dirname(), uw_ntyes_no_cancel(), uw_ntget_filename(), uw_ntmd_filename(),
	uw_nterror();
int uw_gltrk(), uw_ntmenu_choice(), uw_ntstr_prompt(), uw_ntdnprompt(), uw_ntdown_menunum(),
	uw_ntevent(), uw_ntprmerr(), uw_ntwrprm(), uw_ntwrstat(), uw_ntreset_prompt(),
	uw_ntwindow(), uw_ntyes_or_no(), uw_ntbcomlin(), uw_ntecomlin(), ug_noop();
int uw_glinitloc(), uw_glreqloc();
int uz_load_accel();

UG_wsenttbl wsgl={ 
/*
.......Workstation manipulation routines
*/
	msl_glinit,      /* UG_DOPENWS   -- open workstation */ 
	ug_noop,	/* UG_DCLOSEWS	-- close workstation */
	ug_noop ,       /* UG_DACTWS    -- activate workstation */
	ug_noop ,       /* UG_DDEACTWS  -- deactivate workstation */
	ug_noop,   /* UG_DCLEARWS  -- clear workstation */
	ug_noop,  /* UG_DREDRAWWS -- redraw all segs on ws */
	ug_noop,       /* UG_DUPDATE   -- update workstation */
	ug_noop ,     /* UG_DCHGXFORM-- xform changed, fix segs in rect */
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
	ug_noop,    /* UG_DPOLYLN   -- polyline */
	ug_noop,   /* UG_DPOLYLN3  -- polyline (3-d) */
	ug_noop,    /* UG_DPOLYMK   -- polymarker */
	ug_noop,   /* UG_DPOLYMK3  -- polymarker (3-d) */
	ug_noop,      /* UG_DTEXT     -- text */
	ug_noop,  /* UG_DFLAREA   -- fill area */
	ug_noop, /* UG_DFLAREA3  -- fill area 3D */
	ug_noop,  /* UG_DFLAREANM3-- fill area  with normals 3D */
	ug_noop,       /* UG_DCELL     -- cell array */
	ug_noop,      /* UG_DCELL3    -- cell array (3D) */
	ug_noop,    /* UG_DCELLRUN  -- runlength encoded 2D cell */
	ug_noop ,       /* UG_DCELLRUN3 -- runlength encoded 3D cell */
	ug_noop ,       /* UG_DGDP      -- generalized drawing primitive */
	ug_noop,  /* UG_DPOLYLNRAS-- raster polyline */
	ug_noop,  /* UG_DPOLYMKRAS-- raster polymarker */
	ug_noop,   /* UG_DRASTEXT  -- raster text */
	ug_noop,   /* UG_DFLAREARAS-- raster fillarea */
	ug_noop,   /* UG_DCELLRAS  -- raster cell  array */
	ug_noop,/* UG_DCELLRUNRAS-- raster encoded array */
	ug_noop,        /* UG_DGDPRAS   -- raster GDP */
	ug_noop,      /* UG_DBEEP     -- beep the bell */
/*
.......Output attributes
*/
	ug_noop,        /* UG_DLNINDEX  -- set polyline index */
	ug_noop,       /* UG_DLINETYPE -- set linetype */
	ug_noop, /* UG_DLINEWIDTH-- set linewidth factor */
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
	ug_noop,    /* UG_DCRESEG   -- create segment */
	ug_noop,    /* UG_DOPNSEG   -- open segment */
	ug_noop,    /* UG_DCLOSEG   -- close segment */
	ug_noop,        /* UG_DRENSEG   -- rename segment */
	ug_noop,    /* UG_DDELSEG   -- delete segment */
	ug_noop,        /* UG_DDELSEGWS -- delete seg from workstn */
	ug_noop,        /* UG_DASSSEG   -- assoc seg with workstation */
	ug_noop,        /* UG_DCOPYSEG  -- copy seg. to workstation */
	ug_noop ,       /* UG_DINSSEG   -- insert segment */
	ug_noop ,       /* UG_DSEGTRAN  -- set segment transformation */
	ug_noop,       /* UG_DSEGVIS   -- set visibility */
	ug_noop,  /* UG_DHILITE   -- set hilighting */
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
	ug_noop, /* UG_DINITSTROKE-- initialize stroke */
	ug_noop,    /* UG_DINITVAL  -- initialize valuator */
	ug_noop,/* UG_DINITCHOICE-- initialize choice */
	ug_noop,   /* UG_DINITPICK -- initialize pick */
	ug_noop,    /* UG_DINITSTRING-- initialize string */
	ug_noop ,       /* UG_DLOCMODE  -- set locator mode */
	ug_noop ,       /* UG_DSTROKEMODE-- set stroke mode */
	ug_noop ,       /* UG_DVALMODE  -- set valuator mode */
	ug_noop, /* UG_DCHOICEMODE-- set choice mode */
	ug_noop ,       /* UG_DPICKMODE -- set pick mode */
	ug_noop,    /* UG_DSTRINGMODE-- set string mode */
	uw_glreqloc,    /* UG_DREQLOC   -- request locator */
	ug_noop,  /* UG_DREQSTROKE-- request stroke */
	ug_noop,     /* UG_DREQVAL   -- request valuator */
	ug_noop,  /* UG_DREQCHOICE-- request choice */
	ug_noop,    /* UG_DREQPICK  -- request pick */
	ug_noop,     /* UG_DREQSTRING-- request string */
	ug_noop ,       /* UG_DSAMPLOC  -- sample loc */
	ug_noop ,       /* UG_DSAMPSTROKE-- sample stroke */
	ug_noop ,       /* UG_DSAMPVAL  -- sample valuator */
	ug_noop ,       /* UG_DSAMPCHOICE-- sample choice */
	ug_noop ,       /* UG_DSAMPPICK -- sample pick */
	ug_noop ,       /* UG_DSAMPSTRING-- sample string */
	ug_noop,    /* UG_DAWAITDEV -- await event */
	ug_noop,    /* UG_DPUTSTRING-- write to scrolling area*/
	ug_noop,        /* UG_DCHGCHOICEAREA-- change choice area */
	ug_noop,  /* UG_DMENUTEXTSIZE-- menu text size, in DC */
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
	ug_noop,    /* UG_DTXTEXT   -- text extent */
	ug_noop ,       /* UG_DPIXDIM   -- inquire pixel array dimensions */
	ug_noop ,       /* UG_DPIXARRAY -- inquire pixel array */
	ug_noop ,       /* UG_DPIXEL    -- inquire pixel */
/*
.......Secondary entries (lower level, called by sim. routines)
*/
	ug_noop,/* UG_DAWAITCHOICE-- await choice */
	ug_noop,  /* UG_DAWAITPICK-- await pick */
	ug_noop,   /* UG_DAWAITLOC -- await locator */
	ug_noop,   /* UG_DAWAITVAL -- await valuator */
	ug_noop,   /* UG_DAWAITSTRING-- await string */
	ug_noop,/* UG_DAWAITSTROKE-- await stroke */
	ug_noop,        /* UG_DECHOOP   -- echo */
	uw_glndctodev,  /* UG_DNDCDEV   -- NDC to device conversion */
	uw_gldevtondc,  /* UG_DDEVNDC   -- device to NDC conversion */
	ug_noop,        /* UG_DPAGOP    -- page (clears the screen) */
	ug_noop,        /* UG_DSAVSCROP -- save screen */
	ug_noop,    /* UG_DRESSCROP   -- read a raster rectangle */
	ug_noop,     /* UG_DATEXT    -- alpha text */
	ug_noop,        /* UG_DRASGET   -- read a raster rectangle */
	ug_noop,    /* UG_DRASPUT   -- clear a raster rectangle */
	ug_noop,        /* UG_DRASCPY   -- copy a raster rectangle */
	ug_noop,        /* UG_DRASALLOC -- allocate raster memory */
	ug_noop,/* UG_DRASDEALLOC-- de-allocate raster memory */
	ug_noop,   /* UG_DRASLINE  -- draw line raster coords */
	ug_noop, /* UG_DMARKERRAS   -- draw a raster marker */
	ug_noop,	/* UG_DKBD	-- get keyboard */
	ug_noop,        /* UG_D1CHAR    -- get 1 char from keyboard */
	ug_noop,        /* UG_DKEYPAD   -- get a keypad key */
	uw_gltrk,       /* UG_DTRK      -- track a loc cursor */
	ug_noop,       /* UG_DPIK   -- track pick cursor and pick*/
  	uw_ntmenu_choice,	/* UG_DCHOICE	-- get phys chc dev. data */
	ug_noop,        /* UG_DBUTTON   -- get phys button data */
	ug_noop,        /* UG_DVAL      -- get valuator data */
	ug_noop,        /* UG_DSTREAM   -- get stream device data */
	ug_noop,        /* UG_DMOVOP    -- move to x,y */
	ug_noop,      /* UG_DDRWOP    -- line (2D) */
	ug_noop,    /* UG_DPNTOP    -- point at x,y */
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
   uw_ntstr_prompt,   /* UG_DPROMPT  -- put up text prompt*/
/*
.......Prompt and menu functions
*/
	uw_ntdnprompt,	   /* UG_DDNPROMPT	-- take down text prompt */
	ug_noop,	/* UG_DMENU	 put up menu */
	ug_noop,          /* UG_DDNMENU  take down menu */
	ug_noop,          /* UG_DRASPNTS -- take down menu */
/*
... aak aug-7-1998: new output primitive
*/
	ug_noop,   /* UG_DSHADEAREA: 3-d shaded area */ 
/*
.....added for forms
*/
	uw_ntform,         /* UW_FORM */
	uw_ntreset_list,   /* UW_SET_LIST */
	uw_ntget_field,    /* UW_GET_FIELD */
	uw_ntclose_form,  /* UW_CLOSE_FORM */
	uw_ntget_filename,    /* UW_GET_FILENAME */
	uw_ntmd_filename,      /* UW_GET_FNAME_ADV */
	ug_noop,      /* UW_OPEN_WINDOW */
	ug_noop,		/* UW_WIN_OUT	*/
	ug_noop,	/* UW_CLOSE_WINDOW */
	ug_noop,					/* UW_APP_EXIT */	
	uz_load_accel,		/* UZ_LOAD_ACCEL */
	uw_ntevent,			/* UW_EVENT */
	ug_noop,		/* UW_OPEN_POCKET */
	ug_noop,		/* UW_CLOSE_POCKET */
	ug_noop,			/* UW_SIGNON_LOAD */
	uw_nterror,				/* UW_ERROR_MSG */
	uw_ntdown_menu,			/* UW_DOWN_MENU */
	ug_noop,			/* UW_SIGNOFF */
	ug_noop,		/* UW_PRINT_SCREEN */
	ug_noop,		/* UW_LOAD_LAYOUT */
	ug_noop,		/* UW_SAVE_LAYOUT */ 
	ug_noop,		/* UW_MENU_DESIGN */
	ug_noop,                    /* UW_VIEW_SEG */
	ug_noop,             /* UW_DEL_CUTSEG */
	ug_noop,              /* UW_ERASE_CUTSEG */
	ug_noop,              /* UW_RESET_CUTSEG */
	ug_noop,               /* UW_OPEN_CUTSEG */
	ug_noop,              /* UW_POSTN_CUTSEG */
	ug_noop,              /* UW_CLOSE_CUTSEG */
	ug_noop,                   /* UW_GETSURF */
	ug_noop,                   /* UW_SETSURF */
	ug_noop,                 /* UW_GRAPHSURF */
	ug_noop,                   /* UW_POPSURF */
	ug_noop,                   /* UW_CLEAR_VP */
	ug_noop,                             /* UW_FLUSH */  
	ug_noop,                            /* UW_HILITE */
	ug_noop,			/* UW_FORM_CKDATA , not used now */
	uw_ntprmerr,				/* UW_PRMERR */
	uw_ntwrprm,					/* UW_WRPRM	*/
	uw_ntwrstat,				/* UW_WRSTAT */
	uw_ntmenu,					/* UW_MENU */
	uw_ntreset_prompt,			/* UW_RESET_PROMPT */
	uw_ntwindow,				/* UW_WINDOW */
	ug_noop,				/* UW_SIGNON */
	ug_noop,                       /* UW_GET_CLIP */
	ug_noop,                       /* UW_SET_CLIP */
	ug_noop,                           /* UW_DYNDRAW */
	ug_noop,                     /* UW_GET_DEPMASK */
	ug_noop,                        /* UW_SET_DEPMASK */
	ug_noop,                       /* UW_GET_WSSHADE */
	ug_noop,
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
	ug_noop,			/* UG_RPPICK_SEG */
	uw_ntget_dirname,			/* UW_GET_DIRNAME */
	uw_ntdspfrm_invis,			/* UW_DSPFRM_INVIS */
	uw_ntdspfrm_vis	,		/* UW_DSPFRM_VIS */
	uw_ntyes_no_cancel,		/* UW_YESNOCANCEL */
	uw_ntdisfrm_set_label,		/* UW_DISPFRM_SET_LABEL */
	uw_ntdisfrm_set_butlabel,		/* UW_DISPFRM_SET_BUTLABEL */
	ug_noop,		/* UW_MARKER_SIZE */
	uw_ntupd_input,			/* UW_UPDATE_INPUT */
	uw_ntget_focusfrm,	/* UW_GET_FOCUSFRM */
	
	uw_ntfrm_settlist,			/* UW_DISPFRM_SET_TLIST */
	uw_ntfrm_sorttable,			/* UW_FORM_SORTTABLE */
	uw_ntsetwin_title,			/* UW_SETWIN_TITLE */
	uw_ntset_sort_func,			/* UW_SETFORM_TFUNC */
	ug_noop,				/* UW_NEW_SESSION */
	ug_noop,			/*UW_CHKKEY_COM */
	uw_ntdown_menunum,          /* UG_DDNMENUNUM  take down menu */
	uw_ntfrm_set_focus,			/* UW_DISPFRM_SET_FOCUS  set focus of a form field */
	uw_ntfrm_setcolor                     /* UW_DISPFRM_SET_COLOR */
};
