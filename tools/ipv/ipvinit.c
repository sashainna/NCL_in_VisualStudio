/*********************************************************************
**  NAME:  ipvinit.c
**
**      NCLIPV: initial functions section.
**      define UG_wsenttbl wsgl used for NCLIPV
**
**		CONTAINS:
**			nclipv_glinit
**			nclipv_setf_val
**			nclipv_init_auth
**			nclipv_update_win_title
**			nclipv_restart
**			nclipv_setin
**			nclipv_setmm
**       nclipv_parse_cmd
**       nclipv_signon
**
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			ipvinit.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			09/25/18 , 10:31:26
*********************************************************************/
#include <stdio.h>
#include <math.h>
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
#include "mfort.h"
#include "nclver.h"
#include "lipv.h"
#include "lipvmach.h"
#include "lcom.h"
#include "mdunits.h"

int uw_glhicolor, uw_glvrfcolor;
extern int UL_nclipv_print;
extern int NAUTMACH, NAUTIPV;
extern UX_pathname NCL_tpfile;
char *moindx[] = {"JAN","FEB","MAR",
					"APR","MAY","JUN",
					"JUL","AUG","SEP",
					"OCT", "NOV","DEC"};

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

float uw_form_scalex;
float uw_form_scaley;
int XBorder[2],YBorder[2];
int Border_stat;
extern int UL_ipv;

static int Sipv_input = -1;
static int Sunits = -1;
static UX_pathname Sipv_mdffil = "";
static UX_pathname Sipv_cutfil = "";
static UX_pathname Sipv_stkfil = "";
static UX_pathname Sipv_wipfil = "";
static UX_pathname Sipv_machfil = "";
static UX_pathname Sipv_inpfil = "";

void nclipv_setin(), nclipv_setmm();
/*********************************************************************
**    I_FUNCTION :  nclipv_setf_val
**       set nclipv ifl/ifi fortra initial value
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclipv_setf_val()
{
	UM_int2 inx, ival;
	UM_real8 val;
	int kunit;
	char *p,*ux_getenv();
/*
.....set default value
*/
	getver(&val);
	inx = 169;
	setscv(&inx, &val);
	inx = 119;
	setscv(&inx, &val);
/*
......get the initial UNIT value from U_UNITS
*/
	if (Sunits == -1)
	{
		p = ux_getenv("U_UNITS",UX_NPRTERRS);
		if (p != UU_NULL)
		{
			if (ud_strcomp(p, "MM") == -1 || (ud_strcomp(p, "mm") == -1))
			{
				um_setunits(UM_MM);
				kunit=1;
			}
			else
			{
				kunit = 0;
				um_setunits(UM_INCH);
			}
		}
		else
			Sunits = 0;
	}

	if (Sunits == 0)
	{
		kunit = 0;
		um_setunits(UM_INCH);
	}
	else
	{
		kunit = 1;
		um_setunits(UM_MM);
	}
	nclxini (&kunit, &val);

	inx = 134;
	val = .25;
	setscv(&inx, &val);
	inx = 106;
	val = 1.;
	setscv(&inx, &val);
	inx = 170;
	val = 5.0e-8;
	setscv(&inx, &val);
	inx = 175;
	val = .005;
	setscv(&inx, &val);
	inx = 35;
	ival = 0;
	setifl(&inx, &ival);
	if (kunit==0)
		nclipv_setin();
	else
		nclipv_setmm();
}

/*********************************************************************
**    E_FUNCTION     : nclipv_setin()
**       Set the current input units to INCH
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclipv_setin()
{
	UM_int2 inx, ival;
	UM_real8 val;

	inx = 264;
	ival = 0;
	setifl(&inx, &ival);
	inx = 91;
	val = .001 * 2.0;
	setscv(&inx, &val);
	inx = 92;
	val = cos(1.0/57.2957795130823215);
	setscv(&inx, &val);
	inx = 105;
	val = 4.;
	setscv(&inx, &val);
	um_setunits(UM_INCH);
	ival=0;
	setsc(&ival);
}

/*********************************************************************
**    E_FUNCTION     : nclipv_setmm()
**       Set the current input units to MM
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclipv_setmm()
{
	UM_int2 inx, ival;
	UM_real8 val;

	inx = 264;
	ival = 1;
	setifl(&inx, &ival);
	inx = 91;
	val = .025 * 2.0;
	setscv(&inx, &val);
	inx = 92;
	val = cos(1.0/57.2957795130823215);
	setscv(&inx, &val);
	inx = 105;
	val = 100.;
	setscv(&inx, &val);
	um_setunits(UM_MM);
	ival=1;
	setsc(&ival);
}

/*********************************************************************
**    I_FUNCTION :  nclipv_glinit(prms,reply) ----------
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

int nclipv_glinit(prms,reply)
Swid *prms;
UG_rwsinit *reply;      /* struct {Gstatus stat;UG_wdt *wdtptr;} *reply */
{
/*
.....Variable definitions
*/
	int i,maxx,maxy;
	static int onoff = 0;  /* turn on/off X Synchronization */
	char *p,*ux_getenv(),*vptr, buf[80];
	double atof();

	uw_form_scalex = 1.0;
	uw_form_scaley = 1.0;
	p = ux_getenv("UW_FORM_SCALEX", UX_PRTERRS);
	if (p != 0) uw_form_scalex = (float) atof(p);
	p = ux_getenv("UW_FORM_SCALEY", UX_PRTERRS);
	if (p != 0) uw_form_scaley = (float) atof(p);
	uw_form_scalex = (float) (uw_form_scalex * 1.0);
	uw_form_scaley = (float) (uw_form_scaley * 1.0);
	
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
	UL_nclipv_print = 0;
	if ((vptr = ux_getenv("UL_NCLIPV_PRINT")))
	{
		strcpy(buf,vptr);
		if (strcmp(buf,"1") == 0) UL_nclipv_print = 1;
		else UL_nclipv_print = 0;
	}
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
	nclipv_setf_val();
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     :  int nclipv_init_auth()
**       Verify calling routine is authorized to run on this terminal
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          msg: error message
**    RETURNS      : none
**    SIDE EFFECTS : ???
**    WARNINGS     : none
*********************************************************************/
int nclipv_init_auth(msg)
char *msg;
{
	char rel[20], lmsg[80];
	int iop[8], ierr;
	int d,mo,yr;
/*
.....Get current version as date string
*/
	sscanf(last_release,"%d/%d/%d", &mo, &d, &yr);
	if (yr < 90) 
		yr = 2000+yr;
	else 
		yr = 1900+yr;
	sprintf(rel,"%d-%s-%d\0", d, moindx[mo-1], yr);
/*
.....Authorize NCLIPV
*/
	ierr = 0;
#ifdef UU_IPV
	pwdaut("NCLIPV","SIMULATE,MACHINE",rel,iop,lmsg,&ierr);
	if (ierr == 0)
	{
		if (iop[0] == 1)
		{
			NAUTIPV = 1;
		}
		if (iop[1] == 1)
		{
			NAUTMACH = 1;
		}
	}
/*
.....Could not authorize
*/
	else
	{
		sprintf(msg, "NCLIPV: %s\nExiting.\n", lmsg);
	}
#endif
	return(ierr);
}
/*********************************************************************
**    E_FUNCTION     : nclipv_update_win_title
**       updated the window title
**    PARAMETERS
**       INPUT  : 
**			none
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclipv_update_win_title()
{
	char title[500], tmpstr[500], ttmpstr[500];
	UX_pathname tmpfile, dir,fname;
	char sfile[256], numstr[20], *indx, pathstr[2], pptmp[20];
	double ver;
	int i, tmpnum, path = 1, pnum = 40, unum = 40;

	pathstr[0] = UX_PATH_SEP;
	pathstr[1] = '\0';

	getver(&ver);

	if (UD_window_title[0]=='\0')
	{
		sprintf(title,"NCLIPV V%7.2f",ver);
		goto set_title;
	}
	strcpy(title, UD_window_title);
	strcpy(ttmpstr, title);
	strcpy(tmpstr, title);

	sprintf(pptmp, "%%PATH%s%%PARTPGM", pathstr);
	indx = (char*) strstr(tmpstr, pptmp);
	if (indx==NULL)
	{
		strcpy(tmpstr, title);
		indx = (char*) strstr(tmpstr, "%PARTPGM");
		path = 0;
	}
	if (indx!=NULL)
	{
		tmpnum = indx - tmpstr;
		strncpy(title, ttmpstr, tmpnum);
		title[tmpnum] = '\0';
		if (path==0)
			indx = indx + 8;
		else
			indx = indx + 14;
		i = 0;
		while ((*indx!='%')&&(*indx!=' ')&&(*indx!='\0')&&(*indx!='\t')
				&& (*indx!='\r') && (*indx!='\n')) 
		{
			numstr[i++] = *indx;
			indx++;
		}
		numstr[i] = '\0';
		strcpy(tmpfile,NCL_tpfile);
/*
.....if there is no %PATH in layout file, never display path info
*/
		ul_break_fname(tmpfile,dir,fname);
		if (fname[0]!='\0')
		{
			if (path==0)
			{
				strcpy(tmpfile, fname);
			}
			else
			{
				if (dir[0]=='\0')
				{
					_getcwd(dir, UX_MAX_PATH_LEN);
				}
				ul_build_full_fname(dir, fname,NULL,tmpfile);
			}
			if (i!=0)
			{
				pnum = atoi(numstr);
/*
......short filename
*/
				ul_short_filename(tmpfile,sfile,pnum);
			}
			else
				strcpy(sfile, tmpfile);
		}
		else
			sfile[0] = '\0';
		if (NCL_tpfile[0]!='\0')
			strcat(title, sfile);
		if (*indx!='\0')
			strcat (title, indx);
	}
	strcpy(ttmpstr, title);
	strcpy(tmpstr, title);
	indx = (char*) strstr(tmpstr, "%VERSION");
	if (indx!=NULL)
	{
		tmpnum = indx - tmpstr;
		strncpy(title, ttmpstr, tmpnum);
		title[tmpnum] = '\0';
		sprintf(numstr, "%7.2f", ver);
		strcat(title, numstr);
		if (*(indx+8)!='\0')
			strcat (title, (char*)(indx+8));
	}
	strcpy(ttmpstr, title);
	strcpy(tmpstr, title);

	indx = (char*) strstr(tmpstr, "%PROGRAM");
	if (indx!=NULL)
	{
		tmpnum = indx - tmpstr;
		strncpy(title, ttmpstr, tmpnum);
		title[tmpnum] = '\0';
		strcat(title, "NCLIPV");
		if (*(indx+8)!='\0')
			strcat (title, (char*)(indx+8));
	}
set_title:;
	uw_ntsetwin_title(title);
}

/*********************************************************************
**    E_FUNCTION     : nclipv_restart
**       restart NCLIPV
**    PARAMETERS
**       INPUT  : 
**			none
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclipv_restart()
{
	if (ud_yesno(0, "Are you sure you want to Restart NCLIPV?",
							"Restart NCLIPV"))
	{
/*
.....Reset the Unibase & NCL
*/
		LW_auto_reset = 1;
		nclipv_setf_val();
		NCL_tpfile[0] = '\0';
		ul_ipv_terminate();
		ul_init_axisseg();
		UL_ipv = 1;
		ul_verify_reset();
		clinit();
		ncl_cutter_reset();
		ul_ipv_start();
		ul_ipv_load_dummy();
		nclipv_update_win_title();
	}
}
/*********************************************************************
**    E_FUNCTION     : nclipv_parse_cmd(cmdline, errmsg, &err)
**       parse the command line
**    PARAMETERS
**       INPUT  : 
**			cmdline: command line to be parsed
**       OUTPUT :
**          errmsg: error message if err
**			err: 0: no err
**					1: error
**    RETURNS      :
**          0: success and continue
**			1: don't continue success or not
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclipv_parse_cmd(cmdline, errmsg, err)
char *cmdline, *errmsg;
int *err;
{
	int i, j, file_start, len, opt_end;
	char *indx, filen[UX_MAX_PATH_LEN], tmpstr[80], tmpstr2[80], helpmsg[1500];
	*err = 0;
	if (cmdline[0]=='\0')
		return 0;
	filen[0] = '\0';
	len = strlen (cmdline);
	tmpstr[0] = '\0';
	file_start = 0;
	opt_end = 0;
	Sunits = -1;
	for (i=0, j=0; i<len; i++)
	{
		if (((cmdline[i]==' ')||(cmdline[i]=='\t'))
			&&(file_start==0))
		{
			if (tmpstr[0]=='\0')
				continue;
			else
/*
......the option string end
*/
			{
				tmpstr[j] = '\0';
				opt_end = 1;
			}
		}
		else if (cmdline[i]=='\"')
		{
			if (file_start==1)
			{
				file_start = 0;
				opt_end = 1;
			}
			else
				file_start = 1;
			tmpstr[j++] = cmdline[i];
		}
		else
			tmpstr[j++] = cmdline[i];
		if (i==len-1)
		{
			opt_end = 1;
		}
/*
......parse the option
*/
		if (opt_end)
		{
			tmpstr[j] = '\0';
			opt_end = 0;
			if (file_start)
			{
				strcpy (errmsg, "Double quotes do not match!");
				*err = 1;
				return 0;
			}
			strcpy(tmpstr2, tmpstr);
			ul_to_upper(tmpstr2);
			if (strncmp(tmpstr2,"-MCD:", 5)==0)
			{
				strcpy(Sipv_mdffil, &(tmpstr[5]));
				Sipv_input = 1;
			}
			else if (strncmp(tmpstr2,"-CUT:", 5)==0)
			{
				strcpy(Sipv_cutfil, &(tmpstr[5]));
				ul_remove_quotes(Sipv_cutfil);
			}
			else if (strncmp(tmpstr2,"-STK:", 5)==0)
			{
				strcpy(Sipv_stkfil, &(tmpstr[5]));
				ul_remove_quotes(Sipv_stkfil);
			}
			else if (strncmp(tmpstr2,"-WIP:", 5)==0)
			{
				strcpy(Sipv_wipfil, &(tmpstr[5]));
				ul_remove_quotes(Sipv_wipfil);
			}
			else if (strncmp(tmpstr2,"-MACH:", 6)==0)
			{
				strcpy(Sipv_machfil, &(tmpstr[6]));
				ul_remove_quotes(Sipv_machfil);
			}
			else if (strncmp(tmpstr2,"-UNITS:", 7)==0)
			{
				if (strncmp(&tmpstr2[7],"IN",2)==0)
					Sunits = 0;
				else if (strncmp(&tmpstr2[7],"MM",2)==0)
					Sunits = 1;
			}
			else if (strcmp(tmpstr2,"-?")==0)
			{
/*
.......display help message
*/
				strcpy(helpmsg, "NCLIPV will support the following command line.\r\n");
				strcat(helpmsg, "\r\n");
				strcat(helpmsg, "\tnclipv file [-MCD:n] [-CUT:cutter_file] [-STK:stock_file] [-WIP:session_file] [-MACH:machine] [-UNITS:un]\r\n");
				strcat(helpmsg, "\tWhere:\r\n");
				strcat(helpmsg, "\t\tfile = Name of input file.  The file can be placed anywhere on the command line,\r\n");
				strcat(helpmsg, "\t\tbefore the options, after the options, in the middle of the options.  If spaces\r\n");
				strcat(helpmsg, "\t\tare in the filename, then it should be enclosed in double quotes.\r\n");
				strcat(helpmsg, "\r\n");
				strcat(helpmsg, "\t\t-MCD:n = Specifies that the input file is an MCD (.pu1) file and will be\r\n");
				strcat(helpmsg, "\t\tconverted using Pted prior to loading it.  n is the MDF file to use when\r\n");
				strcat(helpmsg, "\t\tconverting it.\r\n");
				strcat(helpmsg, "\r\n");
				strcat(helpmsg, "\t\t-CUT:cutter_file = Specifies the name of the cutter definition file to use\r\n");
				strcat(helpmsg, "\t\twhen converting an MCD file using Pted.\r\n");
				strcat(helpmsg, "\r\n");
				strcat(helpmsg, "\t\t-STK:stock_file = Specifies a default stock/fixture file (.stk) to load when starting.\r\n");
				strcat(helpmsg, "\r\n");
				strcat(helpmsg, "\t\t-WIP:session_file = Specifies the name of a saved NCLIPV session file to load on\r\n");
				strcat(helpmsg, "\t\tstartup.  If a session file is specified, then the stock file (-stk) is ignored.\r\n");
				strcat(helpmsg, "\r\n");
				strcat(helpmsg, "\t\t-MACH:machine = Name of machine to load on startup.  If a machine is specified\r\n");
				strcat(helpmsg, "\t\tthen Machine Simulation is automatically enabled.\r\n");
				strcat(helpmsg, "\r\n");
				strcat(helpmsg, "\t\t-UNITS:un = Specifies the input units, either INCHES or MM.\r\n");
				ncl_cmdmsg(helpmsg);
				return 1;
			}
			else 
			{
				strcpy(Sipv_inpfil, tmpstr);
				ul_remove_quotes(Sipv_inpfil);
				if (Sipv_input==-1)
					Sipv_input = 0;
			}
			file_start = 0;
			opt_end  = 0;
			tmpstr[0] = '\0';
			j = 0;
		}
	}
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : nclipv_signon()
**       Processes the NCLIPV Signon.
**    PARAMETERS
**       INPUT  : 
**			none
**       OUTPUT :
**			none
**    RETURNS      :
**			none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclipv_signon()
{
	UX_pathname filename;
	LW_mach_data_struc mach_desc;
	char lbuf[UX_MAX_PATH_LEN+40];
	int nmodel, nsolid, spindle[10], ntool, pinaxis, idn[2],ierr,status;
	char apin[80];
	char lmsg[80];
	UM_coord mpin[3];
	UU_LIST model, solid,      tools;
	UM_int2 ifl, ival;

	FILE *fd;
	int dummy = 1;

/*
.....The setunit function in nclipv_setf_val
.....at beginning don't set UM_cpln.length_to_cm right
.....because we initial UM_cpln.length_to_cm and
.....UM_cpln.conv_factors value in 
.....function um_init_cpln after we called nclipv_glinit.
.....So we will set unit aagin.
*/
	ifl = 264;
	getifl(&ifl,&ival);
	if (ival == 0) 
		nclipv_setin();
	else
		nclipv_setmm();
/*
......this should not initial here since the OnDraw/OnSize of window
......already call paint fucntion which used those value and create
......the axis segment, we can't simple set them to 0 without delete the old
......one, move this function call to CMainFrame::initial_nclipv()
	ul_init_axisseg(); */
/*
.....this value should not initial here after read *.mod file
.....it is init UW_browse_dir = 0 in linit.c, if we need to init
.....nclipv application to UW_browse_dir = 1; it should put the
.....code at beginning of nclipv code, not here. See no need to do now.
*/
/*	UW_browse_dir = 1; */
	ul_ipv_load_dummy();
	if (Sipv_wipfil[0]!='\0')
	{
/*
......load session file
*/
		dummy = 0;
		ul_ipv_load_session(Sipv_wipfil, UU_TRUE); 
	}
	else if (Sipv_stkfil[0]!='\0')
	{
/*
......load stock file
*/
		dummy = 0;
/*
.....Open file for reading
*/
		strcpy(filename, Sipv_stkfil);
		status = ux_fopen0(Sipv_stkfil,"r",&fd);
		if (status != UU_SUCCESS) goto filerr;
/*
.....Process file
*/
		idn[0] = idn[1] = 0;
		status = ul_ipv_load_stock_cmds(fd, UU_TRUE,&idn,UU_NULL,UU_NULL);
		if (status != UU_SUCCESS) goto readerr;
		ux_fclose0(fd);
	}
	if (Sipv_machfil[0]!='\0')
	{
/*
......load machine file
*/
		dummy = 0;
/*
.....Allocate a Machine Simulation user
*/
		if (LW_session[0] != 0 && !LW_mach_defined)
		{
			pwdall("SIMULATE",lmsg,&ierr);
			if (ierr != 0) status = UU_FAILURE;
		}
/*
.....Load the machine parameters
*/
		if (status == UU_SUCCESS)
			status = ul_ipv_load_mach(LW_mach_dir, Sipv_machfil, filename, &mach_desc,
				&model,&solid,&nmodel,&nsolid, spindle, mpin, &pinaxis, apin);
				
		if (status == UU_SUCCESS)
		{
			if (nmodel > 0)
			{
				ul_ipv_free_mach(&LW_mach_model, &LW_mach_nmodel,
							&LW_mach_solid,&LW_mach_nsolid);
				uu_move_byte(&model,&LW_mach_model,sizeof(UU_LIST));
				uu_move_byte(&solid,&LW_mach_solid,sizeof(UU_LIST));
				uu_move_byte(&mach_desc,&LW_mach_data,sizeof(LW_mach_data_struc));
				LW_mach_nmodel = nmodel;
				LW_mach_nsolid = nsolid;
			}
			if (LW_session[0] != 0 || LW_active)
			{
				if ((LW_session[0] == 0)||(LW_nclipv==LW_STANDALONE)) 
					LW_mach_simul = UU_TRUE;
				ul_ipv_reset_session();
			}
		}
		else 
			LW_mach_simul = UU_FALSE;
	}
	if (Sipv_input==0)
	{
/*
......input file is CL file, load toolpath
*/
		nclu_load_clfile(Sipv_inpfil,1);
		if (UN_clfile)
		{
			strcpy(NCL_tpfile, Sipv_inpfil);
			ud_update_win_title();
		}
	}
	else if (Sipv_input==1)
	{
/*
......input file is MCD file, load toolpath
*/
		nclu_load_mcd(Sipv_inpfil, Sipv_mdffil, Sipv_cutfil);
	}
	ul_ipv_view_segs();
	goto done;
/*
.....Could not open file
*/
filerr:;
	sprintf(lbuf, "Could not open %s.", filename);
	ud_wrerr(lbuf);
	goto done;
/*
.....Error reading from file
*/
readerr:;
	sprintf(lbuf,"Error reading from %s.", filename);
	ud_wrerr(lbuf);
	ux_fclose0(fd);
/*
.....End of routine
*/
done:;
	return;
}

int uw_ntmenu(), uw_ntdown_menu();
int uw_glwind3(), uw_glvport3(),uw_glnormtran(), uw_glwswind(),uw_glwsvport(),
	 uw_glvref3(),  uw_glvpn3(), uw_glvup3(), uw_glmodxf(), uw_glndctodev(),
	uw_gldevtondc();

int uw_ntform_vis(), uw_ntform_invis(), uw_ntform_display(), uw_ntclose_dispfrm(), 
	uw_ntget_frmfield(), uw_ntfrm_setlist(), uw_ntupdate_form(), uw_ntdspfrm_invis(),
	uw_ntdspfrm_vis(), uw_ntdisfrm_set_label(), uw_ntdisfrm_set_butlabel(),
	uw_ntform(), uw_ntreset_list(), uw_ntget_field(), uw_ntclose_form(), uw_ntupd_input(),
	uw_ntget_focusfrm();
int uw_ntget_dirname(), uw_ntyes_no_cancel(), uw_ntget_filename(), uw_ntmd_filename(),
	uw_nterror();
int uw_gltrk(), uw_ntmenu_choice(), uw_ntstr_prompt(), uw_ntdnprompt(),
	uw_ntevent(), uw_ntprmerr(), uw_ntwrprm(), uw_ntwrstat(), uw_ntreset_prompt(),
	uw_ntwindow(), uw_ntyes_or_no(), uw_ntbcomlin(), uw_ntecomlin(), ug_noop();
int uw_glinitloc(), uw_glreqloc(), uw_ntkbd();
int uw_ntfrm_settlist(), uw_ntfrm_sorttable(),
	uw_ntsetwin_title(), uw_ntset_sort_func(), uw_ntdown_menunum();
int uw_glprint_screen();
int uw_ntfrm_set_focus(), uw_ntfrm_setcolor(), uw_check_event();

int uz_load_accel();

int ug_dawaitloc(), ug_dawaitval(), ug_dawaitstr(), ug_dreqstr();

UG_wsenttbl wsgl={ 
/*
.......Workstation manipulation routines
*/
	nclipv_glinit,      /* UG_DOPENWS   -- open workstation */ 
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
	ug_dreqstr,     /* UG_DREQSTRING-- request string */
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
	ug_dawaitloc,   /* UG_DAWAITLOC -- await locator */
	ug_dawaitval,   /* UG_DAWAITVAL -- await valuator */
	ug_dawaitstr,   /* UG_DAWAITSTRING-- await string */
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
	uw_ntkbd,	/* UG_DKBD	-- get keyboard */
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
	uw_glprint_screen,		/* UW_PRINT_SCREEN */
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
	uw_ntget_focusfrm,		/* UW_GET_FOCUSFRM */
	uw_ntfrm_settlist,			/* UW_DISPFRM_SET_TLIST */
	uw_ntfrm_sorttable,			/* UW_FORM_SORTTABLE */
	uw_ntsetwin_title,			/* UW_SETWIN_TITLE */
	uw_ntset_sort_func,			/* UW_SETFORM_TFUNC */
	ug_noop,				/* UW_NEW_SESSION */
	ug_noop,			/*UW_CHKKEY_COM */
	uw_ntdown_menunum,          /* UG_DDNMENUNUM  take down menu */
	uw_ntfrm_set_focus,			/* UW_DISPFRM_SET_FOCUS  set focus of a form field */
	uw_ntfrm_setcolor,                     /* UW_DISPFRM_SET_COLOR */
	uw_check_event			/*UW_CHKWIN_EVENT */
};
