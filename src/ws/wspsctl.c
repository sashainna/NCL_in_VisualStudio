
/*********************************************************************
**    NAME         :  wspsctl.c
**    CONTAINS:
**		uw_psinit
**		uw_psterm
**		uw_psactws
**		uw_psdeact
**		uw_psredrawws
**		uw_psfunc
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       wspsctl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:14
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#define WSPS_FIRST
#include "wsps.h"
#undef WSPS_FIRST
#include "ws.h"
#include "tplot.h"
#include "xenv1.h"
#if UU_COMP == UU_WIN2K
#include <io.h>
#include <errno.h>
#endif

Gsps uw_ps;	
int	 uw_psind;
static struct {Gint op; Gws ws; Gdrect3 wsvport;} coprm; 
char uu_ttline(), uu_ttget(), uu_tttest();
int	 uw_psipt;
extern int  wsplotdx, wsplotdy;
extern int  pts; 
static Gint ltypes[2]={1,2};
static Gint mktypes[2]={1,2};
static Gtxfp fonprecs[2]={1,UG_STRING,1,UG_STROKE};	
static Gpet choicepets[NUMPETS]={3,5,21,22,25};	
static Gchar defchostr[2][2]={" "," "};	
static Gchar *defchoptr[2]={&defchostr[0][0],&defchostr[1][0]};
static Gdefchoice defcho[NCHDEV]={UW_DEFCHO};

static Gpet locpets[1]={1};	
static Gdefloc defloc[1]={0.,0.,1,locpets,0.,0.,1.,1.,};				
static Gpet pickpets[1]={1};	
static Gdefpick defpick[1]={1,pickpets,0.,0.,1.,1.,	" "};
static Gpet stringpets[6]={1,22,22,22,22,22};
static Gdefstring defstring[6]={
						{UW_DEFSTR},
						{UW_DEFSTR},
						{UW_DEFSTR},
						{UW_DEFSTR},
						{UW_DEFSTR},
						{UW_DEFSTR}
						};
static Gpet strokepets[1]={1};	
static Gdefstroke defstroke[1]={1,1,strokepets,0.,0.,1.,1.,	};					
static Gpet valpet1[1]={1};		/* valuator 1 echotype */
static Gpet valpet21[1]={21};		/* valuator 21 echotype */
static Gdefval defval[2]={0.,1,valpet1,0.,0.,1.,1.,	/* default val data */
						100.,0.," ",	/* default val data record */
						0.,1,valpet21,0.,0.,1.,1.,100.,0.," "};
static Gcobundl colrbundl[256]={				/* color bundle table */
				.5,.5,.6,							/* 0:, background, light blue */
				1.,1.,1.,							/* 1:, white */
				0.,0.,0.,							/* 2: black */
				0.,1.,0.,							/* 3: green */
				0.,0.,1.,							/* 4: blue */
				0.,1.,1.,							/* 5: cyan */
				1.,0.,0.,							/* 6: red */
				1.,1.,0.,							/* 7: yellow */
				1.,0.,.83};							/* 8: magenta */
static Glighttbl litetbl[5]= {
			{ UG_AMBIENT, 0.,0.,0.,0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. },
			{ UG_AMBIENT, 0.,0.,0.,0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. },
			{ UG_AMBIENT, 0.,0.,0.,0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. },
			{ UG_AMBIENT, 0.,0.,0.,0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. },
			{ UG_AMBIENT, 0.,0.,0.,0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. } };

static UG_outwdt outwdt={				/* output wdt stuff */
					UG_RASTER,				/* workstation type */
					UG_IMM,UG_IMM,				/* sturcture content, view rep mod */
					{						/* Gmodws - mod of workstation attributes */
						UG_IRG,UG_IRG,UG_IRG,UG_IRG,UG_IRG,UG_IRG
					},
					UG_BNIG,				/* default deferral mode */
					UG_NIVE,				/* default modification mode */
					{						/* polyline facilities */
						2,ltypes,		/* no. and list of line types */
						1,					/* one line width */
/**need change?**/		.01,.01,.01,	/* nom, min, max width */
						0					/* number predefined polyline bundles */
					},
					NULL,					/* array of predefined polyline bundles */
					{						/* polymarker facilities */
						2,mktypes,		/* number and list of marker types */
						1,					/* number of marker sizes */
						.01,.01,.01,	/* nom, min, max marker size */
						0					/* no predefined polymarker bundles */
					},
					NULL,					/* array of predefined polymarker bundles */
					{						/* text facilities */
						2,					/* number of font/precision pairs */
						fonprecs,		/* list of font/precision pairs */
						1,.01,.01,.01,	/* number, nom, min,max char ht */
						1,1.,1.,1.,		/* number, nom, min, max expansion factor */
						0					/* no. predefined text bundles */
					},
					NULL,					/* pointer to predefined text bundles */
					{						/* fill area facilities */
						0,NULL,			/* no. and list of of interior styles */
						0,NULL,			/* number and list of hatch styles */
						0					/* number predefined fill area bundles */
					},
					NULL,					/* pointer to predefined fill area bundles */
					{0,0},				/* pattern facilities */
					NULL,					/* ptr to predefined pattern bundles */
					{256,UG_COLOUR,16},	/* no. colors, color available,no. bundles  */
					colrbundl,			/* pointer to color predefined bundles */
					{0,NULL},			/* GDP facilities */
					NULL,					/* pointer to array of GDPs */
					0,0,0,0,0,0,0,		/* maximum numbers */
					UG_IRG,UG_IRG,UG_IRG,	/* dyn mod of seg cont, prio, unpost */
					UG_IRG,UG_IRG,		/* dynamic mod of add, delete prims to seg */
					5, litetbl		/* no. and definition of light sources */
					};				/* end of outwdt */
static UG_inwdt inwdt={				/* input WDT stuff */
					1,defloc,		/* number and array of LOC input devices */
					1,defstroke,	/* number and array of STROKE devices */
					2,defval,		/* number and array of VALUATOR devices */
					NCHDEV,defcho,		/* number and array of CHOICE devices */
					1,defpick,		/* number and array of PICK devices */
					6,defstring,	/* number and array of STRING devices */
					};					/* end of inwdt */
static UG_wdt wdt={	"sps",UG_OUTIN,	/* workstation type and category */
						UG_DC_METRES,			/* device units */
	    				0.8128, .5334,			/* display size in meters, 32*21 inch */
/*
.....we don't need set one than unit
.....it will affect we caculate the 
.....scale
.....Yurong 8/8/97
*/
						DEVXMAX,DEVYMAX,	/* display size in raster */
/*						DEVXMAX+1,DEVYMAX+1,	/* display size in raster */
						ROWMAX,COLMAX,		/* number rows,cols of text */
						20,					/* no definable viewing table indices */
						1,						/* need NDC box around segments */
						0,						/* UG_DSAVESCRN doesn't work */
						0,						/* Number of bit planes */
						&outwdt,				/* output wdt stuff */
						&inwdt};				/* input wdt stuff */




/*********************************************************************
**    I_FUNCTION :  uw_psfunc(buf,len)
**       write the buffer to a file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_psfunc(buf,len)
char	buf[];
int	len;
{
	int nc,stat;
	uu_denter(UU_GITRC,(us,"uw_psfunc,len=%d", len));
/*
.....Added logic to write 80 col records
.....Bobby  -  8/8/91
*/
	stat = ux_fwrite0(buf,len,sizeof(char),uw_ps.fid,buf,&nc);
	uu_dexit;
}	/* uw_psfunc */


/*-------------------------- control functions ----------------------------*/
/*-----------------------------------------------> UG_DOPENWS */
/*********************************************************************
**    I_FUNCTION :  uw_psinit(prms,reply)
**		perform any processing needed by the workstation hardware to get
**		it ready for use. If the device is accessed through an RS232 serial
**		line, use uu_ttopen, utp_ttputps, etc (described in tt.c) to
**		perform the I/O.  If all went OK, return with reply.stat=OK, else
**		set reply.stat=NONE. Also, set reply.wdtptr=address of the workstation
**		description table 
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          reply
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_psinit(prms,reply)
struct {Gint op; Gws wsid; 
		Gchar *conn;
		} *prms;
UG_rwsinit *reply;

{
	int	i,fid;
	int labsize;
	double atof();
	char	*p, *ux_getenv();
	char num[30];
	uu_denter(UU_GITRC,(us,"uw_psinit(ws=%d)",(*prms).wsid));
	for (i=1; i<NCHDEV; i++) defcho[i]=defcho[0];	/* initialize defcho array*/
	(*reply).stat = UG_OK;
	(*reply).wdtptr = &wdt;
	uw_ps.wid = (*prms).wsid;
	DISK_file = UU_FALSE;
	TEMP_file = UU_FALSE;
/*
.....allow user define DEVX, DEVY 
.....Yurong
*/
	p = ux_getenv("UU_PSDEVX",UX_PRTERRS);
	if (p!=NULL)
		wdt.dspsize.raster.x = (int)(atof(p)*wdt.dspsize.device.x/0.0254) ;
	p = ux_getenv("UU_PSDEVY",UX_PRTERRS);
	if (p!=NULL)
		wdt.dspsize.raster.y = (int)(atof(p)*wdt.dspsize.device.y/0.0254);
/*
.....we use default scale 0.24 as 300 pixel/inch since in PS default rate 1
.....mean 72 pixel/inch, that is too few.
.....Yurong
*/   
	psrate_x = 0.24 * ((double)(DEVXMAX)/(double)(wdt.dspsize.raster.x));
	psrate_y = 0.24 * ((double)(DEVYMAX)/(double)(wdt.dspsize.raster.y));
/* 
.....if the scale is not default to 0.24, the DPS_PLOTDX DPS_PLOTDY
.....need be changed corrispond to it
..... Yurong
*/
	wsplotdx = (int)(wsplotdx*(0.24/psrate_x));
	wsplotdy = (int)(wsplotdy*(0.24/psrate_y));
/*
.....Open port for plotting
*/
	uw_ps.ttfd = 0; 
	if (plotopts.diskfnm[0] == '\0' || plotopts.print == 1)
	{
		uw_ps.ttfd = 0;
#if UU_COMP != UU_WIN2K
		if (plotopts.print == 0) uw_ps.ttfd=uu_ttopen((*prms).conn,3);
/*
........Parallel port specified
........Create temporary file
........for copying or spooling
*/
		if (uw_ps.ttfd < 0 || plotopts.print == 1)
		{
			uw_ps.ttfd = 0; 
			tmpnam(TEMP_fnm);
			if ((ux_fopen0(TEMP_fnm,"w",&uw_ps.fid)) == UU_FAILURE)
			printf("Cannot create temporary file.\n");
			ux_fdopen0(fid,"w",&uw_ps.fid);
/*
.....Allow for NutCracker device names
*/
#if UU_COMP == UU_WINNT
			if (TEMP_fnm[0] = '/' && TEMP_fnm[2] == '=')
			{
				char buf[256];
				strcpy(buf,TEMP_fnm);
				TEMP_fnm[0] = buf[1];
				TEMP_fnm[1] = ':';
				strcpy(&TEMP_fnm[2],&buf[3]);
			}
#endif
			uu_ttstartsave(uw_ps.ttfd,uw_psfunc,0);
			DISK_file = UU_TRUE;
			TEMP_file = UU_TRUE;
		}
		else
		{
			PORT_id = uw_ps.ttfd;
			uw_ps.fid = UU_NULL;
			uu_ttsing(uw_ps.ttfd);			/* set single character mode */
		}
#else
		uw_ps.ttfd = -1;
		PORT_id = uw_ps.ttfd;
		uw_ps.fid = UU_NULL;
#endif
	}
	else
	{
		uw_ps.ttfd = 0;
		if ((ux_fopen0(plotopts.diskfnm,"w",&uw_ps.fid)) == UU_FAILURE)
		{
			printf("Cannot open file %s to write\n", plotopts.diskfnm);
			uw_ps.fid = UU_NULL;
		}
		else
		{
			uu_ttstartsave(uw_ps.ttfd,uw_psfunc,0);
			DISK_file = UU_TRUE;
		}
	}
/*
.....added for init PostScript reading
.....Yurong 9/1/98
*/
	if (uw_ps.fid!=UU_NULL)
		utp_ttputps(uw_ps.ttfd,"%!PS-Adobe-3.0\n",15);
	uw_ps.isrotate = UU_FALSE;				/* no ratation */
	uw_psind = 0;
/*
.....Initialize currently loaded pen, line type and line width
.....Yurong 8/4/98
*/
	uw_pspen(-1);
	uw_pslintype(-1);
	uw_pslinwid(-1);
	uw_psipt = 0;
/* 
...... Let user difine labal size
...... Yurong 12/12/96
*/
	p = ux_getenv("UU_PSLABSIZ",UX_PRTERRS);
	if ( p!=NULL) labsize = (int)atof(p);
	else labsize = 12; 
/*
.....Initialize line type, font size
.....Yurong  12/11/96
*/
	utp_ttputps(uw_ps.ttfd,"/Courier findfont\n",18);
	sprintf(num,"%d scalefont\n",labsize);
	utp_ttputps(uw_ps.ttfd, num, strlen(num));
	utp_ttputps(uw_ps.ttfd,"setfont\n",8); 
	utp_ttputps(uw_ps.ttfd,"0 setlinecap\n",13);
	utp_ttputps(uw_ps.ttfd,"2 setlinejoin\n",14);
	sprintf(num,"%f %f scale\n", psrate_x, psrate_y);
	utp_ttputps(uw_ps.ttfd, num, strlen(num));
/* 
.....Colormap	
*/
	uw_pscolormap();
#if UU_COMP!=UU_WIN2K
	if (DISK_file == UU_FALSE) uu_ttflin(uw_ps.ttfd);					/* flush io input buffer */
	if (DISK_file == UU_FALSE) uu_ttflin(uw_ps.ttfd);					/* flush io input buffer */
#endif
	uu_dexit;
	return 0;
}	/* uw_psinit */


/*********************************************************************
**    I_FUNCTION : uw_psterm(prms,reply) ------ UG_DCLOSEWS
**       Close the postscript plotting.
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          reply
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_psterm(prms,reply)				/* close workstation */
int prms[];								/* no input parameters */
int reply[];							/* no output parameters */
{
#if UU_COMP!=UU_WIN2K
	char buf[512];
#endif
	uu_denter(UU_GITRC,(us,"uw_psterm()"));
	if (uw_ps.fid!=UU_NULL)
	{
		utp_ttputps(uw_ps.ttfd,"showpage\n ",9);
		uu_ttflout(uw_ps.ttfd);
	}
#if UU_COMP!=UU_WIN2K
	if (DISK_file == UU_FALSE) uu_ttnorm(uw_ps.ttfd);
#endif
	if (uw_ps.fid!=UU_NULL)
	{
		uu_ttstopsave(uw_ps.ttfd);
#if UU_OPSYS!=UU_VMS && UU_OPSYS!=UU_ALPHAVMS
		uu_ttclos(uw_ps.ttfd,2);
#endif
/*
.....Close Disk file
*/
		if (DISK_file == UU_TRUE) ux_fclose0(uw_ps.fid);
	}
/*
.....Created temporary file
.....Get it to the output port
.....Then delete it
*/
#if UU_COMP!=UU_WIN2K
	if (TEMP_file == UU_TRUE)
	{
		if (plotopts.print == 1)
		{
			if (plotopts.printque[0] == '\0')
#if UU_COMP == UU_VAXVMS
				sprintf(buf,"print/delete %s",TEMP_fnm);
#else
				sprintf(buf,"lp -c %s",TEMP_fnm);
#endif
			else
			{
#if UU_COMP == UU_VAXVMS
				sprintf(buf,"print/delete/que=%s %s",plotopts.printque,TEMP_fnm);
#else
				sprintf(buf,"lp -c -d%s %s",plotopts.printque,TEMP_fnm);
#endif
			}
		}
		else
		{
#if UU_COMP == UU_VAXVMS
			sprintf(buf,"copy %s %s",TEMP_fnm,plotopts.port);
			remove(TEMP_fnm);
#else
			sprintf(buf,"cat %s > %s",TEMP_fnm,plotopts.port);
#endif
		}
		system(buf);
#if UU_COMP != UU_VAXVMS
		remove(TEMP_fnm);
#endif
	}
#endif
	uu_dexit;
	return 0;
}	/* uw_psterm */


/*********************************************************************
**    I_FUNCTION :  uw_psactws(prms,reply)				
**		This routine is called after the workstation is opened, but
**		before any other entry is called. Usually, no action is necessary 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_psactws(prms,reply)	
int prms[],reply[];

{
	uu_denter(UU_GITRC,(us,"uw_psactws()"));
	uu_dexit;
	return 0;
}	/* uw_psactws */



/*********************************************************************
**    I_FUNCTION :  uw_psdeact(prms,reply)					
**		This routine is called to close the workstation
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          reply
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_psdeact(prms,reply)				/* deactivate workstation */
int prms[],reply[];					/* no parameters */
{
	uu_denter(UU_GITRC,(us,"uw_psdeact()"));
	if (uw_ps.fid!=UU_NULL)
		uu_ttflout(uw_ps.ttfd);
	uu_dexit;
	return 0;
}	/* uw_psdeact */

/*-------------------------------------------------> UG_DREDRAWWS */
/*********************************************************************
**    I_FUNCTION :  uw_psredrawws(prms,reply)			
**		 redraw all segments on this workstation. 
**		 For workstations that don't maintain their own segment storage,
**			routines ug_dredrawvis and gdrecreseg are available to
**			redraw (and re-create) the segments 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_psredrawws(prms,reply)			/* redraw all segs on ws */
int prms[],reply[];				/* no parameters */

{
	ug_dredrawvis();
	return 0;
}

/*------------------------------------------------ external routines */
int ug_noop();

/* the following simulation routines in lib (wsdev.c, wstext.c ) */
int 	ug_dawaitchoice(), ug_dawaitpick(), ug_dawaitloc(), ug_dawaitval(), 
	ug_dawaitstr(), ug_dawaitev(), ug_dreqchoice(), ug_dreqpick(), ug_dreqloc(),
	ug_dreqval(), ug_dinitpick(),ug_dinitchoice(),ug_dreqstr(),
	ug_dinitloc(), ug_dinitval(),ug_dstrmode(),ug_dinitstr();

/* the following simulation routine is in gseg.c */
int ug_dseghilite();

/* the following simulation routines are in gout.c */
int ug_dpolyln(), ug_dpolyln3(), ug_dpolymk(), ug_dpolymk3();

/* the following simulatin routine is in gatts.c */
int ug_dcharheight();

int ug_dcharheight(),	ug_dpolylnras(),
		ug_dpolymkras(),
		ug_dseghilite();
int
		uw_pspolyln(),
		uw_psdrwop(),
		uw_pspntop(),
		uw_psrasline(),
		uw_pstext(),
		uw_psrastext(),
		uw_psflarearas(),
		uw_pswswind(),
		uw_pswsvport(),
		uw_psndctodev(),
		uw_psdevtondc(),
		uw_psraspnts(), 
		uw_pscircle(),
		uw_pscolormap();
/******* WORKSTATION ENTRY POINTS TABLE.  ********/
UG_wsenttbl wsps={ 
		uw_psinit		,	/*	UG_DOPENWS		/* open workstation */
		uw_psterm		,	/* UG_DCLOSEWS		/* close workstation */
		uw_psactws	,	/* UG_DACTWS		/* activate workstation */
		uw_psdeact	,	/* UG_DDEACTWS		/* deactivate workstation */
		ug_noop	,	/* UG_DCLEARWS		/* clear workstation */
		uw_psredrawws,	/* UG_DREDRAWWS	/* redraw all segs on ws */
		ug_noop		,	/* UG_DUPDATE		/* update workstation */
		ug_noop			,	/* UG_DCHGXFORM	/* a xform changed */
		ug_noop			,	/* UG_DSETDEFST	/* set deferral state */
		ug_noop		,	/* UG_DMSG			/* message */
		ug_noop			,	/* UG_DESCAPE		/* escape */
		ug_noop			,	/* UG_DCALL			/* call specific workstation */
		ug_noop			,	/* UG_DIMAGE		/* set imaging mode */
		ug_noop			,	/* UG_DINTRACTV	/* set interactive mode */
		ug_noop			,	/* UG_DHLHSRMODE	/* set hlhsr mode */
		ug_noop			,	/* UG_DSIGNAL		/* set interrupt handler */
		/* output primitives **/
		ug_dpolyln		,	/* UG_DPOLYLN			/* polyline */
		uw_pspolyln		,	/* UG_DPOLYLN3		/* polyline (3-d) */
		ug_dpolymk		,	/* UG_DPOLYMK			/* polymarker */
		ug_dpolymk3		, 	/* UG_DPOLYMK3		/* polymarker (3-d) */
		uw_pstext		,	/* UG_DTEXT			/* text */
		ug_noop			,	/* UG_DFLAREA			/* fill area */
		ug_noop			,	/* UG_DFLAREA3		/* fill area 3D */
		ug_noop			,	/* UG_DFLAREANM3	/* fill area with normals 3D */
		ug_noop			,	/* UG_DCELL			/* cell array */
		ug_noop			,	/* UG_DCELL3		/* cell array (3D) */
		ug_noop			,	/* UG_DCELLRUN		/* runlength encoded 2D cell */
		ug_noop			,	/* UG_DCELLRUN3	/* runlength encoded 3D cell */
		ug_noop			,	/* UG_DGDP			/* generalized drawing primitive */
		ug_dpolylnras	,	/*	UG_DPOLYLNRAS	/*	raster polyline	*/
		ug_dpolymkras	,	/*	UG_DPOLYMKRAS	/*	raster polymarker	*/
		uw_psrastext	,	/*	UG_DRASTEXT		/*	raster text	*/
		uw_psflarearas,	/*	UG_DFLAREARAS	/*	raster fill	area	*/
		ug_noop			,	/*	UG_DCELLRAS		/*	raster cell	array	*/
		ug_noop			,	/* UG_DCELLRUNRAS	/* raster encoded cell array */
		ug_noop			,	/*	UG_DGDPRAS		/*	raster GDP	*/
		ug_noop			,	/* UG_DBEEP			/* beep the bell */

		/** output attributes **/
		ug_noop			,	/* UG_DLNINDEX		/* set polyline index */
		ug_noop	,	/* UG_DLINETYPE		/* set linetype */
		ug_noop			,	/* UG_DLINEWIDTH		/* set linewidth scale factor */
		ug_noop,	/* UG_DLNCINDEX		/* set polyline color index */
		ug_noop			,	/* UG_DMKINDEX		/* set polymarker index */
		ug_noop	,	/* UG_DMKTYPE			/* set marker type */
		ug_noop			,	/* UG_DMKSIZE			/* set marker size scale factor */
		ug_noop,	/* UG_DMKCINDEX		/* set polymarker color index */
		ug_noop			,	/* UG_DTEXTINDEX		/* set text index */
		ug_noop			,	/* UG_DTEXTFP			/* set text font and precision */
		ug_noop			,	/* UG_DCHAREXP		/* set character expansion factor */
		ug_noop			,	/* UG_DCHARSPACE		/* set character spacing */
		ug_noop,	/* UG_DTXCINDEX		/* set text color index */
		ug_dcharheight	,	/* UG_DCHARHT			/* set character height */
		ug_noop			,	/* UG_DTXPLANE		/* set character plane */
		ug_noop			,	/* UG_DCHARUP			/* set character up vector */
		ug_noop			,	/* UG_DCHARUP3		/* set character up vector (3-d) */
		ug_noop			,	/* UG_DTEXTPATH		/* set text path */
		ug_noop			,	/* UG_DTEXTALIGN		/* set text alignment */
		ug_noop			,	/* UG_DFAINDEX		/* set fill area index */
		ug_noop			,	/* UG_DFAINTSTYLE	/* set fill area interior style */
		ug_noop			,	/* UG_DFASTYLEINDEX	/* set fill area style index */
		ug_noop			,	/* UG_DFACINDEX		/* set fill area color index */
		ug_noop			,	/* UG_DPATSIZE		/* set pattern size */
		ug_noop			,	/* UG_DPATREFPT	/* set pattern reference point */
		ug_noop			,	/* UG_DASFS			/* set aspect source flags */
		ug_noop			,	/* UG_DPICKID		/* set pick identifier */
		ug_noop			,	/* UG_DLNREP		/* set polyline representation */
		ug_noop			,	/* UG_DMKREP		/* set polymarker representation */
		ug_noop			,	/* UG_DTEXTREP		/* set text representation */
		ug_noop			,	/* UG_DFAREP		/* set fill area representation */
		ug_noop			,	/* UG_DPATREP		/* set pattern representation */
		ug_noop			,	/* UG_DCOLORREP	/* set color representation */
		ug_noop			,  /* UG_DEDGEFLAG	/* set edge flag 				*/
		ug_noop			,	/* UG_DINTRCOLOR	/* set polygon interior color */
		ug_noop			,	/* UG_DINTRSHADE	/* set polygon shading method */
		ug_noop			,	/* UG_DINTRLIGHT	/* set polygon lighting method */
		ug_noop			,	/* UG_DSURFPROP	/* set polygon surface props */
		ug_noop			,	/* UG_DLIGHTREP	/* set light source rep */
		ug_noop			,	/* UG_DLIGHTSTATE	/* set light source states */
		/** transformations **/
		ug_noop			,	/* UG_DWIND			/* set window  (3D) */
		ug_noop			,	/* UG_DVPORT		/* set viewport (3D) */
		ug_noop			,	/* UG_DVPPRI		/* set viewport input priority */
		ug_noop			,	/* UG_DNORMTRAN	/* select normalization transform */
		ug_noop			,	/* UG_DCLIP			/* set clipping indicator */
		uw_pswswind	,	/* UG_DWSWIND		/* set workstation window (2D) */
		uw_pswsvport	,	/* UG_DWSVPORT		/* set workstation viewport (2D) */
		ug_noop			,	/* UG_DVREF3		/* set view reference point (3D) */
		ug_noop			,	/* UG_DVPN3			/* set view plane (3D) */
		ug_noop			,	/* UG_DVUP3			/* set view up-vector (3D) */
		ug_noop			,	/* UG_DMODXF		/* set world coordinate matrix (3D) */
		ug_noop			,	/* UG_DVMAP			/* set view mapping */
		/** segments **/
		ug_noop			,	/* UG_DCRESEG		/* create segment */
		ug_noop			,	/* UG_DOPNSEG		/* open segment */
		ug_noop			,	/* UG_DCLOSEG		/* close segment */
		ug_noop			,	/* UG_DRENSEG		/* rename segment */
		ug_noop			,	/* UG_DDELSEG		/* delete segment */
		ug_noop			,	/* UG_DDELSEGWS	/* delete seg from workstation */
		ug_noop			,	/* UG_DASSSEG		/* assoc segment with ws */
		ug_noop			,	/* UG_DCOPYSEG		/* copy segment to workstation */
		ug_noop			,	/* UG_DINSSEG		/* insert segment */
		ug_noop			,	/* UG_DSEGTRAN		/* set segment transformation */
		ug_noop			,	/* UG_DSEGVIS		/* set visibility */
		ug_dseghilite	,	/* UG_DHILITE		/* set hilighting */
		ug_noop			,	/* UG_DSEGPRI		/* seg segment priority */
		ug_noop			,	/* UG_DSEGDET		/* set detectability */
		/** segment element functions */
		ug_noop 			,  /* UG_DINLABEL 	/* insert label */
		ug_noop 			,  /* UG_DSETEPT   	/* set element pointer */
		ug_noop 			,  /* UG_DOFEPT    	/* offset element pointer */
		ug_noop 			,  /* UG_DEPTLBL    	/* set element pointer to a label */
		ug_noop 			,  /* UG_DELELT    	/* delete element */
		ug_noop 			,  /* UG_DELELTR    	/* delete element range */
		ug_noop 			,  /* UG_DELELTLBL   /* delete element between labels */
		/** input functions */
		ug_dinitloc		,	/* UG_DINITLOC		/* initialize locator */
		ug_noop			,	/* UG_DINITSTROKE	/* initialize stroke */
		ug_dinitval		,	/* UG_DINITVAL		/* initialize valuator */
		ug_dinitchoice	,	/* UG_DINITCHOICE	/* initialize choice */
		ug_dinitpick	,	/* UG_DINITPICK		/* initialize pick */
		ug_dinitstr		,	/* UG_DINITSTRING	/* initialize string */
		ug_noop			,	/* UG_DLOCMODE		/* set locator mode */
		ug_noop			,	/* UG_DSTROKEMODE	/* set stroke mode */
		ug_noop			,	/* UG_DVALMODE		/* set valuator mode */
		ug_noop			,	/* UG_DCHOICEMODE	/* set choice mode */
		ug_noop			,	/* UG_DPICKMODE		/* set pick mode */
		ug_dstrmode		,	/* UG_DSTRINGMODE	/* set string mode */
		ug_dreqloc		,	/* UG_DREQLOC			/* request locator */
		ug_noop			,	/* UG_DREQSTROKE		/* request stroke */
		ug_dreqval		,	/* UG_DREQVAL			/* request valuator */
		ug_dreqchoice	,	/* UG_DREQCHOICE		/* request choice */
		ug_dreqpick		,	/* UG_DREQPICK		/* request pick */
		ug_dreqstr		,	/* UG_DREQSTRING		/* request string */
		ug_noop			,	/* UG_DSAMPLOC		/* sample loc */
		ug_noop			,	/* UG_DSAMPSTROKE	/* sample stroke */
		ug_noop			,	/* UG_DSAMPVAL		/* sample valuator */
		ug_noop			,	/* UG_DSAMPCHOICE	/* sample choice */
		ug_noop			,	/* UG_DSAMPPICK		/* sample pick */
		ug_noop			,	/* UG_DSAMPSTRING	/* sample string */
		ug_dawaitev		,	/* UG_DAWAITDEV		/* await event */
		ug_noop			,	/* UG_DPUTSTRING		/* write to scrolling text area*/
		ug_dinitchoice	,	/* UG_DCHGCHOICEAREA	/* chg choice echo area */
		ug_noop			,	/* UG_DMENUTEXTSIZE */
		ug_noop			,	/* UG_DCHOICEHILITE /* choice device highlighting */
		/** metafile **/
		ug_noop			,	/* UG_DWRITEGKSM		/* write item to GKSM */
		ug_noop			,	/* UG_DTYPEGKSM		/* get item type from GKSM */
		ug_noop			,	/* UG_DREADGKSM		/* read item type from GKSM */
		/** inquiry **/
		ug_noop			,	/* UG_DTXTEXT			/* text extent */
		ug_noop			,	/* UG_DPIXDIM			/* inquire pixel array dimensions */
		ug_noop			,	/* UG_DPIXARRAY		/* inquire pixel array */
		ug_noop			,	/* UG_DPIXEL			/* inquire pixel */
		/** secondary entries (lower level, called by simulation lib routines) */
		ug_dawaitchoice,	/* UG_DAWAITCHOICE	/* await choice */
		ug_dawaitpick	,	/* UG_DAWAITPICK		/* await pick */
		ug_dawaitloc	,	/* UG_DAWAITLOC		/* await locator */
		ug_dawaitval	,	/* UG_DAWAITVAL		/* await valuator */
		ug_dawaitstr	,	/* UG_DAWAITSTRING	/* await string */
		ug_noop			,	/* UG_DAWAITSTROKE	/* await stroke */
		ug_noop			,	/* UG_DECHOOP			/* echo */
		uw_psndctodev,	/* UG_DNDCDEV			/* NDC to device conversion */
		uw_psdevtondc,	/* UGDDEVNDC			/* device to NDC conversion */
		ug_noop			,	/* UG_DPAGOP			/* page (clears the screen) */
		ug_noop			,	/* UG_DSAVSCROP		/* save screen */
		ug_noop			,	/* UG_DRESSCROP		/* restore screen */
		ug_noop	,	/* UG_DATEXT			/* alpha text */
		ug_noop			,	/* UG_DRASGET			/* read a raster rectangle */
		ug_noop			,	/* UG_DRASPUT			/* put a raster rectangle */
		ug_noop			,	/* UG_DRASCPY			/* copy a raster rectangle */
		ug_noop			,	/* UG_DRASALLOC		/* allocate raster memory */
		ug_noop			,	/* UG_DRASDEALLOC		/* de-allocate raster memory */
		uw_psrasline	,	/* UG_DRASLINE			/* draw line raster coords */
		ug_noop			,	/* UG_DMARKERRAS		/* draw a raster marker */
		ug_noop			,	/* UG_DKBD				/* get keyboard */
		ug_noop	,	/* UG_D1CHAR			/* get 1 char from keyboard */
		ug_noop			,	/* UG_DKEYPAD			/* get a keypad key */
		ug_noop			,	/* UG_DTRK				/* track a loc cursor */
		ug_noop			,	/* UG_DPIK				/* track pick cursor and pick*/
		ug_noop			,	/* UG_DCHOICE			/* get phys choice dev. (menu) data */
		ug_noop			,	/* UG_DBUTTON			/* get phys button (switch) data */
		ug_noop			,	/* UG_DVAL				/* get valuator data */
		ug_noop			,	/* UG_DSTREAM			/* get stream (stroke) device data */
		ug_noop			,	/* UG_DMOVOP			/* move to x,y */
		uw_psdrwop	,	/* UG_DDRWOP			/* line (2D) */
		uw_pspntop	,	/* UG_DPNTOP			/* point at x,y */
		ug_noop	,	/* UG_DCHHTNDC		/* set character height*/
		ug_noop			,	/* UG_DERASE			/* set background color */

/* ANSI Terminal Functions */
		ug_noop			,	/* UG_DANSION		/* turn on ansi terminal mode */
		ug_noop			,	/* UG_DANSIOFF		/* turn off ansi terminal mode */
		ug_noop			,	/* UG_DANSIVIS		/* make ansi terminal visible */
		ug_noop			,	/* UG_DANSINVIS	/* make ansi terminal invisible */
		ug_noop			,	/* UG_DANSIUP		/* move cursor up one row */
		ug_noop			,	/* UG_DANSIDWN		/* move cursor down one row */
		ug_noop			,	/* UG_DANSILFT		/* move cursor left one column */
		ug_noop			,	/* UG_DANSIRGT		/* move cursor right one column */
		ug_noop			,	/* UG_DANSIEC2E	/* Erase from Cursor to End of line */
		ug_noop			,	/* UG_DANSIEB2C	/* Erase Begin of line to Cursor */
		ug_noop			,	/* UG_DANSIEL		/* Erase current Line */
		ug_noop			,	/* UG_DANSIEC2ES	/* Erase Cursor to End of Screen */
		ug_noop			,	/* UG_DANSIEBS2C	/* Erase Begin of Screen to Cursor */
		ug_noop			,	/* UG_DANSIESCR	/* Erase ansi SCReen */
		ug_noop			,	/* UG_DANSISPOS	/* Set cursor POSition (row, col) */
		ug_noop			,	/* UG_DANSIQPOS	/* Inquire cursor position */

		ug_noop			,	/* UG_DPROMPT		/* put up text prompt*/
		ug_noop			,	/* UG_DDNPROMPT	/* take down text prompt */
		ug_noop			,	/* UG_DMENU			/* put up menu */
		ug_noop		    ,	/* UG_DDNMENU		/* take down menu */
		uw_psraspnts	,	/* UG_DRASPNTS		/* plot the raster point coords. kathy */
/*
... added more because the wsgl and wsmf more fiunction but we don't need here,
....just put all to ug_noop for now
....Yurong 12/12/05
*/
		ug_noop			,   /* UG_DSHADEAREA: 3-d shaded area */ 
	ug_noop,         /* UW_FORM */
	ug_noop,   /* UW_SET_LIST */
	ug_noop,    /* UW_GET_FIELD */
	ug_noop,  /* UW_CLOSE_FORM */
	ug_noop,    /* UW_GET_FILENAME */
	ug_noop,      /* UW_GET_FNAME_ADV */
	ug_noop,      /* UW_OPEN_WINDOW */
	ug_noop,		/* UW_WIN_OUT	*/
	ug_noop,	/* UW_CLOSE_WINDOW */
	ug_noop,					/* UW_APP_EXIT */	
	ug_noop,		/* UZ_LOAD_ACCEL */
	ug_noop,			/* UW_EVENT */
	ug_noop,		/* UW_OPEN_POCKET */
	ug_noop,		/* UW_CLOSE_POCKET */
	ug_noop,			/* UW_SIGNON_LOAD */
	ug_noop,				/* UW_ERROR_MSG */
	ug_noop,			/* UW_DOWN_MENU */
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
	ug_noop,				/* UW_PRMERR */
	ug_noop,					/* UW_WRPRM	*/
	ug_noop,				/* UW_WRSTAT */
	ug_noop,					/* UW_MENU */
	ug_noop,			/* UW_RESET_PROMPT */
	ug_noop,				/* UW_WINDOW */
	ug_noop,				/* UW_SIGNON */
	ug_noop,                       /* UW_GET_CLIP */
	ug_noop,                       /* UW_SET_CLIP */
	ug_noop,                           /* UW_DYNDRAW */
	ug_noop,                     /* UW_GET_DEPMASK */
	ug_noop,                        /* UW_SET_DEPMASK */
	ug_noop,                       /* UW_GET_WSSHADE */
	ug_noop,
	ug_noop,				/* UW_YESNO */
	ug_noop,				/* UW_BCOMLINE */
	ug_noop,				/* UW_ECOMLINE */
	ug_noop,				/* UW_FORM_VIS */
	ug_noop,			/* UW_FORM_INVIS */
	ug_noop,		/* UW_FORM_DISPLAY */
	ug_noop,			/* UW_CLOSE_DISPFRM */
	ug_noop,			/* UW_GETFRM_FIELD */
	ug_noop,			/* UW_DISPFRM_SET_LIST */
	ug_noop,			/* UW_UPDATE_FRM */
	ug_noop,			/* UG_RPPICK_SEG */
	ug_noop,			/* UW_GET_DIRNAME */
	ug_noop,			/* UW_DSPFRM_INVIS */
	ug_noop	,		/* UW_DSPFRM_VIS */
	ug_noop,		/* UW_YESNOCANCEL */
	ug_noop,		/* UW_DISPFRM_SET_LABEL */
	ug_noop,		/* UW_MARKER_SIZE */
};
