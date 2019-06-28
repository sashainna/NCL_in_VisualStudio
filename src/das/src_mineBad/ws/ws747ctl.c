
/*********************************************************************
**    NAME         :  ws747ctl.c
**    CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       ws747ctl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:04
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
/*
.....added by Yurong to
.....avoid multiple declare
*/
#define WS7475_FIRST
#include "ws7475.h"
#undef WS7475_FIRST
#include "ws.h"
#include "tplot.h"  /* added kathy */
#include "xenv1.h"
#include<string.h>

/* declared in tplot.h. kathy 
extern struct  { struct{int	x,y;} ll, ur; }	plot_bound;
extern UU_LOGICAL	P1P2_control; */
/*extern UU_LOGICAL    DISK_file;	  */ /*Output to a disk file?  */
/*extern int				PORT_id; */	/* opened device id for gttio use */

Gs7475 uw_7475;					/* declare workstation local data */
int	 uw_7475ind;				/* index to jp-buffer */
/*
.....change to static because only this
.....file use this variable
.....Yurong 8/25/97
*/
static struct {Gint op; Gws ws; Gdrect3 wsvport;} coprm; 
/*struct {Gint op; Gws ws; Gdrect3 wsvport;} coprm;  */
char uu_ttline(), uu_ttget(), uu_tttest();	/* these may be needed to perform
											i/o to a serial rs232 line */
/* added kathy */
int	 uw_7475ipt;				/* index to jp-buffer */
/*
.....changed by Yurong
.....declared in tplot.h
.....here use as extern
.....Yurong 8/26/97
*/
extern int  wsplotdx, wsplotdy; 
extern int  pts; 

/***********  WORKSTATION DESCRIPTION TABLE **************/
static Gint ltypes[2]={1,2};					/* list of 2 avail line types */
static Gint mktypes[2]={1,2};					/* list of 2 available marker types */
static Gtxfp fonprecs[2]={1,UG_STRING,1,UG_STROKE};	/* 2 avail font/prec pairs */
/**Valid Choice PETS :
**	3. Text Menu. "choice number", numbers displayed, kbd input only.
**	5. Icon Menu. "select option", no numbers, mouse input only.
**	21.Text Menu. "choice(F-keys)",numbers displyed, fkey input only.
**		(probably never used.)
**	22.Text Menu. "select option", numbers displayed, mouse or kbd.
**		(probably used most often.)
**	23.Text Menu. same as 22 but with help messages per item.
**	24.Text Menu. same as 22 but with addition of backlighting.
**	25.Text Menu. "select option", no numbers, mouse input only.  */
static Gpet choicepets[NUMPETS]={3,5,21,22,25};	/* default choice echotypes */
static Gchar defchostr[2][2]={" "," "};	/* default choice strings */
static Gchar *defchoptr[2]={&defchostr[0][0],&defchostr[1][0]};
/* The following defined 150 logical choice devices. Numbers 10-100 are
	for menus. This provides for up to 90 menus */
static Gdefchoice defcho[NCHDEV]={UW_DEFCHO};		/* default choice data */

static Gpet locpets[1]={1};		/* locator prompt and echo types */
static Gdefloc defloc[1]={0.,0.,1,locpets,0.,0.,1.,1.,	/* default loc data */
						};					/* should be default loc data record here */
static Gpet pickpets[1]={1};		/* default pick echotypes */
static Gdefpick defpick[1]={1,pickpets,0.,0.,1.,1.,	/* default pick data */
						" "};				/* default pick data record */
static Gpet stringpets[6]={1,22,22,22,22,22};	/* string echotypes */
/* the following allows for 6 string devices, each may have its own
	pop-up area */
static Gdefstring defstring[6]={
						{UW_DEFSTR},
						{UW_DEFSTR},
						{UW_DEFSTR},
						{UW_DEFSTR},
						{UW_DEFSTR},
						{UW_DEFSTR}
						};
static Gpet strokepets[1]={1};	/* default stroke echotypes */
static Gdefstroke defstroke[1]={1,1,strokepets,0.,0.,1.,1.,	/* default stroke data */
						};					/* default stroke data record goes here */
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
						.01,.01,.01,	/* nom, min, max width */
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
					{256,UG_COLOUR,9},	/* no. colors, color available,no. bundles  */
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
static UG_wdt wdt={	"s7475",UG_OUTIN,	/* workstation type and category */
						UG_DC_METRES,			/* device units */
						0.415, 0.258,				/* display size in meters */
						DEVXMAX+1,DEVYMAX+1,	/* display size in raster */
						ROWMAX,COLMAX,		/* number rows,cols of text */
						20,					/* no definable viewing table indices */
						1,						/* need NDC box around segments */
						0,						/* UG_DSAVESCRN doesn't work */
						0,						/* Number of bit planes */
						&outwdt,				/* output wdt stuff */
						&inwdt};				/* input wdt stuff */




/*********************************************************************
**    I_FUNCTION :  uw_7475func(buf,len)
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

uw_7475func(buf,len)
char	buf[];
int	len;
{
	int icnt,nc,ipt,stat,nco;
	char sbuf[82];
	uu_denter(UU_GITRC,(us,"uw_7475func,len=%d", len));
/*
.....Added logic to write 80 col records
.....Bobby  -  8/8/91
*/
	nc = len;
	ipt = 0;
	do
	{
		icnt = nc;
		if (icnt > 80) icnt = 80;
		strncpy (sbuf,&buf[ipt],icnt);
		sbuf[icnt] = '\n';
		nc = nc - icnt;
		ipt = ipt + icnt;
		icnt++;
		stat = ux_fwrite0(sbuf,icnt,sizeof(char),uw_7475.fid,&nco);
	} while (nc > 0);

	uu_dexit;
}	/* uw_7475func */


/*-------------------------- control functions ----------------------------*/
/*-----------------------------------------------> UG_DOPENWS */
/*********************************************************************
**    I_FUNCTION :  uw_7475init(prms,reply)
**		perform any processing needed by the workstation hardware to get
**		it ready for use. If the device is accessed through an RS232 serial
**		line, use uu_ttopen, uu_ttput, etc (described in tt.c) to
**		perform the I/O.  If all went OK, return with reply.stat=OK, else
**		set reply.stat=NONE. Also, set reply.wdtptr=address of the workstation
**		description table 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_7475init(prms,reply)				/* open workstation */
struct {Gint op; Gws wsid; 
		Gchar *conn;				/* connection-filename to access device */
		} *prms;
UG_rwsinit *reply;			/* struct { Gstatus stat; UG_wdt *wdtptr;} *reply*/

{
	/* struct {Gint op; Gws ws; Gdrect3 wsvport;} coprm; */
	int	i,nc;
	int	xy[2];
	char	pbuf[80];
	Gfloat	ndc[2];
	char	*p, *ux_getenv(), *str1, *str2;
	char num[30];

	uu_denter(UU_GITRC,(us,"uw_7475init(ws=%d)",(*prms).wsid));
	for (i=1; i<NCHDEV; i++) defcho[i]=defcho[0];	/* initialize defcho array*/
	(*reply).stat = UG_OK;				/* return status OK */
	(*reply).wdtptr = &wdt;			/* return addr of workstation desc table */
	uw_7475.wid = (*prms).wsid;
	/* wid=(*prms).wsid;  */
	/* (ug_gksstli.wsopen[0].outptr)->curxform.v.llf.x = 0.1; */
	DISK_file = UU_FALSE;
	TEMP_file = UU_FALSE;
/*
.....Open port for plotting
*/
	if (plotopts.diskfnm[0] == '\0' || plotopts.print == 1)
	{
		uw_7475.ttfd = 0;
		if (plotopts.print == 0) uw_7475.ttfd=uu_ttopen((*prms).conn,3);
		if (uw_7475.ttfd < 0 || plotopts.print == 1)
		{
			uw_7475.ttfd = 0; /*uw_7475.ttfd + 100;*/
			tmpnam(TEMP_fnm);
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
			if ((ux_fopen0(TEMP_fnm,"w",&uw_7475.fid)) == UU_FAILURE)
			printf("Cannot create temporary file.\n");
			uu_ttstartsave(uw_7475.ttfd,uw_7475func,0);
			DISK_file = UU_TRUE;
			TEMP_file = UU_TRUE;
		}
		else
		{
			PORT_id = uw_7475.ttfd;
			uw_7475.fid = UU_NULL;
			uu_ttsing(uw_7475.ttfd);			/* set single character mode */
		}
	}
	else
	{
		if ((ux_fopen0(plotopts.diskfnm,"w",&uw_7475.fid)) == UU_FAILURE)
		{
			printf("Cannot open file %s to write\n", plotopts.diskfnm);
			uw_7475.fid = UU_NULL;
		}
		else
		{
			uu_ttstartsave(uw_7475.ttfd,uw_7475func,0);
			DISK_file = UU_TRUE;
		}
	}
/*
.....Allow the user to specify
.....the initialization string
.....Bobby  -  7/6/92
*/
	p = ux_getenv("INIT7470",UX_PRTERRS);
	if (p == NULL)
	{
		if (plotopts.bypass == 1)
			uu_ttput(uw_7475.ttfd,"\033.(",3);/* plotter on */
		uu_ttput(uw_7475.ttfd,"\033.J",3);	/* abort device control */
		uu_ttput(uw_7475.ttfd,"\033.K",3);	/* abort graphic instruction*/
		/*uu_ttput(uw_7475.ttfd,"\033.T:",4);	/* default memory allocation */
		/*uu_ttput(uw_7475.ttfd,"\033.P1:",5);	/* select handshake xon/xoff */
		uu_ttput(uw_7475.ttfd,"\033.M;;10:",8);	/* the echo terminator */
		uu_ttput(uw_7475.ttfd,"\033.I160;;17:",11);/*xon/xoff,for 7475A */
		uu_ttput(uw_7475.ttfd,"\033.N;19:",7);				/*xon/xoff,for 7475A */
		/* Special settings for HP clone plotters.  kathy */
		uu_ttput(uw_7475.ttfd,"IW0,0,16071,10363;",17);
		p = ux_getenv("UU_7475LABSIZ",UX_PRTERRS);
#define WHITE ", \t" 
		if (p!=NULL)
      {
         str1 = strtok(p, WHITE);
         str2 = strtok(NULL, WHITE);
		}
		if((str1==NULL)||(str2==NULL)||(p==NULL))
		{	str1 = "0.190";
			str2 = "0.270";
		}
		sprintf(num,"VS38.1000;DT\003;SI%s,%s;",str1,str2);
		uu_ttput(uw_7475.ttfd,num, strlen(num));	
		uu_ttput(uw_7475.ttfd,"DI100.000,0.000;SL0.000;\003.)",25);	
	}
	else
	{
		nc = strlen(p);
		uu_ttput(uw_7475.ttfd,p,nc);
	}
	uw_7475.isrotate = UU_FALSE;				/* no ratation */
	uw_7475ind = 0;

	/* added kathy */
	uw_7475ipt = 0;
/*
.....Initialize currently loaded pen
.....Bobby  -  7/15/91
*/
	uw_7475pen(-1);


	if (DISK_file == UU_FALSE) uu_ttflin(uw_7475.ttfd);					/* flush io input buffer */
	if (P1P2_control)
		uwi_747setbound();
	if (DISK_file == UU_FALSE) uu_ttflin(uw_7475.ttfd);					/* flush io input buffer */
	uu_dexit;
}	/* uw_7475init */



/*********************************************************************
**    I_FUNCTION :  uwi_747setbound()
**       Get user's p1&p2 information.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uwi_747setbound()
{
	int	i, len;
	char	buf[80],buf2[80];
	int	tp1[2], tp2[2];
	register char	c;

	uu_denter(UU_GITRC,(us,"uw_747setbound()"));
	uu_ttput(uw_7475.ttfd,"OP;",3);
	for (i=0; i<80; i++)
	  {
		buf[i] = c = uu_ttget(uw_7475.ttfd);	
		if (c == '\015')
		  {
			buf[i] = '\0';
			break;
		  }
	  }
	uu_ttput(uw_7475.ttfd,"\015\n",2);/* sent cr-return&line-feed to acknowledge
													the plotter that input has finished*/
		/* Get p1 and p2 point */
	/*sscanf(buf,"%d,%d,%d,%d",&(plot_bound.ll.x),&(plot_bound.ll.y),&(plot_bound.ur.x),&(plot_bound.ur.y));	*/
	sscanf(buf,"%d,%d,%d,%d",&tp1[0],&tp1[1],&tp2[0],&tp2[1]);	
		/* find the lower_left corner and the upper_right corner */
	plot_bound.ll.x = (tp1[0]<=tp2[0])? tp1[0] : tp2[0];
	plot_bound.ll.y = (tp1[1]<=tp2[1])? tp1[1] : tp2[1];
	plot_bound.ur.x = (tp1[0]<=tp2[0])? tp2[0] : tp1[0];
	plot_bound.ur.y = (tp1[1]<=tp2[1])? tp2[1] : tp1[1];
	uu_dprint(UU_GITRC,(us,"bound=%d,%d,%d,%d",plot_bound.ll.x,plot_bound.ll.y,plot_bound.ur.x,plot_bound.ur.y));	

/*-----
	VPx = p1x * 0.000025;
	VPy = p1y * 0.000025;
	wdt.dspsize.raster.x = p2x - p1x + 1;
	wdt.dspsize.raster.y = p2y - p1y + 1;
	wdt.dspsize.device.x = wdt.dspsize.raster.x * 0.000025;
	wdt.dspsize.device.y = wdt.dspsize.raster.y * 0.000025;
-----*/
	uu_dexit;
}	/* uw_747setbound */





/*-----------------------------------------------> UG_DCLOSEWS */
uw_7475term(prms,reply)				/* close workstation */
int prms[];								/* no input parameters */
int reply[];							/* no output parameters */
{
	char buf[512];
	/* Perform any action needed to terminate the workstation.
	such as closing the operating system file I/O connection. Use
	ttclos, etc (see tt.c) if the workstation is accessed by an RS232
	serial line. */
	uu_denter(UU_GITRC,(us,"uw_7475term()"));
   uu_ttput(uw_7475.ttfd,"SP;",3);			/* Put pen back to the carousel */
	if (plotopts.bypass == 1) uu_ttput(uw_7475.ttfd,"\033.)",3);			/* plotter off */
	uu_ttflout(uw_7475.ttfd);
	if (DISK_file == UU_FALSE) uu_ttnorm(uw_7475.ttfd);
	uu_ttstopsave(uw_7475.ttfd);
#if UU_OPSYS!=UU_VMS && UU_OPSYS!=UU_ALPHAVMS
	uu_ttclos(uw_7475.ttfd,2);  /* this cause problem under VMS */
#endif
/*
.....Close Disk file
*/
	if (DISK_file == UU_TRUE) ux_fclose0(uw_7475.fid);
/*
.....Created temporary file
.....Get it to the output port
.....Then delete it
*/
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
/*		printf("%s\n",buf);*/
#if UU_COMP != UU_VAXVMS
		remove(TEMP_fnm);
#endif
	}

	uu_dexit;
}	/* uw_7475term */




/*------------------------------------------------> UG_DACTWS */
/*********************************************************************
**    I_FUNCTION :  uw_7475actws(prms,reply)				
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

uw_7475actws(prms,reply)				/* activate workstation */
int prms[],reply[];				/* no parameters */

{
	struct {Gint op; Gws ws; Gdrect3 wsvport;} coprm; 
	int	xy[2];
	char	pbuf[80];
	Gfloat	ndc[2];

	uu_denter(UU_GITRC,(us,"uw_7475actws()"));
	uu_dexit;
}	/* uw_7475actws */



/*------------------------------------------------> UG_DDEACTWS */
uw_7475deact(prms,reply)				/* deactivate workstation */
int prms[],reply[];					/* no parameters */
{
	/* If this routine is called, no other entries will be called until
	a subsequent call to UG_DACTWS. Usually, no action is necessary */
	uu_denter(UU_GITRC,(us,"uw_7475deact()"));
	uu_ttflout(uw_7475.ttfd);
	uu_dexit;
}	/* uw_7475deact */

/*------------------------------------------------> UG_DCLEARWS */
uw_7475clearws(prms,reply)				/* clear workstation */
int prms[],reply[];				/* no parameters */
{
	/* delete all segments on this workstation and update screen (clear it) */
	uu_denter(UU_GITRC,(us,"uw_7475clearws()"));
	uu_dexit;
}

/*-------------------------------------------------> UG_DREDRAWWS */
/*********************************************************************
**    I_FUNCTION :  uw_7475redrawws(prms,reply)			
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

uw_7475redrawws(prms,reply)			/* redraw all segs on ws */
int prms[],reply[];				/* no parameters */

{
	uu_denter(UU_GITRC,(us,"uw_7475redrawws()"));
	ug_dredrawvis();
	uu_dexit;
}

/*--------------------------------------------------> UG_DUPDATE */
uw_7475upd(prms,reply)				/* update workstation */
int prms[],reply[];				/* no parameters */
{
	/* flush any output buffers, perform any deferred changes, etc */
	uu_denter(UU_GITRC,(us,"uw_7475upd()"));
	uu_dexit;
}

/*---------------------------------------------------> UG_DSETDEFST */
uw_7475setdfst(prms,reply)			/* set deferral state */
struct {int op; Gws id;  Gdefer defstate; } *prms;
int reply[];				/* no output parameters */
{
	/*  not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475setdfst()"));
	uu_dexit;
}

/*---------------------------------------------------> UG_DMSG */
uw_7475msg(prms,reply) 				/* message */
struct { Gint op; Gws ws; Gchar *str;} *prms;
int reply[];
{
	/* Write the message contained in (*prms).str somewhere on the screen.
	If possible, maintain a scrolling area of the last 5 or so messages.
	If possible, don't obscure anything else with these messages */
	uu_denter(UU_GITRC,(us,"uw_7475msg()"));
	uu_dexit;
}

/*----------------------------------------------------> UG_DESCAPE */
uw_7475escape(prms,reply)				/* escape */
int prms[],reply[];
{
	/* for future escape functions. Not used now. */
	uu_denter(UU_GITRC,(us,"uw_7475escape()"));
	uu_dexit;
}

/*----------------------------------------------------> UG_DCALL */
uw_7475call(prms,reply)					/* call specific workstation */
int prms[],reply[];
{
	/* Reserved for future use. */
	uu_denter(UU_GITRC,(us,"uw_7475call()"));
	uu_dexit;
}

/*------------------------------------------------ external routines */
int ug_noop();

/* the following routine is in gdkbd.c */

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

/* ? */
int ug_dcharheight(),	ug_dpolylnras(),
		ug_dpolymkras(),/* ug_drasalloc(), ug_drasdealloc(),	 */
		ug_dseghilite();

/* the following routines are in wss7475ansi.c */
/* the following are in wss7475sec.c */
int uw_74751char(), uw_7475chhtndc();

/* the following are in wss7475att.c */
int uw_7475linetype(), uw_7475linewid();

/* ? */
int
		uw_7475atext(),
		uw_7475polyln3(),
		uw_7475clearws(),
		uw_7475colorrep(),
		uw_7475init(),
		uw_7475drwop(),
		uw_7475lncindex(),	
		uw_7475pntop(),
		uw_7475mkcindex(),	
		uw_7475mktype(),
		uw_7475msg(),
		uw_7475pkid(),
		uw_7475rasline(),
		uw_7475redrawws(),
		uw_7475term(),
		uw_7475text(),
		uw_7475rastext(),
		uw_7475flarearas(),
		uw_7475txcindex(),	
		uw_7475upd(),
		uw_7475wswind(),
		uw_7475wsvport(),
		uw_7475ndctodev(),
		uw_7475devtondc(),
		uw_7475raspnts();  /* added kathy */


/******* WORKSTATION ENTRY POINTS TABLE.  ********/
UG_wsenttbl ws7475={ 
		uw_7475init		,	/*	UG_DOPENWS		/* open workstation */
		uw_7475term		,	/* UG_DCLOSEWS		/* close workstation */
		uw_7475actws	,	/* UG_DACTWS		/* activate workstation */
		uw_7475deact	,	/* UG_DDEACTWS		/* deactivate workstation */
		uw_7475clearws	,	/* UG_DCLEARWS		/* clear workstation */
		uw_7475redrawws,	/* UG_DREDRAWWS	/* redraw all segs on ws */
		uw_7475upd		,	/* UG_DUPDATE		/* update workstation */
		ug_noop			,	/* UG_DCHGXFORM	/* a xform changed */
		ug_noop			,	/* UG_DSETDEFST	/* set deferral state */
		uw_7475msg		,	/* UG_DMSG			/* message */
		ug_noop			,	/* UG_DESCAPE		/* escape */
		ug_noop			,	/* UG_DCALL			/* call specific workstation */
		ug_noop			,	/* UG_DIMAGE		/* set imaging mode */
		ug_noop			,	/* UG_DINTRACTV	/* set interactive mode */
		ug_noop			,	/* UG_DHLHSRMODE	/* set hlhsr mode */
		ug_noop			,	/* UG_DSIGNAL		/* set interrupt handler */
		/* output primitives **/
		ug_dpolyln		,	/* UG_DPOLYLN			/* polyline */
		uw_7475polyln3		,	/* UG_DPOLYLN3		/* polyline (3-d) */
		ug_dpolymk		,	/* UG_DPOLYMK			/* polymarker */
		ug_dpolymk3		, 	/* UG_DPOLYMK3		/* polymarker (3-d) */
		uw_7475text		,	/* UG_DTEXT			/* text */
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
		uw_7475rastext	,	/*	UG_DRASTEXT		/*	raster text	*/
		uw_7475flarearas,	/*	UG_DFLAREARAS	/*	raster fill	area	*/
		ug_noop			,	/*	UG_DCELLRAS		/*	raster cell	array	*/
		ug_noop			,	/* UG_DCELLRUNRAS	/* raster encoded cell array */
		ug_noop			,	/*	UG_DGDPRAS		/*	raster GDP	*/
		ug_noop			,	/* UG_DBEEP			/* beep the bell */

		/** output attributes **/
		ug_noop			,	/* UG_DLNINDEX		/* set polyline index */
		ug_noop			,	/* UG_DLINETYPE		/* set linetype */
		uw_7475linewid	,	/* UG_DLINEWIDTH		/* set linewidth scale factor */
		uw_7475lncindex,	/* UG_DLNCINDEX		/* set polyline color index */
		ug_noop			,	/* UG_DMKINDEX		/* set polymarker index */
		uw_7475mktype	,	/* UG_DMKTYPE			/* set marker type */
		ug_noop			,	/* UG_DMKSIZE			/* set marker size scale factor */
		uw_7475mkcindex,	/* UG_DMKCINDEX		/* set polymarker color index */
		ug_noop			,	/* UG_DTEXTINDEX		/* set text index */
		ug_noop			,	/* UG_DTEXTFP			/* set text font and precision */
		ug_noop			,	/* UG_DCHAREXP		/* set character expansion factor */
		ug_noop			,	/* UG_DCHARSPACE		/* set character spacing */
		uw_7475txcindex,	/* UG_DTXCINDEX		/* set text color index */
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
		uw_7475pkid		,	/* UG_DPICKID		/* set pick identifier */
		ug_noop			,	/* UG_DLNREP		/* set polyline representation */
		ug_noop			,	/* UG_DMKREP		/* set polymarker representation */
		ug_noop			,	/* UG_DTEXTREP		/* set text representation */
		ug_noop			,	/* UG_DFAREP		/* set fill area representation */
		ug_noop			,	/* UG_DPATREP		/* set pattern representation */
		uw_7475colorrep,	/* UG_DCOLORREP	/* set color representation */
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
		uw_7475wswind	,	/* UG_DWSWIND		/* set workstation window (2D) */
		uw_7475wsvport	,	/* UG_DWSVPORT		/* set workstation viewport (2D) */
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
		uw_7475ndctodev,	/* UG_DNDCDEV			/* NDC to device conversion */
		uw_7475devtondc,	/* UGDDEVNDC			/* device to NDC conversion */
		ug_noop			,	/* UG_DPAGOP			/* page (clears the screen) */
		ug_noop			,	/* UG_DSAVSCROP		/* save screen */
		ug_noop			,	/* UG_DRESSCROP		/* restore screen */
		uw_7475atext	,	/* UG_DATEXT			/* alpha text */
		ug_noop			,	/* UG_DRASGET			/* read a raster rectangle */
		ug_noop			,	/* UG_DRASPUT			/* put a raster rectangle */
		ug_noop			,	/* UG_DRASCPY			/* copy a raster rectangle */
		ug_noop			,	/* UG_DRASALLOC		/* allocate raster memory */
		ug_noop			,	/* UG_DRASDEALLOC		/* de-allocate raster memory */
		uw_7475rasline	,	/* UG_DRASLINE			/* draw line raster coords */
		ug_noop			,	/* UG_DMARKERRAS		/* draw a raster marker */
		ug_noop			,	/* UG_DKBD				/* get keyboard */
		uw_74751char	,	/* UG_D1CHAR			/* get 1 char from keyboard */
		ug_noop			,	/* UG_DKEYPAD			/* get a keypad key */
		ug_noop			,	/* UG_DTRK				/* track a loc cursor */
		ug_noop			,	/* UG_DPIK				/* track pick cursor and pick*/
		ug_noop			,	/* UG_DCHOICE			/* get phys choice dev. (menu) data */
		ug_noop			,	/* UG_DBUTTON			/* get phys button (switch) data */
		ug_noop			,	/* UG_DVAL				/* get valuator data */
		ug_noop			,	/* UG_DSTREAM			/* get stream (stroke) device data */
		ug_noop			,	/* UG_DMOVOP			/* move to x,y */
		uw_7475drwop	,	/* UG_DDRWOP			/* line (2D) */
		uw_7475pntop	,	/* UG_DPNTOP			/* point at x,y */
		uw_7475chhtndc	,	/* UG_DCHHTNDC		/* set character height*/
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
		uw_7475raspnts,		/* UG_DRASPNTS		/* plot the raster point coords. kathy */
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
	ug_noop		/* UW_MARKER_SIZE */
		};
