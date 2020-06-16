/*********************************************************************
**  NAME:  tpmain.c
**			Main program to plot the plotfile on the plotter  -
**			HP7580, CALCOM1043
**
**       CONTAINS:
**       names of functions in file
**				main
**				utp_parse
**				utp_interact
**				utp_fileplot
**				utp_fromfile
**				utp_comment
**				utp_help
**.....move this function to tmfplot.c
**.....cause we use Motif function.
**				utp_ermsg
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tpmain.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:13:19
*********************************************************************/
#ifdef UU_IRIX64
#include <sys/sysmips.h>
#endif
#include		"ustdio.h"
#include		"usysdef.h"
#include		"zsysdep.h"
#include	"gobas.h"
#define DEF
#include		"tplot.h"
#include 	"udebug.h"
#include		"xenv1.h"
#include 	"xfsys0.h"
#include 	"unserve.h"
#include "mdrwsize.h"
#define NCLVERSION
#include "nclver.h"
#include "ctype.h"
#include "jplot.h"
#include "xenv1.h"

UX_pathname pfnm;
char	*ux_getenv();
#if (UU_COMP != UU_WIN2K)
FILE	*fopen();
#endif
extern  int wsplotdx, wsplotdy;
extern int NCLPLOT;

void utp_parse();

void utp_help();
/*********************************************************************
**    I_FUNCTION :  main(argc,argv)
**       main program
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : For plot_wntinit: return 1 means continue on Window
**										return 0 means completed application
**										return -1 mean need printing on print
**											queue and window need handle that
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/*
......if is is for WNT,
......we already have main routine,
......here is for initialize
*/
#if (UU_COMP != UU_WIN2K)
main(argc, argv)
#else
plot_wntinit(argc, argv)
#endif
int argc; char *argv[];

{
	char	*p;
	FILE	*erfil;
	int stat;

	uu_denter(UU_GTRC,(us,"enter the main \n"));
					/* uu_toolmalloc_init(); */
/*
.....Disables bus errors due to data alignment problems
.....when running on SGI IRIX 6.5 systems with code
.....generated using -mips2 or above flag
.....Bobby  -  8/22/02
*/
#ifdef UU_IRIX64
   sysmips(MIPS_FIXADE,1,0,0);
#endif
/*
.....add UU_PLOTVARS 
.....Yurong 5/31/01
*/
	NCLPLOT = 1;
	uu_toolmalloc_init();
	ux_init_table(1);	

	/* added kathy */
	P1P2_control = UU_FALSE;
	TP_interactive = UU_FALSE;
	Motif_Plot = 0;

	uu_alloc_init();
	/* initialize the udos file management system number server */
	if ( uu_nserv_init(0,MAXT-1,UU_OT_NM) == -1 )
	{
		printf(" Error init'ing number server for file table index numbers.\n");
		return(0);
	}

	p = ux_getenv("UU_debmask",UX_PRTERRS);
	if (p==NULL) UU_debmask= 0;
	else sscanf(p,"%d",&UU_debmask);
/*
#if UU_COMP==UU_VAXVMS
	erfil=fopen("tt","w");
#else
#if (UU_COMP != UU_WIN2K)
	erfil=fopen("/dev/tty","w");
#else
	erfil=fopen("errfile","w");
#endif
#endif
*/
	erfil = NULL;
 	gopengks(10000,erfil); 

/*
.....Parse the input command line
*/
	utp_parse(argc,argv);
/*
.....Start interactive mode
.....Batch mode is basically the same
.....as interactive mode, except no prompts.
*/
#if (UU_COMP != UU_WIN2K)
	if (Motif_Plot == 1)
		utp_winplot();
#else
/*
.....added for WNT window style
.....Yurong 1/20/00
*/
	if (WNT_Plot == 1)
		return 1;
#endif
	else
	{
/*
.....if run in WNT
.....alloc console window first (printf will 
.....have no effort on MFC 
.....Yurong 
*/
#if (UU_COMP == UU_WIN2K)
		Disp_Console();
/*
.....if it is print to 'print que', do nothing here
.....and return -1 to let the main application now
.....and call print function, otherwise, continued
*/
		if (plotopts.print)
			return -1;
#endif
		stat = utp_interact();
#if (UU_COMP == UU_WIN2K)
		if (stat==-1)
		{
			Free_Console();
			WNT_Plot = 1;
			return 1;
		}
#endif
	}
	return 0;
}	/* main */


/*********************************************************************
**    I_FUNCTION :  utp_parse(argc,argv)
**       Parse the argument line. 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void utp_parse(argc,argv)
int	argc;
char	*argv[];

/*
.....Changed this routine to reflect new
.....command line arguments
.....Bobby  -  3/2/92
*/
{
	int	i,j,len;
	static	UU_LOGICAL	first = UU_TRUE;
	char *p;
	double atof();
	char wd[80];
	uu_denter(UU_GTRC,(us,"enter utp_parse, argc=%d", argc));

/*
.....Initialize routine
*/
	pfnm[0] = '\0';
	plotopts.flag = 0;
	plotopts.bypass = 1;
	plotopts.print = 0;
	plotopts.diskfnm[0] = '\0';
	plotopts.port[0] = '\0';
	plotopts.printque[0] = '\0';
	plotopts.size = 0;
	WNT_Plot = 0;
#if UU_COMP==UU_WIN2K
/*
.....we only support postscript printer in PC
.....so default to PS
*/	
	strcpy(plotopts.type,"ps");
#else
	strcpy(plotopts.type,"7475");
#endif
	plotopts.linewt = 1.01;
/*
.....Get global default parameters
*/
	p = ux_getenv("PLOTFILE",UX_PRTERRS);
	if (p != NULL) strcpy(pfnm,p);
	p = ux_getenv("DISKFNM",UX_PRTERRS);
	if (p != NULL) strcpy(plotopts.diskfnm,p);
	p = ux_getenv("PORT_TYPE",UX_PRTERRS);
	if (p != NULL) strcpy(plotopts.port,p);
	p = ux_getenv("PRINT_PLOT",UX_PRTERRS);
	if (p != NULL)
	{
		if (strcmp(p,"YES") == 0) plotopts.print = 1;
		else if (strcmp(p,"NO") == 0) plotopts.print = 0;
	}
	p = ux_getenv("PRINT_QUE",UX_PRTERRS);
	if (p != NULL) strcpy(plotopts.printque,p);
	p = ux_getenv("WS_TYPE",UX_PRTERRS);
	if (p != NULL) strcpy(plotopts.type,p);
	p = ux_getenv("LINE_WT",UX_PRTERRS);
	if (p != NULL) plotopts.linewt = atof(p);
	p = ux_getenv("BYPASS",UX_PRTERRS);
	if (p != NULL)
	{
		if (strcmp(p,"ON") == 0) plotopts.bypass = 1;
		else if (strcmp(p,"OFF") == 0) plotopts.bypass = 0;
	}
/*
.....Parse the runtime command line
*/
	for (i=1; i<argc; i++)
	  {
/*
........Input file name
*/
		if (*argv[i] != '-')
		  {
			if (first)
			{
				first = UU_FALSE;
				strcpy(pfnm,argv[i]);
			}
			else 
			{
				utp_ermsg("Input file already specified.\n",NULL);
				break;
			}
		  }
		else							/* options */
		  switch	(argv[i][1])
		  {
/*
........Batch mode
*/
			case	'b':
				if (plotopts.flag & INTER)
					plotopts.flag = (plotopts.flag ^ INTER);
				plotopts.flag = (plotopts.flag | BATCH);
				WNT_Plot = 0;
				break;
/*
........Create disk file
*/
			case	'd':
				strcpy(plotopts.diskfnm, &argv[i][3]);
				break;
/*
........Help
*/
			case	'h':
				utp_help();
				exit();
				break;
/*
........Interactive mode
*/
			case	'i':
				if (plotopts.flag & BATCH)
					plotopts.flag = (plotopts.flag ^ BATCH);
				plotopts.flag = (plotopts.flag | INTER);
				Motif_Plot = 1;
				break;
/*
........List status
*/
			case	'l':
				plotopts.flag = (plotopts.flag | LIST);
				break;
/*
........Plotter port
*/
			case	'p':
				strcpy(plotopts.port, &argv[i][3]);
				break;
/*
........Output to print que
*/
			case	'q':
				plotopts.print = 1;
				if ((argv[i][1] != '\0') && (argv[i][2] != '\0')
					  && (argv[i][3] != '\0'))
					strcpy(plotopts.printque, &argv[i][3]);
				break;
/*
........Plot size
*/
			case	's':
#define dnm UM_drawing_size_name
				p = &argv[i][3];
				len = strlen(p);
/*				for (j=0;j<=strlen(p);j++) */
				for (j=0;j<=len;j++)
				{
					if (isalpha(*p) && islower(*p)) *p = toupper(*p);
					p++;
				}
/*
.....We need move p point back
.....Yurong 7/31/97
*/
				p = p - len;
				plotopts.size = -1;
/*
.....size type change to 16
.....Yurong 7/31/97
*/
				for (j=0;j<16;j++)
				{
					if (strcmp(&argv[i][3],dnm[j]) == 0)
					{
						plotopts.size = j;
						break;
					}
				}
				if (plotopts.size == -1)
				{
					utp_ermsg("Illegal plot size specified\n",NULL);
					break;
				}
				break;
/*
........Line weight
*/
			case	'w':
				strcpy(wd,&(argv[i][3]));
				plotopts.linewt = atof(wd) ;
				break;
/*
........Plotter type
*/
			case	't':
				p = &argv[i][3];
				if (strcmp(p,"7475") != 0 && strcmp(p,"7580") != 0 &&
					strcmp(p,"1043") != 0 && strcmp(p,"ps") != 0 &&
					strcmp(p,"PS") != 0)
				{
					plotopts.type[0] = '\0';
					utp_ermsg("Invalid plotter type.\n",NULL);
					break;
				}
				else
				{
					strcpy(plotopts.type,p);
					break;
				}
/*
........Bypass mode
*/
			case	'y':
				p = &argv[i][3];
				if (strcmp(p,"ON") == 0 || strcmp(p,"on") == 0)
				{
					plotopts.bypass = 1;
				}
				else if (strcmp(p,"OFF") == 0 || strcmp(p,"off") == 0)
				{
					plotopts.bypass = 0;
				}
				else
				{
					utp_ermsg("Invalid bypass mode.\n",NULL);
				}
				break;
/*
........Unrecognized option
*/
			default  :
				utp_ermsg("Illegal runtime option: %s\n",argv[i]);
				break;
			}
		}
/*
.....Make sure all parameters were specified
*/
	if (pfnm[0] == '\0' ||
		(plotopts.port[0] == '\0' && plotopts.diskfnm[0] == '\0' &&
		 plotopts.print == 0) ||
		plotopts.type[0] == '\0' ||
		(!(plotopts.flag & INTER) && plotopts.size == -1) ||
		((plotopts.flag & BATCH) && (plotopts.size == -1 ||
		plotopts.diskfnm[0] == '\0')))
	{
		if (plotopts.flag & BATCH)
		{
			if (plotopts.diskfnm[0] == '\0')
				utp_ermsg("Run in batch mode (a disk file (-d) must be specified).\n", NULL);
			else if (pfnm[0] == '\0')
				utp_ermsg("No Input Plot File Specified. Exit\n",NULL);
			else if (plotopts.size == -1)
				utp_ermsg("Wrong Paper Size Specified.  Exit\n",NULL);
			exit();
		}
		else
#if (UU_COMP != UU_WIN2K)
			Motif_Plot = 1;
#else
			WNT_Plot = 1;
#endif
	}
	uu_dexit;

}	/* utp_parse */



/*********************************************************************
**    I_FUNCTION :  utp_interact()
**      Pass on to the interactive mode
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

utp_interact()
{
	UX_pathname wpfnm;
	FILE 	*fd1;
	int	drwsize;
	UU_LOGICAL	qkplot;

	uu_denter(UU_GTRC,(us,"enter utp_interact \n"));
	UJ_plotting = UU_TRUE;
	TP_interactive = UU_TRUE;
/*
.....Load plot information file name in "wpfnm"
.....This should be the plot file base name with
.....an extension of ".pl2"
*/
	sprintf(wpfnm, "%s.pl2", pfnm); 
	if ((fd1=fopen(wpfnm,"r"))==NULL)
	{
		utp_ermsg("Cannot open plot information file named: %s\n",wpfnm);
		return -1;
	}
	else
		if (utp_setupok(fd1,&drwsize,&qkplot,&P1P2_control))
	utp_plot(fd1,drwsize,qkplot,P1P2_control,pfnm);
	uu_dexit;
	return 0;
}	/* utp_interact */

/*********************************************************************
**    I_FUNCTION :  utp_help()
**       help messages.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void utp_help()
{

printf("\n Cadplot is used to plot a file created through NCL502 on either\n");
printf(" a Hewlett Packard or Calcomp plotter.  The following options are\n");
printf(" valid with Cadplot.\n\n");
printf(" plotfile    Input plot file.\n");
printf(" -b          Run in batch mode (a disk file (-d) must be specified).\n");
printf(" -d=file     Create a disk file instead of direct output to the plotter.\n");
printf(" -h          Display this Help text.\n");
printf(" -i          Run in interactive mode with prompts.\n");
printf(" -l          List plotter setup statistics.\n");
printf(" -p=port     Defines the plotter port.\n");
printf(" -q(=dest)   Outputs the plot to a printer que.\n");
printf(" -s=size     Defines the paper size (AH, AV, C, D, etc.).\n");
printf(" -t=type     Plotter type (7475, 7580, 1043, ps).\n");
printf(" -w=linewt   Line weight for thick lines.\n");
printf(" -y=bypass   Bypass Mode (ON,OFF).\n\n");
printf(" Some of the options can be set default values using the following\n");
printf(" environmental variables.\n\n");
printf(" PLOTFILE=file       Name of input plot file (plotfile).\n");
printf(" DISKFNM=file        Name of output disk file (-d).\n");
printf(" PORT_TYPE=port      Plotter port (-p).\n");
printf(" PRINT_PLOT=mode     Output using print spooler (YES/NO) (-q).\n");
printf(" PRINT_QUE=dest      Printer destination (-q=dest).\n");
printf(" WS_TYPE=plotter     Plotter type (-t).\n");
printf(" LINE_WT=linewt      Line weight (-w).\n");
printf(" BYPASS=mode         Bypass Mode (-y).\n\n");
}

