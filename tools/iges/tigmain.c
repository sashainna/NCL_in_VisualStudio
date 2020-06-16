/*********************************************************************
**    NAME         :  tigmain.c
**       CONTAINS:
**             main
**             uig_set
**             uig_exit
**             uig_trap
**		       uig_procmd()	
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tigmain.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:47
*********************************************************************/
#define MAINIGES
#include "tiges.h"
#include "tstep.h"
#undef MAINIGES
#if (UU_COMP!=UU_WIN2K)
#include "tigmf.h"
#include <curses.h>
#endif
#ifdef UU_IRIX64
#include <sys/sysmips.h>
#endif
#include "tigconst.h"
#include "usignal.h"
#include "ustdio.h"
#include "xenv1.h"
#include "ulist.h"

#if UU_COMP==UU_VAXVMS
#include iodef
#include descrip
int UIG_tty_channel;    /* set by SYS$ASSIGN */
#endif

char *procmd;	/* NCL: points to program command name */
int mapncl;

extern int NAUTIGES, NAUT502;
extern UU_LOGICAL UIG_drawing_only;
extern UU_LIST *UIO_surf_list;
extern int UG_def_line_wt;
extern int MAX_PARA_REC;
extern UX_pathname iges_fname, iges_tmpfile;

/*********************************************************************
**    I_FUNCTION     :  main
**       Main routine for the IGES/VDA  translators.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....we don't use main routine for WinNT Native version
.....Yurong 1/5/2000
*/
#if (UU_COMP!=UU_WIN2K)
int
main(argc, argv)		/*jkd52: call vda_main or iges_main */
int argc;
char **argv;
{
	char comstr[UX_MAX_PATH_LEN+80];
	char *p, lmsg[80], tmp[52], msg[UX_MAX_PATH_LEN];
	int i, ierr;

	/* determine program name for errors later */
	uig_procmd(argc, argv);
/*
.....Now we may only run using MOTIF
.....Yurong 6/24/98
*/
	UW_MOTIF = 1;
#ifndef UU_VDA
/*
.....Get IGES license
*/
	ncl_init_auth("IGES");
	if (!NAUTIGES && !NAUT502)
		return FALSE;
/*
.....init data
*/
	UIO_surf_list = NULL;
	xlated_flag = NULL;
	uig_set();
	uio_init();
	dbyte = pbyte = 0;
	if ((p=(char*)ux_getenv("MAX_PARA_REC")) != UU_NULL)
		MAX_PARA_REC = atoi(p);
	UG_def_line_wt = 0;
	if ( (p=(char*)ux_getenv("UG_DEF_LINE_WT")) && !strcmp(p,"TRUE"))
		UG_def_line_wt = 1;
/*
...   Initialize generation of subscripted labels.
*/
	uig_init_sublab();
	label_type = 1;
	UIG_drawing_only = UU_FALSE;
	if (argc>1)
	{
		Iges_batch = 1;
		strcpy(comstr, argv[1]);
		for (i=2; i<argc;i++)
		{
			strcat(comstr, " ");
			strcat(comstr, argv[i]);
		}
		iges_batchrun (comstr);
		if (iges_tmpfile[0]!='\0')
		{
			if (ux_delete0(iges_tmpfile))
			{
				ul_short_filename(iges_fname,tmp,50);
				sprintf (msg, "can't delete %s corresponding %s",
					iges_tmpfile, tmp);
				printf(msg);
			}
			return 0;   
		}
	}
	else
		Iges_batch = 0;
#endif
#ifdef UU_VDA 
	vda_main();
#endif
#ifndef UU_VDA 
		iges_mfmain();
#endif
		return(0);
	}
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_set
**       A routine to initialize some uig constants
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_set()
   {
   int i;

#if UU_COMP==UU_VAXVMS
   short stat;

   /* initialize device descriptor for SYS$ASSIGN call */
   static $DESCRIPTOR(dev, "SYS$COMMAND");
#endif

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
#if UU_COMP==UU_VAXVMS
   /* get channel for calls to SYS$QIOW */
   stat = SYS$ASSIGN(&dev, &UIG_tty_channel, 3, 0);

   if (stat != SS$_NORMAL)
      printf("uig_set: bad SYS$ASSIGN stat %d", stat);
#endif

   delim = delim_st;                /* set field delimitor */
   term = term_st;                  /* set record delimitor */
   t_num = 0;
   for(i=0;i<IG_NUM;i++)
      {
      iges_type[i] = iges_st_type[i];     /* set relation numbers */
      iges_name[i] = iges_st_name[i];     /* set relation names */
      }
   for(i=0;i<DDL_NUM;i++)
      ddl_nam[i] = ddl_st_nam[i];         /* set Uni-ddl relation names */
   
   /* initilize tty curser routines */
#if (UU_COMP!=UU_WIN2K)
	if (UW_MOTIF!=1)
		uig_tty_mode(INIT);
#endif
	return (UU_SUCCESS);
   }


/*********************************************************************
**    I_FUNCTION     :  uig_exit
**       Routine called to exit, resets terminal characteristics.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....we don't use this routine for WinNT Native version
.....Yurong 1/5/2000
*/
#if (UU_COMP!=UU_WIN2K)
void
uig_exit()
{
	int ans;
	/*
	printf("SYSTEM ERROR: please note %s directory number and check data",
		procmd);
		*/
	printf("UNRECOVERABLE SYSTEM ERROR: translating file %s ",
		procmd);
	if (UW_MOTIF!=1)
	{
		uig_prompt(2,&ans);
		uig_clear();
		uig_tty_mode(RESET);
		uig_tty_mode(STANDARD);
	}
	exit();
}
#endif

/*********************************************************************
**    I_FUNCTION     :  uig_trap
**       Routine to set system traps
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....we don't use this routine for WinNT Native version
.....Yurong 1/5/2000
*/
#if (UU_COMP!=UU_WIN2K)
int
uig_trap()
	{
	void uig_exit();							/* routine to call upon termination */

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_emergencyclose(uig_exit);	/* routine which debug will call on exit */

#ifdef UU_IRIX64
	sysmips(MIPS_FIXADE,1,0,0);
#endif

	/* Set the interrupts */
	signal(SIGILL,uig_exit);			/* Illegal instruction */
	signal(SIGFPE,uig_exit);			/* Floating point exception */
	signal(SIGBUS,uig_exit);			/* Bus error */
	signal(SIGSEGV,uig_exit);			/* Segmentation violation */
	signal(SIGSYS,uig_exit);			/* Bad arg to system call */
	signal(SIGINT,uig_exit);			/* Keyboard interrupt */

	return (UU_SUCCESS);
	}
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_procmd
**       Gets program name from command line
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....we don't use this routine for WinNT Native version
.....Yurong 1/5/2000
*/
#if (UU_COMP!=UU_WIN2K)
int
uig_procmd(argc, argv)
int argc;
char **argv;
	{
	if ((argc > 0) && (argv != NULL))
		procmd=argv[0];
	else
		procmd=NULL;

	/* NCL: map geometry to NCL geom instead of WF geom */
	/* mapncl=1 => NCLgeom; mapncl=0 => WireFrame geometry */
	/* pertains to parametric splines only for now */

	mapncl = 0;

	return (UU_SUCCESS);
	}
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_cntl_exit
**       Routine called to do a controlled exit, NOT USED BY SIGNAL 
**       TRAPPING ROUTINES.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....we don't use this routine for WinNT Native version
.....Yurong 1/5/2000
*/
int
uig_cntl_exit()
{
#if (UU_COMP!=UU_WIN2K)

	if (UW_MOTIF!=1)
	{
		uig_clear();
		uig_tty_mode(RESET);
		uig_tty_mode(STANDARD);
	}
	exit();
#endif
	return (UU_SUCCESS);
	}
