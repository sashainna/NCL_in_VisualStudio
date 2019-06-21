/*********************************************************************
**
**    NAME         :  umainc.c
**
**       CONTAINS:
**				main()
**
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       umainc.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       12/14/15 , 08:54:52
**
*********************************************************************/
/* do not remove this include, redefines Fortran function names */
#ifdef UU_IRIX64
#include <sys/sysmips.h>
#endif
#include "usysdef.h"
#if UU_COMP!=UU_WIN2K
#include <stropts.h>
#include <errno.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include "usysdef.h"
#include "mfcifdef.h"
#include "xenv1.h"
#include "mfort.h"
#include "lcom.h"
#include "nclfc.h"
#include "usysg.h"
#include "time.h"
#include "nclver.h"
/*
.....added to Yurong 8/6/97
*/
#include "jplot.h"
/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                             !!!!!!
!!!!!!   NOTE:                                                     !!!!!!
!!!!!!      Recompiling this routine will cause any changes        !!!!!!
!!!!!!      made to the include file NCLICONS.H to                 !!!!!!
!!!!!!      be reflected in the executable.                        !!!!!!
!!!!!!                                                             !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

char uu_sccsident[] = "@(#) umainc.c 9.4 3/24/92 14:05:25";
static FILE *fp=0;
int Ncq_ncl_id = -1;
int NCQ_filenum = 1;
static FILE *logptr = NULL;
int NCQ_running = 0;
unsigned int NCL_subprocess = 0;

void ul_open_logfile()
{
	logptr = fopen("ncl.log","a+");
}
void ul_write_logfile(file, nc, err, warn, cdate, ctime)
int *nc, *err, *warn;
char *file, *cdate, *ctime;
{
	char msg[UX_MAX_PATH_LEN+40];
	char ldate[12], ltime[9];
	file[*nc] = '\0';
	strncpy(ldate, cdate, 11);
	strncpy(ltime, ctime, 8);
	ldate[11] = '\0';
	ltime[8] = '\0';
	if (logptr!=NULL)
	{
		if (*err==-1)
			sprintf(msg, "%s started at %s %s.", file, ldate, ltime);
		else
		{
			if ((*err==0) && (*warn==0))
				sprintf(msg, "%s ended at %s %s.", file, ldate, ltime);
			else if (*err==0)
				sprintf(msg, "%s ended with %d warnings at %s %s.", file, *warn, ldate, ltime);
			else if (*warn==0)
				sprintf(msg, "%s ended with %d errors at %s %s.", file, *err, ldate, ltime);
			else
				sprintf(msg, "%s ended with %d errors and %d warnings at %s %s.", file, *err, *warn, ldate, ltime);
		}		
#if (UU_COMP==UU_WIN2K)
		fprintf (logptr, "%s\r\n", msg);
#else
		fprintf (logptr, "%s\n", msg);
#endif
	}
}
void ul_close_logfile()
{
	if (logptr!=NULL)
		fclose(logptr);
}
/*********************************************************************
**    E_FUNCTION     : int NclxPrintf(buf)
**       Writes a string to the debug file.
**    PARAMETERS
**       INPUT  :
**          buf       Text string to output.
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void NclxPrintf(buf)
char *buf;
{
	if (fp != 0) fputs(buf,fp);
}

#if UU_COMP == UU_CIM
cmain(argc, argv)
#else
#if (UU_COMP!=UU_WIN2K) 
#undef main		/* TEMP-FIX */
main(argc, argv)
#else
nclrun(argc, argv)
#endif
#endif

int argc;
char *argv[];
{
	int i,nc, ver_int;
	UU_LOGICAL iopt;
	char *ux_getenv(),*getenv(),*retstr, temp[256];
	UM_int2 idx,ival;
	UM_int4 cami, cadi, camo, lathe, tl3axis, prepst, vmill;
#if UU_COMP == UU_WINNT || UU_COMP == UU_WIN2K
    UX_pathname hdir;
#endif

	char *p,*tok,*strtok();
	static int loopvar=UU_TRUE;
			
	NCQ_running = 0;
/*printf("NCL Begin\n");*/
/*
.....Debug
*/
	if (p=getenv("UU_DEBUGL"))
	{
		i = 0;
		sscanf (p,"%d",&i);
		if (i != 0) fp = fopen("lp.lis","w");
		NclxMotTrace (NclxPrintf,i);
	}

/*
.....LOOP
*/
	if ((p=getenv("NCL_DEBUG_LOOP"))) while(loopvar);
/*
......Set UJ_plotting to false
.....added by Yurong 8/6/97
*/
	UJ_plotting = UU_FALSE;
	/* nisstr now set in znu_init_runvars() called from uu_init_usr_app() */

	if (((argc==2)&&(strncmp(argv[1], "-q=", 3)==0))
			|| ((argc==2)&&(strncmp(argv[1], "-b", 2)==0)))
	{
		putenv ("runtype=BATCH");
		if (strncmp(argv[1], "-q=", 3)==0)
			NCQ_running = 1;
	}
	if ((argc>=2)&&((strncmp(argv[1], "-n=", 3)==0)
			|| (strncmp(argv[1], "-N=", 3)==0)))
	{
		strcpy(temp, &(argv[1][3]));
/*
......format" -Nprocess"
......Yurong
*/
		NCL_subprocess = atol(temp);
	}
/* -- NCL: Determine if we are running BATCH, INTERACTIVE or NCL501MODE -- */
	retstr = ux_getenv("runtype", UX_NPRTERRS);
	if (retstr == UU_NULL) retstr = "NONE";
	if (strcmp(retstr,"INTER") == 0)   /* interactive */
		{

		ncl_init_auth("NCLCAM,NCLCADD");
#ifdef UU_PROFILE
		moncontrol(0)  ;  /* turn off the profiling  */
#endif
#if UU_COMP!=UU_VAXVMS
		nclsig();
#else
		uu_trap(); 
		ul_trap_control();  /* umbrella  */
#endif
/*
.....Store command line arguments
*/
		UU_comargc = 0;
		iopt = UU_TRUE;
		for(i = 1; (i<COMMAX)&&(i<argc); i++)
		{
/*
........Standard argument
*/
			if (argv[i][0] == '-' || iopt)
			{
				nc = strlen(argv[i]) + 1;
 				UU_comargv[UU_comargc] = (char *)malloc(nc);
				strcpy(UU_comargv[UU_comargc], argv[i]);
				if (argv[i][0] == '-') iopt = UU_TRUE;
				else iopt = UU_FALSE;
				UU_comargc++;
			}
			else
			{
				nc = strlen(UU_comargv[UU_comargc-1]) + strlen(argv[i]) + 2;
				p = (char *)malloc(nc);
				strcpy(p,UU_comargv[UU_comargc-1]);
				strcat(p," ");
				strcat(p,argv[i]);
				free(UU_comargv[UU_comargc-1]);
				UU_comargv[UU_comargc-1] = p;
			}
		}
		UU_comargc++;
/*
.....this will not to be a main function for WIN2K
.....return false to go to window function and call
.....unicad_() later
.....Yurong 7/6/00
*/
#if (UU_COMP==UU_WIN2K) 
		return 0;
#endif
		unicad_();
		}
	else if (strcmp(retstr,"BATCH") == 0) /*  batch  */
		{
			if (argc==2 && strlen(argv[1]) > 3)
			{
				strcpy(temp, &(argv[1][3]));
/*
......now format" -q=UID_NCQNUM
......Yurong
*/
				tok = strtok (temp, "_");
				Ncq_ncl_id = atol(tok);
				tok = strtok (NULL, "_");
				if (tok==NULL)
				{
					NCQ_filenum = 200;
				}
				else
				{
					NCQ_filenum = atoi(tok);
				}
/*
......check to see if a common memory space for NCL info data
......is opened by NCQ. It should opened already from NCQ
......unless the NCQ is abord or open failed.
......the name of the memory object will be named with 
......process id to be unique
*/
				uw_chk_common();
#if UU_COMP == UU_WIN2K
				uw_ntupdate_ncq();
#else
				uw_mfupdate_ncq(2);
#endif
			}
#ifdef UU_IRIX64
		sysmips(MIPS_FIXADE,1,0,0);
#endif
		/* Initialize some things from znu_init_runvars() */
		ncl_init_auth("NCLCAM");
		znu_init_units();
		znu_init_machinetype();

		/* DEFAULTS TO RUNNING EQUIVALENT OF NCLCAM */
		UU_application = UU_NCLCAM;

		/* Verify AUTHORIZATION for this terminal */
		cami = 0;
		cadi = 0;
		auth_batch(&camo,&lathe,&tl3axis,&prepst,&vmill);
		if (camo == -1)
			{
			ud_printmsg("Error trying to authorize CAM terminal.\n");
			exit(1);
			}
/*
.........RAH: set LATHE flag in FORTRAN. Fix MARK implemented in nclc/nauth.c.
.........I moved here because nclc/nauth.c must be application independant. 
*/
		UL_lathe = lathe;
		idx = 142;
		ival = UL_lathe;
		setifl (&idx, &ival);
/*
.........Set the 3AXIS mode and prepst flags in FORTRAN.
*/
		UL_3axis = tl3axis;
		idx = 304;
		ival = UL_3axis;
		setifl (&idx, &ival);
		idx = 351;
		ival = prepst;
		setifl (&idx, &ival);
		idx = 389;
		ival = vmill;
		setifl (&idx, &ival);
/*
.........Enable BATCH
*/
		idx=35;    /* set NCL batch flag  */
		ival=1;
		setifl(&idx,&ival);

		ncl_init_batch();
#if UU_COMP == UU_WINNT || UU_COMP == UU_WIN2K
		ul_get_full_dir("HOMEDIR",hdir);
#endif
		ul_open_logfile();
		/** Loop throu PART PROGRAM files */
		while (getpp() == 1)
			{
			UL_nposts = 0;
			ncl_reset_unicad();
			znu_init_units();
/*
.....Reinitialize note text
*/
			umu_set_def_drwscale();
			ua_init_drafting();
			ua_init_text();
#if UU_COMP == UU_WINNT || UU_COMP == UU_WIN2K
			chdir(hdir);
#endif
#if UU_COMP == UU_WIN2K
			uw_ntupdate_ncq();
#else
			uw_mfupdate_ncq(1);
#endif
			}
		ul_close_logfile();
/*
.....this will not to be a main function for WIN2K
.....return True to not go to window function and end
.....program in MFC function
.....Yurong 7/6/00
*/
#if (UU_COMP==UU_WIN2K) 
		return 1;
#else
		uw_mfupdate_ncq(0);
#endif
		}
/*
....Add code for ncl501 mode
....Sharon - 06Aug91
*/
	else if (strcmp(retstr,"NCLVT") == 0) /*  nclvt mode  */
		{
		/* Initialize some things from znu_init_runvars() */
		ncl_init_auth("NCLCAM");
		znu_init_units();
		znu_init_machinetype();

		/* DEFAULTS TO RUNNING EQUIVALENT OF NCLCAM */
		UU_application = UU_NCLCAM;

		/* Verify AUTHORIZATION for this terminal */
		cami = 0;
		cadi = 0;
        auth_vt(&camo,&lathe,&tl3axis);   
		if (camo == -1)
			{
			ud_printmsg ("Error trying to authorize CAM terminal.\n");
			exit(1);
			}
/*
.........RAH: set LATHE flag in FORTRAN. Fix MARK implemented in nclc/nauth.c.
.........I moved here because nclc/nauth.c must be application independant. 
*/
		UL_lathe = lathe;
		idx = 142;
		ival = UL_lathe;
		setifl (&idx, &ival);
/*
.........Set the 3AXIS mode flag in FORTRAN.
*/
		UL_3axis = tl3axis;
		idx = 304;
		ival = UL_3axis;
		setifl (&idx, &ival);
/*
.........Enable NCLVT
*/
		idx=35;    /* set NCLVT flag  */
		ival=2;
		setifl(&idx,&ival);

        ncl_init_batch();  
        
        runvt();  
        }
/*
....
*/
	else                /* error - run type not defined  */
		{
		ud_printmsg("\n RUNTYPE not defined to be BATCH, INTER, NCLVT or PLUS\n");
		}
/*
.....this will not to be a main function for WIN2K
.....return -1 to not go to window function and end
.....program in MFC function
.....Yurong 7/6/00
*/
#if (UU_COMP==UU_WIN2K) 
		return -1;
#endif
}
