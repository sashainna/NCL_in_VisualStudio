/***********************************************************************
//
//   FILE NAME:  mainc.c
//   CONTAINS:
//			NpwCloseApp
//			NpwCreateDocument
//			NpwCreateMachine
//			NpwGetHelp
//			NpwGetWinLayout
//			NpwInitApp  
//			NpwLoadMachine
//			NpwPutAnswer
//			NpwPutForm
//			NpwWasLoaded
//			NpwSetDefault
//			NpwSaveMachine
//			NpwViewMachine
//
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        mainc.c , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 11:28:06
c
***********************************************************************/
#include "pwenv.h"
#ifndef WNT
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
/* 
......There only fot WNT include
......Yurong
*/
#ifdef WNT 
#include <direct.h>
#include <process.h>
#endif

#include "NpwHeaders.h"
#include <string.h>

int f77argc;
int f77argv[256];
/*
.....no fork function on VAX, use dumm function
.....Yurong
*/
#ifdef VAXVMS
pid_t fork (void) { return 0; }
int execlp (const char *file, const char *arg0, ...);
#endif
/*
......for HPX,VAX and WNT,we don't need define these, but
......for SGI, PLUTO, we do
......Yurong
*/
#ifndef WNT
#ifndef VAXVMS
#ifndef HP
#ifndef IBM
#define npw_close			npw_close_
#define npw_create_doc	npw_create_doc_
#define npw_get_form		npw_get_form_
#define npw_get_help		npw_get_help_
#define npw_load_machine	npw_load_machine_
#define npw_get_menu		npw_get_menu_
#define npw_init			npw_init_
#define npw_mchsim		npw_mchsim_
#define npw_put_answer	npw_put_answer_
#define npw_put_form		npw_put_form_
#define npw_save_machine	npw_save_machine_
#define npw_sim_view		npw_sim_view_
#define npw_was_loaded		npw_was_loaded_
#define npw_set_default		npw_set_default_
#endif
#endif
#endif
#endif

/*
.....added for WNT
.....Yurong
*/
fork() {return 0;}

/***********************************************************************
//
//   SUBROUTINE:  NpwCloseApp
//
//   FUNCTION:  C++ to Fortran interface.  Performs final shutdown.
//
//   INPUT:  none.
//
//   OUTPUT: none.
//
//   RETURNS:  none.
//
***********************************************************************/
int NpwCloseApp()
{
	npw_close();
	return(0);
}

/***********************************************************************
//
//   SUBROUTINE:  NpwCreateDocument(fil,msg1,msg2)
//
//   FUNCTION:  C++ to Fortran interface.  Creates the document file.
//
//   INPUT:  fil     C*1  Dn  -  Name of document file to create.
//
//   OUTPUT: msg1    C*1  Dn  -  Text of error message 1.
//
//           msg2    C*1  Dn  -  Text of error message 2.
//
//   RETURNS:  Non-zero on failure.
//
***********************************************************************/
int NpwCreateDocument(fil,msg1,msg2)
char *fil,*msg1,*msg2;
{
	int err;
	npw_create_doc(fil,msg1,msg2,&err);
	return(err);
}

/***********************************************************************
//
//   SUBROUTINE:  NpwCreateMachine(fil,msg1,msg2)
//
//   FUNCTION:  C++ to Fortran interface.  Creates a machine simulation
//              file.
//
//   INPUT:  kask    I*4  D1  -  0 = Need to ask about overwriting
//                               existing MS file.  1 = Already been
//                               asked.
//
//   OUTPUT: kas1    I*4  D1  -  Returns 1 if we need to ask about over-
//                               writing existing MS file.
//
//           msg1    C*1  Dn  -  Text of error message 1.
//
//           msg2    C*1  Dn  -  Text of error message 2.
//
//   RETURNS:  Non-zero on failure.
//
***********************************************************************/
int NpwCreateMachine(kask,cmsg1,cmsg2)
int *kask;
char *cmsg1,*cmsg2;
{
	int err;
#ifndef WNT
	mode_t cmask;
#endif
	npw_mchsim(kask,cmsg1,cmsg2,&err);
	if (err == 2)
	{
#ifndef WNT
		err = mkdir(cmsg1, cmask);
#else
		err = pw_mk_pthdir(cmsg1);
#endif
		if (!err)
		{
			*kask = 1;
			npw_mchsim(kask,cmsg1,cmsg2,&err);
		}
		else
		{
			strcpy(cmsg2,"Could not create Machine Simulation directory.");
		}
	}
	return(err);
}
/***********************************************************************
//
//   SUBROUTINE:  NpwGetAllHelp(NpwDynWinStruct *winStruct, char *helptext)
//
//   FUNCTION:  Gets the All Help Text for a specific menu.
//
//   INPUT:  winStruct  -   Structure which holds format of 
//                          Prompt Dialog and Form style Dialog.
//			flag: 1:        for prompt dialog
//				  2:		for form dialog
//
//   OUTPUT: helptext   -  Help text. Must alloc space before pass here
//
//
//   RETURNS:  -1 if error.
//
***********************************************************************/
NpwGetAllHelp(winStruct, helptext, flag)
NpwDynWinStruct *winStruct;
char *helptext;
int flag;
{
	char cmsg[82];
	int iflag,knc,kerr,myerr,i, num;
	int npr=0;
	char pbuf[60][20];
	num = 0;

	if (flag==2)
	{
		npr = 1;
		sprintf(pbuf[0],"%s.0", winStruct->winId);
	}
	else
	{
		for (i=0;i<winStruct->numButtons;i++)
		{
			if (winStruct->ButtonId[i] != 0)
			{
				sprintf(pbuf[npr],"%s.%d",winStruct->winId,
						winStruct->ButtonId[i]);
				npr++;
			}
		}
	}
	for (i=0;i<npr;i++)
	{
		iflag = 0;
		do
		{
			NpwGetHelp(pbuf[i],&(helptext[num]),&knc,&iflag,cmsg,&kerr);
			if (kerr == 0)
			{
				myerr = 0;
				if (helptext[num] == '\0') knc = 0;
				num = num + knc;
				helptext[num] = 13;
				num++;
				helptext[num] = '\n';
				num++;
				iflag = 1;
			}
		} while (kerr == 0);
		helptext[num] = 13;
		num++;
		helptext[num] = '\n';
		num++;		
	}
	helptext[num] = '\0';

	if(myerr != 0)
	{
		return -1;
	}
	return 0;
}

/***********************************************************************
//
//   SUBROUTINE:  NpwGetHelp(cprompt,cbuf,knc,kflag,cmsg,kerr)
//
//   FUNCTION:  C++ to Fortran interface.  Gets the Help Text for a
//              specific menu.  Returns it one line at a time.  This
//              routine must be called multiple times to get all Help
//              Text.
//
//   INPUT:  cprompt C*1  Dn  -  Prompt level for help.
//
//           kflag   I*4  D1  -  Set to 0 upon first entry.  Set to 1
//                               on each subsequent entry.
//
//   OUTPUT: cbuf    C*1  Dn  -  Help text.
//
//           knc     I*4  D1  -  Number of chars in 'cbuf'.
//
//           cmsg    C*1  Dn  -  Text of error message.
//
//           kerr    I*4  D1  -  Returns -1 when end of Help Text is
//                               reached, and non-zero for any other
//                               error.
//
//   RETURNS:  None.
//
***********************************************************************/
NpwGetHelp(cprompt,cbuf,knc,kflag,cmsg,kerr)
char *cprompt,*cbuf,*cmsg;
int *knc,*kflag,*kerr;
{
	npw_get_help(cprompt,cbuf,knc,kflag,cmsg,kerr);
	return 1;
}

/***********************************************************************
//
//   SUBROUTINE:  NpwGetWinLayout(level,nlev,wintyp,frmtyp,msg)
//
//   FUNCTION:  C++ to Fortran interface.  Gets the Window Layout for
//              the specified level.
//
//   INPUT:  level   I*4  D10 -  Prompt level for menu.
//
//           nlev    I*4  D1  -  Depth of 'level'.
//
//   OUTPUT: wintyp  S*1  D1  -  Structure which holds format of 
//                               Button or Prompt Menu.
//
//           frmtyp  S*1  D1  -  Structure which holds format of
//                               Form style Menu.
//
//           msg     C*1  Dn  -  Text of error message.
//
//   RETURNS:  Returns non-zero on error
//
***********************************************************************/
int NpwGetWinLayout(level,nlev,wintyp,frmtyp,msg)
int *level,*nlev;
char *msg;
NpwDynWinStruct *wintyp;
NpwDynFormStruct *frmtyp;
{
	int err;
	npw_get_menu(level,nlev,&(wintyp->numButtons),wintyp->Buttonname,
		wintyp->ButtonId,
		wintyp->Buttype,wintyp->Butdefault,wintyp->Butnumchoice,
		wintyp->Butdefchoice,wintyp->Butchoicestring,
		wintyp->winTitle,wintyp->winId,&(wintyp->winType),msg,&err);
	if (wintyp->winType >= 3)
	{
		npw_get_form(&(frmtyp->numRecords),frmtyp->recordString,
			&(frmtyp->numFields),&(frmtyp->totalFields),
			frmtyp->fieldStart,frmtyp->fieldEnd,
			frmtyp->fieldType,frmtyp->fieldLabel,
			frmtyp->numChoice,
			frmtyp->defaultChoice,frmtyp->choiceString);
			wintyp->numButtons = frmtyp->numRecords;
	}
	return(err);
}

/***********************************************************************
//
//   SUBROUTINE:  NpwInitApp(cprt,ctitle,argc,argv,cmsg)
//
//   FUNCTION:  C++ to Fortran interface.  Initializes MakePost.
//
//   INPUT:  argc    C*1  Dn  -  Command line arguments.
//
//           argv    I*4  D1  -  Number of arguments in 'argc'.
//
//   OUTPUT: cprt    C*1  Dn  -  Copyright notice.
//
//           ctitle  C*1  Dn  -  Title for Main Window.
//
//           cmsg    C*1  Dn  -  Text of error message.
//
//   RETURNS:  Returns non-zero on error
//
***********************************************************************/
int NpwInitApp(cprt,ctitle,argc,argv,cmsg)
int *argc;
char *cprt,*ctitle,**argv,*cmsg;
{
	int narg,kerr;
	char *cp,**cv,mbuf[UX_MAX_PATH+80];
/*
.....Store command line arguments
*/
	cv = argv;
	narg = *argc;
	mbuf[0] = '\0';
	while (--narg > 0 && (cp = *++cv))
	{
		strcat(mbuf,cp);
		strcat(mbuf," ");
	}
/*
.....Initialize MakePost
*/
	npw_init(cprt,ctitle,mbuf,cmsg,&kerr);
	return(kerr);
}

/***********************************************************************
//
//   SUBROUTINE:  NpwNTInitApp(cprt,ctitle,argc,argv,cmsg)
//
//   FUNCTION:  C++ to Fortran interface.  Initializes MakePost.
//
//   INPUT:  argc    C*1  Dn  -  Command line arguments.
//
//           argv    I*4  D1  -  Number of arguments in 'argc'.
//
//   OUTPUT: cprt    C*1  Dn  -  Copyright notice.
//
//           ctitle  C*1  Dn  -  Title for Main Window.
//
//           cmsg    C*1  Dn  -  Text of error message.
//
//   RETURNS:  Returns non-zero on error
//
***********************************************************************/
int NpwNTInitApp(cprt,ctitle,mbuf,cmsg)
char *cprt,*ctitle,*mbuf,*cmsg;
{
	int kerr;;
/*
.....Initialize MakePost
*/
	npw_init(cprt,ctitle,mbuf,cmsg,&kerr);
	return(kerr);
}

/***********************************************************************
//
//   SUBROUTINE:  NpwLoadMachine(fil,msg1,msg2)
//
//   FUNCTION:  C++ to Fortran interface.  Loads an MDF file.
//
//   INPUT:  fil     C*1  Dn  -  Name of MDF file to load.
//
//   OUTPUT: msg1    C*1  Dn  -  Text of error message 1.
//
//           msg2    C*1  Dn  -  Text of error message 2.
//
//   RETURNS:  Non-zero on failure.
//
***********************************************************************/
int NpwLoadMachine(fil,msg1,msg2)
char *fil,*msg1,*msg2;
{
	int err;
	npw_load_machine(fil,msg1,msg2,&err);
	return(err);
}

/***********************************************************************
//
//   SUBROUTINE:  NpwPutAnswer(ans,kpt,msg)
//
//   FUNCTION:  C++ to Fortran interface.  Returns the answers to a
//              Prompt Menu to MakePost.
//
//   INPUT:  ans     S*1  D1  -  Structure containing the Prompt numbers
//                               and answers for each prompt.
//
//   OUTPUT: kpt     C*1  Dn  -  Returns number of prompt which caused
//                               an error.
//
//           msg     C*1  Dn  -  Text of error message.
//
//   RETURNS:  Non-zero on failure.
//
***********************************************************************/
int NpwPutAnswer(ans,kpt,msg)
NpwReturnStruct *ans;
char *msg;
int *kpt;
{
	int err;
	npw_put_answer(ans->winId,&(ans->numButtons),ans->ButtonId,ans->Butanswer,
		msg,kpt,&err);
	return(err);
}

/***********************************************************************
//
//   SUBROUTINE:  NpwPutForm(ans,cform,kpt,msg)
//
//   FUNCTION:  C++ to Fortran interface.  Returns the answers to a
//              Form Menu to MakePost.
//
//   INPUT:  ans     S*1  D1  -  Structure containing the identity of
//                               the form.
//
//           cform   C*1  Dn  -  Answers to form.
//
//   OUTPUT: kpt     C*1  Dn  -  Returns number of form record which
//                               caused an error.
//
//           msg     C*1  Dn  -  Text of error message.
//
//   RETURNS:  Non-zero on failure.
//
***********************************************************************/
#ifdef WNT

int NpwPutForm(NpwReturnStruct *ans,char**cform,int*kpt,char*msg)
{
	int err, i, j;
/*
......For WinNT, pass double pointer to Fortran code has problems
......use double arrays as exactly Fortran code does
......Yurong 4/7/98
*/
	char tmp[MAXFMT][132];
	for (i=0; i<MAXFMT; i++)
		for (j=0; j<132; j++)
			tmp[i][j] = cform[i][j];
	npw_put_form(ans->winId,tmp,&(ans->numButtons),msg,kpt,&err);
	return(err);
}
#else

int NpwPutForm(ans,cform,kpt,msg)
NpwReturnStruct *ans;
char cform[MAXFMT][132],*msg;
int *kpt;
{
	int err;
	npw_put_form(ans->winId,cform,&(ans->numButtons),msg,kpt,&err);
	return(err);
}

#endif 
/***********************************************************************
//
//   SUBROUTINE:  NpwWasLoaded(fname)
//
//   FUNCTION:  C++ to Fortran interface.  Determines if an MDF file
//              was loaded in this session.
//
//   INPUT:  none
//
//   OUTPUT: fname   C*1  Dn  -  Returns the name of the MDF file to save.
//
//   RETURNS:  Returns 1 if an MDF file was previously loaded.
//
***********************************************************************/
int NpwWasLoaded(fname)
char *fname;
{
	int err;
	npw_was_loaded(fname,&err);
	return(err);
}

/***********************************************************************
//
//   SUBROUTINE:  NpwSetDefault(fname)
//
//   FUNCTION:  C++ to Fortran interface.  Sets the default directory
//              based.
//
//   INPUT:  fname   C*1  Dn  -  Contains the directory specification that
//                               will be used as the default directory.
//
//   OUTPUT: none
//
//   RETURNS:  Returns 1 if an MDF file was previously loaded.
//
***********************************************************************/
int NpwSetDefault(fname)
char *fname;
{
	int err;
	npw_set_default(fname,&err);
	return(err);
}

/***********************************************************************
//
//   SUBROUTINE:  NpwSaveMachine(kask,msg1,msg2)
//
//   FUNCTION:  C++ to Fortran interface.  Saves the active MDF file.
//
//   INPUT:  kask    I*4  D1  -  0 = Need to ask about overwriting
//                               existing MDF file.  1 = Already been
//                               asked.
//
//   OUTPUT: kask    I*4  D1  -  Returns 1 if we need to ask about over-
//                               writing existing MDF file.
//
//           msg1    C*1  Dn  -  Text of error message 1.
//
//           msg2    C*1  Dn  -  Text of error message 2.
//
//   RETURNS:  Non-zero on failure.
//
***********************************************************************/
int NpwSaveMachine(kask,msg1,msg2)
int *kask;
char *msg1,*msg2;
{
	int err;
	npw_save_machine(kask,msg1,msg2,&err);
	return(err);
}

/***********************************************************************
//
//   SUBROUTINE:  NpwViewMachine(cmsg1,cmsg2)
//
//   FUNCTION:  C++ to Fortran interface.  Spawns the Machine Simulator
//              to view the current machine configuration.
//
//   INPUT:  none.
//
//   OUTPUT: msg1    C*1  Dn  -  Text of error message 1.
//
//           msg2    C*1  Dn  -  Text of error message 2.
//
//   RETURNS:  Non-zero on failure.
//
***********************************************************************/
int NpwViewMachine(cmsg1,cmsg2)
char *cmsg1,*cmsg2;
{
	int err,stat;
	char fnam[UX_MAX_PATH];
/*
.....Get name of Machine Simulation File
*/
	npw_sim_view(fnam,cmsg1,cmsg2,&err);
/*
.....Spawn the Machine Simulator
*/
	if (err == 0)
	{
		if ((stat = fork()) != 0)
		{
			if (stat < 0) goto failed;
		}
		else
		{
			stat = execlp("ms","ms","-file",fnam,(char *)0);
			if (stat == -1) goto failed;
		}
	}
	goto done;
/*
.....Could not spawn the Machine Simulator
*/
failed:;
	err = 1;
	strcpy(cmsg1,"Could not start up the Machine Simulator.");
	cmsg2[0] = '\0';
done:;
	return(err);
}

