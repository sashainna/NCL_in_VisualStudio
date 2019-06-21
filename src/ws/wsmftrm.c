#include "usysdef.h"
#if UU_COMP != UU_WIN2K

/********************************************************************* 
**  NAME:  wsmftrm.c
**
**       Motif workstation terminal emulations.
**
**  COPYRIGHT  1996  NCCS Inc. (c) All rights reserved.
**
**	 CONTAINS:
**
**		uw_open_mf_window
**		uw_close_mf_window
**		uw_mf_win_out
**		uw_open_xw_syswindow(ifl,tt)
**
**     MODULE NAME AND RELEASE LEVEL
**       wsmftrm.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:13
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:DialogS.h>
#include <decw$include:Text.h>
#include <decw$include:PanedW.h>
#include <decw$include:Protocols.h>
#include descrip
#include dvidef
#include iodef
#include dcdef
#include ttdef
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <Xm/DialogS.h>
#include <Xm/Text.h>
#include <Xm/PanedW.h>
#include <Xm/Protocols.h>
#endif

#include "wsxw.h"
#include "wsmf.h"
#include "udebug.h"
#include "dasnog.h"
#include "uims.h"
#include <stdio.h>

#define MAXLIN 100

typedef struct
{
   int vis; /* visible flag */
   Widget shell;  /* display window id */
   Widget text;
   Widget pane;
   XmTextPosition pos;
	Position x,y,w,h;
   int nrows;  /* # of rows */
   int ncols;  /* # of cols */
} UW_MFwindow;

static UW_MFwindow term_window =
{
	UU_FALSE,
	NULL,
	NULL,
	NULL,
	0,
	-1,0,0,0,
	24,
	80,
};
/*
.....added by Yurong
.....8/27/97
*/
extern int XBorder,YBorder;
extern UWS_MF uw_mf;

void uw_mf_win_out();

/*********************************************************************
**	 E_FUNCTION:int uw_open_mf_window(line,cols)
**		This function opens a scrolling window for output.
**	 PARAMETERS	
**		 INPUT  : line = # of lines in window
**			  col = # of columns in window
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int uw_open_mf_window(line,cols)
	int line,cols;
{
	int status,n;
	short r,c;
	Atom watom;
	Arg args[20];
/*	char geom[20];*/
	void uw_mfquitWin();
/*
.....Assume success
*/
	status = UU_SUCCESS;
	term_window.vis = UU_TRUE;
/*
.....Window already created
.....Just remap it
*/
	if (term_window.shell != NULL)
	{
		XtManageChild(term_window.text);
		XtManageChild(term_window.pane);
		uw_mf_win_out(" "); uw_mf_win_out(" ");
		XtPopup(term_window.shell,XtGrabNone);
		return(status);
	}
/*
.....Locate the scrolling window at
.....the lower left corner of the graphics window
*/
/*
	n = 0;
	XtSetArg(args[n],XmNx,&x); n++;
	XtSetArg(args[n],XmNy,&y); n++;
	XtSetArg(args[n],XmNwidth,&wid); n++;
	XtSetArg(args[n],XmNheight,&hgt); n++;
	XtGetValues(uw_mf.graphic_app,args,n);
	y = uw_xw.dev_ymax - (hgt + y);
	sprintf(geom,"+%d-%d",x,y);
*/
/*
....Open PopUp dialog window
*/
	n = 0;
	XtSetArg(args[n],XmNtitle,"Status Window"); n++;
	XtSetArg(args[n],XmNallowResize,True); n++;
/*
	if (term_window.x > 0)
	{
		sprintf(geom,"%dx%d+%d+%d",term_window.w,term_window.h,
			term_window.x,term_window.y);
		XtSetArg(args[n],XmNgeometry,geom); n++;
	}
*/
	term_window.shell = XtCreatePopupShell("statWindow",
		xmDialogShellWidgetClass, uw_mf.graphic_app, args,n);
/*
.....Trap the CLOSE button
*/
	watom =  XmInternAtom(uw_xw.disp,"WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(term_window.shell,watom,uw_mfquitWin,NULL);
/*
.....Create paned window to hold text window
*/
	term_window.pane = XtVaCreateWidget("pane",
		xmPanedWindowWidgetClass,term_window.shell,
		XmNsashWidth,1,XmNsashHeight,1,NULL);
/*
.....Place scrolling text widget in dialog
*/
	n = 0;
	r = line; c = cols;
	XtSetArg(args[n], XmNrows, r); n++;
	XtSetArg(args[n], XmNcolumns, c); n++;
	XtSetArg(args[n], XmNresizeWidth, True); n++;
	XtSetArg(args[n], XmNresizeHeight, True); n++;
	XtSetArg(args[n], XmNscrollVertical, True); n++;
	XtSetArg(args[n], XmNscrollHorizontal, False); n++;
	XtSetArg(args[n], XmNeditable, False); n++;
	XtSetArg(args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
	term_window.text = XmCreateScrolledText(term_window.pane,
		"statWindow",args,n);
/*
.....Blank out the text window
*/
	XmTextSetString(term_window.text,"");
	term_window.pos = 0;
/*
.....Manage the text window
*/
	XtManageChild(term_window.text);
	XtManageChild(term_window.pane);
	XtPopup(term_window.shell,XtGrabNone);
/*
.....Save the window characteristics
*/
	term_window.nrows = line;
	term_window.ncols = cols;
	goto done;
/*
.....Failure
*/
failed:;
	term_window.vis = UU_FALSE;
	status = UU_FAILURE;
done:;
	return(status);
}

/**************************************************************************
**
**  E_FUNCTION:  uw_close_mf_window
**      Close the X-Window terminal emulation window.
**
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int uw_close_mf_window()
{
/*
.....Window is visible
.....Destroy it
*/
	if(term_window.vis == UU_TRUE)
	{
		XtVaGetValues(term_window.shell,
			XmNx,&term_window.x,
			XmNy,&term_window.y,
			XmNwidth,&term_window.w,
			XmNheight,&term_window.h,
			NULL);
		term_window.x -= XBorder;
		term_window.y -= YBorder;
		term_window.vis = UU_FALSE;
		XtUnmanageChild(term_window.text);
		XtUnmanageChild(term_window.pane);
		XtPopdown(term_window.shell);
	}
	return 1;
}

/**************************************************************************
**
**  E_FUNCTION:  uw_mfquitWin(buff)
**      Called when the user closes the scrolling window manually.
**
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uw_mfquitWin()
{
	uw_close_mf_window();
	term_window.shell = NULL;
}

/**************************************************************************
**
**  E_FUNCTION:  uw_mf_win_out(buff)
**      Write a string to the scrolling window window.
**
**  PARAMETERS   
**      INPUT  :  buff	:	string to be written out
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uw_mf_win_out(buff)
char *buff;
#define tab 9
#define field 7
{
	int incy,i,j,k,r,nc,nspc;
	char tmp[256];
/*
.....Window is visible
*/
	if(term_window.vis == UU_TRUE)
	{
/*
.....Convert TABS to spaces
*/
		j = 0;
		nc = strlen(buff);
		for (i=0;i<nc;i++)
		{
			if (buff[i] == tab)
			{
				r = j - field * (j/field);
				nspc = field - r;
				for (k=j; nspc-->0;k++)
				{
					tmp[k] = ' ';
				}
				j = j + (field-r);
			}
			else
			{
				tmp[j] = buff[i];
				j++;
			}
		}
		tmp[j] = '\0';
/*
.....Convert CR/LF to NewLine
.....Assume it can only be at the end
.....of the string
*/
		incy = 0;
		for (i=strlen(tmp)-2;i<strlen(tmp);i++)
		{
			switch(tmp[i])
			{
			case '\015':
				tmp[i] = '\0';
				break;
			case '\012':
				incy = 1;
				tmp[i] = '\0';
				break;
			}
		}
		strcat(tmp,"\n");
/*
.......Write the line to the window
*/
#if UU_COMP != UU_VAXVMS
#if UU_COMP != UU_IRIS4D
#if UU_COMP != UU_HPUX
		XmTextDisableRedisplay(term_window.text);
#endif
#endif
#endif /* VAXVMS */
		XmTextInsert(term_window.text,term_window.pos,tmp);
		term_window.pos = term_window.pos + strlen(tmp);
/*		XtVaSetValues(term_window.text,XmNcursorPosition,term_window.pos,NULL);*/
		XmTextShowPosition(term_window.text,term_window.pos);
#if UU_COMP != UU_VAXVMS
#if UU_COMP != UU_IRIS4D
#if UU_COMP != UU_HPUX
		XmTextEnableRedisplay(term_window.text);
#endif
#endif
#endif
	}
	return;
}

/*********************************************************************
**	 E_FUNCTION:int uw_open_xw_syswindow(ifl,tt)
**		This function opens an X=window terminal emulation
**		window for spawning a system subprocess.
**	 PARAMETERS	
**		 INPUT  : ifl =	0 = NCL System Window. (enter system)
**				1 = NCL Process Window. (spawn subprocess)
**		 OUTPUT : tt = name of the created terminal emulation window.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int uw_open_xw_syswindow(ifl,tt)
int ifl;
char *tt;
{
	int status;
#ifdef VMS
	char *p;
	char dev[80],vt200[3],title[19];
	char param_list[511] = "\0";
	char setup_file[511] = "\0";
	int statvax,i,ttchr[2],pid;
	short devlen,*j;
	$DESCRIPTOR (devnam, dev);
	$DESCRIPTOR (image_file, "sys$system:loginout.exe");
	char *plist =
"DECW$TERMINAL.main.terminal.background:Black\nDECW$TERMINAL.main.terminal.foreground:White\n*autoResizeTerminal:False\n*terminalMode:VT100\nDECW$TERMINAL.x:3\nDECW$TERMINAL.y:595\nDECW$TERMINAL.title:";
/*
.....Assume success
*/
	status = UU_SUCCESS;
/*
.....Create a terminal emulation window
*/
	if (uw_xw.disp == 0)
	{
		if ((uw_xw.disp = XOpenDisplay("")) == NULL)
		{
			printf("Display %s is not available\n",XDisplayName(""));
			goto failed;
		}
	}
	strcpy (param_list, plist);
	if (ifl == 0) strcat (param_list,"NCL System Window ");
	else strcat (param_list,"NCL Process Window");
	statvax = DECwTermPort(uw_xw.disp, &setup_file[0], &param_list[0],
		&dev[0], &devlen);
	if (statvax != 1)
	{
		printf ("\n Failed with status %d\n", statvax);
		goto failed;
	}
	dev[devlen] = '\0';
	strcpy (tt,dev);
/*
.....You must attach the terminal
.....to make it visible
*/
	for (i=devlen+1;i<80;i++) dev[i] = ' ';
	statvax = sys$assign (&devnam,&xw_win_chan,0,0);
	if (!statvax) goto failed;
/*
.....The window must first exist before
.....changing the size
*/
	ttchr[0] = 0;
	ttchr[1] = TT$M_LOWER | TT$M_SCOPE;
	p = &ttchr;
	j = &ttchr;
	*p = DC$_TERM;
	*(p+1) = TT$_VT200_SERIES;
	*(j+1) = 80;
	*(p+7) = 24;
	statvax = sys$qiow (0,xw_win_chan,IO$_SETMODE,0,0,0,&ttchr,0,0,0,0,0);
	if (!statvax) goto failed;
/*
.....Open the window for writing
*/
	xw_win_fp = fopen(dev,"w");
	if (xw_win_fp == 0) goto failed;
	goto done;
#else
/*
.....Open Unix System Window
.....by raising parent window
*/
	int i;
/*
.....Assume success
*/
	status = UU_SUCCESS;
	i = XLowerWindow(uw_xw.disp,uw_xw.wd_id);
/*	i = XRaiseWindow(uw_xw.disp,RootWindow(uw_xw.disp,uw_xw.screen_no));*/
	XFlush(uw_xw.disp);
	goto done;
#endif
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}
#endif
