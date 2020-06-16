#include "usysdef.h"
/***********************************************************************
**
**   FILE NAME:  toolmfc.c
**			contains C-FORTRAN connect functions
**			cause we can't pass window varibles in FORTRAN routines
**   CONTAINS:
**			toolc_mfyesno(title, msg, nc, ans)
**			toolc_mfmsg_box(title, msg, nc, flag)
**			toolc_get_filen(title, filter, fnam, nc)   
**			toolc_mfprompt(title, msg, nc, ln, cols, ans_str, snc)   
**			shfile
**   
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       toolmfc.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:13:31
***********************************************************************/
#if (UU_COMP == UU_SUN) || (UU_COMP == UU_IRIS4D)
#ifndef UU_RS6000
#define toolc_mfyesno toolc_mfyesno_
#define toolc_mfyesnocancel toolc_mfyesnocancel_
#define toolc_getopt_filen toolc_getopt_filen_
#define toolc_mfmsg_box toolc_mfmsg_box_
#define toolc_mfprompt toolc_mfprompt_
#define shfile shfile_
#endif
#endif

/**********************************************************************
**    I_FUNCTION : toolc_mfyesno(title, msg, nc, ans)
**       popup a dialog box for yes or no choice
**    PARAMETERS
**       INPUT  :
**				title: Dialog title
**				msg     = prompt text
**          nc = message length
**       OUTPUT :
**				ans : 1= NO
**						0= YES
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
void toolc_mfyesno(title, msg, nc, ans)
char *title, *msg;
int *nc, *ans;
{
	msg[*nc] = '\0';
	*ans = tool_mfyesno(title, msg, 0);
}

/**********************************************************************
**    I_FUNCTION : toolc_mfyesnocancel(title, msg, nc, ans)
**       popup a dialog box for yes, no, or cancel choice
**    PARAMETERS
**       INPUT  :
**				title: Dialog title
**				msg     = prompt text
**          nc = message length
**       OUTPUT :
**				ans : 1= NO
**						0= YES
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
void toolc_mfyesnocancel(title, msg, nc, ans)
char *title, *msg;
int *nc, *ans;
{
	msg[*nc] = '\0';
	*ans = tool_mfyesnocancel(title, msg, 0);
}


/**********************************************************************
**    I_FUNCTION :  toolc_mfmsg_box(title, msg, nc, flag)
**       Creates and displays the message dialog.
**    PARAMETERS
**       INPUT  :
**          msg     = text to display.
**				title = Dialog title
**				nc = message length
**				flag = 1  (info message box)
**					  = 0  (error message box)
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Disables user interaction until the error
**                   message is acknowledged.
**    WARNINGS     : none
*********************************************************************/
void toolc_mfmsg_box(title, msg, nc, flag)
char *title, *msg;
int *nc, flag;
{
	msg[*nc] = '\0';
	tool_mfmsg_box(0, title, msg, flag);
}

/**********************************************************************
**    I_FUNCTION :  toolc_get_filen(title, filter, fnam, nc)
**       Opens a File Selection dialog and returns the user selected
**       filename.
**    PARAMETERS
**       INPUT  :
**          title     = Title of File Selection dialog.
**          filter    = Filename filter to use for list of available
**                      files.
**       OUTPUT :
**          filename  = Name of selected file.
**          nc        = Number of chars in 'filename'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
toolc_getopt_filen(title, ext, filter, fnam, nc)
char *title, *fnam, *filter, *ext;
int *nc;
{
	fnam[0] = '\0';
	tool_mf_filename(0, title, ext, filter, fnam, nc);
}

/**********************************************************************
**    I_FUNCTION : toolc_mfprompt(title, msg, nc, ln, cols, ans_str, snc)
**       popup a prompt dialog box (it can be mutiple lines
**        in text field)
**
**    PARAMETERS
**       INPUT  :
**          title:  prompt box title
**          msg:    prompt str.
**				nc :    msg length
**          ln: how many lines in text fields.
**          str_ans: user input in text field.
**				snc:	length of str_ans
**       OUTPUT :
**                none
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
toolc_mfprompt(title, msg, nc, ln, cols, ans_str, snc)
char *title, *msg, *ans_str;
int *nc, *ln, *cols, *snc;
{
	msg[*nc] = '\0';
	tool_mfprompt(0, title, msg, *ln, *cols, ans_str);
	*snc = strlen(ans_str);
}

/*********************************************************************
**       E_FUNCTION : shfile(fin,fout,maxc)
**            Shortens a filename if it is more than 'maxc' characters.
**            Used to output filenames in error messages, print files,
**            etc. (Fortran callable).
**       PARAMETERS     
**            INPUT  :
**               fin   = Input filename.
**               nci   = Number of chars in 'fin'.
**               maxc  = Maximum number of characters in 'fout'.
**            OUTPUT :
**               fout  = Output filename..
**       RETURNS:    none.
**       SIDE EFFECTS: none
**       WARNINGS:
*********************************************************************/
void shfile(fin,nci,fout,maxc)
char *fin,*fout;
int *maxc,*nci;
{
	char *flnam1,*flnam2;
	flnam1 = fin;
	flnam2 = fout;
	flnam1[*nci] = '\0';
	tool_short_filename(flnam1,flnam2,*maxc);
}
