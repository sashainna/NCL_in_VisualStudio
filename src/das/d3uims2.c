/*********************************************************************
**
**    NAME         :  d3uims.c
**
**       CONTAINS:
**				ud_wrerr
**				ud_wrprm
**				ud_prmerr
**				ud_killmsg
**				ud_wrstat
**				ud_killstat
**				ud_get_filename
**				ud_winerror		
**				ud_load_accel()
**				ud_get_dirname()
**				ud_yesno
**				ud_yesno2
**				ud_bcomlin
**				ud_ecomlin
**				ud_updatews()
**				ud_format_file_filter()
**				ud_chkwin_event()
**				ud_yesnof
**
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**
C*     MODULE NAME AND RELEASE LEVEL 
C*       d3uims2.c , 25.4
C*    DATE AND TIME OF LAST  MODIFICATION
C*       05/01/17 , 12:36:29
**
*********************************************************************/
#include "udebug.h"
#include "usysdef.h"
#include "zsysdep.h"
#include	"gtbl.h"
#include "g.h"
#include "driver.h"
#include "dsubcom.h"
#include "gentry.h"
#include "usysg.h"
#include "dasnog.h"
#include "dinput.h"
#include "uims.h"
#include "mfort.h"
#include "nclfc.h"
#include "wsmf.h"
#include "gdidd.h"
#include "zkeysym.h"
#include "lcom.h"
#include "mpocket.h"
#include "mxxx.h"
#include "dmotif.h"

void ud_winerror();
void ud_killmsg();
extern int UL_clswin_flag;
extern int MSLite;

static void S_relative_path();

/*********************************************************************
**
**    E_FUNCTION         :  ud_wrerr(error)
**       write a line to the error line
**
**    PARAMETERS   
**       INPUT  : 
**          error = error message
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_wrerr(error)
char *error;					/* error message */
{
	UM_int2 ifl, ifl35=35;
/*
.....Ignore this routine in batch
.....Bobby  -  8/9/95
*/
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;	
/*
.....Motif error message
*/
	ud_winerror(error);
	return;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_prmerr(error)
**       write a prompt line to the error line and do not activate bell
**
**    PARAMETERS   
**       INPUT  : 
**          error = prompt message
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_prmerr(error)
char *error;					/* error message */
{
	UM_int2 ifl, ifl35=35;
/*
.....Ignore this routine in batch
.....Bobby  -  8/9/95
*/
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;

/*
.....Motif error message
*/
	(*(ug_gksstli.wsopen[0].connid)[UW_PRMERR])(error);
	return;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_wrprm(prompt)
**       write a line to the prompt line
**
**    PARAMETERS   
**       INPUT  : 
**          prompt = prompt message
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_wrprm(prompt)
char *prompt;					/* prompt message */
{

	UM_int2 ifl, ifl35=35;
/*
.....Ignore this routine in batch
.....Bobby  -  8/9/95
*/
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;

/*
.....Motif prompt message
*/
	(*(ug_gksstli.wsopen[0].connid)[UW_WRPRM])(prompt);
	return;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_reset_prompt()
**       Resets all prompt lines
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_reset_prompt()
{

	UM_int2 ifl, ifl35=35;
/*
.....Ignore this routine in batch
.....Bobby  -  8/9/95
*/
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;
/*
.....Motif prompt message
*/
	(*(ug_gksstli.wsopen[0].connid)[UW_RESET_PROMPT])();
	return;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_killmsg(flag)
**       erase a prompt or error message
**
**    PARAMETERS   
**       INPUT  : 
**          flag = UD_PROMPTMSG or UD_ERRORMSG
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_killmsg(flag)
UU_LOGICAL flag;					/* prompt or error flag */
{
	UM_int2 ifl, ifl35=35;
/*
.....Ignore this routine in batch
.....Bobby  -  8/9/95
*/
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;

/*
.....Motif prompt message
*/
	if (flag == UD_PROMPTMSG)
		(*(ug_gksstli.wsopen[0].connid)[UW_WRPRM])(" ");
	else 
		(*(ug_gksstli.wsopen[0].connid)[UW_PRMERR])(" ");
	return;
}
#if UU_COMP!=UU_WIN2K
/*********************************************************************
**
**    E_FUNCTION         :  ud_wrstat(msg)
**       write a line to the status area
**
**    PARAMETERS   
**       INPUT  : 
**				field = status field number
**          status = status message
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_wrstat(field, loc, msg)
int field;						/* field number */
Gwpoint3 *loc;					/* l.l. location of message */
char *msg;						/* msg message */
{
	UM_int2 ifl, ifl35=35;
/*
.....Ignore this routine in batch
.....Bobby  -  8/9/95
*/
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;
/*
.....Motif
*/
	(*(ug_gksstli.wsopen[0].connid)[UW_WRSTAT])(field,msg);
	return;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_killstat(field)
**       erase a msg or error message
**
**    PARAMETERS   
**       INPUT  : 
**          field = field number to kill
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_killstat(field)
int field;								/* field number */
{

/*	-- delete the old msg segment -- */

	UM_int2 ifl, ifl35=35;
/*
.....Ignore this routine in batch
.....Bobby  -  8/9/95
*/
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;
/*
.....Motif
*/
	(*(ug_gksstli.wsopen[0].connid)[UW_WRSTAT])(field," ");
	return;
}
#endif
/*********************************************************************
**
**    E_FUNCTION :  ud_get_filename1(prompt,filter,filename,nc,descript,flag)
**       Get a filename from the user.
**
**    PARAMETERS   
**       INPUT  : 
**          prompt = Prompt to display.
**			descript = File type description. Used only for WIN2K now
**			filter = File type for filter.
**						it can be different set of filter for WIN2K
**						(have different set of File type description)
**						compiled with '|'. For UNIX, it will replace '|'
**						with ',' in function UW_GET_FILENAME 
**						because only one set of filter for UNIX
**			open_flag: TRUE: open file browser as "Open"
**							FALSE: open file browser as "Save as"
**
**       flag       = UU_TRUE = Relative pathnames allowed, UU_FALSE = Not.
**       OUTPUT :  
**          filename = Filename from user.
**          nc = Number of chars in Filename.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_get_filename1(prompt,title,filter,filename,nc,descript, open_flag,flag, paths, path_des)
char *prompt,*filter,*filename,*title,*descript, *paths, *path_des;
int *nc, open_flag;
{
	int i;
/*
.....Motif is active
.....Get filename from File Selection Dialog
*/
	(*(ug_gksstli.wsopen[0].connid)[UW_GET_FILENAME])(UU_NULL, title,filter,
		filename,nc,descript, open_flag, paths, path_des);
/*
.....Get relative directory path
*/
	if (flag) S_relative_path(filename,nc);
}
/*********************************************************************
**
**    E_FUNCTION :  ud_get_filename(prompt,filter,filename,nc,descript,flag)
**       Get a filename from the user.
**
**    PARAMETERS   
**       INPUT  : 
**          prompt = Prompt to display.
**			descript = File type description. Used only for WIN2K now
**			filter = File type for filter.
**						it can be different set of filter for WIN2K
**						(have different set of File type description)
**						compiled with '|'. For UNIX, it will replace '|'
**						with ',' in function UW_GET_FILENAME 
**						because only one set of filter for UNIX
**			open_flag: TRUE: open file browser as "Open"
**							FALSE: open file browser as "Save as"
**
**       flag       = UU_TRUE = Relative pathnames allowed, UU_FALSE = Not.
**       OUTPUT :  
**          filename = Filename from user.
**          nc = Number of chars in Filename.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_get_filename(prompt,title,filter,filename,nc,descript, open_flag, flag)
char *prompt,*filter,*filename,*title,*descript;
int *nc, open_flag;
{
	return ud_get_filename1(prompt,title,filter,filename,nc,descript, open_flag, flag, NULL, NULL);
}
/*********************************************************************
**
**    E_FUNCTION:ud_winerror(msg)
**       display error message
**
**    PARAMETERS   
**       INPUT  : 
**          msg: message to display
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_winerror(msg)
char *msg;
{
	UM_int2 ifl, ifl35=35;
/*
.....Ignore this routine in batch
*/
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;

	(*(ug_gksstli.wsopen[0].connid)[UW_ERROR_MSG])(msg);
}


/*********************************************************************
**	 E_FUNCTION : ud_load_accel()
**		This function will translate all key defination
**		in 
**		and create global accelerator array
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
void ud_load_accel()
{
	(*(ug_gksstli.wsopen[0].connid)[UZ_LOAD_ACCEL])();
}

#if UU_COMP==UU_WIN2K
/*********************************************************************
**
**    E_FUNCTION         :  ud_wrstat(name, msg)
**       write a line to the status area
**
**    PARAMETERS   
**       INPUT  : 
**			name = status name
**          status = status message
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_wrstat(name, msg)
char *name, *msg;					
{	
	int i;
	UM_int2 ifl, ifl35=35;
/*
.....Ignore this routine in batch
.....Bobby  -  8/9/95
*/
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;
/*
.....changed the name to filed sub number
*/
	i = 0;
	while (UU_TRUE)
	{
		if (strcmp(UZ_statfuncs[i].name,"~END~") == 0) break;
		if (strcmp(UZ_statfuncs[i].name,name) == 0)
		{	
			(*(ug_gksstli.wsopen[0].connid)[UW_WRSTAT])(i,msg);
			return;
		}
		i++;
	}
}
#endif
/*********************************************************************
**
**    E_FUNCTION         :  ud_get_dirname(prompt,filename,nc)
**       Get a directory name from the user.
**
**    PARAMETERS   
**       INPUT  : 
**          prompt = Prompt to display.
**       OUTPUT :  
**          filename = Filename from user.
**          nc = Number of chars in Filename.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_get_dirname1(prompt,title,filename,nc, paths, path_des)
char *prompt,*filename,*title, *paths, *path_des;
int *nc;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_GET_DIRNAME])(UU_NULL, title, filename,
		nc, paths, path_des);
}

void ud_get_dirname(prompt,title,filename,nc)
char *prompt,*filename,*title;
int *nc;
{
	ud_get_dirname1(prompt,title,filename,nc, NULL, NULL);
}

/*********************************************************************
**
**    E_FUNCTION     :  ud_yesno(parent, msg, title)
**       ask user answer for yes or no or cancel
**			for Motif interface only
**    PARAMETERS
**       INPUT  :
**			parent: parent window
**         msg:   Message display in question box 
**         title: Title for question box
**       OUTPUT :
**          none
**
**    RETURNS      : UU_TRUE is yes and UU_FALSE if no
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
ud_yesno(parent, msg, title)
int *parent;
char *msg, *title;
{
	int answer;
	answer = (*(ug_gksstli.wsopen[0].connid)[UW_YESNOCANCEL])(parent, msg,  title);
	if (answer==-1)
	{
/*
.....if canceled, act as Rejust Op
*/
		ud_jump(-1, UU_FALSE);
	}
	return answer;
}
/*********************************************************************
**
**    E_FUNCTION     :  ud_yesno2(parent, msg, title)
**       ask user answer for yes or no
**			
**    PARAMETERS
**       INPUT  :
**			parent: parent window
**         msg:   Message display in question box 
**         title: Title for question box
**       OUTPUT :
**          none
**
**    RETURNS      : UU_TRUE is yes and UU_FALSE if no
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int ud_yesno2(parent, msg, title)
int *parent;
char *msg, *title;
{
	int answer;
	answer = (*(ug_gksstli.wsopen[0].connid)[UW_YESNO])(parent, msg,  title);
	return answer;
}

/**************************************************************************
**
**  E_FUNCTION:  ud_bcomlin()
**      The insert cursor of the command line go to the beginning
**
**  PARAMETERS   
**      INPUT  :  none
**
**      OUTPUT :  none
**  RETURNS      :  None
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void ud_bcomlin()
{
	(*(ug_gksstli.wsopen[0].connid)[UW_BCOMLINE])();
}


/**************************************************************************
**
**  E_FUNCTION:  ud_ecomlin()
**      The insert cursor of the command line go to the end of the line
**
**  PARAMETERS   
**      INPUT  :  none
**
**      OUTPUT :  none
**  RETURNS      :  None
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void ud_ecomlin()
{
	(*(ug_gksstli.wsopen[0].connid)[UW_ECOMLINE])();
}
/*********************************************************************
**    E_FUNCTION     : ud_updatews(mode)
**       Updates the screen display.
**    PARAMETERS
**       INPUT  :
**          mode    - UG_PERFORM  = Update entire display.
**                    UG_SUPPRESS = Update dirty area only.
**			
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_updatews(mode)
int mode;
{
	UM_int2 ifl, ifl35=35;
/*
.....Ignore this routine in batch
*/
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;	
	gupdatews(UD_ksws,mode);
}

/*********************************************************************
**
**    E_FUNCTION         :  S_relative_path(filename,nc)
**       Determines if the any portion of the input file path and
**       the default directory match and then shortens the input path
**       to a relative pathname if it does.
**
**    PARAMETERS   
**       INPUT  : 
**          filename = Filename from user.
**          nc       = Number of chars in filename.
**       OUTPUT :  
**          filename = Updated filename with possible relative path.
**          nc       = Number of chars in filename.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
static void S_relative_path(filename,nc)
char *filename;
int *nc;
{
	int ncp,inc,i;
	char *p;
#ifndef UU_RS6000
	char *strrchr();
#endif
	UX_pathname dir,path;
/*
.....Remove trailing and preceding spaces
*/
	for (i=0; i<*nc; i++)
	{
		if (filename[i]!=' ') break;
	}
	strcpy(filename, &(filename[i]));
	for (i=strlen(filename); i>0; i--)
	{
		if (filename[i-1]==' ')
			filename[i-1] = '\0';
		else
			break;
	}
/*
......Get default directory
*/
	getcwd(path,UX_MAX_PATH_LEN);
/*
......See if path is relative to current directory
*/
	ncp = strlen(path);
	if (strncmp(path,filename,ncp) == 0)
	{
		inc = ncp;
		if (filename[inc] == UX_PATH_SEP) inc++;
		strcpy(filename,&filename[inc]);
		*nc = strlen(filename);
	}
/*
.....See if path is one level up
*/
	else
	{
		strcpy(dir,path);
		ncp = strlen(dir);
		if (dir[ncp-1] == UX_PATH_SEP) dir[ncp-1] = '\0';
		p = strrchr(dir,UX_PATH_SEP);
		if (p != 0)
		{
			*p = '\0';
			ncp = strlen(dir);
			if (strncmp(dir,filename,ncp) == 0)
			{
				sprintf(dir,"..%c",UX_PATH_SEP);
				inc = ncp;
				if (filename[inc] == UX_PATH_SEP) inc++;
				strcat(dir,&filename[inc]);
				strcpy(filename,dir);
				*nc = strlen(filename);
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ud_format_file_filter(sfilter,fext);
**			Format the supplied file filter for the browser.
**    PARAMETERS
**       INPUT  :
**          fext     = File filter in the format of "fext1,fext2,fext3".
**
**       OUTPUT :
**          sfilter  = File filter formated for a file browser
**                     (*.fext1,*.fext2,*.fext3);
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_format_file_filter(sfilter,fext)
char *sfilter,*fext;
{
	char *p,*q,*strchr();
	char sbuf[UX_SUFFIX_LEN];
/*
.....Initialize routine
*/
	strcpy(sbuf,fext);
	ul_remove_quotes(sbuf);
	strcat(sbuf,",");
	p = sbuf;
	sfilter[0] = '\0';
/*
.....Format browser filter
*/
	do
	{
		strcat(sfilter,"*.");
		q = strchr(p,',');
		strncat(sfilter,p,q-p+1);
		p = q + 1;
	} while (*p != '\0');
	sfilter[strlen(sfilter)-1] = '\0';
}
/*********************************************************************
**    E_FUNCTION     : ud_chkwin_event();
**			Check if there is a window event in the mesage queue
**    PARAMETERS
**       INPUT  :none
**
**       OUTPUT :none
**    RETURNS      : 0: no event in the queue
**					1: yes, there is a message in the queue
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ud_chkwin_event()
{
	return (*(ug_gksstli.wsopen[0].connid)[UW_CHKWIN_EVENT])();
}
/*********************************************************************
**
**    E_FUNCTION     :  ud_yesnof(parent, msg, title)
**       ask user answer for yes or no
**		For call from Fortran routine 
**    PARAMETERS
**       INPUT  :
**			parent: parent window
**         msg:   Message display in question box 
**         title: Title for question box
**       OUTPUT :
**          none
**
**    RETURNS      : UU_TRUE is yes and UU_FALSE if no
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int ud_yesnof(msg, title)
char *msg, *title;
{
	return ud_yesno2(NULL, msg, title);
}

