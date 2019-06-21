/*********************************************************************
**	FILENAME: ldisplay.c
**	CONTAINS:		ulu_display
**				ul_display_file
**				ul_set_display_mod
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       ldisplay.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:10
*********************************************************************/

#include "usysdef.h"
#include "dasnog.h"
#include "dmark.h"
#include "driver.h"
#include "dtypes.h"
#include "lcom.h"
#include "lumb.h"
#include "udebug.h"
#include "uhep.h"
#include "ustdio.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "udfdata.h"
#include "udforms.h"

#define NODEFAULT 0
#define DEFAULT 1

/*********************************************************************
**	 E_FUNCTION : ul_display_(flag)
**			This function displays a file to a scrolling
**			window.
**	 PARAMETERS	
**		 INPUT  :  flag =	1 = Current part program.
**					2 = Current PR file.
**					3 = Current PR1 file.
**					4 = Current PU1 file.
**					5 = By filename.
**		 OUTPUT :  none.
**	 RETURNS:    none.
**	 SIDE EFFECTS: Opens a scrolling window and opens a file.
**	 WARNINGS:
*********************************************************************/

void ulu_display(flag)
	int flag;
{
	char file[UX_MAX_PATH_LEN];
	int numint;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ulu_display(flag)"));
	file[0] = '\0';
	switch (flag)
	{
/*
.....Current Part Program
*/
	case 1:
		strcpy (file,UL_program);
		strcat (file,".");
		strcat (file,UL_program_suffix);
		break;
/*
.....Current PR file
*/
	case 2:
		strcpy (file,UL_program);
		strcat (file,".pr");
		break;
/*
.....Current PR1 file
*/
	case 3:
		strcpy (file,UL_program);
		strcat (file,".pr1");
		break;
/*
.....Current PU1 file
*/
	case 4:
		strcpy (file,UL_program);
		strcat (file,".pu1");
		break;
/*
.....By filename
*/
	case 5:
/*		ud_das (UD_DASSTRING,"Enter filename to display",file,
			sizeof(file),&numint);*/
		ud_get_filename("Enter filename to display","DISPLAY FILE","*.*",
			file,&numint, "All Files (*.*)", 1, UU_FALSE);
/*
.....for WinNT, we allow filename with spaces, 
.....and the filebrowser will not have trailing spaces for WinNT
.....Yurong 1/17/02
*/
#if UU_COMP!=UU_WIN2K
		ul_strip_blanks (file,&numint);
#endif
		if (numint == 0) goto done;
		break;
	}
/*
.....Display file
*/
	ul_display_file (file);
done:;
	uu_dexit;
	return;
}

/*********************************************************************
**	 E_FUNCTION : ul_display_file(file)
**			This function opens a window and displays a file
**			to it.
**	 PARAMETERS	
**		 INPUT  :  file	= name of file to display.
**		 OUTPUT :  none.
**	 RETURNS:    UU_SUCCESS if successful or UU_FAILURE otherwise.
**	 SIDE EFFECTS: Opens a scrolling window and opens a file.
**	 WARNINGS:
*********************************************************************/

ul_display_file(file)
	char *file;
{
	int stat,status,wargs[3],numint,markval;
	char serror[UX_MAX_PATH_LEN],buf[UX_MAX_PATH_LEN],buf1[UX_MAX_PATH_LEN];
	int mode,file_status,i,n,nline;
	static int opend=0;
	static FILE *fptr;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_display_file(file)"));
	status = UU_SUCCESS;
	UL_ctrlc = 0;
/*
.....Command Reject
*/
	UD_MARK (markval,UU_FALSE);
	if (markval != 0)
	{
		ul_close_window();
		if (opend == 1) stat = ux_fclose0 (fptr);
		opend = 0;
		UD_UNMARK (markval);
		return (status);
	}
/*
.....Check to see if file exists
*/
	opend = 0;
	mode = UX_EXISTS|UX_READ;
	stat = ux_file_inquire(UU_NULL,UU_NULL,file,UU_NULL,UU_NULL,&mode,
		&file_status,buf,UX_NPRTERRS);
	if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
	{
		sprintf (serror,"%s does not exist.",file);
		ud_wrerr (serror);
		goto failed;
	}
/*
.....Open file for reading
*/
	stat = ux_fopen0 (file,"r",&fptr);
	if (stat != UU_SUCCESS)
	{
		sprintf (serror,"Cannot open %s.",file);
		ud_wrerr (serror);
		goto failed;
	}
	opend = 1;
/*
.....Open a scrolling window
*/
	wargs[1] = 1;
	if (ul_open_window(26,80,wargs) != UU_SUCCESS)
		goto failed;
/*
.....Read a record from file
*/
	nline = 0;
	do
	{
		stat = ul_fread (fptr,buf,sizeof(buf),&numint);
		if (UL_ctrlc == 1) stat = UX_EOF;
		if (stat == UU_SUCCESS || stat == UX_NO_SPACE)
		{
/*
.....Autowrap is enabled
.....Break record into multiple records
*/
			if (UL_display_wrap == 1)
			{
				i = strlen(buf);
				do
				{
					n = UL_wincol - 2;
					if (n > i) n = i;
					strcpy (buf1,buf);
					strcpy (buf,&buf1[n]);
					buf1[n] = '\0';
					stat = ul_win_out(buf1,nline);
					if (stat != UU_SUCCESS) break;
					i = strlen(buf);
					nline = UL_display_more;
				} while (i > 0);
			}
/*
.....Autowrap is disabled
.....Output only as many chars as will
.....fit in a single line
*/
			else
			{
				if (strlen(buf) > (UL_wincol-2))
					buf[UL_wincol-2] = '\0';
				stat = ul_win_out(buf,nline);
				nline = UL_display_more;
			}
		}
		else if (stat == UX_EOF)
		{
			ud_hakt (UX_UDOS,1);
		}
		else
		{
			sprintf (serror,"Error reading from %s E%d.",file,stat);
			ud_wrerr (serror);
			goto failed;
		}
	}
	while (stat == UU_SUCCESS || stat == UX_NO_SPACE);
	goto done;
/*
.....End of routine
*/
failed:;
	status = UU_FAILURE;
done:;
	if (opend == 1) stat = ux_fclose0 (fptr);
	ul_close_window();
	UD_UNMARK(markval);
	uu_dexit;
	return (status);
}
 
/*********************************************************************
**	 E_FUNCTION : ul_set_display_mod()
**			This function sets the file display modals.
**	 PARAMETERS	
**		 INPUT  :  none
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS:
*********************************************************************/
 
ul_set_display_mod()
{
	int status, wrap[4],more[4];
	int *ans[2];
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_ncl_start()"));
/*
.....Field 0 is Autowrap
*/
	wrap[0] = UL_display_wrap;
	ans[0] = (int *)wrap;
/*
.....Field 1 is Display/more
*/
	more[0] = UL_display_more;
	ans[1] = (int *)more;
/*
.....Get the Form input
*/
	status = ud_form("ldisplay.frm",ans,ans);
	if (status==-1)
	   return -1;
/*
.....Store modals
*/
	UL_display_wrap = wrap[0];
	UL_display_more = more[0];
/*
.....Free the form's allocated space
*/
	uu_dexit;
	return 0;
}
