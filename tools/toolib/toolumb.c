/*********************************************************************
**	FILENAME: toolumb.c
**	CONTAINS:		
**				tool_get_fname(fullname,fname)
**				tool_get_dir(dir,fullname)
**				tool_break_fname(fullname,dir,fname,fext)
**				tool_build_fname(fullname,dir,fname,fext)
**				tool_strip_blanks (str,size)
**				tool_short_filename
**				tool_init_font()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       toolumb.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       10/12/15 , 17:34:45
*********************************************************************/

#include "usysdef.h"
#include <stdio.h>
#include "string.h"
#include "xfsys1.h"
#include "xenv1.h"
#include <sys/types.h>
#include <sys/stat.h>

#if UU_COMP == UU_WIN2K
	static char *delim={"\\"};
#else
	static char *delim={"/"};
#endif

#ifndef UU_RS6000
char *strchr();
#endif

int Tool_font_size1, Tool_font_size2, Tool_font_size3;
char Tool_font_name1[80], Tool_font_name2[80], Tool_font_name3[80]; 
int UL_line_len=80;

/*******************************************************************
**   E_FUNCTION : tool_get_fname(fullname,fname)
**              This function get filename without path
**   PARAMETERS  
**       INPUT  :  fullname  = full filename specification.
**       OUTPUT :  
**		   fname = filename specification of 'fullname'.
**		     Blank if no filename is specified.
**   RETURNS:    none.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
void tool_get_fname(fullname,fname)
char *fullname,*fname;
{
	char *pointer;
	UX_pathname buf;

	strcpy (buf,fullname);

#if UU_COMP == UU_VAXVMS

   pointer = strrchr (buf,']');
	if (pointer != 0)
	{
		pointer++;
		strcpy (fname,pointer);
		*pointer = '\0';
	}
	else
	{
		pointer = strrchr (buf,':');
		if (pointer != 0)
		{
			pointer++;
			strcpy (fname,pointer);
			*pointer = '\0';
		}
		else
		{
			strcpy (fname,buf);
		}
	}
#else
	if (buf[0] == '\0')
	{
		strcpy(fname,buf);
	}
	else
	{
		pointer = strrchr (buf,delim[0]);
		if (pointer != 0)
		{
			pointer++;
			strcpy (fname,pointer);
			pointer--;
			*pointer = '\0';
		}
		else
		{
			strcpy (fname,buf);
		}
	}
#endif
}

/*******************************************************************
**   E_FUNCTION : tool_get_dir(dir,fullname)
**          This function takes the symbol 'dir' and returns the
**          full directory specification in 'fullname'.  'dir' can
**          be  a enviroment variable.
**   PARAMETERS
**       INPUT  :  dir  = directory symbol.
**       OUTPUT :  fullname = full directory specification of 'dir'.
**   RETURNS:    0: can't get valid directory
**				1: get valid durectory
**       otherwise.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
int tool_get_dir(dir,fullname)
char *dir,*fullname;
{
	char *p, *q, *tool_getenv();
	UX_pathname tmp, dir1;
	int ret, done = 0, first = 1;
/*
.....to extend all path
*/
	if (dir[0] == '\0')
	{
		fullname[0] = '\0';
		return 0;
	}
	strcpy(dir1, dir);
get_dir:;
/*
.....Get directory
*/
	strcpy(tmp,dir1);
	p = strchr(tmp,delim[0]);
	if (p != 0 && p != tmp) 
	{
		*p = '\0';
		strcpy(dir1, p+1);
		if (*(p+1)=='\0')
			done = 1;
	}
	else
		done = 1;
	if (strcmp(tmp,".") == 0)
	{
		tool_getwd(tmp);
		q = tmp;
	}
	else if (tmp[0] == '%')
	{
		q = tool_getenv(&tmp[1]);
		if (q==0) q = tmp;
	}
	else
		q = tmp;
	if (q != 0)
	{
		if (first)
		{
			strcpy(fullname, q);
			first = 0;
		}
		else
		{
			strcat (fullname, delim);
			strcat (fullname, q);
		}
	}
/*
.....we need check if this a valid directory
.....Yurong  6/27/01
*/
	if (tool_is_dir(fullname))
	{
		if (done)
			return 1;
		else
			goto get_dir;
	}
	else if (done)
		return 0;
	else
	{
		ret = tool_get_dir(fullname, fullname);
		if (ret==0)
			return 0;
		goto get_dir;
	}
}
int tool_is_dir(pathname)
char *pathname;
{
	int len;
	char *lq, *fq;
	char *right;
	UX_pathname noquote,dirname;
	struct stat stbuf;	

	strcpy(noquote,pathname);

#if UU_COMP == UU_VAXVMS
	if ((right = strrchr(noquote,']')) != UU_NULL)
		if ( *(right+1) == '\0')
		{
			if (ux_vaxdir0(noquote,dirname) != UU_SUCCESS)
				goto failed;
			strcpy(noquote,dirname);
		}
#endif

#if UU_COMP==UU_WIN2K
	len = strlen(noquote);
	if (noquote[len-1]=='\\')
		noquote[len-1] = '\0';
	len = strlen(noquote);
	if (noquote[len-1]==':')
		strcat(noquote, "\\");
#endif
	if  (stat(noquote, &stbuf) != 0)
		return 0;						
	if ((stbuf.st_mode & S_IFMT) == S_IFDIR)
		return 1;								
	else
		return 0;					
}	

/*******************************************************************
**   E_FUNCTION : tool_break_fname(fullname,dir,fname,ext)
**              Breaks a full filename specfication into its
**              subcomponents (directory, filename, file extension).
**   PARAMETERS  
**       INPUT  :  fullname  = full filename specification.
**       OUTPUT :  dir       = Directory.
**                 fname     = Base filename.
**                 ext       = File extension.
**   RETURNS:    none.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
void tool_break_fname(fullname,dir,fname,ext)
char *fullname,*dir,*fname,*ext;
{
	char *p;
/*
.....Get the directory
*/
	strcpy(dir,fullname);
	p = strrchr (dir,delim[0]);
	if (p != 0) *p = '\0';
	else dir[0] = '\0';
/*
.....Get the filename
*/
	tool_get_fname(fullname,fname);
/*
.....Break out the file base name and extension
*/
	p = strchr(fname,'.');
	if (p == 0)
		ext[0] = '\0';
	else
	{
		strcpy(ext,p+1);
		*p = '\0';
	}
}

/******************************************************************* 
**   E_FUNCTION : ul_build_full_fname(dir,fname,fext,fullname)
**              This function builds a full filename specification from
**              the main directory 'dir', filename 'fname', and file
**              extension 'fext'.
**   PARAMETERS
**       INPUT  :  fdir   = Default directory spec. (system dependent)
**                 fname  = Filename.
**                 fext   = File extension.
**       OUTPUT :  fullname = Full filename specification.
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void tool_build_fname(fullname,dir,fname,ext)
char *fullname,*dir,*fname,*ext;
{
	ul_build_full_fname(dir, fname, ext, fullname);	
}
/*********************************************************************
**	E_FUNCTION:	tool_strip_blanks (str,size)
**			This function removes any blanks from a text string
**    PARAMETERS   
**       INPUT  :	str = input text string
**			size =  # of characters in 'str'.
**       OUTPUT :  
**			str = returned string
**			size =  # of characters returned.
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
tool_strip_blanks (str,size)
char *str;
int *size;
{
	int i,j;
/*
.....Remove all blanks from string
*/
	j = 0;
	for (i=0;i<*size;i++)
	{
		if (str[i] == '\0') break;
		if (str[i] > ' ')
		{
			str[j] = str[i];
			j++;
		}
	}
	*size = j;
	str[j] = '\0';
	return;
}

tool_init_font()
{
	char *p,*tool_getenv();

	Tool_font_size1 = 80;
	Tool_font_size2 = 80;
	Tool_font_size3 = 80;
	strcpy (Tool_font_name1, "MS Sans Serif");
	strcpy (Tool_font_name2, "MS Sans Serif");
	strcpy (Tool_font_name3, "Courier");
	p = tool_getenv("UU_TOOLFONT1");
	if (p != 0)
	{
		ul_remove_quotes(p);
		strcpy(Tool_font_name1, p);
	}
	p = tool_getenv("UU_TOOLFONT2");
	if (p != 0)
	{
		ul_remove_quotes(p);
		strcpy(Tool_font_name2, p);
	}
	p = tool_getenv("UU_TOOLFONT3");
	if (p != 0)
	{
		ul_remove_quotes(p);
		strcpy(Tool_font_name3, p);
	}
	p = tool_getenv("UU_TOOLFONTSIZE1");
	if (p != 0)
	{
		Tool_font_size1 = 10*atoi(p);
		if ((Tool_font_size1>500)||(Tool_font_size1<0))
			Tool_font_size1 = 80;
	}
	p = tool_getenv("UU_TOOLFONTSIZE2");
	if (p != 0)
	{
		Tool_font_size2 = 10*atoi(p);
		if ((Tool_font_size2>500)||(Tool_font_size2<0))
			Tool_font_size2 = 80;
	}
	p = tool_getenv("UU_TOOLFONTSIZE3");
	if (p != 0)
	{
		Tool_font_size3 = 10*atoi(p);
		if ((Tool_font_size3>500)||(Tool_font_size3<0))
			Tool_font_size3 = 80;
	}
}

/*********************************************************************
**       E_FUNCTION : tool_short_filename(fin,fout,maxc)
**            Shortens a filename if it is more than 'maxc' characters.
**            Used to output filenames in error messages, print files,
**            etc.
**       PARAMETERS     
**            INPUT  :
**               fin   = Input filename.
**               maxc  = Maximum number of characters in 'fout'.
**            OUTPUT :
**               fout  = Output filename..
**       RETURNS:    none.
**       SIDE EFFECTS: none
**       WARNINGS:
*********************************************************************/
void tool_short_filename(fin,fout,maxc)
char *fin,*fout;
int maxc;
{
	ul_short_filename(fin,fout,maxc);
}

ud_yesno(parent, msg, title)
int *parent;
char *msg, *title;
{
	int answer;
	answer = tool_mfyesnocancel(parent, msg, title);
	if (answer==-1)
		answer = 0;
	return answer;
}
