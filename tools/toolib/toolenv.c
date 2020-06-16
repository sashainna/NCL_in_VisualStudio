/*********************************************************************
**    NAME         :  toolenv.c
**       CONTAINS:
**				tool_loadenv()
**				tool_load_envfile()
**				tool_rmtrail_space()
**				tool_addenv()
**				tool_getenv()
**				tool_getenv()
**				fgetenv()
**
**     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**     MODULE NAME AND RELEASE LEVEL
**       toolenv.c , 26.2
**		DATE AND TIME OF LAST MODIFICATION
**       04/12/18 , 11:16:12
*********************************************************************/
#include "usysdef.h"
#include <stdio.h>
#include "xfsys1.h"
#include "xenv1.h"
#include "nclver.h"

#define TOOL_MAXENV 500
static int TOOL_nenv=0;
static char TOOL_env[TOOL_MAXENV][256];
static char TOOL_envbuf[TOOL_MAXENV][UX_MAX_PATH_LEN];

#if (UU_COMP == UU_SUN) || (UU_COMP == UU_IRIS4D)
#ifndef UU_RS6000
#define fgetenv fgetenv_
#endif
#endif

/*******************************************************************
**   E_FUNCTION : tool_loadenv(err)
**              Loads the '.init' files.
**   PARAMETERS
**       INPUT  : None
**       OUTPUT : err: 1: error happened
**                     0: no err
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
tool_loadenv(err)
int *err;
{
	char *p,*tool_getenv();
	UX_pathname dir,fname,ext,fullname;
	char msg[256], init_fstr[UX_MAX_PATH_LEN];
	int ver_int, status;

	ver_int = NCL_version + 0.5;
	sprintf(init_fstr,"%%NCL%d_INIT_FILES", ver_int);
	status = tool_load_envfile(init_fstr,"nccs.init", msg);
	if (status!=0)
	{
#if UU_COMP == UU_WIN2K
		tool_mfmsg_box(NULL, "Error!", msg, 1);
#else
		printf(msg);
#endif
		*err = 1;
	}
	p = tool_getenv("UU_TOOLVARS");
	if (p != 0)
	{
		tool_break_fname(p,dir,fname,ext);
		tool_build_fname(fullname,dir,fname,ext);
		tool_load_envfile("",fullname, msg);
	}
	*err = 0;
}

/*******************************************************************
**   E_FUNCTION : tool_load_envfile(dir,fname,msg)
**              Loads the '.init' files.
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = Text field associated with browse button.
**          call_data   = Motif callback structure.
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
int tool_load_envfile(dir,fname,msg)
char *dir,*fname,*msg;
{
	int stat,nc,status;
	UX_pathname fullname,buf;
	char nlstr='\0',*p,*strchr();
	FILE *fd;
	void tool_addenv();

	status = 0;
/*
.....Open init file
*/
	tool_build_fname(fullname,dir,fname,&nlstr);
	fd = fopen(fullname,"r");
	if (fd == 0) goto failed;
/*
.....Read a line
*/
	do
	{
		stat = ul_fread(fd,buf,sizeof(buf),&nc,0);
/*
.....Parse line
.....Store in environmental table
*/
		if (stat == 0)
		{
			p = strchr(buf,'=');
			if (p != 0)
			{
				*p = '\0';
				tool_addenv(buf,++p);
			}
		}
#if UU_COMP==UU_WIN2K
		else
/*
.....need parse the last line
*/
		{
			if ((stat == UX_EOF)&&(nc>0))
			{
				buf[nc+1] = '\0';
			}
			p = strchr(buf,'=');
			if (p != 0)
			{
				*p = '\0';
				tool_addenv(buf,++p);
			}
		}
#endif
	} while (stat == 0);
	goto done;
/*
.....Could not open init file
*/
failed:;
	tool_short_filename(fullname,buf,50);
	sprintf(msg, "Could not open init file '%s'\n",buf);
	status = -1;
done:;
	return(status);
}

/*******************************************************************
**   E_FUNCTION : tool_rmtrail_space(string)
**              Removes trailing spaces from a string.
**   PARAMETERS
**       INPUT  :
**          string      = Character string.
**       OUTPUT :
**          string      = Character string with spaces removed.
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
tool_rmtrail_space(string)
char *string;
{
	int i, len;
	len = strlen (string);
	for (i=len-1; i>=0; i--)
	{
		if (string[i]!=' ')
		{
			string[i+1] = '\0';
			break;
		}
	}
}

/*******************************************************************
**   E_FUNCTION : tool_addenv(senv,sval)
**              Stores an environmental variable in the global table.
**   PARAMETERS
**       INPUT  :
**          senv        = Environmental variable to add.
**          sval        = Value to store with environmental variable.
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void tool_addenv(senv,sval)
char *senv,*sval;
{
	int i;
	UX_pathname sval1;
	char *p, senv1[256];
/*
.....remove empty space before/after string
*/
	p = senv;
	while (*p==' ') p++;
	strcpy(senv1, p);
	tool_rmtrail_space(senv1);

	p = sval;
	while (*p==' ') p++;
	strcpy(sval1, p);
	tool_rmtrail_space(sval1);
/*
.....See if this variable already exists
*/
	for (i=0;i<TOOL_nenv;i++)
	{
		if (strcmp(senv1,TOOL_env[i]) == 0)
		{
			strcpy(TOOL_envbuf[i],sval1);
			goto done;
		}
	}
	if (TOOL_nenv < TOOL_MAXENV)
	{
		strcpy(TOOL_env[TOOL_nenv],senv1);
		strcpy(TOOL_envbuf[TOOL_nenv],sval1);
		TOOL_nenv++;
	}
done:;
	return;
}

/*******************************************************************
**   E_FUNCTION : tool_getenv(senv)
**              Retrieves an environmental variable from the global
**					table or from the system table if it does not exist
**					in the global table.
**   PARAMETERS
**       INPUT  :
**          senv        = Environmental variable to retrieve.
**       OUTPUT : None
**   RETURNS:    Value for 'senv'.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
char *tool_getenv(senv)
char *senv;
{
	int i;
	char *p,*getenv();
/*
.....See if this variable exists
*/
	p = 0;
#if UU_COMP == UU_WINNT
	if (strlen(senv) != 2 || senv[1] != ':')
#endif
	{
		for (i=0;i<TOOL_nenv;i++)
		{
			if (strcmp(senv,TOOL_env[i]) == 0)
			{
				p = TOOL_envbuf[i];
				break;
			}
		}
		if (p == 0) p = getenv(senv);
	}
	return(p);
}

/*******************************************************************
**   E_FUNCTION : fgetenv()
**              This is the 'getenv' routine for the Windows NT platform.
**   PARAMETERS  
**       INPUT  :
**       OUTPUT :  
**   RETURNS:    none.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
void fgetenv(sym,val,nc)
char *sym,*val;
int *nc;
{
	char *p,*tool_getenv(),*strchr();
	UX_pathname dir,fname,ext,buf,tmp;
	int i;
	strncpy(buf,sym,79);
	p = strchr(buf,' ');
	*p = '\0';
	p = tool_getenv(buf);
	if (p == 0)
		strcpy(val,buf);
	else
		strcpy(val,p);
/*
.....Allow for NutCracker file names
*/
#if UU_COMP == UU_WINNT
	if (val[0] == '/' && val[2] == '=')
	{
		strcpy(tmp,val);
		val[0] = tmp[1];
		val[1] = ':';
		strcpy(&val[2],&tmp[3]);
	}
#endif
/*
.....Append trailing spaces
*/
	tool_break_fname(val,dir,fname,ext);
	tool_build_fname(val,dir,fname,ext);
	*nc = strlen(val);
}

