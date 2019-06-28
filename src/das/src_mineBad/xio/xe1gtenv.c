/**************************************************************************
**
**  NAME:	xe1gtenv.c 
**
**      contains:   ux_getenv
**		    ux_getenv_list
**		    ux_init_table
**		    ux_modenv
**		    uxi_print_table
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			xe1gtenv.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/12/18 , 11:01:55
**
**************************************************************************/
#include "usysdef.h"
#include <stdio.h>
#if (UU_COMP == UU_WIN2K)
#include <stdlib.h>
#include <string.h>
#endif
#include "udebug.h"
#include "xfsys1.h"
#include "xenv1.h"
#include "unserve.h"
#include "derror.h"			/* needed for error system resolution */
#include "uhep.h"			/* used by the debug system */
#include "ulist.h"			/* used by the debug system */
#include "mfort.h"
#include "nclver.h"
#include "xfsys0.h"
#include "lipv.h"
/*
......MAXT define in xfsys0.h
.....don't define here because it will get different value
......the MAXT use here is for define UX_TABLEREC tab, so use other value
*/
/*#define MAXT 1200*/			/* limit on table length */
#define MAXST 1200			/* limit on symbol/env table length */
#define TRACE UU_FALSE
static UX_TABLEREC tab[MAXST];		/* data structure to hold symbol table */
static int tab_len;			/* current length of symbol table */
static UX_pathname temp_getenv;
extern int NAUTIGES, NAUTCAM, NAUTSTEP;
/*
.....see if it called from NCLPLOT routine
*/
int NCLPLOT = 0, MSLite = 0;
char NCL_init_fstr[20];

/**************************************************************************
**  E_FUNCTION:  ux_getenv(variable, options)
**		   Attempts to get the value of the environmental variable,
**		   "variable", and return it as the value of the function.
**		   Looks first in Unicad tables and then to system.
**	PARAMETERS:
**		"variable": The environmental variable to have its value
**			    retrieved.
**		"options": One of the following values:
**			UX_PRTERRS: print any errors as specified by
**				   uu_uerror functions.
**			UX_NPRTERRS: don't print any errors as 
**				     specified by uu_uerror functions.
**			UX_NQUOTES: don't try to quote an apparent pathname 
**				    found by getenv (not by the table search).
**			UX_QUOTES: return a pathname found by getenv and not
**				   in the table with quotes if it seems like
**				   a system dependent pathname.
**		  NOTE: This is an option so that one doesn't need to remember 
**			to type "setenv fred \"example\" " in a runscript and
**			"setenv fred example" will be okay, where example is a path.
**			If incorrectly specified we will print errors and add
**			quotes to apparent pathnames found by gtenv (not table 
**			search). 
**	RETURNS: note: not an int function
**		 Returns with the value of "variable" if a value can be 
**		 found. Note, if there is more than one value associated 
**		 with "variable" as can be the case with path names, then
**		 all path names are returned. If no value is found for
**		 "variable", then UU_NULL is returned.
**			
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
/*
.....WinNT
*/
void uxf_getenv(variable,cont)
UM_f77_str_ptr variable,cont;
{
	char *cstr,*p,*pstr,var[UX_MAX_PATH_LEN];
	char *ux_getenv();
	int i,n;
	cstr = UM_cstr_of_f77_str(variable);
	strncpy(var,cstr,UX_MAX_PATH_LEN);
	n = UX_MAX_PATH_LEN;
	ul_strip_blanks(var,&n);
	p = ux_getenv(var,UX_NPRTERRS);
	pstr = UM_cstr_of_f77_str(cont);
	if (p != NULL)
	{
		strcpy(pstr,p);
		for (i=strlen(p);i<UX_MAX_PATH_LEN;i++) pstr[i] = ' ';
	}
	else
	{
		for (i=0;i<UX_MAX_PATH_LEN;i++) pstr[i] = ' ';
	}
}

char *ux_getenv(variable,options)
	char *variable;			/* name of variable to be checked	*/
	int options;
{
	UU_LOGICAL printit;
	char *index();
	char *fs;
	char *chr;
	int i;
#if UU_COMP != UU_VAXVMS		/* VAXVMS declares things its own way ... */
	char *getenv();
#endif

	uu_denter(UU_XTRC,(us,"ux_getenv(variable:%s,option:%d)",variable,options));
	printit = (options != (options | UX_NPRTERRS));  /* default is print */
	strcpy(temp_getenv,"");

/*
.....Search Internal table first
.....Bobby  -  1/10/94
*/
/*	uu_dprint(UU_XTRC,(us,"Symbol not in system, search Unicad table:"));*/
	chr = UU_NULL;
	for (i=0;i<=tab_len;i++)
	{
		if (strcmp(variable,tab[i].sname)==0)
		{
			chr = (tab[i].symstr);
			break;
		}
	}
/*
.....Search OS Tables
*/
	if (chr == UU_NULL)
	{
#if UU_COMP == UU_WINNT
		if (strlen(variable) != 2 || variable[1] != ':')
#endif
		chr = getenv(variable);
		if (chr !=UU_NULL)
		{
			if (options == (options|UX_QUOTES))
			{
			/* if variable value is found from getenv and not from table, we
			will return it as quoted pathname value if it seems to be a pathname */
#if UU_COMP == UU_VAXVMS
				fs = index(chr,'[');
#else
#if (UU_COMP != UU_WIN2K)
				fs = index(chr,'/');
#else
				fs = strchr(chr,'/');
#endif
#endif
				if ( fs != UU_NULL) 
				{
					strcpy(temp_getenv,UX_QUOTE_STR);
					strcat(temp_getenv,chr);
					strcat(temp_getenv,UX_QUOTE_STR);
					uu_dexit;
					return(temp_getenv);
				}
			}
		}
	}
	uu_dexit;
	return(chr);
}

/**************************************************************************
**  E_FUNCTION:  ux_getenv_list(variable,varlist,envlist)
**			Returns all environmental variables stored in the Unicad tables
**			whose first characters match 'variable'.  Both the actual
**			environmental variables and their values are returned in a simple
**			list.
**
**		For example:  "FONT_" will return "FONT_ansi", "FONT_duplex", etc.
**
**	PARAMETERS:
**		variable    The environmental variable string to match.
**		varlist     List of environmental variables that match 'variable'.
**		envlist     Values of 'varlist'.
**	RETURNS:
**		Returns number of variables that match.
**			
**  SIDE EFFECTS :
**		The 'varlist' and 'envlist' lists will be initialized in this
**		routine, but must be freed by the calling routine.
**  WARNINGS :  none
**************************************************************************/
int ux_getenv_list(variable,varlist,envlist)
char *variable;
UU_LIST *varlist,*envlist;
{
	int i,nc,nvar;
	char *p;
/*
.....Initialize the lists
*/
	uu_list_init(varlist,sizeof(char *),20,10);
	uu_list_init(envlist,sizeof(char *),20,10);
	nvar = 0;
	nc = strlen(variable);
/*
.....Search Internal table
*/
	for (i=0;i<=tab_len;i++)
	{
		if (strncmp(variable,tab[i].sname,nc)==0)
		{
			p = (tab[i].sname);
			uu_list_push(varlist,&p);
			p = (tab[i].symstr);
			uu_list_push(envlist,&p);
			nvar++;
		}
	}
	return(nvar);
}

/*********************************************************************
**    I_FUNCTION : ux_init_table(options)
**		Routine that reads the Unicad init file and the user's init file
**		and builds the symbol tables from them.
**		Also used to init the open file table index number server.
**			
**    PARAMETERS   
**       INPUT  : 
**		options as above
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_init_table(options)
	int options;
{
	int c, i, index, inifiles,ver_int;
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int fstat, mode;
	UU_LOGICAL printit;
	UU_LOGICAL white;
	int next = 0;
	char cc;
	UX_pathname s,sn,tmpstr,file_path;
	char *t;
	char *fpath;
#if UU_COMP != UU_VAXVMS		/* VAXVMS declares things its own way ... */
	char *getenv();
#endif
	UX_pathname filename,fullname;
	FILE *fp;

	uu_denter(UU_XTRC,(us,"ux_init_table(options:%d)",options));
	status = UU_SUCCESS;
	printit = (options != (options | UX_NPRTERRS));  /* default is print */

	/* First init the number server for the open file table record*/
	if ( uu_nserv_init(0,MAXT-1,UU_OT_NM) == -1 )
	{
		ud_printmsg(" Error init'ing number server for file table index numbers.\n");
		goto failed;
	}
/*
.....first read in initialize file for all program
.....nccs.init
*/
	strcpy(filename, "nccs.init");
/*
	ver_int = NCL_version*10 + 0.5;
        Updated to use year as version number instead of XX.XXX, i.e. YYYY.xxx (KC)
*/
    ver_int = NCL_version + 0.5;
	sprintf(NCL_init_fstr,"NCL%d_INIT_FILES",ver_int);
	fpath = getenv(NCL_init_fstr);
	if (fpath == UU_NULL)
	{
		sprintf(tmpstr, " Error -- a mandatory environment variable \"%s\" cannot be found.\n", NCL_init_fstr);
		ud_printmsg(tmpstr);
		goto failed;
	}
	strcpy(file_path,fpath);
	if (filename[0]!='\0')
	{
		ul_remove_quotes(file_path);
		if (ux_search_for_path(file_path, file_path,
			UX_PRTERRS|UX_NCHK|UX_NQUOTES)!=UU_SUCCESS)
		{
			sprintf(tmpstr, " Error -- the mandatory %s file cannot be found.\n",
						filename);
			ud_printmsg(tmpstr);
			goto failed;
		}
		ul_remove_quotes(file_path);
		mode = UX_EXISTS | UX_READ;
		status = ux_mk_chk_syspath(UU_NULL,file_path,filename,UU_NULL,
					UU_NULL, &mode, filename,UX_NPRTERRS|UX_NQUOTES);
		if (status != UU_SUCCESS || mode&UX_NEXISTS)
		{
			sprintf(tmpstr, " Error -- the mandatory %s file cannot be found.\n",
						filename);
			ud_printmsg(tmpstr);
			goto failed;
		}
	}

	for (inifiles=0; inifiles<4; inifiles++)	/* for each of the two types */
	{
		if (ux_search_for_path(filename, fullname,
			UX_PRTERRS|UX_NCHK|UX_NQUOTES)!=UU_SUCCESS)
			strcpy(fullname, filename);

		if (ux_access0(fullname,4) == UX_FAILURE)	/* check for read access */
		{
			if ((inifiles == 0)||(inifiles == 1))
			{
				uu_dprint(UU_XTRC,(us,
				"Mandatory file %s doesn't have read access.", fullname));
				goto failed;
			}
			else /* second pass is optional user's file */
			{
/*
.....or user initialize file, we search the local directory first, then
.....search the UU_USER_SETTINGS/init directory
*/
				if (filename[0]!='\0')
				{
					status = ul_open_mod_file("UU_USER_SETTINGS", "init", UU_NULL, UU_NULL,
						filename, 0, UU_NULL);
					strcpy(fullname, filename);
				}
				if (status != UU_SUCCESS || (filename[0]=='\0'))
				{
					tab_len = next-1;
					uu_dprint(UU_XTRC,(us,
					"Optional file %s doesn't exist or doesn't have read access. ",
					fullname));
					uu_dprint(UU_XTRC,(us,
					"Not a problem if no user init file is wanted."));
					goto done; 
				}
			}
		}
		if (ux_fopen0(fullname,"r",&fp) == UX_FAILURE)
		{
			uu_dprint(UU_XTRC,(us,"File %s wasn't successfully opend. ",fullname));
			/* no error system yet since table isn't built to give error file */
			goto failed;
		}
	
		while (next<MAXST)
		{
			for (i=0; i<UX_MAX_PATH_LEN; i++) 
			{
				if ((fstat = ux_fgetc0(fp,&cc))==UX_FAILURE)
					goto failed;
				c = cc;
				if (fstat == UX_EOF)
					goto endof;
				else if ((c == ' ') || (c == '\t') || (c == '\n') || (c == '\r'))	/* strip white */
					i--;
				else if (c == '=')
				{
					sn[i] = '\0';
					break;
				}
				else
					sn[i] = c;
			}
			/* if sname is already there, prepare to overwrite its string */
			for (i=0; (i<next)&&(strcmp(tab[i].sname,sn)!=0); i++);
			if (strcmp(tab[i].sname,sn)==0)
			{
				next--;
				index = i;
			}
			else	/* no match was found */
			{
				strcpy(tab[next].sname,sn);
				index = next;
			}
				
			/* read from the = sign to the end of the line */
			white = UU_TRUE;
			for (i=0; i<UX_MAX_PATH_LEN; i++)
			{
				if ((fstat=ux_fgetc0(fp,&cc))==UX_FAILURE)
					goto failed;
				c = cc;
				s[i] = c;
				/*  search for a continuation backslash followed by white space and
				a newline mark, otherwise the newline mark is also end of line  */
				if (c == UX_SEP_CHAR)
					white = UU_TRUE;	/* init whitespace search */
				else if (fstat == UX_EOF)
				{
					s[i] = '\0';			/* EOL mark */
					t = s;
					while ((*t == ' ') || (*t == '\t'))
					/* first character of string is only whitespace, remove it */
						t = t+1;
					strcpy(tab[index].symstr, t);
					next++;
					goto endof;
				}
				else if ((c == '\n' || c == '\r') && white == UU_TRUE)
					i--;		/* keep reading, omit the in-line \n character */
				else if ((c == '\n' || c == '\r') && white == UU_FALSE) 
				{
					s[i] = '\0';			/* EOL mark */
					t = s;
					while ((*t == ' ') || (*t == '\t'))
					/* first character of string is only whitespace, remove it */
						t = t+1;
					strcpy(tab[index].symstr, t);
					break;
				}
				else if ((c != ' ') && (c != '\t')) 
					white = UU_FALSE;
			}
			next++;
		}	/* end of the while loop */
		if (next == MAXST)
			/* then we stopped reading because the table isn't long enough */
			ud_printmsg("Quit reading init file because symbol table is full.\n");

endof:;
		ux_fclose0(fp);
		tab_len = next;
get_fname:;
		if (inifiles == 0)
		{
			if (NAUTCAM==1)
			{
				if ((fpath = ux_getenv("UU_UNIVARS")) == UU_NULL)
				{
					ud_printmsg(" Error -- a mandatory file \"UU_UNIVARS\" cannot be found.\n");
					goto failed;
				}
				else
					strcpy(filename, fpath);
			}
			else if (NAUTIGES==1)
			{
				if ( (fpath = ux_getenv("UU_IGESVARS")) == UU_NULL)
				{
					ud_printmsg(" Error -- a mandatory file \"UU_IGESVARS\" cannot be found.\n");
					goto failed;
				}
				else
					strcpy(filename, fpath);
			}
			else if (NAUTSTEP==1)
			{
				if ( (fpath = ux_getenv("UU_STEPVARS")) == UU_NULL)
				{
					ud_printmsg(" Error -- a mandatory file \"UU_STEPVARS\" cannot be found.\n");
					goto failed;
				}
				else
					strcpy(filename, fpath);
			}
			else if (NCLPLOT==1)
			{
				if ( (fpath = ux_getenv("UU_PLOTVARS")) == UU_NULL)
				{
					ud_printmsg(" Error -- a mandatory file \"UU_PLOTVARS\" cannot be found.\n");
					goto failed;
				}
				else
					strcpy(filename, fpath);
			}
			else if (MSLite==1)
			{
				if ( (fpath = ux_getenv("UU_MSLVARS")) == UU_NULL)
				{
					ud_printmsg(" Error -- a mandatory file \"UU_MSLVARS\" cannot be found.\n");
					goto failed;
				}
				else
					strcpy(filename, fpath);
			}
			else if (LW_nclipv==LW_STANDALONE)
			{
				if ( (fpath = ux_getenv("UU_IPVVARS")) == UU_NULL)
				{
					ud_printmsg(" Error -- a mandatory file \"UU_IPVVARS\" cannot be found.\n");
					goto failed;
				}
				else
					strcpy(filename, fpath);
			}
			else
			{
				inifiles++;
				goto get_fname;
			}
		}
		else 
		{
			if (NAUTCAM==1)
			{
				if ((fpath = ux_getenv("UU_USERVARS")) == UU_NULL)
				{
					break;				
				}
				else
					strcpy(filename, fpath);
			}
			else if (NAUTIGES==1)
			{
				if ( (fpath = ux_getenv("UU_IGESUSERVARS")) == UU_NULL)
				{
					break;				
				}
				else
					strcpy(filename, fpath);
			}
			else if (NAUTSTEP==1)
			{
				if ( (fpath = ux_getenv("UU_STEPUSERVARS")) == UU_NULL)
				{
					break;				
				}
				else
					strcpy(filename, fpath);
			}
		}
	}
	tab_len = next;
	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
/* print out message to screen and quit program */
	ud_printmsg("Init file was not read successfully. Exit. \n");
	uu_dexit;
	exit(1);
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION : ux_modenv(op,variable,value,options) 
**		Routine to handle operations on one symbol of the Unicad symbol table.
**			If "op" == "delete" then "variable" is deleted from the
**			UNICAD environmental table.
**			If "op" == "add" then "value" is added to the end of the
**			current value of "variable".
**			If "op" == "replace" then "value" replaces the current value
**			of "variable".
**			Note, if "variable" is a "shell" variable then any modification
**			of its value in the UNICAD environmental variable table
**			does not affect the shell variable.
**	PARAMETERS:
**		"variable": Environmental variable to its value changed.
**		"value": Value to assign to "variable". 
**		"options": One of the following values:
**			UX_PRTERRS: print any errors as specified by
**				    uu_uerror functions.
**			UX_NPRTERRS: don't print any errors as 
**				     specified by uu_uerror functions.
**			If incorrectly specified we will print errors.
**	RETURNS:
**		UU_SUCCESS is returned if the requested operation is done.
**		UX_FAILURE is returned in all other cases.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_modenv(op,variable,value,options)
	char *op;
	char *variable;
	char *value;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	int i,j,next;

	uu_denter(UU_XTRC,(us,
	"ux_modenv(op:%s,variable:%s,value:%s,option:%d",op,variable,value,options));
	printit = (options != (options | UX_NPRTERRS));  /* default is print */
	status = UU_SUCCESS; /* assume success */

	if (strcmp(op,"add")==0)
	{
		for (i=0; ((i<=tab_len)&&(strcmp(tab[i].sname,variable)!=0)); i++);
		if (strcmp(tab[i].sname,variable)==0)		/* a match was found in table */
		{
			strcat(tab[i].symstr,"\\");
			strcat(tab[i].symstr,value);		/* append new to old */
		}
		else						/* at end of table, no match */
		{
			next = tab_len+1;			/* new value appended to table */
			strcpy(tab[next].sname,variable);
			strcpy(tab[next].symstr,value);
			tab_len++;				/* table length increase by one */
		}
	}
	else if (strcmp(op,"replace")==0)
	{
		for (i=0; ((i<=tab_len)&&(strcmp(tab[i].sname,variable)!=0)); i++);
		if (strcmp(tab[i].sname,variable)==0)		/* a match was found in table */
			strcpy(tab[i].symstr,value);		/* replace old value with new */
		else						/* at end of table, no match */
		{
			UX_ERROR1(5,variable,printit);
			/* Can't find symbol name %s in table to replace it */
			goto failed;
		}
	}
	else if (strcmp(op,"delete")==0)
	{
		for (i=0; ((i<=tab_len)&&(strcmp(tab[i].sname,variable)!=0)); i++);
		if (strcmp(tab[i].sname,variable)==0)		/* a match was found in table */
		{
			for (j=i; j<tab_len; j++)
			{				
				strcpy(tab[j].sname,tab[j+1].sname);
				strcpy(tab[j].symstr,tab[j+1].symstr);
			}
			tab_len--;
		}
	}
	else
	{
		UX_ERROR1(3,op,printit);
		/* Operation %s isn't implemented for modenv operations */
		goto failed;
	}

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    I_FUNCTION : uxi_print_table() 
**		Prints out the current Unicad symbol table.
**			
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int uxi_print_table()
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int i;
	char msg[UX_MAX_PATH_LEN+80];

	uu_denter(UU_XTRC,(us,"uxi_print_table()"));
	status = UU_SUCCESS; /* assume success */

	/* prints out to the screen and to the trace file */
	for ( i=0; i<=tab_len; i++)
	{
		sprintf(msg, "%s = %s\n",tab[i].sname,tab[i].symstr);
		ud_printmsg(msg);
		uu_dprint(UU_XTRC,(us,"%s=%s",(tab[i].sname),(tab[i].symstr)));
	}

	goto done;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}	

