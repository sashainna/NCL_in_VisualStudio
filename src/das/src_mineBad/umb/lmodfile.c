/*********************************************************************
**	FILENAME: lmodfile.c
**	CONTAINS:		
**             ul_open_mod_file
**				ul_open_mod_file2
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lmodfile.c , 25.5
**    DATE AND TIME OF LAST  MODIFICATION
**       10/12/15 , 17:21:04
*********************************************************************/

#include "lcom.h"
#include "lumb.h"
#include "usysdef.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "ustdio.h"
#include "nccs.h"
#include "nclfc.h"
#include "mfort.h"

typedef enum
{
	UL_UNKNOWN_DIR,
	UL_SYSTEM_DIR,
	UL_LOCAL_DIR
} UL_modal_area;
 
int ul_is_mult_dir_support(dir, ext_dir)
char *dir, *ext_dir;
{
	int i, ret;
	char *svalue;
	static char env_str[12][40] = {"NCL_INCDIR", "M_UD_LAYOUT_DIR", "NCL_MENU", 
		"NCL_BITMAP", "NCL_USER_MENU", "UD_FORMDIR", "UB_SYS_M_SYMDIR", "NCL_POST",
		"UL_NCLIPV_MACHINES", "UU_USER_SETTINGS", "NCL_TOOL", "NCL_TOOL_DRAWING"};
	ret = 0;
	strcpy(ext_dir, dir);
	for (i=0; i<12;i++)
	{
		if (stricmp(dir, env_str[i])==0)
		{
			ret = 1;
			break;
		}
	}
	if (ret==1)
	{
		svalue = ux_getenv(dir, UX_NPRTERRS|UX_NQUOTES);
		if (svalue != UU_NULL)
		{
			strcpy(ext_dir, svalue);
		}
	}	
	return ret;
}

extern char NCL_init_fstr[20];

/*********************************************************************
**	 E_FUNCTION : ul_open_mod_file2(in_dir, in_subdir, infname, wr_flag, fptr)
**					
**		open initialization/mod file name from in_dir/in_subdir 
**		(it could be the environmental variable env_str.) and fname.
**		
**	 PARAMETERS	
**		 INPUT  :
**        in_dir    = string containing the input path to open
**                      
**        in_subdir    = string containing the user input path to open
**                      the file.
**        infname     = filename to be opened. (if the filename already include
**                      the path, ignore the input path)
**        wr_flag     = read/write flag: wr_flag=0, Access for reading, but do not open
**                      wr_flag=1, Access for writing, but do not open,
**                      wr_flag=2, Open for reading,
**                      wr_flag=3, Open for writing,
**		 OUTPUT : 
**        infname     = filename be acessed/opened. (with fullpath)
**		  fptr:			file stream opened (UU_NULL if the file could not be opened)
**	 RETURNS:	UU_SUCCESS or UU_FAILURE 
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
int ul_open_mod_file2(in_dir, in_subdir, infname, wr_flag, fptr,  fenv,  ftype)
char *in_dir, *in_subdir, *infname;
int wr_flag;
FILE **fptr;
char *fenv;
char *ftype;
{
	int k, status,mode, hdrfound;
	char *str, *strtok();
	UX_pathname dir, act_dir, ext_dir, userdir, fname;
	UX_pathname fullname;
	UX_pathname localdir;
	UU_LOGICAL found;
	status = UU_SUCCESS;
	userdir[0] = '\0';
	fname[0] = '\0';
	if (fptr!=UU_NULL)
		*fptr = UU_NULL;
/*
......see if the infname include path
.....if the filename have the path, use it
*/
	localdir[0] = '\0';
	if ((infname!=NULL) || (infname[0]!='\0')) 
	{
		strcpy(fullname, infname);
		ul_break_fname(fullname, dir, fname);
		if (dir[0]!='\0')
		{
/*
.....if the file already have the path, use it.
*/
			strcpy(localdir, dir);
		}
	}
/*
.....if input is a blank dirctory
*/
	if ((((in_dir==NULL) || ((in_dir!=NULL)&&(in_dir[0]=='\0')))
		&& (localdir[0]=='\0'))||(localdir[0]!='\0'))
	{
/*
.....empty path and file, just return;
*/
		if ((infname==NULL) || (infname[0]=='\0')) 
			return -1;
		mode = UX_EXISTS | UX_READ;
		status = ux_mk_chk_syspath(UU_NULL, localdir, fname, fenv, ftype,
				&mode, fullname,UX_NPRTERRS|UX_NQUOTES);
/*
.....File exist
*/
		if (!(status != UU_SUCCESS || mode == (mode|UX_NEXISTS)))
		{
			if (wr_flag==2)
				status = ux_fopen0(fullname,"r", fptr);
			else
				return UU_SUCCESS;
			if (status==0)
				return UU_SUCCESS;
		}
	}
	if (((in_dir==NULL) || ((in_dir!=NULL)&&(in_dir[0]=='\0'))))
		return (-1);
		
	mode = UX_EXISTS | UX_READ;
	strcpy(dir, in_dir);
	status = ux_search_for_path(dir, ext_dir, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
	if (status==UU_SUCCESS)
	{
		if ((in_subdir!=NULL)&&(in_subdir[0]!='\0'))
		{
			strcat(dir, UL_DIR_SEP);
			strcat(dir, in_subdir);
		}
		status = ux_search_for_path(dir, ext_dir, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
		mode = UX_EXISTS | UX_READ;
/*
......remove quote from fname
*/
		ul_remove_quotes(fname);
		status = ux_mk_chk_syspath(UU_NULL, ext_dir, fname, fenv, ftype,
				&mode, fullname,UX_NPRTERRS|UX_NQUOTES);
		if (!(status != UU_SUCCESS || mode == (mode|UX_NEXISTS)))
		{
			ul_remove_quotes(fullname);
			strcpy(infname, fullname);
			if (wr_flag==2)
				status = ux_fopen0(infname,"r", fptr);
			else
				return UU_SUCCESS;
			if (status==0)
				return UU_SUCCESS;
		}
	}
	else
	{
/*
.....the dir can be a "format of dir1;dir2;dir3;..."
.....so we get directory one by one
*/
		if (ul_is_mult_dir_support(dir, ext_dir)==0)
			goto failed;
		strcpy(userdir, ext_dir);
/*
.....the ext_dir can be a "format of dir1;dir2;dir3;..."
.....so we get directory one by one
*/
		str = strtok (userdir, ";");
		while (str!=NULL)
		{
			strcpy(dir, str);
/*
.....remove pre or trailing spaces
*/
			k=0;
			while (dir[k]==' ') k++;
			strcpy(dir, &(dir[k]));
/*
......Remove trailing spaces
*/
			for (k=strlen(dir); k>0; k--)
			{
				if (dir[k-1]==' ')
					dir[k-1] = '\0';
				else
					break;
			}
			if ((in_subdir!=NULL)&&(in_subdir[0]!='\0'))
			{
				strcat(dir, UL_DIR_SEP);
				strcat(dir, in_subdir);
			}
			mode = UX_EXISTS | UX_READ;
			status = ux_search_for_path(dir, act_dir, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
			if (status==UU_SUCCESS)
			{
				status = ux_mk_chk_syspath(UU_NULL, act_dir, fname, fenv, ftype,
						&mode, fullname,UX_NPRTERRS|UX_NQUOTES);
				if (!(status != UU_SUCCESS || mode == (mode|UX_NEXISTS)))
				{
					ul_remove_quotes(fullname);
					strcpy(infname, fullname);
					if (wr_flag==2)
						status = ux_fopen0(infname,"r", fptr);
					else
						return UU_SUCCESS;
					if (status==0)
						return UU_SUCCESS;
				}
			}
			str = strtok(NULL, ";");
		}
	}
/*
.....End of routine
*/
failed:;
	fptr = NULL;
	return(-1);
}
/*********************************************************************
**	 E_FUNCTION : int ul_open_mod_file(user_dir,user_subdir, sys_dir,sys_subdir,
**					infname, wr_flag, fptr)
**		open initialization/mod file name from user_dir/user_subdir or 
**		sys_dir/sys_dir (it could be the environmental variable env_str.) and fname.
**		
**	 PARAMETERS	
**		 INPUT  :
**        user_dir    = string containing the user defined path to open
**                      the file.
**        user_subdir    = string containing the user sub path to open
**                      the file.
**        sys_dir     = string containing the system path to open
**                      the file.
**        sys_subdir     = string containing the system sub path to open
**                      the file.
**        infname     = filename to be opened. (if the filename already include
**                      the path, ignore the user_dir and sys_dir)
**        wr_flag     = read/write flag: wr_flag=0, Access for reading, but do not open
**                      wr_flag=1, Access for writing, but do not open,
**                      wr_flag=2, Open for reading,
**                      wr_flag=3, Open for writing,
**                      when it is writing flag, we will create the path if not exist
**		 OUTPUT : 
**        infname     = filename be acessed/opened. (with fullpath)
**		  fptr:			file stream opened (UU_NULL if the file could not be opened)
**	 RETURNS:	UU_SUCCESS or UU_FAILURE 
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
int ul_open_mod_file(user_dir,user_subdir, sys_dir, sys_subdir,
					infname, wr_flag, fptr)
char *user_dir, *user_subdir, *sys_dir, *sys_subdir, *infname;
int wr_flag;
FILE **fptr;
{
	int status,mode;
	char *indx, *strrchr(), *strchr();
	UX_pathname userdir, sysdir,fname, ext_dir;
	UX_pathname fullname, dir, path;
	UU_LOGICAL found;
	char *pathlistptr = UU_NULL;
	status = UU_SUCCESS;
	userdir[0] = '\0';
	sysdir[0] = '\0';
	fname[0] = '\0';
	if (fptr!=UU_NULL)
		*fptr = UU_NULL;
/*
......see if the infname include path
.....if the filename have the path, use it
*/
	if ((infname!=NULL) || (infname[0]!='\0')) 
	{
		strcpy(fullname, infname);
		ul_break_fname(fullname, dir, fname);
		if (dir[0]!='\0')
		{
/*
.....if the file already have the path, use it instead of use sys_dir and user_dir
*/
			strcpy(userdir, dir);
		}
	}
	if ((wr_flag==0)||(wr_flag==2))
	{
		status = ul_open_mod_file2(user_dir, user_subdir, infname, wr_flag, fptr, UU_NULL, UU_NULL);
		if (status!=UU_SUCCESS)
		{
			status = ul_open_mod_file2(sys_dir, sys_subdir, infname, wr_flag, fptr, UU_NULL, UU_NULL);
		}
		return status;
	}
/*
.....if input is a blank dirctory
*/
	if (((user_dir==NULL) || ((user_dir!=NULL)&&(user_dir[0]=='\0')))
		&& ((sys_dir==NULL) || ((sys_dir!=NULL)&&(sys_dir[0]=='\0'))) 
		&& (userdir[0]=='\0'))
	{
/*
.....empty path and file, just return;
*/
		if ((infname==NULL) || (infname[0]=='\0')) 
			return -1;
		userdir[0] = '\0';
		sysdir[0] = '\0';
	}
	else if (userdir[0]=='\0')
	{
/*
......made user directory by using input user_dir & sub directory
*/
		if (user_dir!=NULL)
		{
			strcpy(userdir, user_dir);
/*
......mutil-path first, only get the first path for writing
*/
			if (ul_is_mult_dir_support(userdir, ext_dir)!=0)
			{
				strcpy(userdir, ext_dir);
				indx = strchr(userdir, ';');
				if (indx!=NULL)
					*indx = '\0';
			}
			if ((user_subdir!=NULL)&&(userdir[0]!='\0'))
			{
				strcat(userdir, UL_DIR_SEP);
				strcat(userdir, user_subdir);
			}
		}
		else
			userdir[0] = '\0';
/*
......made system directory by using input sys_dir & sub directory
*/
		if (sys_dir!=NULL)
		{
			strcpy(sysdir, sys_dir);
/*
......mutil-path first, only get the first path for writing
*/
			if (ul_is_mult_dir_support(sysdir, ext_dir)!=0)
			{
				strcpy(sysdir, ext_dir);
				indx = strchr(sysdir, ';');
				if (indx!=NULL)
					*indx = '\0';
			}
			if ((sys_subdir!=NULL)&&(sysdir[0]!='\0'))
			{
				strcat(sysdir, UL_DIR_SEP);
				strcat(sysdir, sys_subdir);
			}
		}
		else
			sysdir[0] = '\0';
	}
/*
......if there are user path, use it first
*/
	if (userdir[0] != '\0')
	{
/*
.....Check for file existance
*/
		if ((wr_flag==0)||(wr_flag==2))
			mode = UX_EXISTS | UX_READ;
		else
			mode = UX_EXISTS | UX_WRITE;
/*
......ux_mk_chk_syspath can't pass in env value, so get the path first using
......ux_search_for_path
*/
		status = ux_search_for_path(userdir, dir, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
		if (status!=UU_SUCCESS)
		{
/*
......if the path not exist and it is read mode
......go check the sysdir now
*/
			if ((wr_flag==0)||(wr_flag==2)) goto system;
/*
......if it is writing mode, create the path
*/
			dir[0] = '\0';
			ux_get_syspath(userdir, &pathlistptr, dir, 
							&found, UX_NPRTERRS|UX_NCHK|UX_NQUOTES);
			uu_lsdel(pathlistptr);
			getcwd(path,UX_MAX_PATH_LEN);
			status = ux_mk_pthdir(dir);
			chdir(path);
			if (status != UU_SUCCESS)
			{
				return UU_FAILURE;
			}
		}
		strcpy(userdir, dir);
		status = ux_mk_chk_syspath(UU_NULL, userdir, fname, UU_NULL,
				UU_NULL, &mode, fullname,UX_NPRTERRS|UX_NQUOTES);
/*
.....File does not exist in user directory
.....try system directory
*/
system:;
		if (status != UU_SUCCESS || mode == (mode|UX_NEXISTS))
		{
/*
.....if the file is supposed opened for writing, it can't used system directory
.....just return
*/
			if ((wr_flag==1) ||(wr_flag==3))
				goto done;
/*
......ux_mk_chk_syspath can't pass in env value, so get the path first using
......ux_search_for_path
*/
			if (sysdir[0]!='\0')
			{
				status = ux_search_for_path(sysdir, dir,
					UX_PRTERRS|UX_NCHK|UX_NQUOTES);
				if (status != UU_SUCCESS)
				{
					return UU_FAILURE;
				}
				strcpy(sysdir, dir);
			}
			mode = UX_EXISTS | UX_READ;
			status = ux_mk_chk_syspath(UU_NULL, sysdir, fname, UU_NULL,
					UU_NULL, &mode, fullname,UX_NPRTERRS|UX_NQUOTES);
		}
	}
	else
/*
.....no user directory, using system directory
*/
	{
		if (sysdir[0] != '\0')
		{
/*
.....if the file is supposed opened for writing, it can't used system directory
.....just return
*/
			if ((wr_flag==1) ||(wr_flag==3))
				goto done;
/*
......ux_mk_chk_syspath can't pass in env value, so get the path first using
......ux_search_for_path
*/
			status = ux_search_for_path(sysdir, dir, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
			if (status != UU_SUCCESS)
			{
				return UU_FAILURE;
			}
			strcpy(sysdir, dir);
			mode = UX_EXISTS | UX_READ;
			status = ux_mk_chk_syspath(UU_NULL, sysdir, fname, UU_NULL,
					UU_NULL, &mode, fullname,UX_NPRTERRS|UX_NQUOTES);
		}
		else
/*
.....no system and user directory, so just check current directory
*/
		{
			if ((wr_flag==0)||(wr_flag==2))
				mode = UX_EXISTS | UX_READ;
			else
				mode = UX_EXISTS | UX_WRITE;
			status = ux_mk_chk_syspath(UU_NULL, NULL, fname, UU_NULL,
					UU_NULL, &mode, fullname,UX_NPRTERRS|UX_NQUOTES);
		}
	}
done:;
	if (((wr_flag==0)||(wr_flag==2)) && (status != UU_SUCCESS || mode == (mode|UX_NEXISTS)))
	{
		return UU_FAILURE;
	}
	else if (((wr_flag==1)||(wr_flag==3)) && (status != UU_SUCCESS))
	{
		return UU_FAILURE;
	}
	ul_remove_quotes(fullname);
	strcpy(infname, fullname);
/*
.....Open file
*/
	if (wr_flag==2)
		status = ux_fopen0(fullname,"r", fptr);
	else if (wr_flag==3)
		status = ux_fopen0(fullname,"w", fptr);
	else
		return UU_SUCCESS;
/*
.....End of routine
*/
	return(status);
}
/*
.....the directory could be env value, if the file can't open in dir
.....will look at the local directory
*/
void ulf_get_mod_file(dir, len1, fname, len2, fullname, len)
UM_f77_str_ptr dir, fname, fullname;
UM_int2 *len, *len1, *len2;
{
	FILE *fptr = NULL;

	char *dirstr, *fullstr, *filestr;
	int len0, status;
	len0 = *len1;
	dirstr = UM_cstr_of_f77_str(dir);
	dirstr[len0] = '\0';
	filestr = UM_cstr_of_f77_str(fname);
	len0 = *len2;
	filestr[len0] = '\0';
	fullstr = UM_cstr_of_f77_str(fullname);
	strcpy(fullstr, filestr);
	status = ul_open_mod_file2(dirstr, NULL, fullstr, 0, &fptr, UU_NULL, UU_NULL);
	if (status==UU_SUCCESS)
	{
		*len = strlen(fullstr);
		return;
	}
	else
	{
/*
......will look at the local directory
*/
		strcpy(fullstr, filestr);
		status = ul_open_mod_file2(".", NULL, fullstr, 0, &fptr, UU_NULL, UU_NULL);
		if (status==UU_SUCCESS)
		{
			*len = strlen(fullstr);
		}
		else
			*len = 0;
	}
}

ul_fparse(cfnami, len1, cfnamo, len, cdev, len2, cext, len3, err)
UM_f77_str_ptr cfnami, cfnamo, cdev, cext;
UM_int2 *len, *len1, *len2, *len3, *err;
{
	UX_pathname filename, dir, ext;
	FILE *fptr = NULL;

	char *dirstr, *fullstr, *filestr, *extstr, *pointer, *strrchr();

	int len0, status;
	len0 = *len2;
	dirstr = UM_cstr_of_f77_str(cdev);
	strncpy(dir, dirstr, len0);
	dir[len0] = '\0';
	
	len0 = *len1;
	filestr = UM_cstr_of_f77_str(cfnami);
	strncpy(filename, filestr, len0);	
	filename[len0] = '\0';

	len0 = *len3;
	extstr = UM_cstr_of_f77_str(cext);
	strncpy(ext, extstr, len0);	
	ext[len0] = '\0';

	fullstr = UM_cstr_of_f77_str(cfnamo);
	strcpy(fullstr, filename);
/*
......add ext to the file
*/
	pointer = strrchr(fullstr,'.');
	if (pointer == UU_NULL && strlen(ext) != 0)
	{
		strcat(fullstr,".");
		strcat(fullstr,ext);
	}
	status = ul_open_mod_file2(dir, NULL, fullstr, 0, &fptr, UU_NULL, UU_NULL);
	if (status!=UU_SUCCESS)
	{
		*err = 1;
		*len = 0;
	}
	else
	{
		*len = strlen(fullstr);
		*err = 0;
	}
}
