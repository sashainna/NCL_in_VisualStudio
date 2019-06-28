/*********************************************************************
**      FILENAME: lsetdir.c
**      CONTAINS:    ul_break_fname
**                   ul_get_fname
**                   getfnm
**                   ul_build_full_dir
**                   ul_build_full_fname
**                   ul_get_full_dir
**                   ul_make_dir
**                   ul_mod_dir
**                   ul_set_dir
**                   ul_getvalid_fulldir
**                   shfile
**                   ul_short_filename
**							ul_fshort_filename
**							ul_default_ftype
**     MODULE NAME AND RELEASE LEVEL 
**       lsetdir.c , 25.4
**    DATE AND TIME OF LAST MODIFICATION
**       12/01/15 , 08:21:49
*********************************************************************/

#include "usysdef.h"

#if (UU_COMP == UU_WIN2K)
#include <string.h>
#define index strchr
#define rindex strrchr
#endif

#include "lcom.h"
#include "lumb.h"
#include "udebug.h"
#include "xenv1.h"
#include "xfsys1.h"

#include "nclfc.h"
#include "mfort.h"

void getfnm();
void ul_getvalid_fulldir();
void ul_short_filename();

/*******************************************************************
**   E_FUNCTION : ul_break_fname(fullname,dir,fname)
**              This function breaks a filename into two parts.
**                 1. device & directory spec
**                 2. filename
**   PARAMETERS  
**       INPUT  :  fullname  = full filename specification.
**       OUTPUT :  dir = directory specification of 'fullname'.
**                   Blank if no directory is specified.
**                 fname = filename specification of 'fullname'.
**                   Blank if no filename is specified.
**   RETURNS:    none.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
void ul_break_fname(fullname,dir,fname)
char *fullname,*dir,*fname;
{
#if UU_COMP == UU_WIN2K
	char *pt1;
#endif
	char *pointer,*rindex(),*index();
	UX_pathname buf;
/*
.....Check for device or directory specification
*/
	*dir = '\0';
	strcpy (buf,fullname);
#if UU_COMP == UU_VAXVMS
	pointer = rindex (buf,']');
	if (pointer != UU_NULL)
	{
		pointer++;
		strcpy (fname,pointer);
		*pointer = '\0';
		ul_get_full_dir(buf,dir);
	}
	else
	{
		pointer = rindex (buf,':');
		if (pointer != UU_NULL)
		{
			pointer++;
			strcpy (fname,pointer);
			*pointer = '\0';
			ul_get_full_dir(buf,dir);
		}
		else
		{
			strcpy (fname,buf);
		}
	}
#else
#if (UU_COMP == UU_WIN2K)
	pointer = rindex (buf,'\\');
	pt1 = rindex(buf,'/');
	if (pt1 > pointer) pointer = pt1;
#else
	pointer = rindex (buf,'/');
#endif
	if (pointer != UU_NULL)
	{
		pointer++;
		strcpy (fname,pointer);
		pointer--;
		*pointer = '\0';
		ul_get_full_dir(buf,dir);
	}
	else
	{
/*
.....WinNT may user c:\..
.....Yurong 1/27/99
*/
#if UU_COMP == UU_WINNT
		pointer = rindex (buf,'\\');
		if (pointer == UU_NULL)
			strcpy (fname,buf);
		else
		{
			pointer++;
			strcpy (fname,pointer);
			pointer--;
			*pointer = '\0';
			ul_get_full_dir(buf,dir);
		}
#else
		strcpy (fname,buf);
#endif
	}
#endif
}

/******************************************************************* 
**   E_FUNCTION : ul_get_fname(str,fullname)
**          This function takes the symbol 'str' and returns the
**          full filename specification in 'fullname'.  'str' can
**          be a directory specification or environmental variable.
**   PARAMETERS
**       INPUT  :  str  = logical name
**       OUTPUT :  fullname = full filename specification of 'str'.
**   RETURNS:    none.
**                      otherwise.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void ul_get_fname(str,fullname)
char *str,*fullname;
{
	UM_f77_str fstr,fname;
	UM_int4 nci,nco;
	UM_init_f77_str(fstr,str,UX_MAX_PATH_LEN);
	UM_init_f77_str(fname,fullname,UX_MAX_PATH_LEN);
	nci = strlen(str);
	getfnm(UM_addr_of_f77_str(fstr),&nci,UM_addr_of_f77_str(fname),&nco);
	return;
}

/******************************************************************* 
**   E_FUNCTION : getfnm(str,fullname)
**          This function takes the symbol 'str' and returns the
**          full filename specification in 'fullname'.  'str' can
**          be a directory specification or environmental variable.
**   PARAMETERS
**       INPUT  :  str  = logical name
**                 nci  = Number of characters in 'str'.
**       OUTPUT :  fullname = full filename specification of 'str'.
**                 nco      = Number of characters in 'fullname'.
**   RETURNS:    UU_SUCCESS if 'fullname' is a directory or UU_FAILURE
**                      otherwise.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void getfnm(str,nci,fullname,nco)
UM_f77_str_ptr str;
UM_f77_str_ptr fullname;
UM_int4 *nci,*nco;
{
	char *rindex(),*index();
	char *cstr, *cname;
	UX_pathname dir,fname;
	int i;
#if UU_COMP == UU_VAXVMS
	char *pointer;
#endif
	cstr = UM_cstr_of_f77_str(str);
	cname = UM_cstr_of_f77_str(fullname);
	cstr[*nci] = '\0';
/*
.....Remove preceding spaces
*/
	for (i=0; i<*nci; i++)
	{
		if (cstr[i]!=' ') break;
	}
	if (i<*nci)
		strcpy(cstr, &(cstr[i]));
	else
		cstr[0] = '\0';
/*
.....Remove trailing spaces
*/
	for (i=*nci-1; i>=0 && cstr[i]==' '; i--)
		cstr[i]='\0';
	ul_remove_quotes (cstr);
/*
.....Resolve environmental variable
*/
	ul_break_fname(cstr,dir,fname);
	ul_get_full_dir(dir,cname);
	ul_remove_quotes (cname);


#if UU_COMP == UU_VAXVMS
	pointer = rindex (cname,']');
	if (pointer != UU_NULL && strlen(cname) != 0)
	{
		*pointer = '\0';
		strcat (cname,"]");
	}
#else
/*
.....Allow for NutCracker device names
*/
#if UU_COMP == UU_WINNT
	if (cname[0] == '/' && cname[2] == '=')
	{
		UX_pathname buf;
		strcpy(buf,cname);
		cname[0] = buf[1];
		cname[1] = ':';
		strcpy(&cname[2],&buf[3]);
	}
#endif
/*
.....Added for native WinNT
*/
#if (UU_COMP == UU_WIN2K)
	if (strlen (cname) != 0)
		strcat(cname,"\\");
#else
	if (strlen(cname) != 0)
	{
#if UU_COMP == UU_WINNT
		pointer = rindex (cname,'\\');
		if (pointer != UU_NULL)
			strcat (cname,"\\");
		else
			strcat (cname,"/");
#else
		strcat (cname,"/");
#endif
	}
#endif
#endif

/*
.....Remove trailing spaces from filname
*/
	for (i=strlen(fname);i>0;i--) if (fname[i-1] != ' ') break;
	if (i > 0)
	{
		if (strlen(cname) !=0)
			strncat (cname,fname,i);
		else
			strcpy (cname,fname);
	}
	*nco = strlen(cname);
	return;
}

/******************************************************************* 
**   E_FUNCTION : ul_get_full_dir(dir,fullname)
**          This function takes the symbol 'dir' and returns the
**          full directory specification in 'fullname'.  'dir' can
**          be a directory specification or environmental variable.
**   PARAMETERS
**       INPUT  :  dir  = directory symbol.
**       OUTPUT :  fullname = full directory specification of 'dir'.
**   RETURNS:    UU_SUCCESS if 'fullname' is a directory or UU_FAILURE
**                      otherwise.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
ul_get_full_dir_old(dir,fullname)
char *dir,*fullname;
{
	int stat,status,mode,file_status,nc,first_path;
	UU_LOGICAL found;
	UX_pathname tmpstr;
	char *q, *index();
	char *pathlistptr = UU_NULL;
/*
.....Debug Enter
*/
	status = UU_SUCCESS;
/*
.....Blank directory, so do nothing
.....otherwise causes seg violation on Solaris 2.3
.....Bobby  -  4/11/94
*/
	if (strlen(dir) == 0)
	{
		fullname[0] = '\0';
		goto failed;
	}
/*
.....the following logic only support one envioment value 
.....just use ux_get_syspath without error display first
.....to extend all path
*/
	fullname[0] = '\0';
	first_path = 0;
	if (dir[0]==UX_PATH_SEP)
		first_path = 1;
	ux_get_syspath(dir, &pathlistptr, fullname, &found, UX_NPRTERRS|UX_NCHK|UX_NQUOTES);
	uu_lsdel(pathlistptr);
/*
......the problem with ux_get_syspath is that if we have a path as '/nclnt/src', it will become
......'nclnt/src', so we need check that out and add '/' back in
*/
	strcpy (tmpstr, fullname);
	q = index(tmpstr, UX_PATH_SEP);
	if (q!=NULL)
		*q = '\0';
/*
......now tmpstr must have disk name such as 'c:' otherwise we need add UX_PATH_SEP back in
......because we could have 'nclnt/src' as result path from ux_get_syspath (when
......'/nclnt/src' as input.
*/
#if UU_COMP == UU_VAXVMS
	q = index(tmpstr, ']');
#else
	q = index(tmpstr, ':');
#endif
	if (q==NULL)
	{
		tmpstr[0] = UX_PATH_SEP;
		tmpstr[1] = '\0';
#if UU_COMP == UU_WIN2K
		if ((first_path)&&(fullname[0]!=UX_PATH_SEP))
		{
			strcat (tmpstr, fullname);
			strcpy (fullname, tmpstr);
		}
#else
		if (fullname[0]!=UX_PATH_SEP)
		{
			strcat (tmpstr, fullname);
			strcpy (fullname, tmpstr);
		}
#endif
	}
/*
.....ul_get_full_dir will keep the last UX_PATH_SEP
.....but ux_get_syspath will not, so add in again
*/
	nc = strlen (dir);
	if (dir[nc-1] == UX_PATH_SEP)
	{
		nc = strlen (fullname);
		if (fullname[nc-1] != UX_PATH_SEP)
		{
			fullname[nc] = UX_PATH_SEP;
			fullname[nc+1] = '\0';
		}
	}
	mode = UX_CREATE|UX_EXISTS|UX_FAREA;
	stat = ux_file_inquire(UU_NULL, fullname,UU_NULL,UU_NULL,UU_NULL,&mode,
		&file_status,fullname,UX_NPRTERRS|UX_NQUOTES);
	if (stat != UU_SUCCESS || mode != (mode|UX_FAREA)) goto failed;
	ul_remove_quotes (fullname);
	goto done;
/*
.....Not a directory
*/
failed:;
	status = UU_FAILURE;
done:;
	return (status);
}

/******************************************************************* 
**   E_FUNCTION : ul_build_full_dir(dir,subdir,fullname)
**              This function builds a full directory spec string from
**              the main directory spec 'dir' and the sub-directory
**              spec 'subdir'.
**   PARAMETERS
**       INPUT  :  dir  = main directory spec. (system dependent)
		subdir = subdirectory spec.
**       OUTPUT :  fullname = full directory specification of 'dir'+'subdir'.
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void ul_build_full_dir(dir,subdir,fullname)
char *dir,*subdir,*fullname;
{
	int nc;
#if UU_COMP == UU_VAXVMS
	char *pointer,*rindex();
#endif
/*
.....Append subdirectory to main directory spec
*/
	strcpy (fullname,dir);
	nc = strlen (fullname);
#if UU_COMP == UU_VAXVMS
	pointer = rindex (fullname,']');
	if (pointer != UU_NULL && strlen(subdir) != 0)
	{
		*pointer = '\0';
		strcat (fullname,UL_DIR_SEP);
		strcat (fullname,subdir);
		strcat (fullname,"]");
	}
	else
	{
		strcat (fullname,subdir);
	}
#else
	if (strlen(dir) != 0)
	{
		if (fullname[nc-1] != UX_PATH_SEP)
			strcat (fullname,UL_DIR_SEP);
		strcat (fullname,subdir);
	}
#endif
	return;
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
void ul_build_full_fname(fdir,fname,fext,fullname)
char *fdir,*fname,*fext,*fullname;
{
	int status;
	char *pointer,*rindex();
	UX_pathname fn,dir;
	UX_pathname filename, ext;
	FILE *fptr = NULL;
	strcpy(filename, fname);
/*
......now we allow muti-path define for many dir value
*/
/*
......add ext to the file
*/
	if ((fext!=NULL)&&(fext[0]!='\0'))
	{
		pointer = rindex(filename,'.');
		if (pointer != UU_NULL)
			*pointer = 0;
		strcat(filename,".");
		strcat(filename, fext);
	}
/*
......if file exist, we allow multi path for fdir env value
......so we will find the first existing one first
*/
	status = ul_open_mod_file2(fdir, NULL, filename, 0, &fptr, UU_NULL, UU_NULL);
	if (status==0)
	{
		strcpy(fullname, filename);
		return;
	}
/*
.....continue build filename if not exist
*/
/*
.....Get full directory
*/
	ul_break_fname(fname,dir,fn);
	if (strlen(dir) == 0) ul_getvalid_fulldir(fdir,dir);
	strcpy(fullname,dir);
/*
.....Append filename
*/
	if (fullname[strlen(fullname)-1] != UL_DIR_SEP[0])
		strcat(fullname,UL_DIR_SEP);
	strcat(fullname,fn);
/*
.....Append file extension
*/
	pointer = rindex(fn,'.');
	if (pointer == UU_NULL && (fext!=NULL) && strlen(fext) != 0)
	{
		strcat(fullname,".");
		strcat(fullname,fext);
	}
	return;
}

/*********************************************************************
**       E_FUNCTION : ul_make_dir(dir)
**                      This function warns the user that a directory
**                      does not exist and asks them if they want to
**                      try and create it.  If YES then it will try and
**                      create the directory.
**       PARAMETERS     
**               INPUT  :  dir  = full directory spec of directory to
**                              create.
**               OUTPUT :  none.
**       RETURNS:    UU_SUCCESS if successful or UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS:
*********************************************************************/

ul_make_dir(direc)
	char *direc;
{
	int stat,status;
	char serror[UX_MAX_PATH_LEN+40];
	UU_LOGICAL lstat,ud_yesno();
	int mode=493;
	status = UU_SUCCESS;
/*
......Prompt user if they want to create directory
*/
	sprintf (serror,"Directory %s does not exist.\nDo you wish to create it",direc);
	lstat = ud_yesno (0, serror, "Directory does not exist");
	if (lstat == UU_FALSE) goto failed;
/*
.....Try and create directory
*/
	stat = ux_mk_dir(direc,mode,UX_NPRTERRS|UX_NCHK);
	if (stat != UU_SUCCESS)
	{
		sprintf (serror,"Cannot create directory %s.",direc);
		ud_wrerr (serror);
		goto failed;
	}
	goto done;
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       E_FUNCTION : ul_mod_dir(direc)
**                      This function changes the UNICAD default directory
**                      environmental variables to the new default
**                      directory.
**       PARAMETERS     
**               INPUT  :  direc = full directory spec of new default
**                                      directory.
**               OUTPUT :  none.
**       RETURNS:    UU_SUCCESS if successful or UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS:
*********************************************************************/

ul_mod_dir(direc)
	char *direc;
{
#if UU_COMP == UU_VAXVMS
	int stat;
#endif
	int status;
/*
.....Modify UNICAD default directories
*/
	status = UU_SUCCESS;
#if UU_COMP == UU_VAXVMS
	if (ux_modenv("replace","UX_HOMEDIR",direc,UX_NPRTERRS) != UU_SUCCESS)
		goto failed;
	stat = ux_modenv("replace","UR_PART_AREA",direc,UX_NPRTERRS);
	stat = ux_modenv("replace","UB_LOC_M_SYMDIR",direc,UX_NPRTERRS);
	stat = ux_modenv("replace","UJ_PENTABLE",direc,UX_NPRTERRS);
	stat = ux_modenv("replace","UJ_PLOTFILES_AREA",direc,UX_NPRTERRS);
	stat = ux_modenv("replace","UJ_PLOTSETUP",direc,UX_NPRTERRS);
	stat = ux_modenv("replace","UJ_SIGNON",direc,UX_NPRTERRS);
	stat = ux_modenv("replace","UM_DRW_ARCHIVE_LIB",direc,UX_NPRTERRS);
	stat = ux_modenv("replace","U_RECORD_PLAYB_AREA",direc,UX_NPRTERRS);
#endif
#if UU_COMP == UU_VAXVMS
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
#endif
	return (status);
}

/*********************************************************************
**       E_FUNCTION : ul_set_dir(dir,direc)
**                      This function sets the default working directory
**                      based upon the users original login directory.
**       PARAMETERS     
**               INPUT  :  dir  = full directory spec or subdirectory of
**                              login directory to set default to.
**               OUTPUT :  direc = fullname of directory spec.
**       RETURNS:    UU_SUCCESS if successful or UU_FAILURE otherwise.
**       SIDE EFFECTS: Changes the current working directory.
**       WARNINGS:
*********************************************************************/

int ul_set_dir (dir,direc)
	char *dir,*direc;
{
	UX_pathname fullname;
	char serror[UX_MAX_PATH_LEN+40];
	char *pointer,*index(),*rindex();
	int status,file_status,stat,mode;
	status = UU_SUCCESS;
/*
.....Check for full directory spec
*/
#if UU_COMP == UU_VAXVMS
	pointer = rindex (dir,'[');
#else
#if UU_COMP == UU_WIN2K
	pointer = index (dir,'\\');
#else
	pointer = index (dir,'/');
	if (pointer != dir) pointer = 0;
#endif
#endif
	if (pointer != 0)
	{
		strcpy (fullname,dir);
	}
/*
.....Not a full directory spec
.....Use login directory as main directory
.....and append "dir" as subdirectory
*/
	else
	{
/*
.....Get full login directory name
*/
		if (ul_get_full_dir("HOMEDIR",fullname) != UU_SUCCESS)
		{
			sprintf (serror,"%s is not a directory.",dir);
			ud_wrerr (serror);
			status = UU_FAILURE;
			goto done;
		}
/*
.....Append part directory to login directory
*/
		ul_build_full_dir (fullname,dir,fullname);
	}
/*
.....Check for valid directory
*/
	mode = UX_CREATE|UX_EXISTS|UX_READ|UX_WRITE|UX_FAREA;
	stat = ux_file_inquire(UU_NULL,fullname,UU_NULL,UU_NULL,UU_NULL,&mode,
	&file_status,direc,UX_NQUOTES|UX_NPRTERRS);
/*
.....Remove quotes from pathname
*/
	ul_remove_quotes (direc);
/*
.....Directory doesn't exist
.....Ask the user if they want to try to create one
*/
	if (stat==UU_SUCCESS && mode == (mode|UX_NEXISTS|UX_CREATE))
	{
		if (ul_make_dir(direc) != UU_SUCCESS) 
			{
			status = UU_FAILURE;
			goto done;
			}
	}
/*
.....Specification is not a directory
*/
	else if (stat != UU_SUCCESS || mode != (mode|UX_FAREA))
	{
		sprintf (serror,"%s is not a directory.",direc);
		ud_wrerr (serror);
		status = UU_FAILURE;
		goto done;
	}
/*
.....Read access denied
*/
	else if (stat != UU_SUCCESS || mode != (mode|UX_READ))
	{
		sprintf (serror,"%s does not have read access.",direc);
		ud_wrerr (serror);
		status = UU_FAILURE;
		goto done;
	}
/*
.....Write access denied
*/
	else if (stat != UU_SUCCESS || mode != (mode|UX_WRITE))
	{
		sprintf (serror,"%s does not have write access.",direc);
		ud_wrerr (serror);
		status = UU_FAILURE;
		goto done;
	}
/*
.....Change the default directory
*/
	if (chdir(direc) != UU_SUCCESS) goto failed;
	if (ul_mod_dir(direc) != UU_SUCCESS) goto failed;
	goto done;
failed:;
	status = UU_FAILURE;
	sprintf (serror,"Cannot set %s as the default directory.",direc);
	ud_wrerr (serror);
done:;
	return (status);
}

/*******************************************************************
**   E_FUNCTION : ul_getvalid_fulldir(char *dir, char *fulldir)
**          This function takes the symbol 'dir' and returns the
**          full directory specification in 'fulldir'.  'dir' can
**          be a directory specification or environmental variable.
**                      if the fulldir does not exist, return UX_HOMEDIR
**   PARAMETERS
**       INPUT  :  dir  = directory symbol.
**       OUTPUT :  fulldir = full directory specification of 'dir'.
**   RETURNS:None
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void ul_getvalid_fulldir(dir,fulldir)
char *dir;
char *fulldir;
{
	int stat;

	stat = ul_get_full_dir(dir, fulldir);
/*
.....ul_get_full_dir have already get a full directory path
.....following statement set fulldir back to dir
*/
/*
	mode = 0;
	stat = ux_file_inquire(UU_NULL,dir,UU_NULL,UU_NULL,UU_NULL,&mode,
		&file_status,fulldir,UX_NQUOTES|UX_NPRTERRS);
	ul_remove_quotes (fulldir);
	if (!(stat==UU_SUCCESS && mode == (mode|UX_EXISTS|UX_FAREA)))
*/
	if (stat!=UU_SUCCESS)
		ul_get_full_dir ("UX_HOMEDIR", fulldir);
}

void ulf_get_full_dir(dir,fullname,len)
UM_f77_str_ptr dir,fullname;
UM_int2 *len;
{
	char *dirstr, *fullstr;
	int len2, status;
	len2 = *len;
	dirstr = UM_cstr_of_f77_str(dir);
	dirstr[len2] = '\0';
	fullstr = UM_cstr_of_f77_str(fullname);
	status = ul_get_full_dir (dirstr, fullstr);
	if (status!=UU_SUCCESS)
		fullstr[0] = '\0';
	*len = strlen(fullstr);
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
UM_f77_str_ptr fin,fout;
UM_int4 *maxc,*nci;
{
	char *flnam1,*flnam2;
	flnam1 = UM_cstr_of_f77_str(fin);
	flnam2 = UM_cstr_of_f77_str(fout);
	flnam1[*nci] = '\0';
	ul_short_filename(flnam1,flnam2,*maxc);
}

/*********************************************************************
**       E_FUNCTION : ul_short_filename(fin,fout,maxc)
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
void ul_short_filename(fin,fout,maxc)
char *fin,*fout;
int maxc;
{
	char *p,*rindex(),*index();
	UX_pathname b1;
	char b2[UX_MAX_FILE_LEN];
	int nc1,nc2;
/*
.....Filename is short enough
*/
	if (strlen(fin) < maxc)
		strcpy(fout,fin);
/*
.....Shorten filename
*/
	else
	{
		ul_break_fname(fin,b1,b2);
		nc1 = strlen(b1);
		nc2 = strlen(b2);
/*
........Filename by itself is too long
........Use first part of directory with filename
*/
		if (nc2+5 >= maxc)
		{
			p = index(&b1[4],UX_PATH_SEP);
			if (p == 0) nc1 = 0;
			else
			{
				nc1 = p - b1 + 1;
				if (nc1 > maxc/4) nc1 = maxc / 4;
				strncpy(fout,b1,nc1);
			}
			fout[nc1] = '\0';
			strcat(fout,"{...}"); nc1 = nc1 + 5;
			p = b2 + nc2 - maxc + nc1;
			strcat(fout,p);
		}
/*
........Use partial directory and filename
*/
		else
		{
			nc1 = maxc - nc2 - 5;
			strncpy(fout,b1,nc1);
			fout[nc1] = '\0';
			strcat(fout,"{...}");
			strcat(fout,b2);
		}
	}
}
/*********************************************************************
**       E_FUNCTION : ul_fshort_filename(kfnam, len, fmax, ksnam, slen)
**            Shortens a filename if it is more than 'fmax' characters.
**            Used to output filenames in error messages, print files,
**            etc. This function called by Fortran function
**       PARAMETERS     
**            INPUT  :
**               kfnam   = Input filename.
**						len :  = Input filename Length.
**               fmax  = Maximum number of characters in 'ksnam'.
**            OUTPUT :
**               ksnam  = Output filename..
**						slen :  = Out filename Length.
**       RETURNS:    none.
**       SIDE EFFECTS: none
**       WARNINGS:
*********************************************************************/
void ul_fshort_filename(kfnam, len, fmax, ksnam, slen)
char *kfnam, *ksnam;
int *len, *fmax, *slen;
{
	kfnam[*len] = '\0';
	ul_short_filename(kfnam, ksnam, *fmax);
	*slen = strlen(ksnam);
}

/*********************************************************************
**       E_FUNCTION : ul_default_ftype(ftype,fname)
**            Appends the default file type to the filename if it does
**            already have a file type.
**       PARAMETERS     
**            INPUT  :
**               ftype   = Default file type.
**               fname   = File name.
**            OUTPUT :
**               fname   = Updated file name.
**       RETURNS:    none.
**       SIDE EFFECTS: none
**       WARNINGS:
*********************************************************************/
void ul_default_ftype(ftype,fname)
char *ftype, *fname;
{
	UX_pathname dir,fullname;
	ul_remove_quotes(fname);
	ul_break_fname(fname,dir,fullname);
	ul_build_full_fname(dir,fullname,ftype,fname);
}

/******************************************************************* 
**   E_FUNCTION : ul_get_full_dir(dir,fullname)
**          This function takes the symbol 'dir' and returns the
**          full directory specification in 'fullname'.  'dir' can
**          be a directory specification or environmental variable.
**   PARAMETERS
**       INPUT  :  dir  = directory symbol.
**       OUTPUT :  fullname = full directory specification of 'dir'.
**   RETURNS:    UU_SUCCESS if 'fullname' is a directory or UU_FAILURE
**                      otherwise.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
ul_get_full_dir(indir,fullname)
char *indir,*fullname;
{
	UX_pathname dir, localdir, ext_dir, act_dir;
	int k, status,mode,file_status,nc,first_path;
	char *str;

	status = UU_SUCCESS;
	if (strlen(indir) == 0)
	{
		fullname[0] = '\0';
		goto failed;
	}
	strcpy(localdir, indir);
	status = ux_search_for_path(localdir, ext_dir, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
	if (status==UU_SUCCESS)
	{
		mode = UX_EXISTS | UX_READ;
		status = ux_mk_chk_syspath(UU_NULL, ext_dir, UU_NULL, UU_NULL,
				UU_NULL, &mode, fullname, UX_NPRTERRS|UX_NQUOTES);
		if (status == UU_SUCCESS && mode == (mode|UX_FAREA))
		{
			ul_remove_quotes(fullname);
			nc = strlen (fullname);
/*
			if (fullname[nc-1] != UX_PATH_SEP)
			{
				fullname[nc] = UX_PATH_SEP;
				fullname[nc+1] = '\0';
			}
*/
/*
......to consistent with function before, no ending SEP for dir
*/
			if (fullname[nc-1] == UX_PATH_SEP)
			{
				fullname[nc-1] = '\0';
			}
			return 0;
		}
	}
/*
.....the dir can be a "format of dir1;dir2;dir3;..."
.....so we get directory one by one
*/
	if (ul_is_mult_dir_support(localdir, ext_dir)==0)
	{
		fullname[0] = '\0';
		goto failed;
	}
	strcpy(localdir, ext_dir);
/*
.....the ext_dir can be a "format of dir1;dir2;dir3;..."
.....so we get directory one by one
*/
	str = strtok (localdir, ";");
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
		mode = UX_EXISTS | UX_READ;
		status = ux_search_for_path(dir, act_dir, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
		if (status==UU_SUCCESS)
		{
			status = ux_mk_chk_syspath(UU_NULL, act_dir, UU_NULL, UU_NULL,
					UU_NULL, &mode, fullname,UX_NPRTERRS|UX_NQUOTES);
			if (status == UU_SUCCESS && mode == (mode|UX_FAREA))
			{
				ul_remove_quotes(fullname);
				nc = strlen(fullname);
/*
......to consistent with function before, no ending SEP for dir
*/
				if (fullname[nc-1] == UX_PATH_SEP)
				{
					fullname[nc-1] = '\0';
				}
				return 0;
			}
		}
		str = strtok(NULL, ";");
	}
/*
.....Not a directory
*/
failed:;
	status = UU_FAILURE;
done:;
	return (status);
}
/*
.....dir, fname define at least 256 and 80 chars
*/
void ulf_break_fname(fullname,nc,dir,nc1,fname,nc2)
UM_f77_str_ptr fullname,dir,fname;
UM_int4 *nc, *nc1, *nc2;
{
	int i;
	char *dirstr, *fullstr, *fstr;
	fullstr = UM_cstr_of_f77_str(fullname);
	fullname[*nc] = '\0';
	dirstr = UM_cstr_of_f77_str(dir);
	fstr = UM_cstr_of_f77_str(fname);
	ul_break_fname(fullstr,dirstr,fstr);
	*nc1 = strlen(dirstr);
	*nc2 = strlen(fstr);
	for (i=*nc1;i<255;i++)
		dirstr[i] = ' ';
	for (i=*nc2;i<80;i++)
		fstr[i] = ' ';
}
