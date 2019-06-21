/************************************************************************
**
**  NAME:	xf1farea.c 
**
**      contains:		ux_chmod
**							ux_copy
**							ux_delete
**							ux_get_file_hdr
**							ux_file_rewind
**							ux_get_flist
**							ux_get_fvers
**							ux_get_os_filedesc
**							ux_mk_chk_syspath
**							ux_mk_dir
**							ux_nxt_file
**							ux_rename
**							ux_rmdir
**							ux_file_inquire
**							ux_get_ver_index
**							ux_get_dct_index
**
**  COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**   MODULE NAME AND RELEASE LEVEL 
**       xf1farea.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 16:59:27
**
**************************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "umoveb.h"
#include "xfsys1.h"			/* the include file for level one file system */
#include "xfsys0.h"
#include "udebug.h"			/* used by the debug system */
#include "xenv1.h"			/* the include file for level one env system */
#include "derror.h"			/* needed for error system resolution */
#include "uhep.h"				/* used by the debug system */
#include "nclver.h"				/* Defines the current NCL502 version num*/
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

static long saver;

#if (UU_COMP == UU_WIN2K)
#include <string.h>
#define index strchr
#define rindex strrchr
#endif

#if UU_COMP == UU_VAXVMS
#	include <types.h>
#	include <stat.h>

/* MILLS  The descrip.h file is included in mfort.h which is included in
			 nclver.h so the following include is commented out to keep from 
			 getting two copies included.   M. Gump 4-27-90
*/
/* #	include <descrip.h> */

#	include <rmsdef.h>
#   include <unistd.h>
#else
#	include <fcntl.h>
#	include <sys/types.h>
#	include <sys/stat.h>
#if (UU_COMP != UU_WIN2K)
#ifdef UU_RS6000
#include <sys/dir.h>
#include <unistd.h>
#else
#if (UU_COMP == UU_WINNT) 
#include <sys/dir.h>
#else

#	if UU_OPSYS == UU_SYS5
#		include <dirent.h>
#	else
#		if UU_OPSYS == UU_SYS53
#			include <bsd/sys/dir.h>
#		else
#			include <sys/dir.h>
#		endif
#	endif
#endif

#endif

#endif

#endif
#define TRACE UU_TRUE

UM_int2 NCL_ubas_unit = -1;
int UR_verindex, UR_dctindex;
/*********************************************************************
**    E_FUNCTION :  ux_delete(pathname, options)
**			Deletes the file (not a file area) given by "pathname".
**			PARAMTERS:
**			"pathname": A full quoted system dependent pathname to 
**				the file to be deleted.
**			"options": A bitwise "OR" of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					UX_CHK: check the syntax of the path name.
**					UX_NCHK: don't check the syntax of the path name.
**					If incorrectly specified we will print errors and not
**					check path name syntax.
**			RETURNS:
**			UU_SUCCESS is returned if the file is deleted;
**			otherwise one of the following values is returned:
**				(these are all considered errors)
**				UX_BAD_SUBJECT: bad syntax for "pathname";
**				UX_NO_ACCESS: don't have permission/access to delete 
**						the file;
**				UX_NFOUND: file not found;
**				UX_FAILURE: something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_delete(pathname, options)
	char *pathname;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	int mode;
	char *index();
	char *rindex();
	char *lq, *fq;
	UX_pathname noquote;

	uu_denter(UU_XTRC,(us,"ux_delete(path: %x, options: %d)",pathname, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathname:%s", pathname));
	}
#endif
	status = UU_SUCCESS; /* assume success */
 	UX_CHK_PATHERRS(pathname, printit, options); 

	/* check that file exists and is a file, check that we have delete access */
	mode = UX_DELETE;		/* check for delete access only */
	if ((status = ux_access1(pathname,&mode,(options|UX_NCHK))) == UX_FAILURE)
		/* may return UU_SUCCESS, UX_FAILURE, or UX_NO_ACCESS */
		goto failed;
	if (status == UX_NO_ACCESS)
	{
		UX_ERROR2(22,pathname,"ux_delete",printit);
		goto done;
	}

	if ( (mode == (UX_NEXISTS|mode)) || (mode == (UX_FAREA|mode)) )
	{
		/* case of file doesn't exist  or is a filearea */
		status = UX_NFOUND;
		UX_ERROR2(22,pathname,"ux_delete",printit);
		goto done;
	}
	if ( (mode | UX_DELETE) != mode) /* then file hasn't delete access */
	{
		status = UX_NO_ACCESS;
		UX_ERROR2(22,pathname,"ux_delete",printit);
		goto done;
	}

	/* remove quotes to get working pathname: */
	fq = index(pathname,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(noquote,(fq+1));
			lq = rindex(noquote,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	else
		strcpy(noquote,pathname);

	if ((status = ux_delete0(noquote)) != UU_SUCCESS)
		/* may return UU_SUCCESS or UX_FAILURE */
		goto failed;

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
**    E_FUNCTION :  ux_rename(currentname, newname, options)
**			Attempts to rename "currentname" to "newname". Overwrites any 
**			file at "newname". 
**		PARAMETERS:
**			"currentname": A quoted system dependent path name 
**				to the file to be renamed.
**			"newname": A quoted system dependent new path name.
**			"options": A bitwise "OR" of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					UX_CHK: check the syntax of "currentname" 
**						and "newname".
**					UX_NCHK: don't check the syntax of either
**						"currentname" nor "newname".
**					If incorrectly specified we will print errors and not
**					check path name syntax.
**		RETURNS:
**			UU_SUCCESS is returned if the file is renamed;
**			otherwise, one of the following values is returned:
**				(these are all considered errors)
**				UX_BAD_SUBJECT: bad syntax "currentname";
**				UX_BAD_TARGET: bad syntax "newname";
**				UX_NO_ACCESS: can't access "pathname";
**				UX_FAILURE: something else went wrong.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_rename(currentname, newname, options)
	char *currentname;
	char *newname;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	char *index();
	char *rindex();
	char *lq, *fq;
	UX_pathname noquote, nqnewname;

	uu_denter(UU_XTRC, (us, "ux_rename(current:%x, new:%x, options:%d)",
		currentname, newname, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (currentname!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"currentname:%s", currentname));
		if (newname!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"newname:%s", newname));
	}
#endif
	UX_CHK_PATHERRS(currentname, printit, options);
	if (options != (options | UX_NCHK))                        
		if (ux_chk_path_syntax(newname, options) != UU_SUCCESS) 
		{                        
			status = UX_BAD_TARGET;
			goto done;                               
		}
	status = UU_SUCCESS; /* assume success */

	/* remove quotes to get working pathname: */
	fq = index(currentname,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(noquote,(fq+1));
			lq = rindex(noquote,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	else
		strcpy(noquote,currentname);

	if (ux_access0(noquote,UX_EXISTS) == UX_FAILURE)
	{
		status = UX_NO_ACCESS;
		UX_ERROR2(22,noquote,"ux_rename",printit);
		goto done;
	}

	/* remove quotes to get working pathname: */
	fq = index(newname,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(nqnewname,(fq+1));
			lq = rindex(nqnewname,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	else
		strcpy(nqnewname,newname);

	/* check to see if nqnewname already exists and question overwriting it */
		if (ux_access0(nqnewname,UX_EXISTS) == UU_SUCCESS)
		{
			status = UX_BAD_TARGET;
			UX_ERROR2(23,nqnewname,"ux_rename",printit);
			goto done;
		}

	if ( ux_rename0(noquote, nqnewname) == UX_FAILURE)
		goto failed;

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
**    E_FUNCTION :  ux_mk_dir(pathname, mode, options)
**			Attempts to create a file area at the location
**			designated by "pathname".
**		PARAMTERS:
**			"pathname": A quoted system dependent full path name to 
**				the file area to be created.
**			"mode": mode the file area is to be created.
**			"options": A bitwise "OR" of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					UX_CHK: check the syntax of "pathname".
**					UX_NCHK: don't check the syntax of "pathname".
**					If incorrectly specified we will print errors and not
**					check path name syntax.
**		RETURNS:
**			UU_SUCCESS is returned if the file area is created;
**			otherwise one of the following values is returned:
**				(these are all considered errors)
**				UX_BAD_SUBJECT: bad syntax for "pathname";
**				UX_NO_ACCESS: don't have access to create file area;
**				UX_BAD_TYPE: wrong UNICAD area type for area in
**					which the file area is to be created;
**				UX_BAD_MODE: can't create file area with this mode;
**				UX_FOUND: file found, already exists at "pathname";
**				UX_FAILURE: something else went wrong.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_mk_dir(pathname, mode, options)
	char *pathname;
	int mode;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	char *index();
	char *rindex();
	char *lq, *fq;
	UX_pathname noquote;

	uu_denter(UU_XTRC, (us, "ux_mk_dir(pathname: %x,mode: %o, options: %d)",
		pathname, mode, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathname:%s", pathname));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	UX_CHK_PATHERRS(pathname, printit, options);

	/* remove quotes to get working pathname: */
	fq = index(pathname,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(noquote,(fq+1));
			lq = rindex(noquote,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	else
		strcpy(noquote,pathname);

	if (ux_access0(noquote,UX_EXISTS) == UU_SUCCESS)
	{
		status = UX_FOUND;
		goto done;
	}

	if ( (mode > 0777) || ( mode < 0 ) )
	{
		status = UX_BAD_MODE;
		goto done;
	}
	if (mkdir(noquote, mode) != UU_SUCCESS)
		goto failed;

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
**		E_FUNCTION:		ux_rmdir(pathname, options)
**				Attempts to delete the file area designated by 
**				"pathname". The area must be empty.
**		PARAMETERS:
**				"pathname": A full quoted system dependent path name 
**					to file area.
**				"options": A bitwise "OR" of the following values:
**						UX_PRTERRS: print any errors as specified by
**							uu_uerror functions.
**						UX_NPRTERRS: don't print any errors as 
**							specified by uu_uerror functions.
**						UX_CHK: check the syntax of the path name.
**						UX_NCHK: don't check the syntax of the path
**							name.
**						If incorrectly specified we will print errors and not
**						check path name syntax.
**			RETURNS:
**				UU_SUCCESS is returned if the directory is deleted;
**				otherwise, one of the following values is returned:
**					(these are all considered errors)
**					UX_BAD_SUBJECT: bad "pathname";
**					UX_NO_ACCESS: don't have access to "pathname";
**					UX_NFOUND: file area not found;
**					UX_FAILURE: something else went wrong.
**		SIDE EFFECTS:	none
**		WARNINGS:	none
*********************************************************************/
int ux_rmdir(pathname, options)
	char *pathname;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int mode;
	UU_LOGICAL printit;
	char *index();
	char *rindex();
	char *lq, *fq;
	UX_pathname noquote;

	uu_denter(UU_XTRC, (us, "ux_rmdir(pathname: %x ,options: %d)",
		pathname, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathname:%s", pathname));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	UX_CHK_PATHERRS(pathname, printit, options);

	mode = UX_DELETE;		/* check for delete access only */
	if ((status = ux_access1(pathname,&mode,(options|UX_NCHK))) != UU_SUCCESS)
		/* may return UU_SUCCESS, UX_FAILURE, or UX_NO_ACCESS */
		goto failed;
	if ( (mode == (UX_NEXISTS|mode)) || (mode != (UX_FAREA|mode)) )
	{
		/* case of filearea doesn't exist  or isn't a filearea */
		status = UX_NFOUND;
		goto done;
	}
	if ( (mode|UX_DELETE) != mode) /* then filearea hasn't delete access */
	{
		status = UX_NO_ACCESS;
		goto done;
	}

	/* remove quotes to get working pathname: */
	fq = index(pathname,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(noquote,(fq+1));
			lq = rindex(noquote,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	else
		strcpy(noquote,pathname);

#if UU_COMP == UU_VAXVMS
	if ((status = ux_delete0(noquote)) != UU_SUCCESS)
		goto failed;
#else
		if (rmdir(noquote) != UU_SUCCESS)
			goto failed;
#endif

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
**		E_FUNCTION: ux_copy(pathfrom, pathto, fenv, options)
**			Copies the file designated by "pathfrom" to the file
**			designated by "pathto". The file can not be a file area.
**		PARAMETERS:
**			"pathfrom": A full quoted system dependent path name to the 
**				file to be copied.
**			"pathto": A full quoted system dependent path name to 
**				where the file is to be copied.
**			"fenv": UU_NULL if no checking is to be done; otherwise it is
**					the environmental variable that determines compatibility 
**					of file type and the directory extension, checking is done
**					on the file to determine if the file can be placed in the
**					directory requested.
**			"options": A bitwise "OR" of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					UX_CHK: check the syntax of both "pathfrom"
**						and "pathto".
**					UX_NCHK: don't check the syntax of either
**						"pathfrom" nor "pathto".
**					If incorrectly specified we will print errors and not
**					check path name syntax.
**		RETURNS:
**			UU_SUCCESS is returned if the file copy was done,
**			otherwise, one of the following values will be returned:
**				(these are all considered errors)
**				UX_BAD_SUBJECT: "pathfrom" is bad;
**				UX_BAD_TARGET: "pathto" is bad;
**				UX_BAD_ENV: "pathto" file area exten. doesn't match "fenv" exten.
**				UX_BAD_TYPE: file to be copied is the wrong type for
**					area being copied to;
**				UX_DIR: "pathfrom" is a directory;
**				UX_FAILURE: something else went wrong.
******************************************************************/
int ux_copy(pathfrom, pathto, fenv, options)
	char *pathfrom;
	char *pathto;
	char *fenv;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	UX_pathname noquote, nqpathto;
	char *index();
	char *rindex();
	char *lq, *fq;

	uu_denter(UU_XTRC,(us,"ux_copy(pathfrom:%x, pathto:%x, fenv:%x, options:%d)",
		pathfrom, pathto, fenv, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathfrom != UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathfrom:%s", pathfrom));
		if (pathto != UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathto:%s", pathto));
		if (fenv != UU_NULL)
			uu_dprint(UU_XTRC,(us,"fenv:%s", fenv));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	UX_CHK_PATHERRS(pathfrom, printit, options);
	/* chk_patherrrs for pathto */
	if (options != (options | UX_NCHK))                        
		if (ux_chk_path_syntax(pathto, options) != UU_SUCCESS) 
		{                        
			status = UX_BAD_TARGET;
			goto done;                               
		}

	/* remove quotes to get working pathname: */
	fq = index(pathfrom,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(noquote,(fq+1));
			lq = rindex(noquote,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	else
		strcpy(noquote,pathfrom);

	if (ux_access0(noquote,UX_EXISTS) == UX_FAILURE)
	{
		status = UX_NO_ACCESS;
		UX_ERROR2(22,noquote,"ux_copy",printit);
		goto done;
	}
	/* check that pathfrom is not a filearea (directory) */
	if (ux_is_farea(noquote,(options|UX_NCHK)) == UU_SUCCESS)
	{
		status = UX_DIR;
		goto done;
	}

	if (fenv != UU_NULL)
		/* check filearea extension and file type compatibility */
		if ((status = ux_is_type_legal(pathfrom,fenv,options))!= UU_SUCCESS)
		{
			if (status == UX_FAILURE)
				goto failed;
			else
				goto done;
		}

	/* remove quotes to get working pathname: */
	fq = index(pathto,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(nqpathto,(fq+1));
			lq = rindex(nqpathto,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	else
		strcpy(nqpathto,pathto);

	/* check to see if pathto already exists and question overwriting it */
		if (ux_access0(nqpathto,UX_EXISTS) == UU_SUCCESS)
		{
			status = UX_BAD_TARGET;
			UX_ERROR2(23,nqpathto,"ux_copy",printit);
			goto done;
		}

	if (ux_copy0(noquote, nqpathto) == UX_FAILURE)
		goto failed;

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}	
/*****************************************************************
**		E_FUNCTION:	ux_chmod(pathname, mode, options)
**			Attempts to change the mode on the file designated by
**			"pathname".
**		PARAMETERS:
**			"pathname": A full quoted system dependent path name 
**				to the file to have its access mode changed.
**			"mode": new mode of the file; UNIX mode specification.
**			"options": A bitwise "OR" of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					UX_CHK: check the syntax of the path name.
**					UX_NCHK: don't check the syntax of the path
**						name.
**					If incorrectly specified we will print errors and not
**					check path name syntax.
**		RETURNS:
**			UU_SUCCESS is returned if the mode was changed to "mode",
**			otherwise one of the following values will be returned:
**				UX_BAD_SUBJECT: "pathname" is bad;
**				UX_BAD_MODE: illegal mode specification;
**				UX_NO_ACCESS: can't access file to change mode;
**				UX_FAILURE: something else went wrong.
**		SIDE EFFECTS: none
**		WARNINGS: none
********************************************************************/
int ux_chmod(pathname, mode, options)
	char *pathname;
	int mode;
	int options;			
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	char *rindex();
	char *index();
	char *lq, *fq;
	UX_pathname noquote;

	uu_denter(UU_XTRC, (us, "ux_chmod(pathname:%x, mode:%o, options:%d)",
		pathname, mode, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathname:%s", pathname));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	UX_CHK_PATHERRS(pathname, printit, options);

	/* remove quotes to get working pathname: */
	fq = index(pathname,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(noquote,(fq+1));
			lq = rindex(noquote,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	else
		strcpy(noquote,pathname);

	if ((ux_access0(noquote,UX_EXISTS) == UX_FAILURE) ||
		(ux_access0(noquote,UX_WRITE) == UX_FAILURE))	
	{
		status = UX_NO_ACCESS;
		goto done;
	}

	if ( (mode>0777) || (mode<0) )
	{
		status = UX_BAD_MODE;
		goto done;
	}

	if (chmod(noquote,mode) != UU_SUCCESS)	/* chmod success returns zero */
		goto failed;

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}	
/*************************************************************
**	E_FUNCITON: ux_get_file_hdr(unifile, hdrptr, statusptr, options)
**			Attempts to read the file header designated for the
**			file associated with the UNICAD file descriptor "unifile". 
**			The file must be open. This function leaves the file 
**			open and the file pointer (if any) pointing to the 
**			next byte/block (if any) beyond the header.
**		PARAMETERS:
**			"unifile": Pointer to the UNICAD file descriptor 
**				obtained from "ux_open".
**			"hdrptr": returns with the input buffer pointed to
**				by "hdrptr" containing any UNICAD file header 
**				contained in the file.
**			"statusptr": Returns a pointer to the one of the following:
**					UX_FOUND: header found on the file.
**					UX_NFOUND: no header found on the file.
**			"options": One of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					If incorrectly specified we will print errors.
**		RETURNS:
**			UU_SUCCESS is returned if no software failures noticed;
**			otherwise, one of the following values is returned:
**				UX_NO_ACCESS: don't have access to the file 
**					associated with "unifile"; i.e. file isn't open;
**				UX_FAILURE: something else went wrong.
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_get_file_hdr(unifile, hdrptr, statusptr, options)
	int unifile;
	UX_fheader *hdrptr;
	int *statusptr;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int sizec, nread,  plus;
	UU_LOGICAL printit;
	char *ptr, *cptr;	/*MILLS: used to get version number from header */
 char units[10];
 int i,iu;
 int fil;


	uu_denter(UU_XTRC, (us,
		"ux_get_file_hdr(unifile:%d,hdrptr:%x,statusptr:%x,options:%d)",
		unifile, hdrptr, statusptr, options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	sizec = sizeof(char);

	if ( (unifile >= otind ) || (otable[unifile].fileptr == UU_NULL) )
	{
		status = UX_NO_ACCESS;
		goto done;
	}
	/* set the pointer for input/output to beginning of file */
	rewind(otable[unifile].fileptr);
	saver = ftell(otable[unifile].fileptr);

	/* if (ux_fseek0(otable[unifile].fileptr,0L,0) != UU_SUCCESS) goto failed; */

	/* char head[30] -- title */
/*
.....call ux_fgets1 with the flag set to read the header till a null character
*/
	ux_fgets1(hdrptr->head, 30, otable[unifile].fileptr,1);
	if (strcmp(hdrptr->head,"12345UNICAD FILE HEADER DATA\n") != 0)
	{
		*statusptr = UX_NFOUND;
		/* set the pointer for input/output to beginning of file */
		rewind(otable[unifile].fileptr);
		/* if (ux_fseek0(otable[unifile].fileptr, 0L, 0) != UU_SUCCESS)
			goto failed; */
		goto done;
	}
	else
		*statusptr = UX_FOUND;
	
		/* read the rest of the fixed part of the header */
		/* char nbrbytes[6] -- number of bytes in total header */
		ux_fgets0(hdrptr->nbrbytes, 6, otable[unifile].fileptr);
		/* char data[30]; date file is created */
		ux_fgets0(hdrptr->data, 30, otable[unifile].fileptr);
/*
...Check if units field is specified in file 
*/
  iu   = 1;
  strncpy (units,&hdrptr->data[1],4);
  if (strncmp(units,"MM",2) == 0)
    NCL_ubas_unit = 1;
  else if (strncmp(units,"INCH",4) == 0)
    NCL_ubas_unit = 0;
  else  iu = 0;
	strcpy(NCL_infile_date,&hdrptr->data[12]);

		/* char version[20]; product and/or version of s/w that created file */
		ux_fgets0(hdrptr->version, 20, otable[unifile].fileptr);

	/*MILLS: get version number from header into NCL_infile_version */
	ptr = hdrptr->version;
	while (!isdigit(*ptr)) ptr++; 
	cptr = ptr;
	while (isdigit(*ptr++));	/* get passed '.' */
	while (isdigit(*ptr)) ptr++;
	*ptr = '\0';
	sscanf(cptr,"%lf", &NCL_infile_version);
/*
.....vp 2/12/98 if file version is old force new header in output file
*/
	if (NCL_infile_version != NCL_version) iu = 0;

	/*MILLS: EOC */

   UR_verindex = ux_get_ver_index();

		/* char precision[6] holds number of bytes in UU_REAL type */
		ux_fgets0(hdrptr->precision, 6, otable[unifile].fileptr);
		/* char hformat[12]; BLOCK (blocked sequencial) or STREAM */
		ux_fgets0(hdrptr->hformat, 12, otable[unifile].fileptr);
		/* char hinterp[12]; ASCII, TEXT (non-ascii text), BINARY */
		ux_fgets0(hdrptr->hinterp, 12, otable[unifile].fileptr);
		/* char machine[20]; */
		ux_fgets0(hdrptr->machine, 20, otable[unifile].fileptr);
	/*MILLS: get machine type from header into NCL_machine_type */
	ptr = hdrptr->machine;
	i = 1;
	while (*ptr == ' ') {i++; ptr++;}
	cptr = ptr;
	while (i<20 && *ptr != ' ' && *ptr != '\0') {i++; ptr++;}
	*ptr = '\0';
	strcpy(NCL_machine_type, cptr);
	/*MILLS: EOC */


 if (iu == 1)
  saver = ftell(otable[unifile].fileptr);
    
		/* char *extra;	extra header info put in by user */
	
		UX_SSCANF0((hdrptr->nbrbytes,"%d", &plus), nread);

		/* determine if there is extra header to read and read it in */
		if (plus > UX_HDR_LEN)
		{
			hdrptr->extra = (char *)uu_toolmalloc(plus-UX_HDR_LEN);
			if (ux_fread0(hdrptr->extra,sizec,(plus-UX_HDR_LEN),
			otable[unifile].fileptr, &nread) != UU_SUCCESS)
			{
				UX_ERROR0(12,printit);
				/* Unable to get the extra header info. */
				goto failed;
			}
		}
		else
			hdrptr->extra = UU_NULL;
/*
......following statement will cause posible wrong buffer position for WinNT
......Yurong 2/28/02
*/
#if UU_COMP!=UU_WIN2K		
	/* 2/5/87:possible buffer trick -- seek no bytes from current position */

	ux_fseek0(otable[unifile].fileptr, 0L, 1);
#else
/*
......In case of format = 'strean' and interp = 'binary', (such as
......pen table file and archieve file, those file open as binary, so it will
......not doing the translations involving carriage-return and linefeed characters), 
......the ux_fseek0 will set the current position at (beginnning + ftell(), 
......but ftell will return positin which ignore '\n'
......thus the position will different than it should be if there is a new line
*/ 
	if (!((strcmp(otable[unifile].format, "STREAM")==0)&&
		(strcmp(otable[unifile].interp, "BINARY")==0)))
		ux_fseek0(otable[unifile].fileptr, 0L, 1);
#endif
   fil = ftell(otable[unifile].fileptr);
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
**    E_FUNCTION : ux_file_rewind(unifile,options) 
**						 Will go to the start of the open file and read past the
**						 text header, positioning for the next I/O at start
**						 of the data.
**			
**    PARAMETERS   
**       INPUT  : 
**			"unifile": The integer value that is the file's index in the
**					Unicad open file table
**			"options": One of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					If incorrectly specified we will print errors.
**       OUTPUT :  
**					none
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_file_rewind(unifile,options)
	int unifile;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UX_fheader hdr;
	int hstat;

	uu_denter(UU_XTRC, (us, "ux_file_rewind(unifile:%d)",unifile));
	status = UU_SUCCESS; /* assume success */

	if (unifile < 0) goto failed;
	if (ux_get_file_hdr(unifile,&hdr,&hstat,options) != UU_SUCCESS)
		goto failed;

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}	

/****************************************************************
**	E_FUNCTION: ux_get_flist(pathname, farea_flag, ftype, listptr, options)
**			Attempts to construct a list of all files of UNICAD
**			type "ftype" in the file area, "pathname".
**		PARAMETERS:
**			"pathname": A full quoted system dependent path name 
**				to the file area containing the files to be listed.
**			"farea_flag": indicates files or file area to be listed.
**			"ftype": UNICAD file type to be listed; UX_ALL indicates
**				that all files in the area will be listed; may be a
**				symbol or a quoted suffix string.
**			"listptr": returns a descriptor to the list structure
**				for listing files, UU_NULL if an empty list and no 
**				files were found to have this ftype extension.
**			"options": A bitwise "OR" of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					UX_CHK: check the syntax of the path name.
**					UX_NCHK: don't check the syntax of the path
**						name.
**					If incorrectly specified we will print errors and not
**					check path name syntax.
**		RETURNS:
**			UU_SUCCESS is returned if the list structure is filled
**			in; otherwise, one of the following values will be 
**			returned: (these are all considered errors)
**				UX_BAD_SUBJECT: "pathname" is bad;
**				UX_FAILURE: something else went wrong.
**		WARNINGS:
**			The application must delete the list when done with it
**			using a call to "uu_lsdel".
****************************************************************/
int ux_get_flist(pathname, farea_flag, ftype, listptr, options)
	char *pathname;
	int farea_flag;
	char *ftype;
	char **listptr;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	char *flist; 
	char *index();
	char *rindex();
	char *lq, *fq;
	char *uu_lsnew();
	char *uu_lsinsrt();
	char *uu_lsdel();
	UU_LOGICAL found;
	UX_pathname noquote;
	UX_pathname outtype;
	char *pathlistptr = UU_NULL;
/*temp undef this function */

#if (UU_COMP != UU_WIN2K)
#if UU_COMP == UU_VAXVMS
	int statvax;
	static char buffv[UX_MAX_PATH_LEN];				/* holds return file name		*/
	static char buff2[UX_MAX_PATH_LEN];				/* holds wild card file name	*/
	static $DESCRIPTOR (wild,buff2);
	static $DESCRIPTOR (result,buffv);
	static int	test[12];							/* dummy parameter				*/
#else
	char *suffix;
#if UU_COMP == UU_RIDGE
	struct direct dirrec;
	int fddir;
	int sizer;
#else
	static DIR *dirptr;		/* routine opendir return pointer to dir info	*/
#if UU_COMP == UU_DECUNIX
	struct dirent *dirbuf;
#else
#if UU_OPSYS == UU_SYS5
	struct dirent *dirbuf;
#else
	struct direct *dirbuf;	/* routine readdir returns file name info */
#endif
#endif
#endif
#endif

#endif /*!UU_WIN2K*/
	uu_denter(UU_XTRC,(us,
		"ux_get_flist(pathname:%x,farea_flag:%d,ftype:%s,???,options:%d)",
		pathname, farea_flag, ftype, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathname:%s", pathname));
	}
#endif
	status = UU_SUCCESS; /* assume success */
 	UX_CHK_PATHERRS(pathname, printit, options); 

	if (strcmp(ftype,UX_ALL) == 0)
		strcpy(outtype,"*");
	else
	{
		if (ux_get_syspath(ftype,&pathlistptr,outtype,&found,(options|UX_NQUOTES))
			!= UU_SUCCESS)
			goto failed;
		uu_lsdel(pathlistptr);	
	}

	/* remove quotes to get working pathname: */
	fq = index(pathname,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(noquote,(fq+1));
			lq = rindex(noquote,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	else
		strcpy(noquote,pathname);

	uu_toolmalloc_init();			/* used by list package */
	flist = uu_lsnew();
	*listptr = flist;

#if (UU_COMP != UU_WIN2K)

#if UU_COMP == UU_VAXVMS	/* if on the VAX: */
	test[0] = 0;
	strcpy(buff2, noquote);			/* assumes [], not .DIR format */
	if (farea_flag == UX_FAREA)
	{										/* resulting in [xx]*_.DIR	*/
		strcat(buff2, "*_");
		strcat(buff2,outtype);
		strcat(buff2,".DIR");
	}
	else
	{
		strcat(buff2, "*.");
		strcat(buff2,outtype);
	}

	while ((statvax = lib$find_file(&wild,&result, test)) == RMS$_NORMAL )
	{
		/* matches suffix for file type, so put onto list */
		flist = uu_lsinsrt(flist,UX_MAX_PATH_LEN);
		uu_move_byte(buffv, flist, UX_MAX_PATH_LEN);
	}
#else
#if UU_COMP  == UU_RIDGE
	/* open the directory pathname */
	if ( (fddir = open(noquote,O_RDONLY)) == -1)
		goto failed;
	sizer = sizeof(struct direct);
	while ( read(fddir,&dirrec,sizer) != 0)
	{
		if (strcmp(outtype,"*") == 0)
		{
				flist = uu_lsinsrt(flist,UX_MAX_PATH_LEN);
				uu_move_byte(dirrec.d_name, flist, UX_MAX_PATH_LEN);
		}
		else
		{
			/* get last "." or "_" and read from there for suffix */
			if ( farea_flag == UX_FAREA)
				suffix = rindex(dirrec.d_name,'_');
			else
				suffix = rindex(dirrec.d_name,'.');
			if (suffix != UU_NULL)
				if (strncmp(++suffix,outtype,(strlen(outtype)))==0)
				{
					/* matches suffix for file type, so put onto list */
					/* limited characters allowed after suffix: ; or \0 */
					/* otherwise the match is not exact */
					if ( (*(suffix+strlen(outtype))=='\0') ||
						  (*(suffix+strlen(outtype))==';') )
					{
					flist = uu_lsinsrt(flist,UX_MAX_PATH_LEN);
					uu_move_byte(dirrec.d_name, flist, UX_MAX_PATH_LEN);
					}
				}
		}
	}
	close(fddir);
#else
	/* open the directory pathname */
	if ((dirptr = opendir(noquote)) == UU_NULL)
		/* directory isn't a directory or can't be accessed */
		goto failed;

	while ((dirbuf = readdir(dirptr)) != UU_NULL)
	{
		if (strcmp(outtype,"*") == 0)
		{
				flist = uu_lsinsrt(flist,UX_MAX_PATH_LEN);
				uu_move_byte(dirbuf->d_name, flist, UX_MAX_PATH_LEN);
		}
		else
		{
			/* get last "." or "_" and read from there for suffix */
			
			if ( farea_flag == UX_FAREA)
				suffix = rindex(dirbuf->d_name,'_');
			else
				suffix = rindex(dirbuf->d_name,'.');

			if (suffix != UU_NULL)
				if (strncmp(++suffix,outtype,(strlen(outtype)))==0)
				{
					/* limited characters allowed after suffix: ; or \0 */
					/* otherwise the match is not exact */
					if ( (*(suffix+strlen(outtype))=='\0') ||
						  (*(suffix+strlen(outtype))==';') )
					{
					/* matches suffix for file type, so put onto list */
					flist = uu_lsinsrt(flist,UX_MAX_PATH_LEN);
					uu_move_byte(dirbuf->d_name, flist, UX_MAX_PATH_LEN);
					}
				}
		}
	}
	closedir(dirptr);
#endif
#endif

#else /*UU_WIN2K*/
	status = uwx_get_flist(pathname, farea_flag, outtype, &flist);
#endif /* !UU_WIN2K */
	if (flist == *listptr)
	{
		/* flist was never incremented for list insertions */
		uu_lsdel(*listptr);		/* delete empty list */
		uu_toolmalloc_term();
		*listptr = UU_NULL;
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
/*****************************************************************
**	E_FUNCITON: ux_nxt_file(listptr, filename, options)
**			Attempts to return the name of the next file in the list
**			of file names associated with "listptr".
**		PARAMETERS:
**			"listptr": pointer to a file name list descriptor that
**				was filled in by the function, "ux_get_flist".
**				Initially pointer to head of the list, it returns
** 			on output the pointer to next item on list.
**			"filename": returns with the name to the next file 
**				in the list.
**			"options": One of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					If incorrectly specified we will print errors.
**		RETURNS:
**			UU_SUCCESS if a file name is returned; otherwise, one 
**			of the following values will be returned:
**				UX_EOL: end of list found (this is not an error);
**				UX_FAILURE: something else went wrong (an error).
**		WARNINGS:
**			Upon exhausting the list or when finished with it,
**			the application must delete it with a call to "uu_lsdel".
****************************************************************/
int ux_nxt_file(listptr, filename, options)
	char **listptr;
	char *filename;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	char *uu_lsnext();

	uu_denter(UU_XTRC,(us,"ux_nxt_file(*listptr:%x, ??, options:%d)",
		*listptr, options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */
	
	if (*listptr == UU_NULL)	 /* no list was made */
		goto failed;

	if (((*listptr) = uu_lsnext(*listptr)) == UU_NULL)	 /* at end of list */
		status = UX_EOL;
	else
		strcpy(filename,*listptr);
	goto done;

failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}	
/****************************************************************
**	E_FUNCTION: ux_mk_chk_syspath(path_prefix, path, filename, fenv, ftype,
**								modeptr,totalpath, options)
**			Attempts to a) construct and return a syntactically 
**			correct system dependent path name from the path pieces:
**			"path_prefix", "path", "filename"; 
**			b) locate the file and return an indication as to which
**			access modes requested of the file desired are valid.
**			Note, we assume each piece of the path is syntactically
**			correct if it is a system dependent path piece.
**		PARAMETERS:
**			"path_prefix": If this is UU_NULL it is ignored. If not 
**				UU_NULL then this can be either a system dependent
**				path name, or, a UNICAD path name containing 
**				environmental variables, or, "." (indicating 
**				the current file area). In any case, this must 
**				indicate a file area (not a specific
**				file). From this parameter the corresponding system 
**				dependent path name is obtained and prepended to the 
**				totalpath, "totalpath", to be returned. Note, if this 
**				parameter corresponds to more than one path name then 
**				only the first path name will be used.
**			"path": may be one of the following:
**				UU_NULL		it is ignored
**				. 	 (is translated to be the current working directory)
**				^ followed immediately by UNICAD pathname that may contain
**					symbols (environmental variables) (the ^requests expansion)
**				system dependent path name (not quoted) which will be treated
**					as a literal path name but a file area extension will be
**					appended if possible
**				quoted system dependent path name used very literally and with
**					no extensions appended
**				In any case, this	must be the portion of the full path
**				following any portion originating from "path_prefix".
**				In addition, as above, if this parameter corresponds 
**				to more than one path name then only the first path 
**				name will be used. If the value of this parameter is a
**				quoted system path name, then "path_prefix" is ignored. 
**				Finally, note that the resulting path name created from 
**				"path_prefix" and "path" should be a file area.
**			"filename": If this is UU_NULL then it is ignored. If not
**				then (modullo any file type extension) this is the 
**				final portion of the path name to be constructed. This
**				may be a quoted system dependent path name, which is used
**				very literally and no type extension will be added, or,
**				^ followed immediately by a UNICAD path name containing 
**				environmental variables, to be expanded, or, a system
**				filename that is not quoted and thus may have type extension
**				added to it if possible. In any case, once any file type 
**				extension is added (see "fenv" below), the result of 
**				"path_prefix", "path" and "filename" must be a specific 
**				file (not a file area). If the value of this parameter is 
**				a quoted path name, then the values in "path_prefix" and 
**				"path" are not used.
**			"fenv": If UU_NULL then no filearea extensions will be added to 
**				the path pieces above. If not UU_NULL, then this 
**				is assumed to be the environmental variable that allows 
**				access to the file type data used in constructing 
**				"fullpath". The following rules apply:
**				Once the path name from the above pieces of the path
**				is created, 
**					If "fenv" is UU_NULL then no filearea extensions are 
**					appended to the last file area.
**					If "filename" is not UU_NULL:
**						If "ftype" is UU_NULL then a file type is
**						appended only if "fenv" is not UU_NULL. Then "fenv" is
**						used to derive the file type extension tro append.
**			"ftype": Symbolic or quoted file type suffix to be used. May
**						be UU_NULL, in which case file type extensions are 
**						determined by the "fenv" variable.
**			"modeptr": 
**				input: Pointer to the bitwise "OR" of the following values: 
**					the pointer "modeptr" may be UU_NULL if no access
**						checking is desired
**					UX_CREATE: can the file designated by "pathname"
**						be created; if the file can be created we 
**						assume it can be read and written;
**					UX_EXISTS: does the designated file already exist;
**					UX_READ: can the file be read;
**					UX_WRITE: can the file be written to;
**					UX_EXECUTE: can the file be executed;
**					UX_DELETE: can the file be deleted;
**				output:
**					One of the following values:
**						UX_NEXIST: If "pathname" corresponds to a file 
**							(not a file area) and the path to the last file 
**							area in "pathname" exists and can be searched 
**							but "pathname" does not exist.
**							If "pathname" designates a file area (not a file)
**							and its parent file area exists and can be 
**							searched but "pathname" does not exist. 
**						UX_NEXIST | UX_CREATE: same as above but the file
**							(area) maybe created. This assumes UX_CREATE was
**							specified as input in "modeptr".
**						UX_FAREA: "pathname" is a file area (that exists).
**						UX_LOCKED: the file is locked.
**						A bitwise "OR" of the above input mode values that 
**						are satisfied, plus, 
**						if the file is currently open
**						then the type of open is bitwise "OR"ed in:
**							UX_R: open for reading;
**							UX_W: open for writing;
**							UX_A: open for appending.
**			"totalpath":	Output parameter returning the totalpath
**					requested; this may be an empty string if all the 
**					above parameters are UU_NULL, or it may be junk if
**					a syntactically correct path can not be created.
**			"options": A bitwise "OR" of one choice in each of the
**				following categories:
**				1.	UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**				2. UX_QUOTES: any output system dependent path is returned 
**						with UX_QUOTE_CHAR surrounding it.
**					UX_NQUOTES: any output system dependent path name is returned
**						with UX_QUOTE_CHAR surrounding it.
**					Incorrectly specified is the same as UX_PRTERRS | UX_QUOTES.
**		RETURNS:
**			UU_SUCCESS is returned if no software failures noticed;
**			otherwise, one of the following values is returned:
**				UX_BAD_SUBJECT: "totalpath" can't be constructed;
**				UX_BAD_ENV: environmental variable not found, or is of
**					the wrong type;
**				UX_NO_ACCESS: don't have access to determine the
**					the status of "totalpath";
**				UX_FAILURE: something else went wrong.
****************************************************************/
int ux_mk_chk_syspath(path_prefix, path, filename, fenv,  ftype, modeptr,
		totalpath, options)
	char *path_prefix;
	char *path;
	char *filename;
	char *fenv;
	char *ftype;
	int *modeptr;
	UX_pathname totalpath;
	int options;
{
	UX_pathname name;
	UX_pathname fullpath;
	UX_pathname temppath;
	int status;
	UU_LOGICAL printit;
	char *pathlist = UU_NULL;
	char *uu_lsdel();
	UU_LOGICAL found;
	int modes;

	uu_denter(UU_XTRC,(us,
	"ux_mk_chk_syspath(pre:%x,path:%x,file:%x,fenv:%x,ftype:%x,modeptr:%x,?,op:%d)",
		path_prefix, path, filename, fenv, ftype, modeptr, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (path_prefix!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"path_prefix:%s", path_prefix));
		if (path!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"path:%s", path));
		if (filename!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"filename:%s", filename));
		if (fenv!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"fenv:%s", fenv));
		if (ftype!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"ftype:%s", ftype));
		if (modeptr!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"mode:%o", *modeptr));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	strcpy(fullpath,"");

	if (path_prefix != UU_NULL)
	{
		/* get the list of sys. dep. paths from the string path_prefix */
		ux_get_syspath(path_prefix,&pathlist,fullpath,&found,options);
		uu_lsdel(pathlist);			/* remove list after done with it */
		if ( (path == UU_NULL) && (fenv != UU_NULL) )
			if (ux_add_farea_exten(fenv,fullpath, options) == UX_FAILURE)
				goto failed;
	}
	/* fullpath may be quoted or not according to options */

	if (path != UU_NULL)
	{
		strcpy(name,path);
		if ( ux_is_opsys_dep(name,options) == UU_SUCCESS) 
			strcpy(fullpath,name);
			/* note that we throw out path_prefix, strcpy not strcat */
		else if ( *path == UX_EXPAND_CHAR )
		{
			strcpy(name,path+1);		/* make a copy without the special character */
			if (path_prefix == UU_NULL)
			{
				pathlist = UU_NULL;		/* reset variable */
				if (ux_get_syspath(name,&pathlist,fullpath,&found,options)
					!= UU_SUCCESS) 
					goto failed;
				uu_lsdel(pathlist);
			}
			else
				if (ux_append_path(fullpath, name, options) == UX_FAILURE)
					goto failed;
			if (fenv != UU_NULL)
				if (ux_add_farea_exten(fenv,fullpath, options) == UX_FAILURE)
					goto failed;
			/* fullpath according to options */
		}
		else		/* case of literal, not symbols, but append extensions */
		{
			/* if fullpath is made with options = NOQUOTES, don't quote */
			if (options == (options | UX_NQUOTES))                        
				strcpy(temppath,path);
			else
			{
				strcpy(temppath,UX_QUOTE_STR);
				strcat(temppath,path);
				strcat(temppath,UX_QUOTE_STR);
			}
			if (fenv != UU_NULL)
				if (ux_add_farea_exten(fenv,temppath, options) == UX_FAILURE)
					goto failed;
			if (ux_cat_paths(fullpath,temppath,fullpath,(options|UX_NCHK))
				!= UU_SUCCESS) goto failed;
		}
	}

	if (filename != UU_NULL)
	{
		strcpy(name,filename);
		if ( ux_is_opsys_dep(name,options) == UU_SUCCESS) 
			strcpy(fullpath,name);
			/* note we don't use path or path_prefix or suffixes */
		else if ( *filename == UX_EXPAND_CHAR)
		{
			strcpy(name, filename+1);	/* a copy without the special character */
			if ( (path == UU_NULL) && (path_prefix == UU_NULL) )
			{
				pathlist = UU_NULL;		/* reset variable */
				ux_get_syspath(name,&pathlist,fullpath,&found,options);
				uu_lsdel(pathlist);
			}
			else
			{
				if (ux_is_farea(fullpath,(options|UX_NCHK)) == UX_NFAREA)
				{
					status = UX_BAD_SUBJECT;
					uu_dprint(UU_XTRC,(us,"status is UX_BAD_SUBJECT"));
					goto done;
				}
				if (ux_append_path(fullpath,name,options) == UX_FAILURE)
					goto failed;
			}
			if (ftype != UU_NULL )
				/* special case: fenv not null so try to get first ftype
				from it (uxi_get_suffixes) */

				if (ux_add_ftype(ftype, fullpath, options) == UX_FAILURE)
					goto failed;
		/* fullpath according to options */
		}
		else		/* case of literal but add filetype extensions and append
			to "fullpath" */
		{
			if (options == (options | UX_NQUOTES))                        
				strcpy(temppath,filename);
			else
			{
				strcpy(temppath,UX_QUOTE_STR);
				strcat(temppath,filename);
				strcat(temppath,UX_QUOTE_STR);
			}
			if (ftype != UU_NULL )
				if (ux_add_ftype(ftype, temppath, options) == UX_FAILURE)
					goto failed;
			if (ux_cat_paths(fullpath,temppath,fullpath,(options|UX_NCHK))
				!= UU_SUCCESS) goto failed;
		}
	}

	/* move the temporary resulting full name into the fullname to return: */
	strcpy(totalpath,fullpath);

	if (modeptr != UU_NULL)
	{
		modes = *modeptr;
		if ( (status = ux_access1(fullpath, &modes, options|UX_NCHK)) ==
		UX_FAILURE )
				goto failed;
		*modeptr = modes;
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
/****************************************************************
**	E_FUNCTION: ux_file_inquire(path_prefix, path, filename, fenv, ftype,
**							modeptr, statptr,	fullpath, options)
**			Attempts to a) construct and return a syntactically 
**			correct system dependent path name from the path pieces:
**			"path_prefix", "path", "filename"; 
**			b) locate the file and return aa status as to access modes
**			and header compatibility.
**		PARAMETERS:
**			"path_prefix": If this is UU_NULL it is ignored, else
**				this can be either a system dependent path name or a 
**				UNICAD path name containing environmental variables or "."
**				(the current file area). In any case, this must be a file 
**				area (not a file). From this parameter the corresponding system 
**				dependent path name is obtained and prepended to the 
**				fullpath, "fullpath", to be returned. Note, if this 
**				parameter corresponds to more than one path name then 
**				only the first path name will be used.
**			"path": may be one of the following:
**				UU_NULL		it is ignored
**				. 	 (is translated to be the current working directory)
**				^ followed immediately by UNICAD pathname that may contain
**					symbols (environmental variables) (the ^requests expansion)
**				system dependent path name (not quoted) which will be treated
**					as a literal path name but a file area extension will be
**					appended if possible
**				quoted system dependent path name used very literally and with
**					no extensions appended
**				In any case, this	must be the portion of the full path
**				following any portion originating from "path_prefix".
**				Note, a file area extension may be appended to this portion 
**				of the path being created, see comment on "fenv" below. 
**				In addition, as above, if this parameter corresponds 
**				to more than one path name then only the first path 
**				name will be used. If the value of this parameter has
**				is a quoted path name, then "path_prefix" is ignored. 
**				Finally, note that the resulting path name created from 
**				"path_prefix" and "path" should be a file area.
**			"filename": If this is UU_NULL then it is ignored. If not
**				then (modullo any file type extension) this is the 
**				final portion of the path name to be constructed. This
**				may be a quoted system dependent path name, which is used
**				very literally and no type extension will be added, or,
**				^ followed immediately by a UNICAD path name containing 
**				environmental variables, to be expanded, or, a system
**				filename that is not quoted and thus may have type extension
**				added to it if possible. In any case, once any file type 
**				extension is added (see 
**				"fenv" below), the result of "path_prefix", "path" and
**				"filename" must be a specific file (not a file area).
**				If the value of this parameter is a quoted path name, then
**				the values in "path_prefix" and "path" are not used.
**			"fenv": If UU_NULL then no filearea extensions will be added to 
**				the path pieces above. If not UU_NULL, then this 
**				is assumed to be the environmental variable that allows 
**				access to the file type data used in constructing 
**				"fullpath". The following rules apply:
**				Once the path name from the above pieces of the path
**				is created, 
**					If "fenv" is UU_NULL then no filearea extensions are 
**					appended to the last file area.
**					If "filename" is not UU_NULL:
**						If "ftype" is UU_NULL then a file type is
**						appended only if "fenv" is not UU_NULL. Then "fenv" is
**						used to derive the file type extension tro append.
**			"ftype": Symbolic or quoted file type suffix to be used. May
**						be UU_NULL, in which case file type extensions are 
**						determined by the "fenv" variable.
**			"modeptr": 
**				input: A bitwise "OR" of the following values:
**					UX_CREATE: can the file designated by "pathname"
**						be created; if the file can be created we 
**						assume it can be read and written;
**					UX_EXISTS: does the designated file already exist;
**					UX_READ: can the file be read;
**					UX_WRITE: can the file be written to;
**					UX_EXECUTE: can the file be executed;
**					UX_DELETE: can the file be deleted;
**				output:
**					One of the following values:
**						UX_NEXIST: If "pathname" corresponds to a file 
**							(not a file area) and the path to the last file 
**							area in "pathname" exists and can be searched 
**							but "pathname" does not exist.
**							If "pathname" designates a file area (not a file)
**							and its parent file area exists and can be 
**							searched but "pathname" does not exist. 
**						UX_NEXIST | UX_CREATE: same as above but the file
**							(area) maybe created. This assumes UX_CREATE was
**							specified as input in "modeptr".
**						UX_FAREA: "pathname" is a file area (that exists).
**						UX_LOCKED: the file is locked.
**						A bitwise "OR" of the above input mode values that 
**						are satisfied, plus, 
**						if the file is currently open
**						then the type of open is bitwise "OR"ed in:
**							UX_R: open for reading;
**							UX_W: open for writing;
**							UX_A: open for appending.
**			"statptr": Output parameter returning the status of the file
**							wrt Unicad file header "UX_FOUND" or "UX_NFOUND"
**			"fullpath":	Output parameter returning the fullpath
**					requested; this may be an empty string if all the 
**					above parameters are UU_NULL, or it may be junk if
**					a syntactically correct path can not be created.
**			"options": A bitwise "OR" of one choice in each of the
**				following categories:
**				1.	UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**				2. UX_QUOTES: any output system dependent path is returned 
**						with UX_QUOTE_CHAR surrounding it.
**					UX_NQUOTES: any output system dependent path name is returned
**						with UX_QUOTE_CHAR surrounding it.
**					Incorrectly specified is the same as UX_PRTERRS | UX_QUOTES.
**		RETURNS:
**			UU_SUCCESS is returned if no software failures noticed;
**			otherwise, one of the following values is returned:
**				UX_BAD_SUBJECT: "fullpath" can't be constructed;
**				UX_BAD_ENV: environmental variable not found, or is of
**					the wrong type;
**				UX_NO_ACCESS: don't have access to determine the
**					the status of "fullpath";
**				UX_FAILURE: something else went wrong.
****************************************************************/
int ux_file_inquire(path_prefix, path, filename, fenv, ftype, modeptr,
						statptr, fullpath, options)
	char *path_prefix;
	char *path;
	char *filename;
	char *fenv;
	char *ftype;
	int *modeptr;
	int *statptr;
	UX_pathname fullpath;
	int options;
{
	UX_pathname path_prefix2, dir, ext_dir;
	char *str;
	int status;
	int mode;
	UX_fheader hdrptr;
	int i,k;
	UU_LOGICAL printit;

	uu_denter(UU_XTRC,(us,
	"ux_file_inquire(prefix:%x,path:%x,filename:%x,fenv:%x,ftype:%x,?,ops:%d)",
	path_prefix, path, filename, fenv, ftype, options));

#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (path_prefix!=UU_NULL)
			uu_dprint(UU_XTRC,(us,"path_prefix:%s", path_prefix));
		if (path!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"path:%s", path));
		if (filename!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"filename:%s", filename));
		if (fenv!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"fenv:%s", fenv));
		if (ftype!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"ftype:%s", ftype));
	}
#endif
	printit = (options != (options|UX_NPRTERRS)); /* default=print errors */

	*statptr = UX_NFOUND;
	mode = *modeptr;
/*
.....the path_prefix allowed be a "format of dir1;dir2;dir3;..."
.....so we get directory one by one
*/
	if ((path_prefix==NULL)||(path_prefix!=NULL)&&(path_prefix[0]=='\0'))
		return ux_file_inquire2(path_prefix, path, filename, fenv, ftype,
							modeptr, statptr,	fullpath, options);
	strcpy(dir, path_prefix);
	if (ul_is_mult_dir_support(dir, ext_dir)==0)
	{
/*
.....not support multi-path
*/
		return ux_file_inquire2(path_prefix, path, filename, fenv, ftype,
							modeptr, statptr,	fullpath, options);
	}
	strcpy(path_prefix2, ext_dir);
/*
.....the ext_dir can be a "format of dir1;dir2;dir3;..."
.....so we get directory one by one
*/
	str = strtok (path_prefix2, ";");
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
		mode = *modeptr;
		status = ux_file_inquire2(dir, path, filename, fenv, ftype,
							modeptr, statptr,	fullpath, options);
		if (!(status != UU_SUCCESS || mode == (mode|UX_NEXISTS)))
			return UU_SUCCESS;
		str = strtok(NULL, ";");
	}
	return UX_FAILURE;
}
/****************************************************************
**	E_FUNCTION: ux_file_inquire2(path_prefix, path, filename, fenv, ftype,
**							modeptr, statptr,	fullpath, options)
**			Attempts to a) construct and return a syntactically 
**			correct system dependent path name from the path pieces:
**			"path_prefix", "path", "filename"; 
**			b) locate the file and return aa status as to access modes
**			and header compatibility.
**		PARAMETERS:
**			"path_prefix": If this is UU_NULL it is ignored, else
**				this can be either a system dependent path name or a 
**				UNICAD path name containing environmental variables or "."
**				(the current file area). In any case, this must be a file 
**				area (not a file). From this parameter the corresponding system 
**				dependent path name is obtained and prepended to the 
**				fullpath, "fullpath", to be returned. Note, if this 
**				parameter corresponds to more than one path name then 
**				only the first path name will be used.
**			"path": may be one of the following:
**				UU_NULL		it is ignored
**				. 	 (is translated to be the current working directory)
**				^ followed immediately by UNICAD pathname that may contain
**					symbols (environmental variables) (the ^requests expansion)
**				system dependent path name (not quoted) which will be treated
**					as a literal path name but a file area extension will be
**					appended if possible
**				quoted system dependent path name used very literally and with
**					no extensions appended
**				In any case, this	must be the portion of the full path
**				following any portion originating from "path_prefix".
**				Note, a file area extension may be appended to this portion 
**				of the path being created, see comment on "fenv" below. 
**				In addition, as above, if this parameter corresponds 
**				to more than one path name then only the first path 
**				name will be used. If the value of this parameter has
**				is a quoted path name, then "path_prefix" is ignored. 
**				Finally, note that the resulting path name created from 
**				"path_prefix" and "path" should be a file area.
**			"filename": If this is UU_NULL then it is ignored. If not
**				then (modullo any file type extension) this is the 
**				final portion of the path name to be constructed. This
**				may be a quoted system dependent path name, which is used
**				very literally and no type extension will be added, or,
**				^ followed immediately by a UNICAD path name containing 
**				environmental variables, to be expanded, or, a system
**				filename that is not quoted and thus may have type extension
**				added to it if possible. In any case, once any file type 
**				extension is added (see 
**				"fenv" below), the result of "path_prefix", "path" and
**				"filename" must be a specific file (not a file area).
**				If the value of this parameter is a quoted path name, then
**				the values in "path_prefix" and "path" are not used.
**			"fenv": If UU_NULL then no filearea extensions will be added to 
**				the path pieces above. If not UU_NULL, then this 
**				is assumed to be the environmental variable that allows 
**				access to the file type data used in constructing 
**				"fullpath". The following rules apply:
**				Once the path name from the above pieces of the path
**				is created, 
**					If "fenv" is UU_NULL then no filearea extensions are 
**					appended to the last file area.
**					If "filename" is not UU_NULL:
**						If "ftype" is UU_NULL then a file type is
**						appended only if "fenv" is not UU_NULL. Then "fenv" is
**						used to derive the file type extension tro append.
**			"ftype": Symbolic or quoted file type suffix to be used. May
**						be UU_NULL, in which case file type extensions are 
**						determined by the "fenv" variable.
**			"modeptr": 
**				input: A bitwise "OR" of the following values:
**					UX_CREATE: can the file designated by "pathname"
**						be created; if the file can be created we 
**						assume it can be read and written;
**					UX_EXISTS: does the designated file already exist;
**					UX_READ: can the file be read;
**					UX_WRITE: can the file be written to;
**					UX_EXECUTE: can the file be executed;
**					UX_DELETE: can the file be deleted;
**				output:
**					One of the following values:
**						UX_NEXIST: If "pathname" corresponds to a file 
**							(not a file area) and the path to the last file 
**							area in "pathname" exists and can be searched 
**							but "pathname" does not exist.
**							If "pathname" designates a file area (not a file)
**							and its parent file area exists and can be 
**							searched but "pathname" does not exist. 
**						UX_NEXIST | UX_CREATE: same as above but the file
**							(area) maybe created. This assumes UX_CREATE was
**							specified as input in "modeptr".
**						UX_FAREA: "pathname" is a file area (that exists).
**						UX_LOCKED: the file is locked.
**						A bitwise "OR" of the above input mode values that 
**						are satisfied, plus, 
**						if the file is currently open
**						then the type of open is bitwise "OR"ed in:
**							UX_R: open for reading;
**							UX_W: open for writing;
**							UX_A: open for appending.
**			"statptr": Output parameter returning the status of the file
**							wrt Unicad file header "UX_FOUND" or "UX_NFOUND"
**			"fullpath":	Output parameter returning the fullpath
**					requested; this may be an empty string if all the 
**					above parameters are UU_NULL, or it may be junk if
**					a syntactically correct path can not be created.
**			"options": A bitwise "OR" of one choice in each of the
**				following categories:
**				1.	UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**				2. UX_QUOTES: any output system dependent path is returned 
**						with UX_QUOTE_CHAR surrounding it.
**					UX_NQUOTES: any output system dependent path name is returned
**						with UX_QUOTE_CHAR surrounding it.
**					Incorrectly specified is the same as UX_PRTERRS | UX_QUOTES.
**		RETURNS:
**			UU_SUCCESS is returned if no software failures noticed;
**			otherwise, one of the following values is returned:
**				UX_BAD_SUBJECT: "fullpath" can't be constructed;
**				UX_BAD_ENV: environmental variable not found, or is of
**					the wrong type;
**				UX_NO_ACCESS: don't have access to determine the
**					the status of "fullpath";
**				UX_FAILURE: something else went wrong.
****************************************************************/
int ux_file_inquire2(path_prefix, path, filename, fenv, ftype, modeptr,
						statptr, fullpath, options)
	char *path_prefix;
	char *path;
	char *filename;
	char *fenv;
	char *ftype;
	int *modeptr;
	int *statptr;
	UX_pathname fullpath;
	int options;
{
	UX_pathname noquote;
	char *lq, *fq;
	char *index();
	char *rindex();
	int status;
	int mode;
	int gstatus;
	UX_fheader hdrptr;
	int i;
	UU_LOGICAL printit;

	uu_denter(UU_XTRC,(us,
	"ux_file_inquire(prefix:%x,path:%x,filename:%x,fenv:%x,ftype:%x,?,ops:%d)",
	path_prefix, path, filename, fenv, ftype, options));

#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (path_prefix!=UU_NULL)
			uu_dprint(UU_XTRC,(us,"path_prefix:%s", path_prefix));
		if (path!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"path:%s", path));
		if (filename!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"filename:%s", filename));
		if (fenv!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"fenv:%s", fenv));
		if (ftype!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"ftype:%s", ftype));
	}
#endif
	printit = (options != (options|UX_NPRTERRS)); /* default=print errors */

	*statptr = UX_NFOUND;
	mode = *modeptr;
	status = ux_mk_chk_syspath(path_prefix, path, filename, fenv, ftype, &mode,
		fullpath, options);
	*modeptr = mode;
	if (status != UU_SUCCESS)
		goto done;
	if (status == UX_FAILURE)
		goto failed;

	if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
	{
		*statptr = UX_NFOUND;
		goto done;
	}

	/* remove quotes to get working pathname: */
	fq = index(fullpath,UX_QUOTE_CHAR);

	if (fq != UU_NULL)
	{
			strcpy(noquote,(fq+1));
			lq = rindex(noquote,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	else
		strcpy(noquote,fullpath);

	/* determine value of statptr, found Unicad file header or not */
	if (mode == (mode|UX_R))
	{
		for (i=0; i<otind; i++)
			/* maybe "is_open" instead and remove quotes */
			if (strcmp(otable[i].fname,noquote) == 0)
				break;
		ux_get_file_hdr(i,&hdrptr,&gstatus,options);
	}
	else		/* open file for reading header */
	{
		if (ux_open(fullpath,"r","STREAM","TEXT", &i, UX_PRTERRS) != UU_SUCCESS)
			goto failed;
		ux_get_file_hdr(i,&hdrptr,&gstatus,options);
		if (ux_close(i,UX_PRTERRS) == UX_FAILURE)
			goto failed;
	}
	*statptr = gstatus;

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}

/*******************************************************************
**	E_FUNCTION:	ux_get_os_filedesc(unifile, osfileptr, options)
**			Attempts to return the operating system file
**			descriptor associated with the UNICAD file table index.
**		PARAMTERS:
**			"unifile": UNICAD file open table index.
**			"osfileptr": returns pointing to the operating system 
**				file descriptor.
**			"options": One of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					If incorrectly specified we will print errors.a
**		RETURNS:
**			UU_SUCCESS is returned if the operating system file
**			descriptor is returned; otherwise:
**				UX_BAD_DES: bad UNICAD file table index;
**				UX_FAILURE:
****************************************************************/
int ux_get_os_filedesc(unifile, osfileptr, options)
	int unifile;
	FILE **osfileptr;
	int options;
{
	int status;
	UU_LOGICAL printit;

	uu_denter(UU_XTRC,(us,"ux_get_os_desc(unifile:%d,osfileptr:%x,options:%d)",
		unifile, osfileptr, options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	if ( (unifile >= otind ) || (otable[unifile].fileptr == UU_NULL) )
	{
		status = UX_BAD_DES;
			goto done;
	}
	else
		*osfileptr = otable[unifile].fileptr;
		
	goto done;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION : ux_get_fversion(fullpath, versionptr) 
**			This function returns the version number of the file specified
**			by "fullpath".
**    PARAMETERS   
**       INPUT  : 
**          fullpath			Full path name to the file whose version number
**									is to be obtained.
**       OUTPUT :  
**          versionptr		Pointer to the version number of the file 
**									specified by "fullpath".
**    RETURNS      : UU_SUCCESS if no problems encountered; UU_FAILURE
**							otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : No error messages printed here.
*********************************************************************/
int ux_get_fversion(fullpath, versionptr,options)
	char *fullpath;
	int *versionptr;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	char *rindex();
	char *index();
	char *lq, *fq;
	UX_pathname noquote;
	struct stat buf;

	uu_denter(UU_XTRC,(us,"ux_get_fversion(fullpath:%x,?,options:%d)",
		fullpath,options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (fullpath!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"fullpath:%s", fullpath));
	}
#endif

	status = UU_SUCCESS; /* assume success */
 	UX_CHK_PATHERRS(fullpath, printit, options); 

	/* remove quotes to get working pathname: */
	fq = index(fullpath,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(noquote,(fq+1));
			lq = rindex(noquote,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	else
		strcpy(noquote,fullpath);

	if (stat(noquote, &buf) != 0) /* there's a problem */
		goto failed;
	*versionptr = (int)buf.st_mtime;

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
**    E_FUNCTION : ux_save_ptr(unifile)
**                       Saves the current pointer position in the open file.
**    PARAMETERS   
**       INPUT  : 
**          "unifile": The integer value that is the file's index in the
**                  Unicad open file table
**       OUTPUT :  
**                  none
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_save_ptr(unifile)
    int unifile;
{
    int status;         /* return status; either UU_SUCCESS or UX_FAILURE */
    long       ftell();

    status = UU_SUCCESS; /* assume success */

    saver = ftell(otable[unifile].fileptr);

    uu_dexit;
    return(status);
}

void ux_get_saver (filptr)
  int *filptr;
{
 *filptr = saver;
}

/*********************************************************************
**    E_FUNCTION : ux_restore_ptr(unifile)
**                Restores the current pointer position in the open file.
**    PARAMETERS
**       INPUT  :
**          "unifile": The integer value that is the file's index in the
**                  Unicad open file table
**       OUTPUT :
**                  none
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_restore_ptr(unifile)
    int unifile;
{
    int status;         /* return status; either UU_SUCCESS or UX_FAILURE */
 
    status = UU_SUCCESS; /* assume success */
 
    fseek(otable[unifile].fileptr,saver,0);
 
    uu_dexit;
    return(status);
}

/*********************************************************************
**    E_FUNCTION : ux_set_ptr(unifile)
**               Set the unibase file pointer to the position previous
**               to the EOF record.
**    PARAMETERS
**       INPUT  :
**          "unifile": The integer value that is the file's index in the
**                  Unicad open file table
**       OUTPUT :
**                  none
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
void ux_set_ptr(unifile)
    int unifile;
{
    int status,stt;         /* return status; either UU_SUCCESS or UX_FAILURE */
    long ptr,ftell();
    status = UU_SUCCESS; /* assume success */

    for(stt = 0, ptr = ftell(otable[unifile].fileptr); !stt;  )
    {
       ptr = ftell(otable[unifile].fileptr);
       stt = ur_skip_ent(unifile);
    }

    fseek(otable[unifile].fileptr,ptr,0);
    ux_save_ptr(unifile);
}
    
/*********************************************************************
**    E_FUNCTION : ux_get_ver_index()
**               Find update index number based on input unibase file
**               version to update unibase data.
**    PARAMETERS
**       INPUT  :
**                  none
**       OUTPUT :
**                  none
**    RETURNS: update level.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/

int ux_get_ver_index()
{
	static UU_REAL VERSIONS[20] =
	{
		8.00, 8.103, 8.105, 8.201,  8.251, 8.34999, 8.4999, 9.049, 9.149, 9.249,
		9.349, 9.405, 9.50, 9.549, 9.649, 9.849, 9.949, 10.04999, 0., 0.
	}; 

	int i;
 
	i   = 0;
	while (NCL_infile_version > VERSIONS[i] && i < 20) i++;

	return(i);
}

/*********************************************************************
**    E_FUNCTION : ux_get_dct_index()
**               Find dictionary index number based on input unibase
**               file version to read old unibase data.
**    PARAMETERS
**       INPUT  :
**                  none
**       OUTPUT :
**                  none
**    RETURNS: dictionary index to use. 0 - current dictionary.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/

int ux_get_dct_index()
{
	static UU_REAL VERSIONS[20] =
	{
		7.999, 8.1029, 8.1049, 8.1999, 8.2029, 8.34999, 8.4999, 9.049, 9.149,
		9.249, 9.349,9.549,9.649 , 9.849, 9.949, 10.04999, 0., 0., 0., 0.
	};

	int i;

	i   = 0;
	while (NCL_infile_version > VERSIONS[i] && i < 20) i++;

	return(i==20? 0:i);
}

/*********************************************************************
**    E_FUNCTION :  ux_mk_pthdir(pathname)
**			Attempts to create a file area at the location
**			designated by "pathname". It wll create the whole path, but not
**			the top directory
**			pathname can't be envioment value
**			after this function, current path will be 'pathname'.
**		PARAMTERS:
**			pathname: full path name to be created.
**		RETURNS:
**			UU_SUCCESS is returned if the path is created;
**			otherwise one of the following values is returned:
**				(these are all considered errors)
**				UX_NO_ACCESS: don't have access to create file area;
**				UX_FOUND: "pathname" already exists;
**				UX_FAILURE: something else went wrong.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_mk_pthdir(pathname)
	char *pathname;
{
	int status,len;
	char *index();
	char *rindex();
	char *str;
	char pathstr[2];
	UX_pathname top_dir, tempstr, tmp_curdir;

	status = UU_SUCCESS;
	pathstr[0] = UX_PATH_SEP;
	pathstr[1] = '\0';
	strcpy (tempstr, pathname);
/*
.....first switch to top-directory of the path
*/
	str = index (tempstr, UX_PATH_SEP);
	if (str==NULL)
	{
/*
......NO PATH, JUST	make a directory
*/
		if (ux_access0(pathname, UX_EXISTS) == UU_SUCCESS)
		{
			goto done;
		}
#if UU_COMP==UU_WIN2K		
		if (mkdir(pathname, 755) != UU_SUCCESS)
#else
		if (mkdir(pathname, 777) != UU_SUCCESS)
		umask(0);
		if (mkdir(pathname, 
			S_IRWXU|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH) != UU_SUCCESS)
#endif
			goto failed;
		goto done;
	}
/*
.....we need keep the last UX_PATH_SEP for the top directory
.....for example, "cd c:\" will change to "c:" but "cd c:" will 
.....doing nothing if we already in c: directory, also if we need
.....change to "/" will need keep UX_PATH_SEP too
*/ 
/*	*str = '\0'; */
	len = str - tempstr + 1;
	strncpy(top_dir, tempstr, len);
	top_dir[len] = '\0';
	if (chdir (top_dir)!= UU_SUCCESS)
		goto failed;
	strcpy(tmp_curdir, top_dir);
/*
.....then mkdir the following directory
*/
make_dir:;
	if (str==NULL)
		goto done;
	strcpy (tempstr, str+1);
	str = index (tempstr, UX_PATH_SEP);
	if (str!=NULL)
		*str = '\0';
	strcpy(top_dir, tempstr);
	strcat(tmp_curdir, top_dir);
/*
.....check if we have this dir already
*/
	if (ux_access0(tmp_curdir, UX_EXISTS) == UU_SUCCESS)
	{
		if (chdir (tmp_curdir)!= UU_SUCCESS)
			goto failed;
		strcat (tmp_curdir, pathstr);
		goto make_dir;
	}
#if UU_COMP==UU_WIN2K		
	if (mkdir(top_dir, 755) != UU_SUCCESS)
#else
	umask(0);
	if (mkdir(top_dir, 
		S_IRWXU|S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH) != UU_SUCCESS)
#endif
		goto failed;
	if (chdir (top_dir)!= UU_SUCCESS)
		goto failed;
	strcat (tmp_curdir, pathstr);
	goto make_dir;
failed: status = UX_FAILURE;
done:;
	return(status);
}	
