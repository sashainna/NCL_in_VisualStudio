/********************************************************************
**    NAME:  xf0udos0.c
**		Lowest udos level, depends only on the debug system. File xf0udos0.c
**		contains system level dependencies and has op. system or C library
**		calls  (Unicad standard c library) embedded in the functions. 
**       CONTAINS:
**				ux_dirtest0
**				ux_access0
**				ux_copy0
**				ux_rename0
**				ux_creat0
**				ux_delete0
**				ux_fclose0
**				ux_fopen0
**				ux_fdopen0
**				ux_freopen0
**				ux_fread0
**				ux_fwrite0
**				ux_fflush0
**				ux_fputc0
**				ux_putw0
**				ux_fgetc0
**				ux_getw0
**				ux_fgets1
**				ux_fgets0
**				ux_fputs0
**				ux_ungetc0
**				ux_fseek0
**				ux_ftell0
**				ux_lseek0
**				ux_vaxdir0
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			xf0udos0.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:32
*********************************************************************/

#include <stdio.h>
#include "usysdef.h"			/* for UU_SUCCESS */
#include "udebug.h"			/* for UU_DEBUGON */

/*#ifdef WNT */
#if (UU_COMP == UU_WIN2K)
#include <io.h>
#include <fcntl.h>
#include <share.h>
#include <sys/types.h>
#define access _access
#define close _close
#define creat _creat
#define fdopen _fdopen
#define getw _getw
#define lseek _lseek
#define open _open
#define putw _putw
#define read _read
#define unlink _unlink
#define write _write
#endif

#if UU_COMP == UU_VAXVMS
/*
.....ALPHA
*/
#include <descrip.h>		/* has macro for $DESCRIPTOR */
#include <ssdef.h>			/* defines system service return values */
#include <file.h>
#include <types.h>
#include <stat.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif
#define UX_F0PGM
#include "xfsys0.h"
#include "xfsys1.h"
#include "unserve.h"
#undef UX_F0PGM
#define TRACE UU_TRUE
/********************************************************************
**	NOTE EOF must be -1 and NULL must be 0 for the tests of success,
** errors, or end-of-file and such to work out okay!!
********************************************************************/
/********************************************************************
**    E_FUNCTION :  ux_dirtest0()
**		Routine for testing the write access within the working directory
**			
**    PARAMETERS   
**       INPUT  : 
**    RETURNS:	UU_SUCCESS if write access within working directory
**		otherwise, UX_FAILURE.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_dirtest0()
{
	int status;
	struct stat stbuf;	/*	info about file obtained from stat()	*/
#if UU_COMP == UU_VAXVMS
	char *getenv();
#else 
#if UU_COMP == UU_RIDGE
	char *getcwd();
#else
	char *getcwd();
/*	char *getwd();*/
#endif
#endif
	UX_pathname dirname;
	char *rindex();
	char *chrs;

	uu_denter(UU_XTRC, (us, "ux_dirtest0()"));
	status = UU_SUCCESS; /* assume success */

#if UU_COMP == UU_VAXVMS
	chrs = getenv("PATH");
	strcpy(dirname,chrs);
#else
#if UU_COMP == UU_RIDGE
	chrs = getcwd(dirname,UX_MAX_PATH_LEN);
	strcpy(dirname,chrs);
#else
	chrs = getcwd(dirname,UX_MAX_PATH_LEN);
	strcpy(dirname,chrs);
/*	getwd(dirname);*/
#endif
#endif

#if UU_COMP == UU_VAXVMS
	/* if VMS file spec is [x], convert the formatto ".dir" type */
	if ((right = rindex(dirname,']')) != UU_NULL)
		if ( *(right+1) == '\0')
		{
			/* need to reformat dir spec on VMS */
			if (ux_vaxdir0(dirname, tempname) != UU_SUCCESS)
				goto failed;
			strcpy(dirname, tempname);
		}
#endif

if  (stat(dirname, &stbuf) != 0)
	goto failed;							/* stat call failed */
/*
.....WinNT
*/
#if (UU_COMP == UU_WINNT) 
if ((stbuf.st_mode & S_IWUSR) == S_IWUSR)
	goto done;									/* write permission, owner*/
#else
#if (UU_COMP == UU_WIN2K)
if ((stbuf.st_mode & S_IWRITE) == S_IWRITE)
	goto done;
#else
if ((stbuf.st_mode & S_IWRITE) == S_IWRITE)
	goto done;									/* write permission, owner*/
#endif
#endif
else
{
	printf("Not successful for write access on .\n");
	goto failed;
}

	/* goto done; */
failed: status = UX_FAILURE;
#if (TRACE) 
		uu_denter2(UU_XTRC,(us, "No write access in the working directory."));
		uu_dexit;			
#endif
done:;
	uu_dexit;
	return(status);
}
/********************************************************************
**    E_FUNCTION :  ux_access0(filename, mode)
**		Checks the given file for accessibility according to "mode".
**			
**    PARAMETERS   
**       INPUT  : 
**				filename		Full system pathname to a file.
**				mode	Values: 4 (read), 2 (write), or 1 (execute) or a 
**						combination (sum). Specifying mode 0 tests whether
**						the directories leading to the file can be searched
**						and the file exists.
**    RETURNS:
**			UU_SUCCESS if tests are successful, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_access0(filename, mode)
	char *filename;
	int mode;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	char  *rindex();

	uu_denter(UU_XTRC, (us, "ux_access0(filename:%s,mode:%d)",filename,mode));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (filename!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"filename:%s", filename));
	}
#endif
	status = UU_SUCCESS; /* assume success */

	/* the VAX access fails on directory file specs if in [xxxx.yyy.zzz]
	format, so convert a copy of the name to [xxxx.yyyy]zzz.DIR format */
/* NCL
.....Changed the calls to access() on VAX/VMS to fopen's
.....Because it always returns the WORLD access protection.
.....Therefore, the user must have WORLD access and not just
.....OWNER access to the specified file.
.....6/23/88 Bob Schultz Jr.
*/
#if UU_COMP == UU_VAXVMS
	switch (mode)
	{
	case 2:
	case 3:
	case 6:
	case 7:
		strcpy (fmode,"r+");
		break;
	default:
		strcpy (fmode,"r");
		break;
	}
	left = rindex(filename,']');
	if (left != UU_NULL)
	{
		if ( *(left+1) == '\0') /* filename is a [] dir spec */
		{
/*
.....ALPHA only allows WRITE access to a directory
.....by its owner.
.....So let's try to open a dummy file in the directory.
.....Bobby  -  1/16/95
*/
#ifdef UU_ALPHA
			if (strcmp(fmode,"r+") == 0)
			{
				strcpy(dirname,filename);
				strcat(dirname,"nclwritq.tmp;0");
				fd = fopen(dirname,"w");
				if (fd == 0) goto failed;
				fclose(fd);
				remove(dirname);
			}
			else
#endif
			{
			/* convert the spec format */
				if (ux_vaxdir0(filename,dirname) != UU_SUCCESS)
					goto failed;		
/* NCL */
/*				if (access(dirname, mode) != 0)		* returns -1 error, 0 success */
/*				goto failed;*/
				fd = fopen (dirname,fmode);
				if (fd == 0) goto failed;
				fclose (fd);
			}
		}
		else
		{
/*			if (access(filename, mode) != 0)		 returns -1 error, 0 success */
/*				goto failed;*/
			fd = fopen (filename,fmode);
			if (fd == 0) goto failed;
			fclose (fd);
		}
	}
	else
	{
/*		if (access(filename, mode) != 0)		* returns -1 error, 0 success */
/*			goto failed;*/
		fd = fopen (filename,fmode);
		if (fd == 0) goto failed;
		fclose (fd);
	}
/* end NCL */
#else
/*
.....Fixed file access mode for windows VISTA
*/
#if UU_COMP == UU_WIN2K
	if ((mode & UX_FAREA) == UX_FAREA)
	{
		struct _stat fstat;
		_stat(filename,&fstat);
		if ((fstat.st_mode & S_IFDIR) != S_IFDIR) goto failed;
		mode = mode & ~(UX_FAREA);
	}
		
	if((mode & UX_EXECUTE) == UX_EXECUTE)
		mode = (mode & ~(UX_EXECUTE)) | UX_WRITE ;
	if((mode & UX_APPEND) == UX_APPEND)
		mode = ((mode & ~(UX_APPEND)) | UX_WRITE) | UX_READ;
	if((mode & UX_DELETE) == UX_DELETE)
		mode = (mode & ~(UX_DELETE)) | UX_WRITE ;
	if((mode & UX_CREATE) == UX_CREATE)
		mode = (mode & ~(UX_CREATE)) | UX_WRITE ;
#endif
	if (access(filename, mode) != 0)		/* returns -1 error, 0 success */
		goto failed;
#endif

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
		uu_denter2(UU_XTRC,(us, "No access under this mode."));
		uu_dexit;			
#endif
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION : ux_copy0(pathfrom, pathto)
**		Copies the "pathfrom" file to the "pathto" file, but "pathfrom"
**		can't be a directory.
**			
**    PARAMETERS   
**       INPUT  :
**				pathfrom: a sys. dep. file specification of the file to
**							be copied.
**				pathto: a correct file specification to be the copy.	
**    RETURNS: UU_SUCCESS if file is copied, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_copy0(pathfrom,pathto)
	char *pathfrom;
	char *pathto;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int fd1, fd2;
	int n;
	char buf[1];

#if UU_COMP == UU_VAXVMS
	$DESCRIPTOR (outdesc,copy_it);		/* string descriptor structure */
#endif

	uu_denter(UU_XTRC, (us, "ux_copy0(pathfrom:%x,pathto:%x)",pathfrom,pathto));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathfrom != UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathfrom:%s", pathfrom));
		if (pathto != UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathto:%s", pathto));
	}
#endif

	status = UU_SUCCESS; /* assume success */

#if UU_COMP == UU_VAXVMS
	sprintf(copy_it, "COPY %s %s", pathfrom, pathto);
	ioerr = lib$spawn(&outdesc);
	if (ioerr != SS$_NORMAL)
		goto failed;
#else
/* sprintf(copy_it,"cp %s %s",pathfrom,pathto); ioerr=system(copy_it); */
/* replaced the system call with low-level Unix code: */
	fd1 = open(pathfrom, 0);
	if (fd1 == -1)
		goto failed;
	fd2 = creat(pathto, 0755);
	if (fd2 == -1)
		goto failed;
	while ( (n=read(fd1,buf,1)) > 0)
		if (write(fd2,buf,n) != n)
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
**    E_FUNCTION : ux_rename0(pathname, newpname)
**			Renames the file "pathname" to "newpname".
**    PARAMETERS   
**       INPUT  : 
**				pathname:  a system dependent file specification of the file
**					to be renamed.
**				newpname: a system dependent file spec for new name of file.
**    RETURNS: UU_SUCCESS if file is renamed, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_rename0(pathname,newpname)
	char *pathname;
	char *newpname;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	
#if UU_COMP == UU_VAXVMS
	$DESCRIPTOR (outdesc,name_it);		/* string descriptor structure */
#endif

	uu_denter(UU_XTRC,(us,"ux_rename0(pathname:%x,newpname:%x)",
		pathname,newpname));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname != UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathname:%s", pathname));
		if (newpname != UU_NULL)
			uu_dprint(UU_XTRC,(us,"newpname:%s", newpname));
	}
#endif
	status = UU_SUCCESS; /* assume success */

#if UU_COMP == UU_VAXVMS
	sprintf(name_it, "REN %s %s", pathname, newpname);
	ioerr = lib$spawn(&outdesc);
	if (ioerr != SS$_NORMAL)
		goto failed;
#else
/*	sprintf(name_it,"mv %s %s",pathname,newpname); ioerr=system(name_it); */
/* replaced the system call with low-level Unix code: */
/*
	fd2 = creat(newpname, 0755);
	if (fd2 == -1)
		goto failed;
 */
	if (rename(pathname, newpname) != 0) goto failed;
/*
	fd1 = open(pathname, 0);
	if (fd1 == -1)
		goto failed;
	fd2 = creat(newpname, 0755);
	if (fd2 == -1)
		goto failed;
	while ( (n=read(fd1,buf,1)) > 0)
		if (write(fd2,buf,n) != n)
			goto failed;
	if (unlink(pathname)!=0)
	{
		unlink(newpname);
		goto failed;					* unlink returns 0 success, -1 error *
	}
*/
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
**    E_FUNCTION :  ux_creat0(filename, mode, filedescptr)
**		Creates a new file or prepares to rewrite an existing file
**		called "filename" given as an address of a null-terminated string.
**		If the file is created, it is given mode "mode".
**			
**    PARAMETERS   
**       INPUT  : 
**				filename	full path name to the file to be created
**				mode        Ignored.  'umask' value is now used to create file
**				            Bobby  -  9/23/97.
**       OUTPUT :  
**				filedescptr	pointer to the file descriptor to be returned
**				a non-negative integer if found.
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: 'mode' is now ignored.
*********************************************************************/
int ux_creat0(filename, mode, filedescptr)
	char *filename;
	int mode;
	int *filedescptr;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int imode;

	uu_denter(UU_XTRC, (us, "ux_creat0(filename:%x, mode:%o, filedescptr:%x)",
	filename,mode,filedescptr));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (filename != UU_NULL)
			uu_dprint(UU_XTRC,(us,"filename:%s", filename));
	}
#endif
	status = UU_SUCCESS; /* assume success */

/*
.....Get the current umask value
.....Bobby  -  9/23/97
*/
#if UU_COMP == UU_WIN2K
	_sopen_s(filedescptr,filename,_O_CREAT|_O_TRUNC,_SH_DENYNO,
		_S_IREAD|_S_IWRITE);
#else
	imode = umask(18);
	umask(imode);
	imode = 438 - imode;	/* 666 octal */
	*filedescptr = creat(filename, imode);
#endif
	if (*filedescptr == -1)
		goto failed;

	/* request next available open table index */
	otnext = uu_nserv_req(UU_OT_NM);
	if ( otnext >= otind)
		otind = otnext + 1;
	if ( otind > MAXT )
		ud_printmsg(" TOO MANY FILES OPEN AT ONCE--CLEANUP NEEDED.\n");
	strcpy(otable[otnext].fname,filename);
	otable[otnext].fdes = *filedescptr;
	strcpy(otable[otnext].rname,"ux_creat0");
	strcpy(otable[otnext].openfor,"w");
	strcpy(otable[otnext].format,"STREAM");
	strcpy(otable[otnext].interp,"TEXT");

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
**    E_FUNCTION : ux_delete0(filename) 
**						 Removes the named file.
**
**    PARAMETERS   
**       INPUT  : 
**				filename: system dependent file specification of file to remove.
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_delete0(filename)
	char *filename;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int i;

	uu_denter(UU_XTRC, (us, "ux_delete0(filename:%x)", filename));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (filename != UU_NULL)
			uu_dprint(UU_XTRC,(us,"filename:%s", filename));
	}
#endif
	status = UU_SUCCESS; /* assume success */

#if UU_COMP == UU_VAXVMS
	if (delete(filename) != 0 ) 
		goto failed;					/* delete returns 0 success, -1 error */
#else
	if (unlink(filename)!=0)
		goto failed;					/* unlink returns 0 success, -1 error */
#endif

/* successfull deletion then remove table entries: */
for (i=0; i<otind; i++)
	if (strcmp(otable[i].fname,filename) == 0)
		{
			strcpy(otable[i].fname,"");
			strcpy(otable[i].rname,"");
			otable[i].fileptr = UU_NULL;
			otable[i].fdes = 0;
			strcpy(otable[i].openfor,"");
			strcpy(otable[i].format,"");
			strcpy(otable[i].interp,"");
			/* leaves "gaps" in the table, so alter "otnext" */
			/* this i is available for the next index */
			uu_nserv_ret(UU_OT_NM, i);
			break;	
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
**    E_FUNCTION :  ux_fclose0(file_ptr)
**		Closes a file, flushing its buffers.
**			
**    PARAMETERS   
**       INPUT  : 
**				file_ptr		a file pointer returned by the open call
**    RETURNS: UU_SUCCESS if closed successfully, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_fclose0(file_ptr)
	FILE *file_ptr;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int i;

	uu_denter(UU_XTRC, (us, "ux_fclose0(file_ptr:%x)",file_ptr));
	status = UU_SUCCESS; /* assume success */

	if (fclose(file_ptr) == EOF )
		goto failed;

	for (i=0;i<otind;i++)
		if ((otable[i].fileptr) == file_ptr)
		{
			strcpy(otable[i].fname,"");
			otable[i].fileptr = UU_NULL;
			otable[i].fdes = 0;
			strcpy(otable[i].rname,"");
			strcpy(otable[i].openfor,"");
			strcpy(otable[i].format,"");
			strcpy(otable[i].interp,"");
			/* leaves "gaps" in the table, so alter "otnext" */
			/* this i is available for the next index */
			uu_nserv_ret(UU_OT_NM, i);
			break;
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
**    E_FUNCTION :  ux_fopen0(filename, type, fileptrptr)
**		Opens the file named "filename" and associates a stream with it.
**			
**    PARAMETERS   
**       INPUT  : 
**				filename		full path name to the file to be opened
**				type			r for reading, w for writing, a for appending
**								also r+ for update (read and write), w+ for
**								truncate or create for update, a+ for append.
**				fileptrptr		returns pointer to stream FILE structure found.
**    RETURNS: UU_SUCCESS if valid file desc. returned, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_fopen0(filename, type, fileptrptr)
	char *filename;
	char *type;
	FILE **fileptrptr;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int i;

	uu_denter(UU_XTRC, (us,"ux_fopen0(filename:%s, type:%s, fileptrptr:%x)",
								filename, type, fileptrptr));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (filename != UU_NULL)
			uu_dprint(UU_XTRC,(us,"filename:%s", filename));
	}
#endif
	status = UU_SUCCESS; /* assume success */

	*fileptrptr = fopen(filename, type);
	if (*fileptrptr == UU_NULL)
			goto failed;
	/* if okay, fill out entry in otable record at "otind" or overwrite previous
	open on the same file: */
	for (i=0;i<otind;i++)
		if (strcmp(otable[i].fname,filename)==0)
		{
			strcpy(otable[i].rname,"ux_fopen0");
			otable[i].fileptr = *fileptrptr;
			/* what about filedes integer ? */
			strcpy(otable[i].openfor,type);
			break;
		}	
	if (i>=otind)		/* not already created, open */
	{
		/* request next available open table index */
		otnext = uu_nserv_req(UU_OT_NM);
		if ( otnext >= otind )
			otind = otnext + 1;
		if ( otind > MAXT )
			ud_printmsg(" TOO MANY FILES OPEN AT ONCE--CLEANUP NEEDED.\n");
		strcpy(otable[otnext].rname,"ux_fopen0");
		strcpy(otable[otnext].fname,filename);
		otable[otnext].fdes = 0;
		otable[otnext].fileptr = *fileptrptr;
		strcpy(otable[otnext].openfor,type);
		strcpy(otable[otnext].format,"STREAM");
		strcpy(otable[otnext].interp,"TEXT");
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
**    E_FUNCTION :  ux_fdopen0(filedesc, type, fileptrptr)
**		Associates a stream with a file descriptor.
**			
**    PARAMETERS   
**			INPUT:
**				filedesc		integer  returned from "ux_creat0"
**				type			r for reading, w for writing, a for appending
**								also r+ for update (read and write), w+ for
**								truncate or create for update, a+ for append.
**       OUTPUT: 
**				fileptrptr		returns pointer to stream FILE structure found.
**    RETURNS: UU_SUCCESS if a vaild stream FILE structure is returned,
**					UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_fdopen0(filedesc, type, fileptrptr)
	int filedesc;
	char *type;
	FILE **fileptrptr;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int i;

	uu_denter(UU_XTRC, (us, "ux_fdopen0(filedesc:%d, type:%x, fileptrptr:%x)",
	filedesc, type, fileptrptr));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (type != UU_NULL)
			uu_dprint(UU_XTRC,(us,"type:%s", type));
	}
#endif
	status = UU_SUCCESS; /* assume success */

	*fileptrptr = fdopen(filedesc, type);
	if (*fileptrptr == UU_NULL)		/* fdopen returns zero for errors */
		goto failed;
	for (i=0;i<otind;i++)
		if (otable[i].fdes == filedesc)
		{
			otable[i].fileptr = *fileptrptr;
			strcpy(otable[i].rname,"ux_fdopen0");
			strcpy(otable[i].openfor,type);
			break;
		}	
	if (i>=otind)	/* file not already open by creat, etc. */
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
**    E_FUNCTION :  ux_freopen0(filename,type, fileptrptr)
**		Substitutes the named file in place of the open fileptrptr, closes
**		the original fileptrptr. 
**			
**    PARAMETERS   
**       INPUT  : 
**				filename		full path name to the file
**				type			r for reading, w for writing, a for appending
**								also r+ for update (read and write), w+ for
**								truncate or create for update, a+ for append.
**				fileptrptr		pointer to stream FILE structure to be redefined
**       OUTPUT: 
**				fileptrptr		returns pointer to stream FILE structure found.
**    RETURNS: UU_SUCCESS if a vaild stream FILE structure is returned,
**					UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_freopen0(filename,type, fileptrptr)
	char *filename, *type;
	FILE **fileptrptr;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int i;

	uu_denter(UU_XTRC, (us, "ux_freopen0(filename:%x,type:%s, fileptrptr:%x)",
	filename, type, fileptrptr));
	status = UU_SUCCESS; /* assume success */

	if ( freopen(filename, type, *fileptrptr) == UU_NULL )
		goto failed;

	for (i=0;i<otind;i++)
		if (strcmp(otable[i].fname,filename) == 0)
		{
			otable[i].fileptr = *fileptrptr;
			strcpy(otable[i].rname, "ux_freopen0");
			strcpy(otable[i].openfor,type);
			break;
		}
	if (i>=otind)	/* file not already open */
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
**    E_FUNCTION : ux_fwrite0(ptr, size, nitems, stream, nout) 
**		appends at most nitems of data of the type of *ptr beginning at
**		ptr to the named output stream. 
**
**    PARAMETERS   
**       INPUT:	ptr: buffer that conatins the data to be written to file
**						size: size of the data item
**						nitems: number of items of this size to write
**						stream: stream pointer to file to be written to
**       OUTPUT:	nout: number of items that were actually written
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**		NOTE that an error or end of file on the fwrite call returns
**		null = 0, not eof = -1 and nout, the return value of fwrite, is
**		the number of items written, maybe not the same as number requested.
*********************************************************************/
int ux_fwrite0(ptr, size, nitems, stream, nout)
	char *ptr;
	int size;
	int nitems, *nout;
	FILE *stream;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_fwrite0(ptr:%x, size:%d, nitems:%d, stream:%x)", ptr, size, nitems, stream));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (ptr != UU_NULL)
			uu_dprint(UU_XTRC,(us,"ptr:%s", ptr));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	*nout = fwrite(ptr, size, nitems, stream);
	fflush(stream);
	if ( (*nout==0) && (feof(stream)==0) )
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
**    E_FUNCTION : ux_fread0(ptr, size, nitems, stream, nread) 
**		Reads into a block beginning at ptr, nitems of data of the type 
**		of *ptr from the named input stream.
**			
**    PARAMETERS   
**       INPUT:	ptr: buffer that will receive the data read from file
**						size: size of the data item
**						nitems: number of items of this size to read
**						stream: stream pointer to file to be read
**       OUTPUT:	nout: number of items that were actually read
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**		NOTE that an error or end of file on the fread call returns
**		null = 0, not eof = -1 and nread, the return value of fread, is
**		the number of items read, maybe not the same as number requested.
*********************************************************************/
int ux_fread0(ptr, size, nitems, stream, nread)
	char *ptr;
	int size;
	int nitems, *nread;
	FILE *stream;
{
	int status,i;			/* return status; either UU_SUCCESS or UX_FAILURE */
	uu_denter(UU_XTRC, (us, "ux_fread0(ptr:%x, size:%d, nitems:%d, stream:%x)", ptr, size, nitems, stream));
	status = UU_SUCCESS; /* assume success */
	*nread = fread(ptr, size, nitems, stream);
	if ( (*nread == 0) && ( feof(stream) == 0) )
		goto failed;
	i = ferror(stream);

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
**    E_FUNCTION : ux_fflush0(stream) 
**		Causes any buffered data for stream to be written to that file.
**
**    PARAMETERS   
**       INPUT  : stream: file pointer returned by the open routines
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_fflush0(stream)
	FILE *stream;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_fflush0(stream:%x)", stream));
	status = UU_SUCCESS; /* assume success */

	if (fflush(stream)== EOF)
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
**    E_FUNCTION : ux_fseek0(stream, offset, ptrname)
**				Sets the position of the next I/O operation on the
**				stream.
**			
**    PARAMETERS   
**       INPUT  : 
**				stream: file pointer returned by the open routines
**				offset: signed distance bytes from the the beginning,
**						current position, or end of file, depending on
**						whether ptrname is 0, 1, 2.
**				ptrname:	can be 0,1,2 as above
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_fseek0(stream, offset, ptrname)
	FILE *stream;
	long offset;
	int ptrname;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_fseek0(stream:%x,offset:%d,ptrname:%d",stream,offset,ptrname));
	status = UU_SUCCESS; /* assume success */

	if ( fseek(stream, offset, ptrname) == EOF )
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
**    E_FUNCTION : ux_ftell0(stream,offset)
**				Returns the offset of the curren byte relative to the
**				beginning of the file associated with the named stream.
**			
**    PARAMETERS   
**       INPUT  : 
**						stream: file pointer as returns from open routines
**       OUTPUT: 
**						stream: file pointer as returns from open routines
**						offset: returns the current byte relative to the beginning
**								of the file associated with the named stream
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_ftell0(stream, offset)
	FILE *stream;
	long *offset;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	long ftell();

	uu_denter(UU_XTRC, (us, "ux_ftell0(stream:%x,offset:%d",stream,offset));
	status = UU_SUCCESS; /* assume success */

	*offset = ftell(stream);
	if ( *offset == EOF )		/* offset is returned or -1 if error */
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
**    E_FUNCTION : ux_fgets0(s,n,stream)
**			Reads characters from stream into the array pointed to by s 
**			until n-1 characters are read.
**			
**    PARAMETERS   
**       INPUT: 
**				s: buffer array to receive characters from the stream
**				n: n-1 characters are read
**				stream: file pointer as returned from open routines
**			OUTPUT:			s buffer filled with n-1 characters and null terminator.
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_fgets0(s,n,stream)
	char *s;
	int n;
	FILE *stream;
{
	int status;
	uu_denter(UU_XTRC, (us, "ux_fgets0(s:%x,n:%d,stream:%x)", s,n,stream));
	status = UU_SUCCESS; /* assume success */
	status = ux_fgets1(s,n,stream,0);
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION : ux_fgets1(s,n,stream,flag)
**		Reads characters from stream into the array pointed to by s 
**		until n-1 characters are read. if the flag is set it read the header
**		till the null character and substitues the rest by spaces.
**			
**    PARAMETERS   
**       INPUT: 
**		s: 	buffer array to receive characters from the stream
**		n: 	n-1 characters are read
**		stream: file pointer as returned from open routines
**		flag: 	if 0 then do not check for header
**			if 1 then check if header and read stringtill null char
**	OUTPUT:
**		s buffer filled with n-1 characters and null terminator.
**    RETURNS: 
**		UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: 
**		none
**    WARNINGS: 
**		none
*********************************************************************/
int ux_fgets1(s,n,stream,flag)
char *s;
int n;
FILE *stream;
int flag;
{
	char *fgets();
	int i,j,status; /* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_fgets0(s:%x,n:%d,stream:%x)", s,n,stream));
	status = UU_SUCCESS; /* assume success */

#if (UU_COMP == UU_WINNT) || (UU_COMP == UU_WIN2K)
repeat:;
	if ( (fgets(s,n,stream)==UU_NULL) || feof(stream) )
		goto failed;
	if (s[0] == '\n' && s[1] == '\0') goto repeat;
	for (i=0;i<n;i++)
	{
		if (s[i] == '\r') s[i] = '\n';
/*
.....if the flag is set read theheader till the null characte
*/
		if (flag && (s[i] == '\0'))
		{
			for (j=i+1;j<n;j++)
				s[j] = ' ';
			break;
		}
	}
#else
	if ( (fgets(s,n,stream)==UU_NULL) || feof(stream) )
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
**    E_FUNCTION : ux_fputs0(s,stream) 
**			Writes the string s to the stream, followed by a newline.
**			
**    PARAMETERS   
**       INPUT  : 
**				s:
**				stream:
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_fputs0(s,stream)
	char *s;
	FILE *stream;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_fputs0(s:%s,stream:%x)", s, stream));

	status = UU_SUCCESS; /* assume success */
	if (fputs(s,stream) == EOF)
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
**    E_FUNCTION : ux_getw0(stream,w)
**		Returns the next word (integer) from the stream named.
**			
**    PARAMETERS   
**       INPUT  : 
**				stream: file pointer as returned by the open routines
**       OUTPUT: 
**				w: returns the next word (integer) from the named stream
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_getw0(stream,w)
	FILE *stream;
	int *w;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_getw0(stream:%x)", stream));
	status = UU_SUCCESS; /* assume success */

	*w = getw(stream);
	if ( (*w == EOF)&&(feof(stream)==0) ) /* an error, not just eof, conditin */
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
**    E_FUNCTION : ux_fgetc0(stream,cg)
**			Returns the next character from stream.
**			
**    PARAMETERS   
**       INPUT  : 
**				stream:
**       OUTPUT: 
**				cg: returns the next char (byte) from the stream
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**				and UX_EOF is cg is EOF constant
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_fgetc0(stream, cg)
	FILE *stream;
	char *cg;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int c;

	status = UU_SUCCESS; /* assume success */
	c = fgetc(stream);
	if ( (c == EOF) && (feof(stream)==0) )
		goto failed;					/* then error, not just eof, condition */
	if (c == EOF)
		status = UX_EOF;

	*cg = c;
	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION : ux_fputc0(c, stream)
**			Writes the character c to the stream.
**			
**    PARAMETERS   
**       INPUT  : 
**				c
**				stream
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_fputc0(c,stream)
	char c;
	FILE *stream;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_fputc0(c:%c,stream:%x)", c, stream));

	status = UU_SUCCESS; /* assume success */
	if (fputc(c,stream) == EOF )		/* then error condition */
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
**    E_FUNCTION : ux_putw0(w,stream) 
**			Writes the word w to the stream.
**			
**    PARAMETERS   
**       INPUT  : 
**				w
**				stream
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_putw0(w,stream)
	int w;
	FILE *stream;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_putw0(w:%d,stream:%x)", w, stream));

	status = UU_SUCCESS; /* assume success */
	if (putw(w,stream) == EOF)
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
**    E_FUNCTION : ux_lseek0(fildes, offset, whence, place) 
**			Move read/write pointer to a particular position in a file
**			prior to read/write.
**			
**    PARAMETERS   
**       INPUT  : 
**				filedes: integer open file descriptor returned by create or
**						open routines
**				offset: bytes 
**				whence: 0 - set from beginning, 1 - set from current location,
**							2 - set from size of the file.
**       OUTPUT: 
**				place:	resulting pointer location measured in bytes from
**							the beginning of the file.
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_lseek0(filedes, offset, whence, place)
	int filedes;
	long offset;
	int whence;
	long *place;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_lseek0(filedes:%d,offset:%d,whence:%d,place:%d)",
	filedes,offset,whence,place));

	status = UU_SUCCESS; /* assume success */
	*place = lseek(filedes, offset, whence);
	if ( *place == EOF )	/* returns -1 for error */
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
**    E_FUNCTION : ux_ungetc0(c,stream)
**			Inserts character c into the buffer associated with an input stream.
**			That character c will be returned on the next getc call.
**			
**    PARAMETERS   
**       INPUT  : 
**				c
**				stream
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_ungetc0(c,stream)
	char c;
	FILE *stream;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_ungetc0(c:%c,stream:%x)", c,stream));
	status = UU_SUCCESS; /* assume success */

	if ( ungetc(c,stream) == EOF )
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
**    E_FUNCTION : ux_vaxdir0(filename,dirspec)
**		Routine to convert a vax file spec format to .DIR type spec,
**		since many lib functions work only on this form 
**			
**    PARAMETERS   
**       INPUT  : 
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_vaxdir0(filename,dirspec)
	char *filename;
	char *dirspec;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	char  *rindex();

	uu_denter(UU_XTRC, (us, "ux_vaxdir0(filename:%s)", filename));
	status = UU_SUCCESS; /* assume success */

	strcpy(dirspec,filename);

#if UU_COMP == UU_VAXVMS
	matchb = rindex(dirspec,'[');
	if (matchb == UU_NULL)
		goto failed;
	endb = rindex(matchb,']');
	if (endb == UU_NULL)
		goto failed;

	if ( (*(endb+1)!='\0') && ( *(endb+1)!='"') )
		/* already in form [xx]yy */
		goto done;

	lastd = rindex(dirspec,'.');
	if (lastd == UU_NULL)
	{	 /* then we probably have format [xxx], change to [-]xxx.DIR */
		*endb = '\0';
		if ( *(endb+1) == '"')
		{
			strcpy(tempname,"\"[-]");
			strcat(tempname,matchb+1);
			strcat(tempname,".DIR\"");
		}
		else
		{
			strcpy(tempname,"[-]");
			strcat(tempname,matchb+1);
			strcat(tempname,".DIR");
		}
		strcpy(dirspec,tempname);
	}
	else	/* convert [xxxx.yyyyy] to [xxxx]yyy.DIR */
	{
		*lastd = ']';	/* replace [xxx.yyyy] with [xxx]yyyy] */
		*endb = '\0';	/* becomes [xxx]yyyy	*/
		strcat(dirspec, ".DIR");	/* becomes [xxx]yyyy.DIR	*/
		if (*(endb+1) == '"')
			strcat(dirspec,"\"");
	}
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
