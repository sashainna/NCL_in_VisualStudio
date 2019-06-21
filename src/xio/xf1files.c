/************************************************************************
**
**  NAME:	xf1files.c 
**
**      contains:		ux_access1
**							ux_close
**							ux_create_file
**							ux_get_and_open
**							ux_is_open
**							ux_is_farea
**							ux_open
**							ux_open_to_data
**							ux_read
**							ux_write
**							ux_fgets
**							uxi_add_unifile_desc
**							uxi_put_file_hdr
**							ux_write_block
**							ux_read_block
**							ux_get_date
**
**  MODULE NAME AND RELEASE LEVEL
**       xf1files.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:33
**
**************************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "xfsys1.h"			/* the include file for level one file system */
#include "xfsys0.h"
#include "nclfc.h"
#include "udebug.h"			/* used by the debug system */
#include "xenv1.h"			/* the include file for level one env system */
#include "derror.h"			/* needed for error system resolution */
#include "uhep.h"				/* used by the debug system */
#define TRACE UU_TRUE
#if UU_COMP == UU_VAXVMS
#include <types.h>
#include <stat.h>
#include <time.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#if UU_COMP == UU_IRIS || UU_COMP == UU_IRIS4D
#ifdef UU_RS6000
#include <sys/time.h>
#endif
#include <time.h>
#else
#if (UU_COMP != UU_WIN2K)
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif
#endif
#if (UU_COMP == UU_WIN2K)
#include <string.h>
#define index strchr
#define rindex strrchr
#define close _close
#endif

#include "nclver.h" 	/*NCL: use NCL version number in UNIBASE header */

void ux_get_date();

/*********************************************************************
**    E_FUNCTION :  ux_access1(pathname, modeptr, options)
**			Determines whether "pathname" is can be accessed by 
**			the mode, "mode". Note, if the portion of "pathname"
**			upto but not including the final piece of the path 
**			can not be accessed (due to permissions or existence)
**			then UX_NO_ACCESS will be returned as the value of 
**			this function.
**			
**    PARAMETERS   
**			"pathname": Can designate either	a file area or an 
**				ordinary file; must be a quoted system dependent path name.
**			"modeptr": 
**				Input: A pointer to an "OR" of the following values: 
**					UX_READ: can the file be read;
**					UX_WRITE: can the file be written to;
**					UX_EXECUTE: can the file be executed;
**					UX_DELETE: can the file be deleted;
**				Output: 
**					One of the following values:
**						UX_NEXISTS: If "pathname" corresponds to a file 
**							(not a file area) and the path to the last file
**							area in "pathname" exists and can be searched 
**							but "pathname" does not exist.
**							If "pathname" designates a file area (not a file)
**							and its parent file area exists and can be 
**							searched but "pathname" does not exist. 
**						UX_NEXISTS | UX_CREATE: same as above but the file
**							(area) maybe created. This assumes UX_CREATE was
**							specified as input in "modeptr".
**						UX_LOCKED: the file is locked.
**						UX_EXISTS: file exists, "OR"'d with a bitwise "OR" of the
**						mode values that are satisfied: 
**							UX_READ, UX_WRITE, UX_EXECUTE, UX_DELETE
**							and, in addition, if the file is currently open by this 
**							process, then the type of open is also "OR"ed in:
**							UX_R: open for reading; UX_W: writing; UX_A: appending.
**						UX_FAREA: "pathname" is a file area (that exists), "OR"'d 
**							with a bitwise "OR" of the mode values that are satisfied: 
**							UX_READ, UX_WRITE, UX_EXECUTE, UX_DELETE
**			"options": A bitwise "OR" of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions; note, the statuses returned
**						by "modeptr" are not considered errors.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					UX_CHK: check the syntax of the path name.
**					UX_NCHK: don't check the syntax of the path
**						name.
**					If incorrectly specified we will print errors and not
**					check path name syntax.
**		RETURNS: 
**			UU_SUCCESS is returned if the file can be accessed
**			with the given mode; otherwise one the following values
**			may be returned: (these are all considered errors)
**				UX_BAD_SUBJECT: the syntax of "pathname" is not valid;
**				UX_BAD_MODE: the mode is illegal;
**				UX_NO_ACCESS: don't have access to determine the
**					the status of "pathname";
**				or, UX_FAILURE if something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_access1(pathname, modeptr, options)
	char *pathname;
	int *modeptr;
	int options;
{
	UU_LOGICAL printit;
	int status;
	int fastat;
#if UU_COMP == UU_VAXVMS
	char *getenv();
	char *chrs;
#else
#if (UU_COMP == UU_RIDGE) || (UU_COMP == UU_WIN2K)
	char *getcwd();
	char *chrs;
#else
	char *getwd();
#endif
#endif
	char *index();
	char *rindex();
	char *fq, *lq;
	UX_pathname cdir;
	UX_pathname noquote;
	int mode, openmode;
	UX_pathname farea, fname;
	int smode;		/* SUN OS 4.1 and SGI work differently than SUN OS 4.0.3 */

	uu_denter(UU_XTRC,(us,"ux_access1(pathname:%x, mode:%d, options:%d)",
								pathname, *modeptr, options));
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
	fq = index(pathname, UX_QUOTE_CHAR);
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

	mode = *modeptr;

	if ((mode | UX_EWRDOCEXF) != UX_EWRDOCEXF) 
	{
		status = UX_BAD_MODE;
		goto done;
	}

	/* check to see if the file (area) can be found */
	if (ux_access0(noquote, UX_EXISTS) == UU_SUCCESS)
		*modeptr = UX_EXISTS;
	else /* file can't be found */
	{
		*modeptr = UX_NEXISTS;

		/* can file area containing "pathname" can be written to? */
		status =
		  ux_decompose_path(pathname,farea,fname,(options|UX_NCHK|UX_NQUOTES));
		if (status!=UU_SUCCESS) 	/* a farea wasn't successfully split off */
		{
#if UU_COMP == UU_VAXVMS
			chrs = getenv("PATH");
			strcpy(cdir,chrs);
#else
#if (UU_COMP == UU_RIDGE) || (UU_COMP == UU_WIN2K)
			chrs = getcwd(cdir,UX_MAX_PATH_LEN);
			strcpy(cdir,chrs);
#else
			getwd(cdir);
#endif
#endif
			strcpy(farea,cdir);
			strcpy(fname,noquote);
		}
		if (ux_access0(farea, UX_EXISTS) != UU_SUCCESS)
		{
			status = UX_NO_ACCESS;
			uu_dprint(UU_XTRC,(us,"status is UX_NO_ACCESS"));
			goto done;
		}
		else /* farea split from pathname exists -- can write to it? */
		{
			status = UU_SUCCESS;
			if (ux_access0(farea, UX_EXECUTE) != UU_SUCCESS)
				*modeptr = UX_NEXISTS;
			else
				*modeptr = (UX_NEXISTS | UX_CREATE);

			goto done;
		}
	}
	/* status should still be UU_SUCCESS here */
	/* now we assume "pathname" exists, check to see if we have a file area */
	if ((fastat = ux_is_farea(noquote, (options | UX_NCHK))) == UU_SUCCESS)
		*modeptr = UX_FAREA;
	else
		if ( fastat != UX_NFAREA)	/* then big error since we accessed ok above */
			goto failed;
		else		/* pathname is a file that exists */
			if (ux_is_open(noquote, &openmode, (options | UX_NCHK)) == UU_SUCCESS)
				*modeptr = *modeptr | openmode;

	if ((mode | UX_EXECUTE) == mode)
		if (ux_access0(noquote, UX_EXECUTE) == UU_SUCCESS)
			*modeptr = *modeptr | UX_EXECUTE;
	if ((mode | UX_WRITE) == mode) 
		if (ux_access0(noquote, UX_WRITE) == UU_SUCCESS)
			*modeptr = *modeptr | UX_WRITE;
	if ((mode | UX_DELETE) == mode)
		{

		/*RAH: SUN 4.1 (and SGI) only accept READ, WRITE, EXECUTE and 0 */
		/* as arguments to acces()                                      */
		/* if (ux_access0(noquote, UX_DELETE) == UU_SUCCESS)            */
		smode = UX_DELETE;
#if UU_COMP != UU_VAXVMS
		if (mode == UX_DELETE)
			smode = UX_WRITE;
#endif
		if (ux_access0(noquote, smode) == UU_SUCCESS)
			*modeptr = *modeptr | UX_DELETE;

		}
	if ((mode | UX_READ) == mode)
		if (ux_access0(noquote, UX_READ) == UU_SUCCESS)
			*modeptr = *modeptr | UX_READ;

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
**    E_FUNCTION :  ux_is_farea(pathname,options)
**			Determines is the sys. dep. pathname is the name of a directory.
**			
**    PARAMETERS   
**       INPUT  : 	pathname: a sys. dep. file specification
**    RETURNS: UU_SUCCESS if no problems encountered,
**				UX_NFAREA if not a file area, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_is_farea(pathname, options)
	char *pathname;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	char *index();
	char *rindex();
	char *lq, *fq;
	UX_pathname noquote;
	struct stat stbuf;	/*	info about file obtained from stat()	*/
#if UU_COMP == UU_WIN2K
	int len;
#endif

	uu_denter(UU_XTRC,(us,"ux_is_farea(pathname:%x, options:%d)",
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
	/* if VMS file spec is [x], convert the formatto ".dir" type */
	if ((right = rindex(noquote,']')) != UU_NULL)
		if ( *(right+1) == '\0')
		{
			/* need to reformat dir spec on VMS */
			if (ux_vaxdir0(noquote,dirname) != UU_SUCCESS)
				goto failed;
			strcpy(noquote,dirname);
		}
#endif

#if UU_COMP==UU_WIN2K
/*
......it may include '\' when directory
......like "d:\ncl\" but this should acceptable
......Yurong 7/20/00
*/
	len = strlen(noquote);
/*
.....if c:\ or d:\ then don't remove '\' because
.....otherwise, 'stat' will return 0 (not aceept as valid name)
.....and also if use input 'c:' or 'd:' it should also accept
.....as 'c:\' or 'd:\'
.....Yurong 3/7/02
*/
	if ((noquote[len-2]!=':')&&(noquote[len-1]=='\\'))
		noquote[len-1] = '\0';
	else if (noquote[len-1]==':')
	{
		noquote[len] = '\\';
		noquote[len+1] = '\0';
	}
#endif
	if  (stat(noquote, &stbuf) != 0)
		goto failed;							/* stat call failed */
	if ((stbuf.st_mode & S_IFMT) == S_IFDIR)
		goto done;									/* file is a directory */
	else
		status = UX_NFAREA;						/* file isn't a directory */

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
**    E_FUNCTION : ux_is_open(pathname, modeptr, options) 
**			Determines is a sys. dep. file spec., pathname, is an open file.
**			
**    PARAMETERS   
**       INPUT  : 
**					pathname: a sys. dep. file spec. to check. 
**					options: 
**       OUTPUT :  
**					modeptr: points to value specifying type of open.
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_is_open(pathname, modeptr, options)
	char *pathname;
	int *modeptr;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL  printit;
	int i;
	char *rindex();
	char *index();
	char *lq, *fq;
	UX_pathname noquote;

	uu_denter(UU_XTRC,(us,"ux_is_open(pathname:%x,modeptr:%x,options:%d)",
			pathname, modeptr, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname != UU_NULL)
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


	for (i=0; i<otind; i++)
		if (strcmp(otable[i].fname,noquote) == 0 )
		{
			if ( strcmp(otable[i].openfor,"r") == 0)
				*modeptr = UX_R;
			if	( strcmp(otable[i].openfor,"r+") == 0)
				*modeptr = UX_R + UX_W;
			if ( strcmp(otable[i].openfor,"w") == 0)
				*modeptr = UX_W;
			if	( strcmp(otable[i].openfor,"w+") == 0)
				*modeptr = UX_W + UX_R;
			if ( strcmp(otable[i].openfor,"a") == 0)
				*modeptr = UX_A;
			if	( strcmp(otable[i].openfor,"a+") == 0)
				*modeptr = UX_A + UX_R + UX_W;
			break;
		}

	if (i >= otind)		/* file not found in open file record */
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
**		E_FUNCTION : ux_create_file(pathname, mode, fenv, form, interp, 
**							dataptr, unifileptr,	options)
**			Attempts to create a file (not a file area), open it,
**			write the UNICAD header (if desired), and return a
**			pointer to a UNICAD open file table entry; the newly created
**			file is left open (for writing) and the file pointer 
**			(if any) is left at the end of the file; i.e. just 
**			beyond the newly written header.
**		PARAMETERS
**			INPUT:
**			"pathname": A full quoted system dependent path name, including 
**				all extensions. 
**			"mode": access permissions for the file, UNIX style;
**			"fenv": If not UU_NULL, then this is the environmental variable
**						used to check to determine if the file can be
**						created in the file area specified in "pathname".
**						If UU_NULL, no checking will be done.
**			"form": Specifies the access method of the file for
**				reading and writing the file; can be:
**					BLOCKED: blocked sequential file;
**					STREAM: stream file.
**			"interp": Specifies the interpretation of the file; i.e.
**					whether the file contains text, or binary data; can be:
**						ASCII: the file contains ASCII text;
**						TEXT: text but not ASCII;
**						BINARY: the file is binary.
**			"dataptr": If not "UX_NOHEADER", then a header will be put 
**				in the file. If "UX_NOEXTRA" then no extra data will
**				be put at the end of the header. Otherwise, this is
**				a pointer to the extra data that	is put at the end 
**				of the header. The data must be delimited by a '\0'.
**			"unifileptr": returns the pointer into the open file table, used to 
**				read and write UNICAD files.
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
**			UU_SUCCESS is returned if the file can be created
**			with the given mode, otherwise one of the following
**			values will be returned: 
**				(these are all considered errors.)
**				UX_BAD_SUBJECT: bad path name;
**				UX_BAD_TYPE: wrong filetype for the filearea;
**				UX_BAD_ENV:	Wrong environmental variable or value not found
**				UX_BAD_MODE: can't create file with this mode;
**				UX_NO_ACCESS: don't have permission/access to create 
**						the file;
**				UX_BAD_FORMAT: can't create file with this format;
**				UX_BAD_INTERP: can't create a file with this inter-
**						pretation;
**				UX_FOUND: file found, already exists;
**				UX_FAILURE: something else went wrong.
**
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ux_create_file(pathname, mode, fenv, form, interp, 
						dataptr, unifileptr,	options)
	char *pathname;
	int mode;
	char *fenv;
	char *form;
	char *interp;
	char *dataptr;
	int *unifileptr;
	int options;
{
	FILE *fileptr;
	int filedesc;
	UU_LOGICAL printit;
	int status;
	int accessmode;
	char *index();
	char *rindex();
	char *lq, *fq;
	UX_pathname noquote;
	
	uu_denter(UU_XTRC,(us,
	"ux_create_file(%x,mod:%o,form:%s,interp:%s,data:%x,?,options:%d)",
						pathname,mode,form,interp,dataptr,options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname != UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathname:%s", pathname));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	UX_CHK_PATHERRS(pathname, printit, options);

	if ( (mode>0777) || (mode<0) )
	{
		status = UX_BAD_MODE;
		goto done;
	}

	/* check to see if we have access to create the file, assume "pathname
	doesn't exist */
	accessmode = UX_CREATE;
	if (ux_access1(pathname, &accessmode, options | UX_NCHK) != UU_SUCCESS) 
			goto failed;
	if ( (accessmode | UX_FAREA) == accessmode)	 /* file area instead of file */
	{
			UX_ERROR2(21,pathname,"ux_create_file",printit);
			/* error is: Error, %s is a file area (%s). */
			goto failed;
	}
	if (accessmode == UX_NEXISTS) /* can't create file in file area */
	{
			UX_ERROR2(16,pathname,"ux_create_file",printit);
			/* error is: File, %s, doesn't exist and can't create it  (%s). */
			goto failed;
	}
	/* if (accessmode == UX_LOCKED) file exists and is locked */
	if (accessmode != (UX_NEXISTS | UX_CREATE)) 
	{
	/* case of accessmode = ( UX_EXISTS | (openmode) | (RWED codes) ) */
		status = UX_FOUND;
		goto done;
	}

	/* check acceptable values for format and interp: */
	if ((strcmp(form,"STREAM")!=0) && (strcmp(form,"BLOCKED")!=0))
	{
		status = UX_BAD_FORMAT;
		goto done;
	}
	if ((strcmp(interp,"ASCII")!=0) && (strcmp(interp,"TEXT")!=0) &&
		(strcmp(interp,"BINARY")!=0) )
	{
		status = UX_BAD_INTERP;
		goto done;
	}

	/* check pathname for properly matched file type and file area */
	if ( fenv !=  UU_NULL)
	{
		if ( ux_is_type_legal(pathname,fenv,options) != UU_SUCCESS)
			goto done;
	}
		
	/* "pathname" can be created; so create it with mode specified */
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

	if (ux_creat0(noquote, mode, &filedesc) != UU_SUCCESS)
		goto failed;
	/* need to close the file which is left open for write only no
	matter what other type is requested in fdopen, etc. */
	close(filedesc);
#if (UU_COMP != UU_WIN2K)
	if (ux_fopen0(noquote, "r+", &fileptr) != UU_SUCCESS)
		goto failed;
#else
	if (strcmp(interp,"BINARY")!=0)
	{
		if (ux_fopen0(noquote, "r+", &fileptr) != UU_SUCCESS)
			goto failed;
	}
	else
	{
		if (ux_fopen0(noquote, "rb+", &fileptr) != UU_SUCCESS)
			goto failed;
	}
#endif
	/* file created and opened for writing; add info to open file record
	 and return a pointer into the record */
	if ((status = uxi_add_unifile_desc(noquote,"r+",form,interp, 
			unifileptr, options)) != UU_SUCCESS) 
	{
		if (ux_fclose0(fileptr) != UU_SUCCESS)
		{
			uu_dprint(UU_XTRC,(us,
			"can't make/add file info; can't close (%s)", "ux_create_file"));
			goto failed;
		}
		goto done;
	}

	if (strcmp(dataptr,"UX_NOHEADER") !=0)	 /* then put a header on the file */
	{
		status = uxi_put_file_hdr(*unifileptr, dataptr, options);
		fflush(fileptr);
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
**    I_FUNCTION: int uxi_add_unifile_desc(pathname, type, 
**								form, interp, unifileptr, options)
**		The file is assumed to exist and be open. We check to see if "form"
**		and "interp" are reasonable values, add this info to the data structure
**		containing all currently open files known to udos, and returns pointer
**		into the record. (an index)
**    PARAMETERS   
**       INPUT: 
**			pathname system dependent path name to the file.
**			type	The type of operation the file is open for one of the 
**						following: r (read), w (write), a (append), r+ (read and 
**						write with file pointer at the beginning of the file, w+
**						(read and write, creates or truncates the file, a+ (read and
**						write, with file pointer at the end of the file).
**			form	Specifies the access method of the file for reading 
**						and writing the file; can be:
**						BLOCKED: blocked sequential file;
**						STREAM: stream file.
**			interp	Specifies the interpretation of the file; i.e.
**						whether the file contains text, or binary data; can
**						be:
**						ASCII: the file contains ASCII text;
**						TEXT: text but not ASCII;
**						BINARY: the file is binary.
**			options	One of the following values:
**							UX_PRTERRS: print any errors as specified by
**								uu_uerror functions.
**							UX_NPRTERRS: don't print any errors as 
**								specified by uu_uerror functions.
**						If incorrectly specified we will print errors.
**       OUTPUT:  
**			unifileptr	Pointer to a filled in UNICAD open file table entry.
**    RETURNS: UU_SUCCESS if the UNICAD file table entry is completed okay;
**					otherwise, UX_FAILURE: something else went wrong.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int uxi_add_unifile_desc(pathname, type, form, interp, unifileptr, options)
	char *pathname;
	char *type;
	char *form;
	char *interp;
	int *unifileptr;
	int options;
{
	int status;
	int i;
	UU_LOGICAL printit;

	uu_denter(UU_XTRC,(us,
	"uxi_add_unifile_desc(%x,type:%s,fmt:%s,interp:%s,?,options:%d)",
		pathname, type, form, interp, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathname:%s", pathname));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	for (i=0; i<=otind; i++)
		if (strcmp(otable[i].fname,pathname) == 0 )
		{
			strcpy(otable[i].openfor, type);
			strcpy(otable[i].format, form);
			strcpy(otable[i].interp,interp);
			*unifileptr = i;
			break;
		}
	if ( i > otind)
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
**    I_FUNCTION: int uxi_put_file_hdr(unifile, dataptr, options)
**		This functions attempts to create and put a UNICAD file header
**		in the file designated by "unifile".
**    PARAMETERS   
**       INPUT: 
**			unifile		Index to a UNICAD open file table entry.
**			dataptr		If "UX_NOEXTRA" then no extra data will be put at the end 
**							of the header. Otherwise, this is a pointer to the extra 
**							data that is put at the end of the header. The data must
**							be delimited by a '\0'.
**			options	One of the following values:
**							UX_PRTERRS: print any errors as specified by
**								uu_uerror functions.
**							UX_NPRTERRS: don't print any errors as 
**								specified by uu_uerror functions.
**						If incorrectly specified we will print errors.
**       OUTPUT:  none.
**    RETURNS: UU_SUCCESS if the header is created and written to the file;
**				 otherwise, one of the following values is returned;
**					UX_FAILURE: something else went wrong.
**    SIDE EFFECTS: none
**    WARNINGS: none
*******************************************************************
**	THIS ROUTINE HAS IN IT THE MACHINE NAMES AND UNICAD VERSION NUMBER.
*********************************************************************/
int uxi_put_file_hdr(unifile, dataptr, options)
	int unifile;
	char *dataptr;
	int options;
{
	UU_LOGICAL printit;
	char strbufr[80],datstr[12],timstr[12];
	int status;
	int nout;
	int plus = 0;
 UM_int2 inx, v264;

	uu_denter(UU_XTRC,(us,
		"uxi_put_file_hdr(unifile:%d,dataptr:%x,options:%d)",
		unifile, dataptr, options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	if (strcmp(dataptr,"UX_NOEXTRA")!=0)
		plus = strlen(dataptr);
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (dataptr != UU_NULL)
			uu_dprint(UU_XTRC,(us,"dataptr:%s", dataptr));
	}		
#endif

	/* char array variables will hold characters plus a string terminator
	and these terminators are not placed in the header by UX_FPRINTF0, and
	UX_HDR_LEN is actual number of bytes in fixed part of header */

	/* char head[30] -- title */
	UX_FPRINTF0((otable[unifile].fileptr,"12345UNICAD FILE HEADER DATA\n"),nout);
	/* char nbrbytes[6] -- number of bytes in total header */
	UX_FPRINTF0((otable[unifile].fileptr,"%5d"
					,((plus+UX_HDR_LEN)*sizeof(char))),nout);
	
	ux_get_date(datstr,timstr);
	UX_SPRINTF0((strbufr,"*******DATE:%s, %s\n",datstr,timstr));
	strbufr[29] = '\0';				/* don't allow to become too long */

	/* units: MM, INCH; replace *** in DATE field vp5/3/94 */
 inx = 264;
 getifl(&inx,&v264);
 if (v264 == 1)
	   strncpy(&strbufr[1],"MM",2);
 else 
	   strncpy(&strbufr[1],"INCH",4);

	UX_FPRINTF0((otable[unifile].fileptr,"%s",strbufr),nout);

	/* char version[20];product and/or version of software which created file */
	/*NCL: incorporate NCL version number as the UNIBASE version number */
	UX_SPRINTF0((strbufr,"NCL VERSION*%.3f****", NCL_version));
	strbufr[19] = '\0';				/* don't allow to become too long */
	UX_FPRINTF0((otable[unifile].fileptr,"%s",strbufr),nout);

	/* char precision[6] holds number of bytes in UU_REAL type */
	UX_FPRINTF0((otable[unifile].fileptr,"%5d",(sizeof(UU_REAL))),nout);

	/* char hformat[12]; BLOCK (blocked sequencial) or STREAM */
	UX_FPRINTF0((otable[unifile].fileptr,"%11s",otable[unifile].format),nout);

	/* char hinterp[12]; ASCII, TEXT (non-ascii text), BINARY */
	UX_FPRINTF0((otable[unifile].fileptr,"%11s",otable[unifile].interp),nout);

	/* char machine[20]; machine, model, and/or op sys used in creating file */
	strcpy(strbufr,"*MACHINE*UNKNOWN*\n");
#if UU_COMP==UU_SUN
#if UU_SUNTYPE != UU_SUN_SUN4
	strcpy(strbufr,"****SUN****\n");
#endif
#if UU_SUNTYPE == UU_SUN_SUN4
	strcpy(strbufr,"****SUN4***\n");
#endif
#endif
#if UU_COMP==UU_CIM
	strcpy(strbufr,"**CIMLINC**\n");
#endif
#if UU_COMP==UU_PYRAMID
	strcpy(strbufr,"**PYRAMID**\n");
#endif
#if UU_COMP==UU_IRIS
	strcpy(strbufr,"**SG*IRIS**\n");
#endif
#if UU_COMP==UU_IRIS4D
#ifdef UU_RS6000
	strcpy(strbufr,"*IBM*RS/6000*\n");
#else
	strcpy(strbufr,"**SG*IRIS4D**\n");
#endif
#endif
#if UU_COMP==UU_HPUX
	strcpy(strbufr,"**HP*PA-RISC*\n");
#endif
#if (UU_COMP==UU_WINNT) || (UU_COMP == UU_WIN2K) 
	strcpy(strbufr,"**WINDOWS*NT*\n");
#endif
#if UU_COMP==UU_386
	strcpy(strbufr,"**COMPAQ386**\n");
#endif
#if UU_COMP==UU_TEK
	strcpy(strbufr,"**TEK*4336**\n");
#endif
#if UU_COMP==UU_RIDGE
	strcpy(strbufr,"**RIDGE**\n");
#endif
#if UU_COMP==UU_APOLLO
	strcpy(strbufr,"**APOLLO**\n");
#endif
#if UU_COMP==UU_VAXVMS
#ifdef UU_ALPHA
	strcpy(strbufr,"*ALPHA/VMS*\n");
#else
	strcpy(strbufr,"**VAXVMS**\n");
#endif
#endif
#if UU_COMP==UU_VAXULTRIX
	strcpy(strbufr,"**VAXULT**\n");
#endif
#if UU_COMP==UU_IBMAT
	strcpy(strbufr,"***IBMAT***\n");
#endif
#if UU_COMP==UU_TEK6130
	strcpy(strbufr,"**TEK6130**\n");
#endif
#if UU_COMP==UU_DECUNIX
	strcpy(strbufr,"**DECUNIX**\n");
#endif
#if UU_COMP==UU_MASSCOMP
	strcpy(strbufr,"**MASSCOMP**\n");
#endif
	/* MILLS: save machine type to compare against while loading a file */
	strcpy(NCL_machine, strbufr);

	UX_FPRINTF0((otable[unifile].fileptr,"%19s", strbufr ),nout);

	/* char *extra;	extra header info put in by user */
	if (strcmp(dataptr,"UX_NOEXTRA")!=0)
		UX_FPRINTF0( (otable[unifile].fileptr,"%s",dataptr), nout);
		/* printed to the file without string terminator */

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
**    E_FUNCTION : ux_read(unifile, rbuf, size, nbritemsptr, options)
**			Attempts to read the next "nbritems" from the file associated
**			with "unifile" at the current location pointed to by the file
**			pointer.  The file must be open for reading to succeed.
**			Upon return the file pointer is incremented to the next position
**			following the last item read.  This is an unformatted read.
**		PARAMETERS:
**			"unifile": Index to the UNICAD open file table from "ux_open".
**			"rbuf": buffer into which to put the data read.
**			"size": sizeof(*rbuf), size of an element in array rbuf
**			"nbritemsptr": 
**				input: pointer to the number of items to read; 
**				output: the number of items actually read; 
**			"options": One of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					If incorrectly specified we will print errors.
**		RETURNS:
**			UU_SUCCESS is returned if the read was accomplished
**			with "*nbritemsptr" items returned; otherwise, one
**			of the following values is returned:
**				UX_EOF: end of file encountered (not an error);
**					(The following are all considered errors.)
**				UX_BAD_DES: bad UNICAD file descriptor;
**				UX_NO_ACCESS: can't read the file;
**				UX_NO_SPACE: can't read the number of bytes/blocks 
**					requested; "rbuf" is too small;
**				UX_FAILURE: something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_read(unifile, rbuf, size, nbritemsptr, options)
	int unifile;
	char *rbuf;
	int size;
	int *nbritemsptr;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int nread;
	UU_LOGICAL printit;
	uu_denter(UU_XTRC,(us,
		"ux_read(unifile:%d,?,size:%d,nbritems:%d, options:%d)",
		unifile, size, *nbritemsptr, options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	if ( (unifile>=otind) || (otable[unifile].fileptr == UU_NULL) )
	{
		status = UX_BAD_DES;
		goto done;
	}

	if ( ( strcmp(otable[unifile].openfor,"w") == 0 ) ||
		( strcmp(otable[unifile].openfor,"a") == 0 ) )
	{
		/* not open for reading */
		status = UX_NO_ACCESS;
		goto done;
	}

	if ( strcmp(otable[unifile].format,"STREAM") == 0 )
		if ( ux_fread0(rbuf, size, *nbritemsptr, otable[unifile].fileptr,
			&nread) == UX_FAILURE)		/* error condition */
			goto failed;
		else	/* success or EOF condition */
		{
			if (nread == 0)
				status = UX_EOF;
			else
				if ( nread < *nbritemsptr )
					status = UX_NO_SPACE;
			*nbritemsptr = nread;
		}
	else 	/* then otable[unifile].format must be "BLOCKED" */
	{
		UX_ERROR0(10,printit);
		/*	Unable to read file that isn't STREAM file */
	}		/* end of if-else */

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
**    E_FUNCTION :  ux_write(unifile, wbuf, size, nbritemsptr, options)
**			Attempts to write the buffer "wbuf" into the file associated
**			with "unifile". The file must be open for writing. No check
**			will be made to assure a text file is being written if text is
**			specified in the UNICAD file descriptor. This is an unformatted
**			write. 
**		PARAMETERS:
**			"unifile": Index to the UNICAD file descriptor from "ux_open".
**			"wbuf": buffer containing data to be written.
**			"size": size in bytes of an item to be written, sizeof(*wbuf).
**			"nbritemsptr": input: number of items to be written
**								output: number of items written.
**			"options": One of the following values:
**					UX_PRTERRS: print any errors as specified by uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as specified by uu_uerror
**						functions.
**					If incorrectly specified we will print errors.
**		RETURNS:
**			UU_SUCCESS is returned if the write was accomplished with the
**			requested "*nbritemsptr" items written; otherwise, one of the
**			following values is returned:
**				(these are all considered errors)
**				UX_BAD_DES: bad UNICAD file descriptor;
**				UX_NO_ACCESS: can't write to the file;
**				UX_BAD_WRITE: write error occurred;
**				UX_FAILURE: something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_write(unifile, wbuf, size, nbritemsptr, options)
	int unifile;
	char *wbuf;
	int size;
	int *nbritemsptr;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int nwrit;
	UU_LOGICAL printit;
	uu_denter(UU_XTRC,(us,
		"ux_write(unifile:%d,wbuf:0x%x,size:%d,nbritems:%d,options:%d)",
		unifile, wbuf, size, *nbritemsptr, options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	if ( (unifile>=otind) || (otable[unifile].fileptr == UU_NULL ))
	{
		status = UX_BAD_DES;
		goto done;
	}

	if ( strcmp(otable[unifile].openfor,"r") == 0 )
	{
		/* not open for writing */
		status = UX_NO_ACCESS;
		goto done;
	}

	if ( strcmp(otable[unifile].format,"STREAM") == 0 )
	{
		if ( ux_fwrite0 (wbuf,size,*nbritemsptr,otable[unifile].fileptr, &nwrit)
			== UX_FAILURE)		/* error condition */
			goto failed;
		else
		{
			if (nwrit != *nbritemsptr)
			{
				status = UX_BAD_WRITE;
				*nbritemsptr = nwrit;
			}
		}
	}
	else	/* file is opened for "BLOCKED" writing */
	{
		UX_ERROR0(11,printit);
		/* Unable to write to file that isn't a STREAM file. */
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
**    E_FUNCTION : ux_fgets(s,n,stream)
**			Reads characters from stream into the array pointed to by s 
**			until n-1 characters are read.
**			
**    PARAMETERS   
**       INPUT: 
**				s: buffer array to receive characters from the stream
**				n: n-1 characters are read
**				stream: file pointer as returned from open routines
**			OUTPUT:			s buffer filled with n-1 characters and null terminator.
**    RETURNS:	UU_SUCCESS, UX_FAILURE  upon an error, or UX_EOF upon EOF
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_fgets(s,n,stream)
	char *s;
	int n;
	FILE *stream;
{
	char *fgets();
	char *chr;
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
#if (UU_COMP == UU_WINNT) || (UU_COMP == UU_WIN2K) 
	int i;
#endif

	uu_denter(UU_XTRC, (us, "ux_fgets(s:%x,n:%d,stream:%x)", s,n,stream));
	status = UU_SUCCESS; /* assume success */

	/* test if this means EOF or an error */
#if (UU_COMP == UU_WINNT) || (UU_COMP == UU_WIN2K) 
repeat:;
	if ((chr=fgets(s,n,stream))==UU_NULL)
	{
		if (feof(stream)==0)
			goto failed;
		else
			status = UX_EOF;
	}
	if (s[0] == '\n' && s[1] == '\0' && status != UX_EOF) goto repeat;
	for (i=0;i<n;i++)
	{
		if (s[i] == '\r') s[i] = '\n';
	}
#else
	if ((chr=fgets(s,n,stream))==UU_NULL)
	{
		if (feof(stream)==0)
			goto failed;
		else
			status = UX_EOF;
	}
#endif

#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (s!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"s:%s", s));
	}
#endif

	goto done;
failed:
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION : ux_open(pathname, type, format, interp, unifileptr, options)
**			Opens the file at "pathname" and returns a UNICAD open file
**			table entry in "unifileptr". The file should not be a file area.
**			Note, the file must already exist. 
**			No multiple paths are searched; only the first path is used.
**		PARAMETERS:
**			"pathname": Either a full quoted system dependent path name, or
**				a UNICAD file specification. If there
**				are multiple path names associated with "pathname",
**				then only the first path is examined.
**			"type": "r", "w", "a", "r+", "w+", "a+"
**				Note that the file is also opened for reading so that this
**				routine can read the file header.
**			"format": file organization format; 
**						any operating system file format known 
**						by fsys may be specified. Note any operating
**						system organization specified must be compatible 
**						with the organization specified in the file 
**						header.
**			"interp": file interpretation;
**						any operating system file interpretation known 
**						by fsys may be specified. Note any operating
**						system organization specified must be compatible 
**						with the organization specified in the file 
**						header.
**			"unifileptr": returns a UNICAD file descriptor, used to 
**					read and write files.
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
**			UU_SUCCESS is returned if the file can be opened,
**			otherwise one of the following values will be returned: 
**				(these are all considered errors)
**				UX_BAD_SUBJECT: bad path name;
**				UX_NO_ACCESS: don't have permission/access to open 
**						the file;
**				UX_NFOUND: file not found;
**				UX_FAREA: file is a file area;
**				UX_BAD_FORMAT: can't open file with this format;
**				UX_BAD_INTERP: can't open a file with this inter-
**						pretation;
**				UX_FAILURE: something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_open(pathname, type, form, interp, unifileptr, options)
	char *pathname;
	char *type;
	char *form;
	char *interp;
	int *unifileptr;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	char *pathlistptr = UU_NULL;
	UU_LOGICAL found, printit;
	UX_pathname filename;
	UX_pathname noquote;
	char *lq, *fq;
	char *index();
	char *rindex();
	int mode;
	FILE *fptr;

	uu_denter(UU_XTRC,(us,"ux_open(path:%x,type:%s,form:%s,intrp:%s,?,optns:%d)",
		pathname, type, form, interp, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathname:%s", pathname));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	status = ux_get_syspath(pathname,&pathlistptr,filename,
			&found, (options|UX_NQUOTES));
	if (status == UX_FAILURE)
		goto failed;
	else if (status != UU_SUCCESS ) 
		goto done;

	uu_lsdel(pathlistptr);
	mode = UX_EWRDOCEXF;		/* defined value for access1 mode */
	status  = ux_access1(filename, &mode, options );
	if (status == UX_FAILURE)
		goto failed;
	else if (status != UU_SUCCESS)
		goto done;

	if ( mode == ( mode | UX_FAREA ) ) 
	{
		status = UX_FAREA;
		goto done;
	}
	if ( mode == (mode | UX_NEXISTS))
	{
		status = UX_NFOUND;
		goto done;
	}
	else			/* case of mode is ( UX_EXISTS | openmode | RWED modes ) */
	{
		/* check acceptable values for format and interp: */
		if ((strcmp(form,"STREAM")!=0) && (strcmp(form,"BLOCKED")!=0))
		{
			status = UX_BAD_FORMAT;
			goto done;
		}
		if ((strcmp(interp,"ASCII")!=0) && (strcmp(interp,"TEXT")!=0) &&
			(strcmp(interp,"BINARY")!=0) )
		{
			status = UX_BAD_INTERP;
			goto done;
		}
		
		/* remove quotes to get working pathname: */
		fq = index(filename,UX_QUOTE_CHAR);
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
			strcpy(noquote,filename);

		if (ux_fopen0(noquote,type,&fptr) == UX_FAILURE)
			goto failed;
		else
			if ((status = uxi_add_unifile_desc
				(noquote,type,form,interp,unifileptr, options)) != UU_SUCCESS) 
				if (ux_fclose0(fptr) != UU_SUCCESS)
				{
					uu_dprint(UU_XTRC,(us,
					"can't make/add file info; can't close (%s)", "ux_open"));
					goto failed;
				}
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
**    E_FUNCTION : ux_get_and_open
**						(pathname, type, form, interp, unifileptr, options)
**			Opens the file at "pathname" and returns a UNICAD 
**			file descriptor. The file should not be a file area.
**			Multiple paths are searched; the first file found that 
**			corresponds to "pathname" is the one opened.
**			The parameters have the same meaning as in "ux_open" above.
**			Returns same values as "ux_open".
**		PARAMETERS:
**			"pathname": Either a full quoted system dependent path name, or
**				a UNICAD file specification. Multiple names searched if there
**				are multiple path names associated with "pathname".
**			"type": "r", "w", "a", "r+", "w+", "a+"
**			"form": file organization format; 
**						any operating system file format known 
**						by fsys may be specified. Note any operating
**						system organization specified must be compatible 
**						with the organization specified in the file 
**						header.
**			"interp": file interpretation;
**						any operating system file interpretation known 
**						by fsys may be specified. Note any operating
**						system organization specified must be compatible 
**						with the organization specified in the file 
**						header.
**			"unifileptr": returns a UNICAD file descriptor, used to 
**					read and write files.
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
**			UU_SUCCESS is returned if the file can be opened,
**			otherwise one of the following values will be returned: 
**				(these are all considered errors)
**				UX_BAD_SUBJECT: bad path name;
**				UX_NO_ACCESS: don't have permission/access to open 
**						the file;
**				UX_NFOUND: file not found;
**				UX_FAREA: file is a file area;
**				UX_BAD_FORMAT: can't open file with this format;
**				UX_BAD_INTERP: can't open a file with this inter-
**						pretation;
**				UX_FAILURE: something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_get_and_open(pathname, type, form, interp, unifileptr, options)
	char *pathname;
	char *type;
	char *form;
	char *interp;
	int *unifileptr;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	char *pathlistptr;
	UU_LOGICAL found, printit;
	UX_pathname filename;
	UX_pathname noquote;
	char *index();
	char *rindex();
	char *fq, *lq;
	int mode;
	FILE *fileptr;

	uu_denter(UU_XTRC, (us, 
		"ux_get_and_open(%x,type:%s,form:%s,interp:%s,?,options:%d)",pathname,
		type, form, interp, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathname:%s", pathname));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

nextpath:;
	status = ux_get_syspath(pathname,&pathlistptr,filename,&found,
		(options | UX_NQUOTES) );
	/* may return UU_SUCCESS, UX_FAILURE, UX_NFOUND (at end of list ) */
	if (status == UX_FAILURE)
		goto failed;
	else if (status == UX_NFOUND) 
		goto done;

	mode = UX_EWRDEX;		/* good defined value for access1 mode */
	status  = ux_access1(filename, &mode, options );
	/* check filename in access1 */
	/* may return UU_SUCCESS, UX_FAILURE, UX_BAD_SUBJECT, UX_NO_ACCESS */
	if (status == UX_FAILURE)
		goto failed;
	else if (status != UU_SUCCESS)
		goto done;

	if ( mode == ( mode | UX_FAREA ))
		goto nextpath;		/* repeated calls to ux_get_syspath */

	if ( mode == (mode | UX_NEXISTS))
	{
		status = UX_NFOUND;
		goto done;
	}
	else /*file exists, case of mode is ( UX_EXISTS | openmode | RWED modes ) */
	{
		/* check acceptable values for format and interp: */
		if ((strcmp(form,"STREAM")!=0) && (strcmp(form,"BLOCKED")!=0))
		{
			status = UX_BAD_FORMAT;
			goto done;
		}
		if ((strcmp(interp,"ASCII")!=0) && (strcmp(interp,"TEXT")!=0) &&
			(strcmp(interp,"BINARY")!=0) )
		{
			status = UX_BAD_INTERP;
			goto done;
		}

		/* remove quotes to get working pathname: */
		fq = index(filename,UX_QUOTE_CHAR);
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
			strcpy(noquote,filename);

		if ( ux_fopen0(noquote,type,&fileptr) == UX_FAILURE)
			goto failed;
		else
			if ((status = uxi_add_unifile_desc
				(noquote,type,form,interp,unifileptr, options)) != UU_SUCCESS) 
				if (ux_fclose0(fileptr) != UU_SUCCESS)
				{	
					uu_dprint(UU_XTRC,(us,
					"cn't make/add file info; can't close (%s)", "ux_get_and_open"));
					goto failed;
				}
	}

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_lsdel(pathlistptr);
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION : ux_close(unifile, options)
**			Close the file associated with "unifile".
**			PARAMETERS:
**			"unifile": UNICAD file descriptor obtained from
**				"ux_open" or "ux_create_file".
**			"options": One of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					If incorrectly specified we will print errors.
**			RETURNS:
**			UU_SUCCESS is returned if the file is closed;
**			otherwise one of the following values is returned:
**				(these are all considered errors)
**				UX_NO_ENTRY: no table entry found to delete;
**				UX_FAILURE: something else went wrong.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_close(unifile, options)
	int unifile;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;

 	uu_denter(UU_XTRC,(us,"ux_close(unifile:%d,options:%d)",
		unifile,options)); 
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	if ( (unifile>=otind) || (otable[unifile].fileptr == UU_NULL) )
	{
		status = UX_NO_ENTRY;
		goto done;
	}		
	if ( (status = ux_fclose0(otable[unifile].fileptr) ) != UU_SUCCESS )
		/* returns UU_SUCCESS or UX_FAILURE */
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
**    E_FUNCTION : ux_open_to_data
**					(pathname, type, format, interp, unifileptr, options)
**			Opens the file at "pathname" and returns a UNICAD open file
**			table entry in "unifileptr". The file should not be a file area.
**			Note, the file must already exist. 
**			No multiple paths are searched; only the first path is used.
**			The file is open with the pointer for next I/O just after the
**			Unicad file header.
**		PARAMETERS:
**			"pathname": Either a full quoted system dependent path name, or
**				a UNICAD file specification. If there
**				are multiple path names associated with "pathname",
**				then only the first path is examined.
**			"type": "r", "w", "a", "r+", "w+", "a+"
**				Note that the file is also opened for reading so that this
**				routine can read the file header.
**			"format": file organization format; 
**						any operating system file format known 
**						by fsys may be specified. Note any operating
**						system organization specified must be compatible 
**						with the organization specified in the file 
**						header.
**			"interp": file interpretation;
**						any operating system file interpretation known 
**						by fsys may be specified. Note any operating
**						system organization specified must be compatible 
**						with the organization specified in the file 
**						header.
**			"unifileptr": returns a UNICAD file descriptor, used to 
**					read and write files.
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
**			UU_SUCCESS is returned if the file can be opened,
**			otherwise one of the following values will be returned: 
**				(these are all considered errors)
**				UX_BAD_SUBJECT: bad path name;
**				UX_NO_ACCESS: don't have permission/access to open 
**						the file;
**				UX_NFOUND: file not found;
**				UX_FAREA: file is a file area;
**				UX_BAD_FORMAT: can't open file with this format;
**				UX_BAD_INTERP: can't open a file with this inter-
**						pretation;
**				UX_FAILURE: something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_open_to_data(pathname, type, form, interp, unifileptr, options)
	char *pathname;
	char *type;
	char *form;
	char *interp;
	int *unifileptr;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int ufptr;

	uu_denter(UU_XTRC, (us, "ux_open_to_data"));
	status = UU_SUCCESS; /* assume success */

	if ( (status = ux_open(pathname, type, form, interp, &ufptr, options) )
		!= UU_SUCCESS)
		goto done;
	else
	{
		*unifileptr = ufptr;
		if ((status = ux_file_rewind(ufptr, options)) != UU_SUCCESS)
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
**    E_FUNCTION : ux_read_block(unifile, rbuf, blocklenptr, nbsptr, options)
**			Attempts to read the next BLOCK from the file associated
**			with "unifile" at the current location pointed to by the file
**			pointer.  The file must be open for reading to succeed.
**			Upon return the file pointer is incremented to the next position
**			following the last item read.  This is an unformatted read.
**		PARAMETERS:
**			"unifile": Index to the UNICAD open file table from "ux_open".
**			"rbuf": buffer into which to put the data read.
**			"options": One of the following values:
**					UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**					If incorrectly specified we will print errors.
**			OUTPUT:
**			"blocklenptr": number of bytes in block, as written at start of block;
**			"nbsptr": the number of bytes actually read;
**		RETURNS:
**			UU_SUCCESS is returned if the read was accomplished
**			with number of bytes found in blocklen in file returned; otherwise,
**			one of the following values is returned:
**				UX_EOF: end of file encountered (not an error);
**					(The following are all considered errors.)
**				UX_BAD_DES: bad UNICAD file descriptor;
**				UX_NO_ACCESS: can't read the file;
**				UX_NO_SPACE: can't read the number of bytes/blocks 
**					requested by the blocklength found ; "rbuf" is too small;
**				UX_FAILURE: something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_read_block(unifile, rbuf, blocklenptr, nbsptr, options)
	int unifile;
	char *rbuf;
	int *blocklenptr;
	int *nbsptr;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int nread, i;
	int blocklen, nr;
	union {
		int blen;
		unsigned char blenstr[sizeof(int)];
		}	block;
	UU_LOGICAL printit;

	uu_denter(UU_XTRC,(us, "ux_read_block(unifile:%d, ?, ?, ?, options:%d)",
		unifile, options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	if ( (unifile>=otind) || (otable[unifile].fileptr == UU_NULL) )
	{
		status = UX_BAD_DES;
		goto done;
	}

	if ( ( strcmp(otable[unifile].openfor,"w") == 0 ) ||
		( strcmp(otable[unifile].openfor,"a") == 0 ) )
	{
		/* not open for reading */
		status = UX_NO_ACCESS;
		goto done;
	}

	if ( strcmp(otable[unifile].format,"BLOCKED") == 0 )
	{
/*	long pos;*/
		for (i=0;i<sizeof(int);i++)
		{
/****temp Yurong

	pos = ftell(otable[unifile].fileptr);
	pos = fseek(otable[unifile].fileptr, pos, 0);
			UX_FSCANF0((otable[unifile].fileptr,"%c",&block.blenstr[i]), nr);
	pos = ftell(otable[unifile].fileptr);	
**********/
		if (ux_fread0(&block.blenstr[i],1,1,otable[unifile].fileptr,&nr) ==
			UX_FAILURE) goto failed;
		}
		blocklen = block.blen;
		if (nr == 0)
		{
			status = UX_EOF;	/* block length value wasn't read */
			goto done;
		}
		if (nr !=1) goto failed; 
		if (blocklen<0) goto failed; 

		/* else we got the block length okay, now read the block */
/*
......we changed here because we need check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....Yurong 11/1/02
*/
		if (*blocklenptr<blocklen)
		{
/*
.....no enough memory in rbuf to hold the data, so don't read
.....and give error and return;
*/
				status = UX_NO_SPACE;
				goto done;
		}
		else
			*blocklenptr = blocklen; /* return blocklen as written in file */
 		if (ux_fread0(rbuf, 1, blocklen, otable[unifile].fileptr,&nread)
			== UX_FAILURE) goto failed;
		else
		{
			if (nread == 0)	/* EOF during read of this block */
			{
				status = UX_EOF;
				goto done;
			}
			else
			{
				*nbsptr = nread;		/* return actual number bytes read */
				if ( nread < blocklen )	/* incomplete read of this block */
				{
					status = UX_NO_SPACE;
					goto done;
				}
			}
		}
	}	
	else
	{
		UX_ERROR0(9,printit);
		/* Unable to read a block from STREAM file. */
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
**    E_FUNCTION :  ux_write_block(unifile, wbuf, blocklen, nbsptr, options)
**			Attempts to write the buffer "wbuf" into the file associated
**			with "unifile". The file must be open for writing. No check
**			will be made to assure a text file is being written if text is
**			specified in the UNICAD file descriptor. This is an unformatted
**			write. 
**		PARAMETERS:
**			"unifile": Index to the UNICAD file descriptor from "ux_open".
**			"wbuf": buffer containing data to be written.
**			"blocklen": number of bytes in this block
**			"options": One of the following values:
**					UX_PRTERRS: print any errors as specified by uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as specified by uu_uerror
**						functions.
**					If incorrectly specified we will print errors.
**			OUTPUT:
**			"nbsptr": number of data bytes successfully written 
**		RETURNS:
**			UU_SUCCESS is returned if the write was accomplished with the
**			requested "blocklen" data items written; otherwise, one of the
**			following values is returned:
**				(these are all considered errors)
**				UX_BAD_DES: bad UNICAD file descriptor;
**				UX_NO_ACCESS: can't write to the file;
**				UX_BAD_WRITE: write error occurred;
**				UX_FAILURE: something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_write_block(unifile, wbuf, blocklen, nbsptr, options)
	int unifile;
	char *wbuf;
	int blocklen;
	int *nbsptr;
	int options;
{
	int status,i;			/* return status; either UU_SUCCESS or UX_FAILURE */
	int nwrit, np;
	UU_LOGICAL printit;
	union {
		int blen;
		char blenstr[sizeof(int)];
		}	block;

	uu_denter(UU_XTRC,(us,
		"ux_write_block(unifile:%d,wbuf:0x%x,blocklen:%d,?,options:%d)",
		unifile, wbuf, blocklen, options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	if ( (unifile>=otind) || (otable[unifile].fileptr == UU_NULL ))
	{
		status = UX_BAD_DES;
		goto done;
	}

	if ( strcmp(otable[unifile].openfor,"r") == 0 )
	{
		/* not open for writing */
		status = UX_NO_ACCESS;
		goto done;
	}

	block.blen = blocklen;

	if ( strcmp(otable[unifile].format,"BLOCKED") == 0 )
	{
		for (i=0;i<sizeof(int);i++)
		{
/****temp Yurong**
#if UU_COMP == UU_CIM
		if (ux_fwrite0(&block.blenstr[i],1,1,otable[unifile].fileptr,&np) ==
			UX_FAILURE) goto failed;
#else
		if (UX_FPRINTF0((otable[unifile].fileptr,"%c", block.blenstr[i]), np) ==
			UX_FAILURE) goto failed;
#endif
***********/
		if (ux_fwrite0(&block.blenstr[i],1,1,otable[unifile].fileptr,&np) ==
			UX_FAILURE) goto failed;
		}
		if (ux_fwrite0(wbuf,1,blocklen,otable[unifile].fileptr, &nwrit) 
			== UX_FAILURE) goto failed;
		else
		{
			*nbsptr = nwrit;
			if (nwrit != blocklen)
			{
				status = UX_BAD_WRITE;
				goto done;
			}
		}
	}
	else
	{
		UX_ERROR0(8,printit);
		/* "Unable to write block to STREAM file. */
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
**    E_FUNCTION :  ux_get_date(datstr,timstr)
**			Returns the current date (mm/dd/yy) and time (hh:mm) in separate
**       character strings.
**		PARAMETERS:
**			INPUT: none
**			OUTPUT:
**			    datstr   = Current date as 'mm/dd/yy'.
**			    timstr   = Current time as 'hh:mm'.
**		RETURNS: none
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
void ux_get_date(datstr,timstr)
char *datstr,*timstr;
{
	int year;
#ifdef UU_RS6000
	struct timeval timval;
#else
	time_t timval;
#endif
	struct tm *time_st;
/*
.....Get current date & time
*/
#ifndef UU_RS6000
	time(&timval);
#else
    ftime(&timval);
#endif
	time_st = localtime(&timval);
/*
.....Adjust for year 2000
*/
	year = time_st->tm_year;
	if (year < 70) year = year + 2000;
	else if (year > 99) year = year + 1900;
/*
.....Store date
*/
	sprintf(datstr,"%02d/%02d/%4d",(time_st->tm_mon)+1,time_st->tm_mday,year);
/*
.....Store time
*/
	sprintf(timstr,"%02d:%02d",time_st->tm_hour,time_st->tm_min);
}
