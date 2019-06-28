/*********************************************************************
**	FILENAME: ldir.c
**	CONTAINS:	getdef
**					setdef
**					ulf_get_base_fname
**					ul_get_base_fname
**					ul_get_flist
**     MODULE NAME AND RELEASE LEVEL 
**       ldir.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:10
*********************************************************************/

#include "usysdef.h"
#include "lumb.h"
#include "udebon.h"
#include "xenv1.h"
#include "nclfc.h"
#include "mfort.h"

#if (UU_COMP == UU_WIN2K)
#include <direct.h>
#endif

#if UU_COMP == UU_VAXVMS	/* if on the VAX: */
#include descrip
#include rmsdef
#else
#include "sys/types.h"
#if (UU_COMP != UU_WIN2K)
#if UU_COMP == UU_CIM
#include "sys/dir.h"
#else
#include "dirent.h"
#endif
#endif
#include "sys/stat.h"
#include "errno.h"
#endif

#if (UU_COMP == UU_WIN2K)
#include <string.h>
#define index strchr
#define rindex strrchr
#endif

#define TRACE UU_FALSE

/*********************************************************************
**    E_FUNCTION : getdef(def,isiz,*imax)
**			This function returns the default directory.  Fortran callable.
**    PARAMETERS   
**       INPUT  : 
**       	imax     = Size of 'def' array.
**       OUTPUT :  
**          def      = Default directory string.
**          isiz     = Length of 'def' string.
**    RETURNS: none
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
void getdef(def,isiz,imax)
UM_int4 *isiz,*imax;
UM_f77_str_ptr def;
{
#if UU_COMP == UU_WIN2K
#define LSEP '\\'
#else
#define LSEP '/'
#endif
	char *ldef;
	int i;
	ldef = UM_cstr_of_f77_str(def);
/*
.....Get the default directory
*/
	ul_get_full_dir(".",ldef);
#if UU_COMP == UU_WINNT
	if (ldef[0] == '/' && ldef[2] == '=')
	{
		char buf[UX_MAX_PATH_LEN];
		strcpy(buf,ldef);
		ldef[0] = ldef[1];
		ldef[1] = ':';
		strcpy(&ldef[2],&buf[3]);
	}
#endif
/*
.....Make sure directory is delimited
*/
	i = strlen(ldef);
	if (ldef[i-1] != LSEP)
	{
		ldef[i] = LSEP;
		ldef[i+1] = '\0';
	}
/*
.....Blank out rest of string
*/
	*isiz = strlen(ldef);
	for (i=*isiz;i<*imax;i++)
	{
		ldef[i] = ' ';
	}
}

/*********************************************************************
**    E_FUNCTION : setdef(def,nc)
**       This function sets the default directory.  It strips the
**       directory from the input file name.  If there is no directory,
**       then this routine does nothing.  Fortran callable.
**    PARAMETERS
**       INPUT  :
**          def      = Input part program name.
**          nc       = Number of chars in 'def'.
**       OUTPUT :
**          none.
**    RETURNS: none
**    SIDE EFFECTS: Changes the current directory.
**    WARNINGS: none
*********************************************************************/
void setdef(def,nc)
UM_f77_str_ptr def;
UM_int4 *nc;
{
	char *p,ldef[UX_MAX_PATH_LEN];
	char dir[UX_MAX_PATH_LEN],fname[UX_MAX_FILE_LEN];
	int i;
/*
.....Convert Fortran string to C string
*/
	p = UM_cstr_of_f77_str(def);
	strncpy(ldef,p,*nc);
/*
.....for WinNT, we allow filename with spaces, 
.....only remove trailling spaces for WinNT
.....Yurong 1/17/02
*/
#if UU_COMP!=UU_WIN2K
	i = *nc;
	ul_strip_blanks(ldef,&i);
#else
/*
.....we also need to remove preceding spaces
.....Yurong 3/1/02
*/
	for (i=0; i<strlen(ldef); i++)
	{
		if (ldef[i]!=' ') break;
	}
	strcpy(ldef, &(ldef[i]));
	for (i=strlen(ldef); i>0; i--)
	{
		if (ldef[i-1]==' ')
			ldef[i-1] = '\0';
		else
			break;
	}
#endif
/*
.....Break out the directory
*/
	ul_break_fname(ldef,dir,fname);
/*
.....Set the default directory
*/
	chdir(dir);
}

/*********************************************************************
**    E_FUNCTION : int ulf_get_base_fname(pathname, basename)
**			Fortran callable routine to return base filename.
**    PARAMETERS   
**       INPUT  : 
**          pathname  = Full pathname to extract base filename from.
**       OUTPUT :  
**          basename  = File base name.
**    RETURNS: none
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
void ulf_get_base_fname(pathname,nci,basename,nco)
char *pathname;
int *nci;
char *basename;
int *nco;
{
	UX_pathname fname;
	int args[2];
/*
.....Initialize routine
*/
	strncpy(fname,pathname,*nci);
	fname[*nci] = '\0';
	args[0] = args[1] = 1;
/*
.....Get base filename
*/
	ul_get_base_fname(fname,basename,args,UX_NPRTERRS);
	ul_remove_quotes(basename);
	*nco = strlen(basename);
}

/*********************************************************************
**    E_FUNCTION : int ul_get_base_fname(pathname, basename, args, options) 
**			This function returns strips off all directory sequences
**			of "pathname", and optionally the file type and version.
**    PARAMETERS   
**       INPUT  : 
**       pathname:		a quoted system dependent pathname (no symbols)
**	 args:			Argument list to show which part of the filename
**				to keep (args[n]=1 means to keep that portion).
**				1.	file type
**				2.	version
**	 options: 		A bitwise "OR" of one choice in each of the 
**				following categories:
**				1.	UX_PRTERRS: print any errors as specified by
**						uu_uerror functions.
**					UX_NPRTERRS: don't print any errors as 
**						specified by uu_uerror functions.
**				2.	UX_CHK: check the syntax of the path name.
**					UX_NCHK: don't check the syntax of the path
**						name.
**				3. UX_QUOTES: any output system dependent path is returned 
**						with UX_QUOTE_CHAR surrounding it.
**					UX_NQUOTES: any output system dependent path name is returned
**						with UX_QUOTE_CHAR surrounding it.
**			Incorrectly specified is the same as UX_PRTERRS | UX_NCHK | UX_QUOTES.
**       OUTPUT :  
**          basename		File base name.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ul_get_base_fname(pathname, basename, args, options)
	char *pathname;
	char *basename;
	int options;
	int args[];
{
	UX_pathname farea, fname;
	char *rindex();
	char *index();
	char *position;
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
/*
.....Check for valid pathname
*/
	status = UU_SUCCESS; /* assume success */
/*
.....Get rid of directory
*/
if (ux_decompose_path(pathname,farea,fname,options|UX_NCHK) != UU_SUCCESS)
		strcpy(basename, pathname);
	else
		strcpy(basename, fname);
/*
.....Get rid of extra spaces
*/
/*
.....For WinNT, we allowed name with spaces inside
.....Yurong 1/16/02
*/
#if UU_COMP!=UU_WIN2K
	position = index(basename,' ');
	if (position != UU_NULL) *position = '\0';
#endif
/*
.....Get rid of file type
*/
	if (args[0] != 1)
	{
		position = rindex(basename, '.');
		if (position != UU_NULL) *position = '\0';
	}
/*
.....Get rid of version number
*/
/*
.....For WinNT, we allowed long name (name with ';' inside)
.....Yurong 1/16/02
*/
#if UU_COMP!=UU_WIN2K
	if (args[1] != 1)
	{
		position = rindex(basename, ';');
		if (position != UU_NULL) *position = '\0';
	}
#endif
	goto done;
/*
.....Failure
*/
failed: status = UU_FAILURE;
#if (TRACE) 
	UL_IF_FAILURE_PRINT_IT
#endif
done:;
	return(status);
}
 
/****************************************************************
**	E_FUNCTION: ul_get_flist(pathname, filename, listptr, ftype, options)
**			Attempts to construct a list of all files with 'filename'
**			in the directory "pathname".
**		PARAMETERS:
**			"pathname": A full quoted system dependent path name 
**				to the file area containing the files to be listed.
**			"filename" Name of the file(s) to list.  Wildcards are
**			           allowed.
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
**			"ftype": 0 = Normal file , 1 = Directory , 2 = Executable
**					Do not specify a file type when ftype=1 or 2.
**		RETURNS:
**			UU_SUCCESS is returned if the list structure is filled
**			in; otherwise, one of the following values will be 
**			returned: (these are all considered errors)
**				UX_BAD_SUBJECT: "pathname" is bad;
**				UU_FAILURE: something else went wrong.
**		WARNINGS:
**			The application must delete the list when done with it
**			using a call to "uu_lsdel".
**		NOTE:
**			This routine was "borrowed" from ux_get_flist.
****************************************************************/
int ul_get_flist(pathname, filename, listptr, ftype, options)
	char *pathname;
	char *filename;
	char **listptr;
   int ftype;
	int options;
{
	int status;			/* return status; either UU_SUCCESS or UU_FAILURE */
	char *flist; 
	char *index();
	char *rindex();
	char *p;
	char *uu_lsnew();
	char *uu_lsinsrt();
	char *uu_lsdel();
	char *uu_lsnext();
	UX_pathname noquote,nofquote;
	static char buffv[UX_MAX_PATH_LEN];				/* holds return file name		*/
	static char buff2[UX_MAX_PATH_LEN];				/* holds wild card file name	*/
	char dirstr[UX_MAX_PATH_LEN];
	char exten[UX_SUFFIX_LEN],ext[UX_SUFFIX_LEN];

#if UU_COMP == UU_VAXVMS
	int statvax;
	static $DESCRIPTOR (wild,buff2);
	static $DESCRIPTOR (result,buffv);
	static int test[2];
	char *position;
#else
#if (UU_COMP == UU_IRIS4D) || (UU_COMP == UU_SUN) || (UU_COMP == UU_DECUNIX) || (UU_COMP == UU_HPUX) || (UU_COMP == UU_WINNT)
	DIR *dirptr;		/* routine opendir return pointer to dir info	*/
	struct dirent *dirbuf;	/* routine readdir returns file name info */
	int stats,match;
	struct stat st;
#endif
#if (UU_COMP == UU_WIN2K)
	int stats,match;
	struct stat st;
#endif
#if (UU_COMP == UU_CIM)
	DIR *dirptr;		/* routine opendir return pointer to dir info	*/
	struct direct *dirbuf;	/* routine readdir returns file name info */
	int stats,match;
	struct stat st;
#endif
#endif

	status = UU_SUCCESS; /* assume success */
/*
.....Remove quotes from pathname
*/
	strcpy (noquote,pathname);
	ul_remove_quotes(noquote);
/*
.....Initialize list package to receive filenames
*/
	uu_toolmalloc_init();
	flist = uu_lsnew();
	*listptr = flist;
#if UU_COMP==UU_WIN2K
	status = uwx_get_flist(pathname, 0, filename, &flist, (UX_NPRTERRS|UX_NCHK));
	return status;
#else
/*
.....Remove single filename from list
*/
	strcpy (nofquote,filename);
	do
	{
#if UU_COMP == UU_VAXVMS
		strcpy (buff2,noquote);
#else
		buff2[0] = '\0';
#endif
		strcat (buff2,nofquote);
		p = index(buff2,',');
		if (p == 0) nofquote[0] = '\0';
		else
		{
			strcpy (nofquote,p+1);
			*p = '\0';
		}
#if UU_COMP == UU_VAXVMS
		switch (ftype)
		{
		case 1:
			strcat (buff2,".DIR");
			break;
		case 2:
			strcat (buff2,".EXE");
			break;
		}

		for (i=strlen(buff2); i < UX_MAX_PATH_LEN; i++) buff2[i] = ' '; 
		test[0] = 0;
/*
.....Find all files that match "filename"
*/
		while ((statvax = lib$find_file(&wild,&result, test)) == RMS$_NORMAL )
		{
			flist = uu_lsinsrt(flist,UX_MAX_PATH_LEN);
			uu_move_byte(buffv, flist, UX_MAX_PATH_LEN);
		}
/*		lib$find_file_end (test);*/
#else	/* all other computers */
/*
.....Find all files that match "filename"
*/
		switch (ftype)
		{
		case 1:
		case 2:
			strcat (buff2,".*");
		}
#if UU_COMP != UU_WIN2K
		if ((dirptr=opendir(noquote)) == 0) goto failed;
#endif
		p = rindex (buff2,'.');
		if (p != 0)
		{	
			strcpy (exten,p+1);
			*p = '\0';
		}
		else
		{
			exten[0] = '\0';
		}
#if UU_COMP != UU_WIN2K
		while ((dirbuf = readdir(dirptr)) != UU_NULL)
		{
/*
.....Check for correct file name
.....Get file name & file type first
*/
			strcpy (buffv,dirbuf->d_name);
			strcpy (dirstr, dirbuf->d_name);
#endif
			p = index (buffv,'.');
			if (p == buffv) match = 0;
			else match = 1;
			if (p != 0)
			{
				strcpy (ext,p+1);
				*p = '\0';
			}
			else
			{
				ext[0] = '\0';
			}
/*
.....Compare file name
*/
			if (match == 1 && strcmp(buff2,"*") == 0 || strcmp(buff2,buffv) == 0)
			{
				match = 1;
			}
			else 
			{
				match = 0;
			}
			if (match == 1 && (strcmp(exten,"*") == 0 || strcmp(exten,ext) == 0))
			{
				match = 1;
			}
			else
			{
				match = 0;
			}
			if (match == 1 && ftype != 0)
			{
				strcpy (buffv,noquote);
				strcat (buffv,"/");
				strcat (buffv,dirstr);
#if (UU_COMP == UU_WINNT)
				stats = stat(buffv,&st);
#else
				stats = lstat(buffv,&st);
#endif
				if (stats == 0)
				{
					if (ftype == 1 && st.st_mode != (st.st_mode|S_IFDIR)) match = 0; 
#if (UU_COMP == UU_WINNT)
					else if (ftype == 2 && st.st_mode != (st.st_mode|S_IXUSR)) match = 0;
#else
					else if (ftype == 2 && st.st_mode != (st.st_mode|S_IEXEC)) match = 0;
#endif
				}
			}
			if (match == 1)
			{                    /* Insert name in list in alphabetical order */
				flist = *listptr;
				p = uu_lsnext(flist);
				while (p != UU_NULL && strcmp(dirstr,p) > 0)
				{
					flist = p;
					p = uu_lsnext(p);
					}
				flist = uu_lsinsrt(flist,UX_MAX_PATH_LEN);
				uu_move_byte(dirstr, flist, UX_MAX_PATH_LEN);
			}
		}
#if UU_COMP != UU_WIN2K
		closedir (dirptr);
#endif
#endif 
	} while (strlen(nofquote) != 0);
	if (flist == *listptr)
	{
		/* flist was never incremented for list insertions */
		uu_lsdel(*listptr);		/* delete empty list */
		uu_toolmalloc_term();
		*listptr = UU_NULL;
	}
	goto done;
failed: status = UU_FAILURE;
done:;
#endif
	return(status);
}	
