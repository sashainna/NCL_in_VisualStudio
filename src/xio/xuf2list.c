/**************************************************************************
**
**  NAME:	xuf2list.c 
**
**      contains:		uxu_list_files
**							uxu_load_file
**							uxu_list_archive
**							uxu_load_archive
**							uxu_create_dsfile
**							uxu_view_dsfile
**							uxu_modify_dsfile
**							ux_window_open
**							ux_udos_out
**
**  COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**   MODULE NAME AND RELEASE LEVEL 
**         xuf2list.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**         04/29/15 , 15:12:33
**************************************************************************/
#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"
#include "uims.h"
#include "dmark.h"
#include "derror.h"			/* needed for error system resolution */
#include "uhep.h"
#include "xenv1.h"			/* needed for data types */
#define UX_F2PGM
#include "xfsys2.h"			/* data structure for UX_descriptor */
#undef UX_F2PGM
#include "xfsys1.h"
#include "xfsys0.h"			/* define UX_IF_FAILURE_PRINT_IT   */
/*
.....Modified to correct compile errors of MPE source on VMS - RAZ
*/
#if UU_COMP == UU_VAXVMS
#include <types.h>
#include <stat.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include "go.h"
#include "dwindow.h"
#include "dasg.h"
#include "dasnog.h"
#include "dinput.h"
#include "gi1.h"			/* for Gpstat in gipick.h */
#include "gipick.h"		/* for Gpickid in gtblopst.h */
#include "usysg.h"
#include "gtblopst.h"
#include "gtblws.h"
#include "gtblst.h"
#include "gtblvar4.h"
#include "ginqdsiz.h"
#include "gcolors.h"         /* color definitions - ud_crwin */

/*********************************************************************
**    E_FUNCTION :  uxu_list_files(pathname, farea_flag, nbr_args, arglist)
**							Will list files (or subdirectories) of the types of
**							arglist
**    PARAMETERS   
**       INPUT  : 
**				"pathname": a system dependent pathname to a file area
**				"farea_flag": flag to specify if libraries or files will be
**						listed
**				"nbr_args":	an integer number of types of files to be listed
**				"arglist": list of types specified by suffix
**       OUTPUT :  
**				none
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int uxu_list_files(pathname, farea_flag, nbr_args, arglist)
	char *pathname;
	int farea_flag;
	int nbr_args;
	char *arglist[];
{
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */
	char *list;
	char *listhead;
	UX_pathname fname;
	UX_pathname basename;
	UX_pathname buff;		/* used as a message buffer */
	int i;

	uu_denter(UU_XTRC,(us,
		"uxu_list_files(pathname:%s,farea_flag:%d,arg_list,nbr_args )",
		pathname, farea_flag));
	status = UU_SUCCESS; /* assume success */

	/* check that pathname is a file area */
	if (ux_is_farea(pathname,UX_PRTERRS) != UU_SUCCESS)
	{
		UX_SPRINTF0((buff," Path is not a directory.\n"));
		ux_udos_out(buff,0);
		goto failed;
	}

	/* for each ftype we have been given, build and write a list: */
	for (i=0; i<nbr_args; i++)
	{
		/* build a list of filenames of this type from the directory given */
		if (ux_get_flist(pathname,farea_flag,arglist[i],&list,
			(UX_PRTERRS|UX_NCHK)) != UU_SUCCESS)
			goto failed;
		listhead = list;

		/* write out what is going to be listed: */
		if (farea_flag == UX_NFAREA)
			UX_SPRINTF0((buff," Files path %s:\n",pathname));
		else
			UX_SPRINTF0((buff," Libraries from area %s: \n", pathname));
		ux_udos_out(buff,0);

		if (list == UU_NULL)
		{
			/* there are no such files in this directory, show the empty list */
			status = UU_SUCCESS;
			ud_hakt(UX_UDOS, 1);
			/* message is: HIT ANY KEY TO CONTINUE. */
			goto done;
		}

		/* get file off the list of files: */
		while ((status = ux_nxt_file(&list,fname,UX_PRTERRS)) == UU_SUCCESS)
		{
			/* strip off anything but base name from "fname" */
			/* note that both names are unquoted */
			if (farea_flag == UX_FAREA)
			{
				if (ux_get_base_farea(fname, basename, (UX_PRTERRS|UX_NQUOTES|UX_NCHK)) 
				== UX_FAILURE) goto failed;
			}
			else
			{
				if (ux_get_base_fname(fname, basename, (UX_PRTERRS|UX_NQUOTES|UX_NCHK)) 
				== UX_FAILURE) goto failed;
			}

			/* write out to window contents of the list: */
			strcpy(buff,basename);
			ux_udos_out(buff,1);
		}
		if (status == UX_FAILURE)
			goto failed;
	
		/* else status is end-of-list reached */
		status = UU_SUCCESS;
		uu_lsdel(listhead);
	}

	ud_hakt(UX_UDOS, 1);
	/* message is: HIT ANY KEY TO CONTINUE. */

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION: int uxu_load_file(filearea, libname, filename, fenv, ftype, 
**												loadfunct )
**       This function loads a save file into UNIBASE. 
**    PARAMETERS   
**       INPUT: 
**				filearea			User's local file area; note, this can be a symbolic
**									name from the Unicad initialization file; this can
**									also be UU_NULL.
**				libname			Library (i.e. directory) name; can be a symbolic
**									name, or UU_NULL;
**				filename			Name of file to load; can not be symbolic.
**				fenv				Symbols that contains Udos file area and type
**									information, may also be UU_NULL
**				ftype				Symbol or quoted file type extension specifying
**									type to be loaded and extensions to add to file.
**				loadfunct		Function to use to load the files in a directory;
**									if nothing peculiar needs to be done, then this 
**									parameter may be UU_NULL; in this case the UNIBASE 
**									load (i.e. merge) function will be called and an 
**									error message will be printed if the UNIBASE load
**									function fails; if this is not what you want, then
**									supply our own function; note, this function MUST
**									RETURN UU_SUCCESS IF NOTHING WENT WRONG. This 
**									function should have a single parameter, the 
**									fullpath name to the file. No error messages are
**									printed in response to YOUR "loadfunct" if it does 
**									not return UU_SUCCESS.
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**					UX_BAD_FILE returned if file doesn't exist or isn't compatible.
**    SIDE EFFECTS: Error messages are printed only in the cases where a bad
**					path is discovered, or the UNIBASE load failed.
**    WARNINGS: none
*********************************************************************/
int uxu_load_file(filearea, libname, filename, fenv, ftype, loadfunct )
	char *filearea;
	char *libname;
	char *filename;
	char *fenv;
	char *ftype;
	int (*loadfunct)();
{
	char fullname[UX_MAX_PATH_LEN];
	int file_status;
	int status;
	int stat;
	
	uu_denter(UU_XTRC, (us, "uxu_load_file(%x,%x,%x,%x,%x,?)",
				 					filearea, libname, filename, fenv, ftype));
#ifdef UU_DEBUGON
	if (filearea != UU_NULL)
		uu_dprint(UU_XTRC,(us,"filearea:%s", filearea));
	if (libname != UU_NULL)
		uu_dprint(UU_XTRC,(us,"libname:%s", libname));
	if (filename != UU_NULL)
		uu_dprint(UU_XTRC,(us,"filename:%s", filename));
	if (fenv != UU_NULL)
		uu_dprint(UU_XTRC,(us,"fenv:%s", fenv));
	if (ftype != UU_NULL)
		uu_dprint(UU_XTRC,(us,"ftype:%s", ftype));
#endif

	status = UU_SUCCESS;	/* assume success */

	file_status = UX_EWRDOCEXF;		/* initialize mode to test status */
	/* get full path name and check file's status */

	if (ux_file_inquire(filearea, libname, filename, fenv, ftype,
			&file_status, &stat, fullname, UX_PRTERRS) != UU_SUCCESS) goto failed;

	if ((file_status == (file_status | UX_NEXISTS)) ||
		(file_status == (file_status | UX_FAREA)) || (stat == UX_NFOUND) )
	{
		/* then file does not exist, or not xio compatible, so can't load file */
		/* uu_uerror2(UB_SYMBOL, 48, fullname, "uxu_load_file"); */
		/* error is: File %s does not exist or is not compatible (%s). */
		status = UX_BAD_FILE;
		goto done;
	} 

	/* the path name to the file has been obtained; load the file into UNIBASE */
	if (loadfunct == UU_NULL) /* then load file into UNIBASE */
	{
		if (ur_lp02(fullname) != 0)
		{
			uu_uerror2(UX_UDOS, 15, fullname, "uxu_load_file");
			/* error message: Error in loading file: %s into UNIBASE (%s). */
			goto failed;
		}
	}
	else /* a load function was given, so use it */
	{
		if ((*loadfunct)(fullname) != UU_SUCCESS)
			goto failed; 
	}
	
	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION :  int uxu_list_archive(descriptor, size)
**       This function creates a udos window and lists the files in 
**			the archive specified by the elements of "descriptor"
**			in the window.
**    PARAMETERS   
**       INPUT  : 
**				descriptor		Array of file descriptors such that each 
**									entry has the following:
**						filearea		File area designation, can be a symbolic name from
**										the Unicad initialization file, a path name, or
**										UU_NULL.
**						libname		Archive (i.e. directory) name; can not be a 
**										symbolic name, but can be UU_NULL; no extension 
**										will be added to this according to the file type.
**						filetype		Udos file type to be listed.
**				size					Size of "filedescriptor".
**       OUTPUT : none.
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE 
**					otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int uxu_list_archive(descriptor, size)
	UX_descriptor descriptor[];
	int size;
{
	char *filearea;
	char *libname;
	int farea_flag;
	char *filetype;
	char fullname[UX_MAX_PATH_LEN];
							/* storage for a master symbol library name */
	int mode;
	char *arg_list[1];
	int cmdreject;
	int i;
	int status;			/* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "uxu_list_archive(descriptor:%x, size:%d)",
								descriptor, size));
#ifdef UU_DEBUGON
	if (descriptor[0].filearea != UU_NULL)
		uu_dprint(UU_XTRC,(us,"filearea:%s", descriptor[0].filearea));
	if (descriptor[0].libname != UU_NULL)
		uu_dprint(UU_XTRC,(us,"libname:%s", descriptor[0].libname));
	if (descriptor[0].filetype != UU_NULL)
		uu_dprint(UU_XTRC,(us,"filetype:%s", descriptor[0].filetype));
#endif
	status = UU_SUCCESS; /* assume success */

	/* archive name gotten; so open window to list it. */
	if (ux_window_open("UX") != UU_SUCCESS)
		goto failed;

	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);
	if (cmdreject) /* then kill window */
		goto done;

	for (i=0; i<size; i++) /* list each file type */
	{
		filearea = descriptor[i].filearea;
		libname = descriptor[i].libname;
		farea_flag = descriptor[i].farea_flag;
		filetype = descriptor[i].filetype;

		mode = 0;
		if (ux_mk_chk_syspath(filearea,libname, UU_NULL, UU_NULL, UU_NULL, &mode,
			fullname, UX_PRTERRS) != UU_SUCCESS)
		{
			uu_uerror2(UX_UDOS, 20, fullname, "uxu_list_archive");
			/* error is: No file area or archive found at %s (%s). */
			ud_hakt(UX_UDOS, 1);
			/* message is: HIT ANY KEY TO CONTINUE. */
		}
		if (mode != (mode | UX_FAREA))	/* then not a directory */
		{
			uu_uerror2(UX_UDOS, 20, fullname, "uxu_list_archive");
			/* error is: No file area or archive found at %s (%s). */
			ud_hakt(UX_UDOS, 1);
			/* message is: HIT ANY KEY TO CONTINUE. */
		}
		else
		{
			/* write to window */
			arg_list[0] = filetype;
			if (uxu_list_files(fullname, farea_flag, 1, arg_list) != UU_SUCCESS)	
				goto failed;
		}
	}
	
	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
/*	ud_kiwin(); /* kill the window */
	ul_close_window();
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION:int uxu_load_archive(filearea, libname, fenv, ftype, 
**														loadfunct ) 
**       This function loads an entire directory of files into UNIBASE.
**    PARAMETERS   
**       INPUT  :
**				filearea	User's local file area; note, this can be a symbolic
**									name from the Unicad initialization file.
**				libname			Library (i.e. directory) name; can not be a symbolic
**									name, but can be UU_NULL.
**				filename			Name of file to load; can not be symbolic.
**				fenv				File extension information, environmental variable.
**				ftype				Udos file type to be loaded; this is the extension
**									added to the file; e.g. "sy" added to symbol files;
**									"dw" is added to drawing files. Symbolic or quoted.
**				loadfunct		Function to use to load the files in a directory;
**									if nothing peculiar needs to be done, then this 
**									parameter may be UU_NULL; in this case the UNIBASE 
**									load (i.e. merge) function will be called and an 
**									error message will be printed if the UNIBASE load
**									function fails; if this is not what you want, then
**									supply our own function; note, this function MUST
**									RETURN UU_SUCCESS IF NOTHING WENT WRONG. This 
**									function should have a single parameter, the 
**									fullpath name to the file. No error messages are
**									printed in response to YOUR "loadfunct" if it does 
**									not return UU_SUCCESS.
**				yesnoptr			pointer to value UU_SUCCESS or UU_FAILURE, where
**									UX_FAILURE indicates users discontinued loading.
**    RETURNS: UU_SUCCESS if no problems (i.e. bugs) encountered, UX_FAILURE 
**					otherwise.
**    SIDE EFFECTS: Error messages are printed only in the cases where a bad
**					path is discovered, or the UNIBASE load failed.
**    WARNINGS: none
*********************************************************************/
int uxu_load_archive(filearea, libname, fenv, ftype, loadfunct, yesnoptr)
	char *filearea;
	char *libname;
	char *fenv;
	char *ftype;
	int (*loadfunct)();
	int *yesnoptr;
{
	char fullibname[UX_MAX_PATH_LEN];
	char filename[UX_MAX_PATH_LEN]; /* name of file to be loaded */
	char basename[UX_MAX_PATH_LEN];
	char *list = UU_NULL;
	char *listhead;
	int file_status;
	int i, mode;
	int cmdreject;
	int status;
	char msg[10000], *msg1, *msg2;

	uu_denter(UU_XTRC, (us, "uxu_load_archive(%x,%x,%x,%x,loadfunct:%x,?,?)", 
				 filearea, libname, fenv, ftype, loadfunct));
#ifdef UU_DEBUGON
	if (filearea != UU_NULL)
		uu_dprint(UU_XTRC,(us,"filearea:%s", filearea));
	if (libname != UU_NULL)
		uu_dprint(UU_XTRC,(us,"libname:%s", libname));
	if (fenv != UU_NULL)
		uu_dprint(UU_XTRC,(us,"fenv:%s", fenv));
	if (ftype != UU_NULL)
		uu_dprint(UU_XTRC,(us,"ftype:%s", ftype));
#endif

	status = UU_SUCCESS;	/* assume success */
	*yesnoptr = UU_SUCCESS;

	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);

	if (ux_mk_chk_syspath(filearea,libname,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,fullibname, UX_PRTERRS) != UU_SUCCESS)
		goto failed;

	if (fullibname != UU_NULL)
	{
		uu_dprint(UU_XTRC,(us,"fullibname:%s", fullibname));
	}
	else
	{
		uu_dprint(UU_XTRC,(us,"fullibname is UU_NULL"));
	}

	if (!cmdreject)	/* then no command reject encountered */
	{
		if (ux_get_flist(fullibname, UX_NFAREA, ftype, &list,
			(UX_PRTERRS|UX_NCHK)) != UU_SUCCESS)
			goto failed;
		listhead = list;
		if (list == UU_NULL)	/* the archive contains no files to load */
			goto done;

		while ( (status = ux_nxt_file(&list,filename,UX_PRTERRS)) == UU_SUCCESS)
		{	/* begin loading */

		if (filename != UU_NULL)
		{
			uu_dprint(UU_XTRC,(us,"filename:%s", filename));
		}
		else
		{
			uu_dprint(UU_XTRC,(us,"filename is UU_NULL"));
		}

			/* strip off anything to get a base name */
			/* note that both names are unquoted */
			if (ux_get_base_fname(filename, basename, (UX_PRTERRS|UX_NCHK)) 
				== UX_FAILURE) goto failed;

			if (uxu_load_file(UU_NULL, fullibname, basename, fenv, ftype,
				loadfunct) == UX_FAILURE)
			{
/*				uu_uerror2(UX_UDOS, 17, filename, "uxu_load_archive");
				/* error message: Error, file %s does not exist or is not 
				 * compatible (%s). */
/*				if (!ud_lyesno(UX_UDOS, 3)) 
				/* prompt is: Ignore this file and continue loading other 
				 * files? */
				msg1 = (char*)uu_uprompt2(UX_UDOS, 17, filename, 
								"uxu_load_archive");
				msg2 = (char*)uu_uprompt0(UX_UDOS, 3);
				sprintf(msg, "%s\n%s",msg1, msg2);
				if (!ud_yesno(0, msg, "Question?"))
				{
					*yesnoptr = UX_FAILURE;
					goto done;
				}
			}	
		}	/* end loading */
		if (status == UX_FAILURE)
			goto failed;
		else /* status is end-of-file-list reached */
		{
			status = UU_SUCCESS;
			uu_lsdel(listhead);
		}
	}	/* end no command reject */
	else /* command reject hit */
		goto failed;

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}	
/**************************************************************************
**
**  E_FUNCTION:  ux_udos_out
**      write a string from the file management routines to the output
**      device.  If the screen is full wait for the user to hit a key
**			to continue
**
**  PARAMETERS   
**      INPUT  :  buff	:	string to be written out
**						start	:	if == 0 reset line counter
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
ux_udos_out(buff, start)
	char *buff;						/* string to be output							*/
	int start;						/*	if == 0 a new command, reset nlines		*/
{
	char buff2[250];				/* output buffer									*/
	static int nlines = {0};	/* number of lines from current command	*/
/*
** write the string to the current output device,  keep track of the number
**	of lines and wait for user to hit a key when screen if full from one
**	command
** 
*/
	uu_denter( UU_XTRC,(us,"ux_udos_out(%.80s %d)", buff, start));

	if (start == 0)
		nlines = 1;
	else
		nlines++;

	if (nlines == UX_windrows)
	{

	/* prompt user to hit any key to continue */
/*		ud_hakt(UX_UDOS, 1);*/
		/* message is: HIT ANY KEY TO CONTINUE. */
		nlines = 1 ;
	}

	/* add spaces at front to make it easier to read */
	strcpy(buff2, "     ");
	strcat(buff2, buff);

	/* add carriage return */
/*	strcat (buff2, "\n");*/

	/* output line to window */
/*	ud_wrwin(buff2);*/
	ul_win_out(buff2,0);

	uu_dexit;
}
/*********************************************************************
**    E_FUNCTION:int ux_window_open(window_type)
**      This function opens a window for output.
**    PARAMETERS   
**       INPUT  :
**				window_type		"UD"= DAS window; "UX"=udos window.
**       OUTPUT : none.
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_window_open(window_type)
  char window_type[3];
{
	Gdrect winbnd;		 		/* help window bounds, lower left and upper right */
	int *rowptr;  				/* row, column pointers to global variables */
	int *colptr; 
	int status;
	UD_AREA *areapt;			/* pointer to an array of areas */
	int bckgrnd;				/* bckgrnd color of ansi window */
	UD_WINDOW_REC wcb;		/* window control block */
	int args[2];

	uu_denter(UU_XTRC,(us, "ux_window_open(window_type:%s)", window_type));
	status = UU_SUCCESS;	/* assume success */

	/*	-- open a window -- */
	/* note, "UD_winrow" is the number of rows in a window, and
	 * "UD_wincol" is the number of columns in a window. Both are defined
	 * in dasnog.h. Similarly, for UX_windowrows, and UX_windwidth */
	if (strcmp(window_type, "UD") == 0) /* then use DAS window */
	{
		rowptr = &UD_winrow;
		colptr = &UD_wincol;
	}
	else /* use udos globals */
	{
		rowptr = &UX_windrows;
		colptr = &UX_windwidth;
	}

/*	-- get terminal graphic area bounds -- */

	/* If no help area in this screen, then use graphics area	*/
	if (UD_duimsdeflt.screen[UD_curlayout.curr_screen].noareas[UD_HLP]==0)
	 areapt= &UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_GRAF][0];
	else	/* Use help  area	*/
	 areapt= &UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_HLP][0];

/*	bckgrnd = dqwinback();
/*	ud_initwin_rec(&wcb, &((*areapt).posn), UG_C_WHITE, bckgrnd);
/*	ud_crwin(&wcb, rowptr, colptr);*/
	args[1] = 1;
	*rowptr = 20; *colptr = 80;
	ul_open_window(*rowptr,*colptr,args);

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION : int uxu_create_dsfile(area,library,name)
**       Create a master symbol text description file.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS   : UU_SUCCESS if no problems encountered, UX_FAILURE
**                otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uxu_create_dsfile(farea,libname,symname)
	char *farea;
	char *libname;
	char *symname;
{
#if UU_COMP == UU_VAXVMS
	char *getenv();
#else
#if UU_COMP == UU_RIDGE || UU_COMP == UU_WIN2K
	char *getcwd();
#else
	char *getwd();
#endif
#endif
	char *chrs;
	UX_pathname cdir, editdir, editfile, fullname;
	char *index();
	int dsfile;			/* index into the udos open file table used for .ds file */
	int comfile;		/* index into the udos open file table used for script */
	UX_pathname comstring, fullsym;
	char *ux_getenv();
	char *editor;
	int nout;
	int fmode;
	int istat;
	int status = UU_SUCCESS;
	struct stat stbuf;

	uu_denter(UU_XTRC,(us,"uxu_create_dsfile(farea:%x,libname:%x,symname:%x)",
		farea,libname,symname));

	/* compose the complete file name and create (no header) and open file */
	fmode = UX_EXISTS; /* work is done only in local area, make full path */
	if (ux_mk_chk_syspath(farea,libname,symname,"UB_SYM_EXTEN",
		"UB_DESC_SUFFIX",&fmode,fullname,UX_PRTERRS) != UU_SUCCESS)
		goto failed;

	if ((fmode == (fmode|UX_NEXISTS)) || (fmode ==(fmode|UX_FAREA)))
		istat = ux_create_file(fullname,0644,UU_NULL,"STREAM","ASCII",
			"UX_NOHEADER", &dsfile, UX_PRTERRS);
	else
		istat = ux_open(fullname,"r+","STREAM","ASCII",&dsfile,UX_PRTERRS);
	if (istat != UU_SUCCESS) 
		goto failed;

	/*gather size info and write out string to file, close it */
	fmode = UX_EXISTS;
	if (ux_mk_chk_syspath(farea,libname,symname,"UB_SYM_EXTEN",
		"UB_SYM_SUFFIX",&fmode, fullsym, UX_PRTERRS|UX_NQUOTES) != UU_SUCCESS)
		goto failed;
	if  (stat(fullsym, &stbuf) != 0)
	{
		/* can't find .sy file  or stat call failed */
		uu_dprint(UU_XTRC,(us,"STAT call failed, unable to get file size."));
	}
	else
	{
		UX_FPRINTF0((otable[dsfile].fileptr,
		"The symbol file %s is %8ld characters.\n",symname,stbuf.st_size),nout);
	}
	ux_close(dsfile,UX_PRTERRS);

	/*compose name of command file and create it (no header) and open it */
	istat = ux_create_file("dscom",0700,UU_NULL,"STREAM","ASCII","UX_NOHEADER",
		&comfile,UX_PRTERRS);
	if (istat == UX_FOUND)	/* old one exists, delete it */
	{
		ux_delete("dscom", UX_PRTERRS);
		istat = ux_create_file("dscom",0644,UU_NULL,"STREAM","ASCII",
			"UX_NOHEADER", &comfile,UX_PRTERRS);
		if ( istat != UU_SUCCESS ) 
			goto failed;
	}
	/*write out the command lines, close it */
	UX_FPRINTF0((otable[comfile].fileptr, "#\n"),nout);
	UX_FPRINTF0((otable[comfile].fileptr,
		"# Command file to edit symbol description text file %s \n",
		symname),nout);

	if ((editor = ux_getenv("UU_EDITOR",UX_PRTERRS|UX_NQUOTES)) == UU_NULL)
	{
		uu_uerror0(UB_SYMBOL,110);
		goto failed;
	}

	/* decompose "fullname" into full path to library (with its extentsion
	and the file name with its extension */
	istat = ux_decompose_path(fullname,editdir,editfile,
		(UX_PRTERRS|UX_NCHK|UX_NQUOTES));
	if (istat!=UU_SUCCESS) 	/* a farea wasn't successfully split off */
	{
		goto failed;
	}

#if UU_COMP == UU_VAXVMS
	strcpy(comstring,"SET DEF ");
#else
	strcpy(comstring,"cd ");
#endif
	strcat(comstring,editdir);
	strcat(comstring,"\n");
	ux_fputs0(comstring,otable[comfile].fileptr);

	strcpy(comstring,editor);
	strcat(comstring," ");
	strcat(comstring,editfile);
	strcat(comstring,"\n");
	ux_fputs0(comstring,otable[comfile].fileptr);

#if UU_COMP == UU_VAXVMS
	strcpy(comstring,"SET DEF ");
#else
	strcpy(comstring,"cd ");
#endif
#if UU_COMP == UU_VAXVMS
	chrs = getenv("PATH");
	strcat(comstring,chrs);
#else
#if UU_COMP == UU_RIDGE || UU_COMP==UU_WIN2K
	chrs = getcwd(cdir,UX_MAX_PATH_LEN);
	strcat(comstring,chrs);
#else
	getwd(cdir);
	strcat(comstring,cdir);
#endif
#endif
	strcat(comstring,"\n");
	ux_fputs0(comstring,otable[comfile].fileptr);

	ux_close(comfile,UX_PRTERRS);

	/* handle command rejects */

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION : int uxu_view_dsfile(area,library,name)
**       View a master symbol text description file in a window.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS   : UU_SUCCESS if no problems encountered, UX_FAILURE
**                otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uxu_view_dsfile(farea,libname,symname)
	char *farea;
	char *libname;
	char *symname;
{
	UX_pathname fullname;
	int dsfile;			/* index into the udos open file table used for .ds file */
	int fmode;
	int istat = UU_SUCCESS;
	int status = UU_SUCCESS;
	int cmdreject;
	FILE *rdfile;
	char buff[250];

	uu_denter(UU_XTRC,(us,"uxu_view_dsfile(farea:%x,libname:%x,symname:%x)",
		farea,libname,symname));

	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);

	/* compose the complete file name and open the file */
	fmode = UX_EXISTS; /* work is done only in local area, make full path */
	if (ux_mk_chk_syspath(farea,libname,symname,"UB_SYM_EXTEN",
		"UB_DESC_SUFFIX",&fmode,fullname,UX_PRTERRS) != UU_SUCCESS)
		/* unable to get the file */
		goto failed;
	if ((fmode == (fmode|UX_NEXISTS)) || (fmode ==(fmode|UX_FAREA)))
		/* the file doesn't exist */
		goto failed;
	else
		istat = ux_open(fullname,"r+","STREAM","ASCII",&dsfile,UX_PRTERRS);
	if (istat != UU_SUCCESS) 
		/* unable to open the file */
		goto failed;

	/* file name found and opened, so open window to print it to. */
	if (ux_window_open("UX") != UU_SUCCESS)
	{
		ux_close(dsfile,UX_PRTERRS);
		goto failed;
	}

	if (!cmdreject) /* then kill window */
	{
		/* get file descriptor */
		if (ux_get_os_filedesc(dsfile,&rdfile,UX_PRTERRS) != UU_SUCCESS)
		{
/*			ud_kiwin(); /* kill the window */
			ul_close_window();
			ux_close(dsfile,UX_PRTERRS);
			goto failed;
		}
		/* write to window */
		strcpy(buff," ");
		ux_udos_out(buff,0);
		while (ux_fgets(buff,250,rdfile)==UU_SUCCESS)
		{
			/* I think there is one \n too many! */
			ux_udos_out(buff,1);
		}
		ux_udos_out(buff,1);

		ud_hakt(UX_UDOS, 1);
		/* message is: HIT ANY KEY TO CONTINUE. */
	}

/*	ud_kiwin(); /* kill the window */
	ul_close_window();
	ux_close(dsfile,UX_PRTERRS);

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION : int uxu_modify_dsfile(area,library,name)
**       Modify a master symbol text description file.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS   : UU_SUCCESS if no problems encountered, UX_FAILURE
**                otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uxu_modify_dsfile(farea,libname,symname)
	char *farea;
	char *libname;
	char *symname;
{
#if UU_COMP == UU_VAXVMS
	char *getenv();
#else
#if UU_COMP == UU_RIDGE || UU_COMP==UU_WIN2K
	char *getcwd();
#else
	char *getwd();
#endif
#endif
	char *chrs;
	UX_pathname cdir, editdir, editfile, fullname;
	int comfile;		/* index into the udos open file table used for script */
	UX_pathname comstring;
	char *ux_getenv();
	char *editor;
	int nout, fmode, istat;
	int status = UU_SUCCESS;

	uu_denter(UU_XTRC,(us,"uxu_modify_dsfile(farea:%x,libname:%x,symname:%x)",
		farea,libname,symname));

	/* compose the complete file name */
	fmode = UX_EXISTS; /* work is done only in local area, make full path */
	if (ux_mk_chk_syspath(farea,libname,symname,"UB_SYM_EXTEN",
		"UB_DESC_SUFFIX",&fmode,fullname,UX_PRTERRS) != UU_SUCCESS)
		goto failed;
	if ((fmode == (fmode|UX_NEXISTS)) || (fmode ==(fmode|UX_FAREA)))
		/* the file doesn't exist, send a message and quit */
		goto failed;

	/*compose name of command file and create it (no header) and open it */
	istat = ux_create_file("modcom",0700,UU_NULL,"STREAM","ASCII","UX_NOHEADER",
		&comfile,UX_PRTERRS);
	if (istat == UX_FOUND)	/* old one exists, delete it */
	{
		ux_delete("modcom", UX_PRTERRS);
		istat = ux_create_file("modcom",0644,UU_NULL,"STREAM","ASCII",
			"UX_NOHEADER", &comfile,UX_PRTERRS);
		if ( istat != UU_SUCCESS ) 
			goto failed;
	}
	/*write out the command lines, close it */
	UX_FPRINTF0((otable[comfile].fileptr, "#\n"),nout);
	UX_FPRINTF0((otable[comfile].fileptr,
		"# Command file to modify symbol description text file %s \n",
		symname),nout);

	if ((editor = ux_getenv("UU_EDITOR",UX_PRTERRS|UX_NQUOTES)) == UU_NULL)
	{
		uu_uerror0(UB_SYMBOL,110);
		goto failed;
	}

	/* decompose "fullname" into full path to library (with its extentsion
	and the file name with its extension */
	istat = ux_decompose_path(fullname,editdir,editfile,
		(UX_PRTERRS|UX_NCHK|UX_NQUOTES));
	if (istat!=UU_SUCCESS) 	/* a farea wasn't successfully split off */
	{
		goto failed;
	}

#if UU_COMP == UU_VAXVMS
	strcpy(comstring,"SET DEF ");
#else
	strcpy(comstring,"cd ");
#endif
	strcat(comstring,editdir);
	strcat(comstring,"\n");
	ux_fputs0(comstring,otable[comfile].fileptr);

	strcpy(comstring,editor);
	strcat(comstring," ");
	strcat(comstring,editfile);
	strcat(comstring,"\n");
	ux_fputs0(comstring,otable[comfile].fileptr);

#if UU_COMP == UU_VAXVMS
	strcpy(comstring,"SET DEF ");
#else
	strcpy(comstring,"cd ");
#endif
#if UU_COMP == UU_VAXVMS
	chrs = getenv("PATH");
	strcat(comstring,chrs);
#else
#if UU_COMP == UU_RIDGE || UU_COMP==UU_WIN2K
	chrs = getcwd(cdir,UX_MAX_PATH_LEN);
	strcat(comstring,chrs);
#else
	getwd(cdir);
	strcat(comstring,cdir);
#endif
#endif
	strcat(comstring,"\n");
	ux_fputs0(comstring,otable[comfile].fileptr);

	ux_close(comfile,UX_PRTERRS);

	/* handle command rejects */

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION : int ux_fix_ds_text(filename)
**			After renames and such, the infor in the text of the description
**			file should be fixed up to show the new name.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS   : UU_SUCCESS if no problems encountered, UX_FAILURE
**                otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ux_fix_ds_text(filename)
	char *filename;
{
	int dsfile;			/* index into the udos open file table used for .ds file */
	int istat;
	int nout;
	char buff[16];		/* buffer to receive first text in the file to compare */
	UX_pathname symname;	/* to receive to basic symbol name fromthe pathname */
	int status = UU_SUCCESS;

	uu_denter(UU_XTRC,(us,"ux_fix_ds_text(filename:%s)",filename));

	istat = ux_open(filename,"r+","STREAM","ASCII",&dsfile,UX_PRTERRS);
	if (istat != UU_SUCCESS) 
		goto failed;

	/* scan the file for the first sentence, the header text */
	ux_fgets0(buff, 16, otable[dsfile].fileptr);
	/* compare the header to text to the standard */
	if (strcmp(buff,"The symbol file") == 0)
	{
		/* get the symbol new name not the fullpathname */
		if (ux_get_base_fname(filename,symname,UX_PRTERRS)!=UU_SUCCESS)
			goto failed;

		/* reset the pointer for input/output to beginning of file */
		rewind(otable[dsfile].fileptr);

		/* overwrite using the new name of the corresponding symbol */
		UX_FPRINTF0((otable[dsfile].fileptr, "The symbol file %s ",symname),nout);
	}
	ux_close(dsfile,UX_PRTERRS);

	/* handle command rejects */

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
