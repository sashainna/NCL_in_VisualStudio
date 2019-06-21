/*********************************************************************
**    NAME:  xuf2libs.c
**       CONTAINS:
**                       uxu_ask_for_lib
**                       uxu_ask_for_area
**                       uxu_ask_for_target_lib
**                       uxu_lib_name_ok
**                       uxu_create_lib
**                       uxu_delete_lib
**                       uxu_list_area
**                       uxu_rename_lib
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**             xuf2libs.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**             12/02/15 , 09:14:50
*******************************************************************/
#include "usysdef.h"            /* for UU_REAL, etc. */
#include "ustdio.h"
#include "uhep.h"                       /* for error system */
#include "udebug.h"             /* for debugging trace facility */
#include "dmark.h"              /* for UD_MARK */
#include "dasnog.h"       /* for DAS */
#include "dinput.h"       /* for DAS input types; e.g. UD_STRING */
#include "xenv1.h"
#include "xfsys1.h"
#include "xfsys2.h"
#include "xfsys0.h"
#include "udforms.h"
#include "udfdata.h"


#define TRACE UU_TRUE  /* for debugging only */
static int lib_direct = 0;
static UX_pathname def_syslib, def_loclib, S_ftpe, S_fenv;
/*********************************************************************
**    S_FUNCTION     :  ux_browse_libfile(fieldno, val, stat)
**       Method called at 'Setup file browser' toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT ux_browse_libfile(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname symlib, tempstr, dirname, dir, lname;
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	int len;
	int hdrfound,mode,numint,status;
	UD_DDATA data;
/*
.....Get the material name
*/
	data.frmstr = tempstr;
	ud_get_field(*fieldno+1, data, UU_FALSE);
	strcpy(symlib, tempstr);
	len = strlen(symlib);
	ul_strip_blanks(symlib,&len);

	sprintf(paths, "%s;%s", def_loclib, def_syslib);
	strcpy(path_des, "Local;System");
	dirname[0] = '\0';

	if (symlib[0]!='\0')
	{
		strcpy(dirname, symlib);
		ul_break_fname(dirname, dir, lname);
		len = strlen(dirname);
		if ((len>=2)&&(dirname[len-2]=='_')&&(dirname[len-1]=='S'))
		{
			dirname[len-2] = '\0';
		}
		if (dir[0]=='\0')
		{
			status = ux_file_inquire(def_loclib, dirname, UU_NULL, 
						S_fenv, S_ftpe, &mode, 
						&hdrfound, dirname, UX_PRTERRS);  
			ul_remove_quotes(dirname);
			len = strlen(dirname);
		}
		else
		{
			strcat(dirname, "_S");
			len = strlen(dirname);
		}
	}
	ud_get_dirname1("Symbol Library", "Symbol Library", dirname, &len, paths, path_des);
	if ((len>=2)&&(dirname[len-2]=='_')&&(dirname[len-1]=='S'))
	{
		dirname[len-2] = '\0';
		len -= 2;
	}
	if (len>0)
	{
		ud_update_answer(*fieldno+1, (int *)dirname);
	}
	else
		*fieldno = -1;
	return(UD_FLDOK);
}
/*********************************************************************
**       E_FUNCTION :int uxu_ask_for_lib
**                               (libdataptr,prompt,fullname,numintptr,file_statusptr) 
**              This function obtains the name of a library     from the user.
**       PARAMETERS     
**               INPUT  : 
**.....we changed using browser, so prompt is meaningless now
**.....Yurong
**                      prompt                  Prompt to be displayed to the user.
**               OUTPUT :
**                      fullname                        Full path name to the requested library.
**                      numintptr               Pointer to the number of user interactions.
**                      file_statusptr Pointer to the file status
**       RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**                               If a directory is not found then UX_FAILURE is returned.
**       SIDE EFFECTS: Prints errors if error in path syntax, or no library found.
**       WARNINGS: none
*********************************************************************/
int uxu_ask_for_lib(libdataptr,prompt,fullname,numintptr,file_statusptr)
	UX_libdata_bag *libdataptr;
	char *prompt;
	char fullname[];
	int *numintptr;
	int *file_statusptr;
{
	UX_pathname libname;
	UX_pathname path_prefix;
	UU_LOGICAL default_flag;
	UU_LOGICAL cmdreject;
	UU_LOGICAL nameok;
	UX_pathname dir, fname;
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	int numint, len;
	int mode, hdrfound;
	int status = UU_SUCCESS;
	libname[0] = '\0';
	if (libdataptr->default_firstim) /* no previous archive */
		default_flag = UD_NODEFAULT;
	else
	{
		if (strcmp(libdataptr->default_area, "system") == 0)
		{
			strcpy(libname, libdataptr->default_lib);
		}
		else
		{
			strcpy(libname, libdataptr->default_lib);
		}
		default_flag = UD_DEFAULT;
	}
	ul_remove_quotes(libname);
	sprintf(paths, "%s;%s", libdataptr->loc_farea, libdataptr->sys_farea);
	strcpy(path_des, "Local;System");
	len = strlen (libname);
	ud_get_dirname1(prompt, "Symbol Library", libname, &len, paths, path_des);


	*numintptr = strlen(libname);
	if (*numintptr <= 0) goto done;

	if (libname[*numintptr-1]==UX_PATH_SEP)
	{
		libname[*numintptr-1] = '\0';
		*numintptr = *numintptr -1;
	}
/*
.....remove "_S" in library name if have any
.....because use may use it or not use it.
.....for symple way, we just remove it
.....Yurong 9/18/98
*/
	if (*numintptr>=2)
	{
		if ((libname[*numintptr-2]=='_')&&(libname[*numintptr-1]=='S'))
		{
			libname[*numintptr-2] = '\0';
			*numintptr = *numintptr - 2;
		}
	}
			
	strcpy(libdataptr->default_lib, libname);
	strcpy(libdataptr->default_area, "local");
	libdataptr->default_firstim = UU_FALSE;

	if (strcmp(libdataptr->default_area, "local") == 0)
		strcpy(path_prefix, libdataptr->loc_farea);
	else
		strcpy(path_prefix, libdataptr->sys_farea);

	mode = UX_READ | UX_WRITE; /* check for read and write access */

	ul_break_fname(libname, dir, fname);
	if (dir[0]!='\0')
	{
		status = ux_file_inquire(UU_NULL, libname, UU_NULL, libdataptr->fenv,
						libdataptr->ftype, &mode, &hdrfound, fullname,
						UX_PRTERRS);                                            
	}
	else
	{
		status = ux_file_inquire(path_prefix, libname, UU_NULL, 
						libdataptr->fenv, libdataptr->ftype, &mode, 
						&hdrfound, fullname, UX_PRTERRS);                                               
	}
	if (status!= UU_SUCCESS)
	{
		uu_uerror2(UX_UDOS, 27, libdataptr->pstring, "ux_ask_for_lib");
		goto failed;
	}

		/* continue only if 1) a directory was found, or 2) nothing was found */
	if ((mode != (mode|UX_FAREA)) && (mode != (mode|UX_NEXISTS))) 
	{  
		uu_uerror3(UX_UDOS,26,libdataptr->pstring,fullname,"ux_ask_for_lib");
			/* error message: No %s library found at %s  (%s). */
		goto failed;
	}

		/* reset default library name for subsequent library references */
		/* only if there is such a library found */
	if (mode == (mode|UX_FAREA))
	{
		libdataptr->default_firstim = UU_FALSE;
		strcpy(libdataptr->default_lib, libname);/* only what user supplied*/
	}

	if (mode == (mode | UX_FAREA)) /* then the path name is to a directory */
		*file_statusptr = UX_FAREA;
	else /* not a directory */
		*file_statusptr = UX_NEXISTS;

	goto done;

failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dprint(UU_XTRC, (us,"fullname:%s, status:%d", fullname, status));
	uu_dexit;
	return(status);
}  

/*********************************************************************
**              E_FUNCTION: int uxu_ask_for_area(libdataptr,area, fullname, numintptr)
**               This function asks the user for a archive area.
**               Currently, there are only 2 areas: a user's local area
**               where the users may both read and write from archives,
**               and a system  area where the user can only read.  The paths to
**               these 2 areas are defined in the ".init" startup file.
**       PARAMETERS     
**               INPUT  : none.
**               OUTPUT :
**               area                           Symbolic name of area.
**               fullname                Full path name to the area.
**               numintptr              Pointer to the number of appropriate user interactions; 
**                                                       either 0 or 1.
**       RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**       SIDE EFFECTS: 
**       WARNINGS: none
*********************************************************************/
int uxu_ask_for_area(libdataptr, area, fullname, numintptr)
	UX_libdata_bag *libdataptr;
	UX_pathname area;
	UX_pathname fullname; /* storage for a library name */
	int *numintptr;
{
	UU_LOGICAL cmdreject;
	static char string[3] = "l";/* for user input to determine whether we are 
										  * to list the local area or the system area */
	int status;
	int mode, hdrfound;
	
	uu_denter(UU_XTRC, (us, "uxu_ask_for_area(?,?,?)"));
	status = UU_SUCCESS; /* assume success */

	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject) /* then no command reject encountered */
	{
		ud_ldas(UD_DASSTRING, UX_UDOS, 10, string, 2, numintptr, UD_DEFAULT);
		/* prompt is: Enter l to list local area; s to list system area */
		if (*numintptr <= 0)
			goto done;

		if (strcmp(string, "l") == 0) /* then list local area */
			strcpy(area, libdataptr->loc_farea);
		else if (strcmp(string, "s") == 0)
			strcpy(area, libdataptr->sys_farea);
		else /* no appropriate choice */
			goto done;

		/* get area fullpath name */
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire(area, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
			&mode, &hdrfound, fullname, UX_PRTERRS) != UU_SUCCESS) goto failed;

		if (mode != (mode|UX_FAREA)) /* then didn't get a directory */
		{
			uu_uerror3(UX_UDOS,28,libdataptr->pstring,fullname,"uxu_ask_for_area");
			/* error is: %s archive area, %s, is not a directory  (%s). */
			goto failed;
		}
	}
	else /* command reject hit */
		*numintptr = 0;

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}  

/*********************************************************************
**       E_FUNCTION :  int uxu_create_lib(libdataptr)
**               This function creates a library.
**       PARAMETERS     
**               INPUT  : none.
**               OUTPUT : none. 
**       RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**       SIDE EFFECTS: none
**       WARNINGS: none
*********************************************************************/
int uxu_create_lib(libdataptr)
	UX_libdata_bag *libdataptr;
{
	UX_pathname libname;
	UX_pathname fullname;
	UX_pathname path_prefix;
	char *prompt, *uu_uprompt0();
	UU_LOGICAL nameok;
	int mode, hdrfound;
	int numint;              /* number of appropriate user interactions */
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UX_FAILURE */
	UX_pathname dir, fname;
	int *ans[3];
	int option;
	static int sav_dir = 0;
	UD_FSTAT uj_noop();
	static UD_METHOD methods[2]  = {
			ux_browse_libfile, uj_noop};
	static char called[] = {6,6,6};
	
	uu_denter(UU_XTRC, (us, "uxu_create_lib()"));

	option = 0;
	libname[0] = '\0';
	lib_direct = sav_dir;
	ans[0] = (int *)&option;
	ans[1] = (int *)libname;
	strcpy(def_syslib, libdataptr->sys_farea);
	strcpy(def_loclib, libdataptr->loc_farea);
	strcpy(S_ftpe, libdataptr->ftype);
	strcpy(S_fenv, libdataptr->fenv);
	status = ud_form1("xgetlib.frm", ans, ans, methods, called, NULL, NULL);
	if (status==-1)
	{
		status = UU_SUCCESS;
		goto done;
	}
	numint = strlen(libname);
	if (numint <= 0) goto done;
/*
......if the last character is '/', remove it.
*/
	if (libname[numint-1]=='/')
	{
		libname[numint-1] = '\0';
		numint = numint -1;
	}
/*
.....remove "_S" in library name if have any
.....because use may use it or not use it
.....for symple way, we just remove it
.....Yurong 9/18/98
*/
	if (numint>=2)
	{
		if ((libname[numint-2]=='_')&&(libname[numint-1]=='S'))
		{
			libname[numint-2] = '\0';
		}
	}
	strcpy(libdataptr->default_lib, libname);
	libdataptr->default_firstim = UU_FALSE;

	mode = UX_READ | UX_WRITE; 
/*
......before we cat path_prefix to libname, we need check if
......libname already have a path
......Yurong changed 9/17/98
*/
	ul_break_fname(libname, dir, fname);
	if (dir[0]!='\0')
	{
		status = ux_file_inquire(UU_NULL, libname, UU_NULL, libdataptr->fenv,
						libdataptr->ftype, &mode, &hdrfound, fullname,
						UX_PRTERRS);                                            
	}
	else
	{
		status = ux_file_inquire(libdataptr->loc_farea, libname, UU_NULL, 
						libdataptr->fenv, libdataptr->ftype, &mode, 
						&hdrfound, fullname, UX_PRTERRS);                                               
	}
	if (status != UU_SUCCESS)
	{
		uu_uerror2(UX_UDOS, 27, libdataptr->pstring, "uxu_create_lib");
		/* error is: Error in path name to %s library.  (%s). */
		goto failed;
	}
	else
		strcpy(libname,fullname);
	ul_remove_quotes(libname);

	if (mode != (mode | UX_NEXISTS))  /* then the file exists */
	{
		uu_uerror3(UX_UDOS, 32, libdataptr->pstring, libname, "uxu_create_lib");
		/* error is: Can't create %s library; %s already exists (%s). */
		goto failed;
	}
	else /* create a new library */
	{
		mode= 0755;
		if (ux_mk_dir(libname, mode, UX_PRTERRS) != UU_SUCCESS)
				goto failed;

		/* reset default library name for subsequnt library references */
		libdataptr->default_firstim = UU_FALSE;
		strcpy(libdataptr->default_lib, libname);/* only what user supplied*/
	}

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  

/*********************************************************************
**       E_FUNCTION :  int uxu_delete_lib(libdataptr)
**              This function deletes a library. Only deletes if the library is empty.
**       PARAMETERS     
**               INPUT  : none.
**               OUTPUT : none. 
**       RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**       SIDE EFFECTS: none
**       WARNINGS: none
*********************************************************************/
int uxu_delete_lib(libdataptr)
	UX_libdata_bag *libdataptr;
{
	int cmdreject;
	UU_LOGICAL default_flag;/* either UD_NODEFAULT or UD_DEFAULT */
	UX_pathname libname;
	UX_pathname prefix;  
	char *list = UU_NULL, *prompt, *uu_uprompt0();
	UU_LOGICAL pathfound;
	int file_status;
	int found;
	int numint;              /* number of appropriate user interactions */
	int len, i;
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UX_FAILURE */

	uu_denter(UU_XTRC, (us, "uxu_delete_lib()"));

	prompt = uu_uprompt0(UX_UDOS, 11);
	/* Prompt is: Give lib. name; for system area, prefix name with \"sys:\". */
	if (uxu_ask_for_lib(libdataptr, prompt, libname, &numint, &file_status) 
		!= UU_SUCCESS) goto failed;
	if (numint <=0) goto done;
		
	if (file_status != UX_FAREA) /* we did not get a directory */
	{
		uu_uerror3(UX_UDOS, 26, libdataptr->pstring, libname, "uxu_delete_lib");
		/* error message: No %s library found at %s  (%s). */
		goto failed;
	}
	else /* try to delete the library */
	{
		/* see if the library we are trying to delete is in the users
		 * local area */
		if (ux_get_syspath(libdataptr->loc_farea, &list, prefix, &pathfound,
			UX_PRTERRS) != UU_SUCCESS) goto failed;               
		uu_lsdel(list);      /* remove list now that we're done with it */
		if (ux_test_prefix(prefix, libname, &found, UX_PRTERRS) != UU_SUCCESS)
		{
			uu_uerror3(UX_UDOS, 31, libname, prefix, "uxu_delete_lib");
			/* Error, can't determine if library, %s, is in area, %s, (%s). */
			goto failed;
		}
		if (found != UU_SUCCESS)
		{
			uu_uerror2(UX_UDOS, 33, libname, "uxu_delete_lib");
			/* error is: Archive path, %s, is out of user local area,  (%s). */
			goto done;
		}
		if (ux_rmdir(libname, UX_PRTERRS) != UU_SUCCESS)
		{
			uu_uerror3(UX_UDOS, 30,libdataptr->pstring, libname, "uxu_delete_lib");
			/* error is: Can't remove %s archive, %s,  (%s). */
			goto failed;
		}
	}
	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  

/*********************************************************************
**       E_FUNCTION :  int uxu_list_area(libdataptr)
**               This function lists archives in an area.
**               Currently, there are only 2 areas: a user's local area
**               where the users may both read and write from archives,
**               and a system area where the user can only read.  The paths to
**               these 2 areas are defined in the ".init" startup file.
**       PARAMETERS     
**               INPUT  : none.
**               OUTPUT : none. 
**       RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**       SIDE EFFECTS: Creates a udos window and lists archives in the window.
**       WARNINGS: none
*********************************************************************/
int uxu_list_area(libdataptr)
	UX_libdata_bag *libdataptr;
{
	UX_pathname fullname; /* storage for a library name */
	UX_pathname libarea;
	int cmdreject;
	static char string[3] = "l";/* for user input to determine whether we are 
										  * to list the local area or the system area */
	char *arg_list[2];
	int numint; /* number of interactions */
	int status = UU_SUCCESS;
	int farea_flag;
	int args[3];
	
	uu_denter(UU_XTRC, (us, "uxu_list_area()"));

	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject) /* then no command reject encountered */
	{
		if (uxu_ask_for_area(libdataptr, libarea, fullname, &numint)
			!= UU_SUCCESS) goto failed;
		if (numint == 0) /* no appropriate users interactions */
			goto done;
		/* library name gotten; so open window to list it. */
/*              if (ux_window_open("UX") != UU_SUCCESS)
			goto failed;*/
		args[1] = 1;
		ul_open_window(20,80,args);


		/* write to window */
		arg_list[0] = libdataptr->farea_suffix;
		/* flag that we wish to list fileareas (not files) */
		farea_flag = UX_FAREA;
		if (uxu_list_files(fullname, farea_flag,1, arg_list) != UU_SUCCESS)
		{
/*                      ud_kiwin(); /* kill window */
			ul_close_window();
			goto failed;
		}

		/* tell user that the end of the output has been reached;
		 * message is: END OF MESSAGE; press any key: ud_hakt(UD_DASHEP, 17);   */

/*              ud_kiwin(); /* kill the window */
		ul_close_window();

	}/* end no command reject */
	else /* command reject hit */ 
/*              ud_kiwin();*/
		ul_close_window();
	
	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}  
/******************************************************************
**       E_FUNCTION :  int uxu_rename_lib(libdataptr)
**              This function renames a library.
**       PARAMETERS     
**               INPUT  : none.
**               OUTPUT : none. 
**       RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**       SIDE EFFECTS: none
**       WARNINGS: none
*********************************************************************/
int uxu_rename_lib(libdataptr)
	UX_libdata_bag *libdataptr;
{
	UX_pathname libname;
	UX_pathname libname2;
	UX_pathname path_prefix;
	UU_LOGICAL nameok;
	UX_pathname dir, fname;
	int mode, hdrfound;
	UX_pathname prefix;  
	char *list = UU_NULL; 
	char *prompt, *uu_uprompt0();
	UU_LOGICAL pathfound;
	int file_status;
	int found;
	int numint;              /* number of appropriate user interactions */
	int len, i;
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UX_FAILURE */
	int *ans[4];
	int option;
	static int sav_dir = 0;
	UD_FSTAT uj_noop(), ux_browse_libfile();
	static UD_METHOD methods[4]  = {
			ux_browse_libfile, uj_noop, ux_browse_libfile, uj_noop};
	static char called[] = {6,6, 6,6};
	
	uu_denter(UU_XTRC, (us, "uxu_rename_lib()"));

/*
......This function changed to use form interface
......Yurong 9/18/98
*/
	option = 0;
	libname[0] = '\0';
	libname2[0] = '\0';
	ans[0] = (int *)&option;
	ans[1] = (int *)libname;
	ans[2] = (int *)&option;
	ans[3] = (int *)libname2;

	strcpy(def_loclib, libdataptr->loc_farea);
	strcpy(def_syslib, libdataptr->sys_farea);
	strcpy(S_ftpe, libdataptr->ftype);
	strcpy(S_fenv, libdataptr->fenv);
	status = ud_form1("xrenlib.frm", ans, ans, methods, called, NULL, NULL);
	if (status==-1)
	{
		status = UU_SUCCESS;
		goto done;
	}
	numint = strlen(libname);
	if (numint <= 0) goto done;
/*
......if the last character is '/', remove it.
*/
	if (libname[numint-1]=='/')
	{
		libname[numint-1] = '\0';
		numint = numint -1;
	}
/*
.....remove "_S" in library name if have any
.....because use may use it or not use it
.....for symple way, we just remove it
.....Yurong 9/18/98
*/
	if (numint>=2)
	{
		if ((libname[numint-2]=='_')&&(libname[numint-1]=='S'))
			libname[numint-2] = '\0';
	}
	strcpy(libdataptr->default_lib, libname);
	libdataptr->default_firstim = UU_FALSE;

	mode = UX_READ | UX_WRITE; /* check for read and write access */
	ul_break_fname(libname, dir, fname);
	if (dir[0]!='\0')
	{
		status = ux_file_inquire(UU_NULL, libname, UU_NULL, libdataptr->fenv,
						libdataptr->ftype, &mode, &hdrfound, libname,
						UX_PRTERRS);                                            
	}
	else
	{
		status = ux_file_inquire(libdataptr->loc_farea, libname, UU_NULL, 
						libdataptr->fenv, libdataptr->ftype, &mode, 
						&hdrfound, libname, UX_PRTERRS);                                                
	}
	if (status!= UU_SUCCESS)
	{
		uu_uerror2(UX_UDOS, 27, libdataptr->pstring, "ux_ask_for_lib");
		goto failed;
	}

	if (mode != (mode|UX_FAREA)) 
	{  
		uu_uerror3(UX_UDOS, 26, libdataptr->pstring, libname, "uxu_rename_lib");
		goto failed;
	}

	mode = UX_READ | UX_WRITE;
	ul_break_fname(libname2, dir, fname);
	if (dir[0]!='\0')
	{
		status = ux_file_inquire(UU_NULL, libname2, UU_NULL, libdataptr->fenv,
						libdataptr->ftype, &mode, &hdrfound, libname2,
						UX_PRTERRS);                                            
	}
	else
	{
		status = ux_file_inquire(path_prefix, libname2, UU_NULL, 
						libdataptr->fenv, libdataptr->ftype, &mode, 
						&hdrfound, libname2, UX_PRTERRS);                                               
	}
	if (status!= UU_SUCCESS)
	{
		uu_uerror2(UX_UDOS, 27, libdataptr->pstring, "uxu_rename_lib");
		goto failed;
	}

	if (ux_get_syspath(libdataptr->loc_farea, &list, prefix, &pathfound,
				UX_PRTERRS) != UU_SUCCESS) goto failed;               
	uu_lsdel(list);  
	if (ux_test_prefix(prefix, libname, &found, UX_PRTERRS) != UU_SUCCESS)
	{
		uu_uerror3(UX_UDOS, 31, libname, prefix, "uxu_rename_lib");
		goto failed;
	}
	if (found != UU_SUCCESS)
	{
		uu_uerror2(UX_UDOS, 33, libname, "uxu_rename_lib");
		goto done;
	}
	if (ux_rename(libname, libname2, UX_PRTERRS) != UU_SUCCESS)
		goto failed;


/************************************************************
	prompt = uu_uprompt0(UX_UDOS, 11);
	if (uxu_ask_for_lib(libdataptr,prompt,libname,&numint,
		&file_status) != UU_SUCCESS)
		goto failed;
	if (numint <=0) goto done;

	if (file_status != UX_FAREA) 
	{
		uu_uerror3(UX_UDOS, 26, libdataptr->pstring, libname, "uxu_rename_lib");
		goto failed;
	}
	else
	{
		if (uxu_ask_for_target_lib(libdataptr,libname2,&numint,&file_status)
			!= UU_SUCCESS) goto failed;
		if (numint <=0) goto done;

		if (ux_get_syspath(libdataptr->loc_farea, &list, prefix, &pathfound,
					UX_PRTERRS) != UU_SUCCESS) goto failed;               
		uu_lsdel(list);      
		if (ux_test_prefix(prefix, libname, &found, UX_PRTERRS) != UU_SUCCESS)
		{
			uu_uerror3(UX_UDOS, 31, libname, prefix, "uxu_rename_lib");
			goto failed;
		}
		if (found != UU_SUCCESS)
		{
			uu_uerror2(UX_UDOS, 33, libname, "uxu_rename_lib");
			goto done;
		}
		if (ux_rename(libname, libname2, UX_PRTERRS) != UU_SUCCESS)
			goto failed;
	}
****************************************************/

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  
