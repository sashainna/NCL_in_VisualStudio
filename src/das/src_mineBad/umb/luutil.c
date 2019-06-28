/*********************************************************************
**	FILENAME: lutil.c
**	CONTAINS:		ul_list_util
**				ul_plot_it
**				postit
**				ul_post_it
**				ul_util_it
**				ul_set_plot_mod
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       luutil.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 12:05:20
*********************************************************************/

#include "usysdef.h"
#include "dasnog.h"
#include "dtypes.h"
#include "lcom.h"
#include "lumb.h"
#include "udebug.h"
#include "udfdata.h"
#include "udforms.h"
#include "uhep.h"
#include "ustdio.h"
#include "xenv1.h"
#include "xfsys0.h"
#include "xfsys1.h"
#include "ctype.h"
#include "nclfc.h"

#if UU_COMP == UU_VAXVMS
#define POST_DIR "NCL$POST:"
#define PLOT_DIR "NCL$PLOT:"
#define UTIL_DIR "NCL$UTIL:"
#else
#define POST_DIR "NCL_POST"
#define PLOT_DIR "NCL_PLOT"
#define UTIL_DIR "NCL_UTIL"
#endif

#define NODEFAULT 0
#define DEFAULT 1

void ul_verify_exe();
void ul_run_util();

/*********************************************************************
**	E_FUNCTION:	ul_list_util (dflag,wflag,util,nc)
**			This function lists the post-processor, plot &
**			utility directories.
**
**       INPUT  :	dflag =		1 = list available post-processors.
**					2 = list available plot routines.
**					3 = list available utility routines.
**			wflag = 	0 = do not open & close window.  The
**					calling routine will handle the window.
**					1 = open & close a scrolling window.
**					The "Hit any key to continue" message
**					will be displayed.
**       OUTPUT :	util =	Utility name chosen.
**			nc =	# of chars in utility name.
**
**    RETURNS      :	UU_SUCCESS if successful, UU_FAILURE otherwise.
**
**    SIDE EFFECTS : Opens and writes to a scrolling window.
**
**    WARNINGS     : none.
*********************************************************************/
#define tabn 18
ul_list_util(dflag,wflag,util,nc)
	char *util;
	int dflag,wflag,*nc;
{
	int status;
	int wargs[2],args[2];
	char *list;
	static char *listhead=UU_NULL;
	char *spac="                       ";
	char pr[80];
	UX_pathname fname,fullname,basename,buff;
	int i,m,n,markval=0,nfind;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC, (us, "ul_list_util(dflag,wflag)"));
	status = UU_SUCCESS; /* assume success */
/*
.....Command Reject
*/
	UD_MARK (markval,UU_FALSE);
	if (markval != 0)
	{
		if (listhead != UU_NULL) uu_lsdel(listhead);
		listhead = UU_NULL;
		ul_close_window();
		UD_UNMARK (markval);
		return(status);
	}
/*
.....Get directory to list
*/
	switch (dflag)
	{
	case 1:
		strcpy (basename,POST_DIR);
		strcpy (pr,"Enter post-processor name");
		break;
	case 2:
		strcpy (basename,PLOT_DIR);
		strcpy (pr,"Enter plot routine name");
		break;
	case 3:
		strcpy (basename,UTIL_DIR);
		strcpy (pr,"Enter utility name");
		break;
	}
/*
.....Get full path name of Routine Directory
*/
	if (ul_get_full_dir(basename,fullname) != UU_SUCCESS)
		goto failed;
/*
.....Build a list of routines
.....From the home directory
*/
	strcpy (fname,"*");
	if (ul_get_flist(fullname,fname,&list,2,(UX_PRTERRS|UX_NCHK))
		 != UU_SUCCESS)
		 goto failed;
	listhead = list;
	nfind = 0;
/*
.....Open a window if requested
*/
	if (wflag == 1)
	{
		wargs[1] = 1;
		if (ul_open_window(10,UL_wincol,wargs) != UU_SUCCESS)
			goto failed;
	}
/*
.....Write out the window header
*/
	UX_SPRINTF0((buff," Routines from Files Path %s:\n",fullname));
	ul_win_out(buff,0);
/*
.....There are no such files in this directory
.....Show the empty list
*/
	if (list == UU_NULL)
	{
		UX_SPRINTF0 ((buff," No routines."));
		ul_win_out (buff,1);
		status = UU_SUCCESS;
		if (wflag == 1)
		{
			ud_hakt (UX_UDOS,1);
			ul_close_window();
		}
		*nc = 0;
		goto done;
	}
/*
.....Get file off the list of files
*/
	else
	{
		strcpy (buff," ");
		args[0] = args[1] = 0;
		while ((status = ux_nxt_file(&list,fname,UX_PRTERRS)) ==
			UU_SUCCESS)
		{
			if (ul_get_base_fname(fname, basename, args,
				(UX_PRTERRS|UX_NQUOTES|UX_NCHK)) == UU_FAILURE)
			{
				if (wflag == 1) ul_close_window();
				goto failed;
			}
			nfind++;
			sprintf (fname,"%3d: %s",nfind,basename);
/*
.....Write out the filename to the window
*/
			if (strlen(buff)+strlen(fname)+1 > UL_wincol)
			{
				ul_win_out(buff,0);
				strcpy (buff," ");
			}
			strcat(buff,fname);
			n = strlen(buff);
			m = (n/tabn+1) * tabn - n;
			if (m+n >= UL_wincol) m = UL_wincol - n - 1;
			strncat (buff,spac,m);
		}
		if (strlen(buff) != 0) ul_win_out (buff,0);
	}

/*
.....Get the utility name
*/
	ud_das (UD_DASSTRING,pr,buff,sizeof(buff),&n);
	ul_strip_blanks (buff,&n);
/*
.....Check for number instead of name
*/
	m = 0;
	if (strlen(buff) == 0) m = 1;
	for (i=0;i<strlen(buff);i++) if (isdigit(buff[i]) == 0) m = 1;
/*
.....Number entered
.....Get utility entry off of list
*/
	if (m == 0)
	{
		i = atol(buff);
		if (i <= 0 || i > nfind) m = 1;
		else
		{
			list = listhead;
			for (n=0;n<i;n++) ux_nxt_file(&list,fname,UX_PRTERRS);
			ul_get_base_fname(fname,util, args,
				(UX_PRTERRS|UX_NQUOTES|UX_NCHK));
		}
	}
/*
.....Utility name entered
*/
	if (m == 1) strcpy (util,buff);
	*nc = strlen(util);
/*
.....Close the window
*/
	if (wflag == 1)
	{
		ud_hakt (UX_UDOS,1);
		ul_close_window();
	}
/*
.....Check for failure
*/
	if (status == UX_FAILURE)
		goto failed;
/*
.....End of list reached
*/
	status = UU_SUCCESS;
	goto done;
/*
.....Failure
*/
failed: status = UU_FAILURE;
done:;
	if (listhead != UU_NULL) uu_lsdel(listhead);
	listhead = UU_NULL;
	UD_UNMARK(markval);
	uu_dexit;
	return(status);
}	

/*********************************************************************
**	E_FUNCTION:	ul_plot_it (flag)
**			This function runs a plot routine. The plot routine
**			must reside in NCL$PLOT.
**
**       INPUT  :	flag =	1 = Run the NCLCADD plot utility.
**				2 = List the available plot routines first and
**					then prompt the user for the plot
**					routine to run.
**				3 = Prompt the user for the plot routine to
**					run.
**       OUTPUT :  none.
**
**    RETURNS      :	UU_SUCCESS if successful, UU_FAILURE otherwise.
**
**    SIDE EFFECTS : Spawns a sub-process and optionally opens & writes to a
**			scrolling window.
**
**    WARNINGS     : none.
*********************************************************************/
ul_plot_it(flag)
	int flag;
{
	char *position,*ux_getenv(),*rindex();
	char buf[80];
	UX_pathname fullname,buf1,com;
	int wargs[2],numint,stat,status,file_status,mode;
	int i,markval,ierr;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_plot_it(flag)"));
	status = UU_SUCCESS;
/*
.....Command Reject
*/
	UD_MARK (markval,UU_FALSE);
	if (markval != 0)
	{
		ul_close_window();
		UD_UNMARK (markval);
		return(status);
	}
/*
.....Run NCLCADD plot tool
*/
	if (flag == 1)
	{
		ul_spawn("nclplot",2);
		goto done;
	}
/*
.....List the available plot routines if requested.
*/
	if (flag == 2)
	{
		wargs[1] = 1;
		if (ul_open_window(10,UL_wincol,wargs) != UU_SUCCESS)
			goto failed;
		if (ul_list_util(2,0,buf,&numint) != UU_SUCCESS)
		{
			ul_close_window();
			goto failed;
		}
	}
/*
.....See if routine name was entered
*/
	if (numint == 0)
	{
		if (flag == 2) ul_close_window();
		goto done;
	}
/*
.....Verify that plot routine exists
*/
	ul_verify_exe("plot",buf,fullname,&ierr);
	if (ierr == 1)
	{
		sprintf (com,"Could not access plot routine directory %s",
			fullname);
		ud_wrerr (com);
		if (flag == 2) ul_close_window();
		goto failed;
	}
	else if (ierr == 2)
	{
		sprintf (com,"Plot routine %s%s does not exist",fullname,buf1);
		ud_wrerr (com);
		if (flag == 2) ul_close_window();
		goto failed;
	}
/*
.....Get name of cl/punch file to run plot routine against
*/
	strcpy (buf1,UL_program);
	position = ux_getenv ("UL_CLFILE1_SUFFIX",UX_NPRTERRS);
	if (position != 0)
	{
		strcat (buf1,".");
		strcat (buf1,position);
	}
	do
	{
		ul_string_def ("Enter plot file name",buf1,sizeof(buf1),
				&numint,&file_status);
		ul_strip_blanks (buf1,&numint);
		if (strlen(buf1) == 0)
		{
			if (flag == 2) ul_close_window();
			goto done;
		}
/*
.....Verify that input file exists
*/
		strcpy (com,buf1);
		position = rindex(com,'/');
		if (position != 0) *position = '\0';
		mode = UX_EXISTS|UX_READ;
		stat = ux_file_inquire(UU_NULL,UU_NULL,com,UU_NULL,UU_NULL,&mode,
			&file_status,com,UX_NPRTERRS);
		ierr = 0;
		if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
		{
			sprintf (com,"Plot file %s does not exist",buf1);
			ud_wrerr (com);
			ierr = 1;
		}
	} while (ierr == 1);
/*
.....Append default switches
*/
	strcpy (com,buf);
	for (i=0;i<strlen(com);i++)
		if (isalpha(com[i]) != 0) com[i] = toupper(com[i]);
	position = rindex(buf1,'/');
	if (position == 0 && (strcmp(com,"CCPLT1") == 0 ||
		strcmp(com,"HPPLT1") == 0 || strcmp(com,"TKPLT1") == 0))
	{
/*
.....Append PL switch
*/
		strcat (buf1,"/PL:");
/*
.....CCPLT1 plotter type
*/
		if (strcmp(com,"CCPLT1") == 0)
		{
			switch (UL_ccp_type)
			{
			case 0:
				strcat (buf1,"1038");
				break;
			case 1:
				strcat (buf1,"1040");
				break;
			case 2:
				strcat (buf1,"1042");
				break;
			case 3:
				strcat (buf1,"1043");
				break;
			case 4:
				strcat (buf1,"1044");
				break;
			case 5:
				strcat (buf1,"1051");
				break;
			case 6:
				strcat (buf1,"1073");
				break;
			case 7:
				strcat (buf1,"1075");
				break;
			case 8:
				strcat (buf1,"1077");
				break;
			}
		}
/*
.....HPPLT1 plotter type
*/
		else if (strcmp(com,"HPPLT1") == 0)
		{
			switch (UL_hpp_type)
			{
			case 0:
				strcat (buf1,"7220");
				break;
			case 1:
				strcat (buf1,"7470");
				break;
			case 2:
				strcat (buf1,"7475");
				break;
			case 3:
				strcat (buf1,"7580");
				break;
			case 4:
				strcat (buf1,"7585");
				break;
			case 5:
				strcat (buf1,"51");
				break;
			case 6:
				strcat (buf1,"52");
				break;
			case 7:
				strcat (buf1,"56");
				break;
			case 8:
				strcat (buf1,"2");
				break;
			case 9:
				strcat (buf1,"928");
				break;
			}
		}
/*
.....TKPLT1 plotter type
*/
		else if (strcmp(com,"TKPLT1") == 0)
		{
			switch (UL_tkp_type)
			{
			case 0:
				strcat (buf1,"4014");
				break;
			case 1:
				strcat (buf1,"4105");
				break;
			case 2:
				strcat (buf1,"4107");
				break;
			case 3:
				strcat (buf1,"4109");
				break;
			case 4:
				strcat (buf1,"4111");
				break;
			case 5:
				strcat (buf1,"4115");
				break;
			case 6:
				strcat (buf1,"4211");
				break;
			}
		}
/*
.....Append SAV switch
*/
		if (UL_plot_save == 1) strcat (buf1,"/SAV");
/*
.....Append OLD/NEW switch
*/
		if (UL_plot_txx == 0) strcat (buf1,"/NEW");
		else strcat (buf1,"/OLD");
/*
.....Append ABS/INC switch
*/
		if (UL_plot_abs == 0) strcat (buf1,"/INC");
		else strcat (buf1,"/ABS");
/*
.....Append BYP switch
*/
		if (strcmp(com,"HPPLT1") == 0)
		{
			if (UL_hpp_bypass == 0) strcat (buf1,"/-BYP");
			else strcat (buf1,"/BYP");
		}
/*
.....Append FP switch
*/
		if (UL_plot_fpp == 0) sprintf (com,"/FP:L%d%d",UL_plot_dleft,
					UL_plot_dright);
		else sprintf (com,"/FP:T%d%d",UL_plot_dleft,UL_plot_dright);
		strcat (buf1,com);
/*
.....Append UN switch
*/
		if (UL_plot_units == 0) strcat (buf1,"/UN:I");
		else strcat (buf1,"/UN:M");
	}
/*
.....Get rid of window
*/
	if (flag == 2) ul_close_window();
/*
.....Run the plot routine
*/
	ul_run_util(fullname,buf,buf1,2);
	goto done;
/*
.....Failure
*/
failed: status = UU_FAILURE;
done:;
	UD_UNMARK(markval);
	uu_dexit;
	return(status);
}	

/*********************************************************************
**	E_FUNCTION:	ul_post_it (flag,batch)
**			This function runs a post-processor against an
**			NCL clfile.  The post-processor must reside in
**			NCL$POSTS.
**
**       INPUT  :	flag =	1 = Run the post-processor(s) as specified
**					by the MACHIN card(s) against the
**					current clfile.
**				2 = List the available post-processors first
**					and then prompt the user for the post-
**					processor to run.
**				3 = Prompt the user for the post-processor to
**					run.
**				4 = Same as (2) except the post is ran in batch.
**			batch = Flag if we are running batch.
**       OUTPUT :  none.
**
**    RETURNS      :	UU_SUCCESS if successful, UU_FAILURE otherwise.
**
**    SIDE EFFECTS : Spawns a sub-process and optionally opens & writes to a
**			scrolling window.
**
**    WARNINGS     : none.
*********************************************************************/
void postit()
{
	ul_post_it(1,1);
}

ul_post_it(flag,batch)
   	int flag,batch;
{
	char *position,*rindex(),*ux_getenv();
	char buf[UX_MAX_FILE_LEN],buf1[UX_MAX_FILE_LEN],com[UX_MAX_PATH_LEN+40];
	char *list = UU_NULL;
	UX_pathname fullname, filter, fullsave;
	int endit,numint,stat,status,file_status,mode,inc,nc;
	int ent,i,markval,ierr,iwin;
	int found;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_post_it(flag)"));
	status = UU_SUCCESS;
	endit = 0;
/*
.....Command Reject
*/
	if (batch == 0)
	{
		UD_MARK (markval,UU_FALSE);
		if (markval != 0)
		{
			ul_close_window();
			UD_UNMARK (markval);
			return(status);
		}
	}
/*
.....List the available post-processors if requested.
*/
	if (flag == 2 || flag == 4)
	{
/*
.....change to use file browser
.....Yurong 9/30/98
*/
		ux_get_syspath(POST_DIR, &list, filter, &found, UX_NPRTERRS);
		uu_lsdel(list);
		if (!found)
		{
/*
.....path not found
*/
			strcpy (filter, "*.*");
		}
		else
		{
			if (strcmp(filter, "\"NCL_POST\"")!=0)
			{
				ux_cat_paths(filter, "*.*", filter, UX_NPRTERRS);
				ul_remove_quotes(filter);
			}
			else
			{
/*
.....NCL_POST not define
*/
				strcpy (filter, "*.*");
			}
		}
		buf[0] = '\0';
		ud_get_filename(UU_NULL, "Browse", "Enter post-processor name",
								filter, buf, &numint, "All Files (*.*)",UU_FALSE);
	}
/*
.....Get the name of the post-processor to run
*/
	ent = 0;
	for (inc=0;inc<UL_nposts;inc++)
	{
/*
.....Get name from MACHIN card stack
*/	
		if (flag == 1)
		{
			nc = strlen(UL_posts[inc]);
			for (i=0;i<nc;i++)
			{
				buf[i] = UL_posts[inc][i];
				if (isalpha(buf[i]) != 0) buf[i] = tolower(buf[i]);
				if (buf[i] == ' ') buf[i] = '\0';
			}
			buf[nc] = '\0';
			ent++;
		}
/*
.....User was prompted for name
*/
		else
		{
			if (numint == 0)
			{
/*				if (flag == 2 || flag == 4) ul_close_window();   */
				goto done;
			}
			endit = 1;
		}
/*
.....Verify that post-processor exists
*/
		ul_verify_exe("post",buf,fullname,&ierr);
		if (ierr == 1)
		{
			if (batch == 0)
			{
				sprintf (com,"Could not access post-processor directory %s",
					fullname);
				ud_wrerr (com);
				if (flag == 2 || flag == 4) ul_close_window();
			}
			else
			{
				sprintf (com, "Could not access post-processor directory %s\n",
					fullname);
				ud_printmsg(com);
			}
			goto failed;
		}
/*
.....Could not find post-processor
.....Try Pworks before outputting error
*/
		else if (ierr == 2)
		{
			if (flag == 1)
			{
				strcpy(fullsave,fullname);
				ul_verify_exe("post","pworks",fullname,&ierr);
				if (ierr == 0) strcpy(buf,"pworks");
				else strcpy(fullname,fullsave);
			}
			if (ierr == 1)
			{
				if (batch == 0)
				{
					sprintf (com,"Post-processor %s%s does not exist",
						fullname,buf1);
					ud_wrerr (com);
					if (flag == 2 || flag == 4) ul_close_window();
				}
				else
				{
					sprintf (com, "Post-processor %s%s does not exist\n",
						fullname,buf1);
					ud_printmsg(com);
				}
				goto failed;
			}
		}
/*
.....Get name of clfile to run post against
*/
		strcpy (buf1,UL_program);
		do
		{
			if (flag != 1)
			{
				ul_string_def ("Enter clfile name",buf1,sizeof(buf1),
						&numint,&file_status);
				ul_strip_blanks (buf1,&numint);
			}
/*
.....Append default clfile suffix
*/
			if (strlen(buf1) == 0)
			{
				if (flag == 2 || flag == 4) ul_close_window();
				goto done;
			}
			position = rindex (buf1,'.');
			if (position == 0)
			{
				strcat (buf1,".");
				if (ent <= 1)
					position = ux_getenv ("UL_CLFILE1_SUFFIX",UX_NPRTERRS);
				else
					position = ux_getenv ("UL_CLFILE2_SUFFIX",UX_NPRTERRS);
				if (position != 0) strcat (buf1,position);
			}
/*
.....Verify that clfile exists
*/
			mode = UX_EXISTS|UX_READ;
			stat = ux_file_inquire(UU_NULL,UU_NULL,buf1,UU_NULL,UU_NULL,&mode,
				&file_status,com,UX_NPRTERRS);
			ierr = 0;
			if ((stat != UU_SUCCESS || mode == (mode|UX_NEXISTS)))
			{
				if (batch == 0)
				{
					sprintf (com,"Clfile %s does not exist",buf1);
					ud_wrerr (com);
/*					if (flag == 2 || flag == 4) ul_close_window();*/
				}
				else
				{
					sprintf (com, "Clfile %s does not exist\n",buf1);
					ud_printmsg(com);
				}
				if (flag == 1) goto done;
				ierr = 1;
			}
		} while (ierr == 1);
/*
.....Run the post in batch
*/
		if (flag == 4)
		{
/*
.....Schedule post
*/
			strcat (buf," ");
			strcat (buf,buf1);
			ul_que_util (buf,1);
/*
.....Run Batch
*/
			ul_submit_batch (2);
			ul_close_window();
			goto done;
		}
/*
.....Get rid of window
*/
		if (flag == 2 || flag == 4) ul_close_window();
/*
.....Run the post
*/
		iwin = 0;
		if (batch == 0) iwin = 1;
#if UU_COMP == UU_WINNT 
		{
			int nn;
			char tbuf[128];
			strcpy(tbuf,buf1);
			buf1[0] = '"';
			strcpy(&buf1[1],tbuf);
			nn = strlen(buf1);
			buf1[nn] = '"';
			buf1[nn+1] = '\0';
		}
#endif
/*
#if UU_COMP == UU_WINNT || UU_COMP == UU_WIN2K
*/
		if (batch == 1)
		{
			if (strcmp(buf,"pworks") == 0 || strcmp(buf,"pmacro") == 0)
				strcat (buf1," -quiet -clfile:1");
		}
/*
#endif
*/
		ul_run_util(fullname,buf,buf1,iwin);
		if (endit == 1) break;
	}
	goto done;
/*
.....Failure
*/
failed: status = UU_FAILURE;
done:;
	if (batch == 0) 
 	{
 		UD_UNMARK(markval);
 	}
	uu_dexit;
	return(status);
}	

/*********************************************************************
**	E_FUNCTION:	ul_util_it (flag,win)
**			This function runs a utility program. The utility
**			must reside in NCL$UTIL.
**
**       INPUT  :	flag =	2 = List the available utilities first and
**					then prompt the user for the utility
**					to run.
**				3 = Prompt the user for the utility to run.
**				4 = Same as (2) except util is ran in batch.
**			win =	1 = Partial screen window.
**				2 = Full screen window.
**       OUTPUT :  none.
**
**    RETURNS      :	UU_SUCCESS if successful, UU_FAILURE otherwise.
**
**    SIDE EFFECTS : Spawns a sub-process and optionally opens & writes to a
**			scrolling window.
**
**    WARNINGS     : none.
*********************************************************************/
ul_util_it(flag,win)
	int flag,win;
{
	char buf[UX_MAX_FILE_LEN],buf1[UX_MAX_FILE_LEN],com[UX_MAX_PATH_LEN+40];
	UX_pathname fullname, filter;
	int numint,status;
	int markval,ierr, found;
	char *list = UU_NULL;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_util_it(flag)"));
	status = UU_SUCCESS;
/*
.....Command Reject
*/
	UD_MARK (markval,UU_FALSE);
	if (markval != 0)
	{
		ul_close_window();
		UD_UNMARK (markval);
		return(status);
	}
/*
.....List the available utilities if requested.
*/
	if (flag == 2 || flag == 4)
	{
/*
.....change to use file browser
.....Yurong 9/30/98
*/
		ux_get_syspath(UTIL_DIR, &list, filter, &found, UX_NPRTERRS);
		uu_lsdel(list);
		if (!found)
		{
			strcpy (filter, "*.*");
		}
		else
		{
			if (strcmp(filter, "\"NCL_UTIL\"")!=0)
			{
				ux_cat_paths(filter, "*.*", filter, UX_NPRTERRS);
				ul_remove_quotes(filter);
			}
			else
				strcpy (filter, "*.*");
		}
		buf[0] = '\0';
		ud_get_filename(UU_NULL, "Browse", "Enter post-processor name",
								filter, buf, &numint, "All Files (*.*)",UU_FALSE);
	}
/*
.....A routine name was not entered
*/
	if (numint == 0)
	{
/*		if (flag == 2 || flag == 4) ul_close_window(); */
		goto done;
	}
/*
.....Verify that utility exists
*/
	ul_verify_exe("util",buf,fullname,&ierr);
	if (ierr == 1)
	{
		sprintf (com,"Could not access utility directory %s",fullname);
		ud_wrerr (com);
/*		if (flag == 2 || flag == 4) ul_close_window(); */
		goto failed;
	}
	else if (ierr == 2)
	{
		sprintf (com,"Utility %s does not exist",buf1);
		ud_wrerr (com);
/*		if (flag == 2 || flag == 4) ul_close_window();   */
		goto failed;
	}
/*
.....Get command line
*/
	/*RAH: initialize string to NULL in case user enters no arguments and DONE  */
	buf1[0] = '\0';
	ud_das (UD_DASSTRING,"Enter command line",buf1,sizeof(buf1),&numint);
/*
.....Run the utility in batch
.....First schedule utility
*/
	if (flag == 4)
	{
		strcat (buf," ");
		strcat (buf,buf1);
		ul_que_util (buf,1);
/*
.....Start batch processor
*/
		ul_submit_batch (2);
		ul_close_window();
		goto done;
	}
/*	if (flag == 2 || flag == 4) ul_close_window();       */
/*
.....Run the utility
*/
	ul_run_util(fullname,buf,buf1,win);
	goto done;
/*
.....Failure
*/
failed: status = UU_FAILURE;
done:;
	UD_UNMARK(markval);
	uu_dexit;
	return(status);
}	

/*********************************************************************
**	 E_FUNCTION : ul_set_plot_mod()
**			This function sets the default options to use
**			when scheduling a plot routine.
**	 PARAMETERS	
**		 INPUT  :  none
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Defines plot routine modals.
**	 WARNINGS: none.
*********************************************************************/

void ul_set_plot_mod()
{
	int ccp[5],hpp[13],tkp[5],save[4],txx[4],abs[12],byp[4],fpp[4],left[2];
	int right[2],units[7];
	int stat;
	int *ans[11];
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_set_plot_mod()"));
/*
.....Set up the defaults
.....Load the input values into
.....local storage area
*/
	ccp[0] = UL_ccp_type;
	hpp[0] = UL_hpp_type;
	tkp[0] = UL_tkp_type;
	save[0] = UL_plot_save;
	txx[0] = UL_plot_txx;
	abs[0] = UL_plot_abs;
	byp[0] = UL_hpp_bypass;
	fpp[0] = UL_plot_fpp;
	left[0] = UL_plot_dleft;
	right[0] = UL_plot_dright;
	units[0] = UL_plot_units;
/*
.....Field 0 is CCP plotter type
*/
	ans[0] = (int *)ccp;
/*
.....Field 1 is HPP plotter type
*/
	ans[1] = (int *)hpp;
/*
.....Field 2 is TKP plotter type
*/
	ans[2] = (int *)tkp;
/*
.....Field 3 is Save work file
*/
	ans[3] = (int *)save;
/*
.....Field 4 is Use work file
*/
	ans[4] = (int *)txx;
/*
.....Field 5 is Abs/Incr mode
*/
	ans[5] = (int *)abs;
/*
.....Field 6 is HPP bypass mode
*/
	ans[6] = (int *)byp;
/*
.....Field 7 is Floating point type
*/
	ans[7] = (int *)fpp;
/*
.....Field 8 is # of digs to left
*/
	ans[8] = (int *)left;
/*
.....Field 9 is # of digs to right
*/
	ans[9] = (int *)right;
/*
.....Field 10 is units
*/
	ans[10] = (int *)units;
/*
.....Get the Form input
*/
	stat = ud_form("lplotmod.frm",ans,ans);
	if (stat==-1)
		return;
/*
.....Store Plot Modals
*/
	UL_ccp_type = ccp[0];
	UL_hpp_type = hpp[0];
	UL_tkp_type = tkp[0];
	UL_plot_save = save[0];
	UL_plot_txx = txx[0];
	UL_plot_abs = abs[0];
	UL_hpp_bypass = byp[0];
	UL_plot_fpp = fpp[0];
	UL_plot_dleft = left[0];
	UL_plot_dright = right[0];
	UL_plot_units = units[0];
failed:;
done:;
	uu_dexit;
	return;
}

/*********************************************************************
**	E_FUNCTION:	ul_verify_exe (dir,exe,path,err)
**			This function verifies that a utility program resides in
**			either the UTIL, PLOT, or POST directories.
**
**       INPUT  :	dir = Directory to check for the utility program.
**					'util', 'plot', or 'post'.
**					exe = Name of utility program.
**       OUTPUT :	path = Full path of util/plot/post directory.
**					err = 0 = Success.  1 = Cannot access directory.
**					2 = Cannot access utility program.
**
**    RETURNS      :	none.
**
**    SIDE EFFECTS : 	none.
**
**    WARNINGS     : none.
*********************************************************************/
void ul_verify_exe(dir,exe,path,err)
int *err;
char dir[],exe[],path[];
{
	int mode,file_status,stat, status;
	char buf1[80],temp[80];
	UX_pathname com;
	FILE *fptr = NULL;
/*
.....Get directory string
*/
	*err = 0;
	ul_break_fname(exe,temp,buf1);
	if (temp[0] == '\0')
	{
		if (strcmp(dir,"util") == 0) strcpy(temp,UTIL_DIR);
		else if (strcmp(dir,"post") == 0) strcpy(temp,POST_DIR);
		else if (strcmp(dir,"plot") == 0) strcpy(temp,PLOT_DIR);
		else
		{
			*err = 1;
			goto done;
		}
		strcpy (buf1,exe);
	}
/*
.....Get full path name of Directory
*/
/**************change to check if the file exist not the path since
.....we allow multi-path now

	if (ul_get_full_dir(temp,path) != UU_SUCCESS)
	{
		*err = 1;
		goto done;
	}
/*
.....above code added in, since we return 2 error code
.....err  =1 mean path not OK
.....err = 2 mean path ok but file can't access
.....ul_get_full_dir allow multi-path too, return first available path 
.....Yurong
*/
	if (ul_get_full_dir(temp,path) != UU_SUCCESS)
	{
		*err = 1;
		goto done;
	}
/*
.....Verify that utility exists
*/
#if UU_COMP == UU_VAXVMS || UU_COMP == UU_WINNT || UU_COMP == UU_WIN2K
	if (index(buf1,'.') == 0) strcat (buf1,".EXE");
#endif
/**************
	mode = UX_EXISTS|UX_READ;
	stat = ux_file_inquire(UU_NULL,path,buf1,UU_NULL,UU_NULL,&mode,
		&file_status,com,UX_NPRTERRS);
	if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
	{
		*err = 2;
		goto done;
	}
************/
	status = ul_open_mod_file2(temp, NULL, buf1, 0,  &fptr, UU_NULL, UU_NULL);
	if (status==-1)
	{
		*err = 2;
		path[0] = '\0';
	}
	else
	{
		*err = 0;
		strcpy(path, buf1);
	}
/*
.....Convert Windows Nutcracker path
.....to Windows NT path
*/
#if UU_COMP == UU_WINNT
	if (path[0] == '/' && path[2] == '=')
	{
		strcpy(buf1,path);
		path[0] = buf1[1];
		path[1] = ':';
		strcpy(&path[2],&buf1[3]);
	}
#endif
done:;
	return;
}

/*********************************************************************
**	E_FUNCTION:	ul_run_util (dir,exe,cmd,win)
**			This function runs a utility program which resides in
**			either the UTIL, PLOT, or POST directories.
**
**       INPUT  :	dir = Full directory path which 'exe' resides in.
**					exe = Name of utility program to run.
**					cmd = Command line to pass to utility program.
**					win = 0 = Don't open window.
**					      1 = Open partial window.
**					      2 = Open full screen window.
**       OUTPUT :	none.
**
**    RETURNS      :	none.
**
**    SIDE EFFECTS : 	Spawns a sub-process.
**
**    WARNINGS     : none.
*********************************************************************/
void ul_run_util(dir,exe,cmd,win)
char dir[],exe[],cmd[];
int win;
{
	char com[UX_MAX_PATH_LEN+40];
/*
.....Let operating system know where utility is
.....so that we may run it
*/
#if UU_COMP == UU_VAXVMS
	strcpy (com,"$ ");
	strcat (com,dir);
	strcat (com,exe);
	ul_set_symbol (exe,com);
#endif
/*
.....Run the utility
*/
/*
.....if need display window, still use old ul_spawn routine,
.....otherwise, create a process to run seperately
*/
	if (win!=0)
	{
/*
.....need add doule quotes for executables in order to output
.....to dos command window
*/
		sprintf (com,"\"%s/%s\" %s",dir,exe,cmd);
		ul_spawn (com,win);
	}
	else
	{
		ul_run_process (dir,exe, cmd);
	}
	return;
}

