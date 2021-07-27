/*********************************************************************
**      FILENAME: lfile.c
**      CONTAINS:               ulu_copy
**                              ulu_delete
**                              ulu_edit
**                              ul_edit
**                              ulu_print
**                              ulu_purge
**                              ulu_rename
**                              ul_status_print
**                              ul_fix_wnt_file
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lufile.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:21
*********************************************************************/

#include "usysdef.h"
#include "dtypes.h"
#include "driver.h"
#include "lcom.h"
#include "lumb.h"
#include "udebug.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "nclfc.h"
#include "mfort.h"
#include "udforms.h"
#include "udfdata.h"

#if UU_COMP == UU_VAXVMS
#define editor "edit "
#define move "copy "
#else
#if UU_COMP == UU_WIN2K
#define editor "notepad "
#define move "rename "
#else
#define editor "vi "
#define move "mv "
#endif
#endif

/*
.....file type default to "All File"
.....Yurong added 9/16/98
*/
static int file_type = 10;   

void ul_edit();
void ul_fix_wnt_file();

/*********************************************************************
**    S_FUNCTION     :  static ul_browse_file(fieldno, val, stat)
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
static UD_FSTAT ul_browse_file(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname filename,descrip;
	char *index(), *ux_getenv();
	int i, len;
	char *p, ext[UX_MAX_PATH_LEN], ext1[UX_SUFFIX_LEN], sufx[UX_SUFFIX_LEN];
	char buff[UX_MAX_PATH_LEN];
	filename[0] = '\0';
	switch (file_type)
	{
		case 0:
			strcpy(descrip, "Unibase Files (");
			strcpy(ext,"*.");
			p = ux_getenv("UR_PART_SUFFIX");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
			}       
			else strcat(ext,"u");
			strcat(descrip, ext);
			strcat(descrip, ")|Textual Unibase Files (*.");
					
			p = ux_getenv("UR_ASCII_PART");
			strcat(ext,"|*.");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
				strcat(descrip, ext1);
			}       
			else
			{
				strcat(ext,"ud");
				strcat(descrip, "ud");
			}
			strcat(descrip, ")");
			ud_get_filename("Unibase Files", "Unibase Files", ext, 
						filename, &len, descrip, 1,UU_FALSE) ;
			break;
		case 1:
			ext[0] = '\0';
			strcpy(descrip, "Part Program Files (");
			p = ux_getenv("UL_PROGRAM_SUFFIX");
			if (p != 0) strcpy(sufx,p);
			else strcpy(sufx,"*");
			ul_remove_quotes(sufx);
			do
			{
				strcat(ext,"*.");
				strcpy(buff,sufx);
				p = (char *) index(buff,',');
				if (p == 0) sufx[0] = '\0';
				else
				{
					strcpy(sufx,p+1);
					*p = '\0';
				}
				strcat(ext,buff);
				strcat(ext,",");
			} while (strlen(sufx) != 0);
			i = strlen(ext) ; if (i != 0) ext[i-1] = '\0';

			strcat(descrip, ext);
			strcat(descrip, ")");
			ud_get_filename("Program Files", "Program Files", ext,
						filename, &len, descrip, 1,UU_FALSE) ;
			break;
		case 2:
			strcpy(ext,"*.");
			strcpy(descrip, "Plot Files (");
			p = ux_getenv("UJ_PLOT1_SUFFIX");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
			}       
			else strcat(ext,"pl1");
			strcat(descrip, ext);
			strcat(descrip, ")|Plot Files (*.");
			
			p = ux_getenv("UJ_PLOT2_SUFFIX");
			strcat(ext,"|*.");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
				strcat(descrip, ext1);
			}       
			else
			{
				strcat(ext,"pl2");
				strcat(descrip, "pl2");
			}
			strcat(descrip, ")");
			ud_get_filename("Plot Files", "Plot Files", ext,
						filename, &len, descrip, 1,UU_FALSE) ;
			break;
		case 3:
			strcpy(ext,"*.");
			strcpy(descrip, "Pen Table Files (");
			p = ux_getenv("UJ_PEN_SUFFIX");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
			}       
			else 
				strcat(ext,"pt");
			strcat(descrip, ext);
			strcat(descrip, ")");
			ud_get_filename("Pen Table Files", "Pen Table Files", ext,
						filename, &len, descrip, 1,UU_FALSE) ;
			break;
		case 4:
			strcpy(ext,"*.");
			strcpy(descrip, "Plot Setup Files (");
			p = ux_getenv("UJ_PLOT_SETUP");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
			}       
			else strcat(ext,"ps");
			strcat(descrip, ext);
			strcat(descrip, ")");
			ud_get_filename("Setup Files", "Setup Files", ext,
						filename, &len, descrip, 1,UU_FALSE);
			break;
		case 5:
			strcpy(ext,"*.");
			strcpy(descrip, "Clfiles (");
			p = ux_getenv("UL_CLFILE1_SUFFIX");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
			}       
			else strcat(ext,"cl");
			strcat(descrip, ext);
			strcat(descrip, ")|Secondary Clfiles (*.");
			
			p = ux_getenv("UL_CLFILE2_SUFFIX");
			strcat(ext,"|*.");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
				strcat(descrip, ext1);
			}       
			else
			{
				strcat(ext,"cln");
				strcat(descrip, "cln");
			}
			strcat(descrip, ")");
			ud_get_filename("CL files", "CL files", ext,
						filename, &len, descrip, 1,UU_FALSE) ;
			break;
		case 6:
			ud_get_filename("APT Files", "APT Files", "*.as",
						filename, &len, "APT Source Files (*.as)", 1,UU_FALSE) ;
			break;
		case 7:
			strcpy(ext,"*.");
			strcpy(descrip, "Record/Playback Files (");
			p = ux_getenv("UD_RPB_SUFFIX");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
			}       
			else strcat(ext,"rp");

			strcat(descrip, ext);
			strcat(descrip, ")");
			ud_get_filename("Playback File", "Playback Files", ext,
						filename, &len, descrip, 1,UU_FALSE) ;
			break;
		case 8:
			strcpy(ext,"*.");
			strcpy(descrip, "Drawing Files (");
			p = ux_getenv("UM_DRAWING_SUFFIX");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
			}       
			else strcat(ext,"dw");
			strcat(descrip, ext);
			strcat(descrip, ")");
			ud_get_filename("Drawing Files", "Drawing Files", ext,
						filename, &len, descrip, 1,UU_FALSE) ;
			break;
		case 9:
			strcpy(ext,"*.");
			strcpy(descrip, "Symbol Files (");
			p = ux_getenv("UB_SYM_SUFFIX");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
			}       
			else strcat(ext,"sy");
			strcat(descrip, ext);
			strcat(descrip, ")");
			ud_get_filename("Symbol Files", "Symbol Files", ext,
						filename, &len, descrip, 1,UU_FALSE) ;
			break;
		case 10:
		default:
			ud_get_filename("browse", "All Files", "*.*",
						filename, &len, "All Files (*.*)", 1,UU_FALSE) ;
			break;
	}
	if (filename[0]!='\0')
	{
		ud_update_answer(*fieldno-1, (int*)filename);
	}
	else
/*
.....canceled
*/
	*fieldno = -1;
	return(UD_FLDOK);
}


/*********************************************************************
**       E_FUNCTION : ulu_copy
**                      This routine copies one file to another.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS:    none.
**       SIDE EFFECTS: none.
**       WARNINGS:
*********************************************************************/
void ulu_copy()
{
	UX_pathname pathnm, pathto;
	UX_pathname outpathnm, outpathto;
	int status;
	int *ans[5];
	int option;
	UD_FSTAT uj_noop();
	static int type = 10;
	static UD_METHOD methods[5] = {
		uj_noop, uj_noop, ul_browse_file, uj_noop, ul_browse_file };
	static char called[] = { 6,6,6,6,6};
	static char traverse[] = { 1,1,1,1,1};
	static char display[] = {1,1,1,1,1,1};

	uu_denter(UU_ITRC,(us,"ulu_copy"));
/*
.....use Motif interface form
.....Yurong 9/14/98
*/
	option = 0;
	strcpy(pathnm, "");
	strcpy(pathto, "");
	file_type = type;
	ans[0] = (int *)&file_type;
	ans[1] = (int *)pathnm;
	ans[2] = (int *)&option;
	ans[3] = (int *)pathto;
	ans[4] = (int *)&option;
	status = ud_form1("jcpyfile.frm",ans, ans, methods, called, display, traverse);
   if (status==-1)
	   return;
/*
.....save file type
*/
	type = file_type;
	if ((strlen(pathnm)== 0)||(strlen(pathto)==0))
	{
		return;
	}
/* 
.....add quotes 
*/
	strcpy(outpathnm, "\"");
	strcat(outpathnm, pathnm);
	strcat(outpathnm, "\"");
	strcpy(outpathto, "\"");
	strcat(outpathto, pathto);
	strcat(outpathto, "\"");

	switch (file_type)
	{
		case 0:
			uj_copyf_ppart(outpathnm, outpathto, "UR_UNB_FILE",
									"UR_SOL_FILE","UR_AG_FILE","UR_UNB_PFILE",
									"UR_SOL_PFILE","UR_AG_PFILE");
			break;
		case 1:
			uj_copyf(outpathnm, outpathto, "pp");
			break;
		case 2:
			uj_copyf_plot(outpathnm, outpathto);
			break;
		case 3:
			uj_copypentb(outpathnm, outpathto);
			break;
		case 4:
			uj_copysetup(outpathnm, outpathto);
			break;
		case 5:
			uj_copyf(outpathnm, outpathto, "UL_CLFILE1_SUFFIX");
			break;
		case 6:
			uj_copyf(outpathnm, outpathto, "as");
			break;
		case 7:
			uj_copyf(outpathnm, outpathto, "UD_RPB_SUFFIX");
			break;
		case 8:
			uj_copyf(outpathnm, outpathto, "UD_DRAWING_SUFFIX");
			break;  
		case 9:
			ubu_copy_sym_file(outpathnm, outpathto);
			break;
		case 10:
		default:
			ux_copy(outpathnm, outpathto, UU_NULL, UX_PRTERRS);
			break;
	}

	uu_dexit;
	return;
}

/*********************************************************************
**       E_FUNCTION : ulu_delete
**                      This routine deletes a file.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS:    none.
**       SIDE EFFECTS: none.
**       WARNINGS:
*********************************************************************/
void ulu_delete()
{
	char *index();
	int numint;
	UX_pathname fname,outpath;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ulu_delete"));
	fname[0] = '\0';
/*
.....Get name of file to copy from
*/
	ud_get_filename("Name of file to delete","DELETE FILE","*.*",
								fname,&numint, "All Files (*.*)", 1,UU_FALSE);

	ul_strip_blanks (fname,&numint);
	strcpy(outpath, fname);
	if (numint == 0) goto done;
	if ( ud_yesno(0, uu_uprompt1(UJ_SUPPORT, 10, outpath),
		"Delete File") == UU_TRUE)
	{
		ux_delete(outpath,UX_PRTERRS);
	}

done:;
	uu_dexit;
	return;
}       

/*********************************************************************
**       E_FUNCTION : ulu_edit(flag)
**                      This spawns the system editor using the specified
**                      file.
**       PARAMETERS     
**               INPUT  :  flag =       1 = Current part program.
**                                      2 = Current PR file.
**                                      3 = Current PR1 file.
**                                      4 = Current PU1 file.
**                                      5 = By filename.
**               OUTPUT :  none.
**       RETURNS:    none.
**       SIDE EFFECTS: Spawns a system editor.
**       WARNINGS:
*********************************************************************/
void uledit(nclwrk1,nc1,nclwrk2,nc2)
UM_f77_str_ptr nclwrk1, nclwrk2;        
UM_int4 *nc1,*nc2;
{
	char *ptr,*ux_getenv(),*index();
	char com[UX_MAX_FILE_LEN+40],com2[UX_MAX_FILE_LEN+40];
	char buf[UX_MAX_FILE_LEN];
	char *flnam1,*flnam2;
	int CURSES;
    UM_int2 ifl,vtmode;

	ptr = ux_getenv("UL_NCL_EDIT",UX_NPRTERRS);
	flnam1 = UM_cstr_of_f77_str(nclwrk1);
	flnam1[*nc1]='\0';
	flnam2 = UM_cstr_of_f77_str(nclwrk2);
	flnam2[*nc2]='\0';


	if (ptr != UU_NULL)
	{
		strcpy (buf,ptr);
		ptr = index (buf,'*');
		if (ptr != UU_NULL)
		{
			*ptr = '\0';
			strcpy (com,buf);
			strcat (com,flnam1);
			ptr++; 
			strcat (com,ptr); 
		}
		else
		{
			strcpy (com,buf);
			strcat (com," ");
			strcat (com,flnam1);
		}
	}
	else
	{
		strcpy (com,editor);
		strcat (com,flnam1);
	}

	/* to copy the nclwrk1.tmp to nclwrk2.tmp */
#if UU_COMP == UU_VAXVMS
	sprintf (com2,"%s/out=%s",com,flnam2);
	ul_spawn (com2,2);
#else

/*
c...Added check for NCL-VT mode
c...Paul  -  11/01/91
c
*/ 
/*  UM_int2 ifl,vtmode;   */

    ifl = 35;
    getifl (&ifl,&vtmode);
    if (vtmode == 2)
    {
       savescr();
       clrscr();
       trmrst();
       system (com);
       CURSES = 1;
       trmatt(&CURSES);
       restscr();
    }
    else
	   ul_spawn (com,2);

	if (strcmp(flnam1,flnam2) != 0)
	{
		sprintf (com2,"%s %s %s",move,flnam1,flnam2);
		ul_spawn (com2,0);
	}
#endif

	return;
}

void ulu_edit(flag)
	int flag;
{
	char *ux_getenv(),*index();
	UX_pathname file;
	int numint;
/*
.....Debug Enter
*/
	file[0] = '\0';
	switch (flag)
	{
/*
.....Current Part Program
*/
	case 1:
		strcpy (file,UL_program);
		strcat (file,".");
		strcat (file,UL_program_suffix);
		break;
/*
.....Current PR file
*/
	case 2:
		strcpy (file,UL_program);
		strcat (file,".pr");
		break;
/*
.....Current PR1 file
*/
	case 3:
		strcpy (file,UL_program);
		strcat (file,".pr1");
		break;
/*
.....Current PU1 file
*/
	case 4:
		strcpy (file,UL_program);
		strcat (file,".pu1");
		break;
/*
.....By filename
*/
	case 5:
		ud_get_filename("Enter filename to edit","Edit File","*.*",
								file,&numint, "All Files (*.*)", 1,UU_FALSE);
		if (numint == 0) goto done;
		break;
	}
	ul_fix_wnt_file(file,file);
/*
.....Edit file
*/
	ul_edit(file);
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**       E_FUNCTION : ul_edit(fname)
**           This routine spawns the system editor using the specified file.
**       PARAMETERS     
**           INPUT  :  fname =   Name of file to edit.
**           OUTPUT :  none.
**       RETURNS:    none.
**       SIDE EFFECTS: Spawns a system editor.
**       WARNINGS:
*********************************************************************/
void ul_edit(fname)
char *fname;
{
	char *ptr,*ux_getenv(),*index();
	UX_pathname file,com,buf;
/*
.....Edit file
*/
	strcpy(file,fname);
	ptr = ux_getenv("UL_NIS_EDIT",UX_NPRTERRS);
	if (ptr != UU_NULL)
	{
		ul_add_quotes(file,file);
		strcpy (buf,ptr);
		ptr = index(buf,'*');
		if (ptr != UU_NULL)
		{
			*ptr = '\0';
			strcpy (com,buf);
			strcat (com,file);
			ptr++;
			strcat (com,ptr);
		}
		else
		{
			strcpy (com,buf);
			strcat (com," ");
			strcat (com,file);
		}
	}
	else
	{
		strcpy (com,editor);
		strcat (com,file);
	}
	ul_spawn (com,2);
done:;
	return;
}

/*********************************************************************
**       E_FUNCTION : ulu_print(flag)
**                      This submits a file to the system print que.
**       PARAMETERS     
**               INPUT  :  flag =       1 = Current part program.
**                                      2 = Current PR file.
**                                      3 = Current PR1 file.
**                                      4 = Current PU1 file.
**                                      5 = By filename.
**               OUTPUT :  none.
**       RETURNS:    none.
**       SIDE EFFECTS: Spawns the system print utility.
**       WARNINGS:
*********************************************************************/

void ulu_print(flag)
	int flag;
{
	UX_pathname file,com,buf;
	char *position,*rindex();
	int numint,stat,file_status,mode;
#if UU_COMP == UU_VAXVMS
#define print "PRINT "
#endif
#if UU_COMP == UU_SUN || UU_COMP == UU_CIM
#define print "lpr "
#endif
#if UU_COMP == UU_IRIS4D || UU_COMP == UU_DECUNIX || UU_COMP == UU_HPUX || UU_COMP == UU_WINNT || UU_COMP == UU_WIN2K
#define print "lp "
#endif
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ulu_print(flag)"));
	file[0] = '\0';
	switch (flag)
	{
/*
.....Current Part Program
*/
	case 1:
		strcpy (file,UL_program);
		strcat (file,".");
		strcat (file,UL_program_suffix);
		break;
/*
.....Current PR file
*/
	case 2:
		strcpy (file,UL_program);
		strcat (file,".pr");
		break;
/*
.....Current PR1 file
*/
	case 3:
		strcpy (file,UL_program);
		strcat (file,".pr1");
		break;
/*
.....Current PU1 file
*/
	case 4:
		strcpy (file,UL_program);
		strcat (file,".pu1");
		break;
/*
.....By filename
*/
	case 5:
/*              ud_das (UD_DASSTRING,"Enter filename to print",file,
			sizeof(file),&numint);*/
		ud_get_filename("Enter filename to print","PRINT FILE","*.*",
								file,&numint, "All Files (*.*)", 1,UU_FALSE);
#if UU_COMP != UU_WIN2K
		ul_strip_blanks (file,&numint);
#endif
		if (numint == 0) goto done;
		break;
	}
/*
.....Verify that the file exists
*/
	strcpy (com,file);
	position = rindex(com,'/');
	if (position != 0) *position = '\0';
	mode = UX_EXISTS|UX_READ;
	stat = ux_file_inquire(UU_NULL,UU_NULL,com,UU_NULL,UU_NULL,&mode,
		&file_status,buf,UX_NPRTERRS);
	if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
	{
		sprintf (buf,"%s does not exist.",com);
		ud_wrerr (buf);
		goto failed;
	}
/*
.....Print file
*/
	strcpy (com,print);
	strcat (com,file);
	ul_spawn (com,1);
failed:;
done:;
	uu_dexit;
	return;
}

/*********************************************************************
**       E_FUNCTION : ulu_purge
**                      This routine spawns the PURGE command.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS:    none.
**       SIDE EFFECTS: Spawns the system PURGE utility.
**       WARNINGS:
*********************************************************************/
void ulu_purge()
{
#if UU_COMP == UU_VAXVMS
	UX_pathname file,com,buf;
	char *position;
	int numint,stat,file_status,mode;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ulu_purge()"));
/*
.....Get filename to purge
*/
	strcpy (file,"*.*");
	ul_string_def ("Enter file to purge",file,sizeof(file),
			&numint,&file_status);
	ul_strip_blanks (file,&numint);
	if (numint == 0) goto done;
/*
.....Verify that the file exists
	strcpy (com,file);
	position = rindex(com,'/');
	if (position != 0) *position = '\0';
	mode = UX_EXISTS|UX_READ;
	stat = ux_file_inquire(UU_NULL,UU_NULL,com,UU_NULL,UU_NULL,&mode,
		&file_status,buf,UX_NPRTERRS);
	if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
	{
		sprintf (buf,"%s does not exist.",com);
		ud_wrerr (buf);
		goto failed;
	}
*/
/*
.....Purge file
*/
	strcpy (com,"PURGE ");
	strcat (com,file);
	ul_spawn (com,0);
failed:;
done:;
#endif
	uu_dexit;
	return;
}

/*********************************************************************
**       E_FUNCTION : ulu_rename
**                      This routine renames a file.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS:    none.
**       SIDE EFFECTS: none.
**       WARNINGS:
*********************************************************************/
void ulu_rename()
{
	UX_pathname pathnm, pathto;
	UX_pathname outpathnm, outpathto;
	int status;
	int *ans[5];
	int option;
	UD_FSTAT uj_noop();
	static int type = 10;
	static UD_METHOD methods[5] = {
		uj_noop, uj_noop, ul_browse_file, uj_noop, ul_browse_file };
	static char called[] = { 6,6,6,6,6};
	static char traverse[] = { 1,1,1,1,1};
	static char display[] = {1,1,1,1,1,1};

	uu_denter(UU_ITRC,(us,"ulu_rename"));
/*
.....use Motif interface form
.....Yurong 9/14/98
*/
	option = 0;
	strcpy(pathnm, "");
	strcpy(pathto, "");
	file_type = type;
	ans[0] = (int *)&file_type;
	ans[1] = (int *)pathnm;
	ans[2] = (int *)&option;
	ans[3] = (int *)pathto;
	ans[4] = (int *)&option;
	status = ud_form1("jrenfile.frm",ans, ans, methods, called, display, traverse);
	if (status==-1)
	   return;
/*
.....save file type
*/
	type = file_type;
	if ((strlen(pathnm)== 0)||(strlen(pathto)==0))
	{
		return;
	}
/* 
.....add quotes 
*/
	strcpy(outpathnm, "\"");
	strcat(outpathnm, pathnm);
	strcat(outpathnm, "\"");
	strcpy(outpathto, "\"");
	strcat(outpathto, pathto);
	strcat(outpathto, "\"");

	switch (file_type)
	{
		case 0:
			uj_renmf_ppart(outpathnm, outpathto, "UR_UNB_FILE",
									"UR_SOL_FILE","UR_AG_FILE","UR_UNB_PFILE",
									"UR_SOL_PFILE","UR_AG_PFILE");
			break;
		case 1:
			uj_renmf(outpathnm, outpathto, "pp");
			break;
		case 2:
			uj_renmf_plot(outpathnm, outpathto);
			break;
		case 3:
			uj_renmf(outpathnm, outpathto, "UJ_PEN_SUFFIX");
			break;
		case 4:
			uj_renmf(outpathnm, outpathto, "UJ_PLOT_SUFFIX");
			break;
		case 5:
			uj_renmf(outpathnm, outpathto, "UL_CLFILE1_SUFFIX");
			break;
		case 6:
			uj_renmf(outpathnm, outpathto, "as");
			break;
		case 7:
			uj_renmf(outpathnm, outpathto, "UD_RPB_SUFFIX");
			break;
		case 8:
			uj_renmf(outpathnm, outpathto, "UD_DRAWING_SUFFIX");
			break;  
		case 9:
			ubu_rename_sym_file(outpathnm, outpathto);
			break;
		case 10:
		default:
			ux_rename(outpathnm,outpathto,UX_PRTERRS);
			break;
	}

	uu_dexit;
	return;
}
 
/*********************************************************************
**       E_FUNCTION : ul_status_print
**                      This routine shows the status of the operating
**                      system print que.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS:    none.
**       SIDE EFFECTS: Opens a window and spawns a sub-process.
**       WARNINGS:
*********************************************************************/
void ul_status_print()
{
#if UU_COMP == UU_VAXVMS
	ul_spawn ("SHOW QUE/DEVICE",1);
#endif
#if UU_COMP == UU_SUN 
	ul_spawn ("lpq",1);
#endif
#if UU_COMP == UU_IRIS4D || UU_COMP == UU_DECUNIX || UU_COMP == UU_HPUX || UU_COMP == UU_WINNT
	ul_spawn ("lpstat",1);
#endif
	return;
}

/*********************************************************************
**       E_FUNCTION : ulrenm(temp1, temp2)
**                      This renams file1 to file2
**       PARAMETERS     
**               OUTPUT :  none.
**       RETURNS:    none.
**       SIDE EFFECTS: Spawns a system move/rename.
**       WARNINGS:
*********************************************************************/
void ulrenm(temp1,temp2)
UM_f77_str_ptr temp1, temp2;    
{
	char com[UX_MAX_PATH_LEN+40];
	char *flnam1,*flnam2;
	int i;
#if UU_COMP == UU_VAXVMS
#define rename "rename "
#else
#define rename "mv "
#endif

	flnam1 = UM_cstr_of_f77_str(temp1);
	for (i=0; i<UX_MAX_PATH_LEN && flnam1[i] != ' '; i++);
	flnam1[i]='\0';
	flnam2 = UM_cstr_of_f77_str(temp2);
	for (i=0; i<UX_MAX_PATH_LEN && flnam2[i] != ' '; i++);
	flnam2[i]='\0';


	/* to rename the flnam1 to flnam2 */
#if UU_COMP == UU_VAXVMS
	sprintf (com,"%s %s %s",rename,flnam1,flnam2);
	ul_spawn (com,0);
#else
	sprintf (com,"%s %s %s",rename,flnam1,flnam2);
	ul_spawn (com,0);
#endif

	return;
}

/*********************************************************************
**       E_FUNCTION : ul_fix_wnt_file(fin, fout)
**                      Converts the Nutcracker '/C=/' device name syntax to 'C:/'.
**       PARAMETERS     
**               INPUT  :  fin = Input filename.
**               OUTPUT :  fout = Output filename..
**       RETURNS:    none.
**       SIDE EFFECTS: none
**       WARNINGS:
*********************************************************************/
void ul_fix_wnt_file(fin,fout)
char *fin,*fout;
{
#if UU_COMP == UU_WINNT
	char tmp[1024];
	if (fin[0] == '/' && fin[2] == '=')
	{
		strcpy(tmp,fin);
		fout[0] = tmp[1];
		fout[1] = ':';
		strcpy(&fout[2],&tmp[3]);
	}
	else
#endif
		strcpy(fout,fin);
}
