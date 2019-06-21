/*********************************************************************
**
**    NAME         :  d3rp.c
**
**       CONTAINS:
**                        ud_recon
**                        ud_playback
**                        ud_getrpdir
**                        ud_prompt     
**                        ud_recstart(fname,stage)
**                        ud_writef(fullname)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d3rp.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       02/08/17 , 11:34:46
**
*********************************************************************/

#include "usysdef.h"
#include "dinput.h"
#include "dasnog.h"
#include "dasg.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "udebug.h"
#include "nclver.h"

static char     recname[UX_MAX_PATH_LEN];     /* save the record file name */
static char     homedir[UX_MAX_PATH_LEN];
static int      first = 0;

double NCL_rpfile_version;

void ud_getrpdir();

/**************************************************************************
**
**  E_FUNCTION         :  ud_prompt()
**      record and playback prompt 
**
**  PARAMETERS   
**      INPUT  : 
**          none
**      OUTPUT :  
**          none
**
**  RETURNS      :  
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
****************************************************************************/
ud_prompt()
{
	UD_DEVENT event;
	int i, markval, saveint, savedev, savepet;
	char buffer[100];
	if (UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{
/*
... get the prompt line and use the error message
... mechanism to output it
*/
		ud_rprd(&event, UU_FALSE);
		if(event.indata.stringdata[0] != '\0')
			ud_prmerr(event.indata.stringdata);
	}
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
	{
		UD_MARK(markval, UU_TRUE);
		if(markval == 0)
		{

/*
...reset the string pet if a form is up 
*/
			ud_qstr(&saveint, &savedev, &savepet);
			ud_dtstr(UD_STRING, 1, 1);

/*
...get the prompt line
*/

			strcpy(buffer, "");

/*
...prompt line
*/
			ud_ldas(UD_DASSTRING, UD_DASHEP, 3, buffer, 100,
													&i, UD_DEFAULT);

/*
reset the string pet to what it was
*/

			ud_dtstr(saveint, savedev, savepet);
		}
			UD_UNMARK(markval);
	}
}
/**************************************************************************
**
**  E_FUNCTION         :  ud_recon()
**      start up record mode
**
**  PARAMETERS   
**      INPUT  : 
**          none
**      OUTPUT :  
**          none
**
**  RETURNS      :  UU_TRUE if now recording, UU_FALSE otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

UU_LOGICAL 
ud_recon()
{
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	char            fname[UX_MAX_PATH_LEN];       /* file name buffer */
	char            fullname[UX_MAX_PATH_LEN];     /* file name buffer */
	int             len;
	int             status; /* open status return */
	int             mode;   /* file status */
	UD_STRREC       str_rec;/* default string record */
	UX_pathname     bname;
	char *p, ext[UX_MAX_FILE_LEN], ext1[UX_MAX_FILE_LEN],*ux_getenv();
	char descrip[UX_MAX_FILE_LEN];
	char *uu_uprompt0();

	/* --- Start of Executable Code --- */

	uu_denter(UU_DTRC, (us, "in recon, RPSTATE:ptr=%d, flag=%d, lun=%d",
			    UD_Rpstate_ptr,
			    UD_Rpstate[UD_Rpstate_ptr].flag,
			    UD_Rpstate[UD_Rpstate_ptr].rplun));

	uu_dprint(UU_DTRC, (us, "in recon, homedir=%s", homedir));

	/*
	 * -- cannot record if already recording, playing back, or suspended
	 * -- 
	 */
	if (UD_Rpstate[UD_Rpstate_ptr].flag == RPOFF) {

		/* --- enter record file name: --- */

		fname[0] = '\0';
		strcpy(ext,"*.");
		p = ux_getenv("UD_RPB_SUFFIX");
		if (p != UU_NULL)
		{
			strcpy(ext1,p);
			ul_remove_quotes(ext1);
			strcat(ext,ext1);
		}       
		else 
			strcat(ext,"rp");
		strcpy(descrip, "Record/Playback File (");
		strcat(descrip,ext);
		strcat(descrip, ")");

		strcpy(paths, "U_RECORD_PLAYB_AREA");
		strcpy(path_des, "System");
		ud_get_filename1(NULL, uu_uprompt0(UD_DASHEP, 7), ext, fname,&len, descrip, 0, UU_FALSE, paths, path_des);

		while (ux_get_base_fname(fname, bname, (UX_NPRTERRS | UX_NCHK)) !=
		       UU_SUCCESS)
		{
			uu_uerror0(UX_UDOS,24);
			return (UU_FALSE);
		}
		ud_recstart(fname,2);
	} 
	else 
	{

		/*---           " currently recording or suspended"             ---*/

		uu_uerror0(UD_DASHEP, 62);
		goto error;
	}

	uu_dprint(UU_DTRC, (us, "RPSTATE:ptr=%d, flag=%d, lun=%d",
			    UD_Rpstate_ptr,
			    UD_Rpstate[UD_Rpstate_ptr].flag,
			    UD_Rpstate[UD_Rpstate_ptr].rplun));

	uu_dprint(UU_DTRC, (us, "leaving recon, status == UU_TRUE"))
		uu_dexit;
	return (UU_TRUE);
error:
	uu_dprint(UU_DTRC, (us, "leaving recon, status == UU_FALSE"))
		uu_dexit;
	return (UU_FALSE);
}
/**************************************************************************
**
**  E_FUNCTION         :  ud_recstart(fname, stage)
**     check for record file and start record 
**
**  PARAMETERS   
**      INPUT  : 
**          fname: record file name
**                              stage: 1 means calls from menu. 2 called from signon form
**      OUTPUT :  
**          none
**
**  RETURNS      :  UU_TRUE if now recording, UU_FALSE otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_recstart(fname,stage)
char *fname;
int stage;
{
	int len, mode, status;
	char fullname[UX_MAX_PATH_LEN];
	char    dir[UX_MAX_PATH_LEN], bname[UX_MAX_FILE_LEN];                       
	char *uu_uprompt0();
	len = strlen(fname);
	if (len == 0)
		goto error;

		/* -- convert to system dependent name -- */

	if (first == 0)
		ud_getrpdir();
	mode = 0;       /* Check for file existence */
/*
.....the fname may already include a path
.....Yurong 9/25/98
*/
	ul_break_fname(fname, dir, bname);
	if (dir[0]!='\0')
	{
		status = ux_mk_chk_syspath(UU_NULL, UU_NULL, fname, UU_NULL,
						"UD_RPB_SUFFIX", &mode, fullname, UX_PRTERRS);
	}
	else
	{
		status = ux_mk_chk_syspath(UU_NULL, homedir, fname, UU_NULL,
		      "UD_RPB_SUFFIX", &mode, fullname, UX_PRTERRS);
	}

	if (!(mode & UX_NEXISTS)) 
	{
		if (ud_yesno(0, uu_uprompt0(UD_DASHEP, 8), "File Exists")) 
		{
			status = ux_delete(fullname, UX_PRTERRS);
		} 
		else 
		{
			goto error;
		}
	}
	ud_writef(fullname);
	return (UU_TRUE);

error:
	uu_dprint(UU_DTRC, (us, "leaving recon, status == UU_FALSE"))
		uu_dexit;
	return (UU_FALSE);
}
/**************************************************************************
**
**  E_FUNCTION         :  ud_writef(fname)
**     write the begin part of record file 
**
**  PARAMETERS   
**      INPUT  : 
**          fname: record file name
**                              stage: 1 means calls from menu. 2 called from signon form
**      OUTPUT :  
**          none
**
**  RETURNS      :  UU_TRUE if now recording, UU_FALSE otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_writef(fullname)
char *fullname;
{
	int status;
		status = ux_create_file(fullname, 0666, UU_NULL, "STREAM", "ASCII",
		"UX_NOEXTRA", &UD_Rpstate[UD_Rpstate_ptr].rplun, UX_PRTERRS);

		if (status == 0)
			/*
			 * -- save the file name to check for possible
			 * playback while recording it -- 
			 */

		{
			strcpy(recname, fullname);
			UD_Rpstate[UD_Rpstate_ptr].flag = RECORD;
		} else {

			/* --- " cannot open file"              --- */

			uu_uerror0(UD_DASHEP, 61);
			goto error;
		}
	return (UU_TRUE);

error:
	uu_dprint(UU_DTRC, (us, "leaving recon, status == UU_FALSE"))
		uu_dexit;
	return (UU_FALSE);
}

/**************************************************************************
**
**  E_FUNCTION         :  ud_playback(fname)
**      to playback a record file
**
**  PARAMETERS   
**      INPUT  : 
**          fname = playback filename
**                                              
**      OUTPUT :  
**          none
**
**  RETURNS      :  UU_TRUE if successfull
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
/*
.....pass in fname
*/
UU_LOGICAL 
ud_playback(parms)
	char *parms;
{
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	char  fullname[UX_MAX_PATH_LEN];       /* file name buffer */
	char    dir[UX_MAX_PATH_LEN], bname[UX_MAX_FILE_LEN];                       
	char  fname[UX_MAX_PATH_LEN];  /* file name buffer */
	int             len;
	int             status; /* status return cell */
	int             mode;   /* file status */
	UD_STRREC       str_rec;/* default string record */
	UD_DEVENT event;
	char *p, ext[UX_MAX_FILE_LEN], ext1[UX_MAX_FILE_LEN],*ux_getenv();
	char descrip[UX_MAX_FILE_LEN];
	char *uu_uprompt0();
	UX_fheader hdr;
	int hstat, user_opened;
	/* --- Start of Executable Code --- */

	uu_denter(UU_DTRC, (us, "entering ud_playback, command=%s", command));

	uu_dprint(UU_DTRC, (us, "RPSTATE:ptr=%d, flag=%d, lun=%d",
			    UD_Rpstate_ptr,
			    UD_Rpstate[UD_Rpstate_ptr].flag,
			    UD_Rpstate[UD_Rpstate_ptr].rplun));

	if (first == 0)
		ud_getrpdir();

	uu_dprint(UU_DTRC, (us, "in playback, homedir=%s", homedir));

	if (UD_Rpstate_ptr < UD_MAXPLAYB - 1) {
/*
.....we are not using command anymore
.....Yurong
		if (strlen(command) > 7)
			strcpy(fname, &command[7]);
		else {
*/
			/*--            enter playback filename: --*/
		user_opened = 0;
		strcpy(fname,parms);
		if (fname[0]=='\0')
		{
			strcpy(ext,"*.");
			p = ux_getenv("UD_RPB_SUFFIX");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
			}       
			else strcat(ext,"rp");
			strcpy(descrip, "Record/Playback File (");
			strcat(descrip,ext);
			strcat(descrip, ")");
			strcpy(paths, "U_RECORD_PLAYB_AREA");
			strcpy(path_des, "System");
			ud_get_filename1(NULL, uu_uprompt0(UD_DASHEP, 10), ext, fname,&len, descrip, 1, UU_FALSE, paths, path_des);
			if (len == 0)
				goto done;
			user_opened = 1;
		} 

		/* -- convert file name to system dependent name -- */

		mode = 0;
/*
.....the fname may already include a path
.....Yurong 9/25/98
*/
		ul_break_fname(fname, dir, bname);
		if (dir[0]!='\0')
		{
			status = ux_mk_chk_syspath(UU_NULL, UU_NULL, fname, UU_NULL,
							"UD_RPB_SUFFIX", &mode, fullname, UX_PRTERRS);
		}
		else
		{
			status = ux_mk_chk_syspath(UU_NULL, homedir, fname, UU_NULL,
			      "UD_RPB_SUFFIX", &mode, fullname, UX_PRTERRS);
		}
		if (mode & UX_NEXISTS) {
			uu_uerror1(UD_DASHEP, 60, fullname);
			goto done;
		}
		status = ux_open_to_data(fullname, "r", "STREAM", "ASCII",
			 &UD_Rpstate[UD_Rpstate_ptr + 1].rplun, UX_PRTERRS);

		uu_dprint(UU_DTRC, (us, "in ud_playback, open status=%d", status));

		if (status == 0) {
			UD_Rpstate_ptr++;
			UD_Rpstate[UD_Rpstate_ptr].flag = PLAYBACK;			
			NCL_rpfile_version = NCL_infile_version;
		} else {

			/*---                           " cannot open file"             ---*/

			uu_uerror0(UD_DASHEP, 61);
			goto done;
		}
	} else {

		/*---           "cannot playback while another record or playback in effect"    ---*/

		uu_uerror0(UD_DASHEP, 66);
		goto done;
	}
	if (user_opened)
		nclc_save_recent_file(fullname, 2);

	uu_dprint(UU_DTRC, (us, "RPSTATE:ptr=%d, flag=%d, lun=%d",
			    UD_Rpstate_ptr,
			    UD_Rpstate[UD_Rpstate_ptr].flag,
			    UD_Rpstate[UD_Rpstate_ptr].rplun));

	uu_dexit;
	return (UU_TRUE);

	/* -- unsuccessful exit -- */

done:
	uu_dexit;
	return (UU_FALSE);
}

/**************************************************************************
**
**  E_FUNCTION         :  ud_getrpdir()
**      get the R/P directory
**
**  PARAMETERS   
**      INPUT  : 
**          none
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
void ud_getrpdir()
{
	char           *p, *ux_getenv();

	if (first == 0) {
		first = 1;

		p = ux_getenv("U_RECORD_PLAYB_AREA", UX_PRTERRS);
		if (p != NULL)
			strcpy(homedir, "^U_RECORD_PLAYB_AREA");
		else
			strcpy(homedir, "^UX_HOMEDIR");
	}
}

