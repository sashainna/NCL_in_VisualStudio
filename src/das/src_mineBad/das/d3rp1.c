/*********************************************************************
**
**    NAME         :  rpstuff.c
**
**       CONTAINS:
**                        ud_recoff
**                        ud_susp
**                        ud_resu
**                        ud_rprd
**                        ud_rdform
**                        ud_rdbrowser
**                        ud_rpwr
**                        ud_rdsignon(filename, cam, cad)
**                        ud_signinit(fullname)
**                        ud_playinit
**                        ud_delay
**                        ud_rpwrmenu(key,msg)
**                        ud_wrsignon(filename, cam, cad)
**                        ud_rpwrbrowser(key, msg)
**                        ud_rpwrcom
**                        ud_decrp
**                        ud_inevt
**                        ud_mult_inevt
**                        ud_outevt
**                        ud_record_auto
**                        ud_signon_play()
**                        ud_menuplay(func, parms, name)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**            d3rp1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**            04/29/15 , 15:05:07
**
*********************************************************************/

#include <stdio.h>
#include "ustdio.h"
#include "usysdef.h"
#include "gtbl.h"
#include "gi.h"
#include "usysg.h"
#include "dinput.h"
#include "dasnog.h"
#include "dasg.h"
#include "udebug.h"
#include "uhep.h"
#include "xenv1.h"
#include "udfrmdec.h"
#include "udfrmdef.h"
#include "ddef.h"
#include "dtypes.h"
#include "udfdata.h"
#include<string.h>
#include "gtbl.h"
#include "gdidd.h"
#include "zkeysym.h"
#include "nclver.h"
#include "mdcpln.h"

extern int UW_nosignonform;
extern int formActive;
extern double NCL_rpfile_version;
void ud_convert_playback_pos();

/**********************************************************************
**    I_FUNCTION :  ud_form_ckdata(fno,buf)
**                      changed from uw_mfform_ckdata(fno,buf)
**       Checks form input data for correct type and values.
**    PARAMETERS   
**       INPUT  : 
**          fno     = Field number to check.
**                              buf     = Form input data to check.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_form_ckdata(fstruct, fdata, fno,buf)
UD_FSTRUCT *fstruct;
UD_FDATA *fdata;
int fno;
char *buf;
{
	UD_DASDATA dsda;
	UD_DASIN de;
	UD_LIST *list_ans;
	int irtn,typ,stat,nc,i,ifl;
	char text[80], erms[80];
/*
.....Initialize routine
*/
	irtn = UU_TRUE;
	for (i=strlen(buf)-1;i>=0;i--) if (buf[i] > ' ') break;
	buf[i+1] = '\0';
	typ = fstruct->input_flds[fno].ud_datatyp;
	if ((typ>=UD_DASSCALAR) && (typ<=UD_SCAUNITLESS))
	{
/*
.....Check if the string is the scalar value/string
.....even though we may keep and return string, but
.....we still need get the value in order to check the range
*/
		strcpy(text,buf);
/*
.....Get the scalar and convert to value
*/
		stat = ncl_parse_scalar_values(text, text, typ);
		if (stat==-1)
		{
			sprintf(erms,"%s is not a valid scalar value",buf);
			ud_wrerr(erms);
			return UU_FALSE;
		}
	}
	if (typ == UD_DASVAL || typ == UD_DASUNITLESS ||
		typ == UD_DASDISTANCE || typ == UD_DASANGLE ||
			typ == UD_DASINT || typ == UD_DASVEC || 
			typ == UD_DASCART )
	{
		strcpy(text,buf);
/*
.....Get the scalar and convert to value
*/
		stat = ncl_parse_scalar_values(text, text, typ);
		if (stat==-1)
		{
			sprintf(erms,"%s is not a valid scalar value",text);
			ud_wrerr(erms);
			return UU_FALSE;
		}
	}
/*
.....Parse if not string input
*/
	if (typ != UD_DASSTRING)
	{
/*
.....if it's a scalar string value, use text get above
*/
		if (stat!=1)
			strcpy(text,buf);
		nc = strlen(text);
		ul_strip_blanks(text,&nc);
		ifl = 0;
		if (typ == UD_DASVAL || typ == UD_DASUNITLESS || 
		    typ == UD_DASINT || typ == UD_DASVEC || typ == UD_DASNDC
			|| typ == UD_DASSCALAR)
			ifl = UD_UNITLESS;
		else if (typ == UD_DASANGLE) ifl = UD_DANGLE;
		irtn = ud_dasin(text,&dsda,ifl);
		if (!irtn) goto failed;
/*
.....handle for for scalar
*/
		if ((typ>=UD_DASSCALAR) && (typ<=UD_SCAUNITLESS))
		{
			ud_scatodas(&dsda, &de, typ);
			stat = ud_ckdata(*fstruct, &de, fno);
			if (stat != UD_VALOK) goto failed;
			else
			{
				strcpy(fdata->ud_data[fno].ud_delem.frmstr,buf);
				goto done;
			}
		}
	}
/*
.....Verify correct type of input
*/
	switch (typ)
	{
/*
.....Allow for the MODSYS matrix
*/
	case UD_DASVEC:
		if (dsda.dtype != 2) goto failed;
		um_ccstomcs(1,dsda.stval.stcord.coord,fdata->ud_data[fno].ud_delem.frmvec);
/*
......adjust the value
*/
		UM_cc_exttoint(fdata->ud_data[fno].ud_delem.frmvec, fdata->ud_data[fno].ud_delem.frmvec);
		break;
	case UD_DASCART:
		if (dsda.dtype != 2) goto failed;
		um_ccstomcs(0,dsda.stval.stcord.coord,fdata->ud_data[fno].ud_delem.frmvec);
		break;
	case UD_DASNDC:
		if (dsda.dtype != 2) goto failed;
		fdata->ud_data[fno].ud_delem.frmvec[0] = dsda.stval.stcord.coord[0];
		fdata->ud_data[fno].ud_delem.frmvec[1] = dsda.stval.stcord.coord[1];
		fdata->ud_data[fno].ud_delem.frmvec[2] = dsda.stval.stcord.coord[2];
		break;
	case UD_DASSCALAR:
		strcpy(fdata->ud_data[fno].ud_delem.frmstr,buf);
		break;
	case UD_DASVAL:
	case UD_DASUNITLESS:
	case UD_DASDISTANCE:
	case UD_DASANGLE:
		if (dsda.dtype != 1) goto failed;
		fdata->ud_data[fno].ud_delem.frmflt[0] = dsda.stval.dval;
		break;
	case UD_DASINT:
		if (dsda.dtype != 1) goto failed;
		fdata->ud_data[fno].ud_delem.frmint[0] = (int)dsda.stval.dval;
		break;
	case UD_DASSTRING:
/*
.....added List here
.....Yurong 8/15/97
*/
		if (fstruct->input_flds[fno].toggle==5)
		{
			list_ans = (UD_LIST *)(fdata->ud_data[fno].ud_delem.frmint) ;
			if (i!=-1)
				strcpy(list_ans->answer, buf);
		}
		else
			strcpy(fdata->ud_data[fno].ud_delem.frmstr,buf);
			
		break;
	default:
		goto failed;
	}
/*
.....Check for valid range
*/
	if (typ != UD_DASSTRING)
	{
		ud_todas(&fdata->ud_data[fno].ud_delem,&de,typ);
		stat = ud_ckdata(*fstruct, &de,fno);
		if (stat != UD_VALOK) goto failed;
	}
	goto done;
/*
.....End of routine
*/
failed:;
	irtn = UU_FALSE;
done:;
	return(irtn);
}

/**********************************************************************
**    I_FUNCTION : ud_signon_play(buffer)
**       Get signon information from playback file 
**                      loads the input file, and enters interactive NCL.
**    PARAMETERS   
**       INPUT  : buffer: first sign on record/playback string
**       OUTPUT : none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_signon_play(buffer)
char *buffer;
{
	int cam,cad,iop[2], flag;
	char *filen,fullname[UX_MAX_PATH_LEN],dir[UX_MAX_PATH_LEN];
	char dir1[UX_MAX_PATH_LEN],fname[UX_MAX_FILE_LEN];
	char fn[UX_MAX_PATH_LEN];
	dir1[0] = '\0';
	cad = 1;
	cam = 1;
	filen = fn;
	filen[0] = '\0';
/*
...get signon information from playback file
...Yurong
*/
	flag = ud_rdsignon(buffer, &cam, &cad, filen);

	if (flag==0)
		return;
	
	strcpy(fullname,filen);
	ul_break_fname(fullname,dir,fname);
	strcat(dir, dir1);
	iop[0] = 1; iop[1] = 0;
	ul_get_base_fname(fname,fname,iop,UX_NPRTERRS);
/*
...  loads the input file, and enters interactive NCL.
*/
	UW_nosignonform = 1;
/*
	uw_mfsignload(dir, fname, cam, cad);
*/
	(*(ug_gksstli.wsopen[0].connid)[UW_SIGNON_LOAD])(dir, fname, cam, cad);
}

/**********************************************************************
**    I_FUNCTION : ud_menuplay(func, parms, name)
**                                              execute the menu function in the playback file
**    PARAMETERS   
**       INPUT  : func: funtion need execute
**                                      parms: function parameter
**                              name: name of func
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_menuplay(func, parms, name)
char *func, *parms;
char *name;
{
	int type,sub,irtn,ifl,type1;
	int pos[2];
	int size[2];
	char **junk;
	short app;
	char *indx;
	char buf[40];
	UZ_keytable ktab;
	pos[0] = -1;
	pos[1] = -1;
	size[0] = -1;
	size[1] = -1;
	if (strchr(func,'.')!=NULL)
	{
/*
...there is menu name needed load in
...load the menu
*/
		if (!formActive)
		{
			if (udm_read_menu(func,pos,size, 1, 1, -1) != UU_SUCCESS)
			{
				sprintf(buf,"Could not load menu: %s",func);
				ud_winerror(buf);
			}
		}
		return;
	}
	else if( ul_to_number(func , &type1)==UU_SUCCESS)
	{
		if(strcmp(name,"KEYBOARD")==0)
/*
...there is keyboard function
...call that function
*/
			uz_user_key(type1,&junk, 1);
	}
	else if (strstr(func,"\\") != NULL)
	{
/*
...there is function key 
...call that function
*/
			uz_user_dascalls(func);
	}
   else if (uz_which_keydef(func,&type,&sub,&app) == UU_SUCCESS)
/*
...menu user define function
*/
   {
      ktab.type = type;
      ktab.sub = sub;
      ktab.flag = app;
	  ktab.params = parms;
      ifl = 1;
      if (formActive) ifl = 0;
      irtn = uz_user_keydef(ktab,&indx,ifl);
		if (irtn == 1 && !formActive)
			uz_daskey1(sub,parms);
	}
}

/**************************************************************************
**
**  E_FUNCTION         :  ud_recoff()
**      to stop recording a session
**
**  PARAMETERS   
**
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

void ud_recoff()
{
	int status;                                             /* close status return cell */
	uu_denter(UU_DTRC,(us,"in recoff, RPSTATE:ptr=%d, flag=%d, lun=%d",
			UD_Rpstate_ptr,
			UD_Rpstate[UD_Rpstate_ptr].flag,
			UD_Rpstate[UD_Rpstate_ptr].rplun));
	if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK || 
		UD_Rpstate[UD_Rpstate_ptr].flag == RECSUSP)
	{
		status = ux_close(UD_Rpstate[UD_Rpstate_ptr].rplun, UX_PRTERRS);
		UD_Rpstate[UD_Rpstate_ptr].flag = RPOFF;
		UD_Rpstate_ptr--;
	}
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
	{
		status = ux_close(UD_Rpstate[UD_Rpstate_ptr].rplun, UX_PRTERRS);
		UD_Rpstate[UD_Rpstate_ptr].flag = RPOFF;

/*      -- clear out record file name -- */

	}
	uu_dprint(UU_DTRC,(us,"RPSTATE:ptr=%d, flag=%d, lun=%d",
			UD_Rpstate_ptr,
			UD_Rpstate[UD_Rpstate_ptr].flag,
			UD_Rpstate[UD_Rpstate_ptr].rplun));
	uu_dexit;
	return;
}

/**************************************************************************
**
**  E_FUNCTION         :  ud_susp()
**      suspend record or playback session
**
**  PARAMETERS   
**
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

void ud_susp()
{

	if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		UD_Rpstate[UD_Rpstate_ptr].flag = RECSUSP;
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
		UD_Rpstate[UD_Rpstate_ptr].flag = PLAYSUSP;
}

/**************************************************************************
**
**  E_FUNCTION         :  ud_resu()
**      resume record or playback session
**
**  PARAMETERS   
**
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

void ud_resu()
{

	if(UD_Rpstate[UD_Rpstate_ptr].flag == RECSUSP)
		UD_Rpstate[UD_Rpstate_ptr].flag = RECORD;
	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYSUSP)
		UD_Rpstate[UD_Rpstate_ptr].flag = PLAYBACK;
}

/**************************************************************************
**
**  I_FUNCTION   ud_rdsignon(buffer, cam, cad, filen)
**      read signon info from record and playback file
**
**  PARAMETERS   
**
**      INPUT  : buffer: first sign on record/playback string
**      OUTPUT :  
**              cad : cad taggle field 0 or 1
**              cam : cam taggle field 0 or 1
**              filen: file selected
**  RETURNS      :  
**				 1: read signon info
**				0: not read signon info
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
int ud_rdsignon(buffer, cam, cad, filen)
char *buffer, *filen;
int *cam, *cad;
{
	FILE *fd;                                                                                       /* os file descriptor */
	int status, status1, first, flag;
	char *junk1, *junk2, *num;
	char inchar;
	fpos_t save_pos;
	static char inbuffer[UD_RPREC_LEN];                     /* input buffer */
	status = 0;
	
	flag = 0;
	filen[0] = '\0';
	ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
	first = 1;
	while(1)
	{
		save_pos = -1;
		if (first)
		{
			strcpy(inbuffer, buffer);
			status1 = UU_SUCCESS;
		}
		else
		{
			if( fgetpos(fd, &save_pos ) != 0 )
			{
				save_pos = -1;
				status1 = -1;
			}
			else
				status1 = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		}
		if(status1 == UU_SUCCESS)
		{
/*                      -- write out to autotest file if autotesting -- */

			ux_write_logfile(inbuffer, "");
			inchar = inbuffer[0];
			if(inchar == 'S')
			{
/*
...get signon info
*/
				junk1 = strtok(inbuffer, " \n");
				junk2 = strtok(NULL, " \n");
				if (strcmp("DONE", junk2)==0)
					break;
				else if (strcmp("START", junk2)==0)
					continue;
				else if (strcmp("CAD", junk2)==0)
				{
					num = strtok(NULL, " \n");
					*cad = atoi(num);
					flag = 1;
				}
				else if (strcmp("CAM", junk2)==0)
				{
					num = strtok(NULL, " \n");
					*cam = atoi(num);
					flag = 1;
				}
				else if (strcmp("SELECTION", junk2)==0)
				{
					num = strtok(NULL, " \n");
					strcpy(filen, num);
					flag = 1;
				}
				else
					break;
			}
			else
			{
/*
.....it is not the signon record, push the record back and return
*/
				if (save_pos!=-1)
					fsetpos(fd, &save_pos) ;
				return flag;
			}
		}
		first = 0;
	}
	return flag;
}                                                               

/**************************************************************************
**
**  I_FUNCTION   ud_rdbrowser(filename)
**      read browser data from record and playback file
**
**  PARAMETERS   
**
**      INPUT  : 
**                                      filename
**      OUTPUT :  
**                                      filename:
**                                      status: 0: no pause occur, no need to display browser
**                                                      1: pause occured, display browser, need click
**                                                                      "OK" or "CANCEL" to contine
**												-1: fail to read browser, need display a new browser
**  RETURNS      :  status 
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_rdbrowser(filename)
char *filename;
{
	FILE *fd;                                                                                       /* os file descriptor */
	int status, status1;
	char *junk1, *junk2, *data;
#if UU_COMP!=UU_WIN2K
	char *junk, *menufunc, *menuname;
#endif
	char inchar, data1[UX_MAX_PATH_LEN];
	int accept_flag;
	fpos_t save_pos;
	char temp1[UX_MAX_PATH_LEN];
	char temp2[UX_MAX_PATH_LEN];
	static char inbuffer[UD_RPREC_LEN];                     /* input buffer */
	int first = 1;
	status = 0;
	data = data1;
	junk1 = temp1;
	junk2 = temp2;
/*
.....added accept flag to see if
.....user accept the browser or cancel the browser
*/
	accept_flag = 0;
	ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
	while(1)
	{
		save_pos = -1;
		if( fgetpos(fd, &save_pos ) != 0 )
			return -1;;
		status1 = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		if(status1 == UU_SUCCESS)
		{
/*                      -- write out to autotest file if autotesting -- */

			ux_write_logfile(inbuffer, "");
			inchar = inbuffer[0];
/*
...get Browser data
*/
			if(inchar == 'B')
			{
				junk1 = strtok(inbuffer, " \n");
				junk2 = strtok(NULL, " \n");
				if (strcmp("DONE", junk2)==0)
					break;
				else if (strcmp("DATA", junk2)==0)
				{
					accept_flag = 1;
					data = strtok(NULL, " \n");
					strcpy(filename, data);
				}
			}               
/*
...allow user use pause/resume functions when browser displaying
......only for platform other than WIN2K
......on WIN2K, we use file dialog (which is model dialog),
......there is no way other event will be acccept.
......but it can't be the first of browser statement
*/
#if UU_COMP!=UU_WIN2K
			else if ((inchar == 'M') && (first==0))   
			{
				junk = strtok(inbuffer, " \n");
				menufunc = strtok(NULL, " \n");
				menuname = strtok(NULL, "\n");
				menuname++;
				if (strcmp("\\pause", menufunc)==0)
					status = 1;
				if (strcmp("\\resume", menufunc)==0)
					status = 1;
			}               
#endif
			else
			{
/*
.....it is not the browser record, push the record back and return
*/
				if (save_pos!=-1)
					fsetpos(fd, &save_pos) ;
				return -1;
			}
			first = 0;
		}
/*
.....added for error 
.....Yurong 8/24/98
*/
		else
		{
			status = status1;
			break;
		}
	}       
/*
.....added for cancel 
.....Yurong 8/24/98
*/
	if (accept_flag==0)
		filename[0] = '\0';
	return status;
}               

/**************************************************************************
**
**  I_FUNCTION   ud_rdform(fStruct, fData, fstruct, fdata)
**      read form data from record and playback file
**
**  PARAMETERS   
**
**      INPUT  : 
**                                      fStruct, fData: original form data
**      OUTPUT :  
**                                      fstruct, fdata: modified form data
**                                      status: 0: for UNIX: no pause occur, no need to display form
**                                              1: for UNIX: pause occured, display form, need accept form to
**                                                                      contine
**                                                      -1: form canceled
**  RETURNS      :  status of read (UU_TRUE or UU_FALSE)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_rdform(fStruct, fData, fstruct, fdata)
UD_FSTRUCT *fStruct, *fstruct;
UD_FDATA *fData, *fdata;
{
	FILE *fd;                                                                                       /* os file descriptor */
	int status, status1, number;
	char *junk1, *junk2, *num, *data, *data2;
	char inchar, data1[40];
	int accept_flag;
	int list_num, k, list_started, len;
	int tlist_started, tlistc_started, dlist_started, dlistc_started, inum;
	fpos_t save_pos;
	UD_LIST list, *old_list;
	UD_TLIST tlist, *old_tlist;
	UD_DLIST dlist, *old_dlist;
	static char inbuffer[UD_RPREC_LEN];                     /* input buffer */
	int first = 1;
#if UU_COMP!=UU_WIN2K
	char *junk,*menufunc,*menuname;
#endif
	status = 0;
	data = data1;
	tlist_started = tlistc_started = 0;
/*
.....added accept flag to see if
.....user accept the form or cancel the form
*/
	accept_flag = 0;
	ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
	while(1)
	{
/*
.....save the position in order to reset
.....it could happened if the record no need to display the form but the second run
.....playback need to display the form, then it will try to read the form data which not in *.rd
.....file. (such as if we record, then restart the ncl without any changes made, no signoff form
.....display. But when we playback after we did something, then it will display signoff form.
.....I made the changes here to allow those case and also avoid the possible fatal error because 
.....of that. Yurong     11/14/05
*/
		save_pos = -1;
		if( fgetpos(fd, &save_pos ) != 0 )
			break;
		status1 = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		if(status1 == UU_SUCCESS)
		{
/*                      -- write out to autotest file if autotesting -- */

			ux_write_logfile(inbuffer, "");
			inchar = inbuffer[0];
/*
...get form data
*/
			if(inchar == 'F')
			{
				junk1 = strtok(inbuffer, " \n");
				junk2 = strtok(NULL, " \n");
				if (strcmp("DONE", junk2)==0)
				{
					junk2 = strtok(NULL, " \n");
					if (strcmp("1", junk2)==0)
					{
						fstruct = fStruct;
						fdata = fData;
						return 1;
					}
					break;
				}
				else if (strcmp("DATA", junk2)==0)
				{
					accept_flag = 1;
					num = strtok(NULL, " \n");
					data = strtok(NULL, " \n");
					if (data!=NULL)
						data2 = strtok(NULL, " \n");
					number = atoi(num);
/*
...check if data valiable and put data in form data
*/ 
					if (fStruct->input_flds[number].toggle==1)
					{
/*
.....add check for data
.....it allows data = NULL
.....yurong 7/22/97
*/
						if (data==NULL) data = " ";
/*
.....pass in the fstruct and fdata because we may have more than one set of
.....fstruct and fdata
*/
						ud_form_ckdata(fStruct, fData, number,data);
/*
						(*(ug_gksstli.wsopen[0].connid)[UW_FORM_CKDATA])(number,data);
*/
					}
					else if ((fStruct->input_flds[number].toggle==5) ||
								(fStruct->input_flds[number].toggle==7) ||
								(fStruct->input_flds[number].toggle==8) ||
								(fStruct->input_flds[number].toggle==14))
					{
						old_list = (UD_LIST *)(fData->ud_data[number].ud_delem.frmint);
						if (data!=NULL)
						{
							strcpy(old_list->answer, data);
						}
					}
					else if (fStruct->input_flds[number].toggle==17)
					{
						if (data!=NULL)
						{
							old_tlist = (UD_TLIST *)(fData->ud_data[number].ud_delem.frmint);
							old_tlist->answer = atoi(data);
						}
					}
					else if (fStruct->input_flds[number].toggle==19)
					{
						if (data!=NULL)
						{
							old_dlist = (UD_DLIST *)(fData->ud_data[number].ud_delem.frmint);
							old_dlist->answer[0] = atoi(data);
							if (data2!=NULL)
								old_dlist->answer[1] = atoi(data2);
						}
					}
					else
					{
						if ((data==NULL)
								&&(fData->ud_data[number].ud_delem.frmint!=NULL)) 
							fData->ud_data[number].ud_delem.frmint[0] = 0;
						else if (fData->ud_data[number].ud_delem.frmint!=NULL)
						{
							fData->ud_data[number].ud_delem.frmint[0] = atoi(data);
						}
					}
				}
				else if (strcmp("LIST", junk2)==0)
				{
					accept_flag = 1;
					num = strtok(NULL, " \n");
					data = strtok(NULL, "\n");
					if (strcmp(num, "START")==0)
					{
						number = atoi(data);
						list_started = 1;
						list_num = 0;
						list.num_item = number;
						list.item = (char **)uu_malloc(list.num_item*sizeof (char*));
					}
					else if (strcmp(num, "DONE")==0)
					{
/*
.....copy list to form list
*/
						number = atoi(data);
						old_list = (UD_LIST *)(fData->ud_data[number].ud_delem.frmint);
						len = strlen(old_list->answer);
						list.answer = (char *)uu_malloc((len+1)*sizeof (char));
						if (len>0)
							strcpy(list.answer, old_list->answer);
						else
							list.answer[0] = '\0';
						ud_free_flist(old_list);
						old_list->item = (char **)uu_malloc(list.num_item*sizeof (char*));
						for (k=0; k<list.num_item;k++)
						{
							len = strlen(list.item[k]);
							old_list->item[k] = (char *)uu_malloc((len+1)*sizeof (char));
							strcpy(old_list->item[k], list.item[k]);
						}
						len = strlen(list.answer);
						old_list->answer = (char *)uu_malloc((len+1)*sizeof (char));
						if (len>0)
							strcpy(old_list->answer, list.answer);
						else
							old_list->answer[0] = '\0';
						old_list->num_item = list.num_item;
						ud_free_flist(&list);
					}
					else
					{
						if (data!=NULL)
						{
							len = strlen(data);
							list.item[list_num] = (char *)uu_malloc((len+1)*sizeof (char));
							strcpy(list.item[list_num], data);
							list_num++;
						}
					}
				}
				else if (strcmp("DLIST", junk2)==0)
				{
					accept_flag = 1;
					num = strtok(NULL, " \n");
					data = strtok(NULL, "\n");
					if (strcmp(num, "START")==0)
					{
						number = atoi(data);
						dlist_started = 1;
						list_num = 0;
						dlist.num_item = number;
						dlist.data = (UD_ITEMDATA *)uu_malloc(dlist.num_item*sizeof (UD_ITEMDATA));
					}
					else if (strcmp(num, "STARTC")==0)
					{
						number = atoi(data);
						dlistc_started = 1;
						dlist.num_col = number;
						dlist.col_label = (char **)uu_malloc(dlist.num_col*sizeof (char*));
					}
					else if (strcmp(num, "DONE")==0)
					{
/*
.....copy list to form list
*/
						number = atoi(data);
						old_dlist = (UD_DLIST *)(fData->ud_data[number].ud_delem.frmint);
						dlist.answer[0] = old_dlist->answer[0];
						dlist.answer[1] = old_dlist->answer[1];
						ud_free_dlist(old_dlist);
						ud_dlist_copy(&dlist, old_dlist);
						ud_free_dlist(&dlist);
					}
					else
					{
						if (data!=NULL)
						{
							if (dlistc_started)
							{
								inum = atoi(num);
								len = strlen(data);
								dlist.col_label[inum] = (char *)uu_malloc((len+1)*sizeof(char));
								strcpy(dlist.col_label[inum], data);
							}
							else if (dlist_started)
							{
								len = strlen(data);
								inum = atoi(num);
								dlist.data[list_num].data_items[inum] = (char *)uu_malloc((len+1)*sizeof (char));
								strcpy(dlist.data[list_num].data_items[inum], data);
								if ((inum+1)%(dlist.data[list_num].itemnum)==0)
									list_num++;
							}
						}
					}
				}
				else if (strcmp("TLIST", junk2)==0)
				{
					accept_flag = 1;
					num = strtok(NULL, " \n");
					data = strtok(NULL, "\n");
					if (strcmp(num, "START")==0)
					{
						number = atoi(data);
						tlist_started = 1;
						list_num = 0;
						tlist.num_item = number;
						tlist.data = (UD_ITEMDATA *)uu_malloc(tlist.num_item*sizeof (UD_ITEMDATA));
					}
					else if (strcmp(num, "STARTC")==0)
					{
						number = atoi(data);
						tlistc_started = 1;
						tlist.num_col = number;
						tlist.col_label = (char **)uu_malloc(tlist.num_col*sizeof (char*));
					}
					else if (strcmp(num, "DONE")==0)
					{
/*
.....copy list to form list
*/
						number = atoi(data);
						old_tlist = (UD_TLIST *)(fData->ud_data[number].ud_delem.frmint);
						tlist.answer = old_tlist->answer;
						ud_free_tlist(old_tlist);
						ud_tlist_copy(&tlist, old_tlist);
						ud_free_tlist(&tlist);
					}
					else
					{
						if (data!=NULL)
						{
							if (tlistc_started)
							{
								inum = atoi(num);
								len = strlen(data);
								tlist.col_label[inum] = (char *)uu_malloc((len+1)*sizeof(char));
								strcpy(tlist.col_label[inum], data);
							}
							else if (tlist_started)
							{
								len = strlen(data);
								inum = atoi(num);
								tlist.data[list_num].data_items[inum] = (char *)uu_malloc((len+1)*sizeof (char));
								strcpy(tlist.data[list_num].data_items[inum], data);
								if ((inum+1)%(tlist.data[list_num].itemnum)==0)
									list_num++;
							}
						}
					}
				}
			}               
/*
...allow user use functions when form displaying
......only for platform other than WIN2K
......on WIN2K, we use model dialog for  form,
......there is no way other event will be acccept.
*/
#if UU_COMP!=UU_WIN2K
			else if ((inchar == 'M')  && (first==0)) 
			{
				junk = strtok(inbuffer, " \n");
				menufunc = strtok(NULL, " \n");
				menuname = strtok(NULL, "\n");
				menuname++;
				if (strcmp("\\pause", menufunc)==0)
					status = 1;
				if (strcmp("\\resume", menufunc)==0)
					status = 1;
			}               
#endif
			else
			{
/*
.....it is not the form record, push the record back and return
*/
				if (save_pos!=-1)
					fsetpos(fd, &save_pos) ;
/*
.....if form is not read at all, consider it is paused and ask to display the form
.....added this because in some case, when record, there is no
.....error in form and function well, so it work well, but when playback,
.....because it may have different setting, the form excutable not well
.....and will asked redisplay the form (such as IPV_LOAD_MACHINE/ul_ipv_mach_form)
.....but because there is no record in playback file, it will return 0 as accept now,
.....and get into a loop, so we change to return 1 to ask redisplay form). 
.....This is changed for avoid that case. The user suppose
.....to set the same setting for record/playback though, but we do not want they
.....get into a dead loop
*/
				if (first==1) return 1;
				return 0;
			}
			first = 0;
		}
/*
.....added for error 
.....Yurong 8/24/98
*/
		else
		{
			status = status1;
			break;
		}
	}       
/*
.....added for cancel form
.....Yurong 8/24/98
*/
	if (accept_flag==0)
#if UU_COMP!=UU_WIN2K
		ud_jump(-1,UU_FALSE);
#else
		return  -1;
#endif
	fstruct = fStruct;
	fdata = fData;
	return status;
}
/**************************************************************************
**
**  I_FUNCTION: ud_chk_rprd()
**      This function read from record and playback file
**                      if playback is on (this is only for convience
**                      on WIN2K because we need this function called from
**                      C++ file but don't want include too much *.h because
**                      we have to changed *.h to declare extern as "extern "C"
**              Don't remove this function until we changed more *.h file  
**
**  PARAMETERS   
**
**      INPUT  : 
**          none
**      OUTPUT :  
**          none
**
**  RETURNS      :  (1 or 0)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_chk_rprd()
{
	UD_GKSEVENT gks_event;
	int stat, jmpflag;

	UD_MARK(jmpflag,UU_TRUE);
	if (UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)
	{
		ud_rprd(&gks_event,UU_FALSE);
		stat =  1;
	}
	else
		stat = 0;
	UD_UNMARK(jmpflag);
	return stat;
}               

/**************************************************************************
**
**  I_FUNCTION: ud_is_playback()
**      This function check if playback is on (this is only for convience
**      on WIN2K because we need this function called from
**      C++ file but don't want include too much *.h because
**      we have to changed *.h to declare extern as "extern "C"
**      Don't remove this function until we changed more *.h file  
**
**  PARAMETERS   
**
**      INPUT  : 
**          none
**      OUTPUT :  
**          none
**
**  RETURNS      :  (1 or 0)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_is_playback()
{
	if (UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)
	{
		return 1;
	}
	else
		return 0;
}               

/**************************************************************************
**
**  I_FUNCTION         :  ud_rprd(event, flushflag)
**      read from record and playback file
**
**  PARAMETERS   
**
**      INPUT  : 
**          flushflag = if UU_TRUE, then flush the next event from the file
**      OUTPUT :  
**          event = event buffer
**
**  RETURNS      :  status of read (UU_TRUE or UU_FALSE)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

UU_LOGICAL ud_rprd(event, flushflag)
UD_GKSEVENT *event;                             /* event buffer */
UU_LOGICAL flushflag;                   /* if UU_TRUE then flush next event from file */
{
	static char inbuffer[UD_RPREC_LEN];                     /* input buffer */
	static Gwpoint stkbuf[UD_RPREC_LEN];            /* stroke input buffer */
	char inchar;
	char* junk, menufunc[40], funcparm[42], menuname[40];
	int status;                                                                                     /* local status register */
	int i, j,inc;
	int equ;
	Gqpicks pick;   /* pick record */
	Gfloat valx, valy, valz;
#if UU_COMP==UU_IRIS 
#ifdef UU_DOUBLE
	long float tempx, tempy, tempz;                                                                /* temporary reals */
#else
	float tempx, tempy, tempz;                                                             /* temporary reals */
#endif 
#endif 
#if UU_COMP != UU_IRIS
	float tempx, tempy, tempz;                                                             /* temporary reals */
#endif
	FILE *fd;                                                                                       /* os file descriptor */

	uu_denter(UU_DTRC,(us, "entering ud_rprd, event=%x", event));

	uu_dprint(UU_DTRC,(us,"RPSTATE:ptr=%d, flag=%d, lun=%d",
			UD_Rpstate_ptr,
			UD_Rpstate[UD_Rpstate_ptr].flag,
			UD_Rpstate[UD_Rpstate_ptr].rplun));

	if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{

/*              -- read the next event -- */

restart:

		status = ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd,
			UX_PRTERRS);
		if (status == UU_SUCCESS)
			status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
		uu_dprint(UU_DTRC,(us, "ud_rprd, fgets0 status = %d", status));

		if (strncmp(inbuffer, "MENU view_mgmt.menu Manage", 26)==0)
			status = UU_SUCCESS;
		if(status == UU_SUCCESS)
		{
			uu_dprint(UU_DTRC,(us, "read x_rdt = %s", inbuffer));

/*                      -- write out to autotest file if autotesting -- */

			ux_write_logfile(inbuffer, "");

			inchar = inbuffer[0];

/*                      -- dummy event to a none type if flushing -- */

			if(flushflag == UU_TRUE)
				inchar = 'N';

/*                      -- comment -- */
		
			if(inchar == '/')
			{
				goto restart;
			}

/*                      -- none -- */

			else if(inchar == 'N')
			{
				(*event).evclass = UD_NONE;
			}
			
/*                      -- locator -- */

			else if(inchar == 'L')
			{
				(*event).evclass = UD_LOCATOR;
				if (NCL_rpfile_version>9.162)
				{
					sscanf(inbuffer, "LOCATOR device = %d trans = %d loc  = (%e %e %e), choice = %d",
						&(*event).evdev,
						&(*event).indata.locdata.transform, 
						&tempx, &tempy, &tempz,
						&(*event).indata.locdata.choice);

					valx = tempx;
					valy = tempy;
					valz = tempz;
					ud_convert_playback_pos(&valx,&valy, &valz,
								(*event).indata.locdata.transform);

					(*event).indata.locdata.position.x = valx;      
					(*event).indata.locdata.position.y = valy;
				}
				else
				{
					sscanf(inbuffer, "LOCATOR device = %d trans = %d loc  = (%e %e), choice = %d",
						&(*event).evdev,
						&(*event).indata.locdata.transform, 
						&tempx, &tempy,
						&(*event).indata.locdata.choice);

/*                                      -- recast for double precision -- */

					(*event).indata.locdata.position.x = tempx;
					(*event).indata.locdata.position.y = tempy;
				}
			}
			
/*              -- stroke -- */
		
			else if(inchar=='S' && inbuffer[3]=='O')
			{
				(*event).evclass = UD_STROKE;

				sscanf(inbuffer, "STROKE device = %d, trigger = %d, count = %d\n", 
					&(*event).evdev, &(*event).indata.strokedata.stktrig, 
					&(*event).indata.strokedata.n_points);

/*                              -- now recover all the stroke points and pass them to
					the user supplied routine -- */

				(*event).indata.strokedata.points = stkbuf;
				for(i=0; i<(*event).indata.strokedata.n_points; i++)
				{
					status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);

					uu_dprint(UU_DTRC,(us, "read x_rdt = %s", inbuffer));

					sscanf(inbuffer, "      point %d = [%e, %e]", &j, &tempx, &tempy);

/*                                      -- recast for double precision -- */

					stkbuf[i].x = tempx;
					stkbuf[i].y = tempy;

					if(UD_stkinit.funct != NULL)
						(*UD_stkinit.funct)(&stkbuf[i]);
				}
			}
			
/*              -- valuator -- */

			else if(inchar ==  'V')
			{
				(*event).evclass = UD_VALUATOR;

				sscanf(inbuffer, "VALUATOR device = %d value = %e",
					&(*event).evdev, &tempx);

/*                                      -- recast for double precision -- */

				(*event).indata.valdata = tempx;
			}
			
/* -- menu -- */
/*
...added Yurong
*/
			else if(inchar == 'M')
			{
				(*event).evclass = UD_PICKMENU;
				junk = strtok(inbuffer, " ");
				junk = strtok(NULL, " \n");
				if (junk!=NULL)
					strcpy(menufunc, junk);
				junk = strtok(NULL, " \n");
				if (junk!=NULL)
					strcpy(funcparm, junk);
				if (funcparm[0]=='\"')
				{
					junk = strtok(NULL, "\n");
					if (junk!=NULL)
						strcpy(menuname, junk);
					ul_remove_quotes(funcparm);
				}
				else
				{
					strcpy(menuname, funcparm);
					funcparm[0] = '\0';
				}
				inc = UD_Rpstate_ptr;
				ud_menuplay(menufunc, funcparm, menuname);
				if (UD_Rpstate[inc].flag == RPOFF) status = 1;
			}               
/*
...added for Form terminal
... Yurong
*/
			else if(inchar == 'F') status = 1;
/*
...added for Form terminal
... Yurong
*/
			else if(inchar == 'B') status = 1;
/*              -- choice --*/

			else if(inchar ==  'C')
			{
				(*event).evclass = UD_CHOICE;

				sscanf(inbuffer, "CHOICE device = %d choice = %d",
					&(*event).evdev,
					&(*event).indata.choicedata);
			}
			
/* -- pick -- */

			else if(inchar ==  'P')
			{
				(*event).evclass = UD_PICK;
				if (NCL_rpfile_version>9.162)
				{
					sscanf(inbuffer, "PICK device = %d trans = %d loc  = (%e %e %e), choice = %d", 
						&(*event).evdev,
						&(*event).indata.pickdata.transform, 
						&tempx, &tempy, &tempz,
						&(*event).indata.pickdata.choice);
					valx = tempx;
					valy = tempy;
					valz = tempz;
					ud_convert_playback_pos(&valx, 
									&valy, &valz,
									(*event).indata.pickdata.transform);
					(*event).indata.pickdata.position.x = valx;
					(*event).indata.pickdata.position.y = valy;
				}
				else
				{
					sscanf(inbuffer, "PICK device = %d trans = %d loc  = (%e %e), choice = %d", 
						&(*event).evdev,
						&(*event).indata.pickdata.transform, 
						&tempx, &tempy,
						&(*event).indata.pickdata.choice);

/*                              -- recast for double precision -- */

					(*event).indata.pickdata.position.x = tempx;
					(*event).indata.pickdata.position.y = tempy;
				}
/*                              -- go get the new pick id -- */

				gpckm((*event).indata.pickdata.position.x,
						 (*event).indata.pickdata.position.y,
						 &pick, (UU_REAL) .01, (*event).indata.pickdata.transform); 

				(*event).indata.pickdata.status = pick.status;
				(*event).indata.pickdata.depth = pick.depth;
				(*event).indata.pickdata.pickpath = pick.pickpath;
			}
/*
... added for SINGON
... Yurong 2/12/97
*/
			else if((inchar == 'S')&&(inbuffer[1]=='I'))                    
			{
/*
				uw_mfsignon_play();
*/
				ud_signon_play(inbuffer);
			}
/*                      -- string -- */

			else if(inchar ==  'S')
			{
				(*event).evclass = UD_STRING;

				sscanf(inbuffer, "STRING device = %d", &(*event).evdev);

/*                              -- squeeze out "\n" if it is at end of the string -- */

				j = strlen(inbuffer);
				if(inbuffer[j-1] == '\n')
				{
					inbuffer[j-1] = '\0';
					j--;
				}
				if (inbuffer[j-1] == '\n' || inbuffer[j-1] == '\r')
				{
					inbuffer[j-1] = '\0';
					j--;
				}

/*                              -- scan for second "=" sign -- */

				equ = 0;
				for(i=0; i<j && equ < 2; i++)
				{
					if(inbuffer[i] == '=')
						equ++;
				}

				(*event).indata.stringdata = &inbuffer[i+1];
			}

/*                      -- prompt -- */

			else if(inchar ==  'P')
			{
				(*event).evclass = UD_PROMPT;
			}
			else
			{

/*---                   "invalid rprd event=%s		---*/

				uu_uerror1(UD_DASHEP, 28, inbuffer);
			}
		}
		else
		{
			ux_close(UD_Rpstate[UD_Rpstate_ptr].rplun, UX_PRTERRS);
			if(UD_Rpstate_ptr >= 0)
				UD_Rpstate[UD_Rpstate_ptr].flag = RPOFF;

			if(UD_Rpstate_ptr > 0)
				UD_Rpstate_ptr--;

			status = 1;
		}
	}
	else
	{
		status = 1;
	}

#if UU_DEBUG==UU_TRUE
	if(UU_debmask & UU_DTRC)
		ud_prevent("ud_rprd", event);
#endif
	uu_dexit;
	if(status == 0)
		return(UU_TRUE);
	else
		return(UU_FALSE);
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_rpwr(event)
**      write to record and playback file
**
**  PARAMETERS   
**
**      INPUT  : 
**          event = event buffer
**      OUTPUT :  
**          none
**
**  RETURNS      :  status of write (UU_TRUE or UU_FALSE)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_rpwr(event)
UD_GKSEVENT *event;                                                             /* event buffer */
{
	char inbuffer[200];                                     /* local character buffer */
	int status;
	int i;
	Gfloat tempx,tempy,tempz;
	FILE *fd;                                                               /* os file descriptor */

	uu_denter(UU_DTRC,(us, "entering ud_rpwr, event=%x", event));

	uu_dprint(UU_DTRC,(us,"RPSTATE:ptr=%d, flag=%d, lun=%d",
				UD_Rpstate_ptr,
				UD_Rpstate[UD_Rpstate_ptr].flag,
				UD_Rpstate[UD_Rpstate_ptr].rplun));

	switch ((*event).evclass)
	{
		case UD_NONE:

			sprintf(inbuffer, "NONE\n");
			break;

		case UD_LOCATOR:
			ug_ndcw3((*event).indata.locdata.transform, &tempx,&tempy,&tempz,(*event).indata.locdata.position.x,
						(*event).indata.locdata.position.y,
						0.0); 
			sprintf(inbuffer, "LOCATOR device = %d trans = %d loc  = (%e %e %e), choice = %d\n",
				(*event).evdev,
				(*event).indata.locdata.transform, 
				tempx,
				tempy,
				tempz,
				(*event).indata.locdata.choice);
	
			break;
	
		case UD_STROKE:
	
			sprintf(inbuffer, "STROKE device = %d, trigger = %d, count = %d\n", 
				(*event).evdev, (*event).indata.strokedata.stktrig, 
				(*event).indata.strokedata.n_points);
			ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
			status = ux_fputs0(inbuffer, fd);

/*                      -- now record all the stroke points -- */

			for(i=0; i<(*event).indata.strokedata.n_points; i++)
			{
/*                              ug_ndcw3((*event).indata.strokedata.transform, 
					&tempx,&tempy,&tempz,(*event).indata.strokedata.points[i].x,
						(*event).indata.strokedata.points[i].y,
						0.0); 
*/
				sprintf(inbuffer, "      point %d = [%e, %e]\n", 
						i, (*event).indata.strokedata.points[i].x, 
						(*event).indata.strokedata.points[i].y);

/*                              -- write out all except the last point (so write statement
					at bottom of this procedure can write it out) -- */
				if(i < (*event).indata.strokedata.n_points-1) {
					ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd,
									UX_PRTERRS);
					status = ux_fputs0(inbuffer, fd);
				}
			}
			break;
	
		case UD_VALUATOR:

			sprintf(inbuffer, "VALUATOR device = %d value = %e\n",
				(*event).evdev,
				(*event).indata.valdata);
	
			break;
	
		case UD_CHOICE:

			sprintf(inbuffer, "CHOICE device = %d choice = %d\n",
				(*event).evdev,
				(*event).indata.choicedata);
	
			break;
	
		case UD_PICK:
			ug_ndcw3((*event).indata.pickdata.transform, &tempx,&tempy,&tempz,(*event).indata.pickdata.position.x,
						(*event).indata.pickdata.position.y,
						0.0); 

			sprintf(inbuffer, "PICK device = %d trans = %d loc  = (%e %e %e), choice = %d\n", 
				(*event).evdev,
				(*event).indata.pickdata.transform, 
				tempx,
				tempy,
				tempz,
				(*event).indata.pickdata.choice);
				
				break;
	
		case UD_STRING:

			sprintf(inbuffer, "STRING device = %d string = %s\n",
				(*event).evdev,
				(*event).indata.stringdata);

			break;

		default:

/*---           "invalid event being recorded"          ---*/

			uu_uerror0(UD_DASHEP, 29);
			strcpy(inbuffer, "invalid event being recorded");
			break;
	}

/*      -- write out the message -- */

	ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
	status = ux_fputs0(inbuffer, fd);
	uu_dprint(UU_DTRC,(us,"leave ud_rpwr, write status=%d", status));
	uu_dexit;
	return(status);
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_playinit()
**      play back start up record file
**
**  PARAMETERS   
**
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

void ud_playinit()
{
	int status;                                             /* file open status return */

	uu_denter(UU_DTRC,(us, " entering playinit"));

/*      -- open file for read access -- */

	status = ux_open_to_data("INITPLAY", "r", "STREAM", "ASCII",
						&UD_Rpstate[UD_Rpstate_ptr+1].rplun, UX_PRTERRS);

	uu_dprint(UU_DTRC,(us,"in ud_playback, open status=%d", status));

	if(status == 0)
	{
		UD_Rpstate_ptr++;
		UD_Rpstate[UD_Rpstate_ptr].flag = PLAYBACK;
		NCL_rpfile_version = NCL_infile_version;
	}

	uu_dexit;
	return;
}
/**************************************************************************
**
**  I_FUNCTION         :  ud_signinit()
**      play back start up record file in the signon form
**
**  PARAMETERS   
**
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
void ud_signinit(fullname)
char *fullname;
{
	int status;                                             /* file open status return */

/*      -- open file for read access -- */

	status = ux_open_to_data(fullname, "r", "STREAM", "ASCII",
						&UD_Rpstate[UD_Rpstate_ptr+1].rplun, UX_PRTERRS);

	uu_dprint(UU_DTRC,(us,"in ud_playback, open status=%d", status));

	if(status == 0)
	{
		UD_Rpstate_ptr++;
		UD_Rpstate[UD_Rpstate_ptr].flag = PLAYBACK;
		NCL_rpfile_version = NCL_infile_version;
	}

	uu_dexit;
	return;
}
/*********************************************************************
**
**    I_FUNCTION :  ud_delay(command)
**       delay n seconds when playback encounters a "\delay,n"
**
**    PARAMETERS   
**
**       INPUT  : 
**          command = command line played back
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_delay(command)
char *command;                                                  /* command string used to invoke */
{
	int delay;

	if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{

/*              -- flush all buffered graphics -- */

		gupdatews(UD_ksws, UG_SUPPRESS);

/*      -- compute number of seconds to delay -- */

		delay = 0;
		sscanf(command, "\\delay,%d", &delay);

		if(delay > 0)
			uu_delay(1000*delay);
	}
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_rpwrcom(msg)
**      write a comment to record and playback file
**
**  PARAMETERS   
**
**      INPUT  : 
**          msg = comment to write
**      OUTPUT :  
**          none
**
**  RETURNS      :  status of write (UU_TRUE or UU_FALSE)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_rpwrcom(msg)
char *msg;                                                      /* comment to write */
{
	char buffer[200];                               /* comment buffer */
	int status;                                             /* status cell */
	FILE *fd;                                               /* os dependent file descriptor */

	uu_denter(UU_DTRC,(us, "entering ud_rpwrcom"));
	status = 0;
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD && UD_autotest==UU_FALSE)
	{
		sprintf(buffer, "/*****  %s  *****/\n", msg);

/*              -- write out the message -- */

		ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
		status = ux_fputs0(buffer, fd);
	}
	uu_dexit;
	return(status);
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_rpwrmenu(key, parms, msg)
**      write a menu to record and playback file
**
**  PARAMETERS   
**
**      INPUT  : 
**          msg: menu label
**                      parms: function parameter
**                      key: key function name (or menu name) 
**      OUTPUT :  
**          none
**
**  RETURNS      :  status of write (UU_TRUE or UU_FALSE)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_rpwrmenu(key,parms, msg)
char *msg;                      /* menu to write */ 
char *key, *parms;              /* key to write */
{
	char buffer[200];          /* menu line buffer */
	int status;                /* status cell */
	FILE *fd;                  /* os dependent file descriptor */

	uu_denter(UU_DTRC,(us, "entering ud_rpwrmenu"));
	status = 0;
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD && UD_autotest==UU_FALSE)
	{
		if ((parms!=NULL)&&(parms[0]!='\0'))
			sprintf(buffer, "MENU %s \"%s\" %s\n", key, parms, msg);
		else
			sprintf(buffer, "MENU %s %s\n", key, msg);

/* 
...write out the menu line 
*/

		ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
		status = ux_fputs0(buffer, fd);
	}
	uu_dexit;
	return(status);
}
/**************************************************************************
**
**  I_FUNCTION         :  ud_wrsignon(choice, data)
**      write a signon choice to record and playback file
**
**  PARAMETERS   
**
**      INPUT  : 
**                      choice, data 
**      OUTPUT :  
**          none
**
**  RETURNS      :  status of write (UU_TRUE or UU_FALSE)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_wrsignon(choice, data)
char *choice;              /* choice to write */
char *data;              /* data to write */
{
   char buffer[200];          /* menu line buffer */
   int status;                /* status cell */
   FILE *fd;                  /* os dependent file descriptor */

   status = 0;
   if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD && UD_autotest==UU_FALSE)
   {
		if (data == UU_NULL)
	   sprintf(buffer, "SIGNON  %s\n", choice);
		else
	   sprintf(buffer, "SIGNON  %s  %s\n", choice, data);
/* 
...write out the sign info line 
*/
      ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
      status = ux_fputs0(buffer, fd);
   }
   uu_dexit;
   return(status);
}
/**************************************************************************
**
**  I_FUNCTION         :  ud_rpwrform(key, msg1,msg2)
**      write a menu to record and playback file
**
**  PARAMETERS   
**
**      INPUT  : 
**          msg1, msg2, key 
**      OUTPUT :  
**          none
**
**  RETURNS      :  status of write (UU_TRUE or UU_FALSE)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_rpwrform(key, msg1, msg2)
char *msg1, *msg2;                      
char *key;
{
   char buffer[200];          /* menu line buffer */
   int status;                /* status cell */
   FILE *fd;                  /* os dependent file descriptor */

   uu_denter(UU_DTRC,(us, "entering ud_rpwrmenu"));
   status = 0;
   if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD && UD_autotest==UU_FALSE)
   {
/* 
...write out the form data line 
*/
		if (msg2 == UU_NULL)
			if (msg1 == UU_NULL)
				sprintf(buffer, "FORM %s\n", key);
			else
				sprintf(buffer, "FORM %s %s\n", key, msg1);
		else
			sprintf(buffer, "FORM %s %s %s\n", key, msg1, msg2);
      ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
      status = ux_fputs0(buffer, fd);
   }
   uu_dexit;
   return(status);
}
/**************************************************************************
**
**  I_FUNCTION         :  ud_decrp(line, event)
**      decode record and playback record
**
**  PARAMETERS   
**
**      INPUT  : 
**                              line = text line to decode
**
**      OUTPUT :  
**          event = event buffer
**
**  RETURNS      :  status of decode (UU_SUCCESS if valid event or UU_FAILURE
**                                                              for invalid event or comment)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

int ud_decrp(line, event)
char *line;                                                     /* input R/P record */
UD_GKSEVENT *event;                             /* event buffer */
{
	char inchar;
	int i, j, len;
	char* junk, menufunc[40], menuname[40], funcparm[42];
	char buf[80];
	int equ;
	Gqpicks pick;                                                                           /* pick record */
	float tempx, tempy, tempz;     /* temporary reals */
	Gfloat valx, valy, valz;

	uu_denter(UU_DTRC,(us, "entering ud_decrp, string=%s", line));

	inchar = line[0];

/*      -- comment -- */
	
	if(inchar == '/')
	{
		goto failure;
	}

/*              -- none -- */

	else if(inchar == 'N')
	{
		(*event).evclass = UD_NONE;
	}
	
/*              -- locator -- */

	else if(inchar == 'L')
	{
		(*event).evclass = UD_LOCATOR;
		if (NCL_rpfile_version>9.162)
		{
			sscanf(line, "LOCATOR device = %d trans = %d loc  = (%e %e %e), choice = %d",
				&(*event).evdev,
				&(*event).indata.locdata.transform, 
				&tempx, &tempy, &tempz,
				&(*event).indata.locdata.choice);               
/*                      -- recast for double precision -- */
			valx = tempx;
			valy = tempy;
			valz = tempz;
			ud_convert_playback_pos(&valx, &valy, &valz,
							(*event).indata.locdata.transform);
			(*event).indata.locdata.position.x = valx;
			(*event).indata.locdata.position.y = valy;
		}
		else
		{
			sscanf(line, "LOCATOR device = %d trans = %d loc  = (%e %e), choice = %d",
				&(*event).evdev,
				&(*event).indata.locdata.transform, 
				&tempx, &tempy,
				&(*event).indata.locdata.choice);

/*                      -- recast for double precision -- */

				(*event).indata.locdata.position.x = tempx;
				(*event).indata.locdata.position.y = tempy;
		}
	}
	
/*      -- valuator -- */

	else if(inchar ==  'V')
	{
		(*event).evclass = UD_VALUATOR;

		sscanf(line, "VALUATOR device = %d value = %e",
			&(*event).evdev, &tempx);

/*                              -- recast for double precision -- */

		(*event).indata.valdata = tempx;
	}
/* -- menu -- */
	else if(inchar == 'M')
	{
/*
		junk = strtok(line, " ");
		junk = strtok(NULL, " \n");
		if (junk!=NULL)
			strcpy(menufunc, junk);
		junk = strtok(NULL, " \n");
		if (junk!=NULL)
			strcpy(funcparm, junk);
		if (funcparm[0]=='\"')
		{
			junk = strtok(NULL, "\n");
			if (junk!=NULL)
				strcpy(menuname, junk);
			ul_remove_quotes(funcparm);
		}
		else
		{
			strcpy(menuname, funcparm);
			funcparm[0] = '\0';
		}
**/
		junk = (char *)index(line,' ');
		if (junk!=0)
		{
			*junk = '\0';
			strcpy(menufunc, line);
			++junk;
			while ((*junk==' ') || (*junk=='\t')) junk++;
			strcpy(buf, junk);
			if (buf[0]=='\"')	
			{
				++junk;
				strcpy(buf, junk);
				junk = (char *)index(buf,'\"');
				*junk = '\0';
				strncpy(funcparm, buf,39);
				funcparm[39] = '\0';
				++junk;
				while ((*junk==' ') || (*junk=='\t')) junk++;
				strcpy(menuname, junk);
			}
			else
				strcpy(menuname, buf);
		}
		len = strlen (menuname);
		if (menuname[len-1]=='\n')
			menuname[len-1] = '\0';
		ud_menuplay(menufunc, funcparm, menuname);
	}               
/*      -- choice --*/

	else if(inchar ==  'C')
	{
		(*event).evclass = UD_CHOICE;

		sscanf(line, "CHOICE device = %d choice = %d",
			&(*event).evdev,
			&(*event).indata.choicedata);
	}
	
/* -- pick -- */

	else if(inchar ==  'P')
	{
		(*event).evclass = UD_PICK;
		if (NCL_rpfile_version>9.162)
		{
			sscanf(line, "PICK device = %d trans = %d loc  = (%e %e %e), choice = %d", 
				&(*event).evdev,
				&(*event).indata.pickdata.transform, 
				&tempx, &tempy, &tempz,
				&(*event).indata.pickdata.choice);

/*                      -- recast for double precision -- */
			valx = tempx;
			valy = tempy;
			valz = tempz;
			ud_convert_playback_pos(&valx, 
							&valy, &valz,
							(*event).indata.pickdata.transform);
			(*event).indata.pickdata.position.x = valx;
			(*event).indata.pickdata.position.y = valy;
		}
		else
		{
			sscanf(line, "PICK device = %d trans = %d loc  = (%e %e), choice = %d", 
				&(*event).evdev,
				&(*event).indata.pickdata.transform, 
				&tempx, &tempy,
				&(*event).indata.pickdata.choice);

/*                      -- recast for double precision -- */

			(*event).indata.pickdata.position.x = tempx;
			(*event).indata.pickdata.position.y = tempy;
		}

/*                      -- go get the new pick id -- */

		gpckm((*event).indata.pickdata.position.x,
				 (*event).indata.pickdata.position.y,
				 &pick, (UU_REAL) .01, (*event).indata.pickdata.transform); 

		(*event).indata.pickdata.status = pick.status;
		(*event).indata.pickdata.depth = pick.depth;
		(*event).indata.pickdata.pickpath = pick.pickpath;
	}
	
/*              -- string -- */

	else if(inchar ==  'S')
	{
		(*event).evclass = UD_STRING;

		sscanf(line, "STRING device = %d", &(*event).evdev);

/*                      -- squeeze out "\n" if it is at end of the string -- */

		j = strlen(line);
		if(line[j-1] == '\n')
		{
			line[j-1] = '\0';
			j--;
		}

/*                      -- scan for second "=" sign -- */

		equ = 0;
		for(i=0; i<j && equ < 2; i++)
		{
			if(line[i] == '=')
				equ++;
		}

		(*event).indata.stringdata = &line[i+1];
	}

/*              -- prompt -- */

	else if(inchar ==  'P')
	{
		(*event).evclass = UD_PROMPT;
	}
	else
	{

/*---           "invalid rprd event=%s		---*/

		uu_uerror1(UD_DASHEP, 28, line);
		goto failure;
	}

#if UU_DEBUG==UU_TRUE
	if(UU_debmask & UU_DTRC)
		ud_prevent("ud_decrp", event);
#endif
	
	uu_dexit;
	return(UU_TRUE);

failure:;
	uu_dexit;
	return(UU_FALSE);
}

/*********************************************************************
**
**    E_FUNCTION :  ud_inevt(line)
**       define an event to input to the DAS
**
**    PARAMETERS   
**
**       INPUT  : 
**          line = text line in R/P record format to buffer up
**       OUTPUT :  
**          none
**
**    RETURNS      : UU_TRUE if room left on Q, UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

UU_LOGICAL ud_inevt(line)
char *line;                                                     /* text line to Q up */
{
	int status;                                             /* status cell */

	uu_denter(UU_DTRC,(us, "enter ud_inevt, line=%s", line));
	if(UD_prc_buf_ptr <  UD_NUMPROC)
	{
		strcpy(UD_prc_buf[UD_prc_buf_ptr], line); 
		UD_prc_buf_ptr++;
		status = UU_TRUE;
	}
	else
		status = UU_TRUE;

	uu_dprint(UU_DTRC,(us, "leave ud_inevt, status=%d", status));
	uu_dexit;
	return(status);
}

/*********************************************************************
**
**    E_FUNCTION :  ud_mult_inevt(count, line)
**       define an event to input to the DAS
**
**    PARAMETERS   
**
**       INPUT  : 
**                              count = number of lines to buffer
**          line = text lines in R/P record format to buffer up
**       OUTPUT :  
**          none
**
**    RETURNS      : UU_TRUE if room left on Q, UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

UU_LOGICAL ud_mult_inevt(count, line)
int count;                                                              /* number of lines */
char *line[];                                                   /* text lines to Q up */
{
	int i;
	UU_LOGICAL      status, ud_inevt();

	uu_denter(UU_DTRC,(us, "enter ud_multi_inevt, count=%d", count));

	if(count > 0 && count < UD_NUMPROC)
	{
		for(i=0; i<count; i++)
		{
			status = ud_inevt(line[i]);
			if(status != UU_TRUE)
				break;
		}
	}
	else
	{
		status = UU_FALSE;
		uu_dprint(UU_DTRC,(us, "leave ud_multi_inevt count field bad=%d",
						count));
	}

	uu_dprint(UU_DTRC,(us, "leave ud_multi_inevt, status=%d", status));
	uu_dexit;
	return(status);
}

/*********************************************************************
**
**    I_FUNCTION :  UU_LOGICAL ud_outevt(line)
**       read an event from the procedure Q to input to the DAS
**
**    PARAMETERS   
**
**       INPUT  : 
**          event = event buffer
**       OUTPUT :  
**          none
**
**    RETURNS      : UU_TRUE if successful, UU_FALSE if not
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

UU_LOGICAL ud_outevt(event)
UD_DEVENT *event;                                                       /* event buffer */
{
	int status;                                                             /* status cell */

	uu_denter(UU_DTRC,(us, "enter ud_outevt, ptr=%d", UD_prc_buf_ptr));

	if(UD_prc_buf_ptr > 0)
	{
		UD_prc_buf_ptr--;
		status = ud_decrp(UD_prc_buf[UD_prc_buf_ptr], (UD_GKSEVENT *)event);
	}
	else
	{
		status = UU_FALSE;
		uu_dprint(UU_DTRC,(us,"ERROR ud_outevt bad buf_ptr=%d", UD_prc_buf_ptr));
		UD_prc_buf_ptr = 0;
	}

	uu_dprint(UU_DTRC,(us,"leave ud_outevt, status=%d, ptr=%d", 
					status, UD_prc_buf_ptr));
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  ud_record_auto(label)
**       auto test record routine to record menu leaf label
**
**    PARAMETERS   
**       INPUT  : 
**          label
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_record_auto(header, label)
char *header;                           /* menu haeder label */
char *label;                            /* menu leaf label */
{
	UD_DEVENT event;                /* input pipe event buffer */
	char buffer[100];               /* command buffer */

/*      -- set up input pipe buffer -- */
	event.evclass = UD_STRING;
	event.evdev = 9;
	strcpy(buffer, "\\at,");
	strcat(buffer, header);
	strcat(buffer, " ");
	strcat(buffer, label);
	event.indata.stringdata = buffer;

/*      -- write out the autotest label as a string event -- */

	ud_rpwr((UD_GKSEVENT *)&event);
}
/**************************************************************************
**
**  I_FUNCTION         :  ud_rpwrbrowser(key, msg)
**      write a browse result to record and playback file
**
**  PARAMETERS   
**
**      INPUT  : 
**          key, msg 
**      OUTPUT :  
**          none
**
**  RETURNS      :  status of write (UU_TRUE or UU_FALSE)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_rpwrbrowser(key, msg)
char *key, *msg;
{
   char buffer[200];          /* browse line buffer */
   int status;                /* status cell */
   FILE *fd;                  /* os dependent file descriptor */

   uu_denter(UU_DTRC,(us, "entering ud_rpwrmenu"));
   status = 0;
   if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD && UD_autotest==UU_FALSE)
   {
      sprintf(buffer, "BROWSER  %s %s\n", key, msg);
/* 
...write out the browse line 
*/

      ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
      status = ux_fputs0(buffer, fd);
   }
   uu_dexit;
   return(status);
}

ud_brower_rprd(filename, nc)
char *filename;
int *nc;
{
	int status = -1;
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
		ud_rpwrbrowser("BEGIN", NULL);

	if(UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)
	{
		status = ud_rdbrowser(filename);
		if (status==0)
		{
			if (filename[0] == '\0')
				*nc = 0;
			else
				*nc = strlen(filename);
		}
	}
	return status;
}


void ud_brower_endrprd(filename, nc)
char *filename;
int nc;
{
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
	{
		if ((filename!=NULL)&&(nc!=0))
			ud_rpwrbrowser("DATA", filename);
		ud_rpwrbrowser("DONE", NULL);
	}
}

void ud_convert_playback_pos(posx, posy, posz, xform)
Gfloat *posx, *posy, *posz;
int xform;
{
	Gfloat npos[3];
/*
.....before version 9.162, we record NDC position
.....now, we record WC position
*/
	if (NCL_rpfile_version<=9.162)
		return;
	ug_xform(*posx,*posy,*posz,npos,ug_cxform[xform]); 
	*posx = npos[0];
	*posy = npos[1];
	*posz = npos[2];
	return;
}
/**************************************************************************
**
**  I_FUNCTION         :  ud_rprd_ipvloc(event, button, x,y)
**      read a ipv location from record and playback file
**
**  PARAMETERS   
**
**      INPUT  : 
**          event: event return
**			button: button number
**			x,y: location
**      OUTPUT :  
**          none
**
**  RETURNS      :  status of read
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_rprd_ipvloc(event, button, x,y)
int *event, *button, *x, *y;                                                      /* comment to write */
{
	char inbuffer[500];                               /* comment buffer */
	int status;                                             /* status cell */
	FILE *fd;                                               /* os dependent file descriptor */

	ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
	status = ux_fgets0(inbuffer, UD_RPREC_LEN, fd);
	if(status == UU_SUCCESS)
	{
		sscanf(inbuffer, "IPVLOCATOR %d %d %d %d\n",
						event, button, x, y);
	}
	return(status);
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_rprd_ipvloc(event, button, x,y)
**      write a ipv location into record and playback file
**
**  PARAMETERS   
**
**      INPUT  : 
**          event: event return
**			button: button number
**			x,y: location
**      OUTPUT :  
**          none
**
**  RETURNS      :  status of read
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_rpwr_ipvloc(event, button, x,y)
int event, button, x,y;                                                      /* comment to write */
{
   char buffer[200];          /* browse line buffer */
   int status;                /* status cell */
   FILE *fd;                  /* os dependent file descriptor */

   status = 0;
   if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
   {
      sprintf(buffer, "IPVLOCATOR %d %d %d %d\n", event, button, x, y);
/* 
...write out the browse line 
*/

      ux_get_os_filedesc(UD_Rpstate[UD_Rpstate_ptr].rplun, &fd, UX_PRTERRS);
      status = ux_fputs0(buffer, fd);
   }
   return(status);
}

