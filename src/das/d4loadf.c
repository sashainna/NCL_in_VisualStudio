/*************************************************************************
**    NAME    : d4loadf.c
**    Contains:
**              ud_set_formdir
**              ud_loadfrm      -- load a form from a file.
**              ud_ldfrm        -- load form.
**              ud_free_flist(formlist)  -- Free formlist memory
**              ud_save_actform_id
**              ud_reset_actform_id
**              ud_read_posform
**              ud_save_frmpos
**              ud_free_dlist
**              ud_dlist_sort
**              ud_dlist_copy
**              ud_free_itemdata
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d4loadf.c , 26.4
**    DATE AND TIME OF LAST  MODIFICATION
**       05/22/18 , 10:15:06
**************************************************************************/

#include "ustdio.h"
#include "usysg.h"
#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "udfmracs.h"
#include "ulist.h"
#include "driver.h"
#include "dselmask.h"
#include "gtbl.h"
#include "xenv1.h"
#include "xfsys1.h"
#include <math.h> 
#include <stdlib.h>

char *frm_ptr;    /* ptr to internal form storage area. */
int UD_novideo_error = 0;
extern int UD_macroflag;
				 /* See macdyn.c                       */
static char sdastyp[25][16]={
	"UD_DASCART","UD_DASSELECT","xxx","UD_DASVAL","UD_DASDISTANCE",
	"UD_DASINT", "UD_DASVEC","UD_DASPICK","UD_DASPCKLOC","UD_DASSTRING",
	"UD_DASCHOICE","UD_DASNDC","UD_DASANGLE","UD_DASUNITLESS", "UD_DASSTRINGDEF", 
	"UD_DASPLANE", "UD_DASSCALAR", "UD_SCACART", "UD_SCAVAL", "UD_SCADISTANCE", "UD_SCAINT", 
	"UD_SCAVEC", "UD_SCANDC", "UD_SCAANGLE", "UD_SCAUNITLESS"};

static char inptyp[6][16] = {
			"FORM_STRING", "FORM_PICK", "FORM_LOCATE", "FORM_RECORD",
			"FORM_LABEL", "FORM_SUBSCR"};

static char reftyp[9][16]={
	"LOWER_LEFT","LOWER_CENTER","LOWER_RIGHT","CENTER_LEFT","CENTER_CENTER",
	"CENTER_RIGHT", "UPPER_LEFT","UPPER_CENTER","UPPER_RIGHT"};

static char Sformdir[20]={"UD_FORMDIR"};

void init_choicedata(), init_colordata();
void ud_delform();
void ud_int_frm_read();
extern char uw_color_name[64][96];
extern int UW_Store_forms;

/*********************************************************************
**    E_FUNCTION :  ud_set_formdir(fdir)
**       Sets the current system form directory.  This directory will
**       automatically be reset after opening the form file.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          fdir    = System form directory.
**    RETURNS      : none
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
void ud_set_formdir(fdir)
char *fdir;
{
	strcpy(Sformdir,fdir);
}
/*********************************************************************
**    E_FUNCTION :  int ud_loadfrm(fname,fstruct) -- load a form 
**                                                      from a file.
**    PARAMETERS   
**       INPUT  :  char *fname -- filename to read from.
**       OUTPUT :       UD_FSTRUT *fstruct; -- structure to put form into.
**    RETURNS      : 0 if all went OK, 1 if form too large and was  reduced,
**                                                      2 = file wouldn't open, etc.
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
int ud_loadfrm(fname,fstruct)/*load a form structure from file*/
char *fname;                                                            /* filename to read from */
UD_FSTRUCT *fstruct;                                            /* structure to load form into */
{
	FILE *fd;
	int irtn, status;
	UX_pathname filename;
	char tmpstr[UX_MAX_PATH_LEN+20];

	uu_denter(UU_DTRC,(us,"ud_loadfrm(%s,%x,%x)",fname,fstruct));
/*
....First check if it is an internal form
*/
    if (!strcmp(fname,"INTERNAL.INTERNAL"))
    {
       irtn=ud_ld_int_frm(fstruct);  /* read the internal structure */
       return(irtn);
    }
    else
    {
		strncpy(fstruct->frmname, fname, UD_MAXNAM);
		fstruct->frmname[UD_MAXNAM] = '\0';     
		uu_dprint(UU_DTRC,(us,"after udos: filename = %s", filename));

		strcpy(filename, fname);
		status = ul_open_mod_file("UU_USER_SETTINGS","forms",Sformdir,
			UU_NULL,filename, 2, &fd);
		if (status != UU_SUCCESS || fd == NULL) 
		{
			if (strcmp(Sformdir,"UD_FORMDIR") == 0)
			{
				sprintf(tmpstr,"Can't open form file %s",filename);
				ud_wrerr(tmpstr);
			}
			uu_dprint(-1,(us,"ud_loadfrm cant open file %s",filename));
			irtn=2;
		}
		else
		{                                                /* file name ok */
			irtn=ud_ldfrm(fd,fstruct);    /* read the structure */
			ux_fclose0 (fd);
		}
/*
.....Reset system form directory
*/
		strcpy(Sformdir,"UD_FORMDIR");
		uu_dexit;
		return(irtn);
	}
}

/*********************************************************************
**    I_FUNCTION :  ud_ldfrm(fd,fstruct); -- load form.
**    PARAMETERS   
**       INPUT  :       FILE *fd -- an open file pointer.
**       OUTPUT :   UD_FSTRUCT *fstruct; -- structure to read form into.
**    RETURNS      : 0 if all went OK, else 1.
**    SIDE EFFECTS : Mallocs memory that is not freed.  Call ud_delform to
**                                                              free this memory.
**    WARNINGS     : none
*********************************************************************/
int ud_ldfrm(fd,fstruct)                                        /* read form into fstruct */
FILE *fd;
UD_FSTRUCT *fstruct;
{
	int i,k,irtn;
	int isub;
	int fldno;

	char formstr[256];
	char ctyp[256],cmsg[256], num[256];
	int stat,status, helplen, help_start, ityp,temp, secno;
	char *tempstr;
	UD_DISPLAY *temp_display_flds;
	UD_PICTURE *temp_picture_flds;
	UD_INPUT *temp_input_flds;
	UD_FRAME *temp_frame_flds;
	UD_SECTION *temp_section_flds;
	int sep_no = 0;
	int maxsub=26;
/*
.....CHOICEBOX: display a comb box can't edited the edit field
.....			input/return is a integer (select choice index number)
.....CHOICE_LIST: display a comb box can't edited the edit field
.....			input/return is a list (UD_LIST), selection is in list.answer
.....COMBLIST_SIMPLE: display a comb box but the user can edited the edit field
.....			The combo box look like the edit field in the top and a list box
.....			under the edit field (CBS_SIMPLE style)
.....			input/return is a list (UD_LIST), selection/text input is in list.answer
.....COMBLIST_DROPDOWN: display a comb box but the user can edited the edit field
.....			The combo box look like the choice list box but the choice is editable
.....			(CBS_DROPDOWN style). In UNIX, The combo box look like the edit field is 
.....			in the left and a choice button at the right, when select the choice from
.....			the choice button, it will update the text field as that choice text.
.....			The "/LEN/ len" will define the text field length, in WinNT PC, this will be 
.....			the whole length of the whole field (dropdown listbox); in UNIX this will be
.....			the length of the text field, the whole length is define in /SIZE/wid, hgt
.....			the WinNT will ignore the 'wid" define here
.....			input/return is a list (UD_LIST), selection/text input is in list.answer
*/
	static char csub[26][20] = { "HEADER", "PUSHBUTTON", "CHOICEBOX", "EDIT",
									"LABEL", "LISTBOX", "CHECKBOX", "COMBLIST_SIMPLE",
									"COMBLIST_DROPDOWN", "HELP", "PICTURE", "DISPLAY",
									 "FRAME", "PROGRESS", "CHOICE_LIST", "EDITBOX", "LISTTABLE",
									 "COLOR", "DATATABLE", "PICTUREBOX", "SECTION", "SECTION_BAR", 
									 "SELECT", "SLIDER", "IMGBUTTON","MACRO"};

	uu_denter(UU_DTRC,(us,"ud_ldfrm()"));
	fstruct->n_input_fields=0;              /* number of input field defs */
	fstruct->n_display_fields=0;    /* number of display fields */
	fstruct->n_picture_fields=0;    /* number of picture fields */
	fstruct->n_frame_fields=0;      /* number of frame */
	fstruct->n_section_fields = 0;
	fstruct->section_flds = NULL;
	fstruct->ud_display_mask = NULL;
	fstruct->helpflag = 0;
	fstruct->frmhelp = NULL;
	fstruct->dock_dir = -1;
	fstruct->win_area[0] = '\0';
	fstruct->dockable[0] = 0;
	fstruct->dockable[1] = 0;
	fstruct->dockable[2] = 0;
	fstruct->dockable[3] = 0;
	fstruct->ref = 4;   /*CENTER_CENTER as default*/
	fstruct->att_win = 0; /*Window as default */
	fstruct->macroflag = 0;

	irtn=0;

	help_start = 0;
/*
......read file and fill in dialog structure
*/
	while (1)
	{
		stat = ul_fread(fd,formstr,sizeof(formstr),&temp);
#if UU_COMP != UU_WIN2K
		if (stat == UX_EOF)
		{
			stat = UU_SUCCESS;
			goto done;
		}
#else
		if (temp <= 0 && stat == UX_EOF)
		{
			stat = UU_SUCCESS;
			goto done;
		}
		else if (stat == UX_EOF)
		{
			formstr[temp+1] = '\0';
			stat = UU_SUCCESS;
/*                      goto check;*/
		}
#endif
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			ud_wrerr("Error reading from Form file.");
			status = UU_FAILURE;
			goto done;
		}
/*
              if (temp==0)
              {
                      if (feof(fd))
                      {
                              if (fstruct->n_input_fields==0)
                              {
                                      ud_wrerr("empty form file");
                                      status==UU_FAILURE;
                                      goto done;
                              }
                              else 
                                      break;
                      }
                      else
                      {
                              ud_wrerr("file read error from form file");
                              status==UU_FAILURE;
                              goto done;
                      }
              }
*/
/*
.....parse the string and fill in form structure.
*/
/*
                indx = (char*)strchr(formstr, '\n');
                if (indx!=NULL)
                      *indx = '\0';
*/
		if ((strlen(formstr)==0)&&(help_start==0))
			continue;
		stat = ul_modal_check (formstr,&ityp,ctyp,cmsg);
/*
.....Invalid syntax
*/
		if ((stat != 0)&&(help_start==0))
		{
			sprintf (num, "Form file syntax error. %s\n",formstr);
			ud_wrerr(num);
			status = UU_FAILURE;
			goto done;
		}
/*
.....help will get any text util reach to another valid # form type
*/
		if ((help_start==1)&&(ityp!=1))
		{
			if (fstruct->frmhelp!=NULL)
			{
				helplen = strlen(fstruct->frmhelp) + strlen(formstr) + 2;
				tempstr = (char *)uu_malloc(helplen*sizeof(char));
				strcpy(tempstr, fstruct->frmhelp);
				strcat(tempstr, formstr);
				strcat(tempstr, "\n");
				uu_free(fstruct->frmhelp);
				fstruct->frmhelp = tempstr;
			}
			else
			{
				helplen = strlen(formstr) + 2;
				fstruct->frmhelp = (char *)uu_malloc(helplen*sizeof(char));
				strcpy(fstruct->frmhelp,formstr);
				strcat(fstruct->frmhelp, "\n");  
			}
			continue;
		}
		switch (ityp)
		{
		case 1:
			for (i=0;i<maxsub;i++)
			{
				ul_to_upper(ctyp);
				if (strcmp(ctyp,csub[i]) == 0) break;
			}
			if ((i >= maxsub)&&(help_start!=1))
			{
				sprintf (num, "Not a valid FORM modal.  /%s/ %s\n",ctyp,cmsg);
				ud_wrerr(num);
				status = UU_FAILURE;
				goto done;
			}
			if ((i >= maxsub)&&(help_start==1))
			{
				if (fstruct->frmhelp!=NULL)
				{
					helplen = strlen(fstruct->frmhelp) + strlen(formstr) + 2;
					tempstr = (char *)uu_malloc(helplen*sizeof(char));
					strcpy(tempstr, fstruct->frmhelp);
					strcat(tempstr, formstr);
					strcat(tempstr, "\n");
					uu_free(fstruct->frmhelp);
					fstruct->frmhelp = tempstr;
				}
				else
				{
					helplen = strlen(formstr) + 2;
					fstruct->frmhelp = (char *)uu_malloc(helplen*sizeof(char));
					strcpy(fstruct->frmhelp,formstr);
					strcat(fstruct->frmhelp, "\n");
				}
				break;
			}
			isub = i + 1;
			if (i==4)
/*
.....label only
*/
			{
				fldno = (fstruct->n_display_fields);
				(fstruct->n_display_fields)++;
				if (fstruct->n_display_fields==1)
				{
					fstruct->display_flds = 
								(UD_DISPLAY *) uu_malloc
								(fstruct->n_display_fields*sizeof(UD_DISPLAY));
				}
				else
				{
					temp_display_flds =
								(UD_DISPLAY *) uu_malloc
								(fstruct->n_display_fields*sizeof(UD_DISPLAY));
					uu_move_byte(fstruct->display_flds, temp_display_flds,
								(fstruct->n_display_fields-1)*sizeof(UD_DISPLAY));
					fstruct->display_flds = temp_display_flds;
				}
				strcpy(fstruct->display_flds[fldno].fcolor, "DEFAULT");
				strcpy(fstruct->display_flds[fldno].bcolor, "DEFAULT");
				fstruct->display_flds[fldno].font_scale = 1.0;
				fstruct->display_flds[fldno].pos.x = -1;        
				fstruct->display_flds[fldno].pos.y = -1;     
				fstruct->display_flds[fldno].section = -1;
				secno = fstruct->n_section_fields-1;
				if ((secno>=0)&&(stricmp(fstruct->section_flds[secno].name, "ALL")==0))
					secno = -1;
				if (secno>=0)
				{
					fstruct->display_flds[fldno].section = secno - sep_no;        
				}
			}
			else if (i==12)
			{
				fldno = (fstruct->n_frame_fields);
				(fstruct->n_frame_fields)++;
				if (fstruct->n_frame_fields==1)
				{
					fstruct->frame_flds = 
								(UD_FRAME *) uu_malloc
								(fstruct->n_frame_fields*sizeof(UD_FRAME));
				}
				else
				{
					temp_frame_flds =
								(UD_FRAME *) uu_malloc
								(fstruct->n_frame_fields*sizeof(UD_FRAME));
					uu_move_byte(fstruct->frame_flds, temp_frame_flds,
								(fstruct->n_frame_fields-1)*sizeof(UD_FRAME));
					fstruct->frame_flds = temp_frame_flds;
				}
				fstruct->frame_flds[fldno].x = -1;
				fstruct->frame_flds[fldno].y = -1;
				fstruct->frame_flds[fldno].cx = -1;
				fstruct->frame_flds[fldno].cy = -1;
				strcpy(fstruct->frame_flds[fldno].fcolor, "DEFAULT");
				strcpy(fstruct->frame_flds[fldno].bcolor, "DEFAULT");
				fstruct->frame_flds[fldno].font_scale = 1.0;
				fstruct->frame_flds[fldno].section = -1;
				secno = fstruct->n_section_fields-1;
				if ((secno>=0)&&(stricmp(fstruct->section_flds[secno].name, "ALL")==0))
					secno = -1;
				if (secno>=0)
				{
					fstruct->frame_flds[fldno].section = secno - sep_no;        
				}
			}
			else if (i==19)
/*
.....picture area only
*/
			{
				fldno = (fstruct->n_picture_fields);
				(fstruct->n_picture_fields)++;
				if (fstruct->n_picture_fields==1)
				{
					fstruct->picture_flds = 
								(UD_PICTURE *) uu_malloc
								(fstruct->n_picture_fields*sizeof(UD_PICTURE));
				}
				else
				{
					temp_picture_flds =
								(UD_PICTURE *) uu_malloc
								(fstruct->n_picture_fields*sizeof(UD_PICTURE));
					uu_move_byte(fstruct->picture_flds, temp_picture_flds,
								(fstruct->n_picture_fields-1)*sizeof(UD_PICTURE));
					fstruct->picture_flds = temp_picture_flds;
				}
				fstruct->picture_flds[fldno].x = -1;        
				fstruct->picture_flds[fldno].y = -1;        
				fstruct->picture_flds[fldno].cx = -1;
				fstruct->picture_flds[fldno].cy = -1;
				fstruct->picture_flds[fldno].fname[0] = '\0';
				fstruct->picture_flds[fldno].name[0] = '\0';
				fstruct->picture_flds[fldno].upt_flag = 0;
				fstruct->picture_flds[fldno].show_flag = 1;
				fstruct->picture_flds[fldno].section = -1;
				secno = fstruct->n_section_fields-1;
				if ((secno>=0)&&(stricmp(fstruct->section_flds[secno].name, "ALL")==0))
					secno = -1;
				if (secno>=0)
				{
					fstruct->picture_flds[fldno].section = secno - sep_no;        
				}
			}
			else if (i==20)
/*
......section
*/
			{
				fldno = fstruct->n_section_fields;
				if (fldno>0)
				{
					while (stricmp(fstruct->section_flds[fldno-1].name, "ALL")==0)
					{
/*
.....don't save all section
*/
						fstruct->n_section_fields--;
						fldno = fstruct->n_section_fields;
						if (fldno<=0) break;
					}
				}
				(fstruct->n_section_fields)++;
				if (fstruct->n_section_fields==1)
				{
					fstruct->section_flds = 
								(UD_SECTION *) uu_malloc
								(fstruct->n_section_fields*sizeof(UD_SECTION));
				}
				else
				{
					temp_section_flds =
								(UD_SECTION *) uu_malloc
								(fstruct->n_section_fields*sizeof(UD_SECTION));
					uu_move_byte(fstruct->section_flds, temp_section_flds,
								(fstruct->n_section_fields-1)*sizeof(UD_SECTION));
					fstruct->section_flds = temp_section_flds;
				}
				fstruct->section_flds[fldno].name[0] = '\0';
				strcpy(fstruct->section_flds[fldno].color, "DEFAULT");
				fstruct->section_flds[fldno].type = 0;
			}
/////////////////////////
			else if (i==21)
/*
......section bar
*/
			{
				fldno = fstruct->n_section_fields;
				(fstruct->n_section_fields)++;
				if (fstruct->n_section_fields==1)
				{
					fstruct->section_flds = 
								(UD_SECTION *) uu_malloc
								(fstruct->n_section_fields*sizeof(UD_SECTION));
				}
				else
				{
					temp_section_flds =
								(UD_SECTION *) uu_malloc
								(fstruct->n_section_fields*sizeof(UD_SECTION));
					uu_move_byte(fstruct->section_flds, temp_section_flds,
								(fstruct->n_section_fields-1)*sizeof(UD_SECTION));
					fstruct->section_flds = temp_section_flds;
				}
				fstruct->section_flds[fldno].name[0] = '\0';
				strcpy(fstruct->section_flds[fldno].color, "DEFAULT");
				fstruct->section_flds[fldno].type = 1;
				sep_no++;
			}
////////////////////////
			else if ((i!=0)&&(i!=9)&&(i!=25))
			{
				(fstruct->n_input_fields)++;
/*
.....For "display type form",
.....the last method callback is "CLOSE FORM" callback
.....so reserve last input field structure for "CLOSE FORM"
*/
				if (fstruct->n_input_fields==1)
				{
					fstruct->input_flds = 
						(UD_INPUT *) uu_malloc
							((fstruct->n_input_fields+fstruct->n_display_fields+1)*
							sizeof(UD_INPUT));
				}
				else
				{
					temp_input_flds =
						(UD_INPUT *) uu_malloc
							((fstruct->n_input_fields+fstruct->n_display_fields+1)*
							sizeof(UD_INPUT));
					uu_move_byte(fstruct->input_flds, temp_input_flds,
								(fstruct->n_input_fields-1)*sizeof(UD_INPUT));
					fstruct->input_flds = temp_input_flds;
				}

				fstruct->input_flds[fstruct->n_input_fields-1].ud_prmloc.x = -1;
				fstruct->input_flds[fstruct->n_input_fields-1].ud_prmloc.y = -1;
				fstruct->input_flds[fstruct->n_input_fields-1].font_scale = 1.0;
				fstruct->input_flds[fstruct->n_input_fields-1].justified = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].shortcut[0] = '\0';
				fstruct->input_flds[fstruct->n_input_fields-1].active_flag = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				for (k=0;k<UD_NMENTWD;k++)
					fstruct->input_flds[fstruct->n_input_fields-1].ud_limit[k] = 0;
				strcpy(fstruct->input_flds[fstruct->n_input_fields-1].fcolor, "DEFAULT");
				strcpy(fstruct->input_flds[fstruct->n_input_fields-1].bcolor, "DEFAULT");
				strcpy(fstruct->input_flds[fstruct->n_input_fields-1].pfcolor, "DEFAULT");
				strcpy(fstruct->input_flds[fstruct->n_input_fields-1].pbcolor, "DEFAULT");
				fstruct->input_flds[fstruct->n_input_fields-1].picarea = NULL;
				fstruct->input_flds[fstruct->n_input_fields-1].n_picarea = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].section = -1;
				secno = fstruct->n_section_fields-1;
				if ((secno>=0)&&(stricmp(fstruct->section_flds[secno].name, "ALL")==0))
					secno = -1;
				if (secno>=0)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].section = secno - sep_no;        
				}
				if (i==1)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 6;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==2)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 2;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
/*
.....ud_input not used for CHOICEBOX before, but for UNIX, we will used it for #COLOR#
.....if #COLOR#, without default string, ud_input = 0;
.....if #COLOR#, with default string, ud_input = 1;
*/
#if UU_COMP!=UU_WIN2K
					fstruct->input_flds[fstruct->n_input_fields-1].ud_input 
								= 0;  
#endif
				}
				if (i==3)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 1;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_input 
								= FORM_STRING;  
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_modal = 0; 
				}
				if (i==5)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 5;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_input 
								= FORM_STRING;  
					fstruct->input_flds[fstruct->n_input_fields-1].ud_modal = 0; 
					fstruct->input_flds[fstruct->n_input_fields-1].ud_echo = 0;
				}
				if (i==6)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 3;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==7)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 5;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 1;
					fstruct->input_flds[fstruct->n_input_fields-1].defaults = NULL;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_echo = 0;
				}
/*
.....we will use COMBLIST_DROPDOWN
*/
				if (i==8)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 5;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 2;
					fstruct->input_flds[fstruct->n_input_fields-1].defaults = NULL;
				}
				if (i==10)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 7;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==11)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 8;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_input 
								= FORM_STRING;  
					fstruct->input_flds[fstruct->n_input_fields-1].ud_modal = 0; 
				}
				if (i==13)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 9;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==14)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 10;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==15)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 11;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==16)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 17;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_echo = 0;
				}
				if (i==17)
				{
/*
......for #color# field, use color button in Windows, but will use choicebox for UNIX
*/
#if UU_COMP == UU_WIN2K
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 18;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
#else
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 2;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 16;
					init_colordata(fstruct->n_input_fields-1, fstruct);
#endif
				}
				if (i==18)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 19;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==22)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 23;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_input = FORM_PICK;
				}
				if (i==23)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 24;
					fstruct->input_flds[fstruct->n_input_fields-1].justified = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].range[0].dint = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].range[1].dint = 100;
				}
				if (i==24)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 25;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].vfile = NULL;
				}
				fstruct->input_flds[fstruct->n_input_fields-1].ud_echo = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].ud_fprec = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].ud_flen = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].range_flag = 0;
			}
			if (i==2)
				init_choicedata(fstruct->n_input_fields-1, fstruct);
			if (i==9)
			{
				help_start = 1;
				fstruct->helpflag = 1;
			}
			else
				help_start = 0;
			if (i==25)
			{
				fstruct->macroflag = 1;
				UD_macroflag = 1;
			}
			break;
		case 2:
			switch (isub)
			{
			case 1:
				status = ud_playout_frmheader (ctyp,cmsg,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 2:
				status = ud_playout_formitem (ctyp,cmsg,1,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 3:
				status = ud_playout_formitem (ctyp,cmsg,2,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 4:
				status = ud_playout_formitem (ctyp,cmsg,3,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 5:
/*
.....label
*/
				status = ud_playout_formitem (ctyp,cmsg,4,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 6:
				status = ud_playout_formitem (ctyp,cmsg,5,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 7:
				status = ud_playout_formitem (ctyp,cmsg, 7,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 8:
				status = ud_playout_formitem(ctyp,cmsg, 8,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 9:
				status = ud_playout_formitem (ctyp,cmsg, 9,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 11:
				status = ud_playout_formitem (ctyp,cmsg, 10,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 12:
				status = ud_playout_formitem (ctyp,cmsg, 11,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 13:
				status = ud_playout_formframe (ctyp,cmsg, fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 14:
				status = ud_playout_formitem (ctyp,cmsg, 13,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 15:
				status = ud_playout_formitem (ctyp,cmsg, 14,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 16:
				status = ud_playout_formitem (ctyp,cmsg, 15,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 17:
				status = ud_playout_formitem (ctyp,cmsg, 16,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 18:
				status = ud_playout_formitem (ctyp,cmsg, 17,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 19:
				status = ud_playout_formitem (ctyp,cmsg, 18,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 20:
				status = ud_playout_formpicture (ctyp,cmsg, fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 21:
				status = ud_playout_formsection (ctyp,cmsg, fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 23:
				status = ud_playout_formitem (ctyp,cmsg, 23,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 24:
				status = ud_playout_formitem (ctyp,cmsg, 24, fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 25:
				status = ud_playout_formitem (ctyp,cmsg, 25, fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			}
			break;
		}
	}
done:;
	fldno = fstruct->n_section_fields;
	if (fldno>0)
	{
		while (stricmp(fstruct->section_flds[fldno-1].name, "ALL")==0)
		{
/*
.....don't save all section
*/
			fstruct->n_section_fields--;
			fldno = fstruct->n_section_fields;
			if (fldno<=0) break;
		}
	}
	return(status);
}
			
ud_playout_frmheader (ctyp, cmsg, fstruct)
char *ctyp, *cmsg;
UD_FSTRUCT *fstruct;
{
	int i,k,status,inum;
	char msg[256], *tok;
	int x, y, wid, hgt;
	int maxsub=6;
	static char csub[6][20] = {"TITLE", "POSITION","SIZE", "DOCKABLE","REFERENCE","ATTACH"};
	UU_REAL rval[2];
	
	status = UU_SUCCESS;
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid FORM modal.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
		return -1;
	}
	switch(i)
	{
/*
.....Position
*/
	case 1:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2)
		{
#if UU_COMP==UU_WIN2K
			if (cmsg[0]!='\0')
				strcpy((*fstruct).win_area, cmsg);
			(*fstruct).dock_dir = ud_find_menuarea_pos(cmsg);
#else
			(*fstruct).ud_frmdf.ud_fwin.ll.x = 0;
			(*fstruct).ud_frmdf.ud_fwin.ll.y = 0;
#endif
		}
		else
		{
			x = (int)rval[0];
			y = (int)rval[1];
			(*fstruct).ud_frmdf.ud_fwin.ll.x = x;
			(*fstruct).ud_frmdf.ud_fwin.ll.y = y;
		}
		break;
/*
.....Size
*/
	case 2:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		wid = (int)rval[0] ;
		hgt = (int)rval[1] ;
		(*fstruct).ud_frmdf.ud_frc.ud_c = wid;
		(*fstruct).ud_frmdf.ud_frc.ud_r = hgt;
		break;
/*
.....TITLE
*/
	case 0:
		strncpy((*fstruct).ud_frmdf.ud_fid, cmsg, UD_MAXNAM);
		(*fstruct).ud_frmdf.ud_fid[UD_MAXNAM] = '\0';
		break;
	case 3:
		ul_to_upper(cmsg);
		tok = (char*)strtok(cmsg, " ,\t\r\n");
check:;
		if (tok==NULL)
			break;
		if (strcmp(tok, "YES")==0)
		{
			(*fstruct).dockable[0] = 1;
			(*fstruct).dockable[1] = 1;
			(*fstruct).dockable[2] = 1;
			(*fstruct).dockable[3] = 1;
			break;
		}
		else if (strcmp(tok, "NO")==0)
		{
			(*fstruct).dockable[0] = 0;
			(*fstruct).dockable[1] = 0;
			(*fstruct).dockable[2] = 0;
			(*fstruct).dockable[3] = 0;
			break;
		}
		else if (strcmp(tok, "TOP")==0)
			(*fstruct).dockable[0] = 1;
		else if (strcmp(tok, "BOTTOM")==0)
			(*fstruct).dockable[1] = 1;
		else if (strcmp(tok, "LEFT")==0)
			(*fstruct).dockable[2] = 1;
		else if (strcmp(tok, "RIGHT")==0)
			(*fstruct).dockable[3] = 1;
		tok = (char*)strtok(NULL, " ,\t\r\n");
		goto check;
		break;
/*
.....reference
*/
	case 4:
		for (k=0; k<9; k++) 
			if (strcmp(cmsg,&reftyp[k][0])==0) break;
		if (k>=9) 
		{
			fstruct->ref = 4;   /*CENTER_CENTER as default*/
			goto bad_parm;
		}
		else 
			fstruct->ref = k;
		break;
/*
.....attach
*/
	case 5:
		fstruct->att_win = 0;
		if (strcmp(cmsg,"SCREEN")==0)
			fstruct->att_win = 1;
		else if (strcmp(cmsg,"WINDOW")==0)
			fstruct->att_win = 0;
		else
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (msg, "Invalid value for FORM modal. /%s/ %s\n",ctyp,cmsg);
	ud_printmsg(msg);
	status = UU_FAILURE;
done:;
	return(status);
}

void init_choicedata(itemnum, fstruct)
int itemnum;
UD_FSTRUCT *fstruct;
{
	int j;
	fstruct->input_flds[itemnum].n_defaults = 0;
	fstruct->input_flds[itemnum].defaults = (UD_DASIN*)uu_malloc
				(80*sizeof(UD_DASIN));
   for (j=0; j<80; j++)
   {
      fstruct->input_flds[itemnum].defaults[j].dstr = NULL;
   }
}

void init_colordata(itemnum, fstruct)
int itemnum;
UD_FSTRUCT *fstruct;
{
	int j, len;
	fstruct->input_flds[itemnum].defaults = (UD_DASIN*)uu_malloc
				(17*sizeof(UD_DASIN));
	for (j=0; j<16; j++)
	{
		len = strlen(uw_color_name[j]);
		fstruct->input_flds[itemnum].defaults[j].dstr =
										(char*)uu_malloc((len+1)*sizeof(char));
		strcpy(fstruct->input_flds[itemnum].defaults[j].dstr, uw_color_name[j]);
	}  
	fstruct->input_flds[itemnum].defaults[16].dstr = NULL;
	fstruct->input_flds[itemnum].n_defaults = 16;
}

void put_formchoice(itemnum, choices, fstruct)
int itemnum;
char *choices;
UD_FSTRUCT *fstruct;
{
	int len, tlen,i,j, start;
	char choice[80];
	int num = fstruct->input_flds[itemnum].n_defaults;
	tlen = strlen(choices);
	start = 0;
	j = 0;
	for (i=0; i<tlen; i++)
	{
		if (choices[i]!='\"')
		{
			if (start==1)
				choice[j++] = choices[i];
			else
				continue;
		}
		else if (start==0)
			start = 1;
		else
		{
			choice[j] = '\0';
			len = strlen(choice);
			fstruct->input_flds[itemnum].defaults[num].dstr =
										(char*)uu_malloc((len+1)*sizeof(char));
/*
.....choice declare more chars than
.....fstruct->input_flds[itemnum].defaults[num].dstr
.....so use strncpy
*/
/*
			strcpy(fstruct->input_flds[itemnum].defaults[num].dstr, choice);
*/
			strncpy(fstruct->input_flds[itemnum].defaults[num].dstr, choice,
							len);
			fstruct->input_flds[itemnum].defaults[num].dstr[len] = '\0';
			(fstruct->input_flds[itemnum].n_defaults)++;
			num = fstruct->input_flds[itemnum].n_defaults;
			start = 0;
			j = 0;
		}
	}
}

ud_playout_formframe(ctyp, cmsg, fstruct)
char *ctyp, *cmsg;
UD_FSTRUCT *fstruct;
{
	int i,status,inum;
	char msg[256], *tok;
	int fldno, len;
	int maxsub=5;
/*
......use color name table uw_color_name
*/
/*
	static char color[16][10] = {"BLACK","WHITE","BLUE", "RED", "GREEN",
	"MAGENTA", "YELLOW", "CYAN", "BROWN","TAN", "LTBLUE", "SEAGREEN",
	"ORANGE", "PINK", "PURPLE","GREY"};
*/
	static char csub[5][20] = {"TITLE", "POSITION","SIZE", "COLOR", "FONT"};
	UU_REAL rval[2];
	status = 0;
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid FRAME modal.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
		goto failed;
	}
	switch(i)
	{
		case 0:
			len=strlen(cmsg);
			fldno = fstruct->n_frame_fields-1;
			strncpy(fstruct->frame_flds[fldno].title, cmsg, len);
			fstruct->frame_flds[fldno].title[len] = '\0';
			break;
		case 1:
/*
......POSITION:
*/
			if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
				(inum != 2)) goto bad_parm;
			fldno = fstruct->n_frame_fields-1;
			fstruct->frame_flds[fldno].x = (int)rval[0]; 
			fstruct->frame_flds[fldno].y = (int)rval[1]; 
			break;
		case 2:
/*
......SIZE
*/
			if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
				(inum != 2)) goto bad_parm;
			fldno = fstruct->n_frame_fields-1;
			fstruct->frame_flds[fldno].cx = (int)rval[0];        
			fstruct->frame_flds[fldno].cy = (int)rval[1];        
			break;

		case 3:
/*
.....PCOLOR
*/
			fldno = fstruct->n_frame_fields-1;
			tok = (char*)strtok (cmsg, " ,\t\r\n");
			if (tok==NULL)
				break;
/*			for (i=0;i<16;i++)
			{
				if (stricmp(tok, color[i]) == 0) break;
			}
*/
			for (i=0;i<64;i++)
			{
				if (stricmp(tok, uw_color_name[i]) == 0) break;
			}
			if ((i==64) && (stricmp(tok, "DEFAULT") != 0))
				goto bad_parm;
			strcpy(fstruct->frame_flds[fldno].fcolor, tok);
		
			tok = (char*)strtok (NULL, " ,\t\r\n");
			if (tok==NULL)
				break;
/*
			for (i=0;i<16;i++)
			{
				if (stricmp(tok, color[i]) == 0) break;
			}
*/
			for (i=0;i<64;i++)
			{
				if (stricmp(tok, uw_color_name[i]) == 0) break;
			}
			if ((i==64) && (stricmp(tok, "DEFAULT") != 0))
				goto bad_parm;
			strcpy(fstruct->frame_flds[fldno].bcolor, tok);
			break;
		case 4:
/*
.....FONT scale
*/
			fldno = fstruct->n_frame_fields-1;
			fstruct->frame_flds[fldno].font_scale = atof(cmsg);
			break;

		default:
			goto bad_parm;
	}
	goto done;
bad_parm:;
	sprintf (msg, "Invalid value for FRAME modal. /%s/ %s\n",ctyp,cmsg);
	ud_printmsg(msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	 return status;
}

ud_playout_formpicture(ctyp, cmsg, fstruct)
char *ctyp, *cmsg;
UD_FSTRUCT *fstruct;
{
	int i,status,inum;
	char msg[256];
	int fldno, len;
	int maxsub=4;
	static char csub[4][20] = {"FILE", "POSITION","SIZE", "NAME"};
	UU_REAL rval[2];
	status = 0;
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid PICTURE modal.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
		goto failed;
	}
	switch(i)
	{
		case 0:
			len=strlen(cmsg);
			fldno = fstruct->n_picture_fields-1;
			strncpy(fstruct->picture_flds[fldno].fname, cmsg, len);
			fstruct->picture_flds[fldno].fname[len] = '\0';
			break;
		case 1:
/*
......POSITION:
*/
			if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
				(inum != 2)) goto bad_parm;
			fldno = fstruct->n_picture_fields-1;
			fstruct->picture_flds[fldno].x = (int)rval[0]; 
			fstruct->picture_flds[fldno].y = (int)rval[1]; 
			break;
		case 2:
/*
......SIZE
*/
			if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
				(inum != 2)) goto bad_parm;
			fldno = fstruct->n_picture_fields-1;
			fstruct->picture_flds[fldno].cx = (int)rval[0];        
			fstruct->picture_flds[fldno].cy = (int)rval[1];        
			break;
		case 3:
/*
......NAME
*/
			len=strlen(cmsg);
			fldno = fstruct->n_picture_fields-1;
			strncpy(fstruct->picture_flds[fldno].name, cmsg, len);
			fstruct->picture_flds[fldno].name[len] = '\0';
			break;
		default:
			goto bad_parm;
	}
	goto done;
bad_parm:;
	sprintf (msg, "Invalid value for Picture modal. /%s/ %s\n",ctyp,cmsg);
	ud_printmsg(msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	 return status;
}
ud_playout_formsection(ctyp, cmsg, fstruct)
char *ctyp, *cmsg;
UD_FSTRUCT *fstruct;
{
	int i,status,inum;
	char msg[256];
	int fldno, len;
	int maxsub=2;
	static char csub[2][20] = {"COLOR", "NAME"};
	UU_REAL rval[2];
	status = 0;
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid SECTION modal.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
		goto failed;
	}
	switch(i)
	{
		case 0:
			len=strlen(cmsg);
			fldno = fstruct->n_section_fields-1;
			if (len>0)
			{	
				strncpy(fstruct->section_flds[fldno].color, cmsg, len);
				if (cmsg[0]=='*')
				{
					len--;
					strncpy(fstruct->section_flds[fldno].color, &(cmsg[1]), len);
				}
				else
					strncpy(fstruct->section_flds[fldno].color, cmsg, len);
			}
			else
				len = 0;
			fstruct->section_flds[fldno].color[len] = '\0';
			break;
		case 1:
/*
......NAME
*/
			len=strlen(cmsg);
			fldno = fstruct->n_section_fields-1;
			strncpy(fstruct->section_flds[fldno].name, cmsg, len);
			fstruct->section_flds[fldno].name[len] = '\0';
			break;
		default:
			goto bad_parm;
	}
	goto done;
bad_parm:;
	sprintf (msg, "Invalid value for Section modal. /%s/ %s\n",ctyp,cmsg);
	ud_printmsg(msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	 return status;
}


int ud_playout_formitem (ctyp, cmsg, type, fstruct)
char *ctyp, *cmsg;
int type;
UD_FSTRUCT *fstruct;
{
	int i,k, status,inum,nc,inc;
	unsigned *ptr;
	char msg[256],cstr[132], tmpstr[256];
	UX_pathname fname;
	int fldno, len, idx;
	char *tok;
	int maxsub=22;
	static char csub[22][20] = {"LABEL", "POSITION","SIZE", "TYPE", "CHOICES", 
					"INPUT", "RANGE", "PREC", "LEN", "MODAL", "COLOR", "PCOLOR",
					"FONT", "JUSTIFIED", "SHORTCUT","LIMIT","ACTIVE", "PICTURE",
					"DOUBLE_CLICK", "SORT", "FILTER", "FILE"};
	UU_REAL rval[2];
	UU_REAL rval2[6];
	UU_REAL pval[4];
/*
	static char color[16][10] = {"BLACK","WHITE","BLUE", "RED", "GREEN",
	"MAGENTA", "YELLOW", "CYAN", "BROWN","TAN", "LTBLUE", "SEAGREEN",
	"ORANGE", "PINK", "PURPLE","GREY"};
*/
	int ngeo=14;
	static char geotyp[14][10]={"POINT","LINE","CIRCLE","PLANE","VECTOR",
		"PNTVEC","CURVE","SURF","PATERN","SYMBOL","ANOTE","MATRIX","SHAPE",
		"SOLID"};
	static char activ[3][10]={"NO","DEFAULT","YES"};


	status = 0;
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid FORM modal.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
		goto failed;
	}
	switch(i)
	{
		case 0:
/*
.....LABEL
*/
			if (type==4)
/*
......only label, no input fiield
*/
			{
				len=strlen(cmsg);
				fldno = fstruct->n_display_fields-1;
				fstruct->display_flds[fldno].message =
					(char*)uu_malloc(sizeof(char)*(len+1));
/*
.....cmsg may declare more chars than
.....fstruct->display_flds[fldno].message
.....so use strncpy
*/
/*
				strcpy(fstruct->display_flds[fldno].message, cmsg);
*/
				strncpy(fstruct->display_flds[fldno].message, cmsg, len);
				fstruct->display_flds[fldno].message[len] = '\0';
			}
			else
			{
				len=strlen(cmsg);
				fldno = fstruct->n_input_fields-1;
				fstruct->input_flds[fldno].prompt =
							(char*)uu_malloc(sizeof(char)*(len+1));
/*
.....cmsg may declare more chars than
.....fstruct->input_flds[fldno].prompt
.....so use strncpy
*/
/*
				strcpy(fstruct->input_flds[fldno].prompt, cmsg);
*/
				strncpy(fstruct->input_flds[fldno].prompt, cmsg, len);
				fstruct->input_flds[fldno].prompt[len] = '\0';
			}
			break;
		case 1:
/*
......POSITION:
*/
			if ((ul_to_reals(pval,&inum,4,cmsg) != UU_SUCCESS) ||
				(inum < 2)) goto bad_parm;
			if (type==4)
/*
......only label, no input fiield
*/
			{
				fldno = fstruct->n_display_fields-1;
				fstruct->display_flds[fldno].pos.ud_c = (int)pval[0];        
				fstruct->display_flds[fldno].pos.ud_r = (int)pval[1];        
			}
			else
			{
				fldno = fstruct->n_input_fields-1;
				fstruct->input_flds[fldno].ud_prmloc.ud_c = (int)pval[0];    
				fstruct->input_flds[fldno].ud_prmloc.ud_r = (int)pval[1];
				if (inum >= 3)
					fstruct->input_flds[fldno].ud_prmloc.x = (int)pval[2];
				else
					fstruct->input_flds[fldno].ud_prmloc.x = -1;
				if (inum == 4)
					fstruct->input_flds[fldno].ud_prmloc.y = (int)pval[3];
				else
					fstruct->input_flds[fldno].ud_prmloc.y = -1;
			}
			break;
		case 2:
/*
.....SIZE: 
*/
			if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
				inum != 2) goto bad_parm;
			if (type!=4)
			{
				fldno = fstruct->n_input_fields-1;
				fstruct->input_flds[fldno].ud_fldloc.ud_c = (int)rval[0];    
				fstruct->input_flds[fldno].ud_fldloc.ud_r = (int)rval[1];    
			}
			else
			{
				fldno = fstruct->n_display_fields-1;
				fstruct->display_flds[fldno].pos.x = (int)rval[0];        
				fstruct->display_flds[fldno].pos.y = (int)rval[1];        
			}
			break;
		case 3:
/*
.....TYPE (data type)
*/
			if (type!=4)
			{
				fldno = fstruct->n_input_fields-1;
				for (k=0; k<25; k++) 
					if (strcmp(cmsg,&sdastyp[k][0])==0) break;
				if (k>=25) 
					fstruct->input_flds[fldno].ud_datatyp = UD_DASSTRING;   
				else 
					fstruct->input_flds[fldno].ud_datatyp = k+1;
				if (type==2)
				{
/*
.....if CHOICEBOX, we always use UD_DASSTRING, so keep consistence with before
*/
					fstruct->input_flds[fldno].ud_datatyp = UD_DASSTRING;
				}
			}
			break;
		case 4:
/*
......CHOICES
*/
			fldno = fstruct->n_input_fields-1;
			if (type!=17)
			{
				put_formchoice(fldno, cmsg, fstruct);
			}
			else if (cmsg[0]!='\0') /*if (strncmp(cmsg, "Default")==0) */
			{
/*
......for #color# field, use color button in Windows, but will use choicebox for UNIX
*/
#if UU_COMP == UU_WIN2K
				fstruct->input_flds[fldno].n_defaults = 1;
				fstruct->input_flds[fldno].defaults = (UD_DASIN*)uu_malloc
								(1*sizeof(UD_DASIN));
				len = strlen(cmsg);
				fstruct->input_flds[fldno].defaults[0].dstr =
										(char*)uu_malloc((len+1)*sizeof(char));
				strncpy(fstruct->input_flds[fldno].defaults[0].dstr, cmsg,
							len);
				fstruct->input_flds[fldno].defaults[0].dstr[len] = '\0';
#else
				len = strlen(cmsg);
				fstruct->input_flds[fldno].defaults[16].dstr =
										(char*)uu_malloc((len+1)*sizeof(char));
				strncpy(fstruct->input_flds[fldno].defaults[16].dstr, cmsg,
							len);
				fstruct->input_flds[fldno].defaults[16].dstr[len] = '\0';
				fstruct->input_flds[fldno].ud_input = 1;  
				fstruct->input_flds[fldno].n_defaults = 17;
#endif
			}
			break;
		case 5:
/*
......INPUT, only for EDIT field, use ud_input for this value, 
......LISTBOX field use INPUT data now
*/
/*			if (type!=3) */
			if ((type!=3)&&(type!=5)&&(type!=7)&&(type!=8)&&(type!=14)&&(type!=23))
				break;
			fldno = fstruct->n_input_fields-1;
			for (k=0; k<6; k++) 
				if (strcmp(cmsg,&inptyp[k][0])==0) break;
			if (k>=6) 
				fstruct->input_flds[fldno].ud_input = FORM_STRING;      
			else
				fstruct->input_flds[fldno].ud_input = k+1;
/*
.....always FORM_PICK for #SELECT#
*/
			if (type==23)
				fstruct->input_flds[fldno].ud_input = FORM_PICK;
			break;
		case 6:
/*
.....RANGE
*/
			if (type==4)
				break;
			fldno = fstruct->n_input_fields-1;
			switch(fstruct->input_flds[fldno].ud_datatyp) 
			{
				case UD_SCAINT:
				case UD_DASINT:
				{
					if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
						inum != 2) goto bad_parm;
					fstruct->input_flds[fldno].range[0].dint = (int)rval[0]; 
					fstruct->input_flds[fldno].range[1].dint = (int)rval[1];
					break;
				}
				case UD_SCACART:
				case UD_SCAVEC:
				case UD_SCANDC:
				case UD_DASCART:
				case UD_DASVEC:
				case UD_DASNDC:
				{
					if ((ul_to_reals(rval2,&inum,6,cmsg) != UU_SUCCESS) ||
						inum != 6) goto bad_parm;
					fstruct->input_flds[fldno].range[0].cord.cord[0] = rval2[0];
					fstruct->input_flds[fldno].range[0].cord.cord[1] = rval2[1];
					fstruct->input_flds[fldno].range[0].cord.cord[2] = rval2[2];
					fstruct->input_flds[fldno].range[1].cord.cord[0] = rval2[3];
					fstruct->input_flds[fldno].range[1].cord.cord[1] = rval2[4];
					fstruct->input_flds[fldno].range[1].cord.cord[2] = rval2[5];
					break;
				}
				case UD_SCAVAL:
				case UD_SCADISTANCE:
				case UD_SCAANGLE:
				case UD_SCAUNITLESS:
				case UD_DASSCALAR:
				case UD_DASVAL:
				case UD_DASDISTANCE:
				case UD_DASANGLE:
				case UD_DASUNITLESS:
				default:
				{
					if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
						inum != 2) goto bad_parm;
					fstruct->input_flds[fldno].range[0].dreal = rval[0]; 
					fstruct->input_flds[fldno].range[1].dreal = rval[1];
					break;
				}
			}
			fstruct->input_flds[fldno].range_flag = 1;
			break;
		case 7:
/*
.....PREC
*/
			if (type==4)
				break;
			fldno = fstruct->n_input_fields-1;
			fstruct->input_flds[fldno].ud_fprec = atoi(cmsg);
			break;
		case 8:
/*
......LEN
*/
			if (type==4)
				break;
			fldno = fstruct->n_input_fields-1;
			fstruct->input_flds[fldno].ud_flen = atoi(cmsg);
			break;
		case 9:
			fldno = fstruct->n_input_fields-1;
			if (stricmp(cmsg, "YES")==0)
				fstruct->input_flds[fldno].ud_modal = 1;
			break;
		case 10:
/*
.....COLOR
*/
			if (type==4)
			{
				fldno = fstruct->n_display_fields-1;
			}
			else
				fldno = fstruct->n_input_fields-1;
			tok = (char*)strtok (cmsg, " ,\t\r\n");
			if (tok==NULL)
				break;
/*
			for (i=0;i<16;i++)
			{
				if (stricmp(tok, color[i]) == 0) break;
			}
*/
			for (i=0;i<64;i++)
			{
				if (stricmp(tok, uw_color_name[i]) == 0) break;
			}
			if ((i==64) && (stricmp(tok, "DEFAULT") != 0))
				goto bad_parm;

			if (type==4)
			{
				strcpy(fstruct->display_flds[fldno].fcolor, tok);
			}
			else
			{
				strcpy(fstruct->input_flds[fldno].fcolor, tok);
			}	
			tok = (char*)strtok (NULL, " ,\t\r\n");
			if (tok==NULL)
				break;

/*			for (i=0;i<16;i++)
			{
				if (stricmp(tok, color[i]) == 0) break;
			}
*/
			for (i=0;i<64;i++)
			{
				if (stricmp(tok, uw_color_name[i]) == 0) break;
			}
			if ((i==64) && (stricmp(tok, "DEFAULT") != 0))
				goto bad_parm;
			if (type==4)
			{
				strcpy(fstruct->display_flds[fldno].bcolor, tok);
			}
			else
			{
				strcpy(fstruct->input_flds[fldno].bcolor, tok);
			}
			break;
		case 11:
/*
.....PCOLOR
*/
			if (type==4)
				goto bad_parm;
			fldno = fstruct->n_input_fields-1;
			tok = (char*)strtok (cmsg, " ,\t\r\n");
			if (tok==NULL)
				break;
/*
			for (i=0;i<16;i++)
			{
				if (stricmp(tok, color[i]) == 0) break;
			}
*/
			for (i=0;i<64;i++)
			{
				if (stricmp(tok, uw_color_name[i]) == 0) break;
			}
			if ((i==64) && (stricmp(tok, "DEFAULT") != 0))
				goto bad_parm;
			strcpy(fstruct->input_flds[fldno].pfcolor, tok);
		
			tok = (char*)strtok (NULL, " ,\t\r\n");
			if (tok==NULL)
				break;
/*
			for (i=0;i<16;i++)
			{
				if (stricmp(tok, color[i]) == 0) break;
			}
*/
			for (i=0;i<64;i++)
			{
				if (stricmp(tok, uw_color_name[i]) == 0) break;
			}
			if ((i==64) && (stricmp(tok, "DEFAULT") != 0))
				goto bad_parm;
			strcpy(fstruct->input_flds[fldno].pbcolor, tok);
			break;
		case 12:
/*
.....FONT scale
*/
			if (type==4)
			{
				fldno = fstruct->n_display_fields-1;
				fstruct->display_flds[fldno].font_scale = atof(cmsg);
			}
			else
			{
				fldno = fstruct->n_input_fields-1;
				fstruct->input_flds[fldno].font_scale = atof(cmsg);
			}
			break;
		case 13:
/*
.....JUSTIFIED
*/
			if (type==4)
				goto bad_parm;
			fldno = fstruct->n_input_fields-1;
			if (stricmp(cmsg, "RIGHT")==0)
				fstruct->input_flds[fldno].justified = 1;
			else if (stricmp(cmsg, "CENTER")==0)
				fstruct->input_flds[fldno].justified = 2;
			else if (stricmp(cmsg, "VERT")==0)
				fstruct->input_flds[fldno].justified = 1;
			else
				fstruct->input_flds[fldno].justified = 0;
			break;
		case 14:
/*
......SHORTCUT
*/
			if (type==4)
				goto bad_parm;
			fldno = fstruct->n_input_fields-1;
			strcpy(fstruct->input_flds[fldno].shortcut, cmsg);
			break;
/*
......LIMIT
*/
		case 15:
			if ((type!=3)&&(type!=23)) goto bad_parm;
			fldno = fstruct->n_input_fields-1;
			inc = 0;
			nc = strlen(cmsg);
			ul_strip_blanks(cmsg,&nc);
			while (inc < nc)
			{
				ul_parse_string(cmsg,nc,&inc,cstr);
				for (k=0; k<ngeo; k++) 
					if (ul_compare_upper(cstr,geotyp[k])==0) break;
				switch (k)
				{
				case 0:
					ptr = UD_ncl_pt;
					break;
				case 1:
					ptr = UD_ncl_ln;
					break;
				case 2:
					ptr = UD_ncl_ci;
					break;
				case 3:
					ptr = UD_ncl_pl;
					break;
				case 4:
					ptr = UD_ncl_ve;
					break;
				case 5:
					ptr = UD_ncl_pv;
					break;
				case 6:
					ptr = UD_ncl_allcv;
					break;
				case 7:
					ptr = UD_ncl_allsf;
					break;
				case 8:
					ptr = UD_ncl_patern;
					break;
				case 9:
					ptr = UD_symbol;
					break;
				case 10:
					ptr = UD_text;
					break;
				case 11:
					ptr = UD_ncl_mx;
					break;
				case 12:
					ptr = UD_ncl_sh;
					break;
				case 13:
					ptr = UD_solid;
					break;
			default:
					ptr = UU_NULL;
					break;
				}
				if (ptr != UU_NULL)
				{
					for (k=0;k<UD_NMENTWD;k++)
						fstruct->input_flds[fldno].ud_limit[k] = 
							fstruct->input_flds[fldno].ud_limit[k] | ptr[k];
				}
			}
			break;
/*
......ACTIVE
*/
		case 16:
			fldno = fstruct->n_input_fields-1;
			inc = 0;
			nc = strlen(cmsg);
			ul_strip_blanks(cmsg,&nc);
			for (k=0; k<3; k++) 
				if (ul_compare_upper(cmsg,activ[k])==0) break;
			if (k == 3) goto bad_parm;
			fstruct->input_flds[fldno].active_flag = k - 1;
			break;
		case 17:
/*
......PICTURE
*/
			len=strlen(cmsg);
			if (len==0) goto bad_parm;
			fldno = fstruct->n_input_fields-1;
			idx = fstruct->input_flds[fldno].n_picarea;
			if ((fstruct->input_flds[fldno].n_picarea==0)&&(fstruct->input_flds[fldno].picarea==NULL))
			{
/*
.....initial maximum 100 /PICTURE/ item
*/
				fstruct->input_flds[fldno].picarea = (UD_PICAREA *) uu_malloc
							(100*sizeof(UD_PICAREA));
				for (k=0; k<100; k++)
				{
					(fstruct->input_flds[fldno].picarea[k]).params = NULL;
					(fstruct->input_flds[fldno].picarea[k]).tooltext = NULL;
				}
			}
/*
.....not use strtok for this first one or two param (names) since it could be any string
.....use ',' in them as soon as it is quoted
			tok = (char*)strtok (cmsg, ",\r\n");
			if (tok==NULL)
			{
				goto bad_parm;
			}
			strcpy(fname, tok);
*/
/*
.....remove proceed spaces
*/
			k=0;
			while (cmsg[k]==' ') k++;
			strcpy(cmsg, &(cmsg[k]));
			strcpy(tmpstr, cmsg);
			if (tmpstr[0]!='\"')
			{
				/*tok = (char*)strtok(tmpstr, ",\t\n");*/
				tok = strtok(tmpstr, ",\t\n");
				if (tok==NULL)
				{
					goto bad_parm;
				}
				strcpy(fname, tok);
				len = strlen(fname);
			}
			else
			{
				/*tok = (char*)strtok(tmpstr+1, "\"");*/
				tok = strtok(tmpstr+1, "\"");
				if (tok==NULL)
				{
					goto bad_parm;
				}
				strcpy(fname, tok);
				len = strlen(fname) + 1;
			}
			strcpy(tmpstr, &(cmsg[len+1]));
/*
.....remove proceed spaces
*/
			k=0;
			while (fname[k]==' ') k++;
			strcpy(fname, &(fname[k]));
/*
......Remove trailing spaces
*/
			for (k=strlen(fname); k>0; k--)
			{
				if (fname[k-1]==' ')
					fname[k-1] = '\0';
				else
					break;
			}
			ul_remove_quotes(fname);

			if (fname[0]=='\0')
			{
				goto bad_parm;
			}
			strcpy ((fstruct->input_flds[fldno].picarea[idx]).name, fname);
/*
.....get next paramter, area name or area position
*/
/*
.....remove proceed spaces and ","
*/
			k=0;
			while ((tmpstr[k]==' ')||(tmpstr[k]==',')) k++;
			strcpy(tmpstr, &(tmpstr[k]));
			if (tmpstr[0]!='\"')
			{
				//tok = (char*)strtok(tmpstr, ",\t\n");
				tok = strtok(tmpstr, ",\t\n");
				if (tok==NULL)
				{
					goto bad_parm;
				}
				strcpy(fname, tok);
			}
			else
			{
				/*tok = (char*)strtok(tmpstr+1, "\"");*/
				tok = strtok(tmpstr+1, "\"");
				if (tok==NULL)
				{
					goto bad_parm;
				}
				strcpy(fname, tok);
			}
/*
.....remove proceed spaces
*/
			k=0;
			while (fname[k]==' ') k++;
			strcpy(fname, &(fname[k]));
/*
......Remove trailing spaces
*/
			for (k=strlen(fname); k>0; k--)
			{
				if (fname[k-1]==' ')
					fname[k-1] = '\0';
				else
					break;
			}
			ul_remove_quotes(fname);
			if (fname[0]=='\0')
			{
				goto bad_parm;
			}
			ul_remove_quotes(fname);
			status = ul_to_number(fname, &inum);
			if (status!=0)
			{
				len =strlen(fname);
				(fstruct->input_flds[fldno].picarea[idx]).tooltext = (char*) uu_malloc((len+1)*sizeof(char));
				strcpy((fstruct->input_flds[fldno].picarea[idx]).tooltext, fname);
				
				tok = (char*)strtok (NULL, ",\r\n");
				if (tok==NULL)
				{
					if ((fstruct->input_flds[fldno].picarea[idx]).tooltext!=NULL)
						uu_free((fstruct->input_flds[fldno].picarea[idx]).tooltext);
					(fstruct->input_flds[fldno].picarea[idx]).tooltext = NULL;
					goto bad_parm;
				}
			}
			status = ul_to_number(tok, &inum);
			if (status==0)
			{
				(fstruct->input_flds[fldno].picarea[idx]).xmin = inum;
			}
			else
			{
				if ((fstruct->input_flds[fldno].picarea[idx]).tooltext!=NULL)
					uu_free((fstruct->input_flds[fldno].picarea[idx]).tooltext);
				(fstruct->input_flds[fldno].picarea[idx]).tooltext = NULL;
				goto bad_parm;
			}
			tok = (char*)strtok (NULL, ",\r\n");
			if (tok==NULL)
			{
				if ((fstruct->input_flds[fldno].picarea[idx]).tooltext!=NULL)
					uu_free((fstruct->input_flds[fldno].picarea[idx]).tooltext);
				(fstruct->input_flds[fldno].picarea[idx]).tooltext = NULL;
				goto bad_parm;
			}
			(fstruct->input_flds[fldno].picarea[idx]).ymin = atoi(tok);

			tok = (char*)strtok (NULL, ",\r\n");
			if (tok==NULL)
			{
				if ((fstruct->input_flds[fldno].picarea[idx]).tooltext!=NULL)
					uu_free((fstruct->input_flds[fldno].picarea[idx]).tooltext);
				(fstruct->input_flds[fldno].picarea[idx]).tooltext = NULL;
				goto bad_parm;
			}
			(fstruct->input_flds[fldno].picarea[idx]).xmax = atoi(tok);

			tok = (char*)strtok (NULL, ",\r\n");
			if (tok==NULL)
			{
				if ((fstruct->input_flds[fldno].picarea[idx]).tooltext!=NULL)
					uu_free((fstruct->input_flds[fldno].picarea[idx]).tooltext);
				(fstruct->input_flds[fldno].picarea[idx]).tooltext = NULL;
				goto bad_parm;
			}
			(fstruct->input_flds[fldno].picarea[idx]).ymax = atoi(tok);

			fstruct->input_flds[fldno].n_picarea++;

			tok = (char*)strtok (NULL, "\r\n");
			if (tok==NULL)
			{
				break;
			}
			len =strlen(tok);
			if (len==0)
				break;
			(fstruct->input_flds[fldno].picarea[idx]).params = (char*) uu_malloc((len+1)*sizeof(char));
			strcpy((fstruct->input_flds[fldno].picarea[idx]).params, tok);
			break;
/*
......DOUBLE CLICK for CHOICE_LIST
*/
		case 18:
			fldno = fstruct->n_input_fields-1;
			if ((type!=5)&&(type!=8)&&(type!=16))
				goto bad_parm;
			nc = strlen(cmsg);
			ul_strip_blanks(cmsg,&nc);
/*
......ud_echo is not used (as defined in structure now), so use it
......here
*/
			if (ul_compare_upper(cmsg, "*ON")==0)
				fstruct->input_flds[fldno].ud_echo = 1;
			else
				fstruct->input_flds[fldno].ud_echo = 0;
			break;
		case 19:
			fldno = fstruct->n_input_fields-1;
			if ((type!=16)&&(type!=18))
				goto bad_parm;
			nc = strlen(cmsg);
			ul_strip_blanks(cmsg,&nc);
/*
......ud_fprec is not used for TABLELIST before, so use it for SORT flag
......here
*/
			if (ul_compare_upper(cmsg, "*ON")==0)
				fstruct->input_flds[fldno].ud_fprec = 0;
			else
				fstruct->input_flds[fldno].ud_fprec = 1;
			break;
		case 20:
			fldno = fstruct->n_input_fields-1;
			if ((type!=16)&&(type!=18))
				goto bad_parm;
			nc = strlen(cmsg);
			ul_strip_blanks(cmsg,&nc);
/*
......ud_flen is not used for TABLELIST before, so use it for FILTER flag
......here
*/
			if (ul_compare_upper(cmsg, "*ON")==0)
				fstruct->input_flds[fldno].ud_flen = 0;
			else
				fstruct->input_flds[fldno].ud_flen = 1;
			break;
		case 21:
			len=strlen(cmsg);
			fldno = fstruct->n_input_fields-1;
			fstruct->input_flds[fldno].vfile =
							(char*)uu_malloc(sizeof(char)*(len+1));
			strncpy(fstruct->input_flds[fldno].vfile, cmsg, len);
			fstruct->input_flds[fldno].vfile[len] = '\0';
			break;
		default:
			goto bad_parm;
	}
	goto done;
bad_parm:;
	sprintf (msg, "Invalid value for FORM modal. /%s/ %s\n",ctyp,cmsg);
	ud_printmsg(msg);
	fldno = fstruct->n_input_fields-1;
	if ((fstruct->input_flds[fldno].n_picarea==0)&&(fstruct->input_flds[fldno].picarea!=NULL))
	{
		uu_free(fstruct->input_flds[fldno].picarea);
		fstruct->input_flds[fldno].picarea = NULL;
	}
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	 return status;
}

/*********************************************************************
**    E_FUNCTION :  ud_delform(fstruct); -- Release memory malloc'ed
**                                                              by ud_ldfrm();
**    PARAMETERS   
**       INPUT  :       UD_FSTRUCT *fstruct; -- structure form was read into.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_delform(fstruct)                                 /* Release memory from fstruct */
UD_FSTRUCT *fstruct;
{
	int i,j;

	uu_denter(UU_DTRC,(us,"ud_delform(%x)", fstruct));
	if(fstruct->n_display_fields > 0) 
	{
		for (i=0; i<fstruct->n_display_fields; i++) 
			uu_free(fstruct->display_flds[i].message);
		uu_free(fstruct->display_flds);
	}
   for (i=0; i<fstruct->n_input_fields; i++)
	{
		uu_free(fstruct->input_flds[i].prompt);
		if(fstruct->input_flds[i].n_defaults > 0)
		{
			if(fstruct->input_flds[i].ud_datatyp == UD_DASSTRING)
			{
				for(j=0;j<fstruct->input_flds[i].n_defaults;j++)
				{
					if (fstruct->input_flds[i].defaults!=NULL)
						uu_free(fstruct->input_flds[i].defaults[j].dstr);
				}
			}
			if (fstruct->input_flds[i].range_flag>0) 
			{
				if(fstruct->input_flds[i].ud_datatyp == UD_DASSTRING)
				{
					if (fstruct->input_flds[i].range[0].dstr!=NULL)
						uu_free(fstruct->input_flds[i].range[0].dstr);
					if (fstruct->input_flds[i].range[1].dstr!=NULL)
						uu_free(fstruct->input_flds[i].range[1].dstr);
				}
			}
			if (fstruct->input_flds[i].defaults!=NULL)
				uu_free(fstruct->input_flds[i].defaults);
		}
		if (fstruct->input_flds[i].picarea!=NULL)
		{
			for(j=0;j<fstruct->input_flds[i].n_picarea;j++)
			{
				if (fstruct->input_flds[i].picarea[j].tooltext!=NULL)
					uu_free(fstruct->input_flds[i].picarea[j].tooltext);
				if (fstruct->input_flds[i].picarea[j].params!=NULL)
					uu_free(fstruct->input_flds[i].picarea[j].params);
			}
			uu_free(fstruct->input_flds[i].picarea);
			fstruct->input_flds[i].picarea = NULL;
		}
    }
	if (fstruct->n_frame_fields>0)
		uu_free(fstruct->frame_flds);
	if (fstruct->n_picture_fields>0)
		uu_free(fstruct->picture_flds);
	if(fstruct->input_flds!=NULL) uu_free(fstruct->input_flds);
	if(fstruct->traverse_mask!=NULL) uu_free(fstruct->traverse_mask);
	if(fstruct->ud_display_mask!=NULL) uu_free(fstruct->ud_display_mask);
	if(fstruct->section_flds!=NULL) 
		uu_free(fstruct->section_flds);
	if (fstruct->frmhelp!=NULL) 
		uu_free(fstruct->frmhelp);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ud_ld_int_frm(fstruct); -- load internal form.
**    PARAMETERS   
**       OUTPUT :   UD_FSTRUCT *fstruct; -- structure to read form into.
**    RETURNS      : 0 if all went OK, else 1.
**    SIDE EFFECTS : Mallocs memory that is not freed.  Call ud_delform to
**                                                              free this memory.
**    WARNINGS     : none
*********************************************************************/
int ud_ld_int_frm(fstruct)                                      /* read form into fstruct */
UD_FSTRUCT *fstruct;
{
	int i,k;
	int isub;
	int fldno;
	char formstr[256];
	char ctyp[256],cmsg[256], num[256];
	char tmpstr[256];
	int stat,status, helplen, help_start, ityp, secno;
	char *indx;
	char *tempstr;
	UD_DISPLAY *temp_display_flds;
	UD_INPUT *temp_input_flds;
	UD_FRAME *temp_frame_flds;
	UD_PICTURE *temp_picture_flds;
	UD_SECTION *temp_section_flds;
	int sep_no = 0;
	int maxsub=26;
	static char csub[26][20] = { "HEADER", "PUSHBUTTON", "CHOICEBOX", "EDIT",
									"LABEL", "LISTBOX", "CHECKBOX", "COMBLIST_SIMPLE",
									"COMBLIST_DROPDOWN", "HELP", "PICTURE", "DISPLAY",
									"FRAME", "PROGRESS", "CHOICE_LIST", "EDITBOX", "LISTTABLE",
									"COLOR", "DATATABLE", "PICTUREBOX", "SECTION", "SECTION_BAR", 
									"SELECT", "SLIDER", "IMGBUTTON", "MACRO"};

/*
......read file and fill in dialog structure
*/

	uu_denter(UU_DTRC,(us,"ud_ldfrm()"));
	fstruct->n_input_fields=0;              /* number of input field defs */
	fstruct->n_display_fields=0;    /* number of display fields */
	fstruct->n_picture_fields=0;    /* number of picture fields */
	fstruct->n_frame_fields=0;      /* number of frame */
	fstruct->n_section_fields = 0;
	fstruct->section_flds = NULL;
	fstruct->ud_display_mask = NULL;
	fstruct->helpflag = 0;
	fstruct->frmhelp = NULL;
	fstruct->dock_dir = -1;
	fstruct->win_area[0] = '\0';
	fstruct->dockable[0] = 0;
	fstruct->dockable[1] = 0;
	fstruct->dockable[2] = 0;
	fstruct->dockable[3] = 0;
	fstruct->ref = 4;   /*CENTER_CENTER as default*/
	fstruct->att_win = 0; /*Window as default */
	fstruct->macroflag = 0;

	status=0;
	help_start = 0;
	ud_int_frm_read(formstr,1,'\n');
	while (1)
	{
		indx = (char*)strchr(formstr, '\n');
		if (indx!=NULL)
			*indx = '\0';
		if (strcmp("~END", formstr)==0)
		{
			if (fstruct->n_input_fields==0)
			{
				sprintf(tmpstr,"empty internal form file");
				ud_wrerr(tmpstr);
				status = UU_FAILURE;
				goto done;
			}
			else    
				break;
		}
/*
.....parse the string and fill in dialog structure.
*/
		if (strlen(formstr)==0)
		{
			ud_int_frm_read(formstr,0,'\n');
			continue;
		}
		stat = ul_modal_check (formstr,&ityp,ctyp,cmsg);
/*
.....Invalid syntax
*/
		if ((stat != 0)&&(help_start==0))
		{
			sprintf (num, "Form file syntax error. %s\n",formstr);
			ud_wrerr(num);
			status = UU_FAILURE;
			goto done;
		}
/*
.....help will get any text util reach to another valid # form type
*/
		if ((help_start==1)&&(ityp!=1))
		{
			if (fstruct->frmhelp!=NULL)
			{
				helplen = strlen(fstruct->frmhelp) + strlen(formstr) + 3;
				tempstr = (char *)uu_malloc(helplen*sizeof(char));
				strcpy(tempstr, fstruct->frmhelp);
				strcat(tempstr, formstr);
				strcat(tempstr, "\n");
				uu_free(fstruct->frmhelp);
				fstruct->frmhelp = tempstr;
			}
			else
			{
				helplen = strlen(formstr) + 3;
				fstruct->frmhelp = (char *)uu_malloc(helplen*sizeof(char));
				strcpy(fstruct->frmhelp,formstr);
				strcat(fstruct->frmhelp, "\n");
			}
			ud_int_frm_read(formstr,0,'\n');
			continue;
		}
		switch (ityp)
		{
		case 1:
			for (i=0;i<maxsub;i++)
			{
				ul_to_upper(ctyp);
				if (strcmp(ctyp,csub[i]) == 0) break;
			}
			if ((i >= maxsub)&&(help_start!=1))
			{
				sprintf (num, "Not a valid FORM modal.  /%s/ %s\n",ctyp,cmsg);
				ud_wrerr(num);
				status = UU_FAILURE;
				goto done;
			}

			if ((i >= maxsub)&&(help_start==1))
			{
				if (fstruct->frmhelp!=NULL)
				{
					helplen = strlen(fstruct->frmhelp) + strlen(formstr) + 3;
					tempstr = (char *)uu_malloc(helplen*sizeof(char));
					strcpy(tempstr, fstruct->frmhelp);
					strcat(tempstr, formstr);
					strcat(tempstr, "\n");
					uu_free(fstruct->frmhelp);
					fstruct->frmhelp = tempstr;
				}
				else
				{
					helplen = strlen(formstr) + 3;
					fstruct->frmhelp = (char *)uu_malloc(helplen*sizeof(char));
					strcpy(fstruct->frmhelp,formstr);
					strcat(fstruct->frmhelp, "\n");
				}
				break;
			}
			isub = i + 1;
			if (i==4)
/*
.....label only
*/
			{
				fldno = fstruct->n_display_fields;
				(fstruct->n_display_fields)++;
				if (fstruct->n_display_fields==1)
				{
					fstruct->display_flds = 
								(UD_DISPLAY *) uu_malloc
								(fstruct->n_display_fields*sizeof(UD_DISPLAY));
				}
				else
				{
					temp_display_flds =
								(UD_DISPLAY *) uu_malloc
								(fstruct->n_display_fields*sizeof(UD_DISPLAY));
					uu_move_byte(fstruct->display_flds, temp_display_flds,
								(fstruct->n_display_fields-1)*sizeof(UD_DISPLAY));
					fstruct->display_flds = temp_display_flds;
				}
				strcpy(fstruct->display_flds[fldno].fcolor, "DEFAULT");
				strcpy(fstruct->display_flds[fldno].bcolor, "DEFAULT");
				fstruct->display_flds[fldno].font_scale = 1.0;
				fstruct->display_flds[fldno].pos.x = -1;        
				fstruct->display_flds[fldno].pos.y = -1;     
				fstruct->display_flds[fldno].section = -1;
				secno = fstruct->n_section_fields-1;
				if ((secno>=0)&&(stricmp(fstruct->section_flds[secno].name, "ALL")==0))
					secno = -1;
				if (secno>=0)
				{
					fstruct->display_flds[fldno].section = secno - sep_no;
				}
			}
			else if (i==12)
			{
				fldno = fstruct->n_frame_fields;
				(fstruct->n_frame_fields)++;
				if (fstruct->n_frame_fields==1)
				{
					fstruct->frame_flds = 
								(UD_FRAME *) uu_malloc
								(fstruct->n_frame_fields*sizeof(UD_FRAME));
				}
				else
				{
					temp_frame_flds =
								(UD_FRAME *) uu_malloc
								(fstruct->n_frame_fields*sizeof(UD_FRAME));
					uu_move_byte(fstruct->frame_flds, temp_frame_flds,
								(fstruct->n_frame_fields-1)*sizeof(UD_FRAME));
					fstruct->frame_flds = temp_frame_flds;
				}
				fstruct->frame_flds[fldno].x = -1;
				fstruct->frame_flds[fldno].y = -1;
				fstruct->frame_flds[fldno].cx = -1;
				fstruct->frame_flds[fldno].cy = -1;
				strcpy(fstruct->frame_flds[fldno].fcolor, "DEFAULT");
				strcpy(fstruct->frame_flds[fldno].bcolor, "DEFAULT");
				fstruct->frame_flds[fldno].font_scale = 1.0;
				fstruct->frame_flds[fldno].section = -1;
				secno = fstruct->n_section_fields-1;
				if ((secno>=0)&&(stricmp(fstruct->section_flds[secno].name, "ALL")==0))
					secno = -1;
				if (secno>=0)
				{
					fstruct->frame_flds[fldno].section = secno - sep_no;
				}
			}
			else if (i==19)
/*
.....picture area only
*/
			{
				fldno = (fstruct->n_picture_fields);
				(fstruct->n_picture_fields)++;
				if (fstruct->n_picture_fields==1)
				{
					fstruct->picture_flds = 
								(UD_PICTURE *) uu_malloc
								(fstruct->n_picture_fields*sizeof(UD_PICTURE));
				}
				else
				{
					temp_picture_flds =
								(UD_PICTURE *) uu_malloc
								(fstruct->n_picture_fields*sizeof(UD_PICTURE));
					uu_move_byte(fstruct->picture_flds, temp_picture_flds,
								(fstruct->n_picture_fields-1)*sizeof(UD_PICTURE));
					fstruct->picture_flds = temp_picture_flds;
				}
				fstruct->picture_flds[fldno].x = -1;        
				fstruct->picture_flds[fldno].y = -1;        
				fstruct->picture_flds[fldno].cx = -1;
				fstruct->picture_flds[fldno].cy = -1;
				fstruct->picture_flds[fldno].fname[0] = '\0';
				fstruct->picture_flds[fldno].name[0] = '\0';
				fstruct->picture_flds[fldno].upt_flag = 0;
				fstruct->picture_flds[fldno].show_flag = 1;
				secno = fstruct->n_section_fields-1;
				fstruct->picture_flds[fldno].section = -1;
				if ((secno>=0)&&(stricmp(fstruct->section_flds[secno].name, "ALL")==0))
					secno = -1;
				if (secno>=0)
				{
					fstruct->picture_flds[fldno].section = secno - sep_no;
				}
			}
			else if (i==20)
/*
......section
*/
			{
				fldno = fstruct->n_section_fields;
				if (fldno>0)
				{
					while (stricmp(fstruct->section_flds[fldno-1].name, "ALL")==0)
					{
						fstruct->n_section_fields--;
						fldno = fstruct->n_section_fields;
						if (fldno<=0) break;
					}
				}
				(fstruct->n_section_fields)++;
				if (fstruct->n_section_fields==1)
				{
					fstruct->section_flds = 
								(UD_SECTION *) uu_malloc
								(fstruct->n_section_fields*sizeof(UD_SECTION));
				}
				else
				{
					temp_section_flds =
								(UD_SECTION *) uu_malloc
								(fstruct->n_section_fields*sizeof(UD_SECTION));
					uu_move_byte(fstruct->section_flds, temp_section_flds,
								(fstruct->n_section_fields-1)*sizeof(UD_SECTION));
					fstruct->section_flds = temp_section_flds;
				}
				fstruct->section_flds[fldno].name[0] = '\0';
				strcpy(fstruct->section_flds[fldno].color, "DEFAULT");
				fstruct->section_flds[fldno].type = 0;
			}
/////////////////////////
			else if (i==21)
/*
......section bar
*/
			{
				fldno = fstruct->n_section_fields;
				(fstruct->n_section_fields)++;
				if (fstruct->n_section_fields==1)
				{
					fstruct->section_flds = 
								(UD_SECTION *) uu_malloc
								(fstruct->n_section_fields*sizeof(UD_SECTION));
				}
				else
				{
					temp_section_flds =
								(UD_SECTION *) uu_malloc
								(fstruct->n_section_fields*sizeof(UD_SECTION));
					uu_move_byte(fstruct->section_flds, temp_section_flds,
								(fstruct->n_section_fields-1)*sizeof(UD_SECTION));
					fstruct->section_flds = temp_section_flds;
				}
				fstruct->section_flds[fldno].name[0] = '\0';
				strcpy(fstruct->section_flds[fldno].color, "DEFAULT");
				fstruct->section_flds[fldno].type = 1;
				sep_no++;
			}
////////////////////////
			else if ((i!=0)&&(i!=9)&&(i!=25))
			{
				(fstruct->n_input_fields)++;
/*
.....For "display type form",
.....the last method callback is "CLOSE FORM" callback
.....so reserve last input field structure for "CLOSE FORM"
*/
				if (fstruct->n_input_fields==1)
				{
					fstruct->input_flds = 
						(UD_INPUT *) uu_malloc
							((fstruct->n_input_fields+fstruct->n_display_fields+1)*
							sizeof(UD_INPUT));
				}
				else
				{
					temp_input_flds =
						(UD_INPUT *) uu_malloc
							((fstruct->n_input_fields+fstruct->n_display_fields+1)*
							sizeof(UD_INPUT));
					uu_move_byte(fstruct->input_flds, temp_input_flds,
								(fstruct->n_input_fields-1)*sizeof(UD_INPUT));
					fstruct->input_flds = temp_input_flds;
				}
				fstruct->input_flds[fstruct->n_input_fields-1].ud_prmloc.x = -1;
				fstruct->input_flds[fstruct->n_input_fields-1].ud_prmloc.y = -1;
				fstruct->input_flds[fstruct->n_input_fields-1].font_scale = 1.0;
				fstruct->input_flds[fstruct->n_input_fields-1].justified = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].active_flag = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].shortcut[0] = '\0';
				fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				for (k=0;k<UD_NMENTWD;k++)
					fstruct->input_flds[fstruct->n_input_fields-1].ud_limit[k] = 0;

				strcpy(fstruct->input_flds[fstruct->n_input_fields-1].fcolor, "DEFAULT");
				strcpy(fstruct->input_flds[fstruct->n_input_fields-1].bcolor, "DEFAULT");
				strcpy(fstruct->input_flds[fstruct->n_input_fields-1].pfcolor, "DEFAULT");
				strcpy(fstruct->input_flds[fstruct->n_input_fields-1].pbcolor, "DEFAULT");
				fstruct->input_flds[fstruct->n_input_fields-1].picarea = NULL;
				fstruct->input_flds[fstruct->n_input_fields-1].n_picarea = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].section = -1;
				secno = fstruct->n_section_fields-1;
				if ((secno>=0)&&(stricmp(fstruct->section_flds[secno].name, "ALL")==0))
					secno = -1;
				if (secno>=0)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].section = secno - sep_no;
				}
				if (i==1)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 6;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==2)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 2;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
/*
.....ud_input not used for CHOICEBOX before, but for UNIX, we will used it for #COLOR#
.....if #COLOR#, ud_input = 1;
*/
#if UU_COMP!=UU_WIN2K
					fstruct->input_flds[fstruct->n_input_fields-1].ud_input 
								= 0;  
#endif
				}
				if (i==3)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 1;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_input 
								= FORM_STRING;  
					fstruct->input_flds[fstruct->n_input_fields-1].ud_modal = 0; 
				}
				if (i==5)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 5;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_input 
								= FORM_STRING;  
					fstruct->input_flds[fstruct->n_input_fields-1].ud_modal = 0; 
					fstruct->input_flds[fstruct->n_input_fields-1].ud_echo = 0;
				}
				if (i==6)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 3;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==7)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 5;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 1;
					fstruct->input_flds[fstruct->n_input_fields-1].defaults = NULL;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_echo = 0;
				}
/*
.....we will use COMBLIST_DROPDOWN
*/
				if (i==8)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 5;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 2;
					fstruct->input_flds[fstruct->n_input_fields-1].defaults = NULL;
				}
				if (i==10)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 7;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==11)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 8;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_input 
								= FORM_STRING;  
					fstruct->input_flds[fstruct->n_input_fields-1].ud_modal = 0; 
				}
				if (i==13)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 9;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==14)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 10;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==15)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 11;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==16)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 17;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_echo = 0;
				}
				if (i==17)
				{
/*
......for #color# field, use color button in Windows, but will use choicebox for UNIX
*/
#if UU_COMP == UU_WIN2K
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 18;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
#else
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 2;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 16;
					init_colordata(fstruct->n_input_fields-1, fstruct);
#endif
				}
				if (i==18)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 19;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
				}
				if (i==22)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 23;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].ud_input = FORM_PICK;
				}
				if (i==23)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 24;
					fstruct->input_flds[fstruct->n_input_fields-1].justified = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].range[0].dint = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].range[1].dint = 100;
				}
				if (i==24)
				{
					fstruct->input_flds[fstruct->n_input_fields-1].toggle = 25;
					fstruct->input_flds[fstruct->n_input_fields-1].n_defaults = 0;
					fstruct->input_flds[fstruct->n_input_fields-1].vfile = NULL;
				}
				fstruct->input_flds[fstruct->n_input_fields-1].ud_echo = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].ud_fprec = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].ud_flen = 0;
				fstruct->input_flds[fstruct->n_input_fields-1].range_flag = 0;
			}
			if (i==2)
				init_choicedata(fstruct->n_input_fields-1, fstruct);
			if (i==9)
			{
				help_start = 1;
				fstruct->helpflag = 1;
			}
			else
				help_start = 0;
			if (i==25)
			{
				fstruct->macroflag = 1;
				UD_macroflag = 1;
			}
			break;
		case 2:
			switch (isub)
			{
			case 1:
				status = ud_playout_frmheader (ctyp,cmsg,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 2:
				status = ud_playout_formitem (ctyp,cmsg,1,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 3:
				status = ud_playout_formitem (ctyp,cmsg,2,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 4:
				status = ud_playout_formitem (ctyp,cmsg,3,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 5:
				status = ud_playout_formitem (ctyp,cmsg,4,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 6:
				status = ud_playout_formitem (ctyp,cmsg,5,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 7:
				status = ud_playout_formitem (ctyp,cmsg, 7,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 8:
				status = ud_playout_formitem(ctyp,cmsg, 8,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 9:
				status = ud_playout_formitem (ctyp,cmsg, 9,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 11:
				status = ud_playout_formitem (ctyp,cmsg, 10,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 12:
				status = ud_playout_formitem (ctyp,cmsg, 11,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 13:
				status = ud_playout_formframe (ctyp,cmsg, fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 14:
				status = ud_playout_formitem (ctyp,cmsg, 13,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 15:
				status = ud_playout_formitem (ctyp,cmsg, 14,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 16:
				status = ud_playout_formitem (ctyp,cmsg, 15,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 17:
				status = ud_playout_formitem (ctyp,cmsg, 16,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 18:
				status = ud_playout_formitem (ctyp,cmsg, 17,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 19:
				status = ud_playout_formitem (ctyp,cmsg, 18,fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 20:
				status = ud_playout_formpicture (ctyp,cmsg, fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 21:
				status = ud_playout_formsection (ctyp,cmsg, fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 23:
				status = ud_playout_formitem (ctyp,cmsg, 23, fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 24:
				status = ud_playout_formitem (ctyp,cmsg, 24, fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			case 25:
				status = ud_playout_formitem (ctyp,cmsg, 25, fstruct);
				if (status==UU_FAILURE)
					goto done;
				break;
			}
			break;
		}
		ud_int_frm_read(formstr,0,'\n');
	}
done:;
	fldno = fstruct->n_section_fields;
	if (fldno>0)
	{
		while (stricmp(fstruct->section_flds[fldno-1].name, "ALL")==0)
		{
/*
.....don't save all section
*/
			fstruct->n_section_fields--;
			fldno = fstruct->n_section_fields;
			if (fldno<=0) break;
		}
	}
	return status;
}

/*********************************************************************
**    E_FUNCTION :  ud_int_frm_read(str,stat,key); -- read a string from 
**                                             the int form storage area.
**    PARAMETERS
**       INPUT  :   char *str -- string to read information into.
**                  int  stat =1 to start from the begining.
**                            =0 to continue from the current position.
**                  char key
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_int_frm_read(str,stat,key)
char *str;
int  stat;
char key;
{
static int counter = 0;
int i;

   for(i=0;i<128;i++)
      str[i] = 0;

   if(stat)
      counter = 0;
   
   i = 0;
   for(;*(&frm_ptr[0] + counter) != key &&
		    *(&frm_ptr[0] + counter) != '\n';counter++,i++)
      *(&str[0]+i) = *(&frm_ptr[0] + counter);

   *(&str[0]+i) = *(&frm_ptr[0] + counter++);
}
/*********************************************************************
**    E_FUNCTION : ud_free_flist(formlist) 
**    PARAMETERS   
**                                              UD_LIST *formlist: structure for forms that need be
**                                                              be released
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added by Yurong for dealloc memory for form list
.....10/1/97
*/
ud_free_flist(formlist)
UD_LIST *formlist;
{
	int i;

   for (i=0; i<formlist->num_item; i++)
	{
      uu_free(formlist->item[i]);
	}
	if (formlist->item!=NULL)
		uu_free(formlist->item);
	if (formlist->answer!=NULL)
		uu_free(formlist->answer);
/*
.....Reset structure
*/
	formlist->num_item = 0;
	formlist->item = NULL;
	formlist->answer = NULL;
	return (0);
}

/*********************************************************************
**    E_FUNCTION : ud_save_actform_id()
**			save the active id
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**             
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_save_actform_id()
{
#if UU_COMP!=UU_WIN2K
	uw_mfsave_actform_id();
#else
	uw_ntsave_actform_id();
#endif
}
/*********************************************************************
**    E_FUNCTION : ud_reset_actform_id()
**			reset the active id
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**             
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_reset_actform_id()
{
#if UU_COMP!=UU_WIN2K
	uw_mfreset_actform_id();
#endif
}

/*********************************************************************
**    E_FUNCTION : ud_free_tlist(formlist) 
**    PARAMETERS   
**             UD_TLIST *formlist: structure for forms that need be
**                                be released
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_free_tlist(formlist)
UD_TLIST *formlist;
{
	int i,j;

	if (formlist->col_label!=NULL)
	{
		for (i=0; i<formlist->num_col; i++)
		{
			if (formlist->col_label[i]!=NULL)
				uu_free (formlist->col_label[i]);
			formlist->col_label[i] = NULL;
		}
	}
	if ((formlist->num_col)&&(formlist->col_label!=NULL))
	{
		uu_free (formlist->col_label);
		formlist->col_label = NULL;
	}

	for (i=0; i<formlist->num_item; i++)
	{
		for (j=0; j<formlist->data[i].itemnum; j++)
		{
			if (formlist->data[i].data_items[j]!=NULL)
				uu_free(formlist->data[i].data_items[j]);
			formlist->data[i].data_items[j] = NULL;
		}
		if (formlist->data[i].itemnum>0)
		{
			if (formlist->data[i].data_items!=NULL)
				uu_free(formlist->data[i].data_items);
			formlist->data[i].data_items = NULL;
		}
	}
	if ((formlist->num_item)&&(formlist->data!=NULL))
		uu_free(formlist->data);
	formlist->data = NULL;
/*
.....Reset structure
*/
	formlist->num_item = 0;
	return (0);
}
/*********************************************************************
**    E_FUNCTION : ud_tlist_copy(list1, list2)
**				copy the UD_TLIST strcuture
**    PARAMETERS   
**       INPUT  :  list1: list to be copied
**					
**       OUTPUT :  list2: list copied
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_tlist_copy(list1, list2)
UD_TLIST *list1, *list2;
{
	int i, j, len;
/*
.....even if there is no data item, we still need copy column info
*/
/*	if (list1->num_item==0)
	{
		list2->num_item = 0;
		return;
	}
*/
	list2->num_item = list1->num_item;
	list2->num_col = list1->num_col;
	list2->answer = list1->answer;
	if (list2->num_col>0)
		list2->col_label = (char**) uu_malloc(list2->num_col*sizeof (char*));
	for (i=0; i<list2->num_col;i++)
	{
		len = strlen (list1->col_label[i]);
		list2->col_label[i] = (char*) uu_malloc((len+1)*sizeof(char));
		strcpy(list2->col_label[i], list1->col_label[i]);
	}
	if (list2->num_item>0)
		list2->data = (UD_ITEMDATA *) uu_malloc(list2->num_item*sizeof(UD_ITEMDATA));
	for (i=0; i<list2->num_item;i++)
	{
		(list2->data[i]).itemnum = (list1->data[i]).itemnum;
		(list2->data[i]).data_items = 
					(char **) uu_malloc(list2->num_col*sizeof(char*));
		for (j=0; j<(list2->data[i]).itemnum; j++)
		{
			len = strlen(list1->data[i].data_items[j]);
			list2->data[i].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(list2->data[i].data_items[j], list1->data[i].data_items[j]);
		}
	}
}
/*********************************************************************
**    E_FUNCTION : ud_tlist_copy_idata(data1, data2)
**				copy the UD_TLIST item UD_ITEMDATA strcuture
**    PARAMETERS   
**       INPUT  :  data1: data to be copied
**					
**       OUTPUT :  data2: data copied
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_tlist_copy_idata(data1, data2)
UD_ITEMDATA *data1, *data2;
{
	int j, len;

	data2->itemnum = data1->itemnum;
	data2->frmid = data1->frmid;
	data2->fldno = data1->fldno;
	data2->flag = data1->flag;
	if ((data1->data_items!=NULL)&&(data1->itemnum>0))
		data2->data_items = (char**)uu_malloc(data1->itemnum*sizeof(char*));
	for (j=0; j<data1->itemnum; j++)
	{
		len = strlen(data1->data_items[j]);
		data2->data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
		strcpy(data2->data_items[j], data1->data_items[j]);
	}
}
/*********************************************************************
**    E_FUNCTION : ud_tlist_free_idata(data)
**				free UD_ITEMDATA strcuture
**    PARAMETERS   
**       INPUT  :  data: data to be freed
**					
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_tlist_free_idata(data)
UD_ITEMDATA *data;
{
	int j;

	if (data->data_items==NULL)
		return;
	for (j=0; j<data->itemnum; j++)
	{
		if (data->data_items[j]!=NULL)
		{
			uu_free(data->data_items[j]);
			data->data_items[j] = NULL;
		}
	}
	if ((data->itemnum>0)&&(data->data_items!=NULL))
		uu_free(data->data_items);
	data->data_items = NULL;
}

int ud_issame_idata(UD_ITEMDATA *data1, UD_ITEMDATA *data2)
{
	int j, len;

	if (data2->itemnum != data1->itemnum)
		return 0;
	if (data2->frmid != data1->frmid)
		return 0;
	if (data2->fldno != data1->fldno)
		return 0;
	if (data2->flag != data1->flag)
		return 0;
	if (data1->itemnum==0)
		return 0;
	for (j=0; j<data1->itemnum; j++)
	{
		if (stricmp(data2->data_items[j], data1->data_items[j])!=0)
			return 0;
	}
	return 1;
}
/*********************************************************************
**    E_FUNCTION : ud_read_posform(frmname, x, y, cx, cy, att_win, ref)
**				read the form position info from form.pos file
**    PARAMETERS   
**       INPUT  :  frmname: form name
**					
**       OUTPUT :  
**					x,y,cx,cy: form position and size
**					att_win: attach window
**					ref:  reference corner
**    RETURNS      : -1: read failed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ud_read_posform(frmname, x, y, cx, cy, att_win, ref)
char *frmname;
int *x, *y, *cx, *cy, *att_win, *ref;
{
	int status, temp;
	char *p;
	UX_pathname filename;
	FILE *fd;
	char formstr[256];

	int i,k,inum,ityp;
	char msg[256], *tok;
	char ctyp[256],cmsg[256];
	int maxsub=4;
	static char csub[4][20] = {"POSITION","SIZE","REFERENCE","ATTACH"};
	UU_REAL rval[2];

	if (strlen(frmname)==0)
		return -1;
	strcpy(filename, frmname);
	p = (char *)rindex(filename,'.');
	if (p!=0) 
	{
		*p = 0;
	}
	strcat(filename, ".pos");
	status = ul_open_mod_file("UU_USER_SETTINGS","forms",UU_NULL,
			UU_NULL, filename, 2, &fd);
	if (status != UU_SUCCESS || fd == NULL) 
	{
		return -1;
	}	
	while (1)
	{                                                
		status = ul_fread(fd, formstr, sizeof(formstr), &temp);
		if (status == UX_EOF)
		{
			status = UU_SUCCESS;
			goto done;
		}
		else if (status != UU_SUCCESS && status != UX_NO_SPACE)
		{
			ud_wrerr("Error reading from Form.pos file.");
			status = -1;
			goto done;
		}
		status = ul_modal_check (formstr,&ityp, ctyp, cmsg);
		if ((status != 0)||(ityp!=2))
		{
			sprintf (msg, "Form file syntax error. %s\n",formstr);
			ud_wrerr(msg);
			status = -1;
			goto done;
		}
		for (i=0;i<maxsub;i++)
		{
			ul_to_upper(ctyp);
			if (strcmp(ctyp,csub[i]) == 0) break;
		}
		if (i >= maxsub)
		{
			return -1;
		}
		switch(i)
		{
/*
.....Position
*/
		case 0:
			if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
				inum != 2)
			{
				*x = *y = 0;
			}
			else
			{
				*x = (int)rval[0];
				*y = (int)rval[1];
			}
			break;
/*
.....Size
*/
		case 1:
			if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
				inum != 2)
			{
				sprintf (msg, "Form file syntax error. %s\n", formstr);
				ud_wrerr(msg);
				status = -1;
				goto done;
			}
			*cx = (int)rval[0] ;
			*cy = (int)rval[1] ;
			break;
/*
.....reference
*/
		case 2:
			for (k=0; k<9; k++) 
				if (strcmp(cmsg,&reftyp[k][0])==0) break;
			if (k<9) 
				*ref = k;
			break;
/*
.....attach
*/
		case 3:
			if (strcmp(cmsg,"SCREEN")==0)
				*att_win = 1;
			else
				*att_win = 0;
			break;
		}		
	}
done:;
	ux_fclose0 (fd);
	return status;
}

/*********************************************************************
**    E_FUNCTION : ud_save_frmpos(frmname,x,y,fcx,fcy,rect, att_win,ref_flag,
**					xborder, yborder)
**				Save the form position info into form.pos file
**    PARAMETERS   
**       INPUT  :  frmname: form name
**					x,y,fcx,fcy: form position and size
**					att_win: attach window
**					ref_flag:  reference corner
**					xborder, yborder: adjust border size
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_save_frmpos(frmname,x,y,fcx,fcy,rect, att_win,ref_flag, 
			xborder, yborder)
char *frmname;
int x,y,fcx,fcy,att_win,ref_flag,xborder, yborder;
int *rect;
{
	int status;
	char *p;
	UX_pathname filename;
	FILE *fd;
	char tmpstr[80];
	int fpos[2], ref[2],cx,cy;

	if (UW_Store_forms==0)
		return;
	if (strlen(frmname)==0)
		return;
	strcpy(filename, frmname);
	p = (char *)rindex(filename,'.');
	if (p!=0) 
	{
		*p = 0;
	}
	strcat(filename, ".pos");
	status = ul_open_mod_file("UU_USER_SETTINGS","forms",UU_NULL,
			UU_NULL, filename, 3, &fd);
	if (status != UU_SUCCESS || fd == NULL) 
	{
		return;
	}
	cx = rect[2];
	cy = rect[3];
	if (att_win==1)
/*
......attached to SCREEN
*/
	{
		if (ref_flag==0)
/*
......LOWER_LEFT
*/
		{
/*
......screen's lower-left
*/
			ref[0]= 0;
			ref[1] = cy;
/*
......form's relative position (to LOWER_LEFT)
*/
			fpos[0] = x - ref[0];
			fpos[1] = ref[1] - fcy - y - yborder;
		}
		else if (ref_flag==1)
/*
......LOWER_CENTER
*/
		{
/*
......screen's lower-center
*/
			ref[0]= 0.5*cx;
			ref[1] = cy;
/*
......form's relative position (to LOWER_CENTER)
*/
			fpos[0] = x - ref[0] + 0.5*fcx;
			fpos[1] = ref[1] - fcy - y - yborder;
		}
		else if (ref_flag==2)
/*
......LOWER_RIGHT
*/
		{
/*
......screen's lower-right
*/
			ref[0]= cx;
			ref[1] = cy;
/*
......form's relative position (to LOWER_RIGHT)
*/
			fpos[0] = x - ref[0] + fcx + xborder;
			fpos[1] = ref[1] - fcy - y - yborder;
		}
		else if (ref_flag==3)
/*
......CENTER_LEFT
*/
		{
/*
......screen's center-left
*/
			ref[0]= 0;
			ref[1] = 0.5*cy;
/*
......form's relative position (to CENTER_LEFT)
*/
			fpos[0] = x - ref[0];
			fpos[1] = ref[1] - 0.5*fcy - y;
		}
		else if (ref_flag==4)
/*
......CENTER_CENTER
*/
		{
/*
......screen's center-center
*/
			ref[0]= 0.5*cx;
			ref[1] = 0.5*cy;
/*
......form's relative position (to CENTER_CENTER)
*/
			fpos[0] = x - ref[0] + 0.5*fcx;
			fpos[1] = ref[1] - 0.5*fcy - y;
		}
		else if (ref_flag==5)
/*
......CENTER_RIGHT
*/
		{
/*
......screen's center-right
*/
			ref[0]= cx;
			ref[1] = 0.5*cy;
/*
......form's relative position (to CENTER_RIGHT)
*/
			fpos[0] = x - ref[0] + fcx + xborder;
			fpos[1] = ref[1] - 0.5*fcy - y;
		}
		else if (ref_flag==6)
/*
......UPPER_LEFT
*/
		{
/*
......form's relative position (to UPPER_LEFT)
*/
			fpos[0] = x;
			fpos[1] = -y;
		}
		else if (ref_flag==7)
/*
......UPPER_CENTER
*/
		{
/*
......screen's upper-center
*/
			ref[0]= 0.5*cx;
			ref[1] = 0;
/*
......form's relative position (to UPPER_CENTER)
*/
			fpos[0] = x - ref[0] + 0.5*fcx;
			fpos[1] = ref[1] - y;
		}
		else if (ref_flag==8)
/*
......UPPER_RIGHT
*/
		{
/*
......screen's upper-right
*/
			ref[0]= cx;
			ref[1] = 0;
/*
......form's relative position (to UPPER_RIGHT)
*/
			fpos[0] = x - ref[0] + fcx + xborder;
			fpos[1] = ref[1] - y;
		}
	}
	else
	{
/*
......Get NCL window size
*/
		if (ref_flag==0)
/*
......LOWER_LEFT
*/
		{
/*
......window's lower-left
*/
			ref[0]= rect[0];
			ref[1] = rect[1] + rect[3];
/*
......form's relative position (to LOWER_LEFT)
*/
			fpos[0] = x - ref[0];
			fpos[1] = ref[1] - fcy - y - yborder;
		}
		else if (ref_flag==1)
/*
......LOWER_CENTER
*/
		{
/*
......window's lower-center
*/
			ref[0]= rect[0] + 0.5*cx ;
			ref[1] = rect[1] + rect[3];
/*
......form's relative position (to LOWER_CENTER)
*/
			fpos[0] = x - ref[0] + 0.5*fcx;
			fpos[1] = ref[1] - fcy - y - yborder;
		}
		else if (ref_flag==2)
/*
......LOWER_RIGHT
*/
		{
/*
......window's lower-right
*/
			ref[0]= rect[0] + rect[2];
			ref[1] = rect[1] + rect[3];
/*
......form's relative position (to LOWER_RIGHT)
*/
			fpos[0] = x - ref[0] + fcx + xborder;
			fpos[1] = ref[1] - fcy - y - yborder;
		}
		else if (ref_flag==3)
/*
......CENTER_LEFT
*/
		{
/*
......window's center-left
*/
			ref[0]= rect[0];
			ref[1] = rect[1] + 0.5*cy;
/*
......form's relative position (to CENTER_LEFT)
*/
			fpos[0] = x - ref[0];
			fpos[1] = ref[1] - 0.5*fcy - y;
		}
		else if (ref_flag==4)
/*
......CENTER_CENTER
*/
		{
/*
......window's center-center
*/
			ref[0]= rect[0] + 0.5*cx;
			ref[1] = rect[1] + 0.5*cy;
/*
......form's relative position (to CENTER_CENTER)
*/
			fpos[0] = x - ref[0] + 0.5*fcx;
			fpos[1] = ref[1] - 0.5*fcy - y;
		}
		else if (ref_flag==5)
/*
......CENTER_RIGHT
*/
		{
/*
......window's center-right
*/
			ref[0]= rect[0] + rect[2];
			ref[1] = rect[1] + 0.5*cy;
/*
......form's relative position (to CENTER_RIGHT)
*/
			fpos[0] = x - ref[0] + fcx + xborder;
			fpos[1] = ref[1] - 0.5*fcy - y;
		}
		else if (ref_flag==6)
/*
......UPPER_LEFT
*/
		{
/*
......window's upper-left
*/
			ref[0]= rect[0];
			ref[1] = rect[1];
/*
......form's relative position (to UPPER_LEFT)
*/
			fpos[0] = x - ref[0];
			fpos[1] = ref[1] - y;
		}
		else if (ref_flag==7)
/*
......UPPER_CENTER
*/
		{
/*
......window's upper-center
*/
			ref[0]= rect[0] + 0.5*cx;
			ref[1] = rect[1];
/*
......form's relative position (to UPPER_CENTER)
*/
			fpos[0] = x - ref[0] + 0.5*fcx;
			fpos[1] = ref[1] - y;
		}
		else if (ref_flag==8)
/*
......UPPER_RIGHT
*/
		{
/*
......window's upper-right
*/
			ref[0]= rect[0] + rect[2];
			ref[1] = rect[1];
/*
......form's relative position (to UPPER_RIGHT)
*/
			fpos[0] = x - ref[0] + fcx + xborder;
			fpos[1] = ref[1] - y;
		}
	}
	sprintf(tmpstr, "/POSITION/ %d,%d\n", fpos[0], fpos[1]);
	ux_fputs0(tmpstr, fd);
	sprintf(tmpstr, "/SIZE/ %d,%d\n", fcx, fcy);
	ux_fputs0(tmpstr, fd);
	if (att_win==0)
		ux_fputs0("/ATTACH/ WINDOW\n", fd);
	else
		ux_fputs0("/ATTACH/ SCREEN\n", fd);
	if (ref_flag<9)
	{
		sprintf(tmpstr, "/REFERENCE/ %s\n", reftyp[ref_flag]);
	}
	else
		strcpy(tmpstr, "/REFERENCE/ CENTER_CENTER\n");
	ux_fputs0(tmpstr, fd);
	ux_fclose0 (fd);
}


/*********************************************************************
**    E_FUNCTION : ud_free_dlist(formlist) 
**    PARAMETERS   
**             UD_TLIST *formlist: structure for forms that need be
**                                be released
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_free_dlist(formlist)
UD_DLIST *formlist;
{
	int i,j;

	if (formlist->col_label!=NULL)
	{
		for (i=0; i<formlist->num_col; i++)
		{
			if (formlist->col_label[i]!=NULL)
				uu_free (formlist->col_label[i]);
			formlist->col_label[i] = NULL;
		}
	}
	if ((formlist->num_col)&&(formlist->col_label!=NULL))
	{
		uu_free (formlist->col_label);
		formlist->col_label = NULL;
	}

	for (i=0; i<formlist->num_item; i++)
	{
		for (j=0; j<formlist->data[i].itemnum; j++)
		{
			if (formlist->data[i].data_items[j]!=NULL)
				uu_free(formlist->data[i].data_items[j]);
			formlist->data[i].data_items[j] = NULL;
		}
		if (formlist->data[i].itemnum>0)
		{
			if (formlist->data[i].data_items!=NULL)
				uu_free(formlist->data[i].data_items);
			formlist->data[i].data_items = NULL;
		}
	}
	if ((formlist->num_item)&&(formlist->data!=NULL))
		uu_free(formlist->data);
	formlist->data = NULL;
/*
.....Reset structure
*/
	formlist->num_item = 0;
	return (0);
}

/*********************************************************************
**    E_FUNCTION : ud_dlist_sort(list)
**				sort the UD_DLIST in alphabetical order
**    PARAMETERS   
**       INPUT  :  list: list to be sort
**       OUTPUT :  list: list sorted
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dlist_sort(list)
UD_DLIST list;
{
	int i,j,nc, add;
	int list_count;
	UU_LIST itemlist;
	UD_ITEMDATA item,*items;
	char tempstr1[80], tempstr2[80];

	list_count = 0;
	uu_list_init(&itemlist, sizeof(UD_ITEMDATA), list.num_item, 100);
/*
.....Store sorted data list entries.
*/
	for (i=0; i<list.num_item;i++)
	{
/*
.....Copy UD_ITEMDATA data.
*/
		item.itemnum = list.data[i].itemnum;
		item.frmid = list.data[i].frmid;
		item.fldno = list.data[i].fldno;
		item.flag = list.data[i].flag;
		item.data_items = (char **) uu_malloc(list.num_col*sizeof(char*));
		for (j=0; j<list.data[i].itemnum; j++)
		{
			nc = strlen(list.data[i].data_items[j]);
			item.data_items[j] = (char*)uu_malloc((nc+1)*sizeof(char));
			strcpy(item.data_items[j], list.data[i].data_items[j]);
		}
/*
.....Place in proper index in list.
*/
		if (i==0)
		{
			uu_list_push(&itemlist, &item);
			list_count++;
		}
		else
		{
			items = (UD_ITEMDATA *) UU_LIST_ARRAY (&itemlist);
			add = 0;
			for (j=list_count-1; j>=0; j--)
			{
				nc = strlen(item.data_items[0]);
				if (nc>(int)strlen(items[j].data_items[0]))
					nc =  strlen(items[j].data_items[0]);
				strcpy(tempstr1, item.data_items[0]);
				ul_to_upper(tempstr1);
				strcpy(tempstr2, items[j].data_items[0]);
				ul_to_upper(tempstr2);
				if (ncl_label_cmp(tempstr1, tempstr2)>0)
				{
					uu_list_insert(&itemlist, j+1, &item);
					list_count++;
					add = 1;
					break;
				}
			}
			if (add==0)
			{
				uu_list_insert(&itemlist, 0, &item);
				list_count++;
			}
		}
	}
/*
.....The following assumes the number of columns is constant.
*/
	items = (UD_ITEMDATA *)UU_LIST_ARRAY(&itemlist);
	for (i=0; i<list.num_item;i++)
	{
/*
.....Free the data_item memory, but keep the data list memory.
*/
		if (list.data[i].itemnum > 0 && list.data[i].data_items != UU_NULL)
		{
			for (j=0;j<list.data[i].itemnum;j++)
			{
				if (list.data[i].data_items[j] != NULL)
					uu_free(list.data[i].data_items[j]);
			}
		}
/*
.....Build copy of data list entries.
*/
		list.data[i].itemnum = items[i].itemnum;
		list.data[i].frmid = items[i].frmid;
		list.data[i].fldno = items[i].fldno;
		list.data[i].flag = items[i].flag;
		for (j=0;j<items[i].itemnum;j++)
		{
			nc = strlen(items[i].data_items[j]);
			list.data[i].data_items[j] = (char*)uu_malloc((nc+1)*sizeof(char));
			strcpy(list.data[i].data_items[j], items[i].data_items[j]);
		}
	}
	uu_list_free(&itemlist);
}

/*********************************************************************
**    E_FUNCTION : ud_dlist_copy(list1, list2)
**				copy the UD_DLIST strcuture
**    PARAMETERS   
**       INPUT  :  list1: list to be copied
**					
**       OUTPUT :  list2: list copied
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dlist_copy(list1, list2)
UD_DLIST *list1, *list2;
{
	int i, j, len;

	list2->num_item = list1->num_item;
	list2->num_col = list1->num_col;
	list2->answer[0] = list1->answer[0];
	list2->answer[1] = list1->answer[1];
	if (list2->num_col>0)
		list2->col_label = (char**) uu_malloc(list2->num_col*sizeof (char*));
	for (i=0; i<list2->num_col;i++)
	{
		len = strlen (list1->col_label[i]);
		list2->col_label[i] = (char*) uu_malloc((len+1)*sizeof(char));
		strcpy(list2->col_label[i], list1->col_label[i]);
	}
	if (list2->num_item>0)
		list2->data = (UD_ITEMDATA *) uu_malloc(list2->num_item*sizeof(UD_ITEMDATA));
	for (i=0; i<list2->num_item;i++)
	{
		(list2->data[i]).itemnum = (list1->data[i]).itemnum;
		(list2->data[i]).data_items = 
					(char **) uu_malloc(list2->num_col*sizeof(char*));
		for (j=0; j<(list2->data[i]).itemnum; j++)
		{
			len = strlen(list1->data[i].data_items[j]);
			list2->data[i].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(list2->data[i].data_items[j], list1->data[i].data_items[j]);
		}
	}
}

void ud_free_itemdata(UD_ITEMDATA *data)
{
	int i;
	if (data->itemnum<=0)
		return;
	if (data->data_items==NULL)
		return;
	for (i=0; i<data->itemnum;i++)
	{
		if (data->data_items[i] != NULL)
			uu_free(data->data_items[i]);
	}
	uu_free(data->data_items);
}

