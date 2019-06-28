/*********************************************************************
**      FILENAME: lmodals.c
**      CONTAINS:
**          ul_load_nis_mod
**          ul_load_modfile
**          ul_modals_include
**          ul_load_clrfile
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       lmodals.c , 25.6
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 12:02:31
*********************************************************************/

#include <math.h>
#include "lcom.h"
#include "usysdef.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "ustdio.h"
#include "mconst.h"
#include "mdattr.h"
#include "mdcpln.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclstack.h"
#include "spmouse.h"
#include "nclcmd.h"
#include "nclmplay.h"
#include "mattr.h"
#include "mfort.h"
#include "mrenddl.h"
#include "mdrel.h"
#include "driver.h"
#include "nclmodals.h"
#if UU_COMP==UU_WIN2K
#include "wsgl.h"
#endif
#include "dselect.h"
#include "class.h"
#include "ddef.h"
#include "dmotif.h"

int NCL_accuracy;
UU_LOGICAL NCL_batch_run=UU_FALSE;

extern UU_LOGICAL UN_motmodal_cmd;
extern int UN_motion_color,UN_motion_line,UN_rapid_color,UN_rapid_line;
extern int UN_motion_pen,UN_rapid_pen;
extern int UR_restore_lights;
extern int UR_restore_mtrl;
extern int UR_restore_clr;
extern UU_LOGICAL UR_restore_units;
extern int UM_light_keys[];
extern UU_REAL NCL_pick_aper;
extern UU_LOGICAL NCL_pick_verify;
extern int uw_glhicolor,uw_glvrfcolor;
extern int NCL_mark_method;
extern int UV_dyndisply, UV_dyncenter, UV_dyn_zval, UV_dynstatln, UV_dynsegs, 
			LW_dyncenter, LW_dyn_zval, UV_dynwheel;
extern char NCL_init_fstr[20];
extern double UN_motion_width;

static void S_init_lgtmod(),S_update_lgtmod();

static char chaindir[4][64] = {"*UV","*PREVIOUS","*PERPTO","*EDGE"};
static char yesno[3][64] = {"*NO","*YES","*PARTIAL"};
static char yesno2[3][64] = {"*NO","*YES","*MERGE"};
static char pri[10][64] = {"*A","*B","*C","*D","*E","*F","*G","*H","*I","*J"};
static char ccp[9][64] = {"*1038","*1040","*1042","*1043","*1044","*1051",
	"*1073","*1075","*1077"};
static char hpp[10][64] = {"*7220","*7470","*7475","*7580","*7585","*DMP_51",
	"*DMP_52","*DMP_56","*ALPHA_II","*OCI_928"};
static char tkp[7][64] = {"*4014","*4105","*4107","*4109","*4111","*4115",
	"*4211"};
static char posit[2][64] = {"*INCR","*ABS"};
static char fpp[2][64] = {"*LZS","*TZS"};
static char nclunits[2][64] = {"*INCH","*METRIC"};
static char dnc_mode[4][64] = {"*SEND","*RECEIVE","*VERIFY","*EMULATE"};
static char igonoff[3][64] = {"*IGNORE","*ON","*OFF"};
static char onoff[2][64] = {"*OFF","*ON"};
static char beginend[2][64] = {"*BEGIN","*END"};
static char keypad_fmt[2][64] = {"*NUMERIC","*FUNCTION"};
static char word_case[3][64] = {"*SAME","*UPPER","*LOWER"};
static char menu_fmt[3][64] = {"*ICON","*TEXT", "*BOTH"};
static char icon_size[5][64] = {"*16","*24", "*32","*40", "*48"};
static char com_size[10][64] = {"*8","*10","*12","*14","*16","*18","*20","*24",
	"*36","*48"};
static char scolor[2][96] = {"Default", "Auto"};

static char direction[3][64] = {"*DIRECT","*POINT", "*SPOT"};
static char space_char[2][64] = {"*MODEL", "*SCREEN"};
static char pikmark[4][64] = {"*HILITE", "*DIAMOND", "*NONE", "*DYNAMIC"};
static char offonsame[3][64] = {"*OFF", "*ON", "*SAME"};
static char chaincond[2][64] = {"*NORMAL","*CONDTN"};
static char inex[2][64] = {"*EXCLUDE","*INCLUDE"};
static char dpart[3][64] = {"*PART","*AXIS", "*BOTH"};
static char vrot[3][64] = {"*VIEWPORT","*USER", "*AUTO"};
static char browsop[2][64] = {"*LOCAL","*SAVED"};
static char zcen[3][64] = {"*PICK","*CALC", "*OFF"};
static char type[2][64] = {"*TIME","*CHANGES"};
static char uptmod[2][64] = {"*CHANGED","*IDLE"};

static int unicolor[5];
static int uniblank[5];
static int blank[5];
static struct UM_light_rec light_mod[5];
static struct UM_attrdata_rec light_attr[5];
static struct UM_light_rec light_uni[5];
static struct UM_mtrlmdl_rec UM_mtrlmdl_uni;
static int S_cus_color = -1;
static char S_cus_color_name[48][96];
extern char uw_color_name_sav[64][96];
extern char uw_color_name[64][96];
extern int uw_color_table[64][3];
extern int uw_color_table_sav[64][3];
extern int NCL_pik_hierarchy;
extern UU_LOGICAL UR_ason;
extern long	UR_last_autosave;
extern int	UR_switch;
extern int	UR_time;
extern int	UR_num_chg;
extern UU_LOGICAL NCL_ason;
extern long NCL_last_autosave;
extern int NCL_switch;
extern int NCL_time;
extern int NCL_num_chg;
extern int NCL_max_saves;
/*********************************************************************
**       E_FUNCTION : ul_load_nis_mod()
**                      This function loads the umbrella default modals
**                      from a disk file.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/

ul_load_nis_mod()
{
	char serror[80];
	int i,status,stat,opend;
	UX_pathname fname;
	FILE *fptr;
/*
.....Assume success
*/
	status = UU_SUCCESS;
	opend = 0;
/*
.....Initialize light modals
*/
	S_init_lgtmod();
/*
.....Check for modals file
*/
	fname[0] = '\0';
	stat = ul_open_mod_file (UU_NULL, UU_NULL, "UL_NIS_MODALS", UU_NULL,
		fname, 2, &fptr);
	if (stat!=UU_SUCCESS || fptr==UU_NULL)
	{
		sprintf (serror,"Cannot open Modals file %s.", fname);
		ud_wrerr (serror);
		goto failed;
	}
	opend = 1;
	status = ul_load_modfile(fptr);
	if (status == UU_SUCCESS)
		goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	S_update_lgtmod();
	if (opend == 1) ux_fclose0 (fptr);
	return (status);
}

/*********************************************************************
**       E_FUNCTION : ul_load_modfile(fptr)
**           This function loads the umbrella default modals
**                      from a disk file.
**       PARAMETERS     
**               INPUT  :  fptr: modal file to be load.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int ul_load_modfile(fptr)
FILE *fptr;
{
	char serror[80],buf[80],ctyp[40],cmsg[40];
	int status,stat,numint,ityp,i,isub,istat;
	int maxsub=33;
	static char csub[33][16]={"SIGNON","DISPLAY","PLOT","PUNCH","DNC",
		"NCL_QUE","NCL","IPV","UNIBASE","SOURCE","INTERFACE", "POCKET",
		"MEASUREMENT", "MOTION", "VIEW", "INCLUDE",
		"DISPLAY_BUFFER", "MATERIAL", "LIGHTS", "PICKING",
		"CMD_LINE","CHAIN","LABELS","PLAY_FEEDS","PLAY_INTERP", "MACROS" ,
		"GEO_COLORS", "BACKGROUND", "SRFATTR", "COLOR", "MODALS_FILES",
		"AUTOSAVE", "PREVIEW_MOTION"};
/*
.....Assume success
*/
	status = UU_SUCCESS;
	S_cus_color = -1;
	for (i=0; i<48;i++)
	{
		S_cus_color_name[i][0] = '\0';
	}
/*
.....Read a record
*/
	do
	{
		stat = ul_fread (fptr,buf,sizeof(buf),&numint);
		if (stat == UX_EOF)
		{
			goto file_done;
		}
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			sprintf (serror,"Error reading from mod file E%d.",stat);
			ud_wrerr (serror);
			goto failed;
		}
/*
.....Check for record type
*/
		istat = ul_modal_check (buf,&ityp,ctyp,cmsg);
/*
.....Invalid syntax
*/
		if (istat != UU_SUCCESS)
		{
			sprintf (serror,"Modals file syntax error. %s",buf);
			ud_wrerr (serror);
		}
/*
.....Subsystem type
*/
		switch (ityp)
		{
		case 1:
			for (i=0;i<maxsub;i++)
			{
				ul_to_upper(ctyp);
				if (strcmp(ctyp,csub[i]) == 0) break;
			}
			if (i >= maxsub)
			{
				sprintf (serror,"Not a valid Modals parameter. %s",
					buf);
				ud_wrerr (serror);
				break;
			}
			isub = i + 1;
			break;
		case 2:
			switch (isub)
			{
			case 0:
				ud_wrerr (serror,"A main Modals form is not in effect.");
				break;
			case 1:
				S_modals_signon (ctyp,cmsg);
				break;
			case 2:
				S_modals_display (ctyp,cmsg);
				break;
			case 3:
				S_modals_plot (ctyp,cmsg);
				break;
			case 4:
				S_modals_punch (ctyp,cmsg);
				break;
			case 5:
				S_modals_dnc (ctyp,cmsg);
				break;
			case 6:
				S_modals_que (ctyp,cmsg);
				break;
			case 7:
				S_modals_ncl (ctyp,cmsg);
				break;
			case 8:
				S_modals_ipv (ctyp,cmsg);
				break;
			case 9:
				S_modals_unibase (ctyp,cmsg);
				break;
			case 10:
				S_modals_source (ctyp,cmsg);
				break;
			case 11:
				ul_modals_interface (ctyp,cmsg);
				break;
			case 12:
				S_modals_pocket (ctyp,cmsg);
				break;
			case 13:
				S_modals_measurement (ctyp,cmsg);
				break;
			case 14:
				S_modals_motion (ctyp,cmsg);
				break;
			case 15:
				ul_modals_view (ctyp,cmsg);
				break;
			case 16:
				ul_modals_include (ctyp,cmsg,0);
				break;
			case 17:
				S_modals_buffer (ctyp,cmsg);
				break;
			case 18:
				S_modals_material (ctyp,cmsg);
				break;
			case 19:
				S_modals_lights (ctyp,cmsg);
				break;
			case 20:
				S_modals_pick (ctyp,cmsg);
				break;
			case 21:
				S_modals_cmdline (ctyp,cmsg);
				break;
			case 22:
   			S_modals_chain (ctyp,cmsg);
   			break;
			case 23:
   			S_modals_labels(ctyp,cmsg);
   			break;
			case 24:
   			ul_modals_pfeeds(ctyp,cmsg);
   			break;
			case 25:
   			ul_modals_pinterp(ctyp,cmsg);
   			break;
			case 26:
   			ul_modals_macros(ctyp,cmsg);
   			break;
			case 27:
   			S_modals_colors(ctyp,cmsg);
   			break;
			case 28:
   			ul_modals_background(ctyp,cmsg);
   			break;
			case 29:
   			S_modals_srfattr(ctyp,cmsg);
   			break;
			case 30:
   			S_modals_custom_color(ctyp,cmsg);
   			break;
			case 31:
   			S_modals_files(ctyp,cmsg);
   			break;
			case 32:
   			S_modals_autosave(ctyp,cmsg);
   			break;
			case 33:
   			S_modals_preview(ctyp,cmsg);
   			break;
			}
		}
	}
	while (stat == UU_SUCCESS || stat == UX_NO_SPACE);
file_done:;

	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	if (S_cus_color!=-1)
		ncl_update_colors(0);
	return (status);
}

/*********************************************************************
**       I_FUNCTION : ul_modals_include(ctyp,cmsg,which)
**                      This function sets the 'include' modals.
**       PARAMETERS
**               INPUT  :  ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**                         which = 0 - Called when loading umbrella modals.
**                                 1 - Called when loading NCLIPV modals.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int ul_modals_include (ctyp,cmsg,which)
char *ctyp,*cmsg;
int which;
{
	UX_pathname incfile;
	char serror[200],sbuf[80];
	int status,stat,opend;
	FILE *fptr;
	opend = 0;
/*
.....Get modal to define
*/
	ul_to_upper(ctyp);
	if (strcmp(ctyp,"FILE") != 0)
	{
		sprintf (serror,"Not a valid INCLUDE modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	incfile[0] = '\0';
	strcpy(incfile, cmsg);
	if (incfile[0]=='\0')
		goto done;
/*
.....load include file
*/
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", NCL_init_fstr, UU_NULL,
		incfile, 2, &fptr);
	if (stat!=UU_SUCCESS || fptr==UU_NULL)
	{
		ul_short_filename(incfile,sbuf,50);
		sprintf (serror,"Cannot open Modals file %s",incfile);
		ud_wrerr (serror);
		return UU_FAILURE;
	}
/*
.....Load the included modals file
*/
	opend = 1;
	if (which == 0) status = ul_load_modfile(fptr);
	else status = ul_ipv_load_modfile(fptr);
	if (status == UU_SUCCESS)
		goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	if (opend == 1) ux_fclose0 (fptr);
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_signon(ctyp,cmsg)
**                      This function sets the Signon Form modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_signon (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status;
	int maxsub=3;
	static char csub[3][20] = {"CAM","CAD","LEVELS"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid SIGNON modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....CAM Terminal
*/
	case 0:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_cam) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Cad Terminal
*/
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_cad) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....List all levels of sub-directories
*/
	case 2:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_list_part) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for SIGNON modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_display(ctyp,cmsg)
**                      This function sets the Display file modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_display (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status;
	int maxsub=2;
	static char csub[2][20] = {"WRAP","PAGE"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid DISPLAY modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Autowrap
*/
	case 0:
		if (ul_modal_toggle(cmsg,onoff,2,&UL_display_wrap) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Display a page at a time
*/
	case 1:
		if (ul_modal_toggle(cmsg,onoff,2,&UL_display_more) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for DISPLAY modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_plot(ctyp,cmsg)
**                      This function sets the Plot modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_plot (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status;
	int maxsub=11;
	static char csub[11][20] = {"CCPLT1","HPPLT1","TKPLT1","SAVE","USE_SAVED",
		"ABS_INCR","BYPASS","FP_TYPE","FP_LEFT","FP_RIGHT","UNITS"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid PLOT modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....CCPLT1 plotter type
*/
	case 0:
		if (ul_modal_toggle(cmsg,ccp,9,&UL_ccp_type) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....HPPLT1 plotter type
*/
	case 1:
		if (ul_modal_toggle(cmsg,hpp,10,&UL_hpp_type) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....TKPLT1 plotter type
*/
	case 2:
		if (ul_modal_toggle(cmsg,tkp,7,&UL_tkp_type) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Save work file
*/
	case 3:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_plot_save) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Use work file
*/
	case 4:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_plot_txx) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Positioning mode
*/
	case 5:
		if (ul_modal_toggle(cmsg,posit,10,&UL_plot_abs) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....HPPLT1 bypass mode
*/
	case 6:
		if (ul_modal_toggle(cmsg,onoff,2,&UL_hpp_bypass) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Floating point type
*/
	case 7:
		if (ul_modal_toggle(cmsg,fpp,2,&UL_plot_fpp) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Digits to left of decimal point
*/
	case 8:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 9) goto bad_parm;
		UL_plot_dleft = n;
		break;
/*
.....Digits to right of decimal point
*/
	case 9:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 9) goto bad_parm;
		UL_plot_dright = n;
		break;
/*
.....Floating point type
*/
	case 10:
		if (ul_modal_toggle(cmsg,nclunits,2,&UL_plot_units) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for PLOT modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_punch(ctyp,cmsg)
**                      This function sets the Punch modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_punch (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status;
	int maxsub=12;
	static char csub[12][20] = {"GRECO","DISPLAY","REC_LEN","PCH_ON","PCH_OFF",
		"TRANS_FILE","USER_TRANS","LEADER","SPEED","PARTNO","START",
		"COMMENT"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid PUNCH modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Greco disk file
*/
	case 0:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_pch_greco) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Display file
*/
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_pch_list) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Record length
*/
	case 2:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 132) goto bad_parm;
		UL_pch_reclen = n;
		break;
/*
.....Punch on code
*/
	case 3:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 377) goto bad_parm;
		UL_pch_on = n;
		break;
/*
.....Punch off code
*/
	case 4:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 377) goto bad_parm;
		UL_pch_off = n;
		break;
/*
.....Translation file
*/
	case 5:
		if (strlen(cmsg) > 3) goto bad_parm;
		strcpy (UL_pch_tfile,cmsg);
		break;
/*
.....User translated codes
*/
	case 6:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_pch_trans) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Leader
*/
	case 7:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 999) goto bad_parm;
		UL_pch_ldr = n;
		break;
/*
.....Punch off code
*/
	case 8:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 9999) goto bad_parm;
		UL_pch_speed = n;
		break;
/*
.....Man Readable PARTNO
*/
	case 9:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_pch_partno) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Start string
*/
	case 10:
		if (strlen(cmsg) > 20) goto bad_parm;
		strcpy (UL_pch_strt,cmsg);
		break;
/*
.....Comment Character
*/
	case 11:
		if (strlen(cmsg) > 1) goto bad_parm;
		strcpy (UL_pch_com,cmsg);
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for PUNCH modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_dnc(ctyp,cmsg)
**                      This function sets the DNC modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_dnc (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status;
	int maxsub=15;
	static char csub[15][20] = {"MODE","COMM_PORT","SEND_PR","RCV_PR","ECHO_PR",
		"START","XON","DISPLAY","REC_LEN","PARITY","CHAR_WAIT",
		"REC_WAIT","EOF","EOT","TRANS_FILE"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid DNC modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....DNC mode
*/
	case 0:
		if (ul_modal_toggle(cmsg,dnc_mode,4,&UL_dnc_mode) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Communications port
*/
	case 1:
		if (strlen(cmsg) > 20) goto bad_parm;
		strcpy (UL_dnc_port,cmsg);
		break;
/*
.....Send prompt
*/
	case 2:
		if (strlen(cmsg) > 20) goto bad_parm;
		strcpy (UL_dnc_send_pr,cmsg);
		break;
/*
.....Receive prompt
*/
	case 3:
		if (strlen(cmsg) > 20) goto bad_parm;
		strcpy (UL_dnc_rcv_pr,cmsg);
		break;
/*
.....Machine echo prompt
*/
	case 4:
		if (strlen(cmsg) > 20) goto bad_parm;
		strcpy (UL_dnc_echo_pr,cmsg);
		break;
/*
.....Start string
*/
	case 5:
		if (strlen(cmsg) > 20) goto bad_parm;
		strcpy (UL_dnc_strt,cmsg);
		break;
/*
.....XON/XOFF
*/
	case 6:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_dnc_xon) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Display file
*/
	case 7:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_dnc_list) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Record length
*/
	case 8:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 132) goto bad_parm;
		UL_dnc_reclen = n;
		break;
/*
.....Even Parity
*/
	case 9:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_dnc_par) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Character wait
*/
	case 10:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 9999) goto bad_parm;
		UL_dnc_chw = n;
		break;
/*
.....CR wait
*/
	case 11:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 9999) goto bad_parm;
		UL_dnc_crw = n;
		break;
/*
.....EOF string
*/
	case 12:
		if (strlen(cmsg) > 20) goto bad_parm;
		strcpy (UL_dnc_eof,cmsg);
		break;
/*
.....EOT string
*/
	case 13:
		if (strlen(cmsg) > 20) goto bad_parm;
		strcpy (UL_dnc_eot,cmsg);
		break;
/*
.....Translation file
*/
	case 14:
		if (strlen(cmsg) > 3) goto bad_parm;
		strcpy (UL_dnc_tfile,cmsg);
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for DNC modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_que(ctyp,cmsg)
**                      This function sets the NCL Que modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_que (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status;
	int maxsub=7;
	static char csub[7][20] = {"CLFILE","APT_SOURCE","PRINT","UPDATE",
		"#_LINES","PRIORITY","POST_PROC"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid NCL_QUE modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Create clfile
*/
	case 0:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_ncq_cl) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Create Apt source file
*/
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_ncq_as) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Create print file
*/
	case 2:
		if (ul_modal_toggle(cmsg,yesno,3,&UL_ncq_pr) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Update part program
*/
	case 3:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_ncq_pp) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....# of lines in print file
*/
	case 4:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 10 || n > 999) goto bad_parm;
		UL_ncq_nl = n;
		break;
/*
.....Priority
*/
	case 5:
		if (ul_modal_toggle(cmsg,pri,10,&UL_ncq_pri) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Run post-processor
*/
	case 6:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_ncq_post) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for NCL_QUE modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_ncl(ctyp,cmsg)
**                      This function sets the NCL Form modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_ncl (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status;
	int maxsub=5;
	static char csub[5][20] = {"LOAD_PP","CLFILE","APT_SOURCE","SAVE_PP",
		"RUN_PP"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid NCL modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Load PP
*/
	case 0:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_load_pp) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Create clfile
*/
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_create_cl) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Create APT source file
*/
	case 2:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_create_as) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Save PP
*/
	case 3:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_save_pp) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Automaticall start processing
*/
	case 4:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_run_ncl) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for NCL modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_ipv(ctyp,cmsg)
**                      This function sets the IPV Form modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_ipv (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status;
	int maxsub=3;
	static char csub[3][20] = {"CURVE_PTS","DELETE_IPV","DELETE_WIP"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid IPV modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Number of points per curve to create
.....in stock file
*/
    case 0:
	if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
	if (n < 0 || n > 100) goto bad_parm;
	UL_ipv_npts = n;
	break;
/*
.....Delete all IPV tool paths (.ipv)
*/
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_del_ipv) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Delete all IPV work in process files (.wip)
*/
	case 2:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_del_wip) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for IPV modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_unibase(ctyp,cmsg)
**                      This function sets the Unibase Modals Form modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_unibase (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status;
	int maxsub=6;
	static char csub[6][20] = {"SAVE_DISPLAY","SAVE_TESSEL","RESTORE_LIGHT",
		"RESTORE_MATERIAL", "RESTORE_COLOR", "RESTORE_UNITS"};
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
		sprintf (serror,"Not a valid UNIBASE modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Number of points per curve to create
.....in stock file
*/
    case 0:
		if (ul_modal_toggle(cmsg,yesno,2,&UR_save_display) != UU_SUCCESS)
			goto bad_parm;
		break;
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&UR_save_tessel) != UU_SUCCESS)
			goto bad_parm;
		break;
    case 2:
      if (ul_modal_toggle(cmsg,yesno,2,&UR_restore_lights) != UU_SUCCESS)
         goto bad_parm;
      break;
    case 3:
      if (ul_modal_toggle(cmsg,yesno,2,&UR_restore_mtrl) != UU_SUCCESS)
         goto bad_parm;
      break;
    case 4:
      if (ul_modal_toggle(cmsg,yesno2,3,&UR_restore_clr) != UU_SUCCESS)
         goto bad_parm;
      break;
		case 5:
		if (ul_modal_toggle(cmsg,yesno2,3,&UR_restore_units) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for UNIBASE modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}
/*********************************************************************
**       I_FUNCTION : S_modals_source(ctyp,cmsg)
**                      This function sets the Source Modals Form modals.
**       Added to support the formatting of command source lines.
**       JLS 2/14/00
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_source (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status;
	int maxsub=8;
	static char csub[8][20] = {"MAJOR_CASE","LABEL_CASE","VOCAB_CASE",
				"ACCURACY", "ALIGNMENT","INDENT_ALL","INDENT_SEP","FORMAT"};
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
		sprintf (serror,"Not a valid SOURCE modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
   case 0:
		if (ul_modal_toggle(cmsg,word_case,3,&UL_major_case) != UU_SUCCESS)
			goto bad_parm;
		break;
	case 1:
		if (ul_modal_toggle(cmsg,word_case,3,&UL_label_case) != UU_SUCCESS)
			goto bad_parm;
		break;
	case 2:
		if (ul_modal_toggle(cmsg,word_case,3,&UL_vocab_case) != UU_SUCCESS)
			goto bad_parm;
		break;
	case 3:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 ) goto bad_parm;
		UL_accuracy = n;
		NCL_accuracy = UL_accuracy;
		break;
	case 4:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 ) goto bad_parm;
		UL_alignment = n;
		break;
	case 5:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 ) goto bad_parm;
		UL_indent_all = n;
		break;
	case 6:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 ) goto bad_parm;
		UL_indent_sep = n;
		break;
   case 7:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_format_line) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for SOURCE modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : ul_modals_interface(ctyp,cmsg)
**                      This function sets the Unibase Modals Form modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
/*
......added INTERFACE section
......Yurong 4/7/00
*/
int ul_modals_interface (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,comsize,color;
	int maxsub=24;
	static char csub[24][20] = {"AUTO_CURSOR","TEXT_CURSOR",
		"TEXT_HIGHLIGHT", "MENU_FORMAT", "ICON_SIZE", "TEXT_SIZE", "FORM_HELP_FONT", "FORM_HELP_SIZE",
		"TEXT_FONT", "STATUS_TEXT_SIZE", "STATUS_TEXT_FONT",
		"PLABEL_SIZE", "PLABEL_FONT", "ELABEL_SIZE", "ELABEL_FONT",
		"COM_KEYPAD", "BROWSER", "FORM_HIGHLIGHT", "LIVE_MOUSE", "ACTIVE_LINE", "CURRENT_LINE",
		"FORM_PICTURE_POS", "STATUS_BUTTON", "FORM_SIZE"};
	static int font_size[] = {8,10,12,14,16,18,20,24,36,48};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid INTERFACE modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Automate cursor on
*/
    case 0:
		if (ul_modal_toggle(cmsg,onoff,2,&UW_auto_cursor) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Cursor on begining or end of text
*/
	case 1:
		if (ul_modal_toggle(cmsg,beginend,2,&UW_text_cursor) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
......highlight command line text field when focused
*/
	case 2:
		if (ul_modal_toggle(cmsg,onoff,2,&UW_text_select) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....removed
	case 3:
		if (ul_modal_toggle(cmsg,menu_fmt,3,&UW_menu_fmt) != UU_SUCCESS)
			goto bad_parm;
		break;
*/
	case 4:
		if (ul_modal_toggle(cmsg,icon_size,5,&UW_icon_size) != UU_SUCCESS)
			goto bad_parm;
		break; 
/*
.....removed Label font size and color, and added them to the label modals
*/
	case 5:
		if (ul_modal_toggle(cmsg,com_size,10,&comsize) != UU_SUCCESS)
			goto bad_parm;
		UW_com_size = font_size[comsize];
		break;
	case 6:
		if (cmsg[0]=='\0')
			strcpy(UW_form_font, "COURIER");
		else
			strcpy(UW_form_font, cmsg);
		break;
	case 7:
		if (ul_modal_toggle(cmsg,com_size,10,&comsize) != UU_SUCCESS)
			goto bad_parm;
		UW_form_helpsize = font_size[comsize];
		break;
	case 8:
		if (cmsg[0]=='\0')
			strcpy(UW_com_font, "COURIER");
		else
			strcpy(UW_com_font, cmsg);
		break;
	case 9:
		if (ul_modal_toggle(cmsg,com_size,10,&comsize) != UU_SUCCESS)
			goto bad_parm;
		UW_status_fontsize = font_size[comsize];
		break;
	case 10:
		if (cmsg[0]=='\0')
			strcpy(UW_status_font, "COURIER");
		else
			strcpy(UW_status_font, cmsg);
		break;
	case 11:
		if (ul_modal_toggle(cmsg,com_size,10,&comsize) != UU_SUCCESS)
			goto bad_parm;
		UW_prmpt_size = font_size[comsize];
		break;
	case 12:
		if (cmsg[0]=='\0')
			strcpy(UW_prmpt_font, "MS Sans Serif");
		else
			strcpy(UW_prmpt_font, cmsg);
		break;
	case 13:
		if (ul_modal_toggle(cmsg,com_size,10,&comsize) != UU_SUCCESS)
			goto bad_parm;
		UW_error_size = font_size[comsize];
		break;
	case 14:
		if (cmsg[0]=='\0')
			strcpy(UW_error_font, "MS Sans Serif");
		else
			strcpy(UW_error_font, cmsg);
		break;
	case 15:
		if (ul_modal_toggle(cmsg,keypad_fmt,2,&UW_keypad) != UU_SUCCESS)
			goto bad_parm;
		break;
	case 16:
		if (ul_modal_toggle(cmsg,browsop,2,&UW_browse_dir) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
......highlight form's text field when focused
*/
	case 17:
		if (ul_modal_toggle(cmsg,onoff,2,&UW_frmtext_select) != UU_SUCCESS)
			goto bad_parm;
		break;
	case 18:
		if (ul_modal_toggle(cmsg,onoff,2,&UW_live_mouse) != UU_SUCCESS)
			goto bad_parm;
		uw_ntset_livemouse(UW_live_mouse);
		break;
	case 19:
		if (ul_modal_color(cmsg, &color, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (color<0)
			goto bad_parm;
		UDM_layout.command_clr[0] = color;
		break;
	case 20:
		if (ul_modal_color(cmsg, &color, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (color<0)
			goto bad_parm;
		UDM_layout.command_clr[1] = color;
		break;
/*
......display position tooltip on picturebox
*/
	case 21:
		if (ul_modal_toggle(cmsg,onoff,2,&UW_picture_pos) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
......mode for updating the bottom statusbar
*/
	case 22:
		if (ul_modal_toggle(cmsg,uptmod,2,&UW_stat_mode) != UU_SUCCESS)
			goto bad_parm;
		break;
	case 23:
		if (ul_modal_toggle(cmsg,com_size,10,&comsize) != UU_SUCCESS)
			goto bad_parm;
/*
...
... Restrict form size not to exceed 18
... Default to size 8 if exceeds 18
...
*/
		if (comsize < 6)
		    UW_form_fontsize = font_size[comsize];
		else
		    UW_form_fontsize = 8;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for INTERFACE modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}


/*********************************************************************
**       I_FUNCTION : S_modals_pocket(ctyp,cmsg)
**                      This function sets the POCKET modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_pocket (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,inum;
	UU_REAL rval[2];
	int maxsub=3;
	static char csub[3][20] = {"TITLE","POSITION","SIZE"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid POCKET modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Title
*/
    case 0:
		strcpy(UW_pocket_title, cmsg);
		break;
/*
.....Position
*/
	case 1:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
#if UU_COMP!=UU_WIN2K
/*		UW_pocket_pos[0] = rval[0] * uw_xw.dev_xmax;
		UW_pocket_pos[1] = rval[1] * uw_xw.dev_ymax;*/
#else
		UW_pocket_pos[0] = (int)(rval[0] * uw_gl.dev_xmax);
		UW_pocket_pos[1] = (int)(rval[1] * uw_gl.dev_ymax);
#endif
		break;
/*
......Size
*/
	case 2:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
#if UU_COMP!=UU_WIN2K
/*		UW_pocket_size[0] = rval[0] * uw_xw.dev_xmax;
		UW_pocket_size[1] = rval[1] * uw_xw.dev_ymax;*/
#else
		UW_pocket_size[0] = (int)(rval[0] * uw_gl.dev_xmax);
		UW_pocket_size[1] = (int)(rval[1] * uw_gl.dev_ymax);
#endif
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for POCKET modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_measurement(ctyp,cmsg)
**                      This function sets the measurement modals.
**       PARAMETERS
**               INPUT  :  ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_measurement (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status,inum;
	UU_REAL rval;
	int maxsub=7;
	static char csub[7][20] = {"DISTTOL","RADTOL","RANGE",
		"GRID","GRID_ONLY","COLOR1","COLOR2"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
/*
.....Get modal to define
*/
	ul_to_upper(ctyp);
	for (i=0;i<maxsub;i++)
	{
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (serror,"Not a valid MEASUREMENT modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Disttol
*/
    case 0:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UM_len_exttoint(rval,UL_measurement_disttol);
		break;
/*
.....Radtol
*/
	case 1:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UM_len_exttoint(rval,UL_measurement_radtol);
		break;
/*
......Range
*/
	case 2:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UM_len_exttoint(rval,UL_measurement_range);
		break;
/*
......Minimum sf grid.
*/
	case 3:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 ) goto bad_parm;
		UL_measurement_minsfgrid = n;
		break;
/*
......Sf grid only.
*/
	case 4:
		if (ul_modal_toggle(cmsg,yesno,2,&UL_measurement_sfgridonly)
			!= UU_SUCCESS) goto bad_parm;
		break;
/*
.....first color
*/
	case 5:
		if (ul_modal_color(cmsg, &UL_measurement_color1, scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((UL_measurement_color1<0)&&(UL_measurement_color1!=-2))
			goto bad_parm;
		else if (UL_measurement_color1<0)
			UL_measurement_color1 = -1;
		break;
/*
.....first color
*/
	case 6:
		if (ul_modal_color(cmsg, &UL_measurement_color2, scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((UL_measurement_color2<0)&&(UL_measurement_color2!=-2))
			goto bad_parm;
		else if (UL_measurement_color2<0)
			UL_measurement_color2 = -1;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for MEASUREMENT modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_motion(ctyp,cmsg)
**                      This function sets the MOTION modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_motion (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	UU_REAL real_num;
	int i,n,status,trafl,cfl[10];
	double i1;
	int maxsub=24;
	UM_int2 n2,ifl;
	UN_motseg_cutattr cattr;
	static char csub[24][20] = {"CUTTER_COLOR","CUTTER_PEN","MOTION_COLOR",
		"MOTION_STYLE","MOTION_PEN","RAPID_COLOR","RAPID_STYLE","RAPID_PEN",
		"CUTTER_STEP","TRACUT","CUTTER_ITERATE","STACK_SIZE","SHANK_COLOR",
		"SHANK_PEN","HOLDER_COLOR","HOLDER_PEN","COMMANDS","CUTTER_SHADED",
		"SHANK_SHADED","HOLDER_SHADED","CUTTER_TRANS","SHANK_TRANS",
		"HOLDER_TRANS", "MOTION_WID"};
	static char lstyle[8][64] = {"*SOLID","*SMALL_DASH","*DOTTED","*CENTER",
		"*PHANTOM","*DASHED","*DASH_DOT","*DASH_SPACE"};
	static char lmstyle[4][64] ={"*STANDARD","*MEDIUM","*HEAVY","*EXHEAVY"};
	static char ltra[2][64] = {"*IGNORE","*APPLY"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
/*
.....Get current default attributes
*/
	ncl_cutter_get_defattr(&cattr);
	cutget_flag(cfl);
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
		sprintf (serror,"Not a valid MOTION modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Cutter color
*/
	case 0:
		if (ul_modal_color(cmsg, &cattr.color[0], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((cattr.color[0]<0)&&(cattr.color[0]!=-2))
			goto bad_parm;
		else if (cattr.color[0]<0)
			cattr.color[0] = -1;
		break;
/*
.....Cutter pen
*/
	case 1:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 256) goto bad_parm;
		cattr.pen[0] = n;
		break;
/*
.....Motion color
*/
	case 2:
		if (ul_modal_color(cmsg, &UN_motion_color, scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((UN_motion_color<0)&&(UN_motion_color!=-2))
			goto bad_parm;
		else if (UN_motion_color<0)
			UN_motion_color = -1;
		break;
/*
.....Motion line style
*/
	case 3:
		if (ul_modal_toggle(cmsg,lstyle,8,&n) != UU_SUCCESS) goto bad_parm;
		UN_motion_line = n + 1;
		break;
/*
.....Motion pen
*/
	case 4:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 256) goto bad_parm;
		UN_motion_pen = n;
		break;
/*
.....Rapid color
*/
	case 5:
		if (ul_modal_color(cmsg, &UN_rapid_color, scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((UN_rapid_color<0)&&(UN_rapid_color!=-2))
			goto bad_parm;
		else if (UN_rapid_color<0)
			UN_rapid_color = -1;
		break;
/*
.....Rapid line style
*/
	case 6:
		if (ul_modal_toggle(cmsg,lstyle,8,&n) != UU_SUCCESS) goto bad_parm;
		UN_rapid_line = n + 1;
		break;
/*
.....Rapid pen
*/
	case 7:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 256) goto bad_parm;
		UN_rapid_pen = n;
		break;
/*
.....Cutter steps
*/
	case 8:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0) goto bad_parm;
		n2 = n;
		upiter(&n2);
		break;
/*
.....TRACUT
*/
	case 9:
		if (ul_modal_toggle(cmsg,ltra,2,&trafl) != UU_SUCCESS) goto bad_parm;
		strafl(&trafl);
		break;
/*
.....Cutter iterations
*/
	case 10:
		if (ul_modal_toggle(cmsg,yesno,2,&n) != UU_SUCCESS) goto bad_parm;
		ifl = 120;
		n2 = n;
		setlfl(&ifl,&n2);
		break;
/*
.....Stack size
*/
	case 11:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 ) goto bad_parm;
		UN_mot_stack_size = n;
		break;
/*
.....Shank color
*/
	case 12:
		if (ul_modal_color(cmsg, &cattr.color[1], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((cattr.color[1]<0)&&(cattr.color[1]!=-2))
			goto bad_parm;
		else if (cattr.color[1]<0)
			cattr.color[1] = -1;
		break;
/*
.....Shank pen
*/
	case 13:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 256) goto bad_parm;
		cattr.pen[1] = n;
		break;
/*
.....Holder color
*/
	case 14:
		if (ul_modal_color(cmsg, &cattr.color[2], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((cattr.color[2]<0)&&(cattr.color[2]!=-2))
			goto bad_parm;
		else if (cattr.color[2]<0)
			cattr.color[2] = -1;
		break;
/*
.....Holder pen
*/
	case 15:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 256) goto bad_parm;
		cattr.pen[2] = n;
		break;
/*
.....Output command(s)
*/
	case 16:
		if (ul_modal_toggle(cmsg,yesno,2,&UN_motmodal_cmd) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Tool Shaded
*/
	case 17:
	case 18:
	case 19:
		if (ul_modal_toggle(cmsg,yesno,2,&cattr.shaded[i-17]) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Tool Translucency
*/
	case 20:
	case 21:
	case 22:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 100) goto bad_parm;
		cattr.trans[i-20] = n;
		break;
/*
.....Motion width
*/
	case 23:
/*
		if (ul_to_reals(&real_num,&n,1,cmsg) != UU_SUCCESS)
			goto bad_parm;
		UN_motion_width = real_num;
		break;
*/
		if (ul_modal_toggle(cmsg,lmstyle,8,&n) != UU_SUCCESS) goto bad_parm;
		if (n == 0)			
		    UN_motion_width = 1.0;
		else if (n == 1)
			UN_motion_width = 2.0;
		else if (n == 2)
			UN_motion_width = 3.0;
		else
			UN_motion_width = 4.0;
		break;
	}
/*
.....Set the cutter attributes
*/
	ncl_cutter_set_attr(&cattr);
	cfl[3] = cattr.shaded[0];
	cfl[6] = cattr.shaded[1];
	cfl[7] = cattr.shaded[2];
	cutset_flag(cfl);
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for MOTION modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : ul_modals_view(ctyp,cmsg)
**                      This function sets the 'view' modals.
**       PARAMETERS
**               INPUT  :  ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int ul_modals_view (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,inum;
	UU_REAL rval;
	int maxsub=15;
	static char csub[15][20] = {"MS_PAN_GAIN","MS_ROTATE_GAIN","MS_ZOOM_GAIN",
		"KB_PAN_GAIN","KB_ROTATE_GAIN","KB_ZOOM_GAIN","SM_PAN_GAIN","SM_ROTATE_GAIN",
		"SM_ZOOM_GAIN","DISPLAY", "CENTER", "Z_CENTER", "STATUS_LINE", "SEGMENT", "MS_WHEEL"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
/*
.....Get modal to define
*/
	ul_to_upper(ctyp);
	for (i=0;i<maxsub;i++)
	{
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (serror,"Not a valid VIEW modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Mouse pan sensitivity
*/
    case 0:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UV_MS_pangain = (float) rval;
		break;
/*
.....Mouse rotation sensitivity
*/
    case 1:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UV_MS_rotgain = (float) rval;
		break;
/*
.....Mouse zoom sensitivity
*/
    case 2:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UV_MS_zoomgain = (float) rval;
		break;
/*
.....Keyboard pan sensitivity
*/
    case 3:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UV_KB_pangain = (float) rval;
		break;
/*
.....Keyboard rotation sensitivity
*/
    case 4:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UV_KB_rotgain = (float) rval;
		break;
/*
.....Keyboard zoom sensitivity
*/
    case 5:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UV_KB_zoomgain = (float) rval;
		break;
/*
.....SpaceMouse pan sensitivity
*/
    case 6:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UV_SM_pangain = (float) rval;
		break;
/*
.....SpaceMouse rot sensitivity
*/
    case 7:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UV_SM_rotgain = (float) rval;
		break;
/*
.....SpaceMouse pan sensitivity
*/
    case 8:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UV_SM_zoomgain = (float) rval;
		break;
    case 9:
		if (ul_modal_toggle(cmsg,dpart,3,&UV_dyndisply) != UU_SUCCESS)
			goto bad_parm;
		break;
    case 10:
		if (ul_modal_toggle(cmsg,vrot,3,&UV_dyncenter) != UU_SUCCESS)
			goto bad_parm;
		LW_dyncenter = UV_dyncenter;
		break;
    case 11:
		if (ul_modal_toggle(cmsg,zcen,3,&UV_dyn_zval) != UU_SUCCESS)
			goto bad_parm;
		LW_dyn_zval = UV_dyn_zval;
		break;
    case 12:
		if (ul_modal_toggle(cmsg,onoff,2,&UV_dynstatln) != UU_SUCCESS)
			goto bad_parm;
		break;
    case 13:
		if (ul_to_number(cmsg,&UV_dynsegs) != UU_SUCCESS) goto bad_parm;
		break;
    case 14:
		if (ul_modal_toggle(cmsg,onoff,2,&UV_dynwheel) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for VIEW modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_buffer(ctyp,cmsg)
**                      This function sets the DISPLAY_BUFFER modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_buffer (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status;
	int maxsub=3;
	static char csub[3][20] = {"BUFFER","ERASE","CUTTER"};
	static char dispbuf[2][64] = {"*SWAP","*PIXEL"};
	static char erase[2][64] = {"*REDRAW","*ERASE"};
	static char cutter[2][64] = {"*FAST","*SMOOTH"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid DISPLAY_BUFFER modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Draw buffer
*/
	case 0:
		if (ul_modal_toggle(cmsg,dispbuf,2,&UW_disp_buf) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Erase method
*/
	case 1:
		if (ul_modal_toggle(cmsg,erase,2,&UW_erase_method) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Cutter method
*/
	case 2:
		if (ul_modal_toggle(cmsg,cutter,2,&UW_cutter_method) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for DISPLAY_BUFFER modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_material(ctyp,cmsg)
**                      This function sets the MATERIAL modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_material (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,inum;
	int maxsub=7;
	UU_REAL rval[3];
	static int material = -1;
	static char csub[7][20] = {"TYPE","NAME", "AMBIENT", "DIFFUSE", "SPECULAR",
							"S_COLOR", "EXPONENT"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid MATERIAL modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....type
*/
	case 0:
		if (ul_to_number(cmsg,&material) != UU_SUCCESS) goto bad_parm;
		material--;
		if (material < 0 || material > 16) goto bad_parm;
		break;
/*
.....name
*/
	case 1:
		if (material<0)
		{
			ud_wrerr ("You should define material type first before you define other characters");
			return UU_FAILURE;
		}
		if (cmsg[0]!='\0')
			strcpy((char*)(&(UM_mtrlmdl.name[material])),cmsg);
		break;
/*
.....Ambient
*/
	case 2:
		if (material<0)
		{
			ud_wrerr ("You should define material type first before you define other characters");
			return UU_FAILURE;
		}
		if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UM_mtrlmdl.ka[material] = rval[0];
		break;
/*
.....diffuse
*/
	case 3:
		if (material<0)
		{
			ud_wrerr ("You should define material type first before you define other characters");
			return UU_FAILURE;
		}
		if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UM_mtrlmdl.kd[material] = rval[0];
		break;
/*
.....specular
*/
	case 4:
		if (material<0)
		{
			ud_wrerr ("You should define material type first before you define other characters");
			return UU_FAILURE;
		}
		if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UM_mtrlmdl.ks[material] = rval[0];
		break;
/*
.....Spec_Color
*/
	case 5:
		if (material<0)
		{
			ud_wrerr ("You should define material type first before you define other characters");
			return UU_FAILURE;
		}
		if ((ul_to_reals(rval,&inum,3,cmsg) != UU_SUCCESS) ||
			inum != 3) goto bad_parm;
		UM_mtrlmdl.ks_r[material] = rval[0];
		UM_mtrlmdl.ks_g[material] = rval[1];
		UM_mtrlmdl.ks_b[material] = rval[2];
		break;
/*
.....Exponent
*/
	case 6:
		if (material<0)
		{
			ud_wrerr ("You should define material type first before you define other characters");
			return UU_FAILURE;
		}
/*		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 128) goto bad_parm;
		UM_mtrlmdl.spec_exp[material] = n;
*/
		if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		if (rval[0] < 0 || rval[0] > 128) goto bad_parm;
		UM_mtrlmdl.spec_exp[material] = rval[0];
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for MATERIAL modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_lights(ctyp,cmsg)
**                      This function sets the LIGHTS modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_lights (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status,inum, active, type, space, color;
	int maxsub=10;
	UU_REAL rval[3];
	static int light = -1;
	static char csub[10][20] = {"LIGHT","ACTIVE", "TYPE", "SPACE", "COLOR", "INTENSITY",
							"POSITION", "CONE", "DIRECTION", "AMBIENT"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid LIGHTS modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....light index
*/
	case 0:
		if (ul_to_number(cmsg,&light) != UU_SUCCESS) goto bad_parm;
		light--;
		if (light < 0 || light > 5) goto bad_parm;
		light_mod[light].key = UM_light_keys[light];
		light_mod[light].rel_num = UM_LIGHT_REL;
		break;
/*
.....active
*/
	case 1:
		if (light<0)
		{
			ud_wrerr ("You should define light index first before you define other characters");
			return UU_FAILURE;
		}
		if (ul_modal_toggle(cmsg,yesno,2,&active) != UU_SUCCESS)
			goto bad_parm;
		blank[light] = !active;
		ur_update_blanked(light_mod[light].key, !active);
		break;
/*
.....type
*/
	case 2:
		if (light<0)
		{
			ud_wrerr ("You should define light index first before you define other characters");
			return UU_FAILURE;
		}
		
		if (ul_modal_toggle(cmsg,direction,3,&type) != UU_SUCCESS)
			goto bad_parm;
		light_mod[light].type = type+1;
		break;
/*
.....space
*/
	case 3:
		if (light<0)
		{
			ud_wrerr ("You should define light index first before you define other characters");
			return UU_FAILURE;
		}
		if (ul_modal_toggle(cmsg,space_char,2,&space) != UU_SUCCESS)
			goto bad_parm;
		light_mod[light].space = space;
		break;
/*
.....color
*/
	case 4:
		if (light<0)
		{
			ud_wrerr ("You should define light indxe first before you define other characters");
			return UU_FAILURE;
		}
		if (ul_modal_color(cmsg, &color, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (color<0)
			goto bad_parm;
		light_attr[light].key = UM_light_keys[light];
		light_attr[light].color = color;		
		ur_update_attr(&(light_attr[light]));
		break;
/*
.....intensity
*/
	case 5:
		if (light<0)
		{
			ud_wrerr ("You should define light index first before you define other characters");
			return UU_FAILURE;
		}
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 100) goto bad_parm;
		light_mod[light].intens = n;
		break;
/*
......position
*/
	case 6:
		if (light<0)
		{
			ud_wrerr ("You should define light index first before you define other characters");
			return UU_FAILURE;
		}
		if ((ul_to_reals(rval,&inum,3,cmsg) != UU_SUCCESS) ||
			inum != 3) goto bad_parm;
		
		if (light_mod[light].space == UM_WORLD_POS)
		{
			UM_cc_exttoint(rval,light_mod[light].position);
		}
		else
			um_vctovc(rval,light_mod[light].position);
		break;
/*
.....Cone angle
*/
	case 7:
		if (light<0)
		{
			ud_wrerr ("You should define light index first before you define other characters");
			return UU_FAILURE;
		}
		if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		if (rval[0] < 0 || rval[0] > 90) goto bad_parm;
		light_mod[light].cone_angle = rval[0];
		break;
/*
......direction
*/
	case 8:
		if (light<0)
		{
			ud_wrerr ("You should define light index first before you define other characters");
			return UU_FAILURE;
		}
		if ((ul_to_reals(rval,&inum,3,cmsg) != UU_SUCCESS) ||
			inum != 3) goto bad_parm;
		light_mod[light].direction[0] = rval[0];
		light_mod[light].direction[1] = rval[1];
		light_mod[light].direction[2] = rval[2];
		break;
/*
......ambient
*/
	case 9:
		if (light<0)
		{
			ud_wrerr ("You should define light index first before you define other characters");
			return UU_FAILURE;
		}
		if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		light_mod[light].ambient[0] = rval[0];
		light_mod[light].ambient[1] = rval[0];
		light_mod[light].ambient[2] = rval[0];
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for LIGHTS modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_pick(ctyp,cmsg)
**                      This function sets the picking modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_pick (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200], buf[80];
	int i,status,inum, color;
	int maxsub=6;
	UU_REAL rval[3];
	static char csub[6][20] = {"APERTURE","VERIFY_MODE", 
									"HIGHLIGHT_COLOR", "VERIFY_COLOR", "MARKING", "HIERARCHY"};
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid PICKING modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
	case 0:
		if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		NCL_pick_aper = rval[0];
		break;
	case 1:
		if (ul_modal_toggle(cmsg,onoff,2,&NCL_pick_verify) != UU_SUCCESS)
			goto bad_parm;
		break;
	case 2:
		if (ul_modal_color(cmsg, &color, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (color<0)
			goto bad_parm;
		uw_glhicolor = color;		
		break;
	case 3:
		if (ul_modal_color(cmsg, &color, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (color<0)
			goto bad_parm;
		uw_glvrfcolor = color;		
		break;
	case 4:
		if (ul_modal_toggle(cmsg,pikmark,4,&NCL_mark_method) != UU_SUCCESS)
			goto bad_parm;
		ncl_set_mark(buf);
		break;
	case 5:
		if (ul_modal_toggle(cmsg,onoff,2,&NCL_pik_hierarchy) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for PICKING modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_cmdline(ctyp,cmsg)
**                      This function sets the commandline modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_cmdline (ctyp,cmsg)
char *ctyp,*cmsg; 
{
	char serror[200];
	int i,status,n;
	int maxsub=5;
	UM_int2 idx,ival;
	static char csub[5][20] = {"EDIT", "WINDOW", "INSERT", "LENGTH", "COMMENT"};
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
		sprintf (serror,"Not a valid CMD_LINE modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
	case 0:
		if (ul_modal_toggle(cmsg,yesno,2,&NCL_edit_mode) != UU_SUCCESS)
			goto bad_parm;
		break;
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&NCL_cmd_window_mode) != UU_SUCCESS)
			goto bad_parm;
		break;
	case 2:
		if (ul_modal_toggle(cmsg,offonsame,3,&NCL_com_mode) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Command line length
*/
	case 3:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 10 || n > NCL_MAX_COMLINE) goto bad_parm;
		UL_line_len = n;
		idx = 106; ival = n; setifl(&idx,&ival);
		break;
/*
.....Starting comment column
*/
	case 4:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n != 0 && n < UL_line_len) goto bad_parm;
		UL_comment_column = n;
		idx = 387; ival = n; setifl(&idx,&ival);
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for CMD_LINE modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_chain(ctyp,cmsg)
**                      This function sets the Chain modals.
**       PARAMETERS
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_chain (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,inum,irtn;
	int maxsub = 12;
	UU_REAL rval[3];
	UD_DASDATA dsda;
	static char csub[12][20] = {"CHAIN","TOL","PLANAR","PLANE","LINES","CIRCLES",
		"BSPLINES","COMPCURVES","SURFTOL","VECTORS","DIRECTION","MULTI_PICK"}; 
/*
.....Not applicable in batch mode
*/
	status = UU_SUCCESS;
	if (NCL_batch_run) goto done;
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
		sprintf (serror,"Not a valid CHAIN modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}

	switch(i)
	{
		case 0:
			if (ul_modal_toggle(cmsg,chaincond,2,&UD_chain_mod.conditional) !=
					UU_SUCCESS)
				goto bad_parm;
			break;
		case 1:
/*			if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
   			inum != 1) 
				goto bad_parm;
*/
			irtn = ud_dasin(cmsg, &dsda, 0);
			if (!irtn) goto bad_parm;
			UM_len_exttoint(dsda.stval.dval, UD_chain_mod.toler);
			break;
		case 2:
			if (ul_modal_toggle(cmsg,yesno,2,&UD_chain_mod.planar) != UU_SUCCESS)
   			goto bad_parm;	
			break;
		case 3:
			strcpy(UD_chain_mod.plane,cmsg);
			break;
		case 4:
			if (ul_modal_toggle(cmsg,inex,2,&UD_chain_mod.lines) != UU_SUCCESS)
   			goto  bad_parm;
			break;
		case 5:
			if (ul_modal_toggle(cmsg,inex,2,&UD_chain_mod.circles) != UU_SUCCESS)
   			goto  bad_parm;
			break;
		 case 6:
    		if (ul_modal_toggle(cmsg,inex,2,&UD_chain_mod.splines) != UU_SUCCESS)
       		goto bad_parm;
			break;
		 case 7:
    		if (ul_modal_toggle(cmsg,inex,2,&UD_chain_mod.composites)!= UU_SUCCESS)
       		goto  bad_parm;
			break;
		 case 8:
 /*  		if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
      			inum != 1)
      		goto bad_parm;
*/
			irtn = ud_dasin(cmsg, &dsda, 0);
			if (!irtn) goto bad_parm;
			UM_len_exttoint(dsda.stval.dval, UD_chain_mod.surftoler);
			break;
		 case 9:
			if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
   			inum != 1) goto bad_parm;
			UD_chain_mod.vectors = (int)rval[0];
			break;
		 case 10:
			if (ul_modal_toggle(cmsg,chaindir,4,&UD_chain_mod.direction)!= UU_SUCCESS)
			 	goto  bad_parm;
			break;
		case 11:
			if (ul_modal_toggle(cmsg,yesno,2,&UD_chain_mod.mult_surf) !=
				UU_SUCCESS) goto bad_parm;	
			break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for CHAIN modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       E_FUNCTION : ul_load_lgtmod(type)
**                      This function loads light default modals
**       PARAMETERS
**               INPUT  :  
**							type - 0 : load the light settings,
**									 1 : load the material settings.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void ul_load_lgtmod(type)
int type;
{
	static int nfld=2, ifld=0;
	char modfl[UX_MAX_PATH_LEN];
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	char banner[30],fname[UX_MAX_PATH_LEN], descrip[UX_MAX_PATH_LEN];
	char serror[80],ext[UX_SUFFIX_LEN];
	int mode, file_status, nc,status, opend;
	UX_pathname fullname,dir,flname;
	FILE *fptr;

	if (type == 0) strcpy(banner,"Load Light Modals");
	else strcpy(banner,"Load Material Modals");
	fname[0] = '\0'; nc = 0;
	strcpy(paths, NCL_init_fstr);
	strcpy(path_des, "System;Local");
	strcat(paths, ";");
	strcat(paths, "%UU_USER_SETTINGS\\modals");
	strcpy(descrip, "Modal Files (");
	strcpy(ext,"*.mod");
	strcat(descrip, "Modals file (*.mod)");
	ud_get_filename1(NULL, banner, ext, fname,&nc, descrip, 1, UU_FALSE, paths, path_des);

	if (strlen(fname) == 0) goto failed;
	status = UU_SUCCESS;
	strcpy(modfl,fname);
	opend = 0;
	ul_break_fname(modfl,dir,flname);
	mode = UX_EXISTS|UX_READ;
	status = ux_file_inquire (UU_NULL,dir,flname,UU_NULL,UU_NULL,&mode,
		&file_status,fullname,UX_NPRTERRS);
	if (status != UU_SUCCESS || mode == (mode|UX_NEXISTS))
	{
		sprintf (serror,"Modal file %s does not exist.",modfl);
		ud_wrerr (serror);
		goto failed;
	}
	ul_remove_quotes(fullname);
/*
.....Open the modals file
*/
	status = ux_fopen0(fullname,"r",&fptr);
	if (status  != UU_SUCCESS)
	{
		sprintf (serror,"Cannot open Modals file E%d",status);
		ud_wrerr (serror);
		goto failed;
	}
	opend = 1;
/*
.....Load the mod file
*/
	status = ul_load_modfile(fptr);
	if (status == UU_SUCCESS)
   	ux_fclose0 (fptr);
	if(type)
		S_update_lgtmod();
	uz_repaint(0);
failed:;
}

/*********************************************************************
**       E_FUNCTION : ul_save_lgtmod()
**                      This function restores the light default modals
**       PARAMETERS
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void ul_save_lgtmod()
{
	int i,blanked;
	struct UM_light_rec e;			
   struct UC_attributedatabag attr;
   UU_KEY_ID key;

	if (NCL_batch_run) return;
	for(i=0; i<5; i++) 
	{
		e.key = UM_light_keys[i];
		e.rel_num = UM_LIGHT_REL;
		uc_retrieve_data(&e, sizeof(struct UM_light_rec));

		light_uni[i].type = e.type;	
		light_uni[i].intens = e.intens;

		light_uni[i].position[0] = e.position[0];
		light_uni[i].position[1] = e.position[1];
		light_uni[i].position[2] = e.position[2];
		light_uni[i].direction[0] = e.direction[0];
		light_uni[i].direction[1] = e.direction[1];
		light_uni[i].direction[2] = e.direction[2];
		light_uni[i].cone_angle = e.cone_angle;
		light_uni[i].space = e.space;
		light_uni[i].ambient[0] = light_uni[i].ambient[1] = light_uni[i].ambient[2] = e.ambient[0];
		light_uni[i].ambient[3] = 1.0;
      key = e.key;
      uc_retrieve_attr(key,&attr);
      unicolor[i] = attr.color ;
		ur_retrieve_blanked(e.key, &blanked);
      uniblank[i] = blanked;
	}
}

/*********************************************************************
**       E_FUNCTION : ul_unibase_lights()
**                      This function restores the unibase light settings
**                      
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void ul_unibase_lights()
{
	int i, blanked ;
	struct UM_light_rec e;			
   struct UC_attributedatabag attr;
   UU_KEY_ID key;

	if (NCL_batch_run) return;
	for(i=0; i<5; i++) 
	{
		e.key = UM_light_keys[i];
		e.rel_num = UM_LIGHT_REL;
		uc_retrieve_data(&e, sizeof(struct UM_light_rec));

		e.type = light_uni[i].type;	
		e.intens = light_uni[i].intens;

		e.position[0] = light_uni[i].position[0];
		e.position[1] = light_uni[i].position[1];
		e.position[2] = light_uni[i].position[2];
		e.direction[0] = light_uni[i].direction[0];
		e.direction[1] = light_uni[i].direction[1];
		e.direction[2] = light_uni[i].direction[2];
		e.cone_angle = light_uni[i].cone_angle;
		e.space = light_uni[i].space;
		e.ambient[0] = e.ambient[1] = e.ambient[2] = light_uni[i].ambient[0];
		e.ambient[3] = 1.0;
		ur_update_data(&e);
      /* Save new light attributes */
      key = e.key;
      uc_retrieve_attr(key,&attr);
      attr.color = unicolor[i];
      blanked = uniblank[i];
      ur_update_attr(&attr);
      ur_update_blanked(e.key, blanked);
	}
	uz_repaint(0);
}

/*************************************************************************
**       E_FUNCTION : ul_save_mtrlmod()
**                      This function saves the material default modals
**       PARAMETERS
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void ul_save_mtrlmod()
{
	int i;
	UM_mtrlmdl_uni.index = UM_mtrlmdl.index;
	if (NCL_batch_run) return;
		for( i=0; i<64; i++ ) 
		{
			UM_mtrlmdl_uni.ka[i] = UM_mtrlmdl.ka[i];
			UM_mtrlmdl_uni.kd[i] = UM_mtrlmdl.kd[i];
			UM_mtrlmdl_uni.ks[i] = UM_mtrlmdl.ks[i];
			UM_mtrlmdl_uni.spec_exp[i] = UM_mtrlmdl.spec_exp[i];
			UM_mtrlmdl_uni.ks_r[i] = UM_mtrlmdl.ks_r[i];
			UM_mtrlmdl_uni.ks_g[i] = UM_mtrlmdl.ks_g[i];
			UM_mtrlmdl_uni.ks_b[i] = UM_mtrlmdl.ks_b[i];
			strcpy(UM_mtrlmdl_uni.name[i],UM_mtrlmdl.name[i]);
		}
}
/*************************************************************************
**       E_FUNCTION : ul_unibase__mtrl()
**                      This function restores the material default modals
**       PARAMETERS
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void ul_unibase_mtrl()
{
   int i;
   UM_mtrlmdl.index = UM_mtrlmdl_uni.index;
	if (NCL_batch_run) return;
      for( i=0; i<64; i++ )
      {
         UM_mtrlmdl.ka[i] = UM_mtrlmdl_uni.ka[i];
         UM_mtrlmdl.kd[i] = UM_mtrlmdl_uni.kd[i];
         UM_mtrlmdl.ks[i] = UM_mtrlmdl_uni.ks[i];
         UM_mtrlmdl.spec_exp[i] = UM_mtrlmdl_uni.spec_exp[i];
         UM_mtrlmdl.ks_r[i] = UM_mtrlmdl_uni.ks_r[i];
         UM_mtrlmdl.ks_g[i] = UM_mtrlmdl_uni.ks_g[i];
         UM_mtrlmdl.ks_b[i] = UM_mtrlmdl_uni.ks_b[i];
         strcpy(UM_mtrlmdl.name[i],UM_mtrlmdl_uni.name[i]);
		}
}
/*********************************************************************
**       I_FUNCTION : S_modals_labels(ctyp,cmsg)
**                      This function sets the Labels Modals Form modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_labels (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,inum;
	UU_REAL rval[2];
	int maxsub=10;
	static char csub[10][20] = {"LABELS","LABEL_COLOR","FONT_SIZE","OVERLAP_DISTANCE",
		"BACKGROUND", "BACKGROUND_COLOR", "LEADER_LINES", "LEADER_COLOR",
		 "LEADER_ARROW","OUTPUT_CMD"};
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
		sprintf (serror,"Not a valid LABEL modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Label display
*/
    case 0:
		if (ul_modal_toggle(cmsg,igonoff,3,&UW_label_on) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Label text color
*/
	case 1:
		if (ul_modal_color(cmsg, &UW_label_clr, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (UW_label_clr<0)
			goto bad_parm;
		break;
/*
.....Label font size(height,width)
*/
	case 2:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		UW_label_size[1] = (int)rval[0];
		UW_label_size[0] = (int)rval[1];
		break;
/*
.....Minimal overlap distance
*/
	case 3:
		if ((ul_to_reals(rval,&inum,1,cmsg) != UU_SUCCESS) ||
			inum != 1) goto bad_parm;
		UW_overlap_dis = (int)rval[0];
		break;
/*
......Label background box
*/
	case 4:
		if (ul_modal_toggle(cmsg,igonoff,3,&UW_bkg_on) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Label background box color
*/
	case 5:
		if (ul_modal_color(cmsg, &UW_bkg_clr, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (UW_bkg_clr<0)
			goto bad_parm;
		break;
/*
.....Leader lines
*/
	case 6:
		if (ul_modal_toggle(cmsg,igonoff,3,&UW_ldr_on) != UU_SUCCESS)
			goto bad_parm;
		break;     
/*
.....Leader line color
*/
	case 7:
		if (ul_modal_color(cmsg, &UW_ldr_clr, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (UW_ldr_clr<0)
			goto bad_parm;
		break;
/*
.....Leader line arrow
*/
	case 8:
		if (ul_modal_toggle(cmsg,igonoff,3,&UW_ldr_arrow) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Output Command
*/
	case 9:
		if (ul_modal_toggle(cmsg,yesno,2,&UW_out_cmd) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for LABEL modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_colors(ctyp,cmsg)
**                      This function sets the Colors Modals Form modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_colors (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int color,i,status;
	int maxsub=21;
	static char csub[21][20] = {"POINT","LINE","CIRCLE","PLANE","VECTOR",
		"PNTVEC","MATRIX","PATERN","SHAPE", "NCL_CV","SPLINE","COMP_CV","SSPLINE",
		"NCL_SURF","NURBS_SURF","TRIM_SURF", "REV_SURF","NET_SURF","SOLID",
		"MAXIS","WAXIS"};
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
		sprintf (serror,"Not a valid GEO_COLORS modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	if (ul_modal_color(cmsg, &color, scolor, 2)
		!= UU_SUCCESS) goto bad_parm;
	if ((color<0)&&(color!=-2))
		goto bad_parm;
	else if (color<0)
		color = -1;
	switch(i)
	{
/*
.....point
*/
    case 0:
		UL_color_mod.point =color;
		break;
/*
.....Line
*/
	case 1:
		UL_color_mod.line =color;
		break;
/*
.....circle
*/
	case 2:
		UL_color_mod.circle =color;
		break;
/*
.....plane
*/
	case 3:
		UL_color_mod.plane =color;
		break;
/*
......vector
*/
	case 4:
		UL_color_mod.vector =color;
		break;
/*
.....pntvec
*/
	case 5:
		UL_color_mod.pntvec =color;
		break;
/*
.....matrix
*/
	case 6:
		UL_color_mod.matrix =color;
		break;
/*
.....patern
*/
	case 7:
		UL_color_mod.patern =color;
		break;
/*
.....shape
*/
	case 8:
		UL_color_mod.shape =color;
		break;   
/*
.....ncl crv
*/
	case 9:
		UL_color_mod.nclcv =color;
		break;
/*
.....spline
*/
	case 10:
		UL_color_mod.spline =color;
		break;
/*
.....comp cv
*/
	case 11:
		UL_color_mod.comp =color;
		break;
/*
.....sspline
*/
	case 12:
		UL_color_mod.sspline =color;
		break;
/*
.....NCL surf
*/
	case 13:
		UL_color_mod.nclsf =color;
		break;
/*
.....NURBS
*/
	case 14:
		UL_color_mod.nurbs =color;
		break;
/*
.....trimmed surfaces
*/
	case 15:
		UL_color_mod.trimsf =color;
		break;
/*
.....rev surfaces
*/
	case 16:
		UL_color_mod.rvsf =color;
		break;
/*
.....net surfaces
*/
	case 17:
		UL_color_mod.netsf =color;
		break;
/*
.....Solids
*/
	case 18:
		UL_color_mod.solid =color;
		break;
/*
.....Solids
*/
	case 19:
		UL_color_mod.maxis =color;
		break;
/*
.....Solids
*/
	case 20:
		UL_color_mod.waxis =color;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for COLOR modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**	 I_FUNCTION : ul_material_number(cmsg,var)
**			This function sets a material number by a name.
**	 PARAMETERS	
**		 INPUT  :  cmsg = Input toggle value.
**		 OUTPUT :  var = toggle chosen.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
static void ul_material_number(cmsg,var)
char *cmsg;
int *var;
{
	char buf[80],bfi[20];
	int i,max;
/*
.....Assume the number of defined materials as in m2umodal.c
*/
	max = 16;
/*
.....Convert input to uppercase
*/
	if (*cmsg == '\0') return;
	strcpy (buf,cmsg);
	ul_to_upper(buf);
/*
.....Check for recognized toggle value
*/
	for (i=0;i<max;i++)
	{
		strcpy(bfi,(char*)(&(UM_mtrlmdl.name[i])));

		ul_to_upper(bfi);
		if (strcmp(buf,bfi) == 0)
		{
			*var = i;
			return;
		}
	}
}

/*********************************************************************
**       I_FUNCTION : S_modals_srfattr(ctyp,cmsg)
**                      This function sets the Surface Attributes Form modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_srfattr (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status;
	int maxsub=9;
	static char *csub[] = {"U_PATHS", "U_PTS", "V_PATHS", "V_PTS",
		"MATERIAL", "EDGE_DISPLAY", "EDGE_COLOR", "SHADED", "TRANSLUCENCY"};
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
		sprintf (serror,"Not a valid SRFATTR modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Numupaths
*/
	case 0:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 100) goto bad_parm;
		UM_srfattr.numupaths = n;
		break;
/*
.....Ptsperucrv
*/
	case 1:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 1000) goto bad_parm;
		UM_srfattr.ptsperucrv = n;
		break;
/*
.....Numupaths
*/
	case 2:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 100) goto bad_parm;
		UM_srfattr.numvpaths = n;
		break;
/*
.....Ptspervcrv
*/
	case 3:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 1000) goto bad_parm;
		UM_srfattr.ptspervcrv = n;
		break;
/*
.....Material
*/
	case 4:
		UM_srfattr.material = 0;
		ul_material_number(cmsg,&UM_srfattr.material);
		break;
/*
.....Edge Display
*/
	case 5:
		if (ul_modal_toggle(cmsg,yesno,2,&UM_srfattr.edge_disp) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Edge Color
*/
	case 6:
		if (ul_modal_color(cmsg, &UM_srfattr.edge_color, scolor, 1)
			!= UU_SUCCESS) goto bad_parm;
		break;
/*
.....Shaded
*/
	case 7:
		if (ul_modal_toggle(cmsg,yesno,2,&UM_srfattr.shaded) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Translucency
*/
	case 8:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 100) goto bad_parm;
		UM_srfattr.lucency = n;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for SRFATTR modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_autosave(ctyp,cmsg)
**                      This function sets the Auto Save modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_autosave (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status,ason;
	int maxsub=7;
	static char *csub[] = {"AUTO_U", "TYPE_U", "INTERVAL_U", "AUTO_PP",
		"TYPE_PP", "INTERVAL_PP", "MAX_SAVE_PP"};
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
		sprintf (serror,"Not a valid Auto Save modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....UNIBASE ON/OFF
.......Fixed handling of UR_ason flag - ASF 2/19/14.
*/
	case 0:
		if (ul_modal_toggle(cmsg,onoff,2,&ason) != UU_SUCCESS)
			goto bad_parm;
		if (ason) ur_enable_auto_save();
		else ur_disable_auto_save();
		break;
/*
.....UNIBASE type of auto save condition
*/
	case 1:
		if (ul_modal_toggle(cmsg,type,2,&UR_switch) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....UNIBASE auto save condition value
*/
	case 2:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0) goto bad_parm;
		if (UR_switch) UR_num_chg = n;
		else UR_time = n*60;
		break;
/*
.....PART PROGRAM ON/OFF
*/
	case 3:
		if (ul_modal_toggle(cmsg,onoff,2,&NCL_ason) != UU_SUCCESS)
			goto bad_parm;
		if (NCL_ason) NCL_last_autosave = time((long *) 0);
		break;
/*
.....PART PROGRAM type of auto save condition
*/
	case 4:
		if (ul_modal_toggle(cmsg,type,2,&NCL_switch) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....PART PROGRAM auto save condition value
*/
	case 5:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0) goto bad_parm;
		if (NCL_switch) NCL_num_chg = n;
		else NCL_time = n*60;
		break;
/*
.....PART PROGRAM maximum number of auto save files
*/
	case 6:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0) goto bad_parm;
		NCL_max_saves = n;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for AUTOSAVE modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_preview(ctyp,cmsg)
**                      This function sets the Preview Motion modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_preview (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status,ason;
	int maxsub=5;
	static char *csub[] = {"UNUSED_GEO", "COLOR", "KEEP_STOCK", "SINGLE_SEL",
		"AUTO_PREVIEW"};
	static char untog[2][64] = {"*HIDE","*FADE"};
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
		sprintf (serror,"Not a valid Auto Save modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Unused Geometry flag
*/
	case 0:
		if (ul_modal_toggle(cmsg,untog,2,&UN_unused_geo_flag) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Unused Geometry color
*/
	case 1:
		if (ul_modal_color(cmsg,&UN_unused_geo_color,scolor,1) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Keep stock
*/
	case 2:
		if (ul_modal_toggle(cmsg,yesno,2,&UN_keep_stock) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Single selection
*/
	case 3:
		if (ul_modal_toggle(cmsg,yesno,2,&UN_mot_single_sel) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Auto Preview
*/
	case 4:
		if (ul_modal_toggle(cmsg,yesno,2,&UN_mot_auto_preview) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for PREVIEW_MOTION modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       E_FUNCTION : S_init_lgtmod()
**                      This function initial light default modals
**                      from a disk file.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static void S_init_lgtmod()
{
	int i;
	UM_int2 ifl,idx;
	struct UM_light_rec e;			

	ifl = 35; getifl(&ifl,&idx);
	if (idx == 1) return;
	for(i=0; i<5; i++) 
	{
		e.key = UM_light_keys[i];
		e.rel_num = UM_LIGHT_REL;
		uc_retrieve_data(&e, sizeof(struct UM_light_rec));

		light_mod[i].type = e.type;	
		light_mod[i].intens = e.intens;

		light_mod[i].position[0] = e.position[0];
		light_mod[i].position[1] = e.position[1];
		light_mod[i].position[2] = e.position[2];
		light_mod[i].direction[0] = e.direction[0];
		light_mod[i].direction[1] = e.direction[1];
		light_mod[i].direction[2] = e.direction[2];
		light_mod[i].cone_angle = e.cone_angle;
		light_mod[i].space = e.space;
		light_mod[i].ambient[0] = light_mod[i].ambient[1] = light_mod[i].ambient[2] = e.ambient[0];
		light_mod[i].ambient[3] = 1.0;
		
		light_uni[i].type = e.type;
      light_uni[i].intens = e.intens;

      light_uni[i].position[0] = e.position[0];
      light_uni[i].position[1] = e.position[1];
      light_uni[i].position[2] = e.position[2];
      light_uni[i].direction[0] = e.direction[0];
      light_uni[i].direction[1] = e.direction[1];
      light_uni[i].direction[2] = e.direction[2];
      light_uni[i].cone_angle = e.cone_angle;
      light_uni[i].space = e.space;
      light_uni[i].ambient[0] = light_uni[i].ambient[1] = light_uni[i].ambient[2
] = e.ambient[0];
      light_uni[i].ambient[3] = 1.0;
	}
}

/*********************************************************************
**       E_FUNCTION : S_update_lgtmod()
**                      This function update light default modals
**                      
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static void S_update_lgtmod()
{
	int i, blanked ;
	UM_int2 ifl,idx;
	struct UM_light_rec e;			
   struct UC_attributedatabag attr;
   UU_KEY_ID key;


	ifl = 35; getifl(&ifl,&idx);
	if (idx == 1) return;
	for(i=0; i<5; i++) 
	{
		e.key = UM_light_keys[i];
		e.rel_num = UM_LIGHT_REL;
		uc_retrieve_data(&e, sizeof(struct UM_light_rec));

		e.type = light_mod[i].type;	
		e.intens = light_mod[i].intens;

		e.position[0] = light_mod[i].position[0];
		e.position[1] = light_mod[i].position[1];
		e.position[2] = light_mod[i].position[2];
		e.direction[0] = light_mod[i].direction[0];
		e.direction[1] = light_mod[i].direction[1];
		e.direction[2] = light_mod[i].direction[2];
		e.cone_angle = light_mod[i].cone_angle;
		e.space = light_mod[i].space;
		e.ambient[0] = e.ambient[1] = e.ambient[2] = light_mod[i].ambient[0];
		e.ambient[3] = 1.0;
		ur_update_data_fixed(&e);
      /* Save new light attributes */
		key = e.key;
		uc_retrieve_attr(key,&attr);
		attr.color = light_attr[i].color;
		ur_update_attr(&attr);
		blanked = blank[i];
		ur_update_blanked(e.key, blanked);
	}
	uw_gllight_define();
}
/*********************************************************************
**       I_FUNCTION : S_modals_custom_color(ctyp,cmsg)
**                      This function sets the custom color modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int S_modals_custom_color (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,inum;
	UU_REAL rval[3];
	int color_defined;
	int maxsub=2;
	static char *csub[] = {"NAME", "RGB"};
/*
.....remove trailing space and '\t'
*/
	for (i=strlen(cmsg); i>0; i--)
	{
		if ((cmsg[i-1]==' ')||(cmsg[i-1]=='\t'))
			cmsg[i-1] = '\0';
		else
			break;
	}
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
		sprintf (serror,"Not a valid COLOR modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Name
*/
	case 0:
/*
......check if mode file have more than 48 colors defined
*/
		if (S_cus_color>=47)
		{
			strcpy (serror,"The Modal file defined more than 48 custom colors.\r\n");
			strcat (serror, "Only 48 custom colors will be accepted.");
			ud_wrerr (serror);
			goto failed;
		}
/*
......check if this color defined already, if yes, give the error message and ignore it
*/
		color_defined = 0;
		for (i=0; i<16;i++)
		{
			if ((uw_color_name[i][0]!='\0')&&(stricmp(uw_color_name[i], cmsg)==0))
			{
				color_defined = 1;
				break;
			}
		}
		if (color_defined)
		{
			sprintf (serror,"Basic Color %s cannot be changed!", cmsg);
			ud_wrerr (serror);
			goto failed;
		}
		color_defined = 0;
		for (i=0; i<48;i++)
		{
			if ((S_cus_color_name[i][0]!='\0')&&(stricmp(S_cus_color_name[i], cmsg)==0))
			{
				color_defined = 1;
				break;
			}
		}
		if (color_defined)
		{
			sprintf (serror,"Color %s already defined. It's value will be ignored here.", cmsg);
			ud_wrerr (serror);
			goto failed;
		}
		S_cus_color++;
		strcpy(uw_color_name[S_cus_color+16], cmsg);
/*
......SAVE ALL IN UPPER CASE
*/
		ul_to_upper(uw_color_name[S_cus_color+16]);
		strcpy(S_cus_color_name[S_cus_color], uw_color_name[S_cus_color+16]);
		break;
/*
.....RGB values
*/
	case 1:
		if ((ul_to_reals(&rval,&inum,3,cmsg) != UU_SUCCESS) || inum != 3)
			goto bad_parm;
		uw_color_table[S_cus_color+16][0] = (int)rval[0];
		uw_color_table[S_cus_color+16][1] = (int)rval[1];
		uw_color_table[S_cus_color+16][2] = (int)rval[2];
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for COLOR modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : S_modals_files(ctyp,cmsg)
**             This function sets the modals file modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int S_modals_files (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,inum;
	int maxsub=2;
	static char *csub[] = {"SAVE_COLORS", "SAVE_FORMS"};
/*
.....remove trailing space and '\t'
*/
	for (i=strlen(cmsg); i>0; i--)
	{
		if ((cmsg[i-1]==' ')||(cmsg[i-1]=='\t'))
			cmsg[i-1] = '\0';
		else
			break;
	}
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
		sprintf (serror,"Not a valid MODAL_FILES modal.  /%s/ %s", ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....save_color
*/
	case 0:
		if (ul_modal_toggle(cmsg,yesno,2,&UW_Store_color) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Save_forms
*/
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&UW_Store_forms) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for MODALS_FILES modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*************************************************************************
**       E_FUNCTION : ul_save_clrmod()
**          This function saves the color default modals
**       PARAMETERS
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void ul_save_clrmod()
{
	int i;
	for( i=0; i<64; i++ )
	{
         uw_color_table_sav[i][0] = uw_color_table[i][0];
         uw_color_table_sav[i][1] = uw_color_table[i][1];
         uw_color_table_sav[i][2] = uw_color_table[i][2];
		 if (uw_color_name[i][0] !='\0')
			strcpy(uw_color_name_sav[i], uw_color_name[i]);
		 else
			 uw_color_name_sav[i][0] = '\0';
	}
}
static int S_IsColorDefined(color_name)
char color_name[96];
{
	int i;
	for( i=0; i<64; i++ )
	{
		if (stricmp(color_name, uw_color_name_sav[i])==0)
			return 1;
	}
	return 0;
}
/*************************************************************************
**       E_FUNCTION : ul_unibase_clr()
**          This function restores the color default modals
**       PARAMETERS
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void ul_unibase_clr()
{
	int i,j,k, nxt_indx,ret,num;
	double dx, dy, dz;
	int color_table[64][3];
	char color_name[64][96], tmpstr[96];
	if (UR_restore_clr==1)
	{
		for( i=0; i<64; i++ )
		{
			 uw_color_table[i][0] = uw_color_table_sav[i][0];
			 uw_color_table[i][1] = uw_color_table_sav[i][1];
			 uw_color_table[i][2] = uw_color_table_sav[i][2];
			 if (uw_color_name_sav[i][0] !='\0')
				strcpy(uw_color_name[i], uw_color_name_sav[i]);
			 else
				 uw_color_name[i][0] = '\0';
		}
	}
	else if (UR_restore_clr==2)
/*
......merge the saved color table with the new one
*/
	{
/*
......check if the loaded color table have color name such as "IGES_COLOR_#", "COLOR"
*/
		nxt_indx = -1;
		for( i=16; i<64; i++ )
		{
			if (uw_color_name[i][0]=='\0')
			{
				nxt_indx = i;
				break;
			}
			strcpy(tmpstr, uw_color_name[i]);
			ret = sscanf(tmpstr, "IGES_COLOR_%d", &num);
			if (((ret<=0)||(num<0))&& stricmp(uw_color_name[i], "color")!=0)
			{
/*
......real color name
*/
				continue;
			}
/*
......it's not a real color name, search the saved NCL color table to see if have a match there
*/
			for (j=16;j<64;j++)
			{
				dx = 1.0*uw_color_table[i][0] - 1.0*uw_color_table_sav[j][0];
				dy = 1.0*uw_color_table[i][1] - 1.0*uw_color_table_sav[j][1];
				dz = 1.0*uw_color_table[i][2] - 1.0*uw_color_table_sav[j][2];
				if ((abs(dx)<=1.0)&&(abs(dy)<=1.0)&&(abs(dz)<=1.0))
				{
/*
.....find the match, replace the color name with NCL color name
*/
					if (uw_color_name_sav[j][0]!='\0')
						strcpy(uw_color_name[i], uw_color_name_sav[j]);
					break;
				}
			}
		}
/*
.....added the NCL custom color from nxt_indx
*/
		if ((nxt_indx==-1)||(nxt_indx>63))
			goto done;
		k = nxt_indx;
		for (i=16; i<64; i++)
		{
			for (j=0;j<k;j++)
			{
				dx = 1.0*uw_color_table_sav[i][0] - 1.0*uw_color_table[j][0];
				dy = 1.0*uw_color_table_sav[i][1] - 1.0*uw_color_table[j][1];
				dz = 1.0*uw_color_table_sav[i][2] - 1.0*uw_color_table[j][2];
				if ((abs(dx)<=1.0)&&(abs(dy)<=1.0)&&(abs(dz)<=1.0))
				{
					break;
				}
			}
			if (j<k)
			{
/*
.....this color have already in the list, ignore this color
*/
				i++;
				continue;
			}
			uw_color_table[k][0] = uw_color_table_sav[i][0];
			uw_color_table[k][1] = uw_color_table_sav[i][1];
			uw_color_table[k][2] = uw_color_table_sav[i][2];
			if (uw_color_name_sav[i][0]!='\0')
				strcpy(uw_color_name[k], uw_color_name_sav[i]);
			else
				uw_color_name[k][0] = '\0';
			k++;
			if (k>=64)
				break;
		}
	}
done:;
	ncl_update_colors(0);
}

/*********************************************************************
**       E_FUNCTION : ul_load_clrfile(fptr)
**           This function loads the color file
**                      from a disk file.
**       PARAMETERS     
**               INPUT  :  fptr: color file to be load.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int ul_load_clrfile(fptr)
FILE *fptr;
{
	char serror[80],buf[80],ctyp[40],cmsg[40];
	int status,stat,numint,ityp,istat;
	int maxsub=31;
/*
.....Assume success
*/
	status = UU_SUCCESS;
	S_cus_color = -1;
/*
.....Read a record
*/
	do
	{
		stat = ul_fread (fptr,buf,sizeof(buf),&numint);
		if (stat == UX_EOF)
		{
			goto file_done;
		}
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			sprintf (serror,"Error reading from color file E%d.",stat);
			ud_wrerr (serror);
			goto failed;
		}
/*
.....Check for record type
*/
		istat = ul_modal_check (buf,&ityp,ctyp,cmsg);
/*
.....Invalid syntax
*/
		if (istat != UU_SUCCESS)
		{
			sprintf (serror,"Color file syntax error. %s",buf);
			ud_wrerr (serror);
		}
/*
.....Subsystem type
*/
		switch (ityp)
		{
		case 1:
			ul_to_upper(ctyp);
			if (strcmp(ctyp, "COLOR") != 0)
			{
				sprintf (serror, "Not a valid COLOR parameter. %s", buf);
				ud_wrerr (serror);
				break;
			}
   			break;
		case 2:
   			S_modals_custom_color(ctyp,cmsg);
   			break;
		}
	}
	while (stat == UU_SUCCESS || stat == UX_NO_SPACE);
file_done:;
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	if (S_cus_color!=-1)
		ncl_update_colors(0);
	return (status);
}
/*********************************************************************
**       I_FUNCTION : ul_ipvmod_custom_color(first,ctyp,cmsg)
**                      This function sets the custom color modals for NCLIPV.
**       PARAMETERS     
**               INPUT  :  first: first time parser in
**							ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
ul_ipvmod_custom_color(first,ctyp,cmsg)
int first;
char *ctyp, *cmsg;
{
	int i;
	if (first==1)
	{
		S_cus_color = -1;
		for (i=0; i<48;i++)
		{
			S_cus_color_name[i][0] = '\0';
		}
	}
	S_modals_custom_color(ctyp,cmsg);
}

