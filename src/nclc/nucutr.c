/*********************************************************************
**    NAME         :  nucutr.c
**       CONTAINS:
**          nclu_cutter_def()
**          Call back routines for cutter form.
**          ncl_getcut_profil
**       NOTE
**          The toolib database record and form fields are described
**          in /ncl502/nclfiles/info/toolib.txt.  If the fields or
**          form change, then this file should be modified to reflect
**          the changes.
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nucutr.c , 25.7
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 10:54:54
*********************************************************************/

#include "stdio.h"
#include "usysdef.h"
#include "class.h"
#include "dselmask.h"
#include "lcom.h"
#include "mdrel.h"
#include "mfort.h"
#include "mxxx.h"
#include "mdpick.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclmplay.h"
#include "nclfile.h"
#include "nclvx.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"
#include "mpocket.h"
#include "toolibdata.h"

static int mainmarkval=0;
static UM_int4 deficfl[10];
static UM_real8 defcbuf[6], defdbuf[20];
static UM_real8 savdbuf[6];
static char defsym[MAXSYMLEN], defsymlb[256];
/*
......saved (accepted) input field  for parameter form
*/
static char parmstr[21][12];
/*
......form input field  for parameter form
*/
static char parm_cstr[21][16];
static int parm_nary;
/*
......default parameters from tool
*/
static char parm_fcstr[20][22];
static UM_real8 tempdbuf[6];
/*
......if paramter redefined by user
*/
static int pspec = UU_FALSE;
static UM_int4 savnary = 0;

static char current_draw[30] = "";
static double toolnum = 0;
static int SortFunc2(char* cData1, char* cData2, int lParamSort);

static int x=0;
static int y=0;
static int y_max=0;

static int cut_form_dsp = 0;
static int parm_form_dsp = 0;
/*
......string input form field / saved (accepted) input field  for cutterdef.frm
*/
static char def_cstr[6][NCL_MAX_LABEL+1], savdbuf_str[6][NCL_MAX_LABEL+1];

extern char *frm_ptr;
static int **ans_ptr;
static int S_select_tool = 0;

extern UD_METHOD UD_initfrm_intry;

static UD_FSTAT OnChoice(),OnCutType(),OnNoTool(),OnPseudo(),OnPseudoDef();
static UD_FSTAT OnSymbol(),OnListCalbacks(),OnViewOpt(),OnShade();
static UD_FSTAT OnShank(),OnParm(),OnInitForm();
static UD_FSTAT S_tool_library(), OnBrowse();
static void OnCtypeChg();
static void S_load_tools();
static void S_load_toolnum();
static void S_form_loadcut();

static struct TL_tooldata_rec Tool_current_data;
static struct TL_toolhead_rec Tool_head;
#define FTOOL 0
#define FDESC 1
#define FTYPE 2
#define FPSEU 3
#define FDEFI 4
#define FDIA1 5
#define FRAD1 6
#define FHGT1 7
#define FANG1 8
#define FZHT1 9
#define FLAT1 10
#define FSEGM 11
#define FMOVE 12
#define FSHAD 13
#define FSYM1 14
#define FSYM2 15
#define FSYM3 16
#define FSLIBB 17
#define FSLIB 18
#define FSHK1 19
#define FSHK2 20
#define FSHK3 21
#define FHLD1 22
#define FHLD2 23
#define FHLD3 24
#define FDISP 25
#define FVIWT 26
#define FTLIB 27
#define FTLTX 28
#define FPARM 29
//#define FFILT 29
#define FOUTP 30
#define FLIST 31
/*
.....Could not find where these three are used so I left them the
.....same. - Andrew 2/14/13
*/
#define FLAB1 33
#define FLAB2 34
#define FLAB3 35

#define LABOFS 3

#define FDSM1 0
#define FDSM2 1
#define FDAX1 2
#define FDAX2 3
#define FDSHD 4
#define FDAT1 5
#define FDAT2 6
#define FDAT3 7
#define FDAT4 8
#define FDCLA 9
#define FDLS1 10
#define FDLS2 11

#define BLADE 191
#define LATHE 700
/*
......Scfl[1]: segment
......Scfl[2]: move
......Scfl[3]: shaded Scfl3 = Scfl[3] (for cutter form, main form)
......for fsymbol:
......Scfl[0]: 0 = No symbol defined, 1 = Parameters, 2 = Symbol
......			3 = Geometry, 4 = Profile.
......			when No symbol defined (0,1), Scfl[0]=Spseudo;
......for fshank 
......Scfl[4]: 0 = No symbol defined, 1 = Parameters, 2 = Symbol
......			3 = Geometry, 4 = Profile.
......			when No symbol defined (0,1), Scfl[4]=fshank;
......for fholder:
......Scfl[5]: 0 = No symbol defined, 1 = Parameters, 2 = Symbol
......			3 = Geometry, 4 = Profile.
......			when No symbol defined (0,1), Scfl[5]=fholder;
......Scfl[6]: shaded for shank form
......Scfl[7]: shaded for Holder form
*/
static int Scfl[10],Scfl3;
/*
......Ssymfl[0]: symbol checkbox
......Ssymfl[1]: shank checkbox
......Ssymfl[2]: holder checkbox
*/
static UU_LOGICAL Ssymfl[3];
static char Ssymlib[256],Ssymbol[3][MAXSYMLEN],Spv[3][NCL_MAX_LABEL];
static char Scut[6][NCL_MAX_LABEL+1],Satt[12][NCL_MAX_LABEL+1];
static UD_TLIST Stool_list;
static UD_LIST Stype_list;
static UD_TABLEINFO saved_info = {-1,-1,-1,-1,-1,0};
static int toolno_click = 0;
static int type_click = 0;
static int descript_click = 0;
static int diam_click = 0;
static int rad_click = 0;
static int hgt_click = 0;
static int angle_click = 0;


static int Stype = 1;
static int Spseudo = 0;
static int Sfilt = 0;
static int Sclash = 1;
/*
.....data changed after tool select: 1; not changed: 0
*/
static int Sdefflag = 1;
static char Stool[20] = "";
static char Sdesc[42] = "";
static int SortFunc(char* cData1, char* cData2, int lParamSort);
char **ncl_get_toolist(int*, int);
static int Soutput = 0;
static UU_LOGICAL Sload_prof = UU_FALSE;
static char S_cutparm[20][256];

nclf_getlsc(inx, txt, len) 
UM_f77_str_ptr txt;
UM_int2 *inx, *len;
{
	int i;
	char *cstr;
	cstr = UM_cstr_of_f77_str(txt);
	*len = strlen(S_cutparm[*inx]);
	if (*len>0)
		strncpy(cstr, S_cutparm[*inx], *len);
}

nclf_savelsc(inx, txt, len)
UM_f77_str_ptr txt;
UM_int2 *inx, *len;
{
	char *cstr;
	cstr = UM_cstr_of_f77_str(txt);
	strncpy(S_cutparm[*inx], cstr, *len);
	S_cutparm[*inx][*len] = '\0';
}

static void S_call_tmpval(valstr, val)
char *valstr, *val;
{
	char ostr[500];
	NCL_cmdbuf cmdbuf;
	ncl_init_cmdbuf(&cmdbuf);
	
	sprintf(ostr,"%s=\"%s\"", valstr, val);
	ncl_add_token(&cmdbuf, ostr, UU_FALSE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
}
int S_sym_handle(fullname)
	char *fullname;
{
	struct UB_symbol_rec sym;
	UU_LOGICAL found;
	char fname[UB_SYMBOL_NAME_LEN_PLUS],symname[UB_SYMBOL_NAME_LEN_PLUS];
	char farea[UB_MAX_PATH_LEN];
	char *rindex(), *position;
	int status = UU_SUCCESS;

	if (ux_decompose_path(fullname,farea,fname,UX_PRTERRS|UX_NQUOTES)
		!= UU_SUCCESS) goto failed;
	strcpy(symname, fname);
	position = rindex(symname, '.');
	if (position != UU_NULL)
		*position = '\0';
	ncl_parse_label(symname,sym.label,&sym.subscr);
	if (ub_get_symmaster_by_name(&sym, &found, 1,1) != UU_SUCCESS) 
		goto failed;

	if (found)
	{
		if (ubu_del_symmaster(sym.key, UU_FALSE,UU_TRUE) != UU_SUCCESS) 
			goto done;
	}
	if (ur_lp02(fullname) != 0)
	{
		goto failed;
	}
	if (ubi_FixMsymsAfterMsymLoad(UU_NULL,farea,sym.label,"NOOUTPUT",UU_NULL)
			!= UU_SUCCESS) goto failed;
	ubi_update_symbol(symname);
	goto done;
failed: 
	status = UB_FAILURE;
done:;
	return(status);
}  
/*
.....the function is same as ubu_load_archive but will overwrite the exist symbol, so no asking overwrite
*/
int S_load_archive(area, libname)
	char *area;
	char *libname;
{
	struct UB_symbol_rec sym;
	int nxtuple;
	int ok;
	int status;

	status = UU_SUCCESS;

	if (uxu_load_archive(area, libname, UU_NULL, "UB_SYM_SUFFIX",
				S_sym_handle, &ok) != UU_SUCCESS) goto failed;
	if (ok != UU_SUCCESS) goto failed;
	nxtuple = 1;
	while (ur_get_next_new_data_key(UB_SYMBOL_REL,&nxtuple,&sym.key)==0)
	{
		if (ub_retrieve_sym(&sym,sizeof(sym)) != UU_SUCCESS)
				goto failed;
		if (ux_get_fversion(sym.path, &(sym.version), UX_PRTERRS) 
					!= UU_SUCCESS)	goto failed; 
		if (ubi_update_data_fixed(&sym) != UU_SUCCESS) 
				goto failed;
		nxtuple++;
	}
	goto done;
failed: 
	status = UB_FAILURE;
done:;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION     : nclu_cutter_def()
**       Processes the NCL CUTTER DEFINITION form.
**                              it could load a tool or define a cutter
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_cutter_def()
{
	int i,j,inum,ifl,ifl1;
	char lstr[UX_MAX_PATH_LEN],ostr[UX_MAX_PATH_LEN],tstr[20];
	UM_real8 cnew[6];
	UM_f77_str sym_str,ssym_str,hsym_str;
	NCL_cmdbuf cmdbuf;
	UX_pathname lib,dir,dir2,tmplib;
	UD_METHOD save_entry;
	UM_int4 nclkey;

	char *p, *ux_getenv(), *index();
	char tch2[256];
	int status;
	char drawng[64], symdir[64], slib_name[64];
	static int redisp_form = 0;
	static char tch[256] = "";
/*
.....Set up form fields
*/
	static int ofl=1,idum;
	static char cstr[6][NCL_MAX_LABEL+1];
	static char traverse[] = {
		0,0, 1,1,1, 1,1,1,1,0,0, 1,1,1, 1,1,1,1,1, 1,0,0,1,0,0, 1,0,
		1,1,0,1, 1};

	static UD_METHOD methods[] = {
		UU_NULL, UU_NULL, OnCutType,OnPseudo,OnPseudoDef, 
		OnNoTool,OnNoTool,OnNoTool,OnNoTool,OnNoTool,OnNoTool,
		OnNoTool,OnNoTool,OnShade,
		OnChoice,OnShank, UU_NULL, OnBrowse, UU_NULL,
		OnChoice,OnShank,UU_NULL,OnChoice,OnShank,UU_NULL,
		UU_NULL,OnViewOpt,
		S_tool_library,OnNoTool,OnParm, UU_NULL, OnListCalbacks};

	static char called[] = {
		6,6, 6,6,6, 6,6,6,6,6,6, 6,6,6, 6,6,6,6, 6,6,6,6,6,6, 6,6,
		6,6,6,6,6,6};

	static char display[] = {1,1,1,
		1,1, 1,1,1, 1,1,1,1,1,1,  1,1,1, 1,1,1,1, 1,1,1,1,1,1, 1,1,
		1,1,1,1,1, 1,1,1};

	static int *ans[] = {
		(int *)Stool,(int *)Sdesc, (int *)&Stype_list,&Spseudo,&idum,
		(int *)Scut[0],(int *)Scut[1],(int *)Scut[2],
		(int *)Scut[3],(int *)Scut[4],(int *)Scut[5],
//		&idum,
		&Scfl[1],&Scfl[2],&Scfl[3],
		&Ssymfl[0],&idum,(int *)Ssymbol[0], &idum, (int *)Ssymlib,
		&Ssymfl[1],&idum,(int *)Ssymbol[1],&Ssymfl[2],&idum,(int *)Ssymbol[2],
		&ofl,&idum,
		&idum,(int *)tch,&idum, &Soutput,(int *)&Stool_list};
/*
.....Initialize routine
*/
	pspec = UU_FALSE;
	Sdefflag = UU_TRUE;
	Sload_prof = UU_FALSE;
/*
.....Command Reject
*/
	save_entry = 0;
	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		ud_free_tlist(&Stool_list);
		ud_free_flist(&Stype_list);
		nclu_tool_free();
		redisp_form = 0;
		um_close_pocket_window(UM_DRAWING_WINDOW);
		UD_initfrm_intry = save_entry;
		if (Tool_current_data.command!=NULL)
			uu_free (Tool_current_data.command);
		if (Tool_current_data.plabel!=NULL)
			uu_free (Tool_current_data.plabel);
		if (Tool_current_data.loadtl!=NULL)
			uu_free (Tool_current_data.loadtl);
		Tool_current_data.command = NULL;
		Tool_current_data.plabel = NULL;
		Tool_current_data.loadtl = NULL;
		UD_UNMARK (mainmarkval);
		return;
	}
/*
.....Open NCL Tool Library &
.....Initiate search
*/
/*
.....check if toolib loaded
.....if not load it, load file store in NCL_TOOLIB
*/
	ncl_gettool_head (&Tool_head);
	if (Stype>=Tool_head.utype_no+14)
		Stype = 1;
	S_load_tools(Sfilt-1,UU_FALSE);
	strcpy (lib, Tool_head.name);
/*
.....Load the list of tools
*/
	nclu_tool_profs(NULL);
/*
.....don't load symbols from default symlib
*/
/*	sprintf(tmplib, "%s_S", Tool_head.symlib);
	S_load_archive(UB_libdata_rec.sys_farea, tmplib);
	S_load_archive(UB_libdata_rec.loc_farea, tmplib);
*/	
	Sload_prof = UU_TRUE;
	nclu_tool_syms(0);
/*
.....Get the current cutter parameters
*/
	UM_init_f77_str(sym_str,Ssymbol[0],MAXSYMLEN);
	UM_init_f77_str(ssym_str,Ssymbol[1],MAXSYMLEN);
	UM_init_f77_str(hsym_str,Ssymbol[2],MAXSYMLEN);
	cutget(defcbuf, defdbuf,deficfl,UM_addr_of_f77_str(sym_str),
		UM_addr_of_f77_str(ssym_str),UM_addr_of_f77_str(hsym_str));
/*
.....Break out Point Vector from any symbol label
*/
	for (i=0;i<3;i++)
	{
		inum = MAXSYMLEN;
		ul_strip_blanks(Ssymbol[i],&inum);
		p = index(Ssymbol[i],'&');
		if (p != UU_NULL)
		{
			strcpy(Spv[i],p+1);
			*p = '\0';
		}
		else
			Spv[i][0] = '\0';
	}
/*
.....Blade cutter
*/
	if (defcbuf[0] == BLADE-10000)
	{
		defcbuf[0] = defcbuf[4];
		defcbuf[1] = defcbuf[3];
		defcbuf[3] = defcbuf[5];
		defcbuf[4] = 0; defcbuf[5] = 0;
		Stype = 9;
	}
/*
.....Lathe cutter
*/
	else if (defcbuf[0] == LATHE-10000)
	{
		for (i=1;i<6;i++) defcbuf[i-1] = defcbuf[i];
		defcbuf[5] = 0.;
		Stype = (int)(defdbuf[8] - 10 + 9);
	}
/*
.....Setup local cutter display flags
*/
	for (i=0;i<10;i++) Scfl[i] = deficfl[i];
	Scfl3 = Scfl[3];
	if (Stype >= 9)
	{
		traverse[FPSEU] = 0;
	}
/*
.....If a CUTTER/DISPLY is in effect
.....Then switch the real and display cutter parameters
*/
	if (deficfl[0] == 1)
	{
		for (i=0;i<6;i++)
		{
			cnew[i] = defcbuf[i];
			defcbuf[i] = defdbuf[i];
			defdbuf[i] = cnew[i];
		}
	}
	for (i=0;i<6;i++)
	{
		savdbuf[i] = defdbuf[i];
		sprintf(savdbuf_str[i],"%g",defdbuf[i]);
	}
/*
.....Set up the field entries
*/
	for (i=0;i<6;i++) sprintf(Scut[i],"%g",defcbuf[i]);
/*
........Cutter symbol
*/
	if (deficfl[0] > 1) Ssymfl[0] = UU_TRUE;
	else Ssymfl[0] = UU_FALSE;
	if (Stype >= 10 && Scfl[0] >= 2)
	{
		sprintf(Satt[0],"%g",defdbuf[18]);
		sprintf(Satt[1],"%g",defdbuf[19]);
	}
	else
	{
		strcpy(Satt[0],"0");
		strcpy(Satt[1],"0");
	}
	strcpy(Satt[2],"0");
	strcpy(Satt[3],"0");
/*
........Shank symbol
*/
	if (deficfl[4] == 0)
	{
		Ssymfl[1] = UU_FALSE;
		strcpy(Satt[4],"0");
		strcpy(Satt[5],"0");
		strcpy(Satt[6],"0");
		strcpy(Satt[7],"0");
	}
	else
	{
		Ssymfl[1] = UU_TRUE;
		sprintf(Satt[4],"%g",defdbuf[13]);
		sprintf(Satt[5],"%g",defdbuf[14]);
		sprintf(Satt[6],"%g",defdbuf[15]);
		sprintf(Satt[7],"%g",defdbuf[16]);
	}
	Sclash = (int)defdbuf[17];
/*
........Holder symbol
*/
	if (deficfl[5] == 0)
	{
		Ssymfl[2] = 0;
		strcpy(Satt[8],"0");
		strcpy(Satt[9],"0");
		strcpy(Satt[10],"0");
		strcpy(Satt[11],"0");
	}
	else
	{
		Ssymfl[2] = 1;
		sprintf(Satt[8],"%g",defdbuf[9]);
		sprintf(Satt[9],"%g",defdbuf[10]);
		sprintf(Satt[10],"%g",defdbuf[11]);
		sprintf(Satt[11],"%g",defdbuf[12]);
	}
/*
........Other fields
*/
	toolnum = 0.0;
	Stool[0] = '\0';
	Sdesc[0] = '\0';
	for (i=0;i<3;i++)
	{
		inum = MAXSYMLEN;
		ul_strip_blanks(Ssymbol[i],&inum);
		Ssymbol[i][inum] = '\0';
	}
	strcpy(defsym,Ssymbol[0]);
	ncl_get_tool_symlib(Ssymlib);

	sprintf(tmplib, "%s_S", Ssymlib);
/*
.....don't load symbols from default symlib
*/
/*
//	S_load_archive(UB_libdata_rec.sys_farea, tmplib);	
//	S_load_archive(UB_libdata_rec.loc_farea, tmplib);
//	nclu_tool_uptsyms();
*/
	strcpy(defsymlb,Ssymlib);

	ul_break_fname(lib,dir,tch2);
	ul_get_full_dir("NCL_TOOL", dir2);
	if (stricmp(dir, dir2)!=0)
	{
		ul_get_full_dir(".", dir2);
		if (stricmp(dir, dir2)==0)
			strcpy(tch, tch2);
		else
			strcpy(tch, lib);
	}
	else
		strcpy(tch, tch2);

/*
.....Setup display and traversal flags
*/
	if (toolnum>0)
	{
		if ((!Sdefflag) && (current_draw[0]!='\0'))
			traverse[FVIWT] = 1;
		else
			traverse[FVIWT] = 0;
		if (savnary>0)
			traverse[FPARM] = 1;
		else
			traverse[FPARM] = 0;
	}
	else
	{
		traverse[FVIWT] = 0;
		traverse[FPARM] = 0;
	}
	if (deficfl[0] == 1)
	{
		traverse[FDEFI] = 1;
	}
	else
		traverse[FDEFI] = 0;
	traverse[FSYM2] = traverse[FSYM3] = Ssymfl[0];
	traverse[FSHK2] = traverse[FSHK3] = Ssymfl[1];
	traverse[FHLD2] = traverse[FHLD3] = Ssymfl[2];
			
	ncl_inittool_data (&Tool_current_data);
	ncl_gettool_head (&Tool_head);
	Stype_list.answer = (char *) uu_malloc(81 * sizeof(char));
	if (Stype<0) Stype = 1;
	strcpy(Stype_list.answer, Stype_list.item[Stype]);
/*
.....Set the tool number & description
*/
form:;
	if (toolnum == 0)
	{
		Stool[0] = '\0';
		Sdesc[0] = '\0';
	}
/*
.....Get the Form input
*/
/*
.....set this value to one to know that form displayed and if call from
.....OnCutForm or OnParm, we don't need setup tool_list, draw_array
.....and some other values again, reset this value to 0 when accept
.....or canceled this cutter main form.
*/
	redisp_form = 1;
	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = OnInitForm;
	S_select_tool = 0;
	status = ud_form1("cutter.frm", ans,ans, methods, called, display, traverse);
/*
.....Set the correct cutter type
.....Based on the symbol labels
*/
	if (status != -1) nclu_tool_ctype(Ssymfl,Ssymbol,Spseudo,Scfl);
	for (i=0; i<Stype_list.num_item;i++)
	{
		if (stricmp(Stype_list.item[i], Stype_list.answer)==0)
		{
			Stype = i;
			break;
		}
	}
/*
.....Free up memory
*/
	UD_initfrm_intry = save_entry;
	ud_free_tlist(&Stool_list);
	ud_free_flist(&Stype_list);
	nclu_tool_free();
	um_close_pocket_window(UM_DRAWING_WINDOW);
	if (status==-1) goto done;
	toolnum = atof (Stool);
/*
.....if user select a tool load the tool and not changed after that
*/
	if (!Sdefflag)
	{
		S_load_toolnum(toolnum, drawng);
		goto done;
	}
	toolnum = 0;
/*
.....conside if we need temp value for symlib path
.....or profile path
*/
	symdir[0] = '\0';
	if (Scfl[0] > 1 && ((ifl == 1 || ifl1 == 1) ||
		(strcmp(Ssymbol[0],defsym) != 0 || strcmp(Ssymlib,defsymlb) != 0)))
	{
		if (Scfl[0] == 2)
		{
/*
.....Remove trailing spaces
*/
			for (inum=strlen(Ssymlib); inum>0; inum--)
			{
				if (Ssymlib[inum-1]==' ')
					Ssymlib[inum-1] = '\0';
				else
					break;
			}
			if (inum > 0)
			{
				if ((inum>=2)&&(Ssymlib[inum-2]=='_')&&(Ssymlib[inum-1]=='S'))
				{
					Ssymlib[inum-2] = '\0';
					inum -= 2;
				}
				if (inum>0)
				{
					strcat(ostr,Ssymlib);
					strcat(ostr,",");
					if (strlen(Ssymlib)>=62)
					{
						S_call_tmpval("@TX01", Ssymlib);
						strcpy(symdir, "@TX01");
					}
				}
			}
		}
	}
/*
.....else define a cutter
*/
/*
.....Initialize NCL command buffers
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Output a standard CUTTER statement
*/
	ifl = 0;
	if (Scfl[0] == 1)
	{
		for (i=0;i<6;i++)
		{
			if (savdbuf[i] != defdbuf[i]) 
				ifl = 1;
			strcpy(cstr[i],savdbuf_str[i]);
			cnew[i] = savdbuf[i];
		}
	}
	else
	{
		for (i=0;i<6;i++)
		{
			if (ncl_get_scalar_value(Scut[i],&cnew[i]) == -1)
			{
				ud_wrerr("Invalid Actual Cutter entry.");
				goto form;
			}
			if (cnew[i] != defcbuf[i]) ifl = 1;
			strcpy(cstr[i],Scut[i]);
		}
	}
	if (ifl == 1)
	{
		strcpy(ostr,"CUTTER/");
		if (Stype == 9) strcat(ostr,"BLADE,");
		else if ((Stype >= 10)&&(Stype<15)) strcat(ostr,"LATHE,");
		strcat(ostr,cstr[0]);
		for (i=5;i>0;i--)
		{
			if (cnew[i] != 0.) break;
		}
		if (Stype == 9) i = 3;
		else if (Stype >= 10 && Stype != 13 && Stype < 15) i = 4;
		if (i != 0)
		{
			for (j=1;j<=i;j++)
			{
				strcat(ostr,",");
				strcat(ostr,cstr[j]);
			}
		}
		ncl_add_token(&cmdbuf,ostr,UU_FALSE);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....Output a CUTTER/DISPLAY statement
*/
	if (ofl == 1) strcpy(tstr,"CUTTER/DISPLY,");
	else strcpy(tstr,"*CUTTER/DISPLY,");
	if (Scfl[0] == 1)
	{
		ifl1 = 0;
		for (i=0;i<6;i++)
		{
			if (ncl_get_scalar_value(Scut[i],&cnew[i]) == -1)
			{
				ud_wrerr("Invalid Actual Cutter entry.");
				goto form;
			}
			if (cnew[i] != defdbuf[i]) ifl1 = 1;
		}
		if (ifl1 == 1)
		{
			strcpy(ostr,tstr);
			strcat(ostr,Scut[0]);
			for (i=5;i>0;i--)
			{
				if (cnew[i] != 0.) break;
			}
			if (i != 0)
			{
				for (j=1;j<=i;j++)
				{
					strcat(ostr,",");
					strcat(ostr,Scut[j]);
				}
			}
			ncl_add_token(&cmdbuf,ostr,UU_FALSE);
			ncl_add_cmdbuf(&cmdbuf);
		}
	}
/*
.....Output a CUTTER/symbol statement
*/
	if (Scfl[0] > 1 && ((ifl == 1 || ifl1 == 1) ||
		(strcmp(Ssymbol[0],defsym) != 0 || strcmp(Ssymlib,defsymlb) != 0)))
	{
		strcpy(ostr,tstr);
		ncl_add_token(&cmdbuf,ostr,UU_FALSE);
		if (Scfl[0] == 2)
		{
/*
.....Remove trailing spaces
*/
			for (inum=strlen(Ssymlib); inum>0; inum--)
			{
				if (Ssymlib[inum-1]==' ')
					Ssymlib[inum-1] = '\0';
				else
					break;
			}
			if (inum > 0)
			{
				if ((inum>=2)&&(Ssymlib[inum-2]=='_')&&(Ssymlib[inum-1]=='S'))
				{
					Ssymlib[inum-2] = '\0';
					inum -= 2;
				}
				if (inum>0)
				{
					if ((strlen(Ssymlib)>=62)&&(symdir[0]!='\0'))
					{
						ncl_add_token(&cmdbuf,symdir,UU_FALSE);
					}
					else
					{
						ncl_add_token(&cmdbuf,Ssymlib,UU_FALSE);
					}
					ncl_add_token(&cmdbuf,",",UU_FALSE);
				}
			}
		}
		inum = MAXSYMLEN;
		ul_strip_blanks(Ssymbol[0],&inum);
		ncl_add_token(&cmdbuf, Ssymbol[0], UU_FALSE);		
		ostr[0] = '\0';
		if (nclu_tool_is_curve(Ssymbol[0]) && strlen(Spv[0]) != 0)
		{
			strcpy(ostr,","); strcat(lstr,Spv[0]);
		}
		if ((Stype >= 10)&&(Stype <15))
		{
			inum = strlen(Satt[0]);
			ul_strip_blanks(Satt[0],&inum);
			if (inum != 0 && strcmp(Satt[0],"0") != 0)
			{
				inum = strlen(Satt[1]);
				ul_strip_blanks(Satt[1],&inum);
				sprintf(lstr,",OFFSET,%s,%s",Satt[0],Satt[1]);
				strcat(ostr,lstr);
			}
		}
		if (ostr[0]!='\0')
			ncl_add_token(&cmdbuf,ostr,UU_FALSE);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....Output a CUTTER/SHANK statement
*/
	if (Ssymfl[1])
	{
		strcpy(ostr,tstr);
		if (Stype <= 9)
		{
			if (Scfl[4] < 2)
			{
				strcpy(lstr,Satt[4]);
				strcat(lstr,","); strcat(lstr,Satt[5]);
				strcat(lstr,","); strcat(lstr,Satt[6]);
				strcat(lstr,","); strcat(lstr,Satt[7]);
			}
			else
			{
				inum = strlen(Ssymbol[1]);
				ul_strip_blanks(Ssymbol[1],&inum);
				strcpy(lstr,Ssymbol[1]);
				if (nclu_tool_is_curve(Ssymbol[1]) && strlen(Spv[1]) != 0)
				{
					inum = strlen(Spv[1]);
					ul_strip_blanks(Spv[1],&inum);
					strcat(lstr,","); strcat(lstr,Spv[1]);
				}
				strcat(lstr,","); strcat(lstr,Satt[4]);
			}
		}
		else
		{
			if (Scfl[4] < 2)
			{
				strcpy(lstr,Satt[4]);
				strcat(lstr,","); strcat(lstr,Satt[5]);
				strcat(lstr,","); strcat(lstr,Satt[6]);
				strcat(lstr,","); strcat(lstr,Satt[7]);
			}
			else
			{
				inum = strlen(Ssymbol[1]);
				ul_strip_blanks(Ssymbol[1],&inum);
				strcpy(lstr,Ssymbol[1]);
				strcat(lstr,","); strcat(lstr,Satt[4]);
				strcat(lstr,","); strcat(lstr,Satt[5]);
				strcat(lstr,","); strcat(lstr,"OFFSET");
				strcat(lstr,","); strcat(lstr,Satt[6]);
				strcat(lstr,","); strcat(lstr,Satt[7]);
			}
		}
		strcat(ostr,"SHANK,");
		strcat(ostr,lstr);
		if (Sclash == 0)
			strcat(ostr,",CUTTER");
		else
			strcat(ostr,",HOLDER");
		ncl_add_token(&cmdbuf,ostr,UU_FALSE);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....Output a CUTTER/HOLDER statement
*/
	if (Ssymfl[2])
	{
		strcpy(ostr,tstr);
		if (Stype <= 9)
		{
			if (Scfl[5] < 2)
			{
				strcpy(lstr,Satt[8]);
				strcat(lstr,","); strcat(lstr,Satt[9]);
				strcat(lstr,","); strcat(lstr,Satt[10]);
				strcat(lstr,","); strcat(lstr,Satt[11]);
			}
			else
			{
				inum = strlen(Ssymbol[2]);
				ul_strip_blanks(Ssymbol[2],&inum);
				strcpy(lstr,Ssymbol[2]);
				if (nclu_tool_is_curve(Ssymbol[2]) && strlen(Spv[2]) != 0)
				{
					inum = strlen(Spv[2]);
					ul_strip_blanks(Spv[2],&inum);
					strcat(lstr,","); strcat(lstr,Spv[2]);
				}
				strcat(lstr,","); strcat(lstr,Satt[8]);
			}
		}
		else
		{
			if (Scfl[5] < 2)
			{
				strcpy(lstr,Satt[8]);
				strcat(lstr,","); strcat(lstr,Satt[9]);
				strcat(lstr,","); strcat(lstr,Satt[10]);
				strcat(lstr,","); strcat(lstr,Satt[11]);
			}
			else
			{
				inum = strlen(Ssymbol[2]);
				ul_strip_blanks(Ssymbol[2],&inum);
				strcpy(lstr,Ssymbol[2]);
				strcat(lstr,","); strcat(lstr,Satt[8]);
				strcat(lstr,","); strcat(lstr,Satt[9]);
				strcat(lstr,","); strcat(lstr,"OFFSET");
				strcat(lstr,","); strcat(lstr,Satt[10]);
				strcat(lstr,","); strcat(lstr,Satt[11]);
			}
		}
		strcat(ostr,"HOLDER,");
		strcat(ostr,lstr);
		ncl_add_token(&cmdbuf,ostr,UU_FALSE);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....Output a CUTTER/(PART,ALL) statement
*/
	if (Scfl[1] != deficfl[1])
	{
		strcpy(ostr,tstr);
		if (Scfl[1] == 0) strcat(ostr,"PART");
		else strcat(ostr,"ALL");
		ncl_add_token(&cmdbuf,ostr,UU_FALSE);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....Output a CUTTER/MOVE statment
*/
	if (Scfl[2] != deficfl[2])
	{
		strcpy(ostr,tstr);
		strcat(ostr,"MOVE,");
		if (Scfl[2] == 0) strcat(ostr,"OFF");
		else strcat(ostr,"ON");
		ncl_add_token(&cmdbuf,ostr,UU_FALSE);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....Output a CUTTER/SHADE statment
*/
	if (Scfl[3] != deficfl[3] || Scfl[6] != deficfl[6] || Scfl[7] != deficfl[7])
	{
		strcpy(lstr,tstr);
		strcat(lstr,"SHADE,");
		if (Scfl[3] == Scfl[6] && Scfl[3] == Scfl[7])
		{
			strcpy(ostr,lstr);
			if (Scfl[3] == 0) strcat(ostr,"OFF");
			else strcat(ostr,"ON");
			ncl_add_token(&cmdbuf,ostr,UU_FALSE);
			ncl_add_cmdbuf(&cmdbuf);
		}
		else
		{
			if (Scfl[3] != deficfl[3])
			{
				strcpy(ostr,lstr);
				if (Scfl[3] == 0) strcat(ostr,"OFF");
				else strcat(ostr,"ON");
				strcat(ostr,",CUTTER");
				ncl_add_token(&cmdbuf,ostr,UU_FALSE);
				ncl_add_cmdbuf(&cmdbuf);
			}
			if (Scfl[6] != deficfl[6])
			{
				strcpy(ostr,lstr);
				if (Scfl[6] == 0) strcat(ostr,"OFF");
				else strcat(ostr,"ON");
				strcat(ostr,",SHANK");
				ncl_add_token(&cmdbuf,ostr,UU_FALSE);
				ncl_add_cmdbuf(&cmdbuf);
			}
			if (Scfl[7] != deficfl[7])
			{
				strcpy(ostr,lstr);
				if (Scfl[7] == 0) strcat(ostr,"OFF");
				else strcat(ostr,"ON");
				strcat(ostr,",HOLDER");
				ncl_add_token(&cmdbuf,ostr,UU_FALSE);
				ncl_add_cmdbuf(&cmdbuf);
			}
		}
	}
/*
.....Process CUTTER statement(s)
*/
	ncl_call(&cmdbuf);
	if (symdir[0]!='\0')
	{
		getkey(symdir, &nclkey);
		dtdele (&nclkey);
	}
	goto done;
done:;
	if (Tool_current_data.command!=NULL)
		uu_free (Tool_current_data.command);
	if (Tool_current_data.plabel!=NULL)
		uu_free (Tool_current_data.plabel);
	if (Tool_current_data.loadtl!=NULL)
		uu_free (Tool_current_data.loadtl);
	Tool_current_data.command = NULL;
	Tool_current_data.plabel = NULL;
	Tool_current_data.loadtl = NULL;
	redisp_form = 0;
	UD_UNMARK(mainmarkval);
	return;

}
/*********************************************************************
**    S_FUNCTION     :  OnInitForm(fieldno,val,stat)
**       Method called at when Cutter form is initialized.
**    PARAMETERS
**       INPUT  :
**          fieldno  Ignored.
**          val      Ignored.
**          stat     Ignored.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnInitForm(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char disp[41],trav[41], file[UX_MAX_PATH_LEN];
	int fldno, status;
	UD_TABLEINFO info;
	UD_DDATA seldata;
	UD_ITEMDATA data;
	UD_TLIST *dlist;

	UD_initfrm_intry = UU_NULL;
	if (Stype>=Tool_head.utype_no+14)
		Stype = 1;
//	ud_update_answer(FLIST,(int *)&Stool_list);	
	dlist = (UD_TLIST *)&Stool_list;
	fldno = FLIST;
	info.flag = 1;
	info.frmid = 0;
	info.fldid = -1;
	info.col = 0;
	info.row = 0;
	info.data_ptr = (int*)&(dlist->data[0]);
	seldata.frmint = (int *)&info;
	OnListCalbacks(&fldno, &seldata, UD_FLDOK);	

	sprintf(file, "toolib_%s.jpg", Stype_list.item[Stype]);
/*
.....we need check if the file exist first, otherwise
.....use tool_custom_type.jpg instead
.....open for read to see if ok
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "forms", "UD_FORMDIR", (char*)UU_NULL, 
		file, 0, (FILE**)UU_NULL);
	if (status!=0)
		strcpy(file, "tool_custom_type.jpg");
	ud_update_frmpic(0, file);
	OnCtypeChg(Stype,disp,trav,UU_TRUE);
/*
.....set the init sort method for cutter list
*/
/*
......if saved_info.col<0, mean the never sorted by user, don't
......changed here, QAR97241 item A
*/
	if ((saved_info.col==0)&&(toolno_click%2==0))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc2);
	}
	else if ((saved_info.col==0)&&(toolno_click%2))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc);
	}
	if ((saved_info.col==1)&&(type_click%2==0))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc2);
	}
	else if ((saved_info.col==1)&&(type_click%2))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc);
	}
	if ((saved_info.col==2)&&(descript_click%2==0))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc2);
	}
	else if ((saved_info.col==2)&&(descript_click%2))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc);
	}
	if ((saved_info.col==3)&&(diam_click%2==0))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc2);
	}
	else if ((saved_info.col==3)&&(diam_click%2))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc);
	}
	if ((saved_info.col==4)&&(rad_click%2==0))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc2);
	}
	else if ((saved_info.col==4)&&(rad_click%2))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc);
	}
	if ((saved_info.col==5)&&(hgt_click%2==0))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc2);
	}
	else if ((saved_info.col==5)&&(hgt_click%2))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc);
	}
	if ((saved_info.col==6)&&(angle_click%2==0))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc2);
	}
	else if ((saved_info.col==6)&&(angle_click%2))
	{
		ud_setform_sortfunc(0, FLIST, (UD_SMETHOD)SortFunc);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnCloseparm()
**       Method called at when define form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnCloseparm()
{
	int i;
	int idid;

	parm_form_dsp = 0;

	idid = UU_FALSE;
	for (i=0;i<parm_nary+1;i++)
	{
		if (i == 0)
			strcpy(parmstr[i], parm_cstr[i]);
		else
		{
			idid = UU_TRUE;
			strcpy(parmstr[i], parm_cstr[i]);
		}
	}
	pspec = idid;

	uu_lsdel(frm_ptr);
	if (ans_ptr != UU_NULL) uu_free(ans_ptr);
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  static OnParm(fieldno, val, stat)
**       Method called at when pushed "Parameter..." button
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnParm(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,iary[20];
	int ivis[21];
	int kerr, status, disp_parm;
	char drawng[64];
	char cparms[20][80];
	int internal;

	static UD_METHOD methods[50];
	static char called[50];

	if (parm_form_dsp)
		return(UD_FLDOK);
	if (Tool_head.no_tools==0) goto done;
	if (toolnum==0) goto done;

	if (Tool_current_data.toolno==0) goto done;
/*
.....always using internal form
.....Yurong 2/4/03
*/
	kerr = ncl_findtl(toolnum, drawng, iary, parm_fcstr, &parm_nary);
	if (kerr==-1) goto done;
	kerr = ncl_gettparms(toolnum, cparms);

	internal = 1;
	if ((parm_nary > 0)&&(internal==1))
	{
		sprintf(parm_cstr[0],"%-15.0f",toolnum);
		for (i=0;i<20;i++)
		{
			ivis[i+1] = iary[i];
			if (pspec)
				strcpy(parm_cstr[i+1], parmstr[i+1]);
			else
			{
				strcpy(parm_cstr[i+1], parm_fcstr[i]);
			}
		}
		ans_ptr = (int **)uu_malloc(sizeof(ans_ptr)*(parm_nary+1));
		ans_ptr[0] = (int *)&parm_cstr[0][0];
		
		disp_parm = 0;
		for (i=1, j=1; (i<21) && (j<parm_nary+1); i++)
		{
			if (ivis[i]==1)
			{
				ans_ptr[j++] = (int *)&parm_cstr[i][0];
				disp_parm++;
			}
			else
				continue;
		}
		frm_ptr = (char *)uu_lsnew();
		if (frm_ptr == 0)
		{
			ud_wrerr("Could not allocate memory for the form.");
			goto done;
		}
		frm_ptr = (char *)uu_lsinsrt(frm_ptr,10000);
		for(i=0;i<120;i++) frm_ptr[i] = 0;
		for (i=0;i<20;i++)
		{
			if (strlen (cparms[i])==0)
				sprintf(cparms[i],"Parameter #%2d:",i+1);
		}
		S_form_loadcut(cparms, ivis);
		for (i=0; i<50;i++)
		{
			methods[i] = UU_NULL;
			called[i] = 6;
		}
		methods[disp_parm+1] = OnCloseparm;
		parm_form_dsp = 1;
		status = ud_form_display1("INTERNAL.INTERNAL", ans_ptr,ans_ptr, methods, called, UU_NULL, UU_NULL);
	}
	if (status==-1)
	{
		parm_form_dsp = 0;
		goto done;
	}
/*
.....End of routine
*/
done:;
	*fieldno = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClosedef()
**       Method called at when define form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClosedef()
{
	int i;
	UU_LOGICAL chg;
	UM_real8 cnew[6];

	cut_form_dsp = 0;
/*
.....form accept, save data
*/
	chg = UU_FALSE;
	for (i=0; i<6; i++)
	{
		if (ncl_get_scalar_value(def_cstr[i],&cnew[i]) == -1)
		{
			ud_wrerr("Invalid Actual Cutter entry.");
			return(UD_BADTYP);
		}
		if ((def_cstr[i][0]!='\0') && (cnew[i] != tempdbuf[i])) 
		{
			chg = UU_TRUE;
			savdbuf[i] = cnew[i];
			strcpy(savdbuf_str[i],def_cstr[i]);
		}
	}
	if (chg)
	{
		Sdefflag = UU_TRUE;
		toolnum = 0;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static OnCutForm()
**       display a cutter define form
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void OnCutForm()
{
	int i, stat;
	UD_METHOD save_entry;
	static UD_METHOD methods[] = {UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL, UU_NULL, OnClosedef};
	static char called[] = {6,6,6,6,6,6, 6};
	static char traverse[] = {1,1,1,1,1,1};
	static char display[] = {1,1,1,1,1,1};
	static int *ans[] = {(int *)&def_cstr[0][0],
								(int *)&def_cstr[1][0], (int *)&def_cstr[2][0],
								(int *)&def_cstr[3][0],
								(int *)&def_cstr[4][0], (int *)&def_cstr[5][0]};
	if (cut_form_dsp)
		return;
	for (i=0; i<6; i++)
	{
		if (Sdefflag)
			strcpy(def_cstr[i],savdbuf_str[i]);
		else
		{
			if (Tool_current_data.toolno!=0)
				sprintf(def_cstr[i], "%g", Tool_current_data.pseudo[i]);
			else
			{
				strcpy(def_cstr[i], "0.");
				tempdbuf[i] = 0.;
			}
		}
	}
/*
.....Pop up cutter actural define form
*/
	cut_form_dsp = 1;
	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = NULL;
	stat = ud_form_display1("cutterdef.frm",ans,ans, methods, called, display,
				traverse);
	UD_initfrm_intry = save_entry;
	if (stat==-1)  goto err1;
	goto done;
err1:;
	ud_wrerr("Could not create cutter define form.");
	cut_form_dsp = 0;
	return;
done:;
	return;
}

/*********************************************************************
**    S_FUNCTION     :  static OnPseudoDef(fieldno, val, stat)
**       Method called at when pushed "Define..." button
**                      side with Psuedo Cutter field
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnPseudoDef(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	OnCutForm();
	*fieldno = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static OnPseudo(fieldno, val, stat)
**       Method called at when  Psuedo Cutter toggle button is selected
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnPseudo(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	Spseudo = val->frmint[0];
	if (val->frmint[0]==1)
	{
		ud_set_traverse_mask(FDEFI, UU_TRUE);
		OnCutForm();
	}
	else
		ud_set_traverse_mask(FDEFI, UU_FALSE);
	return UD_FLDOK;
}

/*********************************************************************
**    S_FUNCTION     :  static OnShade(fieldno, val, stat)
**       Method called when Shaded field is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnShade(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (val->frmint[0] != Scfl3)
	{
		nclu_tool_shade_fields(val->frmint[0],-1,-1);
		Scfl3 = val->frmint[0];
	}
	return UD_FLDOK;
}

/*********************************************************************
**    S_FUNCTION     :  OnShank (fieldno, val, stat)
**       Method called when Symbol, Shank, or Holder button is pressed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnShank(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int len, ifl,ipt[3],ipt1[3],inc,fld[3],libfld, ityp;
	char tmplib[UX_MAX_PATH_LEN];
	ifl = 0;
	if (*fieldno == FSHK2) ifl = 1;
	if (*fieldno == FHLD2) ifl = 2;
/*
.....Display the symbol form
*/
	ipt[0] = 0; ipt[1] = 4; ipt[2] = 5;
	ipt1[0] = 3; ipt1[1] = 6; ipt1[2] = 7;
	inc = ifl * 4;
	fld[0] = FSYM3; fld[1] = FSHK3; fld[2] = FHLD3;
	libfld = FSLIB;

	ityp = 0; if (*fieldno == FSHK2) ityp = 1;
/*
.....Remove trailing spaces
*/
	for (len=strlen(Ssymlib); len>0; len--)
	{
		if (Ssymlib[len-1]==' ')
			Ssymlib[len-1] = '\0';
		else
			break;
	}
	if ((len>=2)&&(Ssymlib[len-2]=='_')&&(Ssymlib[len-1]=='S'))
	{
		Ssymlib[len-2] = '\0';
		len -= 2;
	}
/*do not update symbol list */	
/*	if (len>0)
	{
		sprintf(tmplib, "%s_S", Ssymlib);
		S_load_archive(UB_libdata_rec.sys_farea, tmplib);	
		S_load_archive(UB_libdata_rec.loc_farea, tmplib);
		nclu_tool_uptsyms();
	}
*/
	nclu_tool_form(ifl,Ssymbol[ifl],Spv[ifl],&Scfl[ipt1[ifl]],&inc,&inc,&Sclash,
		Satt[inc],&Stype,&Scfl[ipt[ifl]],&Spseudo,fld[ifl],ityp, Ssymlib, libfld);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static OnChoice(fieldno, val, stat)
**       Method called when tool symbol checkbox is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnChoice(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Modify the active fields
*/
	switch (*fieldno)
	{
	case FSYM1:
		Ssymfl[0] = val->frmint[0];
		ud_set_traverse_mask(FSYM2,Ssymfl[0]);
		ud_set_traverse_mask(FSYM3,Ssymfl[0]);
		if (!Ssymfl[0]) nclu_tool_close_form(0);
		break;
	case FSHK1:
		Ssymfl[1] = val->frmint[0];
		ud_set_traverse_mask(FSHK2,Ssymfl[1]);
		ud_set_traverse_mask(FSHK3,Ssymfl[1]);
		if (!Ssymfl[1]) nclu_tool_close_form(1);
		break;
	case FHLD1:
		Ssymfl[2] = val->frmint[0];
		ud_set_traverse_mask(FHLD2,Ssymfl[2]);
		ud_set_traverse_mask(FHLD3,Ssymfl[2]);
		if (!Ssymfl[2]) nclu_tool_close_form(2);
		break;
	}
/*
.....Reset the active tool
*/
	OnNoTool(fieldno,val,stat);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnNoTool(fieldno, val, stat)
**       Method called when any field that overrides the tool library
**       selection is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnNoTool(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (S_select_tool==1)
		return(UD_FLDOK);
/*
.....if those field already setup,
.....we don't need do anything about it
*/
	if (!Sdefflag)
	{
		ud_set_traverse_mask(FPARM,UU_FALSE);
		ud_set_traverse_mask(FVIWT,UU_FALSE);
/*
......why set FTOOL, FDESC field, it is alrady accept the field value as Stool, Sdesc
......when we leave those edit field, so if the toolno field changed, it aleady sets
......Stool to that number. I don't really understand why put following code
......question
*/
/*
.....if def tool changed, reset Stool & Sdesc
*/
		Stool[0] = '\0';
		Sdesc[0] = '\0';
		ud_update_answer(FTOOL, (int*)&Stool);
		ud_update_answer(FDESC, (int*)&Sdesc);
		Sdefflag = UU_TRUE;
	}
	ncl_reinit_tldata();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSymbol(fieldno, val, stat)
**       Method called when the Shank or Holder buttons are pressed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSymbol(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....if those field already setup,
.....we don't need do anything about it
*/
	if (!Sdefflag)
	{
		ud_set_traverse_mask(FPARM,UU_FALSE);
		ud_set_traverse_mask(FVIWT,UU_FALSE);
		ud_update_answer(FTOOL, (int*)&Stool);
		ud_update_answer(FDESC, (int*)&Sdesc);
		Sdefflag = UU_TRUE;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static OnCutType(fieldno, val, stat)
**       Method called at when cutter type is changed
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  OnCutType(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char disp[41],trav[41], file[UX_MAX_PATH_LEN];
	int i,choice, status;
	if (*fieldno!=FTYPE)
		return(UD_FLDOK);
/*
.....val->frmint contains choice
*/
	if (val->frmstr[0]=='\0')
		return(UD_FLDOK);
	choice = -1;
	for (i=0; i<Stype_list.num_item;i++)
	{
		if (stricmp(Stype_list.item[i], val->frmstr)==0)
		{
			choice = i;
			break;
		}
	}
/*
.....user define, since we don't save the type
.....we can just an user define type
*/
	if (choice==-1)
	{
		choice = Stype_list.num_item+1;
		sprintf(file, "toolib_%s.jpg", val->frmstr);
	}
	else
		sprintf(file, "toolib_%s.jpg", Stype_list.item[choice]);
/*
.....we need check if the file exist first, otherwise
.....use tool_custom_type.jpg instead
.....open for read to see if ok
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "forms", "UD_FORMDIR", (char*)UU_NULL, 
		file, 0, (FILE**)UU_NULL);
	if (status!=0)
		strcpy(file, "tool_custom_type.jpg");
	ud_update_frmpic(0, file);
	OnCtypeChg(choice,disp,trav,UU_TRUE);
/*
.....clean toolnum and description field and
.....disable view tool and parameter field
.....if those field already setup,
.....we don't need do anything about it
*/
/*
.....only if this called is not from OnSelect callback, it will set the toolno and cutter type, thus call to here
*/
	if (S_select_tool==0)
		OnNoTool(fieldno,val,stat);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static OnCtypeChg(choice,disp,trav,update)
**       Sets the display and traverse values for the cutter definition
**       fields based on the cutter type.
**    PARAMETERS
**       INPUT  :
**          choice = Cutter type.
**          update = UU_TRUE = Call routines to update fields in active
**                   form.  Should be UU_FALSE if the form is not active
**                   yet.
**       OUTPUT :
**          disp   = Display flags for each cutter field.
**          trav   = Traverse flags for each cutter field.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void OnCtypeChg(choice,disp,trav,update)
int choice;
char disp[],trav[];
UU_LOGICAL update;
{
	int i,ifl;
	char spr[6][20];

	Stype = choice;
/*
.....Set the display and traverse masks
........First the default settings
*/
	disp[FDIA1] = UU_TRUE;
	disp[FRAD1] = UU_TRUE;
	disp[FHGT1] = UU_TRUE;
	disp[FANG1] = UU_TRUE;
	disp[FZHT1] = UU_TRUE;
	disp[FLAT1] = UU_TRUE;

	trav[FDIA1] = UU_TRUE;
	trav[FRAD1] = UU_TRUE;
	trav[FHGT1] = UU_TRUE;
	trav[FANG1] = UU_FALSE;
	trav[FZHT1] = UU_FALSE;
	trav[FLAT1] = UU_FALSE;

	strcpy(spr[0],"Diameter:");
	strcpy(spr[1],"Corner Rad:");
	strcpy(spr[2],"Height:");
	strcpy(spr[3],"Side Angle:");
	strcpy(spr[4],"Z-Height:");
	strcpy(spr[5],"Flat Angle:");
/*
........Barrel Cutter
*/
	switch (choice)
	{
	case 2:
		strcpy(spr[3],"Side Radius:");
		strcpy(spr[4],"Z-Height:");
		trav[FANG1] = UU_TRUE;
		trav[FZHT1] = UU_TRUE;
		trav[FLAT1] = UU_TRUE;
		break;
/*
........Cone Cutter
........Bell Cutter
*/
	case 3:
	case 4:
		trav[FANG1] = UU_TRUE;
		break;
/*
........Drill
........Chamfer Tool
*/
	case 5:
	case 8:
		trav[FANG1] = UU_TRUE;
		trav[FRAD1] = UU_FALSE;
		break;
/*
........Blade
*/
	case 9:
		strcpy(spr[0],"Width:");
		strcpy(spr[1],"Chizel:");
		strcpy(spr[3],"Angle:");
		trav[FANG1] = UU_TRUE;
		break;
/*
........Square Insert
........Diamond Insert
........Triangle Insert
*/
	case 10:
	case 11:
	case 12:
		strcpy(spr[0],"Radius:");
		strcpy(spr[1],"Diameter:");
		strcpy(spr[3],"Angle:");
		strcpy(spr[4],"Mount Angle:");
		if (choice == 11) trav[FANG1] = UU_TRUE;
		trav[FZHT1] = UU_TRUE;
		break;
/*
........Circular Insert
*/
	case 13:
		strcpy(spr[0],"Radius:");
		strcpy(spr[1],"Diameter:");
		strcpy(spr[3],"Angle:");
		strcpy(spr[4],"Mount Angle:");
		trav[FRAD1] = UU_FALSE;
		break;
/*
........Grooving Tool
*/
	case 14:
		strcpy(spr[0],"Radius:");
		strcpy(spr[1],"Diameter:");
		strcpy(spr[3],"Angle:");
		strcpy(spr[4],"Length:");
		trav[FZHT1] = UU_TRUE;
		break;
/*
........Face Mill
........End Mill
........Boring Tool
........Reamer
*/
	default:
		if (choice>14)
			trav[FANG1] = UU_TRUE;
		break;
	}
/*
.....Update picture
*/
	if (update)
	{
/*
.....Update display & traverse masks in form
*/
		for (i=FDIA1;i<=FLAT1;i++)
		{
			ifl = disp[i];
			ud_set_display_mask(UD_INPUTF,i,ifl);
			ifl = trav[i];
			ud_set_traverse_mask(i,ifl);
			ud_update_prompt(i,spr[i-FDIA1]);
		}
/*
.....Update Pseudo Cutter fields
*/
		if ((Stype >= 9)&&(Stype < 15))
		{
			ifl = 0;
			ud_set_traverse_mask(FPSEU,UU_FALSE);
			ud_set_traverse_mask(FDEFI,UU_FALSE);
			ud_update_answer(FPSEU, (int*)&ifl);
		}
		else
		{
			ud_set_traverse_mask(FPSEU,UU_TRUE);
			ud_set_traverse_mask(FDEFI,Spseudo);
		}
		
/*
.....Update lathe insert fields
*/
		if ((choice >= 10)&&(choice < 15))
		{
			switch (choice)
			{
/*
........Square Insert
*/
			case 10:
				strcpy(Scut[3],"90.");
				ud_update_answer(FANG1,(int*)&Scut[3]);
				break;
/*
........Triangle Insert
*/
			case 12:
				strcpy(Scut[3],"60.");
				ud_update_answer(FANG1,(int*)&Scut[3]);
				break;
/*
........Circular Insert
*/
			case 13:
				strcpy(Scut[1],"0.");
				ud_update_answer(FRAD1,(int*)&Scut[1]);
				strcpy(Scut[3],"0.");
				ud_update_answer(FANG1,(int*)&Scut[3]);
				strcpy(Scut[4],"0.");
				ud_update_answer(FZHT1,(int*)&Scut[4]);
				break;
/*
........Grooving Tool
*/
			case 14:
				strcpy(Scut[3],"0.");
				ud_update_answer(FANG1,(int*)&Scut[3]);
				break;
			}
		}
/*
.....Set attach point fields
*/
		nclu_tool_shank_fields(UU_FALSE);
	}
}
/*********************************************************************
**   I_FUNCTION: SortFunc()
**      Sort the list on Select toollist form in partical order
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static int SortFunc(char* cData1, char* cData2, int lParamSort)
{
	UD_ITEMDATA* pData1, *pData2;
	int nRetVal;
	UU_REAL val1, val2;

	pData1 = (UD_ITEMDATA*)cData1;
	pData2 = (UD_ITEMDATA*)cData2;
	nRetVal = 0;

/*
......"toolno" "type" "Description"
*/
	switch(lParamSort)
	{
	case 0:
/*
.....by value
*/
		val1 = atof (pData1->data_items[0]);
		val2 = atof (pData2->data_items[0]);
		if (val1>val2)
			nRetVal = 1;
		else if (val1==val2)
			nRetVal = 0;
		else
			nRetVal = -1;
		break;
	case 1:	
		nRetVal = strcmp(pData1->data_items[1],
                                 pData2->data_items[1]);
		break;
	case 2:
		nRetVal = strcmp(pData1->data_items[2],
                                 pData2->data_items[2]);
		break;
	case 3:	
		nRetVal = strcmp(pData1->data_items[3],
                                 pData2->data_items[3]);
		break;
	case 4:
		nRetVal = strcmp(pData1->data_items[4],
                                 pData2->data_items[4]);
		break;
	case 5:	
		nRetVal = strcmp(pData1->data_items[5],
                                 pData2->data_items[5]);
		break;
	case 6:
		nRetVal = strcmp(pData1->data_items[6],
                                 pData2->data_items[6]);
		break;
	default:
		break;
	}
	return nRetVal;
}

/*********************************************************************
**   I_FUNCTION: SortFunc2()
**      Sort the list on Select Scalar form in partical order
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static int SortFunc2(char* cData1, char* cData2, int lParamSort)
{
	UD_ITEMDATA* pData1, *pData2;
	int nRetVal;
	UU_REAL val1, val2;
	pData1 = (UD_ITEMDATA*)cData1;
	pData2 = (UD_ITEMDATA*)cData2;
	nRetVal = 0;

/*
......"toolno" "type" "Description"
*/
	switch(lParamSort)
	{
	case 0:
/*
.....by value
*/
		val1 = atof (pData1->data_items[0]);
		val2 = atof (pData2->data_items[0]);
		if (val1<val2)
			nRetVal = 1;
		else if (val1==val2)
			nRetVal = 0;
		else
			nRetVal = -1;
		break;
	case 1:	
		nRetVal = -strcmp(pData1->data_items[1],
                                 pData2->data_items[1]);
		break;
	case 2:
		nRetVal = -strcmp(pData1->data_items[2],
                                 pData2->data_items[2]);
		break;
	case 3:	
		nRetVal = -strcmp(pData1->data_items[3],
                                 pData2->data_items[3]);
		break;
	case 4:
		nRetVal = -strcmp(pData1->data_items[4],
                                 pData2->data_items[4]);
		break;
	case 5:	
		nRetVal = -strcmp(pData1->data_items[5],
                                 pData2->data_items[5]);
		break;
	case 6:
		nRetVal = -strcmp(pData1->data_items[6],
                                 pData2->data_items[6]);
		break;
	default:
		break;
	}
	return nRetVal;
}
/*********************************************************************
**   I_FUNCTION: resort_table()
**      resort the list as last appeared after reload/filter on cutter form.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static void resort_table()
{
	UD_TABLEINFO info;
/*
.....if the table does not resort before, don't resort now
*/
	if ((toolno_click==0)&&(type_click==0)
		&&(descript_click==0)&&(diam_click==0)
		&&(rad_click==0)&&(hgt_click==0)
		&&(angle_click==0))
		return;
	
	info.flag = saved_info.flag;
	info.frmid = saved_info.frmid;
	info.fldid = saved_info.fldid;
	info.col = saved_info.col;
	info.row = -1;
		
	if ((info.col==0)&&(toolno_click%2==0))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc2);
	}
	else if ((info.col==0)&&(toolno_click%2))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc);
	}
	if ((info.col==1)&&(type_click%2==0))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc2);
	}
	else if ((info.col==1)&&(type_click%2))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc);
	}
	if ((info.col==2)&&(descript_click%2==0))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc2);
	}
	else if ((info.col==2)&&(descript_click%2))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc);
	}
}

/*********************************************************************
**    S_FUNCTION     :  static OnListCalbacks(fieldno, val, stat)
**       Method called for tool list table
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnListCalbacks(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int sel, toolno, status;
	UD_ITEMDATA *data;
	UD_TABLEINFO *info = (UD_TABLEINFO *)(val->frmint);
	if (info->flag==1)
	{
/*
......list selected, doing selection callback
......if it is un-select, info->row = -1;
*/
		Stool_list.answer = sel = info->row;
		data = (UD_ITEMDATA *)(info->data_ptr);
		if (sel>=0)
			toolno = atoi(data->data_items[0]);
		else
			toolno = -1;
		status = OnToolSel(toolno);
		if (status==0)
		{
/*
.....tool selected, updated data and redisplayed inside OnToolSel
.....so don't redisplay again
*/
			*fieldno = -1;
		}
	}
	else
	{
/*
......remember the last sort info, we need it when we reload the list
*/
		saved_info.flag = info->flag;
		saved_info.frmid = info->frmid;
		saved_info.fldid = info->fldid;
		saved_info.col = info->col;
/*
......column button is pushed, doing sorting
*/
		if ((info->col==0)&&(toolno_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			toolno_click++;
		}
		else if ((info->col==0)&&(toolno_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			toolno_click++;
		}
		if ((info->col==1)&&(type_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			type_click++;
		}
		else if ((info->col==1)&&(type_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			type_click++;
		}
		if ((info->col==2)&&(descript_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			descript_click++;
		}
		else if ((info->col==2)&&(descript_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			descript_click++;
		}
		if ((info->col==3)&&(diam_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			diam_click++;
		}
		else if ((info->col==3)&&(diam_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			diam_click++;
		}
		if ((info->col==4)&&(rad_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			rad_click++;
		}
		else if ((info->col==4)&&(rad_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			rad_click++;
		}
		if ((info->col==5)&&(hgt_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			hgt_click++;
		}
		else if ((info->col==5)&&(hgt_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			hgt_click++;
		}
		if ((info->col==6)&&(angle_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			angle_click++;
		}
		else if ((info->col==6)&&(angle_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			angle_click++;
		}
	}
	return(UD_FLDOK);
}


OnToolSel(toolno)
int toolno;
{
	int fieldno;
	UD_DDATA val;
	int stat;
	char temp[82], temp2[82], *tok, *strtok(), sym[MAXSYMLEN];
	char disp[41],trav[41];
	UM_f77_str f77_str;
	char drawng[64];
	int i;
	UM_int4 iary[20];
	UM_int4 icfl[10];
	UM_real8 cbuf[6],dbuf[20];

	pspec = UU_FALSE;
	toolnum = toolno;
/*
.....first should load the toolib and get the value
*/
	for (i=0;i<6;i++) Tool_current_data.pseudo[i] = 0.0;
/*
......using C unibase routine now
......Yurong
*/
	if (toolnum>0)
	{
		if (Tool_current_data.command!=NULL)
			uu_free (Tool_current_data.command);
		if (Tool_current_data.plabel!=NULL)
			uu_free (Tool_current_data.plabel);
		if (Tool_current_data.loadtl!=NULL)
			uu_free (Tool_current_data.loadtl);
		Tool_current_data.command = NULL;
		Tool_current_data.plabel = NULL;
		Tool_current_data.loadtl = NULL;
		ncl_get_tooldata (toolnum, &Tool_current_data);
		ncl_findtl(toolnum, drawng, iary, parm_fcstr, &parm_nary);
		
		Stype = Tool_current_data.ctype;
		if (stricmp(Stype_list.answer, Stype_list.item[Stype])!=0)
		{
			strcpy(Stype_list.answer, Stype_list.item[Stype]);
		}
/*
.....set flag here since cutter type change will cause the cutter type callback
*/
		S_select_tool = 1;
		Scfl[3] = Tool_current_data.shade;
		Scfl[6] = Tool_current_data.sshade;
		Scfl[7] = Tool_current_data.hshade;
		Scfl[1] = Tool_current_data.segments;
		Scfl[2] = Tool_current_data.move;
		Spseudo = Tool_current_data.fpseudo;
		Ssymfl[0] = Tool_current_data.fsymbol;
		Ssymfl[1] = Tool_current_data.fshank;
		Ssymfl[2] = Tool_current_data.fholder;
		if (Tool_current_data.symbol[0]!='\0')
		{
			strcpy(Ssymbol[0], Tool_current_data.symbol);
			Scfl[0] = 2;
		}
		else
		{
			Ssymbol[0][0] = '\0';
			Scfl[0] = Tool_current_data.fpseudo;
		}
		if (Tool_current_data.fshank)
		{
			Ssymfl[1] = 1;
			if (Tool_current_data.symshk[0]!='\0')
			{
				strcpy(Ssymbol[1], Tool_current_data.symshk);
				Scfl[4] = 2;
			}
			else
				Scfl[4] = 1;
		}
		else
		{
			Ssymfl[1] = 0;
			Ssymbol[1][0] = '\0';
			Scfl[4] = 0;
		}
		if (Tool_current_data.fholder)
		{
			Ssymfl[2] = 1;
			if (Tool_current_data.symhld[0]!='\0')
			{
				strcpy(Ssymbol[2], Tool_current_data.symhld);
				Scfl[5] = 2;
			}
			else
				Scfl[5] = 1;
		}
		else
		{
			Ssymfl[2] = 0;
			Ssymbol[2][0] = '\0';
			Scfl[5] = 0;
		}
/*
........Setup CUTTER/DISPLY flags
*/
/*		if (Scfl[1] == 1 || Scfl[2] == 1 || Scfl[3] == 1) */
		{
			UM_init_f77_str(f77_str,sym,MAXSYMLEN);
			cutget(cbuf,dbuf,icfl,UM_addr_of_f77_str(f77_str),
					UM_addr_of_f77_str(f77_str),UM_addr_of_f77_str(f77_str));
		}
		if (Scfl[2] == 0) Scfl[2] = icfl[2];
		else if (Scfl[2] == 1) Scfl[2] = 1;
		else Scfl[2] = 0;

		if (Scfl[3] == 0) Scfl[3] = icfl[3];
		else if (Scfl[3] == 1) Scfl[3] = 1;
		else Scfl[3] = 0;
		
		if (Scfl[1] == 0) Scfl[1] = icfl[1];
		else if (Scfl[1] == 1) Scfl[1] = 0;
		else Scfl[1] = 1;

		if (Scfl[6] == 0) Scfl[6] = icfl[6];
		else if (Scfl[6] == 1) Scfl[6] = 1;
		else Scfl[6] = 0;

		if (Scfl[7] == 0) Scfl[7] = icfl[7];
		else if (Scfl[7] == 1) Scfl[7] = 1;
		else Scfl[7] = 0;

		if (Stype==-1)
			Stype = 0;
		strcpy (Sdesc, Tool_current_data.description);
/*
.....Set up the field entries
*/
		for (i=0;i<6;i++) sprintf(Scut[i],"%g",Tool_current_data.cutter[i]);
		for (i=0;i<12;i++) strcpy(Satt[i],"0");

		if (Tool_current_data.fsymbol)
		{
			sprintf(Satt[0],"%g",Tool_current_data.catt[0]);
			sprintf(Satt[1],"%g",Tool_current_data.catt[1]);
			sprintf(Satt[2],"%g",Tool_current_data.catt[2]);
			sprintf(Satt[3],"%g",Tool_current_data.catt[3]);
		}
		if (Tool_current_data.fshank)
		{
			sprintf(Satt[4],"%g",Tool_current_data.satt[0]);
			sprintf(Satt[5],"%g",Tool_current_data.satt[1]);
			sprintf(Satt[6],"%g",Tool_current_data.satt[2]);
			sprintf(Satt[7],"%g",Tool_current_data.satt[3]);
		}
		if (Tool_current_data.fholder)
		{
			sprintf(Satt[8],"%g",Tool_current_data.hatt[0]);
			sprintf(Satt[9],"%g",Tool_current_data.hatt[1]);
			sprintf(Satt[10],"%g",Tool_current_data.hatt[2]);
			sprintf(Satt[11],"%g",Tool_current_data.hatt[3]);
		}
/*
.....then use ud_update_answer to update all data
*/
		sprintf(Stool, "%-15.0f", toolnum);
		ud_update_answer(FTOOL, (int*)&Stool[0]);
		ud_update_answer(FDESC, (int*)&Sdesc[0]);
		ud_update_answer(FDIA1, (int*)&Scut[0][0]);
		ud_update_answer(FRAD1, (int*)&Scut[1][0]);
		ud_update_answer(FHGT1, (int*)&Scut[2][0]);
		ud_update_answer(FANG1, (int*)&Scut[3][0]);
		ud_update_answer(FZHT1, (int*)&Scut[4][0]);
		ud_update_answer(FLAT1, (int*)&Scut[5][0]);
		ud_update_answer(FPSEU,&Tool_current_data.fpseudo);
		ud_set_traverse_mask(FDEFI,Tool_current_data.fpseudo);
		ud_update_answer(FMOVE, (int*)&Scfl[2]);
		ud_update_answer(FSEGM, (int*)&Scfl[1]);
		ud_update_answer(FSHAD, (int*)&Scfl[3]);
		ud_update_answer(FSLIB, (int*)&Ssymlib[0]);
		ud_update_answer(FSYM1, (int*)&Ssymfl[0]);
		ud_set_traverse_mask(FSYM2,Ssymfl[0]);
		ud_update_answer(FSYM3, (int*)&Ssymbol[0]);
		ud_update_answer(FSHK1, (int*)&Ssymfl[1]);
		ud_set_traverse_mask(FSHK2,Ssymfl[1]);
		ud_update_answer(FSHK3, (int*)&Ssymbol[1]);
		ud_update_answer(FHLD1, (int*)&Ssymfl[2]);
		ud_set_traverse_mask(FHLD2,Ssymfl[2]);
		ud_update_answer(FHLD3, (int*)&Ssymbol[2]);
		OnCtypeChg(Stype,disp,trav,UU_TRUE);
		nclu_tool_shank_fields(UU_TRUE);
		if (!Ssymfl[0]) nclu_tool_close_form(0);
		if (!Ssymfl[1]) nclu_tool_close_form(1);
		if (!Ssymfl[2]) nclu_tool_close_form(2);
		if (strlen(Tool_current_data.drawing)>0)
		{
			strcpy(current_draw, Tool_current_data.drawing);
			ud_set_traverse_mask(FVIWT,UU_TRUE);
		}
		else
		{
			current_draw[0] = '\0';
			ud_set_traverse_mask(FVIWT,UU_FALSE);
		}
		if (parm_nary > 0) ud_set_traverse_mask(FPARM, UU_TRUE);
		else  ud_set_traverse_mask(FPARM, UU_FALSE);
		Sdefflag = UU_FALSE;
		savnary = parm_nary;
/*
.....update the form display here
.....since set the form item will call callback function
.....but the callback function need to know where it called
.....since if we changed the form data, then OnNoTool is called
.....so that the toolno is reset to empty, if we select a tool,
.....the form data changed, then we don't need call OnNoTool
*/
		uw_ntupdate_form(0);
		S_select_tool = 0;
		stat = 0;
		Sdefflag = UU_FALSE;
	}
/*
......unselect
*/
	else
	{
		stat = -1;
		OnNoTool(&fieldno,&val,stat);
	}
	for (i=0;i<6;i++) tempdbuf[i] = Tool_current_data.pseudo[i];

	um_close_pocket_window(UM_DRAWING_WINDOW);
	return(stat);
}

/*********************************************************************
**    S_FUNCTION     :  static OnViewOpt(fieldno, val, stat)
**       Method called at when "View..." button is pushed
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnViewOpt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
	if (*fieldno!=FVIWT)
		return(UD_BADTYP);
/*
.....close the drawing first, then reopen
*/
	um_close_pocket_window(UM_DRAWING_WINDOW);
	um_close_pocket_window(UM_GRAPHIC_WINDOW);
/*
.....View the tool drawing
*/
	status = um_load_pocket_drawing(NULL, current_draw, current_draw, "NCL_TOOL_DRAWING", 1);
	if (status==0)
		ud_setform_pocket(0, 1);
	return(UD_FLDOK);
}

#define tabn 18

/*********************************************************************
**    S_FUNCTION     :  static OnBrowse(fieldno, val, stat)
**       Method called at 'browser' button field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnBrowse(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname dirname, farea, dname;
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	int len;
	UD_DDATA data;

	sprintf(paths, "%s;%s", UB_libdata_rec.sys_farea, UB_libdata_rec.loc_farea);
	strcpy(path_des, "System;Local");
	dirname[0] = '\0';
	ud_get_dirname1("Symbol Library", "Symbol Library", dirname, &len, paths, path_des);
	if (stricmp(Ssymlib, dirname)!=0)
	{
/*
.....load in symbols in dirname
*/
/*
.....since the symlib only can support 12 char name
.....only add the last directory name as symlib name
*/
		ux_decompose_path(dirname, farea, dname, UX_NQUOTES);
/* 
.....don't update the symbol list now
*/
/*
		S_load_archive(farea, dname);
		nclu_tool_uptsyms();
*/
		ud_update_answer(FSLIB, (int*)dname);
	}
	return UD_FLDOK;
}

static UD_FSTAT S_tool_library(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UX_pathname filename,tch,dir, dir2;
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	int nc,status;
	char msg[80],sfile[40];
	static int opened = 0;
/*
.....Verify tool library exists
*/
	ncl_gettool_head (&Tool_head);
	strcpy(filename, Tool_head.name);
	if (opened==0)
	{
		opened = 1;
		strcpy(paths, "NCL_TOOL");
		strcpy(path_des, "System");
		ud_get_filename1(NULL, "Enter NCL Tool Library:", "*.TLB", filename,&nc, "Tool Library Files (*.TLB)", 1, UU_FALSE, paths, path_des);
		opened = 0;
	}
	if (nc<=0) goto done;
/*
.....Update text field
*/
	ul_break_fname(filename,dir,tch);
	status = ncl_load_toolncl(filename);
	if (status != UU_SUCCESS) goto failed;
/*
.....if it is not the NCL_TOOL or local folder
.....then we need include the whole path
*/
	ul_get_full_dir("NCL_TOOL", dir2);
	if (stricmp(dir, dir2)!=0)
	{
		ul_get_full_dir(".", dir2);
		if (stricmp(dir, dir2)==0)
			ud_update_answer(FTLTX, (int*)tch);
		else
			ud_update_answer(FTLTX, (int*)filename);
	}
	else
		ud_update_answer(FTLTX, (int*)tch);
/*
.....Load tools
*/
	S_load_tools(Sfilt-1,UU_TRUE);
	goto done;
done:;
	return(UD_FLDOK);
/*
.....Failed to load tool library
*/
failed:;
//	ul_short_filename(filename,sfile,sizeof(sfile)-1);
//	sprintf(msg,"Failed to load '%s'",sfile);
	sprintf(msg,"Failed to load '%s'",filename);
	ud_wrerr(msg);
	goto done;
}
/*********************************************************************
**    I_FUNCTION     : S_load_tools(filter_type,chg)
**       Loads the tools of the specified cutter type out of the tool
**       library into the form list.
**    PARAMETERS
**       INPUT  :
**				filter_type = Cutter type to load. -1 = All tools.
**				chg         = UU_TRUE if the list already exists and needs updating.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_load_tools(filter_type,chg)
int filter_type;
UU_LOGICAL chg;
{
	char lib[UX_MAX_PATH_LEN],*p;
	char *ux_getenv(), direc[UX_MAX_PATH_LEN], fullname[UX_MAX_PATH_LEN], *ext, *index();
	int status, mode;
	int fno;
	UD_DDATA val;
	char **ncl_get_tool_type();
	FILE *fptr = NULL;

	ncl_gettool_head (&Tool_head);
	if (Tool_head.name[0]=='\0')
	{
/*
.....Open NCL Tool Library &
.....Initiate search
*/
		p = ux_getenv ("NCL_TOOLIB", UX_NPRTERRS);
		if (p != UU_NULL)
		{
			ul_get_full_dir("HOMEDIR",direc);
			mode = 0;
			status = ux_mk_chk_syspath(UU_NULL , direc, p,
					UU_NULL, UU_NULL, &mode, lib, UX_PRTERRS);
			if (mode & UX_NEXISTS)
			{
				strcpy(fullname, lib);
				ext = rindex(fullname,'.');
				if (ext!=NULL)
					*ext = '\0';
				strcat(fullname,".");
				strcat(fullname, "TLB");
				status = ul_open_mod_file2("NCL_TOOL", NULL, fullname, 0,  &fptr, UU_NULL, UU_NULL);
				if (status==-1)
					lib[0] = '\0';
				else
					strcpy(lib, fullname);
			}
			ux_strip_quotes(lib);
		}
		ncl_load_toolncl(lib);
	}
	if (chg)
	{
		ud_free_tlist(&Stool_list);
		ud_free_flist(&Stype_list);
	}

	Stool_list.num_item = ncl_get_tool_tlist(&Stool_list, filter_type);
	Stool_list.answer = 0;
	Stool_list.sort = saved_info.col;
/*
.....if there is tools, select the first as default
*/
	Stype_list.item = ncl_get_tool_type(&(Stype_list.num_item), UU_NULL);
	Stype_list.answer = (char *) uu_malloc(81 * sizeof(char));
	strcpy(Stype_list.answer, Stype_list.item[1]);
/*
.....Set the list
*/
	if (chg) 
	{
		ud_update_answer(FLIST,(int *)&Stool_list);
		resort_table();
		ud_update_answer(FTYPE,(int *)&Stype_list);
/*
.....need call this, it will not automate all
*/
		fno = FTYPE;
		val.frmstr = (char*)&Stype_list.answer;
		OnCutType(&fno, &val, UD_FLDOK);
	}
}
/*********************************************************************
**    E_FUNCTION     : S_load_toolnum(toolnum, drawng)
**       loads an NCL Tool Library tool.
**    PARAMETERS
**       INPUT  :
**				toolnum: tool to load
**				drawng:  drawing name
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_load_toolnum(tool, drawng)
double tool;
char *drawng;
{
	int i,iary[20],nc;
	UM_int4 nclkey;
	char ostr[500],fcstr[20][20],lnum[10];
	NCL_cmdbuf cmdbuf;
	UX_pathname lib,dir;
	int kerr;

	ncl_gettool_head (&Tool_head);
	if (Tool_head.no_tools==0) return;
	kerr = ncl_findtl(tool, drawng, iary, fcstr, &parm_nary);
	if (kerr==-1 || tool == 0) goto done;

	if (strlen(Tool_head.name)>=62)
	{
/*		ul_break_fname(Tool_head.name,dir,lib);
		S_call_tmpval("@TX01", dir); */
		S_call_tmpval("@TX01", Tool_head.name);
	}
/*
.....Initialize NCL command buffers
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Output a CUTTER/TOOL statement
*/
	if (Soutput) strcpy(ostr,"CUTTER/READ, ");
	else strcpy(ostr,"CUTTER/TOOL, ");
/*
.....We still need to add tool library here because
.....it needs to be written into the PP file
.....Do not include path here
.....may cause problems due to char length limits in Fortran
.....Yurong
*/
/*
.....coomand include path here now, but it will limit '64' chars
.....token, if more than 64 chars, save into a tmp value and pass in
.....the command
*/
/*
//	ul_break_fname(Tool_head.name,dir,lib);
//	strcat(ostr,lib);
*/
	if (strlen(Tool_head.name)<62)
	{
		strcat(ostr,"\"");
		strcat(ostr,Tool_head.name);
		strcat(ostr,"\"");
	}
	else
	{
/*		strcat(ostr, "@TX01,");
		strcat(ostr,lib);
*/
		strcat(ostr, "@TX01");
	}
	strcat(ostr,",");
	sprintf(lnum,"%-15.0f",tool);
	strcat(ostr,lnum);
	ncl_add_token(&cmdbuf,ostr,UU_FALSE);
	if (pspec)
	{
		for (i=0;i<parm_nary;i++)
		{
			ncl_add_token(&cmdbuf,",",UU_FALSE);
			if (iary[i] == 1)
			{
				nc = strlen(parmstr[i+1]);
				ul_strip_blanks(parmstr[i+1],&nc);
				if ((nc > 0)&&(strcmp(parm_fcstr[i],parmstr[i+1])!=0))
				{
					ncl_add_token(&cmdbuf,parmstr[i+1],UU_FALSE);
				}
				else
				{
					ncl_add_token(&cmdbuf,"SAME",UU_FALSE);
				}
			}
			else
			{
				ncl_add_token(&cmdbuf,"SAME",UU_FALSE);
			}
		}
	}
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....delete the temp value
*/
	if (strlen(Tool_head.name)>=62)
	{
		getkey("@TX01", &nclkey);
		dtdele (&nclkey);
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION :  S_form_loadcut(prmstrs,vis)
**      Create data entries in a load cutter tool form.
**
**    PARAMETERS
**       INPUT
**         prmstrs    - Pointer to data to put on form.
**                      because we only need text fields on this form,
**                      we only need a prompt data in prmstrs
**         vis        - 1 = This field is visible.
**
**       OUTPUT:
**         frm_ptr    - Form data.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_form_loadcut(prmstrs,vis)
char prmstrs[20][80];
int vis[21];
{
	char tmp[1000];
	int i,len_form,len_hdr;
	int inx,len,strlength,optx, sizex;
	int k = 0;
/*
...   One prompt per line
*/
/*
.....start at y pixel = 8
.....         x pixel = 10
*/
	y = 8;
	strlength = 0;
	for(i = 0; i<20; i++)
	{
		if (vis[i+1]==0)
			continue;
		if (strlength<strlen(prmstrs[i]))
			strlength = strlen(prmstrs[i]);
/*
.... y = 17 pixel per line
*/
		y = y+17;
	}
	x = 10;
	y_max = y;

	if (strlength<=5)
		strlength = 5;
	optx = (int)(strlength*3.5 + 15);
	sizex = optx + 15*4 + 20;

	sprintf(tmp,"#HEADER#\n");
	i = strlen(tmp);
	sprintf(&tmp[i],"/TITLE/ LOAD CUTTING TOOL\n");
	i = strlen(&tmp[0]);
	sprintf(&tmp[i],"/POSITION/ 0,0\n");
	i = strlen(&tmp[0]);
	sprintf(&tmp[i],"/SIZE/ %d,%d\n", sizex, y_max+45);
	strcpy (&frm_ptr[0],&tmp[0]);

   len_form = strlen(&frm_ptr[100]);
   len_hdr = strlen(tmp);

   for(k = 100; k < 100+len_form; k++)
   {
      frm_ptr[k-(100-len_hdr)] = frm_ptr[k];
   }

   for(k = len_form+len_hdr; k < 100+len_form; k++)
   {
      frm_ptr[k] = '\n';
   }

	k = 0;
/*
...String only
*/
	y = 8;
	inx = 100;
	sprintf(&frm_ptr[0]+inx,"#DISPLAY#\n");
	inx = inx + strlen(&frm_ptr[0]+inx);
	sprintf(&frm_ptr[0]+inx,"/LABEL/ TOOL:\n");
	inx = inx + strlen(&frm_ptr[0]+inx);
	sprintf(&frm_ptr[0]+inx,"/POSITION/ %d,%d, %d, %d\n", x, y, optx, y);
	inx = inx + strlen(&frm_ptr[0]+inx);
	sprintf(&frm_ptr[0]+inx,"/SIZE/ %d,%d\n", 150, 14);
	inx = inx + strlen(&frm_ptr[0]+inx);
	sprintf(&frm_ptr[0]+inx,"/TYPE/ UD_DASSTRING\n");
	inx = inx + strlen(&frm_ptr[0]+inx);
	sprintf(&frm_ptr[0]+inx,"/PREC/ 15\n");
	inx = inx + strlen(&frm_ptr[0]+inx);
	sprintf(&frm_ptr[0]+inx,"/LEN/ 15\n");
	inx = inx + strlen(&frm_ptr[0]+inx);

	y = y + 17;
	for (i=0; i<20; i++)
	{
		if (vis[i+1]==0)
			continue;
		sprintf(&frm_ptr[0]+inx,"#EDIT#\n");
		inx = inx + strlen(&frm_ptr[0]+inx);
		sprintf(&frm_ptr[0]+inx,"/LABEL/ %s\n",prmstrs[i]);
		inx = inx + strlen(&frm_ptr[0]+inx);
		len = strlen(prmstrs[i]);
		sprintf(&frm_ptr[0]+ inx ,"/POSITION/%d,%d, %d, %d\n",x,y,optx,y);
		inx = inx  + strlen(&frm_ptr[0]+inx);
		len = strlen(prmstrs[i]);
		x = (len + 8)*5;
		sprintf(&frm_ptr[0]+inx,"/SIZE/ %d,%d\n", x, 14);
		inx = inx + strlen(&frm_ptr[0]+inx);
		sprintf(&frm_ptr[0]+inx,"/TYPE/ UD_DASSTRING\n");
		inx = inx + strlen(&frm_ptr[0]+inx);
		sprintf(&frm_ptr[0]+inx,"/PREC/ 15\n");
		inx = inx + strlen(&frm_ptr[0]+inx);
		sprintf(&frm_ptr[0]+inx,"/LEN/ 15\n");
		inx = inx + strlen(&frm_ptr[0]+inx);
/*
...Prepare the coordinates for the next prompt
*/
		x = 10;
		y = y + 17;
	}
	sprintf(&frm_ptr[0]+inx,"~END\n");
}

/*********************************************************************
**    I_FUNCTION     :ncl_load_toolncl(libfile)
**       Loads the tools from the libfile, if libfile s empty,
**			load the NCL_TOOLIB file in local/NCL_TOOL diectory
**    PARAMETERS
**       INPUT  :
**				libfile: toolib file to be load
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_load_toolncl(libfile)
char *libfile;
{
	char *p, *ux_getenv(), direc[UX_MAX_PATH_LEN], fullname[UX_MAX_PATH_LEN];
	int status, mode;
	UU_LOGICAL lprof;
	UX_pathname filename;
	char *ext;
	FILE *fptr = NULL;

	if (strlen(libfile)==0)
	{
		p = ux_getenv ("NCL_TOOLIB", UX_NPRTERRS);
		if (p != UU_NULL)
		{
			strcpy(libfile, p);
		}
	}
	mode = 0;
	status = ux_mk_chk_syspath(UU_NULL , NULL, libfile,
				UU_NULL, "TLB", &mode, fullname, UX_PRTERRS);
	if (mode & UX_NEXISTS)
	{
		status = ul_open_mod_file2("NCL_TOOL", NULL, fullname, 0,  &fptr, UU_NULL, "TLB");
		if (status==-1)
			fullname[0] = '\0';
	}
	ul_remove_quotes(fullname);
	if (fullname[0]=='\0') return -1;
/*
.....Don't reload the same tool library
.......Reload if the profile library has changed - Andrew 2/25/13
*/
	status = UU_SUCCESS;
	ncl_getcut_proffl(&lprof);
	if (strcmp(fullname,Tool_head.name) != 0 || lprof)
	{
		status = ncl_load_tool(fullname);
		if (status == UU_SUCCESS)
		{
			ncl_gettool_head (&Tool_head);
/*
.....load the profile defined in toolib file
*/
			if (strcmp(fullname,Tool_head.name) != 0 && !Sload_prof)
			{
   				nclu_tool_profs(Tool_head.proflib);
				Sload_prof = UU_TRUE;
			}
		}
	}
	return(status);
}
		
/*********************************************************************
**    I_FUNCTION     : ncl_load_toolf(toolib, nc,ierr)
**       Loads the tools from the toolib file. fortain callable
**    PARAMETERS
**       INPUT  :
**				toolib: toolib file to be load
**				nc: number of character of toolib
**       OUTPUT :
**          ierr   = 1 - Error processing tool library.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_load_toolf(toolib, nc,ierr)
char *toolib;
int *nc;
int *ierr;
{
	int status;
	char filename[256];

	*ierr = 0;
	strncpy(filename, toolib, *nc);
	filename[*nc] = '\0';
	status = ncl_load_toolncl(filename);
	if (status != UU_SUCCESS) *ierr = 1;
}

/*********************************************************************
**    E_FUNCTION     : ncl_sel_toolf(nctool, err)
**       fortain callable ncl_sel_tool
**    PARAMETERS
**       INPUT  : 
**          nctool: tool number to be set as courrect active tool
**       OUTPUT :
**          none
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_sel_toolf(nctool, err)
double *nctool;
int *err;
{
	*err = ncl_sel_tool(*nctool);
}

/*********************************************************************
**    E_FUNCTION     : nclf_getcut_profil(outcmd,outlen)
**       Builds CUTTER/PROFIL command.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          outcmd - command string created.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_getcut_profil(outprof)
char *outprof;
{
	int nc = 80;
	
	ncl_gettool_head (&Tool_head);
	S_load_tools(Sfilt-1,UU_FALSE);
	if (Tool_head.proflib[0] == '\0') return (UU_FAILURE);
//need handle Tool_head.proflib>63 char
	sprintf(outprof,"CUTTER/PROFIL,\"%s\"",Tool_head.proflib);
	return (UU_SUCCESS);
}

