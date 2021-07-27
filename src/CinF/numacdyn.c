/*********************************************************************
**    NAME         :  numacdyn.c
**			Previously 'macdyn.c'
**       CONTAINS: 
**				stricmp()
**				ncl_macro_call()
**				nclu_list_macros()
**				nclu_macro_form()
**				ncl_macro_formdesgn(parms)
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       numacdyn.c , 26.3
**    DATE AND TIME OF LAST  MODIFICATION
**       05/22/18 , 10:23:12
**       
*********************************************************************/
#include "ulist.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "dtypes.h"
#include "lcom.h"
#include "mfort.h"
#include "mxxx.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"
#include <ctype.h>
#include "mpocket.h"
#include "nccs.h"
#include "uhep.h"
#include "mdrel.h"
#include "dselmask.h"
#include "lumb.h"
#include "nclmodals.h"

#define WORD_LIMIT 20

static int x=0;
static int y=0;
static int y_max=0;
static int x_max=0;
static int S_data_num = 0;
static int S_data_type[200];
static int S_data_type_f[200];

static int fitem;
static int newload;
static int maclist;
static char filter[NCL_MAX_LABEL+1];
static char cur_class[21] = "ALL";
static char *ptr1_first;
static UD_TLIST macro_namelist;
#define filt_fld 1
#define clas_fld 2
#define list_fld 3

extern char *frm_ptr;
extern UD_METHOD UD_initfrm_intry;
static char S_tmp_dat_n = 0;
static char S_tmp_dat[100][65];

static void form_options();
static void form_elmt();
static void form_header();
static void ncl_calform_initsize();
static int S_save_macro_mf();
static UD_FSTAT string_picked_loop();

#define MAX_PROMPT_SIZE 25
#define STRING_SIZE 20
#define X_RATIO 4
#if UU_COMP==UU_WIN2K
#define SECOND_COLUMN 130
#define PROMPT_SPACING 20
#else
#define SECOND_COLUMN 180
#define PROMPT_SPACING -10
#endif
static UD_LIST mclass_list;
extern int UD_form_bypick;
static int macro_substr[500], macro_pflag[500];
static int macro_modal;
static int  *type_of_ans;
static int  totprm, form_item=0;
static char macro_name[NCL_MAX_LABEL+1];
static	int  save_def;
static UD_TABLEINFO saved_info = {-1,-1,-1,-1,-1,0};
static int name_click = 0;
static int descript_click = 0;
static int class_click = 0;
static void resort_table();

static int S_form_design = 0;
extern int UD_macroflag;

void ncl_str2lval();
/*
.....this union is defined for a value either a label with subnum
.....or a real value used for macro define form
*/
typedef union
{
	int frmint;
	double	frmval;
	char frmstr[75];
	UD_SELECT_STR *frmlst;
} ANS_DATA;

UD_SELECT_STR S_sel_list[200];
extern int UD_novideo_error;
/***********************************************************************
c
c   FUNCTION: stricmp(str1, str2)
c			compare string case no-sensitive
c			active seems as stricmp from WinNT
c
c   INPUT:  str1, str2: string to be compared
c
c   OUTPUT :
c			0:  the same.
c   RETURN: None.
c
**********************************************************************/
#if UU_COMP!=UU_WIN2K
int stricmp(str1, str2)
char *str1, *str2;
{
	char *temp1, *temp2;
	int ret, len1, len2;
	len1 = strlen(str1);
	len2 = strlen(str2);
	if ((len1==0) || (len2==0))
		return -1;
	temp1 = (char *) uu_malloc((len1+1)*sizeof(char));
	temp2 = (char *) uu_malloc((len2+1)*sizeof(char));
	strcpy(temp1, str1);
	strcpy(temp2, str2);
	ul_to_lower(temp1);
	ul_to_lower(temp2);
	ret =  strcmp(temp1, temp2);
	uu_free (temp1);
	uu_free (temp2);
	return ret;
}
#endif

static void S_add_label (list,sbuf)
UU_LIST *list;
char *sbuf;
{
	char *buf;

	buf = (char *) uu_malloc((strlen(sbuf)+1)*sizeof(char));
	strcpy (buf,sbuf);	
	uu_list_push (list,&buf);
}

static int S_handle_mutl_selstr2(name, select_list, selstr)
char *name, *selstr;
UU_LIST *select_list;
{
	struct NCL_fixed_databag e;
	char dname[65], label[65], cmdstr[80];
	UU_LIST data_list;
	NCL_cmdbuf cmdbuf;
	char **geom;
	int i, called, nc, nc1, nc2, num = 0;
	geom = (char**)UU_LIST_ARRAY(select_list);
	selstr[0] = '\0';
	dname[0]  = '\0';
	
	if (UU_LIST_LENGTH(select_list)==0)
		return 1;
	if ((UU_LIST_LENGTH(select_list)>0)&&(UU_LIST_LENGTH(select_list)<=12))
	{
		for (i=0;i<UU_LIST_LENGTH(select_list);i++) 
		{
			if (i==0)
				strcat(dname, geom[i]);
			else
			{
				strcat(dname, ",");
				strcat(dname, geom[i]);
			}
		}
		strcpy(selstr, dname);
		uu_list_free (select_list);
		return 1;
	}
	uu_list_init(&data_list, sizeof(char *), 10,10);
/*
.....more than 12 items
*/
	nc1 = strlen(name);
	ncl_tmpdatn(name, &nc1, dname, &nc2);
	strcpy(S_tmp_dat[S_tmp_dat_n], dname);
	S_tmp_dat_n++;
	ncl_init_cmdbuf(&cmdbuf);
	sprintf(cmdstr,"%s=DATA/", dname);
	ncl_add_token(&cmdbuf, cmdstr, NCL_nocomma);
	S_add_label (&data_list, dname);

	called = 0;
	i = 0;
	while (i<UU_LIST_LENGTH(select_list))
	{
		if (cmdbuf.num_cmd < 1)
		{
			ncl_add_token(&cmdbuf, geom[i], NCL_comma);
		}
		else
		{
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			called = 1;
			if (i<UU_LIST_LENGTH(select_list))
			{
				ncl_tmpdatn(name, &nc1, dname, &nc2);
				strcpy(S_tmp_dat[S_tmp_dat_n], dname);
				S_tmp_dat_n++;
				ncl_init_cmdbuf(&cmdbuf);
				sprintf(cmdstr,"%s=DATA/", dname);
				ncl_add_token(&cmdbuf, cmdstr, NCL_nocomma);
				S_add_label (&data_list, dname);
				called = 0;
			}
		}
	}
	if (called==0)
	{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
	S_handle_mutl_selstr2(name, data_list, selstr);
	uu_list_free (select_list);
	return 1;
}

static int S_handle_mutl_selstr(name, select_list, selstr)
char *name, *selstr;
UU_LIST *select_list;
{
	struct NCL_fixed_databag e;
	char dname[65], label[65], cmdstr[80];
	UU_LIST data_list;
	NCL_cmdbuf cmdbuf;
	UD_sel_geo *geom;
	char **data_name;
	int i, called, nc, nc1, nc2, num = 0;
	geom = (UD_sel_geo*)UU_LIST_ARRAY(select_list);
	selstr[0] = '\0';
	dname[0]  = '\0';
	
	if (UU_LIST_LENGTH(select_list)==0)
		return 1;
	if ((UU_LIST_LENGTH(select_list)>0)&&(UU_LIST_LENGTH(select_list)<=12))
	{
		for (i=0;i<UU_LIST_LENGTH(select_list);i++) 
		{
			e.key = geom[i].key;
			ur_retrieve_data_fixed(&e);
			if (e.key != 0) ncl_get_label(&e, label);
			else 
				continue;
			nc = strlen(label);
			ul_strip_blanks(label,&nc);
			if (nc==0)
				continue;
			if (i==0)
				strcat(dname, label);
			else
			{
				strcat(dname, ",");
				strcat(dname, label);
			}
		}
		strcpy(selstr, dname);
		uu_list_free (select_list);
		return 1;
	}
	uu_list_init(&data_list, sizeof(char *), 10,10);
/*
.....more than 12 items
*/
	nc1 = strlen(name);
	ncl_tmpdatn(name, &nc1, dname, &nc2);
	strcpy(S_tmp_dat[S_tmp_dat_n], dname);
	S_tmp_dat_n++;
	ncl_init_cmdbuf(&cmdbuf);
	sprintf(cmdstr,"%s=DATA/", dname);
	ncl_add_token(&cmdbuf, cmdstr, NCL_nocomma);
	S_add_label (&data_list, dname);

	called = 0;
	i = 0;
	while (i<UU_LIST_LENGTH(select_list))
	{
		if (cmdbuf.num_cmd < 1)
		{
			e.key = geom[i].key;
			i++;
			ur_retrieve_data_fixed(&e);
			if (e.key != 0) ncl_get_label(&e, label);
			else 
				continue;
			nc = strlen(label);
			ul_strip_blanks(label,&nc);
			if (nc==0)
				continue;
			ncl_add_token(&cmdbuf, label, NCL_comma);
		}
		else
		{
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			called = 1;
			if (i<UU_LIST_LENGTH(select_list))
			{
				ncl_tmpdatn(name, &nc1, dname, &nc2);
				strcpy(S_tmp_dat[S_tmp_dat_n], dname);
				S_tmp_dat_n++;
				ncl_init_cmdbuf(&cmdbuf);
				sprintf(cmdstr,"%s=DATA/", dname);
				ncl_add_token(&cmdbuf, cmdstr, NCL_nocomma);
				S_add_label (&data_list, dname);
				called = 0;
			}
		}
	}
	if (called==0)
	{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
	S_handle_mutl_selstr2(name, data_list, selstr);
	uu_list_free (select_list);
	return 1;
}
/***********************************************************************
c
c   FUNCTION: match_field_parm(fieldno, indx)
c			match a field number in current macro parameter form
c			to index of the paramter of the macro
c
c   INPUT:  fieldno: field number in current macro parameter form
c
c   OUTPUT :
c			indx:  index of the paramter of the macro.
c   RETURN: None.
c
**********************************************************************/
static int match_field_parm(fieldno, indx)
int *fieldno, *indx;
{
	int i, j, k;
	k = 0;
	for (i=0, j=0; j<form_item; i++, j++)
	{
		if (*fieldno<=j)
		{
			*indx = i+1;
/*
			if (type_of_ans[k]==-1)
				*fieldno += 1;
*/
			return 0;
		}
		if (type_of_ans[k]==0 || type_of_ans[k]==-2)
		{
			k++;
		}
		else if (type_of_ans[k]>0)
		{
			k = k+1+(*(type_of_ans+k)*6); 
		}
		else
		{
/*
			j++;
			if (*fieldno<=j)
			{
				*indx = i+1;
				return 0;
			}
*/
			k = k + 21;
		}
	}
	return -1;
}

/***********************************************************************
c
c   FUNCTION: limit_pick(wrdlist, counter)
c			Sets up the /LIMIT/ buffer with the limit geo entities specified
c        by the calling Macro.
c
c   INPUT:
c      wrdlist    list of limited geomotry 
c      counter    number of geometry to be limited.
c
c   OUTPUT :
c      lbuf       /LIMIT/ buffer to output as part of form field.
c   RETURN: None.
c
**********************************************************************/
static void limit_pick(wrdlist,counter,lbuf)
char *wrdlist;
int counter;
char *lbuf;
{
	int i, len;
    int Add_mask[UD_NMENTWD];
	char geom[10];
	int scaler, shape, point, vector, line, plane, circle, curve, surf, matrix;
	int pv, patern, solid;

	scaler = 0; 
	shape = 0;
	point = 0;
	vector = 0;
	line = 0;
	plane = 0;
	circle = 0;
	curve = 0;
	surf = 0; 
	matrix = pv = patern = 0;
	solid = 0;
	strcpy(lbuf,"/LIMIT/");
	for(i=0; i<counter; i++)
	{
		for (len = 0; wrdlist[len+i*24] > ' ' && len < 24; len++)
			geom[len] = wrdlist[len+i*24];
		geom[len] = '\0';
		if ((strcmp(geom, "SHAPE")==0) || (strcmp(geom, "SH")==0))
			strcat(lbuf,"SHAPE");
		else if ((strcmp(geom, "POINT")==0) || (strcmp(geom, "PT")==0))
			strcat(lbuf,"POINT");
		else if ((strcmp(geom, "VECTOR")==0) || (strcmp(geom, "VE")==0))
			strcat(lbuf,"VECTOR");
		else if ((strcmp(geom, "LINE")==0) || (strcmp(geom, "LN")==0))
			strcat(lbuf,"LINE");
		else if ((strcmp(geom, "PLANE")==0) || (strcmp(geom, "PL")==0))
			strcat(lbuf,"PLANE");
		else if ((strcmp(geom, "CIRCLE")==0) || (strcmp(geom, "CI")==0))
			strcat(lbuf,"CIRCLE");
		else if ((strcmp(geom, "CURVE")==0) || (strcmp(geom, "CV")==0))
			strcat(lbuf,"CURVE");
		else if ((strcmp(geom, "SURF")==0) || (strcmp(geom, "SF")==0))
			strcat(lbuf,"SURF");
		else if ((strcmp(geom, "MATRIX")==0) || (strcmp(geom, "MX")==0))
			strcat(lbuf,"MATRIX");
		else if ((strcmp(geom, "PV")==0) || (strcmp(geom, "PNTVEC")==0))
			strcat(lbuf,"PNTVEC");
		else if ((strcmp(geom, "PATERN")==0) || (strcmp(geom, "PN")==0))
			strcat(lbuf,"PATERN");
		else if ((strcmp(geom, "SOLID")==0) || (strcmp(geom, "SO")==0))
			strcat(lbuf,"SOLID");
		if (i+1 != counter) strcat(lbuf,",");
	}
	strcat(lbuf,"\n");
}

/***********************************************************************
c
c   FUNCTION: limit_pick_flag(wrdlist, counter)
c			limit the picking from passin geo-list
c
c   INPUT:  wrdlist: list of limited geomotry 
c			counter: number of geometry to be limited.
c
c   OUTPUT :
c			None.
c   RETURN: None.
c
**********************************************************************/
static void limit_pick_flag(wrdlist, counter)
char *wrdlist;
int counter;
{
	int i, len;
    int Add_mask[UD_NMENTWD];
	char geom[10];
	int scaler, shape, point, vector, line, plane, circle, curve, surf, matrix;
	int pv, patern, solid;

	scaler = 0; 
	shape = 0;
	point = 0;
	vector = 0;
	line = 0;
	plane = 0;
	circle = 0;
	curve = 0;
	surf = 0; 
	matrix = pv = patern = 0;
	solid = 0;
	for(i=0; i<counter; i++)
	{
		for (len = 0; wrdlist[len+i*24] > ' ' && len < 24; len++)
			geom[len] = wrdlist[len+i*24];
		geom[len] = '\0';
		if (strcmp(geom, "SCALAR")==0)
		{
			scaler = 1;
		}
		else if ((strcmp(geom, "SHAPE")==0)
				|| (strcmp(geom, "SH")==0))
		{
			shape = 1;
		}
		else if ((strcmp(geom, "POINT")==0) ||
					(strcmp(geom, "PT")==0))
		{
			point = 1;
		}
		else if ((strcmp(geom, "VECTOR")==0) 
				|| (strcmp(geom, "VE")==0))
		{
			vector = 1;
		}
		else if ((strcmp(geom, "LINE")==0)
				|| (strcmp(geom, "LN")==0))
		{
			line = 1;
		}
		else if ((strcmp(geom, "PLANE")==0)
				|| (strcmp(geom, "PL")==0))
		{
			plane = 1;
		}
		else if ((strcmp(geom, "CIRCLE")==0)
				|| (strcmp(geom, "CI")==0))
		{
			circle = 1;
		}
		else if ((strcmp(geom, "CURVE")==0)
				|| (strcmp(geom, "CV")==0))
		{
			curve = 1;
		}
		else if ((strcmp(geom, "SURF")==0)
				|| (strcmp(geom, "SF")==0))
		{
			surf = 1;
		}
		else if ((strcmp(geom, "MATRIX")==0)
				|| (strcmp(geom, "MX")==0))
		{
			matrix = 1;
		}
		else if ((strcmp(geom, "PV")==0)
				|| (strcmp(geom, "PNTVEC")==0))
		{
			pv = 1;
		}
		else if ((strcmp(geom, "PATERN")==0)
				|| (strcmp(geom, "PN")==0))
		{
			patern = 1;
		}
		else if ((strcmp(geom, "SOLID")==0)
				|| (strcmp(geom, "SO")==0))
		{
			solid = 1;
		}
	}
/*
....Select appropriate mask for geom type
*/
    for (i=0; i<UD_NMENTWD; i++)
    {
		Add_mask[i] = 0;
		if (scaler==1)	 Add_mask[i] = Add_mask[i] | UD_scale[i];
		if (shape==1)	Add_mask[i] = Add_mask[i] | UD_ncl_sh[i];
		if (point==1)	Add_mask[i] = Add_mask[i] | UD_ncl_pt[i];
		if (vector==1)	Add_mask[i] = Add_mask[i] | UD_ncl_ve[i];
		if (line==1)	Add_mask[i] = Add_mask[i] | UD_ncl_ln[i];
		if (plane==1)	Add_mask[i] = Add_mask[i] | UD_ncl_pl[i];
		if (circle==1)	Add_mask[i] = Add_mask[i] | UD_ncl_ci[i];
/*		if (curve==1)	Add_mask[i] = Add_mask[i] | UD_ncl_cv[i] | UD_bspline[i] 
										| UD_rbspline[i] | UD_conics[i];
*/
		if (curve==1)	Add_mask[i] = Add_mask[i] | UD_ncl_allcv[i];
		if (surf==1)	Add_mask[i] = Add_mask[i] | UD_ncl_sf[i] | UD_ncl_mesh[i] | 
			UD_ncl_quilt[i] | UD_ncl_netsf[i] | UD_ncl_trimsf[i] |
			UD_ncl_revsf[i] | UD_ncl_allsf[i]; 
		if (matrix==1)	Add_mask[i] = Add_mask[i] | UD_ncl_mx[i];
		if (pv==1)		Add_mask[i] = Add_mask[i] | UD_ncl_pv[i];
		if (patern==1)	Add_mask[i] = Add_mask[i] | UD_ncl_patern[i];
		if (solid==1)	Add_mask[i] = Add_mask[i] | UD_solid[i];
    }
	ud_lgeo(UU_TRUE, Add_mask);
}

/***********************************************************************
c
c   FUNCTION: check_call_macro()
c			check the paramter field and call macro
c
c   INPUT:  None.
c
c   OUTPUT :
c			None.
c   RETURN: None.
c
**********************************************************************/
static int check_call_macro()
{
	int item;
	int i, j, k, ii, indx, choice, c, nc, stat, rsub;
	UD_DDATA fdat;
	UM_real8 value;
	char fstr[NCL_MAX_LABEL+50];
	char cin[513], dlabel[NCL_MAX_LABEL+15], rlab[NCL_MAX_LABEL+1];
	UM_int2 iflg, detype, clas, pclas, geoflag;
	UM_int4 dsub, psub;
	UM_real8 dvalue, rvalue;
	char prmptr[41], wrdlist[24*WORD_LIMIT+1];
	UM_real8 min, max;
	UM_int2 prsvalue, lenvalue;
	int counter, substr, pflag, psel, pclr;
	char fmt[16], label[NCL_MAX_LABEL+1], chstr[26];
	int outflag, dispflag;
	NCL_cmdbuf cmdbuf;
	char *comma_ptr, *prev_comma_ptr, *index();
	char token[80];
	char *tmpans;
/*
.....first, check if all input field is filled
*/
	k = 0;
	fdat.frmstr = fstr;
	for(j=0,item=0; j<totprm;j++, item++)
	{
		ud_get_field(item,fdat,UU_TRUE);
		nc = strlen(fstr);
		ul_strip_blanks(fstr,&nc);
		if (nc == 0) return -1;
		if (type_of_ans[k] == 0)
			k++;
		else if( type_of_ans[k] > 0)
			k = k+1+(*(type_of_ans+k)*6);
		else if (type_of_ans[k] == -2)
			k++;
		else
			k = k + 21;
	}

	for(k=0;k<512;k++) cin[k] = ' ';
	cin[512] = 0;
	k = 0;
	i = 0;

	fdat.frmint = &outflag;
	ud_get_field(form_item,fdat,UU_FALSE);
	fdat.frmint = &dispflag;
	ud_get_field(form_item+1,fdat,UU_FALSE);
	fdat.frmint = &save_def;
	ud_get_field(form_item+2,fdat,UU_FALSE);
	if (outflag==1)
		sprintf(&cin[i],"CALL/%s",macro_name);
	else
		sprintf(&cin[i],"*CALL/%s",macro_name);

	i = strlen(cin);

	k = 0;
	fdat.frmstr = fstr;
	for(j=0,item=0; j<totprm;j++, item++)
	{
		indx = j+1;
		ncl_getmac_parms(&indx, label, &clas, prmptr, &counter, wrdlist, &geoflag,
			&min, &max, &prsvalue, &lenvalue, &substr, &pflag, &psel, &pclr);
		ncl_getmac_values(&indx, &iflg, &psub, dlabel, &dsub, &dvalue, &pclas,
			&detype);			
		ud_get_field(item,fdat,UU_TRUE);
		choice = *(fdat.frmint);
		if( type_of_ans[k] == 0)
		{
			stat = ncl_get_scalar_value(fstr, &value);
/*
.....Error, the input is not a scalar value
*/
			if (stat==-1)
			{
				ud_wrerr("The input is not the scalar value");
				return -1;
			}
			if ( iflg == 1 || value != dvalue || save_def == 1)
			{
				sprintf(&cin[i],",%s=",&label[0]);
				i = strlen(cin);
				if (lenvalue >0)
				{
					sprintf(fmt,"%%%d.%df", lenvalue, prsvalue);
					sprintf(&cin[i],fmt,value);
				}
				else
					ncl_sprintf(&cin[i],&value,1); 
			}
			if (dispflag)
			{
				indx = j+1;
				ncl_setmac_rvalue(indx, value, NULL, 0, 2);	
			}
			k++;
		}
/*
.....Checkbox field
*/
		if( type_of_ans[k] == -2)
		{
			if ( iflg == 1 || value != dvalue || save_def == 1)
				sprintf(&cin[i],",%s=%s",&label[0],fstr);
			if (dispflag)
			{
				indx = j+1;
				value = choice;
				ncl_setmac_rvalue(indx, value, NULL, 0, 2);	
			}
			k++;
		}
/*
.....Vocabulary word (Choice field)
*/
		else if( type_of_ans[k] > 0)
		{

			//strncpy(chstr, &(wrdlist[choice*24]), 24);
			//chstr[24] = '\0';

			strcpy(chstr,fdat);
			for (ii=strlen(chstr);ii<24;ii++) chstr[ii] = ' ';
			chstr[24] = '\0';

			if ( iflg == 1 || strcmp(chstr,dlabel) != 0 || save_def == 1)
			{
				sprintf(&cin[i],",%s=%s",&label[0],fstr);
/*
				for(c=0; c<24; c++)
				{
					cin[i+c] = chstr[c];
					if (cin[i+c] == '\0') cin[i+c] = ' ';
				}
*/
			}
			k = k+1+(*(type_of_ans+k)*6);
			if (dispflag)
			{
				tmpans = (char *)&rvalue;
				strncpy(tmpans, chstr,24); tmpans[24] = '\0';
				indx = j+1;
				ncl_setmac_rvalue(indx, rvalue, NULL, 0, 1);	
			}
		}
/*
......-1, string only, value or label
*/
		else
		{
			nc = strlen(fstr);
			ul_strip_blanks(fstr,&nc);
/*
.....Fstr could be a defined scalar value string
*/
			stat = ncl_get_scalar_value(fstr, &value);
/*
.....Input is not a scalar value, but a name
*/
			if (stat==-1)
			{
				dlabel[64] = '\0';
				if (dsub!=0)
				{
					dlabel[64] = '\0';
					ii = 63;
					while ((dlabel[ii]==' ')||(ii<0))
						ii--;
					dlabel[ii+1] = '\0';
					sprintf(dlabel, "%s(%d)", dlabel, dsub);
				}
				if (iflg == 1 || strcmp(fstr, dlabel) != 0 || save_def == 1)
					sprintf(&cin[i],",%s=%s",&label[0],fstr);
				if (dispflag)
				{
					ncl_getlabel_sub(fstr, rlab, &rsub);
					indx = j+1;
					ncl_setmac_rvalue(indx, rvalue, rlab, rsub, 1);	
				}
			}
/*
.....Scalar/Number
*/
			else
			{
				if (iflg == 1 || value != dvalue || save_def == 1)
					sprintf(&cin[i],",%s=%s",&label[0],fstr);
				if (dispflag)
				{
					rvalue = value;
					indx = j+1;
					ncl_setmac_rvalue(indx, rvalue, NULL, 0, 2);	
				}
			}
			k = k + 21;
		}
		i = strlen(cin);
		ul_strip_blanks (cin,&i);
	}

	for(k=i;k<512;k++) cin[k] = ' ';
	ncl_init_cmdbuf(&cmdbuf);
	for(prev_comma_ptr = cin;;)
	{
		comma_ptr = index(prev_comma_ptr,',');
		if(!comma_ptr) break;
		strncpy(token,prev_comma_ptr,(int)comma_ptr - (int)prev_comma_ptr + 1);
		token[(int)comma_ptr - (int)prev_comma_ptr + 1] = '\0';
		ncl_add_token(&cmdbuf,token,NCL_nocomma);
		prev_comma_ptr = comma_ptr+1;
	}
	strncpy(token,prev_comma_ptr,(int)cin + i - (int)prev_comma_ptr);
	token[(int)cin + i - (int)prev_comma_ptr] = '\0';
	ncl_add_token(&cmdbuf,token,NCL_nocomma);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....NEED reset the macro pointer to the original name
.....because "ncl_call(&cmdbuf);" will term the current macro
.....but we still inside form and need that
*/
	ncl_set_curmac (macro_name);
	return 0;
}

/*********************************************************************
**    E_FUNCTION :  ncl_macro_call(parms)
**    PARAMETERS
**
**    This routine is the main routine of the "Dynamic Macro Call".
**
**		 INPUT:  parms: include macro name and MODAL macro flag
**						such as "macro, MODAL"
**							 or "macro, ONCE"
**       OUTPUT    : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_macro_call(parms)
char *parms;
{
	char name[NCL_MAX_LABEL+1],buf[80], *str,*strchr();
	int i;
	UM_int2 ifl38,val38,itmp;
	UM_f77_str f77_tmp;
	UM_f77_str f7name;
	int modal;
	int nc;
	UX_pathname fname;
	NCL_cmdbuf cmdbuf;
	UM_int4 kend;
	UM_int2 ierr;
	int sav_err, status;
/*
......set this flag to not display error message
......when play a video file without found the file
......need remember to reset
*/
	sav_err = UD_novideo_error;
	UD_novideo_error = 1;
	modal = NCL_macro_modal;
	status = 0;
/*
.....Initialize routine
*/
	for (i=0; i < NCL_MAX_LABEL+1; i++) name[i] = 0;
/*
.....Call this routine to give user the list of the currently specified
.....macros and to get the answer from him.
*/
	if (parms[0]=='\0')
	{
		if (nclu_list_macros(name,&modal) != UU_SUCCESS) 
		{
			status = 0;
			goto ret;
		}
	}
	else
	{
		ul_to_upper(parms);
		strcpy(name, parms);
		str = strchr(name,',');
		if (str!=NULL)
		{
			*str = '\0'; str++;
		}
		if (str!=NULL)
		{
			if (strcmp(str, "MODAL")==0) modal = UU_TRUE;
		}
	}
/*
.....If there is no selected macro name - return
*/
	if (strlen(name)==0) goto done;
	if (!strncmp(name,"      ",6) ) goto done;
	for (i=strlen(name); i < NCL_MAX_LABEL+1; i++) name[i] = ' ';
	name[NCL_MAX_LABEL] = '\0';
/*
.....Make sure this macro is not currently
.....On the call stack
*/
	UM_init_f77_str(f77_tmp,name, NCL_MAX_LABEL+1);
	lrfind(UM_addr_of_f77_str(f77_tmp),&itmp);
	if (itmp == 1)
	{
		sprintf(buf,"Macro '%s' is currently executing.",name);
		ud_wrerr(buf);
		status = -1;
		goto ret;
	}
/*
.....Macro is not defined
.....try and load it
*/
	else if (itmp == -1)
	{
		strcpy(fname,name);
		ul_to_lower(fname);
		nc = strlen(fname);
		for (i=nc-1 ; i>0 ; i--) if (fname[i] != ' ') break;
		fname[i+1] = '\0';
		ux_add_ftype("mac",fname,UX_PRTERRS);
		nc = strlen (fname);
		for (i=nc;i<UX_MAX_PATH_LEN;i++) fname[i] = ' ';
		UM_init_f77_str(f7name,fname,UX_MAX_PATH_LEN);
		ncl_set_cmdmode(UU_TRUE);
		lodmac(UM_addr_of_f77_str(f7name),&kend,&ierr);
		ncl_set_cmdmode(UU_FALSE);
/*
.....Process the INCLUD file
*/
		if (ierr == 0)
		{
			ncl_init_cmdbuf(&cmdbuf);
			sprintf(fname,"*RUN/%d",kend+1);
			ncl_add_token(&cmdbuf,fname,NCL_nocomma);
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			rstrct();
		}
		lrfind(UM_addr_of_f77_str(f77_tmp),&itmp);
	}
/*
.....Macro is still not defined
*/
	if (itmp == -1)
	{
		sprintf(buf,"Macro '%s' is not defined.",name);
		ud_wrerr(buf);
		status = -1;
		goto ret;
	}
/*
.....Call the macro form routine to put the form on the screen,
.....to get the answers and to execute the macro.
*/
	strcpy(macro_name, name);
	nclu_macro_form (name,modal);
/*
.....Check if the macro were executed correctly (without any NCL errors)
.....go back to the "Control menu", if we got an error - leave the subsystem.
.....See ZNCLCNTL.MU for details.
*/
	ifl38 = 38;
	getifl(&ifl38,&val38);
	if (val38 == 2)
	{
		status = -1;
		goto ret;
	}
done:;
	um_close_pocket_window(UM_DRAWING_WINDOW);
	um_close_pocket_window(UM_GRAPHIC_WINDOW);
/*
.....save the NCL_macro_xxx into ncl_macro.mod file
*/
	S_save_macro_mf();
	status = 0;
ret:;
	UD_novideo_error = sav_err;
	return(status);
}
/*********************************************************************
**
**			filter_noop
**
*********************************************************************/
static UD_FSTAT filter_noop(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	*fieldno = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION :  string_picked(fieldno,val,stat)
**    PARAMETERS
**
**    This routine is the callback function of paramter prompt button
**
**		 INPUT:  fieldno: include macro name and MODAL macro flag
**				val:	
**				stat:
**
**       OUTPUT    : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT string_picked(fieldno,val,fstat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT fstat;
{
	char lstr[82];
	int cmdreject, status,stat;
	int indx;
	UU_LOGICAL first;
	UM_int2 iflg, detype, clas, pclas,geoflag;
	UM_int4 dsub, psub;
	UM_real8 dvalue;
	char prmptr[41],wrdlist[24*WORD_LIMIT+1],label[NCL_MAX_LABEL+1],dlabel[NCL_MAX_LABEL+15];
	UM_real8 min, max;
	UM_int2 prsvalue, lenvalue;
	int counter, substr, pflag, psel, pclr;
    int Add_mask[UD_NMENTWD];
	UD_FSTAT dstat;
/*
.....Initalize routine
*/
	dstat = UD_FLDOK;
	first = UU_TRUE;
/*	if (UD_form_bypick == 0) goto done;*/
/*
.....Find the matching Macro parameter
.....for this form field
*/
	status = match_field_parm(fieldno, &indx);
	if (status==-1)
		goto done;
	ncl_getmac_parms(&indx, label, &clas, prmptr, &counter, wrdlist,
		&geoflag, &min, &max, &prsvalue, &lenvalue, &substr, &pflag, &psel, &pclr);
	ncl_getmac_values(&indx, &iflg, &psub, dlabel, &dsub, &dvalue, &pclas,
		&detype);
/*
.....Update the field
*/
/*
	ud_update_answer(*fieldno, (int*)val->frmstr);
	ud_update_form(0);
*/
/*
.....Determine if we should call Macro immediately
*/
	if (pflag==0)
		stat = check_call_macro();
	else
		stat = -1;
/*
.....Otherwise, traverse to the next field
.....If next field allows By Pick
*/
	if (stat==-1)
	{
		*fieldno += 1;
		if ((*fieldno>form_item) && (first)) 
		{
			first = UU_FALSE;
			*fieldno = 0;
		}
		else if ((*fieldno>form_item) && (first==0)) 
			goto done;
	}
/*
.....Loop through fields
*/
/*
......when fstat==UD_FLDOK, means the call from the execusion of the function accept
......only in the case, we goes to picking loop.
*/
	if (fstat==UD_FLDOK)
		string_picked_loop(fieldno,val,fstat);
	dstat = UD_FLDOK;
/*
.....End of routine
*/
done:;
	return(dstat);
}

/*********************************************************************
**    E_FUNCTION :  string_picked_loop(fieldno,val,fstat)
**    PARAMETERS
**
**    This routine is the callback function of paramter prompt button
**
**		 INPUT:  fieldno: include macro name and MODAL macro flag
**				val:	
**				stat:
**
**       OUTPUT    : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT string_picked_loop(fieldno,val,fstat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT fstat;
{
	char lstr[82];
	int cmdreject, status, first,stat,ifl;
	int indx;
	UU_LOGICAL nxt;
	UM_int2 iflg, detype, clas, pclas,geoflag;
	UM_int4 dsub, psub;
	UM_real8 dvalue;
	char prmptr[41],wrdlist[24*WORD_LIMIT+1],label[NCL_MAX_LABEL+1],dlabel[NCL_MAX_LABEL+15];
	UM_real8 min, max;
	UM_int2 prsvalue, lenvalue;
	int counter, substr, pflag, psel, pclr;
	int Add_mask[UD_NMENTWD];
/*
.....Take down the form
*/
	ud_form_invis();
/*
.....set UD_form_bypick = 1
.....to let ud_gevt1 know we don't
.....need "BY TEXT" active
*/
	UD_form_bypick = 1;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;
	first = 1;
	nxt = UU_FALSE;
	ud_lgeo(UU_FALSE, Add_mask);
	while (1)
	{
/*
.....if the field is the prompt button, the field number will add one
*/
		status = match_field_parm(fieldno, &indx);
		if (status==-1)
			goto done;

		ncl_getmac_parms(&indx, label, &clas, prmptr, &counter, wrdlist, &geoflag,
			&min, &max, 
				&prsvalue, &lenvalue, &substr, &pflag, &psel, &pclr);
		ncl_getmac_values(&indx, &iflg, &psub, dlabel, &dsub, &dvalue, &pclas, &detype);
/*		if (!(((pclas == 0) && (iflg!=3)) || ((pclas>0) && geoflag))) */
		if (!((pclas == 0) || ((pclas>0) && geoflag)))
			goto done;
		if (strlen(prmptr)==0)
			strcpy(prmptr, label);
/*
.....Set the appropriate selection mask
*/ 
		if ((pclas>0) && geoflag)
			limit_pick_flag(wrdlist, counter);

		ifl = 1;
		if (macro_substr[*fieldno]==1) ifl = 4;
		if (macro_substr[*fieldno]==2) ifl = 5;
		if (psel)
		{
			status = ud_get_pick_selstr(prmptr, &(S_sel_list[*fieldno].select_list), pclr);
		}
		else
			status = ud_get_pickstr(prmptr, UD_DASSTRING, lstr, 20, 20, ifl);
/*
.....release the DAS picking limit
*/
		ud_lgeo(UU_FALSE, Add_mask);

		if (UD_form_bypick==0)
			goto done;
		if (status == DE_DONE)
		{
			goto done;
		}
		if (psel==0)
		{
			ud_update_answer(*fieldno, (int*)lstr);
			ud_update_form(0);
		}
/*
.....check the field to see if need to call macro imidiately now
*/
		if (pflag==0 && !nxt)
			stat = check_call_macro();
		else
			stat = -1;
/*
.....traverse to pick the next
*/
		if (stat==-1)
		{
			nxt = UU_TRUE;
			*fieldno += 1;
			if ((*fieldno>form_item) && (first)) 
			{
				first = 0;
				*fieldno = 0;
			}
			else if ((*fieldno>form_item) && (first==0)) 
				break;
		}
	}
/*
.....End of routine
.....Redisplay the form
*/
done:;
	ud_lgeo(UU_FALSE, Add_mask);
	ud_form_vis();
	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnVideo(fieldno, val, stat)
**       Routine to enter dynamic viewing within the dynamic generated macro formcallback for video button
**		in new routine (added video button, we doing nothing, the video button function itself will handle)
**		but when use old macro file, since there is no video button, we execute OnView here
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
static UD_FSTAT OnVideo(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (UD_macroflag)
		return(UD_FLDOK);
/*
.....Take down form
*/
	ud_form_invis();
/*
.....Enter dynamic viewing
*/
	uz_dyn_mouse();
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_form_vis();
	return(UD_FLDOK);
}


/*********************************************************************
**    S_FUNCTION     :  OnView(fieldno, val, stat)
**       Routine to enter dynamic viewing within the dynamic generated macro form
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
static UD_FSTAT OnView(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Take down form
*/
	ud_form_invis();
/*
.....Enter dynamic viewing
*/
	uz_dyn_mouse();
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_form_vis();
	return(UD_FLDOK);
}

/*********************************************************************
**
**			load_macro
**
*********************************************************************/
static UD_FSTAT load_macro(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UX_pathname fname,dir,fn;
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	int nc,i;
	NCL_cmdbuf cmdbuf;
	UM_int2 ierr,ifl;
	UM_int4 kend;
	UM_f77_str f7name;
/*
.....Get the file to load
*/
	fname[0] = '\0' ; nc = 0;

	strcpy(paths, "NCL_INCDIR");
	strcpy(path_des, "System");
	ud_get_filename1(NULL, "Load Macro File", "*.mac", fname,&nc, "Macro Files (*.mac)", 1, UU_FALSE, paths, path_des);
/*
.....Issue the INCLUD command
*/
	if (nc != 0)
	{
		if (nc > 66)
		{
			ul_break_fname(fname,dir,fn);
			strcpy(fname,fn);
			nc = strlen(fname);
		}
		for (i=nc;i<UX_MAX_PATH_LEN;i++) fname[i] = ' ';
		UM_init_f77_str(f7name,fname,UX_MAX_PATH_LEN);
		ncl_set_cmdmode(UU_TRUE);
		lodmac(UM_addr_of_f77_str(f7name),&kend,&ierr);
		ncl_set_cmdmode(UU_FALSE);
/*
.....Process the INCLUD file
*/
		if (ierr == 0)
		{
			ncl_init_cmdbuf(&cmdbuf);
			sprintf(fname,"*RUN/%d",kend+1);
			ncl_add_token(&cmdbuf,fname,NCL_nocomma);
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			rstrct();
/*
.....Close the form so that the main routine can
.....redisplay it with any newly defined macros
*/
			ifl = 2;
			getifl(&ifl,&ierr);
			if (ierr == 0)
			{
				newload = 1;
				ud_mfclose_form(1);
/*
.....if we close form, we must
.....cancel the redisplay form
*/
				*fieldno = -1;
			}
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**
**			view_macro
**
*********************************************************************/
static UD_FSTAT view_macro(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char name[NCL_MAX_LABEL+1],mac[NCL_MAX_LABEL+50], desp[41], classname[21], title[NCL_MAX_LABEL+50];
	UD_DDATA data;
	int outflag, dispflag;
/*
.....Create the drawing file name
*/
/*...use tablelist now
	if (maclist)
	{
		data.frmstr = mac;
		ud_get_field(3,data,UU_FALSE);
		strncpy(name, mac, NCL_MAX_LABEL);
	}
	else
*/		strncpy(name,macro_name,NCL_MAX_LABEL);

	name[NCL_MAX_LABEL] = '\0';
/*
.....View the drawing
*/
	ncl_getmac_desp(name, classname, &outflag, &dispflag, desp);
	sprintf (title, "%s: %s", name, desp);
/*
.....close the drawing first, then reopen
*/
	um_close_pocket_window(UM_DRAWING_WINDOW);
	um_close_pocket_window(UM_GRAPHIC_WINDOW);

	um_load_pocket_drawing(title, name,name,"NCL_INCDIR", 1);
/*
.....let the form know the pocket window is open
......because when form close, it will close pocket window
......because form is the parent window but it don't call
......um_close_pocket_window to reset pocket pointer until
......we destroy the pocket window parent (which is form), and then we
......call um_close_pocket_window which will cause an error
......because pocket window already destroyed (when destroy the parent window form)

......or we can use main_frame as parent window for pocket window, but the drawback
......is that when shrink the pocket window or close pocket window, it will reactive
......the parent window (which is main frame) as it should not because the form is still open
......Yurong
*/
	ud_setform_pocket(0, 1);
	return(UD_FLDOK);
}

/*********************************************************************
**
**			filter_macro
**
*********************************************************************/
static UD_FSTAT filter_macro(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,nc,ifl,icnt,len;
	char mac[NCL_MAX_LABEL+1], buf[NCL_MAX_LABEL+1];
	char *ptr1,fstr[NCL_MAX_LABEL+1], clastr[21], classname[21], prmpt[41];
	UD_DDATA fdat;
	UD_DDATA fcdat;
	int outflag, dispflag;
	char tempptr[NCL_MAX_LABEL+50], *tok;
/*
.....Get the filter string
*/
	if (*fieldno == -1)
	{
		strcpy(fstr,filter);
		strcpy(clastr, cur_class);
	}
	else
	{
		fdat.frmstr = fstr;
		ud_get_field(filt_fld,fdat,UU_FALSE);
		fcdat.frmstr = clastr;
		ud_get_field(clas_fld, fcdat,UU_FALSE);
	}
	ul_to_upper(fstr);
	ul_to_upper(clastr);
/*
.....Determine type of filter to support
.....Valid filter styles are:
.....*    = 1
.....mk*  = 2
.....*mk  = 3
.....*mk* = 4
*/
	nc = strlen(fstr);
	ul_strip_blanks(fstr,&nc);
	if (nc == 0)
	{
		strcpy(fstr,"*");
		nc = 1;
	}
	ul_to_upper(fstr);
	if (strcmp(fstr,"*") == 0) ifl = 1;
	else if (fstr[0] == '*')
	{
		if (fstr[nc-1] == '*')
		{
			ifl = 4;
			strncpy(mac,&fstr[1],nc-2);
			mac[nc-2] = '\0';
		}
		else
		{
			ifl = 3;
			strcpy(mac,&fstr[1]);
		}
	}
	else if (fstr[nc-1] == '*')
	{
		ifl = 2;
		strncpy(mac,fstr,nc-1);
		mac[nc-1] = '\0';
	}
	else
	{
		ifl = 2;
		strcpy(mac,fstr);
	}
	macro_namelist.num_item = fitem;
	if (macro_namelist.num_item>0)
		macro_namelist.data = (UD_ITEMDATA *) uu_malloc(macro_namelist.num_item*sizeof(UD_ITEMDATA));
	for (i=0; i<macro_namelist.num_item;i++)
	{
		(macro_namelist.data[i]).itemnum = macro_namelist.num_col;
		(macro_namelist.data[i]).data_items = 
					(char **) uu_malloc(macro_namelist.num_col*sizeof(char*));
	}
	nc = strlen(mac);
/*
.....Filter macro names
*/		
	ptr1 = ptr1_first;
	icnt = 0;
	for (i=0;i<fitem;i++)
	{
		ptr1 = (char *)uu_lsnext(ptr1);
		strcpy(tempptr, ptr1);
		tok = (char*)strtok(tempptr, " \r\n");
		if (ifl == 1 ||
		   (ifl == 2 && strncmp(mac,tok,nc) == 0) ||
		   (ifl == 3 && strcmp(mac,&tok[strlen(tok)-nc]) == 0) ||
			(ifl == 4 && strstr(tok,mac) != 0))
		{
/*
.....check the classname is the same
*/
			ncl_getmac_desp(tok, classname, &outflag, &dispflag, prmpt);
			if (((strcmp(clastr, "ALL")==0) || 
				(strcmp(clastr, classname)==0))
				&& (stricmp(classname, "none")!=0))
			{
				strncpy(buf, ptr1, NCL_MAX_LABEL);
				buf[NCL_MAX_LABEL] = '\0';
				for (j=strlen(buf);j>0;j--) 
					if (buf[j-1] != ' ') break;
				buf[j] = '\0';
				len = strlen(buf);
				macro_namelist.data[icnt].data_items[0] = (char*)uu_malloc((len+1)*sizeof(char));
				strcpy(macro_namelist.data[icnt].data_items[0], buf);
				len = strlen(prmpt);
				macro_namelist.data[icnt].data_items[1] = (char*)uu_malloc((len+1)*sizeof(char));
				strcpy(macro_namelist.data[icnt].data_items[1], prmpt);
/*
......add macro class
*/
				macro_namelist.data[icnt].data_items[2] = (char*)uu_malloc(21*sizeof(char));
				strcpy(macro_namelist.data[icnt].data_items[2], classname);
				icnt++;
			}
		}
	}
/*
.....even list len is 0, we still need column label to create the empty
.....table list in order to add list item later, so don't free list's col_label
.....it never changed when form display, we only free it after form done.
*/
	if (icnt==0)
	{
/*
		for (i=0; i<macro_namelist.num_col;i++)
		{
			uu_free(macro_namelist.col_label[i]);
		}
		uu_free(macro_namelist.col_label);
*/
		for (i=0; i<macro_namelist.num_item; i++)
		{
			if (macro_namelist.data[i].itemnum>0)
			{
				if (macro_namelist.data[i].data_items!=NULL)
					uu_free(macro_namelist.data[i].data_items);
				macro_namelist.data[i].data_items = NULL;
			}
		}
		if ((macro_namelist.num_item)&&(macro_namelist.data!=NULL))
			uu_free(macro_namelist.data);
		macro_namelist.data = NULL;
	}
	macro_namelist.num_item = icnt;
	macro_namelist.answer = -1;
/*
.....Store filtered list
*/
	if (*fieldno != -1) 
	{
		ud_update_answer(list_fld, &macro_namelist);
		resort_table();
	}
	return(UD_FLDOK);
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
......"Name" "Description" "Macro Class"
*/
	switch(lParamSort)
	{
	case 0:
		nRetVal = strcmp(pData1->data_items[0],
                                 pData2->data_items[0]);
		break;
	case 1:	
		nRetVal = strcmp(pData1->data_items[1],
                                 pData2->data_items[1]);
	case 2:	
		nRetVal = strcmp(pData1->data_items[2],
                                 pData2->data_items[2]);
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
......"Name" "Description" "Macro Class"
*/
	switch(lParamSort)
	{
	case 0:	
		nRetVal = -strcmp(pData1->data_items[0],
                                 pData2->data_items[0]);
		break;
	case 1:
		nRetVal = -strcmp(pData1->data_items[1],
                                 pData2->data_items[1]);
	case 2:
		nRetVal = -strcmp(pData1->data_items[2],
                                 pData2->data_items[2]);
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
	if ((name_click==0)&&(descript_click==0)&&(class_click==0))
		return;
	
	info.flag = saved_info.flag;
	info.frmid = saved_info.frmid;
	info.fldid = saved_info.fldid;
	info.col = saved_info.col;
	info.row = -1;
		
	if ((info.col==0)&&(name_click%2==0))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc2);
	}
	else if ((info.col==0)&&(name_click%2))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc);
	}
	if ((info.col==1)&&(descript_click%2==0))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc2);
	}
	else if ((info.col==1)&&(descript_click%2))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc);
	}
	if ((info.col==2)&&(class_click%2==0))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc2);
	}
	else if ((info.col==2)&&(class_click%2))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc);
	}
}
/*********************************************************************
**    S_FUNCTION     :  OnInitForm(fieldno,val,stat)
**       Method called at when macro list form is initialized.
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
	char disp[41],trav[41];
	UD_initfrm_intry = UU_NULL;
/*
.....set the init sort method for cutter list
*/
/*
......if saved_info.col<0, mean the never sorted by user, don't
......changed here, QAR97159 item 4
*/
/*	if (saved_info.col<0) saved_info.col = 0; */
	if ((saved_info.col==0)&&(name_click%2==0))
	{
		ud_setform_sortfunc(0, 3, (UD_SMETHOD)SortFunc2);
	}
	else if ((saved_info.col==0)&&(name_click%2))
	{
		ud_setform_sortfunc(0, 3, (UD_SMETHOD)SortFunc);
	}
	if ((saved_info.col==1)&&(descript_click%2==0))
	{
		ud_setform_sortfunc(0, 3, (UD_SMETHOD)SortFunc2);
	}
	else if ((saved_info.col==1)&&(descript_click%2))
	{
		ud_setform_sortfunc(0, 3, (UD_SMETHOD)SortFunc);
	}
	if ((saved_info.col==2)&&(class_click%2==0))
	{
		ud_setform_sortfunc(0, 3, (UD_SMETHOD)SortFunc2);
	}
	else if ((saved_info.col==2)&&(class_click%2))
	{
		ud_setform_sortfunc(0, 3, (UD_SMETHOD)SortFunc);
	}
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  static OnListCalbacks(fieldno, val, stat)
**       Method called for macro list table
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
	int sel, toolno;
	char video_file[UX_MAX_PATH_LEN];
	UD_ITEMDATA *data;
	UD_TABLEINFO *info = (UD_TABLEINFO *)(val->frmint);
	if (info->flag==1)
	{
/*
......list selected, doing selection callback
......if it is un-select, info->row = -1;
*/
		macro_namelist.answer = info->row;
		if (info->row>=0)
		{
			data = (UD_ITEMDATA *)(info->data_ptr);
			strcpy(macro_name, data->data_items[0]);
/*
.....Set new video player filename
*/
			strcpy(video_file, macro_name);
			strcat(video_file, ".mp4");
			ud_update_videofile(0, 7, (int*)video_file);
		}
		else
			macro_name[0] = '\0';
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
		if ((info->col==0)&&(name_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			name_click++;
		}
		else if ((info->col==0)&&(name_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			name_click++;
		}
		if ((info->col==1)&&(descript_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			descript_click++;
		}
		else if ((info->col==1)&&(descript_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			descript_click++;
		}
		if ((info->col==2)&&(class_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			class_click++;
		}
		else if ((info->col==2)&&(class_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			class_click++;
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION :  nclu_list_macros(name,modal);
**    PARAMETERS
**
**       INPUT     : none
**       OUTPUT    :
**                   name   -  The name of the selected macro or the NULL string
**                   modal  -  UU_TRUE if this is a modal Macro Call form
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_list_macros(name,modal)
char *name;
int *modal;
{
	int i,j,n,status,class,outflag, dispflag;
	char *ptr1,*tptr,*tptrf;
	UD_METHOD save_entry;
	UU_LOGICAL cmdreject;
	int fieldno, clsnum;
	UD_DDATA val;
	UD_FSTAT stat;
/*
.....Macro description variables
*/
	char classname[21], prmpt[41];
	char temp[1024],mname[NCL_MAX_LABEL+1];
	UM_f77_str f77_tmp,f77_tmp1;
	char video_file[UX_MAX_PATH_LEN];
	static int first=1;
/*
.....Form fields
*/
	static UD_METHOD methods[] = {filter_macro, filter_macro, filter_macro, OnListCalbacks,
		filter_noop,load_macro,view_macro, UU_NULL};
	static char called[] = {6,6,6,6,6,6,6,6};
	static char traverse[] = {1,1,1,1,1,1,1,1};
	static char display[] = {1,1,1,1,1,1,1,1,1};
	int *ans[8];
	int button;
/*
.....Set up filter
*/
	if (first == 1)
	{
		strcpy(filter,"*");
		strcpy(cur_class, "ALL");
		first = 0;
	}
/*
.....Initialize routine
*/
	ptr1_first = UU_NULL;
	tptrf = UU_NULL;
	status = UU_SUCCESS;
	macro_name[0] = '\0';
/*
.....Disable command reject
*/
	UD_MARK (cmdreject, UU_FALSE);
	if (cmdreject != 0)
	{
		um_close_pocket_window(UM_DRAWING_WINDOW);
		um_close_pocket_window(UM_GRAPHIC_WINDOW);
		status = UU_FAILURE;
		goto done;
	}
/*
.....Create the list of the currently defined macros.
*/
	tptr = (char *)uu_lsnew();
	tptrf = tptr;
	ptr1 = (char *)uu_lsnew();
	ptr1_first = ptr1;
/*
.....Could not allocate memory for the list
*/
	if (ptr1 == 0 || tptr == 0)
	{
		ud_wrerr("Could not allocate enough memory for internal usage.");
		status = UU_FAILURE;
		goto done;
	}
/*
.....Get list of the all currently defined macros.
*/
repeat:;
	class  = 11;
	status = ncl_entnam_list (class, tptr, &fitem);
/*
.....Append Macro descriptions to list
*/
	UM_init_f77_str(f77_tmp,mname, NCL_MAX_LABEL+1);
	UM_init_f77_str(f77_tmp1,temp,1024);
	ptr1 = ptr1_first;
	mclass_list.item = (char **) uu_malloc((fitem+1) *sizeof(char *));
	clsnum = 0;
	mclass_list.item[0] = (char *) uu_malloc(21*sizeof(char));
	strcpy(mclass_list.item[0], "ALL");
	clsnum++;
	for (i=0;i<fitem;i++)
	{
		tptr = (char *)uu_lsnext(tptr);
/*
........Create new list item to hold prompt
........Original list allocated only enough memory
........for the Macro label
......This list should includ macro name and macro description (40 + 8 + spaces)
......Yurong
*/
/*
......the macro name now can be NCL_MAX_LABEL=64 chars, we need (40+64+spaces)
*/
		ptr1 = (char *)uu_lsinsrt(ptr1, NCL_MAX_LABEL+50);
		strcpy(ptr1,tptr);
		ncl_getmac_desp(ptr1, classname, &outflag, &dispflag, prmpt);

		for (n=strlen(ptr1); n<NCL_MAX_LABEL+1; n++)
			ptr1[n] = ' ';
		ptr1[n] = '\0';
		strcat (ptr1, " ");
		strcat (ptr1, prmpt);
/*
.....store macro class into mclass_list
*/
		for (j=0; j<clsnum; j++)
		{
			if (strcmp(classname, mclass_list.item[j])==0)
				break;
		}
		if (j>=clsnum)
		{
			if (stricmp(classname, "none")!=0)
			{
				mclass_list.item[clsnum] = (char *) uu_malloc(21 * sizeof(char));
				strcpy(mclass_list.item[clsnum], classname);
				clsnum++;
			}
		}		
	}
	mclass_list.num_item = clsnum;
	mclass_list.answer = (char *) uu_malloc(21 * sizeof(char));
	strcpy(mclass_list.answer, cur_class);
	ud_list_sort(&mclass_list);
/*
......initial macro_namelist data structure
*/
/*
......Name	Description class
*/
	macro_namelist.num_col = 3; 
	macro_namelist.num_item = fitem;
	macro_namelist.answer = -1;

	if (macro_namelist.num_col>0)
		macro_namelist.col_label = (char**) uu_malloc(macro_namelist.num_col*sizeof (char*));
	for (i=0; i<macro_namelist.num_col;i++)
	{
		macro_namelist.col_label[i] = (char*) uu_malloc(20*sizeof(char));
	}
	strcpy(macro_namelist.col_label[0], "Name");
	strcpy(macro_namelist.col_label[1], "Description");
	strcpy(macro_namelist.col_label[2], "Macro Class");
/*
.....filter put macro list
*/
	fieldno = -1;
	filter_macro(&fieldno,&val,stat);
	macro_namelist.sort = saved_info.col;
/*
.....Get form input
*/
	ans[0] = (int *)&button;
	ans[1] = (int *)filter;
	ans[2] = (int *) &mclass_list;
	ans[3] = (int *)&macro_namelist;
	ans[4] = (int *)modal;
	ans[5] = UU_NULL; ans[6] = UU_NULL;
	video_file[0] = '\0';
	ans[7] = (int *)video_file;
	newload = 0;
	maclist = UU_TRUE;
	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = OnInitForm;
/*
.....initial default select
*/
//	strcpy(macro_name, macro_namelist.data->data_items[0]);
	status = ud_form1("maclist.frm",ans,ans,methods,called,display,traverse);
	if (status==-1)
	{
		um_close_pocket_window(UM_DRAWING_WINDOW);
		um_close_pocket_window(UM_GRAPHIC_WINDOW);
		status = UU_FAILURE;
		goto done;
	}
	strcpy(cur_class, mclass_list.answer);
	NCL_macro_modal = *modal;
/*
.....The user loaded an external macro file
.....Redisplay the form
*/
	if (newload == 1)
	{
		ud_free_tlist(&macro_namelist);
		ud_free_flist(&mclass_list);
		goto repeat;
	}
/*
.....Return selected macro
*/
	strcpy(name,  macro_name);
/*
.....Free any allocated memory
*/
done:;
	UD_initfrm_intry = save_entry;
	if (ptr1_first != UU_NULL) uu_lsdel(ptr1_first);
	if (tptrf != UU_NULL) uu_lsdel(tptrf);
	ud_free_tlist(&macro_namelist);
	ud_free_flist(&mclass_list);
	ptr1_first = UU_NULL; tptrf = UU_NULL; 
	UD_UNMARK (cmdreject);
/*
.....End of routine
*/
	maclist = UU_FALSE;
	return(status);
}

/*********************************************************************
**   I_FUNCTION: S_init_layerpfrm(fieldno,val,stat)
**      Callback function for the "->" button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: UD_FLDOK.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT S_init_MacroForm(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_dispfrm_set_init_datatyp(0, S_data_num, S_data_type);
	ud_dispfrm_get_form_datatyp(0, S_data_num, S_data_type_f);
	return(UD_FLDOK);
}
/*********************************************************************
**    E_FUNCTION :  nclu_macro_form(name);
**      Create a dynamic macro form, call it & pass the result to NCL.
**    PARAMETERS
**       INPUT:
**         name        - Name of macro.
**         modal       - UU_TRUE if this is a modal Macro Call form.
**       OUTPUT:
**         none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_macro_form (name, modal)
char *name;
int modal;
{
	int i,j,k,ii,c,m,stat,status,cmdreject,indx, rsub;
	char *tp,*cp;
	int (*(*ans_ptr));
	ANS_DATA *ans, *def;
	ANS_DATA *answer;
	int *intern_ptr;
	NCL_cmdbuf cmdbuf;
	char token[80];
	char fmt[16], label[NCL_MAX_LABEL+15], rlab[NCL_MAX_LABEL+1];
	char *comma_ptr, *prev_comma_ptr, *index();
	char ctmp[NCL_MAX_LABEL+15],cin[1536],dlabel[NCL_MAX_LABEL+15], deflabel[NCL_MAX_LABEL+15];
	char *tmpans;
	int nc,item, rtyp;
	char *called,*traverse,*display, tmpstr[256];
	UD_METHOD *methods;
	UM_int2 iflg, detype, clas, pclas,geoflag;
	UM_int4 dsub, psub, nclkey;
	UM_real8 dvalue, rvalue,value;
	char prmptr[41], wrdlist[24*WORD_LIMIT+1], descrip[41];
	UM_real8 min, max;
	UM_int2 prsvalue, lenvalue;
	int counter, substr, pflag, form_file, psel, pclr;
	char classname[21];
	UX_pathname fname,ftmp;
	int outflag, dispflag;
	char *savptr = NULL,*ncl_get_macptr();
	FILE *fd;
	char macnam[NCL_MAX_LABEL+1];
	UD_METHOD save_entry;
	char selstr[256];
	int repeat = 0;
	int times = 0;

    ans = UU_NULL;
    ans_ptr = UU_NULL;
    type_of_ans = UU_NULL;
    methods = UU_NULL;
    called = UU_NULL;
    traverse = UU_NULL;
    display = UU_NULL;
	
	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = S_init_MacroForm;

	macro_modal = modal;
	for (i=0;i<500;i++)
	{
		macro_pflag[i] = 0;
		macro_substr[i] = 0;
	}
/*
.....Disable command reject
*/
	UD_MARK (cmdreject, UU_FALSE);
	if (cmdreject != 0)
	{
		status = UU_FAILURE;
		goto done;
	}
	save_def = NCL_macro_outdefault;
/*
.....save current active macro and set the nameed macro active
.....in order to get the macro data
*/
	savptr = ncl_get_macptr();
	ncl_set_curmac (name);
	ncl_getmac_pnum(&totprm);
	ncl_getmac_desp(name, classname, &outflag, &dispflag, descrip);
/*
...
...Create "form" in memory if macro has parameters
...
*/
modal_repeat:
/*
.....Reset the form so all the fields can be reset - ASF 12/19/13.
*/
	S_tmp_dat_n = 0;
	ncl_calform_initsize(totprm);
	if (totprm>0)
	{
		if (!modal || times == 0)
		{
			ans     = (ANS_DATA *)uu_malloc(sizeof(ANS_DATA)*(totprm+6));
			def     = (ANS_DATA *)uu_malloc(sizeof(ANS_DATA)*(totprm+6));
			ans_ptr = (int **)uu_malloc(sizeof(int*)*(totprm*2+6));
		
			methods = (UD_METHOD *)uu_malloc(sizeof(UD_METHOD)*(totprm*2+7));
			called = (char *)uu_malloc(sizeof(char)*(totprm*2+7));
			traverse = (char *)uu_malloc(sizeof(char)*(totprm*2+7));
			display = (char *)uu_malloc(sizeof(char)*(totprm*2+7));
			type_of_ans = (int *)uu_malloc(sizeof(int)*(24*WORD_LIMIT/4+1)*totprm);
		}
		frm_ptr = (char *)uu_lsnew();
		if (frm_ptr == 0)
		{
			ud_wrerr("Could not allocate memory for the form.");
			status = UU_FAILURE;
			goto done;
		}
		frm_ptr = (char *)uu_lsinsrt(frm_ptr,13000);
		for(i=0;i<FORM_MAX_INT_HEADER+20;i++) frm_ptr[i] = 0;
		k = 0;
		methods[0] = filter_noop;
		called[0] = 6;
		traverse[0] = 1;
		display[0] = 1;
/*		ncl_getmac_desp(name, classname, &outflag, &dispflag, descrip); */
		 
		for(j=0,item=0,i=FORM_MAX_INT_HEADER,stat=totprm; j<totprm;j++, item++, stat=0)
		{
			if(i > 12000)
			{
				ud_wrerr("Not enough memory allocated for the form storage.");
				status = UU_FAILURE;
				goto done;
			}
			indx = j + 1;
			ncl_getmac_parms(&indx, label, &clas, prmptr, &counter, wrdlist,
				&geoflag, &min, &max, 
				&prsvalue, &lenvalue, &substr, &pflag, &psel, &pclr);
			macro_substr[item] = substr;
			macro_pflag[item] = pflag;

			ncl_getmac_values(&indx, &iflg, &psub, dlabel, &dsub, &dvalue, &pclas, &detype);			
			if (strlen(prmptr)==0)
				strcpy(prmptr, label);
			form_elmt(stat, &i, type_of_ans, frm_ptr, pclas, wrdlist, counter,
				geoflag, substr, prmptr, prsvalue, lenvalue, min, max, iflg, psel);
/*
... Create defaults & set up pointers for the form.
... If type_of_ans for the answer is 0 or N>0 ans_ptr for this
... answer will point to the cell in the ans array. If it is N<0
... ans_ptr will point to the string reserved in the type_of_ans array.
*/
			methods[item] = filter_noop;
			if( type_of_ans[k] == 0)
			{
				ans[j].frmval = 0.0;
				if (dispflag)
				{
					status = ncl_getmac_rvalue(indx, &rvalue, rlab, &rsub, &rtyp);
					if (rtyp==0)
						rtyp = iflg-1;
					if (status==0)
					{
						ans[j].frmval = rvalue;
						iflg = 3;
					}
					else
/*
.....use default value
*/
						ans[j].frmval = dvalue;
				}
				else if (iflg == 3) ans[j].frmval = dvalue;

				ans_ptr[item] = (int*)&ans[j];
				if (iflg == 1)
				{
					ans[j].frmstr[0] = '\0';
					def[j].frmstr[0] = '\0';
					S_data_type[j] = UD_DASSTRING;
				}
				else
				{
					def[j].frmval = ans[j].frmval;
					S_data_type[j] = UD_DASUNITLESS;
				}
				k++;
			}
/*
........Checkbox field
*/
			else if (type_of_ans[k] == -2)
			{
				ans[j].frmint = 0;
				if (dispflag)
				{
					status = ncl_getmac_rvalue(indx, &rvalue, rlab, &rsub, &rtyp);
					if (rtyp==0)
						rtyp = iflg-1;
					if (status==0)
					{
						ans[j].frmint = rvalue;
						iflg = 3;
					}
					else
/*
.....use default value
*/
						ans[j].frmint = dvalue;
				}
				else if (iflg == 3) ans[j].frmint = dvalue;

				ans_ptr[item] = (int*)&ans[j];
				def[j].frmint = ans[j].frmint;
				S_data_type[j] = UD_DASINT;
				k++;
			}
/*
.....Vocabulary word
*/
			else if( type_of_ans[k] > 0)
			{
				if (dispflag)
				{
					status = ncl_getmac_rvalue(indx, &rvalue, rlab, &rsub, &rtyp);
					if (status==0)
					{
						strncpy(deflabel, rlab, 24);
						deflabel[24] = '\0';
						iflg = 2;
					}
				}          
				intern_ptr = (int *)&ans[j];
				*intern_ptr = 0;
				ans_ptr[item] = (int*)&ans[j];      
				cp = (char *)&def[j];
/*
........Store the default answer
*/
				if (iflg == 1)
					cp[0] = '\0';
				else
				{
					if ((dispflag==0) || (status!=0))
						strncpy (cp, dlabel, 24);
					else
						strncpy (cp, deflabel, 24);
					cp[24] = '\0';  
					if (dsub!=0)
					{
						ii = 24;
						while ((cp[ii]==' ')||(ii<0))
							ii--;
						cp[ii+1] = '\0';
					}
					nc = strlen(cp);
					ul_strip_blanks(cp,&nc);
/*
........Set the default toggle value
*/
					for (m=0;m<type_of_ans[k];m++)
					{
						tp = (char *)(&type_of_ans[k+m*6+1]);
						strncpy(ctmp,tp,24); ctmp[24] = '\0';
						nc = strlen(ctmp);
						ul_strip_blanks(ctmp,&nc);
						if (strcmp(ctmp,cp) == 0) *intern_ptr = m;
					}
				}
				S_data_type[j] = UD_DASINT;
				k += type_of_ans[k]*6+1;
			}
			else
/* -1: string only */
			{
				if (((pclas == 0) || ((pclas>0) && geoflag))&&(psel))
				{
/*
.....if multi-select, no value will be set&saved
*/
					S_sel_list[j].color = pclr;
					ans_ptr[item] = (int*)&(S_sel_list[j]);
					S_data_type[j] = UD_DASSTRING;	
				}
				else
				{
					if (dispflag)
					{
						status = ncl_getmac_rvalue(indx, &rvalue, rlab, &rsub, &rtyp);
						if (rtyp==0)
							rtyp = iflg-1;
						if (status==0)
						{
							iflg = rtyp+1;
							if (iflg==3)
							{
								ncl_sprintf(deflabel,&rvalue,1);
							}
							else
							{
								if (rsub==0)
									sprintf(deflabel, "%s", rlab);
								else
									sprintf(deflabel, "%s(%d)", rlab, rsub);
								iflg = 2;
							}
	/*
	......extend the size to 74 (label 6 4chars plus sub, such SRF123(1234))
	*/
							deflabel[74] = '\0';
						}
					}  
					ans_ptr[item] = (int*)&ans[j];
					S_data_type[j] = UD_DASSTRING;	
					tp = (char *)ans_ptr[item];
					if (iflg == 1)
						tp[0] = '\0';
					else if (iflg == 2)
					{
						if ((dispflag==0) || (status!=0))
						{
							strncpy (tp, dlabel, 64);
							tp[64] = '\0'; 
							if (dsub!=0)
							{
								ii = 63;
								while ((tp[ii]==' ')||(ii<0))
									ii--;
								tp[ii+1] = '\0';
								sprintf(tp, "%s(%d)", tp, dsub);
							}
						}
						else
							strcpy (tp, deflabel);
					}
					else
					{
						if ((dispflag==0) || (status!=0))
							ncl_sprintf(tmpstr,&dvalue,1);
						else
							ncl_sprintf(tmpstr,&rvalue,1);
						strncpy(tp, tmpstr, 74);
						tp[74] = '\0';
					}
					cp = (char *)&def[j];
					strcpy(cp,tp);
					nc = strlen(cp);
					ul_strip_blanks(cp,&nc);
					k += 21;
					methods[item] = string_picked;
					macro_substr[item] = substr;
					macro_pflag[item] = pflag;
				}
			}
			called[item] = 6;
			traverse[item] = 1;
			display[item] = 1;
		}
		S_data_num = item;
		form_item = item;
		form_options(frm_ptr,&i);
		
		methods[item] = filter_noop;
		called[item] = 6;
		traverse[item] = 1;
		display[item] = 1;
		display[item+1] = 1;
		ans_ptr[item] = (int *)&outflag;
		
		methods[item+1] = filter_noop;
		called[item+1] = 6;
		traverse[item+1] = 1;
		display[item+2] = 1;
		ans_ptr[item+1] = (int *)&dispflag;

		methods[item+2] = filter_noop;
		called[item+2] = 6;
		traverse[item+2] = 1;
		display[item+3] = 1;
		ans_ptr[item+2] = (int *)&save_def;

		methods[item+3] = view_macro;
		called[item+3] = 6;
		traverse[item+3] = 1;
		display[item+4] = 1;
		ans_ptr[item+3] = (int*)&ans[totprm+3];
/*
.....added video button
*/
		methods[item+4] = OnVideo;
		called[item+4] = 6;
		traverse[item+4] = 1;
		display[item+5] = 1;
		ans_ptr[item+4] = UU_NULL;

		methods[item+5] = OnView;
		called[item+5] = 6;
		traverse[item+5] = 1;
		display[item+6] = 1;
		ans_ptr[item+5] = (int*)&ans[totprm+4];
/*
.....Create the form header
*/
		form_header(frm_ptr,totprm,name);
/*
......set end of form
*/
		strcpy(&frm_ptr[0]+(i),"~END\n");
		if (S_form_design == 0) ncl_set_macptr(savptr);
/*
.....Create the form name
*/
		strcpy(fname,name);
		nc = strlen(name);
		ul_strip_blanks(fname,&nc);
		strcat(fname,".frm");
		ul_to_lower(fname);
/*
.....Display the form
*/
form:;
/*
........Try a form file first
*/
		strcpy(ftmp,fname);
		status = ul_open_mod_file("UU_USER_SETTINGS","forms","NCL_INCDIR",UU_NULL,
			ftmp,2,&fd);
		repeat = 0;
		if (S_form_design==0)
		{
			if (status == UU_SUCCESS)
			{
				ux_fclose0(fd);
				ud_set_formdir("NCL_INCDIR");
				status = ud_form1(fname,ans_ptr,ans_ptr,methods,called,display,
					traverse);
			}
/*
........Form file does not exist
........Create internal form
*/
			else
			{
				status = ud_form1("INTERNAL.INTERNAL",ans_ptr,ans_ptr,methods,called,
					display,traverse);
			}
			if (status==-1)
			{
				status = UU_FAILURE;
				goto done;
			}
		}
		else
		{
			if (status == UU_SUCCESS)
			{
				ux_fclose0(fd);
				ud_set_formdir("NCL_INCDIR");
				repeat = uw_form_desgn(1, fname);
			}
			else
			{
				repeat = uw_form_desgn(1, "INTERNAL.INTERNAL");
			}
			ncl_set_macptr(savptr);
		}
	}
	NCL_macro_outdefault = save_def;
	if (S_form_design==0)
	{
		for(k=0;k<1535;k++) cin[k] = ' ';
		cin[1535] = 0;
		k = 0;
		i = 0;
		if (outflag==1)
			sprintf(&cin[i],"CALL/%s",name);
		else
			sprintf(&cin[i],"*CALL/%s",name);

		i = strlen(cin);
		if (totprm>0)
			ncl_setmac_desp(name, classname, &outflag, &dispflag, descrip);

		for(j=0,k=0; j < totprm; j++)
		{     
			ul_strip_blanks (cin,&i);   
			indx = j+1;
			ncl_getmac_parms(&indx, label, &clas, prmptr, &counter, wrdlist,
				&geoflag, &min, &max, 
				&prsvalue, &lenvalue, &substr, &pflag, &psel, &pclr);
			ncl_getmac_values(&indx, &iflg, &psub, dlabel, &dsub, &dvalue, &pclas, &detype);			
			if(*(type_of_ans+k) == 0)
			{
				if ( (iflg == 1) || ans[j].frmval != dvalue || save_def == 1)
				{
					sprintf(&cin[i],",%s=",&label[0]);
					i = strlen(cin);
					if (lenvalue >0)
					{
						sprintf(fmt,"%%%d.%df", lenvalue, prsvalue);
						sprintf(&cin[i],fmt,ans[j].frmval);
					}
					else
						ncl_sprintf(&cin[i],&ans[j].frmval,1);
				}
				if (dispflag)
				{
					rvalue = ans[j].frmval;
					indx = j+1;
					ncl_setmac_rvalue(indx, rvalue, NULL, 0, 2);	
				}
				k++;
			}
/*
........Checkbox field
*/
			else if (*(type_of_ans+k) == -2)
			{
				if ( (iflg == 1) || ans[j].frmint != dvalue || save_def == 1)
					sprintf(&cin[i],",%s=%d",&label[0],ans[j].frmint);
				if (dispflag)
				{
					rvalue = ans[j].frmint;
					indx = j+1;
					ncl_setmac_rvalue(indx, rvalue, NULL, 0, 2);	
				}
				k++;
			}
			else if(*(type_of_ans+k) > 0)
			{ 
/*
.....WORD still 24 chars
*/
				intern_ptr = (int *)&ans[j];
				strncpy(ctmp, &(wrdlist[*intern_ptr*24]), 24);
				ctmp[24] = '\0';
				nc = strlen(ctmp);
				ul_strip_blanks(ctmp,&nc);
				if ( iflg == 1 || strcmp(ctmp,dlabel) != 0 || save_def == 1) 
				{
					sprintf(&cin[i],",%s=",&label[0]);
					i = strlen(cin);
					for(c=0; c<24; c++)
					{
						cin[i+c] = ctmp[c];
						if (cin[i+c] == '\0') cin[i+c] = ' ';
					}
				}
				k = k+1+(*(type_of_ans+k)*6); 
				if (dispflag)
				{
					rvalue = 0;
					indx = j+1;
					ncl_setmac_rvalue(indx, rvalue, ctmp, 0, 1);	
				}
			}
			else if(*(type_of_ans+k) == -1)
			{
/*
......type of ans - STRING: could be scalar or other geom
*/
				answer = &ans[j];
				if ((S_data_type_f[j]==UD_DASUNITLESS)||(S_data_type_f[j]==UD_DASVAL)
					||(S_data_type_f[j]==UD_DASANGLE))
				{
					sprintf(ctmp, "%f", answer->frmval);
				}
				else if (S_data_type_f[j]==UD_DASINT)
				{
					sprintf(ctmp, "%d", answer->frmint);
				}
				else if (((pclas == 0) || ((pclas>0) && geoflag))&&(psel))
				{
/*
.....if multi-select, no value will be set&saved
*/
					strcpy(macnam, name);
					macnam[64] = '\0';
					ii = 63;
					while ((macnam[ii]==' ')||(ii<0))
						ii--;
					macnam[ii+1] = '\0';
					S_handle_mutl_selstr(macnam, &(S_sel_list[j].select_list), selstr);
				}
				else
				{
					tp = (char *)&ans[j]; 
					strcpy(ctmp,tp);
				}
				if (((pclas == 0) || ((pclas>0) && geoflag))&&(psel))
				{
					sprintf(&cin[i],",%s=",&label[0]);
					i = strlen(cin);
					sprintf(&cin[i],"%s", selstr);
				}
				else
				{
					nc = strlen(ctmp);
					ul_strip_blanks(ctmp,&nc);
					if (nc == 0)
					{
						ud_wrerr("Blank fields are not allowed.  All fields must contain a value.");
						goto form;
					}
/*
.....the ctmp could be a defined scalar value string
*/
					stat = ncl_get_scalar_value(ctmp, &value);
					if (stat==-1)
					{
/*
.....the input is not a scalar value, but a name
*/
						dlabel[64] = '\0';
						if (dsub!=0)
						{
							dlabel[64] = '\0';
							ii = 63;
							while ((dlabel[ii]==' ')||(ii<0))
								ii--;
							dlabel[ii+1] = '\0';
							sprintf(dlabel, "%s(%d)", dlabel, dsub);
						}
						if (iflg == 1 || strcmp(ctmp,dlabel) != 0 || save_def == 1)
						{
							sprintf(&cin[i],",%s=",&label[0]);
							i = strlen(cin);
							sprintf(&cin[i],"%s",ctmp);
						}
						if (dispflag)
						{
/*
.....when save as real value for string, it have 2 part, first 6 chars for label
.....and last 2 chars for a short value sub number
*/
							ncl_getlabel_sub(ctmp, rlab, &rsub);
							indx = j+1;
							ncl_setmac_rvalue(indx, rvalue, rlab, rsub, 1);	
						}
					}
					else
					{
/*
......a scalar/number
*/
						if (iflg == 1 || value != dvalue || save_def == 1)
						{
							sprintf(&cin[i],",%s=",&label[0]);
							i = strlen(cin);
							sprintf(&cin[i],"%s",ctmp);
						}
						if (dispflag)
						{
							rvalue = value;
							indx = j+1;
							ncl_setmac_rvalue(indx, rvalue, NULL, 0, 2);	
						}
					}
				}
				k = k + 21;	
			}
			i = strlen(cin);
		}
		ul_strip_blanks (cin,&i);
		for(k=i;k<512;k++) cin[k] = ' ';
		ncl_init_cmdbuf(&cmdbuf);
		for(prev_comma_ptr = cin;;)
		{
			comma_ptr = index(prev_comma_ptr,',');
			if(!comma_ptr) break;
			strncpy(token,prev_comma_ptr,(int)comma_ptr - (int)prev_comma_ptr + 1);
			token[(int)comma_ptr - (int)prev_comma_ptr + 1] = '\0';
			ncl_add_token(&cmdbuf,token,NCL_nocomma);
			prev_comma_ptr = comma_ptr+1;
		}
		strncpy(token,prev_comma_ptr,(int)cin + i - (int)prev_comma_ptr);
		token[(int)cin + i - (int)prev_comma_ptr] = '\0';
		ncl_add_token(&cmdbuf,token,NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		NCL_macro_remval = dispflag;
/*
.....Modal Macro Call
.....Redisplay form
.......Added option to forget previous answers - ASF 12/19/13.
*/
		if (modal == UU_TRUE && totprm > 0) 
		{
/*
.....NEED reset the macro pointer to the original name
.....because "ncl_call(&cmdbuf);" will term the current macro
.....but we still inside form and need that
*/
			ncl_set_curmac (name);
/*
.....Remember previous entries.
*/
			if (dispflag) goto form;
/*
.....Reset form entries.
*/
			else
			{
				times++;
				uu_lsdel(frm_ptr);
				goto modal_repeat;
			}
		}
	}
/*
.....Modal Macro Call
.....Redisplay form
*/
/*	if (modal == UU_TRUE)
	{
		goto form;
	}
*/	status = UU_SUCCESS;
/*
.....Free up memory
*/
done:;
/*
.....delete temp data
*/
	for (i=0; i<S_tmp_dat_n;i++)
	{
		getkey(S_tmp_dat[i], &nclkey);
		dtdele (&nclkey);
	}
	if (totprm > 0) uu_lsdel(frm_ptr);
	if (type_of_ans != UU_NULL) uu_free(type_of_ans);
	if (ans != UU_NULL) uu_free(ans);
	if (ans_ptr != UU_NULL) uu_free(ans_ptr);
	if (methods != UU_NULL) uu_free(methods);
	if (called != UU_NULL) uu_free(called);
	if (traverse != UU_NULL) uu_free(traverse);
	if (display != UU_NULL) uu_free(display);
	um_close_pocket_window(UM_DRAWING_WINDOW);
	um_close_pocket_window(UM_GRAPHIC_WINDOW);
	UD_initfrm_intry = save_entry;
	UD_UNMARK (cmdreject);
	if (S_form_design==0)
		return(status);
	else
		return repeat;
}

/*********************************************************************
**    E_FUNCTION :  form_elmt(stat,i,type_of_ans,frm_ptr,clas,wrdlist,
**                            counter,geomflag,substr,prmptr,prsvalue,lenvalue, 
**                            min,max,iflg, psel)
**      Create an entry in a form.
**    PARAMETERS
**       INPUT
**         i            - Index into output form data.
**         stat         - Number of form fields on first call,
**                        0 for subsequent calls.
**         clas         - prompt value type
**         wrdlist      - wrdlist or geom-list
**         counter      - number of items in 'wrdlist'
**         geomflag     - 1 = geom-list is specified.
**         substr       - 0 = FORM_STRING/FORM_PICK, 1 = FORM_LABEL,
**                        2 = FORM_SUBSCR
**         prmstr       - prompt string
**         prsvalue
**         lenvalue 
**         min,max:     - max/min of input data
**         iflg         - =1 no default.
**                        =2 default value is name.
**                        =3 default value is scalar value.
**			psel:		value to see if need multi-selection
**       OUTPUT:
**         frm_ptr      - Form data.
**         type_of_ans  - -1, String field
**0, Real field
**>0, Toggle field.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void form_elmt(stat, i, type_of_ans,frm_ptr, clas, wrdlist, counter,
		geomflag, substr, prmptr, prsvalue, lenvalue, min, max, iflg, psel)
char *frm_ptr, *wrdlist, *prmptr;
int *type_of_ans;
UM_int2 clas, geomflag, prsvalue, lenvalue;
UM_int2 iflg;
UM_real8 min, max;
int *i, stat,counter,substr, psel;
{
	int j,jfk,len,sizex, cx,maxlen;
	char *tmpptr;
	static int k = 0;  
	static int item = 0;
	if(stat != 0)
	{
		k = 0;
		item = 0;
	}
/*
...String only
*/
/*   if (((clas == 0) && (iflg!=3)) || ((clas>0) && geomflag)) */
	if ((clas == 0) || ((clas>0) && geomflag)) 
	{
		*(type_of_ans+(k++)) = -1;    /* type of ans - STRING */
		k = k + 20;     
		if( x != 10)
		{
			item = 0;
			x = 10;
			y = y + 17;
		}
	  
		if (psel)
			sprintf(&frm_ptr[0]+(*i),"#SELECT#\n");
		else
			sprintf(&frm_ptr[0]+(*i),"#EDIT#\n");

		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		sprintf(&frm_ptr[0]+(*i),"/LABEL/ %s\n",prmptr);		
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		len = strlen(prmptr);

		sprintf(&frm_ptr[0]+ (*i) ,"/POSITION/%d,%d\n",x,y);
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));

		if (psel)
			cx = (len + 10)*X_RATIO;
		else
			cx = (len + 4)*X_RATIO + 20*X_RATIO;

		if (x + cx + PROMPT_SPACING> x_max)
			x_max = x + cx+ PROMPT_SPACING; 
	  
		sprintf(&frm_ptr[0]+(*i),"/SIZE/ %d,%d\n", cx, 14);
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		sprintf(&frm_ptr[0]+(*i),"/TYPE/ UD_DASSTRING\n");
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		len = STRING_SIZE;
		sprintf(&frm_ptr[0]+(*i),"/PREC/ %d\n", len);
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		sprintf(&frm_ptr[0]+(*i),"/LEN/ %d\n", len);
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		if (((clas>0) && geomflag)&&(psel))
			sprintf(&frm_ptr[0]+(*i),"/INPUT/ FORM_PICK\n");
		else if (substr == 1)
			sprintf(&frm_ptr[0]+(*i),"/INPUT/ FORM_LABEL\n");
		else if (substr == 2)
			sprintf(&frm_ptr[0]+(*i),"/INPUT/ FORM_SUBSCR\n");
		else
			sprintf(&frm_ptr[0]+(*i),"/INPUT/ FORM_PICK\n");
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		if (geomflag)
		{
			limit_pick(wrdlist,counter,&frm_ptr[0]+(*i));
			(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		}
/*
...Prepare the coordinates for the next prompt
*/
		x = 10;
		y = y + 17;
	}
/* 
...String  & min & max or String with default scalar value
*/ 
	if (clas == -1)
	{
		len = strlen(prmptr);
		if(len > MAX_PROMPT_SIZE && x != 10)
		{
			item = 0;
			x = 10;
			y = y + 17;
		}
		else
			item++;
		if (min == 0 && max == 0)
		{
			*(type_of_ans+(k++)) = -2;           /* type of ans - LOGICAL */
			sprintf(&frm_ptr[0]+(*i),"#CHECKBOX#\n");
			(*i) = (*i) + strlen(&frm_ptr[0]+(*i));	
			sizex = (len + 2 + prsvalue + 2)*X_RATIO;
		}
		else
		{
			*(type_of_ans+(k++)) = 0;               /* type of ans - REAL */
			sprintf(&frm_ptr[0]+(*i),"#EDIT#\n");
			(*i) = (*i) + strlen(&frm_ptr[0]+(*i));	
			sprintf(&frm_ptr[0]+(*i),"/TYPE/ UD_DASUNITLESS\n");
			(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
			sprintf(&frm_ptr[0]+(*i),"/PREC/ %d\n", prsvalue);
			(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
			sprintf(&frm_ptr[0]+(*i),"/LEN/ %d\n", lenvalue);
			(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
			sprintf(&frm_ptr[0]+(*i),"/INPUT/ FORM_STRING\n");
			(*i) = (*i) + strlen(&frm_ptr[0]+(*i));	
			sprintf(&frm_ptr[0]+(*i),"/RANGE/ %f,%f\n", min,max);
			(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
			sizex = 120;
		}
		sprintf(&frm_ptr[0]+(*i),"/LABEL/ %s\n",prmptr);
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		len = strlen(prmptr);
		sprintf(&frm_ptr[0]+ (*i) ,"/POSITION/%d,%d\n",x,y);
		(*i) = (*i)  + strlen(&frm_ptr[0]+(*i));
		len = strlen(prmptr);
/*		sizex = (len + 2 + lenvalue + prsvalue + 2)*X_RATIO;*/
		if (x == 10) x = SECOND_COLUMN;
		else x += sizex + PROMPT_SPACING;
		sprintf(&frm_ptr[0]+(*i),"/SIZE/ %d,%d\n", sizex, 14);
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
/*
...Prepare the coordinates for the next prompt
*/ 
		len = strlen(prmptr);
		if ((len+4+lenvalue+prsvalue>=MAX_PROMPT_SIZE)||(item==2))
		{  
			if (x + PROMPT_SPACING> x_max)
				x_max = x + PROMPT_SPACING; 
			item = 0;
			x = 10;  
			y = y + 17; 
		}
		if (x + PROMPT_SPACING> x_max)
			x_max = x + PROMPT_SPACING; 
	}
/* 
...String  and vocabulary words
*/  
	if ((clas > 0) && (geomflag==0))
	{ 
/*
.........Get maximum word length
*/
		maxlen = 0;
		for(j=0;j < clas; j++)
		{
			for(len = 0; wrdlist[len+j*24] > ' ' && len < 24; len++);
			if (len > maxlen) maxlen = len;
		}
		if (maxlen > 10) maxlen = 25;
		else maxlen = 11;

		len = strlen(prmptr);
		if(len+maxlen> MAX_PROMPT_SIZE && x != 10)
		{
			item = 0;
			x = 10;
			y = y + 17;
		}
		else
			item++;
		sprintf(&frm_ptr[0]+(*i),"#CHOICEBOX#\n");
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		sprintf(&frm_ptr[0]+(*i),"/LABEL/ %s\n",prmptr);
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		len = strlen(prmptr);
		sprintf(&frm_ptr[0]+ (*i) ,"/POSITION/%d,%d\n",x,y);
		(*i) = (*i)  + strlen(&frm_ptr[0]+(*i));
		len = strlen(prmptr);
		sizex = (len + maxlen)*(X_RATIO+1);
		if (x == 10) x = SECOND_COLUMN;
		else x += sizex + PROMPT_SPACING;
		sprintf(&frm_ptr[0]+(*i),"/SIZE/ %d,%d\n", sizex, 60);
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		sprintf(&frm_ptr[0]+(*i),"/TYPE/ UD_DASSTRING\n");
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		sprintf(&frm_ptr[0]+(*i),"/CHOICES/");
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		*(type_of_ans+(k++)) = clas;
		for(j=0;j < clas; j++)
		{
			sprintf(&frm_ptr[0]+(*i),"\"");
			(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
			for(len = 0; wrdlist[len+j*24] > ' ' && len < 24; len++)
				*(frm_ptr+(*i)+len) = wrdlist[len+j*24];
			*(frm_ptr+(*i)+len) = 0;
			(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
			if (j==clas-1)
				sprintf(&frm_ptr[0]+(*i),"\"\n");
			else
				sprintf(&frm_ptr[0]+(*i),"\", ");
			(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
			
			tmpptr =(char *)(type_of_ans+k);
			for(jfk = 0; jfk <24; jfk++)
				tmpptr[jfk] = wrdlist[j*24+jfk];
			tmpptr[24] = '\0';
			k = k + 6;
		}
		sprintf(&frm_ptr[0]+(*i),"/PREC/ 9\n");
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
		sprintf(&frm_ptr[0]+(*i),"/LEN/ 9\n");
		(*i) = (*i) + strlen(&frm_ptr[0]+(*i));
/*
...Prepare the coordinates for the next prompt
*/ 
		if (y > y_max) y_max = y;
		len = strlen(prmptr);
		if ((len+maxlen>MAX_PROMPT_SIZE)||(item==2))
		{
			if (x + PROMPT_SPACING> x_max)
				x_max = x + PROMPT_SPACING; 
			item = 0;
			x = 10;  
			y = y + 17;
		}
		if (x + PROMPT_SPACING> x_max)
			x_max = x + PROMPT_SPACING; 
	}
}

/********************************************************************* 
**    E_FUNCTION :  form_header(frm_ptr,totprm,name)
**    PARAMETERS 
** 
**       OUTPUT    : none 
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none 
*********************************************************************/
static void form_header(frm_ptr,totprm,name)
char *frm_ptr,*name;
int totprm;
{
/*
......form header extend to 200 chars because 'desp' max=40 and 'name' max=75
*/
	char tmp[FORM_MAX_INT_HEADER], desp[41];
	int i,len_form,len_hdr,k;
	char classname[21];
	int outflag, dispflag;

	desp[0] = '\0';
	ncl_getmac_parms0(classname, &outflag, &dispflag, desp);
/*
.....Define Header section
*/
	sprintf(tmp,"#HEADER#\n");
	i = strlen(tmp);
	if (strlen(desp) > 0)
		sprintf(&tmp[i],"/TITLE/ %s\n",desp);
	else
		sprintf(&tmp[i],"/TITLE/ Dynamic Call for Macro %s\n",name);
	i = strlen(&tmp[0]);
	sprintf(&tmp[i],"/POSITION/ 0,0\n");
	i = strlen(&tmp[0]);
	if (x_max<230)
		x_max = 230;
	sprintf(&tmp[i],"/SIZE/ %d,%d\n", x_max+20, y_max+17+45);
	strcpy (&frm_ptr[0],&tmp[0]);
/*
......Initialize rest of form structure
*/
	len_form = strlen(&frm_ptr[FORM_MAX_INT_HEADER]);
	len_hdr = strlen(tmp);

	for(k = FORM_MAX_INT_HEADER; k < FORM_MAX_INT_HEADER+len_form; k++)
	{
		frm_ptr[k-(FORM_MAX_INT_HEADER-len_hdr)] = frm_ptr[k];
	}
	for(k = len_form+len_hdr; k < FORM_MAX_INT_HEADER+len_form; k++)
	{
		frm_ptr[k] = '\n';            
	}
}

/********************************************************************* 
**    E_FUNCTION :  form_options(frm_ptr,i)
**    PARAMETERS 
** 
**       INPUT:
**         frm_ptr      - Form data.
**         i            - Index into output form data.
**       OUTPUT:
**         frm_ptr      - Form data.
**         i            - Index into output form data.
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none 
*********************************************************************/
static void form_options(frm_ptr,i)
char *frm_ptr;
int *i;
{
	char tmp[256];
	int nc;
	char vfile[256];
/*
.....Added "MACRO" flag
*/
	sprintf(tmp,"#MACRO#\n");
	nc = strlen(tmp);
/*
.....Add "Place in Source" field
*/
	sprintf(&tmp[nc],"#CHECKBOX#\n");
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/LABEL/ Place in Source\n");
	nc = strlen(tmp);
	sprintf(&tmp[nc],"/POSITION/%d,%d\n",10,y_max+17);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/SIZE/ %d,%d\n", 120, 14);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/TYPE/ UD_DASSTRING\n");
	nc = strlen(tmp);
/*
.....Add "Remeber Input values" field
*/
	sprintf(&tmp[nc],"#CHECKBOX#\n");
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/LABEL/ Remember Input Values\n");
	nc = strlen(tmp);
	sprintf(&tmp[nc],"/POSITION/%d,%d\n",130,y_max+17);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/SIZE/ %d,%d\n", 120, 14);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/TYPE/ UD_DASSTRING\n");
	nc = strlen(tmp);
	y_max += 17;
	strcpy(&frm_ptr[0]+(*i),tmp);
	*i = *i + nc;
/*
.....Add "Output Defaults" field
*/
	sprintf(tmp,"#CHECKBOX#\n");
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/LABEL/ Output Default Values\n");
	nc = strlen(tmp);
	sprintf(&tmp[nc],"/POSITION/%d,%d\n",10,y_max+17);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/SIZE/ %d,%d\n", 120, 14);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/TYPE/ UD_DASSTRING\n");
/*
.....Add "View" field
*/
	sprintf(&tmp[nc],"#PUSHBUTTON#\n");
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/LABEL/ View\n");
	nc = strlen(tmp);
	sprintf(&tmp[nc],"/POSITION/%d,%d\n",130,y_max+17);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/SIZE/ %d,%d\n", 50, 14);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/TYPE/ UD_DASSTRING\n");
	nc = strlen(tmp);
/*
.....Add "Video" button
*/
	sprintf(&tmp[nc],"#IMGBUTTON#\n");
	nc = strlen(tmp);
//should have a fixed image file 
	sprintf(&tmp[nc], "/LABEL/ video.bmp\n");
	ncl_get_curmac (vfile, &nc);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/FILE/ %s.mp4\n", vfile);
	nc = strlen(tmp);
	sprintf(&tmp[nc],"/POSITION/%d,%d\n",130+50+10,y_max+17);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/SIZE/ %d,%d\n", 50, 14);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/TYPE/ UD_DASSTRING\n");
	nc = strlen(tmp);

	y_max += 17;
	strcpy(&frm_ptr[0]+(*i),tmp);
	*i = *i + nc;
/*
.....Add "Dynamic View" field
*/
	sprintf(tmp,"#PUSHBUTTON#\n");
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/LABEL/ Dynamic View\n");
	nc = strlen(tmp);
	sprintf(&tmp[nc],"/POSITION/%d,%d\n",10,y_max+17);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/SIZE/ %d,%d\n", 80, 14);
	nc = strlen(tmp);
	sprintf(&tmp[nc], "/TYPE/ UD_DASSTRING\n");
	nc = strlen(tmp);
/*
.....Add option fields to form
*/
	strcpy(&frm_ptr[0]+(*i),tmp);
	*i = *i + nc;


	return;
}

/********************************************************************* 
**    E_FUNCTION :  ncl_calform_initsize(totprm)
**		Calculate the initial size of the parameter form 
**    PARAMETERS 
** 
**       INPUT:
**         frm_ptr      - Form data.
**         i            - Index into output form data.
**       OUTPUT:
**         frm_ptr      - Form data.
**         i            - Index into output form data.
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none 
*********************************************************************/
static void ncl_calform_initsize(totprm)
int totprm;
{
	int i,item,k, len, psel, pclr;
	UM_int2 iflg, detype, pclas;
	char label[NCL_MAX_LABEL+1],prmptr[41],wrdlist[24*WORD_LIMIT+1],dlabel[NCL_MAX_LABEL+15];
	UM_real8 min, max;
	UM_int2 clas, prsvalue, lenvalue, geoflag;
	int counter, substr, pflag;
	UM_int4 dsub, psub;
	UM_real8 dvalue;
		
	x_max = 0;		
	y_max = 0;
/*
...   Count the number of lines in the form. String answer or
...   prompt > MAX_PROMPT_SIZE characters are one per line, rest are 2 per line.
*/
	y = 8;
	x = 10;		
	len = 0;
      
	for(item=0, k=1,i=FORM_MAX_INT_HEADER; k<totprm+1;k++)
	{
		ncl_getmac_parms(&k, label, &clas, prmptr, &counter, wrdlist, &geoflag,
			&min, &max, 
			&prsvalue, &lenvalue, &substr, &pflag, &psel, &pclr);
		ncl_getmac_values(&k, &iflg, &psub, dlabel, &dsub, &dvalue, &pclas, &detype);			
		if (pclas!=0)
		{
			len = strlen(prmptr);
		}
		if ((pclas == 0) || ((pclas>0) && geoflag)) 
		{
			if( x != 10)
			{
				item = 0;
				y = y + 17;
			}
			x = 10;
			y = y + 17;
/*
.....snce we added y, start from beginning net line, so reset item
*/
			item = 0;
		}
		if (pclas == -1)
		{
			if(len > MAX_PROMPT_SIZE && x != 10)
			{
				item = 0;
				x = 10;
				y = y + 17;
			}
			else
			{
				item++;
/*
.....x is not the inition X anymore
*/
				x = x + 10;
			}
			if ((len+4+lenvalue+prsvalue>=MAX_PROMPT_SIZE)||(item==2))
			{  
				if (k!=totprm)
					item = 0;
				x = 10;  
				y = y + 17; 
/*
.....snce we added y, start from beginning net line, so reset item
*/
				item = 0;
			}
		}
		if ((pclas > 0) && (geoflag==0))
		{ 
/*
			if( x != 10)
			{
				item = 0;
				y = y + 17;
			}
			x = 10;
			y = y + 17;
*/
			len = strlen(prmptr);
			if(len+11> MAX_PROMPT_SIZE && x != 10)
			{
				item = 0;
				x = 10;
				y = y + 17;
			}
			else
			{
				item++;
/*
.....x is not the inition X anymore
*/
				x = x + 10;
			}
			if ((len+11>MAX_PROMPT_SIZE)||(item==2))
			{
				if (k!=totprm)
					item = 0;
				x = 10;  
				y = y + 17;
/*
.....snce we added y, start from beginning net line, so reset item
*/
				item = 0;
			}
		}
	}
	x = 10;		
	if (item==0)		
		y_max = y - 17;
	else
		y_max = y;

	y = 8;
}

/*********************************************************************
**    E_FUNCTION     : S_save_macro_mf()
**       Save the NCL macro settings into modals file.
**       
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_macro_mf()
{
	int stat;
	UX_pathname fname;
	FILE *fptr;
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy (fname, "ncl_macro.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) goto done;
/*
.....Store playback modals
*/
	ux_fputs0("#MACROS#\n", fptr);

	if (NCL_macro_modal==1)
		ux_fputs0("/MODAL/ *YES\n", fptr);
	else
		ux_fputs0("/MODAL/ *NO\n", fptr);
/*
	if (NCL_macro_outflag==1)
		ux_fputs0("/SOURCE/ *YES\n", fptr);
	else
		ux_fputs0("/SOURCE/ *NO\n", fptr);
*/
	if (NCL_macro_outdefault==1)
		ux_fputs0("/DEFAULT/ *YES\n", fptr);
	else
		ux_fputs0("/DEFAULT/ *NO\n", fptr);
	if (NCL_macro_remval==1)
		ux_fputs0("/VALUES/ *YES\n", fptr);
	else
		ux_fputs0("/VALUES/ *NO\n", fptr);
/*
.....Close modals file
*/
	ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
	return(stat);
}
/*********************************************************************
**    S_FUNCTION     :  static OnListCalbacks2(fieldno, val, stat)
**       Method called for macro list table
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
static UD_FSTAT OnListCalbacks2(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int sel, toolno;
	UD_ITEMDATA *data;
	UD_TABLEINFO *info = (UD_TABLEINFO *)(val->frmint);
	if (info->flag==1)
	{
/*
......list selected, doing selection callback
......if it is un-select, info->row = -1;
*/
		macro_namelist.answer = info->row;
		if (info->row>=0)
		{
			data = (UD_ITEMDATA *)(info->data_ptr);
			strcpy(macro_name, data->data_items[0]);
		}
		else
			macro_name[0] = '\0';
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
		if ((info->col==0)&&(name_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			name_click++;
		}
		else if ((info->col==0)&&(name_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			name_click++;
		}
		if ((info->col==1)&&(descript_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			descript_click++;
		}
		else if ((info->col==1)&&(descript_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			descript_click++;
		}
		if ((info->col==2)&&(class_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			class_click++;
		}
		else if ((info->col==2)&&(class_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			class_click++;
		}
	}
	return(UD_FLDOK);
}
static void S_load_macro_form(name)
char *name;
{
}

/*********************************************************************
**    E_FUNCTION :  ncl_macro_formdesgn(parms)
**    PARAMETERS
**
**    This routine is the main routine of the "Define Macro Form".
**
**		 INPUT:  parms: include macro name
**							 
**       OUTPUT    : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_macro_formdesgn(parms)
char *parms;
{
	int i,j,n,status,class,outflag, dispflag,ret;
	char *ptr1,*tptr,*tptrf;
	UD_METHOD save_entry;
	UU_LOGICAL cmdreject;
	int fieldno, clsnum;
	UD_DDATA val;
	UD_FSTAT stat;
/*
.....Macro description variables
*/
	char classname[21], prmpt[41];
	char temp[1024],mname[NCL_MAX_LABEL+1];
	UM_f77_str f77_tmp,f77_tmp1;
	static int first=1;
/*
.....Form fields
*/
	static UD_METHOD methods[] = {filter_macro, filter_macro, filter_macro, OnListCalbacks2,
		filter_noop, load_macro, view_macro,  UU_NULL};
	static char called[] = {6,6,6,6,6,6,6,6};
	static char traverse[] = {1,1,1,1,1,1,1,1};
	static char display[] = {1,1,1,1,0,1,0,0};
	int *ans[8];
	int button, mode;
	char video_file[UX_MAX_PATH_LEN];
	S_form_design = 1;
/*
.....Set up filter
*/
	if (first == 1)
	{
		strcpy(filter,"*");
		strcpy(cur_class, "ALL");
		first = 0;
	}
start_again:;
/*
.....Initialize routine
*/
	ptr1_first = UU_NULL;
	tptrf = UU_NULL;
	status = UU_SUCCESS;
	macro_name[0] = '\0';
/*
.....Disable command reject
*/
	UD_MARK (cmdreject, UU_FALSE);
	if (cmdreject != 0)
	{
		um_close_pocket_window(UM_DRAWING_WINDOW);
		um_close_pocket_window(UM_GRAPHIC_WINDOW);
		status = UU_FAILURE;
		goto done;
	}
/*
.....Create the list of the currently defined macros.
*/
	tptr = (char *)uu_lsnew();
	tptrf = tptr;
	ptr1 = (char *)uu_lsnew();
	ptr1_first = ptr1;
/*
.....Could not allocate memory for the list
*/
	if (ptr1 == 0 || tptr == 0)
	{
		ud_wrerr("Could not allocate enough memory for internal usage.");
		status = UU_FAILURE;
		goto done;
	}
/*
.....Get list of the all currently defined macros.
*/
repeat:;
	class  = 11;
	status = ncl_entnam_list (class, tptr, &fitem);
/*
.....Append Macro descriptions to list
*/
	UM_init_f77_str(f77_tmp,mname, NCL_MAX_LABEL+1);
	UM_init_f77_str(f77_tmp1,temp,1024);
	ptr1 = ptr1_first;
	mclass_list.item = (char **) uu_malloc((fitem+1) *sizeof(char *));
	clsnum = 0;
	mclass_list.item[0] = (char *) uu_malloc(21*sizeof(char));
	strcpy(mclass_list.item[0], "ALL");
	clsnum++;
	for (i=0;i<fitem;i++)
	{
		tptr = (char *)uu_lsnext(tptr);
/*
........Create new list item to hold prompt
........Original list allocated only enough memory
........for the Macro label
......This list should includ macro name and macro description (40 + 8 + spaces)
......Yurong
*/
/*
......the macro name now can be NCL_MAX_LABEL=64 chars, we need (40+64+spaces)
*/
		ptr1 = (char *)uu_lsinsrt(ptr1, NCL_MAX_LABEL+50);
		strcpy(ptr1,tptr);
		ncl_getmac_desp(ptr1, classname, &outflag, &dispflag, prmpt);

		for (n=strlen(ptr1); n<NCL_MAX_LABEL+1; n++)
			ptr1[n] = ' ';
		ptr1[n] = '\0';
		strcat (ptr1, " ");
		strcat (ptr1, prmpt);
/*
.....store macro class into mclass_list
*/
		for (j=0; j<clsnum; j++)
		{
			if (strcmp(classname, mclass_list.item[j])==0)
				break;
		}
		if (j>=clsnum)
		{
			if (stricmp(classname, "none")!=0)
			{
				mclass_list.item[clsnum] = (char *) uu_malloc(21 * sizeof(char));
				strcpy(mclass_list.item[clsnum], classname);
				clsnum++;
			}
		}		
	}
	mclass_list.num_item = clsnum;
	mclass_list.answer = (char *) uu_malloc(21 * sizeof(char));
	strcpy(mclass_list.answer, cur_class);
	ud_list_sort(&mclass_list);
/*
......initial macro_namelist data structure
*/
/*
......Name	Description
*/
	macro_namelist.num_col = 3; 
	macro_namelist.num_item = fitem;
	macro_namelist.answer = -1;

	if (macro_namelist.num_col>0)
		macro_namelist.col_label = (char**) uu_malloc(macro_namelist.num_col*sizeof (char*));
	for (i=0; i<macro_namelist.num_col;i++)
	{
		macro_namelist.col_label[i] = (char*) uu_malloc(20*sizeof(char));
	}
	strcpy(macro_namelist.col_label[0], "Name");
	strcpy(macro_namelist.col_label[1], "Description");
	strcpy(macro_namelist.col_label[2], "Macro Class");
/*
.....filter put macro list
*/
	fieldno = -1;
	filter_macro(&fieldno,&val,stat);
	macro_namelist.sort = saved_info.col;
/*
.....Get form input
*/
	mode = 0;
	ans[0] = (int *)&button;
	ans[1] = (int *)filter;
	ans[2] = (int *) &mclass_list;
	ans[3] = (int *)&macro_namelist;
	ans[4] = &mode;
	ans[5] = UU_NULL; 
	ans[6] = UU_NULL;
	video_file[0] = '\0';
	ans[7] = (int *)video_file;

	newload = 0;
	maclist = UU_TRUE;
	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = OnInitForm;
	status = ud_form1("maclist.frm",ans,ans,methods,called,display,traverse);
	if (status==-1)
	{
		um_close_pocket_window(UM_DRAWING_WINDOW);
		um_close_pocket_window(UM_GRAPHIC_WINDOW);
		status = UU_FAILURE;
		goto done;
	}
	strcpy(cur_class, mclass_list.answer);
/*
.....The user loaded an external macro file
.....Redisplay the form
*/
	if (newload == 1)
	{
		ud_free_tlist(&macro_namelist);
		ud_free_flist(&mclass_list);
		goto repeat;
	}
/*
.....Free any allocated memory
*/
done:;
	UD_initfrm_intry = save_entry;
	if (ptr1_first != UU_NULL) uu_lsdel(ptr1_first);
	if (tptrf != UU_NULL) uu_lsdel(tptrf);
	ud_free_tlist(&macro_namelist);
	ud_free_flist(&mclass_list);
	ptr1_first = UU_NULL; tptrf = UU_NULL; 
	UD_UNMARK (cmdreject);
/*
.....End of routine
*/
	maclist = UU_FALSE;
	ret = 0;
	if (status!=-1)
		ret = nclu_macro_form(macro_name, mode);
	if (ret==1)
		goto start_again;
	S_form_design = 0;
	return(status);
}

