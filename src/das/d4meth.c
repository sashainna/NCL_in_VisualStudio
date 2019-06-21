/*********************************************************************
**    NAME         :  d4meth.c
**       CONTAINS:
**				UD_FSTAT ud_default_method(fno, val, stat)
**							ud_set_display_mask(type, fieldno, val)
**							ud_set_traverse_mask(fieldno, val)
**							ud_update_answer(fieldno)	 (Not implemented)
**							ud_set_list(fieldno, list)
**							ud_mfclose_form()
**							ud_get_field(fieldno, data)
**							ud_update_prompt()
**							ud_formstr_format()
**							ud_get_pickstr()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d4meth.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 17:27:54
*********************************************************************/
#include <string.h>
#include "usysdef.h"

#include "udebug.h"
#include "dinput.h"
#include "ddef.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "dtypes.h"
#include "udfmracs.h"
#include "driver.h"
#include	"gtbl.h"
#include "gdidd.h"
#include "nccs.h"
#include "dselect.h"
#include "vsegbf.h"

#define DIAMOND_MARK 1
#define NO_MARK 2
#define DYNAMIC_MARK 3

extern int **ans1;
extern int *dfltno;

extern int NCL_nopick_cnt;

extern int UD_form_bypick;
extern int NCL_mark_method;
extern int UD_select_key;

void ud_set_list();
extern int UR_active;

/*********************************************************************
**    E_FUNCTION :ud_default_method(fno, val, stat) - Default method.  
**							used ifthere are no user supplied methods.
**    PARAMETERS   
**       INPUT  :  
**				int *fno; -- Pointer to field number
**				UD_DDATA *val;	- Pointer to union containing users input
**				UD_FSTAT stat; - Action that caused method to be called.  Note
**							that this is not the same as values passed in method_ret
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UD_FSTAT ud_default_method(fno, val, stat)
int *fno;
UD_DDATA *val;	
UD_FSTAT stat;
{
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION : ud_set_display_mask - Set display mask for a single
**															form field
**    PARAMETERS   
**       INPUT  :  type		:	UD_DISPLAYF or UD_INPUTF
**						 fieldno	:	Field number of type 'type'
**						 val		:	Value to set to: 1 = On, 0 = OFF
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_set_display_mask(type, fieldno, val)
int type, fieldno, val;
{
	ud_setfrm_display_mask(0, type, fieldno, val);
	return;
}
/*********************************************************************
**    E_FUNCTION : ud_get_traverse_mask - Get traverse mask for a form field
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number of type 'input'
**       OUTPUT :  none
**    RETURNS:      
**						 1 = traverse
**						 0 = Dont traverse
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ud_get_traverse_mask(fieldno)
int fieldno;					
{
	return ud_getfrm_traverse_mask(0, fieldno);
}

/*********************************************************************
**    E_FUNCTION : ud_set_traverse_mask - Set traverse mask for a form field
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number of type 'input'
**						 val		:	Value to set to: 1 = traverse
**															  0 = Dont traverse
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_set_traverse_mask(fieldno, val)
int fieldno;					/* Input field number	*/
int val;	
{
	ud_setfrm_traverse_mask(0, fieldno, val);
	return;
}

/*********************************************************************
**    E_FUNCTION : ud_update_answer 
**				update a field with a value
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_update_answer(fieldno, val)
int fieldno;
int *val;
{
	ud_dispfrm_update_answer(0, fieldno, val);
	return;
}  

void ud_update_frmpic(fieldno, val)
int fieldno;
int *val;
{
	ud_dispfrm_update_frmpic(0, fieldno, val);
	return;
}

/*********************************************************************
**    E_FUNCTION : ud_set_list(fieldno, list)
**			reset field fieldno use input new list
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**						 list : UD_LIST structure 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_set_list(fieldno, form_list)
int fieldno;
UD_LIST *form_list;
{
	ud_dispfrm_set_list(0, fieldno, form_list);
	return;
}
/*********************************************************************
**    E_FUNCTION : ud_get_field(fieldno, data, str_flag)
**			reset field fieldno use input UD_DDATA
**    PARAMETERS   
**       INPUT  :
**          fieldno   : Field number
**          data      : UD_DDATA structure 
**          str_flag  : UU_TRUE if a character string is to be returned
**                      with each field type.
**       OUTPUT :  data : UD_DDATA structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....Need malloc space for UD_DDATA data
.....Before get in this function
.....Yurong added 10/2/97
*/
void ud_get_field(fieldno, data, str_flag)
int fieldno;
UD_DDATA data;
{
	ud_getfrm_field(0, fieldno, data, str_flag);
	return;
}
/*********************************************************************
**    E_FUNCTION : ud_mfclose_form()
**			Close current form and accept data
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : status: 1: close success
**									  0: close unsuccess
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....Yurong added
.....10/2/97
*/
ud_mfclose_form(flag)
int flag;
{
	int status;
	status = (*(ug_gksstli.wsopen[0].connid)[UW_CLOSE_FORM])(flag);
	return status;
}
/*********************************************************************
**    E_FUNCTION : ud_update_prompt 
**				update a field prompt with a value
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**					 :	 prompt:    Prompt string
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added by Yurong
.....5/3/00
*/
void ud_update_prompt(fieldno, prompt)
int fieldno;
char *prompt;
{
	ud_dispfrm_update_prompt(0, fieldno, prompt);
	return;
}


/**********************************************************************
**    I_FUNCTION :  ud_formstr_format(typ,data,prec,len,buf)
**       it is original function  uw_mfform_format 
**			but acturally, this function have nothing to do
**			with ws function, it can used both by WNT and UNIX
**			Formats a form field for display depending on data type.
**    PARAMETERS   
**       INPUT  : 
**          typ     = Data type for form field.  One of UD_DASCART,
**			             UD_DASVAL, UD_DASUNITLESS, UD_DASDISTANCE,
**				          UD_DASINT, UD_DASVEC, UD_DASSTRING, UD_DASNDC,
**				          UD_DASANGLE
**				data    = Input data to format.
**				prec    = Number of digits to right of decimal for real
**				          numbers.
**				len     = Size of field.
**       OUTPUT :  
**          buf     = Formatted value.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_formstr_format(typ,data,prec,len,buf)
int typ,prec,len;
UD_DDATA data;
char *buf;
{
	int stat;
	char ctrl[32], msg[200];
	char *ud_uncord(),*ud_undist(),*ud_unang(),*ud_unvec();
/*
.....Switch on DAS type
*/
	switch (typ)
	{
	case UD_DASCART:
		strcpy(buf,ud_uncord(prec,len,data.frmvec));
		break;
	case UD_DASVAL:
	case UD_DASUNITLESS:
		sprintf(ctrl,"%%%d.%df",1,prec);
		sprintf(buf,ctrl,data.frmflt[0]);
		break;
	case UD_DASDISTANCE:
		strcpy(buf,ud_undist(prec,len,data.frmflt[0]));
		break;
	case UD_DASINT:
		sprintf(ctrl,"%%%dd",1);
		sprintf(buf,ctrl,data.frmint[0]);
		break;
	case UD_DASVEC:
		strcpy(buf,ud_unvec(prec,len,data.frmflt));
		break;
/*
......SCALAR value as string for now
*/
	case UD_SCACART:
	case UD_SCAVAL:
	case UD_SCAUNITLESS:
	case UD_SCADISTANCE:
	case UD_SCAINT:
	case UD_SCAVEC:
	case UD_SCANDC:
	case UD_SCAANGLE:
	case UD_DASSCALAR:
/*
.....check if the string is valid type scalar string
*/
		if (data.frmstr[0]=='\0')
		{
			buf[0] = '\0';
			break;
		}
		stat = ncl_parse_scalar_values(data.frmstr, buf, typ);
		if (stat==-1)
		{
			sprintf (msg, "It need a scalar input for input %s", data.frmstr);
			ud_winerror(msg);
		}
		strcpy(buf, data.frmstr);
		break;
	case UD_DASSTRING:
/*
.....We can not cut the default string
.....if the text field len is less that
.....the default string
.....Yurong changed 8/29/97
*/
/*		sprintf(ctrl,"%%-%d.%ds",len,prec);
		sprintf(buf,ctrl,data.frmstr); */
		strcpy(buf, data.frmstr);
		break;
	case UD_DASNDC:
		sprintf(ctrl,"<%%%d.%df,%%%d.%df,%%%d.%df>",
			1,prec,1,prec,1,prec);
		sprintf(buf,ctrl,data.frmflt[0],data.frmflt[1],data.frmflt[2]);
		break;
	case UD_DASANGLE:
		strcpy(buf,ud_unang(prec,len,data.frmflt[0]));
		break;
	default:
		buf[0] = '\0';
		break;
	}
}
