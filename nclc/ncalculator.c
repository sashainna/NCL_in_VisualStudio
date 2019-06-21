/*********************************************************************
**    NAME         :  ncalculator.c
**       CONTAINS: 
**       ncl_calculator
**
**    COPYRIGHT 2006 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       ncalculator.c , 25.2
**     DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 10:33:01
*********************************************************************/
#include "usysdef.h" 
#include "mfort.h"
#include "udfconst.h"
#include "udforms.h"
#include "nclcmd.h"
#include "dselmask.h"
#include "nclinp.h"
#include "mdrel.h"
#include "nclfc.h"
#include "nclinp.h"
#include "mdcoord.h"
#include "uhep.h"
#include <math.h>
#include "nccs.h"
#include "mdpick.h"
#include "ncl.h"

static int Scalculator = -1, Schcmod = 1;
#define PARES_LEVEL 20
/*
.....this is the string display in result window and saved value string (for undo)
*/
static char Svalue_str[256], Svalue_str_undo[256];
static char Senter_value_str[256], Senter_value_str_undo[256];
/*
.....this is the value saved in memomry
*/
static UU_REAL Smem_val=0.0;
/*
.....this is the current displayed direct value string (and saved value string (for undo))
.....for exeample, we enter "3+4*5+6", the Svalue_dir_str
.....will be '3' then '4' then '5', then '3+4*5', then '6'.if we enter
.....a + (c+d) + f, then the Svalue_dir_str will be 'a' then 'c'
.....then d, then '(c+d)', then 'a + (c+d)', the 'f'
*/
static char Svalue_dir_str[256], Svalue_dir_str_undo[256];
/*
.....save the first part of TWO value
*/
static char Svalue_dir_str_1[256], Svalue_str_1[256];
/*
.....this is the string display in formula window, it have PARES_LEVEL level by now
.....the last level Sform_str[0] will show it in formula window, the different level is 
.....for every parenthesis level
*/
/*
.....the Spre_form_str will store the string that not include the current value string
.....for example,  "1+2*(3+4*(5+6", the Spre_form_str[0] = "1+2*(3+4*(5+", 
.....the Spre_form_str[1] = "(3+4*(5+",  the Spre_form_str[2] = "(5+", 
.....Sform_str will always be Spre_form_str + current_value string
.....in above case, Sform_str[0] = "1+2*(3+4*(5+6" (which will display in 'formula window'
.....Sform_str[1] = (3+4*(5+6" (which is level 1 formula) and
.....Sform_str[2] = (5+6" (which is level 2 formula), after enter '6', if we "pie", the 
.....new formula will because Sform_str[0] = Spre_form_str[0] + 'pie' = "1+2*(3+4*(5+3.1412"
.....Sform_str[1] = Spre_form_str[1] + 'pie' = (3+4*(5+3.1412"
.....Sform_str[2] = Spre_form_str[2] + 'pie' = (5+3.1412",
.....if then we entered ')', then we need calculte Sform_str[2] = "(5+3.1412)" = 8.1412
.....and the new Sform_str[2] = Spre_form_str[2] is does not matter (only level 0 and level 1 now)
.....the new Spre_form_str[1] will become Spre_form_str[1] - Spre_form_str[2] = "(3+4*"
.....and the Spre_form_str[0] will become Spre_form_str[0] - Spre_form_str[2] = "1+2*(3+4*"
.....and current value string will because "(5+3.1412)"
.....So new formula will because Sform_str[0] = Spre_form_str[0] + "(5+3.1412)" = "1+2*(3+4*(5+3.1412)"
.....Sform_str[1] = Spre_form_str[1] + "(5+3.1412)" = (3+4*(5+3.1412)"
.....if then we entered '7' after ')', we will replace the result "(5+3.1412)" to '7'
.....Then new formula will because Sform_str[0] = Spre_form_str[0] + "7" = "1+2*(3+4*7"
.....Sform_str[1] = Spre_form_str[1] + "(5+3.1412)" = (3+4*7"
.....Yurong
*/
static char Sform_str[PARES_LEVEL][256], Spre_form_str[PARES_LEVEL][256], Spre_form_str_undo[PARES_LEVEL][256];
/*
.....another set of formula to be used for value2 of the TWO_VALUE ("value1, value2")
.....it will be act same as Sform_str/Spre_form_str/Spre_form_str_undo except is is for two value
*/
static char Sform_str_2[PARES_LEVEL][256], Spre_form_str_2[PARES_LEVEL][256], Spre_form_str_2_undo[PARES_LEVEL][256];

static char tempsav1[256], tempsav2[256];
static int inv_chk, open_pares = 0;
/*
.....open parenthsis for value 2
*/
static int open_pares_2 = 0;
static int Sval_prec=-1, Sdisp_type = 0;
static int Update_result = 1;
/*
......remember the previous input key
*/
#define TRIG_FUNC	0
#define GEOM_FUNC	1
#define NUMERIC_FUNC	2
#define NUMERIC_KEYS	3
#define PLUSMIN_KEY		4
#define OPERATION_KEY	5
#define PI_KEY	6
#define EXPO_KEY	7
#define INVERSION_KEY	8
#define COMMA_KEY	9
#define PAREN_LEFTKEY	10
#define PAREN_RIGHTKEY	11
#define MEMORY_KEY	12
#define EQUEL_KEY	13
#define SCALAR_KEY	14
#define EXPO_KEYP	15
#define SIGNED_KEY	16
#define TRIG_FUNC_2	17
#define TYPECHG_KEY	18
#define EXITFORM_KEY	19
/*
.....this value is check if we need reenter number or append the input keypad
.....for example, we enter number "abc", then change Snumber_reenter = 1, then
.....enter keypad "4", it will become '4' instead of '1234'
*/
static int	Snumber_reenter = 0;
static int pre_opr = -1, save_pre_opr = -1, save_reset_opr = -1;
static int Sundo_formula = 0;
/*
.....this is for last data type enter, it could be normal value (one value)
.....or two_value as (data, data) seperate by comma
*/
#define ONE_VALUE	1
#define TWO_VALUES	2
#define THREE_VALUES 3
static int Sdata_entered = ONE_VALUE, Spre_data_entered = ONE_VALUE;
static int Svalue_key = 0;
/*
.....when we have ')' or "x^2" or "x^3" or Trig, geom, numeric function (abs, int,...)
.....and "+/-" (plus_milus key), Inversion Key, we will immidialtly calculate the result
.....and display the result in the result window.
.....when we enter  '*', '/', or "**", do nothing of calculate.
.....when we entered the operator key '+', '-', we need update the result of 
.....the latest level of the result. For example:
.....we enter 1+3+2*4+2**4-1=, the result window will be '1', '3', '4' (which is 1+3),
.....'2', '4' (not update when enter '*'), '12' (which is 1+3+2*4)
.....'2', '4', '28' (which is 1+3+2*4+2**4), '1' then '27' (which is final result)
..... followed the Window's 'scientific calculator'
*/
/*
.....we won't allow undo twice, if undo more than once, it will stay the same
*/
static int	Sundo = 0;
static int SCal_cancel=0;
static int Sassigned = 0;

void ncl_update_input();
static UD_FSTAT  calc_formula_str();

/**********************************************************************
**    I_FUNCTION :  handle_formula_window(flag, string)
**       Handle formula window input, it will check if the formula
**		is at the focus now, also return the formula window text
**		if the flag =1, calculate the formula
**    PARAMETERS   
**       INPUT  :	flag: 1: calculate the formula
**
**	     OUTPUT : string: formula window text
**    RETURNS      : 1: formula window is at focus
**					0: No
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
handle_formula_window(flag, string)
int flag;
char *string;
{
	char tempstr[256];
	UD_DDATA data;
	int fldno, frmid;
/*
......check if the key is pushed when formula window is active
......using shortcut key (which the focus still in formula window)
......will have call Oncalc_formula_str function
......while directly push the button (which the focus is on the button)
......will call Oncalc_formula_str function
*/
	ud_get_focusfrm(&frmid, &fldno);
	data.frmstr = (char*)&tempstr;
	ud_getfrm_field(Scalculator, 4, data,UU_FALSE);
	strcpy (string, tempstr);
	if ((frmid==Scalculator) && (fldno==4))
/*
.....call Oncalc_formula_str function
*/
	{
		if (flag==1)
			calc_formula_str(tempstr);
		return 1;
	}
	return 0;
}
/*
......flag=1: val_str is a single value without sign
......adding this flag just for purpose of if we need add parenthsis
*/
/**********************************************************************
**    I_FUNCTION :  if_valid_exp(val_str, flag)
**       Check if the input string is a valid expression, if it is
**		also check if the expression is a single value without sign
**    PARAMETERS   
**       INPUT  :val_str: input string to be checked 
**	     OUTPUT : flag: 1: it is a single value without sign 
**    RETURNS      : 1: yes
**					0: No
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
if_valid_exp (val_str, flag)
char *val_str;
int *flag;
{
	int stat, i, len;
	UM_real8 value;
	char tmp1[256];
	int nc;

	*flag = 0;
	strcpy (tmp1, val_str);
	nc = strlen(tmp1);
	parse_expr (tmp1, &nc, &value, &stat);	
	if (stat==-1)
	{
		return 0;
	}
/*
.....check if the value is a single value without sign
*/
	len = strlen (tmp1);
	for (i=0; i<len; i++)
	{
		if ((isdigit(tmp1[i])==0)&&(tmp1[i]!='.'))
		{
			*flag = 0;
			return 1;
		}
	}
	*flag = 1;
	return 1;
}
/*********************************************************************
**    E_FUNCTION     : ncl_DMS2DMS_str(frmstr1, frmstr2)
**        Formats a DMS string into a DMS string with different precise
**    PARAMETERS
**       INPUT  :
**          frmstr1      input DMS string
**          
**       OUTPUT :
**          frmstr2      output DMS string
**          
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_DMS2DMS_str (frmstr1, frmstr2)
char *frmstr1, *frmstr2;
{
	int stat;
	char tmp1[256];
	UM_real8 value, sec, temp,fabs();
	int deg,min,nc;
	char sec_str[256];

	strcpy (tmp1, frmstr1);
	nc = strlen(tmp1);
	parse_expr (tmp1, &nc, &value, &stat);	
	temp = fabs(value);
	deg = (int)(temp);
	min = (int)((temp-deg) * 60.0 + .000000001);
	sec = fabs(((temp - deg) * 60.0 - min)*60.0);
	ncl_cal_printf(sec_str, &sec, 1);
	if (value < 0.)
		sprintf (frmstr2, "-%d'%d^%s", deg, min, sec_str);
	else
		sprintf (frmstr2, "%d'%d^%s", deg, min, sec_str);
}

/*********************************************************************
**    E_FUNCTION     : ncl_DMS2Dec_str(frmstr1, frmstr2)
**        Formats a DMS string into a Decimal string
**    PARAMETERS
**       INPUT  :
**          frmstr1      input DMS string
**          
**       OUTPUT :
**          frmstr2      output Decimal string
**          
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_DMS2Dec_str (frmstr1, frmstr2)
char *frmstr1, *frmstr2;
{
	UM_real8 value;
	int stat, nc;
	char tmp1[256];

	strcpy (tmp1, frmstr1);
	nc = strlen(tmp1);
	parse_expr (tmp1, &nc, &value, &stat);	
	if (stat==-1)
		return -1;
	else
	{
		ncl_cal_printf(frmstr2, &value, 1);
		return 0;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_Dec2DMS_str(frmstr1, frmstr2)
**        Formats a Decimal string into a DMS string
**    PARAMETERS
**       INPUT  :
**          frmstr1      input Decimal string
**          
**       OUTPUT :
**          frmstr2      output DMS string
**          
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_Dec2DMS_str (frmstr1, frmstr2)
char *frmstr1, *frmstr2;
{
	UM_real8 value, sec, temp,fabs();
	int deg,min;
	char sec_str[256];

	value = atof (frmstr1);
	temp = fabs(value);
	deg = (int)(temp);
	min = (int)((temp-deg) * 60.0 + .000000001);
	sec = fabs(((temp - deg) * 60.0 - min)*60.0);
	ncl_cal_printf(sec_str, &sec, 1);
	if (value < 0.)
		sprintf (frmstr2, "-%d'%d^%s", deg, min, sec_str);
	else
		sprintf (frmstr2, "%d'%d^%s", deg, min, sec_str);

}

/*********************************************************************
**    E_FUNCTION     : ncl_val2str (value, value_str)
**        Convert a value into a string
**    PARAMETERS
**       INPUT  :
**          value      value to be converted
**          
**       OUTPUT :
**          value_str      output value string
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_val2str (value, value_str)
UM_real8 value;
char *value_str;
{
	UM_real8 sec, temp,fabs();
	int deg,min;
	char sec_str[256];
	if (Sdisp_type==0)
	{
		ncl_cal_printf(value_str, &value, 1);
	}
	else
	{
		temp = fabs(value);
		deg = (int)(temp);
		min = (int)((temp-deg) * 60.0 + .000000001);
		sec = fabs(((temp - deg) * 60.0 - min)*60.0);
		ncl_cal_printf(sec_str, &sec, 1);
		if (value < 0.)
			sprintf (value_str, "-%d'%d^%s", deg, min, sec_str);
		else
			sprintf (value_str, "%d'%d^%s", deg, min, sec_str);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_cal_printf(strng,elm,num)
**        Formats a string with a real number use calculator precise method
**    PARAMETERS
**       INPUT  :
**          strng      A character string
**          elm        An array of reals to be formatted
**          num        The number of elements in elm
**          
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_cal_printf(strng,elm,num)
char strng[];
UU_REAL elm[];
int num;
{
	return ncl_sprintf2(strng,elm,num, Sval_prec);
}
/*********************************************************************
**    E_FUNCTION     : handle_inver_key
**        Handled 1/x key input
**    PARAMETERS
**       INPUT  : none
**          
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....need change Svalue_dir_str/Svalue_dir_str_undo
*/
void handle_inver_key()
{
	UM_real8 value;
	int i,stat,len, flag, nc;
	char tempstr[256], value_dir_str[256], temp1[256], temp2[256];

	handle_formula_window(1, tempstr);
/**************
	if (pre_opr == -1)
	{
		ud_wrerr("No data input yet!");
		return;
	}	
*/
	if (pre_opr == COMMA_KEY)
	{
		ud_wrerr("1/x cannot follow a comma key.");
		return;
	}	
	if (pre_opr == PAREN_LEFTKEY)
	{
		ud_wrerr("1/x cannot follow a left parenthesis.");
		return;
	}	
	if (atof(Svalue_str)==0.0)
	{
		ud_wrerr("The inverse of 0 is not allowed.");
		return;
	}	
/*	if ((Svalue_dir_str[0]=='\0') && ((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP))) */
	if (Svalue_dir_str[0]=='\0')
	{
		ud_wrerr("1/x must apply to a value.");
		return;
	}	
	if (if_valid_exp (Svalue_dir_str, &flag)==0)
	{
		ud_wrerr("1/x must apply to a value.");
		return;
	}	
	if (pre_opr==INVERSION_KEY)
	{
/*
.....toggle back to before
*/
		strcpy(temp1, Svalue_dir_str);
		strcpy(temp2, Svalue_dir_str_undo);
		strcpy (Svalue_dir_str, temp2);
		strcpy (Svalue_dir_str_undo, temp1);
	}
	else
	{
		strcpy(Svalue_dir_str_undo, Svalue_dir_str);
		len = strlen (Svalue_dir_str);
		if (len==0)
			return;
		if (if_valid_exp (Svalue_dir_str, &flag)==0)
		{
			ud_wrerr("An operator must follow a number or scalar.");
			return;
		}
		if (((Svalue_dir_str[0]=='(') && (Svalue_dir_str[len-1]==')')) || (flag==1))
			sprintf(tempstr, "1/%s", Svalue_dir_str);
		else
			sprintf(tempstr, "1/(%s)", Svalue_dir_str);
		strcpy(Svalue_dir_str, tempstr);
	}
	if (Sdata_entered != TWO_VALUES)
	{
		for (i=0; i<open_pares+1; i++)
		{
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
			if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
/*
.....replace old Svalue_dir_str with new Svalue_dir_str in the end of Sform_str if the
.....previous entered value
*/
				strcpy(Sform_str[i], Spre_form_str[i]);
			else
			{
/*
.....even though it pre key is an operation key, but it could still be a value
.....because when "2-sin" it will become "2-sin(2)", the pre key is '-', not 'sin'
.....which we need set that way in order to undo "2-sin" to "2-".
*/
				if (Svalue_key)
				{
					strcpy(tempstr, Spre_form_str[i]);
				}
				else
				{
/*
.....it is the first time enter number, save Spre_form_str
*/
					strcpy(Spre_form_str[i], Sform_str[i]);
				}
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
/*				strcpy(Spre_form_str_undo[i], Spre_form_str[i]); */
				if (Svalue_key)
				{
					strcpy(Sform_str[i], tempstr);
				}
			}
			strcat(Sform_str[i], Svalue_dir_str);
		}
	}
	else
	{
/*
.....update two value formula
*/
		for (i=0; i<open_pares_2+1; i++)
		{
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
			if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
				strcpy(Sform_str_2[i], Spre_form_str_2[i]);
			else
			{
/*
.....even though it pre key is an operation key, but it could still be a value
.....because when "2-sin" it will become "2-sin(2)", the pre key is '-', not 'sin'
.....which we need set that way in order to undo "2-sin" to "2-".
*/
				if (Svalue_key)
				{
					strcpy(tempstr, Spre_form_str_2[i]);
				}
				else
				{
/*
.....it is the first time enter number, save Spre_form_str
*/
					strcpy(Spre_form_str[i], Sform_str[i]);
				}
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
/*				strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]); */
				if (Svalue_key)
				{
					strcpy(Sform_str_2[i], tempstr);
				}
			}
			strcat(Sform_str_2[i], Svalue_dir_str);
		}
/*
.....also we need update the main formula too
*/
		strcpy(value_dir_str, Svalue_dir_str_1);
		strcat(value_dir_str, ",");
		strcat(value_dir_str, Sform_str_2[0]);
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Sform_str[i], Spre_form_str[i]);
			strcat(Sform_str[i], value_dir_str);
		}
	}
	
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
/*
.....get the result
*/
	nc = strlen(Svalue_dir_str);
	parse_expr (Svalue_dir_str, &nc, &value, &stat);	
	strcpy (Svalue_str_undo, Svalue_str);
	ncl_val2str (value, Svalue_str);
	ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
	save_pre_opr = pre_opr;
	if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
	{
		pre_opr = INVERSION_KEY;
	}
	Svalue_key = 1;
	Sundo = 0;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : handle_comma_key()
**        Handled comma key input
**    PARAMETERS
**       INPUT  : none
**          
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void handle_comma_key()
{
	int i;
	UM_real8 value;
	char tempstr[256];
	handle_formula_window(1, tempstr);

	if (Sdata_entered==TWO_VALUES)
	{
		ud_wrerr("Maxinum of two values allowed.");
		return;
	}	
/**************
	if (pre_opr == -1)
	{
		ud_wrerr("No data input yet!");
		return;
	}	
*/
/*
.....it must be a value before comma
*/
	if (Svalue_key==0)
	{
		ud_wrerr("An comma must follow a number or scalar.");
		return;
	}	

	for (i=0; i<open_pares+1; i++)
	{
		strcat(Sform_str[i], ",");
	}
	strcpy(Svalue_dir_str_undo, Svalue_dir_str);

	strcpy (Svalue_str_undo, Svalue_str);
/*
.....after we enter comma, it mean user want to enter a TWO_VALUE, we will now save the first value
.....we save the first value in Svalue_str_1 (digital value) and Svalue_dir_str_1 (expression value)
.....then start to accept the second value
.....but we do need initial Sform_str_2, which will be used for calculate value2
.....the two value
*/
	strcpy (Svalue_str_1, Svalue_str);
	strcpy (Svalue_dir_str_1, Svalue_dir_str);
/*
.....when comman entered, reset value to 0.0 as the second value of comma
*/
	value = 0.0;
	ncl_val2str (value, Svalue_str);
/*
	Svalue_dir_str[0] = '\0';
*/
	strcpy (Svalue_dir_str, Svalue_str);
	Svalue_dir_str_undo[0] = '\0';

	Spre_data_entered = Sdata_entered;	
	Sdata_entered = TWO_VALUES;
	open_pares_2 = 0;
	Sform_str_2[0][0] = '\0';
	Spre_form_str_2[0][0] = '\0';
	Spre_form_str_2_undo[0][0] = '\0';

	save_pre_opr = pre_opr;
	pre_opr = COMMA_KEY;
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
	ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
	Sundo = 0;
	Svalue_key = 0;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : handle_leftp_key()
**        Handled Left parenthsis key input
**    PARAMETERS
**       INPUT  : none
**          
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void handle_leftp_key()
{
	int i;
	char value_dir_str[256];

	char tempstr[256];
	handle_formula_window(1, tempstr);

	if ((pre_opr==EXPO_KEY) || (pre_opr==TRIG_FUNC) || (pre_opr==NUMERIC_FUNC)
			|| (pre_opr==GEOM_FUNC) || (pre_opr==INVERSION_KEY)
			|| (pre_opr==PI_KEY) || (pre_opr==EQUEL_KEY)
			|| (pre_opr==SCALAR_KEY) || (pre_opr==MEMORY_KEY)
			|| (pre_opr==PAREN_RIGHTKEY) || (pre_opr==NUMERIC_KEYS)
			||	(pre_opr==PLUSMIN_KEY) || (pre_opr==TRIG_FUNC_2)
			|| (pre_opr==EXITFORM_KEY))
	{
		ud_wrerr("Left parenthesis cannot follow a value!");
		return;
	}	
	if (Sdata_entered != TWO_VALUES)
	{
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
			strcpy(Spre_form_str[i], Sform_str[i]);
			strcat(Sform_str[i], "(");
		}
	}
	else
	{
/*
.....update two value formula
*/
		for (i=0; i<open_pares_2+1; i++)
		{
			strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
			strcpy(Spre_form_str_2[i], Sform_str_2[i]);
			strcat(Sform_str_2[i], "(");
		}
/*
.....also we need update the main formula too
*/
		strcpy(value_dir_str, Svalue_dir_str_1);
		strcat(value_dir_str, ",");
		strcat(value_dir_str, Sform_str_2[0]);
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Sform_str[i], Spre_form_str[i]);
			strcat(Sform_str[i], value_dir_str);
		}
	}
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
/*
......when type '(', just init set new Svalue_dir_str, Sform_str[open_pares],
......Spre_form_str[open_pares] value
*/
	if (Sdata_entered != TWO_VALUES)
	{
		open_pares++;
		ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares);
		if (open_pares<=0)
			ud_setfrm_traverse_mask(Scalculator, 51, UU_FALSE);
		else
			ud_setfrm_traverse_mask(Scalculator, 51, UU_TRUE);
		strcpy(Sform_str[open_pares], "(");
		Spre_form_str[open_pares][0] = '\0';
		Spre_form_str_undo[open_pares][0] = '\0';
	}
	else
	{
		open_pares_2++;
		ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares_2);
		if (open_pares_2<=0)
			ud_setfrm_traverse_mask(Scalculator, 51, UU_FALSE);
		else
			ud_setfrm_traverse_mask(Scalculator, 51, UU_TRUE);
		strcpy(Sform_str_2[open_pares_2], "(");
		Spre_form_str_2[open_pares_2][0] = '\0';
		Spre_form_str_2_undo[open_pares_2][0] = '\0';
	}
	strcpy(Svalue_dir_str_undo, Svalue_dir_str);
	strcpy(Svalue_str_undo, Svalue_str);


	strcpy(Svalue_dir_str, "0.0");

	if (open_pares!=0)
	{
		if (Schcmod)
			ud_setfrm_traverse_mask(Scalculator, 3, UU_FALSE);
		ud_setfrm_traverse_mask(Scalculator, 56, UU_FALSE);
	}
	else
	{
		if (Schcmod)
			ud_setfrm_traverse_mask(Scalculator, 3, UU_TRUE);
		ud_setfrm_traverse_mask(Scalculator, 56, UU_TRUE);
	}
	ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares);
	if (open_pares<=0)
		ud_setfrm_traverse_mask(Scalculator, 51, UU_FALSE);
	else
		ud_setfrm_traverse_mask(Scalculator, 51, UU_TRUE);
	save_pre_opr = pre_opr;
	pre_opr = PAREN_LEFTKEY;
	Sundo = 0;
	Svalue_key = 0;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
}
/*
.....may be close like "(a+b)", so the new "(a+b)" because the Svalue_dir_str
.....need change Svalue_dir_str/Svalue_dir_str_undo
*/
/*********************************************************************
**    E_FUNCTION     : handle_rightp_key()
**        Handled right parenthsis key input
**    PARAMETERS
**       INPUT  : none
**          
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void handle_rightp_key()
{
	UM_real8 value;
	int i,stat, len, len0, nc;
	char erms[400];
	char value_dir_str[256], tempstr[256];

	handle_formula_window(1, tempstr);

	if ((pre_opr == -1) || (open_pares<=0))
	{
		ud_wrerr("Right parenthesis entered without corresponding left parenthesis!");
		return;
	}	
	if ((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP))
	{
		ud_wrerr("Right parenthesis must follow a number or a scalar!");
		return;
	}	
	if (pre_opr == COMMA_KEY)
	{
		ud_wrerr("Right parenthesis must follow a number or a scalar!");
		return;
	}	
	if (pre_opr == PAREN_LEFTKEY)
	{
		ud_wrerr("Right parenthesis must follow a number or a scalar!");
		return;
	}	
/*
.....check if enclose a valid value
*/
	if (Sdata_entered != TWO_VALUES)
	{
		strcpy (tempstr, Sform_str[open_pares]);
	}
	else
	{
		strcpy (tempstr, Sform_str_2[open_pares_2]);
	}
	strcat(tempstr, ")");
	nc = strlen (tempstr);
	parse_expr (tempstr, &nc, &value, &stat);
	if (stat==-1)
	{
		sprintf(erms,"\"%s\" is not a valid formula.",tempstr);
		ud_wrerr(erms);
		return;
	}
/*
......when type ')', show result of this level and put the string within
......this '(', ')' into the Svalue_dir_str, reset Svalue, Svalue_str
*/
/*
.....and the new Spre_form_str[open_pares] is does not matter (only open_pares level now)
.....the new Spre_form_str[i] will become Spre_form_str[i] - Spre_form_str[open_pares]
.....refer the comment when define Spre_form_str
*/
	if (Sdata_entered != TWO_VALUES)
	{
		len0 = strlen (Spre_form_str[open_pares]);
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
			if (i<open_pares)
			{
				len = strlen (Spre_form_str[i]) - len0;
				Spre_form_str[i][len] = '\0';
			}
			strcat(Sform_str[i], ")");
		}
	}
	else
	{
/*
.....update two value formula
*/
		len0 = strlen (Spre_form_str_2[open_pares_2]);
		for (i=0; i<open_pares_2+1; i++)
		{
			strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
			if (i<open_pares_2)
			{
				len = strlen (Spre_form_str_2[i]) - len0;
				Spre_form_str_2[i][len] = '\0';
			}
			strcat(Sform_str_2[i], ")");
		}
/*
.....also we need update the main formula too
*/
		strcpy(value_dir_str, Svalue_dir_str_1);
		strcat(value_dir_str, ",");
		strcat(value_dir_str, Sform_str_2[0]);
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Sform_str[i], Spre_form_str[i]);
			strcat(Sform_str[i], value_dir_str);
		}
	}
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
/*
.....check if it is valid formula and get the result
*/
	if (Sdata_entered != TWO_VALUES)
	{
		nc = strlen (Sform_str[open_pares]);
		parse_expr (Sform_str[open_pares], &nc, &value, &stat);
		if (stat!=-1)
		{
			strcpy (Svalue_str_undo, Svalue_str);
			ncl_val2str (value, Svalue_str);
			ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
			strcpy(Svalue_dir_str_undo, Svalue_dir_str);
			strcpy(Svalue_dir_str, Sform_str[open_pares]);
		}
		else
		{
			sprintf(erms,"\"%s\" is not a valid formula.",Sform_str[open_pares]);
			ud_wrerr(erms);
			return;
		}
		Spre_form_str[open_pares][0] = '\0';
		open_pares--;
		ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares);
		if (open_pares<=0)
			ud_setfrm_traverse_mask(Scalculator, 51, UU_FALSE);
		else
			ud_setfrm_traverse_mask(Scalculator, 51, UU_TRUE);
	}
	else
	{
		nc = strlen(Sform_str_2[open_pares_2]);
		parse_expr (Sform_str_2[open_pares_2], &nc, &value, &stat);
		if (stat!=-1)
		{
			strcpy (Svalue_str_undo, Svalue_str);
			ncl_val2str (value, Svalue_str);
			ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
			strcpy(Svalue_dir_str_undo, Svalue_dir_str);
			strcpy(Svalue_dir_str, Sform_str[open_pares]);
		}
		else
		{
			sprintf(erms,"\"%s\" is not a valid formula.",Sform_str_2[open_pares_2]);
			ud_wrerr(erms);
			return;
		}
		Spre_form_str[open_pares_2][0] = '\0';
		open_pares_2--;
		ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares_2);
		if (open_pares_2<=0)
			ud_setfrm_traverse_mask(Scalculator, 51, UU_FALSE);
		else
			ud_setfrm_traverse_mask(Scalculator, 51, UU_TRUE);
	}
	if (open_pares!=0)
	{
		if (Schcmod)
			ud_setfrm_traverse_mask(Scalculator, 3, UU_FALSE);
		ud_setfrm_traverse_mask(Scalculator, 56, UU_FALSE);
	}
	else
	{
		if (Schcmod)
			ud_setfrm_traverse_mask(Scalculator, 3, UU_TRUE);
		ud_setfrm_traverse_mask(Scalculator, 56, UU_TRUE);
	}
	save_pre_opr = pre_opr;
	pre_opr = PAREN_RIGHTKEY;
	Svalue_key = 1;
	Sundo = 0;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
}
/*********************************************************************
**    E_FUNCTION     : handle_equel_key()
**        Handled equel key input
**    PARAMETERS
**       INPUT  : none
**          
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void handle_equel_key()
{
	int len, stat,flag,nc;
	UM_real8 value;
	char erms[400], tempstr[256];
	int  form;
	
	form = handle_formula_window(0, tempstr);
	if (form==0)
	{
		if (pre_opr == EQUEL_KEY)
			return;

		if (open_pares!=0)
		{
			sprintf(erms,"\"%s\" have unmatched parenthesis",Sform_str[0]);
			ud_wrerr(erms);
			return;
		}
		if (Sdata_entered == TWO_VALUES)
		{
			ud_wrerr("= cannot apply to multiple values!");
			return;
		}	
		if (((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)) && (Svalue_key==0))
		{
			ud_wrerr("= cannot follow an operator.");
			return;
		}	
		if (pre_opr == COMMA_KEY)
		{
			ud_wrerr("= cannot follow a comma key!");
			return;
		}	
		if (pre_opr == PAREN_LEFTKEY)
		{
			ud_wrerr("= cannot follow a left parenthesis!");
			return;
		}	
	}
	strcpy(Sform_str[0], tempstr);
	if (Sform_str[0]!='\0')
	{
		strcpy(tempstr, Sform_str[0]);
	}
	else
		tempstr[0] = '\0';
/*
.....remove trailing space
*/
	len = strlen (tempstr);
	if (len==0)
		return;;
	while (len>=0)
	{
		if (tempstr[len]==' ')
			len--;
		else
			break;
	}
	if (len<=0)
		return;
	tempstr[len] = '\0';
/*
.....check if it is valid formula and get the result
*/
	nc = strlen(tempstr);
	parse_expr (tempstr, &nc, &value, &stat);
	if (stat!=-1)
	{
		strcpy (Svalue_str_undo, Svalue_str);
		ncl_val2str (value, Svalue_str);
		ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
		strcpy(Svalue_dir_str_undo, Svalue_dir_str);
	}
	else
	{
		sprintf(erms,"\"%s\" is not a valid formula.",tempstr);
		ud_wrerr(erms);
		return;
	}
		strcpy(Svalue_dir_str, Sform_str[0]);
/*
.....if the formula already have double parenthesis, don't add them again
*/
	len = strlen (Sform_str[0]);
/*
.....see if it is single value or expression
*/
	if_valid_exp (Sform_str[0], &flag);
	if (((Sform_str[0][0]=='(') && (Sform_str[0][len-1]==')')) || (flag==1))
		strcpy(tempstr, Sform_str[0]);
	else
		sprintf(tempstr, "(%s)", Sform_str[0]);
	strcpy(Sform_str[0], tempstr);
/*
.....when '=', the Spre_form_str[0] should be '\0' because the whole formula
.....become the Svalue_dir_str, and the old Spre_form_str[0] should saved in pre_form_str_undo[0]
*/
	strcpy(Spre_form_str_undo[0], Spre_form_str[0]);
	Spre_form_str[0][0] = '\0';
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
	save_pre_opr = pre_opr;
	pre_opr = EQUEL_KEY;
	Svalue_key = 1;
	Sundo = 0;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : handle_scalar_key
**       display a list form include all scalars and descriptions
**		if the user not cancel the form and select a scalar, then it will be
**		displayed in the Results window and the scalar added to the formula
**		
**		This function is similar to ncl_scalar_form but it need a seperate one
**		because it update the different area
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void handle_scalar_key()
{
	char text_str[256];
/*
.....set focus to this calculator input formula field 4 in order for apply buuton to
.....set selected scalar to the input formula field
*/
	ud_dispfrm_set_focus(Scalculator, 4);
	ncl_scalar_form(text_str); 
	return;
}
/*********************************************************************
**    E_FUNCTION     : handle_equel_key()
**        Handled MS, MR, M+, M- key input
**    PARAMETERS
**       INPUT  : flag: 1: MR
**						2:	MS	3: M+	4: M-
**          
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void handle_mem_key(flag)
int flag;
{
	char val_str[256], value_dir_str[256];
	int i;
	char tempstr[256];
	handle_formula_window(1, tempstr);
	if (flag==1)
	{
/*
......MR
*/
		ncl_val2str (Smem_val, val_str);
/*
.....enter the memory value into the Results window, the same 
.....as if the digits were entered individually.  If a number 
.....is currently being input (the last key pressed was a number key), 
.....then this number will be replaced with the memory value
*/
/*
.....if it previous operation is a function or generate a value
.....then result display a number, when we enter a number right now
.....it will replace the old result with this new number
*/
/*
......if we entered a two values, replace the number we just entered
*/
		if ((pre_opr==EXPO_KEY) || (pre_opr==TRIG_FUNC) || (pre_opr==NUMERIC_FUNC)
			|| (pre_opr==GEOM_FUNC) || (pre_opr==INVERSION_KEY)
			|| (pre_opr==PI_KEY) || (pre_opr==EQUEL_KEY)
			|| (pre_opr==SCALAR_KEY) || (pre_opr==MEMORY_KEY)
			|| (pre_opr==PAREN_RIGHTKEY) || (pre_opr==NUMERIC_KEYS)
			|| (pre_opr==SIGNED_KEY) || (pre_opr==TRIG_FUNC_2)
			||	(pre_opr==PLUSMIN_KEY)
			|| (pre_opr==EXITFORM_KEY))
		{
			if (Sdata_entered != TWO_VALUES)
			{
/*
.....save the Spre_form_str_undo here, because it is not first time enter number
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
				for (i=0; i<open_pares+1; i++)
				{
					strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
					strcpy(Sform_str[i], Spre_form_str[i]);
					strcat(Sform_str[i], val_str);
				}
			}
			else
			{
/*
.....save the Spre_form_str_undo here, because it is not first time enter number
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
				for (i=0; i<open_pares_2+1; i++)
				{
					strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
					strcpy(Sform_str_2[i], Spre_form_str_2[i]);
					strcat(Sform_str_2[i], val_str);
				}
/*
.....also we need update the main formula too
*/
				strcpy(value_dir_str, Svalue_dir_str_1);
				strcat(value_dir_str, ",");
				strcat(value_dir_str, Sform_str_2[0]);
				for (i=0; i<open_pares+1; i++)
				{
					strcpy(Sform_str[i], Spre_form_str[i]);
					strcat(Sform_str[i], value_dir_str);
				}
			}
		}
		else
		{
/*
.....OPERATION_KEY, EXPO_KEYP, COMMA_KEY, PAREN_LEFTKEY, SIGNED_KEY or -1
*/
			if (Sdata_entered != TWO_VALUES)
			{
/*
.....save the Spre_form_str here, because it is first time enter number
*/
				for (i=0; i<open_pares+1; i++)
				{
					strcpy(Spre_form_str[i], Sform_str[i]);
					strcat(Sform_str[i], val_str);
				}
			}
			else
			{
				for (i=0; i<open_pares_2+1; i++)
				{
					strcpy(Spre_form_str_2[i], Sform_str_2[i]);
					strcat(Sform_str_2[i], val_str);
				}
/*
.....also we need update the main formula too
*/
				strcpy(value_dir_str, Svalue_dir_str_1);
				strcat(value_dir_str, ",");
				strcat(value_dir_str, Sform_str_2[0]);
				for (i=0; i<open_pares+1; i++)
				{
					strcpy(Sform_str[i], Spre_form_str[i]);
					strcat(Sform_str[i], value_dir_str);
				}
			}
		}	
		strcpy (Svalue_str_undo, Svalue_str);
		strcpy(Svalue_str, val_str);
		if ((pre_opr==OPERATION_KEY) || (pre_opr==EXPO_KEYP) 
				|| (pre_opr==PAREN_LEFTKEY))
/*
.....first time enter number after operation
*/
			Svalue_dir_str_undo[0] = '\0';
		else
			strcpy(Svalue_dir_str_undo, Svalue_dir_str);
		strcpy(Svalue_dir_str, val_str);
		ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
		ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
/*
.....save the operation key because it will affect 'result' and 'formula'
*/
		save_pre_opr = pre_opr;
		pre_opr = MEMORY_KEY;
		Svalue_key = 1;
		Sundo = 0;
		ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
	}
	else if (flag==2)
	{
/*
.....MS, save the value into the memory, but not change the result and formula
*/
/**************
		if (pre_opr == -1)
		{
			ud_wrerr("No data input yet!");
			return;
		}	
		if (Sdata_entered==TWO_VALUES)
		{
			sprintf(erms,"The result \"%s\" is two value and can't save into memory",
				Svalue_str);
			ud_wrerr(erms);
			return;
		}
*/
		Smem_val = atof (Svalue_str);
/*
.....no need to save the operation key because it not affect 'result' and 'formula'
		save_pre_opr = pre_opr;
		pre_opr = MEMORY_KEY;
*/
	}
	else if (flag==3)
	{
/*
.....M+, add the value into the memory, but not change the result and formula
*/
/**************
		if (pre_opr == -1)
		{
			ud_wrerr("No data input yet!");
			return;
		}	
		if (Sdata_entered==TWO_VALUES)
		{
			sprintf(erms,"The result \"%s\" is two value and can't do M+ operation",
				Svalue_str);
			ud_wrerr(erms);
			return;
		}
*/
		Smem_val += atof (Svalue_str);
/*
.....no need to save the operation key because it not affect 'result' and 'formula'
		save_pre_opr = pre_opr;
		pre_opr = MEMORY_KEY;
*/
	}
	else if (flag==4)
	{
/*
.....MC, Clear the value in the memory, but not change the result and formula
*/
		Smem_val = 0.0;
/*
.....no need to save the operation key because it not affect 'result' and 'formula'
		save_pre_opr = pre_opr;
		pre_opr = MEMORY_KEY;
*/
	}
	else if (flag==5)
	{
/*
.....M-, decrese the value from the memory, but not change the result and formula
*/
/**************
		if (pre_opr == -1)
		{
			ud_wrerr("No data input yet!");
			return;
		}	
		if (Sdata_entered==TWO_VALUES)
		{
			sprintf(erms,"The result \"%s\" is two value and can't do M- operation",
				Svalue_str);
			ud_wrerr(erms);
			return;
		}
*/
		Smem_val -= atof (Svalue_str);
/*
.....no need to save the operation key because it not affect 'result' and 'formula'
		save_pre_opr = pre_opr;
		pre_opr = MEMORY_KEY;
*/
	}
}


/*********************************************************************
**    E_FUNCTION :  handle_geom_func(flag)
**       This function handle the Geom function input
**		The geometry functions require the user to select one or more geometric
**		entities from the screen.  Once the entities are selected, the results
**		will be displayed in the Results window just as if a number was entered
**		and the geometry function will be added to the formula.  For the TDIST 
**    fuction, an additional input value will be required to signify distance
**    type.
**
**    PARAMETERS
**       INPUT  :
**          flag - geometry function function
**					1: ANGLF
**					2: DIST
**					3: DOT
**					4: LNTH
**             5: TDIST
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....may need change Svalue_dir_str/Svalue_dir_str_undo
*/
void handle_geom_func(flag)
int flag;
{
	int rel, pr;
	int i, stat, status, numint,nc,typ;
	char opn[256], val_str[256], dir_val_str[256], value_dir_str[256];
	UM_real8 value;
	char tmpstr[256], label1[256], label2[256], label3[256];
	NCL_cmdbuf cmdbuf;
    int pick_mask[UD_NMENTWD], *mask;
	UD_PLOCREC pick1, pick2;
	UM_PICKENT pent;
	struct NCL_fixed_databag e;
	struct NCL_fixed_databag Se[2];
	UM_coord pt[2];
	UM_vector vc[2];
	UM_int2 etype;
	int markval=0, choice;
	struct UM_point_rec *nrpt = 0;

/*
.....Reset flag to use TDISTF when inverse box is checked.
*/
	if ((inv_chk==1) && (flag==2)) flag = 5;

	handle_formula_window(1, opn);

	ncl_init_cmdbuf(&cmdbuf);

	strcpy(tempsav1, Svalue_dir_str_undo); 
	strcpy(tempsav2, Svalue_dir_str); 
/*
.....Take down form
*/
	ud_dspfrm_invis(Scalculator);
/*
.....Command Reject
*/
	UD_MARK (markval,UU_TRUE);
	if (markval != 0)
	{
		strcpy(Svalue_dir_str_undo, tempsav1); 
		strcpy(Svalue_dir_str, tempsav2);
		goto done;
	}
	if (flag==1)
	{
/*
.....anglf(line1, line2)
.....		(plane1, plane2)
.....		(vector1, vector2)
.....		(ptvector1, ptvector2)
.....		(vector1, ptvector2)
.....		(ptvector1, vector2)
.....		(circle, point)
*/
		for (i=0; i<UD_NMENTWD; i++)
		{
			pick_mask[i] = 0;
			pick_mask[i] = pick_mask[i] | UD_ncl_ln[i];
			pick_mask[i] = pick_mask[i] | UD_ncl_pl[i];
			pick_mask[i] = pick_mask[i] | UD_ncl_ve[i];
			pick_mask[i] = pick_mask[i] | UD_ncl_pv[i];
			pick_mask[i] = pick_mask[i] | UD_ncl_ci[i];
		}
		status = ncl_add_label_rel(UD_DASPCKLOC, 
			&cmdbuf, 238, pick_mask, &rel);
		if (status == NCL_OKINPUT)
		{
		switch (rel)
			{
/*
.....anglf(line1, line2)
*/
			case NCL_LINE_REL:
			case UM_LINE_REL:
				status = ncl_add_label(UD_DASPCKLOC, 
					&cmdbuf, 238, UD_ncl_ln);
				break;
/*
.....anglf(plane1, plane2)
*/
			case NCL_PLN_REL:
			case UM_RBSPLSRF_REL:
			case UM_RBSPLCRV_REL:
			case NCL_SURF_REL:
			case NCL_TRIMSF_REL:
				status = ncl_add_label(UD_DASPCKLOC,
					&cmdbuf, 238, UD_ncl_pl);
				break;
			case NCL_VECTOR_REL:
/*
.....anglf(vector1, vector2)
.....		(vector1, ptvector2)
*/
				status = ncl_add_label(UD_DASPCKLOC,
					&cmdbuf, 238, UD_ncl_vepv);
				break;
/*
.....anglf(ptvector1, ptvector2)
.....		(ptvector1, vector2)
*/
			case NCL_POINTVEC_REL:
				status = ncl_add_label(UD_DASPCKLOC, 
					&cmdbuf, 238, UD_ncl_vepv);
				break;
/*
.....anglf(circle, point)
*/
			case NCL_CIRCLE_REL:
			case UM_CIRCLE_REL:
				status = ncl_add_label(UD_DASPCKLOC, 
					&cmdbuf, 238, UD_ncl_pt);
				break;
			}
		}
		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
			ncl_del_token(&cmdbuf,"", UU_TRUE);
			strcpy(tmpstr, cmdbuf.cur_str);
			strcpy(opn, "Anglf");
			strcat (opn, "(");
			strcat (opn, tmpstr);
			strcat (opn, ")");
			nc = strlen(opn);
			parse_expr (opn, &nc, &value, &stat);
		}
		else
			goto done;
	}
	else if (flag==2)
	{
/*
.....Distf(entity1, entity2)
.....nclu_dist, no DISTF for CV now, all need 2 entities
*/
/*
.....the first entity must be PT, PV, PL, LN, it can't be SF, CV
*/
/********
		for (i=0; i<UD_NMENTWD; i++)
		{
			pick_mask[i] = UD_ncl_pt[i] | UD_ncl_ln[i] | UD_ncl_pv[i] | UD_ncl_pl[i];
		}
		status = ncl_add_label_rel(UD_DASPCKLOC, 
			&cmdbuf, 238, pick_mask, &rel);
		if (status == NCL_OKINPUT)
		{
		switch (rel)
			{
			case NCL_POINT_REL:
			case UM_POINT_REL:
*/
/*
.....PT to PT, PV, LN, SF, PLAIN
*/
/********
			case NCL_POINTVEC_REL:
*/
/*
.....PV to PT, PV, LN, SF, PLAIN
*/
/********
				for (i=0; i<UD_NMENTWD; i++)
				{
					pick_mask[i] = UD_ncl_ptplsf[i] | UD_ncl_ln[i] | UD_ncl_pv[i];
				}
				status = ncl_add_label(UD_DASPCKLOC, 
					&cmdbuf, 238, pick_mask);
				break;
			case NCL_LINE_REL:
			case UM_LINE_REL:
				for (i=0; i<UD_NMENTWD; i++)
				{
					pick_mask[i] = UD_ncl_ln[i] | UD_ncl_pt[i] | UD_ncl_pv[i];
				}
				status = ncl_add_label(UD_DASPCKLOC, 
					&cmdbuf, 238, pick_mask);
				break;
			case NCL_PLN_REL:
			case UM_RBSPLSRF_REL:
			case UM_RBSPLCRV_REL:
				status = ncl_add_label(UD_DASPCKLOC,
					&cmdbuf, 238, UD_ncl_ptpl);
				break;
			}
		}
		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
			ncl_del_token(&cmdbuf,"", UU_TRUE);
			strcpy(tmpstr, cmdbuf.cur_str);
			strcpy(opn, "Dist");
			strcat (opn, "(");
			strcat (opn, tmpstr);
			strcat (opn, ")");
			parse_expr (opn, &value, &stat);
		}
		else
			goto done;
*/
/*
.....Get user selection
*/
		for (i=0; i<UD_NMENTWD; i++)
		{
			pick_mask[i] = UD_ncl_pt[i] | UD_ncl_ln[i] | UD_ncl_pv[i] |
				UD_ncl_pl[i] | UD_ncl_allcv[i] | UD_ncl_allsfsh[i] | UD_ncl_ci[i];
		}
		mask = (int *)pick_mask;
		ud_lgeo(UU_TRUE,mask);
		pr = 653;
		ud_ldas(UD_DASPCKLOC,UA_NCL,pr,&pick1,1,&numint,1,UD_NODEFAULT);
		if (numint == 0) 		
			goto done;
/*
.....Get picked entity
*/
		um_d_pickresolve(&(pick1.ppath),1,&pent);
		e.key = um_get_pickkey(&pent,1);
		ncl_retrieve_data_fixed(&e);
		strcpy(label1, pick1.pndc.label);
		Se[0] = e;
		ncl_get_type(e.rel_num,&etype);
		switch (etype)
		{
/*
........Point, can't measure distand with vector
*/
		case NCLI_POINT:
			for (i=0; i<UD_NMENTWD; i++)
			{
				pick_mask[i] = UD_ncl_pt[i] | UD_ncl_ln[i] | UD_ncl_pl[i] |
					UD_ncl_pv[i] | UD_ncl_allcv[i] | UD_ncl_allsfsh[i] |
					UD_ncl_ci[i];
			}
			break;
/*
...........Vector, can't measure distand with point or vector
*/
/*		case NCLI_VECTOR:
			for (i=0; i<UD_NMENTWD; i++)
			{
				pick_mask[i] = UD_ncl_ln[i] | UD_ncl_pl[i] | UD_ncl_pv[i] |
					UD_ncl_allcv[i] | UD_ncl_allsfsh[i] | UD_ncl_ci[i];
			}
			break;
*/
/*
...........Point-vector
*/
		case NCLI_POINTVEC:
/*
...........Line
*/
		case NCLI_LINE:
/*
...........Plane
*/
		case NCLI_PLANE:
/*
...........Circle
*/
		case NCLI_CIRCLE:
/*
...........Curve
*/
		case NCLI_CURVE:
/*
...........Surface
*/
		case NCLI_SURF:
			for (i=0; i<UD_NMENTWD; i++)
			{
			pick_mask[i] = UD_ncl_pt[i] | UD_ncl_ln[i] | UD_ncl_pv[i] |
				UD_ncl_pl[i] | UD_ncl_ve[i] | UD_ncl_allcv[i] | UD_ncl_allsfsh[i] |
				UD_ncl_ci[i];
			}
			break;
		}
/*
.....Get second entity
*/
		mask = (int *)pick_mask;
		ud_lgeo(UU_TRUE,mask);
		pr = 654;
		ud_ldas(UD_DASPCKLOC,UA_NCL,pr,&pick2,1,&numint,1,UD_NODEFAULT);
		if (numint > 0)
		{
/*
.....Get picked entity
*/
			um_d_pickresolve(&(pick2.ppath),1,&pent);
			e.key = um_get_pickkey(&pent,1);
			ncl_retrieve_data_fixed(&e);
			Se[1] = e;
			strcpy(label2, pick2.pndc.label);
		}
		else
			goto done;
/*
.....Distance
*/
		stat = ncl_geom_distance(Se,pt,vc, 0, &value,UU_TRUE,UU_TRUE,20);
		if (stat != -1)
		{
			sprintf(tmpstr, "%s, %s", label1, label2);
			strcpy(opn, "Dist");
			strcat (opn, "(");
			strcat (opn, tmpstr);
			strcat (opn, ")");
		}
		else
			goto done;
	}
	else if (flag==3)
	{
/*
.....	Dot(plane1, plane2)
.....		(vector1, vector2)
.....		(ptvector1, ptvector2)
.....		(vector1, ptvector2)
.....		(ptvector1, vector2)
*/
		for (i=0; i<UD_NMENTWD; i++)
		{
			pick_mask[i] = 0;
			pick_mask[i] = pick_mask[i] | UD_ncl_pl[i];
			pick_mask[i] = pick_mask[i] | UD_ncl_ve[i];
			pick_mask[i] = pick_mask[i] | UD_ncl_pv[i];
		}
		status = ncl_add_label_rel(UD_DASPCKLOC, 
			&cmdbuf, 238, pick_mask, &rel);
		if (status == NCL_OKINPUT)
		{
		switch (rel)
			{
/*
.....Dot(plane1, plane2)
*/
			case NCL_PLN_REL:
			case UM_RBSPLSRF_REL:
			case UM_RBSPLCRV_REL:
			case NCL_SURF_REL:
			case NCL_TRIMSF_REL:
				status = ncl_add_label(UD_DASPCKLOC,
					&cmdbuf, 238, UD_ncl_pl);
				break;
			case NCL_VECTOR_REL:
/*
.....Dot(vector1, vector2)
.....		(vector1, ptvector2)
*/
				status = ncl_add_label(UD_DASPCKLOC,
					&cmdbuf, 238, UD_ncl_vepv);
				break;
/*
.....Dot(ptvector1, ptvector2)
.....		(ptvector1, vector2)
*/
			case NCL_POINTVEC_REL:
				status = ncl_add_label(UD_DASPCKLOC, 
					&cmdbuf, 238, UD_ncl_vepv);
				break;
			}
		}
		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
			ncl_del_token(&cmdbuf,"", UU_TRUE);
			strcpy(tmpstr, cmdbuf.cur_str);
			strcpy(opn, "Dot");
			strcat (opn, "(");
			strcat (opn, tmpstr);
			strcat (opn, ")");
			nc = strlen(opn);
			parse_expr (opn, &nc, &value, &stat);
		}
		else
			goto done;
	}
	else if (flag==4)
	{
/*
.....	Lnthf(line)
.....		(vector)
.....		(ptvector)
.....		(circle)
.....		(curve1)
*/
		for (i=0; i<UD_NMENTWD; i++)
		{
			pick_mask[i] = 0;
			pick_mask[i] = pick_mask[i] | UD_ncl_ln[i];
			pick_mask[i] = pick_mask[i] | UD_ncl_ve[i];
			pick_mask[i] = pick_mask[i] | UD_ncl_pv[i];
			pick_mask[i] = pick_mask[i] | UD_ncl_ci[i];
			pick_mask[i] = pick_mask[i] | UD_ncl_allcv[i];
		}
		status = ncl_add_label_rel(UD_DASPCKLOC, 
			&cmdbuf, 238, pick_mask, &rel);
		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
			ncl_del_token(&cmdbuf,"", UU_TRUE);
			strcpy(tmpstr, cmdbuf.cur_str);
			strcpy(opn, "Lnthf");
			strcat (opn, "(");
			strcat (opn, tmpstr);
			strcat (opn, ")");
			nc = strlen(opn);
			parse_expr (opn, &nc, &value, &stat);
		}
		else
			goto done;
	}
	else if (flag==5)
	{
/*
.....Get user selected point vector
*/
		for (i=0; i<UD_NMENTWD; i++)
		{
			pick_mask[i] = UD_ncl_pv[i];
		}
		mask = (int *)pick_mask;
		ud_lgeo(UU_TRUE,mask);
		pr = 240;
		ud_ldas(UD_DASPCKLOC,UA_NCL,pr,&pick1,1,&numint,1,UD_NODEFAULT);
		if (numint == 0) 		
			goto done;
/*
.....Get picked entity
*/
		um_d_pickresolve(&(pick1.ppath),1,&pent);
		e.key = um_get_pickkey(&pent,1);
		ncl_retrieve_data_fixed(&e);
		strcpy(label1, pick1.pndc.label);
		Se[0] = e;
		ncl_get_type(e.rel_num,&etype);
/*
.....Get second entity
*/
		for (i=0; i<UD_NMENTWD; i++)
		{
			pick_mask[i] = UD_ncl_ln[i] | UD_ncl_ci[i] | UD_ncl_pl[i] | 
			UD_ncl_allcv[i] | UD_solid[i] | UD_ncl_revsf[i] | 
			UD_ncl_trimsf[i] | UD_ncl_sf[i] | UD_rbsplsrf[i];
		}
		mask = (int *)pick_mask;
		ud_lgeo(UU_TRUE,mask);
		pr = 654;
		ud_ldas(UD_DASPCKLOC,UA_NCL,pr,&pick2,1,&numint,1,UD_NODEFAULT);
		if (numint > 0)
		{
/*
.....Get picked entity
*/
			um_d_pickresolve(&(pick2.ppath),1,&pent);
			e.key = um_get_pickkey(&pent,1);
			ncl_retrieve_data_fixed(&e);
			Se[1] = e;
			strcpy(label2, pick2.pndc.label);
		}
		else
			goto done;
/*
.....Get Distance Type
*/
		status = UU_FAILURE;
		while (status != UU_SUCCESS)
		{
			status = ncl_popup(NCL_TDISTF, &choice);
			switch(choice)
			{
			case 1:
				typ = 1;
				break;
			case 2:
				typ = 2;
				break;
			case 3:
				typ = -1;
				break;
			case 4:
				typ = -2;
				break;
			case 5:
				typ = 3;
				break;
			default:
				goto done;
			}
		}
/*
.....Near is used so its data is found.
*/
		if (typ == 3)
		{
			for (i=0; i<UD_NMENTWD; i++)
				{
					pick_mask[i] = UD_ncl_pt[i];
				}
				mask = (int *)pick_mask;
				ud_lgeo(UU_TRUE,mask);
				pr = 484;
				ud_ldas(UD_DASPCKLOC,UA_NCL,pr,&pick2,1,&numint,1,UD_NODEFAULT);
				if (numint > 0)
				{
/*
.....Get picked entity
*/				
					um_d_pickresolve(&(pick2.ppath),1,&pent);
					e.key = um_get_pickkey(&pent,1);
					ncl_retrieve_data_fixed(&e);
					nrpt = (struct UM_point_rec *)&e;
					strcpy(label3, pick2.pndc.label);
					typ = 3;
				}
				else
					goto done;
		}
/*
.....Find the desired distance
.......The near point needs to be initialized if no near point was selected
*/
		if (nrpt == UU_NULL) nrpt = (struct UM_point_rec *)&Se[0];
		stat = ncl_geom_tdistance(Se,&typ, nrpt, &value);
		if (stat != -1)
		{
			if (fabs(typ) < 3) sprintf(tmpstr, "%s, %s, %d", label1, label2, typ);
			else sprintf(tmpstr, "%s, %s, %s", label1, label2, label3);
			strcpy(opn, "Tdist");
			strcat (opn, "(");
			strcat (opn, tmpstr);
			strcat (opn, ")");
		}
		else
		{
			sprintf(opn,"");
			value = 0;
			ud_wrerr("No intersection found. Invalid input.");
		}
	}
/*
.....this acts just like we enter a number (such as pie, MR, it will replace the number we
.....just entered, but instead of 'number value', here is function value
*/
	strcpy(dir_val_str, opn);
	if ((pre_opr==EXPO_KEY) || (pre_opr==TRIG_FUNC) || (pre_opr==NUMERIC_FUNC)
		|| (pre_opr==GEOM_FUNC) || (pre_opr==INVERSION_KEY)
		|| (pre_opr==PI_KEY) || (pre_opr==EQUEL_KEY)
		|| (pre_opr==SCALAR_KEY) || (pre_opr==MEMORY_KEY)
		|| (pre_opr==NUMERIC_KEYS) || (pre_opr==PAREN_RIGHTKEY)
		||	(pre_opr==PLUSMIN_KEY) || (pre_opr==TRIG_FUNC_2)
		|| (pre_opr==EXITFORM_KEY))
	{
		if (Sdata_entered != TWO_VALUES)
		{
/*
.....save the Spre_form_str_undo here, because it is not first time enter number
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			for (i=0; i<open_pares+1; i++)
			{
				strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
				strcpy(Sform_str[i], Spre_form_str[i]);
				strcat(Sform_str[i], dir_val_str);
			}
		}
		else
		{
/*
.....save the Spre_form_str_undo here, because it is not first time enter number
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			for (i=0; i<open_pares_2+1; i++)
			{
				strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
				strcpy(Sform_str_2[i], Spre_form_str_2[i]);
				strcat(Sform_str_2[i], dir_val_str);
			}
/*
.....also we need update the main formula too
*/
			strcpy(value_dir_str, Svalue_dir_str_1);
			strcat(value_dir_str, ",");
			strcat(value_dir_str, Sform_str_2[0]);
			for (i=0; i<open_pares+1; i++)
			{
				strcpy(Sform_str[i], Spre_form_str[i]);
				strcat(Sform_str[i], value_dir_str);
			}
		}
	}
	else
	{
/*
.....OPERATION_KEY, EXPO_KEYP, COMMA_KEY, PAREN_LEFTKEY, SIGNED_KEY or -1
*/
		if (Sdata_entered != TWO_VALUES)
		{
/*
.....save the Spre_form_str here, because it is first time enter number
*/
			for (i=0; i<open_pares+1; i++)
			{
				strcpy(Spre_form_str[i], Sform_str[i]);
				strcat(Sform_str[i], dir_val_str);
			}
		}
		else
		{
			for (i=0; i<open_pares_2+1; i++)
			{
				strcpy(Spre_form_str_2[i], Sform_str_2[i]);
				strcat(Sform_str_2[i], dir_val_str);
			}
/*
.....also we need update the main formula too
*/
			strcpy(value_dir_str, Svalue_dir_str_1);
			strcat(value_dir_str, ",");
			strcat(value_dir_str, Sform_str_2[0]);
			for (i=0; i<open_pares+1; i++)
			{
				strcpy(Sform_str[i], Spre_form_str[i]);
				strcat(Sform_str[i], value_dir_str);
			}
		}
	}	
	strcpy (Svalue_str_undo, Svalue_str);
	ncl_val2str (value, val_str);
	strcpy(Svalue_str, val_str);

	if ((pre_opr==OPERATION_KEY) || (pre_opr==EXPO_KEYP) 
			|| (pre_opr==PAREN_LEFTKEY))
/*
.....first time enter number after operation
*/
		Svalue_dir_str_undo[0] = '\0';
	else
		strcpy(Svalue_dir_str_undo, Svalue_dir_str);
	strcpy(Svalue_dir_str, dir_val_str);
	ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
	save_pre_opr = pre_opr;
	pre_opr = GEOM_FUNC;
	Sundo = 0;
	Svalue_key = 1;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
done:;
	ud_dspfrm_vis(Scalculator);
	UD_UNMARK (markval);
}

/*********************************************************************
**    E_FUNCTION :  handle_numeric_func(flag)
**       This function handle the numeric function input
**
**    PARAMETERS
**       INPUT  :
**          flag - numeric function flag
**					1: Abs
**					2: Int
**					3: Nint
**					4: Expf
**					5: Sqrt
**					6: Cbrtf
**					7: Log
**					8: Log10f
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void handle_numeric_func(flag)
int flag;
{
	int i, stat,ff, nc;
	char opn[5], tmpstr[256], value_dir_str[256], tempstr[256];
	UM_real8 value;
	char erms[400];

	handle_formula_window(1, tempstr);

/**************
	if (pre_opr == -1)
	{
		ud_wrerr("No data input yet!");
		return;
	}	
*/
/*
.....it must be a value entered before function
*/
/*
......numeric function is a instant function (instant display value base on the value
......display in result window, so we allow this function even though before is a operation
......function for example "1+2+abs" will execute "1+2+abs(1+2)"
*/
/*	if ((Svalue_dir_str[0]=='\0') && ((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP))) */
	if (Svalue_dir_str[0]=='\0')
	{
		ud_wrerr("Numeric Functions must apply to a value.");
		return;
	}	
	if (if_valid_exp (Svalue_dir_str, &ff)==0)
	{
		ud_wrerr("Numeric function must apply to a value.");
		return;
	}	
	if (flag==1)
		strcpy(opn, "Abs");
	else if (flag==2)
		strcpy(opn, "Int");
	else if (flag==3)
		strcpy(opn, "Nint");
	else if (flag==4)
		strcpy(opn, "Expf");
	else if (flag==5)
		strcpy(opn, "Sqrt");
	else if (flag==6)
		strcpy(opn, "Cbrtf");
	else if (flag==7)
		strcpy(opn, "Log");
	else if (flag==8)
		strcpy(opn, "Log10f");

	strcpy(tmpstr, opn);
	strcat (tmpstr, "(");
	if (Svalue_dir_str[0]=='\0')
		strcat (tmpstr, "0.0");
	else
		strcat (tmpstr, Svalue_dir_str);
	strcat (tmpstr, ")");

	strcpy(tempsav1, Svalue_dir_str_undo); 
	strcpy(tempsav2, Svalue_dir_str); 
	
	strcpy(Svalue_dir_str_undo, Svalue_dir_str);
	strcpy(Svalue_dir_str, tmpstr);

	if ((atof(Svalue_str)<=0.0) && ((flag==7)||(flag==8)))
	{
		sprintf(erms,"\"%s\" is not a valid formula.",Svalue_dir_str);
		ud_wrerr(erms);
		strcpy(Svalue_dir_str_undo, tempsav1); 
		strcpy(Svalue_dir_str, tempsav2);
		return;
	}	
/*
.....Get the result of Svalue_dir_str and display in result window
*/
	nc = strlen(Svalue_dir_str);
	parse_expr (Svalue_dir_str, &nc, &value, &stat);
	if (stat!=-1)
	{
		strcpy (Svalue_str_undo, Svalue_str);
		ncl_val2str (value, Svalue_str);
		ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
	}
	else
	{
		sprintf(erms,"\"%s\" is not a valid formula.",Svalue_dir_str);
		ud_wrerr(erms);
		strcpy(Svalue_dir_str_undo, tempsav1); 
		strcpy(Svalue_dir_str, tempsav2);
		return;
	}
	if (Sdata_entered != TWO_VALUES)
	{
		for (i=0; i<open_pares+1; i++)
		{
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
			if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
/*
.....replace old Svalue_dir_str with new Svalue_dir_str in the end of Sform_str if the
.....previous entered value
*/
				strcpy(Sform_str[i], Spre_form_str[i]);
			else
			{
/*
.....even though it pre key is an operation key, but it could still be a value
.....because when "2-sin" it will become "2-sin(2)", the pre key is '-', not 'sin'
.....which we need set that way in order to undo "2-sin" to "2-".
*/
				if (Svalue_key)
				{
					strcpy(tempstr, Spre_form_str[i]);
				}
				else
				{
/*
.....it is the first time enter number, save Spre_form_str
*/
					strcpy(Spre_form_str[i], Sform_str[i]);
				}
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
/*				strcpy(Spre_form_str_undo[i], Spre_form_str[i]); */
				if (Svalue_key)
				{
					strcpy(Sform_str[i], tempstr);
				}
			}
			strcat(Sform_str[i], Svalue_dir_str);
		}
	}
	else
	{
/*
.....update two value formula
*/
		for (i=0; i<open_pares_2+1; i++)
		{
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
			if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
				strcpy(Sform_str_2[i], Spre_form_str_2[i]);
			else
			{
/*
.....even though it pre key is an operation key, but it could still be a value
.....because when "2-sin" it will become "2-sin(2)", the pre key is '-', not 'sin'
.....which we need set that way in order to undo "2-sin" to "2-".
*/
				if (Svalue_key)
				{
					strcpy(tempstr, Spre_form_str_2[i]);
				}
				else
				{
/*
.....it is the first time enter number, save Spre_form_str
*/
					strcpy(Spre_form_str[i], Sform_str[i]);
				}
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
/*				strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]); */
				if (Svalue_key)
				{
					strcpy(Sform_str_2[i], tempstr);
				}
			}
			strcat(Sform_str_2[i], Svalue_dir_str);
		}
/*
.....also we need update the main formula too
*/
		strcpy(value_dir_str, Svalue_dir_str_1);
		strcat(value_dir_str, ",");
		strcat(value_dir_str, Sform_str_2[0]);
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Sform_str[i], Spre_form_str[i]);
			strcat(Sform_str[i], value_dir_str);
		}
	}
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
	save_pre_opr = pre_opr;
	if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
	{
		pre_opr = NUMERIC_FUNC;
	}
	Sundo = 0;
	Svalue_key = 1;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION :  handle_trig_func(flag)
**       This function handle the TRIG function input
**
**    PARAMETERS
**       INPUT  :
**          flag - numeric function flag
**					1: Sin/Asin
**					2: Cos/Acos
**					3: Tan/Atan
**					4: Atan2
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void handle_trig_func(flag)
int flag;
{
	int i, stat, ff, nc;
	char opn[5], tmpstr[256], value_dir_str[256], tempstr[256];
	UM_real8 value;
	char erms[400];

	handle_formula_window(1, tempstr);
/**************
	if (pre_opr == -1)
	{
		ud_wrerr("No data input yet!");
		return;
	}	
*/
/*
.....it must be a value entered before function
*/
/*
......Trig function is a instant function (instant display value base on the value
......display in result window), so we allow this function even though before is a operation
......function for example "1+2+sin" will execute "1+2+sin(1+2)"
*/
/*	if ((Svalue_dir_str[0]=='\0') && ((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP))) */
	if (Svalue_dir_str[0]=='\0')
	{
		ud_wrerr("Trig function must apply to a value.");
		return;
	}	
/*
.....see if Svalue_dir_str is a valid value or expression
*/
	if (if_valid_exp (Svalue_dir_str, &ff)==0)
	{
		ud_wrerr("Trig function must apply to a value.");
		return;
	}	

	if ((inv_chk==0) && (flag==1))
		strcpy(opn, "Sin");
	else if ((inv_chk==1) && (flag==1))
		strcpy(opn, "Asin");
	else if ((inv_chk==0) && (flag==2))
		strcpy(opn, "Cos");
	else if ((inv_chk==1) && (flag==2))
		strcpy(opn, "Acos");
	else if ((inv_chk==0) && (flag==3))
		strcpy(opn, "Tan");
	else if ((inv_chk==1) && (flag==3))
		strcpy(opn, "Atan");

	strcpy(tmpstr, opn);
	strcat (tmpstr, "(");
	if (Svalue_dir_str[0]=='\0')
		strcat (tmpstr, "0.0");
	else
		strcat (tmpstr, Svalue_dir_str);
	strcat (tmpstr, ")");

	strcpy(tempsav1, Svalue_dir_str_undo); 
	strcpy(tempsav2, Svalue_dir_str); 
		
	strcpy(Svalue_dir_str_undo, Svalue_dir_str);
	strcpy(Svalue_dir_str, tmpstr);
/*
.....Get the result of Svalue_dir_str and display in result window
*/
	if (flag==4)
	{
		if (Sdata_entered != TWO_VALUES)
		{
			ud_wrerr("Atan2 function requires two values!");
			strcpy(Svalue_dir_str_undo, tempsav1); 
			strcpy(Svalue_dir_str, tempsav2);
			return;
		}	
		if (open_pares_2!=0)
		{
			sprintf(erms,"Unmatched parenthesis for second value \"%s\"",Sform_str_2[0]);
			ud_wrerr(erms);
			strcpy(Svalue_dir_str_undo, tempsav1); 
			strcpy(Svalue_dir_str, tempsav2);
			return;
		}	
		strcpy(value_dir_str, Svalue_dir_str_1); 
		strcat(value_dir_str, ",");
		strcat(value_dir_str, Sform_str_2[0]);
		strcpy(tmpstr, "Atan2");
		strcat (tmpstr, "(");
		strcat (tmpstr, value_dir_str);
		strcat (tmpstr, ")");
		strcpy(value_dir_str, tmpstr);
		strcpy(Svalue_dir_str, tmpstr);
	}
	else
		strcpy(value_dir_str, Svalue_dir_str); 
	nc = strlen (tmpstr);
	parse_expr (tmpstr, &nc, &value, &stat);
	if (stat!=-1)
	{
		strcpy (Svalue_str_undo, Svalue_str);
		ncl_val2str (value, Svalue_str);
		ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
		if (flag==4)
		{
			Spre_data_entered = Sdata_entered;
			Sdata_entered = ONE_VALUE;
		}
	}
	else
	{
		sprintf(erms,"\"%s\" is not a valid formula.",tmpstr);
		ud_wrerr(erms);
		strcpy(Svalue_dir_str_undo, tempsav1); 
		strcpy(Svalue_dir_str, tempsav2);
		return;
	}
	if (Sdata_entered != TWO_VALUES)
	{
		for (i=0; i<open_pares+1; i++)
		{
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
			if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
/*
.....replace old Svalue_dir_str with new Svalue_dir_str in the end of Sform_str if the
.....previous entered value
*/
				strcpy(Sform_str[i], Spre_form_str[i]);
			else
			{
/*
.....even though it pre key is an operation key, but it could still be a value
.....because when "2-sin" it will become "2-sin(2)", the pre key is '-', not 'sin'
.....which we need set that way in order to undo "2-sin" to "2-".
*/
				if (Svalue_key)
				{
					strcpy(tempstr, Spre_form_str[i]);
				}
				else
				{
/*
.....it is the first time enter number, save Spre_form_str
*/
					strcpy(Spre_form_str[i], Sform_str[i]);
				}
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
/*				strcpy(Spre_form_str_undo[i], Spre_form_str[i]); */
				if (Svalue_key)
				{
					strcpy(Sform_str[i], tempstr);
				}
			}
			strcat(Sform_str[i], Svalue_dir_str);
		}
	}
	else
	{
/*
.....update two value formula
*/
		for (i=0; i<open_pares_2+1; i++)
		{
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
			if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
				strcpy(Sform_str_2[i], Spre_form_str_2[i]);
			else
			{
/*
.....even though it pre key is an operation key, but it could still be a value
.....because when "2-sin" it will become "2-sin(2)", the pre key is '-', not 'sin'
.....which we need set that way in order to undo "2-sin" to "2-".
*/
				if (Svalue_key)
				{
					strcpy(tempstr, Spre_form_str_2[i]);
				}
				else
				{
/*
.....it is the first time enter number, save Spre_form_str
*/
					strcpy(Spre_form_str[i], Sform_str[i]);
				}
/*
.....save the Spre_form_str_undo here, because it even is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
/*				strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]); */
				if (Svalue_key)
				{
					strcpy(Sform_str_2[i], tempstr);
				}
			}
			strcat(Sform_str_2[i], Svalue_dir_str);
		}
/*
.....also we need update the main formula too
*/
		strcpy(value_dir_str, Svalue_dir_str_1);
		strcat(value_dir_str, ",");
		strcat(value_dir_str, Sform_str_2[0]);
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Sform_str[i], Spre_form_str[i]);
			strcat(Sform_str[i], value_dir_str);
		}
	}
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
	save_pre_opr = pre_opr;
	if ((!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP))) || (Svalue_key==1))
	{
		if (flag==4)
			pre_opr = TRIG_FUNC_2;
		else
			pre_opr = TRIG_FUNC;
	}
	Sundo = 0;
	Svalue_key = 1;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
} 
/*********************************************************************
**    E_FUNCTION     : handle_rightp_key()
**        Handled Pie key input
**    PARAMETERS
**       INPUT  : none
**          
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void handle_pie_key()
{
	int i, form;
	UM_real8 value;
	char val_str[256], value_dir_str[256], tempstr[256];
/*
.....this key will replace any one value displayed in the result window
.....or the second value of the two value
.....with pie value 
*/
	value = 3.14159265358979323846;
	ncl_val2str (value, val_str);

	form  = handle_formula_window(0, tempstr);
	if (form==1)
	{
		strcat (tempstr, val_str);
		ud_dispfrm_update_answer(Scalculator, 4, (int *)tempstr);
		return;
	}
		
	if ((pre_opr==EXPO_KEY) || (pre_opr==TRIG_FUNC) || (pre_opr==NUMERIC_FUNC)
		|| (pre_opr==GEOM_FUNC) || (pre_opr==INVERSION_KEY)
		|| (pre_opr==PI_KEY) || (pre_opr==EQUEL_KEY)
		|| (pre_opr==SCALAR_KEY) || (pre_opr==MEMORY_KEY)
		|| (pre_opr==NUMERIC_KEYS) || (pre_opr==PAREN_RIGHTKEY)
		||	(pre_opr==PLUSMIN_KEY) || (pre_opr==TRIG_FUNC_2)
		|| (pre_opr==EXITFORM_KEY))
	{
		if (Sdata_entered != TWO_VALUES)
		{
/*
.....save the Spre_form_str_undo here, because it is not first time enter number
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			for (i=0; i<open_pares+1; i++)
			{
				strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
				strcpy(Sform_str[i], Spre_form_str[i]);
				strcat(Sform_str[i], val_str);
			}
		}
		else
		{
/*
.....save the Spre_form_str_undo here, because it is not first time enter number
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			for (i=0; i<open_pares_2+1; i++)
			{
				strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
				strcpy(Sform_str_2[i], Spre_form_str_2[i]);
				strcat(Sform_str_2[i], val_str);
			}
/*
.....also we need update the main formula too
*/
			strcpy(value_dir_str, Svalue_dir_str_1);
			strcat(value_dir_str, ",");
			strcat(value_dir_str, Sform_str_2[0]);
			for (i=0; i<open_pares+1; i++)
			{
				strcpy(Sform_str[i], Spre_form_str[i]);
				strcat(Sform_str[i], value_dir_str);
			}
		}
	}
	else
	{
/*
.....OPERATION_KEY, EXPO_KEYP, COMMA_KEY, PAREN_LEFTKEY, SIGNED_KEY or -1
*/
		if (Sdata_entered != TWO_VALUES)
		{
/*
.....save the Spre_form_str here, because it is first time enter number
*/
			for (i=0; i<open_pares+1; i++)
			{
				strcpy(Spre_form_str[i], Sform_str[i]);
				strcat(Sform_str[i], val_str);
			}
		}
		else
		{
			for (i=0; i<open_pares_2+1; i++)
			{
				strcpy(Spre_form_str_2[i], Sform_str_2[i]);
				strcat(Sform_str_2[i], val_str);
			}
/*
.....also we need update the main formula too
*/
			strcpy(value_dir_str, Svalue_dir_str_1);
			strcat(value_dir_str, ",");
			strcat(value_dir_str, Sform_str_2[0]);
			for (i=0; i<open_pares+1; i++)
			{
				strcpy(Sform_str[i], Spre_form_str[i]);
				strcat(Sform_str[i], value_dir_str);
			}
		}
	}	
	strcpy (Svalue_str_undo, Svalue_str);
	strcpy(Svalue_str, val_str);
	if ((pre_opr==OPERATION_KEY) || (pre_opr==EXPO_KEYP) 
			|| (pre_opr==PAREN_LEFTKEY))
/*
.....first time enter number after operation
*/
		Svalue_dir_str_undo[0] = '\0';
	else
		strcpy(Svalue_dir_str_undo, Svalue_dir_str);
	strcpy(Svalue_dir_str, val_str);
	save_pre_opr = pre_opr;
	pre_opr = PI_KEY;
	ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
	Svalue_key = 1;
	Sundo = 0;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
}
/*********************************************************************
**    E_FUNCTION     : handle_num_keypad(key)
**        Handled number keypad input
**    PARAMETERS
**       INPUT  : key: input keypad
**          
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....save Spre_form_str the first time value entered, but not when there is already values (this string never include the currecnt value entered) in this function
.....or other value entered function (such as pie_key, mem_key, even some functions of get immidiate value
.....such as 'sin', cos'...)
.....we save Spre_form_str in other functions other than valued entered, such as 'operation', 'expo_fun'
.....leftp_key, rightp_key
*/
/*
.....may need change Svalue_dir_str/Svalue_dir_str_undo
*/
void handle_num_keypad(key)
char *key;
{
	int i, stat, nc;
	UM_real8 value;
	char *indx, *indx1;
	char val_str[256], value_dir_str[256], test_value_str[256], erms[400];
	int enabled;
	char tempstr[256];
	int form  = 0;

	form = handle_formula_window(0, tempstr);
	if (form==1)
/*
.....don't check anything and redisplay anything, just treat as input
.....a keypad into formula window (the iwndow itself will handle it
*/
	{
		return;
	}
	if ((strcmp (key, "'")==0) || (strcmp (key, "^")==0))
	{
		if ((Snumber_reenter==1) || (Svalue_key==0))
		{
			sprintf(erms,"Degrees and Minutes must be integer.");
			ud_wrerr(erms);
			return;
		}
		strcpy (test_value_str, Senter_value_str);
		indx = (char*)strchr (test_value_str, '.');
		if (indx!=UU_NULL)
		{
			sprintf(erms,"Degrees and Minutes must be integer.");
			ud_wrerr(erms);
			return;
		}
		indx = (char*)strchr (test_value_str, '\'');
		indx1 = (char*)strchr (test_value_str, '^');

		if ((strcmp (key, "'")==0) && ((indx!=UU_NULL) || (indx1!=UU_NULL)))
		{
			strcat (test_value_str, "'");
			sprintf(erms,"Invalid key pressed: \"%s\" is not a valid number.",test_value_str);
			ud_wrerr(erms);
			return;
		}
		if ((strcmp (key, "^")==0) && (indx1!=UU_NULL))
		{
			strcat (test_value_str, "^");
			sprintf(erms,"Invalid key pressed: \"%s\" is not a valid number.",test_value_str);
			ud_wrerr(erms);
			return;
		}
	}
/*
.....if it previous operation is a function or generate a value
.....then result display a number, when we enter a number right now
.....it will replace the old result with this new number
*/
	strcpy(val_str, key);
	if ((pre_opr==EXPO_KEY) || (pre_opr==TRIG_FUNC) || (pre_opr==NUMERIC_FUNC)
		|| (pre_opr==GEOM_FUNC) || (pre_opr==INVERSION_KEY)
		|| (pre_opr==PI_KEY) || (pre_opr==EQUEL_KEY)
		|| (pre_opr==SCALAR_KEY) || (pre_opr==MEMORY_KEY)
		|| (pre_opr==PAREN_RIGHTKEY)
		||	(pre_opr==PLUSMIN_KEY) || (pre_opr==TRIG_FUNC_2)
		|| (pre_opr==EXITFORM_KEY))
	{
		if (Sdata_entered != TWO_VALUES)
		{
/*
.....not save Spre_form_str here
*/
			for (i=0; i<open_pares+1; i++)
			{
/*
.....save the Spre_form_str_undo here, because it is not first time enter number
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
				strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
				strcpy(Sform_str[i], Spre_form_str[i]);
				strcat(Sform_str[i], val_str);
			}
		}
		else
		{
/*
.....save the Spre_form_str_undo here, because it is not first time enter number
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			for (i=0; i<open_pares_2+1; i++)
			{
				strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
				strcpy(Sform_str_2[i], Spre_form_str_2[i]);
				strcat(Sform_str_2[i], val_str);
			}
/*
.....also we need update the main formula too
*/
			strcpy(value_dir_str, Svalue_dir_str_1);
			strcat(value_dir_str, ",");
			strcat(value_dir_str, Sform_str_2[0]);
			for (i=0; i<open_pares+1; i++)
			{
				strcpy(Sform_str[i], Spre_form_str[i]);
				strcat(Sform_str[i], value_dir_str);
			}
		}
	}
	else

	{
/*
.....NUMERIC_KEYS, OPERATION_KEY, EXPO_KEYP, COMMA_KEY, PAREN_LEFTKEY, SIGNED_KEY or -1
*/
		if (((pre_opr==NUMERIC_KEYS) || (pre_opr==SIGNED_KEY)) && (Snumber_reenter==0) 
			&&(Senter_value_str[0]!='\0'))
		{
/*
.....check the new input is valid
*/
			if ((strcmp (key, "'")!=0) && (strcmp (key, "^")!=0))
			{
				strcpy (test_value_str, Svalue_dir_str);
				strcat(test_value_str, key);
				nc = strlen (test_value_str);
				parse_expr (test_value_str, &nc, &value, &stat);
				if (stat==-1)
				{
					sprintf(erms,"Invalid key pressed: \"%s\" is not a valid number.",test_value_str);
					ud_wrerr(erms);
					return;
				}
			}
			if (Sdata_entered != TWO_VALUES)
			{
				for (i=0; i<open_pares+1; i++)
				{
/*
.....not save the Spre_form_str here, just strcat the key input
*/
					strcat(Sform_str[i], val_str);
				}
			}
			else
			{
/*
.....update two value formula
*/
				for (i=0; i<open_pares_2+1; i++)
				{
					strcat(Sform_str_2[i], val_str);
				}
/*
.....also we need update the main formula too
*/
				strcpy(value_dir_str, Svalue_dir_str_1);
				strcat(value_dir_str, ",");
				strcat(value_dir_str, Sform_str_2[0]);
				for (i=0; i<open_pares+1; i++)
				{
					strcpy(Sform_str[i], Spre_form_str[i]);
					strcat(Sform_str[i], value_dir_str);
				}
			}
		}
		else
		{
/*
.....save the Spre_form_str here, because it is first time enter number
*/
			if (Sdata_entered != TWO_VALUES)
			{
/*
.....save the Spre_form_str here, because it is first time enter number
*/
				for (i=0; i<open_pares+1; i++)
				{
					if (pre_opr!=NUMERIC_KEYS)
					{
						strcpy(Spre_form_str[i], Sform_str[i]);
					}
					else
					{
						strcpy(Sform_str[i], Spre_form_str[i]);
					}
					strcat(Sform_str[i], val_str);
				}
			}
			else
			{
				for (i=0; i<open_pares_2+1; i++)
				{
					if (pre_opr!=NUMERIC_KEYS)
					{
						strcpy(Spre_form_str_2[i], Sform_str_2[i]);
					}
					else
					{
						strcpy(Sform_str_2[i], Spre_form_str_2[i]);
					}
					strcat(Sform_str_2[i], val_str);
				}
/*
.....also we need update the main formula too
*/
				strcpy(value_dir_str, Svalue_dir_str_1);
				strcat(value_dir_str, ",");
				strcat(value_dir_str, Sform_str_2[0]);
				for (i=0; i<open_pares+1; i++)
				{
					strcpy(Sform_str[i], Spre_form_str[i]);
					strcat(Sform_str[i], value_dir_str);
				}
			}
		}
	}
	strcpy (Svalue_str_undo, Svalue_str);
	strcpy (Senter_value_str_undo, Senter_value_str);

	if ((pre_opr==OPERATION_KEY) || (pre_opr==EXPO_KEYP) 
			|| (pre_opr==PAREN_LEFTKEY))
/*
.....first time enter number after operation
*/
		Svalue_dir_str_undo[0] = '\0';
	else
		strcpy(Svalue_dir_str_undo, Svalue_dir_str);

	if (((pre_opr==NUMERIC_KEYS) || (pre_opr==SIGNED_KEY)) && (Snumber_reenter==0)
					&&(Senter_value_str[0]!='\0'))
	{
		strcat (Senter_value_str, key);
		strcat(Svalue_dir_str, key);
	}
	else
	{
		strcpy (Senter_value_str, key);
		strcpy(Svalue_dir_str, key);
	}
	enabled = ud_getfrm_traverse_mask(Scalculator, 41);
	if (enabled==0)
	{
		Senter_value_str_undo[0] = '\0';
		save_reset_opr = pre_opr;
	}

	ud_setfrm_traverse_mask(Scalculator, 41, UU_TRUE);
	save_pre_opr = pre_opr;
	pre_opr = NUMERIC_KEYS;

	if (!((strcmp (key, "'")==0) || (strcmp (key, "^")==0)))
	{
		ncl_DMS2Dec_str (Senter_value_str, Svalue_str);
		if (Sdisp_type==1)
		{
			ncl_Dec2DMS_str (Svalue_str, Svalue_str);
		}
	}
	ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);

	Svalue_key = 1;
	Sundo = 0;
	Snumber_reenter = 0;
}
/*********************************************************************
**    E_FUNCTION     : handle_expo_key(flag)
**        Handled Exponent key input
**    PARAMETERS
**       INPUT  : flag: 1: x^2
**						2: x^3
**						3: x^y
**          
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....we are not consider the expo_key as function key now
.....so the result will not show in the result screen
.....unless you push '=' key
*/
void handle_expo_key(flag)
int flag;
{
	int i, stat, len, ff, nc;
	UM_real8 value;
	char erms[400], value_dir_str[256], tempstr[256];
	
	int form  = 0;

	form = handle_formula_window(0, tempstr);
	if (form==1)
/*
.....don't check anything and redisplay anything, just treat as input
.....a keypad into formula window
*/
	{
		if (flag==1)
			strcat (tempstr, "**2");
		else if (flag==2)
			strcat (tempstr, "**3");
		else if (flag==3)
			strcat (tempstr, "**");
		ud_dispfrm_update_answer(Scalculator, 4, (int *)tempstr);
		return;
	}
/**************
	if (pre_opr == -1)
	{
		ud_wrerr("No data input yet!");
		return;
	}	
*/
/*
.....it must be a value entered before x^y function
*/
/*	if ((!((pre_opr==EXPO_KEY) || (pre_opr==TRIG_FUNC) || (pre_opr==NUMERIC_FUNC)
			|| (pre_opr==GEOM_FUNC) || (pre_opr==INVERSION_KEY)
			|| (pre_opr==PI_KEY) || (pre_opr==EQUEL_KEY)
			|| (pre_opr==SCALAR_KEY) || (pre_opr==MEMORY_KEY)
			|| (pre_opr==PAREN_RIGHTKEY) || (pre_opr==NUMERIC_KEYS)
			|| (pre_opr==PLUSMIN_KEY) || (pre_opr==TRIG_FUNC_2))) && (flag==3))
*/
/*	if ((Svalue_dir_str[0]=='\0') && ((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP))) */
	if (Svalue_dir_str[0]=='\0')
	{
		ud_wrerr("An operator must follow a number or scalar.");
		return;
	}	
	if (if_valid_exp (Svalue_dir_str, &ff)==0)
	{
		ud_wrerr("An operator must follow a number or scalar.");
		return;
	}	
/*
.....if it is x^2 and x^3, we use current value display as base x
*/
/*
......x^2 and x^3 function is a instant function (instant display value base on the value
......display in result window), so we allow this function even though before is a operation
......function for example "1+2+^2" will execute "1+2+(1+2)**2"
*/
/*	if ((Svalue_dir_str[0]=='\0') && ((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)) */
/*	if ((Svalue_dir_str[0]=='\0')
				&& ((flag==1) || (flag==2)))
	{
		ud_wrerr("An operator must follow a number or scalar.");
		return;
	}	
*/
	save_pre_opr = pre_opr;
	if ((flag==1) || (flag==2))
	{
/*
.....calculate and display the value of X**2 (X is the value entered before **2, it
.....could be a expression with '(' ')' or a single value, such as 3**2 or (a+b*3)**2
*/
		len = strlen (Svalue_dir_str);
		if (if_valid_exp (Svalue_dir_str, &ff)==0)
		{
			ud_wrerr("An operator must follow a number or scalar.");
			return;
		}
		if (((Svalue_dir_str[0]=='(') && (Svalue_dir_str[len-1]==')')) || (ff==1))
			strcpy(value_dir_str, Svalue_dir_str);
		else
		{
			sprintf (value_dir_str, "(%s)", Svalue_dir_str);
		}
		if (flag==1)
			strcat(value_dir_str, "**2");
		else
			strcat(value_dir_str, "**3");
		nc = strlen(value_dir_str);
		parse_expr (value_dir_str, &nc, &value, &stat);
		if (stat!=-1)
		{
			strcpy (Svalue_str_undo, Svalue_str);
			ncl_val2str (value, Svalue_str);
			ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
		}
		else
		{
			sprintf(erms,"\"%s\" is not a valid formula. internal error",Svalue_dir_str);
			ud_wrerr(erms);
			return;
		}
		strcpy(Svalue_dir_str_undo, Svalue_dir_str);
		strcpy(Svalue_dir_str, value_dir_str);
		if (Sdata_entered != TWO_VALUES)
		{
			for (i=0; i<open_pares+1; i++)
			{
/*
.....save the Spre_form_str_undo here
.....the Spre_form_str_undo should be the same as Spre_form_str
*/
				strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
				if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
/*
.....replace old Svalue_dir_str with new Svalue_dir_str in the end of Sform_str if the
.....previous entered value
*/
					strcpy(Sform_str[i], Spre_form_str[i]);
				else
				{
/*
.....even though it pre key is an operation key, but it could still be a value
.....because when "2-sin" it will become "2-sin(2)", the pre key is '-', not 'sin'
.....which we need set that way in order to undo "2-sin" to "2-".
*/
					if (Svalue_key)
					{
						strcpy(tempstr, Spre_form_str[i]);
					}
					else
					{
/*
.....it is the first time enter number, save Spre_form_str
*/
						strcpy(Spre_form_str[i], Sform_str[i]);
					}
					if (Svalue_key)
					{
						strcpy(Sform_str[i], tempstr);
					}
				}
				strcat(Sform_str[i], Svalue_dir_str);
			}
		}
		else
		{
/*
.....update two value formula
*/
			for (i=0; i<open_pares_2+1; i++)
			{
/*
.....save the Spre_form_str_undo here
.....the Spre_form_str_undo should be the same as Spre_form_str
*/
				strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
				if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
					strcpy(Sform_str_2[i], Spre_form_str_2[i]);
				else
				{
/*
.....even though it pre key is an operation key, but it could still be a value
.....because when "2-sin" it will become "2-sin(2)", the pre key is '-', not 'sin'
.....which we need set that way in order to undo "2-sin" to "2-".
*/
					if (Svalue_key)
					{
						strcpy(tempstr, Spre_form_str_2[i]);
					}
					else
					{
/*
.....it is the first time enter number, save Spre_form_str
*/
						strcpy(Spre_form_str[i], Sform_str[i]);
					}
					if (Svalue_key)
					{
						strcpy(Sform_str_2[i], tempstr);
					}
				}
				strcat(Sform_str_2[i], Svalue_dir_str);
			}
/*
.....also we need update the main formula too
*/
			strcpy(value_dir_str, Svalue_dir_str_1);
			strcat(value_dir_str, ",");
			strcat(value_dir_str, Sform_str_2[0]);
			for (i=0; i<open_pares+1; i++)
			{
				strcpy(Sform_str[i], Spre_form_str[i]);
				strcat(Sform_str[i], value_dir_str);
			}
		}
		if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
		{
			pre_opr = EXPO_KEY;
		}
		Svalue_key = 1;
	}
	else if (flag==3)
	{
/*
......even though the Svalue_str is not change, but because we have operation enter, we still
......need save into Svalue_str_undo because when undo, it is the same
*/
		strcpy(Svalue_str_undo, Svalue_str);
		if (Sdata_entered!=TWO_VALUES)
		{
			for (i=0; i<open_pares+1; i++)
			{
				strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
				strcpy(Spre_form_str[i], Sform_str[i]);
				if ((pre_opr==-1) || (pre_opr==PAREN_LEFTKEY))
					strcat(Sform_str[i], Svalue_dir_str);
				strcat(Sform_str[i], "**");
			}
		}
		else
		{
			for (i=0; i<open_pares_2+1; i++)
			{
				strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
				strcpy(Spre_form_str_2[i], Sform_str_2[i]);
				if ((pre_opr==COMMA_KEY) || (pre_opr==PAREN_LEFTKEY))
					strcat(Sform_str_2[i], Svalue_dir_str);
				strcat(Sform_str_2[i], "**");
			}
		}
		strcpy(Svalue_dir_str_undo, Svalue_dir_str);
		pre_opr = EXPO_KEYP;
		Svalue_key = 0;
	}
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
	Sundo = 0;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
}
/*********************************************************************
**    E_FUNCTION     : handle_operation_key(key)
**        Handled operation key input
**    PARAMETERS
**       INPUT  : key: operation key input
**          
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....when we have ')' or "x^2" or "x^3" or Trig, geom, numeric function (abs, int,...)
.....and "+/-" (plus_milus key), Inversion Key, we will immidialtly calculate the result
.....and display the result in the result window.
.....when we enter  '*', '/', or "**", do nothing of calculate.
.....when we entered the operator key '+', '-', we need update the result of 
.....the latest level of the result. For example:
.....we enter 1+3+2*4+2**4-1=, the result window will be '1', '3', '4' (which is 1+3),
.....'2', '4' (not update when enter '*'), '12' (which is 1+3+2*4)
.....'2', '4', '28' (which is 1+3+2*4+2**4), '1' then '27' (which is final result)
..... followed the Window's 'scientific calculator'
*/

void handle_operation_key(key)
char *key;
{
	int i, stat, nc;
	char val_str[256], value_dir_str[256], tempstr[256];
	UM_real8 value;
	char erms[400];
	int form  = 0;

	form = handle_formula_window(0, tempstr);
	if (form==1)
/*
.....don't check anything and redisplay anything, just treat as input
.....a keypad into formula window (the iwndow itself will handle it)
*/
	{
		return;
	}

	if (((pre_opr == -1) || (pre_opr==COMMA_KEY) || (pre_opr==PAREN_LEFTKEY))
		&& ((strcmp(key, "+")==0) || (strcmp(key, "-")==0)))
	{
		save_pre_opr = pre_opr;
		pre_opr = SIGNED_KEY;
		strcpy(Svalue_str_undo, Svalue_str);
		strcpy(Svalue_dir_str_undo, Svalue_dir_str);
		
		strcpy (Svalue_str, key);
		strcpy(Svalue_dir_str, Svalue_str);
		ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
		strcpy (Senter_value_str_undo, Senter_value_str);
		strcpy (Senter_value_str, key);
	}
	else if (pre_opr == PAREN_LEFTKEY) 
	{
		if ((strcmp(key, "+")!=0) && (strcmp(key, "-")!=0))
		{
			ud_wrerr("Operator cannot follow a left parenthesis!");
			return;
		}
		else
		{
			save_pre_opr = pre_opr;
			pre_opr = SIGNED_KEY;

			strcpy(Svalue_str_undo, Svalue_str);
			strcpy(Svalue_dir_str_undo, Svalue_dir_str);

			strcpy (Svalue_str, key);
			strcpy(Svalue_dir_str, Svalue_str);
			ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
		}
	}	
/*
.....it must be a value entered before function
*/
	else if (!((pre_opr==EXPO_KEY) || (pre_opr==TRIG_FUNC) || (pre_opr==NUMERIC_FUNC)
			|| (pre_opr==GEOM_FUNC) || (pre_opr==INVERSION_KEY)
			|| (pre_opr==PI_KEY) || (pre_opr==EQUEL_KEY)
			|| (pre_opr==SCALAR_KEY) || (pre_opr==MEMORY_KEY)
			|| (pre_opr==PAREN_RIGHTKEY) || (pre_opr==NUMERIC_KEYS)
			|| (pre_opr==PLUSMIN_KEY) || (pre_opr==TRIG_FUNC_2)
			|| (pre_opr==EXITFORM_KEY)
			|| (pre_opr == -1) || (Svalue_key)
			)) 
	{
		ud_wrerr("An operator must follow a number or scalar.");
		return;
	}	
	else
	{
		save_pre_opr = pre_opr;
		pre_opr = OPERATION_KEY;
	}
/*
.....see if we need display the result
*/
	if (pre_opr == OPERATION_KEY)
	{
		if ((strcmp(key, "+")==0) || (strcmp(key, "-")==0))
		{
/*
.....calculate the latest level of the result
*/
/*
.....calculate and display the value
*/
			if (Sdata_entered != TWO_VALUES)
			{
				if (open_pares==0)
					strcpy (val_str, Sform_str[open_pares]);
				else
					strcpy (val_str, &(Sform_str[open_pares][1]));
			}
			else
			{
				if (open_pares_2==0)
					strcpy (val_str, Sform_str_2[open_pares_2]);
				else
					strcpy (val_str, &(Sform_str_2[open_pares_2][1]));
			}
			nc = strlen(val_str);
			parse_expr (val_str, &nc, &value, &stat);
			if (stat!=-1)
			{
				strcpy (Svalue_str_undo, Svalue_str);
				ncl_val2str (value, Svalue_str);
				ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
				strcpy(Svalue_dir_str_undo, Svalue_dir_str);
				strcpy(Svalue_dir_str, val_str);
			}
			else
			{
				sprintf(erms,"\"%s\" is not a valid formula, internal error",Svalue_dir_str);
				ud_wrerr(erms);
				return;
			}
		}
		else if ((strcmp(key, "*")==0) || (strcmp(key, "/")==0))
		{
/*
......even though the Svalue_str is not change, but because we have operation enter, we still
......need save into Svalue_str_undo  and Svalue_dir_str_undo because when undo, it is the same
*/
			strcpy(Svalue_str_undo, Svalue_str);
			strcpy(Svalue_dir_str_undo, Svalue_dir_str);
		}
	}
	if (Sdata_entered != TWO_VALUES)
	{
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
			strcpy(Spre_form_str[i], Sform_str[i]);
			if ((save_pre_opr==-1) || (save_pre_opr==PAREN_LEFTKEY))
				strcat(Sform_str[i], Svalue_dir_str);
			if (pre_opr != SIGNED_KEY)
				strcat(Sform_str[i], key);
		}
	}
	else
	{
/*
.....update two value formula
*/
		for (i=0; i<open_pares_2+1; i++)
		{
			strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
			strcpy(Spre_form_str_2[i], Sform_str_2[i]);
			if ((save_pre_opr==COMMA_KEY) || (save_pre_opr==PAREN_LEFTKEY))
				strcat(Sform_str_2[i], Svalue_dir_str);
			if (pre_opr != SIGNED_KEY)
				strcat(Sform_str_2[i], key);
		}
/*
.....also we need update the main formula too
*/
		strcpy(value_dir_str, Svalue_dir_str_1);
		strcat(value_dir_str, ",");
		strcat(value_dir_str, Sform_str_2[0]);
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Sform_str[i], Spre_form_str[i]);
			strcat(Sform_str[i], value_dir_str);
		}
	}
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
	Sundo = 0;
	Svalue_key = 0;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : handle_minplus_key
**        Handled +/- key input
**    PARAMETERS
**       INPUT  : none
**          
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void handle_minplus_key()
{
	UM_real8 value;
	int i,stat,len, flag, nc;
	char tempstr[256], value_dir_str[256], temp1[256], temp2[256];

	handle_formula_window(1, temp1);

/***************
	if (pre_opr == -1)
	{
		ud_wrerr("No data input yet!");
		return;
	}	
	if (Sdata_entered==TWO_VALUES)
	{
		ud_wrerr("+/- cannot follow a two values");
		return;
	}	
	if (pre_opr == COMMA_KEY)
	{
		ud_wrerr("+/- cannot follow a comma key");
		return;
	}	
*/
	if (pre_opr == PAREN_LEFTKEY)
	{
		ud_wrerr("+/- cannot follow a left parenthesis");
		return;
	}	
/*	if ((Svalue_dir_str[0]=='\0') && ((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP))) */
	if (Svalue_dir_str[0]=='\0')
	{
		ud_wrerr("An operator must follow a number or scalar.");
		return;
	}	
	if (if_valid_exp (Svalue_dir_str, &flag)==0)
	{
		ud_wrerr("An operator must follow a number or scalar.");
		return;
	}			
	if (pre_opr == PLUSMIN_KEY)
	{
/*
.....toggle back to before
*/
		strcpy(temp1, Svalue_dir_str);
		strcpy(temp2, Svalue_dir_str_undo);
		strcpy (Svalue_dir_str, temp2);
		strcpy (Svalue_dir_str_undo, temp1);
	}
	else
	{
		strcpy(Svalue_dir_str_undo, Svalue_dir_str);

		len = strlen (Svalue_dir_str);
		if (len==0)
			return;
/*
.....check if Svalue_dir_str is a valid formula/value
*/
		if (if_valid_exp (Svalue_dir_str, &flag)==0)
		{
			ud_wrerr("An operator must follow a number or scalar.");
			return;
		}
		if (((Svalue_dir_str[0]=='(') && (Svalue_dir_str[len-1]==')')) || (flag==1))
			sprintf(tempstr, "-%s", Svalue_dir_str);
		else
			sprintf(tempstr, "-(%s)", Svalue_dir_str);

		strcpy(Svalue_dir_str, tempstr);
	}
	if (Sdata_entered != TWO_VALUES)
	{
		for (i=0; i<open_pares+1; i++)
		{
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			strcpy(Spre_form_str_undo[i], Spre_form_str[i]);
			if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
/*
.....replace old Svalue_dir_str with new Svalue_dir_str in the end of Sform_str if the
.....previous entered value
*/
				strcpy(Sform_str[i], Spre_form_str[i]);
			else
			{
/*
.....even though it pre key is an operation key, but it could still be a value
.....because when "2-sin" it will become "2-sin(2)", the pre key is '-', not 'sin'
.....which we need set that way in order to undo "2-sin" to "2-".
*/
				if (Svalue_key)
				{
					strcpy(tempstr, Spre_form_str[i]);
				}
				else
				{
/*
.....it is the first time enter number, save Spre_form_str
*/
					strcpy(Spre_form_str[i], Sform_str[i]);
				}
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
/*				strcpy(Spre_form_str_undo[i], Spre_form_str[i]); */
				if (Svalue_key)
				{
					strcpy(Sform_str[i], tempstr);
				}
			}
			strcat(Sform_str[i], Svalue_dir_str);
		}
	}
	else
	{
/*
.....update two value formula
*/
		for (i=0; i<open_pares_2+1; i++)
		{
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
			strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]);
			if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
				strcpy(Sform_str_2[i], Spre_form_str_2[i]);
			else
			{
/*
.....even though it pre key is an operation key, but it could still be a value
.....because when "2-sin" it will become "2-sin(2)", the pre key is '-', not 'sin'
.....which we need set that way in order to undo "2-sin" to "2-".
*/
				if (Svalue_key)
				{
					strcpy(tempstr, Spre_form_str_2[i]);
				}
				else
				{
/*
.....it is the first time enter number, save Spre_form_str
*/
					strcpy(Spre_form_str[i], Sform_str[i]);
				}
/*
.....save the Spre_form_str_undo here, because it is the first time enter number
.....but it are applied 'trig function to a result from previous operation, 
.....so the Spre_form_str_undo should be the same as Spre_form_str
*/
/*				strcpy(Spre_form_str_2_undo[i], Spre_form_str_2[i]); */
				if (Svalue_key)
				{
					strcpy(Sform_str_2[i], tempstr);
				}
			}
			strcat(Sform_str_2[i], Svalue_dir_str);
		}
/*
.....also we need update the main formula too
*/
		strcpy(value_dir_str, Svalue_dir_str_1);
		strcat(value_dir_str, ",");
		strcat(value_dir_str, Sform_str_2[0]);
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Sform_str[i], Spre_form_str[i]);
			strcat(Sform_str[i], value_dir_str);
		}
	}
	ud_dispfrm_update_answer(Scalculator, 4, (int *)Sform_str[0]);
/*
.....get the result
*/
	nc = strlen(Svalue_dir_str);
	parse_expr (Svalue_dir_str, &nc, &value, &stat);	
	strcpy (Svalue_str_undo, Svalue_str);
	ncl_val2str (value, Svalue_str);
	ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);

	save_pre_opr = pre_opr;
	if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
	{
		pre_opr = PLUSMIN_KEY;
	}
	Svalue_key = 1;
	Sundo = 0;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
}
/*********************************************************************
**    S_FUNCTION     :  static OnPrecise(filedno, val, stat)
**       Method called at when  Precise choice box is clicked
**                     
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  OnPrecise(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_real8 value;

	Sval_prec = val->frmint[0]-1;

	if (Sdisp_type==0)
	{
		value = atof (Svalue_str);
		ncl_val2str (value, Svalue_str);
	}
	else
	{
/*
.....data display as DMS, so we need convert differently
*/
		ncl_DMS2DMS_str (Svalue_str, Svalue_str);
	}
	ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);

	if (Sdisp_type==0)
	{
		value = atof (Svalue_str_undo);
		ncl_val2str (value, Svalue_str);
	}
	else
	{
/*
.....data display as DMS, so we need convert differently
*/
		ncl_DMS2DMS_str (Svalue_str_undo, Svalue_str_undo);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static OnDisType(filedno, val, stat)
**       Method called at when "Dec/DMS" choice box is clicked
**                     
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  OnDisType(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{

	if (Sdisp_type==val->frmint[0])
		return(UD_FLDOK);

/*
.....when display type changed, mark it, so whne it enter keypad number again, we need
.....replace current number instead of append the number after it, for example,
.....enter "10.06", then changed "display type from 'DEC' to 'DMS", the result window will show
....."10'3^36", then enter keypad "9", the result window and formula all will display "9", instead of
....."10'3^369".
*/
	Snumber_reenter = 1;
	Sdisp_type = val->frmint[0];

	if (Svalue_key)
	{
		save_pre_opr = pre_opr;
		pre_opr = SCALAR_KEY;
	}
	if (Sdisp_type==0)
	{
/*
.....then the old will be DMS
*/
		ncl_DMS2Dec_str (Svalue_str, Svalue_str);
	}
	else
	{
/*
.....data display as Dec
*/
		ncl_Dec2DMS_str (Svalue_str, Svalue_str);
	}
	ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);

	if (Sdisp_type==0)
	{
/*
.....then the old will be DMS
*/
		ncl_DMS2Dec_str (Svalue_str_undo, Svalue_str_undo);
	}
	else
	{
/*
.....data display as Dec
*/
		ncl_Dec2DMS_str (Svalue_str_undo, Svalue_str_undo);
	}
	return(UD_FLDOK);
}

static int remove_endopr (instr, outstr)
char *instr, *outstr;
{
	int nc;
	nc = strlen(instr)-1;
	if (nc<=0)
		return 0;
	if ((instr[nc]=='+')||(instr[nc]=='-')||(instr[nc]=='/'))
	{
		strcpy(outstr, instr);
		outstr[nc] = '\0';
		return OPERATION_KEY;
	}
	if (instr[nc]=='*')
	{
		strcpy(outstr, instr);
		if (instr[nc-1]=='*')
		{
			nc = nc - 1;
			outstr[nc] = '\0';
			return EXPO_KEYP;
		}
		outstr[nc] = '\0';
		return OPERATION_KEY;
	}
	return 0;
}

/*********************************************************************
**    S_FUNCTION     :  static calc_formula_str(string)
**       Method called at when  hit enter or exit "formula" edit field
**                     
**    PARAMETERS
**       INPUT  : not used
**          fieldno  Field number.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  calc_formula_str(string)
char *string;
{
	char erms[400], valstr[256];
	UM_real8 value;
	int len, stat, endopr, nc;

	strcpy(tempsav1, Spre_form_str[0]); 
	strcpy(tempsav2, Sform_str[0]); 
	if (string!=UU_NULL)
		strcpy(Sform_str[0], string);

	if (string!=UU_NULL)
	{
		len = strlen (Sform_str[0]);
		if (len==0)
			goto done;
		while (len>=0)
		{
			if (Sform_str[0][len]==' ')
				len--;
			else
				break;
		}
		if (len<=0)
			goto done;
		Sform_str[0][len] = '\0';
/*
.....check if it is valid formula and get the result
*/
		strcpy(valstr, Sform_str[0]);
		endopr = 0;
check:;
		nc = strlen(valstr);
		parse_expr (valstr, &nc, &value, &stat);
		if (stat!=-1)
		{
			strcpy (Svalue_str_undo, Svalue_str);
			ncl_val2str (value, Svalue_str);
			ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
			strcpy(Svalue_dir_str_undo, Svalue_dir_str);
			strcpy(Svalue_dir_str, Sform_str[0]);
/*
.....when '=', the Spre_form_str should be '\0' because the whole formula
.....become the Svalue_dir_str and Svalue_dir_str_undo

*/
			strcpy(Spre_form_str_undo[0], Spre_form_str[0]);
			Spre_form_str[0][0] = '\0';
			save_pre_opr = pre_opr;
			if (endopr==0)
			{
				pre_opr = EXITFORM_KEY; 
			}
			else
			{
				pre_opr = endopr;
			}
			open_pares = 0;
			ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares);
			ud_setfrm_traverse_mask(Scalculator, 51, UU_FALSE);
			if (Schcmod)
				ud_setfrm_traverse_mask(Scalculator, 3, UU_TRUE);
			ud_setfrm_traverse_mask(Scalculator, 56, UU_TRUE);
			goto done;
		}
		else
		{
/*
.....if the string end with the operator, such as '+','-','*', '/', '**',
.....remove it and recal the string value
*/
			endopr = remove_endopr (valstr, valstr);
			if (endopr==0)
			{
				sprintf(erms,"\"%s\" is not a valid formula.",Sform_str[0]);
				ud_wrerr(erms);
				strcpy(Spre_form_str[0], tempsav1); 
				strcpy(Sform_str[0], tempsav2); 
				goto bad;
			}
			else
				goto check;
		}
	}
	Svalue_key = 1;
	goto done;
bad:
	return(UD_BADREQ);
done:;
	Sundo = 0;
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static Oncalc_formula_str(filedno, val, stat)
**       Method called at when  hit enter or exit "formula" edit field
**                     
**    PARAMETERS
**       INPUT  : not used
**          fieldno  Field number.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  Oncalc_formula_str(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fldno, frmid;
/*
.....we don't allow the user click other key while in formula window
.....if they click other key, we have to calculate the formula first
.....but we allow them DO "C: clear the formula" and close the form, in this case,
.....just return.
.....also we allow user click 'CE' (undo) which will reset the formula to before
.....they active window or pick 'formula' check box
*/
	ud_get_focusfrm(&frmid, &fldno);
	if ((frmid==Scalculator) && (fldno==43))
		return(UD_FLDOK);

	if ((frmid==Scalculator) && (fldno==42))
	{
		Sundo_formula = 1;
		Sundo = 0;
		return(UD_FLDOK);
	}
/*
.....in other case (except click 'C' button) and user hit return key or exit this field
.....(hit other key), we calculate the formula first and display the result, and consider as 
.....it hit a "=" button
*/
	if (val->frmstr==UU_NULL)
		return (UD_FLDOK);
	if ((frmid==Scalculator) && (fldno!=56))
	{
		return calc_formula_str(val->frmstr);
	}
	pre_opr = EXITFORM_KEY;
	return (UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose()
**      close the calculator form and out put the result.
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
static UD_FSTAT OnClose(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_close_dispfrm(Scalculator);
	SCal_cancel = 0;
	Scalculator = -1;
	*fieldno = -1;
	ud_reset_actform_id();
	return(UD_FRMCLOSE);
}



/*********************************************************************
**    S_FUNCTION     :  static Oncalc_assign(filedno, val, stat)
**       Method called at when  "Assign" button is clicked
**                     
**    PARAMETERS
**       INPUT  : not used
**          fieldno  Field number.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  Oncalc_assign(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char erms[400], tempstr[256], tempstr2[256];
	UM_real8 value;
	int len,flag, nc;
	UD_DDATA data;
/*
.....save the formula first
*/
	data.frmstr = (char*)&tempstr;
	ud_getfrm_field(Scalculator, 4, data,UU_FALSE);
	if (Schcmod==0)
	{
		nc = strlen(tempstr);
		parse_expr (tempstr, &nc, &value, &stat);
		if (stat==-1)
		{
			sprintf(erms,"\"%s\" is not a valid formula.",tempstr);
			ud_wrerr(erms);
			return(UD_BADREQ);
		}
		else
			ncl_val2str (value, tempstr2);
		Sassigned = 1;
		stat = OnClose(fieldno,val,stat);
		strcpy(Sform_str[0], tempstr);
		strcpy(Svalue_str, tempstr2);			
		ncl_update_input(Svalue_str); 
		return(UD_FRMCLOSE);
	}

	if (open_pares!=0)
	{
		sprintf(erms,"\"%s\" have unmatched parenthesis",Sform_str[0]);
		ud_wrerr(erms);
		return(UD_BADREQ);
	}
	if (Sdata_entered == TWO_VALUES)
	{
		ud_wrerr("Assign cannot apply to multiple values!");
		return(UD_BADREQ);
	}
	strcpy(Sform_str[0], tempstr);
	if (Sform_str[0]!='\0')
	{
		strcpy(tempstr, Sform_str[0]);
	}
	else
		tempstr[0] = '\0';
/*
.....remove trailing space
*/
	len = strlen (tempstr);
	if (len==0)
		return(UD_FLDOK);
	while (len>=0)
	{
		if (tempstr[len]==' ')
			len--;
		else
			break;
	}
	if (len<=0)
		return(UD_FLDOK);
	tempstr[len] = '\0';
/*
.....check if it is valid formula and get the result
*/
	nc = strlen(tempstr);
	parse_expr (tempstr, &nc, &value, &stat);
	if (stat!=-1)
	{
		strcpy (Svalue_str_undo, Svalue_str);
		ncl_val2str (value, Svalue_str);
		ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
		strcpy(Svalue_dir_str_undo, Svalue_dir_str);
	}
	else
	{
		sprintf(erms,"\"%s\" is not a valid formula.",tempstr);
		ud_wrerr(erms);
		return(UD_BADREQ);
	}
	strcpy(Svalue_dir_str, Sform_str[0]);
/*
.....if the formula already have double parenthesis, don't add them again
*/
	len = strlen (Sform_str[0]);
/*
.....see if it is single value or expression
*/
	if_valid_exp (Sform_str[0], &flag);
	if (((Sform_str[0][0]=='(') && (Sform_str[0][len-1]==')')) || (flag==1))
		strcpy(tempstr, Sform_str[0]);
	else
		sprintf(tempstr, "(%s)", Sform_str[0]);
	strcpy(Sform_str[0], tempstr);
/*
.....when '=', the Spre_form_str[0] should be '\0' because the whole formula
.....become the Svalue_dir_str, and the old Spre_form_str[0] should saved in pre_form_str_undo[0]
*/
	strcpy(Spre_form_str_undo[0], Spre_form_str[0]);
	Spre_form_str[0][0] = '\0';
/*
......display scalar define form and put the Sform_str into the value field
*/
	ncl_scalar_define(Svalue_dir_str);
	
	save_pre_opr = pre_opr;
	pre_opr = EQUEL_KEY;
	Svalue_key = 1;

	Sundo = 0;
	Svalue_key = 1;

	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  static Oncalc_inv(filedno, val, stat)
**       Method called at when  "inv" check box is clicked
**                     
**    PARAMETERS
**       INPUT  : not used
**          fieldno  Field number.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  Oncalc_inv(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (val->frmint[0]==0)
	{
		ud_dispfrm_update_butlabel(Scalculator, 7, "Sin");
		ud_dispfrm_update_butlabel(Scalculator, 8, "Cos");
		ud_dispfrm_update_butlabel(Scalculator, 9, "Tan");
		ud_dispfrm_update_butlabel(Scalculator, 12, "Dist");
	}
	else
	{
		ud_dispfrm_update_butlabel(Scalculator, 7, "Asin");
		ud_dispfrm_update_butlabel(Scalculator, 8, "Acos");
		ud_dispfrm_update_butlabel(Scalculator, 9, "Atan");
		ud_dispfrm_update_butlabel(Scalculator, 12, "Tdist");
	}
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  static Oncalc_BackSpace(filedno, val, stat)
**       Method called at when  "NS" button is clicked
**                     
**    PARAMETERS
**       INPUT  : not used
**          fieldno  Field number.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  Oncalc_BackSpace(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i, len, vlen;
	char value_dir_str[256];
	UM_real8 value;
	vlen = strlen (Senter_value_str);
	if (vlen<=0)
	{
		ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
		return(UD_FLDOK);
	}
	strcpy (Svalue_str_undo, Svalue_str);
	strcpy(Svalue_dir_str_undo, Svalue_dir_str);
	strcpy(Senter_value_str_undo, Senter_value_str);

	Senter_value_str[vlen-1] = '\0';
		
	len = strlen (Svalue_dir_str);
	Svalue_dir_str[len-1] = '\0';

	if (Sdata_entered != TWO_VALUES)
	{
		for (i=0; i<open_pares+1; i++)
		{
/*
.....not save the Spre_form_str here, just strcat the key input
*/
			len = strlen (Sform_str[i]);
			Sform_str[i][len-1] = '\0';
		}
	}
	else
	{
/*
.....update two value formula
*/
		for (i=0; i<open_pares_2+1; i++)
		{
			len = strlen (Sform_str_2[i]);
			Sform_str_2[i][len-1] = '\0';
		}
/*
.....also we need update the main formula too
*/
		strcpy(value_dir_str, Svalue_dir_str_1);
		strcat(value_dir_str, ",");
		strcat(value_dir_str, Sform_str_2[0]);
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Sform_str[i], Spre_form_str[i]);
			strcat(Sform_str[i], value_dir_str);
		}
	}
/*
.....if back to the last one, value changed to 0
*/
	if (vlen-1<=0)
	{
		ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
		Svalue_key = 0;
	}
	if (Senter_value_str[0]!='\0')
	{
		if (!( (Senter_value_str[vlen-2]=='\'') || (Senter_value_str[vlen-2]=='^') ))
		{
			ncl_DMS2Dec_str (Senter_value_str, Svalue_str);
			if (Sdisp_type==1)
			{
				ncl_Dec2DMS_str (Svalue_str, Svalue_str);
			}
		}
	}
	else
	{
		value = 0.0;
		ncl_val2str (value, Svalue_str);
	}
	ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
	ud_dispfrm_update_answer(Scalculator, 4, (int *)&Sform_str[0]);
	if (Svalue_dir_str[0]=='\0')
		strcpy(Svalue_dir_str, "0.0");
	Sundo = 0;
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  static Oncalc_ClearE(filedno, val, stat)
**       Method called at when  "CE" button is clicked
**                     
**    PARAMETERS
**       INPUT  : not used
**          fieldno  Field number.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  Oncalc_ClearE(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i, len,flag;
	char tempstr[256], value_dir_str[256], temp1[256], temp2[256];
/*
.....only undo once
*/
	if (Sundo)
		return(UD_FLDOK);

	if (Sundo_formula)
/*
.....just reset the result and formula
*/
	{
		ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
		ud_dispfrm_update_answer(Scalculator, 4, (int *)&Sform_str[0]);
		return(UD_FLDOK);
	}
	Sundo_formula = 0;

	if (pre_opr==COMMA_KEY)
	{
		strcpy(Svalue_str, Svalue_str_1);
		strcpy(Svalue_dir_str, Svalue_dir_str_1);
	}
	else if ((pre_opr == PLUSMIN_KEY) || (pre_opr == INVERSION_KEY))
	{
/*
.....toggle back to before
*/
		strcpy(temp1, Svalue_dir_str);
		strcpy(temp2, Svalue_dir_str_undo);
		strcpy (Svalue_dir_str, temp2);
		strcpy (Svalue_dir_str_undo, temp1);
		strcpy(temp1, Svalue_str);
		strcpy(temp2, Svalue_str_undo);
		strcpy (Svalue_str, temp2);
		strcpy (Svalue_str_undo, temp1);
	}
	else
	{
		strcpy(Svalue_str, Svalue_str_undo);
		if (Svalue_dir_str_undo[0]!='\0')
			strcpy(Svalue_dir_str, Svalue_dir_str_undo);
		else
			strcpy (Svalue_dir_str, "0.0");
		strcpy(Senter_value_str, Senter_value_str_undo);
	}
	if ((Sdata_entered != TWO_VALUES) || (pre_opr==COMMA_KEY))
	{
		if (pre_opr==PAREN_LEFTKEY)
		{
			open_pares--;
			ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares);
		}
		if (pre_opr==PAREN_RIGHTKEY)
		{
			open_pares++;
			ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares);
		}
		if (open_pares<=0)
			ud_setfrm_traverse_mask(Scalculator, 51, UU_FALSE);
		else
			ud_setfrm_traverse_mask(Scalculator, 51, UU_TRUE);
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Sform_str[i], Spre_form_str[i]);
			if (!((strlen (Senter_value_str)>0) && (pre_opr==NUMERIC_KEYS)))
				strcpy(Spre_form_str[i], Spre_form_str_undo[i]);
		}
/*
......we need strcat with the undo_value because we need replaced the current value 
......with the undo_value, but if the previous key input is operation, we don't need 
......strcat the undo value because it already have it (no current value input yet).
*/
		if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
		{
			for (i=0; i<open_pares+1; i++)
			{
				if (pre_opr==COMMA_KEY)
				{
					strcat(Sform_str[i], Svalue_dir_str_1);
					Svalue_key = 1;
				}
				else
				{
					if ((pre_opr==EQUEL_KEY) || (pre_opr==PAREN_RIGHTKEY)
						|| (pre_opr==EXITFORM_KEY))
						strcpy(Sform_str[i], Spre_form_str[i]);
					if (pre_opr!=PAREN_LEFTKEY) 
					{
						if ((Svalue_dir_str_undo[0]!='\0') && (strcmp(Svalue_dir_str, "0.0")!=0))
							strcat(Sform_str[i], Svalue_dir_str);
					}
				}
				if (Svalue_dir_str[0]=='\0')
					Svalue_key = 0;
				else
					Svalue_key = 1;
			}
		}
		else
			Svalue_key = 1;
	}
	else
	{
		if (pre_opr==PAREN_LEFTKEY)
		{
			open_pares_2--;
			ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares_2);
		}
		if (pre_opr==PAREN_RIGHTKEY)
		{
			open_pares_2++;
			ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares_2);
		}
		if (open_pares_2<=0)
			ud_setfrm_traverse_mask(Scalculator, 51, UU_FALSE);
		else
			ud_setfrm_traverse_mask(Scalculator, 51, UU_TRUE);
		for (i=0; i<open_pares_2+1; i++)
		{
			strcpy(Sform_str_2[i], Spre_form_str_2[i]);
			strcpy(Spre_form_str_2[i], Spre_form_str_2_undo[i]);
		}
/*
......we need strcat with the undo_value because we need replaced the current value 
......with the undo_value, but if the previous key input is operation, we don't need 
......strcat the undo value because it already have it (no current value input yet).
*/
		if (!((pre_opr == OPERATION_KEY) || (pre_opr == EXPO_KEYP)))
		{
			for (i=0; i<open_pares_2+1; i++)
			{
				if (pre_opr==PAREN_RIGHTKEY)
					strcpy(Sform_str_2[i], Spre_form_str_2[i]);
				if (pre_opr!=PAREN_LEFTKEY)
					strcat(Sform_str_2[i], Svalue_dir_str);
			}
			if (Svalue_dir_str[0]=='\0')
				Svalue_key = 0;
			else
				Svalue_key = 1;
		}
		else
			Svalue_key = 1;
/*
.....also we need update the main formula too
*/
		strcpy(value_dir_str, Svalue_dir_str_1);
		strcat(value_dir_str, ",");
		strcat(value_dir_str, Sform_str_2[0]);
		for (i=0; i<open_pares+1; i++)
		{
			strcpy(Sform_str[i], Spre_form_str[i]);
			strcat(Sform_str[i], value_dir_str);
		}
	}
	ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);

	if ((strlen (Senter_value_str)<=0) && (pre_opr==NUMERIC_KEYS))
	{
		ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
		Svalue_key = 0;
	}
	if ((strlen (Senter_value_str)>0) && (pre_opr==NUMERIC_KEYS))
	{
		ud_setfrm_traverse_mask(Scalculator, 41, UU_TRUE);
		Svalue_key = 1;
	}

	if ((pre_opr == COMMA_KEY)||(pre_opr == TRIG_FUNC_2))
		Sdata_entered = Spre_data_entered;
/*
.....if the number keypad entered before redo operation
.....we need set Snumber_reenter = 1
*/
	if ((pre_opr!=NUMERIC_KEYS) && (save_pre_opr==NUMERIC_KEYS))
		Snumber_reenter = 1;

	if (!((strlen (Senter_value_str)>0) && (pre_opr==NUMERIC_KEYS)))
		pre_opr = save_pre_opr;
/*
.....if after undo, it is EQUEL_KEY entered before,
.....we need reset 
*/
	if ((pre_opr==EQUEL_KEY)	|| (pre_opr==EXITFORM_KEY))
	{
		len = strlen (Sform_str[0]);
		if (len!=0)
		{
/*
.....see if it is single value or expression
*/
			if_valid_exp (Sform_str[0], &flag);
			if (((Sform_str[0][0]=='(') && (Sform_str[0][len-1]==')')) || (flag==1))
				strcpy(tempstr, Sform_str[0]);
			else
				sprintf(tempstr, "(%s)", Sform_str[0]);
			strcpy(Sform_str[0], tempstr);
			strcpy(Spre_form_str_undo[0], Spre_form_str[0]);
			Spre_form_str[0][0] = '\0';
		}
	}
	ud_dispfrm_update_answer(Scalculator, 4, (int *)&Sform_str[0]);
	Sundo = 1;
	if ((open_pares!=0) || (Sdata_entered == TWO_VALUES))
	{
		if (Schcmod)
			ud_setfrm_traverse_mask(Scalculator, 3, UU_FALSE);
		ud_setfrm_traverse_mask(Scalculator, 56, UU_FALSE);
	}
	else
	{
		if (Schcmod)
			ud_setfrm_traverse_mask(Scalculator, 3, UU_TRUE);
		ud_setfrm_traverse_mask(Scalculator, 56, UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnCancel()
**       Method called at when calculator form is canceled/closed without output.
**		the form function will close the window
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
static UD_FSTAT OnCancel(fieldno, val, stat) 
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....User rejected the form
*/
	if (Sassigned==0)
		SCal_cancel = -1;
	Scalculator = -1;
	return(UD_FLDOK);
}


/*********************************************************************
**    S_FUNCTION     :  static Oncalc_Clear(filedno, val, stat)
**       Method called at when  "C" button is clicked
**                     
**    PARAMETERS
**       INPUT  : not used
**          fieldno  Field number.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  Oncalc_Clear(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_real8 value;
	
	Sundo_formula = 0;
	open_pares = 0;
	open_pares_2 = 0;
	Spre_form_str[0][0] = '\0';
	Spre_form_str_undo[0][0] = '\0';
	Sform_str[0][0] = '\0';
	Spre_form_str_2[0][0] = '\0';
	Spre_form_str_2_undo[0][0] = '\0';
	Sform_str_2[0][0] = '\0';
	ud_dispfrm_update_answer(Scalculator, 4, (int *)&Sform_str[0]);
	value = 0.0;
	ncl_val2str (value, Svalue_str);
	ncl_val2str (value, Svalue_str_undo);
/*
	Svalue_dir_str[0] = '\0';
	Svalue_dir_str_1[0] = '\0';
*/
	strcpy (Svalue_dir_str, Svalue_str);
	strcpy (Svalue_dir_str_1, Svalue_str);
	Senter_value_str_undo[0] = '\0';
	Senter_value_str[0] = '\0';
/*
	Svalue_str_1[0] = '\0';
*/
	Svalue_dir_str_undo[0] = '\0';
	strcpy (Svalue_str_1, Svalue_str);

	ud_dispfrm_update_answer(Scalculator, 0, (int *)Svalue_str);
	ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares);
	if (open_pares<=0)
		ud_setfrm_traverse_mask(Scalculator, 51, UU_FALSE);
	else
		ud_setfrm_traverse_mask(Scalculator, 51, UU_TRUE);
	Sdata_entered = ONE_VALUE;
	Spre_data_entered = ONE_VALUE;
	pre_opr = save_pre_opr = save_reset_opr = -1;
	Sundo = 0;
	Svalue_key = 0;
	
	if (Schcmod)
		ud_setfrm_traverse_mask(Scalculator, 3, UU_TRUE);
	ud_setfrm_traverse_mask(Scalculator, 56, UU_TRUE);
	ud_setfrm_traverse_mask(Scalculator, 41, UU_FALSE);
	ud_dispfrm_update_answer(Scalculator, 5, (int *)&open_pares);

	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  static Oncalc_call(filedno, val, stat)
**       Method called at when  the button of the calculator is clicked
**                     
**    PARAMETERS
**       INPUT  : 
**          fieldno  Field number. 
**          val      Current field value. not used
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  Oncalc_call(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	Update_result = 1;
	switch (*fieldno)
	{
/*10*/
	case 7:
		handle_trig_func(1);
		break;
	case 8:
		handle_trig_func(2);
		break;
	case 9:
		handle_trig_func(3);
		break;
	case 10:
		handle_trig_func(4);
		break;
	case 11:
		handle_geom_func(1);
		break;
	case 12:
		handle_geom_func(2);
		break;
	case 13:
		handle_geom_func(3);
		break;
	case 14:
		handle_geom_func(4);
		break;
	case 15:
		handle_numeric_func(1);
		break;
	case 16:
		handle_numeric_func(2);
		break;
	case 17:
		handle_numeric_func(3);
		break;
	case 18:
		handle_numeric_func(4);
		break;
	case 19:
		handle_numeric_func(5);
		break;
	case 20:
		handle_numeric_func(6);
		break;
	case 21:
		handle_numeric_func(7);
		break;
	case 22:
		handle_numeric_func(8);
		break;
	case 23:
		handle_num_keypad("'");
		break;
	case 24:
		handle_num_keypad("^");
		break;
	case 25:
		handle_num_keypad("7");
		break;
	case 26:
		handle_num_keypad("8");
		break;
	case 27:
		handle_num_keypad("9");
		break;
	case 28:
		handle_operation_key("/");
		break;
	case 29:
		handle_num_keypad("4");
		break;
	case 30:
		handle_num_keypad("5");
		break;
	case 31:
		handle_num_keypad("6");
		break;
	case 32:
		handle_operation_key("*");
		break;
	case 33:
		handle_num_keypad("1");
		break;
	case 34:
		handle_num_keypad("2");
		break;
	case 35:
		handle_num_keypad("3");
		break;
	case 36:
		handle_operation_key("-");
		break;
	case 37:
		handle_num_keypad("0");
		break;
	case 38:
		handle_num_keypad(".");
		break;
	case 39:
		handle_minplus_key();
		break;
	case 40:
		handle_operation_key("+");
		break;


	case 44:
		handle_pie_key();
		break;
	case 45:
		handle_expo_key(1);
		break;
	case 46:
		handle_expo_key(2);
		break;
	case 47:
		handle_expo_key(3);
		break;
	case 48:
		handle_inver_key();
		break;
	case 49:
		handle_comma_key();
		break;
	case 50:
		handle_leftp_key();
		break;
	case 51:
		handle_rightp_key();
		break;
	case 52:
		handle_mem_key(1);
		break;
	case 53:
		handle_mem_key(2);
		break;
	case 54:
		handle_mem_key(3);
		break;
	case 55:
		handle_mem_key(4);
		break;
	case 56:
		handle_equel_key();
		break;
	case 57:
		handle_mem_key(5);
		break;
	case 58:
/*
......handle_scalar_key will display form and waiting, but the main form could be
......closed during this function, so we need check. If the main close already,
......then *fieldno = -1; to avoid redisplay main form
*/
		handle_scalar_key();
		if (Scalculator==-1)
			*fieldno = -1;
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : ncl_calculator(outstr)
**       Display a calautor form
**
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          outstr: result/format output string
**					empty if no output/canceled
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_calculator (outstr)
char *outstr;
{
	static int but;
	UM_int2 cmode;
	int frmmode;
	UM_real8 value;
	static char form_str[256];
	static int val_prec, disp_type;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1,1,1,1,1,1,1,1,
								1,1,1,1,1,1,1,1,1,1,
								1,1,1,1,1,1,1,1,1,1,
								1,1,1,1,1,1,1,1,1,1,
								1,1,1,1,1,1,1,1,1,1,
								1,1,1,1,1,1,1,1,1,1,1};
	static UD_METHOD methods[] = {UU_NULL,OnPrecise, OnDisType, Oncalc_assign,Oncalc_formula_str, 
								UU_NULL, Oncalc_inv, 
								Oncalc_call,Oncalc_call,
								Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,
								Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,
								Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,
								Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,
								Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,
								Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,
								Oncalc_call,Oncalc_call,
								Oncalc_BackSpace,Oncalc_ClearE,Oncalc_Clear,
								Oncalc_call,Oncalc_call,Oncalc_call,
								Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,
								Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,Oncalc_call,
								Oncalc_call,Oncalc_call, OnCancel};
	static char called[] = {6,6,6,6,6,6,6,6,6,6,
							6,6,6,6,6,6,6,6,6,6,
							6,6,6,6,6,6,6,6,6,6,
							6,6,6,6,6,6,6,6,6,6,
							6,6,6,6,6,6,6,6,6,6,
							6,6,6,6,6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1,1,1,1,1,1,1,
								1,1,1,1,1,1,1,1,1,1,1,
								1,1,1,1,1,1,1,1,1,1,1,
								1,1,1,1,1,1,1,1,1,1,1,
								1,1,1,1,1,1,1,1,1,1,1,
								1,1,1,1,1};
	static int *ans[] = {(int *)Svalue_str, (int *)&val_prec, (int*) &disp_type, (int *)&but, (int *)form_str,
		(int *)&open_pares, (int *)&inv_chk, &but, &but, &but, 
		&but, &but,  &but, &but, &but, &but, &but, &but, &but, &but,
		&but, &but, &but, &but,  &but, &but, &but, &but, &but, &but, 
		&but, &but, &but, &but,  &but, &but, &but, &but, &but, &but, 
		&but, &but, &but, &but,  &but, &but, &but, &but, &but, &but, 
		&but, &but, &but, &but,  &but, &but, &but, &but, &but, &but};

	val_prec = Sval_prec+1;
	disp_type = Sdisp_type;
	value = 0.0;
	ncl_val2str (value, Svalue_str);
	ncl_val2str (value, Svalue_str_undo);
	Spre_form_str[0][0] = '\0';
	Spre_form_str_undo[0][0] = '\0';
	Sform_str[0][0] = '\0';
	form_str[0] = '\0';
	Spre_form_str_2[0][0] = '\0';
	Spre_form_str_2_undo[0][0] = '\0';
	Sform_str_2[0][0] = '\0';

/***************
	Svalue_dir_str_1[0] = '\0';
	Svalue_str_1[0] = '\0';
*/
	strcpy (Svalue_dir_str_1, Svalue_str);
	strcpy (Svalue_str_1, Svalue_str);
/*
.....this is entered string displayed in result widow
*/
	Senter_value_str_undo[0] = '\0';
	Senter_value_str[0] = '\0';
/*
	Svalue_dir_str[0] = '\0';
*/
	Svalue_dir_str_undo[0] = '\0';
	strcpy (Svalue_dir_str, Svalue_str);

	Sdata_entered = Spre_data_entered = ONE_VALUE;
	pre_opr = -1, save_pre_opr = -1;	

	inv_chk = 0;
	open_pares = 0;
	open_pares_2 = 0;
	Sundo_formula = 0;
	but = 1;
	Sundo = 0;
	traverse[41] = 0;
	cmdmod(&cmode);
	frmmode = uw_if_formopen();
	if ((cmode==0) || (frmmode))
	{
		Schcmod = 0;
	}
	else
	{
/*
.....check if the prompt opened
*/
		if (ud_is_prompt_open())
			Schcmod = 0;
		else
			Schcmod = 1;
	}
/*
......save the active form ID before we display the scalarlist form
......so that we can update it's field
*/
	ud_save_actform_id();
/*
.....Make sure form is not already active
*/
	if (Scalculator!= -1) return;

	Scalculator = ud_form_display1("calculator.frm",ans,ans,methods,called,display,
		traverse);
	if (Scalculator == -1) goto nofrm;
/*
.....Wait for the user to select a view
.....or cancel the form
*/
	SCal_cancel = 0;
	Sassigned = 0;
/*	uw_dispfrm_wait(Scalculator); */
	goto done;
nofrm:
	ud_wrerr("Could not load 'calculator.frm'.");
	SCal_cancel = -1;
done:
/*	ud_reset_actform_id();
	if (SCal_cancel!=-1)
	{
		if (cmode==0)
		{
			strcpy(outstr, Sform_str[0]);
		}
		else if (Schcmod==0)
		{
			if (frmmode)
				ncl_update_input(Svalue_str); 
			else
				strcpy(outstr, Svalue_str);
		}
	}
	else
*/
		outstr[0] = '\0';
}

