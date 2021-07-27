/*********************************************************************
**    NAME         :  nescalar.c
**       CONTAINS: routines to handle NCL scalars
**             ncl_is_valid_label
**             ncl_get_scalar(e, ncl)
**             ncl_put_scalar(e, ncl)
**             int sclvoc (nclkey, iret)
**					ncl_scalar_form
**					ncl_update_input
**					ncl_get_scalar_list
**					ncl_savscalar
**					ncl_get_scalar_class
**					ncl_parse_subnum
**					ncl_getscalar
**					ncl_scalar_define
**					ncl_filter_str2
**
**    COPYRIGHT 1988 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nescalar.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       11/22/17 , 10:45:36
*********************************************************************/
#include <math.h>
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mdcoord.h"
#include "udforms.h"
#include "ulist.h"
#include "gtbl.h"
#include "gdidd.h"

#include "ncl.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclfc.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "nclvx.h"

#if UU_COMP == UU_IRIS || UU_COMP == UU_IRIS4D
#ifdef UU_RS6000
#include <sys/time.h>
#endif
#include <time.h>
#else
#if (UU_COMP != UU_WIN2K)
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

static char Selscalar[256];
static int Sfrm_cancel=0, Ssfrm = -1;
static UD_LIST Dclass_list;
static char Sfilter[65]="*", Efilter[65]="", Sclassstr[65]="All", Eclassstr[65]="None";
static char Sfilter_old[65]="*", Efilter_old[65]="";
static char Slabel[64], Sclassnm[21], Sdescript[64];
static char Svalue_str[256];
extern int UR_active;
static UD_TLIST Scalar_tlist;

static UD_FSTAT OnSelectClass(), OnFilter1(), OnExcludeClass(), 
		OnFilter2(), OnTableCalbacks(), OnApplyScalar(), OnCancelScalar(), 
		OnCloseScalar();
void ncl_update_input(), ncl_update_scalar_frm();
/*
.....select class list and exclude class list
*/
static UD_LIST Sclass_list, Eclass_list;
static UD_TABLEINFO saved_info;
static int name_click = 0;
static int value_click = 0;
static int class_click = 0;
static int modify_click = 0;
static int descript_click = 0;

int ncl_label_cmp(label1, label2)
char *label1, *label2;
{
	int sub1, sub2;
	char name1[65], name2[65];
/*
.....seperate label and subscript if it have
*/
	ncl_parse_label(label1,name1, &sub1);
	ncl_parse_label(label2,name2, &sub2);
	if (strcmp(name1, name2)==0)
	{
		if (sub1>sub2)
			return 1;
		else if (sub1==sub2)
			return 0;
		else
			return (-1);
	}
	return strcmp(name1, name2);
}

/*********************************************************************
**   I_FUNCTION: SortFunc(char* cData1, char* cData2, int lParamSort)
**      Sort the list on Select Scalar form in partical order
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
......"Name" "Class" "Value" "Modified" "Description"
*/
	switch(lParamSort)
	{
	case 0:
		nRetVal = ncl_label_cmp(pData1->data_items[0],
                                 pData2->data_items[0]);
		break;
	case 1:	
		nRetVal = strcmp(pData1->data_items[1],
                                 pData2->data_items[1]);
		break;
	case 2:
/*
.....by value
*/
		val1 = atof (pData1->data_items[2]);
		val2 = atof (pData2->data_items[2]);
		if (val1>val2)
			nRetVal = 1;
		else if (val1==val2)
			nRetVal = 0;
		else
			nRetVal = -1;
		break;
	case 3:
		nRetVal = strcmp(pData1->data_items[3],
                                 pData2->data_items[3]);
		break;
	case 4:	
		nRetVal = strcmp(pData1->data_items[4],
                                 pData2->data_items[4]);
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
......"Name" "Class" "Value" "Modified" "Description"
*/
	switch(lParamSort)
	{
	case 0:
		nRetVal = -ncl_label_cmp(pData1->data_items[0],
                                 pData2->data_items[0]);
		break;
	case 1:	
		nRetVal = -strcmp(pData1->data_items[1],
                                 pData2->data_items[1]);
		break;
	case 2:
/*
.....by value
*/
		val1 = atof (pData1->data_items[2]);
		val2 = atof (pData2->data_items[2]);
		if (val1<val2)
			nRetVal = 1;
		else if (val1==val2)
			nRetVal = 0;
		else
			nRetVal = -1;
		break;
	case 3:
		nRetVal = -strcmp(pData1->data_items[3],
                                 pData2->data_items[3]);
		break;
	case 4:	
		nRetVal = -strcmp(pData1->data_items[4],
                                 pData2->data_items[4]);
		break;
	default:
		break;
	}
	return nRetVal;
}
/*********************************************************************
**   I_FUNCTION: resort_table()
**      resort the list as last appeared after reload/filter on Select Scalar form.
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
	if ((name_click ==0)&&(value_click==0)
		&&(class_click==0)&&(modify_click==0)
		&&(descript_click==0))
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
	if ((info.col==1)&&(class_click%2==0))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc2);
	}
	else if ((info.col==1)&&(class_click%2))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc);
	}
	if ((info.col==2)&&(value_click%2==0))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc2);
	}
	else if ((info.col==2)&&(value_click%2))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc);
	}
	if ((info.col==3)&&(modify_click%2==0))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc2);
	}
	else if ((info.col==3)&&(modify_click%2))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc);
	}
	if ((info.col==4)&&(descript_click%2==0))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc2);
	}
	else if ((info.col==4)&&(descript_click%2))
	{
		ud_form_sorttable(&info, (UD_SMETHOD)SortFunc);
	}
}
/*********************************************************************
**   I_FUNCTION: OnSelectClass(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelectClass(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_TLIST slist;
	if (strcmp(val->frmstr, Eclassstr)==0)
	{
		ud_wrerr("The select class can not be the same as exclude class");
		strcpy (Sclass_list.answer, "None");
		ud_dispfrm_update_answer(Ssfrm, 0, (int *)&Sclass_list);
		return(UD_FLDOK);
	}	
	if (val->frmstr[0]!='\0')
		strcpy(Sclassstr, val->frmstr);
	else
		Sclassstr[0] = '\0';
	ul_to_upper(Efilter);
	ul_to_upper(Sfilter);
	slist.num_item = ncl_get_scalar_tlist(&slist, Sclassstr, Sfilter, Eclassstr, Efilter);
	Selscalar[0] = '\0';
/*
.....copy the list into Scalar_list (because the form input is point to it)
*/
	ud_free_tlist(&Scalar_tlist);
	ud_tlist_copy(&slist, &Scalar_tlist);
	ud_free_tlist(&slist);
	ud_dispfrm_update_answer(Ssfrm, 6, (int *)&Scalar_tlist);
	resort_table();

	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnSelectClass(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnExcludeClass(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_TLIST slist;

	if (strcmp(val->frmstr, Sclassstr)==0)
	{
		ud_wrerr("The exclude class can not be the same as select class");
		strcpy (Eclass_list.answer, "None");
		ud_dispfrm_update_answer(Ssfrm, 3, (int *)&Eclass_list);
		return(UD_FLDOK);
	}
	if (val->frmstr[0]!='\0')
		strcpy(Eclassstr, val->frmstr);
	else
		Eclassstr[0] = '\0';
	ul_to_upper(Efilter);
	ul_to_upper(Sfilter);
	slist.num_item = ncl_get_scalar_tlist(&slist, Sclassstr, Sfilter, Eclassstr, Efilter);
	Selscalar[0] = '\0';
/*
.....copy the list into Scalar_list (because the form input is point to it)
*/
	ud_free_tlist(&Scalar_tlist);
	ud_tlist_copy(&slist, &Scalar_tlist);
	ud_free_tlist(&slist);
	ud_dispfrm_update_answer(Ssfrm, 6, (int *)&Scalar_tlist);
	resort_table();

	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnFilter1(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnFilter1(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_TLIST slist;
	UD_DDATA data;
	char filter[65];
	
	data.frmstr = filter;
	ud_getfrm_field(Ssfrm, 2, data, UU_FALSE);
	strcpy(Sfilter, filter);

	ud_getfrm_field(Ssfrm, 5, data, UU_FALSE);
	strcpy(Efilter, filter);

	ul_to_upper(Sfilter);
	ul_to_upper(Efilter);
	if ((stricmp(Sfilter, Sfilter_old)==0)&&(stricmp(Efilter, Efilter_old)==0))
		return(UD_FLDOK);
	strcpy(Sfilter_old, Sfilter);
	strcpy(Efilter_old, Efilter);

	slist.num_item = ncl_get_scalar_tlist(&slist, Sclassstr, Sfilter, Eclassstr, Efilter);
	Selscalar[0] = '\0';
/*
.....copy the list into Scalar_list (because the form input is point to it)
*/
	ud_free_tlist(&Scalar_tlist);
	ud_tlist_copy(&slist, &Scalar_tlist);
	ud_free_tlist(&slist);
	ud_dispfrm_update_answer(Ssfrm, 6, (int *)&Scalar_tlist);
	resort_table();
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnFilter2(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnFilter2(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_TLIST slist;
	UD_DDATA data;
	char filter[65];
	
	data.frmstr = filter;
	ud_getfrm_field(Ssfrm, 2, data, UU_FALSE);
	strcpy(Sfilter, filter);

	ud_getfrm_field(Ssfrm, 5, data, UU_FALSE);
	strcpy(Efilter, filter);

	ul_to_upper(Sfilter);
	ul_to_upper(Efilter);
	if ((stricmp(Sfilter, Sfilter_old)==0)&&(stricmp(Efilter, Efilter_old)==0))
		return(UD_FLDOK);
	strcpy(Sfilter_old, Sfilter);
	strcpy(Efilter_old, Efilter);

	slist.num_item = ncl_get_scalar_tlist(&slist, Sclassstr, Sfilter, Eclassstr, Efilter);
	Selscalar[0] = '\0';
/*
.....copy the list into Scalar_list (because the form input is point to it)
*/
	ud_free_tlist(&Scalar_tlist);
	ud_tlist_copy(&slist, &Scalar_tlist);
	ud_free_tlist(&slist);
	ud_dispfrm_update_answer(Ssfrm, 6, (int *)&Scalar_tlist);
	resort_table();
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnTableCalbacks(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnTableCalbacks(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_ITEMDATA *data;
	UD_TABLEINFO *info = (UD_TABLEINFO *)(val->frmint);
	if (info->flag==1)
	{
/*
......list selected, doing selection callback
*/
		if (info->row>=0)
		{
			data = (UD_ITEMDATA *)(info->data_ptr);
			strcpy(Selscalar, data->data_items[0]);
		}
		else
			Selscalar[0] = '\0';
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
		if ((info->col==1)&&(class_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			class_click++;
		}
		else if ((info->col==1)&&(class_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			class_click++;
		}
		if ((info->col==2)&&(value_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			value_click++;
		}
		else if ((info->col==2)&&(value_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			value_click++;
		}
		if ((info->col==3)&&(modify_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			modify_click++;
		}
		else if ((info->col==3)&&(modify_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			modify_click++;
		}
		if ((info->col==4)&&(descript_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			descript_click++;
		}
		else if ((info->col==4)&&(descript_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			descript_click++;
		}
	}
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnApplyScalar(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnApplyScalar(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (Scalar_tlist.answer!=-1)
		ncl_update_input(Selscalar);
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnCancelScalar(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnCancelScalar(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....User rejected the form
*/
	Sfrm_cancel = -1;
	ud_close_dispfrm(Ssfrm);
	*fieldno = -1;
	Scalar_tlist.answer = -1;
	Ssfrm = -1;
	return(UD_FRMCLOSE);
}

/*********************************************************************
**    E_FUNCTION     : ncl_is_valid_label(label)
**       Check if the input string is valid scalar label
**    PARAMETERS   
**       INPUT  : 
**          label:	label to be checked
**       OUTPUT :  
**          none
**    RETURNS      : 
**       1: valid
**       0: not valid
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_is_valid_label(label)
char *label;
{
	int i, flag,len,ret;
	char *indx;
	UU_REAL	scalar_value;

	UU_REAL tmp;
	char label1[256], substr[256], erms[256];
/*
.....Check for blank string
*/
	len = strlen (label);
	ul_strip_blanks(label,&len);
	if (len == 0)
	{
		ud_wrerr("Blank label entered.");
		return 0;
	}
/*
.....check if the label is a number
*/
	if (ul_to_reals(&tmp, &i, 1, label)==0)
	{
		sprintf(erms, "\"%s\" is not a valid label, it can't be a value",label);
		ud_wrerr(erms);
		return 0;
	}
/*
.....check if the label is WORD and not allow to be used as scalar
*/
	len = strlen (label);
	isvocwd(label, &len, &flag);
	if (flag==1)
	{
		ud_wrerr("A vocabulary word cannot be used for label");
		return 0;
	}
/*
.....check if this label is less/equel to 63 chars + subscript number
*/
	strcpy(label1, label);
	substr[0] = '\0';
	indx = (char*)strchr(label1, '(');
	if (indx!=0)
	{
		*indx = '\0';
		if (*(indx+1)!='\0')
		{
			strcpy(substr, indx+1);
			indx = (char*)strchr(substr, ')');
			if ((indx!=0)&&(*(indx+1)=='\0'))
			{
				*indx = '\0';
			}
			else
			{
				sprintf(erms,"\"%s\" is not a valid label",label);
				ud_wrerr(erms);
				return 0;
			}
		}
		else
		{
			sprintf(erms,"\"%s\" is not a valid label",label);
			ud_wrerr(erms);
			return 0;
		}
	}
	if (strlen(label1)>63)
	{
		sprintf(erms,"\"%s\" is not a valid label, cannot exceed 63 characters",
			label);
		ud_wrerr(erms);
		return 0;
	}
/*
.....check substr is a value subnum
*/
	if (substr[0]=='\0')
		return 1;
	ret = ncl_get_scalar_value(substr, &scalar_value);
/*
.....support subnum to 1000000
*/
	if ((ret==-1)||scalar_value<=0||(scalar_value>1000000))
	{
		sprintf(erms,"\"%s\" is not a valid label, the subscript is out of range",
			label);
		ud_wrerr(erms);
		return 0;
	}
	return 1;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_scalar(e, ncl)
**       Convert a UNIBASEe scalar to an NCLI  scalar.
**    PARAMETERS   
**       INPUT  : 
**          e                 UNIBASE scalar entity
**       OUTPUT :  
**          ncl               buffer to place NCL scalar
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_scalar(e, ncl)
struct NCL_scalar_rec *e;
struct NCLI_scalar_rec *ncl;
	{
	int status;

	uu_denter(UU_MTRC,(us,"ncl_get_scalar(key=%x, ncl=%x)",
	  e->key, ncl));

	status = UU_SUCCESS;
	ncl_uureal_to_real8(1, &e->scalar_value, &ncl->scalar_value);
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_put_scalar(ncl, e)
**       Convert an NCL scalar to a UNICAD scalar.
**    PARAMETERS   
**       INPUT  : 
**          ncl                  buffer holding NCL scalar
**       OUTPUT :  
**          e                    UNICAD scalar entity
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_put_scalar(ncl, e)
struct NCLI_scalar_rec *ncl;
struct NCL_scalar_rec *e;

{
	int status;

	uu_denter(UU_MTRC,(us,"ncl_put_scalar(ncl=%x, e=%x)",
		ncl, e));
	status = UU_SUCCESS;
	e->rel_num = NCL_SCALAR_REL;
	if (e->key != 0)
		status = ur_retrieve_data(e, sizeof(struct NCL_scalar_rec));
	ncl_real8_to_uureal(1, &ncl->scalar_value, &e->scalar_value);
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int sclvoc (nclkey, iret)
**       Determine if a scalar is holding a vocabulary word.
**    PARAMETERS   
**       INPUT  : 
**          nclkey        - Key of scalar.
**       OUTPUT :  
**          iret          - 1 iff scalar is vocab word, 0 otherwise
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
sclvoc (nclkey, iret)
UM_int4 *nclkey;
UM_int2 *iret;
   {
   int status;
   struct NCL_scalar_rec sclr;

   *iret = 0;
   if (*nclkey==0) return 0;
   sclr.key = *nclkey;
   status = ur_retrieve_data_relnum(sclr.key, &sclr.rel_num);
   if (status == UU_SUCCESS && sclr.rel_num == NCL_SCALAR_REL)
     {
     status = ur_retrieve_data_fixed (&sclr);
     if (status == UU_SUCCESS && sclr.subscr < 0) *iret = 1;
     }
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_savscalar(scal, len1, classnm, len2, descript, len3)
**       save a scalar class/description into a scalar entity
**		
**    PARAMETERS   
**       INPUT  : 
**          scal: scalar name
**			len1: length of scalar label
**			classnm: class of scalar to be saved
**			len2: length of scalar class name
**			descript: decription of scalar to be saved
**			len3: length of scalar description
**       OUTPUT :  
**          none
**    RETURNS      : 
**       0: save scalar descripttion successfully
**       -1: failed (no scalar entity found)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_savscalar(scal, len1, classnm, len2, descript, len3)
UM_int2 *len1, *len2, *len3;
char *scal, *classnm, *descript;
{
	struct NCL_scalar_rec e;
	char label[65];
	UM_f77_str ncllabel;
	UM_int4 nclsubscript;
	UM_int4 ipg,iel,nclkey;
	void ncl_parse_subnum ();
	int status, ifnd;
	UM_int2 nwds,ietype;
	int len_i1, len_i2, len_i3;
	time_t todaytime;

	len_i1 = *len1;
	len_i2 = *len2;
	len_i3 = *len3;
	if (len_i1==0)
		return -1;

	scal[len_i1] = '\0';
	classnm[len_i2] = '\0';
	descript[len_i3] = '\0';
	strcpy(label, scal);
	ul_to_upper(label);
/*
.....seperate the input label into label and subnumber
*/
	ncl_parse_subnum (label, &nclsubscript);
	UM_init_f77_str(ncllabel, label, NCL_MAX_LABEL);
	ifnd = vxchk(UM_addr_of_f77_str(ncllabel), &nclsubscript, &nclkey, &ipg,
			&iel,&nwds,&ietype);
	
	if ((ifnd != UU_SUCCESS) || (nclkey<=0))
		return UU_FAILURE;

	e.rel_num = NCL_SCALAR_REL;
	e.key = nclkey;
	status = ur_retrieve_data(&e, sizeof(struct NCL_scalar_rec));
	if (len_i3!=0)
		strcpy(e.descript, descript);
	if (len_i2!=0)
		strcpy(e.classnm, classnm);
/*
......save the modified time too
*/
#ifndef UU_RS6000
	time(&todaytime);
#else
	ftime(&todaytime);
#endif
	e.modified = (double)todaytime;
	ur_update_data(&e);
	ncl_update_scalar_frm();
	return 0;
}

/*********************************************************************
**    E_FUNCTION         :  ncl_get_scalar_class(number, flag)
**       get all the scalar class list
**
**    PARAMETERS   
**       INPUT  :
**				flag: 1. normal unibase
**					2. secondary unibase
**       OUTPUT : number: number of scalar name 
**    RETURNS      : a list of scalar class name
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char **ncl_get_scalar_class(number, flag)
int *number, flag;
{
	int i, status, entnum, len, add;
	char **scalar_class, classnm[21];
	struct NCL_scalar_rec e;
	int switched = 0;

	status = 0;
	entnum = 0;
	*number = 0;
	len = 0;
	scalar_class = NULL;
	if (flag==1)
	{
		if (UR_active==2)
		{
			ur_getu_work();
			switched = 1;
		}
	}
	else
	{
		if (UR_active==1)
		{
			ur_getu_second();
			switched = 1;
		}
	}
	e.rel_num = NCL_SCALAR_REL;

	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(e.rel_num, &entnum,
								&e.key);
		if (status == 0)
			len++;
	}
	if (len==0)
		goto done;

	scalar_class = (char **) uu_malloc(len *sizeof(char *));

	status = 0;
	entnum = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(e.rel_num, &entnum,
								&e.key);
		if (status == 0)
		{
			ur_retrieve_data(&e, sizeof(e));
/*
.....remove trailing spaces
*/
			strcpy(classnm, e.classnm);
			i = strlen(classnm);
			while ((i!=0) &&(classnm[i-1]==' ')) i--;
				classnm[i] = '\0';
/*
.....add classnm into list
.....first check if it's there
*/
			add = 1;
			for (i=0; i<*number; i++)
			{
				if (strcmp(scalar_class[i],classnm)==0)
				{
					add = 0;
					break;
				}
			}
			if (add==1)
			{
				len = strlen (classnm);
				scalar_class[*number] = (char *) uu_malloc(21 * sizeof(char));	
				strcpy(scalar_class[*number], classnm);
				(*number)++;
			}
		}
	}
	if (switched==1)
	{
		if (flag==1)
		{
			ur_getu_second();
		}
		else
		{
			ur_getu_work();
		}
	}
done:;
	if ((*number==0)&&(scalar_class!=NULL))
	{
		uu_free(scalar_class);
		return NULL;
	}
	return (scalar_class);
}

/*********************************************************************
**    E_FUNCTION         :  ncl_get_scalar_list(number, classnm)
**       get all the scalar name, vlaue & descriptions list
**
**    PARAMETERS   
**       INPUT  : classnm: if classnm is empty, then return all
**       OUTPUT : number: number of scalar name 
**    RETURNS      : a list of scalar name
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char **ncl_get_scalar_list(number, classnm)
int *number;
char *classnm;
{
	int i, status, entnum, len;
	char **scalar_list, descript[65], label[65];
	struct NCL_scalar_rec e;
	char temp_str[256], val_str[80];
	short snc;
	status = 0;
	entnum = 0;
	*number = 0;	
	len = 0;

	e.rel_num = NCL_SCALAR_REL;

	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(e.rel_num, &entnum,
								&e.key);
		if (status == 0)
			len++;
	}
	if (len==0)
		return NULL;
	scalar_list = (char **) uu_malloc(len *sizeof(char *));

	status = 0;
	entnum = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(e.rel_num, &entnum,
								&e.key);
/*
.....label use up to 20 chars spaces at least, if more than 20chars, then use longer spaces
.....value use the same character spaces as 'label' also
.....between 'label', 'value' and 'decription', using 8 character to seperate.
*/
		if (status == 0)
		{
			ur_retrieve_data(&e, sizeof(e));
			if ((classnm==NULL)||(classnm[0]=='\0')
				||(strcmp(classnm, e.classnm)==0)
				||(strcmp(classnm, "All")==0))
			{
/*
.....remove trailing spaces
*/
				strcpy(descript, e.descript);
				i = strlen(descript);
				if (i>0)
				{
					while ((i!=0) &&(descript[i-1]==' ')) i--;
						descript[i] = '\0';
				}
				strncpy(label, e.label, NCL_MAX_LABEL);
				label[63] = '\0';
				i = strlen(label);
				if (i>0)
				{
					while ((i!=0) &&(label[i-1]==' ')) i--;
						label[i] = '\0';
				}
/*
.....seem like when trace label inside a macro, it will
.....have a label define as "@UN" which are not defined by users.
*/
				if (strcmp(label, "@UN")==0)
					continue;
				snc = (short) strlen (label);
				rmlab_prefix(label, label, &snc);
				if (e.subscr>0)
					sprintf (label, "%s(%d)", label, e.subscr);
				len = strlen(label);
				for (i=len; i<20;i++)
					label[i] = ' ';
				if (len<20) label[20] = '\0';
				strcpy(temp_str, label);
				strcat (temp_str, "\t");

				ncl_sprintf(val_str, &(e.scalar_value), 1);	
				len = strlen(val_str);
				for (i=len; i<20;i++)
					val_str[i] = ' ';
				if (len<20) val_str[20] = '\0';
				strcat(temp_str, val_str);
				strcat (temp_str, "\t");
				strcat(temp_str, descript);

				len = strlen(temp_str);
				scalar_list[*number] = (char *) uu_malloc(21 * sizeof(char));	
				strcpy(scalar_list[*number], temp_str);
				(*number)++;
			}
		}
	}
	if (*number==0)
	{
		uu_free(scalar_list);
		return NULL;
	}
	return (scalar_list);
}
/*********************************************************************
**   I_FUNCTION: OnCloseScalar()
**      Callback function for the Close button on Scalar Selection form..
**   PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnCloseScalar()
{
	if ((Sfrm_cancel!=-1) && (Selscalar[0] != '\0'))
	{
		ncl_update_input(Selscalar);
	}
/*
.....Mark the form as closed
*/
	Ssfrm = -1;
/*
.....free the memory
*/
	ud_free_flist(&Sclass_list);
	ud_free_flist(&Eclass_list);
	ud_free_tlist(&Scalar_tlist);
	ud_reset_actform_id();
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : ncl_update_input(text)
**       Update the active input prompt (it could be
**		the input of command prompt or active form field if any of them is active).
**		
**    PARAMETERS   
**       INPUT  : text: text to be updated
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_update_input(text)
char *text;
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_UPDATE_INPUT])(text);
}
/*********************************************************************
**    E_FUNCTION     : ncl_parse_subnum (string, subscript)
**       seperate the label into string label and sub number
**		if the string is not in format "xxx(num)" then, return
**		the string as it is and subnum = 0
**    PARAMETERS   
**       INPUT  : 
**          string: label with/without the number
**       OUTPUT :  
**          string: label without the number
**          subscript:	sub number of the string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_parse_subnum (string, subscript)
char *string;
int	*subscript;
{
	int status, len, label_end, sub_end, i, j, not_num;
	char label[65], substr[20];

	status = 1;
	*subscript = 0;
/*
.....get rid of preceeding space
*/
	len = strlen (string);
	i = 0;
	while ((string[i]==' ') && (i<len)) i++;
	strcpy (string, &(string[i]));
	i = strlen (string);
	if (i==0)
		return;
/*
.....get rid of trailing space
*/  
	while ((string[i-1]==' ') && (i>0)) i--;
	string[i] = '\0';
	len = i;

	for (i=0, j=0, label_end = 0, sub_end = 0, not_num=0; i<len;i++)
	{
		if (isalpha(string[i]) || (string[i]=='#') || (string[i]=='_'))
/*
.....label string
*/
		{
			not_num = 1;
		}
		else if ((string[i]=='(') && (label_end==0))
		{
			strncpy(label, string, i);
			label[i] = '\0';
			label_end = 1;
			not_num = 1;
			continue;
		}
		else if ((string[i]==')') && (sub_end==0))
		{
			substr[j] = '\0';
			sub_end = 1;
			not_num = 1;
			continue;
		}
		else if ((isdigit(string[i])==0)&&(string[i]!='.')
			&&(string[i]!='+') && (string[i]!='-'))
		{
			return;
		}
		if (label_end==0)
			label[i] = string[i];
		else if (sub_end==0)
			substr[j++] = string[i];
		else
			return;
	}

	if (not_num==0)
	{
		return;
	}
	if (sub_end)
		*subscript = atoi(substr);
	else
		*subscript = 0;
	if ((i>0) && (label_end==0))
		label[i] = '\0';

	strcpy(string, label);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_getscalar(scal, len1, classnm, len2,
**                                       descript, len3, value)
**       get a scalar class/description from a scalar string
**		
**    PARAMETERS   
**       INPUT  : 
**          scal: scalar name
**			len1: length of scalar label
**       OUTPUT :  
**			classnm: class of scalar
**			len2: length of scalar class name
**			descript: decription of scalar
**			len3: length of scalar description
**			value: scalar value
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_getscalar(scal, len1, classnm, len2, descript, len3, value)
int *len1, *len2, *len3;
char *scal, *classnm, *descript;
UU_REAL *value;
{
	struct NCL_scalar_rec e;
	char label[65];
	UM_f77_str ncllabel;
	UM_int4 nclsubscript;
	UM_int4 ipg,iel,nclkey;
	void ncl_parse_subnum ();
	int status, ifnd;
	UM_int2 nwds,ietype;

	scal[*len1] = '\0';
	*len2 = 0;
	*len3 = 0;
	strcpy(label, scal);
	ul_to_upper(label);
/*
.....seperate the input label into label and subnumber
*/
	ncl_parse_subnum (label, &nclsubscript);
	UM_init_f77_str(ncllabel, label, NCL_MAX_LABEL);
	ifnd = vxchk(UM_addr_of_f77_str(ncllabel), &nclsubscript, &nclkey, &ipg,
			&iel,&nwds,&ietype);
	
	if ((ifnd != UU_SUCCESS) || (nclkey<=0))
		return -1;

	e.rel_num = NCL_SCALAR_REL;
	e.key = nclkey;
	status = ur_retrieve_data(&e, sizeof(struct NCL_scalar_rec));
	if (status!=-1)
	{
		strcpy(descript, e.descript);
		strcpy(classnm, e.classnm);
		*len2 = strlen(classnm);
		*len3 = strlen(descript);
		*value = e.scalar_value;
		return 0;
	}
	return -1;
}

/*********************************************************************
**   I_FUNCTION: OnScalarValue(fieldno,val,stat)
**      Callback function for 'Value' field from the Define Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnScalarValue(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char value_str[256];
	char erms[256];
	UM_real8 value;
	int nc;

	if (val->frmstr[0]=='\0')
	{
		*fieldno = -1;
		return UD_FLDOK;
	}
	strcpy(value_str, val->frmstr);
	nc = strlen(value_str);
	parse_expr (value_str, &nc, &value, &stat);
	if (stat!=-1)
	{
/*
.....check if the value string is the valid expression
*/
		ud_dispfrm_update_answer(0, 2, (int *)value_str);
		return(UD_FLDOK);
	}
	else
	{
		sprintf(erms,"\"%s\" is not a valid scalar value",value_str);
		ud_wrerr(erms);
		return(UD_BADREQ);
	}
}

/*********************************************************************
**   I_FUNCTION: OnDefineDesc(fieldno,val,stat)
**      Callback function for 'description' field from the Define Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnDefineDesc(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int len;
	char erms[256];

	if (val->frmstr[0]=='\0')
	{
		*fieldno = -1;
		return UD_FLDOK;
	}
	len = strlen(val->frmstr);
/*
.....check if this description is less/equel to 64 chars
.....actually, it only allow 63 chars because we need '\0' end charater
*/
	if (len>63)
	{
		sprintf(erms, "Scalar description exceeds 63 characters. Please shorten it");
		ud_wrerr(erms);
		return(UD_BADREQ);
	}
	*fieldno = -1;
	return UD_FLDOK;
}
/*********************************************************************
**   I_FUNCTION: OnDefineLabel(fieldno,val,stat)
**      Callback function for 'Label' field from the Define Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnDefineLabel(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char tempstr[256], tempstr2[256], descript[65], classnm[21];
	UU_REAL value;
	int len1, len2, len3, status;
	UD_DDATA data;

	if (val->frmstr[0]=='\0')
	{
		*fieldno = -1;
		return UD_FLDOK;
	}

	strcpy (tempstr, val->frmstr);
	if (ncl_is_valid_label(tempstr)==0)
		return UD_BADREQ;

	strcpy(Slabel, tempstr);
/*
.....changes made to auto-update the scalar value, class and description
.....now we only auto-update when the field is empty (user may click other field:
.....form field lost focus (run callback function here) but still want to keep 
.....their input value, if auto-update, the input value will lost)
.....Yurong
*/
	data.frmstr = (char*)&tempstr2;
	ud_getfrm_field(0, 2, data, UU_FALSE);
	if (tempstr2[0]=='\0')
	{
/*
.....Get the scalar value, class and description updated
*/
		len1 = strlen (tempstr);
		status = ncl_getscalar(tempstr, &len1, classnm, &len2, descript, &len3, &value);
		if (status!=-1)
		{
			ncl_sprintf(Svalue_str, &value, 1);	
			ud_dispfrm_update_answer(0, 2, (int *)Svalue_str);
			strcpy(Dclass_list.answer, classnm);
			ud_dispfrm_update_answer(0, 1, (int *)&Dclass_list);
			ud_dispfrm_update_answer(0, 3, (int *)descript);
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnSelectDClass(fieldno,val,stat)
**      Callback function for a list Selection from the Define Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelectDClass(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char erms[256];
	if (strlen(val->frmstr)>20)
	{
		sprintf(erms,"\"%s\" is not a valid scalar class, can't excess 20 characters",val->frmstr);
		ud_wrerr(erms);
		return(UD_BADREQ);
	}
	strncpy(Sclassnm, val->frmstr, 20);
	Sclassnm[20] = '\0';
	strncpy(Dclass_list.answer, Sclassnm, 20);
	Dclass_list.answer[20] = '\0';
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnScalarDef(fieldno,val,stat)
**      Callback function for a list Selection from the Define Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnScalarDef(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_LIST sclass_list2;
	NCL_cmdbuf cmdbuf;
	UM_int2 ifl, ival;
	char lbuf[80], msg[256];
	int i, j;
	int markval=0;
/*
.....define scalar
*/
	if (Slabel[0]=='\0')
	{
		*fieldno = -1;
		return UD_FLDOK;
	}
/*
.....Command Reject
*/
	UD_MARK (markval,UU_TRUE);
	if (markval != 0)
	{
		*fieldno = -1;
		goto done;
	}
	ncl_set_cmdmode(UU_TRUE);
	ncl_init_cmdbuf(&cmdbuf);
	sprintf(lbuf,"%s=%s", Slabel, Svalue_str);	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....check if ncl_call hits error
*/
	ifl = 275;
	ival = 0;
	getifl(&ifl, &ival);
	if (ival!=0)
		goto done;
	if (Dclass_list.answer[0]!='\0')
		strcpy(Sclassnm, Dclass_list.answer);
	if ((Sclassnm[0]=='\0') && (Sdescript[0]=='\0'))
	{
		goto define_done;
	}
	if ((strcmp(Sclassnm, "Default")==0) && (Sdescript[0]=='\0'))
	{
		goto define_done;
	}
	ncl_init_cmdbuf(&cmdbuf);
	sprintf(lbuf,"prompt/scalar, %s, \"%s\"", 
		Slabel, Sclassnm);	
	ncl_add_token(&cmdbuf, lbuf, NCL_comma);		
	sprintf(lbuf,"\"%s\"", Sdescript);	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....check if ncl_call hits error
*/
	ifl = 275;
	ival = 0;
	getifl(&ifl, &ival);
	if (ival!=0)
		goto done;
define_done:;
	sprintf(msg, "Scalar %s is defined", Slabel);
	ud_printmsg(msg);
	Slabel[0] = '\0';
	strcpy(Svalue_str, "0.0");
	Sdescript[0] = '\0';
/*
.....update the class list
*/
	ud_free_flist(&Dclass_list);
	sclass_list2.item = ncl_get_scalar_class(&(sclass_list2.num_item), 1);
	sclass_list2.answer = (char *) uu_malloc(21 * sizeof(char));

	Dclass_list.num_item = sclass_list2.num_item + 1;
	Dclass_list.item = (char **) uu_malloc(Dclass_list.num_item *sizeof(char *));
	Dclass_list.item[0] = (char *) uu_malloc(21 *sizeof(char *));
	strcpy(Dclass_list.item[0], "Default");
	for (i=0,j=1; i<sclass_list2.num_item; i++)
	{
		if (strcmp(sclass_list2.item[i], "Default")!=0)
		{
			Dclass_list.item[j] = (char *) uu_malloc(21 * sizeof(char));
			strcpy(Dclass_list.item[j], sclass_list2.item[i]);
			j++;
		}
	}
	Dclass_list.num_item = j;
	Dclass_list.answer = (char *) uu_malloc(21 * sizeof(char));
	strcpy(Dclass_list.answer, Sclassnm);
	ud_list_sort(&Dclass_list);
	ud_free_flist(&sclass_list2);

	ud_dispfrm_update_answer(0, 0, (int *)Slabel);
	ud_dispfrm_update_answer(0, 2, (int *)Svalue_str);
	ud_dispfrm_update_answer(0, 1, (int *)&Dclass_list);
	ud_dispfrm_update_answer(0, 3, (int *)Sdescript);
done:;
	UD_UNMARK (markval);
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : ncl_scalar_define(def_scalar_val)
**       display a form to define the scalar.
**		
**    PARAMETERS   
**       INPUT  : def_scalar_val: default scalar value
**					if it is UU_NULL, display the empty field
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_scalar_define(def_scalar_val)
char *def_scalar_val;
{
	UD_LIST sclass_list2;
	int i, j, len, status, *ans[5];
	NCL_cmdbuf cmdbuf;
	char lbuf[80];
	static UD_METHOD methods[] = {OnDefineLabel, OnSelectDClass, OnScalarValue, OnDefineDesc, OnScalarDef};
	static char called[]       = {6,6,6,6,6};
	static char traverse[]     = {1,1,1,1,1};
	static char disp[]         = {1,1,1,1,1,1};
	int markval=0;
/*
.....Setup the default answers
*/
	Slabel[0] = '\0';
	Sdescript[0] = '\0';
	if ((def_scalar_val==NULL) || (def_scalar_val[0]=='\0'))
	{
		strcpy(Svalue_str, "0.0");
	}
	else
	{
		strcpy(Svalue_str, def_scalar_val);
	}
	sclass_list2.item = ncl_get_scalar_class(&(sclass_list2.num_item),1);
	sclass_list2.answer = (char *) uu_malloc(21 * sizeof(char));

	Dclass_list.num_item = sclass_list2.num_item + 1;
	Dclass_list.item = (char **) uu_malloc(Dclass_list.num_item *sizeof(char *));
	Dclass_list.item[0] = (char *) uu_malloc(21 *sizeof(char *));
	strcpy(Dclass_list.item[0], "Default");

	for (i=0,j=1; i<sclass_list2.num_item; i++)
	{
		if (strcmp(sclass_list2.item[i], "Default")!=0)
		{
			Dclass_list.item[j] = (char *) uu_malloc(21 * sizeof(char));
			strcpy(Dclass_list.item[j], sclass_list2.item[i]);
			j++;
		}
	}
	Dclass_list.num_item = j;
	Dclass_list.answer = (char *) uu_malloc(21 * sizeof(char));
	strcpy(Dclass_list.answer, "Default");
	strcpy(Sclassnm, "Default");
	ud_list_sort(&Dclass_list);
	ud_free_flist(&sclass_list2);

	ans[0] = (int *)Slabel;
	ans[2] = (int *)Svalue_str;
	ans[1] = (int *)&Dclass_list;
	ans[3] = (int *)Sdescript;
	ans[4] = NULL;
/*
.....Command Reject
*/
	UD_MARK (markval,UU_TRUE);
	if (markval != 0)
	{
		goto done;
	}
/*
.....Display the form
*/
form:;
	status = ud_form1("scalardef.frm", ans, ans, methods, called, disp,
		traverse);
	if (status==-1)
		goto done;
/*
.....define scalar
*/
/*
.....remove trailing space
*/
	len = strlen (Slabel);
	if (len==0)
		goto done;
	while (len>=0)
	{
		if (Slabel[len]==' ')
			len--;
		else
			break;
	}
	if (len<=0)
		goto done;
	Slabel[len] = '\0';
	if (ncl_is_valid_label(Slabel)==0)
		goto form;
/*
.....check if the label and value field is valid
*/
	ncl_init_cmdbuf(&cmdbuf);
	sprintf(lbuf,"%s=%s", Slabel, Svalue_str);	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);

	if (Dclass_list.answer[0]!='\0')
		strcpy(Sclassnm, Dclass_list.answer);
	if ((Sclassnm[0]=='\0') && (Sdescript[0]=='\0'))
	{
		goto done;
	}
	if ((strcmp(Sclassnm, "Default")==0) && (Sdescript[0]=='\0'))
	{
		goto done;
	}
	ncl_init_cmdbuf(&cmdbuf);
	sprintf(lbuf,"prompt/scalar, %s, \"%s\", \"%s\"", 
		Slabel, Sclassnm, Sdescript);	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
	goto done;
/*
.....End of routine
*/
done:
	ud_free_flist(&Dclass_list);
	UD_UNMARK (markval);
	return;
}
		
/*********************************************************************
**    E_FUNCTION     : void nclf_get_scalar_value(string, len, scalar_value, typ)
**       Get a scalar value from a scalar string scalar.
**    PARAMETERS   
**       INPUT  : 
**          string: scalar name
**			len:    length of scalar name
**       OUTPUT :  
**          scalar_value:	scalar value
**			typ: 1: scalar value (not number)
**               0: it's a number value
**               -1: not a scalar value
**    RETURNS      : none 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_get_scalar_value(string, len, scalar_value, typ)
UM_f77_str_ptr string;
UM_int2 *len, *typ;
UU_REAL	*scalar_value;
{
	char *p, token[65];
	p = UM_cstr_of_f77_str(string);
	strncpy(token, p, *len);
	token[*len] = '\0';
	*typ = ncl_get_scalar_value(token, scalar_value);
}

/*********************************************************************
**    E_FUNCTION     : void nclf_get_mcpam_value(string, len, scalar_value, typ)
**       Get a macro scalar value from a scalar string scalar.
**    PARAMETERS   
**       INPUT  : 
**          string: scalar name
**			len:    length of scalar name
**       OUTPUT :  
**          scalar_value:	scalar value
**			typ: 1: a macro scalar value (not number)
**               0: it's not a macro scalar value
**    RETURNS      : none 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_get_mcpam_value(string, len, scalar_value, typ)
UM_f77_str_ptr string;
UM_int2 *len, *typ;
UU_REAL	*scalar_value;
{
	int ipg, iel, isub, nclkey, msub, i;
	short nwds, ietype;
	char *p, token[65], mtok[65];
	short ifnd, iclas;
	UU_REAL aval;

	p = UM_cstr_of_f77_str(string);
	strncpy(token, p, *len);
	token[*len] = '\0';
	isub = 0;
getval:;
	vxchk (token, &isub, &nclkey, &ipg, &iel, &nwds, &ietype);
	if (ietype==12)
	{
		mclfnd (token, &isub, &ifnd, &iclas, &ietype, &aval, mtok, &msub, &nclkey);
		if (ifnd==0)
		{
			*typ = 0;
			return;
		}
		if (iclas!=2)
		{
			*typ = 0;
			return;
		}
		if ((nclkey==0) && mtok[0]==' ')
		{
			*typ = 0;
			return;
		}
		if (mtok[0]==' ') goto done;
		if (ietype==NCLI_SCALAR) goto done;
/*
.....If parameter is set to a scalar constant,
.....a temporary scalar entity named @UN
.....was created in unibase but not in name list.
.....We have its value now & don't need to look it up.
*/
		if (strncmp(mtok, "@UN", 3)==0) goto done;
		
		for (i=0;i<NCL_MAX_LABEL;i++)
			if (mtok[i] == ' ') break;
		if (i == NCL_MAX_LABEL) i--;
		mtok[i] = '\0';
		strcpy(token, mtok);
		isub = msub;
		goto getval;
	}
	else
	{
		*typ = ncl_get_scalar_value(token, scalar_value);
		return;
	}
	if (iclas!=2) 
	{
		*typ = 0;
		return;
	}
done:;
	*scalar_value = aval;
	*typ = 1;
}

/*********************************************************************
**    E_FUNCTION     : ncl_scalar_form (outstr)
**       display a list form include all scalars and descriptions
**		if the user not cancel the form and select a scalar, then
**		this scalar will be as the input of command prompt or 
**		active form field if any of them is active.
**		
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  
**          outstr: selected scalar/output string
**					empty if no scalar selected/canceled
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_scalar_form(outstr)
char *outstr;
{
	UD_LIST sclass_list2;
	int i, *ans[9], frmmode, temp, but;
	static UD_METHOD methods[] = {OnSelectClass, OnFilter1, NULL, OnExcludeClass,
		OnFilter2, NULL, OnTableCalbacks, OnApplyScalar, OnCancelScalar, OnCloseScalar};
	static char called[]       = {6,6,6,6,6,6,6,6,6,6};
	static char traverse[]     = {1,1,1,1,1,1,1,1,1,1};
	static char disp[]         = {1,1,1,1,1,1,1,1,1,1,1,1,1};
/*
......only allow one scalar-list form exist, otherwise, have problem
......when update the scalar from outside
*/
	if (Ssfrm!=-1)
		return;
/*
......save the active form ID before we display the scalarlist form
......so that we can update it's field
*/
	ud_save_actform_id();
/*
.....Setup the default answers
*/
	sclass_list2.item = ncl_get_scalar_class(&(sclass_list2.num_item),1);
	sclass_list2.answer = (char *) uu_malloc(21 * sizeof(char));

	Sclass_list.num_item = sclass_list2.num_item + 1;
	Sclass_list.item = (char **) uu_malloc(Sclass_list.num_item *sizeof(char *));
	Sclass_list.item[0] = (char *) uu_malloc(21 *sizeof(char *));
	strcpy(Sclass_list.item[0], "All");
	for (i=1; i<Sclass_list.num_item; i++)
	{
		Sclass_list.item[i] = (char *) uu_malloc(21 * sizeof(char));	
		strcpy(Sclass_list.item[i], sclass_list2.item[i-1]);
	}
	Sclass_list.answer = (char *) uu_malloc(21 * sizeof(char));
	strcpy(Sclass_list.answer, Sclassstr);
	ud_list_sort(&Sclass_list);
	ud_free_flist(&sclass_list2);
/*
......copy Sclass_list to Eclass_list
*/
	Eclass_list.num_item = Sclass_list.num_item;
	Eclass_list.item = (char **) uu_malloc(Eclass_list.num_item *sizeof(char *));
	Eclass_list.item[0] = (char *) uu_malloc(21 *sizeof(char *));
	strcpy(Eclass_list.item[0], "None");
	for (i=1; i<Eclass_list.num_item; i++)
	{
		Eclass_list.item[i] = (char *) uu_malloc(21 * sizeof(char));	
		strcpy(Eclass_list.item[i], Sclass_list.item[i]);
	}
	Eclass_list.answer = (char *) uu_malloc(21 * sizeof(char));
/*
.....remeber the last filter
*/
/*	strcpy(Eclassstr, "None");
	strcpy(Sclassstr, "All");
	strcpy(Sfilter, "*");
	Efilter[0] = '\0'; */
	strcpy(Eclass_list.answer, Eclassstr);
/*
......create scalar table record list with
......Name	Class	Value	Modified	Description
*/
	ul_to_upper(Efilter);
	ul_to_upper(Sfilter);
	Scalar_tlist.num_item = ncl_get_scalar_tlist(&Scalar_tlist, Sclassstr, Sfilter, Eclassstr, Efilter);
	if (Scalar_tlist.num_item==0) 
	{
		outstr[0] = '\0';
	}
	Selscalar[0] = '\0';
	frmmode = uw_if_formopen();
	ans[0] = (int *)&Sclass_list;
	ans[1] = (int *) &but;
	ans[2] = (int *)Sfilter;
	ans[3] = (int *)&Eclass_list;
	ans[4] = (int *) &but;
	ans[5] = (int *)Efilter;
	ans[6] = (int *)&Scalar_tlist;
	ans[7] = (int *)&temp;
	ans[8] = (int *)&temp;
/*
......we want to form to remember the last sort,
......so don't reset those value
*/
/*	name_click = 0;
	value_click = 0;
	class_click = 0;
	modify_click = 0;
	descript_click = 0;
*/
/*
.....Display the form
*/
	Ssfrm = ud_form_display1("scalarsel.frm", ans, ans, methods, called, disp,
		traverse);

	if (Ssfrm == -1) goto nofrm;
	resort_table();
/*
.....Wait for the user to select a view
.....or cancel the form
*/
	Sfrm_cancel = 0;
/*	uw_dispfrm_wait(Ssfrm); */
	goto done;
nofrm:
	ud_wrerr("Could not load 'scalarsel.frm'.");
	Sfrm_cancel = -1;
	goto done;
/*
.....End of routine
*/
done:
/*
	ud_free_flist(&Sclass_list);
	ud_free_flist(&Eclass_list);
	ud_free_tlist(&Scalar_tlist);
	ud_reset_actform_id();
*/
/*
.....the outstr will be always empty now because we update it on command/form field
.....already in 'APPLY' or 'CLOSE' button callback
*/
	outstr[0] = '\0';
	return;
}
int ncl_filter_str(label, filter)
char *label, *filter;
{
	int nc1, nc2, match;
	nc1 = strlen (label);
	while ((nc1>1)&&(label[nc1-1]==' ')) nc1--;
	label[nc1] = '\0';
	nc2 = strlen (filter);
	while ((nc2>1)&&(filter[nc2-1]==' ')) nc2--;
	filter[nc2] = '\0';
	chkwstr(label, &nc1, filter, &nc2, &match);
	return match;
}
/*********************************************************************
**    E_FUNCTION         :  ncl_get_scalar_tlist(tlist, classnm, filter, eclassnm, efilter)
**       get all the scalar list with all scalar informaton
**		include Name	Class	Value	Modified	Description
**		into a UD_TLIST struction
**
**    PARAMETERS   
**       INPUT  : classnm: if classnm is empty, then treat as 'all'
**					filter: filter for class name
**					eclassnm: classnm as reversed for scalar name
**					efilter: filter as reversed for scalar name
**       OUTPUT : tlist: UD_TLIST struction include scalar info 
**    RETURNS      : number of the list
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_scalar_tlist(tlist, classnm, filter, eclassnm, efilter)
UD_TLIST *tlist;
char *classnm, *filter;
char *eclassnm, *efilter;
{
	int i, status, entnum, len, itemnum;
	char descript[65], label[65], classnm2[65];
	struct NCL_scalar_rec e;
	char val_str[80], timestr[80];
	short snc;
	int year, today_year, today_mon, today_day;
	time_t mtime, todaytime;
	struct tm * timeinfo;
	struct tm * today_tinfo;
	struct NCL_scalar_rec sclr;
	static char *mon[]={"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG",
		"SEP","OCT","NOV","DEC"};
	
	status = 0;
	entnum = 0;
	itemnum = 0;
	len = 0;

#ifndef UU_RS6000
	time(&todaytime);
#else
	ftime(&todaytime);
#endif
	today_tinfo = localtime(&todaytime);
	today_year = today_tinfo->tm_year;
	today_mon = today_tinfo->tm_mon;
	today_day = today_tinfo->tm_mday;

	e.rel_num = NCL_SCALAR_REL;

	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(e.rel_num, &entnum,
								&e.key);
		if (status == 0)
			len++;
	}
/*
......Name	Class	Value	Modified	Description
*/
	tlist->num_col = 5; 
	tlist->num_item = len;
	tlist->answer = 0;
	if (tlist->num_col>0)
		tlist->col_label = (char**) uu_malloc(tlist->num_col*sizeof (char*));
	for (i=0; i<tlist->num_col;i++)
	{
		tlist->col_label[i] = (char*) uu_malloc(20*sizeof(char));
	}
	strcpy(tlist->col_label[0], "Name");
	strcpy(tlist->col_label[1], "Class");
	strcpy(tlist->col_label[2], "Value");
	strcpy(tlist->col_label[3], "Modified");
	strcpy(tlist->col_label[4], "Description");

	if (tlist->num_item>0)
		tlist->data = (UD_ITEMDATA *) uu_malloc(tlist->num_item*sizeof(UD_ITEMDATA));
	for (i=0; i<tlist->num_item;i++)
	{
		(tlist->data[i]).itemnum = tlist->num_col;
		(tlist->data[i]).data_items = 
					(char **) uu_malloc(tlist->num_col*sizeof(char*));
	}
/*
.....even list len is 0, we still need column label to create the empty
.....table list in order to add list item later
*/
	if (len==0)
		return 0;
	status = 0;
	entnum = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(e.rel_num, &entnum,
								&e.key);

		if (status == UU_SUCCESS)
		{
			sclr.key = e.key;
			status = ur_retrieve_data_fixed (&sclr);
			if (status == UU_SUCCESS && sclr.subscr < 0)
			{
/*
.....scalar is vocab word
*/
				continue;
			}
		}
		if (status == 0)
		{
			ur_retrieve_data(&e, sizeof(e));
/*
.....don't return temperate scalar start with "@"
.....Yurong
*/
			if (e.label[0]!='@')
			{
				if ( ((classnm==NULL)||(classnm[0]=='\0')
					||(strcmp(classnm, e.classnm)==0)
					||(strcmp(classnm, "All")==0))
					&& (ncl_filter_str2(e.label, filter)==1)
					&& ((eclassnm==NULL)||(eclassnm[0]=='\0')
					||(strcmp(eclassnm, e.classnm)!=0)
					||(strcmp(eclassnm, "None")==0))
					&& (ncl_efilter_str2(e.label, efilter)==1))			
				{
/*
......convert the 'modified' into a string
*/
					mtime = (time_t)e.modified;
					if (mtime<0)
					{
						mtime = 0;
						ud_wrerr("Internal error: wrong time value");
					}
					timeinfo = localtime(&mtime);
					if (e.modified<=0)
/*
.....older version without modified define, use 00-NOV-2008 which mean older version
*/
					{
						strcpy(timestr,"00-NOV-2008");
					}
					else if ((timeinfo->tm_year<today_year)
						|| (timeinfo->tm_mon<today_mon)
						|| (timeinfo->tm_mday<today_day))
					{
/*
.....older than today
.....use format 02-OCT-2008
*/
						year = timeinfo->tm_year;
						if (year < 70) year = year + 2000;
						else if (year > 99) year = year + 1900;
						sprintf(timestr,"%02d-%s-%04d",timeinfo->tm_mday,mon[timeinfo->tm_mon],year);
					}
					else
					{
/*
.....modified today
.....use format 10:30:02
*/
						sprintf(timestr,"%02d:%02d:%02d",timeinfo->tm_hour,timeinfo->tm_min,timeinfo->tm_sec);
					}
/*
......data_items[3] for Modified
*/
					len = strlen (timestr);
					tlist->data[itemnum].data_items[3] = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy(tlist->data[itemnum].data_items[3], timestr);
/*
.....remove trailing spaces
*/
					strcpy(classnm2, e.classnm);
					i = strlen(classnm2);
					if (i>0)
					{
						while ((i!=0) &&(classnm2[i-1]==' ')) i--;
						classnm2[i] = '\0';
					}
/*
......data_items[1] for Class
*/
					tlist->data[itemnum].data_items[1] = (char*)uu_malloc((i+1)*sizeof(char));
					strcpy(tlist->data[itemnum].data_items[1], classnm2);
/*
.....remove trailing spaces
*/
					strcpy(descript, e.descript);
					i = strlen(descript);
					if (i>0)
					{
						while ((i!=0) &&(descript[i-1]==' ')) i--;
						descript[i] = '\0';
					}
/*
......data_items[4] for Description
*/
					tlist->data[itemnum].data_items[4] = (char*)uu_malloc((i+1)*sizeof(char));
					strcpy(tlist->data[itemnum].data_items[4], descript);

					strncpy(label, e.label, NCL_MAX_LABEL);
					label[63] = '\0';
					i = strlen(label);
					if (i>0)
					{
						while ((i!=0) &&(label[i-1]==' ')) i--;
							label[i] = '\0';
					}
/*
.....seem like when trace label inside a macro, it will
.....have a label define as "@UN" which are not defined by users.
*/
					if (strcmp(label, "@UN")==0)
						continue;
					snc = (short) strlen (label);
					rmlab_prefix(label, label, &snc);
					if (e.subscr>0)
						sprintf (label, "%s(%d)", label, e.subscr);
					len = strlen(label);
/*
......data_items[0] for Name
*/
					tlist->data[itemnum].data_items[0] = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy(tlist->data[itemnum].data_items[0], label);
					ncl_sprintf(val_str, &(e.scalar_value), 1);	
					len = strlen(val_str);
/*
......data_items[2] for Value
*/
					tlist->data[itemnum].data_items[2] = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy(tlist->data[itemnum].data_items[2], val_str);
					itemnum++;
				}
			}
		}
	}
/*
.....even list len is 0, we still need column label to create the empty
.....table list in order to add list item later, so don't free list's col_label
.....it never changed when form display, we only free it after form done.
*/
	if (itemnum==0)
	{
/*
		for (i=0; i<tlist->num_col;i++)
		{
			uu_free(tlist->col_label[i]);
		}
		uu_free(tlist->col_label);
*/
		for (i=0; i<tlist->num_item; i++)
		{
			if (tlist->data[i].itemnum>0)
			{
				if (tlist->data[i].data_items!=NULL)
					uu_free(tlist->data[i].data_items);
				tlist->data[i].data_items = NULL;
			}
		}
		if ((tlist->num_item)&&(tlist->data!=NULL))
			uu_free(tlist->data);
		tlist->data = NULL;
	}
	tlist->num_item = itemnum;
	return (itemnum);
}
/*********************************************************************
**    E_FUNCTION         :  ncl_update_scalar_frm()
**       update the scalar list 
**
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT : 
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_update_scalar_frm()
{
	UD_TLIST slist;
	UD_LIST sclass_list2;
	int i;

	if (Ssfrm==-1)
		return;
	ul_to_upper(Efilter);
	ul_to_upper(Sfilter);
	slist.num_item = ncl_get_scalar_tlist(&slist, Sclassstr, Sfilter, Eclassstr, Efilter);
	Selscalar[0] = '\0';
/*
.....copy the list into Scalar_list (because the form input is point to it)
*/
	ud_free_tlist(&Scalar_tlist);
	ud_tlist_copy(&slist, &Scalar_tlist);
	ud_free_tlist(&slist);
	ud_dispfrm_update_answer(Ssfrm, 6, (int *)&Scalar_tlist);
/*
.....update the class list
*/
	ud_free_flist(&Sclass_list);
	ud_free_flist(&Eclass_list);
	sclass_list2.item = ncl_get_scalar_class(&(sclass_list2.num_item),1);
	sclass_list2.answer = (char *) uu_malloc(21 * sizeof(char));

	Sclass_list.num_item = sclass_list2.num_item + 1;
	Sclass_list.item = (char **) uu_malloc(Sclass_list.num_item *sizeof(char *));
	Sclass_list.item[0] = (char *) uu_malloc(21 *sizeof(char *));
	strcpy(Sclass_list.item[0], "All");

	for (i=1; i<Sclass_list.num_item; i++)
	{
		Sclass_list.item[i] = (char *) uu_malloc(21 * sizeof(char));	
		strcpy(Sclass_list.item[i], sclass_list2.item[i-1]);
	}
	Sclass_list.answer = (char *) uu_malloc(21 * sizeof(char));
	strcpy(Sclass_list.answer, Sclassstr);
	ud_list_sort(&Sclass_list);
	ud_free_flist(&sclass_list2);
/*
......copy Sclass_list to Eclass_list
*/
	Eclass_list.num_item = Sclass_list.num_item;
	Eclass_list.item = (char **) uu_malloc(Eclass_list.num_item *sizeof(char *));
	Eclass_list.item[0] = (char *) uu_malloc(21 *sizeof(char *));
	strcpy(Eclass_list.item[0], "None");
	for (i=1; i<Eclass_list.num_item; i++)
	{
		Eclass_list.item[i] = (char *) uu_malloc(21 * sizeof(char));	
		strcpy(Eclass_list.item[i], Sclass_list.item[i]);
	}
	Eclass_list.answer = (char *) uu_malloc(21 * sizeof(char));
	strcpy(Eclass_list.answer, Eclassstr);

	ud_dispfrm_update_answer(Ssfrm, 6, (int *)&Scalar_tlist);
	ud_dispfrm_update_answer(Ssfrm, 0, (int *)&Sclass_list);
	ud_dispfrm_update_answer(Ssfrm, 3, (int *)&Eclass_list);
	resort_table();
	ud_update_form(Ssfrm);
}
ncl_get_sclar_frm()
{
	return Ssfrm;
}

/*********************************************************************
**    E_FUNCTION         :  ncl_filter_str2(label, filter)
**       This function doing the same as ncl_filter_str but the filter is without "*"
**		and the 'filter' can be anywhere in the string
**
**    PARAMETERS   
**       INPUT  : label: string to be checked
**					filter: filter string
**       OUTPUT : 
**    RETURNS      : 0: not match, 1: match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_filter_str2(label, filter)
char *label, *filter;
{
	char tmpstr[1024], label_str[1024], filter_str[1024];
	int i, nc1, nc2, match;
/*
.....should not change the input string
*/
	strcpy(label_str, label);
	strcpy(filter_str, filter);
	nc1 = strlen (label_str);
	while ((nc1>1)&&(label_str[nc1-1]==' ')) nc1--;
	label_str[nc1] = '\0';
/*
.....remove '*' in the front and end, then add in 
.....(to make sure the phase can be anywhere in the string)
*/
	nc2 = strlen (filter_str);
	i=0;
	while ((nc2>1)&&(filter_str[nc2-1]==' ')) nc2--;
	if (nc2==0)
/*
.....all empty for filter, always return matched
*/
		return 1;

	while ((nc2>1)&&(filter_str[nc2-1]=='*')) nc2--;
	filter_str[nc2] = '\0';
	
	if (nc2>0)
	{
		strcpy(tmpstr, "*");
		strcat(tmpstr, filter_str);
		strcat(tmpstr, "*");
		strcpy(filter_str, tmpstr);
		nc2 = strlen (filter_str);
		chkwstr(label_str, &nc1, filter_str, &nc2, &match);
		return match;
	}
/*
......if nc2 = 0, if mean only have "*", so always match
*/
	return 1;
}

/*********************************************************************
**    E_FUNCTION         :  ncl_efilter_str2(label, filter)
**       This function is similar to ncl_filter_str2 except
**			it check if the filter is not in the string
**
**    PARAMETERS   
**       INPUT  : label: string to be checked
**					filter: exclude filter string
**       OUTPUT : 
**    RETURNS      : 0: not match, 1: match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_efilter_str2(label, filter)
char *label, *filter;
{
	char tmpstr[1024], label_str[1024], filter_str[1024];
	int i, nc1, nc2, match;
/*
.....should not change the input string
*/
	strcpy(filter_str, filter);
	nc2 = strlen (filter_str);
	i=0;
	while ((nc2>1)&&(filter_str[nc2-1]==' ')) nc2--;
	if (nc2==0)
/*
.....all empty for exclude filter, always return matched
*/
		return 1;

	if (ncl_filter_str2(label, filter_str))
		return 0;
	else
		return 1;
}


