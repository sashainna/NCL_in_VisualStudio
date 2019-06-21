/*********************************************************************
**    NAME         :  nedata.c
**       CONTAINS: routines to handle NCL Datas
**					ncl_data_form
**					ncl_update_data_frm
**					ncl_get_data_value
**
**    COPYRIGHT 2012 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**        nedata.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**        04/10/18 , 15:14:07
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

static char SelData[256];
static char Sdata_ele[256], Stype[256];
static int Sfrm_cancel=0, Ssfrm = -1;
static char Sfilter[65]="*", Efilter[65]="";
static char Sfilter_old[65]="*", Efilter_old[65]="";
static UD_DLIST Sdata_list;

static UD_FSTAT OnFilter1(), OnFilter2(), OnDataCallbacks(), OnApplyData(), OnCancelData(), 
		OnCloseData();
void ncl_update_input(), ncl_update_data_frm();

static UD_TABLEINFO saved_info;
static int item_click[20] = {0,0,0,0,0,0,0,0,0,0,
								0,0,0,0,0,0,0,0,0,0};
static int Stotal_cols = 0;

static int S_tmpdatn_num = 0;
static char S_tmpdatn[20][64];
/*********************************************************************
**   I_FUNCTION: SortFunc()
**      Sort the list on Select Data form in partical order
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

	switch(lParamSort)
	{
	case 0:
		nRetVal = ncl_label_cmp(pData1->data_items[0],
                                 pData2->data_items[0]);
		break;
	default:
		if ((pData1->data_items[lParamSort]==UU_NULL)
			&& (pData2->data_items[lParamSort]==UU_NULL))
			nRetVal = 0;
		else if (pData1->data_items[lParamSort]==UU_NULL)
			nRetVal = strcmp("",pData2->data_items[lParamSort]);
		else if (pData2->data_items[lParamSort]==UU_NULL)
			nRetVal = strcmp(pData1->data_items[lParamSort], "");
		else
			nRetVal = strcmp(pData1->data_items[lParamSort],
                                 pData2->data_items[lParamSort]);
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
	switch(lParamSort)
	{
	case 0:
		nRetVal = -ncl_label_cmp(pData1->data_items[0],
                                 pData2->data_items[0]);
		break;
	default:	
		if ((pData1->data_items[lParamSort]==UU_NULL)
			&& (pData2->data_items[lParamSort]==UU_NULL))
			nRetVal = 0;
		else if (pData1->data_items[lParamSort]==UU_NULL)
			nRetVal = -strcmp("",pData2->data_items[lParamSort]);
		else if (pData2->data_items[lParamSort]==UU_NULL)
			nRetVal = -strcmp(pData1->data_items[lParamSort], "");
		else
			nRetVal = -strcmp(pData1->data_items[lParamSort],
                                 pData2->data_items[lParamSort]);
		break;
	}
	return nRetVal;
}
/*********************************************************************
**   I_FUNCTION: resort_table()
**      resort the list as last appeared after reload/filter on Select data form.
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
	int i;

	for (i=0;i<Stotal_cols;i++)
	{
		if (item_click[i]==1)
			break;
	}
	if (i==Stotal_cols)
		return;
	
	info.flag = saved_info.flag;
	info.frmid = saved_info.frmid;
	info.fldid = saved_info.fldid;
	info.col = saved_info.col;
	info.row = -1;

	for (i=0;i<Stotal_cols;i++)
	{
		if ((info.col==i)&&(item_click[i]%2==0))
		{
			ud_form_sorttable(&info, (UD_SMETHOD)SortFunc2);
		}
		else if ((info.col==i)&&(item_click[i]%2))
		{
			ud_form_sorttable(&info, (UD_SMETHOD)SortFunc);
		}
	}
}
/*********************************************************************
**   I_FUNCTION: OnFilter1(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Data form.
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
	UD_DLIST slist;
	
	if (strcmp(val->frmstr, Efilter)==0)
	{
		ud_wrerr("The select filter can not be the same as exclude filter");
		return UD_BADREQ;
	}
	if (val->frmstr[0]!='\0')
	{
		strcpy(Sfilter, val->frmstr);
		ul_to_upper(Sfilter);
	}
	else
		Sfilter[0] = '\0';
	ul_to_upper(Efilter);
	if ((stricmp(Sfilter, Sfilter_old)==0)&&(stricmp(Efilter, Efilter_old)==0))
		return(UD_FLDOK);
	strcpy(Sfilter_old, Sfilter);
	strcpy(Efilter_old, Efilter);
	slist.num_item = ncl_get_data_list(&slist, Sfilter, Efilter);
	SelData[0] = '\0';
/*
.....copy the list into Scalar_list (because the form input is point to it)
*/
	ud_free_dlist(&Sdata_list);
	ud_dlist_copy(&slist, &Sdata_list);
	ud_free_dlist(&slist);
	ud_dispfrm_update_answer(Ssfrm, 6, (int *)&Sdata_list);
//	resort_table();
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
	UD_DLIST slist;
	if (strcmp(val->frmstr, Sfilter)==0)
	{
		ud_wrerr("The exclude filter can not be the same as select filter");
		return UD_BADREQ;
	}
	if (val->frmstr[0]!='\0')
	{
		strcpy(Efilter, val->frmstr);
		ul_to_upper(Efilter);
	}
	else
		Efilter[0] = '\0';
	ul_to_upper(Sfilter);
	slist.num_item = ncl_get_data_list(&slist, Sfilter, Efilter);
	SelData[0] = '\0';
/*
.....copy the list into Scalar_list (because the form input is point to it)
*/
	ud_free_dlist(&Sdata_list);
	ud_dlist_copy(&slist, &Sdata_list);
	ud_free_dlist(&slist);
	ud_dispfrm_update_answer(Ssfrm, 6, (int *)&Sdata_list);
	resort_table();
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnFilter1_but(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Data form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnFilter1_but(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_DLIST slist;
	UD_DDATA data;
	char filter[65], type_str[20];

	data.frmstr = filter;
	ud_getfrm_field(Ssfrm, 3, data, UU_FALSE);
	strcpy(Sfilter, filter);

	ud_getfrm_field(Ssfrm, 5, data, UU_FALSE);
	strcpy(Efilter, filter);

	ul_to_upper(Sfilter);
	ul_to_upper(Efilter);
	if ((stricmp(Sfilter, Sfilter_old)==0)&&(stricmp(Efilter, Efilter_old)==0))
		return(UD_FLDOK);
	strcpy(Sfilter_old, Sfilter);
	strcpy(Efilter_old, Efilter);
	slist.num_item = ncl_get_data_list(&slist, Sfilter, Efilter);
	SelData[0] = '\0';
/*
.....copy the list into Scalar_list (because the form input is point to it)
*/
	ud_free_dlist(&Sdata_list);
	ud_dlist_copy(&slist, &Sdata_list);
	ud_free_dlist(&slist);
	ud_dispfrm_update_answer(Ssfrm, 6, (int *)&Sdata_list);
	resort_table();

	type_str[0] = '\0';
	SelData[0]  = '\0';
	ud_dispfrm_update_answer(Ssfrm, 0, (int *)SelData);
	ud_dispfrm_update_answer(Ssfrm, 1, (int *)type_str);

	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnFilter2_but(fieldno,val,stat)
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
static UD_FSTAT OnFilter2_but(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_DLIST slist;
	UD_DDATA data;
	char filter[65], type_str[20];

	data.frmstr = filter;
	ud_getfrm_field(Ssfrm, 3, data, UU_FALSE);
	strcpy(Sfilter, filter);

	ud_getfrm_field(Ssfrm, 5, data, UU_FALSE);
	strcpy(Efilter, filter);

	ul_to_upper(Sfilter);
	ul_to_upper(Efilter);
	slist.num_item = ncl_get_data_list(&slist, Sfilter, Efilter);
	SelData[0] = '\0';
/*
.....copy the list into Scalar_list (because the form input is point to it)
*/
	ud_free_dlist(&Sdata_list);
	ud_dlist_copy(&slist, &Sdata_list);
	ud_free_dlist(&slist);
	ud_dispfrm_update_answer(Ssfrm, 6, (int *)&Sdata_list);
	resort_table();
			
	type_str[0] = '\0';
	SelData[0]  = '\0';
	ud_dispfrm_update_answer(Ssfrm, 0, (int *)SelData);
	ud_dispfrm_update_answer(Ssfrm, 1, (int *)type_str);

	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnDataCallbacks(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Data form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnDataCallbacks(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_ITEMDATA *data;
	int	subscript, status;
	UU_KEY_ID nclkey;
	int relnum, nc;
	UM_int2 etype;
	char data_value[NCL_MAX_LABEL+1], label[NCL_MAX_LABEL*2], type_str[20],
		tempstr[NCL_MAX_LABEL+1];
	int i, type;
	UD_TABLEINFO *info = (UD_TABLEINFO *)(val->frmint);
	if (info->flag==1)
	{
/*
......list selected, doing selection callback
*/
		if (info->row>=0)
		{
			data = (UD_ITEMDATA *)(info->data_ptr);
/*
.....not the data value, but the reference
*/
/*
			strcpy(SelData, data->data_items[info->col]);
*/
			strcpy(tempstr, data->data_items[0]);
/*
.....remove trailing spaces
*/
			nc = strlen (tempstr);
			while ((nc>1)&&(tempstr[nc-1]==' ')) nc--;
			tempstr[nc] = '\0';
			if (info->col>0)
				sprintf(SelData, "%s[%d]", tempstr, info->col);
			else
				strcpy(SelData, tempstr);
		}
		else
			SelData[0] = '\0';
/*
......update other data fields
*/
		ncl_get_data_value(SelData, data_value, &type);
		if (type==0)
			strcpy(type_str, "Value");
		else if (type==1)
			strcpy(type_str, "Vocabulary");
		else if (type==24)
			strcpy(type_str, "Text");
		else if (type==2)
		{
/*
.....check the data value to see if it is a geometry
*/
			strcpy(type_str, "Text");
			strcpy(label, data_value);
			ncl_parse_subnum (label, &subscript);
			status = ncl_vxchk (label,subscript, &nclkey, &relnum);
			if ((status==0)&&(nclkey>0))
			{
				ncl_get_type(relnum, &etype);
				switch (etype)
				{
					case NCLI_POINT:
						strcpy(type_str, "Point");
						break;
					case NCLI_POINTVEC:
						strcpy(type_str, "Point Vector");
						break;
					case NCLI_VECTOR:
						strcpy(type_str, "Vector");
						break;
					case NCLI_LINE:
						strcpy(type_str, "Line");
						break;
					case NCLI_PLANE:
						strcpy(type_str, "Plane");
						break;
					case NCLI_CIRCLE:
						strcpy(type_str, "Circle");
						break;
					case NCLI_CURVE:
						strcpy(type_str, "Curve");
						break;
					case NCLI_SOLID:
						strcpy(type_str, "Solid");
						break;
					case NCLI_MATRIX:
						strcpy(type_str, "Matrix");
						break;
					case NCLI_LABEL:
						strcpy(type_str, "Label");
						break;
					case NCLI_SHAPE:
						strcpy(type_str, "Shape");
						break;
					case NCLI_PATERN:
						strcpy(type_str, "Patern");
						break;
					case NCLI_MESHSURF:
						strcpy(type_str, "Mesh Surface");
						break;
					case NCLI_QUILTSURF:
						strcpy(type_str, "Quilt Surface");
						break;
					case NCLI_NETSF:
						strcpy(type_str, "Net Surface");
						break;
					case NCLI_EVALSF:
						strcpy(type_str, "Surface");
						break;
					case NCLI_RBSF:
						strcpy(type_str, "Surface");
						break;
					case NCLI_REGSURF:
						strcpy(type_str, "Surface");
						break;
					case NCLI_REVSURF:
						strcpy(type_str, "Surface");
						break;
					case NCLI_UVCVONSF:
						strcpy(type_str, "Surface");
						break;
					case NCLI_SURF:
						strcpy(type_str, "Surface");
						break;
					case NCLI_SYMBOL:
						strcpy(type_str, "Symbol");
						break;
					case NCLI_INSTANCE:
						strcpy(type_str, "Instance");
						break;
				}
			}
		}
		else
			type_str[0] = '\0';
		ud_dispfrm_update_answer(Ssfrm, 0, (int *)SelData);
		ud_dispfrm_update_answer(Ssfrm, 1, (int *)type_str);
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
		for (i=0;i<Stotal_cols;i++)
		{
			if ((info->col==i)&&(item_click[i]%2==0))
			{
				ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
				item_click[i]++;
			}
			else if ((info->col==i)&&(item_click[i]%2))
			{
				ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
				item_click[i]++;
			}
		}
	}
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnApplyData(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Data form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnApplyData(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (Sdata_list.answer[0]!=-1)
		ncl_update_input(SelData);
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnCancelData(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Data form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnCancelData(fieldno,val,stat)
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
	Sdata_list.answer[0] = -1;
	Ssfrm = -1;
	return(UD_FRMCLOSE);
}
/*********************************************************************
**   I_FUNCTION: OnCloseData()
**      Callback function for the Close button on Data Selection form..
**   PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnCloseData()
{
	if ((Sfrm_cancel!=-1) && (SelData[0] != '\0'))
	{
		ncl_update_input(SelData);
	}
/*
.....Mark the form as closed
*/
	Ssfrm = -1;
/*
.....free the memory
*/
	ud_free_dlist(&Sdata_list);
	ud_reset_actform_id();
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: Smatch_data_filter(datarec, filter, efilter)
**      check if the DATA matched the searching string
**   PARAMETERS
**       INPUT  : datarec: data structure
**				filter, efilter: include and exclude filter
**       OUTPUT : none.
**   RETURNS: 1: match, 0: not match
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static int Smatch_data_filter(datarec, filter, efilter)
struct NCL_datast_rec datarec;
char *filter, *efilter;
{
	struct NCL_datael_rec *dsp;
	char tempstr[NCL_MAX_LABEL*2], filtstr_cap[NCL_MAX_LABEL*2], 
		filter_cap[NCL_MAX_LABEL*2], efilter_cap[NCL_MAX_LABEL*2];
	int i, nc, len;
	int match = 0;
	int ematch = 0;

	strcpy(tempstr, datarec.label);
	nc = strlen (tempstr);
	while ((nc>1)&&(tempstr[nc-1]==' ')) nc--;
	tempstr[nc] = '\0';
	if (datarec.subscr>0)
		sprintf(tempstr, "%s(%d)", tempstr, datarec.subscr);

	strcpy_s(filtstr_cap, NCL_MAX_LABEL*2, tempstr);
	strcpy_s(filter_cap, NCL_MAX_LABEL*2, filter);		
	strcpy_s(efilter_cap, NCL_MAX_LABEL*2, efilter);		
	ul_to_upper(filtstr_cap);
	ul_to_upper(filter_cap);
	ul_to_upper(efilter_cap);

	match = ncl_filter_str2(filtstr_cap, filter_cap);
	ematch = ncl_efilter_str2(filtstr_cap, efilter_cap);

	dsp = datarec.datael;
	if (datarec.no_datael<=0)
		return 0;
	for (i=0; i<datarec.no_datael;i++)
	{
		if (dsp[i].type==0)
			ncl_sprintf (tempstr, &(dsp[i].value), 1);
		else if (dsp[i].type==1)
		{
			ncl_get_vocwrd((int)(dsp[i].value), tempstr);
		}
		else
		{
			strncpy(tempstr, dsp[i].label, NCL_MAX_LABEL); 
			tempstr[NCL_MAX_LABEL] = '\0';
			len = strlen(tempstr);
			while ((len!=0) &&(tempstr[len-1]==' ')) len--;
				tempstr[len] = '\0';
			if (dsp[i].isub>0)
				sprintf (tempstr, "%s(%d)", tempstr, dsp[i].isub);
		}
		strcpy_s(filtstr_cap, NCL_MAX_LABEL*2, tempstr);
		ul_to_upper(filtstr_cap);
		if (match==0)
			match = ncl_filter_str2(filtstr_cap, filter_cap);
		if (ematch==1)
			ematch = ncl_efilter_str2(filtstr_cap, efilter_cap);
	}
	if ((match==1)&&(ematch==1))
		return 1;
	return 0;
}

/*********************************************************************
**    E_FUNCTION         :  ncl_get_data_list(list, filter, efilter)
**       get all the Data list with all data item
**		into a UD_DLIST struction
**
**    PARAMETERS   
**       INPUT  : 
**					filter: filter for data name
**					efilter: filter as reversed for data name
**       OUTPUT : dlist: UD_DLIST struction include data info 
**    RETURNS      : number of the list
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_data_list(dlist, filter, efilter)
UD_DLIST *dlist;
char *filter, *efilter;
{
	struct NCL_datael_rec *dsp;
	int i, status, entnum, len, itemnum, matched;
	struct NCL_datast_rec datarec;
	char tempstr[NCL_MAX_LABEL*2], filtstr_cap[NCL_MAX_LABEL*2], 
		filter_cap[NCL_MAX_LABEL*2], efilter_cap[NCL_MAX_LABEL*2];
	int total_cols, nc;
	
	status = 0;
	entnum = 0;
	itemnum = 0;
	len = 0;

	datarec.rel_num = NCL_DATAST_REL;

	Stotal_cols = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(datarec.rel_num, &entnum,
								&datarec.key);
		if (status == 0)
		{
			len++;
/*
.....check the number of the data item
*/
			ncl_retrieve_data_fixed (&datarec);
/*
.....don't return temperate data start with "@"
.....Yurong
*/
			if (datarec.label[0]!='@')
			{
				matched = Smatch_data_filter(datarec, filter, efilter);
				if (matched)
				{
					total_cols = datarec.no_datael + 1;
					if (total_cols>Stotal_cols)
						Stotal_cols = total_cols;
				}
			}
		}
	}
	dlist->num_col = Stotal_cols; 
	dlist->num_item = len;
	dlist->answer[0] = 0;
	dlist->answer[1] = 0;
	if (dlist->num_col>0)
		dlist->col_label = (char**) uu_malloc(dlist->num_col*sizeof (char*));

	for (i=0; i<dlist->num_col;i++)
	{
		dlist->col_label[i] = (char*) uu_malloc(10*sizeof(char));
		if (i==0)
			strcpy(dlist->col_label[0], "Name");
		else
			sprintf(dlist->col_label[i], "%d", i); 
	}

	if (dlist->num_item>0)
		dlist->data = (UD_ITEMDATA *) uu_malloc(dlist->num_item*sizeof(UD_ITEMDATA));
	for (i=0; i<dlist->num_item;i++)
	{
		(dlist->data[i]).itemnum = dlist->num_col;
		(dlist->data[i]).data_items = 
					(char **) uu_malloc(dlist->num_col*sizeof(char*));
	}
	if (len==0)
		return 0;
	status = 0;
	entnum = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_data_key(datarec.rel_num, &entnum,
								&datarec.key);
		if (status == 0)
		{
			ncl_retrieve_data_fixed (&datarec);
/*
.....don't return temperate data start with "@"
.....Yurong
*/
			if (datarec.label[0]!='@')
			{
				matched = Smatch_data_filter(datarec, filter, efilter);
				if (matched)
				{
/*
.....label define as 64 chars
*/
					nc = strlen(datarec.label);
					if (nc>64) nc = 64;
					strncpy(tempstr, datarec.label, nc);
					tempstr[nc] = '\0';
					while ((nc>1)&&(tempstr[nc-1]==' ')) nc--;
					tempstr[nc] = '\0';
					if (datarec.subscr>0)
						sprintf(tempstr, "%s(%d)", tempstr, datarec.subscr);

					len  = strlen(tempstr);
					dlist->data[itemnum].data_items[0] = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy(dlist->data[itemnum].data_items[0], tempstr);
/*
......get the data item and put in the data_items
*/
					dsp = datarec.datael;
					for (i=0; i<datarec.no_datael;i++)
					{
						if (dsp[i].type==0)
							ncl_sprintf (tempstr, &(dsp[i].value), 1);
						else if (dsp[i].type==1)
						{
							ncl_get_vocwrd((int)(dsp[i].value), tempstr);
						}
						else
						{
							strncpy(tempstr, dsp[i].label, NCL_MAX_LABEL); 
							tempstr[NCL_MAX_LABEL] = '\0';
/*
.....remove trailing spaces
*/
							len = strlen(tempstr);
							while ((len!=0) &&(tempstr[len-1]==' ')) len--;
								tempstr[len] = '\0';
							if (dsp[i].isub>0)
								sprintf (tempstr, "%s(%d)", tempstr, dsp[i].isub);
						}
						len = strlen (tempstr);
						dlist->data[itemnum].data_items[i+1] = (char*)uu_malloc((len+1)*sizeof(char));
						strcpy(dlist->data[itemnum].data_items[i+1], tempstr);
					}
/*
......fill in NULL for blank spaces
*/
					for (i=datarec.no_datael; i<dlist->num_col-1;i++)
					{
						dlist->data[itemnum].data_items[i+1] = UU_NULL;
					}
					(dlist->data[itemnum]).itemnum = datarec.no_datael + 1;
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
		for (i=0; i<dlist->num_item; i++)
		{
			if (dlist->data[i].itemnum>0)
			{
				if (dlist->data[i].data_items!=NULL)
					uu_free(dlist->data[i].data_items);
				dlist->data[i].data_items = NULL;
			}
		}
		if ((dlist->num_item)&&(dlist->data!=NULL))
			uu_free(dlist->data);
		dlist->data = NULL;
	}
	dlist->num_item = itemnum;
	return (itemnum);
}
/*********************************************************************
**    E_FUNCTION         :  ncl_update_data_frm()
**       update the scalar list 
**
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT : 
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_update_data_frm()
{
	UD_DLIST dlist;
	int i;

	if (Ssfrm==-1)
		return;
	ul_to_upper(Efilter);
	ul_to_upper(Sfilter);
	dlist.num_item = ncl_get_data_list(&dlist, Sfilter, Efilter);
	SelData[0] = '\0';

	ud_free_dlist(&Sdata_list);
	ud_dlist_copy(&dlist, &Sdata_list);
	ud_free_dlist(&dlist);
	ud_dispfrm_update_answer(Ssfrm, 6, (int *)&Sdata_list);

	resort_table();
	ud_update_form(Ssfrm);
}

/*********************************************************************
**    E_FUNCTION     : ncl_data_form ()
**       display a list form include all data and descriptions
**		if the user not cancel the form and select a data item, then
**		this scalar will be as the input of command prompt or 
**		active form field if any of them is active.
**		
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_data_form()
{
	int i, *ans[9], frmmode, temp, but;
	static UD_METHOD methods[] = {NULL, NULL, OnFilter1_but, NULL, OnFilter2_but, NULL, 
		OnDataCallbacks, OnApplyData, OnCancelData, OnCloseData};
	static char called[]       = {6,6,6,6,6,6,6,6,6,6};
	static char traverse[]     = {1,1,1,1,1,1,1,1,1,1,1};
	static char disp[]         = {1,1,1,1,1,1,1,1,1,1,1,1,1};
/*
......only allow one data-list form exist, otherwise, have problem
......when update the scalar from outside
*/
	if (Ssfrm!=-1)
		return;
/*
......save the active form ID before we display the scalarlist form
......so that we can update it's field
*/
	ud_save_actform_id();

	Sdata_list.num_item = ncl_get_data_list(&Sdata_list, Sfilter, Efilter);
/*
.....Added call to sorting routine to ensure labels are in order on first
.....open of form - ASF 10/24/13.
*/
	ud_dlist_sort(Sdata_list);
	SelData[0] = '\0';
	Sdata_ele[0] = '\0';
	Stype[0] = '\0';
	frmmode = uw_if_formopen();
	ans[0] = (int *)Sdata_ele;
	ans[1] = (int *)Stype;
	ans[2] = (int *) &but;
	ans[3] = (int *)Sfilter;
	ans[4] = (int *) &but;
	ans[5] = (int *)Efilter;
	ans[6] = (int *)&Sdata_list;
	ans[7] = (int *)&temp;
	ans[8] = (int *)&temp;

	strcpy(Sfilter_old, Sfilter);
	strcpy(Efilter_old, Efilter);
/*
.....Display the form
*/
	Ssfrm = ud_form_display1("datasel.frm", ans, ans, methods, called, disp,
		traverse);

	if (Ssfrm == -1) goto nofrm;
//	resort_table();
/*
.....Wait for the user to select a view
.....or cancel the form
*/
	Sfrm_cancel = 0;
	goto done;
nofrm:
	ud_wrerr("Could not load 'datasel.frm'.");
	Sfrm_cancel = -1;
	goto done;
/*
.....End of routine
*/
done:
	return;
}


/*********************************************************************
**    E_FUNCTION     : ctmpdatn(macnam, nc1, tmpnam, nc2)
**       Create a temp data name use macnam. 
**    PARAMETERS
**       INPUT  :
**				macnam  = Name of Macro
**				nc1      = # of chars in 'macnam'.
**       OUTPUT :
**				tmpnam  = temp data name.
**				nc2      = # of chars in 'tmpnam'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_tmpdatn(macnam, nc1, tmpnam, nc2)
char *macnam, *tmpnam;
int *nc1,*nc2;
{
	char tempname[65], dataname[65];
	char mac[65];
	struct NCL_datast_rec datarec;
	int i, j, entnum = 0, status = 0, matched = 0;
	strncpy(mac, macnam, *nc1);
	mac[*nc1] = '\0';
	datarec.rel_num = NCL_DATAST_REL;
	for (i=0; i<100;i++)
	{
		sprintf(tempname, "@%s%.2d", mac, i+1);
/*
.....check if this name is used for DATA
*/
		status = 0;
		matched = 0;
		entnum = 0;
		while (status == 0)
		{
			entnum++;
			status = ur_get_next_data_key(datarec.rel_num, &entnum,
								&datarec.key);
			if (status==0)
			{
				ncl_retrieve_data_fixed (&datarec);
				strcpy(dataname, datarec.label);
				j = strlen(dataname)-1;
				while ((dataname[j]==' ')||(j<0))
					j--;
				dataname[j+1] = '\0';
				if (stricmp(dataname, tempname)==0)
				{
					matched = 1;
					break;
				}
			}
		}
		if (matched==0)
		{
			strcpy(tmpnam, tempname);
			*nc2 = strlen(tempname);
			break;
		}
		else
			matched = 0;
	}
	if (i==100)
	{
		*nc2 = 0;
	}
}

void ncl_fix_data (datarec)
struct NCL_datast_rec *datarec;
{
	int i;
	struct NCL_datael_rec *dsp;
	dsp = datarec->datael;
	for (i=0; i<datarec->no_datael;i++)
	{
		if ((dsp[i].type==2)&&(dsp[i].value==24))
		{
/*
.....text string, the past version use value 24 for text string variable
.....which sometime cause error (when the variable value = 24)
.....we changed to use type 24 for text value, so we need handle the old unibase
*/
			dsp[i].type = 24;
		}
	}
	ur_update_data_fixed (datarec);
}
