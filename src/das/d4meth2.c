/*********************************************************************
**    NAME         :  d4meth2.c
**       CONTAINS:
**				ud_setfrm_display_mask
**				ud_setfrm_traverse_mask
**				ud_dispfrm_update_answer
**				ud_dispfrm_set_list
**				ud_dispfrm_update_prompt
**				ud_getfrm_field(frmid, fieldno, data)
**				ud_update_form
**				ud_dispfrm_update_label
**				ud_list_sort
**				ud_list_copy
**				ud_dispfrm_update_butlabel
**				ud_get_focusfrm
**				ud_dispfrm_set_color
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			d4meth2.c , 26.3
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:13:32
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
#include "ulist.h"

UD_METHOD UD_initfrm_intry = NULL;	
UD_METHOD UD_form_secsel = NULL;	

UD_FSTRUCT *UD_dispfrm[60] = 	
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };

UD_FDATA *UD_dispfdata[60] = 	
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
typedef struct
{
	char name[500];
} list_item;

void ud_dispfrm_set_list();
void ud_dispfrm_set_tlist();
void ud_dispfrm_set_color();

/*********************************************************************
**    E_FUNCTION : ud_setfrm_display_mask - Set display mask for a display
**															form field
**    PARAMETERS   
**       INPUT  :  type		:	UD_DISPLAYF or UD_INPUTF
**					fieldno	:	Field number of type 'type'
**					val		:	Value to set to: 1 = On, 0 = OFF
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_setfrm_display_mask(frmid, type, fieldno, val)
int type, fieldno, val, frmid;
{
	extern int ud_curfldno;

	uu_denter(UU_DTRC,(us,"ud_set_display_mask(%d %d %d)", type, fieldno, val));

	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;

	if(val == 1)
	{
		if (type == UD_DISPLAYF) 
		{
			if(UD_dispfrm[frmid]->ud_display_mask[fieldno] == 0)
			{
				UD_dispfrm[frmid]->ud_display_mask[fieldno] = 1;
			}
		}		
		else if(UD_dispfrm[frmid]->ud_display_mask[UD_dispfrm[frmid]->n_display_fields+fieldno] == 0)
		{
			UD_dispfrm[frmid]->ud_display_mask[UD_dispfrm[frmid]->n_display_fields+fieldno] = 1;

		}
	}	

	else
	{
		if (type == UD_DISPLAYF) 
		{
			if(UD_dispfrm[frmid]->ud_display_mask[fieldno] == 1)
			{
				UD_dispfrm[frmid]->ud_display_mask[fieldno] = 0;
			}
		}		
		else if(UD_dispfrm[frmid]->ud_display_mask[UD_dispfrm[frmid]->n_display_fields+fieldno] == 1)
		{
			UD_dispfrm[frmid]->ud_display_mask[UD_dispfrm[frmid]->n_display_fields+fieldno] = 0;

		}
	}
	uu_dexit;
}
/*********************************************************************
**    E_FUNCTION : ud_getfrm_traverse_mask - Get traverse mask for a form field
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number of type 'input'
**       OUTPUT :  none
**    RETURNS:      
**						 1 = traverse
**						 0 = Dont traverse
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ud_getfrm_traverse_mask(frmid, fieldno)
int fieldno, frmid;					
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return -1;

	return (UD_dispfrm[frmid]->traverse_mask[fieldno]);
}

/*********************************************************************
**    E_FUNCTION : ud_setfrm_traverse_mask - Set traverse mask for a form field
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number of type 'input'
**						 val		:	Value to set to: 1 = traverse
**															  0 = Dont traverse
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_setfrm_traverse_mask(frmid, fieldno, val)
int frmid, fieldno;					/* Input field number	*/
int val;	
{
	uu_denter(UU_DTRC,(us,"ud_set_traverse_mask(fno = %d, val=%d)",
			fieldno, val));
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;

	UD_dispfrm[frmid]->traverse_mask[fieldno] = val;
	uu_dexit;
}
/*********************************************************************
**    E_FUNCTION : ud_dispfrm_update_frmpic 
**				update a picture field with new image file
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_update_frmpic(frmid, fieldno, val)
int frmid, fieldno;
int *val;
{
	char fname[UX_MAX_FILE_LEN];
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	strcpy(UD_dispfrm[frmid]->picture_flds[fieldno].fname, (char*)val);
	UD_dispfrm[frmid]->picture_flds[fieldno].upt_flag = 1;
}

/*********************************************************************
**    E_FUNCTION : ud_dispfrm_update_answer 
**				update a field with a value
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_update_answer(frmid, fieldno, val)
int frmid, fieldno;
int *val;
{
	UD_LIST *form_list;
	UD_TLIST *table_list;
	UD_DLIST *data_list;
	char *text, strbuf[80], erms[256];
	int *choice, stat;
	UU_REAL *fval;
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	if ((UD_dispfrm[frmid]->input_flds[fieldno].toggle == 5)
		|| (UD_dispfrm[frmid]->input_flds[fieldno].toggle == 10))
	{
		form_list = (UD_LIST*)val;
		ud_dispfrm_set_list(frmid, fieldno, form_list);
	}
	else if (UD_dispfrm[frmid]->input_flds[fieldno].toggle == 17)
	{
		table_list = (UD_TLIST*)val;
		ud_dispfrm_set_tlist(frmid, fieldno, table_list);
	}
	else if (UD_dispfrm[frmid]->input_flds[fieldno].toggle == 18)
	{
		ud_dispfrm_set_color(frmid, fieldno, *val);
	}
	else if (UD_dispfrm[frmid]->input_flds[fieldno].toggle == 19)
	{
		data_list = (UD_DLIST*)val;
		ud_dispfrm_set_tlist(frmid, fieldno, data_list);
	}
	else if ((UD_dispfrm[frmid]->input_flds[fieldno].toggle == 1)
			||(UD_dispfrm[frmid]->input_flds[fieldno].toggle == 8))
	{
		switch (UD_dispfrm[frmid]->input_flds[fieldno].ud_datatyp)
		{
			case UD_DASCART:
				UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmvec = (UU_REAL*)val;
				break;
			case UD_DASVAL:
			case UD_DASUNITLESS:
				fval = (UU_REAL*)val;
				UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmflt[0] = *fval;
				break;
			case UD_DASDISTANCE:
				fval = (UU_REAL*)val;
				UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmflt[0] = *fval;
				break;
			case UD_DASINT:
				UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmint[0] = *val;
				break;
			case UD_DASVEC:
				UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmflt = (UU_REAL*)val;
				break;
			case UD_DASSCALAR:
			case UD_DASSTRING:
				text = (char*)val;
				strcpy(UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmstr, text);
				break;
			case UD_DASNDC:
				UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmflt = (UU_REAL*)val;
				break;
			case UD_DASANGLE:
				fval = (UU_REAL*)val;
				UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmflt[0] = *fval;
				break;
			case UD_SCACART:
			case UD_SCAVAL:
			case UD_SCAUNITLESS:
			case UD_SCADISTANCE:
			case UD_SCAINT:
			case UD_SCAVEC:
			case UD_SCANDC:
			case UD_SCAANGLE:
				text = (char*)val;
/*
.....check if the string is valid type scalar string
*/
				stat = ncl_parse_scalar_values(text, strbuf, 
							UD_dispfrm[frmid]->input_flds[fieldno].ud_datatyp);
				if (stat!=-1)
				{
/*
.....all scalar treat as string, so the format is always following input string if no error
*/
/*					strcpy(UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmstr, strbuf); */
					strcpy(UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmstr, text);
				}
				else
				{
/*
.....we will allow empty string for  SCA* value
*/
					if (text[0]!='\0')
					{	
						sprintf(erms,"%s is not a valid scalar value",text);
						ud_wrerr(erms);
						return;
					}
					else
						UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmstr[0] = '\0';
				}
				break;
		}
/*
.....dflg is used for default answer, it can be 0 or 1 initially.
.....But it never used after form displayed. we need a flag to set
.....if this field need reset on the form, so we pich this flag and
.....set to 2
.....Yurong 6/17/98
*/
		UD_dispfdata[frmid]->ud_data[fieldno].dflg = 2;
	}
	else if (UD_dispfrm[frmid]->input_flds[fieldno].toggle == 2 ||
		UD_dispfrm[frmid]->input_flds[fieldno].toggle == 3)
	{
		choice = (int*)val;
		UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmint[0] = *choice;
	}
	else if (UD_dispfrm[frmid]->input_flds[fieldno].toggle == 7)
	{
		UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmint = (int*)val;
		UD_dispfdata[frmid]->ud_data[fieldno].dflg = 2;
	}
	else if (UD_dispfrm[frmid]->input_flds[fieldno].toggle == 8)
	{
		UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmint = (int*)val;
		UD_dispfdata[frmid]->ud_data[fieldno].dflg = 2;
	}
	else if (UD_dispfrm[frmid]->input_flds[fieldno].toggle == 9)
	{
		UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmint = (int*)val;
		UD_dispfdata[frmid]->ud_data[fieldno].dflg = 2;
	}
}  

void ud_update_videofile(frmid, fieldno, val)
int frmid, fieldno;
int *val;
{
	char *text;
	if (UD_dispfrm[frmid]->input_flds[fieldno].toggle != 25)
		return;
	if (UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmstr==UU_NULL)
		return;
	text = (char*)val;
	strcpy(UD_dispfdata[frmid]->ud_data[fieldno].ud_delem.frmstr, text);
}

/*********************************************************************
**    E_FUNCTION : ud_dispfrm_set_tlist(fieldno, list)
**			reset field fieldno use input new list
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**						 list : UD_LIST structure 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_set_list(frmid, fieldno, form_list)
int frmid, fieldno;
UD_LIST *form_list;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;

	(*(ug_gksstli.wsopen[0].connid)[UW_DISPFRM_SET_LIST])(frmid, fieldno, form_list);
}

/*********************************************************************
**    E_FUNCTION : ud_dispfrm_set_tlist(fieldno, list)
**			reset field fieldno use input new list
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**						 list : UD_LIST structure 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_set_tlist(frmid, fieldno, form_list)
int frmid, fieldno;
UD_LIST *form_list;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;

	(*(ug_gksstli.wsopen[0].connid)[UW_DISPFRM_SET_TLIST])(frmid, fieldno, form_list);
}

/*********************************************************************
**    E_FUNCTION : ud_getfrm_field(frmid, fieldno, data, str_flag)
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
.....Yurong
*/
void ud_getfrm_field(frmid, fieldno, data, str_flag)
int frmid, fieldno;
UD_DDATA data;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;

	(*(ug_gksstli.wsopen[0].connid)[UW_GETFRM_FIELD])(frmid, fieldno, data,
		str_flag);
}
/*********************************************************************
**    E_FUNCTION : ud_dispfrm_update_prompt 
**				update a field prompt with a value
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**					 :	 prompt:    Prompt string
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_update_prompt(frmid, fieldno, prompt)
int frmid, fieldno;
char *prompt;
{
	int len;
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;

/*
.....not for button, button label change should by value
*/
	if (UD_dispfrm[frmid]->input_flds[fieldno].toggle == 6)
		return;
/*
.....free old prompt memory and locate new (because the new prompt
.....string may bigger
*/
	if (UD_dispfrm[frmid]->input_flds[fieldno].prompt!=NULL)
		uu_free(UD_dispfrm[frmid]->input_flds[fieldno].prompt);
	len = strlen(prompt);
	UD_dispfrm[frmid]->input_flds[fieldno].prompt = (char*)uu_malloc(sizeof(char)*(len+1));
	strcpy(UD_dispfrm[frmid]->input_flds[fieldno].prompt, prompt);
/*
.....ud_echo for old form capatability before, and used for button to reset
.....correspond text field when ud_echo = 1
.....Now, I will used ud_echo = 2 to mean that we need reset label
.....Yurong 5/3/00
*/
	UD_dispfrm[frmid]->input_flds[fieldno].ud_echo = 2;
}


/*********************************************************************
**    E_FUNCTION : ud_update_form(frmid)
**			update the form display
**    PARAMETERS   
**       INPUT  :  frmid: form ID
**				
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_update_form(frmid)
int frmid;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;

	(*(ug_gksstli.wsopen[0].connid)[UW_UPDATE_FRM])(frmid);
}
/*********************************************************************
**    E_FUNCTION : ud_dispfrm_update_label 
**				update a label field with a value
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**					text:    label string
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_update_label(frmid, fieldno, text)
int frmid, fieldno;
char *text;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_DISPFRM_SET_LABEL])(frmid, fieldno, text);	
}
/*********************************************************************
**    E_FUNCTION : ud_form_sorttable 
**				update a label field with a value
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**					text:    label string
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_form_sorttable(info, sortfunc)
UD_TABLEINFO *info;
UD_SMETHOD sortfunc;
{
	if (info->frmid==-1)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_FORM_SORTTABLE])(info, sortfunc);	
}


/*********************************************************************
**    E_FUNCTION : ud_list_sort(list)
**				sort the UD_LIST in alphabetical order
**    PARAMETERS   
**       INPUT  :  list: list to be sort
**					
**       OUTPUT :  list: list sorted
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_list_sort(list)
UD_LIST *list;
{
	int i,j,nc, add;
	int list_count;
	UU_LIST itemlist;
	char tempstr1[500], tempstr2[500];
	list_item item, *item_array;

	list_count = 0;
	uu_list_init(&itemlist, sizeof(list_item), 100, 100);
	for (i=0; i<list->num_item;i++)
	{
		strcpy(item.name, list->item[i]);
		if (i==0)
		{
			uu_list_push(&itemlist, &item);
			list_count++;
		}
		else
		{
			item_array = (list_item *) UU_LIST_ARRAY (&itemlist);
			add = 0;
			for (j=list_count-1; j>=0; j--)
			{
				nc = strlen(item.name);
				if (nc>(int)strlen(item_array[j].name))
					nc =  strlen(item_array[j].name);
				strcpy(tempstr1, item.name);
				ul_to_upper(tempstr1);
				strcpy(tempstr2, item_array[j].name);
				ul_to_upper(tempstr2);
				if (strcmp(tempstr1, tempstr2)<=0)
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
	item_array = (list_item *)UU_LIST_ARRAY(&itemlist);
	for (i=0; i<list->num_item;i++)
	{
		strcpy(list->item[i],item_array[list->num_item-i-1].name);
	}
	uu_list_free(&itemlist);
}

/*********************************************************************
**    E_FUNCTION : ud_list_copy(list1, list2)
**				copy the UD_LIST strcuture
**    PARAMETERS   
**       INPUT  :  list1: list to be copied
**					
**       OUTPUT :  list2: list copied
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_list_copy(list1, list2)
UD_LIST *list1, *list2;
{
	int i, len;
	if (list1->num_item==0)
	{
		list2->num_item = 0;
		return;
	}
	list2->num_item = list1->num_item;
	list2->item = (char**)uu_malloc(list2->num_item * sizeof (char*));
	for (i=0; i<list1->num_item;i++)
	{
		len = strlen (list1->item[i]);
		list2->item[i] = (char*)uu_malloc((len+1)* sizeof (char));
		strcpy(list2->item[i], list1->item[i]);
	}
	list2->answer = (char*)uu_malloc((len+1)* sizeof (char));
	strcpy(list2->answer, list1->answer);
}
/*********************************************************************
**    E_FUNCTION : ud_dispfrm_update_butlabel 
**				update a button label
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**					text:    label string
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_update_butlabel(frmid, fieldno, text)
int frmid, fieldno;
char *text;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_DISPFRM_SET_BUTLABEL])(frmid, fieldno, text);	
}
/*********************************************************************
**    E_FUNCTION : ud_get_focusfrm(frmid, fieldno)
**				Get current active form and focus field
**    PARAMETERS   
**       INPUT  :  none
**			
**       OUTPUT :  fieldno	:	Field number
**					frmid:    form ID
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_get_focusfrm(frmid, fieldno)
int *frmid, *fieldno;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_GET_FOCUSFRM])(frmid, fieldno);	
}
/*********************************************************************
**    E_FUNCTION : ud_setform_sortfunc(frmid, fieldno, (UD_SMETHOD)SortFunc);
**			set the form field sort function for table list only
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**						 list : UD_LIST structure 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_setform_sortfunc(frmid, fieldno, SortFunc)
int frmid, fieldno;
UD_SMETHOD SortFunc;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_SETFORM_TFUNC])(frmid, fieldno, SortFunc);
}
/*********************************************************************
**    E_FUNCTION : ud_dispfrm_set_focus(fieldno, list)
**			reset field fieldno use input new list
**    PARAMETERS   
**       INPUT  :  frmid, fieldno	:	form id and Field number
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_set_focus(frmid, fieldno)
int frmid, fieldno;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_DISPFRM_SET_FOCUS])(frmid, fieldno);
}

/*********************************************************************
**    E_FUNCTION : ud_dispfrm_set_color(frmid, fieldno, color)
**			reset field fieldno use input new color
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**					color : color to be set
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_set_color(frmid, fieldno, color)
int frmid, fieldno;
int color;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_DISPFRM_SET_COLOR])(frmid, fieldno, color);
}
/*********************************************************************
**    E_FUNCTION : ud_form_section_enable(int frmid, char *sec_name, int enable_flag)
**			enable/disable the current form section button
**    PARAMETERS   
**       INPUT  :   frmid: form index number
**					sec_name	:	section button name
**					enable_flag: 1: enable the section
**								0: disable the section
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_form_section_enable(frmid, sec_name, enable_flag)
char *sec_name;
int frmid, enable_flag;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_FRMSEC_ENABLE])(frmid, sec_name, enable_flag);
}

/*********************************************************************
**    E_FUNCTION : ud_form_section_color(int frmid, char *sec_name, int color)
**			set the current form section button text color
**    PARAMETERS   
**       INPUT  :  frmid: form index number
**					sec_name	:	section button name
**					color: color index to be set
**					
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_form_section_color(frmid, sec_name, color, bold)
char *sec_name;
int frmid, color[3], bold;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_FRMSEC_SET_COLOR])(frmid, sec_name, color, bold);
}

/*********************************************************************
**    E_FUNCTION : ud_form_section_active(char *sec_name)
**			Set the current form active section
**    PARAMETERS   
**       INPUT  :  frmid: form index number
**					sec_name	:	section button name
**					
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_form_section_active(frmid, sec_name)
int frmid;
char *sec_name;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_FRMSEC_ACTIVE])(frmid, sec_name);
}
/*********************************************************************
**    E_FUNCTION : ud_dispfrm_set_init_datatyp(frmid, n, data_type)
**			Set the pass in form data type
**			sometime, we need form routine know the pass in default type
**			in order for the form to set the correct default data
**    PARAMETERS   
**       INPUT  :  frmid	:	Form ID number, start from 0
**				n: total number setting in data_type
**				data_type: data array of the data type
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_set_init_datatyp(frmid, n, data_type)
int frmid, n;
int *data_type;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_DISPFRM_SET_INIT_DATATYP])(frmid, n, data_type);
}

/*********************************************************************
**    E_FUNCTION : ud_dispfrm_get_form_datatyp(frmid, n, data_type)
**			Get the data type used in the form.
**			sometime, we need know the current form data type
**			in order for save the correct input data
**    PARAMETERS   
**       INPUT  :  frmid	:	Form ID number, start from 0
**				n: total number setting in data_type
**       OUTPUT :  
**				data_type: data array to save the data type
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_get_form_datatyp(frmid, n, data_type)
int frmid, n;
int *data_type;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_DISPFRM_GET_FORM_DATATYP])(frmid, n, data_type);
}

/*********************************************************************
**    E_FUNCTION : ud_dispfrm_set_attribs(frmid, fieldno, fg, bg)
**			reset field fieldno new foreground and background color
**    PARAMETERS   
**       INPUT  :  frmid: form ID/index
**				fieldno	:	Field number
**				fg, bg : new foreground and background color index 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_set_attribs(frmid, fieldno, fg, bg)
int frmid, fieldno;
int fg, bg;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;

	(*(ug_gksstli.wsopen[0].connid)[UW_DISPFRM_SET_ATTR])(frmid, fieldno, fg, bg);
}
void ud_frm_enable_ok(flag)
UU_LOGICAL flag;
{
	if (UD_dispfrm[0] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_FRM_ENABLE_OK])(flag);
}

void ud_frm_enable_close(frmid, flag)
UU_LOGICAL flag;
{
	if (UD_dispfrm[frmid] == UU_NULL)
		return;
	(*(ug_gksstli.wsopen[0].connid)[UW_FRM_ENABLE_CLOSE])(frmid,flag);
}

/*********************************************************************
**    E_FUNCTION : ud_dispfrm_show_frmpic 
**				show/hide a picture field
**    PARAMETERS   
**       INPUT  :  fieldno	:	Field number
**					val = 1: show the picture field
**						= 0: hide the picture field
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_dispfrm_show_frmpic(frmid, fieldno, val)
int frmid, fieldno;
int val;
{
	if (frmid==-1 || UD_dispfrm[frmid] == UU_NULL)
		return;
	UD_dispfrm[frmid]->picture_flds[fieldno].show_flag = val;
}

