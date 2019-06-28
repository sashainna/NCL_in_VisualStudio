/*********************************************************************
**    NAME         :  nsecondu.c
**       CONTAINS:
**     int nclu_open_usecond()
**     int nclu_close_usecond()
**     int nclu_put_layer()
**     int nclu_put_geo_type()
**     int nclu_get_allgeo()
**		nclu_get_allgeo_rename()
**     int nclu_get_geo();
**     int nclu_get_geo_rename();
**		nclu_get_geo_thru()
**		nclu_get_geo_thru_re()
**		nclu_get_layer()
**		nclu_get_type()
**		nclu_put_allgeo()
**		nclu_put_geo()
**		nclu_put_geo_thru()
**		nclu_get_data
**		nclu_put_data
**
**    COPYRIGHT 2005 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nsecondu.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:02
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "nclfc.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "xenv1.h"
#include "dselect.h"
#include "view.h"
#include "mxxx.h"
#include "udforms.h"
#include "udfdata.h"
#include "nclvx.h"
#include "lcom.h"

#define MAXTYPE 24

static int name_click1 = 0;
static int value_click1 = 0;
static int name_click2 = 0;
static int value_click2 = 0;

static int S_load_layers(), S_load_2layers();
static int S_load_list();
static void S_layer_command();
static UD_FSTAT filter_data();
void ncl_get_delim();
void ncl_get_vocwrd();

static int Ssel,Snlayer,Smax,Sactive,Scur_active;
static char S_layer_selstr1[80], S_layer_selstr2[80];
static char Scmd[80];
static char *Slayer_ptr;
static struct UM_layer_rec Scur;

static char S_type_selstr1[40], S_type_selstr2[40];
static UD_ITEMDATA S_data_seldata1, S_data_seldata2;
static char filter[100];
static int uni_flag = 1;
static UU_LIST Slayer;
static UD_LIST Sin_layer, Sout_layer;
static UD_LIST Sin_type, Sout_type;
static UD_TLIST Sin_data, Sout_data;
static UD_LIST sclass_list;
static UD_FSTAT OnGet(), OnRemove(), OnThru(), OnSelect1(), OnSelect2();
static int S_layer_start=-1, S_layer_end=-1, S_layer_sel1=-1, S_layer_sel2=-1;
static int S_layer_thru = 0;
extern int UR_active;
extern char UBopen[100];
void add_list();
extern UD_METHOD UD_initfrm_intry;
extern UU_KEY_ID UR_low_key;
char **ncl_get_typename();
/*
.....save the label in case we need compare if the label is duplicate
*/
static char* ent_labels[20000];
static int labels_num = 0;

static void init_labels()
{
	int i;
	for (i=0; i<20000; i++)
		ent_labels[i] = NULL;
	labels_num = 0;
}

static void add_label (label)
char *label;
{
	int len = strlen (label) + 1;
	ent_labels[labels_num] = (char*) uu_malloc (len*sizeof (char));
	strcpy(ent_labels[labels_num], label);
	labels_num++;
}

static int is_duplicate_label(label)
char *label;
{
	int i;
	for (i=0; i<labels_num; i++)
	{
		if (strcmp(ent_labels[i], label)==0)
			return 1;
	}
	return 0;
}

static void free_labels()
{
	int i;
	for (i=0; i<labels_num; i++)
	{
		if (ent_labels[i]!=NULL)
			uu_free (ent_labels[i]);
		ent_labels[i] = NULL;
	}
	labels_num = 0;
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
static UD_FSTAT S_init_layerpfrm(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_dispfrm_update_label(0, 0, "Select Layers to Put");
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : nclu_open_usecond
**       Open secondary unibase.
**    PARAMETERS   
**       INPUT  : ufile: unibase file to be opend
**                
**       OUTPUT :  
**						None
**    RETURNS      : 
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_open_usecond(ufile)
char *ufile;
{
	NCL_cmdbuf cmdbuf;
	int status,length, len;
	char filename[UX_MAX_PATH_LEN], lbuf[NCL_MAX_COMLINE];
	UX_pathname ext,descrip;
	char *p, *ux_getenv(), ext1[UX_SUFFIX_LEN];
/*
.....Initialize the command buffer.
*/
	status = UU_SUCCESS;
	ncl_init_cmdbuf(&cmdbuf);
	if ((ufile!=NULL) && (ufile[0]!='\0'))
		strcpy(filename, ufile);
	else
	{
		strcpy(ext,"*.");
		strcpy(descrip, "Unibase Files (");
		p = ux_getenv("UR_PART_SUFFIX");
		if (p != UU_NULL)
		{
			strcpy(ext1,p);
			ul_remove_quotes(ext1);
			strcat(ext,ext1);
		}       
		else 
			strcat(ext,"u");

		strcat(descrip, ext);
		strcat(descrip, ")|Textual Unibase Files (*.");
		p = ux_getenv("UR_ASCII_PART");
		strcat(ext,"|*.");
		if (p != UU_NULL)
		{
			strcpy(ext1,p);
			ul_remove_quotes(ext1);
			strcat(ext,ext1);
			strcat(descrip, ext1);
		}       
		else
		{
			strcat(ext,"ud");
			strcat(descrip, "ud");
		}
		strcat(descrip, ")");
		filename[0] = '\0';
		ud_get_filename("Enter Secondary Unibase Name",
			"Enter Secondary Unibase Name", ext,
			filename, &length, descrip, 1, UU_TRUE);
		if (length==0)
			return status;
	}
	strcpy(lbuf,"UBFN/");
	len = strlen(filename);
/*
.....ncl_add_token define lbuf (l_cmd_str) as UL_line_len chars
.....can't more than that
*/
/*
	if (len+5>UL_line_len-1)
	{
		ud_printmsg("Internal error!");
		return UU_FAILURE;
	}
*/
	strcat(lbuf,filename);
	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : nclu_close_usecond
**       close secondary unibase.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_close_usecond()
{
	NCL_cmdbuf cmdbuf;
	char lbuf[80];
/*
.....Initialize the command buffer.
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lbuf,"UBFN/close");	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
}

/*********************************************************************
**    E_FUNCTION     : nclu_put_layer
**       put segments into secondary unibase by layer.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_put_layer()
{
	NCL_cmdbuf cmdbuf;
	int i, layer_num1, layer_num2;
	char temp[80], *tok, *strtok();
	char lbuf[80];
	int status, *ans[7];
	UD_METHOD save_entry;
	int mainmarkval=0;
	static UD_METHOD methods[] =
	{
		NULL, NULL, OnSelect1, OnSelect2, OnGet, OnRemove, OnThru
	};
	static char called[]       = {6,6,6,6, 6,6,6};
	static char traverse[]     = {1,0,1,1,1, 1,1};
	static char disp[]         = {1,0,0,1,1, 1,1,1};
	static int rename = 0, offset = 0;

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
/*
.....Initialize routine
*/
	Sin_layer.item = UU_NULL;
	Ssel = 0;
	Sactive = 0;
/*
.....Load the layers into a list
*/
	status = S_load_layers();
	if (status != UU_SUCCESS) goto nomem;
/*
.....Load the layer list into the form
*/
	status = S_load_list();
	if (status != UU_SUCCESS) goto nomem;

	Sout_layer.num_item = 1;
	Sout_layer.item = (char **)uu_malloc(sizeof (char*));
	Sout_layer.item[0] = (char *)uu_malloc(80*sizeof (char));
	Sout_layer.answer = (char *)uu_malloc(80*sizeof (char));
	Sout_layer.item[0][0] = '\0';
	Sout_layer.answer[0] = '\0';

	ans[0] = (int *)&rename;
	ans[1] = (int *)&offset;
	ans[2] = (int *)&Sin_layer;
	ans[3] = (int *)&Sout_layer;
	ans[4] = NULL;
	ans[5] = NULL;
	ans[6] = NULL;

	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = S_init_layerpfrm;
/*
.....Command Reject
*/
	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		UD_initfrm_intry = save_entry;
		ud_free_flist(&Sin_layer);
		ud_free_flist(&Sout_layer);
		UD_UNMARK (mainmarkval);
		return;
	}
	status = ud_form1("layertran.frm",ans,ans,methods,called,disp,traverse);
	UD_initfrm_intry = save_entry;
	ud_free_flist(&Sin_layer);
	if (status == -1) 
	{
		ud_free_flist(&Sout_layer);
		goto done;
	}
/*
.....Initialize the command buffer.
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lbuf,"PUT/");	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);
	ncl_add_token(&cmdbuf, "LAYER", NCL_comma);

	for (i=0; i<Sout_layer.num_item; i++)
	{
		layer_num1 = layer_num2 = -1;
		strcpy(temp, Sout_layer.item[i]);
		tok = strtok(temp, " ");
		if (tok!=NULL)
		{
			layer_num1 = atoi(tok);
			tok = strtok(NULL, " ");
			if ((tok==NULL) || (strcmp(tok, "THRU")!=0))
				layer_num2 = -1;
			else
			{
				tok = strtok(NULL, " ");
				if (tok!=NULL)
					layer_num2 = atoi(tok);
				else
					layer_num2 = -1;
			}	
		}
		if (layer_num1>=0)
		{
			sprintf (lbuf, "%d", layer_num1);
			ncl_add_token(&cmdbuf, lbuf, NCL_comma);
		}
		if (layer_num2>=0)
		{
			sprintf (lbuf, "THRU,%d", layer_num2);
			ncl_add_token(&cmdbuf, lbuf, NCL_comma);
		}
	}
	ncl_del_token(&cmdbuf,"", UU_TRUE);
	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
	ud_free_flist(&Sout_layer);
	goto done;
/*
.....Could not allocate memory
*/
nomem:
	ud_wrerr("Could not allocate memory for form.");
	goto done;
done:;
	UD_UNMARK (mainmarkval);
}
/*********************************************************************
**   I_FUNCTION: OnGetType(fieldno,val,stat)
**      Callback function for the "->" button in select geometry type form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnGetType(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,same;
	UD_LIST out_type;

	if (S_type_selstr1[0]=='\0')
	{
		*fieldno = -1;
		goto done;
	}
	if ((Sout_type.num_item==1) && (Sout_type.item[0][0]=='\0'))
		out_type.num_item = 1;
	else
		out_type.num_item = Sout_type.num_item + 1;

	out_type.item = (char **)uu_malloc(out_type.num_item*sizeof (char*));
	j = 0;
	same = 0;
	out_type.answer = (char *)uu_malloc(40*sizeof (char));
	for (i=0; i<Sout_type.num_item; i++)
	{
		out_type.item[j] = (char *)uu_malloc(40*sizeof (char));
		if ((i==0) && (Sout_type.num_item==1)&&(Sout_type.item[0][0]=='\0'))
		{
			break;
		}
		else
		{
			if (strcmp(Sout_type.item[i], S_type_selstr1)==0)
			{
				same = 1;
				break;
			}
			else
				strcpy(out_type.item[j++], Sout_type.item[i]);
		}
	}
	if (same)
		goto update;
	out_type.item[j] = (char *)uu_malloc(40*sizeof (char));
	strcpy(out_type.item[j++], S_type_selstr1);	
	ud_free_flist(&Sout_type);
	Sout_type.num_item = j;
	Sout_type.item = (char **)uu_malloc(Sout_type.num_item*sizeof (char*));
	Sout_type.answer = (char *)uu_malloc(40*sizeof (char));
	Sout_type.answer[0] = '\0';

	for (i=0; i<Sout_type.num_item; i++)
	{
		Sout_type.item[i] = (char *)uu_malloc(40*sizeof (char));
		strcpy(Sout_type.item[i], out_type.item[i]);
	}
	ud_free_flist(&out_type);
	ud_list_sort(&Sout_type);
update:;
	strcpy(Sout_type.answer, S_type_selstr1);
	strcpy(S_type_selstr2, S_type_selstr1);
	ud_update_answer(2, (int*)&Sout_type);
done:;
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnRemoveType(fieldno,val,stat)
**      Callback function for the "<-" button in select geometry type form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnRemoveType(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j;
	UD_LIST out_type;

	if ((S_type_selstr2[0]=='\0') ||
		((Sout_type.num_item==1)&&(Sout_type.item[0][0]=='\0')))
	{
		*fieldno = -1;
		goto done;
	}
	out_type.num_item = Sout_type.num_item - 1;
	if (out_type.num_item==0)
	{
		Sout_type.item[0][0] = '\0';
		Sout_type.answer[0] = '\0';
		goto update;
	}
	out_type.item = (char **)uu_malloc(out_type.num_item*sizeof (char*));
	out_type.answer = (char *)uu_malloc(40*sizeof (char));
	j = 0;
	for (i=0; i<Sout_type.num_item; i++)
	{
		out_type.item[j] = (char *)uu_malloc(40*sizeof (char));
		if (strcmp(S_type_selstr2, Sout_type.item[i])==0)
		{
			if (i-1>=0)
				strcpy(out_type.answer, Sout_type.item[i-1]);
			else
				out_type.answer[0] = '\0';
			continue;
		}
		else
		{
			strcpy(out_type.item[j++], Sout_type.item[i]);
		}
	}
	ud_free_flist(&Sout_type);
	Sout_type.num_item = j;
	Sout_type.item = (char **)uu_malloc(Sout_type.num_item*sizeof (char*));
	Sout_type.answer = (char *)uu_malloc(40*sizeof (char));
	Sout_type.answer[0] = '\0';

	for (i=0; i<Sout_type.num_item; i++)
	{
		Sout_type.item[i] = (char *)uu_malloc(40*sizeof (char));
		strcpy(Sout_type.item[i], out_type.item[i]);
	}
	strcpy(Sout_type.answer, out_type.answer);
	ud_free_flist(&out_type);
update:;
	ud_update_answer(2, (int*)&Sout_type);
done:;
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnSelectType1(fieldno,val,stat)
**      Callback function for the "Geometry Types" list.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelectType1(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	strcpy(S_type_selstr1, val->frmstr);
	*fieldno = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnSelectType2(fieldno,val,stat)
**      Callback function for the "Transfer Types" list.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelectType2(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	strcpy(S_type_selstr2, val->frmstr);
	*fieldno = -1;
	return(UD_FLDOK);
}
/*********************************************************************
**    E_FUNCTION     : nclu_put_geo_type
**       put segments into secondary unibase by geometry type.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_put_geo_type()
{
	NCL_cmdbuf cmdbuf;
	int i;
	char *strtok();
	char lbuf[80];
	int status, *ans[5];
	static UD_METHOD methods[] =
	{
		UU_NULL, OnSelectType1, OnSelectType2, OnGetType, OnRemoveType
	};
	static char called[]       = {6,6,6, 6,6,};
	static char traverse[]     = {1,1,1, 1,1,};
	static char disp[]         = {1,0,1, 1,1,1};
	static int rename = 0;
	int mainmarkval=0;

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
/*
.....Initialize routine
*/
	Sin_type.item = UU_NULL;
	S_type_selstr1[0]='\0';
	S_type_selstr2[0]='\0';

	Sin_type.item = ncl_get_typename(&(Sin_type.num_item), 1);
	if (Sin_type.num_item==0)
	{
		Sin_type.num_item = 1;
		Sin_type.item[0] = (char *)uu_malloc(40*sizeof (char));
		Sin_type.item[0][0] = '\0';
	}
	ud_list_sort(&Sin_type);
	Sin_type.answer = (char *) uu_malloc(40 * sizeof(char));
	strcpy(Sin_type.answer, Sin_type.item[0]);
	strcpy(S_type_selstr1,  Sin_type.item[0]);

	Sout_type.num_item = 1;
	Sout_type.item = (char **)uu_malloc(sizeof (char*));
	Sout_type.item[0] = (char *)uu_malloc(40*sizeof (char));
	Sout_type.answer = (char *)uu_malloc(40*sizeof (char));
	Sout_type.item[0][0] = '\0';
	Sout_type.answer[0] = '\0';

	ans[0] = (int*)&rename;
	ans[1] = (int *)&Sin_type;
	ans[2] = (int *)&Sout_type;
	ans[3] = NULL;
	ans[4] = NULL;
/*
.....Command Reject
*/
	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		ud_free_flist(&Sin_type);
		ud_free_flist(&Sout_type);
		UD_UNMARK (mainmarkval);
		return;
	}
	status = ud_form1("typetran.frm",ans,ans,methods,called,disp,traverse);
	ud_free_flist(&Sin_type);
	if (status == -1) 
	{
		ud_free_flist(&Sout_type);
		goto done;
	}
/*
.....Initialize the command buffer.
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lbuf,"PUT/");	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);		

	for (i=0; i<Sout_type.num_item; i++)
	{
		ncl_add_token(&cmdbuf, Sout_type.item[i], NCL_comma);
	}
	if ((Sout_type.num_item>=1) && (Sout_type.item[0][0]!='\0'))
	{
		ncl_del_token(&cmdbuf,"", UU_TRUE);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
	ud_free_flist(&Sout_type);
	goto done;
done:;
	UD_UNMARK (mainmarkval);
}
/*********************************************************************
**    E_FUNCTION     : nclu_get_allgeo
**       get all geometries from secondary unibase.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_get_allgeo()
{
	NCL_cmdbuf cmdbuf;
	char lbuf[80];

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
/*
.....Initialize the command buffer.
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lbuf,"GET/all");	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_set_cmdmode(UU_TRUE);
	ncl_call(&cmdbuf);
}
/*********************************************************************
**    E_FUNCTION     : nclu_get_geo
**       get the geometries from secondary unibase.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_get_geo()
{
	NCL_cmdbuf cmdbuf;
	int status, stype;
	int second_view;
	char lbuf[80];
	unsigned long mask[4];
	int cmdmark;
	int numint;
	UU_LOGICAL found, lstatus;
	struct UC_entitydatabag e1;
	char str[256];

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
	mask[0] = mask[1] = mask[2] = mask[3] = -1;
	second_view = uv_secondv_act();

	stype = ud_getpick_type();
	ud_setpick_type(UD_PICK_SECONDARY);
	UD_MARK(cmdmark,UU_FALSE);
	if (cmdmark == 0)
	{
/* 
......pick geometry to get 
*/
		if (second_view==0)
		{
			while (UU_TRUE)
			{
				ncl_init_cmdbuf(&cmdbuf);
				strcpy(lbuf,"GET/");	
				status = ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);
				status = ncl_add_label(UD_DASSTRING, &cmdbuf, 661, mask);
				if (status != NCL_OKINPUT)
					goto done;
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
				ud_setpick_type(UD_PICK_SECONDARY);
			}
		}
		else
		{
			init_labels();
			ud_lgeo(UU_TRUE, mask);
			ud_setpick_type(UD_PICK_SECONDARY);
			status = ud_ldas(UD_DASSELECT, UA_NCL, 661, UU_NULL, NCL_MAXPICK, &numint, UD_NODEFAULT);
			if (status == UU_ALTACTION)
			{
				status = NCL_OKINPUT;
				while (status == NCL_OKINPUT)
				{
					ncl_init_cmdbuf(&cmdbuf);
					strcpy(lbuf,"GET/");	
					status = ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);
					status=ncl_add_str(&cmdbuf, 511, NCL_comma);
					if (status == NCL_DONE)
					{
						ncl_add_cmdbuf(&cmdbuf);
						ncl_call(&cmdbuf);
						goto done;
					}
				}
			}
			lstatus = ud_gnxt(UU_TRUE, UU_NULL, &e1.key, 1);
			while (lstatus == UU_TRUE)
			{
				ncl_init_cmdbuf(&cmdbuf);
				strcpy(lbuf,"GET/");	
				status = ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);
				found = UU_FALSE;
				while ((lstatus == UU_TRUE) && (cmdbuf.num_cmd < 3))
				{
					if ((ur_retrieve_data_fixed(&e1) == UU_SUCCESS))
					{
						if (ncl_legal_relation(e1.rel_num))
						{
							ncl_get_label(&e1, str);
/*
......test if label is same as previous labels - this avoids duplicate labels 
*/
							if (is_duplicate_label(str)==0)
							{	
								ncl_add_token(&cmdbuf, str, NCL_comma);
								found = UU_TRUE;
								add_label(str);
							}
						}
					}
					lstatus = ud_gnxt(UU_FALSE, UU_NULL, &e1.key, 1);
				}
				if (found == UU_TRUE)
				{
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
/*
.....the ncl_call may change the active unibase 
*/
					ud_setpick_type(UD_PICK_SECONDARY);
				}
				if (lstatus != UU_TRUE) break;
			}
		}
	}
done:;
	if (labels_num!=0)
		free_labels ();
	ud_setpick_type(stype);
	UD_UNMARK(cmdmark);
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclu_get_geo_rename
**       get the geometries from secondary unibase and rename it.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_get_geo_rename()
{
	NCL_cmdbuf cmdbuf;
	int status, stype;
	int second_view;
	char lbuf[80];
	unsigned long mask[4];
	int cmdmark;
	int numint;
	UU_LOGICAL found, lstatus;
	struct UC_entitydatabag e1;
	char str[256];

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
	mask[0] = mask[1] = mask[2] = mask[3] = -1;

	status = NCL_OKINPUT;
	second_view = uv_secondv_act();

	stype = ud_getpick_type();
	ud_setpick_type(UD_PICK_SECONDARY);
	UD_MARK(cmdmark,UU_FALSE);
	if (cmdmark == 0)
	{
/* 
......pick geometry to get/rename
*/
		if (second_view==0)
		{
			while (UU_TRUE)
			{
				ncl_init_cmdbuf(&cmdbuf);
				strcpy(lbuf,"GET/RENAME");	
				status = ncl_add_token(&cmdbuf, lbuf, NCL_comma);
				status = ncl_add_label(UD_DASSTRING, &cmdbuf, 661, mask);
				if (status != NCL_OKINPUT)
					goto done;
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
				ud_setpick_type(UD_PICK_SECONDARY);
			}
		}
		else
		{
			init_labels();
			ud_lgeo(UU_TRUE, mask);
			ud_setpick_type(UD_PICK_SECONDARY);
			status = ud_ldas(UD_DASSELECT, UA_NCL, 661, UU_NULL, NCL_MAXPICK, &numint, UD_NODEFAULT);
			if (status == UU_ALTACTION)
			{
				status = NCL_OKINPUT;
				while (status == NCL_OKINPUT)
				{
					ncl_init_cmdbuf(&cmdbuf);
					strcpy(lbuf,"GET/RENAME");	
					status = ncl_add_token(&cmdbuf, lbuf, NCL_comma);
					status=ncl_add_str(&cmdbuf, 511, NCL_comma);
					if (status == NCL_DONE)
					{
						ncl_add_cmdbuf(&cmdbuf);
						ncl_call(&cmdbuf);
						goto done;
					}
				}
			}
			lstatus = ud_gnxt(UU_TRUE, UU_NULL, &e1.key, 1);
			while (lstatus == UU_TRUE)
			{
				ncl_init_cmdbuf(&cmdbuf);
				strcpy(lbuf,"GET/RENAME");	
				status = ncl_add_token(&cmdbuf, lbuf, NCL_comma);
				found = UU_FALSE;
				while ((lstatus == UU_TRUE) && (cmdbuf.num_cmd < 3))
				{
					if ((ur_retrieve_data_fixed(&e1) == UU_SUCCESS))
					{
						if (ncl_legal_relation(e1.rel_num))
						{
							ncl_get_label(&e1, str);
/*
......test if label is same as previous labels - this avoids duplicate labels 
*/
							if (is_duplicate_label(str)==0)
							{	
								ncl_add_token(&cmdbuf, str, NCL_comma);
								found = UU_TRUE;
								add_label(str);
							}
						}
					}
					lstatus = ud_gnxt(UU_FALSE, UU_NULL, &e1.key, 1);
				}
				if (found == UU_TRUE)
				{
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
/*
.....the ncl_call may change the active unibase 
*/
					ud_setpick_type(UD_PICK_SECONDARY);
				}
				if (lstatus != UU_TRUE) break;
			}
		}
	}
done:;
	if (labels_num!=0)
		free_labels ();
	ud_setpick_type(stype);
	UD_UNMARK(cmdmark);
	return;
}
/*********************************************************************
**    E_FUNCTION     : nclu_get_geo_thru
**       get the geometries thru first pickec geometry to second
**			geometry from secondary unibase.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_get_geo_thru()
{
	NCL_cmdbuf cmdbuf;
	int status, stype, second_view;
	char lbuf[80];
	unsigned long mask[4];
	int cmdmark;

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
	mask[0] = mask[1] = mask[2] = mask[3] = -1;

	stype = ud_getpick_type();
	ud_setpick_type(UD_PICK_SECONDARY);
	second_view = uv_secondv_act();
	UD_MARK(cmdmark,UU_FALSE);
	if (cmdmark == 0)
	{
		while (UU_TRUE)
		{
/*
.....Initialize command buffer and status.
*/
			ncl_init_cmdbuf(&cmdbuf);
			status = NCL_OKINPUT;
			strcpy(lbuf,"GET/");	
			status = ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);

			if (second_view==0)
			{
				status = ncl_add_label(UD_DASSTRING, &cmdbuf, 662, mask);
				if (status != NCL_OKINPUT)
					return;
			}
			else
			{
/*
.....Prompt user for beginning geo to get.
*/
				status = ncl_add_label1(&cmdbuf, 662, mask);
/*
.....If done was selected, exit.
*/
				if (status != NCL_OKINPUT) goto done;
			}
/*
.....Put THRU into the command.
*/
			status = ncl_add_token(&cmdbuf, "THRU", NCL_comma);
			if (second_view==0)
			{
				status = ncl_add_label(UD_DASSTRING, &cmdbuf, 663, mask);
				if (status != NCL_OKINPUT)
					goto done;
			}
			else
			{
/*
.....Prompt user for End Geometry to get
*/
				status = ncl_add_label1(&cmdbuf, 663, mask);
			}
/*
.....Process command.
*/
			if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
				ud_setpick_type(UD_PICK_SECONDARY);
			}
		}
	}
done:;
	ud_setpick_type(stype);
	UD_UNMARK(cmdmark);
	return;
}
/*********************************************************************
**    E_FUNCTION     : nclu_get_geo_thru
**       get the geometries thru first pickec geometry to second
**			geometry from secondary unibase and rename it.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_get_geo_thru_re()
{
	NCL_cmdbuf cmdbuf;
	int status, stype, second_view;
	char lbuf[80];
	unsigned long mask[4];
	int cmdmark;

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
	mask[0] = mask[1] = mask[2] = mask[3] = -1;

	stype = ud_getpick_type();
	ud_setpick_type(UD_PICK_SECONDARY);
	second_view = uv_secondv_act();
	UD_MARK(cmdmark,UU_FALSE);
	if (cmdmark == 0)
	{
		while (UU_TRUE)
		{
/*
.....Initialize command buffer and status.
*/
			ncl_init_cmdbuf(&cmdbuf);
			status = NCL_OKINPUT;
			strcpy(lbuf,"GET/RENAME");	
			status = ncl_add_token(&cmdbuf, lbuf, NCL_comma);

			if (second_view==0)
			{
				status = ncl_add_label(UD_DASSTRING, &cmdbuf, 662, mask);
				if (status != NCL_OKINPUT)
					return;
			}
			else
			{
/*
.....Prompt user for beginning geo to get.
*/
				status = ncl_add_label1(&cmdbuf, 662, mask);
/*
.....If done was selected, exit.
*/
				if (status != NCL_OKINPUT) goto done;
			}
/*
.....Put THRU into the command.
*/
			status = ncl_add_token(&cmdbuf, "THRU", NCL_comma);
			if (second_view==0)
			{
				status = ncl_add_label(UD_DASSTRING, &cmdbuf, 663, mask);
				if (status != NCL_OKINPUT)
					goto done;
			}
			else
			{
/*
.....Prompt user for End Geometry to get
*/
				status = ncl_add_label1(&cmdbuf, 663, mask);
			}
/*
.....Process command.
*/
			if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
				ud_setpick_type(UD_PICK_SECONDARY);
			}
		}
	}
done:;
	ud_setpick_type(stype);
	UD_UNMARK(cmdmark);
	return;
}
/*********************************************************************
**   I_FUNCTION: OnGet(fieldno,val,stat)
**      Callback function for the "->" button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnGet(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,same;
	UD_LIST out_layer;

	if (S_layer_thru)
	{
		ud_wrerr("You must select a layer from 'defined layers' list to completed THRU clause.");
		goto done;
	}
	if (S_layer_sel1==-1)
	{
		*fieldno = -1;
		goto done;
	}
	if ((Sout_layer.num_item==1) && (Sout_layer.item[0][0]=='\0'))
		out_layer.num_item = 1;
	else
		out_layer.num_item = Sout_layer.num_item + 1;

	out_layer.item = (char **)uu_malloc(out_layer.num_item*sizeof (char*));
	j = 0;
	same = 0;
	out_layer.answer = (char *)uu_malloc(80*sizeof (char));
	for (i=0; i<Sout_layer.num_item; i++)
	{
		out_layer.item[j] = (char *)uu_malloc(80*sizeof (char));
		if ((i==0) && (Sout_layer.num_item==1)&&(Sout_layer.item[0][0]=='\0'))
		{
			break;
		}
		else
		{
			if (strcmp(Sout_layer.item[i], S_layer_selstr1)==0)
			{
				same = 1;
				break;
			}
			else
				strcpy(out_layer.item[j++], Sout_layer.item[i]);
		}
	}
	if (same)
		goto update;
	out_layer.item[j] = (char *)uu_malloc(80*sizeof (char));
	strcpy(out_layer.item[j++], S_layer_selstr1);	
	ud_free_flist(&Sout_layer);
	Sout_layer.num_item = j;
	Sout_layer.item = (char **)uu_malloc(Sout_layer.num_item*sizeof (char*));
	Sout_layer.answer = (char *)uu_malloc(80*sizeof (char));
	Sout_layer.answer[0] = '\0';

	for (i=0; i<Sout_layer.num_item; i++)
	{
		Sout_layer.item[i] = (char *)uu_malloc(80*sizeof (char));
		strcpy(Sout_layer.item[i], out_layer.item[i]);
	}
	ud_free_flist(&out_layer);
	ud_list_sort(&Sout_layer);
update:;
	strcpy(Sout_layer.answer, S_layer_selstr1);
	strcpy(S_layer_selstr2, S_layer_selstr1);
	ud_update_answer(3, (int*)&Sout_layer);
done:;
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnRemove(fieldno,val,stat)
**      Callback function for the "<-" button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnRemove(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j;
	UD_LIST out_layer;

/*	if (S_layer_thru)
	{
		ud_wrerr("You must select a layer from 'defined layers' list to completed THRU clause.");
		goto done;
	}
*/
	if ((S_layer_sel2==-1)|| ((Sout_layer.num_item==1)&&(Sout_layer.item[0][0]=='\0')))
	{
		*fieldno = -1;
		goto done;
	}
	out_layer.num_item = Sout_layer.num_item - 1;
	if (out_layer.num_item==0)
	{
		Sout_layer.item[0][0] = '\0';
		Sout_layer.answer[0] = '\0';
		goto update;
	}
	out_layer.item = (char **)uu_malloc(out_layer.num_item*sizeof (char*));
	out_layer.answer = (char *)uu_malloc(80*sizeof (char));
	j = 0;
	for (i=0; i<Sout_layer.num_item; i++)
	{
		out_layer.item[j] = (char *)uu_malloc(80*sizeof (char));
		if (strcmp(S_layer_selstr2, Sout_layer.item[i])==0)
		{
			if (i-1>=0)
				strcpy(out_layer.answer, Sout_layer.item[i-1]);
			else
				out_layer.answer[0] = '\0';
			continue;
		}
		else
		{
			strcpy(out_layer.item[j++], Sout_layer.item[i]);
		}
	}
	ud_free_flist(&Sout_layer);
	Sout_layer.num_item = j;
	Sout_layer.item = (char **)uu_malloc(Sout_layer.num_item*sizeof (char*));
	Sout_layer.answer = (char *)uu_malloc(80*sizeof (char));
	Sout_layer.answer[0] = '\0';

	for (i=0; i<Sout_layer.num_item; i++)
	{
		Sout_layer.item[i] = (char *)uu_malloc(80*sizeof (char));
		strcpy(Sout_layer.item[i], out_layer.item[i]);
	}
	strcpy(Sout_layer.answer, out_layer.answer);
	ud_free_flist(&out_layer);
update:;
	S_layer_thru = 0;
	ud_update_answer(3, (int*)&Sout_layer);
done:;
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnThru(fieldno,val,stat)
**      Callback function for the "Thru" button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnThru(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (S_layer_start<0)
	{
		ud_wrerr("You must select a layer from 'defined layers' list to start THRU clause.");
		return(UD_FLDOK);
	}
	if (S_layer_thru)
	{
		ud_wrerr("You must select a layer from 'defined layers' list to completed THRU clause.");
		return(UD_FLDOK);
	}
	add_list(1);

	S_layer_thru = 1;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : S_remove_item (layer_list, remove_item)
**       removed a item from the list
**			
**    PARAMETERS   
**       INPUT  : layer_list: list to remove item from
**                remove_item: remove item index number
**       OUTPUT :  
**						layer_list:
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_remove_item (layer_list, remove_item)
UD_LIST *layer_list;
int remove_item;
{
	UD_LIST out_layer;
	int i, j;
	
	out_layer.item = (char **)uu_malloc(layer_list->num_item*sizeof (char*));
	out_layer.answer = (char *)uu_malloc(80*sizeof (char));
	j = 0;
	for (i=0; i<layer_list->num_item; i++)
	{
		out_layer.item[j] = (char *)uu_malloc(80*sizeof (char));
		if (i!=remove_item)			
			strcpy(out_layer.item[j++], layer_list->item[i]);
	}
	layer_list->num_item = out_layer.num_item = j;
	for (i=0; i<layer_list->num_item; i++)
	{
		strcpy(layer_list->item[i], out_layer.item[i]);
	}
	ud_free_flist(&out_layer);
}
/*********************************************************************
**    E_FUNCTION     : add_list(int flag)
**       Add a item into the list.
**			
**    PARAMETERS   
**       INPUT  : flag: 1: add "S_layer_start Thru S_layer_end"
**						2: add "S_layer_start Thru"
**                
**       OUTPUT :  
**				
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void add_list(flag)
int flag;
{
	int i,j,item, remove_item;
	char layer_label[80], old_label[80], new_layer_label[80];
	UD_LIST out_layer;
	
	if ((flag==2) && ((S_layer_start>S_layer_end) || (S_layer_start<0) || (S_layer_end<0)))
	{
		ud_wrerr("Invalid THRU clause: start layer > end layer.");
	}
	if (flag==1)
	{
		if ((Sout_layer.num_item==1) && (Sout_layer.item[0][0]=='\0'))
			out_layer.num_item = 1;
		else
			out_layer.num_item = Sout_layer.num_item + 1;
	
		out_layer.item = (char **)uu_malloc(out_layer.num_item*sizeof (char*));
		out_layer.answer = (char *)uu_malloc(80*sizeof (char));
		j = 0;
		sprintf(new_layer_label,"%d THRU",S_layer_start);
		for (i=0; i<Sout_layer.num_item; i++)
		{
			out_layer.item[j] = (char *)uu_malloc(80*sizeof (char));
			if ((i==0) && (Sout_layer.num_item==1)&&(Sout_layer.item[0][0]=='\0'))
			{
				strcpy(out_layer.item[j++], new_layer_label);
				goto copy;
			}
			else
			{
				if (strcmp(new_layer_label, Sout_layer.item[i])!=0)
					strcpy(out_layer.item[j++], Sout_layer.item[i]);
			}
		}
		out_layer.item[j] = (char *)uu_malloc(80*sizeof (char));
		strcpy(out_layer.item[j++], new_layer_label);
	}
copy:;
	if (flag == 1)
	{
		ud_free_flist(&Sout_layer);
		Sout_layer.num_item = j;
		Sout_layer.item = (char **)uu_malloc(Sout_layer.num_item*sizeof (char*));
		Sout_layer.answer = (char *)uu_malloc(80*sizeof (char));
		Sout_layer.answer[0] = '\0';

		for (i=0; i<Sout_layer.num_item; i++)
		{
			Sout_layer.item[i] = (char *)uu_malloc(80*sizeof (char));
			strcpy(Sout_layer.item[i], out_layer.item[i]);
		}
		ud_free_flist(&out_layer);
		strcpy(Sout_layer.answer, new_layer_label);
		ud_list_sort(&Sout_layer);
	}
	if (flag==2)
/*
......replace "%d THRU" to "%d THRU %d"
*/
	{
		item = -1;
		remove_item = -1;
		sprintf(old_label,"%d THRU",S_layer_start);
		sprintf(layer_label,"%d THRU %d",S_layer_start, S_layer_end);
		for (i=0; i<Sout_layer.num_item; i++)
		{
			if (strcmp(Sout_layer.item[i], layer_label)==0)
				remove_item = i;
			if (strcmp(Sout_layer.item[i], old_label)==0)
			{
				item = i;
			}
		}
		if (item>=0)
		{
			strcpy(Sout_layer.item[item], layer_label);
			strcpy(Sout_layer.answer, layer_label);
		}
		if (remove_item!=-1)
			S_remove_item (&Sout_layer, remove_item);
	}
	if (flag==2)
		S_layer_thru = 0;

	ud_update_answer(3, (int*)&Sout_layer);
}



/*********************************************************************
**   I_FUNCTION: OnSelect1(fieldno,val,stat)
**      Callback function for the "Defined Layer" list.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelect1(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char temp[82], *tok, *strtok();

	strcpy(temp, val->frmstr);
	strcpy(S_layer_selstr1, val->frmstr);
	tok = strtok(temp, " ");
	if (tok!=NULL)
	{
		S_layer_sel1 = atoi(tok);
	}
	else
		S_layer_sel1 = -1;
	if (S_layer_thru)
	{
		S_layer_end = S_layer_sel1;
		if ((S_layer_start>S_layer_end) || (S_layer_start<0) || (S_layer_end<0))
		{
			ud_wrerr("Invalid THRU clause: start layer > end layer.");
			goto done;
		}
/*
.....move the layers into "transfer list"
*/
		add_list(2);
		S_layer_thru = 0;
		S_layer_start = S_layer_sel1;
		S_layer_end = -1;
	}
	else
		S_layer_start = S_layer_sel1; 
done:;
	*fieldno = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnSelect2(fieldno,val,stat)
**      Callback function for the "Transfer Layer" list.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelect2(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char temp[82], *tok, *strtok();

	if (S_layer_thru)
	{
		ud_wrerr("You must select a layer from 'defined layers' list to completed THRU clause.");
		goto done;
	}

	strcpy(temp, val->frmstr);
	strcpy(S_layer_selstr2, val->frmstr);
	tok = strtok(temp, " ");
	if (tok!=NULL)
	{
		S_layer_sel2 = atoi(tok);
	}
	else
		S_layer_sel2 = -1;
	*fieldno = -1;
done:;
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: S_load_layers()
**      Load all layer definitions from working unibase into a local list.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static int S_load_layers()
{
	int status,i,save_act;
	struct UM_layer_rec *sptr;
/*
.....Get all of the layers and
.....place them into the list
*/
	save_act = UR_active;
	ur_getu_work();
	status = umu_load_layers(&Slayer,&Snlayer,&Scur_active);
	if (save_act==2)
		ur_getu_second();
	if (status != UU_SUCCESS) return(status);
/*
.....Find active layer
*/
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
	for (i=0;i<Snlayer;i++)
	{
		if (sptr[i].num == Scur_active)
		{
			Ssel = i;
			break;
		}
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**   I_FUNCTION: S_load_2layers()
**      Load all layer definitions from second unibase into a local list.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static int S_load_2layers()
{
	int status,i,save_act;
	struct UM_layer_rec *sptr;
/*
.....Get all of the layers and
.....place them into the list
*/
	save_act = UR_active;
	ur_getu_second();
	status = umu_load_layers(&Slayer,&Snlayer,&Scur_active);
	if (save_act==1)
		ur_getu_work();
	if (status != UU_SUCCESS) return(status);
/*
.....Find active layer
*/
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
	for (i=0;i<Snlayer;i++)
	{
		if (sptr[i].num == Scur_active)
		{
			Ssel = i;
			break;
		}
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}
/*********************************************************************
**   I_FUNCTION: S_layer_command(sptr,obuf);
**      Format a layer definition for form output.
**   PARAMETERS
**       INPUT  : sptr   = Pointer to layer record.
**       OUTPUT : obuf   = Formatted output.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static void S_layer_command(sptr,obuf)
struct UM_layer_rec *sptr;
char *obuf;
{
	char sbuf[100];
	sprintf(sbuf,"%d  %s",sptr->num,sptr->name);
	sbuf[80] = '\0';
	strcpy(obuf,sbuf);
}

/*********************************************************************
**   I_FUNCTION: S_load_list()
**      Load all layer definitions into the form list.
**   PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static int S_load_list()
{
	int i;
	struct UM_layer_rec *sptr;
	char *ptr1;
	
	Smax = Snlayer + 20;
/*
.....Allocate memory for form list
*/
	Slayer_ptr = (char *)uu_malloc(sizeof(char)*80*Smax);
	if (Slayer_ptr == UU_NULL) return(UU_FAILURE);
	Sin_layer.item = (char **)uu_malloc(Smax * sizeof(char *));
	if (Sin_layer.item == UU_NULL) return(UU_FAILURE);
/*
.....Store layer list in form list
*/
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
	ptr1 = Slayer_ptr;
	for (i=0;i<Snlayer;i++)
	{
		S_layer_command(&sptr[i],ptr1);
		Sin_layer.item[i] = (char *)uu_malloc(sizeof(char)*80);
		strcpy(Sin_layer.item[i], ptr1);
		ptr1 = ptr1 + 80;
		if (i == Ssel) uu_move_byte(&sptr[i],&Scur,sizeof(struct UM_layer_rec));
	}
	strcpy(Scmd,Sin_layer.item[Ssel]);
	Sin_layer.num_item = Snlayer;
	Sin_layer.answer = (char *)uu_malloc(sizeof(char)*80);
	strcpy(Sin_layer.answer, Scmd);
	S_layer_sel1 = S_layer_start = Scur_active;
	strcpy(S_layer_selstr1, Sin_layer.item[Ssel]);
/*
.....End of routine
*/
	return(UU_SUCCESS);
}


/*********************************************************************
**    E_FUNCTION     : nclu_get_layer
**       get segments from secondary unibase by layer.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_get_layer()
{
	NCL_cmdbuf cmdbuf;
	int i, layer_num1, layer_num2;
	char temp[80], *tok, *strtok();
	char lbuf[80];
	int status, *ans[7];
	static UD_METHOD methods[] =
	{
		UU_NULL, UU_NULL, OnSelect1, OnSelect2, OnGet, OnRemove, OnThru
	};
	static char called[]       = {6,6,6, 6,6,6,6};
	static char traverse[]     = {1,1,1,1, 1,1,1};
	static char disp[]         = {1,1,1,1, 1,1,1,1};
	static int rename = 0, offset = 0;
	int mainmarkval=0;

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
/*
.....Initialize routine
*/
	Sin_layer.item = UU_NULL;
	Ssel = 0;
	Sactive = 0;
/*
.....Load the layers into a list
*/
	status = S_load_2layers();
	if (status != UU_SUCCESS) goto nomem;
/*
.....Load the layer list into the form
*/
	status = S_load_list();
	if (status != UU_SUCCESS) goto nomem;


	Sout_layer.num_item = 1;
	Sout_layer.item = (char **)uu_malloc(sizeof (char*));
	Sout_layer.item[0] = (char *)uu_malloc(80*sizeof (char));
	Sout_layer.answer = (char *)uu_malloc(80*sizeof (char));
	Sout_layer.item[0][0] = '\0';
	Sout_layer.answer[0] = '\0';

	ans[0] = (int *)&rename;
	ans[1] = (int *)&offset;
	ans[2] = (int *)&Sin_layer;
	ans[3] = (int *)&Sout_layer;
	ans[4] = NULL;
	ans[5] = NULL;
	ans[6] = NULL;

/*
.....Command Reject
*/
	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		ud_free_flist(&Sin_layer);
		ud_free_flist(&Sout_layer);
		UD_UNMARK (mainmarkval);
		return;
	}
	status = ud_form1("layertran.frm",ans,ans,methods,called,disp,traverse);
	ud_free_flist(&Sin_layer);
	if (status == -1) 
	{
		ud_free_flist(&Sout_layer);
		goto done;
	}
/*
.....Initialize the command buffer.
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lbuf,"GET/");	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);
	if (rename)
		ncl_add_token(&cmdbuf, "RENAME", NCL_comma);
	ncl_add_token(&cmdbuf, "LAYER", NCL_comma);

	for (i=0; i<Sout_layer.num_item; i++)
	{
		layer_num1 = layer_num2 = -1;
		strcpy(temp, Sout_layer.item[i]);
		tok = strtok(temp, " ");
		if (tok!=NULL)
		{
			layer_num1 = atoi(tok);
			tok = strtok(NULL, " ");
			if ((tok==NULL) || (strcmp(tok, "THRU")!=0))
				layer_num2 = -1;
			else
			{
				tok = strtok(NULL, " ");
				if (tok!=NULL)
					layer_num2 = atoi(tok);
				else
					layer_num2 = -1;
			}	
		}
		if (layer_num1>=0)
		{
			sprintf (lbuf, "%d", layer_num1);
			ncl_add_token(&cmdbuf, lbuf, NCL_comma);
		}
		if (layer_num2>=0)
		{
			sprintf (lbuf, "THRU,%d", layer_num2);
			ncl_add_token(&cmdbuf, lbuf, NCL_comma);
		}
	}
	if ((offset!=0)&&(layer_num1>=0))
	{
		sprintf(lbuf, "OFFSET,%d", offset);
		ncl_add_token(&cmdbuf, lbuf, NCL_comma);
	}
	ncl_del_token(&cmdbuf,"", UU_TRUE);
	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
	ud_free_flist(&Sout_layer);
	goto done;
/*
.....Could not allocate memory
*/
nomem:
	ud_wrerr("Could not allocate memory for form.");
	goto done;
done:;
	UD_UNMARK (mainmarkval);
}
/*********************************************************************
**    E_FUNCTION     : nclu_get_type
**       get geometries from secondary unibase by geometry type.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_get_type()
{
	NCL_cmdbuf cmdbuf;
	int i;
	char *strtok();
	char lbuf[80];
	int status, *ans[6];
	static UD_METHOD methods[] =
	{
		UU_NULL, OnSelectType1, OnSelectType2, OnGetType, OnRemoveType
	};
	static char called[]       = {6,6,6, 6,6,};
	static char traverse[]     = {1,1,1, 1,1,};
	static char disp[]         = {1,1,1, 1,1,1};
	static int rename = 0;
	int mainmarkval=0;

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
/*
.....Initialize routine
*/
	Sin_type.item = UU_NULL;
	S_type_selstr1[0]='\0';
	S_type_selstr2[0]='\0';

	Sin_type.item = ncl_get_typename(&(Sin_type.num_item), 2);
	if (Sin_type.num_item==0)
	{
		Sin_type.num_item = 1;
		Sin_type.item[0] = (char *)uu_malloc(40*sizeof (char));
		Sin_type.item[0][0] = '\0';
	}
	ud_list_sort(&Sin_type);
	Sin_type.answer = (char *) uu_malloc(40 * sizeof(char));
	strcpy(Sin_type.answer, Sin_type.item[0]);
	strcpy(S_type_selstr1,  Sin_type.item[0]);

	Sout_type.num_item = 1;
	Sout_type.item = (char **)uu_malloc(sizeof (char*));
	Sout_type.item[0] = (char *)uu_malloc(40*sizeof (char));
	Sout_type.answer = (char *)uu_malloc(40*sizeof (char));
	Sout_type.item[0][0] = '\0';
	Sout_type.answer[0] = '\0';
		
	ans[0] = (int *)&rename;
	ans[1] = (int *)&Sin_type;
	ans[2] = (int *)&Sout_type;
	ans[3] = NULL;
	ans[4] = NULL;
/*
.....Command Reject
*/
	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		ud_free_flist(&Sin_type);
		ud_free_flist(&Sout_type);
		UD_UNMARK (mainmarkval);
		return;
	}
	status = ud_form1("typetran.frm",ans,ans,methods,called,disp,traverse);
	ud_free_flist(&Sin_type);
	if (status == -1) 
	{
		ud_free_flist(&Sout_type);
		goto done;
	}
/*
.....Initialize the command buffer.
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lbuf,"GET/");	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);
	if (rename)
		ncl_add_token(&cmdbuf, "RENAME", NCL_comma);

	for (i=0; i<Sout_type.num_item; i++)
	{
		ncl_add_token(&cmdbuf, Sout_type.item[i], NCL_comma);
	}
	if ((Sout_type.num_item>=1) && (Sout_type.item[0][0]!='\0'))
	{
		ncl_del_token(&cmdbuf,"", UU_TRUE);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
	ud_free_flist(&Sout_type);
	goto done;
done:;
	UD_UNMARK (mainmarkval);
}
/*********************************************************************
**    E_FUNCTION     : nclu_put_allgeo
**       put all geometries into secondary unibase.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_put_allgeo()
{
	NCL_cmdbuf cmdbuf;
	char lbuf[80];
	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
/*
.....Initialize the command buffer.
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lbuf,"PUT/all");	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_set_cmdmode(UU_TRUE);
	ncl_call(&cmdbuf);
}
/*********************************************************************
**    E_FUNCTION     : nclu_put_geo
**       put geometries into secondary unibase.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_put_geo()
{
	NCL_cmdbuf cmdbuf;
	int status, work_view;
	char lbuf[80];
	unsigned long mask[4];
	int cmdmark, stype;
	int numint;
	UU_LOGICAL found, lstatus;
	struct UC_entitydatabag e1;
	char str[256];

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
	mask[0] = mask[1] = mask[2] = mask[3] = -1;
	
	status = NCL_OKINPUT;
	work_view = uv_workv_act();

	stype = ud_getpick_type();
	ud_setpick_type(UD_PICK_NORMAL);
	UD_MARK(cmdmark,UU_FALSE);
	if (cmdmark == 0)
	{
/* 
......pick geometry to get 
*/
		if (work_view==0)
		{
			while (UU_TRUE)
			{
				ncl_init_cmdbuf(&cmdbuf);
				strcpy(lbuf,"PUT/");	
				status = ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);
				status = ncl_add_label(UD_DASSTRING, &cmdbuf, 667, mask);
				if (status != NCL_OKINPUT)
					goto done;
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
				ud_setpick_type(UD_PICK_NORMAL);
			}
		}
		else
		{
			init_labels();
			ud_lgeo(UU_TRUE, mask);
			ud_setpick_type(UD_PICK_NORMAL);
			status = ud_ldas(UD_DASSELECT, UA_NCL, 667, UU_NULL, NCL_MAXPICK, &numint, UD_NODEFAULT);
			if (status == UU_ALTACTION)
			{
				status = NCL_OKINPUT;
				while (status == NCL_OKINPUT)
				{
					ncl_init_cmdbuf(&cmdbuf);
					strcpy(lbuf,"GET/");	
					status = ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);
					status=ncl_add_str(&cmdbuf, 511, NCL_comma);
					if (status == NCL_DONE)
					{
						ncl_add_cmdbuf(&cmdbuf);
						ncl_call(&cmdbuf);
						goto done;
					}
				}
			}
			lstatus = ud_gnxt(UU_TRUE, UU_NULL, &e1.key, 1);
			while (lstatus == UU_TRUE)
			{
				ncl_init_cmdbuf(&cmdbuf);
				strcpy(lbuf,"PUT/");	
				status = ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);
				found = UU_FALSE;
				while ((lstatus == UU_TRUE) && (cmdbuf.num_cmd < 3))
				{
					if ((ur_retrieve_data_fixed(&e1) == UU_SUCCESS))
					{
						if (ncl_legal_relation(e1.rel_num))
						{
							ncl_get_label(&e1, str);
/*
......test if label is same as previous labels - this avoids duplicate labels 
*/
							if (is_duplicate_label(str)==0)
							{	
								ncl_add_token(&cmdbuf, str, NCL_comma);
								found = UU_TRUE;
								add_label(str);
							}
						}
					}
					lstatus = ud_gnxt(UU_FALSE, UU_NULL, &e1.key, 1);
				}
				if (found == UU_TRUE)
				{
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
/*
.....the ncl_call may change the active unibase 
*/
					ud_setpick_type(UD_PICK_NORMAL);
				}
				if (lstatus != UU_TRUE) break;
			}
		}
	}
done:;
	if (labels_num!=0)
		free_labels ();
	ud_setpick_type(stype);
	UD_UNMARK(cmdmark);
	return;
}
/*********************************************************************
**    E_FUNCTION     : nclu_put_geo_thru
**       put geometries thru first pickec geometry to second
**			geometry into secondary unibase.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_put_geo_thru()
{
	NCL_cmdbuf cmdbuf;
	int status, work_view;
	char lbuf[80];
	unsigned long mask[4];
	int cmdmark, stype;

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
	mask[0] = mask[1] = mask[2] = mask[3] = -1;

	work_view = uv_workv_act();
	stype = ud_getpick_type();
	ud_setpick_type(UD_PICK_NORMAL);
	UD_MARK(cmdmark,UU_FALSE);
	if (cmdmark == 0)
	{
		while (UU_TRUE)
		{
/*
.....Initialize command buffer and status.
*/
			ncl_init_cmdbuf(&cmdbuf);
			status = NCL_OKINPUT;
			strcpy(lbuf,"PUT/");	
			status = ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);
/*
.....Prompt user for beginning geo to put.
*/
			if (work_view==0)
			{
				status = ncl_add_label(UD_DASSTRING, &cmdbuf, 664, mask);
				if (status != NCL_OKINPUT)
					return;
			}
			else
			{
				status = ncl_add_label1(&cmdbuf, 664, mask);
/*
.....If done was selected, exit.
*/
				if (status != NCL_OKINPUT) goto done;
			}
/*
.....Put THRU into the command.
*/
			status = ncl_add_token(&cmdbuf, "THRU", NCL_comma);
/*
.....Prompt user for End Geometry to Put
*/
			if (work_view==0)
			{
				status = ncl_add_label(UD_DASSTRING, &cmdbuf, 665, mask);
				if (status != NCL_OKINPUT)
					return;
			}
			else
			{
				status = ncl_add_label1(&cmdbuf, 665, mask);
				if (status != NCL_OKINPUT)
					return;
			}
/*
.....Process command.
*/
			if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
				ud_setpick_type(UD_PICK_NORMAL);
			}
		}
	}
done:;
	ud_setpick_type(stype);
	UD_UNMARK(cmdmark);
	return;
}

/*********************************************************************
**    E_FUNCTION     : uv_secondv_act
**       check if the secondary unibase view is active
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : 1: yes
**					0: no
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uv_secondv_act()
{
	UV_vport vport;
	UV_view view;
	int i, second_view;
	second_view = 0;
	for (i=0; i<UV_act_screen[0].nvports; i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_getvid(vport.cur_view, &view);
		if (view.vtype==UV_SECONDARY_VIEW)
		{
			second_view = 1;
			break;
		}
	}
	return second_view;
}

/*********************************************************************
**    E_FUNCTION     : uv_workv_act
**       check if the working view is active
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : 1: yes
**					0: no
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uv_workv_act()
{
	int i, work_view;
	UV_vport vport;
	UV_view view;
	work_view = 0;
	for (i=0; i<UV_act_screen[0].nvports; i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_getvid(vport.cur_view, &view);
		if ((view.vtype!=UV_SECONDARY_VIEW) &&
				(view.vtype!=UV_INVISIBLE_VIEW))
		{
			work_view = 1;
			break;
		}
	}
	return work_view;
}


int uv_invisv_act()
{
	int i, inv_view;
	UV_vport vport;
	UV_view view;
	inv_view = 0;
	for (i=0; i<UV_act_screen[0].nvports; i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_getvid(vport.cur_view, &view);
		if (view.vtype==UV_INVISIBLE_VIEW)
		{
			inv_view = 1;
			break;
		}
	}
	return inv_view;
}

/*********************************************************************
**    E_FUNCTION         :  ncl_get_typename(number, flag)
**       get all the geometry type names in the unibase
**
**    PARAMETERS   
**       INPUT  : 
**				flag: 1. normal unibase
**						2. secondary unibase
**       OUTPUT : number: number of geometry type in unibase
**    RETURNS      : a list of geometry type name
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char **ncl_get_typename(number, flag)
int *number, flag;
{
	UU_LOGICAL init;
	struct NCL_fixed_databag e;
	char **type_name, lbuf[40],sbuf[40];
	int i, len, switched = 0;
/*
......max type number is MAXTYPE
*/
	type_name = (char **) uu_malloc(MAXTYPE *sizeof(char *));

	*number = 0;	
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
	init = UU_TRUE;		
	while (um_getallobjs(init, &e.key) == 0)
	{
		init = UU_FALSE;
		lbuf[0] = '\0';
		if ( e.key >= UR_low_key)
		{
			ur_retrieve_data_relnum(e.key,&e.rel_num);
			if (e.rel_num == UM_POINT_REL ||
							e.rel_num == NCL_POINT_REL)  
				strcpy(lbuf, "POINT");
			else if ( e.rel_num == NCL_POINTVEC_REL )  
				strcpy(lbuf, "PNTVEC");
			else if ( e.rel_num == NCL_VECTOR_REL )   
				strcpy(lbuf, "VECTOR");
			else if (e.rel_num == UM_LINE_REL ||
							e.rel_num == NCL_LINE_REL )   
				strcpy(lbuf, "LINE");
			else if (e.rel_num == UM_CIRCLE_REL || 
						e.rel_num == NCL_CIRCLE_REL )  
				strcpy(lbuf, "CIRCLE");
			else if (e.rel_num == NCL_PLN_REL)
				strcpy(lbuf, "PLANE");
			else if (e.rel_num == NCL_CURVE_REL || 
						e.rel_num == NCL_EVALCV_REL ||
						e.rel_num == UM_RBSPLCRV_REL ||
						e.rel_num == UM_CONIC_REL ||   
						e.rel_num == UM_COMPCRV_REL)
				strcpy(lbuf, "CURVE");
			else if (e.rel_num == NCL_SURF_REL ||
						e.rel_num == NCL_REVSURF_REL ||
						e.rel_num == NCL_EVALSF_REL ||
						e.rel_num == UM_RBSPLSRF_REL ||
						e.rel_num == NCL_MESHSURF_REL ||
						e.rel_num == NCL_QUILTSURF_REL ||
						e.rel_num == NCL_NETSF_REL ||
						e.rel_num == NCL_TRIMSF_REL )
				strcpy(lbuf, "SURF");
			else if ( e.rel_num == NCL_PATERN_REL ) 
				strcpy(lbuf, "PATERN");
			else if (e.rel_num == NCL_MATRIX_REL)
				strcpy(lbuf, "MATRIX");
			else if (e.rel_num == NCL_SHAPE_REL) 
				strcpy(lbuf, "SHAPE");
			else if ( e.rel_num == NCL_DATAST_REL ) 
				strcpy(lbuf, "DATA");
			else if ( e.rel_num == UM_SOLID_REL ) 
				strcpy(lbuf, "SOLID");
			else if ( e.rel_num == UA_TEXT_REL ) 
				strcpy(lbuf, "ANOTE");
			else if ( e.rel_num == UA_LINEAR_DIMS_REL ) 
				strcpy(lbuf, "DRAFT");
			else if ( e.rel_num == UB_SYMBOL_REL ) 
				strcpy(lbuf, "SYMBOL");
			else if ( e.rel_num == UB_INSTANCE_REL ) 
				strcpy(lbuf, "PLACE");
		}
		len = strlen(lbuf);
		if (len==0)
			continue;
/*
......check if this type is already add in the list
*/
		for (i=0; i<*number; i++)
		{
			if (strcmp(lbuf, type_name[i])==0)
				break;
		}
		if (i>=*number)
		{
			if (*number >= MAXTYPE)
			{
				sprintf(sbuf,"Maximum of %d types can be defined. Internal error.",
					MAXTYPE);
				ud_printmsg(sbuf);
				*number = MAXTYPE - 1;
			}
			type_name[*number] = (char *) uu_malloc(40 * sizeof(char));	
			strcpy(type_name[*number], lbuf);
			(*number)++;
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
	return (type_name);
}
/*********************************************************************
**    E_FUNCTION     : nclu_get_allgeo_rename
**       get and rename all geometries from secondary unibase.
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_get_allgeo_rename()
{
	NCL_cmdbuf cmdbuf;
	char lbuf[80];

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
/*
.....Initialize the command buffer.
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lbuf,"GET/RENAME, ALL");	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_set_cmdmode(UU_TRUE);
	ncl_call(&cmdbuf);
}
/*********************************************************************
**   I_FUNCTION: S_init_datafrm(fieldno,val,stat)
**      intial function for transfer data form
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: UD_FLDOK.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT S_init_datafrm(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_dispfrm_update_label(0, 0, "Select Variable/Data to Get");
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: S_remove_data(listdata1, listdata2, item)
**      remove a UD_ITEMDATA structure from a UD_TLIST structure and put
**		into output UD_TLIST structure 
**   PARAMETERS
**       INPUT  : listdata1: original UD_TLIST structure.
**                item      = item to be added.
**       OUTPUT : listdata2: structure with original UD_TLIST and and 
**					remove item UD_ITEMDATA structure.
**   RETURNS: UD_FLDOK.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static void S_remove_data(listdata1, listdata2, item)
UD_TLIST* listdata1, *listdata2;
UD_ITEMDATA *item;
{
	int i,j,k,len;
	listdata2->num_item = listdata1->num_item-1;
	listdata2->num_col = listdata1->num_col;
	listdata2->answer = listdata1->answer;
	if (listdata2->num_col>0)
		listdata2->col_label = (char**) uu_malloc(listdata2->num_col*sizeof (char*));
	for (i=0; i<listdata2->num_col;i++)
	{
		len = strlen (listdata1->col_label[i]);
		listdata2->col_label[i] = (char*) uu_malloc((len+1)*sizeof(char));
		strcpy(listdata2->col_label[i], listdata1->col_label[i]);
	}
	if (listdata2->num_item<=0) return;
	if (listdata2->num_item>0)
		listdata2->data = (UD_ITEMDATA *) uu_malloc(listdata2->num_item*sizeof(UD_ITEMDATA));
	for (i=0,k=0; i<listdata1->num_item;i++)
	{
		(listdata2->data[k]).itemnum = (listdata1->data[i]).itemnum;
		(listdata2->data[k]).data_items = 
					(char **) uu_malloc(listdata2->num_col*sizeof(char*));
		if (strcmp((listdata1->data[i]).data_items[0], item->data_items[0])==0)
			continue;
		for (j=0; j<(listdata2->data[k]).itemnum; j++)
		{
			len = strlen(listdata1->data[i].data_items[j]);
			listdata2->data[k].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(listdata2->data[k].data_items[j], listdata1->data[i].data_items[j]);
		}
		k++;
	}
}
/*********************************************************************
**   I_FUNCTION: S_add_data(listdata1, listdata2, item)
**      adding a UD_ITEMDATA structure into a UD_TLIST structure and put
**		into output UD_TLIST structure 
**   PARAMETERS
**       INPUT  : listdata1: original UD_TLIST structure.
**                item      = item to be added.
**       OUTPUT : listdata2: structure with original UD_TLIST and and 
**					new item UD_ITEMDATA structure.
**   RETURNS: UD_FLDOK.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static void S_add_data(listdata1, listdata2, item)
UD_TLIST* listdata1, *listdata2;
UD_ITEMDATA *item;
{
	int i,j,len;
	for (i=0; i<listdata1->num_item; i++)
	{
		if (strcmp((listdata1->data[i]).data_items[0], item->data_items[0])==0)
		{
/*
......this item already there. copy it and returm
*/
			ud_tlist_copy(listdata1, listdata2);
			return;
		}
	}
	listdata2->num_item = listdata1->num_item+1;
	listdata2->num_col = listdata1->num_col;
	listdata2->answer = listdata1->answer;
	if (listdata2->num_col>0)
		listdata2->col_label = (char**) uu_malloc(listdata2->num_col*sizeof (char*));
	for (i=0; i<listdata2->num_col;i++)
	{
		len = strlen (listdata1->col_label[i]);
		listdata2->col_label[i] = (char*) uu_malloc((len+1)*sizeof(char));
		strcpy(listdata2->col_label[i], listdata1->col_label[i]);
	}
	if (listdata2->num_item>0)
		listdata2->data = (UD_ITEMDATA *) uu_malloc(listdata2->num_item*sizeof(UD_ITEMDATA));
	for (i=0; i<listdata1->num_item;i++)
	{
		(listdata2->data[i]).itemnum = (listdata1->data[i]).itemnum;
		(listdata2->data[i]).data_items = 
					(char **) uu_malloc(listdata2->num_col*sizeof(char*));
		for (j=0; j<(listdata2->data[i]).itemnum; j++)
		{
			len = strlen(listdata1->data[i].data_items[j]);
			listdata2->data[i].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(listdata2->data[i].data_items[j], listdata1->data[i].data_items[j]);
		}
	}
	listdata2->data[i].itemnum = item->itemnum;
	listdata2->data[i].frmid = item->frmid;
	listdata2->data[i].fldno = item->fldno;
	listdata2->data[i].flag = item->flag;
	if (item->itemnum>0)
		listdata2->data[i].data_items = (char**)uu_malloc(item->itemnum*sizeof(char*));
	for (j=0; j<item->itemnum; j++)
	{
		len = strlen(item->data_items[j]);
		listdata2->data[i].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
		strcpy(listdata2->data[i].data_items[j], item->data_items[j]);
	}
}

/*********************************************************************
**   I_FUNCTION: OnGetData(fieldno,val,stat)
**      Callback function for the "->" button in select variable/data form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnGetData(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,same;
	UD_TLIST out_data;

	if (S_data_seldata1.data_items==NULL)
	{
		*fieldno = -1;
		goto done;
	}
	S_add_data(&Sout_data, &out_data, &S_data_seldata1);
	ud_free_tlist(&Sout_data);
	ud_tlist_copy(&out_data, &Sout_data);
	ud_free_tlist(&out_data);
	ud_update_answer(5, (int*)&Sout_data);
done:;
	return(UD_FLDOK);
}


/*********************************************************************
**   I_FUNCTION: OnRemoveData(fieldno,val,stat)
**      Callback function for the "<-" button in select variable/data  form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnRemoveData(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j;
	UD_TLIST out_data;

	if (S_data_seldata2.data_items==NULL)
	{
		*fieldno = -1;
		goto done;
	}
	S_remove_data(&Sout_data, &out_data, &S_data_seldata2);
	ud_free_tlist(&Sout_data);
	ud_tlist_copy(&out_data, &Sout_data);
	ud_free_tlist(&out_data);
update:;
	ud_update_answer(5, (int*)&Sout_data);
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: SortFunc()
**      Sort the list on unibase transfer form in partical order
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
......"Name" "Value"
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
......"Name" "Value"
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
	default:
		break;
	}
	return nRetVal;
}


/*********************************************************************
**   I_FUNCTION: OnTableCalbacks1(fieldno,val,stat)
**      Callback function for a first tablelist Selecton from the unibase transfer form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnTableCalbacks1(fieldno,val,stat)
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
			ud_tlist_copy_idata(data, &S_data_seldata1);
		}
		else
		{
			if (S_data_seldata1.data_items!=NULL)
				ud_tlist_free_idata(&S_data_seldata1);
		}
	}
	else
	{
/*
......column button is pushed, doing sorting
*/
		if ((info->col==0)&&(name_click1%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			name_click1++;
		}
		else if ((info->col==0)&&(name_click1%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			name_click1++;
		}
		if ((info->col==1)&&(value_click1%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			value_click1++;
		}
		else if ((info->col==1)&&(value_click1%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			value_click1++;
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnTableCalbacks2(fieldno,val,stat)
**      Callback function for second tablelist Selecton from the unibase transfer form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnTableCalbacks2(fieldno,val,stat)
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
			ud_tlist_copy_idata(data, &S_data_seldata2);
		}
		else
		{
			if (S_data_seldata2.data_items!=NULL)
				ud_tlist_free_idata(&S_data_seldata2);
		}
	}
	else
	{
/*
......column button is pushed, doing sorting
*/
		if ((info->col==0)&&(name_click2%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			name_click2++;
		}
		else if ((info->col==0)&&(name_click2%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			name_click2++;
		}
		if ((info->col==1)&&(value_click2%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			value_click2++;
		}
		else if ((info->col==1)&&(value_click2%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			value_click2++;
		}
	}
	return(UD_FLDOK);
}
/*********************************************************************
**    E_FUNCTION         :  ncl_get_dataname_tlist(tlist, flag, type, filter, clas)
**       get all the data names and value list in the unibase
**		include Name and Value into a UD_TLIST struction
**
**    PARAMETERS   
**       INPUT  : 
**				flag: 1. normal unibase
**					2. secondary unibase
**				type: 0: scalar
**						1: text value
**						2: Data statement
**				filter: filter string
**				clas: class name of the scalar (only apply when type==0)
**       OUTPUT : tlist: UD_TLIST struction include scalar info 
**    RETURNS      : a list of variable/data name
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_dataname_tlist(tlist, flag, type, filter, clas)
UD_TLIST *tlist;
int flag, type;
char *filter, *clas;
{
	UU_LOGICAL init;
	struct NCL_fixed_databag e;
	struct NCL_textvar_rec txtrec;
	struct NCL_datast_rec datarec;
	struct NCL_scalar_rec scalrec;
	struct NCL_datael_rec *dsp;
	char lbuf[400], tempstr[400], fstr[20], filt[21];
	int i, ifl, fnc, len, max_len, data_num, status, itemnum, switched = 0;
/*
......if there are class defined but not "All", it should only for 'Scalar' type
*/
	if (!((clas==NULL)||(strcmp(clas, "All")==0)))
		type = 0;
/*
.....Determine type of filter to support
.....Valid filter styles are:
.....*    = 1
.....mk*  = 2
.....*mk  = 3
.....*mk* = 4
*/
	strcpy (fstr, filter);
	fnc = strlen(fstr);
	ul_strip_blanks(fstr,&fnc);
	if (fnc == 0)
	{
		strcpy(fstr,"*");
		fnc = 1;
	}
	ul_to_upper(fstr);
	if (strcmp(fstr,"*") == 0) ifl = 1;
	else if (fstr[0] == '*')
	{
		if (fstr[fnc-1] == '*')
		{
			ifl = 4;
			strncpy(filt,&fstr[1],fnc-2);
			filt[fnc-2] = '\0';
		}
		else
		{
			ifl = 3;
			strcpy(filt,&fstr[1]);
		}
	}
	else if (fstr[fnc-1] == '*')
	{
		ifl = 2;
		strncpy(filt,fstr,fnc-1);
		filt[fnc-1] = '\0';
	}
	else
	{
		ifl = 2;
		strcpy(filt,fstr);
	}
	fnc = strlen(filt);

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
	init = UU_TRUE;		
	data_num = 0;
	max_len = 0;
	while (um_getallobjs(init, &e.key) == 0)
	{
		init = UU_FALSE;
		lbuf[0] = '\0';
		if ( e.key >= UR_low_key)
		{
/*
......get the total number of the label
*/
			ur_retrieve_data_relnum(e.key,&e.rel_num);
			if (((e.rel_num == NCL_SCALAR_REL) && ((type==0)||(type==-1)))
				|| (( e.rel_num == NCL_TEXTVAR_REL ) && ((type==1)||(type==-1)))
				|| (( e.rel_num == NCL_DATAST_REL ) && ((type==2)||(type==-1))))
			{
				if (e.rel_num == NCL_SCALAR_REL)
				{
					scalrec.key = e.key;
					status = ncl_retrieve_data_fixed(&scalrec);
					strncpy (tempstr, scalrec.label, NCL_MAX_LABEL);
					tempstr[64] = '\0';
/*
.....remove trailing spaces
*/
					i = strlen(tempstr);
					while ((i!=0) &&(tempstr[i-1]==' ')) i--;
						tempstr[i] = '\0';
/*
.....see if it mactch the fileter
*/
					if (!( (ifl == 1) ||
						(ifl == 2 && strncmp(filt,tempstr,fnc) == 0) ||
						(ifl == 3 && strcmp(filt, &tempstr[strlen(tempstr)-fnc]) == 0) ||
						(ifl == 4 && strstr(tempstr,filt) != 0) ) )
						continue;
					if (!((clas==NULL)||(strcmp(scalrec.classnm, clas)==0)
							||(strcmp(clas, "All")==0)))
						continue;
					if (scalrec.subscr>0)
						sprintf (tempstr, "%s(%d)", tempstr, scalrec.subscr);
					data_num++;
				}
				else if ( e.rel_num == NCL_TEXTVAR_REL ) 
				{
					txtrec.key = e.key;
					status = ncl_retrieve_data_fixed(&txtrec);
					strncpy (tempstr, txtrec.label, NCL_MAX_LABEL);
					tempstr[64] = '\0';
/*
.....remove trailing spaces
*/
					i = strlen(tempstr);
					while ((i!=0) &&(tempstr[i-1]==' ')) i--;
						tempstr[i] = '\0';
/*
.....see if it mactch the fileter
*/
					if (!( (ifl == 1) ||
						(ifl == 2 && strncmp(filt,tempstr,fnc) == 0) ||
						(ifl == 3 && strcmp(filt, &tempstr[strlen(tempstr)-fnc]) == 0) ||
						(ifl == 4 && strstr(tempstr,filt) != 0) ) )
						continue;
					if (txtrec.subscr>0)
						sprintf (tempstr, "%s(%d)", tempstr, txtrec.subscr);
					data_num++;
				}
				else if ( e.rel_num == NCL_DATAST_REL ) 
				{
					datarec.key = e.key;
					status = ncl_retrieve_data_fixed(&datarec);
					if (status == UU_SUCCESS && datarec.no_datael>0)
					{
						strncpy (tempstr, datarec.label, NCL_MAX_LABEL);
						tempstr[64] = '\0';
/*
.....remove trailing spaces
*/
						i = strlen(tempstr);
						while ((i!=0) &&(tempstr[i-1]==' ')) i--;
							tempstr[i] = '\0';
						if (datarec.subscr>0)
							sprintf (tempstr, "%s(%d)", tempstr, datarec.subscr);
/*
.....see if it mactch the fileter
*/
						if (!( (ifl == 1) ||
							(ifl == 2 && strncmp(filt,tempstr,fnc) == 0) ||
							(ifl == 3 && strcmp(filt, &tempstr[strlen(tempstr)-fnc]) == 0) ||
							(ifl == 4 && strstr(tempstr,filt) != 0) ) )
							continue;
						data_num++;
					}
				}

			}
			else
				continue;
		}
	}
/*
......Name	Value
*/
	tlist->num_col = 2; 
	tlist->num_item = data_num;
	tlist->answer = 0;
	if (tlist->num_col>0)
		tlist->col_label = (char**) uu_malloc(tlist->num_col*sizeof (char*));
	for (i=0; i<tlist->num_col;i++)
	{
		tlist->col_label[i] = (char*) uu_malloc(20*sizeof(char));
	}
	strcpy(tlist->col_label[0], "Name");
	strcpy(tlist->col_label[1], "Value");

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
	if (data_num==0)
		return 0;
	status = 0;
	init = UU_TRUE;		
	itemnum = 0;
	while (um_getallobjs(init, &e.key) == 0)
	{
		init = UU_FALSE;
		lbuf[0] = '\0';
		if ( e.key >= UR_low_key)
		{
			ur_retrieve_data_relnum(e.key,&e.rel_num);
			if (((e.rel_num == NCL_SCALAR_REL) && ((type==0)||(type==-1)))
				|| (( e.rel_num == NCL_TEXTVAR_REL ) && ((type==1)||(type==-1)))
				|| (( e.rel_num == NCL_DATAST_REL ) && ((type==2)||(type==-1))))
			{
/*
.....get the label and value and put into the list
*/
				if (e.rel_num == NCL_SCALAR_REL)
				{
					scalrec.key = e.key;
					status = ncl_retrieve_data_fixed(&scalrec);
					strncpy (tempstr, scalrec.label, NCL_MAX_LABEL);
					tempstr[64] = '\0';
/*
.....remove trailing spaces
*/
					i = strlen(tempstr);
					while ((i!=0) &&(tempstr[i-1]==' ')) i--;
						tempstr[i] = '\0';
/*
.....see if it mactch the fileter
*/
					if (!( (ifl == 1) ||
						(ifl == 2 && strncmp(filt,tempstr,fnc) == 0) ||
						(ifl == 3 && strcmp(filt, &tempstr[strlen(tempstr)-fnc]) == 0) ||
						(ifl == 4 && strstr(tempstr,filt) != 0) ) )
						continue;
					if (!((clas==NULL)||(strcmp(scalrec.classnm, clas)==0)
							||(strcmp(clas, "All")==0)))
						continue;
					if (scalrec.subscr>0)
						sprintf (tempstr, "%s(%d)", tempstr, scalrec.subscr);					
					len = strlen (tempstr);
					tlist->data[itemnum].data_items[0] = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy(tlist->data[itemnum].data_items[0], tempstr);
					sprintf (lbuf, "%f", scalrec.scalar_value);
					len = strlen (lbuf);
					tlist->data[itemnum].data_items[1] = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy(tlist->data[itemnum].data_items[1], lbuf);
					itemnum++;
				}
				else if ( e.rel_num == NCL_TEXTVAR_REL ) 
				{
					txtrec.key = e.key;
					status = ncl_retrieve_data_fixed(&txtrec);
					strncpy (tempstr, txtrec.label, NCL_MAX_LABEL);
					tempstr[64] = '\0';
/*
.....remove trailing spaces
*/
					i = strlen(tempstr);
					while ((i!=0) &&(tempstr[i-1]==' ')) i--;
						tempstr[i] = '\0';
/*
.....see if it mactch the fileter
*/
					if (!( (ifl == 1) ||
						(ifl == 2 && strncmp(filt,tempstr,fnc) == 0) ||
						(ifl == 3 && strcmp(filt, &tempstr[strlen(tempstr)-fnc]) == 0) ||
						(ifl == 4 && strstr(tempstr,filt) != 0) ) )
						continue;
					if (txtrec.subscr>0)
						sprintf (tempstr, "%s(%d)", tempstr, txtrec.subscr);
					len = strlen (tempstr);
					tlist->data[itemnum].data_items[0] = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy(tlist->data[itemnum].data_items[0], tempstr);
					len = strlen (txtrec.text);
					tlist->data[itemnum].data_items[1] = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy(tlist->data[itemnum].data_items[1], txtrec.text);
					itemnum++;
				}
				else if ( e.rel_num == NCL_DATAST_REL ) 
				{
					datarec.key = e.key;
					status = ncl_retrieve_data_fixed(&datarec);
					if (status == UU_SUCCESS && datarec.no_datael>0)
					{
						strncpy (tempstr, datarec.label, NCL_MAX_LABEL);
						tempstr[64] = '\0';
/*
.....remove trailing spaces
*/
						i = strlen(tempstr);
						while ((i!=0) &&(tempstr[i-1]==' ')) i--;
							tempstr[i] = '\0';
						if (datarec.subscr>0)
							sprintf (tempstr, "%s(%d)", tempstr, datarec.subscr);
/*
.....see if it mactch the fileter
*/
						if (!( (ifl == 1) ||
							(ifl == 2 && strncmp(filt,tempstr,fnc) == 0) ||
							(ifl == 3 && strcmp(filt, &tempstr[strlen(tempstr)-fnc]) == 0) ||
							(ifl == 4 && strstr(tempstr,filt) != 0) ) )
							continue;
						len = strlen (tempstr);
						tlist->data[itemnum].data_items[0] = (char*)uu_malloc((len+1)*sizeof(char));
						strcpy(tlist->data[itemnum].data_items[0], tempstr);

						lbuf[0] = '\0';
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
							strcat (lbuf, tempstr);
							ncl_get_delim(dsp[i].delim, tempstr);
							strcat (lbuf, tempstr);
						}
						len = strlen (lbuf);
						tlist->data[itemnum].data_items[1] = (char*)uu_malloc((len+1)*sizeof(char));
						strcpy(tlist->data[itemnum].data_items[1], lbuf);
						itemnum++;
					}
				}
			}
			else
				continue;
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
	tlist->num_item = itemnum;
	return (itemnum);
}

/*********************************************************************
**   I_FUNCTION: filter_type(fieldno,val,stat)
**      Callback function for the "Data type" choice button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT filter_type(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int chc = val->frmint[0];
	if (chc>1)
	{
		ud_setfrm_traverse_mask(0, 3, UU_FALSE);
	}
	else
	{
		ud_setfrm_traverse_mask(0, 3, UU_TRUE);
	}
	return filter_data(fieldno,val,stat);
}

/*********************************************************************
**   I_FUNCTION: filter_class(fieldno,val,stat)
**      Callback function for the "scalar class" choice button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT filter_class(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_DDATA fdat, cdat;
	int chc;
	char fstr[21], clas[21];

	fdat.frmstr = fstr;
	ud_get_field(1,fdat,UU_FALSE);
	ud_get_field(2, &chc,UU_FALSE);
	cdat.frmstr = clas;
	ud_get_field(3, cdat,UU_FALSE);
	ud_free_tlist(&Sin_data);
	chc--;
	Sin_data.num_item = ncl_get_dataname_tlist(&Sin_data, uni_flag, chc, fstr, clas);
	ud_dispfrm_update_answer(0, 4, (int *)&Sin_data);
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: filter_data(fieldno,val,stat)
**      Callback function for the "Filter" button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT filter_data(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_DDATA fdat;
	int chc;
	char fstr[21];

	fdat.frmstr = fstr;
	ud_get_field(1,fdat,UU_FALSE);
	ud_get_field(2, &chc,UU_FALSE);
	ud_free_tlist(&Sin_data);

	chc--;
	Sin_data.num_item = ncl_get_dataname_tlist(&Sin_data, uni_flag, chc, fstr, NULL);
	ud_dispfrm_update_answer(0, 4, (int *)&Sin_data);
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : nclu_get_data
**       get variables/datas from secondary unibase
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_get_data()
{
	NCL_cmdbuf cmdbuf;
	int i,j,len;
	char *strtok(), tempstr[80], *tok;
	char lbuf[80];
	int status, *ans[8],opt;
	static UD_METHOD methods[] =
	{
		filter_data, filter_data, filter_type, filter_class, OnTableCalbacks1, OnTableCalbacks2, OnGetData, OnRemoveData
	};
	static char called[]       = {6,6,6, 6,6,6, 6, 6};
	static char traverse[]     = {1,1,1, 1,1,1,1, 1,1};
	static char disp[]         = {1,1,1, 1,1,1,1,1,1};
	static int dtyp = 0;
	UD_METHOD save_entry;
	UD_LIST class_list;
	int mainmarkval=0;
	char **ncl_get_scalar_class();

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
/*
.....Initialize routine
*/
	S_data_seldata1.data_items = NULL;
	S_data_seldata2.data_items = NULL;

	uni_flag = 2;
	Sin_data.num_item = ncl_get_dataname_tlist(&Sin_data,  2, dtyp-1, "*", NULL);
	
	Sout_data.num_col = 2; 
	Sout_data.num_item = 0;
	Sout_data.answer = 0;
	if (Sout_data.num_col>0)
		Sout_data.col_label = (char**) uu_malloc(Sout_data.num_col*sizeof (char*));
	for (i=0; i<Sout_data.num_col;i++)
	{
		Sout_data.col_label[i] = (char*) uu_malloc(20*sizeof(char));
	}
	strcpy(Sout_data.col_label[0], "Name");
	strcpy(Sout_data.col_label[1], "Value");

	strcpy(filter, "*");
	
	class_list.item = ncl_get_scalar_class(&(class_list.num_item),2);
	if (class_list.num_item==0)
	{
		sclass_list.num_item = 1;
		sclass_list.item = (char **)uu_malloc(sizeof (char*));
		sclass_list.item[0] = (char *)uu_malloc(21*sizeof (char));
		strcpy(sclass_list.item[0], "All");
	}
	else
	{
		ud_list_sort(&class_list);
		class_list.answer = (char *) uu_malloc(21 * sizeof(char));
		sclass_list.num_item = class_list.num_item + 1;					
		sclass_list.item = (char **) uu_malloc(sclass_list.num_item *sizeof(char *));
		sclass_list.item[0] = (char *) uu_malloc(21 *sizeof(char *));
		strcpy(sclass_list.item[0], "All");

		for (i=0,j=1; i<class_list.num_item; i++, j++)
		{
			len = strlen (class_list.item[i]);
			sclass_list.item[j] = (char *) uu_malloc((len+1) * sizeof(char));
			strcpy(sclass_list.item[j], class_list.item[i]);
		}
	}
	sclass_list.answer = (char *) uu_malloc(21 * sizeof(char));
	strcpy(sclass_list.answer, "All");

	ans[0] = (int *)&opt;;
	ans[1] = (int *)&filter;;
	ans[2] = (int *)&dtyp;
	ans[3] = (int *) &sclass_list;
	ans[4] = (int *)&Sin_data;
	ans[5] = (int *)&Sout_data;
	ans[6] = NULL;
	ans[7] = NULL;
	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = S_init_datafrm;
/*
.....Command Reject
*/
	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		UD_initfrm_intry = save_entry;
		ud_free_tlist(&Sin_data);
		ud_free_tlist(&Sout_data);
		ud_tlist_free_idata(&S_data_seldata1);
		ud_tlist_free_idata(&S_data_seldata2);
		ud_free_flist(&sclass_list);
		UD_UNMARK (mainmarkval);
		return;
	}
	status = ud_form1("datatran.frm",ans,ans,methods,called,disp,traverse);
	UD_initfrm_intry = save_entry;
	ud_free_tlist(&Sin_data);
	ud_free_flist(&sclass_list);
	if (status == -1) 
	{
		ud_free_tlist(&Sout_data);
		ud_tlist_free_idata(&S_data_seldata1);
		ud_tlist_free_idata(&S_data_seldata2);
		goto done;
	}
/*
.....Initialize the command buffer.
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lbuf,"GET/");	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);

	for (i=0; i<Sout_data.num_item; i++)
	{
		strcpy (tempstr, Sout_data.data[i].data_items[0]);
		ncl_add_token(&cmdbuf, tempstr, NCL_comma);
	}
	if (Sout_data.num_item>=1)
	{
		ncl_del_token(&cmdbuf,"", UU_TRUE);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
	ud_free_tlist(&Sout_data);
	ud_tlist_free_idata(&S_data_seldata1);
	ud_tlist_free_idata(&S_data_seldata2);
	goto done;
done:;
	UD_UNMARK (mainmarkval);
}

/*********************************************************************
**    E_FUNCTION     : nclu_put_data
**       put variables/data into secondary unibase
**    PARAMETERS   
**       INPUT  : None
**                
**       OUTPUT :  
**						None
**    RETURNS      : none
**       
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_put_data()
{
	NCL_cmdbuf cmdbuf;
	int i,j,len;
	char *strtok(), tempstr[80], *tok;
	char lbuf[80];
	int status, *ans[8],opt;
	UD_LIST class_list;
	char **ncl_get_scalar_class();
	static UD_METHOD methods[] =
	{
		filter_data, filter_data, filter_type, filter_class, OnTableCalbacks1, OnTableCalbacks2, OnGetData, OnRemoveData
	};
	static char called[]       = {6,6,6, 6,6,6, 6,6};
	static char traverse[]     = {1,1,1, 1,1,1,1,1};
	static char disp[]         = {1,1,1, 1,1,1,1,1,1};
	static int dtyp = 0;
	int mainmarkval=0;

	if (UBopen[0]=='\0')
	{
		ud_wrerr("No secondary unibase file opened!");
		return;
	}
/*
.....Initialize routine
*/
	S_data_seldata1.data_items = NULL;
	S_data_seldata2.data_items = NULL;

	uni_flag = 1;
	Sin_data.num_item = ncl_get_dataname_tlist(&Sin_data,  1, dtyp-1, "*", NULL);
	Sout_data.num_col = 2; 
	Sout_data.num_item = 0;
	Sout_data.answer = 0;
	if (Sout_data.num_col>0)
		Sout_data.col_label = (char**) uu_malloc(Sout_data.num_col*sizeof (char*));
	for (i=0; i<Sout_data.num_col;i++)
	{
		Sout_data.col_label[i] = (char*) uu_malloc(20*sizeof(char));
	}
	strcpy(Sout_data.col_label[0], "Name");
	strcpy(Sout_data.col_label[1], "Value");
		
	strcpy(filter, "*");
	class_list.item = ncl_get_scalar_class(&(class_list.num_item),1);
/*
.....ncl_get_scalar_class does not include "All" option but we need it
*/
	if (class_list.num_item==0)
	{
		sclass_list.num_item = 1;
		sclass_list.item = (char **)uu_malloc(sizeof (char*));
		sclass_list.item[0] = (char *)uu_malloc(21*sizeof (char));
		strcpy(sclass_list.item[0], "All");
	}
	else
	{
		ud_list_sort(&class_list);
		class_list.answer = (char *) uu_malloc(21 * sizeof(char));
		sclass_list.num_item = class_list.num_item + 1;					
		sclass_list.item = (char **) uu_malloc(sclass_list.num_item *sizeof(char *));
		sclass_list.item[0] = (char *) uu_malloc(21 *sizeof(char *));
		strcpy(sclass_list.item[0], "All");

		for (i=0,j=1; i<class_list.num_item; i++, j++)
		{
			len = strlen (class_list.item[i]);
			sclass_list.item[j] = (char *) uu_malloc((len+1) * sizeof(char));
			strcpy(sclass_list.item[j], class_list.item[i]);
		}
	}
	sclass_list.answer = (char *) uu_malloc(21 * sizeof(char));
	strcpy(sclass_list.answer, "All");

	ans[0] = (int *)&opt;;
	ans[1] = (int *)&filter;;
	ans[2] = (int *)&dtyp;
	ans[3] = (int *) &sclass_list;
	ans[4] = (int *)&Sin_data;
	ans[5] = (int *)&Sout_data;
	ans[6] = NULL;
	ans[7] = NULL;
/*
.....Command Reject
*/
	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		ud_free_tlist(&Sin_data);
		ud_free_tlist(&Sout_data);
		ud_tlist_free_idata(&S_data_seldata1);
		ud_tlist_free_idata(&S_data_seldata2);
		ud_free_flist(&sclass_list);
		UD_UNMARK (mainmarkval);
		return;
	}
	status = ud_form1("datatran.frm",ans,ans,methods,called,disp,traverse);
	ud_free_tlist(&Sin_data);
	ud_free_flist(&sclass_list);
	if (status == -1) 
	{
		ud_free_tlist(&Sout_data);
		ud_tlist_free_idata(&S_data_seldata1);
		ud_tlist_free_idata(&S_data_seldata2);
		goto done;
	}
/*
.....Initialize the command buffer.
*/
	ncl_init_cmdbuf(&cmdbuf);
	strcpy(lbuf,"PUT/");	
	ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);		

	for (i=0; i<Sout_data.num_item; i++)
	{
		strcpy (tempstr, Sout_data.data[i].data_items[0]);
		ncl_add_token(&cmdbuf, tempstr, NCL_comma);
	}
	if (Sout_data.num_item>=1)
	{
		ncl_del_token(&cmdbuf,"", UU_TRUE);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
	ud_free_tlist(&Sout_data);
	ud_tlist_free_idata(&S_data_seldata1);
	ud_tlist_free_idata(&S_data_seldata2);
	goto done;
done:;
	UD_UNMARK (mainmarkval);
}

void ncl_get_delim (num, str)
int num;
char *str;
{
	str[0] = '\0';
	if (num==9)
		strcpy (str, ",");
	else if (num==5)
		strcpy (str, "/");
	else if (num==1)
		strcpy (str, "=");
	else if (num==11)
		str[0] = '\0';
	else if (num==2)
		strcpy (str, "+");
	else if (num==3)
		strcpy (str, "-");
	else if (num==13)
		strcpy (str, ":");
	else if (num==4)
		strcpy (str, "*");
	else if (num==10)
		strcpy (str, "**");
	else if (num==6)
		strcpy (str, "(");
	else if (num==7)
		strcpy (str, ")");
	else if (num==8)
		strcpy (str, ".");
	else if (num==14)
		strcpy (str, "\'");
	else if (num==15)
		strcpy (str, "[");
	else if (num==16)
		strcpy (str, "]");
	else if (num==17)
		strcpy (str, "^");
	else if (num==18)
		strcpy (str, "&");
	else if (num==19)
		strcpy (str, "{");
	else if (num==20)
		strcpy (str, "}");
}
