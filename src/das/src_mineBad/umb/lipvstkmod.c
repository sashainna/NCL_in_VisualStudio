/*********************************************************************
**   FILENAME: lipvstkmod.c
**   CONTAINS: 
**             ul_ipv_stock_mod()
**             ul_ipv_stock_attr()
**
**     COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       lipvstkmod.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 11:59:10
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "lcom.h"
#include "mdcpln.h"
#include "mdrel.h"
#include "mfort.h"
#include "nclfc.h"
#include "nccs.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "lipv.h"

#define FACOL 0
#define FAVIS 1
#define FAACT 2
#define FARST 3
#define FAIMP 4
#define FATRL 5
#define FATOL 6
#define FAEDG 7
#define FAEDC 8
#define FAXFL 9
#define FAXNM 10
#define FAXLD 11
#define FAXMD 12
#define FAXIN 13
#define FAXAX 14
#define FAXAC 15
#define FAGEO 16
#define FAALL 17
#define FADEL 18
#define FAVIW 19
#define FAAPP 20

#define FMIDN 0
#define FMCOL 1
#define FMTRL 2
#define FMIMP 3
#define FMVIS 4
#define FMACT 5
#define FMTOL 6
#define FMEDG 7
#define FMEDC 8
#define FMRES 9
#define FMDEL 10
#define FMBIN 11
#define FMSTP 12
#define FMDEA 13
#define FMSKP 14
#define FMXFL 15
#define FMXNM 16
#define FMXLD 17
#define FMXMD 18
#define FMXAX 19
#define FMXAC 20

static UD_FSTAT OnMatrix(),OnMxname(),OnLoadmx(),OnModmx();
static UD_FSTAT OnDelete(),OnView(),OnSelect(),OnApply();
static void S_modify_stock(),S_store_matrix();

static int stock_sel,stock_which;
static UU_LOGICAL allfl=UU_FALSE,freset,*Sreset,Sincr=UU_FALSE;
static char stock_cmd[80];
char *stk_ptr;
static UD_LIST geom_list;
static LW_stock_struc stock_cur,*attr_ptr;
static int Smxfrm = -1;
static LtMaterial Soldmat,Soldm;
static int Sndel;
static LtBoolean Sedge[50];
static LtColour Secolor[50];
static UD_LIST Smxlist;

static char *gname[]={"Stock","Fixture"};
extern char uw_color_name[64][96];

/*********************************************************************
**   E_FUNCTION: ul_ipv_stock_mod(which)
**      This function controls the Stock Modals form.
**   PARAMETERS
**       INPUT  : which = 0 - Stock modals.
**                        1 - Fixture modals.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_stock_mod(which)
int which;
{
	int status,color,vis,act,tran,bin,id,edg,edgcol,import,resatr,delstk;
	UU_LOGICAL sstop,sdeact,sskip;
	UU_REAL toler;
	int *ans[FMXAC+2];	/* default answers/answers for form */
	UU_LOGICAL cmdreject;
	static UD_METHOD methods[] = {UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL, UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		OnMatrix,OnMxname,OnLoadmx,OnModmx,UU_NULL,UU_NULL};
	static char called[]   = {6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6};
	static char disp[]     = {0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1};
	static char traverse[] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1};
/*
.....Load the input values into
.....local storage area
*/
	Soldmat = 0;
	disp[which] = 1;
	disp[1-which] = 0;
	id = LW_stock_idn[which];
	color = LW_stock_default[which].color;
	vis = (int)LW_stock_default[which].visible;
	act = (int)LW_stock_default[which].active;
	tran = (int)LW_stock_default[which].translucency;
	import = LW_stock_default[which].important;
	UM_len_inttoext(LW_stock_default[which].toler,toler);
	edg = (int)LW_stock_default[which].edge;
	edgcol = LW_stock_default[which].edge_color;
	resatr = LW_reset_attr[which];
	delstk = LW_delete_stocks;
	bin = (int)LW_stl_format;
	sstop = (int)LW_stl_flag[0];
	sdeact = (int)LW_stl_flag[1];
	sskip = (int)LW_stl_flag[2];

	stock_cur.mxflag = LW_stock_default[which].mxflag;
	strcpy(stock_cur.mxname,LW_stock_default[which].mxname);
	um_tftotf(LW_stock_default[which].matrix,stock_cur.matrix);
	stock_cur.axes = LW_stock_default[which].axes;
	stock_cur.axes_color = LW_stock_default[which].axes_color;

	nclu_load_matrix_list(&Smxlist,stock_cur.mxname);

	ans[FMIDN] = (int *)&id;
	ans[FMCOL] = (int *)&color;
	ans[FMTRL] = (int *)&tran;
	ans[FMIMP] = (int *)&import;
	ans[FMVIS] = (int *)&vis;
	ans[FMACT] = (int *)&act;
	ans[FMTOL] = (int *)&toler;
	ans[FMEDG] = (int *)&edg;
	ans[FMEDC] = (int *)&edgcol;
	ans[FMRES] = (int *)&resatr;
	ans[FMDEL] = (int *)&delstk;
	ans[FMBIN] = (int *)&bin;
	ans[FMSTP] = (int *)&sstop;
	ans[FMDEA] = (int *)&sdeact;
	ans[FMSKP] = (int *)&sskip;
	ans[FMXFL] = (int *)&stock_cur.mxflag;
	ans[FMXNM] = (int *)&Smxlist;
	ans[FMXLD] = UU_NULL;
	ans[FMXMD] = UU_NULL;
	ans[FMXAX] = (int *)&stock_cur.axes;
	ans[FMXAC] = (int *)&stock_cur.axes_color;
/*
.....Set traversal flags
*/
	if (stock_cur.mxflag)
	{
		traverse[FMXNM] = 1;
		traverse[FMXLD] = 1;
		traverse[FMXMD] = 1;
	}
	else
	{
		traverse[FMXNM] = 0;
		traverse[FMXLD] = 0;
		traverse[FMXMD] = 0;
	}
	if (LW_nclipv == LW_STANDALONE)
	{
		disp[FMXNM+2] = disp[FMXLD+2] = 0;
		traverse[FMXNM] = traverse[FMXLD] = 0;
	}
/*
.....Get the Form input
*/
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{
		status = ud_form1("ipvstkmod.frm", ans, ans, methods, called, disp, 
			traverse);
		if (status==-1)
			goto done;
	}	
	else
		goto done;
/*
.....Store Stock Modals
*/
	LW_stock_idn[which] = id;
	LW_stock_default[which].color = color;
	LW_stock_default[which].visible = vis;
	LW_stock_default[which].active = act;
	LW_stock_default[which].translucency = tran;
	LW_stock_default[which].important = import;
	LW_stock_default[which].edge = edg;
	LW_stock_default[which].edge_color = edgcol;
	LW_delete_stocks = delstk;
	LW_reset_attr[which] = resatr;
	UM_len_exttoint(toler,LW_stock_default[which].toler);
	LW_stl_format = bin;
	LW_stl_flag[0] = sstop; LW_stl_flag[1] = sdeact; LW_stl_flag[2] = sskip;

	LW_stock_default[which].mxflag = stock_cur.mxflag;
	strcpy(LW_stock_default[which].mxname,Smxlist.answer);
	um_tftotf(stock_cur.matrix,LW_stock_default[which].matrix);
	LW_stock_default[which].axes = stock_cur.axes;
	LW_stock_default[which].axes_color = stock_cur.axes_color;
/*
.....Save modals file
*/
	S_save_modfile(which);
done:
	ud_free_flist(&Smxlist);
	UD_UNMARK(cmdreject);
	return;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_stock_attr(which)
**      This function controls the Stock Attributes form.
**   PARAMETERS
**       INPUT  : which = 0 - Stock modals.
**                        1 - Fixture modals.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_stock_attr(which)
int which;
{
	int status,i;
	UU_LOGICAL fflag = UU_FALSE,cmdreject;
	UU_REAL det;
	char *ptr1,sbuf[200];
	LW_stock_struc *sptr;

	int *ans[FAAPP+2];
	static UD_METHOD methods[] = {UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		OnMatrix,OnMxname,OnLoadmx,OnModmx,UU_NULL,UU_NULL,UU_NULL,
		OnSelect,UU_NULL,OnDelete,OnView,OnApply};
	static char called[]   = {6,6,6,6,6,6,6, 6,6, 6,6,6,6,6,6,6, 6, 6,6,6,6};
	static char traverse[] = {1,1,1,1,1,1,1, 1,1, 1,1,1,1,1,1,1, 1, 1,1,1,1};
	static char disp[]     = {0,0,1,1,1,1,1,1,1, 1,1, 1,1,1,1,1,1,1, 1, 1,1,1,1};
/*
.....Initialize routine
*/
	Sndel = 0;
	stk_ptr = UU_NULL;
	geom_list.item = UU_NULL;
	attr_ptr = UU_NULL;
	Sreset = UU_NULL;
	if (LW_nstock[which] == 0) goto nogeom;
	disp[which] = 1;
	disp[1-which] = 0;
	stock_which = which;
	stock_sel = 0;
/*
.....Allocate memory for lists
*/
	stk_ptr = (char *)uu_malloc(sizeof(char)*80*LW_nstock[which]);
	if (stk_ptr == UU_NULL) goto nomem;
	attr_ptr =
		(LW_stock_struc *)uu_malloc(sizeof(LW_stock_struc)*LW_nstock[which]);
	if (attr_ptr == UU_NULL) goto nomem;
	geom_list.item = (char **)uu_malloc(LW_nstock[which] * sizeof(char *));
	if (geom_list.item == UU_NULL) goto nomem;
	Sreset = (int *)uu_malloc(sizeof(int)*LW_nstock[which]);
/*
.....Initialize Reset Colors settings
*/
	freset = UU_FALSE;
	for (i=0;i<LW_nstock[which];i++) Sreset[i] = UU_FALSE;
/*
.....Get list of all stocks/fixtures
*/
	sptr = LW_stock_first[which];
	ptr1 = stk_ptr;
	for (i=0;i<LW_nstock[which];i++)
	{
		ul_ipv_stock_cmd(which,sptr,sbuf,sizeof(sbuf),UU_TRUE);
		sbuf[65] = '\0';
		sprintf(ptr1,"%d  %s",sptr->id,sbuf);
		geom_list.item[i] = ptr1;
		uu_move_byte(sptr,&attr_ptr[i],sizeof(LW_stock_struc));
		attr_ptr[i].color = attr_ptr[i].color;
		sptr = (LW_stock_struc *)uu_lsnext(sptr);
		ptr1 = ptr1 + 80;
	}
	strcpy(stock_cmd,stk_ptr);
	geom_list.num_item = LW_nstock[which];
	geom_list.answer = stock_cmd;
/*
.....Load the input values into
.....local storage area
*/
	stock_cur.type = LW_stock_first[which]->type;
	stock_cur.stock = LW_stock_first[which]->stock;
	stock_cur.color = LW_stock_first[which]->color;
	stock_cur.visible = (int)LW_stock_first[which]->visible;
	stock_cur.active = (int)LW_stock_first[which]->active;
	stock_cur.translucency = (int)LW_stock_first[which]->translucency;
	stock_cur.important = (int)LW_stock_first[which]->important;
	UM_len_inttoext(LW_stock_first[which]->toler,stock_cur.toler);
	stock_cur.edge = LW_stock_first[which]->edge;
	stock_cur.edge_color = LW_stock_first[which]->edge_color;
	stock_cur.bin = LW_stock_first[which]->bin;
	stock_cur.data = LW_stock_first[which]->data;

	stock_cur.mxflag = LW_stock_first[which]->mxflag;
	strcpy(stock_cur.mxname,LW_stock_first[which]->mxname);
	um_tftotf(LW_stock_first[which]->matrix,stock_cur.matrix);
	stock_cur.axes = LW_stock_first[which]->axes;
	stock_cur.axes_color = LW_stock_first[which]->axes_color;

	nclu_load_matrix_list(&Smxlist,stock_cur.mxname);

	ans[FACOL] = (int *)&stock_cur.color;
	ans[FAVIS] = (int *)&stock_cur.visible;
	ans[FAACT] = (int *)&stock_cur.active;
	ans[FARST] = (int *)&freset;
	ans[FATRL] = (int *)&stock_cur.translucency;
	ans[FAIMP] = (int *)&stock_cur.important;
	ans[FATOL] = (int *)&stock_cur.toler;
	ans[FAEDG] = (int *)&stock_cur.edge;
	ans[FAEDC] = (int *)&stock_cur.edge_color;

	ans[FAXFL] = (int *)&stock_cur.mxflag;
	ans[FAXNM] = (int *)&Smxlist;
	ans[FAXLD] = UU_NULL;
	ans[FAXMD] = UU_NULL;
	ans[FAXIN] = &Sincr;
	ans[FAXAX] = (int *)&stock_cur.axes;
	ans[FAXAC] = (int *)&stock_cur.axes_color;

	ans[FAGEO] = (int *)&geom_list;
	ans[FAALL] = (int *)&allfl;
	ans[FADEL] = UU_NULL; ans[FAVIW] = UU_NULL;
/*
.....Set traversal flags
*/
	if (stock_cur.mxflag)
	{
		traverse[FAXNM] = 1;
		traverse[FAXLD] = 1;
		traverse[FAXMD] = 1;
	}
	else
	{
		traverse[FAXNM] = 0;
		traverse[FAXLD] = 0;
		traverse[FAXMD] = 0;
	}
	if (LW_active) traverse[FAAPP] = 1;
	else traverse[FAAPP] = 0;
	if (LW_nclipv == LW_STANDALONE)
	{
		disp[FAXNM+2] = disp[FAXLD+2] = 0;
		traverse[FAXNM] = traverse[FAXLD] = 0;
	}
/*
........Highlight stock
*/
	if (LW_active)
	{
		ul_ipv_highlight_stock(&Soldmat,&stock_cur,
			LW_material[LW_highlight_color],&Sedge,Secolor);
	}
/*
.....Get the Form input
*/
	UD_MARK(cmdreject, UU_FALSE);
	fflag = UU_TRUE;
	if (!cmdreject)
	{
		do
		{
			status = ud_form1("ipvstkattrib.frm", ans, ans, methods, called, disp, 
				traverse);
			if (status==-1)
				goto done;
/*
........Make sure it is not a mirror matrix
*/
			if (stock_cur.mxflag)
			{
				um_determinant(stock_cur.matrix,&det);
				if (det <= 0.)
				ud_wrerr("A mirror matrix is not allowed.");
			}
			else
				det = 1.0;
		} while (det <= 0.);
	}	
	else
		goto done;
/*
.....Store Stock Modals
*/
	S_modify_stock(UU_TRUE);
	Soldmat = 0;
/*
.....Update important solids
*/
	ul_ipv_display_obstruct(UU_FALSE);
	goto done;
/*
.....No stocks defined
*/
nogeom:;
	sprintf(sbuf,"No %ss have been defined.",gname[which]);
	ud_wrerr(sbuf);
	goto done;
/*
.....Could not allocate memory
*/
nomem:;
	printf("Could not allocate memory for form.");
	goto done;
/*
.....End of routine
*/
done:
	ud_free_flist(&Smxlist);
	if (stk_ptr != UU_NULL) uu_free(stk_ptr);
	if (geom_list.item != UU_NULL) uu_free(geom_list.item);
	if (attr_ptr != UU_NULL) uu_free(attr_ptr);
	if (Smxfrm != -1) 
		ud_close_dispfrm(Smxfrm);
	Smxfrm = -1;
/*
.....Unhighlight stock
*/
	if (LW_active && Soldmat != 0)
	{
		ul_ipv_highlight_stock(&Soldm,&stock_cur,Soldmat,&Sedge,Secolor);
	}
	if (fflag)
/*
.....UD_UNMARK is more than one statement, so it have to have {}
*/
	{
		UD_UNMARK(cmdreject);
	}
/*
.....Redisplay NCLIPV window
.....in case there are stock axes displayed
*/
	ul_ipv_view_segs();
	return;
}

/*********************************************************************
**   I_FUNCTION: OnSelect(fieldno,val,stat)
**      Callback function for the stock list.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelect(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char *ptr1,str[80];
	int i,j,inc,f;
	UD_DDATA data;
/*
.....Store attributes for last selected stock
*/
	data.frmstr = str;
	ud_get_field(3,data,UU_FALSE);
	i = strlen(str);
	ul_strip_blanks(str,&i);
	ul_to_number(str,&stock_cur.translucency);
	ud_get_field(4,data,UU_FALSE);
	i = strlen(str);
	ul_strip_blanks(str,&i);
	ul_to_reals(&stock_cur.toler,&i,1,str);
	attr_ptr[stock_sel].type = stock_cur.type;
	attr_ptr[stock_sel].stock = stock_cur.stock;
	attr_ptr[stock_sel].color = stock_cur.color;
	attr_ptr[stock_sel].visible = stock_cur.visible;
	attr_ptr[stock_sel].active = stock_cur.active;
	attr_ptr[stock_sel].translucency = stock_cur.translucency;
	attr_ptr[stock_sel].important = stock_cur.important;
	UM_len_exttoint(stock_cur.toler,attr_ptr[stock_sel].toler);
	attr_ptr[stock_sel].edge = stock_cur.edge;
	attr_ptr[stock_sel].edge_color = stock_cur.edge_color;
	attr_ptr[stock_sel].mxflag = stock_cur.mxflag;
	attr_ptr[stock_sel].axes = stock_cur.axes;
	attr_ptr[stock_sel].axes_color = stock_cur.axes_color;
	attr_ptr[stock_sel].bin = stock_cur.bin;
	attr_ptr[stock_sel].data = stock_cur.data;
	strcpy(attr_ptr[stock_sel].mxname,stock_cur.mxname);
	um_tftotf(stock_cur.matrix,attr_ptr[stock_sel].matrix);
	Sreset[stock_sel] = freset;
/*
.....Unhighlight stock
*/
	if (LW_active && Soldmat != 0)
	{
		ul_ipv_highlight_stock(&Soldm,&stock_cur,Soldmat,&Sedge,Secolor);
		Soldmat = 0;
	}
/*
.....Get stock selection
*/
	ptr1 = stk_ptr;
	for (i=0;i<LW_nstock[stock_which];i++)
	{
		if (strcmp(ptr1,val->frmstr) == 0)
		{
			strcpy(stock_cmd,val->frmstr);
			stock_sel = i;
			break;
		}
		ptr1 = ptr1 + 80;
	}
/*
.....Change the attribute fields
*/
	if (stock_sel < LW_nstock[stock_which])
	{
		stock_cur.type = attr_ptr[stock_sel].type;
		stock_cur.stock = attr_ptr[stock_sel].stock;
		stock_cur.color = attr_ptr[stock_sel].color;
		stock_cur.visible = attr_ptr[stock_sel].visible;
		stock_cur.active = attr_ptr[stock_sel].active;
		stock_cur.translucency = attr_ptr[stock_sel].translucency;
		stock_cur.important = attr_ptr[stock_sel].important;
		UM_len_inttoext(attr_ptr[stock_sel].toler,stock_cur.toler);
		stock_cur.edge = attr_ptr[stock_sel].edge;
		stock_cur.edge_color = attr_ptr[stock_sel].edge_color;
		stock_cur.mxflag = attr_ptr[stock_sel].mxflag;
		strcpy(stock_cur.mxname,attr_ptr[stock_sel].mxname);
		um_tftotf(attr_ptr[stock_sel].matrix,stock_cur.matrix);
		stock_cur.axes = attr_ptr[stock_sel].axes;
		stock_cur.axes_color = attr_ptr[stock_sel].axes_color;
		stock_cur.bin = attr_ptr[stock_sel].bin;
		stock_cur.data = attr_ptr[stock_sel].data;
		freset = Sreset[stock_sel];
/*
........Update Matrix form
*/
		if (Smxfrm != -1)
		{
			if (stock_cur.mxflag == 0)
			{
				ud_close_dispfrm(Smxfrm);
				Smxfrm = -1;
			}
			else
			{
				ud_dispfrm_update_answer(Smxfrm,0,(int *)&stock_cur.mxname);
				inc = 1;
				for (j=0;j<3;j++)
				{
					for (i=0;i<4;i++)
						ud_dispfrm_update_answer(Smxfrm,inc++,
							(int *)&stock_cur.matrix[i][j]);
				}
				ud_update_form(Smxfrm);
			}
		}
/*
........Update main form
*/
		ud_update_answer(FACOL,(int *)&stock_cur.color);
		ud_update_answer(FAVIS,(int *)&stock_cur.visible);
		ud_update_answer(FAACT,(int *)&stock_cur.active);
		ud_update_answer(FATRL,(int *)&stock_cur.translucency);
		ud_update_answer(FAIMP,(int *)&stock_cur.important);
		ud_update_answer(FATOL,(int *)&stock_cur.toler);
		ud_update_answer(FAEDG,(int *)&stock_cur.edge);
		ud_update_answer(FAEDC,(int *)&stock_cur.edge_color);
		ud_update_answer(FAXFL,(int *)&stock_cur.mxflag);
		strcpy(Smxlist.answer,stock_cur.mxname);
		ud_update_answer(FAXNM,(int *)&Smxlist);
		ud_update_answer(FAXAX,(int *)&stock_cur.axes);
		ud_update_answer(FAXAC,(int *)&stock_cur.axes_color);
		ud_update_answer(FARST,(int *)&freset);
		f = FAXFL;
		OnMatrix(&f,val,stat);
/*
........Highlight stock
*/
		if (LW_active)
		{
			ul_ipv_highlight_stock(&Soldmat,&stock_cur,
				LW_material[LW_highlight_color],&Sedge,Secolor);
		}
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnMatrix(fieldno,val,stat)
**      Callback function to enable/disable transformation.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnMatrix(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,k;
/*
.....Initialize field pointers
*/
	if (*fieldno == FMXFL)
	{
		i = FMXNM;
		j = FMXLD;
		k = FMXMD;
	}
	else
	{
		i = FAXNM;
		j = FAXLD;
		k = FAXMD;
	}
/*
.....Set traversal flags
*/
	if (stock_cur.mxflag)
	{
		ud_set_traverse_mask(i,1);
		ud_set_traverse_mask(j,1);
		ud_set_traverse_mask(k,1);
	}
	else
	{
		ud_set_traverse_mask(i,0);
		ud_set_traverse_mask(j,0);
		ud_set_traverse_mask(k,0);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnMxname(fieldno,val,stat)
**      Callback function when new matrix name is selected.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnMxname(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Get and store the selected matrix name
*/
	strcpy(stock_cur.mxname,val->frmstr);
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnLoadmx(fieldno,val,stat)
**      Callback function to load a predefined matrix.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnLoadmx(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,nc,status,inc;
	char label[NCL_MAX_LABEL+1],sbuf[NCL_MAX_LABEL+20];
	UM_f77_str ncllabel;
	UU_KEY_ID key;
	struct NCL_fixed_databag e;
	struct NCL_matrix_rec *mx;
/*
.....Verify that the matrix exists
*/
	strcpy(label,stock_cur.mxname);
	nc = strlen(label);
	if (nc == 0) goto failed;
	for (i=0;i<nc;i++) label[i] = islower(label[i]) ? toupper(label[i]) :
		label[i];
	for (i=nc;i<NCL_MAX_LABEL;i++) label[i] = ' ';
	UM_init_f77_str(ncllabel,label,NCL_MAX_LABEL);
	getkey(UM_addr_of_f77_str(ncllabel),&key);
	if (key == 0) goto failed;
/*
.....Get the matrix data and
.....place it into the structure
*/
	e.key = key;
	status = ur_retrieve_data_fixed(&e);
	if (status != UU_SUCCESS) goto failed;
	if (e.rel_num != NCL_MATRIX_REL) goto notmx;
	mx = (struct NCL_matrix_rec *)&e;
	ncl_34mx_to_43mx(mx->mat,stock_cur.matrix);
/*
.....Update the form if displayed
*/
	if (Smxfrm != -1)
	{
		ud_dispfrm_update_answer(Smxfrm,0,(int *)stock_cur.mxname);
		inc = 1;
		for (j=0;j<3;j++)
		{
			for (i=0;i<4;i++)
				ud_dispfrm_update_answer(Smxfrm,inc++,
					(int *)&stock_cur.matrix[i][j]);
		}
	}
	ud_update_form(Smxfrm);
	goto done;
/*
.....Could not load matrix
*/
failed:;
	sprintf(sbuf,"Could not load Matrix '%s'",label);
	ud_wrerr(sbuf);
	goto done;
/*
.....Not a matrix
*/
notmx:;
	sprintf(sbuf,"'%s' is not a Matrix",label);
	ud_wrerr(sbuf);
	goto done;
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnMxclose(fieldno,val,stat)
**      Callback function when matrix form is closed.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnMxclose(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	Smxfrm = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnModmx(fieldno,val,stat)
**      Callback function to display the active stock matrix form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnModmx(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int *ans[13],i,j,inc;
	static char called[] = {6,6,6,6,6,6,6,6,6,6,6,6};
	static UD_METHOD methods[] = {UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		OnMxclose};
/*
......only allow one form
*/
	if (Smxfrm!=-1)
		return;
/*
.....Setup the form field defaults
*/
	ans[0] = (int *)stock_cur.mxname;
	inc = 1;
	for (j=0;j<3;j++)
	{
		for (i=0;i<4;i++) ans[inc++] = (int *)&stock_cur.matrix[i][j];
	}
/*
.....Display the form
*/
	Smxfrm = ud_form_display1("ipvstkmx.frm",ans,ans,methods,called,UU_NULL,
		UU_NULL);
	if (Smxfrm == -1) goto failed;
	goto done;
/*
.....Could not display form
*/
failed:;
	Smxfrm = 0;
	ud_wrerr("Could not create Matrix form.");
	goto done;
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnView(fieldno,val,stat)
**      Callback function to view the stock(s).
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnView(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
/*
.....Get stock selection &
.....View stock
*/
	attr_ptr[stock_sel].color = stock_cur.color;
	for (i=0;i<LW_nstock[stock_which];i++)
	{
		if (allfl || i == stock_sel)
		{
			if (i == stock_sel)
			{
				stock_cur.color = stock_cur.color;
				stock_cur.type = attr_ptr[i].type;
				stock_cur.data = attr_ptr[i].data;
				ul_ipv_view_stock(stock_which,&stock_cur);
				stock_cur.color = stock_cur.color;
			}
			else
			{
				ul_ipv_view_stock(stock_which,&attr_ptr[i]);
			}
		}
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnDelete(fieldno,val,stat)
**      Callback function to delete the stock(s).
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnDelete(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char *ptr1;
	int i,j,ifl,inc;
	UU_LOGICAL iflag;
	LW_stock_struc *sptr,*stock;
	LtData data;
/*
.....Initialize routine
*/
	if (LW_nstock[stock_which] == 0) goto done;
	sptr = LW_stock_first[stock_which];
/*
.....Stop the lathe if it is active
*/
	iflag = UU_FALSE;
	if (LW_active && LW_mach_type == LW_LATHE) iflag = ul_ipv_lathe_stop();
/*
.....Delete all stocks
*/
	if (allfl || LW_nstock[stock_which] == 1)
	{
		if (LW_active)
		{
			if (LW_mach_mode == LW_RAPIDCUT)
			{
				LW_nstock[stock_which] = 0;
				ul_ipv_reset_session(UU_FALSE);
			}
			else
			{
				LiDataSetBoolean(&data,FALSE);
				for (i=0;i<LW_nstock[stock_which];i++)
				{
					ifl = 0;
					do
					{
						inc = ifl;
						ul_ipv_get_next_stock(sptr,&stock,&ifl,UU_FALSE);
						if (ifl == -2) break;
						ul_ipv_remove_prim(stock);
					} while (ifl != -1);
					sptr = (LW_stock_struc *)uu_lsnext(sptr);
				}
			}
		}
		LW_nstock[stock_which] = 0;
/*
		LW_stock_data[stock_which] =
			(LW_stock_struc *)uu_lsempty(LW_stock_first[stock_which]);
		LW_stock_data[stock_which] =
			(LW_stock_struc *)uu_lsinsrt(LW_stock_data[stock_which]);
*/
		LW_stock_first[stock_which] = LW_stock_data[stock_which];
	}
/*
.....Get stock selection &
.....Delete stock
*/
	else
	{
		ptr1 = stk_ptr;
		LiDataSetBoolean(&data,FALSE);
		for (i=0;i<LW_nstock[stock_which];i++)
		{
			if (i == stock_sel)
			{
				if (LW_active)
				{
					ifl = 0;
					do
					{
						inc = ifl;
						ul_ipv_get_next_stock(sptr,&stock,&ifl,UU_FALSE);
						if (ifl == -2) break;
						ul_ipv_remove_prim(stock);
					} while (ifl != -1);
				}
				if (sptr == LW_stock_first[stock_which])
					LW_stock_first[stock_which] = (LW_stock_struc *)uu_lsdele(sptr);
				else if (i == LW_nstock[stock_which]-1)
					LW_stock_data[stock_which] = (LW_stock_struc *)uu_lsdele(sptr);
				else
					sptr = (LW_stock_struc *)uu_lsdele(sptr);
				LW_nstock[stock_which]--;
				if (i != LW_nstock[stock_which] && LW_nstock[stock_which] != 0)
				{
					for (j=i;j<LW_nstock[stock_which];j++)
					{
						uu_move_byte(&attr_ptr[j+1],&attr_ptr[j],sizeof(LW_stock_struc));
						strcpy(ptr1,ptr1+80);
						ptr1 = ptr1 + 80;
					}
				}
				break;
			}
			else
			{
				sptr = (LW_stock_struc *)uu_lsnext(sptr);
				ptr1 = ptr1 + 80;
			}
		}
	}
/*
.....Restart the lathe
*/
	if (iflag) ul_ipv_lathe_start();
/*
.....Update list of all stocks/fixtures
*/
	sptr = LW_stock_first[stock_which];
	ptr1 = stk_ptr;
	for (i=0;i<LW_nstock[stock_which];i++)
	{
		geom_list.item[i] = ptr1;
		ptr1 = ptr1 + 80;
	}
	strcpy(ptr1," ");
	strcpy(stock_cmd,stk_ptr);
	geom_list.num_item = LW_nstock[stock_which];
	geom_list.answer = stock_cmd;
	ud_update_answer(FAGEO,(int *)&geom_list);
/*
.....End of routine
*/
	ul_ipv_delete_sessions();
	ul_ipv_flush();
	um_reset_pocket_graphics(UM_IPV_WINDOW);
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnApply(fieldno,val,stat)
**      Callback function to modify the stock(s).
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnApply(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_REAL det;
/*
.....Make sure it is not a mirror matrix
*/
	if (stock_cur.mxflag)
	{
		um_determinant(stock_cur.matrix,&det);
		if (det <= 0.)
		{
			ud_wrerr("A mirror matrix is not allowed.");
			return(UD_FLDOK);
		}
	}
/*
.....Modify selected stock
*/
	S_modify_stock(UU_FALSE);
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : S_modify_stock(istkfl)
**       Modifies a single or all defined stocks.
**    PARAMETERS   
**       INPUT  : 
**          istkfl   = UU_TRUE = Modify all stocks
**                     UU_FALSE = Modify only selected stock.
**       OUTPUT :  
**          none
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static void S_modify_stock(istkfl)
UU_LOGICAL istkfl;
{
	int i,inc,ifl,incr;
	UU_LOGICAL chg_color,chg_vis,chg_act,chg_trans,chg_imp,chg_tol,chg_edge;
	UU_LOGICAL chg_ecol;
	LW_stock_struc *sptr,*stock;
/*
.....Store Stock Modals
*/
	for (i=0;i<LW_nstock[stock_which];i++)
	{
		if (allfl || i == stock_sel)
		{
			attr_ptr[i].color = stock_cur.color;
			attr_ptr[i].visible = stock_cur.visible;
			attr_ptr[i].active = stock_cur.active;
			attr_ptr[i].translucency = stock_cur.translucency;
			attr_ptr[i].important = stock_cur.important;
			UM_len_exttoint(stock_cur.toler,attr_ptr[i].toler);
			attr_ptr[i].edge = stock_cur.edge;
			attr_ptr[i].edge_color = stock_cur.edge_color;
			attr_ptr[i].mxflag = stock_cur.mxflag;
			strcpy(attr_ptr[i].mxname,Smxlist.answer);
			um_tftotf(stock_cur.matrix,attr_ptr[i].matrix);
			attr_ptr[i].axes = stock_cur.axes;
			attr_ptr[i].axes_color = stock_cur.axes_color;
			Sreset[i] = freset;
			if (!allfl) break;
		}
	}
/*
.....Modify stock(s)
*/
	incr = Sincr;
	sptr = LW_stock_first[stock_which];
	for (i=0;i<LW_nstock[stock_which];i++)
	{
		Sincr = 0;
		if (i == stock_sel) Sincr = incr;
		if (istkfl || allfl || i == stock_sel)
		{
/*
........Determine what attributes have changed
........for a composite stock
*/
			if (sptr->type == LW_STOCK_COMPOS)
			{
				chg_color = chg_vis = chg_act = chg_trans = chg_imp = UU_FALSE;
				chg_tol = chg_edge = chg_ecol = UU_FALSE;
				if (sptr->color != attr_ptr[i].color) chg_color = UU_TRUE;
				if (sptr->visible != attr_ptr[i].visible) chg_vis = UU_TRUE;
				if (sptr->active != attr_ptr[i].active) chg_act = UU_TRUE;
				if (sptr->translucency != attr_ptr[i].translucency)
					chg_trans = UU_TRUE;
				if (sptr->important != attr_ptr[i].important) chg_imp = UU_TRUE;
				if (sptr->toler != attr_ptr[i].toler) chg_tol = UU_TRUE;
				if (sptr->edge != attr_ptr[i].edge) chg_edge = UU_TRUE;
				if (sptr->edge_color != attr_ptr[i].edge_color) chg_ecol = UU_TRUE;
			}
/*
........Store attributes
*/
			sptr->color = attr_ptr[i].color;
			sptr->visible = attr_ptr[i].visible;
			sptr->active = attr_ptr[i].active;
			sptr->translucency = attr_ptr[i].translucency;
			sptr->important = attr_ptr[i].important;
			sptr->toler = attr_ptr[i].toler;
			sptr->edge = attr_ptr[i].edge;
			sptr->edge_color = attr_ptr[i].edge_color;
			sptr->axes = attr_ptr[i].axes;
			sptr->axes_color = attr_ptr[i].axes_color;
			S_store_matrix(sptr,&attr_ptr[i]);
/*
........Composite stock
*/
			if (sptr->type == LW_STOCK_COMPOS)
			{
/*
........Loop through composite stocks
*/
				ifl = 0;
				do
				{
					inc = ifl;
					ul_ipv_get_next_stock(sptr,&stock,&ifl,UU_FALSE);
					if (ifl == -2) break;
/*
...........Set the stock's attributes
*/
					if (chg_color) stock->color = sptr->color;
					if (chg_vis) stock->visible = sptr->visible;
					if (chg_act) stock->active = sptr->active;
					if (chg_trans) stock->translucency = sptr->translucency;
					if (chg_imp) stock->important = sptr->important;
					if (chg_tol) stock->toler = sptr->toler;
					if (chg_edge) stock->edge = sptr->edge;
					strcpy(stock->mxname,sptr->mxname);
					stock->mxchg = sptr->mxchg;
					stock->mxflag = sptr->mxflag;
					um_tftotf(sptr->matrix,stock->matrix);
					ul_ipv_modify_stock(stock,Sreset[i]);
				} while (ifl > 0);
/*
.....Disable axes display
*/
				if (!sptr->axes && sptr->axis_seg != -1)
					ul_delaxis_ipv(sptr);
/*
........Display axes
*/
				else if (sptr->axes)
					sptr->axis_seg = ul_ipv_create_axis(sptr);
			}
/*
........Modify Single stock
*/
			else
				ul_ipv_modify_stock(sptr,Sreset[i]);
		}
/*
........Point to next stock
*/
		sptr = (LW_stock_struc *)uu_lsnext(sptr);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_store_matrix(stock,attr)
**       Stores a matrix in a stock definition.
**    PARAMETERS   
**       INPUT  : 
**          sptr     = Stock definition to store matrix in.
**          attr     = Stock definition that contains the new matrix
**                     definition.
**       OUTPUT :  
**          none
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static void S_store_matrix(sptr,attr)
LW_stock_struc *sptr,*attr;
{
	int i,j,inc;
/*
.....Determine if matrix has changed
*/
	sptr->mxchg = Sincr;
	if (!um_tfeqtf(attr->matrix,sptr->matrix)) sptr->mxchg = UU_TRUE;
	if (attr->mxflag != sptr->mxflag) sptr->mxchg = UU_TRUE;
/*
.....Store incremental matrix
*/
	if (Sincr)
	{
		um_tftmtf(sptr->matrix,attr->matrix,sptr->matrix);
		um_tftotf(sptr->matrix,stock_cur.matrix);
/*
........Uncheck Incremental flag
*/
		Sincr = UU_FALSE;
		ud_update_answer(FAXIN,&Sincr);
/*
........Update matrix form with new matrix
*/
		if (Smxfrm != 0)
		{
			ud_dispfrm_update_answer(Smxfrm,0,(int *)stock_cur.mxname);
			inc = 1;
			for (j=0;j<3;j++)
			{
				for (i=0;i<4;i++)
					ud_dispfrm_update_answer(Smxfrm,inc++,
						(int *)&stock_cur.matrix[i][j]);
			}
			ud_update_form(Smxfrm);
		}
	}
/*
.....Store absolute matrix
*/
	else if (sptr->mxchg)
		um_tftotf(attr->matrix,sptr->matrix);
/*
.....Store remaining matrix attributes
*/
	strcpy(sptr->mxname,attr->mxname);
	sptr->mxflag = attr->mxflag;
}

/*********************************************************************
**    E_FUNCTION     : S_save_modfile(which)
**       Save the NCLIPV stock/fixture settings into modals file.
**    PARAMETERS   
**       INPUT  : 
**          which  = 0 = Save stock modals file, 1 = Fixture.
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_modfile(which)
int which;
{
	int i, stat;
	char msg[80];
	FILE *fptr;
	UX_pathname fname;
	char ecolor[65][96];
	static char yesno[2][10] = {"*NO","*YES"};
	static char fmt[2][10] = {"*ASCII","*BINARY"};

	strcpy(ecolor[0], "*DEFAULT");
	for (i=0; i<64;i++)
	{
		sprintf(ecolor[i+1], "*%s", uw_color_name[i]);
	}
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	if (which == 0) strcpy(fname,"nclipv_stock.mod");
	else strcpy(fname,"nclipv_fixture.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) goto done;
/*
.....Store display modals
*/
	if (which == 0) ux_fputs0("#STOCK#\n", fptr);
	else ux_fputs0("#FIXTURE#\n", fptr);

	sprintf(msg,"/COLOR/ %s\n",uw_color_name[LW_stock_default[which].color]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/VISIBLE/ %s\n",yesno[LW_stock_default[which].visible]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/ACTIVE/ %s\n",yesno[LW_stock_default[which].visible]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/TRANSLUCENCY/ %d\n",LW_stock_default[which].translucency);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/IMPORTANT/ %s\n",yesno[LW_stock_default[which].important]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/TOLERANCE/ %g\n",LW_stock_default[which].toler);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/EDGE_DISPLAY/ %s\n",yesno[LW_stock_default[which].edge]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/EDGE_COLOR/ %s\n",ecolor[LW_stock_default[which].edge_color+1]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/ATTR_RESET/ %s\n",yesno[LW_reset_attr[which]]);
	ux_fputs0(msg, fptr);

	if (which == 0)
	{
		sprintf(msg,"/SESS_DELETE/ %s\n",yesno[LW_delete_stocks]);
		ux_fputs0(msg, fptr);
	}

	sprintf(msg,"/AXES_COLOR/ %s\n",uw_color_name[LW_stock_default[which].axes_color]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/AXES/ %s\n",yesno[LW_stock_default[which].axes]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/AXES_COLOR/ %s\n",uw_color_name[LW_stock_default[which].axes_color]);
	ux_fputs0(msg,fptr);

	if (which == 0)
	{
		sprintf(msg,"/STL_FORMAT/ %s\n",fmt[LW_stl_format]);
		ux_fputs0(msg,fptr);

		sprintf(msg,"/STL_STOP/ %s\n",yesno[LW_stl_flag[0]]);
		ux_fputs0(msg, fptr);

		sprintf(msg,"/STL_INACTIVE/ %s\n",yesno[LW_stl_flag[1]]);
		ux_fputs0(msg, fptr);

		sprintf(msg,"/STL_SKIP_ERROR/ %s\n",yesno[LW_stl_flag[2]]);
		ux_fputs0(msg, fptr);
	}
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

