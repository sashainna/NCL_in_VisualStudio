/*********************************************************************
**    NAME         :  m2ulayer.c
**       CONTAINS:
**   		 	umu_sea_ent_layer_num()
**    		umu_sea_ent_layer_name()
**		    	umu_sda1_layer_num()
**          umu_load_layers()
**		    	umu_layer_select()
**		    	umu_layer_manage()
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2ulayer.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 10:25:56
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "usysg.h"
#include "uhep.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msol.h"
#include "mdpick.h"
#include "mdebug.h"
#include "modef.h"
#include "mattr.h"
#include "mdattr.h"
#include "mxxx.h"
#include "nccs.h"
#include "nclfc.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"

#define FALAY 0
#define FANAM 1
#define FAMOD 2
#define FAVIS 3
#define FAVAL 4
#define FAVSW 5
#define FAPIK 6
#define FAPAL 7
#define FAPSW 8
#define FAACT 9
#define FAFND 10
#define FARST 11
#define FAGEO 12
#define FAMOV 13
#define FADEL 14
#define FAAPL 15

UU_LOGICAL	ud_gnxt();

static int S_load_layers();
static int S_load_list();
static void S_layer_command();
static int S_store_layers();
static UD_FSTAT S_add_layer();
static UD_FSTAT OnLayer(),OnAll(),OnSwap(),OnFind(),OnReset();
static UD_FSTAT OnMove(),OnDelete(),OnSelect(),OnAdd(),OnApply();
static UD_FSTAT OnSelect1(),OnCancel(),OnClose();

static int Sselact = UU_FALSE;
static int Sdelact = UU_FALSE;
static int Ssel,Ssel1,Snlayer,Snlay1,Smax,Sactive,Scur_active;
static int Scancel,Sfrm=-1;
static char Scmd[80];
static char *Slayer_ptr;
static char *Slayptr1;
static struct UM_layer_rec Scur;
static UU_LIST Slayer;
static UD_LIST Sgeom_list;

/*********************************************************************
**    E_FUNCTION     : umu_sea_ent_layer_num()
**       Change the layer number of an entity.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_sea_ent_layer_num()
{
	struct UC_entitydatabag e;						/* picked entity */
	int numint;
	UU_LOGICAL initialize;							/* initialize get next entity */
	int status;											/* -1 iff error, 0 otherwise */
	int number;											/* new layer number */
	int cont;											/* continue */

	uu_denter(UU_MTRC,(us,"umu_sea_ent_layer_num()"));
	cont = 1;
	while (cont)
	{
		ud_ldas(UD_DASINT,/*layer number*/UM_MODEL,234,&number,1,
			&numint,UD_NODEFAULT);
		if (numint != 0)
		{
			if (number < 0 || number > 9999)
			{
				uu_uerror0(/* layer number must be in the range 1 - 9999 */
								UM_MODEL,205);
			}
			else
			{
				um_set_layer_num(number);
				ud_ldas(UD_DASSELECT,/*pick entity*/UM_MODEL,223, UU_NULL,
							1,&numint,UD_NODEFAULT);
				initialize = UU_TRUE;
				while(ud_gnxt(initialize, UU_NULL, &e.key, 1) == UU_TRUE)
				{
					initialize = UU_FALSE;
					status = um_update_layer(e.key,number);
					if (status == 0)
					{
						uc_retrieve_data(&e, sizeof(e));
						uc_display(&e);
					}
				}
				break;
			}
		}
		else
		break;
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : umu_sea_ent_layer_name()
**       Change the layer name of an entity.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_sea_ent_layer_name()
{
	struct UC_entitydatabag e;						/* picked entity */
	int numint;
	UU_LOGICAL initialize;							/* initialize get next entity */
	int status;											/* -1 iff error, 0 otherwise */
	char name[16];										/* layer name */
	int found;											/* found match */
	struct UM_layer_rec layer;
	int entnum;										/* next entity number for unibase */
	int ur_get_next_tuple_index();

	uu_denter(UU_MTRC,(us,"umu_sea_ent_layer_name()"));
	
	found = -1;

	ud_ldas(UD_DASSTRING,/* layer name */UM_MODEL,235,
				name,16,&numint,UD_NODEFAULT);
	if (numint != 0)
	{
		status = 0;
		entnum = 0;
		layer.rel_num = UM_LAYER_REL;

		while(status == 0)
		{
			entnum++;
			status = ur_get_next_tuple_index(layer.rel_num, &entnum);
			if (status == 0) 
			{
				ur_retrieve_tuple(layer.rel_num, entnum, &layer);
				found = strcmp(name, layer.name);
				if (found == 0) break;
			}
		}

		if (found == 0)
		{
			ud_ldas(UD_DASSELECT,/*pick entity*/UM_MODEL,223, UU_NULL,
						1,&numint,UD_NODEFAULT);
			initialize = UU_TRUE;
			while(ud_gnxt(initialize, UU_NULL, &e.key, 1) == UU_TRUE)
			{
				initialize = UU_FALSE;
				status = um_update_layer(e.key,layer.num);
				if (status == 0)
				{
					uc_retrieve_data(&e, sizeof(e));
					uc_display(&e);
				}
			}
		}
		else uu_uerror0(/* there is no layer found by that name */UM_MODEL, 210);
	}
	uu_dexit;
}
/*********************************************************************
**    E_FUNCTION     : umu_sda1_layer_num()
**       Set the default layer number attribute .
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_sda1_layer_num()
{
	int num;								/* layer number */
	int cont;							/* continue until the user gets it right, or
												(s)he wants to quit trying */
	int numint;							/* number of entities returned from DAS */

	uu_denter(UU_MTRC,(us,"umu_sda1_layer_num()"));
	cont = 1;
	while (cont)
	{
		num = ur_get_attrmdl_layer();
		ud_ldas(UD_DASINT,/*layer number*/UM_MODEL,234,&num,1,
			&numint,UD_DEFAULT);
		if (numint != 0)
		{
			if (num < 0 || num > 9999)
			{
				uu_uerror0(/* layer number must be in the range 0 - 9999 */
								UM_MODEL,205);
				cont = 1;
			}
			else
			{
				um_set_layer_num(num);
				um_set_active_layer(num);
				cont = 0;
			}
		}
		else
			cont = 0;
	}
	uu_dexit;
}

/*********************************************************************
**   I_FUNCTION: umu_load_layers(layer,nlay,activ)
**      Load all layer definitions into a list.  The list will be
**      created by this routine.
**   PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          laylst = List array of defined layers.
**          nlay   = Number of layers in list.
**          activ  = Pointer into array of active layer.
**   RETURNS: UU_SUCCESS if all goes well, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int umu_load_layers(laylst,nlay,activ)
UU_LIST *laylst;
int *nlay;
int *activ;
{
	int status,entnum,lstnum,i;
	struct UM_layer_rec layer,*sptr;
/*
.....Create a new layer list
*/
	uu_list_init(laylst,sizeof(struct UM_layer_rec),100,100);
	if (laylst->data == UU_NULL) return(UU_FAILURE);
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(laylst);
	*nlay = 0;
	entnum = 0;
	lstnum = 0;
	*activ = ur_get_attrmdl_layer();
/*
.....Get all of the layers and
.....place them into the list
*/
	layer.rel_num = UM_LAYER_REL;
	status = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_tuple_index(layer.rel_num,&entnum);
		if (status == 0)
		{
			ur_retrieve_tuple(layer.rel_num,entnum,&layer);
			if (layer.num < lstnum)
			{
				for (i=0;i<*nlay;i++)
				{
					if (layer.num < sptr[i].num)
					{
						uu_list_insert(laylst,i,&layer);
						sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(laylst);
						break;
					}
				}
			}
			else
			{
				uu_list_push(laylst,&layer);
				sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(laylst);
			}
			if (layer.num > lstnum) lstnum = layer.num;
			*nlay = *nlay + 1;
		}
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**   E_FUNCTION: umu_layer_select()
**      This function controls the Layer Selection form.  It displays
**      a list of layers and returns the user selection (position of
**      list item selected).
**   PARAMETERS
**       INPUT  : sptr   = Array of Layer structures to select from.
**                nlay   = Number of layers in list.
**                flag   = 0 = Select input layer,
**                         1 = Select layer to move entities to.
**       OUTPUT : layer  = Position in list of item selected.
**   RETURNS: UU_FAILURE if the user did not select any item.
**   SIDE EFFECTS: none 
**   WARNINGS: none
*********************************************************************/
int umu_layer_select(sptr,nlay,flag,layer)
struct UM_layer_rec *sptr;
int nlay,flag,*layer;
{
	int i;
/*	short si=0,sj=0;*/
	UD_LIST geom;
	char *ptr1,sbuf[200];

	int *ans[2];
	static UD_METHOD methods[] = {OnSelect1,OnCancel,OnClose};
	static char called[]       = {6,6};
	static char traverse[]     = {1,1};
	static char disp[]         = {1,0, 1,1};
/*
.....Allocate memory for form list
*/
	ptr1 = UU_NULL;
	Slayptr1 = UU_NULL;
	geom.item = UU_NULL;
	ptr1 = (char *)uu_malloc(sizeof(char)*80*nlay);
	if (ptr1 == UU_NULL) goto nomem;
	geom.item = (char **)uu_malloc(nlay * sizeof(char *));
	if (geom.item == UU_NULL) goto nomem;
/*
.....Store layer list in form list
*/
	Slayptr1 = ptr1;
	Snlay1 = nlay;
	for (i=0;i<nlay;i++)
	{
		S_layer_command(&sptr[i],ptr1);
		geom.item[i] = ptr1;
		if (i == 0) strcpy(sbuf,ptr1);
		ptr1 = ptr1 + 80;
	}
	geom.num_item = nlay;
	geom.answer = sbuf;
/*
.....Setup visible fields
*/
	disp[flag] = 1;
	disp[1-flag] = 0;
/*
.....Setup the default answers
*/
	ans[0] = (int *)&geom;
	ans[1] = UU_NULL;
	Ssel1 = 0;
/*
.....Display the form
*/
	Sfrm = ud_form_display1("layersel.frm", ans, ans, methods, called, disp,
		traverse);
	if (Sfrm == -1) goto nofrm;
/*
.....Wait for the user to select a layer
.....or cancel the form
*/
	Scancel = UU_SUCCESS;
/*	do
	{
		ckintr(&si,&sj);
	} while (Sfrm);
*/
	uw_dispfrm_wait(Sfrm);

/*
.....Return the selection
*/
	*layer = Ssel1;
	goto done;
/*
.....Could not allocate memory
*/
nomem:
	ud_wrerr("Could not allocate memory for form.");
	Scancel = UU_FAILURE;
	goto done;
/*
.....Could not allocate memory
*/
nofrm:
	ud_wrerr("Could not load 'layersel.frm'.");
	Scancel = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:
	if (ptr1 != UU_NULL) uu_free(Slayptr1);
	if (geom.item != UU_NULL) uu_free(geom.item);
	return(Scancel);
}

/*********************************************************************
**   I_FUNCTION: OnSelect1(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Layer form.
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
	int i;
	char *ptr1;
/*
.....Find the current selection
*/
	ptr1 = Slayptr1;
	for (i=0;i<Snlay1;i++)
	{
		if (strcmp(ptr1,val->frmstr) == 0)
		{
				Ssel1 = i;
				break;
		}
		ptr1 = ptr1 + 80;
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnCancel(fieldno,val,stat)
**      Callback function for the Cancel button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnCancel(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....User rejected the form
*/
	Scancel = UU_FAILURE;
	ud_close_dispfrm(Sfrm);
/*
.....form already close, no need to update the form
.....have to set this, if not, the form will try to 
.....update the form field and will cause segmentation error
.....because the form have already gone. Yurong
*/
	*fieldno = -1;
	Sfrm = -1;
	return(UD_FRMCLOSE);
}

/*********************************************************************
**   I_FUNCTION: OnClose()
**      Callback function for the Close button.
**   PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnClose()
{
/*
.....Mark the form as closed
*/
	Sfrm = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**   E_FUNCTION: umu_layer_manage()
**      This function controls the Layer Management form.
**   PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void umu_layer_manage()
{
	int status;
	UU_LOGICAL fflag = UU_FALSE,cmdreject,chg;
	UD_FSTAT fstat;

	int *ans[20];
	static UD_METHOD methods[] =
	{
		OnLayer, UU_NULL, OnAdd, UU_NULL, OnAll, OnSwap, UU_NULL, OnAll, 
		OnSwap, UU_NULL, OnFind, OnReset, OnSelect, OnMove, OnDelete, OnApply
	};
	static char called[]       = {6,6,6, 6,6,6, 6,6,6, 6,6,6, 6, 6,6,6};
	static char traverse[]     = {1,1,1, 1,1,1, 1,1,1, 1,1,1, 1, 1,1,1};
	static char disp[]         = {1,1,1, 1,1,1, 1,1,1, 1,1,1, 1, 1,1,1};
/*
.....Initialize routine
*/
	Sgeom_list.item = UU_NULL;
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
	status = S_load_list(UU_TRUE);
	if (status != UU_SUCCESS) goto nomem;
/*
.....Load the stock answers
*/
	ans[FALAY] = (int *)&(Scur.num);
	ans[FANAM] = (int *)&(Scur.name);
	ans[FAMOD] = UU_NULL;

	ans[FAVIS] = (int *)&(Scur.displayable);
	ans[FAVAL] = UU_NULL;
	ans[FAVSW] = UU_NULL;
	ans[FAPIK] = (int *)&(Scur.selectable);
	ans[FAPAL] = UU_NULL;
	ans[FAPSW] = UU_NULL;
	Sactive = 0;
	if (Scur_active == Scur.num) Sactive = 1;
	ans[FAACT] = (int *)&Sactive;
	ans[FAFND] = UU_NULL;
	ans[FARST] = UU_NULL;

	ans[FAGEO] = (int *)&Sgeom_list;

	ans[FAMOV] = UU_NULL;
	ans[FADEL] = UU_NULL;
	ans[FAAPL] = UU_NULL;
/*
.....Get the Form input
*/
	UD_MARK(cmdreject, UU_FALSE);
	fflag = UU_TRUE;
	if (!cmdreject)
	{
		do
		{
			status = ud_form1("layer.frm",ans,ans,methods,called,disp,traverse);
			if (status == -1) goto done;
			fstat = S_add_layer(&chg);
		} while (fstat != UD_FLDOK);
	}	
	else
		goto done;
/*
.....Store Layer Attributes
*/
	S_store_layers();
	goto done;
/*
.....Could not allocate memory
*/
nomem:;
	ud_wrerr("Could not allocate memory for form.");
	goto done;
/*
.....End of routine
*/
done:
	if (Slayer.data != UU_NULL) uu_list_free(&Slayer);
	if (Sgeom_list.item != UU_NULL) uu_free(Sgeom_list.item);
/*	if (Smxfrm != UU_NULL) ud_close_dispfrm(Smxfrm);*/
	if (fflag)
	{
		UD_UNMARK(cmdreject);
	}
	return;
}

/*********************************************************************
**   I_FUNCTION: S_load_layers()
**      Load all layer definitions into a local list.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static int S_load_layers()
{
	int status,i;
	struct UM_layer_rec *sptr;
/*
.....Get all of the layers and
.....place them into the list
*/
	status = umu_load_layers(&Slayer,&Snlayer,&Scur_active);
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
**   I_FUNCTION: S_load_list(flag)
**      Load all layer definitions into the form list.
**   PARAMETERS
**       INPUT  : flag   = UU_TRUE if list should be initialized.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static int S_load_list(flag)
UU_LOGICAL flag;
{
	int i;
	struct UM_layer_rec *sptr;
	char *ptr1;
	UU_LOGICAL init;
/*
.....Initialize routine
*/
	init = flag;
	if (Snlayer > Smax) init = UU_TRUE;
/*
.....Allow for 20 more layers that currently defined
*/
	if (init)
	{
		Smax = Snlayer + 20;
/*
.....Deallocate memory from last list
*/
		if (!flag)
		{
			uu_free(Slayer_ptr);
			uu_free(Sgeom_list.item);
		}
/*
.....Allocate memory for form list
*/
		if (flag)
		{
			Slayer_ptr = (char *)uu_malloc(sizeof(char)*80*Smax);
			if (Slayer_ptr == UU_NULL) return(UU_FAILURE);
			Sgeom_list.item = (char **)uu_malloc(Smax * sizeof(char *));
			if (Sgeom_list.item == UU_NULL) return(UU_FAILURE);
		}
	}
/*
.....Store layer list in form list
*/
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
	ptr1 = Slayer_ptr;
	for (i=0;i<Snlayer;i++)
	{
		S_layer_command(&sptr[i],ptr1);
		Sgeom_list.item[i] = ptr1;
		ptr1 = ptr1 + 80;
		if (i == Ssel) uu_move_byte(&sptr[i],&Scur,sizeof(struct UM_layer_rec));
	}
	strcpy(Scmd,Sgeom_list.item[Ssel]);
	Sgeom_list.num_item = Snlayer;
	Sgeom_list.answer = Scmd;
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**   I_FUNCTION: S_add_layer(chg)
**      Adds the current layer to the list or modifies it if it already
**      exists.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : chg   = UU_TRUE if the layer fields are different than
**                the layer list entry.
**   RETURNS: UD_FLDOK if all goes well, UD_BADREQ otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT S_add_layer(chg)
UU_LOGICAL *chg;
{
	int i,imod;
/*	int nc;*/
	char sbuf[80];
	struct UM_layer_rec *sptr;
/*
.....Initialize routine
*/
	*chg = UU_TRUE;
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
	S_layer_command(&Scur,sbuf);
/*
.....Make sure a name was specified
*/
/*
	nc = strlen(Scur.name);
	if (nc == 0)
	{
		ud_wrerr("A layer description must be specified.");
		return(UD_BADREQ);
	}
*/
/*
.....Store attributes for last selected layer
*/
	if (Scur.num == sptr[Ssel].num)
	{
		if (strcmp(sbuf,Sgeom_list.item[Ssel]) == 0) *chg = UU_FALSE;
		sptr[Ssel].displayable = Scur.displayable;
		sptr[Ssel].selectable = Scur.selectable;
		strcpy(sptr[Ssel].name,Scur.name);
		sptr[Ssel].no_layers = 0;
		strcpy(Sgeom_list.item[Ssel],sbuf);
		if (Sactive) Scur_active = Scur.num;
	}
/*
........Number is different
........See if this layer already exists
*/
	else
	{
		imod = UU_FALSE;
		for (i=0;i<Snlayer;i++)
		{
			if (Scur.num == sptr[i].num)
			{
				if (ud_yesno(0,
					"This layer exists.  Do you want to redefine it?",
					"Layer Exists"))
				{
					Ssel = i;
					imod = UU_TRUE;
					sptr[Ssel].displayable = Scur.displayable;
					sptr[Ssel].selectable = Scur.selectable;
					strcpy(sptr[Ssel].name,Scur.name);
					strcpy(Sgeom_list.item[Ssel],sbuf);
				}
				else
					return(UD_BADREQ);
			}
			else if (Scur.num < sptr[i].num) break;
		}
/*
.......Add a new layer
*/
		if (Sactive) Scur_active = Scur.num;
		if (!imod)
		{
			uu_list_insert(&Slayer,i,&Scur);
			Ssel = i;
			Snlayer++;
		}
	}
/*
.....Update form list
*/
	S_load_list(UU_FALSE);
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: S_load_select(isel)
**      Loads the selected layer into the current layer fields.  This
**      routine will not change the list selection.
**   PARAMETERS
**       INPUT  : isel  = Selected layer.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static void S_load_select(isel)
int isel;
{
	struct UM_layer_rec *sptr;
	char *ptr1;
/*
.....Store the default answer
*/
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
	ptr1 = Slayer_ptr + isel * 80;
	strcpy(Scmd,ptr1);
/*
.....Store the current layer settings
*/
	uu_move_byte(&sptr[Ssel],&Scur,sizeof(struct UM_layer_rec));
/*
.....Update the main form
*/
	ud_update_answer(FALAY,(int *)&(Scur.num));
	ud_update_answer(FANAM,(int *)&(Scur.name));
	ud_update_answer(FAVIS,(int *)&(Scur.displayable));
	ud_update_answer(FAPIK,(int *)&(Scur.selectable));
	Sactive = 0;
	if (Scur.num == Scur_active) Sactive = 1;
	ud_update_answer(FAACT,(int *)&Sactive);
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
	sprintf(sbuf,"%4d   %s",sptr->num,sptr->name);
	sbuf[80] = '\0';
	strcpy(obuf,sbuf);
}

/*********************************************************************
**   I_FUNCTION: S_store_layers()
**      Stores all changes to the layers.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static int S_store_layers()
{
	int status,entnum,i,oldlay,relnum;
	UU_LOGICAL init;
	UU_KEY_ID key;
	struct UM_layer_rec layer,*sptr;
/*
.....Initialize routine
*/
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
/*
.....Loop through the stored layers and
.....modify them
*/
	layer.rel_num = UM_LAYER_REL;
	status = 0;
	entnum = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_tuple_index(layer.rel_num,&entnum);
		if (status == 0)
		{
			ur_retrieve_tuple(layer.rel_num,entnum,&layer);
			for (i=0;i<Snlayer;i++)
			{
				if (layer.num == sptr[i].num)
				{
					ur_update_tuple(layer.rel_num,entnum,&sptr[i]);
					sptr[i].num = sptr[i].num + 100000;
					break;
				}
			}
		}
	}
/*
.....Add remaining layers
*/
	for (i=0;i<Snlayer;i++)
	{
		if (sptr[i].num < 100000)
			ur_create_tuple(layer.rel_num,&entnum,&sptr[i]);
		else
			sptr[i].num = sptr[i].num - 100000;
	}
/*
.....Modify the entities on the layers
*/
	init = UU_TRUE;
	while (um_getallobjs(init,&key) == 0)
	{
		init = UU_FALSE;
		um_retrieve_data_relnum(key,&relnum);
		switch (relnum)
		{
		case UM_COORDSYS_REL:
			break;
		default:
			ur_retrieve_layer(key,&oldlay);
			um_update_layer(key,oldlay);
		}
	}
/*
.....Set active layer
*/
	um_set_active_layer(Scur_active);
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**   I_FUNCTION: OnLayer(fieldno,val,stat)
**      Callback function for the layer number field.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnLayer(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (Sdelact) return(UD_FLDOK);
	return(UD_FLDOK);
}


/*********************************************************************
**   I_FUNCTION: OnAdd(fieldno,val,stat)
**      Callback function for the Add button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnAdd(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL chg;
	UD_FSTAT status;
/*
.....Add the current layer to the list
*/
	if (Sdelact) return(UD_FLDOK);
	status = S_add_layer(&chg);
	if (status != UD_FLDOK) return(status);
/*
.....Update the form
*/
	S_layer_command(&Scur,Scmd);
	if (chg)
	{
		Sselact = UU_TRUE;
		ud_update_answer(FAGEO,(int *)&Sgeom_list);
		Sselact = UU_FALSE;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnAll(fieldno,val,stat)
**      Callback function for the Visible and Pickable All buttons.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnAll(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	struct UM_layer_rec *sptr;
/*
.....Set all layers to have the selected attribute
*/
	if (Sdelact) return(UD_FLDOK);
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
	for (i=0;i<Snlayer;i++)
	{
		if (*fieldno == FAVAL) sptr[i].displayable = Scur.displayable;
		else sptr[i].selectable = Scur.selectable;
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnSwap(fieldno,val,stat)
**      Callback function for the Visible and Pickable Swap buttons.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSwap(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	struct UM_layer_rec *sptr;
/*
.....Set all layers to have the selected attribute
*/
	if (Sdelact) return(UD_FLDOK);
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
	for (i=0;i<Snlayer;i++)
	{
		if (*fieldno == FAVSW) sptr[i].displayable =  1 - sptr[i].displayable;
		else sptr[i].selectable = 1 - sptr[i].selectable;
	}
/*
.....Update the attribute fields
*/
	if (*fieldno == FAVSW)
	{
		Scur.displayable =  sptr[Ssel].displayable;
		ud_update_answer(FAVIS,(int *)&(Scur.displayable));
	}
	else
	{
		Scur.selectable = sptr[Ssel].selectable;
		ud_update_answer(FAPIK,(int *)&(Scur.selectable));
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnFind(fieldno,val,stat)
**      Callback function for the Find button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnFind(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	UU_LOGICAL chg;
	struct UM_layer_rec *sptr;
	UD_FSTAT status;
/*
.....Add the current layer back to the list
*/
	if (Sdelact) return(UD_FLDOK);
	if (Sactive == 1) return(UD_FLDOK);
	status = S_add_layer(&chg);
	if (status != UD_FLDOK) return(status);
/*
.....Find the active layer
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
.....Store the current layer settings
*/
	S_load_select(Ssel);
/*
.....Update the form
*/
	Sselact = UU_TRUE;
	S_layer_command(&Scur,Scmd);
	ud_update_answer(FAGEO,(int *)&Sgeom_list);
	Sselact = UU_FALSE;
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnReset(fieldno,val,stat)
**      Callback function for the Reset button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnReset(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Store the current layer settings
*/
	if (Sdelact) return(UD_FLDOK);
	S_load_select(Ssel);
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnMove(fieldno,val,stat)
**      Callback function for the Move button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnMove(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int entnum,numint,init,istat;
	UU_KEY_ID key;
	UU_LOGICAL cmdreject,chg;
	struct UM_layer_rec layer,*sptr;
	UD_FSTAT status;
/*
.....Add the current layer back to the list
*/
	if (Sdelact) return(UD_FLDOK);
	status = S_add_layer(&chg);
	if (status != UD_FLDOK) return(status);
/*
.....Add this layer to the Unibase
*/
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
	layer.rel_num = UM_LAYER_REL;
	istat = 0;
	entnum = 0;
	while (istat == 0)
	{
		entnum++;
		istat = ur_get_next_tuple_index(layer.rel_num,&entnum);
		if (istat == 0)
		{
			ur_retrieve_tuple(layer.rel_num,entnum,&layer);
			if (layer.num == Scur.num)
			{
				ur_update_tuple(layer.rel_num,entnum,&layer);
				break;
			}
		}
	}
/*
.....Layer did not exist in Unibase
.....Add it
*/
	if (istat != 0) ur_create_tuple(Scur.rel_num,&entnum,&Scur);
/*
.....Update the form
*/
	S_layer_command(&Scur,Scmd);
	if (chg)
	{
		Sselact = UU_TRUE;
		ud_update_answer(FAGEO,(int *)&Sgeom_list);
		Sselact = UU_FALSE;
	}
/*
.....Take down the form
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Set the appropriate selection mask
*/
	ud_leditable(UU_TRUE);
/*
.....Get the geometry selection(s)
*/
	ud_ldas(UD_DASSELECT,UM_MODEL,223,UU_NULL,1,&numint,UD_NODEFAULT);
/*
.....Loop through selections
*/
	init = UU_TRUE;
	while (ud_gnxt(init,UU_NULL,&key,1))
	{
		init = UU_FALSE;
		um_update_layer(key,Scur.num);
	}
/*
.....End of routine
.....Redisplay the form
*/
done:;
	ud_form_vis();
	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnDelete(fieldno,val,stat)
**      Callback function for the Delete button.
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
	int i,inc,status,layinc,oldlay,entnum;
	UU_LOGICAL init,found;
	UU_KEY_ID key;
	struct UM_layer_rec *sptr,layer;
/*
.....Initialize routine
*/
	if (Sdelact) return(UD_FLDOK);
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
/*
.....You cannot delete all layers
*/
	if (Snlayer == 1)
	{
		ud_wrerr("You cannot delete the only layer.");
		goto done;
	}
/*
.....Make sure this layer exists
*/
	layer.rel_num = UM_LAYER_REL;
	status = 0;
	entnum = 0;
	found = UU_FALSE;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_tuple_index(layer.rel_num,&entnum);
		if (status == 0)
		{
			ur_retrieve_tuple(layer.rel_num,entnum,&layer);
			if (layer.num == Scur.num)
			{
				found = UU_TRUE;
				break;
			}
		}
	}
/*
	if (!found)
	{
		sprintf(sbuf,"Layer %d does not exist.",Scur.num);
		ud_wrerr(sbuf);
		goto done;
	}
*/
/*
.....Get the layer to move the entities to
*/
	if (found)
	{
		Sdelact = UU_TRUE;
		status = umu_layer_select(sptr,Snlayer,1,&inc);
		Sdelact = UU_FALSE;
/*
.....You cannot move entities to the same layer
*/
		layinc = sptr[inc].num;
		if (layinc == Scur.num)
		{
			ud_wrerr("You cannot move entities to the same layer.");
			goto done;
		}
	}
/*
.....Move the entities onto the new layer
*/
	if (status == UU_SUCCESS)
	{
		if (found)
		{
			init = UU_TRUE;
			while (um_getallobjs(init,&key) == 0)
			{
				init = UU_FALSE;
				ur_retrieve_layer(key,&oldlay);
				if (oldlay == Scur.num) um_update_layer(key,layinc);
			}
/*
.....Delete the current layer
*/
			ur_delete_tuple(layer.rel_num,entnum);
		}
/*
.....Delete the layer from the list
*/
		for (i=0;i<Snlayer;i++)
		{
			if (sptr[i].num == Scur.num)
			{
				uu_list_delete(&Slayer,i,1);
				Snlayer--;
				if (Ssel >= Snlayer) Ssel = Snlayer - 1;
				break;
			}
		}
/*
.....If this was the active layer then
.....make the new layer active
*/
		if (Scur_active == Scur.num) Scur_active = layinc;
/*
.....Update the form list
*/
		S_load_select(Ssel);
		S_load_list(UU_FALSE);
		Sselact = UU_TRUE;
		S_layer_command(&Scur,Scmd);
		ud_update_answer(FAGEO,(int *)&Sgeom_list);
		Sselact = UU_FALSE;
	}
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnApply(fieldno,val,stat)
**      Callback function for the Apply button.
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
	UU_LOGICAL chg;
	UD_FSTAT status;
/*
.....Add the current layer back to the list
*/
	if (Sdelact) return(UD_FLDOK);
	status = S_add_layer(&chg);
	if (status != UD_FLDOK) return(status);
/*
.....Store Layer Attributes
*/
	S_store_layers();
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnSelect(fieldno,val,stat)
**      Callback function for the Move button.
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
	int i;
	char *ptr1;
	UU_LOGICAL chg;
	struct UM_layer_rec *sptr;
	UD_FSTAT status;
/*
.....Initialize routine
*/
	if (Sselact) return(UD_FLDOK);
	if (Sdelact) return(UD_FLDOK);
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&Slayer);
/*
.....Add the current layer back to the list
*/
	status = S_add_layer(&chg);
	if (status != UD_FLDOK) return(status);
/*
.....Find the current selection
*/
	ptr1 = Slayer_ptr;
	for (i=0;i<Snlayer;i++)
	{
		if (strcmp(ptr1,val->frmstr) == 0)
		{
				Ssel = i;
				break;
		}
		ptr1 = ptr1 + 80;
	}
/*
.....Store the current layer settings
*/
	S_load_select(Ssel);
	S_layer_command(&Scur,Scmd);
	if (chg)
	{
		Sselact = UU_TRUE;
		ud_update_answer(FAGEO,(int *)&Sgeom_list);
		Sselact = UU_FALSE;
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

