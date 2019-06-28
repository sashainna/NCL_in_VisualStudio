/*********************************************************************
**    NAME         :  m2ilayer.c
**       CONTAINS:
**					um_get_layer_name(number, name)
**					um_set_layer_num(number)
**					um_set_layer_name(number, name)
**					um_layerinit
**					um_set_active_layer
**					um_update_layer
**					um_copy_layer_struct
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       m2ilayer.c , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:07:47
*********************************************************************/
#include "usysdef.h"
#include "ustdio.h"
#include "uims.h"
#include "class.h"
#include "dwindow.h"
#include "dasnog.h"
#include "udebug.h"
#include "uhep.h"
#include "mdattr.h"
#include "mattr.h"
#include "mdrel.h"
#include "mxxx.h"
#include "mdgenent.h"
#include "mdebug.h"
#include "gcolors.h"         /* color definitions - ud_crwin */

extern int UR_active;

#define UM_NULL_NAME "               "

/*********************************************************************
**    E_FUNCTION     : um_get_layer_name(number, name)
**			Get the layer name given the number.
**    PARAMETERS   
**       INPUT  : 
**				number				number of desired layer
**       OUTPUT :  
**          name					name of layer with number number
**    RETURNS      : 0 iff layer was found, -1 iff no layer was found.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_get_layer_name(number, name)
	int number;							/* number of desired layer */
	char name[16];						/* corresponding name */

	{
	struct UM_layer_rec layer;
	int entnum;						/* next entity number from unibase */
	int status;						/* status return from unibase */
	int found;						/* match was found */

	uu_denter(UU_MTRC,(us,"um_get_layer_name(number = %d)",number));

	entnum = 0;
	status = 0;
	found = -1;

	layer.rel_num = UM_LAYER_REL;
	while (status == 0)
		{

		entnum++;
		status = ur_get_next_tuple_index(layer.rel_num, &entnum);
		if (status == 0) 
			{
			ur_retrieve_tuple(layer.rel_num, entnum, &layer);
			if (number == layer.num)
				{
				found = 0;
				strcpy(name, layer.name);
				break;
				}
			}
		}
	uu_dexit;
	return(found);
	}

/*********************************************************************
**    E_FUNCTION     : um_set_layer_num(num)
**       Set the layer number attribute .
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : status		0 if success , -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_set_layer_num(num)
	int num;									/* layer number */	

	{
	int entnum;							/* next tuple index */
	int status;							/* status returned from UNIBASE */
	struct UM_layer_rec oldlayer;
	struct UM_layer_rec newlayer;
/*	char tempstr[20];*/
/*	UU_KEY_ID layer;*/

	uu_denter(UU_MTRC,(us,"um_set_layer_num(%d)", num));
	status = 0;
	entnum = 0;
	oldlayer.rel_num = UM_LAYER_REL;

	while (status == 0)
		{
		entnum++;
		status = ur_get_next_tuple_index(oldlayer.rel_num, &entnum);
		if (status == 0)
			{
			ur_retrieve_tuple(oldlayer.rel_num, entnum, &oldlayer);
			if (oldlayer.num == num)
				{
				break;	
				}
			}
		}
	if (status == -1)
		{
		newlayer.rel_num = UM_LAYER_REL;
		newlayer.num = num;
		newlayer.key = 0;
		newlayer.displayable = 1;
		newlayer.selectable = 1;
		newlayer.no_layers = 0;
		strcpy(newlayer.name, UM_NULL_NAME);
		ur_create_tuple(newlayer.rel_num, &entnum, &newlayer);
/*
......don't add layer view when add layer, it only added view when
......user select to display view (function load view, view management)
*/
/*		sprintf(tempstr, "Layer%d", num);
		uv_special_view(&layer, tempstr, 2);
*/
	}

	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : um_set_layer_name(num, name)
**       Set the layer name .
**    PARAMETERS   
**       INPUT  : 
**				name				layer name
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_set_layer_name(num, name)
	int num;											/* layer number */
	char name[16];									/* new layer name */

	{
	int entnum;									/* next tuple index */
	int status;									/* status returned from UNIBASE */
	int ur_get_next_data_key();
	struct UM_layer_rec layer;

	uu_denter(UU_MTRC,(us,"um_set_layer_name(%d, %s)", num, name));
	status = 0;
	entnum = 0;
	layer.rel_num = UM_LAYER_REL;
	
	while (status == 0)
		{
		entnum++;
		status = ur_get_next_tuple_index(layer.rel_num, &entnum);
		if (status == 0)
			{
			ur_retrieve_tuple(layer.rel_num, entnum, &layer);
			if (layer.num == num)
				{
				strcpy(layer.name, name);
				ur_update_tuple(layer.rel_num, entnum, &layer);
				break;
				}
			}
		}
	if (status == -1)
		{
		layer.num = num;
		layer.key = 0;
		strcpy(layer.name, name);
		layer.displayable = 1;
		layer.selectable = 1;
		layer.no_layers = 0;
		ur_create_tuple(layer.rel_num, &entnum, &layer);
		}
	uu_dexit;
	return (0);
	}
/*********************************************************************
**    E_FUNCTION     : um_layerinit()
**       Initialize the first layer tuple
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_layerinit()
{
	struct UM_layer_rec layer;
	int	entnum,status;
	char  name[16];

	uu_denter(UU_MTRC,(us,"um_layerinit()"));
	UM_BASE_LAYER = 0;
	layer.rel_num = UM_LAYER_REL;
/*
.....could be call twice, check first
*/
	status = 0;
	entnum = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_tuple_index(layer.rel_num, &entnum);
		if (status == 0)
		{
			ur_retrieve_tuple(layer.rel_num, entnum, &layer);
/*
.....don't add a default layer if a layer numbered 1 exists.
*/
			if (layer.num == 1)
				return 0;
		
/*
......don't add default layer view, it only added view when
......user select to display view (function load view, view management)
*/
/*			if (layer.num == 1)
			{
				uv_special_view(&layer, "Layer1", 2);
				return 0;
			}
*/
		}
	}
	layer.num = 1;
	layer.key = 0;
	layer.displayable = UU_TRUE;
	layer.selectable = UU_TRUE;
	layer.no_layers = 0;
	sprintf(name,"default");
	strcpy(layer.name,name);
	ur_create_tuple(layer.rel_num, &entnum, &layer);
/*
......don't add layer view when add layer, it only added view when
......user select to display view (function load view, view management)
*/
/*	uv_special_view(&layer, "Layer1", 2);
*/
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : um_set_active_layer(num)
**       Sets the active layer and the default visible and selectable
**       attributes from this layer.
**    PARAMETERS   
**       INPUT  : 
**				num    = Layer number to set.
**       OUTPUT :  
**          none
**    RETURNS      : status		0 if success , -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_set_active_layer(num)
int num;
{
	int entnum;									/* next tuple index */
	int status;									/* status returned from UNIBASE */
	int ur_get_next_data_key();
	struct UM_layer_rec layer;
/*
.....Initialize routine
*/
	status = 0;
	entnum = 0;
	layer.rel_num = UM_LAYER_REL;
/*
.....Find the active layer in the Unibase
*/
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_tuple_index(layer.rel_num, &entnum);
		if (status == 0)
		{
			ur_retrieve_tuple(layer.rel_num, entnum, &layer);
			if (layer.num == num) break;
		}
	}
/*
.....Found the layer
.....Set it as the default and
.....Set the Displayable and Selectable attributes
*/
	if (status == 0)
	{
		ur_put_attrmdl_layer(num);
		ur_put_attrmdl_displayable(1-layer.displayable);
		ur_put_attrmdl_selectable(layer.selectable);
		return(UU_SUCCESS);
	}
/*
.....Could not find the requested layer
*/
	else
		return(UU_FAILURE);
}

/*********************************************************************
**    E_FUNCTION     : um_update_layer(key,num)
**       Modifies the entities 'selectable' and 'displayable' flags
**       based on the specified layer.
**    PARAMETERS   
**       INPUT  : 
**				num    = Layer number to set.
**       OUTPUT :  
**          none
**    RETURNS      : status		0 if success , -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_update_layer(key,num)
UU_KEY_ID key;
int num;
{
	int entnum;									/* next tuple index */
	int status;									/* status returned from UNIBASE */
	int disp,flag,dsegid;
	struct UM_layer_rec layer;
	struct UC_entitydatabag e;
/*
.....Initialize routine
*/
	status = 0;
	entnum = 0;
	layer.rel_num = UM_LAYER_REL;
/*
.....Find the requested layer in the Unibase
*/
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_tuple_index(layer.rel_num, &entnum);
		if (status == 0)
		{
			ur_retrieve_tuple(layer.rel_num, entnum, &layer);
			if (layer.num == num) break;
		}
	}
/*
.....Found the layer
.....Modify the entities Displayable and Selectable attributes
*/
	if (status == 0)
	{
/*
........Modify the Displayable flag
*/
		disp = UM_DISPLAYABLE;
		if (layer.displayable == 0) disp = UM_UNDISPLAYABLE;
		ur_retrieve_displayable(key,&flag);
		if (flag != disp && flag != UM_NEVERDISPLAYABLE)
		{
			ur_update_displayable(key,disp);
			ur_retrieve_disp_segid(key,&dsegid);
			if (dsegid != -1) gssegvis(dsegid,disp);
			else
			{
				e.key = key;
				status = uc_retrieve_data(&e,sizeof(e));
				if (status == 0) uc_display(&e);
			}
		}
/*
........Modify the Selectable flag
*/
		ur_retrieve_selectable(key,&flag);
		if (flag != layer.selectable)
		{
			ur_update_selectable(key,layer.selectable);
			ur_retrieve_disp_segid(key,&dsegid);
			if (dsegid != -1) gssegdet(dsegid,layer.selectable);
		}
/*
........Update the entity's layer itself
*/
		if (num >= 0) ur_update_layer(key,num);
		return(UU_SUCCESS);
	}
/*
.....Could not find the requested layer
*/
	else
		return(UU_FAILURE);
}

/*********************************************************************
**    E_FUNCTION     : um_copy_layer_struct(num, flag)
**       Copy the layer struct from first/second unibase to/from 
**			second/first unibase
**    PARAMETERS   
**       INPUT  : 
**				num: layer number to be copied
**				offset: offset of layer when copied
**				flag: 1: copy from secondary unibase to first unibase
**					: 2: copy from first unibase to secondary unibase
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_copy_layer_struct(num, offset, flag)
int num, offset, flag;	
{
	int entnum, save_active;						
	int status;							
	struct UM_layer_rec fromlayer;
	struct UM_layer_rec tolayer;
/*	UU_KEY_ID layer;*/
/*	char tempstr[20];*/

	save_active = UR_active;

	if (flag==1)
	{
		ur_getu_second();
	}
	if (flag==2)
	{
		ur_getu_work();
	}
	status = 0;
	entnum = 0;
	fromlayer.rel_num = UM_LAYER_REL;

	while (status == 0)
	{
		entnum++;
		status = ur_get_next_tuple_index(fromlayer.rel_num, &entnum);
		if (status == 0)
		{
			ur_retrieve_tuple(fromlayer.rel_num, entnum, &fromlayer);
			if (fromlayer.num == num)
			{
				break;	
			}
		}
	}
	if (status == -1)
		goto done;

	if (flag==2)
	{
		ur_getu_second();
	}
	if (flag==1)
	{
		ur_getu_work();
	}

	status = 0;
	entnum = 0;
	tolayer.rel_num = UM_LAYER_REL;

	while (status == 0)
	{
		entnum++;
		status = ur_get_next_tuple_index(tolayer.rel_num, &entnum);
		if (status == 0)
		{
			ur_retrieve_tuple(tolayer.rel_num, entnum, &tolayer);
			if (tolayer.num == num + offset)
			{
				fromlayer.num = fromlayer.num + offset;
				ur_update_tuple(fromlayer.rel_num, entnum, &fromlayer);
				break;	
			}
		}
	}
	if (status == -1)
	{
		fromlayer.num = fromlayer.num + offset;
		ur_create_tuple(fromlayer.rel_num, &entnum, &fromlayer);
	}
done:;
	if (save_active==2)
		ur_getu_second();
	else
		ur_getu_work();
}
