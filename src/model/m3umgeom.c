/********************************************************************* 
**    NAME         :  m3umgeom.c
**       CONTAINS: user interface routines to modify entity
**    E_FUNCTION     : umu_modify_entity_geometry()
**    E_FUNCTION     : int uc_modify_entity(key)
**    E_FUNCTION     : int umu_modify_line(key)
**    E_FUNCTION     : int umu_modify_circle(key)
**    E_FUNCTION     : int umu_modify_conic(key)
**    E_FUNCTION     : int umu_modify_polygon(key)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3umgeom.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:59
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mdpick.h"
#include "mpopmenu.h"
#include "mcrv.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION     : ucu_modify_entity()
**       Allow the user to pick an entity which is to be modified.
**			The choices available for modification will depend upon
**			the entity picked.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_modify_entity_geometry()

	{
	UM_PICKENT pent;
	int numint;
	int status;
	UU_KEY_ID key;

	uu_denter(UU_MTRC,(us,"umu_modify_entity_geometry()"));

	ud_leditable(UU_TRUE);
	while (UU_TRUE)
		{
		um_dl_pdas(UD_DASPICK, /*pick entity to modify */UM_MODEL, 270, &pent, 
						1, &numint, 1);
		if (numint < 1) goto done;
	
		key = um_get_pickkey(&pent, 1);
		status = uc_modify_entity(key);
		if (status != UU_SUCCESS)
			uu_uerror0(/* no methods available to modify this entity */
				UM_MODEL, 237);
		}

done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int uc_modify_entity(key)
**			Modify the specified entity (KEY).
**    PARAMETERS   
**       INPUT  : 
**				key				key of entity to modify
**       OUTPUT :  
**          none
**    RETURNS      : 
**				UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_modify_entity(key)
UU_KEY_ID key;

	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;
	int rel_num;

	uu_denter(UU_MTRC,(us,"uc_modify_entity(key=%x)",
		key));
	ur_retrieve_data_relnum(key, &rel_num);
	class = CONVERT_REL_TO_CLASS(rel_num);
	function = ucu_validate(&UC_cot_descriptor, class, UC_MODIFY_ENTITY);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		status = (*function)(key);
	uu_dexitstatus("uc_modify_entity", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int umu_modify_line(key)
**			Modify the specified line (KEY).
**    PARAMETERS   
**       INPUT  : 
**				key				key of line to modify
**       OUTPUT :  
**          none
**    RETURNS      : 
**				UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umu_modify_line(key)
	UU_KEY_ID key;

	{
	int status;
	int option;

	uu_denter(UU_MTRC,(us,"umu_modify_line(key=%x)",
		key));

	status = um_popupmenu(UM_MODIFY_LINE, &option);
	umu_m2_modline(option);

	status = UU_SUCCESS;

	uu_dexitstatus("umu_modify_line", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int umu_modify_circle(key)
**			Modify the specified circle (KEY).
**    PARAMETERS   
**       INPUT  : 
**				key				key of circle to modify
**       OUTPUT :  
**          none
**    RETURNS      : 
**				UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umu_modify_circle(key)
	UU_KEY_ID key;

	{
	int status;
	int option;

	uu_denter(UU_MTRC,(us,"umu_modify_circle(key=%x)",
		key));

	status = um_popupmenu(UM_MODIFY_CIRCLE, &option);

	if (option == 1)
		umu_m3_mvcirend();
	else if (option == 2)
		umu_m3_chgcirang();
	else if (option == 3)
		umu_m3_modcirrad(key);
	else if (option == 4)
		umu_m3_breakcirc();
	else if (option == 5)
		umu_m3_closecir(key);
	else if (option == 6)
		umu_m3_modcircen(key);
	else if (option == 7)
		umu_m3_modcircomplement(key);

	status = UU_SUCCESS;

	uu_dexitstatus("umu_modify_circle", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int umu_modify_conic(key)
**			Modify the specified conic (KEY).
**    PARAMETERS   
**       INPUT  : 
**				key				key of conic to modify
**       OUTPUT :  
**          none
**    RETURNS      : 
**				UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umu_modify_conic(key)
	UU_KEY_ID key;

	{
	int status;
	int option;
	struct UM_conic_rec conic;

	uu_denter(UU_MTRC,(us,"umu_modify_conic(key=%x)",
		key));
	
	conic.key = key;
	status = uc_retrieve_data(&conic, sizeof(conic));
	if (status != UU_SUCCESS) goto done;

	switch (conic.type)
		{
		case UM_ELLIPSE:
			status = um_popupmenu(UM_MODIFY_ELLIPSE, &option);
			if (option == 1)
				umu_m4_ellaxes(key);
			else if (option == 2)
				umu_m4_close(key);
			status = UU_SUCCESS;
			break;
		case UM_PARABOLA:
		case UM_HYPERBOLA:
		default:
			status = UU_FAILURE;
			break;
		}

done:
	uu_dexitstatus("umu_modify_conic", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int umu_modify_polygon(key)
**			Modify the specified polygon (KEY).
**    PARAMETERS   
**       INPUT  : 
**				key				key of polygon to modify
**       OUTPUT :  
**          none
**    RETURNS      : 
**				UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umu_modify_polygon(key)
	UU_KEY_ID key;

	{
	int status;
	int option;

	uu_denter(UU_MTRC,(us,"umu_modify_polygon(key=%x)",
		key));

	status = um_popupmenu(UM_MODIFY_POLYGON, &option);

	umu_m40_modpolygon(key,option);

	status = UU_SUCCESS;

	uu_dexitstatus("umu_modify_polygon", status);
	return(status);
	}

