/*********************************************************************
**    NAME         :	m2savld.c
**       CONTAINS:
**			um_print_duplist(title, duplist)
**			um_pre_save(saveall)
**			um_post_save()
**			um_pre_load(loadoperation)
**			um_pre_load_layers(loadoperation)
**			um_pre_load_screens(loadoperation)
**			um_pre_load_vports()(loadoperation)
**			um_pre_load_views(loadoperation)
**    	um_post_load_lights(loadoperation)
**			um_post_load(fnames, fnamea, loadoperation)
**			um_post_load_layers(loadoperation)
**			um_post_load_screens(loadoperation)
**			um_post_load_vports(loadoperation)
**			um_post_load_views(loadoperation)
**			um_post_load_coordsys(loadoperation)
**			um_post_restore()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m2savld.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:48
*********************************************************************/
#include "usysdef.h"
#include "lipv.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mrenddl.h"
#include "mdebug.h"
#include "mderror.h"
#include "mattr.h"
#include "mxxx.h"
#include "msave.h"
#include "mdgenent.h"
#include "rmtuple.h"
#include "view.h"
#include "vconst.h"
#include "gtbldef.h"
#include "dasnog.h"
#include "nclver.h"
#include "nclmodals.h"

/*NCL: flag == UU_TRUE if we are loading a unibase without view rels */
extern UU_LOGICAL NCL_view_load;
extern int UR_restore_lights;

#define UM_NULL_NAME "                "

struct UM_dup_list {
	int olddata;
	int newdata;
	char name[20];
	};

extern UU_LIST NCL_ldr_list;
/*********************************************************************
**    I_FUNCTION     : um_print_duplist(title, duplist)
**       print a duplication list.
**    PARAMETERS   
**       INPUT  : 
**          title						title to print
**				duplist					list of UM_dup_list entities
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_print_duplist(title, duplist)
	char *title;
	UU_LIST *duplist;

	{
	struct UM_dup_list *list;
	int i;

	list = (struct UM_dup_list *) UU_LIST_ARRAY(duplist);
	um_pscroll(title);
	for (i=0; i<duplist->cur_cnt; i++)
		{
		sprintf(UM_sbuf,"old=%d, new=%d, name=%s", list[i].olddata,
			list[i].newdata, list[i].name);
		um_pscroll(UM_sbuf);
		}
	um_pscroll("");
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     :		um_pre_save(saveall)
**			Save a list of all the current border segements. 
**			Set the border segements to -1, so that when this file is
**			loaded, the system will not try deleteing gks segements
**			that do not exist.	
**    PARAMETERS   
**       INPUT  : 
**				saveall		logical flag to set selective save or full save
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_pre_save(saveall)
	UU_LOGICAL saveall;				/* for selective save and standard parts */

	{
	UV_vport	vport;					/* view port record */
	int	i;

	uu_denter(UU_MTRC,(us,"um_pre_save(saveall)"));

	/*NCL: if altered label location table is unused, delete it */
	if (ncl_used_labtbl() != UU_SUCCESS)
		ncl_delete_labtbl();

	/* set all the view port border segments to -1 in the save file */
	/* This is a necessary operation for load but not for a merge */
	for (i = 0; i < UV_act_screen[0].nvports; i++)
		{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		UM_BORD_SEGS[i] = vport.bord_seg;
		vport.bord_seg = -1;
		uv_putvp(&vport);
		}

	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     :	um_post_save()
**			Set the border segments (vport.bord_seg variable) back
**			to what they were before the save operation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_post_save()
	{
	UV_vport	vport;					/* view port record */
	int	i;

	uu_denter(UU_MTRC,(us,"um_post_save()"));

	/* NCL: in case label table was deleted, recreate it */
	ncl_init_labtbl((UU_KEY_ID)1);

	/* set all the view port border segments back to their pre_save values */
	for (i = 0; i < UV_act_screen[0].nvports; i++)
		{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		vport.bord_seg = UM_BORD_SEGS[i];
		uv_putvp(&vport);
		}
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : um_pre_load(loadoperation)
**			Prelaod layers, screens, view ports and views.
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_pre_load(loadoperation)
	UU_LOGICAL loadoperation;

	{
	uu_denter(UU_MTRC,(us,"um_pre_load(loadop=%d)", loadoperation));

	/* MILLS: Used to fix-up environment after load ... */
	ncl_pre_load_environ();

	um_pre_load_layers(loadoperation);
	um_pre_load_screens(loadoperation);
	um_pre_load_vports(loadoperation);
	um_pre_load_views(loadoperation);

/*
	um_print_duplist("layers", &UM_cur_layers);
	um_print_duplist("screens", &UM_cur_screens);
	um_print_duplist("vports", &UM_cur_vports);
	um_print_duplist("views", &UM_cur_views);
*/

	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     :	um_pre_load_layers(loadoperation)
**       Save a list of all the layers existing in unibase in the 
**			UU_LIST UM_cur_layers.
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_pre_load_layers(loadoperation)
	UU_LOGICAL loadoperation;

	{
	int status;							/* status of UNIBASE return */
	int entnum;							/* next entity to return from UNIBASE */
	struct UM_dup_list listelem;
	struct UM_layer_rec layer;

	uu_denter(UU_MTRC,(us,"um_pre_load_layers(loadop=%d)",loadoperation));

	uu_list_init(&UM_cur_layers, sizeof(struct UM_dup_list), 100, 100);

	entnum = 0;
	status = 1;
	while (status >= 0)
		{
		entnum++;
		status = ur_get_next_tuple_index(UM_LAYER_REL, &entnum);
		if (status == 0) 
			{
			ur_retrieve_tuple(UM_LAYER_REL, entnum, &layer);
			listelem.olddata = layer.num;
			listelem.newdata = 0;
			strcpy(listelem.name, "");
			uu_list_push(&UM_cur_layers, &listelem);
			}
		}

	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     :	um_pre_load_screens(loadoperation)
**       Save a list of all the screens existing in unibase in the 
**			UU_LIST UM_cur_screens.
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_pre_load_screens(loadoperation)
	UU_LOGICAL loadoperation;

	{
	int		status;						/* status of UNIBASE return */
	int		entnum;						/* next entity to return from UNIBASE */
	UU_KEY_ID key;
	struct UM_dup_list listelem;
	UV_screen screen;

	uu_denter(UU_MTRC,(us,"um_pre_load_screens(loadop=%d)",loadoperation));

	uu_list_init(&UM_cur_screens, sizeof(listelem), 100, 100);

	status = 1;
	entnum = 0;
	while (status >= 0)
		{
		entnum++;
		status = ur_get_next_data_key(UV_SCREEN_REL, &entnum, &key);
		if (status >= 0)
			{
			uv_getscid(key, &screen);
			listelem.olddata = key;
			strcpy(listelem.name, screen.name);
			listelem.newdata = 0;
			uu_list_push(&UM_cur_screens, &listelem);
			}
		}

	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     :	um_pre_load_vports(loadoperation)
**       Save a list of all the vports existing in unibase in the 
**			UU_LIST UM_cur_vports.
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_pre_load_vports(loadoperation)
	UU_LOGICAL loadoperation;

	{
	int		status;						/* status of UNIBASE return */
	int		entnum;						/* next entity to return from UNIBASE */
	UU_KEY_ID key;
	struct UM_dup_list listelem;
	UV_vport vport;

	uu_denter(UU_MTRC,(us,"um_pre_load_vports(loadop=%d)",loadoperation));

	uu_list_init(&UM_cur_vports, sizeof(struct UM_dup_list), 100, 100);

	status = 1;
	entnum = 0;
	while (status >= 0)
		{
		entnum++;
		status = ur_get_next_data_key(UV_VPORT_REL, &entnum, &key);
		if (status >= 0)
			{
			uv_getvpid(key, &vport);
			listelem.olddata = key;
			strcpy(listelem.name, vport.name);
			listelem.newdata = 0;
			uu_list_push(&UM_cur_vports, &listelem);
			}
		}

	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     :	um_pre_load_views(loadoperation)
**       Save a list of all the views existing in unibase in the 
**			UU_LIST UM_cur_views.
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_pre_load_views(loadoperation)
	UU_LOGICAL loadoperation;

	{
	int		status;						/* status of UNIBASE return */
	int		entnum;						/* next entity to return from UNIBASE */
	UU_KEY_ID key;
	struct UM_dup_list listelem;
	UV_view view;

	uu_denter(UU_MTRC,(us,"um_pre_load_views(loadop=%d)",loadoperation));

	uu_list_init(&UM_cur_views, sizeof(listelem), 100, 100);

	status = 1;
	entnum = 0;
	while (status >= 0)
		{
		entnum++;
		status = ur_get_next_data_key(UV_VIEW_REL, &entnum, &key);
		if (status >= 0)
			{
			uv_getvid(key, &view);
			listelem.olddata = key;
			strcpy(listelem.name, view.name);
			listelem.newdata = 0;
			uu_list_push(&UM_cur_views, &listelem);
			}
		}

	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     :	um_post_load(fnames, fnamea, loadoperation)
**			Post process all the layers, screens, view ports, views,
**			and  solids. Load the symbol table for the calculator.
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS: UU_SUCCESS if no problems encountered; UM_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_post_load(fnames, fnamea, loadoperation)
	char *fnames;
	char *fnamea;
	UU_LOGICAL loadoperation;

	{
	int status = UU_SUCCESS;
	UU_KEY_ID invisible;
	uu_denter(UU_MTRC,(us,"um_post_load(loadop=%d)",loadoperation));

	/* NCL: if we loaded a Unibase without viewing, initialize missing things in unibase ... */
	ncl_reset_unibase(loadoperation);
	um_post_load_lights(loadoperation);
	ncl_post_load_color(loadoperation);
	um_post_load_layers(loadoperation);
	um_post_load_screens(loadoperation);
	um_post_load_vports(loadoperation);
	um_post_load_views(loadoperation);
	uv_special_view(&invisible, "Invisible", 3);
/*
.....check if secondary unibase exist, if yes, add "external unibase view"
*/
	uv_special_view(&invisible, "Extern Unibase", 4);
/*
.....load the screen layout file again because the unibase will change it
.....the screen definintion in layout file will not overwrite the unibase (just loaded)
.....screen definition
*/
	uv_load_scr_layout(0);

	um_post_load_activesc(loadoperation);
	um_post_load_coordsys(loadoperation);
/*
......load the light before drawing anything
*/
/*	um_post_load_lights(loadoperation); */
	um_post_load_sol(fnames, loadoperation);
	um_post_load_appgeo();
	uq_load(!loadoperation);
	ncl_post_load(loadoperation);
	if (ubu_chk_mastersyms() != UU_SUCCESS) goto failed;

	/*NCL: determine label table ... */
	ncl_init_labtbl((UU_KEY_ID)1);
/*
.....Initialize the list used for labels and leader lines
*/
	uu_list_init0 (&NCL_ldr_list);
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_post_load", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     :	um_post_load_layers(loadoperation)
**			Add the user specified base layer number to all newly loaded
**			layer records.	If any of the new layer records have the same
**			number as a current layer, then the new layer record will be
**			deleted and the user notified.
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_post_load_layers(loadoperation)
	UU_LOGICAL  loadoperation;

	{
	int		numint;						/* number of entities returned from DAS */
	int		status;						/* status of UNIBASE return */
	int		entnum;						/* tuple index to retrieve from UNIBASE */
	int		base_number;				/* base number to add to the new layers */
	int		i;
	UU_LOGICAL update;
	struct UM_layer_rec newlayer;
	struct UM_dup_list *oldlayer;
	int layer_number;
	int number_old_layers;
	UU_KEY_ID key;

	uu_denter(UU_MTRC,(us,"um_post_load_layers(loadop=%d)",loadoperation));

	/* get the new base layer number */
	base_number = 0;
	if (!loadoperation)
		{
		ud_ldas(UD_DASINT,/*base number*/UM_MODEL,253,&base_number,1,&numint,
			UD_DEFAULT);
		}

	/* remove duplicate layer tuples */
	status = 0;
	entnum = 0;
	newlayer.rel_num = UM_LAYER_REL;
	number_old_layers = UM_cur_layers.cur_cnt;
	oldlayer = (struct UM_dup_list *) UU_LIST_ARRAY(&UM_cur_layers);
	while(status == 0)
		{
		entnum++;
		status = ur_get_next_new_tuple_index(newlayer.rel_num, &entnum);
		if (status == 0) 
			{
			ur_retrieve_tuple(newlayer.rel_num, entnum, &newlayer);
			newlayer.num = newlayer.num + base_number;
			update = UU_TRUE;
			for (i=0; i<number_old_layers; i++)
				{
				if (newlayer.num == oldlayer[i].olddata)
					{
					ur_delete_tuple(newlayer.rel_num, entnum);
					update = UU_FALSE;
					break;
					}
				}
			if (update)
				ur_update_tuple(newlayer.rel_num, entnum, &newlayer);
			}
		}

	/* add the base layer number to the attribute records associated
		with all new relations */
	if ((base_number != 0) || !loadoperation)
		{
		entnum = 0;
		status = 1;
		while (status >= 0)
			{
			entnum++;
			status = ur_get_next_new_tuple_index(UR_MTUPLE_REL, &entnum) ;
			if (status >= 0)
				{
				ur_rt2k(UR_MTUPLE_REL, entnum, &key) ;
				ur_retrieve_layer(key, &layer_number);
				uu_dprint(UU_MTRC,(us,"*****layer_number = %d*****",layer_number));
				layer_number = layer_number + base_number;
				uu_dprint(UU_MTRC,(us,"****layer_number = %d****",layer_number));
				ur_update_layer(key, layer_number);
				}
			}
		}
	uu_list_free(&UM_cur_layers);
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     :	um_post_load_screens(loadoperation)
**			For each new screen record, if a screen with the same name
**			already exists in UNIBASE - then delete it.
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_post_load_screens(loadoperation)
	UU_LOGICAL  loadoperation;

	{
	int			status;						/* status of UNIBASE return */
	int			entnum;						/* tuple index */
	UV_screen	newscreen;					/* new screen */
	int number_old_screens;
	struct UM_dup_list *oldscreen;
	int			i;

	uu_denter(UU_MTRC,(us,"um_post_load_screens(loadop=%d)",loadoperation));

	if (!loadoperation)
		{
		/* delete all of the new screen tuples */
		oldscreen = (struct UM_dup_list *) UU_LIST_ARRAY(&UM_cur_screens);
		number_old_screens = UM_cur_screens.cur_cnt;
 		newscreen.rel_num = UV_SCREEN_REL;
		status = 1;
		entnum = 0;
		while (status >= 0)
			{
			entnum++;
			status = ur_get_next_new_data_key(newscreen.rel_num, 
														 &entnum, &newscreen.key);
			if (status >= 0)
				{
				uv_getscid(newscreen.key, &newscreen);
				for (i=0; i<number_old_screens; i++)
					{
					if (strcmp(oldscreen[i].name, newscreen.name) == 0)
						{
						/* uu_uerror1(UM_MODEL, 216, newscreen.name); */
						ur_delete_all(newscreen.key);
						break;
						}
					}
				}
			}
		}
	uu_list_free(&UM_cur_screens);
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     :	um_post_load_activesc(loadoperation)
**			Call the viewing system to activate screens
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_post_load_activesc(loadoperation)
	UU_LOGICAL  loadoperation;

	{
	int			status;						/* status of UNIBASE return */
	int			entnum;						/* tuple index */
	UV_screen	newscreen;					/* new screen */

	uu_denter(UU_MTRC,(us,"um_post_load_activesc(loadop=%d)",loadoperation));

	if (loadoperation)
		{
 		newscreen.rel_num = UV_SCREEN_REL;
		status = 1;
		entnum = 0;
		uv_clear();
		while (status >= 0)
			{
			entnum++;
			status = ur_get_next_data_key(newscreen.rel_num, &entnum,
													&newscreen.key);
			if (status >= 0)
				{
				ur_retrieve_data(&newscreen, sizeof(UV_screen));
				if (newscreen.active == UV_ACTIVE)
					{
				/*	UV_no_act_screens = 0;*/
					uv_set_screen(&newscreen, UU_TRUE);
					}
				}
			}
		}
	uu_list_free(&UM_cur_screens);
	uu_dexit;
	return 0;
	}
/*********************************************************************
**    E_FUNCTION     :	um_post_load_vports(loadoperation)
**			For each new vport record, if a vport with the same name
**			already exists in UNIBASE - then delete it.
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_post_load_vports(loadoperation)
	UU_LOGICAL  loadoperation;
	{
	int		status;						/* status of UNIBASE return */
	int		entnum;						/* tuple index to retrieve from UNIBASE */
	UV_vport	newvport;					/* vport relation of new vports */
	struct UM_dup_list *oldvport;
	int number_old_vports;
	int		i;
	UV_view nclipv;

	uu_denter(UU_MTRC,(us,"um_post_load_vports(loadop=%d)",loadoperation));

	/* NCL: to account for Unibases with missing viewing relations */
	if (loadoperation && (NCL_view_load != 0))
		{
		/* set all border segments to -1 (i.e. not created in DIGS) */
		newvport.rel_num = UV_VPORT_REL;
		status = 1;
		entnum = 0;
		while (status >= 0)
			{
			entnum++;
			status = ur_get_next_new_data_key(newvport.rel_num, 
														 &entnum, &newvport.key);
			if (status == 0)
				{
				uv_getvpid(newvport.key, &newvport);
				newvport.bord_seg = -1;
				uv_putvp(&newvport);
				}
			}
		}
	else
		{
		/* delete all the new vport tuples which are duplicates of old
			vport tuples*/
		oldvport = (struct UM_dup_list *) UU_LIST_ARRAY(&UM_cur_vports);
		number_old_vports = UM_cur_vports.cur_cnt;
		newvport.rel_num = UV_VPORT_REL;
		status = 1;
		entnum = 0;
		while (status >= 0)
			{
			entnum++;
			status = ur_get_next_new_data_key(newvport.rel_num, 
														 &entnum, &newvport.key);
			if (status >= 0)
				{
				uv_getvpid(newvport.key, &newvport);
				for (i=0; i<number_old_vports; i++)
					{
					if (strcmp(oldvport[i].name, newvport.name) == 0)
						{
						ur_delete_all(newvport.key);
						break;
						}
					}
				}
			}
		}
	uu_list_free(&UM_cur_vports);
/*
.....nclipv viewport must exist
*/
	if (uv_getvnm("Nclipv",&nclipv) != UU_SUCCESS)
		uv_nclipv_view(&nclipv.key);
	if (uv_getvpnm("nclipv",&LW_vport) != UU_SUCCESS)
		uv_nclipv_screen(nclipv.key);
	else
		uv_vtovp(&LW_vport,nclipv.key);
	uu_dexit;
	return 0;
	}
/*********************************************************************
**    E_FUNCTION     :	um_post_load_views(loadoperation)
**			For each new view record: if it is a reference view - delete
**			it, if a view with the same name already exists in UNIBASE
**			then delete the new version.
**			Cycle through all the new entities to be sure they point to
**			a valid view key, after all deletes have occured.
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_post_load_views(loadoperation)
	UU_LOGICAL  loadoperation;

	{
	int		status, stat;				/* status of UNIBASE return */
	int		entnum, next_index;		/* tuple index to retrieve from UNIBASE */
	UV_view	newview;						/* view relation of new views */
	int		i;
	UU_KEY_ID	key;						/* key of master tuple entity */
	UU_KEY_ID 	view_key;				/* key of the view that is associated
													with master tuple */
	struct UM_dup_list *oldview;
	int number_old_views;
	int			relnum;	

	uu_denter(UU_MTRC,(us,"um_post_load_views(loadop=%d)",loadoperation));

	if (!loadoperation)
		{
		/* delete all new views which have name which are identical to
			an old view name */
		oldview = (struct UM_dup_list *) UU_LIST_ARRAY(&UM_cur_views);
		number_old_views = UM_cur_views.cur_cnt;
		newview.rel_num = UV_VIEW_REL;
		status = 1;
		entnum = 0;
		while (status >= 0)
			{
			entnum++;
			status = ur_get_next_new_data_key(newview.rel_num, &entnum,
														 &newview.key);
			if (status >= 0)
				{
				uv_getvid(newview.key, &newview);
				for (i=0; i<number_old_views; i++)
					{
					if (strcmp(oldview[i].name, newview.name) == 0)
						{
						oldview[i].newdata = newview.key;
						if (newview.vtype != UV_SYSDEF_VIEW)
							uu_uerror1(UM_MODEL, 215, newview.name);
						ur_delete_all(newview.key);
						break;
						}
					}
				}
			}

		/* update the view key on all new MASTER TUPLE relations
			to contain the old view key */
		next_index = 0;
		stat = 1;
		while (stat >= 0)
			{
			next_index++;
			stat = ur_get_next_new_tuple_index(UR_MTUPLE_REL, &next_index);
			if (stat >= 0)
				{
				ur_rt2k(UR_MTUPLE_REL, next_index, &key) ;
				ur_retrieve_data_relnum(key, &relnum);
				if ((relnum != UV_SCREEN_REL) &&
					 (relnum != UV_VPORT_REL) &&
					 (relnum != UV_VIEW_REL))
					{
					ur_retrieve_view_key(key, &view_key);
					for (i=0; i<number_old_views; i++)
						{
						if ((view_key != 0) && (view_key == oldview[i].newdata))
							{
							ur_update_view_key(key, oldview[i].olddata);
							break;
							}
						}
					}
				}
			}
		}
	uu_list_free(&UM_cur_views);
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : um_post_load_lights(loadoperation)
**       If this is a merge operation (LOADOPERATION is false) then
**			delete all of the new lights systems. If this is a load
**			operation, must save new light keys in UM_light_keys.
**			This is temporary only!!!!
**
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_post_load_lights(loadoperation)
	UU_LOGICAL loadoperation;

	{ 
	int status; 
	int entnum;
	int index,not_blank;
	UU_KEY_ID key, savkey;
	extern int UM_light_keys[];
	struct UM_light_rec light;
	struct UM_attrdata_rec attr;
	UU_LOGICAL	blanked;

	uu_denter(UU_MTRC,(us,"um_post_load_lights(loadop=%d)",
		loadoperation));
	if (loadoperation)
		{
		/* Save all new light keys in UM_light_keys */
		status = 1;
		entnum = 0;
		index = 0;
		not_blank = 0;
		savkey = 0;
		while (status >= 0)
		{
			entnum++;
			status = ur_get_next_new_data_key(UM_LIGHT_REL, 
														 &entnum, &light.key);
/*
.....Check for maximum light number
.....As on there has been at least one case
.....where Unibases were merged and the
.....number of lights were doubled.
.....Bobby  -  2/28/92
*/
			if (entnum > GMAXLIGHTS) status = 1;

			if (status == 0)
				{

				/* Save this relnum */
				UM_light_keys[index] = light.key;

				/* Update digs light source */
				uc_retrieve_data(&light, sizeof(struct UM_light_rec));
				uc_retrieve_attr(light.key, &attr);
				ur_retrieve_blanked(light.key, &blanked);
/*
.....this only version may not using light, but we have to enable one light
.....in order to see
*/
				if (blanked==0)
					not_blank = 1;
				if (index==0)
					savkey = light.key;
				index++;
				
				}
			}
			if ((not_blank==0) && (NCL_infile_version < 9.350) && savkey != 0)
				ur_update_blanked(savkey, UU_FALSE);
		}
	else
		{/* must be a merge operation */
/*
.....Initialize entnum.  JLS 4/19/99
*/
		entnum =0;
		status = 0;		/* initialize so we don't skip the while loop */
		while(status == 0)
			{
			entnum++;
			status = ur_get_next_new_data_key(UM_LIGHT_REL, &entnum, &key);
			if (status == 0) ur_delete_all(key);
			}
		}
	if(loadoperation && UR_restore_lights)
		ul_unibase_lights();
	
	
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : um_post_load_coordsys(loadoperation)
**       If this is a merge operation (LOADOPERATION is false) then
**			delete all of the new coordinate systems. This is temporary
**			only!!!!
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_post_load_coordsys(loadoperation)
	UU_LOGICAL loadoperation;

	{
	int status;
	int entnum;
	UU_KEY_ID key;
	struct UM_coordsys_rec cpln;

	uu_denter(UU_MTRC,(us,"um_post_load_coordsys(loadop=%d)",
		loadoperation));

	if (loadoperation)
		{
		cpln.key = ur_get_dispattr_cpln_key();
		status = uc_retrieve_data(&cpln, sizeof(cpln));
		if (status == 0)
		{
			um_setcpln_zdepth(cpln.z_depth);
			um_setcpln(cpln.origin, cpln.xaxis, cpln.yaxis, cpln.zaxis);
/*
.....Found Unibases with the CPLN string set to MCS
.....So let's fix it.  (Don't know how these Unibases
.....were created though)
.....Bobby  -  12/6/99
*/
/*
.....This code caused problems because the modeling coord sys was
.....not being set correctly below.
**			if (strcmp(cpln.name,"CPLN") != 0)
**			{
**				strcpy(cpln.name,"CPLN");
**				um_update_geom(&cpln,UM_DEFAULT_TF);
**			}
*/
		}
/*
...vp 4/14/98 find it unibase if possible
*/
		um_get_axis ("MCS",&key);
		if (key) um_reset_mod_axis(key-1);
		}
	else
		{/* must be a merge operation */
		status = 0;
		entnum = 0;
		while(status == 0)
			{
			entnum++;
			status = ur_get_next_new_data_key(UM_COORDSYS_REL, &entnum, &key);
			if (status == 0) ur_delete_all(key);
			}
		}
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : um_post_restore()
**       Process UNIBASE relations after a "restore" operation. 
**			Currently, this means setting all embedded DIGS display
**			segment identifiers to -1 in the view port relation, and
**			setting an active screen.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_post_restore()

	{
	int		status;						/* status of UNIBASE return */
	int		entnum;						/* tuple index to retrieve from UNIBASE */
	UV_vport	newvport;
	UV_screen newscreen;

	uu_denter(UU_MTRC,(us,"um_restore()"));

	/* set all border segments to -1 (i.e. not created in DIGS) */
	newvport.rel_num = UV_VPORT_REL;
	status = 1;
	entnum = 0;
	while (status >= 0)
		{
		entnum++;
		status = ur_get_next_new_data_key(newvport.rel_num, 
													 &entnum, &newvport.key);
		if (status == 0)
			{
			uv_getvpid(newvport.key, &newvport);
			newvport.bord_seg = -1;
			uv_putvp(&newvport);
			}
		}

	/* activate a screen */
 	newscreen.rel_num = UV_SCREEN_REL;
	status = 1;
	entnum = 0;
	while (status >= 0)
		{
		entnum++;
		status = ur_get_next_new_data_key(newscreen.rel_num, &entnum,
												&newscreen.key);
		if (status >= 0)
			{
			ur_retrieve_data(&newscreen, sizeof(UV_screen));
			if (newscreen.active == UV_ACTIVE)
				{
				UV_no_act_screens = 0;
				uv_set_screen(&newscreen, UU_TRUE);
				}
			}
		}
	uu_dexit;
	return 0;
	} 


/*********************************************************************
**    E_FUNCTION     : um_post_load_lights2(loadoperation)
**       If this is a merge operation (LOADOPERATION is false) then
**			delete all of the new lights systems. If this is a load
**			operation, must save new light keys in UM_light_keys.
**			This is temporary only!!!!
**
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*um_post_load_lights2(loadoperation)
	UU_LOGICAL loadoperation;

	{ 
	int status; 
	int entnum;
	int index,not_blank;
	UU_KEY_ID key, savkey;
	struct UM_light_rec light;
	struct UM_attrdata_rec attr;
	UU_LOGICAL	blanked;

	uu_denter(UU_MTRC,(us,"um_post_load_lights(loadop=%d)",
		loadoperation));
	if (loadoperation)
		{
		status = 1;
		entnum = 0;
		index = 0;
		not_blank = 0;
		while (status >= 0)
			{
			entnum++;
			status = ur_get_next_new_data_key(UM_LIGHT_REL, 
														 &entnum, &light.key);
			if (entnum > GMAXLIGHTS) status = 1;

			if (status == 0)
				{

				UM_light_keys2[index] = light.key;

				uc_retrieve_data(&light, sizeof(struct UM_light_rec));
				uc_retrieve_attr(light.key, &attr);
				ur_retrieve_blanked(light.key, &blanked);

				if (blanked==0)
					not_blank = 1;
				if (index==0)
					savkey = light.key;
				index++;
				}
			}
			if ((not_blank==0) && (NCL_infile_version < 9.350))
				ur_update_blanked(savkey, UU_FALSE);
		}
	else
		{
		entnum =0;
		status = 0;	
		while(status == 0)
			{
			entnum++;
			status = ur_get_next_new_data_key(UM_LIGHT_REL, &entnum, &key);
			if (status == 0) ur_delete_all(key);
			}
		}
	uu_dexit;
	return 0;
	}

*/
