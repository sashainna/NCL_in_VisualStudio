/*********************************************************************
**    NAME         :  m8irsol4.c
**       CONTAINS: routines to save/load and snapsave/restore ROMULUS
**							database
**			um_sv_romulus(filename)
**			um_selectivesave_romulus(filename)
**			um_ld_romulus(loadoperation)
**			um_post_load_sol(fnames, loadoperation)
**			um_restore_romulus(filename)
**			um_snapsave_romulus(filename)
**    	umatch(noofby, bodyids, bodynames)
**    	um_rom_appear(name, color, number)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m8irsol4.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:11
*********************************************************************/

#include <ctype.h>
#include "mfcifdef.h"
#include "ustdio.h"
#include "usysdef.h"
#include "xenv1.h"
#include "umath.h"
#include "udebug.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "msol.h"
#include "mdraw.h"
#include "mromcom.h"
#include "mdebug.h"
#include "rebus.h"

static UX_pathname UM_romulus_fname;
static UU_LOGICAL UM_romulus_load;


/*********************************************************************
**    I_FUNCTION     : um_sv_romulus(filename)
**      Save all of the ROMULUS solids in the database.
**    PARAMETERS   
**       INPUT  : 
**				filename						name of the file to write to
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_sv_romulus(filename)
	char *filename;
	{

/*
	char cmd[UX_MAX_PATH_LEN+20];
	sprintf(cmd,"TRANSMIT ^-BODIES %s",filename);
	um_romulus(cmd);
*/

	}

/*********************************************************************
**    I_FUNCTION     : um_selectivesave_romulus(filename)
**      	Save all of the marked ROMULUS solids in the database.
**			This routine is used in the save/place standard parts code.
**    PARAMETERS   
**       INPUT  : 
**				filename						name of the file to write to
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_selectivesave_romulus(filename)
	char *filename;
	{
	struct UM_body_rec e;
	struct UM_body_rec e1;
	int romstatus = UU_SUCCESS;
	int status;
	int entnum;
	char cmd[UX_MAX_PATH_LEN+20];
	char namelist[80];		

	uu_denter( UU_MTRC,(us,"um_selectivesave_romulus()"));

	/* build the namelist of the selected Romulus bodies */
	strcpy(namelist,"");
	e1.rel_num = UM_BODY_REL;
	status = 1;
	entnum = 0;
	while (status >= 0)
		{
		entnum++;
		/* get all the marked bodies to save */
		status = ur_get_next_marked_data_key(e1.rel_num, &entnum, &e1.key);
		if (status >= 0)
			{
			um_get_all_geom(&e1, sizeof(struct UM_body_rec));
			/* form a list of their names */
			strcat(namelist, ",");
			strcat(namelist, e1.name);
			}
		}

	sprintf(cmd,"TRANSMIT %s %s", namelist, filename);
	um_romulus(cmd);

	uu_dexit;
	return(romstatus);
	}	

/*********************************************************************
**    I_FUNCTION     : um_ld_romulus(loadoperation)
**    PARAMETERS   
**       INPUT  : 
**				loadoperation				UU_TRUE => load
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ld_romulus(loadoperation)
	UU_LOGICAL loadoperation;
	{

	uu_denter( UU_MTRC,(us,"um_ld_romulus()"));

	if (loadoperation) UM_NUM_OF_SOLIDS = 0;

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_post_load_sol(fnames, loadoperation)
**    PARAMETERS   
**       INPUT  : 
**				fnames					romulus file name to load
**				loadoperation			== UU_TRUE -> load
**											== UU_FALSE -> merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_post_load_sol(fnames, loadoperation)
	char *fnames;
	UU_LOGICAL loadoperation;
	{
	int status, entnum;
	int body_color;
	char cmd[UX_MAX_PATH_LEN+20];
	struct UM_body_rec e;

	uu_denter( UU_MTRC, (us, "um_post_load_sol"));

	sprintf(cmd, "RECEIVE %s", fnames);
	um_romulus(cmd);

	e.rel_num = UM_BODY_REL;

	/* update the body_number in ROMULUS */
	status = 1;
	entnum = 0;
	while (status >= 0)
		{
		entnum++;
		status = ur_get_next_new_data_key(e.rel_num, &entnum, &e.key);
		if (status >= 0)
			{
			um_get_all_geom(&e, sizeof(struct UM_body_rec));
			gtbycl(&e.id, &body_color);

			/* set the color, and body number in the ROMULUS data base */
			um_rom_appear(e.name, body_color, e.body_number);
			}
		}


	uu_dexit;
 	}

/*********************************************************************
**    I_FUNCTION     : um_restore_romulus(filename)
**			Initialize ROMULUS and issue the ROMULUS "GET SESSION"
**			command to read a save file in binary format.
**    PARAMETERS   
**       INPUT  : 
**				filename						name of file to read from
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_restore_romulus(filename)
	char *filename;
	{
	char cmd[UX_MAX_PATH_LEN+20];

	uu_denter( UU_MTRC,(us,"um_restore_romulus()"));
	strcpy(UM_rombuf.cmd[0], "ROMULUS");
	sprintf(UM_rombuf.cmd[1], "GET SESSION %s", filename);
	UM_rombuf.cur_cmd = 0;
	UM_rombuf.num_cmd = 2;
	romain();
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_snapsave_romulus(filename)
**			Issue the ROMULUS "SAVE SESSION" command to write a save file in
**			binary format.
**    PARAMETERS   
**       INPUT  : 
**				filename						name of file to read from
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_snapsave_romulus(filename)
	char *filename;
	{
	char cmd[UX_MAX_PATH_LEN+20];

	uu_denter( UU_MTRC,(us,"um_snapsave_romulus()"));
	sprintf(cmd,"SAVE SESSION %s",filename);
	um_romulus(cmd);
	uu_dexit;
	}


/*********************************************************************
**    E_FUNCTION     : umatch(noofby, bodyids, bodynames)
**			Match the new bodies in UNIBASE with each of the bodies created
**			in ROMULUS by the RECEIVE command. This is done by comparing
**			the body numbers stored in ROMULUS with the body numbers stored
**			in UNIBASE. If they match, the UNIBASE entity is updated with
**			the new name, id and edge data from ROMULUS. Finally, a new
**			body number is assigned to the UNIBASE entity. The same body
**			number will be assigned to the ROMULUS entity later in the
**			load/merge sequence (this is necessary since FORTRAN is not
**			reentrant).
**    PARAMETERS   
**       INPUT  : 
**				noofby		number of new bodies loaded in the RECEIVE cmd.
**				bodyids		ids (ROMULUS pointers) of the new bodies
**				bodynames	names of the new bodies
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umatch(noofby, bodyids, bodynames)
	int *noofby;								/* number of new bodies */
	int bodyids[];								/* ROMULUS ids for new bodies */
	char bodynames[];							/* ROMULUS names for new bodies */

	{
	int i, j, k;
	int status, entnum;
	struct  UM_body_rec e;					/* body to be created */
	int body_number;
	UU_KEY_ID active_drawing;

	uu_denter( UU_MTRC,(us,"umatch(%d,?,?)",*noofby));

	/* just for debugging */
	um_pscroll("here we are folks");
	for (i=0, k=0; i<*noofby; i++, k+=8)
		{
		sprintf(UM_sbuf,"bodyid[%d] = %x, name=<%8.8s>",
			i, bodyids[i], &bodynames[k]);
		um_pscroll(UM_sbuf);
		}

	active_drawing = ur_get_drwmdl_curdrw();
	e.rel_num = UM_BODY_REL;

	/* reconnect each ROMULUS database solid with its corresponding
		counterpart in UNIBASE */
	k = 0;
	for (i=0; i<*noofby; i++)
		{
		/* get body number for solid */
		gtbyno(&bodyids[i], &body_number);
		uu_dprint( UU_MTRC,(us,"body id = %x, body number = %d",
			bodyids[i], body_number));

		/* find unibase counterpart of solid */
		status = 1;
		entnum = 0;
		while (status >= 0)
			{
			entnum++;
			status = ur_get_next_new_data_key(e.rel_num, &entnum, &e.key);
			if (status >= 0)
				{
				um_get_all_geom(&e, sizeof(struct UM_body_rec));
				if (e.body_number == body_number) break;
				}
			}
		  
		if (status < 0)
			{/* no match found */
			uu_dprint(UU_MTRC,(us,"body number %d: no match found",
				body_number));
			}
		else
			{/* match found */
			uu_dprint(UU_MTRC,(us,"body number %d: matches key=%d",
				body_number, e.key));

			/* update body id (e.g. pointer) */
			e.id = bodyids[i];
	
			/* update solid body name */
			for (j=0; ((bodynames[k] == ' ') || (!isprint(bodynames[k]))); j++) k++;
			for (j=0; j<8; j++)
				{
				if (bodynames[k] == '@')
					e.name[j] = '\0';
				else
					e.name[j] = bodynames[k];
				k++;
				if (e.name[j] == '\0') break;
				}
			e.name[8] = '\0';
			uu_dprint( UU_MTRC,(us,"e.name = %s",e.name));
	
			/* update edge data */
			um_get_by_edge(UM_MAXEDGE, &e.id, &e.no_edge, e.edge);
	
			/* now save updates in unibase (partially complete data) */
			um_update_geom(&e, UM_DEFAULT_TF);
	
			/* make sure solids are visible in all views */
			ur_update_view_key(e.key, 0);
			if (active_drawing != 0)
				{
				ur_update_displayable(e.key, UM_UNDISPLAYABLE);
				}
	
			}
		}

	/* update body number in UNIBASE for all newly read in solids */
	status = 1;
	entnum = 0;
	while (status >= 0)
		{
		entnum++;
		status = ur_get_next_new_data_key(e.rel_num, &entnum, &e.key);
		if (status >= 0)
			{
			um_get_all_geom(&e, sizeof(struct UM_body_rec));
			UM_NUM_OF_SOLIDS++;
			e.body_number = UM_NUM_OF_SOLIDS;
			um_update_geom(&e, UM_DEFAULT_TF);
			}
		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_set_rom_color(eptr, color)
**       update the color record in romulus data base
**    PARAMETERS   
**       INPUT  : 
**          eptr			pointer to body structure to be modified
**				color			new color
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_set_rom_color(eptr, color)
	struct UM_body_rec *eptr;
	int color;

	{
	UU_REAL fnumber;
	int number;
	int bodycolor;									/* color to set into ROMULUS data
															base */
	int bodynumber;
	char cmd[80];									/* ROMULUS command buffer to send
															APPEARENCE command */

	uu_denter( UU_MTRC,(us,"um_set_rom_color(color=%d)",color));

	um_get_all_geom(eptr, sizeof(struct UM_body_rec));
	gtbyno(&eptr->id, &bodynumber);
	uu_dprint( UU_MTRC,(us,"return from gtbyno(number = %d)",
					bodynumber));

	um_rom_appear(eptr->name, color, bodynumber);

	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_rom_appear(name, color, number)
**      Process the Romulus APPEARANCE cmd. Input is in integer form.
**			Romulus input is UU_REAL. (Unibase stores an integer, but 
**			Romulus needs a number between 0.0 and 1.0, and not less than
**			.01).
**    PARAMETERS   
**       INPUT  : 
**				name						name of the body
**				color						body color (integer)
**				number					body number (integer)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_rom_appear(name, color, number)
	char *name;
	int color;
	int number;

	{
	UU_REAL body_color;					/* color for ROMULUS cmd. */
	UU_REAL body_number;					/* body number - used to match	*/
												/*	bodies after load/merge cmd. */
	char cmd[120];

	uu_denter( UU_MTRC,(us,"um_rom_appear(%s,%d,%d)",
		name, color, number));

	body_color  = (color  * .01) + .005;	/* Convert integer to a number */
														/* between 0.0 and 1.0. Add 	 */
														/* .005 to account for Romulus */
														/* truncation errors				 */
	body_number = (number * .01) + .005;	/* Convert integer to a number */
														/* between 0.0 and 1.0. Add 	 */
														/* .005 to account for Romulus */
														/* truncation errors				 */

	sprintf(cmd, "APPEARANCE %s SET COLOUR %f 0 %f",
			  name, body_color, body_number);
	um_romulus(cmd);
	um_init_rombuf();

	uu_dexit;
	}
