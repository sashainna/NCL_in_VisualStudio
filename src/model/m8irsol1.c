
/*********************************************************************
**    NAME         :  m8irsol1.c
**       CONTAINS:
**			um_callromulus()
**			um_init_rombuf()
**			um_add_rombuf(str)
**			um_romulus(cmd)
**			um_cre_body_list(option, color, list)
**			um_cre_bodies(option, color)
**			um_get_by_name(option, id, name)
**			um_get_by_edge(maxid, id, numid, edgeid)
**			oclash()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m8irsol1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:10
*********************************************************************/
#include "mfcifdef.h"
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "usysg.h"
#include "ulist.h"
#include "go.h"
#include "dmark.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mattr.h"
#include "msol.h"
#include "mromcom.h"
#include "mdebug.h"

#define CURRENT_BODY 0
#define NEWEST_BODY 1


/*********************************************************************
**    I_FUNCTION     : um_callromulus()
**			Call ROMULUS to execute all of the commands currently in 
**			the command buffer.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_callromulus()

	{
	int markval;

	uu_denter( UU_MTRC,(us,"um_callromulus()"));
	UD_MARK(markval, UU_FALSE);
	if (markval == 0)
		{
		romain();
		}
	else
		{
		ud_hakt(UM_MODEL,116);
		strcpy( UM_rombuf.cmd[0],"ROMULUS");
		strcpy( UM_rombuf.cmd[1],"GET SESSION backup.tmp");
		UM_rombuf.cur_cmd = 0;
		UM_rombuf.num_cmd = 2;
		romain();
		}
	gplout();
	UD_UNMARK(markval);
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_init_rombuf()
**      Initialize the command buffer used to hold commands for ROMULUS.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_init_rombuf()
	{

	uu_denter( UU_MTRC,(us,"um_init_rombuf()"));
	UM_rombuf.interactive = UU_FALSE;
	UM_rombuf.cur_cmd = 0;
	UM_rombuf.max_cmd =  UM_MAX_CMD;
	strcpy( UM_rombuf.cmd[0],"SAVE SESSION backup.tmp");
	UM_rombuf.num_cmd = 1;
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_add_rombuf(str)
**      Add a command string (STR) to the ROMULUS command buffer.
**    PARAMETERS   
**       INPUT  : 
**				str					string to append to command buffer
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_add_rombuf(str)
	char *str;
	{

	uu_denter( UU_MTRC,(us,"um_add_rombuf(%s)",str));
	strcpy( UM_rombuf.cmd[ UM_rombuf.num_cmd],str);
	UM_rombuf.num_cmd++;
	if ( UM_rombuf.num_cmd ==  UM_rombuf.max_cmd)
		{
		um_callromulus();
		um_init_rombuf();
		}
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_romulus(cmd)
**      Add a command (CMD) to the ROMULUS command buffer and call ROMULUS.
**    PARAMETERS   
**       INPUT  : 
**				cmd						command string
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_romulus(cmd)
	char *cmd;
	{

	uu_denter( UU_MTRC,(us,"um_romulus(?)"));
	um_init_rombuf();
	um_add_rombuf(cmd);
	um_callromulus();
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_cre_body_list(option, color, list)
**			Create bodies in UNIBASE and return a LIST of keys to these
**			entities for each ROMULUS (either newest or current) body.
**    PARAMETERS   
**       INPUT  : 
**				option							0=> use current body 
**													1=> use newest body
**				color								color of created bodies
**       OUTPUT :  
**          list								list of keys created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_cre_body_list(option, color, list)
	int option;
	int color;
	UU_LIST *list;

	{
	struct UM_body_rec e;						/* body to be created */
	int opt;											/* option */
	int bodyids[10];								/* ROMULUS ids for new bodies */
	char bodynames[320];							/* ROMULUS names for new bodies */
	int nbodies;									/* number of new bodies */
	int i,j,k;										/* indicies */

	uu_denter( UU_MTRC,(us,"um_cre_body_list(option=%d,color=%d)",option,color));

	ur_setup_data(UM_BODY_REL, &e, sizeof(struct UM_body_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	opt = option;
	unewby(&opt,&nbodies,bodyids,bodynames);
	k = 0;
	for (i=0; i<nbodies; i++)
		{
		for (j=0; j<8; j++,k++)
			{
			if (bodynames[k] == '@') e.name[j] = '\0';
			else e.name[j] = bodynames[k];
			}
		e.color = color;
		e.id = bodyids[i];
		UM_NUM_OF_SOLIDS++;
		uu_dprint( UU_MTRC,(us,"UM_NUM_OF_SOLIDS = %d",UM_NUM_OF_SOLIDS));
		e.body_number = UM_NUM_OF_SOLIDS;
		um_get_by_edge(UM_MAXEDGE, &e.id, &e.no_edge, e.edge);
		um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		ur_update_color(e.key, e.color);
		uu_list_push(list, &e.key);

		/* set the color, and body number in the ROMULUS data base */
		um_rom_appear(e.name, color, UM_NUM_OF_SOLIDS);

		}
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_cre_bodies(option, color)
**			Create a solid body for each of the ROMULUS solid bodies 
**			created by the last operation.
**    PARAMETERS   
**       INPUT  : 
**				option							0=> use current body 
**													1=> use newest body
**				color								newest body => use color
**													current body => use default
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_cre_bodies(option, color)
	int option;
	int color;

	{
	UU_LIST list;
	int bodycolor;
	int i;
	struct UM_body_rec e;
	UU_KEY_ID *keylist;

	uu_denter( UU_MTRC,(us,"um_cre_bodies(option=%d,color=%d)",option,color));

	/* determine the color of the created bodies */
	if (option == NEWEST_BODY)
		bodycolor = color;
	else
		bodycolor = ur_get_attrmdl_color();

	uu_list_init(&list, sizeof(UU_KEY_ID), 10, 10);

	um_cre_body_list(option, bodycolor, &list);

	keylist = (UU_KEY_ID *) UU_LIST_ARRAY(&list);
	for (i=0; i<list.cur_cnt; i++)
		{
		e.key = keylist[i];
		um_get_all_geom(&e, sizeof(e));
		uc_display(&e);
		}

	uu_list_free(&list);

	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_get_by_name(option, id, name)
**			Get the name (NAME) and ROMULUS identifier (ID) of the 
**			current/newest (OPTION) body created in ROMULUS.
**    PARAMETERS   
**       INPUT  : 
**				option							0=>currentbody; 1=>newest
**       OUTPUT :  
**				id                         id of body
**          name								name of the current body
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_get_by_name(option, id, name)
	int option;
	int *id;
	char *name;
	{
	int i;
	int opt;

	uu_denter( UU_MTRC,(us,"um_get_by_name(%d,?,?)",option));
	opt = option;
	unmcby(&opt, id, name);
	for (i=0; i<16; i++)
		if (name[i] == '@') name[i] = '\0';
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_get_by_edge(maxid, id, numid, edgeid)
**			Get up to MAXID edge identifiers (EDGEID) of the specified
**			body (ID) from ROMULUS.
**    PARAMETERS   
**       INPUT  : 
**				maxid				maximum number to retrieve
**          id             identifier of the ROMULUS body
**       OUTPUT :  
**				numid				number of ids retrieved
**				edgeid			edge ids retrieved
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_get_by_edge(maxid, id, numid, edgeid)
	int maxid;
	int *id;
	int *numid;
	int edgeid[];

	{
	int max;

	uu_denter(UU_MTRC,(us,"um_get_by_edge(%d,%d,%x,%x)",maxid,*id,numid,edgeid));
	max = maxid;
	uedcby(&max, id, numid, edgeid);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : oclash(n)
**			Output a message indicating the results of the interference
**			check made as a result of umu_clash.
**    PARAMETERS   
**       INPUT  : 
**          n						1 => interference between bodies
**          						2 => no interference between bodies
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			This is a FORTRAN callable routine.
*********************************************************************/
oclash(n)
	int *n;								/* output option, 1 = yes, 2 = no */
	{

	uu_denter( UU_MTRC,(us,"um_clash()"));
	switch (*n)
		{
		case 1:
			uu_uerror0( UM_MODEL, 137); /* yes, there is interference between these
													two bodies */
			break;
		case 2:
			uu_uerror0( UM_MODEL, 138); /* no interference between these two 
													bodies */
			break;
		}
		uu_dexit;
	}

