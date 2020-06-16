/*********************************************************************
**    NAME         :  tigmaps.c
**       CONTAINS:
**					uig_map_instance
**					uig_map_master
**					uig_map_symnote
**                  uig_sym_name(target,source)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigmaps.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:48
*********************************************************************/

#include <ctype.h>
#include "tiges.h"
#include "tigdefs.h"
#include "mcrv.h"
#include "mdattr.h"
#include "atext.h"
#include "tigsupp.h"
#include "mdrel.h"
#include "udebug.h"
#include "usysdef.h"
#include "mxxx.h"
#include "bsym.h"

extern UU_REAL instance_tf[4][3];
char *uu_toolmalloc();

/*********************************************************************
**    I_FUNCTION     :  uig_map_instance(key, no_ptr_geom, ptr_geom, 
**   							no_ptr_notes, ptr_notes, dblk, key)
**				Complete generation of symbol entity
**    PARAMETERS   
**       INPUT  : 
**				name							symbol name
**				no_ptr_geom					number of KEY_ID's in ptr
**				ptr_geom						pointer array
**				no_ptr_notes				number of KEY_ID's in ptr
**				ptr_notes					pointer array
**          dblk							directory blk for the group
**       OUTPUT :  
**				key							UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_instance(master_key, no_ptr_geom, inst_ptr_geom, 
								no_ptr_notes, inst_ptr_notes, dblk, key)
	UU_KEY_ID master_key;
	int       no_ptr_geom;
	UU_KEY_ID inst_ptr_geom[100];
	int       no_ptr_notes;
	UU_KEY_ID inst_ptr_notes[20];
	struct dir_rec *dblk;				/* IGES instance directory record */
	UU_KEY_ID *key;
	{

	struct UB_symbol_rec   master;
	struct UB_instance_rec   instance;
	struct UB_symattr_rec   attr;
	struct UB_inst_rec *inst_ptr;
	int i, status, no_instances;
	UU_REAL tt[4][3];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_map_instance"));

	/* setup unibase storage area */

	ur_setup_data(UB_INSTANCE_REL,&instance,sizeof(struct UB_instance_rec));

	/* malloc space for data */

	uu_dprint(UU_MTRC,(us,"no_ptr_geom %d, no_ptr_notes %d", no_ptr_geom, no_ptr_notes));
	if(no_ptr_geom > 0)
			instance.geom=(UU_KEY_ID *)uu_toolmalloc
										(no_ptr_geom  * sizeof( UU_KEY_ID));
	if(no_ptr_notes > 0)
			instance.text_nod=(struct UB_text_nod_rec *)uu_toolmalloc
										(no_ptr_notes * sizeof(struct UB_text_nod_rec));

	/* get master */
	master.key = master_key;
	ur_retrieve_data_relnum(master.key, &master.rel_num);
	uio_getdata(&master, &attr);

	/* create instance */
	instance.no_geom = no_ptr_geom;
	for(i=0;i<no_ptr_geom;i++)
		{
		instance.geom[i] = inst_ptr_geom[i];
		}
	instance.no_text_nod = no_ptr_notes;
	for(i=0;i<no_ptr_notes;i++)
		{
		instance.text_nod[i].prompt =  2;
		instance.text_nod[i].visibility =  0;
		instance.text_nod[i].masterindx =  i;
		instance.text_nod[i].text_key =  inst_ptr_notes[i];
		}
/*
.....Label the instance
*/
	sprintf(instance.label,"%s_%d",master.label,master.no_inst+1);
	instance.subscr = master.subscr;
/*
.....Store the instance
*/	
	instance.no_snap_nod = 0;
	DDC_displayable_flag = UM_DISPLAYABLE;
	uig_update_sym_attr(dblk, &attr);
	uig_create_symbol(&instance,0,&attr, dblk->view_ptr);
	UIG_unibase_entities++;

	/* now update master with instance data */

	no_instances = master.no_inst;
	uu_dprint(UU_MTRC,(us,"no_instances = %d size = %d", no_instances,
	sizeof(struct UB_inst_rec)));
	inst_ptr=(struct UB_inst_rec *)uu_toolmalloc((no_instances + 1)*
														sizeof(struct UB_inst_rec));
	master.no_inst = no_instances + 1;
	for(i=0;i<no_instances;i++)
		{
		inst_ptr[i].inst_key = master.inst[i].inst_key;
		um_tftotf(master.inst[i].tfmat, inst_ptr[i].tfmat);
		}	
	inst_ptr[no_instances].inst_key = instance.key;
	um_tftotf(instance_tf, tt);
	um_vctmsc(tt[3],unit_scale,tt[3]);	
	um_tftotf(tt, inst_ptr[no_instances].tfmat);
	master.inst = inst_ptr;
	status = ur_update_data(&master);
	*key = instance.key;
	uu_toolfree(inst_ptr);
	if(no_ptr_geom > 0)
		uu_toolfree(instance.geom);
	if(no_ptr_notes > 0)
		uu_toolfree(instance.text_nod);

	uu_dexit;
	return 0;
	}
/*********************************************************************
**    I_FUNCTION     :  uig_map_master(name, no_ptr_geom, ptr_geom, 
**   							no_ptr_notes, ptr_notes, dblk, key)
**				Complete generation of symbol entity
**    PARAMETERS   
**       INPUT  : 
**				name							symbol name
**				no_ptr_geom					number of KEY_ID's in ptr
**				ptr_geom						pointer array
**				no_ptr_notes				number of KEY_ID's in ptr
**				ptr_notes					pointer array
**          dblk							directory blk for the group
**       OUTPUT :  
**				key							UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_master(name, no_ptr_geom, master_ptr_geom,
					no_ptr_notes, master_ptr_notes, dblk, key)
	char 		name[80];
	int       no_ptr_geom;
	UU_KEY_ID master_ptr_geom[100];
	int       no_ptr_notes;
	UU_KEY_ID master_ptr_notes[20];
	struct dir_rec *dblk;				/* IGES master directory record */
	UU_KEY_ID *key;
	{

	struct UB_symbol_rec   master;
	struct UB_symattr_rec   attr;
	int i;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_map_master"));

	/* setup unibase storage area */

	ur_setup_data(UB_SYMBOL_REL,&master,sizeof(struct UB_symbol_rec));

	/* malloc space for data */

	master.inst=(struct UB_inst_rec *)uu_toolmalloc(sizeof(struct UB_inst_rec));
	if(no_ptr_geom)
		master.geom=(UU_KEY_ID *)uu_toolmalloc(no_ptr_geom  * sizeof( UU_KEY_ID));
	if(no_ptr_notes)
		master.text_nod=(struct UB_text_nod_rec *)uu_toolmalloc(no_ptr_notes * 
									sizeof(struct UB_text_nod_rec));

	/* first create symbol master */

/*	name[11] = '\0'; */
	uig_sym_name(master.label, name);
	master.subscr = 0;
	master.version = UB_NOLIB;
	master.path[0] = '\0';
	master.no_masters = 0;
	master.no_inst = 0;
	master.no_geom = no_ptr_geom;
	for(i=0;i<no_ptr_geom;i++)
		{
		master.geom[i] = master_ptr_geom[i];
		}
	master.no_text_nod = no_ptr_notes;
	for(i=0;i<no_ptr_notes;i++)
		{
		master.text_nod[i].prompt =  2;
		master.text_nod[i].visibility =  0;
		master.text_nod[i].masterindx =  i;
		master.text_nod[i].text_key =  master_ptr_notes[i];
		}
	master.no_snap_nod = 0;
	DDC_displayable_flag = UM_NEVERDISPLAYABLE;

	uig_update_sym_attr(dblk, &attr);
	uig_create_symbol(&master,0,&attr, dblk->view_ptr);

	DDC_displayable_flag = UM_DISPLAYABLE;
	uig_update_sym_attr(dblk, &attr);
	*key = master.key;
	uu_toolfree(master.inst);
	if(no_ptr_geom)
		uu_toolfree(master.geom);
	if(no_ptr_notes)
		uu_toolfree(master.text_nod);

	uu_dexit;
	return 0 ;
	}
/*********************************************************************
**    I_FUNCTION     :  uig_map_symnote(dblk,t,gnote,key)
**				Map a IGES general note to a Unibase model note.
**    PARAMETERS   
**       INPUT  : 
**				dblk							directory block
**				t								IGES transformation matrix
**				gnote							note parameter block
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_symnote(dblk,t,g,key)
	struct dir_rec *dblk;				/*  directory record */
	UU_REAL t[12];
	struct IG_igesnote_rec *g;			/* parameter record */
	UU_KEY_ID *key;
	{

	struct UA_txt_rec e;
	struct UA_txtattr_rec attr;

	int start;
	int count;
	struct IG_gnote_rec *p;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_map_symnote"));

	ur_setup_data(UA_TEXT_REL,&e,sizeof(struct UA_txt_rec));

	/*********************
	**                  **
	** load text 		  **
	**                  **
	*********************/

	p = g->gnote;

	e.no_tchar = p->str_cnt;
	count = strlen(&p->str[0]);
	start = count - p->str_cnt;
	strcpy(&e.tchar[0],&p->str[start]);
	uig_tran_coor(&p->xyzt[0],t,&e.position[0]);
	um_vctmsc(&e.position[0],unit_scale,&e.position[0]);
	e.dx = unit_scale * p->wt;
	e.dy = unit_scale * p->ht;
/*
.....vp 7-jan-97 fix angle of text and added slant to attribute
*/
	e.tangle = -p->ang;

	/* create Unibase record */

	uig_update_text_attr(dblk, t, &attr);
	attr.slant = p->sl-1.570796;
	uig_create_text(&e,0,&attr, dblk->view_ptr);
	*key = e.key;

	uu_dexit;
	return 0 ; 
	}
/*********************************************************************
**    I_FUNCTION     :  uig_sym_name(target,source)
**		Strip Hollerith information from symbol name.
**      Also remove blanks, lower case & convert spec chars to '_'.
**		Also check if the name has been used, 
**			if yes then name it DUPSYM1,DUPSYM2....
**		
**    PARAMETERS   
**       INPUT  : 
**				source					   source string
**       OUTPUT :  
**				target					   target string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_sym_name(target,source)
char target[11], source[11];
{

	int status,i,j,len,nxtuple;
	struct UB_symbol_rec symptr;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	len = strlen(source);
	for(i=0;i<len;i++) if(source[i] == 'H') break;
	i++;
	for (j=0; j<10 && source[i] != '\0'; i++)
		{
		if (source[i] != ' ')
			{
			if (isalpha(source[i]))
				{
				if (isupper(source[i])) target[j] = tolower(source[i]);
				else target[j] = source[i];
				}
			else if (isdigit(source[i]))
				{
				target[j] = source[i];
				}
			else
				{
				target[j] = '_';
				}
			j++;
			}
		}
	target[j] = '\0';
/*
.....Check if the name has been used, 
.....		if yes then rename it to DUPSYM1,DUPSYM2...
*/
	nxtuple = 1;
	while(ur_get_next_data_key(UB_SYMBOL_REL, &nxtuple, &(symptr.key))>-1)
	{
		status =ur_retrieve_data_fixed(&symptr);
		if(strcmp(target, symptr.label) == 0)
		{
			target[0] = '\0';
			dupsym++;
			sprintf(target, "%s%d", "DUPSYM",dupsym);
			break;
		}
		nxtuple++;
	}

return(0);
}
