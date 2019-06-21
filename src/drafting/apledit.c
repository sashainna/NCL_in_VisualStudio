/*********************************************************************
**    NAME         : apledit.c
**       CONTAINS:
**    			ua_move_pl
**    			ua_select_pl
**    			ua_edit_pl
**    			ua_modify_pl_entry
**					ua_delete_pl_entry
**					ua_add_pl_entry
**    			ua_select_entry
**					ua_update_pl
**					ua_update_all_entries
**					ua_update_selected_entries
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       apledit.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:37
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]=	{
	"@(#) apledit.c 3.2 3/24/89 08:26:19 single"	};
#else
static char uu_sccsident[]=	{
	"@(#) apledit.c 3.2 3/24/89 08:26:19 double"	};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"
#include "class.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mdcoord.h"
#include "mdmatrix.h"
#include "mcrv.h"
#include "modef.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mdeval.h"
#include "mdattr.h"
#include "mattr.h"
#include "mdebug.h"
#include "atext.h"
#include <ctype.h>

/* PARTS LIST table sizes */
extern UU_REAL UA_line_hgt;
extern UU_REAL UA_pl_title;
extern UU_REAL UA_pl_sub_title;
extern UU_REAL UA_pl_char_hgt;
extern UU_REAL UA_pl_title_char_hgt;
extern UU_REAL UA_pl_subtitle_char_hgt;
extern UU_REAL UA_line_wdth[7];
extern int UA_max_no_entries;

extern int UD_editable[UD_NMENTWD];
extern UU_KEY_ID  UA_drafting_view;
extern UU_LOGICAL UA_pl_table_entry_method;
extern char *uu_malloc();

static char    *form_msg = {"Current Table input mode is via a FORM"};
static char    *str_msg = {"Current Table input mode is via a STRING"};

/*********************************************************************
**    E_FUNCTION     : ua_move_pl()
**       User interaction routine for moving a PARTS LIST table
**    PARAMETERS   
**       INPUT  : 
**          none					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_move_pl()
	{
	UU_LIST							pl_list;
	struct UA_generic_draft		pl;
	int								i, j, k, count, d_status, num1, msg, num_entries, index;
	UM_coord							cpln_origin, xaxis, yaxis, zaxis, entity_origin,
										location, y_off_set, x_del, y_del, tmp_vec, del_vec;
	UU_REAL 							scale, hgt, length, char_del;
	struct   UA_txt_rec			*note;
	UU_LOGICAL						ua_select_pl();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_move_pl()"));

	/* check the number of PARTS LIST tables in the data base */
	uu_list_init(&pl_list, sizeof(struct UA_generic_draft), 10, 10);
	note = (struct UA_txt_rec *) uu_malloc(sizeof(struct UA_txt_rec));
	if(!ua_select_pl(&pl_list, &pl, 0))
		{
		uu_uerror0(UA_DRAFTING, 44);
		goto fexit;
		}
	else
		{
	
		/* get the new lower left corner from the user */
		d_status = ud_world_coord(13, 146, location, 1, &num1, UU_FALSE);
		if( d_status==0 || d_status==2 ) goto fexit;
		if(  num1 == 0 ) goto fexit;
	
		/* project new location onto the plane of the table */
		ua_getcpln(&pl, cpln_origin, xaxis, yaxis, zaxis);
		um_nptpln(location, cpln_origin, zaxis, tmp_vec);
	
		/* update the table position */
		um_vcmnvc(tmp_vec, pl.dim_origin, del_vec);
		um_vctovc(tmp_vec, pl.dim_origin);
		
		/* update line block positions */
		if(pl.line_blk_use > 0)
			{
			for(i=0;i<pl.line_blk_use;i++)
				{
				for(j=0;j<pl.line_blk[i].num_pts;j++)
					um_vcplvc(pl.line_blk[i].line_seg[j],
							del_vec, pl.line_blk[i].line_seg[j]);
				}
			}
		
		/* update table text positions */
		if(pl.asso_blk_use > 0)
			{
			for(i=0;i<pl.asso_blk_use;i++)
				{
				note->key = pl.asso_blk[i].key;
				uc_retrieve_data(note, sizeof(struct UA_txt_rec	)) ;
				um_vcplvc(note->position, del_vec, note->position);
				d_status = um_update_geom(note, UM_DEFAULT_TF);
				}
			}
	
		/* update table record and re-display */
		ua_regen_update_record(&pl);
		}

fexit:
	uu_list_free(&pl_list);
	uu_free(note);
	uu_dexit;
	}
		
/*********************************************************************
**    E_FUNCTION     : ua_select_pl(pl_list, pl_tbl, mode)
**       User interface routine to select a Parts List Table.
**    PARAMETERS   
**       INPUT  : 
**				mode							select msg mode
**       OUTPUT  : 
**          pl_list						list of active tables
**				pl_tbl						Parts List Table selected by the user
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ua_select_pl(pl_list, pl_tbl, mode)

	UU_LIST							*pl_list;
	struct UA_generic_draft		*pl_tbl;
	int								mode;
	{
	struct UA_PLOCREC				plocrec;
	struct UA_PICKENT				pickent;
	UU_KEY_ID 						curr_key, key, view_key;
	int								i, j, k, count, d_status, num1, msg, num_entries,
										relation;
	UU_LOGICAL						init, status;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_select_pl()"));

	/* check the number of PARTS LIST tables in the data base */
	init = UU_TRUE;
	relation = 48;
	count = 0;
	status = UU_FALSE;

	/* loop over displayed drafting entities looking for PARTS LIST tables */
	while(uv_getalldispinrelation(init, relation, &key) == UU_SUCCESS)
		{
		pl_tbl->key = key;
		pl_tbl->rel_num = relation;
		if(ur_retrieve_view_key(key, &view_key) == UU_SUCCESS)
			{
			if(view_key == UA_drafting_view)
				{
				status = uc_retrieve_data(pl_tbl, sizeof(struct UA_generic_draft));
				if( status == UU_SUCCESS)
					if(pl_tbl->etype == UA_PL_DIM)
					{
					uu_list_push(pl_list, pl_tbl);
					count++;
					}
				}
			}
		init = UU_FALSE;
		}

	switch(count)
		{
		case 0:
			status = UU_FALSE;
			break;
		case 1:
			uu_list_pop(pl_list, pl_tbl);
			status = UU_TRUE;
			break;
		default:
			/* get user selection of the PARTS LIST table */
			switch(mode)
				{
				case 0:
					msg = 145;		/* move table */
					break;
				case 1:				/* add entry */
					msg = 154;
					break;
				case 2:				/* modify entry */
					msg = 147;
					break;
				case 3:				/* delete entry */
					msg = 152;
					break;
				case 4:				/* update table */
					msg = 162;	
					break;
				}
			ud_lgeo(UU_TRUE, UD_editable);
			d_status = ud_pick_loc(UA_DRAFTING, msg, &plocrec, 1, &num1, UU_FALSE);
			if(d_status == UA_REJECT || num1 < 1) goto fexit;
			d_status = um_d_pickresolve(&plocrec.ppick, 1, &pickent);
			curr_key = um_get_pickkey(&pickent, 1);
		
			/* check that the users actually selected a PARTS LIST table */
			for(i=0;i<count;i++)
				{
				uu_list_pop(pl_list, pl_tbl);
				if(curr_key == pl_tbl->key)
					{
					status = UU_TRUE;
					break;
					}
				}
			break;
		}

fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     :  ua_edit_pl()
**      Get USERS choice for edit options.
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_edit_pl()
	{
	UU_LIST							pl_list;
	struct UA_generic_draft		pl;
	UU_KEY_ID 						curr_key, save_key, title_keys[10], key, view_key;
	int								i, j, k, d_status, num1, msg, num_entries,
										relation, num_chars, retstat, mode, d_stat,
										count, status;
	UU_LOGICAL						ua_select_pl();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_STRC,(us,"ua_edit_pl"));

	uu_list_init(&pl_list, sizeof(struct UA_generic_draft), 10, 10);
start:;

	/* get users choice for the edit operation to be performed */
	d_stat = ua_popmenu(25, &mode);
	if(  d_stat == 2 || (mode > 3 || d_stat == 0)
							|| (d_stat == 1 && mode == 0)  ) goto fexit;

	/* check the number of PARTS LIST tables in the data base */
	if(!ua_select_pl(&pl_list, &pl, mode))
		{
		uu_uerror0(UA_DRAFTING, 44);
		goto start;
		}
	else
		{

		/* now execute appropriate routine */
		switch( mode )
			{
			case 1:
				ua_add_pl_entry(&pl);
				break;
			case 2:
				ua_modify_pl_entry(&pl);
				break;
			case 3:
				ua_delete_pl_entry(&pl);
				break;
			}
		}
	goto start;

fexit:;
	uu_list_free(&pl_list);
	uu_dexit;
	}
		
/*********************************************************************
**    E_FUNCTION     : ua_modify_pl_entry(pl)
**       User interaction routine for modification of a PARTS LIST table
**    PARAMETERS   
**       INPUT  : 
**          pl								PARTS LIST table record					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_modify_pl_entry(pl)

struct UA_generic_draft	*pl;

	{
	UU_KEY_ID 				curr_key;
	int						i, j, k, count, d_status, num1, msg, num_entries, len,
								relation, num_chars, retstat, indx, find_t, find_no[1000];
	char 						txtstr[255], b_field[7][100], a_field[7][100];
	struct   UA_txt_rec	note;
	UU_LOGICAL           changed, status, ua_get_pl_text_form();
	UU_TRUEDOUBLE        ff, uai_atof();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_STRC,(us,"ua_modify_pl()"));

	/* inform use of current format */
	if(UA_pl_table_entry_method)
		ud_prmerr(form_msg);
	else
		ud_prmerr(str_msg);

loop:;
	/* loop over users selection of table entries for modification */
	if(ua_select_entry(pl, 148, &curr_key))
		{
		indx = 0;
		for(i=0;i<pl->asso_blk_use;i++)
			{
			note.key = pl->asso_blk[i].key;
			if(note.key != curr_key && pl->asso_blk[i].modifier != -99)
				{
				uc_retrieve_data(&note, sizeof(struct UA_txt_rec	)) ;
				strcpy(txtstr, note.tchar);
				num_chars = strlen(txtstr);
				ua_strip_pl_entry(txtstr, &num_chars, b_field);
				ff = uai_atof(b_field[0]);
				find_no[indx] = ff;
				indx++;
				}
			}
		note.key = curr_key;
		for(i=0;i<pl->asso_blk_use;i++)
			{
			if(note.key == pl->asso_blk[i].key && pl->asso_blk[i].modifier != -99)
				{
				/* YES - retrieve the UNIBASE record and present to user
					for modification */
				uc_retrieve_data(&note, sizeof(struct UA_txt_rec	)) ;
repeat:;
				strcpy(txtstr, note.tchar);
				num_chars = strlen(txtstr);
				ua_strip_pl_entry(txtstr, &num_chars, b_field);
				if(UA_pl_table_entry_method)
					{
					status = ua_get_pl_text_form("apltxt.frm", txtstr, &num_chars,
																UU_TRUE);
					if(!status) goto fexit;
					}
				else
					{
					d_status = ud_string_def(UA_DRAFTING, 125, txtstr, 255,
													&num_chars, &retstat);
					if( d_status != UA_OPCOMPLETE || num_chars <= 0 ) goto fexit;
					}
	
				/* check if any modifications made */
				ua_strip_pl_entry(txtstr, &num_chars, a_field);
				changed = UU_FALSE;
				for(i=0;i<7;i++)
					{
					if(strcmp(b_field[i], a_field[i]) != 0)
						{
						changed = UU_TRUE;
						break;
						}
					}
				if(changed)
					{
					len = strlen(a_field[0]);
					if(len > 3)
						{
						uu_uerror0(UA_DRAFTING,45);
						goto repeat;
						}
					for(i=0;i<len;i++)
						{
						if(isdigit(a_field[0][i]) == 0)
							{
							uu_uerror0(UA_DRAFTING,52);
							goto repeat;
							}
						}
					/* check find number */
					ff = uai_atof(a_field[0]);
					find_t = ff;
					for(j=0;j<indx;j++)
						{
						if(find_t == find_no[j])
							{
							uu_uerror0(UA_DRAFTING,50);
							goto repeat;
							}
						}

					/* check quantity field */
					len = strlen(a_field[5]);
					if(len > 0)
						{
						if(len > 3)
							{
							uu_uerror0(UA_DRAFTING,57);
							goto repeat;
							}
						if(strcmp(a_field[5], "AR") != 0)
							{
							for(i=0;i<len;i++)
								{
								if(isdigit(a_field[5][i]) == 0)
									{
									uu_uerror0(UA_DRAFTING,58);
									goto repeat;
									}
								}
							}
						}

					/* store user modifications */
					note.no_tchar = num_chars;
					strcpy(note.tchar, txtstr);
			 		d_status = um_update_geom(&note, UM_DEFAULT_TF);
			
					/* update the PARTS LIST table record and re-display */
					ua_regen_update_record(pl);
					}
				goto loop;
				}
			}
		goto loop;
		}

fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_delete_pl_entry(pl)
**       User interaction routine to delete an entry in a PARTS LIST table
**    PARAMETERS   
**       INPUT  : 
**          pl							PARTS LIST table record					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_delete_pl_entry(pl)

struct UA_generic_draft	*pl;

	{
	struct UA_PLOCREC		plocrec;
	struct UA_PICKENT		pickent;
	UU_KEY_ID 				curr_key, title_keys[10];
	int						i, j, k, count, d_status, num1, msg, num_entries,
								relation, num_chars, retstat;
	struct   UA_txt_rec	note;
	UM_coord 				cpln_origin, xaxis, yaxis, zaxis, location, y_off_set,
								x_del, y_del;
	char 						txtstr[255];
	UU_REAL 					scale, hgt, length, char_del;
	UU_LOGICAL				ua_select_entry();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_delete_pl_entry()"));


	/* table selection ok! get entry selection from the user */
	if(ua_select_entry(pl, 153, &curr_key))
		{
		/* find entry in the TABLE */
		for(k=0;k<pl->asso_blk_use;k++)
			{
			if(curr_key == pl->asso_blk[k].key && pl->asso_blk[k].modifier != -99)
				{
				/* delete the UNIBASE record for the table entry */
				ua_delete_text(curr_key);
	
				/* update the PARTS LIST table association list */
				num_entries = pl->asso_blk_use - 8;
				scale = pl->asso_blk[num_entries + 1].location[0];
	
				/* save old title entities */
				count = 0;
				for(j=0;j<pl->asso_blk_use;j++)
					{
					if(pl->asso_blk[j].modifier == -99)
						{
						title_keys[count] = pl->asso_blk[j].key;
						count++;
						}
					}
	
				/* shuffle list of entries */
				for(j=k;j<num_entries;j++)
					pl->asso_blk[j].key = pl->asso_blk[j+1].key;
	
				num_entries--;
				pl->line_blk_use = 0;
	
				/* re-generate the new table outline */
				ua_getcpln(pl, cpln_origin, xaxis, yaxis, zaxis);
				ua_pl_tbl_box(pl, num_entries, cpln_origin, xaxis, yaxis,
									zaxis, scale);
			
				/* loop on number of entries - updating position */
				hgt = UA_line_hgt /scale;
				char_del = 0.8*((hgt - UA_pl_char_hgt/scale)/2.0);
				um_vctmsc(xaxis, char_del, x_del);
				um_vctmsc(yaxis, char_del, y_del);
				char_del = (UA_pl_title + UA_pl_sub_title)/scale;
	
				for(i=0;i<num_entries;i++)
					{
					length = i*hgt + char_del;
					um_vctmsc(yaxis, length, y_off_set);
					um_vcplvc(pl->dim_origin, y_off_set, location);
					um_vcplvc(location, x_del, location);
					um_vcplvc(location, y_del, location);
					note.key = pl->asso_blk[i].key;
					ua_get_text(&note, sizeof(struct UA_txt_rec));
					um_vctovc(location, note.position);
					d_status = um_update_geom(&note, UM_DEFAULT_TF);
					}
	
				/* add title keys to the list */
				pl->asso_blk_use = num_entries + 8;
				for(i=0;i<8;i++)
					{
					pl->asso_blk[num_entries+i].key = title_keys[i];
					pl->asso_blk[num_entries+i].modifier = -99;
					pl->asso_blk[num_entries+i].location[0] = scale;
					}
	
				/* update PARTS LIST table record and re-display */
				ua_regen_update_record(pl);
				goto fexit;
				}
			}
		}

fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_add_pl_entry(pl)
**       User interaction routine to add an entry to a PARTS LIST table
**    PARAMETERS   
**       INPUT  : 
**          pl							PARTS LIST table record					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_add_pl_entry(pl)

struct UA_generic_draft	*pl;

	{
	UU_KEY_ID 				curr_key, save_key, title_keys[10];
	int						i, j, k, count, d_status, num1, msg, num_entries,
								relation, num_chars, retstat, asso_indx, indx
								,find_no[1000], find_t, find_no_index;
	struct   UA_txt_rec	note;
	UM_coord 				cpln_origin, xaxis, yaxis, zaxis, location, y_off_set,
								x_del, y_del;
	char 						txtstr[255], b_field[7][100];
	UU_REAL 					scale, hgt, length, char_del;
	UU_LOGICAL				ua_select_entry();
	UU_TRUEDOUBLE        ff, uai_atof();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_STRC,(us,"ua_add_pl_entry()"));

	/* inform use of current format */
	if(UA_pl_table_entry_method)
		ud_prmerr(form_msg);
	else
		ud_prmerr(str_msg);


loop:;
	/* table selection ok! get entry selection from the user */
	if(ua_select_entry(pl, 155, &curr_key))
		{
		indx = 0;
		for(i=0;i<pl->asso_blk_use;i++)
			{
			note.key = pl->asso_blk[i].key;
			if(pl->asso_blk[i].modifier != -99)
				{
				uc_retrieve_data(&note, sizeof(struct UA_txt_rec	)) ;
				strcpy(txtstr, note.tchar);
				num_chars = strlen(txtstr);
				ua_strip_pl_entry(txtstr, &num_chars, b_field);
				ff = uai_atof(b_field[0]);
				find_no[indx] = ff;
				indx++;
				}
			}
		/* find entry in the TABLE */
		for(k=0;k<pl->asso_blk_use;k++)
			{
			if(curr_key == pl->asso_blk[k].key && pl->asso_blk[k].modifier != -99)
				{
	
				/* update the PARTS LIST table association list */
				num_entries = pl->asso_blk_use - 8;
				scale = pl->asso_blk[num_entries + 1].location[0];
				asso_indx = k + 1;
	
				/* save old title entities */
				count = 0;
				for(j=0;j<pl->asso_blk_use;j++)
					{
					if(pl->asso_blk[j].modifier == -99)
						{
						title_keys[count] = pl->asso_blk[j].key;
						count++;
						}
					}
		
				/* shuffle list of entries */
				if(asso_indx <  num_entries)
					{
					for(j=(num_entries-1);j>=asso_indx;j--)
						{
						pl->asso_blk[j+1].key = pl->asso_blk[j].key;
						pl->asso_blk[j+1].modifier = 0;
						}
					}
	
				num_entries++;
				pl->line_blk_use = 0;
	
				/* re-generate the new table outline */
				ua_getcpln(pl, cpln_origin, xaxis, yaxis, zaxis);
				ua_pl_tbl_box(pl, num_entries, cpln_origin, xaxis, yaxis,
									zaxis, scale);
	
				/* loop on number of entries - updating position */
				hgt = UA_line_hgt /scale;
				char_del = 0.8*((hgt - UA_pl_char_hgt/scale)/2.0);
				um_vctmsc(xaxis, char_del, x_del);
				um_vctmsc(yaxis, char_del, y_del);
				char_del = (UA_pl_title + UA_pl_sub_title)/scale;
	
				for(i=0;i<num_entries;i++)
					{
					length = i*hgt + char_del;
					um_vctmsc(yaxis, length, y_off_set);
					um_vcplvc(pl->dim_origin, y_off_set, location);
					um_vcplvc(location, x_del, location);
					um_vcplvc(location, y_del, location);
					if(i == asso_indx)
						{
						/* get the text for current entry and create a note entity */

						ua_pl_tbl_entry(pl, i, location, scale, &indx,
																find_no, &curr_key);
						if(curr_key == -1) goto fexit;

						pl->asso_blk[i].key = curr_key;
						pl->asso_blk[i].modifier = 0;
						}
					else
						{
						note.key = pl->asso_blk[i].key;
						ua_get_text(&note, sizeof(struct UA_txt_rec));
						um_vctovc(location, note.position);
						d_status = um_update_geom(&note, UM_DEFAULT_TF);
						}
					}
	
			/* add title keys to the list */
				pl->asso_blk_use = num_entries + 8;
				for(i=0;i<8;i++)
					{
					pl->asso_blk[num_entries+i].key = title_keys[i];
					pl->asso_blk[num_entries+i].modifier = -99;
					pl->asso_blk[num_entries+i].location[0] = scale;
					}
	
				/* update PARTS LIST table record and re-display */
				ua_regen_update_record(pl);
				break;
				}
			}
		}

fexit:
	uu_dexit;
	}
		
/*********************************************************************
**    E_FUNCTION     : ua_select_entry(pl, mode, select_key)
**       User interface routine to select a Parts List Table Entry
**    PARAMETERS   
**       INPUT  : 
**				pl								parts list table
**				mode							select msg mode
**       OUTPUT  : 
**          select_key					Unibase key ID of selected entry
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ua_select_entry(pl, mode, select_key)

	struct UA_generic_draft		*pl;
	int								mode;
	UU_KEY_ID						*select_key;
	{
	struct UA_PLOCREC				plocrec;
	struct UA_PICKENT				pickent;
	UU_KEY_ID 						curr_key, key, view_key;
	int								i, j, k, count, d_status, num1, msg, num_entries,
										relation;
	UU_LOGICAL						init, status;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_select_entry()"));

	status = UU_FALSE;
loop:;
	ud_lgeo(UU_TRUE, UD_editable);
	d_status = ud_pick_loc(UA_DRAFTING, mode, &plocrec, 1, &num1, UU_FALSE);
	if(d_status == UU_TRUE && num1 < 1) goto fexit;

	/* switch on user response */
	switch(d_status)
		{
		case UU_FALSE:
			goto fexit;
			break;
		case UU_ALTACTION:
			UA_pl_table_entry_method = !UA_pl_table_entry_method;
			if(UA_pl_table_entry_method)
				ud_prmerr(form_msg);
			else
				ud_prmerr(str_msg);
			goto loop;
			break;
		default:
			d_status = um_d_pickresolve(&plocrec.ppick, 2, &pickent);
			if(pickent.key[0] != pl->key)
				{
				uu_uerror0(UA_DRAFTING,55);
				goto fexit;
				}
			if(pickent.key[0] != pickent.key[1])
				{
				curr_key = um_get_pickkey(&pickent, 2);
				d_status = um_retrieve_data_relnum(curr_key, &relation);
				if( relation != UA_TEXT_REL )
					uu_uerror0(UA_DRAFTING,54);
				else
					status = UU_TRUE;
				}
			else
				{
				ua_closest_pl_entry(&plocrec.ndcloc, pl, &curr_key);
				if(curr_key == 0)
					uu_uerror0(UA_DRAFTING,54);
				else
					status = UU_TRUE;
				}
			*select_key = curr_key;
			break;
		}
fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int  ua_closest_pl_entry (ploc, pl, curr_key)
**       Find PARTS LIST table entry closest to PLOC
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_closest_pl_entry (plocrec, pl, curr_key)
	struct UA_NDCLOCREC		*plocrec;
	struct UA_generic_draft *pl;
	UU_KEY_ID					*curr_key;
	{
	int							i, jj;
	UU_REAL 						dist, dist_ln, um_dcccc();
	UM_coord						lpt,  ploc_pt;
	struct   UA_txt_rec	note;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_closest_pl_entry()"));

	um_ploctocc(plocrec, ploc_pt);

	jj = 0;
	dist = 100000.;
	*curr_key = 0;
	for(i=0;i<pl->asso_blk_use;i++)
		{
		if(pl->asso_blk[i].modifier != -99)
			{
			note.key = pl->asso_blk[i].key;
			ua_get_text(&note, sizeof(struct UA_txt_rec));
			um_vctovc(note.position, lpt);
			dist_ln = um_dcccc(ploc_pt, lpt);
			if(dist_ln < dist)
				{
				dist = dist_ln;
				jj = i;
				}
			}
		}
	*curr_key = pl->asso_blk[jj].key;
fexit:;
   uu_dexit;
	return(jj);
	}
		
/*********************************************************************
**    E_FUNCTION     :  ua_update_pl()
**      Update a parts_list quantity count.
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_update_pl()
	{
	UU_LIST							pl_list;
	struct UA_generic_draft		pl;
	UU_KEY_ID 						curr_key, save_key, title_keys[10], key, view_key;
	int								i, j, k, d_status, num1, msg, num_entries,
										relation, num_chars, retstat, mode, d_stat,
										count, status;
	UU_LOGICAL						ua_select_pl();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_update_pl"));

start:;
	d_stat = ua_popmenu(27, &mode);
	if(  d_stat == 2 || (mode > 2 || d_stat == 0)
							|| (d_stat == 1 && mode == 0)  ) goto fexit;

	/* check the number of PARTS LIST tables in the data base */
	uu_list_init(&pl_list, sizeof(struct UA_generic_draft), 10, 10);
	if(!ua_select_pl(&pl_list, &pl, 4))
		{
		uu_uerror0(UA_DRAFTING, 44);
		goto start;
		}
	else
		{
		/* now execute appropriate routine */
		switch( mode )
			{
			case 1:
				ua_update_all_entries(&pl);
				break;
			case 2:
				ua_update_selected_entries(&pl);
				break;
			}
		}

fexit:;
	uu_list_free(&pl_list);
	uu_dexit;
	}
		
/*********************************************************************
**    E_FUNCTION     : ua_update_all_entries(pl)
**       Update the quantity entry for all entries in table pl.
**    PARAMETERS   
**       INPUT  : 
**          pl								PARTS LIST table record					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_update_all_entries(pl)

struct UA_generic_draft	*pl;

	{
	UU_KEY_ID 				curr_key;
	int						i, j, k, count, d_status, num1, msg, num_entries,
								relation, num_chars, retstat;
	char 						field_str[7][100], txtstr[1024];
	struct   UA_txt_rec	note;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_update_all_entries()"));

	/* loop over entries in the parts list table */
	uu_dprint(UU_MTRC,(us,"update_all asso_blks = %d", pl->asso_blk_use));
	for(i=0;i<pl->asso_blk_use;i++)
		{
	uu_dprint(UU_MTRC,(us,"i = %d modifier = %d",i, pl->asso_blk[i].modifier));
		if(pl->asso_blk[i].modifier != -99)
			{
			note.key = pl->asso_blk[i].key;
			uc_retrieve_data(&note, sizeof(struct UA_txt_rec	)) ;
			strcpy(txtstr, note.tchar);
			num_chars = note.no_tchar;
			ua_strip_pl_entry(txtstr, &num_chars, field_str);
			ua_update_pl_count_field(field_str, txtstr, &num_chars);
			strcpy(note.tchar, txtstr);
			note.no_tchar = num_chars;
	 		d_status = um_update_geom(&note, UM_DEFAULT_TF);
			}
		}

	/* update the PARTS LIST table record and re-display */
	ua_regen_update_record(pl);

fexit:
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_update_selected_entries(pl)
**       User interaction routine for modification of a PARTS LIST table
**    PARAMETERS   
**       INPUT  : 
**          pl								PARTS LIST table record					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_update_selected_entries(pl)

struct UA_generic_draft	*pl;

	{
	struct UA_PLOCREC		plocrec;
	struct UA_PICKENT		pickent;
	int						i, j, k, count, status, num1, msg, num_entries,
								relation, num_chars, retstat, d_status, loop;
	char 						field_str[7][100], txtstr[255];
	struct   UA_txt_rec	note;
	UU_KEY_ID            curr_key, key_list[50];
	UU_LOGICAL           start, ua_select_entry();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_update_selected_entries()"));

	/* get list of entries */
	ud_lgeo(UU_TRUE, UD_editable);
	loop = UA_OPCOMPLETE;
	count = 0;
	while(loop != UA_REJECT)
		{
		if(ua_select_entry(pl, 163, &curr_key))
			{
			key_list[count] = curr_key;
			count++;
			}
		else
			loop = UA_REJECT;
		}

	/* check if any good table entries selected - update QUANTITY fields */
	if(count > 0)
		{
		for(i=0; i<count; i++)
			{
			note.key = key_list[i];
			if(ua_get_text(&note, sizeof(struct UA_txt_rec)) == UU_SUCCESS)
				{
				num_chars = note.no_tchar;
				strcpy(txtstr, note.tchar);
				ua_strip_pl_entry(txtstr, &num_chars, field_str);
				ua_update_pl_count_field(field_str, txtstr, &num_chars);
				strcpy(note.tchar, txtstr);
				note.no_tchar = num_chars;
	
				status = um_update_geom(&note, UM_DEFAULT_TF);
				}
			}


		/* update the PARTS LIST table record and re-display */
		ua_regen_update_record(pl);
		}

fexit:
	uu_dexit;
	}
