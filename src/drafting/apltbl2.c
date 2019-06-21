/*********************************************************************
**    NAME         : apltbl2.c
**       CONTAINS:
**    			ua_merge_pl_tables
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       apltbl2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:37
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]=	{
	"@(#) apltbl2.c 3.1 3/24/89 17:15:11 single"	};
#else
static char uu_sccsident[]=	{
	"@(#) apltbl2.c 3.1 3/24/89 17:15:11 double"	};
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
#include "uhep.h"
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
#include "msol.h"
#include "mxxx.h"
#include "mdraw.h"
#include "mdebug.h"
#include "atext.h"

struct entries{					/* data for each row of the merged parts list table */
	int find_no;
	int quantity;
	char str[1024];
					};

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
extern char *uu_malloc();


/*********************************************************************
**    E_FUNCTION     :  ua_merge_pl_tables()
**      Merge user selected PARTS-LISTS TABLES.
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_merge_pl_tables()
	{
	UU_LIST                    pl_list;
	int                        pl_cnt = 10;
	struct UA_generic_draft    *pl;
	struct UA_generic_draft    *pl_next;
	UU_LIST							row_list;
	int                        row_cnt = 50;
	struct entries             *rows;
	struct entries             *next_row;

	struct UC_attributedatabag	*attr;
	struct UA_txt_rec          *note;
	struct UA_txtattr_rec      *txtattr;
	struct UA_PLOCREC				plocrec;
	struct UA_PICKENT				pickent;
	UU_KEY_ID 						curr_key, save_key, title_keys[10], key, view_key;
	int								i, j, k, d_status, num1, msg, num_entries, loop,
										relation, num_chars, retstat, mode, d_stat,
										count, status, id, quantity, row_count;
	UU_LOGICAL 						init, found;
	UU_TRUEDOUBLE              ff, uai_atof();

	char                       buff[1025];
	char								*txtstr;
	char                       field_str[7][100];

	UM_coord                   location, cpln_origin, xaxis, yaxis, zaxis,
										entity_origin, x_off_set, y_off_set, x_del,
										y_del;

	UU_REAL                    scale, hgt, length, char_del;

	int ua_rowcmp();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_merge_pl_tables"));

	attr = (struct UC_attributedatabag *) uu_malloc(sizeof(struct UC_attributedatabag));
	pl_next  = (struct UA_generic_draft *) uu_malloc(sizeof(struct UA_generic_draft));
	note  = (struct UA_txt_rec *) uu_malloc(sizeof(struct UA_txt_rec));
	txtattr  = (struct UA_txtattr_rec *) uu_malloc(sizeof(struct UA_txtattr_rec));
	txtstr   = (char *) uu_malloc(1024*sizeof(char));
	rows  = (struct entries *) uu_malloc(sizeof(struct entries));
	next_row  = (struct entries *) uu_malloc(sizeof(struct entries));
	uu_list_init(&pl_list, sizeof(struct UA_generic_draft), pl_cnt, pl_cnt);
	uu_list_init(&row_list, sizeof(struct entries), row_cnt, row_cnt);

	/* get user selection of the PARTS LIST tables */
	ud_lgeo(UU_TRUE, UD_editable);
	loop = UA_OPCOMPLETE;
	count = 0;
	while(loop != UA_REJECT)
		{
		num1 = 0;
		status = ud_pick_loc(UA_DRAFTING, 167, &plocrec, 1, &num1, UU_FALSE);
		if(status == UA_REJECT ) goto fexit;
		if(num1 != 0)
			{
			d_status = um_d_pickresolve(&plocrec.ppick, 1, &pickent);
			curr_key = um_get_pickkey(&pickent, 1);
		
			pl_next->key = curr_key;

			/* check that the users actually selected a PARTS LIST table */
			d_status = um_retrieve_data_relnum(pl_next->key, &relation);
			if(  relation != UA_LINEAR_DIM )
				uu_uerror0(UA_DRAFTING,44);
			else
				{
				uc_retrieve_data(pl_next, sizeof(struct UA_generic_draft	)) ;
				if(pl_next->etype != UA_PL_DIM)
					uu_uerror0(UA_DRAFTING,44);
				else
					{
					if(count == 0 ) ua_retrieve_attr(pl_next->key, attr);
					uu_list_push(&pl_list, pl_next);
					count++;
					}
				}
			}
		else
			loop = UA_REJECT;
		}

	/* user completed selection of the tables - now extract rows of the tables */
	if(count > 1)
		{
		pl =  (struct UA_generic_draft *) UU_LIST_ARRAY(&pl_list);
		row_count = 0;
		for(i=0;i<count;i++)
			{
			num_entries = pl[i].asso_blk_use;
			for(j=0;j<num_entries;j++)
				{
				if(pl[i].asso_blk[j].modifier != -99)
					{
					note->key = pl[i].asso_blk[j].key;
					uc_retrieve_data(note, sizeof(struct UA_txt_rec));

					if(i == 0 && j == 0)      /* if first row record retrieved, get attributes */	
						{
						txtattr->key = note->key;
						ur_retrieve_attr(txtattr);
						}

					/* transfer row data into a local merge list */
					strcpy(txtstr, note->tchar);
					num_chars = note->no_tchar;
					ua_strip_pl_entry(txtstr, &num_chars, field_str);
					ff = uai_atof(field_str[0]);
					id = ff;
					if(strcmp(field_str[5], "AR") != 0)
						{
						ff = uai_atof(field_str[5]);
						quantity = ff;
						}
					else
						quantity = -99;

					if(i == 0) 					/* first table selected is copied directly */
						{
						strcpy(rows->str, txtstr);
						rows->find_no = id;
						rows->quantity = quantity;
						uu_list_push(&row_list, rows);
						row_count++;
						}
					else						/* compare FIND NO.'s and sum quantity fields if == */
						{
						next_row = (struct entries *) UU_LIST_ARRAY(&row_list);
						found = UU_FALSE;
						for(k=0;k<row_count;k++)
							{
							if(id == next_row[k].find_no)   /* found one with same FIND NO. */
								{
								if(next_row[k].quantity != -99)
									next_row[k].quantity += quantity;
								found = UU_TRUE;
								break;
								}
							}
						if(!found)									/* unique FIND NO. , store data */
							{
							strcpy(rows->str, txtstr);
							rows->find_no = id;
							rows->quantity = quantity;
							uu_list_push(&row_list, rows);
							row_count++;
							}
						}
					}
				}
			}

		if(row_count > UA_max_no_entries)
			{
			uu_uerror0(UA_DRAFTING, 56);
			goto fexit;
			}

		/* completed extraction of old table rows
			now create new merged table */
		uu_list_sort(&row_list, ua_rowcmp);
		next_row = (struct entries *) UU_LIST_ARRAY(&row_list);

		/* get new location from the user */
		status = ud_world_coord(UA_DRAFTING, 142, location, 1, &num1, UU_FALSE);
		if(status == 0 || status == 2 || num1 == 0) goto fexit;
		ua_init_entity(UA_PL_DIM, 1, pl_next);
		pl_next->txt_just = UA_LEFT;
		pl_next->entity_site = UA_MIDDLE_CENTER;
		ua_getcpln(pl_next, cpln_origin, xaxis, yaxis, zaxis);
		um_vctovc(location, pl_next->dim_origin);

		/* generate table outline and titles */
		um_get_drwscale(&scale);
		ua_pl_tbl_box(pl_next, row_count, cpln_origin, xaxis, yaxis, zaxis, scale);
		ua_pl_titles(pl_next, cpln_origin, xaxis, yaxis, zaxis, title_keys, scale);
	
		/* loop on number of entries */
		hgt = UA_line_hgt /scale;
		char_del = 0.8*((hgt - UA_pl_char_hgt/scale)/2.0);
		um_vctmsc(xaxis, char_del, x_del);
		um_vctmsc(yaxis, char_del, y_del);
		char_del = (UA_pl_title + UA_pl_sub_title)/scale;
	
		for(i=0;i<row_count;i++)
			{
			 
			/* compute each entry off-set from the bottom of the table */
			length = i*hgt + char_del;
			um_vctmsc(yaxis, length, y_off_set);
			um_vcplvc(pl_next->dim_origin, y_off_set, location);
			um_vcplvc(location, x_del, location);
			um_vcplvc(location, y_del, location);
	
			/* create a note entity for the current row  */
			ua_pl_tbl_row(pl_next, next_row[i].quantity, next_row[i].str ,
							location, scale, txtattr,  &curr_key);
	
			/* add note key to list of entities associated with the table */
			if( curr_key != -1 )
				pl_next->asso_blk[i].key = curr_key;
			else
				{
				for(j=0;j<8;j++)
					uc_delete(title_keys[j]);
				for(j=0;j<i;j++)
					uc_delete(pl_next->asso_blk[j].key);
				goto fexit;
				}
			}
	
		/* add the table titles to the list of associated entities */
		pl_next->asso_blk_use = row_count + 8;
		for(i=0;i<8;i++)
			{
			pl_next->asso_blk[row_count+i].key = title_keys[i];
			pl_next->asso_blk[row_count+i].modifier = -99;
			pl_next->asso_blk[row_count+i].location[0] = scale;
			}
	
		/* delete old tables, create and display the new merged table */
		num_entries = UU_LIST_LENGTH(&pl_list);
		pl =  (struct UA_generic_draft *) UU_LIST_ARRAY(&pl_list);
		title_keys[0] = pl[0].key;
		for(i=1; i< num_entries;i++)
			{
			curr_key = pl[i].key;
			for(j=0;j<i;j++)
				{
				if(curr_key == title_keys[j])
					{
					curr_key = 0;
					break;
					}
				} 
			title_keys[i] = curr_key;
			}
		for(i=0;i<num_entries;i++)
			if(title_keys[i] > 0) uc_delete(title_keys[i]);
		ua_create_entity(pl_next, &curr_key);
		uc_display(pl_next);
		}

fexit:;
	/* free-up allocated space */
	uu_free(attr);
	uu_free(pl_next);
	uu_free(note);
	uu_free(txtattr);
	uu_free(rows);
	uu_free(next_row);
	uu_free(txtstr);
	uu_list_free(&pl_list);
	uu_list_free(&row_list);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_rowcmp(e1,e2)
**       Sort routine for the PARTS LIST merge algorithm.
**    PARAMETERS   
**       INPUT  : 
**				e1									first element to be compared
**				e2									second element
**       OUTPUT :  
**				none
**    RETURNS      :  1 if  e1->find_no < e2->find_no
**							-1 if e1->find_no > e2->find_no
**							 0 if e1->find_no = e2->find_no
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_rowcmp(e1,e2)

	struct entries *e1,*e2;

	{
	int id1,id2;

	/*------------------------------------------------------------------------
	**  Start of Executable Code
	**-----------------------------------------------------------------------*/

	id1 = e1->find_no;
	id2 = e2->find_no;

	if(id1 > id2) return(1);

	else if(id1 < id2) return(-1);

	else return(0);

	}

/*********************************************************************
**    E_FUNCTION :  ua_pl_tbl_row(pl, guantity, str,location, scale, txtattr, key)
**       Create a merged PARTS LIST table entry.
**    PARAMETERS   
**       INPUT  : 
**          pl											PARTS LIST structure
**          quantity                         new quantity value
**				str                              old entry string
**				location									location of the entry in the table
**				scale										drawing scale
**				txtattr									text attribute bundle
**       OUTPUT :  
**          key										UNIBASE key to the text
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_pl_tbl_row(pl, quantity, str, location, scale, txtattr,  key)

	struct UA_generic_draft  	*pl;
	int 								quantity;
	char								*str;
	UM_coord 						location;
	UU_REAL 							scale;
	struct	UA_txtattr_rec		*txtattr;	
	UU_KEY_ID 						*key;

	{
	struct   UA_txt_rec			note;			
	struct	UA_txtattr_rec		attr;	
	char								field_str[7][100], new_str[1025];
	int 								i, str_length;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_pl_tbl_row() quantity = %d"));
	
	/* create new text entity	*/
	ur_setup_data(UA_TEXT_REL,&note,sizeof(struct UA_txt_rec));
	ua_init_txtrec(&note,&attr,&UA_txtattr,UU_FALSE);

	/* copy information into the text record */
	if(quantity > 0)
		{
		str_length = strlen(str);
		ua_strip_pl_entry(str, &str_length, field_str);
		sprintf(new_str, "%d", quantity);
		strcpy(field_str[5], new_str);
		new_str[0] = '\0';
		for(i=0;i<7;i++)
			{
			strcat(new_str, field_str[i]);
			strcat(new_str, ";");
			}
		}
	else
		strcpy(new_str, str);

	strcpy(note.tchar, new_str);
	note.no_tchar = strlen(new_str);

	/* set position of the text entity */
	um_vctovc(location, note.position);
	ua_txt_origin(&note,txtattr,UU_FALSE);

	/* create the UNIBASE record */
	uc_create_data(&note,UM_DEFAULT_TF,txtattr);
	*key = note.key;

done:
	uu_dexit;
	return;
	}
