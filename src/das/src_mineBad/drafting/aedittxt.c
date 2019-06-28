/*********************************************************************
**    NAME         :
**       CONTAINS:
**				ua_edit_save
**				ua_edit_restore
**				ua_edit_init
**				ua_edit_text
**				ua_count_lines
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aedittxt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:34
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aedittxt.c 4.3 8/10/89 08:02:16 single"};
#else
static char uu_sccsident[]={"@(#) aedittxt.c 4.3 8/10/89 08:02:16 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "mdcoord.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"
#include "adrfdefs.h"
#include "adraft.h"
#include "adrfcom.h"
#include "atext.h"
#include "mdrel.h"

#define UA_EDIT_MAIN_TEXT 1
#define UA_EDIT_APP_TEXT  2
#define UA_REP_APP_TEXT   3
#define CLSEP '\n'


static  int save_dim_line_font;
static  int save_dim_line_color;
static  int save_ext_line_font;
static  int save_ext_line_color;
static  int save_arrow_symbol;
static  int save_arrow_color;
static  UU_REAL 	save_dim_line_dens;
static  UU_REAL 	save_ext_line_dens;
static  UU_REAL 	save_arrow_dens;

/*********************************************************************
**    E_FUNCTION     : ua_edit_save(edrf, base_pt, base_vec)
**		Save system defaults and replace with entity defaults
**    PARAMETERS   
**       INPUT  : 
**				edrf								drafting entity record
**       OUTPUT :  
**				base_pt							base point for linear dimensions
**				base_vec							base vector for linear dimensions
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_edit_save(edrf, base_pt, base_vec)
struct UA_generic_draft	(*edrf);
UM_coord	base_pt, base_vec;
	{
	int		i, j, k;
	UM_coord tmp1;

	uu_denter(UU_STRC,(us,"ua_edit_save"));

	save_dim_line_font = UA_dim_line_font;
	save_dim_line_color = UA_dim_line_color;
	save_ext_line_font = UA_ext_line_font;
	save_ext_line_color = UA_ext_line_color;
	save_arrow_symbol = UA_arrow_symbol;
	save_arrow_color = UA_arrow_color;
	save_dim_line_dens = UA_dim_line_dens;
	save_ext_line_dens = UA_ext_line_dens;
	save_arrow_dens = UA_arrow_dens;

	if(  edrf->line_blk_use>0  )
		{
		for(i=0;i<edrf->line_blk_use;i++)
			{
			switch( edrf->line_blk[i].subtype )
				{
				case dim_line:
					{
					UA_dim_line_font = edrf->line_blk[i].line.line_font;
					UA_dim_line_color = edrf->line_blk[i].line.color;
					UA_dim_line_dens = edrf->line_blk[i].line.line_density;
					um_vctovc(edrf->line_blk[i].line_seg[0],base_pt);
					um_vcmnvc(edrf->line_blk[i].line_seg[1],edrf->
						    line_blk[i].line_seg[0],tmp1);
					um_unitvc(tmp1,base_vec);
					}
					break;
				case ext_line:
					{
					UA_ext_line_font = edrf->line_blk[i].line.line_font;
					UA_ext_line_color = edrf->line_blk[i].line.color;
					UA_ext_line_dens = edrf->line_blk[i].line.line_density;
					}
					break;
				}
			}
		}
	if( ( ( edrf->arc_blk_use!=0 )&&( edrf->line_blk_use==0) ) )
		{
		UA_dim_line_font = edrf->arc_blk[0].arc.line_font;
		UA_dim_line_color = edrf->arc_blk[0].arc.color;
		UA_dim_line_dens = edrf->arc_blk[0].arc.line_density;
		}
	if( ( edrf->arrow_blk_use!=0 ) )
		{
		UA_arrow_symbol = edrf->arrow_blk[0].arrow_type;
		UA_arrow_color = edrf->arrow_blk[0].arrow.color;
		UA_arrow_dens = edrf->arrow_blk[0].arrow.line_density;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_edit_init(edrf, prev_txt_blk_use, app_txt_blk)
**		Initialize entity data blocks
**    PARAMETERS   
**       INPUT  : 
**				edrf								drafting entity record
**				prev_txt_blk_use				
**				app_txt_blk							
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_edit_init(edrf, prev_txt_blk_use, app_txt_blk)
struct UA_generic_draft	*edrf;
int		*prev_txt_blk_use;
int		*app_txt_blk;
	{
	int		i;

	uu_denter(UU_STRC,(us, "ua_edit_init"));

	if( ( edrf->arc_blk_use>0 ) )
		ua_initent_arc_blk(edrf,1,1);
	else
		ua_initent_arc_blk(edrf,1,0);

	edrf->arc_blk_use = 0;

	if( ( edrf->line_blk_use>0 ) )
		ua_initent_line_blk(edrf,1,1);
	else
		ua_initent_line_blk(edrf,1,0);

	edrf->line_blk_use = 0;

	if( ( edrf->arrow_blk_use>0 ) )
		ua_initent_arrow_blk(edrf,1,1);
	else
		ua_initent_arrow_blk(edrf,1,0);

	edrf->arrow_blk_use = 0;

	if( ( edrf->txt_entry==UA_SYS_TEXT ) )
		{
		if( ( edrf->txt_blk_use>0 ) )
			{
			if( ( edrf->txt_blk[0].subtype==app_cre_txt ) )
				{
				edrf->txt_blk_use = 1;
				ua_initent_txt_blk(edrf,2,1);
				}
			else
				{
				edrf->txt_blk_use = 0;
				ua_initent_txt_blk(edrf,1,1);
				}
			}
		else
			{
			edrf->txt_blk_use = 0;
			ua_initent_txt_blk(edrf,1,0);
			}
		}
	else
		{
		if( ( edrf->txt_blk_use>0 ) )
			{
			ua_initent_txt_blk(edrf,( edrf->txt_blk_use+1 ),1);
			}
		else
			{
			ua_initent_txt_blk(edrf,1,0);
			}
		}
	(*prev_txt_blk_use) = edrf->txt_blk_use;
	(*app_txt_blk) = 0;
	for(i=0;i<edrf->txt_blk_use;i++)
		{
		if( ( edrf->txt_blk[i].subtype==app_cre_txt ) )
			{
			(*app_txt_blk) = i+1;
			}
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_edit_text()
**		Edit text blocks of existing drafting entity and regenerate.
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_edit_text()
	{
	struct UA_PLOCREC	plocrec;
	struct UA_PICKENT	pickent;
	struct UA_generic_draft	edrf;
	struct UA_txt_rec  text;
	int		prev_txt_blk_use, number_entered, option, i, status,
				app_txt_blk, num, line_no, relation, size, choice, itxt,
				msg, key, count, text_fit_flag,
text_rot_flag;
	UM_coord base_pt, cpln_origin, xaxis, base_vec, yaxis, zaxis, ext_vec, tmp1;
	UU_REAL	off_set, ext_offset_dist;
	UU_LOGICAL	base_flg, ok, txtfound, main_text, appended_text, ud_yesno();

	uu_denter(UU_STRC,(us,"ua_edit_text()"));

entity:

	text_fit_flag = 1;
	text_rot_flag = 1;
	/* get entity from user */
	status = ud_pick_loc(13,120,&(plocrec),1,&(number_entered), UU_FALSE);
	switch( status )
		{
		case UA_REJECT:
			goto procexit;
		case UA_ALT_ACTION:
			goto entity;
		case UA_OPCOMPLETE:
			break;
		}
	if(  number_entered < 1  )
		{
		uu_dexit;
		return;
		}
	status = um_d_pickresolve(&(plocrec.ppick),1,&(pickent));
	key = um_get_pickkey(&(pickent),1);
	edrf.key = key;
	text.key = key;

	/* check type of entity picked */
	status = um_retrieve_data_relnum(edrf.key,&(relation));
	if(  relation != UA_LINEAR_DIM  && relation != UA_TEXT_REL )
		{
		uu_uerror0(UA_DRAFTING,22);
		goto entity;
		}

	/* branch on type */
	if( relation == UA_LINEAR_DIM)
		{
		uc_retrieve_data(&edrf,sizeof(struct UA_generic_draft	)) ;
		}
	else
		{
		uc_retrieve_data(&text,sizeof(struct UA_txt_rec	)) ;
		ua_edit_txt(&text);
		goto entity;
		}

	/* reject FPT for now */
	if( ( edrf.etype==UA_FANDP_TOL ) )
		{
		uu_uerror0(UA_DRAFTING,27);
		goto entity;
		}

	/* check if NOTE, LABEL or BALLOON  */
	if( ( ( edrf.etype==UA_LABEL_DIM )||
			( edrf.etype==UA_NOTE_DIM  )||
			( edrf.etype==UA_BALLOON_DIM)) )
		{
		option = 5;
		}
	else
		{
			
		/*check if any editable text */
		txtfound = UU_FALSE;
		main_text = UU_FALSE;
		appended_text = UU_FALSE;
		for(itxt=0;itxt<edrf.txt_blk_use;itxt++)
			{
			if(( edrf.txt_blk[itxt].subtype==main_txt1 )
												&&( edrf.txt_entry==UA_USER_TEXT ))
				{
				txtfound = UU_TRUE;
				main_text = UU_TRUE;
				}
			else if(( edrf.txt_blk[itxt].subtype==main_txt2 )
													&&( edrf.txt_entry==UA_USER_TEXT ))
				{
				txtfound = UU_TRUE;
				main_text = UU_TRUE;
				}
			else if(( edrf.txt_blk[itxt].subtype==app_cre_txt ))
				{
				txtfound = UU_TRUE;
				appended_text = UU_TRUE;
				}
			}
		if(!txtfound)
			{
			uu_uerror0(UA_DRAFTING,21);
			ok = ud_yesno(0, "Add Appended text?", "Question");
			if(!ok)
				goto entity;
			else
				option = UA_REP_APP_TEXT;
			}
				
		if(main_text && appended_text)
			{
			status = ua_popmenu(21,&(option));
			if( ( status!=UA_OPCOMPLETE ) ) goto entity;
			if( ( ( status==UA_OPCOMPLETE )&&( option==0 ) ) ) goto entity;
			}
		else if(main_text && !appended_text)
			option = UA_EDIT_MAIN_TEXT;
		else
			option = UA_EDIT_APP_TEXT;
		}

	ua_getcpln(&(edrf),cpln_origin,xaxis,yaxis,zaxis);
	if( ( option==5 ) )
		{

		/* processing a NOTE, LABEL or BALLOON */
note_loop:
		itxt = 1;
		status = ua_popmenu(23,&(choice));
		if( ( status!=UA_OPCOMPLETE ) ) goto entity;
		if( ( ( status==UA_OPCOMPLETE )&&( choice==0 ) ) ) goto entity;
		if( ( choice==5 ) )
			{
			goto entity;
			}
		else
			{
			ua_edit_init(&(edrf),&(prev_txt_blk_use),&(app_txt_blk));
			ua_count_lines(edrf.txt_blk[itxt-1].tstring,
								&(edrf.txt_blk[itxt-1].char_cnt),&line_no);
			if(line_no != 1)
				{
				switch( choice )
					{
					case 1:
						msg = 121;
						break;
					case 2:
						msg = 122;
						break;
					case 3:
						msg = 124;
						break;
					case 4:
						msg = 123;
						break;
					}
				status = ud_integer(UA_DRAFTING,msg,&(line_no),1,&(num),UU_FALSE);
				if( ( status!=UA_OPCOMPLETE ) )
					{
					goto entity;
					}
				if( ( line_no<1 ) )
					{
					uu_uerror0(UA_DRAFTING,38);
					goto entity;
					}
				}
			}
		status = ua_edit_text_trvfrm(edrf.txt_blk[itxt-1].tstring,&(
		edrf.txt_blk[itxt-1].char_cnt),line_no,choice);
		if( ( status>0 ) ) goto entity;
		switch( edrf.etype )
			{
			case UA_LABEL_DIM:
				{
				ua_label_create(&(edrf));
				}
				break;
			case UA_NOTE_DIM:
				{
				ua_create_note(&(edrf));
				}
				break;
			case UA_BALLOON_DIM:
				{
				ua_balloon_create(&(edrf));
				}
				break;
			}
		status = ua_update_entity(edrf.key,&(edrf));
		if( ( status!=0 ) ) ua_create_entity(&(edrf),&(edrf.key));
		uc_display(&(edrf));
		uc_retrieve_data(&(edrf),sizeof(struct UA_generic_draft	)) ;
		ua_getcpln(&(edrf),cpln_origin,xaxis,yaxis,zaxis);
		goto note_loop;
		}
	else
		{
		txtfound = UU_FALSE;
		ua_edit_save(&(edrf),base_pt,base_vec);
		ua_edit_init(&(edrf),&(prev_txt_blk_use),&(app_txt_blk));
		for(itxt=0;itxt<edrf.txt_blk_use;itxt++)
			{
			if( ( ( ( edrf.txt_blk[itxt].subtype==main_txt1 )
												&&( option==UA_EDIT_MAIN_TEXT ) )
												&&( edrf.txt_entry==UA_USER_TEXT ) ) )
				{
				switch( edrf.etype )
					{
					case UA_LINEAR_DIM:
					case UA_SYM_DIM:
					case UA_ANGULAR_DIM:
					case UA_ARC_LEN_DIM:
					case UA_DIA_IN_DIM:
					case UA_RAD_CEN_DIM:
						{
						txtfound = UU_TRUE;
						ua_edit_text_trvfrm(edrf.txt_blk[itxt].tstring,
								&(edrf.txt_blk[itxt].char_cnt),1,1);
						}
						break;
					}
				}
			else if( ( ( ( edrf.txt_blk[itxt].subtype==main_txt2 )
													&&( option==UA_EDIT_MAIN_TEXT ) )
													&&( edrf.txt_entry==UA_USER_TEXT ) ) )
				{
				switch( edrf.etype )
					{
					case UA_LINEAR_DIM:
					case UA_SYM_DIM:
					case UA_ANGULAR_DIM:
					case UA_ARC_LEN_DIM:
					case UA_DIA_IN_DIM:
					case UA_RAD_CEN_DIM:
						{
						txtfound = UU_TRUE;
						ua_edit_text_trvfrm(edrf.txt_blk[itxt].tstring,
									&(edrf.txt_blk[itxt].char_cnt),1,1);
						}
						break;
					}
				}
			else if(( edrf.txt_blk[itxt].subtype==app_cre_txt )
													&&( option==UA_EDIT_APP_TEXT ))
				{
				switch( edrf.etype )
					{
					case UA_LINEAR_DIM:
					case UA_SYM_DIM:
					case UA_ANGULAR_DIM:
					case UA_ARC_LEN_DIM:
					case UA_DIA_IN_DIM:
					case UA_RAD_CEN_DIM:
						{
						txtfound = UU_TRUE;
						status = ua_popmenu(23,&choice);
						if(( status!=UA_OPCOMPLETE ) ||
							(( status==UA_OPCOMPLETE )&&( choice==0 )) ||
							(choice == 5)) goto entity;
						ua_count_lines(edrf.txt_blk[itxt].tstring,
									&(edrf.txt_blk[itxt].char_cnt),&line_no);
						if(line_no != 1)
							{
							switch( choice )
								{
								case 1:
									msg = 121;
									break;
								case 2:
									msg = 122;
									break;
								case 3:
									msg = 124;
									break;
								case 4:
									msg = 123;
									break;
								}
							status = ud_integer(13,msg,&line_no,1,&num,UU_FALSE);
							if( ( status!=UA_OPCOMPLETE ) ) goto entity;
							}
						ua_edit_text_trvfrm(edrf.txt_blk[itxt].tstring,
									&(edrf.txt_blk[itxt].char_cnt),line_no,choice);
						}
						break;
					}
				}
			}

		switch( option )
			{
			case UA_REP_APP_TEXT:
				{
				if( ( app_txt_blk==0 ) )
					ua_initent_apptext(&(edrf));
				else
					edrf.txt_blk_use = ( app_txt_blk-1 );
				if( ( edrf.appn_text==UA_NO_APP_TXT ) )
					{
					uu_uerror0(UA_DRAFTING,25);
					goto entity;
					}
				ua_apptext_subf(&(edrf));
				if( ( app_txt_blk>0 ) )
					{
					edrf.txt_blk_use = prev_txt_blk_use;
					}
				txtfound = UU_TRUE;
				}
				break;
			}

		if( txtfound )
			{
			switch( edrf.etype )
				{
				case UA_LINEAR_DIM:
					{
					off_set = 0.000000e+000;
					status = ua_cre_lin_dim(cpln_origin,xaxis,yaxis,zaxis,
								base_pt,base_vec,&off_set,&edrf, &text_fit_flag, &text_rot_flag);
					if( ( status!=1 ) )
						{
						ua_edit_restore();
						uu_uerror0(UA_DRAFTING,26);
						goto entity;
						}
					break;
					}
				case UA_CHAIN_DIM:
					{
					um_vctovc(edrf.asso_blk[3].location,base_pt);
					um_vctovc(edrf.asso_blk[2].location,base_vec);
					base_flg = UU_FALSE;
					status = ua_cre_chain(cpln_origin,xaxis,yaxis,zaxis,& base_flg,
									base_pt,base_vec,&edrf);
					if( ( status!=1 ) )
						{
						ua_edit_restore();
						uu_uerror0(UA_DRAFTING,26);
						goto entity;
						}
					break;
					}
				case UA_BASELN_DIM:
					{
					um_vctovc(edrf.asso_blk[3].location,base_pt);
					um_vctovc(edrf.asso_blk[2].location,base_vec);
					um_vctovc(edrf.asso_blk[4].location,ext_vec);
					count = 0;
					ext_offset_dist = edrf.gap_to_geo;
					base_flg = UU_FALSE;
					status = ua_cre_baseln(cpln_origin,xaxis,yaxis,zaxis,&base_flg
								,base_pt,base_vec,count,ext_offset_dist, ext_vec,&edrf);
					if( ( status!=1 ) )
						{
						ua_edit_restore();
						uu_uerror0(UA_DRAFTING,26);
						goto entity;
						}
					break;
					}
				case UA_ARC_LEN_DIM:
					ua_arclen_create(&edrf);
					break;
				case UA_ANGULAR_DIM:
					ua_angular_regenerate(&edrf);
					break;
				case UA_RAD_CEN_DIM:
					ua_radius_regen(&edrf);
					break;
				case UA_DIA_IN_DIM:
					{
					switch( edrf.subtype )
						{
						case 3:
						case 2:
						case 1:
							ua_diam_regen(&edrf);
							break;
						case 7:
							ua_dia_shaft_regen(&edrf);
							break;
						case 8:
							ua_dia_cyln_regen(&edrf);
							break;
						}
					}
					break;
				}
display:
			status = ua_update_entity(edrf.key,&edrf);
			if( ( status!=0 ) ) ua_create_entity(&edrf,&edrf.key);
			ua_edit_restore();
			uc_display(&edrf);
			}
		else
			{
			uu_uerror0(UA_DRAFTING,21);
			}
		goto entity;
		}

procexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_edit_restore()
**		Restore system defaults
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_edit_restore()
	{
	uu_denter(UU_STRC,(us,"ua_edit_restore()"));

	UA_dim_line_font = save_dim_line_font;
	UA_dim_line_color = save_dim_line_color;
	UA_ext_line_font = save_ext_line_font;
	UA_ext_line_color = save_ext_line_color;
	UA_arrow_symbol = save_arrow_symbol;
	UA_arrow_color = save_arrow_color;
	UA_dim_line_dens = save_dim_line_dens;
	UA_ext_line_dens = save_ext_line_dens;
	UA_arrow_dens = save_arrow_dens;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_count_lines(stg,&stglen,no_lines)
**			Count number of lines in a string.
**    PARAMETERS   
**       INPUT  : 
**			stg						multi-line string to edit.
**			stglen					length of stg.
**       OUTPUT :  
**			no_lines					number of lines in the string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_count_lines(stg,stglen,no_lines)
	char		*stg;				
	int		*stglen;		
	int		*no_lines;
	{
	int i, ilinemax;

	/*------- make sure string ends in a line sep char ------------*/
	if ( stg[*stglen-1]!=CLSEP )
		{
		stg[*stglen] = CLSEP;		
		(*stglen)++;
		}
	/*-------- go through string and count lines ------------------*/
	ilinemax = 0;
	for (i=0; i<*stglen; i++)
		{
		if (stg[i]==CLSEP) ilinemax++;
		}
	*no_lines = ilinemax;
	}
